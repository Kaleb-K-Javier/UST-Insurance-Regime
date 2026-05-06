# ==============================================================================
# 04a_TX_Premium_All_1999_onwards.R
# ==============================================================================
# PURPOSE
#
#   Build a Mid-Continent premium variable for EVERY Texas facility-year from
#   1999 onward, regardless of who actually purchased a Mid-Continent contract.
#   The four existing era scripts (12_/13_/14_) only rate actual Mid-Continent
#   buyers and only cover 2006+. This wrapper fills both gaps.
#
#   Working assumption: every Texas-treated firm is treated as if covered under
#   Mid-Continent. For 1999-2005 (no rate filing exists) we apply the 2006 era
#   schedule flat in nominal terms — Texas rate filings do not move with
#   inflation due to legal constraints, so backward extrapolation at the 2006
#   level is the most defensible imputation.
#
# OUTPUT
#
#   Data/Analysis/tx_midcont_premium_all_1999_onwards.csv
#   Columns:
#     panel_id           "<FACILITY_ID>_TX"  (matches 02b's panel_id convention)
#     panel_year         int
#     mean_tank_premium  $/tank/yr — mean over facility-months in the year
#     base_premium       facility-year base premium (mean over months of
#                        sum-across-tanks)
#     sched_min, sched_max  ±40% schedule envelope on base_premium
#     n_tanks_rated      mean # tanks contributing in that year
#     source_era         "2006","2014","2019","2021" — modal era for the
#                        facility-year (most months)
#     is_imputed_pre2006 1 if year ∈ [1999, 2005], else 0
#
#   Designed for direct left-join to facility_panel.csv on (panel_id,
#   panel_year). For control-state rows the join leaves these columns NA;
#   the DCM prep script falls back to state_fr_premium for controls.
#
# DESIGN NOTES
#
#   The four era scripts each define their own rating-factor brackets (BASE_RATE
#   is constant at $300 across eras; ILF, age, construction, leak, piping
#   factors evolve filing-by-filing). Rather than refactor those scripts, we
#   copy the era-specific factor definitions in place here and dispatch by
#   YEAR/MONTH. This duplicates rating tables across files; that is a
#   deliberate trade-off to keep the existing buyer-rating pipeline untouched.
#
#   The 2019 and 2021 filings introduced optional surcharges (flood, marina,
#   aquifer, retro date, site capacity, upgrade credit). The static tank
#   table doesn't carry those flags for most facilities, so they default to
#   neutral (0). Coverage form defaults to "A" (1.00). Deductible defaults
#   to $5k (1.00). These defaults match what the era scripts assume.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(here)
})

source(here::here("Code", "Helpers", "data_paths.R"))

setDTthreads(0L)

# ---- Paths ----
# Raw tank snapshot lives at Z:/.../Data/Raw/state_databases/Texas/raw_pst_ust.csv
# (the existing 12_/13_/14_ scripts source it via a now-stale "panel_merge_staging"
# alias; we go to the canonical location). Section A below applies the same
# transformations those scripts apply locally to build the static tank table.
RAW_TX_DIR  <- z_path("Data", "Raw", "state_databases", "Texas")
OUT_DIR     <- here::here("Data", "Analysis")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Constants (constant across all four eras) ----
BASE_RATE          <- 300
SCHED_MIN_FACTOR   <- 0.60
SCHED_MAX_FACTOR   <- 1.40

# ---- Synthetic-panel window ----
PANEL_START <- ymd("1999-01-01")
PANEL_END   <- ymd("2030-12-31")

# Era boundaries (closed-open on the right)
# Era 1999-2005 reuses the 2006 engine (per legal-fixed-rate assumption).
ERA_BOUNDS <- list(
  era_2006 = c(ymd("1999-01-01"), ymd("2014-04-30")),
  era_2014 = c(ymd("2014-05-01"), ymd("2019-01-31")),
  era_2019 = c(ymd("2019-02-01"), ymd("2021-04-30")),
  era_2021 = c(ymd("2021-05-01"), PANEL_END)
)

cat("=================================================================\n")
cat("04a: All-TX Mid-Continent premium imputation, 1999 onwards\n")
cat("=================================================================\n\n")

# ==============================================================================
# 1. Build static tank table inline (Section A logic from the era scripts)
# ==============================================================================
# The era scripts (12_, 13_, 14_) each duplicate this Section A locally. We
# replicate it here once so 04a is self-contained: read the raw tank snapshot
# from Z and assemble the rating-relevant attribute table the engines need.
cat("[1/5] Loading raw tank snapshot and assembling static tank table...\n")

raw_path <- file.path(RAW_TX_DIR, "raw_pst_ust.csv")
if (!file.exists(raw_path))
  stop("raw_pst_ust.csv not found at: ", raw_path)

ust_raw <- fread(raw_path)

# 1) merge keys
ust_raw[, FACILITY_ID := trimws(substr(FACILITY_ID_PAD, 1, 6))]
ust_raw[, UST_ID      := toupper(UST_ID)]

# 2) basic parsing
ust_raw[, INSTALL_DATE := ymd(INSTALL_DATE)]
ust_raw[, STATUS       := toupper(trimws(STATUS))]

# 3) STATUS -> Mid-Continent canonical labels
ust_raw[, tank_status := fcase(
  STATUS == "IN USE",                                             "In Use",
  STATUS == "TEMP OUT OF SERVICE",                                "Temporarily Out of Use",
  STATUS %chin% c("REMOVED FROM GROUND", "PERM FILLED IN PLACE"), "Permanently Out of Use",
  default = "Unknown")]

# 4) closure flag (informational; rating uses EFF_DATE filter)
ust_raw[, is_closed_removed := as.integer(
  STATUS %chin% c("REMOVED FROM GROUND", "PERM FILLED IN PLACE"))]
ust_raw[, CLOSED_DATE := ymd(end_date)]

# 5) wall-type flags
ust_raw[, `:=`(
  single_walled = as.integer(TANK_SINGLE == "Y"),
  double_walled = as.integer(TANK_DOUBLE == "Y"))]

# 6) tank-construction dummies
ust_raw[, `:=`(
  is_steel_cathodic        = single_walled & (CORR_TANK_CP == "Y"),
  is_double_walled_steel   = double_walled & (TANK_MAT_STEEL == "Y"),
  is_reinforced_fiberglass = (TANK_MAT_FRP == "Y"))]
ust_raw[, is_other_tank_material := as.integer(
  !(is_steel_cathodic | is_double_walled_steel | is_reinforced_fiberglass))]

# 7) piping dummies
ust_raw[, `:=`(
  pip_steel_cathodic = as.integer(PIP_MAT_STEEL == "Y" & CORR_PIPE_CP == "Y"),
  pip_fiberglass     = as.integer(PIP_MAT_FRP   == "Y"),
  pip_double_walled  = as.integer(PIP_DOUBLE    == "Y"),
  pip_suction        = as.integer(PIPE_TYPE == "Suction"),
  pip_pressure_ll    = as.integer(PIPE_TYPE == "Pressurized" | DET_P_LLD == "Y"))]

# 8) leak-detection dummies
ust_raw[, `:=`(
  det_interstitial = as.integer(DET_C_INTERSTITIAL == "Y" | DET_P_INTERSTITIAL == "Y"),
  det_ATG          = as.integer(DET_C_ATG == "Y"          | DET_P_LLD == "Y"),
  det_vapor        = as.integer(DET_C_VAPOR == "Y"        | DET_P_VAPOR == "Y"),
  det_groundwater  = as.integer(DET_C_GW == "Y"           | DET_P_GW == "Y"))]

# 9) assemble static table
tank_static <- ust_raw[, .(
  FACILITY_ID, UST_ID, INSTALL_DATE, CLOSED_DATE, tank_status,
  is_steel_cathodic, is_double_walled_steel, is_reinforced_fiberglass,
  pip_steel_cathodic, pip_fiberglass, pip_double_walled,
  pip_suction, pip_pressure_ll,
  det_interstitial, det_ATG, det_vapor, det_groundwater)]
rm(ust_raw); invisible(gc())

cat(sprintf("  Tanks: %s | facilities: %s\n",
            format(nrow(tank_static),               big.mark = ","),
            format(uniqueN(tank_static$FACILITY_ID), big.mark = ",")))


# ==============================================================================
# 2. Rating-factor definitions, organized by era
# ==============================================================================
# Each era's tank_premium = BASE_RATE * ILF * (1 + sec_load) [* coverage_factor
# in 2019/2021]. Era-specific brackets copied from the four 12_/13_/14_ files.

to_bool <- function(x) !is.na(x) & as.logical(x)

status_factor <- function(s) {
  fcase(
    s == "In Use",                  0.00,
    s == "Temporarily Out of Use", -0.50,
    s == "Permanently Out of Use", -0.75,
    default = NA_real_
  )
}

construction_factor <- function(is_steel_cathodic, is_reinforced_fiberglass,
                                is_double_walled_steel) {
  dw  <- to_bool(is_double_walled_steel)
  sc  <- to_bool(is_steel_cathodic)
  frp <- to_bool(is_reinforced_fiberglass)
  fcase(dw, -0.20, sc | frp, 0.00, default = 0.00)
}

leak_factor <- function(det_interstitial, det_ATG, det_vapor, det_groundwater) {
  di <- to_bool(det_interstitial); da <- to_bool(det_ATG)
  dv <- to_bool(det_vapor);        dg <- to_bool(det_groundwater)
  fcase(di, -0.20, da, -0.10, dv | dg, -0.10, default = 0.00)
}

pipe_const_factor <- function(pip_steel_cathodic, pip_fiberglass,
                              pip_double_walled) {
  st <- to_bool(pip_steel_cathodic); fb <- to_bool(pip_fiberglass)
  dw <- to_bool(pip_double_walled)
  fcase(st, +0.10, dw, -0.10, fb, 0.00, default = 0.00)
}

pipe_design_factor <- function(pip_suction, pip_pressure_ll) 0  # neutral, all eras

# ---- Era 2006 (also used 1999-2005 by assumption) ---------------------------
ilf_factor_2006 <- function(occ, agg) {
  occ <- fifelse(is.na(occ), 1e6, occ)
  agg <- fifelse(is.na(agg), 1e6, agg)
  fcase(occ == 1e6 & agg == 1e6, 1.18,
        occ == 1e6 & agg == 2e6, 1.20,
        default = 1.00)
}

age_factor_2006 <- function(a) {
  fcase(is.na(a), NA_real_,
        a <=  5, -0.10, a <= 10,  0.00, a <= 15,  0.05,
        a <= 20,  0.10, a <= 25,  0.20, a >  25,  0.25)
}

# ---- Era 2014 ---------------------------------------------------------------
ilf_factor_2014 <- function(occ, agg) {
  occ <- fifelse(is.na(occ), 1e6, occ)
  agg <- fifelse(is.na(agg), 1e6, agg)
  fcase(occ == 1e6 & agg == 1e6, 1.18,
        occ == 1e6 & agg == 2e6, 1.20,
        default = 1.18)
}

age_factor_2014 <- function(a) {
  fcase(is.na(a), NA_real_,
        a <=  5, -0.10, a <= 10,  0.00, a <= 15,  0.05,
        a <= 20,  0.10, a <= 25,  0.20, a <= 30,  0.25,
        a <= 35,  0.35, a <= 40,  0.45, a <= 45,  0.55,
        a <= 50,  0.65, a >  50,  0.75)
}

# ---- Era 2019 / 2021 (granular age, plus coverage / deductible multipliers)
ilf_factor_2019 <- ilf_factor_2014  # same table

age_factor_2019 <- function(a) {
  fcase(is.na(a), NA_real_,
        a <=  2, -0.20, a <=  4, -0.16, a <=  6, -0.12, a <=  8, -0.08,
        a <= 10, -0.04, a <= 15,  0.00, a <= 20,  0.10, a <= 25,  0.20,
        a <= 30,  0.25, a <= 35,  0.35, a <= 40,  0.45, a <= 45,  0.55,
        a <= 50,  0.65, a >  50,  0.75)
}

deduct_factor_2019 <- function(ded) {
  fcase(ded <=  5e3 | is.na(ded), 1.00,
        ded <= 10e3,              0.92,
        ded <= 25e3,              0.80,
        ded <= 50e3,              0.60,
        ded >  50e3,              0.40)
}

coverage_factor_2019 <- function(form) {
  fcase(toupper(form) == "B", 1.50,
        toupper(form) == "C", 1.25,
        default = 1.00)
}

# 2021 is byte-identical to 2019 in the rating tables — share definitions.
ilf_factor_2021     <- ilf_factor_2019
age_factor_2021     <- age_factor_2019
deduct_factor_2021  <- deduct_factor_2019
coverage_factor_2021 <- coverage_factor_2019


# ==============================================================================
# 3. Era-engine functions — same input columns, return tank_premium added
# ==============================================================================
# Each takes a data.table dt with these columns and adds tank_premium:
#   tank_status, age_years, COVER_OCC, COVER_AGG (NA -> defaults),
#   is_steel_cathodic, is_reinforced_fiberglass, is_double_walled_steel,
#   det_interstitial, det_ATG, det_vapor, det_groundwater,
#   pip_steel_cathodic, pip_fiberglass, pip_double_walled,
#   pip_suction, pip_pressure_ll
# Optional 2019/2021 inputs default to neutral when missing:
#   COVER_FORM ("A"), DEDUCTIBLE_USD (5e3)

apply_engine <- function(dt, era) {
  stopifnot(era %in% c("era_2006","era_2014","era_2019","era_2021"))

  status_load <- status_factor(dt$tank_status)
  age_load <- switch(era,
    era_2006 = age_factor_2006(dt$age_years),
    era_2014 = age_factor_2014(dt$age_years),
    era_2019 = age_factor_2019(dt$age_years),
    era_2021 = age_factor_2021(dt$age_years))
  ilf <- switch(era,
    era_2006 = ilf_factor_2006(dt$COVER_OCC, dt$COVER_AGG),
    era_2014 = ilf_factor_2014(dt$COVER_OCC, dt$COVER_AGG),
    era_2019 = ilf_factor_2019(dt$COVER_OCC, dt$COVER_AGG),
    era_2021 = ilf_factor_2021(dt$COVER_OCC, dt$COVER_AGG))
  cons_load <- construction_factor(dt$is_steel_cathodic,
                                   dt$is_reinforced_fiberglass,
                                   dt$is_double_walled_steel)
  leak_load <- leak_factor(dt$det_interstitial, dt$det_ATG,
                           dt$det_vapor,        dt$det_groundwater)
  pc_load   <- pipe_const_factor(dt$pip_steel_cathodic, dt$pip_fiberglass,
                                 dt$pip_double_walled)
  pd_load   <- pipe_design_factor(dt$pip_suction, dt$pip_pressure_ll)

  sec_load  <- rowSums(cbind(status_load, age_load, cons_load,
                             leak_load, pc_load, pd_load), na.rm = TRUE)

  # 2019 / 2021 also multiply by deductible and coverage-form factors.
  # Defaults: COVER_FORM = "A", DEDUCTIBLE_USD = 5e3 → both = 1.00, so for
  # the all-TX synthetic panel these multiplicands are 1 unless caller fills
  # the columns. Kept explicit so the structure matches the era scripts.
  dt[, tank_premium := BASE_RATE * ilf * (1 + sec_load)]
  if (era %in% c("era_2019", "era_2021")) {
    cover_form <- if ("COVER_FORM"     %in% names(dt)) dt$COVER_FORM     else "A"
    ded_usd    <- if ("DEDUCTIBLE_USD" %in% names(dt)) dt$DEDUCTIBLE_USD else 5e3
    cf <- switch(era, era_2019 = coverage_factor_2019(cover_form),
                       era_2021 = coverage_factor_2021(cover_form))
    df <- switch(era, era_2019 = deduct_factor_2019(ded_usd),
                       era_2021 = deduct_factor_2021(ded_usd))
    dt[, tank_premium := tank_premium * cf * df]
  }

  dt[]
}

# ==============================================================================
# 4. Synthetic facility-month panel for every TX tank in service
# ==============================================================================
cat("[2/5] Building synthetic facility-month panel 1999 onwards...\n")

# Calendar of (YEAR, MONTH) pairs in the window, plus eff_date (1st of month)
months_dt <- data.table(eff_date = seq(PANEL_START, PANEL_END, by = "month"))
months_dt[, `:=`(YEAR = year(eff_date), MONTH = month(eff_date))]

# Cartesian join: every tank × every month
# Keep memory bounded — drop tanks with no INSTALL_DATE
tank_static_keep <- tank_static[!is.na(INSTALL_DATE)]
cat(sprintf("  Tanks with INSTALL_DATE: %s\n",
            format(nrow(tank_static_keep), big.mark = ",")))

# Tank × month cartesian. ~3M tanks × ~384 months = too big to materialize
# directly. Build per-year and rbind to control peak memory.
year_seq <- sort(unique(months_dt$YEAR))
cat(sprintf("  Years in panel: %d (%d to %d)\n",
            length(year_seq), min(year_seq), max(year_seq)))

ust_tm_all <- vector("list", length(year_seq))

for (i in seq_along(year_seq)) {
  yr <- year_seq[i]
  yr_months <- months_dt[YEAR == yr]

  # Cross-join tanks alive in any month of this year
  yr_start <- ymd(sprintf("%d-01-01", yr))
  yr_end   <- ymd(sprintf("%d-12-31", yr))
  alive <- tank_static_keep[
    INSTALL_DATE <= yr_end &
    (is.na(CLOSED_DATE) | CLOSED_DATE >= yr_start)
  ]
  if (nrow(alive) == 0L) next

  # CJ on tank-row × month: dummy-key trick (more portable than on=character(0))
  alive_k <- copy(alive)[, .__k := 1L]
  ym_k    <- copy(yr_months[, .(eff_date, YEAR, MONTH)])[, .__k := 1L]
  tm <- merge(alive_k, ym_k, by = ".__k", allow.cartesian = TRUE)
  tm[, .__k := NULL]
  rm(alive_k, ym_k)
  # Drop rows where tank wasn't actually in service that month
  tm <- tm[INSTALL_DATE <= eff_date &
           (is.na(CLOSED_DATE) | CLOSED_DATE > eff_date)]
  tm[, age_years := pmax(0,
       floor(as.numeric(eff_date - INSTALL_DATE) / 365.25))]

  # Default coverage limits — statutory 1m/1m, deductible $5k, form A
  tm[, `:=`(COVER_OCC = 1e6, COVER_AGG = 1e6,
            DEDUCTIBLE_USD = 5e3, COVER_FORM = "A")]

  ust_tm_all[[i]] <- tm
  if (i %% 5L == 0L)
    cat(sprintf("    [%d/%d] year=%d  rows=%s\n",
                i, length(year_seq), yr,
                format(nrow(tm), big.mark = ",")))
}

ust_tm <- rbindlist(ust_tm_all, use.names = TRUE, fill = TRUE)
rm(ust_tm_all); invisible(gc())

cat(sprintf("  Total tank-months: %s\n",
            format(nrow(ust_tm), big.mark = ",")))


# ==============================================================================
# 5. Apply era engines and aggregate
# ==============================================================================
cat("[3/5] Tagging era and applying engines...\n")

ust_tm[, source_era := fcase(
  eff_date <= ERA_BOUNDS$era_2006[2], "era_2006",
  eff_date <= ERA_BOUNDS$era_2014[2], "era_2014",
  eff_date <= ERA_BOUNDS$era_2019[2], "era_2019",
  default                            = "era_2021"
)]

# Apply each era engine to its slice and rebind
ust_tm <- rbindlist(
  lapply(unique(ust_tm$source_era), function(e) {
    cat(sprintf("    %s: %s rows\n", e,
                format(ust_tm[source_era == e, .N], big.mark = ",")))
    apply_engine(ust_tm[source_era == e], e)
  }),
  use.names = TRUE, fill = TRUE
)

# Schedule envelope
ust_tm[, `:=`(
  tank_premium_sched_min = tank_premium * SCHED_MIN_FACTOR,
  tank_premium_sched_max = tank_premium * SCHED_MAX_FACTOR
)]

cat("[4/5] Aggregating to facility-year...\n")

# First: facility-month sum-across-tanks (so base_premium is per facility-month)
fac_month <- ust_tm[, .(
  base_premium_fm  = sum(tank_premium,           na.rm = TRUE),
  sched_min_fm     = sum(tank_premium_sched_min, na.rm = TRUE),
  sched_max_fm     = sum(tank_premium_sched_max, na.rm = TRUE),
  mean_tank_prem_fm = mean(tank_premium,         na.rm = TRUE),
  n_tanks_fm       = uniqueN(UST_ID),
  source_era_fm    = source_era[1]
), by = .(FACILITY_ID, YEAR, MONTH)]

# Then: facility-year mean over months. Mean (not sum or December snapshot)
# matches the user's call — premium is a price per month, we want the
# representative annual price.
fac_year <- fac_month[, .(
  mean_tank_premium = mean(mean_tank_prem_fm, na.rm = TRUE),
  base_premium      = mean(base_premium_fm,   na.rm = TRUE),
  sched_min         = mean(sched_min_fm,      na.rm = TRUE),
  sched_max         = mean(sched_max_fm,      na.rm = TRUE),
  n_tanks_rated     = mean(n_tanks_fm,        na.rm = TRUE),
  source_era        = names(sort(table(source_era_fm), decreasing = TRUE))[1]
), by = .(FACILITY_ID, YEAR)]

setnames(fac_year, "YEAR", "panel_year")

# panel_id matches 02b convention: paste(toupper(trimws(FACILITY_ID)), "TX")
fac_year[, panel_id := paste(toupper(trimws(FACILITY_ID)), "TX", sep = "_")]
fac_year[, is_imputed_pre2006 := as.integer(panel_year <= 2005L)]

# Reorder columns
setcolorder(fac_year, c("panel_id", "panel_year",
                        "mean_tank_premium", "base_premium",
                        "sched_min", "sched_max",
                        "n_tanks_rated", "source_era",
                        "is_imputed_pre2006", "FACILITY_ID"))

cat(sprintf("  Facility-years: %s | unique facilities: %s\n",
            format(nrow(fac_year),                  big.mark = ","),
            format(uniqueN(fac_year$FACILITY_ID),   big.mark = ",")))

# ==============================================================================
# 6. Write output
# ==============================================================================
cat("[5/5] Writing output...\n")
out_path <- file.path(OUT_DIR, "tx_midcont_premium_all_1999_onwards.csv")
fwrite(fac_year, out_path)
cat(sprintf("  Saved: %s\n  rows = %s\n",
            out_path, format(nrow(fac_year), big.mark = ",")))

# Quick sanity summary by era
cat("\nMean facility-year premium by era (USD):\n")
print(fac_year[, .(
  N             = .N,
  mean_base     = round(mean(base_premium,      na.rm = TRUE), 0),
  median_base   = round(median(base_premium,    na.rm = TRUE), 0),
  mean_per_tank = round(mean(mean_tank_premium, na.rm = TRUE), 0)
), by = source_era][order(source_era)])

cat("\nMean facility-year premium by year (1999-onwards selected):\n")
print(fac_year[panel_year %in% c(1999, 2002, 2005, 2008, 2014, 2019, 2021, 2024),
               .(N            = .N,
                 mean_base    = round(mean(base_premium, na.rm = TRUE), 0),
                 median_base  = round(median(base_premium, na.rm = TRUE), 0)),
               by = panel_year][order(panel_year)])

cat("\n04a complete.\n")
