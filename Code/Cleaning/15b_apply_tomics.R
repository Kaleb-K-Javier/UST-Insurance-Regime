###############################################################################
# 15b_apply_tomics.R — TOMICS premium: load raw data, price, write output
# Filing:  SERFF TEXS-131241913  (eff. 2018-01-01)
# Carrier: Tank Owners Members Insurance Company
# Window:  TOMICS in TX FR data 1999-2023 (long contract terms; FR data is truth)
#          source_era ∈ {"2006" (≤2013), "2014" (2014-2018), "2019" (≥2019)}
#
# Sources 15a_engine_tomics.R for all rating functions and module constants.
# Builds tank attributes from raw_pst_ust.csv (DO NOT READ texas_static_tank_details.csv
# — that file is empty/absent on the server; building from raw is the fix).
###############################################################################

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  BEHAVIORAL ASSUMPTIONS — TOMICS filing TEXS-131241913                     ║
# ╠══════════════════════════════════════════════════════════════════════════════╣
# ║ A1  Policy limits: COVER_OCC / COVER_AGG from FR contract panel used       ║
# ║     directly. NA or 0 → 1M occ / 1M agg reference (ILF load = 0.000).    ║
# ║ A2  Schedule rating: ±40% band (p.3). No credit or debit guessed.          ║
# ║     standard_prem = filed schedule-neutral premium.                        ║
# ║     min_prem = standard × 0.60  ;  max_prem = standard × 1.40.           ║
# ║     All three floored at $350 filing minimum policy premium (p.2).         ║
# ║ A3  Deductible: $5k reference tier (p.1) → 0.000 load.                    ║
# ║     Field absent from contract panel [V2]; held at reference throughout.   ║
# ║ A4  Defense sub-limit: $1M reference (p.1) → 0.000 load. Not in data.     ║
# ║ A5  Retro date: 0-1yr reference (p.1) → 0.000 load. Not in data.         ║
# ║ A6  Site-capacity table: "Above Ground Tanks Only" header (p.2) →         ║
# ║     0.000 load for all underground storage tanks.                          ║
# ║ A7  Prior contamination: LUST join deferred to v2 → 0.000 load.           ║
# ║ A8  TOMICS 2nd program (filing 131214138) is OUT OF SCOPE.                 ║
# ║     Single filed card (131241913, eff. 2018-01-01) applied to all TOMICS   ║
# ║     contract-years in the FR data (1999-2023; long contract terms explain   ║
# ║     the full range). TX FR data taken as truth — no year filter applied.    ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(data.table)
library(lubridate)
library(here)

source(here("Code", "Cleaning", "15a_engine_tomics.R"))
# inherits: SCHED_CAP, POLICY_MIN_PREMIUM, ENERGY_ACT_DATE
# inherits: base_rate_tomics, ilf_load_tomics, age_load_tomics,
#           leak_load_tomics, construction_load_tomics, pipe_load_tomics

CARRIER_KEY  <- "TOMICS"
ISSUER_EXACT <- "TANK OWNERS MEMBERS INS CO"   # confirmed in contract panel

era_of_year <- function(y) {
  fcase(y <= 2013L, "2006", y <= 2018L, "2014", default = "2019")
}


###############################################################################
## STEP 1 — Build tank attributes from raw_pst_ust.csv                       ##
##          (12_Rate_MidCont Section A recipe; mirrors 04a Step 1)            ##
###############################################################################

cat("=== STEP 1: load + derive tank attributes from raw ===\n")

raw_path <- here("Data", "Raw", "state_databases", "Texas", "raw_pst_ust.csv")
if (!file.exists(raw_path))
  raw_path <- here("Data", "Raw_do_not_write", "panel_merge_staging", "raw_pst_ust.csv")
if (!file.exists(raw_path))
  stop("raw_pst_ust.csv not found at canonical or staging path — check Data/ on server")

# Needed raw columns — check names before reading
actual_names <- names(fread(raw_path, nrows = 0L))

needed_raw <- c("UST_ID", "INSTALL_DATE",
                "TANK_DOUBLE",
                "TANK_MAT_FRP", "TANK_MAT_COMPOSITE",
                "PIP_DOUBLE", "PIP_MAT_FLEX",
                "DET_C_INTERSTITIAL", "DET_P_INTERSTITIAL",
                "end_date")

# FACILITY_ID: prefer FACILITY_ID_PAD (padded) if available, else FACILITY_ID
id_col <- if ("FACILITY_ID_PAD" %in% actual_names) "FACILITY_ID_PAD" else "FACILITY_ID"
if (!id_col %in% actual_names)
  stop("Neither FACILITY_ID_PAD nor FACILITY_ID found in raw_pst_ust.csv")

missing_flds <- setdiff(needed_raw, actual_names)
if (length(missing_flds) > 0L) {
  cat("  STOP: fields not found in raw_pst_ust.csv:\n")
  cat(paste(" ", missing_flds, collapse = "\n"), "\n")
  stop("Fix the column names above and re-run.")
}

raw_ust <- fread(raw_path, select = c(id_col, needed_raw))

# Merge key: 6-char trimmed FACILITY_ID
raw_ust[, FACILITY_ID := trimws(substr(get(id_col), 1L, 6L))]
if (id_col != "FACILITY_ID") raw_ust[, (id_col) := NULL]

raw_ust[, UST_ID       := toupper(trimws(UST_ID))]
raw_ust[, INSTALL_DATE := ymd(INSTALL_DATE)]
raw_ust[, CLOSED_DATE  := ymd(end_date)]
raw_ust[, end_date     := NULL]

# ── Derive binary tank attributes (all 0/1, no NA) ───────────────────────────
raw_ust[, double_walled     := fifelse(!is.na(TANK_DOUBLE)         & TANK_DOUBLE         == "Y", 1L, 0L)]
raw_ust[, det_interstitial  := fifelse((!is.na(DET_C_INTERSTITIAL) & DET_C_INTERSTITIAL  == "Y") |
                                       (!is.na(DET_P_INTERSTITIAL) & DET_P_INTERSTITIAL  == "Y"), 1L, 0L)]
raw_ust[, is_composite      := fifelse((!is.na(TANK_MAT_COMPOSITE) & TANK_MAT_COMPOSITE  == "Y") |
                                       (!is.na(TANK_MAT_FRP)       & TANK_MAT_FRP        == "Y"), 1L, 0L)]
raw_ust[, pip_double_walled := fifelse(!is.na(PIP_DOUBLE)          & PIP_DOUBLE          == "Y", 1L, 0L)]
raw_ust[, pip_flex          := fifelse(!is.na(PIP_MAT_FLEX)        & PIP_MAT_FLEX        == "Y", 1L, 0L)]
raw_ust[, pip_dw_rigid      := fifelse(pip_double_walled == 1L & pip_flex == 0L, 1L, 0L)]

stopifnot(!anyNA(raw_ust$double_walled))
stopifnot(!anyNA(raw_ust$det_interstitial))
stopifnot(!anyNA(raw_ust$is_composite))
stopifnot(!anyNA(raw_ust$pip_dw_rigid))

# Drop source columns no longer needed
raw_ust[, c("TANK_DOUBLE", "TANK_MAT_FRP", "TANK_MAT_COMPOSITE",
            "PIP_DOUBLE", "PIP_MAT_FLEX",
            "DET_C_INTERSTITIAL", "DET_P_INTERSTITIAL",
            "pip_double_walled", "pip_flex") := NULL]

# ── base_category: New / Replace / Upgrade (2006-01-01 cutoff) ───────────────
# min install per facility across ALL tanks (incl. removed — captures pit history)
fac_min <- raw_ust[!is.na(INSTALL_DATE),
                   .(min_fac_install = min(INSTALL_DATE, na.rm = TRUE)),
                   by = FACILITY_ID]
raw_ust <- fac_min[raw_ust, on = "FACILITY_ID"]

raw_ust[, base_category := fcase(
  INSTALL_DATE <  ENERGY_ACT_DATE,                                    "Upgrade",
  INSTALL_DATE >= ENERGY_ACT_DATE & INSTALL_DATE > min_fac_install,  "Replace",
  INSTALL_DATE >= ENERGY_ACT_DATE & INSTALL_DATE <= min_fac_install, "New",
  default = "Upgrade"   # NA INSTALL_DATE -> Upgrade
)]
raw_ust[, min_fac_install := NULL]

cat(sprintf("  Raw tanks loaded: %d | facilities: %d\n",
            nrow(raw_ust), uniqueN(raw_ust$FACILITY_ID)))
cat("  base_category distribution:\n")
print(raw_ust[, .N, by = base_category][order(-N)])


###############################################################################
## STEP 2 — TOMICS contracts: filter + cartesian tank x contract-month       ##
###############################################################################

cat("=== STEP 2: TOMICS contracts ===\n")

contract_path <- here("Data", "Processed", "texas_fr_contract_month_panel.csv")
if (!file.exists(contract_path))
  stop("texas_fr_contract_month_panel.csv not found at: ", contract_path)

fa_contracts <- fread(contract_path)[
  ISSUER_NAME == ISSUER_EXACT,
  .(FACILITY_ID, YEAR, MONTH, EFF_DATE, COVER_OCC, COVER_AGG)
]
fa_contracts[, EFF_DATE := ymd(EFF_DATE)]
fa_contracts[, FACILITY_ID := trimws(FACILITY_ID)]

stopifnot(nrow(fa_contracts) > 0L)
cat(sprintf("  TOMICS contracts: %d rows | %d facilities | years %d-%d\n",
            nrow(fa_contracts), uniqueN(fa_contracts$FACILITY_ID),
            min(fa_contracts$YEAR), max(fa_contracts$YEAR)))

# Subset tank table to TOMICS facilities only
tomics_facs <- unique(fa_contracts$FACILITY_ID)
tank_sub    <- raw_ust[FACILITY_ID %in% tomics_facs]
cat(sprintf("  Tanks at TOMICS facilities: %d\n", nrow(tank_sub)))

# Cartesian join: tank x contract-month at same facility
ust_tm <- fa_contracts[tank_sub, on = "FACILITY_ID",
                        allow.cartesian = TRUE, nomatch = 0L]

# Keep tanks in-service at contract inception
ust_tm <- ust_tm[
  !is.na(INSTALL_DATE) &
  INSTALL_DATE <= EFF_DATE &
  (is.na(CLOSED_DATE) | CLOSED_DATE > EFF_DATE)
]

# Age at contract inception (whole years, floored at 0)
ust_tm[, age_years := pmax(0L, floor(as.numeric(EFF_DATE - INSTALL_DATE) / 365.25))]

cat(sprintf("  Tank-months (in-service): %d\n", nrow(ust_tm)))
stopifnot(nrow(ust_tm) > 0L)


###############################################################################
## STEP 3 — Price each tank-month; roll up tank -> month(sum) -> year(mean)  ##
###############################################################################

cat("=== STEP 3: price + roll up ===\n")

# Per tank-month: apply all rating factors
ust_tm[, `:=`(
  base_rate         = base_rate_tomics(base_category),
  ilf_load          = ilf_load_tomics(COVER_OCC, COVER_AGG),
  age_load          = age_load_tomics(age_years),
  leak_load         = leak_load_tomics(det_interstitial),
  construction_load = construction_load_tomics(double_walled, is_composite),
  pipe_load         = pipe_load_tomics(pip_dw_rigid)
)]
# Held-reference loads: ded, defense, retro, contam, capacity all 0.000 (see A3-A7)
ust_tm[, sec_load     := ilf_load + age_load + leak_load + construction_load + pipe_load]
ust_tm[, tank_premium := base_rate * (1 + sec_load)]

stopifnot(all(is.finite(ust_tm$tank_premium)))
stopifnot(all(ust_tm$tank_premium >= 0))

cat(sprintf("  tank_premium: mean=$%.0f  min=$%.0f  max=$%.0f\n",
            mean(ust_tm$tank_premium), min(ust_tm$tank_premium), max(ust_tm$tank_premium)))

# Facility-month: sum across tanks
fac_month <- ust_tm[, .(
  fac_std_fm = sum(tank_premium, na.rm = TRUE),
  n_tanks_fm = uniqueN(UST_ID)
), by = .(FACILITY_ID, YEAR, MONTH)]

# Facility-year: mean over months ("mean last", then apply band + floor)
fac_year <- fac_month[, .(
  standard_prem_pre = mean(fac_std_fm,  na.rm = TRUE),
  n_tanks_rated     = as.integer(round(mean(n_tanks_fm)))
), by = .(FACILITY_ID, YEAR)]

# Schedule ±40% band + $350 minimum policy premium floor (all at facility level)
fac_year[, `:=`(
  min_prem      = pmax(standard_prem_pre * (1 - SCHED_CAP), POLICY_MIN_PREMIUM),
  standard_prem = pmax(standard_prem_pre,                    POLICY_MIN_PREMIUM),
  max_prem      = pmax(standard_prem_pre * (1 + SCHED_CAP), POLICY_MIN_PREMIUM)
)]
fac_year[, standard_prem_pre := NULL]

stopifnot(all(fac_year$min_prem      >= POLICY_MIN_PREMIUM))
stopifnot(all(fac_year$standard_prem >= POLICY_MIN_PREMIUM))
stopifnot(all(fac_year$max_prem      >= POLICY_MIN_PREMIUM))
stopifnot(all(fac_year$min_prem      <= fac_year$standard_prem))
stopifnot(all(fac_year$standard_prem <= fac_year$max_prem))


###############################################################################
## STEP 4 — Assemble canonical schema + write                                ##
###############################################################################

cat("=== STEP 4: assemble + write ===\n")

# Canonical panel_id: "<FACILITY_ID>_TX" (matches 02b / 04a convention)
fac_year[, panel_id  := paste(toupper(trimws(FACILITY_ID)), "TX", sep = "_")]
fac_year[, panel_year := as.integer(YEAR)]
fac_year[, source_era := era_of_year(panel_year)]
fac_year[, carrier    := CARRIER_KEY]

setcolorder(fac_year, c("panel_id", "panel_year",
                        "min_prem", "standard_prem", "max_prem",
                        "source_era", "carrier", "n_tanks_rated"))

# Type enforcement
fac_year[, panel_year    := as.integer(panel_year)]
fac_year[, min_prem      := as.double(min_prem)]
fac_year[, standard_prem := as.double(standard_prem)]
fac_year[, max_prem      := as.double(max_prem)]
fac_year[, source_era    := as.character(source_era)]
fac_year[, carrier       := as.character(carrier)]
fac_year[, n_tanks_rated := as.integer(n_tanks_rated)]

# Drop the working FACILITY_ID / YEAR columns (not in the canonical schema)
fac_year[, c("FACILITY_ID", "YEAR") := NULL]

# Write
out_dir  <- here("Data", "Analysis", "rate_engines")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, "TOMICS_facility_year_premium.csv")
fwrite(fac_year, out_path)

# Print summary only (no full table)
cat(sprintf("\n  Output: %s\n", out_path))
cat(sprintf("  Rows: %d | Facilities: %d | Years: %d-%d\n",
            nrow(fac_year),
            uniqueN(fac_year$panel_id),
            min(fac_year$panel_year),
            max(fac_year$panel_year)))

cat("\n  Mean standard_prem by source_era:\n")
print(fac_year[, .(
  N             = .N,
  mean_std_prem = round(mean(standard_prem), 2),
  median_std    = round(median(standard_prem), 2)
), by = source_era][order(source_era)])

cat("\n=== 15b DONE ===\n")
