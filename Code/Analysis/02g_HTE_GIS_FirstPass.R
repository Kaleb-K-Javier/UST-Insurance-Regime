################################################################################
# Code/Analysis/02g_HTE_GIS_FirstPass.R
#
# FIRST-PASS heterogeneous-treatment-effect (HTE) DiD on the reform, using the
# new GIS enrichment layers (Code/GIS/ -> Data/Processed/GIS/) and the panel's
# own fuel classification. This is an EXPLORATORY pass, not a publication build:
#   - unit = tank-year (the established reduced-form unit; same sample as 02c)
#   - outcome = closure_event (annual closure LPM; the clean causal margin)
#   - design = triple-difference: did_term + did_term x Z, facility FE + cell x
#     year FE, wild cluster bootstrap (score) by state (G = 18). This is exactly
#     spec RF-G1/RF-T1/RF-C1 from the expanded-reduced-form note, applied at the
#     tank-year unit and absorbing the time-invariant Z main effect in the
#     facility FE.
#
# Dimensions (each a binary Z, interacted with did_term):
#   geo_rural    RUCA small_town/rural (primary >= 7)        [gis_03]
#   dem_dense    top quartile of tract population            [gis_09]
#                (note's externality logic: per-release damage scales with the
#                number of people nearby; population is the HTE dim that matters)
#   dem_lowinc   <  median tract median HH income             [gis_09]  (secondary)
#   gas_station  facility sells gasoline (>=1 Gasoline-Only tank) vs ALL OTHER
#                (diesel/oil/other only). PRIMARY split-sample DiD — see Step 4.
#   fuel_gasonly facility sells ONLY gasoline (every tank)   (secondary) [panel]
#
# SET ASIDE (2026-06-25): local-market competition (neighbor counts, gis_02) is
#   PARKED — the within-distance self-join counts are not trustworthy yet
#   (suspect coords/dedup -> wrong counts). Re-add comp_thin once gis_02 is rebuilt.
#
# Outputs:
#   Output/Tables/T_HTE_GIS_FirstPass.csv     (one row per dimension x term)
#   Output/Tables/T_HTE_Fuel_SplitSample.csv  (gas-only vs non-gas, separate DiD)
#   logs/02g_HTE_GIS_FirstPass_<TIMESTAMP>.log
#
# Run (dev machine, panel on Z):
#   Sys.setenv(UST_ANALYSIS_DIR = "Z:/ust_ins_move_to_github/Data/Analysis")
#   source("Code/Analysis/02g_HTE_GIS_FirstPass.R")
# Run (server, panel in-repo): just source it.
#
# ----------------------------------------------------------------------------
# IDENTIFYING ASSUMPTION (record; same family across all three units)
# ----------------------------------------------------------------------------
# TANK-YEAR (current main results): parallel trends WITHIN make-model cell.
#   Same wall x fuel x capacity x cohort, same year, one tank in TX vs one in a
#   control state. cell_vintage_year_fe (delta_ct) holds make-model-year fixed;
#   TX-vs-control post-1998 is the only variation. Absent the reform the TX tank
#   and its same-cell control twin close at the same rate. Cleanest statement.
#
# FACILITY-YEAR (A) Direct TWFE + composition shares: parallel trends CONDITIONAL
#   ON COMPOSITION. sum_c lambda_ct * s_fct is the portfolio-weighted delta_ct.
#   Identical-tank twin -> same-MIX twin. Holds the mix via continuous shares
#   (linear adjustment), not an exact within-cell FE.
#
# FACILITY-YEAR (B) Imputation / predicted-rate: SAME assumption as (A). Build
#   Yhat0_ft = sum_c s_fct * delta_hat_ct from UNTREATED (control-state) data
#   only, then tau = Y - Yhat0. Compares each TX facility to a make-model-matched
#   synthetic control-state twin. (A) and (B) share identification; differ only
#   in how the control counterfactual is formed.
#
# NOTE: ~82-87% of facility-years are single-composition (homogeneity / T020
#   diagnostics) -> for those the facility statement reduces to the exact tank
#   twin comparison; shares/imputation only bind in the mixed-portfolio tail.
# ----------------------------------------------------------------------------
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(here)
})

# =============================================================================
# === LOGGING ===
# =============================================================================
dir.create(here::here("logs"), recursive = TRUE, showWarnings = FALSE)
.script_name <- "02g_HTE_GIS_FirstPass"
.log_path <- here::here("logs", paste0(.script_name, "_",
                                       format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
            .log_path, .script_name, R.version.string, getwd()))

# =============================================================================
# === SETUP ===
# =============================================================================
cat("=== SETUP ===\n")
source(here::here("Code", "Helpers", "reduced_form_utils.R"))
rf_use_threads()

ANALYSIS_DIR <- Sys.getenv("UST_ANALYSIS_DIR", here::here("Data", "Analysis"))
PANEL_FILE   <- Sys.getenv("UST_PANEL_FILE",   "matched_tanks_birth_cem.csv")
GIS_DIR      <- here::here("Data", "Processed", "GIS")
# Headline panel = matched_tanks_birth_cem.csv (the birth-CEM matched panel that
# 02b_tank_closure_analysis.R aliases as `matched_tanks`). On the dev laptop it
# lives on Z (UST_ANALYSIS_DIR=Z:/ust_ins_move_to_github/Data/Analysis); the
# locally-built xwalk + GIS lookups stay in the local repo (decoupled below).
XWALK_PATH   <- Sys.getenv("UST_XWALK_PATH",
                           here::here("Data", "Analysis", "_facility_xwalk_coords.csv"))

stopifnot(file.exists(file.path(ANALYSIS_DIR, PANEL_FILE)))
# GIS lookups + coord-xwalk are LOCAL-ONLY (Data/ is gitignored) -> OPTIONAL.
# Present  => add the rural / low-population HTE dims.
# Absent   => (e.g. on the server) skip them; the core results (ATT, event study,
#             gas-station HTE) come from the panel alone.
HAS_GIS <- file.exists(XWALK_PATH) &&
           file.exists(file.path(GIS_DIR, "gis_03_urban_rural.csv")) &&
           file.exists(file.path(GIS_DIR, "gis_09_demographics.csv"))
cat(sprintf("  GIS lookups present: %s\n", HAS_GIS))
dir.create(OUTPUT_TABLES, recursive = TRUE, showWarnings = FALSE)

BOOT_B    <- as.integer(Sys.getenv("UST_BOOT_B", "9999"))   # override for quick passes
BOOT_SEED <- 20260624L

# =============================================================================
# === STEP 1 — LOAD PANEL & BUILD ACTIVE-AT-TREATMENT SAMPLE ===
# =============================================================================
cat("=== STEP 1: LOAD PANEL & BUILD SAMPLE ===\n")

.hdr  <- names(fread(file.path(ANALYSIS_DIR, PANEL_FILE), nrows = 0L))
.cols <- c("tank_panel_id", "panel_id", "state", "texas_treated",
           "install_yr_int", "make_model_noage", "cem_weight",
           "panel_year", "closure_event")
has_fuel <- "mm_fuel" %in% .hdr
if (has_fuel) .cols <- c(.cols, "mm_fuel")
# Headline-spec mandate controls (02b_tank_closure_analysis.R:3143) — additive.
MANDATES  <- c("mandate_release_det", "mandate_spill_overfill", "mandate_integrity")
mand_have <- intersect(MANDATES, .hdr)
.cols     <- c(.cols, mand_have)
if ("first_year_churn" %in% .hdr) .cols <- c(.cols, "first_year_churn")

dt <- fread(file.path(ANALYSIS_DIR, PANEL_FILE), na.strings = c("", "NA"),
            select = .cols)
if ("first_year_churn" %in% names(dt)) {
  dt <- dt[first_year_churn == 0L | is.na(first_year_churn)]
}
data_active <- build_active_at_treatment_sample(dt)
data_active[, post := as.integer(panel_year >= 1999L)]
data_active[, did_term := texas_treated * post]
cat(sprintf("  active sample: %s tank-years | %s facilities | %s states\n",
            fmt_n(nrow(data_active)), fmt_n(uniqueN(data_active$panel_id)),
            uniqueN(data_active$state)))

# =============================================================================
# === STEP 2 — FACILITY-LEVEL COVARIATES (GIS + FUEL) ===
# =============================================================================
cat("=== STEP 2: BUILD FACILITY COVARIATES ===\n")

# Fuel covariates from the PANEL (no GIS): gas_station + fuel_gasonly.
fac_cov <- NULL
if (has_fuel) {
  fac_cov <- data_active[, .(
    gas_station  = as.integer(any(mm_fuel == "Gasoline-Only", na.rm = TRUE)),
    fuel_gasonly = as.integer(all(mm_fuel == "Gasoline-Only", na.rm = TRUE) &
                              any(mm_fuel == "Gasoline-Only", na.rm = TRUE))
  ), by = panel_id]
} else cat("  WARNING: mm_fuel absent -> gas-station / fuel dims skipped.\n")

# GIS covariates (LOCAL-only; skipped on the server): rural + LOW population.
if (HAS_GIS) {
  xw <- fread(XWALK_PATH, select = c("panel_id", "facility_id", "state"),
              colClasses = list(character = c("panel_id", "facility_id", "state")))
  g_ur <- fread(file.path(GIS_DIR, "gis_03_urban_rural.csv"),
                select = c("facility_id", "state", "ruca_primary"),
                colClasses = list(character = c("facility_id", "state")))
  g_dm <- fread(file.path(GIS_DIR, "gis_09_demographics.csv"),
                select = c("facility_id", "state", "tract_pop", "med_hh_income"),
                colClasses = list(character = c("facility_id", "state")))
  fac <- Reduce(function(a, b) merge(a, b, by = c("facility_id", "state"), all.x = TRUE),
                list(unique(xw), g_ur, g_dm))
  fac[, geo_rural  := as.integer(ruca_primary >= 7)]
  # Low population = bottom 25% of tract population (vs the upper three quartiles).
  fac[, dem_lowpop := as.integer(tract_pop <= quantile(tract_pop, 0.25, na.rm = TRUE))]
  fac[, dem_lowinc := as.integer(med_hh_income < median(med_hh_income, na.rm = TRUE))]
  gis_cov <- fac[, .(panel_id, geo_rural, dem_lowpop, dem_lowinc)]
  fac_cov <- if (is.null(fac_cov)) gis_cov else merge(fac_cov, gis_cov, by = "panel_id", all = TRUE)
} else cat("  GIS lookups absent -> rural / low-population HTE skipped.\n")

if (!is.null(fac_cov)) data_active <- merge(data_active, fac_cov, by = "panel_id", all.x = TRUE)
for (z in intersect(c("gas_station","fuel_gasonly","geo_rural","dem_lowpop","dem_lowinc"),
                    names(data_active))) {
  cat(sprintf("  %-13s matched: %.1f%% | share Z=1: %.3f\n",
              z, 100 * mean(!is.na(data_active[[z]])), mean(data_active[[z]], na.rm = TRUE)))
}

# =============================================================================
# === STEP 3 — TRIPLE-DIFFERENCE HTE (one Z at a time) ===
# =============================================================================
cat("=== STEP 3: TRIPLE-DIFFERENCE HTE ===\n")
# Spec: closure_event ~ did_term + did_Z | panel_id + cell_vintage_year_fe
# did_Z = did_term * Z is the triple-diff term; Z main effect absorbed by
# facility FE; cell x year FE = the tank-level analogue of delta_ct.
# INFERENCE: analytic cluster-robust by state (fast). WCB is the proper one-
# treated-cluster inference -> re-enable run_wcb_ols for the final tables.
RHS_MAND <- if (length(mand_have)) paste("+", paste(mand_have, collapse = " + ")) else ""
ZLAB <- c(gas_station = "Gas station", geo_rural = "Rural", dem_lowpop = "Low population",
          dem_lowinc = "Low income", fuel_gasonly = "Pure gasoline")
Z_DIMS <- intersect(c("gas_station", "geo_rural", "dem_lowpop", "dem_lowinc",
                      "fuel_gasonly"), names(data_active))
hte_rows <- list(); m_list <- list()
for (z in Z_DIMS) {
  d <- data_active[!is.na(get(z))]
  d[, did_Z := did_term * get(z)]
  m  <- feols(as.formula(sprintf(
    "closure_event ~ did_term + did_Z %s | panel_id + cell_vintage_year_fe", RHS_MAND)),
    data = d, cluster = ~state)
  m_list[[z]] <- m
  ct <- coeftable(m)
  for (prm in c("did_term", "did_Z")) {
    est <- ct[prm, "Estimate"]; se <- ct[prm, "Std. Error"]
    hte_rows[[paste(z, prm)]] <- data.table(
      dimension = z, term = prm, n_obs = m$nobs,
      beta = est, se = se, ci_lo = est - 1.96 * se, ci_hi = est + 1.96 * se,
      p = ct[prm, "Pr(>|t|)"])
  }
  cat(sprintf("  %-13s did=%+.4f  didxZ=%+.4f (p %.3f)\n", z,
              ct["did_term", "Estimate"], ct["did_Z", "Estimate"], ct["did_Z", "Pr(>|t|)"]))
}
fwrite(rbindlist(hte_rows), file.path(OUTPUT_TABLES, "T_HTE_GIS_FirstPass.csv"))
pub_etable(m_list, file.path(OUTPUT_TABLES, "T_HTE_Tank_Pub.tex"),
  headers = unname(ZLAB[names(m_list)]),
  title   = "Heterogeneous closure response to the 1998 Texas reform (tank-year)",
  notes   = "Dependent variable: annual tank closure (linear probability). Each column reports the average effect (Reform $\\times$ Post) and its interaction with the subgroup indicator. Facility and cell-by-year fixed effects throughout; RCRA mandate controls included. Standard errors clustered by state (18 clusters).")
cat("  -> Output/Tables/T_HTE_Tank_Pub.tex + T_HTE_GIS_FirstPass.csv\n")

# NOTE (2026-06-25): split-sample DiD REMOVED. Splitting the sample re-estimates
# panel_id + cell_vintage_year_fe SEPARATELY within each group, so the two halves
# are demeaned against different baselines and the cell x year support differs by
# group -> not comparable. The interaction (did_Z, common FEs) is the correct
# differential. The gas-station effect is read off the gas_station row:
#   non-gas ATT = did_term ; gas ATT = did_term + did_Z ; differential = did_Z.

cat("\n=== 02g COMPLETE ===\n")
