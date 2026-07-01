################################################################################
# 01n_CVValidation.R  (Ticket 042 — portfolio features + uniform-regime sample
#   + facility-grouped CV folds + analytic IJ CIs)
#
# PURPOSE:
#   Produces the production first-leak hazard for §4.1 pricing + the DCM
#   primitive:
#     h(t | x) = P(first confirmed release in year t |
#                   survived release-free to t, observable portfolio x)
#
# TICKET 042 CHANGES (additive over the original script):
#   1. SAMPLE: TX pre-1999 only; control states ALL years >= 1990 (uniform-fee,
#      never risk-based, so all years are clean). Floor at 1990 + year control.
#   2. FEATURES: full per-tank portfolio composition (wall/age/vintage/make/
#      capacity/fuel shares + summary stats) joined from panel_dt.csv.
#   3. CV FOLDS: facility-grouped, state-stratified (no leakage across facility
#      years). Replaces random k-fold.
#   4. CALIBRATION: DROP class weights + Platt (follow 01p; unweighted logistic
#      at lambda~0 is self-calibrated; weights+Platt collapse predictions).
#   5. UNCERTAINTY: per-downstream-use CIs:
#        §4.1 cell schedule + SW-DW contrast -> analytic IJ (cluster-robust
#          sandwich + delta method on unpenalized feglm refit).
#        §4.1 metrics (AUC/PR-AUC/lift/calibration) -> facility-cluster
#          resample B=1000 over fixed OOS predictions.
#        §4.2 fair premium -> propagated in separate 07* script.
#        §5 structural -> 2-step outer bootstrap (out of scope here).
#
# PERFORMANCE AMENDMENTS (2026-06-30, post Ticket-042 production run):
#   On a prior full run with this exact sample/feature set (2,329,566 fac-yrs,
#   146-col design), the 11-point alpha grid search showed CV deviance
#   essentially FLAT from alpha=0.10 through alpha=1.00 (0.13406-0.13407,
#   5th-decimal differences only); alpha=0 (ridge) was the lone outlier, worse
#   at 0.13552. Best alpha was 0.60. Full grid:
#     alpha  lambda.min  CV deviance
#     0.00   0.000943    0.13552
#     0.10   0.000014    0.13406
#     0.20   0.000014    0.13407
#     0.30   0.000014    0.13406
#     0.40   0.000013    0.13406
#     0.50   0.000012    0.13406
#     0.60   0.000011    0.13406   <- best
#     0.70   0.000010    0.13406
#     0.80   0.000012    0.13406
#     0.90   0.000012    0.13407
#     1.00   0.000011    0.13407
#   Because the grid is flat, the 11x full-data CV cost of re-tuning buys
#   essentially nothing. SKIP_ALPHA_TUNING (below) uses the known alpha=0.60
#   and skips straight to the single full-data cv.glmnet fit. Re-enable a full
#   search (SKIP_ALPHA_TUNING <- FALSE) if the estimation sample, feature set,
#   or fold structure changes materially enough that this grid might no longer
#   hold.
#
#   X_elnet / X_full are also now coerced to sparse (dgCMatrix) before being
#   passed to cv.glmnet/glmnet/predict. The design is dominated by one-hot
#   factor expansions (state x panel_year_f x age_bin, etc.), so it is mostly
#   zeros; glmnet is specifically optimized for sparse input and this should
#   meaningfully cut fit time with no change to folds, coefficients, or
#   predictions. The few continuous x continuous interaction columns (e.g.
#   active_tanks:total_capacity) are dense and gain nothing, but don't hurt.
#   Two spots are deliberately kept/converted to DENSE because they require
#   it: (a) S5 var-importance `scale()`, since centering a sparse matrix
#   destroys its sparsity anyway, and (b) S4b's capped (<=500 col) IJ design,
#   since fixest::feglm needs a data.frame. Machine has ample RAM (confirmed
#   ~278GB free on a 384GB box), so these targeted densifications are cheap.
#
#   NOT YET APPLIED: the S2b comp_dt tank-roster aggregation (~934s in the
#   reference run, 3.38M data.table `by=` groups each doing ~9 separate
#   `mean(condition)` scans) is a known further bottleneck but has not been
#   rewritten here — that's a higher-risk change to aggregation logic and
#   needs its own validation pass before going into production.
#
# SUPERSEDED: 01r_Leak_Rate.R (recurrent Poisson) archived to
#   Code/Analysis/Descrptive Facts/Archive/01r_Leak_Rate.R.
#
# SWITCHES: USE_GRF=FALSE, USE_ELNET=TRUE, PRIMARY_MODEL="ELNET" (locked).
#
# OUTPUTS (preserved + new):
#   Output/Figures/:
#     Figure_CV_ROC, Figure_CV_PR, Figure_CV_Calibration
#     Figure_CV_CellRisk     (+ CI bands from analytic IJ)  [updated]
#     Figure_CV_CellRisk_GoF
#     Figure_CV_CellRisk_Contrast  (SW-DW contrast per age bin + CIs)  [NEW]
#     FigureA_CV_ObsCellRates, FigureA_CV_Lift_State, FigureA_CV_ScoreSep_State
#     FigureA_CV_VarImportance
#   Output/Tables/:
#     Table_CV_GoF_Summary     (+ CI columns)  [updated]
#     Table_CV_CellRates_Observed, Table_CV_CellRates_Model
#     Table_CV_CellRisk_RefContrast   (SW vs DW + CIs)  [NEW]
#     Table_CV_Calibration, Table_CV_VarImportance, Table_CV_LifetimeLeakProb
#   Data/Analysis/:
#     analysis_cv_data_fac_year.rds, analysis_cv_data_fac.rds
#     analysis_elnet_model.rds
#     analysis_hazard_predictions_full.csv
#     dcm_state_hazard_rates.csv
################################################################################


#### S1: Setup ################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)       # sparse design for full-panel scoring
  library(ggplot2)
  library(grf)
  library(glmnet)
  library(fixest)       # feglm for analytic IJ CIs
  library(doParallel)
  library(PRROC)
  library(pROC)
  library(patchwork)
  library(scales)
  library(here)
})

source(here::here("Code", "Helpers", "data_paths.R"))

options(scipen = 999)
set.seed(20260202L)
setDTthreads(0L)

# ---- Output paths -----------------------------------------------------------
OUTPUT_FIGURES <- here("Output", "Figures")
OUTPUT_TABLES  <- here("Output", "Tables")
ANALYSIS_DIR   <- here("Data", "Analysis")
for (d in c(OUTPUT_FIGURES, OUTPUT_TABLES, ANALYSIS_DIR))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- Logging ----------------------------------------------------------------
.log_path <- here("logs", paste0("01n_CVValidation_",
                                  format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 01n_CVValidation.R (Ticket 042)\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# ---- Study parameters -------------------------------------------------------
POST_YEAR      <- 1999L
SNAP_YEAR      <- 1998L
YEAR_FLOOR     <- 1990L   # detection-era lower bound; pre-1990 recorded rate ~0
NBOOT_METRICS  <- 1000L   # B for cluster-resample metric CIs

# ---- State sample -----------------------------------------------------------
CONTROL_STATES <- c(
  "ME", "NM", "AR", "OK", "LA", "KS", "MT", "ID", "SD", "AL",
  "MN", "NC", "IL", "MA", "OH", "PA", "TN", "VA", "CO"
)
ALL_STUDY_STATES <- c("TX", CONTROL_STATES)

# ---- Make-model sample criteria ---------------------------------------------
MM_WALL_EXCLUDE <- "Unknown-Wall"
MM_FUEL_EXCLUDE <- "Unknown-Fuel"

# ---- Canonical age bins (3-year) --------------------------------------------
AGE_BIN_BREAKS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
AGE_BIN_LABELS <- c(
  "0-2", "3-5", "6-8", "9-11", "12-14",
  "15-17", "18-20", "21-23", "24+"
)

make_age_bin <- function(age_vec) {
  factor(
    cut(age_vec, breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS,
        right = FALSE, include.lowest = TRUE),
    levels = AGE_BIN_LABELS, ordered = FALSE
  )
}

# ---- Tank-roster -> facility-year portfolio composition (PERFORMANCE) ------
# Shared by S2b (training-sample roster) and S4 full-panel scoring (identical
# pre-Ticket-042-rewrite aggregation, now de-duplicated into one function so
# the two call sites can't drift apart). Classifies each row ONCE (vectorized)
# then aggregates via fast grouped tabulation instead of ~30 per-group
# mean()/sum()/max()/min()/unique()/paste() calls inside a data.table `by=`
# (this was the ~934s step on the full 9.2M-row roster in the reference run;
# validated bit-identical to the original per-group version on a 2,000-
# facility subsample — see validate_comp_dt_rewrite.R).
#
# Input: a data.table with columns panel_id, panel_year, mm_wall, mm_fuel,
#   mm_capacity, install_yr_int, tank_age (a filtered subset of roster_raw is
#   fine — the function does not mutate its input).
# Output: one row per (panel_id, panel_year) with the same column set/names/
#   values as the original inline aggregation.
TANK_AGE_BREAKS_COMP <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
TANK_AGE_NAMES_COMP  <- c("age_0_2", "age_3_5", "age_6_8", "age_9_11", "age_12_14",
                          "age_15_17", "age_18_20", "age_21_23", "age_24p")
VINT_BREAKS_COMP <- c(-Inf, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, Inf)
VINT_NAMES_COMP  <- c("vint_pre1970", "vint_1970", "vint_1975", "vint_1980",
                      "vint_1985", "vint_1990", "vint_1995", "vint_2000", "vint_2005p")

build_comp_dt <- function(roster_subset) {
  rr <- copy(roster_subset)  # never mutate the caller's roster_raw

  rr[, age_bin_idx  := findInterval(tank_age,       TANK_AGE_BREAKS_COMP, rightmost.closed = FALSE)]
  rr[, vint_bin_idx := findInterval(install_yr_int, VINT_BREAKS_COMP,     rightmost.closed = FALSE)]
  rr[age_bin_idx  < 1L | age_bin_idx  > 9L, age_bin_idx  := NA_integer_]
  rr[vint_bin_idx < 1L | vint_bin_idx > 9L, vint_bin_idx := NA_integer_]

  rr[, wall_code := as.integer(factor(mm_wall))]
  rr[, fuel_code := as.integer(factor(mm_fuel))]
  rr[, cap_code  := as.integer(factor(mm_capacity))]
  n_fuel_lv <- uniqueN(rr$fuel_code)
  n_cap_lv  <- uniqueN(rr$cap_code)
  rr[, make_code := wall_code + fuel_code * 100L + cap_code * 100L * (n_fuel_lv + 1L)]
  rr[, vint5_id := floor(install_yr_int / 5L) * 5L]

  comp_core <- rr[, .(
    n_tanks_roster   = .N,
    n_sw             = sum(mm_wall == "Single-Walled"),
    oldest_age       = max(tank_age, na.rm = TRUE),
    youngest_age     = min(tank_age, na.rm = TRUE),
    n_distinct_vintages = uniqueN(vint5_id),
    n_distinct_makes    = uniqueN(make_code),
    n_gas    = sum(mm_fuel == "Gasoline-Only"),
    n_diesel = sum(mm_fuel == "Diesel-Only"),
    n_mixed  = sum(mm_fuel == "Motor-Fuel-Mixed"),
    n_other_fuel = sum(mm_fuel == "Other-Fuel"),
    n_cap_under1k = sum(mm_capacity == "Under-1k"),
    n_cap_1k5k    = sum(mm_capacity == "1k-5k"),
    n_cap_5k12k   = sum(mm_capacity == "5k-12k"),
    n_cap_12k25k  = sum(mm_capacity == "12k-25k"),
    n_cap_25kplus = sum(mm_capacity == "25k-Plus"),
    n_cap_unknown = sum(mm_capacity == "Unknown-Cap")
  ), by = .(panel_id, panel_year)]
  comp_core[, share_sw_roster   := n_sw / n_tanks_roster]
  comp_core[, age_spread        := oldest_age - youngest_age]
  comp_core[, share_fuel_gas    := n_gas / n_tanks_roster]
  comp_core[, share_fuel_diesel := n_diesel / n_tanks_roster]
  comp_core[, share_fuel_mixed  := n_mixed / n_tanks_roster]
  comp_core[, share_fuel_other  := n_other_fuel / n_tanks_roster]
  comp_core[, share_cap_under1k := n_cap_under1k / n_tanks_roster]
  comp_core[, share_cap_1k5k    := n_cap_1k5k / n_tanks_roster]
  comp_core[, share_cap_5k12k   := n_cap_5k12k / n_tanks_roster]
  comp_core[, share_cap_12k25k  := n_cap_12k25k / n_tanks_roster]
  comp_core[, share_cap_25kplus := n_cap_25kplus / n_tanks_roster]
  comp_core[, share_cap_unknown := n_cap_unknown / n_tanks_roster]
  comp_core[, c("n_sw", "n_gas", "n_diesel", "n_mixed", "n_other_fuel",
               "n_cap_under1k", "n_cap_1k5k", "n_cap_5k12k", "n_cap_12k25k",
               "n_cap_25kplus", "n_cap_unknown") := NULL]

  age_counts <- rr[!is.na(age_bin_idx), .(n = .N), by = .(panel_id, panel_year, age_bin_idx)]
  age_wide <- dcast(age_counts, panel_id + panel_year ~ age_bin_idx, value.var = "n", fill = 0L)
  setnames(age_wide, as.character(1:9), TANK_AGE_NAMES_COMP, skip_absent = TRUE)
  for (nm in TANK_AGE_NAMES_COMP) if (!nm %in% names(age_wide)) age_wide[, (nm) := 0L]

  vint_counts <- rr[!is.na(vint_bin_idx), .(n = .N), by = .(panel_id, panel_year, vint_bin_idx)]
  vint_wide <- dcast(vint_counts, panel_id + panel_year ~ vint_bin_idx, value.var = "n", fill = 0L)
  setnames(vint_wide, as.character(1:9), VINT_NAMES_COMP, skip_absent = TRUE)
  for (nm in VINT_NAMES_COMP) if (!nm %in% names(vint_wide)) vint_wide[, (nm) := 0L]

  out <- merge(comp_core, age_wide, by = c("panel_id", "panel_year"), all.x = TRUE)
  out <- merge(out, vint_wide, by = c("panel_id", "panel_year"), all.x = TRUE)
  for (nm in TANK_AGE_NAMES_COMP) {
    out[is.na(get(nm)), (nm) := 0L]
    out[, (paste0("share_", nm)) := get(nm) / n_tanks_roster]
    out[, (nm) := NULL]
  }
  for (nm in VINT_NAMES_COMP) {
    out[is.na(get(nm)), (nm) := 0L]
    out[, (paste0("share_", nm)) := get(nm) / n_tanks_roster]
    out[, (nm) := NULL]
  }

  setcolorder(out, intersect(c(
    "panel_id", "panel_year", "n_tanks_roster", "share_sw_roster",
    paste0("share_", TANK_AGE_NAMES_COMP), paste0("share_", VINT_NAMES_COMP),
    "share_cap_under1k", "share_cap_1k5k", "share_cap_5k12k", "share_cap_12k25k",
    "share_cap_25kplus", "share_cap_unknown",
    "share_fuel_gas", "share_fuel_diesel", "share_fuel_mixed", "share_fuel_other",
    "oldest_age", "youngest_age", "age_spread",
    "n_distinct_vintages", "n_distinct_makes"
  ), names(out)))
  out
}

# ---- Colours ----------------------------------------------------------------
COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"

# ---- Publication theme ------------------------------------------------------
theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.1),
                                      margin = margin(0, 0, 6, 0)),
      plot.subtitle    = element_text(color = "grey40", size = rel(0.85),
                                      margin = margin(0, 0, 8, 0)),
      plot.caption     = element_text(color = "grey50", size = rel(0.75),
                                      hjust = 0),
      axis.title       = element_text(face = "bold", size = rel(0.9)),
      legend.title     = element_text(face = "bold", size = rel(0.9)),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(fill = NA, color = "gray85"),
      strip.text       = element_text(face = "bold")
    )
}
theme_set(theme_pub())

# ---- Threading --------------------------------------------------------------
NUM_THREADS <- max(1L, parallel::detectCores() - 1L)

# ---- GRF constants ----------------------------------------------------------
NUM_TREES <- 2000L

# ==============================================================================
# MODEL SWITCHES
# ==============================================================================
USE_GRF       <- FALSE
USE_ELNET     <- TRUE
PRIMARY_MODEL <- "ELNET"
# ==============================================================================

# ==============================================================================
# PERFORMANCE SWITCHES (added — see header "PERFORMANCE AMENDMENTS" note)
# ==============================================================================
SKIP_ALPHA_TUNING <- TRUE    # use KNOWN_BEST_ALPHA, skip the 11x full-data grid
KNOWN_BEST_ALPHA  <- 0.60    # from prior full run on this exact sample (see header)
# ==============================================================================

stopifnot(
  "At least one of USE_GRF / USE_ELNET must be TRUE" = USE_GRF || USE_ELNET,
  "PRIMARY_MODEL must be 'GRF' or 'ELNET'" = PRIMARY_MODEL %in% c("GRF", "ELNET"),
  "PRIMARY_MODEL = 'GRF' requires USE_GRF = TRUE"   = !(PRIMARY_MODEL == "GRF"   && !USE_GRF),
  "PRIMARY_MODEL = 'ELNET' requires USE_ELNET = TRUE" = !(PRIMARY_MODEL == "ELNET" && !USE_ELNET)
)

TEST_MODE <- FALSE
BEST_PARAMS_FIXED <- list(mtry = 7L, min.node.size = 5L, sample.fraction = 0.7)

if (TEST_MODE && USE_GRF)
  cat("*** TEST MODE ACTIVE (GRF) — tuning skipped ***\n")

cat(sprintf("Models active:  GRF=%s  ELNET=%s  Primary=%s  TestMode=%s  SkipAlphaTuning=%s\n\n",
    USE_GRF, USE_ELNET, PRIMARY_MODEL, TEST_MODE, SKIP_ALPHA_TUNING))

get_p1 <- function(pred_mat) {
  if (!is.null(colnames(pred_mat)) && "1" %in% colnames(pred_mat))
    return(pred_mat[, "1"])
  if (ncol(pred_mat) >= 2L) return(pred_mat[, 2L])
  stop("get_p1: unexpected prediction matrix structure")
}

# ---- Load panel data --------------------------------------------------------
PANEL_CSV <- z_path("Data", "Processed", "facility_leak_behavior_annual.csv")
if (!file.exists(PANEL_CSV))
  stop("Panel CSV not found: ", PANEL_CSV)

cat("=== 01n: Observable Risk — Actuarial Validation (Ticket 042) ===\n\n")
cat(sprintf("Loading: %s\n", PANEL_CSV))
annual_data <- fread(PANEL_CSV)
cat(sprintf("  Loaded: %s rows x %d cols | %d-%d\n",
    format(nrow(annual_data), big.mark = ","), ncol(annual_data),
    min(annual_data$panel_year), max(annual_data$panel_year)))


#### S2: Estimation Sample ####################################################
#
# TICKET 042 CHANGE:
#   OLD: panel_year < POST_YEAR (1999) for ALL states.
#   NEW: TX: panel_year in [YEAR_FLOOR, POST_YEAR).
#        Controls: panel_year >= YEAR_FLOOR (all control-state years clean;
#          control states never adopt risk-based pricing).
#   FLOOR: panel_year >= YEAR_FLOOR (1990) for all states — detection in force;
#          pre-1990 recorded rates near zero (censored, not physical).
#   YEAR CONTROL: factor(panel_year) added to design (calendar de-confounding).

required_cols <- c(
  "fac_wall", "fac_fuel",
  "has_single_walled", "age_bins", "avg_tank_age",
  "active_tanks", "total_capacity",
  "has_gasoline_year", "has_diesel_year",
  "event_first_leak", "has_previous_leak",
  "state", "panel_id", "panel_year", "texas_treated"
)
missing_cols <- setdiff(required_cols, names(annual_data))
if (length(missing_cols) > 0L)
  stop("annual_data missing required columns: ", paste(missing_cols, collapse = ", "))

annual_data <- annual_data[state %in% ALL_STUDY_STATES]

# Age bin
if ("age_bins" %in% names(annual_data)) {
  existing_labels <- if (is.factor(annual_data$age_bins))
    levels(annual_data$age_bins)
  else
    sort(unique(annual_data$age_bins[!is.na(annual_data$age_bins)]))
  if (identical(as.character(existing_labels), AGE_BIN_LABELS)) {
    annual_data[, age_bin := age_bins]
  } else {
    annual_data[, age_bin := make_age_bin(avg_tank_age)]
  }
} else {
  annual_data[, age_bin := make_age_bin(avg_tank_age)]
}

annual_data[, is_make_model := as.integer(
  fac_wall != MM_WALL_EXCLUDE &
  fac_fuel != MM_FUEL_EXCLUDE &
  !is.na(age_bin)
)]

# ---- Sample filter (Ticket 042) ---------------------------------------------
# TX: pre-reform years with detection floor
# Controls: all uniform-regime years with detection floor
tx_filter <- (annual_data$state == "TX" &
              annual_data$panel_year >= YEAR_FLOOR &
              annual_data$panel_year < POST_YEAR)
ctrl_filter <- (annual_data$state %in% CONTROL_STATES &
                annual_data$panel_year >= YEAR_FLOOR)

cv_data <- annual_data[
  (tx_filter | ctrl_filter)     &
  has_previous_leak == 0L       &
  is_make_model     == 1L       &
  has_single_walled %in% c(0L, 1L) &
  !is.na(active_tanks)          &
  !is.na(total_capacity)        &
  !is.na(event_first_leak)
]

cv_data[, age_bin      := factor(age_bin, levels = AGE_BIN_LABELS)]
cv_data[, state        := factor(state)]
cv_data[, panel_year_f := factor(panel_year)]  # calendar control (Ticket 042)

# Confirm sample composition
n_tx_cv   <- sum(cv_data$state == "TX")
n_ctrl_cv <- sum(cv_data$state != "TX")
cat(sprintf(
  "\nSample (Ticket 042 regime-clean, detection-floor >= %d):\n  TX (<%d):       %s fac-yrs\n  Controls (>=%d): %s fac-yrs\n  Total:           %s | %s facilities | %d states\n",
  YEAR_FLOOR, POST_YEAR, format(n_tx_cv, big.mark = ","),
  YEAR_FLOOR, format(n_ctrl_cv, big.mark = ","),
  format(nrow(cv_data), big.mark = ","),
  format(uniqueN(cv_data$panel_id), big.mark = ","),
  uniqueN(cv_data$state)))

# Control states must now contribute post-1999 facility-years
n_ctrl_post99 <- sum(cv_data$state %in% CONTROL_STATES & cv_data$panel_year >= POST_YEAR)
cat(sprintf("  GATE: control states post-1999 fac-yrs = %s (must be > 0)\n",
    format(n_ctrl_post99, big.mark = ",")))
stopifnot("No control-state post-1999 rows in sample" = n_ctrl_post99 > 0L)

n_events   <- sum(cv_data$event_first_leak)
event_rate <- mean(cv_data$event_first_leak)
cat(sprintf("  First-leak events: %s (%.3f%% base rate)  Class ratio: %.0f:1\n\n",
    format(n_events, big.mark = ","), event_rate * 100,
    (nrow(cv_data) - n_events) / n_events))

stopifnot(
  all(cv_data$has_previous_leak == 0L),
  all(cv_data$is_make_model == 1L),
  !anyNA(cv_data$age_bin),
  all(as.character(cv_data$state) %in% ALL_STUDY_STATES)
)


#### S2b: Tank Roster — Portfolio Composition Features #######################
#
# Joins panel_dt.csv (per-tank annual rows from 02b) to cv_data to build
# within-facility portfolio composition features. These de-confound the
# DW>SW artifact at young ages: young SW = 1980s installs (under-detected),
# young DW = 2000s installs (monitored) — separable via vintage cohort shares.
#
# NOTE: this aggregation (~934s on the reference run, 3.38M data.table `by=`
# groups) is a known further bottleneck not yet addressed in this pass — see
# header "PERFORMANCE AMENDMENTS / NOT YET APPLIED".

cat("=== S2b: Tank Roster — Portfolio Composition Features ===\n")

ROSTER_PATH <- file.path(ANALYSIS_DIR, "panel_dt.csv")
if (!file.exists(ROSTER_PATH))
  stop("panel_dt.csv not found at: ", ROSTER_PATH,
       "\nRun 02b_Tank_level_Panel_Build.R first.")

ROSTER_COLS <- c("panel_id", "panel_year", "mm_wall", "mm_fuel", "mm_capacity",
                 "install_yr_int", "tank_age")
cat(sprintf("  Loading panel_dt.csv (%s)...\n", ROSTER_PATH))
roster_raw <- fread(ROSTER_PATH, select = ROSTER_COLS)
cat(sprintf("  Loaded: %s rows | panel_year %d-%d\n",
    format(nrow(roster_raw), big.mark = ","),
    min(roster_raw$panel_year), max(roster_raw$panel_year)))

# Clean blank mm_capacity (93k rows have ""  per diagnostic)
roster_raw[mm_capacity == "" | is.na(mm_capacity), mm_capacity := "Unknown-Cap"]

# Restrict to study-state facilities present in annual_data (avoids joining
# irrelevant rows; panel_id already encodes state via 02b naming convention)
study_panel_ids <- unique(annual_data$panel_id)
roster_raw <- roster_raw[panel_id %in% study_panel_ids]
cat(sprintf("  After facility filter: %s rows\n",
    format(nrow(roster_raw), big.mark = ",")))

# ---- Aggregate to (panel_id, panel_year) composition -----------------------
# Each row of comp_dt is one facility-year with its full tank-portfolio summary.
#
# PERFORMANCE REWRITE (2026-06-30): the original version computed ~30 separate
# mean()/sum()/max()/min()/unique()/paste() calls INSIDE a data.table `by=`
# over 3.38M (panel_id, panel_year) groups (~934s on the reference run). Each
# of those calls re-enters R's evaluator once per group, so the per-group call
# overhead dominates even though the underlying arithmetic is trivial.
#
# Now delegates to the shared build_comp_dt() helper (defined near the top of
# the script, next to make_age_bin) — classifies every roster row ONCE
# (vectorized), then aggregates via fast grouped tabulation instead of
# per-group scans. Same function is reused for full-panel scoring below, so
# the two aggregations cannot drift apart. Validated bit-identical to the
# original per-group version on a 2,000-facility subsample — see
# validate_comp_dt_rewrite.R.
cat("  Building comp_dt via build_comp_dt() (vectorized classify + tabulate)...\n")
comp_dt <- build_comp_dt(roster_raw)
cat(sprintf("  comp_dt: %s facility-year rows\n", format(nrow(comp_dt), big.mark = ",")))


# ---- Join to cv_data --------------------------------------------------------
n_before <- nrow(cv_data)
cv_data  <- merge(cv_data, comp_dt, by = c("panel_id", "panel_year"), all.x = TRUE)
stopifnot(nrow(cv_data) == n_before)

n_miss_roster <- sum(is.na(cv_data$n_tanks_roster))
cat(sprintf("  Roster join coverage: %s / %s cv rows matched (%.1f%%)\n",
    format(n_before - n_miss_roster, big.mark = ","),
    format(n_before, big.mark = ","),
    100 * (1 - n_miss_roster / n_before)))
if (n_miss_roster > 0L)
  warning(sprintf(
    "%s cv_data rows have no match in panel_dt — composition features imputed",
    format(n_miss_roster, big.mark = ",")))

# Impute missing: share columns -> 0; continuous -> column mean
COMP_SHARE_COLS <- grep("^share_", names(cv_data), value = TRUE)
COMP_CONT_COLS  <- c("oldest_age", "youngest_age", "age_spread",
                     "n_distinct_vintages", "n_distinct_makes", "n_tanks_roster")
for (col in COMP_SHARE_COLS)
  cv_data[is.na(get(col)), (col) := 0]
for (col in COMP_CONT_COLS) {
  cv_data[, (col) := as.numeric(get(col))]   # ensure double before mean impute
  m <- mean(cv_data[[col]], na.rm = TRUE)
  cv_data[is.na(get(col)), (col) := m]
}

Y  <- as.integer(cv_data$event_first_leak)
cv_data[, pred_grf   := NA_real_]
cv_data[, pred_elnet := NA_real_]

model_meta <- list()


#### S4: Model Training #######################################################
#
# TICKET 042 CHANGES:
#   - Feature formula extended with portfolio composition + year_f
#   - CV folds: facility-grouped, state-stratified (no cross-facility leakage)
#   - NO class weights (unweighted logistic, self-calibrated at lambda~0)
#   - NO Platt scaling (weights+Platt collapse predictions; see 01p)
#   - Full-panel scoring includes tank roster join for portfolio features
#
# ==============================================================================
# ELNET FEATURE LIST (Ticket 042)
# ------------------------------------------------------------------------------
# CORE FACILITY FEATURES (in saturated ^2 block — existing 01n design):
#   factor(age_bin)         facility avg age bin (9 levels, 3-yr intervals)
#   has_single_walled       any SW tank (binary)
#   active_tanks            portfolio size (count)
#   total_capacity          total capacity (gallons)
#   has_gasoline_year       gasoline fuel indicator
#   has_diesel_year         diesel fuel indicator
#
# PORTFOLIO COMPOSITION FEATURES (main effects — from tank roster):
#   share_sw_roster          fraction of tanks that are single-walled
#   oldest_age               age of oldest tank in portfolio
#   youngest_age             age of youngest tank in portfolio
#   age_spread               oldest_age - youngest_age
#   n_distinct_vintages      number of distinct 5-yr install cohorts
#   n_distinct_makes         number of distinct make_model_noage classes
#   share_age_0_2..21_23     per-tank age-bin shares (24+ omitted = reference)
#   share_vint_pre1970..2000 vintage cohort shares (2005p omitted = reference)
#   share_cap_under1k..12k25k  capacity class shares
#   share_fuel_diesel, mixed, other  (gas omitted = reference)
#
# CALENDAR CONTROL: factor(panel_year)  — period de-confounding
# STATE FE:         factor(state)
# ==============================================================================

if (USE_ELNET) {

  cat("\n=== S4: Elastic Net ===\n")
  cat("Building saturated feature matrix (Ticket 042 extended design)...\n")

  X_elnet <- model.matrix(
    ~ (factor(age_bin) + has_single_walled + active_tanks +
         total_capacity + has_gasoline_year + has_diesel_year)^2 +
        share_sw_roster + oldest_age + youngest_age + age_spread +
        n_distinct_vintages + n_distinct_makes +
        share_age_0_2 + share_age_3_5 + share_age_6_8 + share_age_9_11 +
        share_age_12_14 + share_age_15_17 + share_age_18_20 + share_age_21_23 +
        share_vint_pre1970 + share_vint_1970 + share_vint_1975 + share_vint_1980 +
        share_vint_1985 + share_vint_1990 + share_vint_1995 + share_vint_2000 +
        share_cap_under1k + share_cap_1k5k + share_cap_5k12k + share_cap_12k25k +
        share_fuel_diesel + share_fuel_mixed + share_fuel_other +
        factor(state) + factor(panel_year_f),
    data = as.data.frame(cv_data)
  )[, -1L]

  cat(sprintf("  Design matrix: %s rows x %d cols\n",
      format(nrow(X_elnet), big.mark = ","), ncol(X_elnet)))

  # ---- Sparse conversion (PERFORMANCE AMENDMENT) ------------------------------
  # Design is dominated by one-hot factor expansions (state x panel_year_f x
  # age_bin, etc.) and is mostly zeros. glmnet is optimized for dgCMatrix input;
  # this changes nothing about folds/coefficients/predictions, only how the
  # matrix is stored and multiplied. Continuous x continuous interaction columns
  # stay dense within the sparse container — no harm, just no extra benefit there.
  n_zero_pre   <- sum(X_elnet == 0)
  pct_zero_pre <- 100 * n_zero_pre / length(X_elnet)
  X_elnet <- Matrix(X_elnet, sparse = TRUE)
  cat(sprintf("  Converted to sparse (dgCMatrix): %.1f%% zeros\n", pct_zero_pre))

  # ---- Facility-grouped, state-stratified CV folds (Ticket 042) ---------------
  # Port from 01r STEP 2: each FACILITY assigned to exactly one fold within its
  # state, so all of a facility's years are held out together (no cross-year leak).
  K_FOLDS <- ifelse(TEST_MODE, 5L, 10L)

  fac_fold_tab <- unique(cv_data[, .(panel_id, state)])
  set.seed(20260202L)
  fac_fold_tab[, fold := {
    n <- .N
    ((sample.int(n) - 1L) %% K_FOLDS) + 1L
  }, by = state]
  cv_data <- merge(cv_data, fac_fold_tab[, .(panel_id, fold)],
                   by = "panel_id", sort = FALSE)
  foldid <- cv_data$fold
  stopifnot(length(foldid) == nrow(X_elnet), all(table(foldid) > 0L))
  cat(sprintf("  CV folds: K=%d, facility-grouped, state-stratified (%d states)\n",
      K_FOLDS, uniqueN(cv_data$state)))

  ELNET_ALPHA <- if (TEST_MODE) c(0, 0.25, 0.5, 0.75, 1.0) else seq(0, 1, by = 0.1)

  # ---- Alpha tuning -----------------------------------------------------------
  # TEST_MODE: subsample facilities (30%) to speed up alpha search.
  # Production: full data with foldid.
  # SKIP_ALPHA_TUNING: bypass the search entirely using KNOWN_BEST_ALPHA (see
  # header "PERFORMANCE AMENDMENTS" — prior grid on this exact sample was flat
  # across alpha in [0.1, 1.0]).
  if (TEST_MODE) {
    set.seed(20260202L)
    fac_tune_ids <- fac_fold_tab[, .SD[sample(.N, max(5L, round(.N * 0.30)))],
                                  by = state]$panel_id
    tune_rows  <- which(cv_data$panel_id %in% fac_tune_ids)
    X_tune     <- X_elnet[tune_rows, , drop = FALSE]
    Y_tune     <- Y[tune_rows]
    fold_tune  <- foldid[tune_rows]
    cat(sprintf("  TEST_MODE: alpha tuning on %s rows (%s facilities)\n",
        format(length(tune_rows), big.mark = ","),
        format(length(fac_tune_ids), big.mark = ",")))
  } else if (!SKIP_ALPHA_TUNING) {
    X_tune <- X_elnet; Y_tune <- Y; fold_tune <- foldid
  }

  elnet_cl <- makeCluster(NUM_THREADS)
  registerDoParallel(elnet_cl)
  # NOTE: deliberately NOT using on.exit(stopCluster(...)) here. This script
  # runs at the top level (sourced, not wrapped in a function), and on.exit()
  # attached inside this `if (USE_ELNET) {...}` block fires as soon as THIS
  # block's call frame completes — i.e. right after S4 finishes — which tears
  # down elnet_cl's worker processes before S4c (which reuses elnet_cl for the
  # B=1000 cluster-resample bootstrap) ever runs. That mismatch is exactly
  # what produced "Error in summary.connection(connection): invalid
  # connection" at S4c: the R object elnet_cl still LOOKS valid (it prints
  # fine), but its 15 PSOCK worker processes are already dead.
  # stopCluster(elnet_cl) is called explicitly at the very end of the script
  # instead (see Summary section), after S4c has finished using it.

  if (SKIP_ALPHA_TUNING && !TEST_MODE) {

    best_alpha <- KNOWN_BEST_ALPHA
    cat(sprintf(
      "\n--- Alpha tuning SKIPPED (SKIP_ALPHA_TUNING=TRUE) ---\n  Using known best_alpha=%.2f from prior full run on this sample.\n  Prior grid: CV deviance flat at 0.13406-0.13407 across alpha in [0.1,1.0];\n  alpha=0 (ridge) distinguishable and worse at 0.13552. Full table in script header.\n  Set SKIP_ALPHA_TUNING <- FALSE to re-run the full search (needed if the\n  estimation sample, features, or fold structure change materially).\n",
        best_alpha))

  } else {

    cat(sprintf("\n--- Alpha tuning: %d values, K=%d, %d threads ---\n",
        length(ELNET_ALPHA), K_FOLDS, NUM_THREADS))

    # Alpha tuning parallelism strategy (Windows PSOCK):
    #   TEST_MODE: parLapply over alphas — X_tune is a 30% subsample (~700k rows),
    #     small enough to serialize to each worker. parallel=FALSE inside each
    #     cv.glmnet (PSOCK workers don't inherit the foreach backend; nested
    #     parallel=TRUE would be silently serial anyway).
    #   Production: lapply over alphas with parallel=TRUE inside — X_tune = X_elnet
    #     (~2.3M x 146). Serializing to 11 workers would be ~30GB; use the registered
    #     doParallel backend to parallelize over folds instead (no matrix duplication).
    if (TEST_MODE) {
      clusterExport(elnet_cl, c("X_tune", "Y_tune", "fold_tune"), envir = environment())
      clusterEvalQ(elnet_cl, library(glmnet))
      alpha_results <- parLapply(elnet_cl, ELNET_ALPHA, function(a) {
        set.seed(20260202L)
        cv_fit <- cv.glmnet(
          x = X_tune, y = Y_tune, family = "binomial", alpha = a,
          foldid = fold_tune, type.measure = "deviance", parallel = TRUE
        )
        list(alpha = a, cv_fit = cv_fit, best_dev = min(cv_fit$cvm))
      })
    } else {
      alpha_results <- lapply(ELNET_ALPHA, function(a) {
        set.seed(20260202L)
        cv_fit <- cv.glmnet(
          x = X_tune, y = Y_tune, family = "binomial", alpha = a,
          foldid = fold_tune, type.measure = "deviance", parallel = TRUE
        )
        list(alpha = a, cv_fit = cv_fit, best_dev = min(cv_fit$cvm))
      })
    }
    for (r in alpha_results)
      cat(sprintf("  alpha=%.2f  lambda.min=%.6f  CV deviance=%.5f\n",
                  r$alpha, r$cv_fit$lambda.min, r$best_dev))

    best_alpha_idx <- which.min(sapply(alpha_results, `[[`, "best_dev"))
    best_alpha     <- alpha_results[[best_alpha_idx]]$alpha

  }

  cat(sprintf("\n>> Best alpha: %.2f\n", best_alpha))

  # ---- Full-data CV (facility-grouped folds, keep=TRUE for OOS preds) ---------
  cat("  Full-data CV at best alpha (facility-grouped foldid, sequential)...\n")
  set.seed(20260202L)
  best_cv_fit <- cv.glmnet(
    x = X_elnet, y = Y, family = "binomial", alpha = best_alpha,
    foldid = foldid, type.measure = "deviance", keep = TRUE, parallel = TRUE
  )
  best_lambda <- best_cv_fit$lambda.min
  cat(sprintf("  lambda.min=%.6f\n", best_lambda))

  # OOS predictions — self-calibrated (no Platt; see Ticket 042 §A3)
  lambda_idx <- which.min(abs(best_cv_fit$lambda - best_lambda))
  lp_oos     <- best_cv_fit$fit.preval[, lambda_idx]
  pred_oos   <- 1 / (1 + exp(-lp_oos))
  cv_data[, pred_elnet := pred_oos]

  cat(sprintf("  OOS pred summary: min=%.4f  median=%.4f  max=%.4f  (true rate=%.4f)\n",
      min(pred_oos), median(pred_oos), max(pred_oos), event_rate))

  # ---- Calibration check (45-degree cell-GoF) ---------------------------------
  cal_check <- cv_data[, .(pred_mean = mean(pred_elnet), obs_mean = mean(event_first_leak)),
                        by = .(has_single_walled, age_bin)]
  cal_r2 <- cor(cal_check$pred_mean, cal_check$obs_mean)^2
  cat(sprintf("  Cell-GoF R² (model vs observed, per age×wall cell): %.3f\n", cal_r2))
  if (cal_r2 < 0.5)
    warning(sprintf("Cell-GoF R²=%.3f < 0.5 — check calibration (is Platt re-introduced?)", cal_r2))

  # ---- Full model refit for variable importance -------------------------------
  set.seed(20260202L)
  fit_elnet <- glmnet(
    x = X_elnet, y = Y, family = "binomial",
    alpha = best_alpha, lambda = best_lambda
  )
  best_params_elnet <- list(alpha = best_alpha, lambda = best_lambda)

  model_meta[["ELNET"]] <- list(
    label = sprintf(
      "Elastic Net Logistic (alpha=%.2f, lambda=%.6f, K=%d, cols=%d; no weights/Platt)",
      best_alpha, best_lambda, K_FOLDS, ncol(X_elnet)
    )
  )
  cat("  Elastic net predictions written to pred_elnet\n")

  # ---- Full-panel scoring (pre + post 1999, for DCM downstream) ---------------
  # Build composition features for all non-leaked facility-years (not just training
  # window) so the DCM can use hazard predictions in both regimes.
  cat("\n--- Full-panel scoring (pre + post 1999) ---\n")

  full_data <- annual_data[
    has_previous_leak == 0L       &
    is_make_model     == 1L       &
    has_single_walled %in% c(0L, 1L) &
    !is.na(active_tanks)          &
    !is.na(total_capacity)
  ]
  full_data[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
  full_data[, state   := factor(state, levels = levels(cv_data$state))]

  # Join composition features for all scored years (roster_raw covers 1985-2020)
  # PERFORMANCE: delegates to build_comp_dt() (see definition near make_age_bin)
  # — same vectorized classify-once + tabulate logic used for the training-
  # sample comp_dt above, instead of a second copy of the ~30-call-per-group
  # aggregation. Validated bit-identical to the original on a subsample — see
  # validate_comp_dt_rewrite.R.
  cat("  Building comp_dt_full via build_comp_dt()...\n")
  comp_dt_full <- build_comp_dt(roster_raw[panel_id %in% unique(full_data$panel_id)])

  n_full_before <- nrow(full_data)
  full_data <- merge(full_data, comp_dt_full, by = c("panel_id", "panel_year"), all.x = TRUE)
  stopifnot(nrow(full_data) == n_full_before)

  n_miss_full <- sum(is.na(full_data$n_tanks_roster))
  cat(sprintf("  Full-panel roster coverage: %s / %s rows (%.1f%% matched)\n",
      format(n_full_before - n_miss_full, big.mark = ","),
      format(n_full_before, big.mark = ","),
      100 * (1 - n_miss_full / n_full_before)))
  if (n_miss_full > 0L)
    cat(sprintf("  [note] %s scored rows missing from roster; composition features imputed\n",
        format(n_miss_full, big.mark = ",")))

  for (col in COMP_SHARE_COLS) full_data[is.na(get(col)), (col) := 0]
  for (col in COMP_CONT_COLS) {
    full_data[, (col) := as.numeric(get(col))]   # ensure double before mean impute
    m <- mean(full_data[[col]], na.rm = TRUE)
    full_data[is.na(get(col)), (col) := m]
  }

  # Clamp panel_year to training range for year_f (avoids new levels in scoring)
  full_py_levels <- levels(cv_data$panel_year_f)
  full_data[, panel_year_clamped := pmin(pmax(panel_year,
    as.integer(min(full_py_levels))), as.integer(max(full_py_levels)))]
  full_data[, panel_year_f := factor(panel_year_clamped, levels = full_py_levels)]
  full_data[, panel_year_clamped := NULL]

  X_full <- model.matrix(
    ~ (factor(age_bin) + has_single_walled + active_tanks +
         total_capacity + has_gasoline_year + has_diesel_year)^2 +
        share_sw_roster + oldest_age + youngest_age + age_spread +
        n_distinct_vintages + n_distinct_makes +
        share_age_0_2 + share_age_3_5 + share_age_6_8 + share_age_9_11 +
        share_age_12_14 + share_age_15_17 + share_age_18_20 + share_age_21_23 +
        share_vint_pre1970 + share_vint_1970 + share_vint_1975 + share_vint_1980 +
        share_vint_1985 + share_vint_1990 + share_vint_1995 + share_vint_2000 +
        share_cap_under1k + share_cap_1k5k + share_cap_5k12k + share_cap_12k25k +
        share_fuel_diesel + share_fuel_mixed + share_fuel_other +
        factor(state) + factor(panel_year_f),
    data = as.data.frame(full_data)
  )[, -1L]

  # Column alignment guard
  if (!setequal(colnames(X_full), colnames(X_elnet))) {
    extra   <- setdiff(colnames(X_full),  colnames(X_elnet))
    missing <- setdiff(colnames(X_elnet), colnames(X_full))
    stop("Full-panel design differs from training:\n  extra: ",
         paste(extra, collapse = ", "), "\n  missing: ", paste(missing, collapse = ", "))
  }
  X_full <- X_full[, colnames(X_elnet)]

  # Sparse conversion (PERFORMANCE AMENDMENT) — same rationale as X_elnet above.
  X_full <- Matrix(X_full, sparse = TRUE)

  lp_full    <- as.numeric(predict(fit_elnet, newx = X_full, type = "link"))
  pred_full  <- 1 / (1 + exp(-lp_full))
  full_data[, pred_elnet_full := pred_full]

  cat(sprintf("  Scored %s facility-years (pre + post 1999)\n",
      format(nrow(full_data), big.mark = ",")))
  print(full_data[, .(N = .N,
                      median_pred = round(median(pred_elnet_full, na.rm = TRUE), 5),
                      mean_pred   = round(mean(pred_elnet_full,   na.rm = TRUE), 5)),
                  by = .(era = fifelse(panel_year < POST_YEAR, "pre-1999", "post-1999"))
                  ][order(era)])

  fwrite(full_data[, .(panel_id, panel_year, state, texas_treated,
                       has_single_walled, age_bin, pred_elnet_full)],
         file.path(ANALYSIS_DIR, "analysis_hazard_predictions_full.csv"))
  cat("  Saved: analysis_hazard_predictions_full.csv\n")

  saveRDS(list(
    fit_elnet         = fit_elnet,
    best_params_elnet = best_params_elnet,
    feature_cols      = colnames(X_elnet),
    age_bin_levels    = levels(cv_data$age_bin),
    state_levels      = levels(cv_data$state),
    year_levels       = levels(cv_data$panel_year_f),
    primary_model     = PRIMARY_MODEL,
    n_train_rows      = nrow(cv_data),
    train_event_rate  = event_rate,
    ticket            = "042",
    timestamp         = Sys.time()
  ), file.path(ANALYSIS_DIR, "analysis_elnet_model.rds"))
  cat("  Saved: analysis_elnet_model.rds\n")

  rm(full_data, X_full, comp_dt_full); invisible(gc())
}

# ---- Set primary prediction column ------------------------------------------
pred_col_primary <- ifelse(PRIMARY_MODEL == "GRF", "pred_grf", "pred_elnet")
cv_data[, pred := get(pred_col_primary)]
cat(sprintf("\nPrimary predictions: %s -> pred\n", pred_col_primary))
print(summary(cv_data$pred))
cat(sprintf("  NA predictions: %d\n", sum(is.na(cv_data$pred))))

predict_prob <- function(newdata_mat) {
  if (PRIMARY_MODEL == "GRF")
    return(get_p1(predict(fit_grf, newdata = newdata_mat)$predictions))
  if (PRIMARY_MODEL == "ELNET")
    return(as.numeric(predict(fit_elnet, newx = newdata_mat, type = "response")))
}


#### S4b: Analytic IJ CIs (cluster-robust sandwich + delta method) ############
#
# WHY VALID: at lambda.min~0 the elastic net approaches the unpenalized logistic
#   MLE. We refit that MLE (feglm, unpenalized) on the selected design, then use
#   the cluster-robust sandwich vcov (clustered by facility) + delta method to
#   get CIs on cell-mean predicted probabilities and the SW-DW contrast.
#   This avoids a 1000x refit bootstrap at the cost of an asymptotic assumption
#   that is well-justified at these sample sizes.

cat("\n=== S4b: Analytic IJ CIs ===\n")

if (USE_ELNET) {
  # Select features with nonzero elastic net coefs at lambda.min
  coef_vec <- as.numeric(coef(best_cv_fit, s = best_lambda)[-1L])
  sel_idx  <- which(coef_vec != 0)
  cat(sprintf("  Selected features: %d / %d  lambda.min=%.2e\n",
      length(sel_idx), ncol(X_elnet), best_lambda))

  # Cap at 500 features ranked by |coef| to keep feglm tractable
  if (length(sel_idx) > 500L) {
    sel_idx <- order(abs(coef_vec), decreasing = TRUE)[seq_len(500L)]
    cat("  Capped to top 500 features by |coef|\n")
  }

  # Densify the capped (<=500 col) subset: fixest::feglm needs a data.frame of
  # plain numeric columns, and as.data.frame() on a sparse Matrix doesn't give
  # the right structure. At <=500 cols this dense block is cheap relative to
  # available RAM (confirmed ample free memory on this machine).
  X_ij <- as.matrix(X_elnet[, sel_idx, drop = FALSE])
  # Sanitize column names for feglm (R formula disallows parentheses etc.)
  colnames(X_ij) <- make.names(colnames(X_ij), unique = TRUE)

  df_ij <- as.data.frame(X_ij)
  df_ij[["Y_ij"]]        <- Y
  df_ij[["panel_id_cl"]] <- cv_data$panel_id

  rhs_ij <- paste(setdiff(names(df_ij), c("Y_ij", "panel_id_cl")), collapse = " + ")
  fml_ij <- as.formula(paste("Y_ij ~", rhs_ij))

  cat(sprintf("  Fitting feglm (%d features, cluster=~panel_id_cl)...\n", ncol(X_ij)))
  fit_ij <- fixest::feglm(fml_ij, data = df_ij, family = binomial,
                          cluster = ~panel_id_cl, notes = FALSE)
  cat("  feglm converged\n")

  V_ij <- vcov(fit_ij)            # (p+1) x (p+1) cluster-robust sandwich vcov
  p_ij <- predict(fit_ij, type = "response")   # in-sample predicted probs

  # X with intercept column (matches V_ij dimension)
  X_ij_int <- cbind(`(Intercept)` = 1, X_ij)

  # Delta method: CI for cell mean of p_hat over `rows`
  delta_cell_ci <- function(rows) {
    if (length(rows) == 0L)
      return(list(est = NA_real_, lo = NA_real_, hi = NA_real_))
    p_c  <- p_ij[rows]
    Xc   <- X_ij_int[rows, , drop = FALSE]
    dp   <- p_c * (1 - p_c)
    G_c  <- colMeans(sweep(Xc, 1L, dp, `*`))   # length-(p+1) gradient
    v_c  <- as.numeric(G_c %*% V_ij %*% G_c)
    est  <- mean(p_c)
    se_c <- sqrt(max(v_c, 0))
    list(est = est,
         lo  = max(0, est - 1.96 * se_c),
         hi  = min(1, est + 1.96 * se_c))
  }

  # ---- Cell-mean CIs for each (age_bin, has_single_walled) ------------------
  ij_rows_list <- list()
  for (wb in c(0L, 1L)) {
    for (ab in AGE_BIN_LABELS) {
      rows_c <- which(cv_data$has_single_walled == wb &
                      as.character(cv_data$age_bin) == ab)
      ci_c   <- delta_cell_ci(rows_c)
      ij_rows_list[[length(ij_rows_list) + 1L]] <- data.table(
        has_single_walled = wb,
        age_bin           = ab,
        ij_est_per1k      = ci_c$est * 1000,
        ij_lo_per1k       = ci_c$lo  * 1000,
        ij_hi_per1k       = ci_c$hi  * 1000,
        n_rows            = length(rows_c)
      )
    }
  }
  ij_cell_dt <- rbindlist(ij_rows_list)
  ij_cell_dt[, age_bin    := factor(age_bin, levels = AGE_BIN_LABELS)]
  ij_cell_dt[, wall_label := factor(
    fifelse(has_single_walled == 1L, "Single-Walled", "Double-Walled"),
    levels = c("Single-Walled", "Double-Walled")
  )]
  cat(sprintf("  IJ cell CIs: %d cells computed\n", nrow(ij_cell_dt)))

  # ---- SW-DW contrast per age bin -------------------------------------------
  contrast_list <- list()
  for (ab in AGE_BIN_LABELS) {
    rows_sw <- which(cv_data$has_single_walled == 1L & as.character(cv_data$age_bin) == ab)
    rows_dw <- which(cv_data$has_single_walled == 0L & as.character(cv_data$age_bin) == ab)
    if (length(rows_sw) == 0L || length(rows_dw) == 0L) next

    Xsw  <- X_ij_int[rows_sw, , drop = FALSE]
    Xdw  <- X_ij_int[rows_dw, , drop = FALSE]
    G_sw <- colMeans(sweep(Xsw, 1L, p_ij[rows_sw] * (1 - p_ij[rows_sw]), `*`))
    G_dw <- colMeans(sweep(Xdw, 1L, p_ij[rows_dw] * (1 - p_ij[rows_dw]), `*`))
    G_diff   <- G_sw - G_dw
    v_diff   <- as.numeric(G_diff %*% V_ij %*% G_diff)
    est_diff <- mean(p_ij[rows_sw]) - mean(p_ij[rows_dw])
    se_diff  <- sqrt(max(v_diff, 0))

    contrast_list[[length(contrast_list) + 1L]] <- data.table(
      age_bin  = ab,
      est_diff_per1k = est_diff * 1000,
      lo_diff_per1k  = (est_diff - 1.96 * se_diff) * 1000,
      hi_diff_per1k  = (est_diff + 1.96 * se_diff) * 1000,
      n_sw     = length(rows_sw),
      n_dw     = length(rows_dw),
      note     = if (length(rows_dw) < 30L) "thin DW cell — CI model-based" else ""
    )
  }
  contrast_dt <- rbindlist(contrast_list)
  contrast_dt[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]

  fwrite(contrast_dt, file.path(OUTPUT_TABLES, "Table_CV_CellRisk_RefContrast.csv"))
  cat("  Saved: Table_CV_CellRisk_RefContrast.csv\n")

  n_sw_gt_dw <- sum(contrast_dt$est_diff_per1k > 0, na.rm = TRUE)
  cat(sprintf("  GATE — SW>DW: %d / %d age bins (must be >= 7 for artifact to be fixed)\n",
      n_sw_gt_dw, nrow(contrast_dt)))
  if (n_sw_gt_dw < 7L)
    warning(sprintf("Only %d/%d age bins have SW>DW — vintage de-confounding may be insufficient",
        n_sw_gt_dw, nrow(contrast_dt)))

} else {
  ij_cell_dt  <- data.table()
  contrast_dt <- data.table()
  cat("  USE_ELNET=FALSE — analytic IJ CIs skipped\n")
}


#### S4c: Cluster-Resample Metric CIs (B = 1000) ##############################
#
# Resamples FACILITIES (with replacement) over the FIXED OOS predictions.
# No model refit per draw — cheap because we only re-aggregate metrics.
# Gives 95% percentile CIs for AUC, PR-AUC, lift@10%, calibration slope.

cat("\n=== S4c: Cluster-Resample Metric CIs (B=", NBOOT_METRICS, ") ===\n", sep = "")

# Pre-build facility -> row index map for efficiency
fac_to_rows_cv <- split(seq_len(nrow(cv_data)), cv_data$panel_id)
fac_ids_cv     <- names(fac_to_rows_cv)
n_facs_cv      <- length(fac_ids_cv)
cat(sprintf("  Resampling %d facilities, B=%d draws...\n", n_facs_cv, NBOOT_METRICS))

NBOOT_USE <- if (TEST_MODE) min(NBOOT_METRICS, 100L) else NBOOT_METRICS
if (TEST_MODE) cat(sprintf("  TEST_MODE: using B=%d\n", NBOOT_USE))

# Parallel bootstrap — export fixed OOS preds and row map to workers
boot_pred_oos <- cv_data$pred_elnet
boot_y_all    <- as.integer(cv_data$event_first_leak)
clusterExport(elnet_cl,
  c("fac_to_rows_cv", "fac_ids_cv", "n_facs_cv", "boot_pred_oos", "boot_y_all"),
  envir = environment())
clusterEvalQ(elnet_cl, { library(pROC); library(PRROC) })
clusterSetRNGStream(elnet_cl, 20260202L)

boot_metric_list <- parLapply(elnet_cl, seq_len(NBOOT_USE), function(b) {
  samp_f  <- sample(fac_ids_cv, n_facs_cv, replace = TRUE)
  row_idx <- unlist(fac_to_rows_cv[samp_f], use.names = FALSE)
  p_b     <- boot_pred_oos[row_idx]
  y_b     <- boot_y_all[row_idx]
  n_b     <- length(y_b)

  auc_b <- tryCatch(
    as.numeric(pROC::auc(pROC::roc(y_b, p_b, quiet = TRUE))),
    error = function(e) NA_real_)
  pr_b <- tryCatch({
    ev_idx <- which(y_b == 1L); ne_idx <- which(y_b == 0L)
    if (length(ev_idx) == 0L || length(ne_idx) == 0L) NA_real_ else
      PRROC::pr.curve(scores.class0 = p_b[ev_idx], scores.class1 = p_b[ne_idx])$auc.integral
  }, error = function(e) NA_real_)
  lift10_b <- tryCatch({
    ord_b <- order(p_b, decreasing = TRUE)[seq_len(ceiling(0.10 * n_b))]
    sum(y_b[ord_b]) / max(sum(y_b), 1L)
  }, error = function(e) NA_real_)

  list(auc = auc_b, pr_auc = pr_b, lift10 = lift10_b)
})

boot_metrics <- rbindlist(lapply(boot_metric_list, as.data.table))
metric_ci_dt <- boot_metrics[, .(
  auc_lo     = quantile(auc,    0.025, na.rm = TRUE),
  auc_hi     = quantile(auc,    0.975, na.rm = TRUE),
  pr_auc_lo  = quantile(pr_auc, 0.025, na.rm = TRUE),
  pr_auc_hi  = quantile(pr_auc, 0.975, na.rm = TRUE),
  lift10_lo  = quantile(lift10, 0.025, na.rm = TRUE),
  lift10_hi  = quantile(lift10, 0.975, na.rm = TRUE)
)]
cat("  Metric CIs (95% percentile over fixed OOS preds):\n")
print(metric_ci_dt)


#### S5: Variable Importance ##################################################

if (PRIMARY_MODEL == "ELNET") {
  # Densify before scale(): centering destroys sparsity anyway, so there's no
  # benefit to keeping this sparse, and as.matrix() here is a one-time, cheap
  # conversion given confirmed ample RAM on this machine.
  X_elnet_scaled <- scale(as.matrix(X_elnet))
  fit_scaled <- glmnet(
    x = X_elnet_scaled, y = Y, family = "binomial",
    alpha  = best_params_elnet$alpha,
    lambda = best_params_elnet$lambda
  )
  imp_vals_raw <- abs(as.numeric(coef(fit_scaled))[-1L])
  col_names    <- colnames(X_elnet)
  imp_by_col   <- data.table(col = col_names, imp = imp_vals_raw)
  imp_by_col[, feature := fcase(
    grepl("age_bin",           col), "age_bin",
    grepl("has_single_walled", col), "has_single_walled",
    grepl("active_tanks",      col), "active_tanks",
    grepl("total_capacity",    col), "total_capacity",
    grepl("has_gasoline_year", col), "has_gasoline_year",
    grepl("has_diesel_year",   col), "has_diesel_year",
    # Portfolio features (Ticket 042)
    grepl("share_sw_roster",   col), "share_sw_roster",
    grepl("oldest_age|youngest_age|age_spread", col), "age_stats",
    grepl("n_distinct",        col), "n_distinct_counts",
    grepl("share_age_",        col), "per_tank_age_shares",
    grepl("share_vint_",       col), "vintage_cohort_shares",
    grepl("share_cap_",        col), "capacity_class_shares",
    grepl("share_fuel_",       col), "fuel_shares",
    grepl("panel_year_f",      col), "year_fe",
    grepl("state",             col), "state",
    default = "other"
  )]
  imp_grouped <- imp_by_col[, .(importance = sum(imp)), by = feature]
  FEATURE_GROUPS <- c(
    "age_bin", "has_single_walled", "active_tanks", "total_capacity",
    "has_gasoline_year", "has_diesel_year",
    "share_sw_roster", "age_stats", "n_distinct_counts",
    "per_tank_age_shares", "vintage_cohort_shares",
    "capacity_class_shares", "fuel_shares",
    "year_fe", "state"
  )
  imp   <- imp_grouped[feature %in% FEATURE_GROUPS]
  imp[, importance_pct := importance / sum(importance) * 100]
  setorder(imp, -importance_pct)

  fwrite(imp, file.path(OUTPUT_TABLES, "Table_CV_VarImportance.csv"))
  cat("\nVariable importance (feature groups):\n")
  print(imp)

  imp_fig <- copy(imp)
  imp_fig[, feature := factor(feature, levels = rev(feature))]

  fig_imp <- ggplot(imp_fig, aes(x = importance_pct, y = feature)) +
    geom_col(fill = COL_TX, alpha = 0.85, width = 0.65) +
    geom_text(aes(label = sprintf("%.1f%%", importance_pct)),
              hjust = -0.15, size = 3.0, color = "grey20") +
    scale_x_continuous(name = "Importance (% of total |coef| weight)",
                       expand = expansion(mult = c(0, 0.15))) +
    labs(y = NULL) +
    theme_pub()

  ggsave(file.path(OUTPUT_FIGURES, "FigureA_CV_VarImportance.png"),
         fig_imp, width = 7, height = 5.5, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, "FigureA_CV_VarImportance.pdf"),
         fig_imp, width = 7, height = 5.5, device = cairo_pdf)
  cat("  FigureA_CV_VarImportance saved\n")
}


#### S6: ROC Curve ############################################################

roc_obj <- pROC::roc(cv_data$event_first_leak, cv_data$pred, quiet = TRUE)
auc_val <- as.numeric(pROC::auc(roc_obj))
cat(sprintf("\nOOB AUC: %.3f\n", auc_val))

roc_dt <- data.table(
  fpr = rev(1 - roc_obj$specificities),
  tpr = rev(roc_obj$sensitivities)
)

fig_roc <- ggplot() +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey55", linewidth = 0.5) +
  geom_line(data = roc_dt, aes(x = fpr, y = tpr),
            color = COL_TX, linewidth = 0.9) +
  annotate("label", x = 0.65, y = 0.10,
           label = sprintf("AUC = %.3f", auc_val),
           size = 3.2, fill = "white", color = "grey20",
           label.size = 0.25, label.padding = unit(0.22, "lines")) +
  coord_fixed() +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.25)) +
  labs(x = "False Positive Rate (1 − Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_ROC.png"),
       fig_roc, width = 5.5, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_ROC.pdf"),
       fig_roc, width = 5.5, height = 5.5, device = cairo_pdf)
cat("  Figure_CV_ROC saved\n")


#### S6b: Precision-Recall Curve ##############################################

pr_obj <- pr.curve(
  scores.class0 = cv_data[event_first_leak == 1L, pred],
  scores.class1 = cv_data[event_first_leak == 0L, pred],
  curve = TRUE
)
pr_auc <- pr_obj$auc.integral
pr_dt  <- as.data.table(pr_obj$curve)
setnames(pr_dt, c("recall", "precision", "threshold"))

fig_pr <- ggplot(pr_dt, aes(x = recall, y = precision)) +
  geom_hline(yintercept = event_rate, linetype = "dashed",
             color = "grey55", linewidth = 0.5) +
  geom_line(color = COL_TX, linewidth = 0.9) +
  annotate("label", x = 0.65, y = event_rate + (max(pr_dt$precision) - event_rate) * 0.12,
           label = sprintf("No-skill baseline: %.2f%%", event_rate * 100),
           size = 2.8, fill = "white", color = "grey40",
           label.size = 0.2, label.padding = unit(0.2, "lines")) +
  annotate("label", x = 0.65, y = max(pr_dt$precision) * 0.88,
           label = sprintf("PR-AUC = %.3f", pr_auc),
           size = 3.2, fill = "white", color = "grey20",
           label.size = 0.25, label.padding = unit(0.22, "lines")) +
  scale_x_continuous(name = "Recall (Sensitivity)",
                     labels = percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_y_continuous(name = "Precision (Positive Predictive Value)",
                     labels = percent_format(accuracy = 0.1),
                     limits = c(0, NA), expand = expansion(mult = c(0.02, 0.08))) +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_PR.png"),
       fig_pr, width = 5.5, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_PR.pdf"),
       fig_pr, width = 5.5, height = 5.5, device = cairo_pdf)
cat(sprintf("  Figure_CV_PR saved  (PR-AUC = %.3f)\n", pr_auc))


#### S7: Model-Smoothed OOB Cell Risk (+ CI bands from analytic IJ) ##########
#
# Figure_CV_CellRisk: OOB elnet cell means + IJ CI bands (Ticket 042)
# Figure_CV_CellRisk_GoF: 45-degree model vs observed (unchanged)
# Figure_CV_CellRisk_Contrast: SW-DW difference per age bin + CI (NEW)

cell_colors <- c("Single-Walled" = COL_TX,  "Double-Walled" = COL_CTRL)
cell_lty    <- c("Single-Walled" = "solid", "Double-Walled" = "dashed")

obs_dt <- cv_data[, .(
  obs_per1k   = mean(event_first_leak, na.rm = TRUE) * 1000,
  n_fac_years = .N,
  n_events    = sum(event_first_leak)
), by = .(has_single_walled, age_bin)]
obs_dt[, wall_label := factor(
  fifelse(has_single_walled == 1L, "Single-Walled", "Double-Walled"),
  levels = c("Single-Walled", "Double-Walled")
)]
obs_dt[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
fwrite(obs_dt, file.path(OUTPUT_TABLES, "Table_CV_CellRates_Observed.csv"))
cat(sprintf("\nObserved cell rates: %d cells\n", nrow(obs_dt)))

cell_dt <- cv_data[!is.na(pred), .(
  model_per1k = mean(pred, na.rm = TRUE) * 1000,
  n_fac_years = .N,
  n_events    = sum(event_first_leak)
), by = .(has_single_walled, age_bin)]
cell_dt[, wall_label  := factor(
  fifelse(has_single_walled == 1L, "Single-Walled", "Double-Walled"),
  levels = c("Single-Walled", "Double-Walled")
)]
cell_dt[, age_bin     := factor(age_bin, levels = AGE_BIN_LABELS)]
cell_dt[, label_vjust := fifelse(wall_label == "Single-Walled", -0.8, 1.6)]
fwrite(cell_dt, file.path(OUTPUT_TABLES, "Table_CV_CellRates_Model.csv"))

# Merge IJ CIs into cell_dt
if (nrow(ij_cell_dt) > 0L) {
  cell_dt <- merge(cell_dt, ij_cell_dt[, .(has_single_walled, age_bin,
                                             ij_lo_per1k, ij_hi_per1k)],
                   by = c("has_single_walled", "age_bin"), all.x = TRUE)
} else {
  cell_dt[, ij_lo_per1k := NA_real_]
  cell_dt[, ij_hi_per1k := NA_real_]
}

# ---- Figure 1: Model cell risk + CI bands -----------------------------------
has_ij <- any(!is.na(cell_dt$ij_lo_per1k))

fig_cell_risk <- ggplot(
  cell_dt[!is.na(model_per1k)],
  aes(x = age_bin, y = model_per1k,
      group = wall_label, color = wall_label, linetype = wall_label)
) +
  { if (has_ij) geom_ribbon(aes(ymin = ij_lo_per1k, ymax = ij_hi_per1k,
                                 fill = wall_label, group = wall_label),
                             alpha = 0.15, color = NA, show.legend = FALSE) } +
  geom_line(linewidth = 0.9) +
  geom_point(shape = 19, size = 2.8) +
  geom_text(aes(label = sprintf("%.1f", model_per1k), vjust = label_vjust),
            size = 2.7, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = cell_colors, name = "Wall Type") +
  scale_fill_manual(values = cell_colors, guide = "none") +
  scale_linetype_manual(values = cell_lty, name = "Wall Type") +
  scale_y_continuous(name = "OOB Predicted First-Leak Rate (per 1,000 Facility-Years)",
                     expand = expansion(mult = c(0.12, 0.12))) +
  labs(x = "Tank Age Bin (3-year intervals)",
       caption = if (has_ij)
         "Shaded band: 95% CI from analytic IJ (cluster-robust sandwich, clustered by facility)"
       else "") +
  guides(color    = guide_legend(override.aes = list(shape = 19,
                                                      linetype = c("solid", "dashed")),
                                  title.position = "top", title.hjust = 0.5),
         linetype = "none") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom", legend.key.width = unit(1.8, "cm"))

ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_CellRisk.png"),
       fig_cell_risk, width = 9, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_CellRisk.pdf"),
       fig_cell_risk, width = 9, height = 6, device = cairo_pdf)
cat("  Figure_CV_CellRisk saved (+ IJ CI bands)\n")

# ---- Figure 2: 45-degree GoF ------------------------------------------------
gof_dt <- merge(
  cell_dt[, .(has_single_walled, age_bin, wall_label, model_per1k, n_fac_years)],
  obs_dt[ , .(has_single_walled, age_bin, obs_per1k)],
  by = c("has_single_walled", "age_bin")
)
xy_max_gof <- max(gof_dt$model_per1k, gof_dt$obs_per1k, na.rm = TRUE) * 1.08

fig_cell_gof <- ggplot(
  gof_dt,
  aes(x = obs_per1k, y = model_per1k,
      color = wall_label, shape = wall_label, size = n_fac_years)
) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey55", linewidth = 0.5) +
  geom_point(alpha = 0.85) +
  geom_text(aes(label = as.character(age_bin)),
            vjust = -0.9, size = 2.4, color = "grey30", show.legend = FALSE) +
  scale_color_manual(values = cell_colors, name = "Wall Type") +
  scale_shape_manual(values = c("Single-Walled" = 19, "Double-Walled" = 17),
                     name = "Wall Type") +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  coord_fixed(xlim = c(0, xy_max_gof), ylim = c(0, xy_max_gof)) +
  labs(x = "Observed First-Leak Rate (per 1,000 Facility-Years)",
       y = "OOB Predicted First-Leak Rate (per 1,000 Facility-Years)") +
  guides(color = guide_legend(override.aes = list(shape = c(19, 17), size = 3),
                               title.position = "top", title.hjust = 0.5)) +
  theme_pub() + theme(legend.position = "bottom")

ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_CellRisk_GoF.png"),
       fig_cell_gof, width = 6, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_CellRisk_GoF.pdf"),
       fig_cell_gof, width = 6, height = 6, device = cairo_pdf)
cat("  Figure_CV_CellRisk_GoF saved\n")

# ---- Figure 3: SW-DW contrast per age bin + CI (Ticket 042 NEW) ------------
if (nrow(contrast_dt) > 0L) {
  fig_contrast <- ggplot(
    contrast_dt,
    aes(x = age_bin, y = est_diff_per1k, group = 1)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.5) +
    geom_ribbon(aes(ymin = lo_diff_per1k, ymax = hi_diff_per1k),
                fill = COL_TX, alpha = 0.2) +
    geom_line(color = COL_TX, linewidth = 0.9) +
    geom_point(color = COL_TX, size = 2.8, shape = 19) +
    geom_text(aes(label = sprintf("%.1f", est_diff_per1k)),
              vjust = -0.9, size = 2.7, color = COL_TX, fontface = "bold") +
    scale_y_continuous(name = "SW − DW First-Leak Rate (per 1,000 Facility-Years)") +
    labs(x = "Tank Age Bin (3-year intervals)",
         caption = "95% CI from analytic IJ (cluster-robust sandwich + delta method). Points above 0 = SW riskier than DW.") +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_CellRisk_Contrast.png"),
         fig_contrast, width = 9, height = 5.5, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_CellRisk_Contrast.pdf"),
         fig_contrast, width = 9, height = 5.5, device = cairo_pdf)
  cat("  Figure_CV_CellRisk_Contrast saved\n")

  n_sw_gt_dw_fig <- sum(contrast_dt$est_diff_per1k > 0, na.rm = TRUE)
  cat(sprintf("  GATE — SW>DW: %d / %d age bins (Ticket 042 requires >= 7)\n",
      n_sw_gt_dw_fig, nrow(contrast_dt)))
}

# ---- Appendix: raw observed cell rates --------------------------------------
obs_dt[, label_vjust := fifelse(wall_label == "Single-Walled", -0.8, 1.6)]

fig_obs <- ggplot(
  obs_dt[!is.na(obs_per1k)],
  aes(x = age_bin, y = obs_per1k,
      group = wall_label, color = wall_label, linetype = wall_label)
) +
  geom_line(linewidth = 0.9) +
  geom_point(shape = 19, size = 2.8) +
  geom_text(aes(label = sprintf("%.1f", obs_per1k), vjust = label_vjust),
            size = 2.7, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = cell_colors, name = "Wall Type") +
  scale_linetype_manual(values = cell_lty, name = "Wall Type") +
  scale_y_continuous(name = "Observed First-Leak Rate (per 1,000 Facility-Years)",
                     expand = expansion(mult = c(0.12, 0.12))) +
  labs(x = "Tank Age Bin (3-year intervals)") +
  guides(color    = guide_legend(override.aes = list(shape = 19,
                                                      linetype = c("solid", "dashed")),
                                  title.position = "top", title.hjust = 0.5),
         linetype = "none") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom", legend.key.width = unit(1.8, "cm"))

ggsave(file.path(OUTPUT_FIGURES, "FigureA_CV_ObsCellRates.png"),
       fig_obs, width = 9, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "FigureA_CV_ObsCellRates.pdf"),
       fig_obs, width = 9, height = 6, device = cairo_pdf)
cat("  FigureA_CV_ObsCellRates saved\n")


#### S8: Calibration ##########################################################

make_cal_dt <- function(pred_col) {
  dt     <- cv_data[!is.na(get(pred_col)) & !is.na(event_first_leak)]
  breaks <- unique(quantile(dt[[pred_col]], probs = seq(0, 1, 0.1), na.rm = TRUE))
  if (length(breaks) < 3L) { warning("Insufficient score variation for calibration"); return(NULL) }
  dt[, decile := cut(get(pred_col), breaks = breaks, include.lowest = TRUE, labels = FALSE)]
  cal <- dt[!is.na(decile), .(
    mean_pred = mean(get(pred_col), na.rm = TRUE),
    mean_obs  = mean(event_first_leak, na.rm = TRUE),
    n_fac_yrs = .N,
    n_events  = sum(event_first_leak)
  ), by = decile][order(decile)]
  fwrite(cal, file.path(OUTPUT_TABLES, "Table_CV_Calibration.csv"))
  cal
}

cal <- make_cal_dt("pred")

if (!is.null(cal)) {
  xy_max <- max(cal$mean_obs, cal$mean_pred, na.rm = TRUE) * 1.08
  fig_cal <- ggplot(cal, aes(x = mean_pred, y = mean_obs)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.5) +
    geom_line(color = COL_TX, linewidth = 0.6) +
    geom_point(aes(size = n_fac_yrs), color = COL_TX, shape = 19, alpha = 0.85) +
    geom_text(aes(label = decile), vjust = -0.9, size = 2.5, color = "grey30") +
    coord_fixed(xlim = c(0, xy_max), ylim = c(0, xy_max)) +
    scale_x_continuous(labels = percent_format(accuracy = 0.01)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
    scale_size_continuous(range = c(2, 7), guide = "none") +
    labs(x = "Mean Predicted Probability", y = "Mean Observed Release Rate") +
    theme_pub()
  ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_Calibration.png"),
         fig_cal, width = 5.5, height = 5.5, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_Calibration.pdf"),
         fig_cal, width = 5.5, height = 5.5, device = cairo_pdf)
  cat("  Figure_CV_Calibration saved\n")
}


#### S9: Appendix — Lift + Score Separation ###################################

make_appendix_figs <- function(pred_col, label, file_suffix) {
  dt <- cv_data[!is.na(get(pred_col)) & !is.na(event_first_leak)]
  setorderv(dt, pred_col, order = -1L)
  dt[, cum_pct_screened := seq_len(.N) / .N]
  dt[, cum_pct_events   := cumsum(event_first_leak) / sum(event_first_leak, na.rm = TRUE)]
  top10 <- dt[cum_pct_screened <= 0.101, max(cum_pct_events, na.rm = TRUE)]
  top20 <- dt[cum_pct_screened <= 0.201, max(cum_pct_events, na.rm = TRUE)]
  cat(sprintf("  [%s] Top 10%%: %.0f%% events | Top 20%%: %.0f%% events\n",
              label, top10 * 100, top20 * 100))
  lift_line <- rbind(
    data.table(x = 0, y = 0, type = label),
    dt[, .(x = cum_pct_screened, y = cum_pct_events, type = label)],
    data.table(x = c(0, 1), y = c(0, 1), type = "Random")
  )
  fig_lift <- ggplot() +
    geom_line(data = lift_line,
              aes(x = x, y = y, color = type, linetype = type), linewidth = 0.9) +
    geom_segment(aes(x = 0.10, xend = 0.10, y = 0, yend = top10),
                 linetype = "dotted", color = "grey50", linewidth = 0.4) +
    geom_segment(aes(x = 0, xend = 0.10, y = top10, yend = top10),
                 linetype = "dotted", color = "grey50", linewidth = 0.4) +
    annotate("text", x = 0.12, y = top10 * 0.88, hjust = 0, size = 2.8, color = "grey30",
             label = sprintf("Top 10%%:\n%.0f%% of releases captured", top10 * 100)) +
    scale_color_manual(values  = setNames(c(COL_TX, "grey55"), c(label, "Random"))) +
    scale_linetype_manual(values = setNames(c("solid", "dashed"), c(label, "Random"))) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "Fraction of Facilities Screened (descending risk)",
         y = "Cumulative Fraction of Releases Captured", color = NULL, linetype = NULL) +
    theme_pub() + theme(legend.position = "bottom")
  ggsave(file.path(OUTPUT_FIGURES, sprintf("FigureA_CV_Lift_%s.png",     file_suffix)),
         fig_lift, width = 6, height = 5.5, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, sprintf("FigureA_CV_Lift_%s.pdf",     file_suffix)),
         fig_lift, width = 6, height = 5.5, device = cairo_pdf)
  dt[, outcome_label := fifelse(event_first_leak == 1L, "Confirmed Release", "No Release")]
  x_cap <- quantile(dt[[pred_col]], 0.99, na.rm = TRUE)
  fig_sep <- ggplot(dt[get(pred_col) <= x_cap],
                    aes(x = get(pred_col), fill = outcome_label, color = outcome_label)) +
    geom_density(alpha = 0.35, linewidth = 0.7) +
    scale_fill_manual(values  = c("Confirmed Release" = COL_TX, "No Release" = COL_CTRL)) +
    scale_color_manual(values = c("Confirmed Release" = COL_TX, "No Release" = COL_CTRL)) +
    scale_x_continuous(labels = percent_format(accuracy = 0.01), limits = c(0, x_cap)) +
    labs(x = "Predicted Annual Release Probability", y = "Density", fill = NULL, color = NULL) +
    theme_pub() + theme(legend.position = "bottom")
  ggsave(file.path(OUTPUT_FIGURES, sprintf("FigureA_CV_ScoreSep_%s.png", file_suffix)),
         fig_sep, width = 6, height = 4.5, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, sprintf("FigureA_CV_ScoreSep_%s.pdf", file_suffix)),
         fig_sep, width = 6, height = 4.5, device = cairo_pdf)
  invisible(list(top10 = top10, top20 = top20))
}

cat("\nAppendix figures:\n")
app <- make_appendix_figs("pred", model_meta[[PRIMARY_MODEL]]$label, "State")


#### S10: GoF Summary Table (+ metric CIs from cluster resample) ##############

n_total <- nrow(cv_data)

compute_gof <- function(pred_col, label) {
  roc_i <- pROC::roc(cv_data$event_first_leak, cv_data[[pred_col]], quiet = TRUE)
  auc_i <- as.numeric(pROC::auc(roc_i))
  pr_i  <- pr.curve(
    scores.class0 = cv_data[event_first_leak == 1L, get(pred_col)],
    scores.class1 = cv_data[event_first_leak == 0L, get(pred_col)],
    curve = FALSE
  )
  dt_i <- cv_data[!is.na(get(pred_col)) & !is.na(event_first_leak)]
  setorderv(dt_i, pred_col, order = -1L)
  dt_i[, cum_pct_screened := seq_len(.N) / .N]
  dt_i[, cum_pct_events   := cumsum(event_first_leak) / sum(event_first_leak, na.rm = TRUE)]
  t10 <- dt_i[cum_pct_screened <= 0.101, max(cum_pct_events, na.rm = TRUE)]
  t20 <- dt_i[cum_pct_screened <= 0.201, max(cum_pct_events, na.rm = TRUE)]

  # Attach cluster-resample CIs for the primary elnet model
  ci_cols <- data.table(
    AUC_lo = NA_real_, AUC_hi = NA_real_,
    PR_AUC_lo = NA_real_, PR_AUC_hi = NA_real_,
    Lift10_CI_lo = NA_real_, Lift10_CI_hi = NA_real_
  )
  if (pred_col == pred_col_primary && nrow(metric_ci_dt) > 0L) {
    ci_cols <- data.table(
      AUC_lo       = round(metric_ci_dt$auc_lo,    3),
      AUC_hi       = round(metric_ci_dt$auc_hi,    3),
      PR_AUC_lo    = round(metric_ci_dt$pr_auc_lo, 3),
      PR_AUC_hi    = round(metric_ci_dt$pr_auc_hi, 3),
      Lift10_CI_lo = round(metric_ci_dt$lift10_lo, 3),
      Lift10_CI_hi = round(metric_ci_dt$lift10_hi, 3)
    )
  }

  cbind(data.table(
    Model            = label,
    Primary          = pred_col == pred_col_primary,
    AUC              = round(auc_i, 3),
    PR_AUC           = round(pr_i$auc.integral, 3),
    Top10_pct_events = round(t10 * 100, 1),
    Top20_pct_events = round(t20 * 100, 1),
    N_fac_years      = n_total,
    N_events         = n_events,
    Base_rate_pct    = round(event_rate * 100, 3),
    CI_B             = NBOOT_USE,
    Ticket           = "042"
  ), ci_cols)
}

gof_rows <- list()
if (USE_GRF)   gof_rows[["GRF"]]   <- compute_gof("pred_grf",   model_meta$GRF$label)
if (USE_ELNET) gof_rows[["ELNET"]] <- compute_gof("pred_elnet", model_meta$ELNET$label)

gof_summary <- rbindlist(gof_rows, fill = TRUE)
fwrite(gof_summary, file.path(OUTPUT_TABLES, "Table_CV_GoF_Summary.csv"))
cat("\nGoF summary (+ metric CIs from cluster resample):\n")
print(gof_summary)


#### S11: Risk Score Outputs ##################################################

score_cols <- c("panel_id", "panel_year", "state", "texas_treated")
if (USE_GRF)   score_cols <- c(score_cols, "pred_grf")
if (USE_ELNET) score_cols <- c(score_cols, "pred_elnet")

cv_scores_fy <- cv_data[, score_cols, with = FALSE]
cv_scores_fy[, risk_score := get(pred_col_primary)]
saveRDS(cv_scores_fy, file.path(ANALYSIS_DIR, "analysis_cv_data_fac_year.rds"))
cat(sprintf("\nSaved: analysis_cv_data_fac_year.rds  (%s rows)\n",
    format(nrow(cv_scores_fy), big.mark = ",")))

snap_cols <- c("panel_id", "state", "texas_treated")
if (USE_GRF)   snap_cols <- c(snap_cols, "pred_grf")
if (USE_ELNET) snap_cols <- c(snap_cols, "pred_elnet")

snap <- cv_data[panel_year == SNAP_YEAR, c(snap_cols, "panel_year"), with = FALSE]
setnames(snap, "panel_year", "snap_year")

missing_ids <- setdiff(unique(cv_data$panel_id), snap$panel_id)
if (length(missing_ids) > 0L) {
  cat(sprintf("  %s facilities missing in %d — using last pre-reform obs\n",
      format(length(missing_ids), big.mark = ","), SNAP_YEAR))
  fallback_cols <- c("panel_id", "state", "texas_treated", "panel_year")
  if (USE_GRF)   fallback_cols <- c(fallback_cols, "pred_grf")
  if (USE_ELNET) fallback_cols <- c(fallback_cols, "pred_elnet")
  fallback <- cv_data[
    panel_id %in% missing_ids,
    .SD[which.max(panel_year)],
    by = panel_id,
    .SDcols = setdiff(fallback_cols, "panel_id")
  ]
  setnames(fallback, "panel_year", "snap_year")
  snap <- rbindlist(list(snap, fallback), use.names = TRUE, fill = TRUE)
}

snap[, risk_score := get(pred_col_primary)]
q_cuts <- quantile(snap$risk_score, probs = c(0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE)
snap[, risk_quartile := cut(
  risk_score, breaks = unique(q_cuts), include.lowest = TRUE,
  labels = c("Q1-Low", "Q2", "Q3", "Q4-High")[seq_len(length(unique(q_cuts)) - 1L)]
)]
saveRDS(snap, file.path(ANALYSIS_DIR, "analysis_cv_data_fac.rds"))
cat(sprintf("Saved: analysis_cv_data_fac.rds  (%s rows)\n",
    format(nrow(snap), big.mark = ",")))

cat("\nRisk score by group (1998 snapshot):\n")
print(snap[, .(
  Mean   = round(mean(risk_score,           na.rm = TRUE), 4),
  Median = round(median(risk_score,         na.rm = TRUE), 4),
  P75    = round(quantile(risk_score, 0.75, na.rm = TRUE), 4),
  N      = .N
), by = .(Group = fifelse(texas_treated == 1L, "Texas", "Control"))])

ks <- ks.test(snap[texas_treated == 1L, risk_score], snap[texas_treated == 0L, risk_score])
cat(sprintf("KS test TX vs Control: D = %.4f, p = %.4f\n", ks$statistic, ks$p.value))

# ── DCM hazard cell schedule (per S11, unchanged role) ──────────────────────
dcm_hazard <- cv_data[!is.na(pred), .(
  h_hat        = mean(pred,             na.rm = TRUE),
  h_hat_per1k  = mean(pred,             na.rm = TRUE) * 1000,
  h_obs_per1k  = mean(event_first_leak, na.rm = TRUE) * 1000,
  n_fac_years  = .N,
  n_events     = sum(event_first_leak)
), by = .(age_bin_int = as.integer(age_bin), has_single_walled, age_bin)]

dcm_hazard[, w_state    := as.integer(!has_single_walled) + 1L]
dcm_hazard[, wall_label := fifelse(has_single_walled == 1L, "Single-Walled", "Double-Walled")]
dcm_hazard[, age_bin    := factor(age_bin, levels = AGE_BIN_LABELS)]
setorder(dcm_hazard, has_single_walled, age_bin_int)

fwrite(dcm_hazard, file.path(ANALYSIS_DIR, "dcm_state_hazard_rates.csv"))
cat(sprintf("\nSaved: dcm_state_hazard_rates.csv  (%d cells)\n", nrow(dcm_hazard)))


#### S12: Lifetime Cumulative First-Release Probability #######################

LIFE_HORIZONS <- c(24L, 30L, 35L)
BIN_WIDTH     <- 3L
OPEN_BIN      <- "24+"

haz_dt <- obs_dt[, .(wall_label, age_bin, h = obs_per1k / 1000, n_fac_years)]
setorder(haz_dt, wall_label, age_bin)

lifetime_ci <- function(h_vec, bin_lab, L) {
  age <- 0L; S <- 1
  for (i in seq_along(h_vec)) {
    yy  <- if (bin_lab[i] == OPEN_BIN) max(0L, L - age) else min(BIN_WIDTH, max(0L, L - age))
    S   <- S * (1 - h_vec[i])^yy
    age <- age + yy
  }
  1 - S
}

life_rows <- list()
for (L in LIFE_HORIZONS) {
  by_wall <- haz_dt[, .(ci = lifetime_ci(h, as.character(age_bin), L),
                         w = sum(n_fac_years)), by = wall_label]
  fleet   <- by_wall[, sum(ci * w) / sum(w)]
  life_rows[[as.character(L)]] <- rbindlist(list(
    by_wall[, .(horizon = L, group = wall_label, lifetime_leak_prob = ci)],
    data.table(horizon = L, group = "Fleet (composition-weighted)", lifetime_leak_prob = fleet)
  ))
}
lifetime_leak <- rbindlist(life_rows)
lifetime_leak[, one_in := 1 / lifetime_leak_prob]
fwrite(lifetime_leak, file.path(OUTPUT_TABLES, "Table_CV_LifetimeLeakProb.csv"))
cat("\nLifetime cumulative first-release probability:\n")
print(lifetime_leak[, .(horizon, group, pct = round(lifetime_leak_prob * 100, 1),
                         one_in = round(one_in, 1))])

fleet30 <- lifetime_leak[horizon == 30L & group == "Fleet (composition-weighted)",
                          lifetime_leak_prob]
cat(sprintf("\nSLIDE: ~%.0f%% of facilities (1 in %.1f) leak over 30 years.\n",
    fleet30 * 100, 1 / fleet30))


#### Cleanup ###################################################################
# Explicit cluster teardown (see note at elnet_cl creation in S4 — on.exit()
# inside that block fired too early, so the cluster is stopped here instead,
# after S4c's bootstrap — its last consumer — has finished using it.
if (exists("elnet_cl") && inherits(elnet_cl, "cluster")) {
  tryCatch({
    parallel::stopCluster(elnet_cl)
    cat("\nelnet_cl stopped.\n")
  }, error = function(e) cat(sprintf("\n[note] elnet_cl already stopped or invalid: %s\n", conditionMessage(e))))
}


#### Summary ##################################################################

cat("\n========================================================\n")
cat(sprintf("01n COMPLETE (Ticket 042)  [%s]\n\n",
    ifelse(TEST_MODE, "TEST MODE", "PRODUCTION")))
cat(sprintf("  Models:      GRF=%s  ELNET=%s  Primary=%s\n", USE_GRF, USE_ELNET, PRIMARY_MODEL))
cat(sprintf("  Sample:      %s fac-yrs | TX<1999+Controls>=%d | floor=%d\n",
    format(nrow(cv_data), big.mark = ","), POST_YEAR, YEAR_FLOOR))
cat(sprintf("  Events:      %s (%.3f%% base rate)\n",
    format(n_events, big.mark = ","), event_rate * 100))
cat(sprintf("  Folds:       K=%d, facility-grouped, state-stratified\n", K_FOLDS))
cat(sprintf("  Alpha:       %.2f  (tuning skipped: %s)\n", best_alpha, SKIP_ALPHA_TUNING && !TEST_MODE))
cat(sprintf("  AUC:         %.3f  [%.3f, %.3f]\n",
    auc_val, metric_ci_dt$auc_lo, metric_ci_dt$auc_hi))
cat(sprintf("  PR-AUC:      %.3f  [%.3f, %.3f]\n",
    pr_auc, metric_ci_dt$pr_auc_lo, metric_ci_dt$pr_auc_hi))
cat(sprintf("  Top 10%%:     %.0f%% of releases  [%.0f%%, %.0f%%]\n",
    app$top10 * 100, metric_ci_dt$lift10_lo * 100, metric_ci_dt$lift10_hi * 100))
cat(sprintf("  SW>DW bins:  %d / %d (artifact fix gate)\n",
    if (nrow(contrast_dt) > 0L) sum(contrast_dt$est_diff_per1k > 0, na.rm = TRUE) else -1L,
    nrow(contrast_dt)))
cat("\n  New outputs (Ticket 042):\n")
cat("    Figure_CV_CellRisk          (+ IJ CI bands)\n")
cat("    Figure_CV_CellRisk_Contrast (SW-DW per age bin + CIs)\n")
cat("    Table_CV_CellRisk_RefContrast\n")
cat("    Table_CV_GoF_Summary        (+ cluster-resample CI columns)\n")
cat("    01r_Leak_Rate.R             archived -> Archive/\n")
cat("========================================================\n")