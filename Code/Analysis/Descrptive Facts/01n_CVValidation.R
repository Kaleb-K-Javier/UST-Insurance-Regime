################################################################################
# 01n_CVValidation.R
# Observable Risk Characteristics and the Actuarial Pricing Foundation
#
# PURPOSE:
#   Establishes that observable facility characteristics — wall construction
#   type and tank age — predict confirmed releases out-of-sample. Produces
#   figures and tables for the "Observable Risk Characteristics" section.
#
# ESTIMAND:
#   h(t | x) = P(first confirmed release in year t |
#               survived release-free to t, facility covariates x)
#
#   This is the annual first-release hazard in the never-yet-leaked risk set.
#   It directly mirrors the insurer's underwriting problem: given what I can
#   observe about this facility at the start of the policy year, what is the
#   probability of a claim this year?
#
# MODEL:
#   GRF probability forest (grf package). Single specification including
#   all facility-level observables and state. State is included because
#   Mid-Continent and other insurers file rates by state — a no-state model
#   would be artificially handicapped relative to actual underwriting practice.
#
#   State serves dual roles:
#     - As a split feature: learns state-specific baseline hazard rates
#     - As a cluster ID: ensures honest inference across state groups
#   These are independent concerns — GRF handles both simultaneously.
#
#   OOB predicted probabilities extracted via fit$predictions[, "1"] (by
#   column name throughout — safer than positional indexing).
#   No separate CV loop is needed: GRF's subsampling gives each observation
#   an OOB prediction, and with 2,000 trees every observation has a stable
#   OOB estimate.
#
# CLASS IMBALANCE:
#   Release events are rare (~1-3% annual base rate). Without correction the
#   forest learns to predict the majority class (no release) everywhere.
#   Fix: case.weights = n_0/n_1 for all event == 1 rows, 1.0 for event == 0.
#   Each tree's bootstrap sample then sees roughly equal 0s and 1s, forcing
#   the forest to learn the discriminating signal in the minority class.
#   Note: this does NOT collapse predicted probabilities to 50/50.
#   GRF outputs P(Y=1|X) on the original scale. Calibration plot (S8)
#   verifies this empirically.
#
# COVARIATES (insurer's observable portfolio at start of policy year):
#   has_single_walled  — time-varying wall composition (binary)
#   age_bin            — current active stock age (3-year bins, as integer)
#   active_tanks       — portfolio size
#   total_capacity     — total capacity (gallons)
#   has_gasoline_year  — gasoline fuel indicator
#   has_diesel_year    — diesel fuel indicator
#   state_int          — state integer encoding for state-specific baselines
#
# ESTIMATION SAMPLE:
#   All states (TX + controls), pre-treatment (panel_year < POST_YEAR).
#   Never-yet-leaked risk set: has_previous_leak == 0.
#   Make-model primary sample: is_make_model == 1.
#
# TEST MODE:
#   Set TEST_MODE <- TRUE (below) to skip hyperparameter tuning entirely and
#   run with pre-validated parameters at a reduced tree count (200 trees).
#   All output files are written to their normal paths so the full pipeline
#   can be verified end-to-end before committing to a production run.
#   Set TEST_MODE <- FALSE for a full production run (2,000 trees + tuning).
#
# OUTPUTS
# -------
# Main figures (Output/Figures/):
#   Figure_CV_ROC               ROC curve with OOB AUC
#   Figure_CV_PartialDep        Marginal PD: predicted hazard by age x wall type
#   Figure_CV_Calibration       Calibration decile plot
#
# Appendix figures (Output/Figures/):
#   FigureA_CV_Lift_State       Lift curve
#   FigureA_CV_ScoreSep_State   Score separation density
#   FigureA_CV_VarImportance    Variable importance
#
# Tables (Output/Tables/):
#   Table_CV_GoF_Summary        AUC + lift summary
#   Table_CV_Calibration        Decile calibration table
#   Table_CV_CellRates          Predicted + observed rates by (wall, age) cell
#   Table_CV_VarImportance      Variable importance scores
#
# Risk score outputs (Data/Analysis/):
#   analysis_cv_data_fac_year.rds   Facility-year predictions (panel_id x panel_year)
#   analysis_cv_data_fac.rds        Facility-level score at reform year (panel_id only)
################################################################################


#### S1: Setup ################################################################
#
# STANDALONE SCRIPT — no dependency on 01a_Setup.R or any other 01x file.
# All constants, paths, colours, and helpers are defined here.
#
# Input:  Data/Processed/facility_leak_behavior_annual.csv
#         (produced by 10_Build_Annual_Panel_Optimized.R)
# Output: figures → Output/Figures/
#         tables  → Output/Tables/
#         scores  → Data/Analysis/

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(grf)
  library(pROC)
  library(patchwork)
  library(scales)
  library(here)
})

options(scipen = 999)
set.seed(20260202L)
setDTthreads(14L)

# ---- Output paths -----------------------------------------------------------
OUTPUT_FIGURES <- here("Output", "Figures")
OUTPUT_TABLES  <- here("Output", "Tables")
ANALYSIS_DIR   <- here("Data", "Analysis")
for (d in c(OUTPUT_FIGURES, OUTPUT_TABLES, ANALYSIS_DIR))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- Study parameters -------------------------------------------------------
POST_YEAR  <- 1999L   # first post-treatment year
SNAP_YEAR  <- 1998L   # reform snapshot year for facility-level risk scores

# ---- State sample -----------------------------------------------------------
# CONTROL_STATES: approved control states for this study.
# NJ excluded: private insurance mandate 2003 contaminates the post period.
# WA, OR, and all other treated/remainder states are excluded by not being
# listed here. The state filter in S2 is the enforcement point.
CONTROL_STATES <- c(
  "ME", "NM", "AR", "OK", "LA", "KS", "MT", "ID", "SD", "AL",
  "MN", "NC", "IL", "MA", "OH", "PA", "TN", "VA", "CO"
)
# TX is added explicitly in the filter — CONTROL_STATES is controls only.
ALL_STUDY_STATES <- c("TX", CONTROL_STATES)

# ---- Make-model sample criteria ---------------------------------------------
# Facilities where insurer CANNOT assign a rate cell are excluded.
# These are the ONLY exclusion criteria for the primary sample.
MM_WALL_EXCLUDE <- "Unknown-Wall"
MM_FUEL_EXCLUDE <- "Unknown-Fuel"

# ---- Canonical age bins -----------------------------------------------------
# Must exactly match 10_Build_Annual_Panel_Optimized.R Section 9.2:
#   cut(avg_tank_age_dec,
#       breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf),
#       labels = c("0-2","3-5","6-8","9-11","12-14","15-17","18-20","21-23","24+"))
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

# ---- GRF constants ----------------------------------------------------------
# NUM_TREES is set here; TEST_MODE (below) may override it to 200.
NUM_TREES <- 2000L   # production forest size; tuning uses 50-tree forests

# ---- Fast test mode ---------------------------------------------------------
# Set TEST_MODE = TRUE to skip tuning entirely and run with pre-validated
# parameters at a reduced tree count. All output files are written to their
# normal paths — use this for a full end-to-end smoke test before production.
# Set TEST_MODE = FALSE for the real production run (2,000 trees + tuning).
#
# Pre-validated parameters (from tuning log):
#   mtry = 7, min.node.size = 5, sample.fraction = 0.7, Brier = 0.18048
TEST_MODE <- TRUE   # <-- flip to TRUE for a fast smoke test

BEST_PARAMS_FIXED <- list(
  mtry            = 7L,
  min.node.size   = 5L,
  sample.fraction = 0.7
)

if (TEST_MODE) {
  NUM_TREES <- 200L
  cat("================================================================================\n")
  cat("*** TEST MODE ACTIVE — tuning skipped, pre-validated parameters injected ***\n")
  cat(sprintf("    mtry=%d  min.node.size=%d  sample.fraction=%.1f  Brier=%.5f\n",
              BEST_PARAMS_FIXED$mtry,
              BEST_PARAMS_FIXED$min.node.size,
              BEST_PARAMS_FIXED$sample.fraction,
              0.18048))
  cat(sprintf("    Tree count reduced to %d for speed.\n", NUM_TREES))
  cat("    All output files will be written to their normal paths.\n")
  cat("================================================================================\n\n")
}

# ---- Safe GRF prediction extractor ------------------------------------------
# GRF probability_forest returns a predictions matrix with columns named by
# factor level ("0", "1"). Some GRF versions omit dimnames in certain
# configurations, causing [, "1"] to error with "no dimnames attribute".
# This helper tries named access first, falls back to positional column 2
# (always P(Y=1) when levels = c(0L, 1L)), and errors clearly if neither works.
get_p1 <- function(pred_mat) {
  if (!is.null(colnames(pred_mat)) && "1" %in% colnames(pred_mat))
    return(pred_mat[, "1"])
  if (ncol(pred_mat) >= 2L)
    return(pred_mat[, 2L])
  stop("get_p1: cannot extract P(Y=1) — unexpected prediction matrix structure")
}

# ---- Load panel data --------------------------------------------------------
PANEL_CSV <- here("Data", "Processed", "facility_leak_behavior_annual.csv")
if (!file.exists(PANEL_CSV))
  stop(
    "Panel CSV not found: ", PANEL_CSV,
    "\nRun 10_Build_Annual_Panel_Optimized.R first."
  )

cat("=== 01n: Observable Risk — Actuarial Validation ===\n\n")
cat(sprintf("Loading: %s\n", PANEL_CSV))
annual_data <- fread(PANEL_CSV)
cat(sprintf("  Loaded: %s rows x %d cols | %d-%d\n",
    format(nrow(annual_data), big.mark = ","),
    ncol(annual_data),
    min(annual_data$panel_year),
    max(annual_data$panel_year)))



#### S2: Estimation Sample ####################################################
#
# FILTERS (in order):
#   1. State: TX + CONTROL_STATES only (ALL_STUDY_STATES defined in S1)
#             Any state not in this list — other treated states, WA, OR,
#             remainder states — is dropped here. Hard stop.
#   2. Pre-treatment years: panel_year < POST_YEAR (< 1999)
#   3. Never-yet-leaked risk set: has_previous_leak == 0
#   4. Make-model classified: fac_wall != MM_WALL_EXCLUDE
#                              fac_fuel != MM_FUEL_EXCLUDE
#      Drops facilities the insurer cannot assign to a rate cell.
#   5. Binary wall only: has_single_walled %in% c(0L, 1L)
#      Drops Mixed-wall portfolios (ambiguous pricing signal for GRF feature).
#
# COLUMN MAPPING from facility_leak_behavior_annual.csv:
#   age_bins    -> age_bin  (time-varying avg stock age, 9 bins)
#   has_single_walled  (alias for has_single_walled_dec, built in panel S10)
#   active_tanks       (alias for active_tanks_dec)
#   total_capacity     (alias for total_capacity_dec)
#
# TODO — time-varying oldest-tank age: add current-year oldest active tank
# age to 10_Build_Annual_Panel_Optimized.R Section 9.2, then add it as a
# GRF feature here.

required_cols <- c(
  "fac_wall", "fac_fuel",
  "has_single_walled",
  "age_bins",
  "avg_tank_age",
  "active_tanks", "total_capacity",
  "has_gasoline_year", "has_diesel_year",
  "event_first_leak", "has_previous_leak",
  "state", "panel_id", "panel_year", "texas_treated"
)
missing_cols <- setdiff(required_cols, names(annual_data))
if (length(missing_cols) > 0L)
  stop("annual_data missing required columns: ",
       paste(missing_cols, collapse = ", "))

# ---- FILTER 1: State whitelist — TX + approved controls ONLY ---------------
n_before_state <- nrow(annual_data)
annual_data    <- annual_data[state %in% ALL_STUDY_STATES]
n_dropped_state <- n_before_state - nrow(annual_data)

cat(sprintf("\nState filter:\n"))
cat(sprintf("  Kept:    %s rows (%d states)\n",
    format(nrow(annual_data), big.mark = ","),
    uniqueN(annual_data$state)))
cat(sprintf("  Dropped: %s rows from states outside TX + controls\n",
    format(n_dropped_state, big.mark = ",")))
cat(sprintf("  States retained: %s\n",
    paste(sort(unique(annual_data$state)), collapse = ", ")))

# Hard stop if TX or any expected control is missing
missing_states <- setdiff(ALL_STUDY_STATES, unique(annual_data$state))
if (length(missing_states) > 0)
  warning("Expected states not found in panel: ",
          paste(missing_states, collapse = ", "))

# ---- Age bin: rename age_bins -> age_bin with defensive fallback -----------
# If panel builder labels match AGE_BIN_LABELS, use directly.
# Otherwise recut from avg_tank_age using make_age_bin().
if ("age_bins" %in% names(annual_data)) {
  existing_labels <- if (is.factor(annual_data$age_bins))
    levels(annual_data$age_bins)
  else
    sort(unique(annual_data$age_bins[!is.na(annual_data$age_bins)]))

  if (identical(as.character(existing_labels), AGE_BIN_LABELS)) {
    annual_data[, age_bin := age_bins]
    cat("\nage_bin: labels match — using age_bins directly\n")
  } else {
    cat("\nage_bin: WARNING — labels differ, recomputing from avg_tank_age\n")
    cat(sprintf("  Panel:    %s\n", paste(existing_labels, collapse = ", ")))
    cat(sprintf("  Expected: %s\n", paste(AGE_BIN_LABELS,  collapse = ", ")))
    annual_data[, age_bin := make_age_bin(avg_tank_age)]
  }
} else {
  cat("\nage_bin: age_bins column absent, computing from avg_tank_age\n")
  annual_data[, age_bin := make_age_bin(avg_tank_age)]
}

# ---- Derive is_make_model --------------------------------------------------
annual_data[, is_make_model := as.integer(
  fac_wall != MM_WALL_EXCLUDE &
  fac_fuel != MM_FUEL_EXCLUDE &
  !is.na(age_bin)
)]
cat(sprintf("\nis_make_model: %s / %s facility-years classified (%.1f%%)\n",
    format(sum(annual_data$is_make_model == 1L), big.mark = ","),
    format(nrow(annual_data), big.mark = ","),
    mean(annual_data$is_make_model == 1L) * 100))

# ---- FILTERS 2-5: Build estimation sample ----------------------------------
cv_data <- annual_data[
  panel_year        <  POST_YEAR    &   # pre-treatment only
  has_previous_leak == 0L           &   # never-yet-leaked risk set
  is_make_model     == 1L           &   # classified wall + fuel + age
  has_single_walled %in% c(0L, 1L)  &   # binary wall (drop Mixed)
  !is.na(active_tanks)              &
  !is.na(total_capacity)            &
  !is.na(event_first_leak)
]

# Enforce ordered factor
cv_data[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
cv_data[, state   := factor(state)]

# Sanity checks — hard stop on any violation
stopifnot("has_previous_leak must be 0 for all rows" =
            all(cv_data$has_previous_leak == 0L))
stopifnot("is_make_model must be 1 for all rows" =
            all(cv_data$is_make_model == 1L))
stopifnot("No NA age_bin allowed — check AGE_BIN_LABELS vs panel labels" =
            !anyNA(cv_data$age_bin))
stopifnot("Only TX and control states should remain" =
            all(as.character(cv_data$state) %in% ALL_STUDY_STATES))

n_events   <- sum(cv_data$event_first_leak)
n_total    <- nrow(cv_data)
event_rate <- mean(cv_data$event_first_leak)

cat("Estimation sample (all states, pre-treatment):\n")
cat(sprintf("  Facility-years:    %s\n",   format(n_total,   big.mark = ",")))
cat(sprintf("  Unique facilities: %s\n",
    format(uniqueN(cv_data$panel_id), big.mark = ",")))
cat(sprintf("  States:            %d (TX + %d controls)\n",
    uniqueN(cv_data$state),
    uniqueN(cv_data[texas_treated == 0L, state])))
cat(sprintf("  Age bins:          %d levels (%s to %s)\n",
    nlevels(cv_data$age_bin),
    levels(cv_data$age_bin)[1],
    levels(cv_data$age_bin)[nlevels(cv_data$age_bin)]))
cat(sprintf("  First-leak events: %s (%.3f%% base rate)\n",
    format(n_events, big.mark = ","), event_rate * 100))
cat(sprintf("  Class ratio (0:1): %.0f:1\n",
    (n_total - n_events) / n_events))


#### S3: Class Weights ########################################################

n_0     <- sum(cv_data$event_first_leak == 0L)
n_1     <- sum(cv_data$event_first_leak == 1L)
w_ratio <- n_0 / n_1

cv_data[, case_weight := fifelse(event_first_leak == 1L, w_ratio, 1.0)]

cat(sprintf("\nClass weighting: w(1) = %.1f, w(0) = 1.0\n", w_ratio))
cat(sprintf("  Upweighting %s rare events by factor %.1fx\n",
    format(n_1, big.mark = ","), w_ratio))


#### S4: Fit GRF Probability Forest ###########################################
#
# Single model: facility-level observables + state.
# tune.parameters = "all" is NOT available for probability_forest.
# Manual 50-tree grid search used instead — see tuning function below.
# In TEST_MODE, tuning is skipped and BEST_PARAMS_FIXED is used directly.

FEATURES <- c(
  "has_single_walled",
  "age_bin_int",
  "active_tanks",
  "total_capacity",
  "has_gasoline_year",
  "has_diesel_year",
  "state_int"
)

# Encode ordered factor age_bin as integer (preserves ordinality for GRF splits)
cv_data[, age_bin_int := as.integer(age_bin)]
# Encode state as integer — used as both feature and cluster ID
cv_data[, state_int   := as.integer(factor(state))]

X  <- as.matrix(cv_data[, FEATURES, with = FALSE])
Y  <- as.integer(cv_data$event_first_leak)
W  <- cv_data$case_weight
CL <- cv_data$state_int   # cluster vector — one integer per row

cat(sprintf("\nGRF feature matrix: %d rows x %d cols\n", nrow(X), ncol(X)))
cat(sprintf("Features: %s\n", paste(FEATURES, collapse = ", ")))
cat(sprintf("Clusters (states): %d unique\n", uniqueN(CL)))

# Helper to safely read OOB error from either GRF slot
oob_error <- function(fit) {
  if (!is.null(fit$debiased.error))   return(round(fit$debiased.error,   5))
  if (!is.null(fit$prediction.error)) return(round(fit$prediction.error, 5))
  return(NA_real_)
}

# ---- Manual tuning grid -----------------------------------------------------
#
# tune.parameters = "all" is NOT available for probability_forest (only for
# regression_forest and causal_forest). Manual grid search required.
#
# Speed: n_trees_tune = 50 per combination — same as GRF's own internal tuner.
#   24 combinations × 50 trees = 1,200 tuning trees total.
#   Previous version used 500 × 24 = 12,000 — 10x slower for no ranking gain.
#   OOB Brier scores at 50 trees are noisy in absolute terms but stable enough
#   to rank combinations against each other.
#
# honesty = FALSE: pure prediction task, not causal inference.
#   GRF honesty halves each bootstrap sample into "splitting" and "estimation"
#   subsets. With only 20 state clusters, the estimation half gets ~3 states
#   per tree → empty leaves → NaN OOB predictions. Disabling honesty also
#   improves predictive AUC: all sampled observations inform both split
#   selection and leaf estimates.
#
# ci.group.size = 1L: GRF default (2) requires sample.fraction < 0.5 to
#   build paired trees for variance estimation. Setting to 1 removes that
#   constraint and allows sample.fraction = 0.7 in the grid.
#   Safe: ci.group.size only affects causal forest SEs.

tune_prob_forest <- function(X_mat, Y_vec, W_vec, CL_vec,
                             n_trees_tune = 50L) {
  p <- ncol(X_mat)
  mtry_candidates <- unique(c(
    max(1L, floor(p / 3L)),
    floor(sqrt(p)),
    3L, 4L, p
  ))
  grid <- expand.grid(
    mtry            = mtry_candidates,
    min.node.size   = c(5L, 20L),
    sample.fraction = c(0.5, 0.7)
  )

  best_err    <- Inf
  best_params <- NULL

  cat(sprintf("Tuning: %d combinations x %d trees = %d total tuning trees\n",
              nrow(grid), n_trees_tune, nrow(grid) * n_trees_tune))

  for (i in seq_len(nrow(grid))) {
    p_i      <- grid[i, ]
    fit_tune <- tryCatch(
      {
        set.seed(20260202L)
        probability_forest(
          X               = X_mat,
          Y               = factor(Y_vec, levels = c(0L, 1L)),
          num.trees       = n_trees_tune,
          sample.weights  = W_vec,
          clusters        = CL_vec,
          honesty         = FALSE,
          ci.group.size   = 1L,
          mtry            = p_i$mtry,
          min.node.size   = p_i$min.node.size,
          sample.fraction = p_i$sample.fraction,
          seed            = 20260202L
        )
      },
      error = function(e) {
        cat(sprintf("  [%2d/%d] ERROR: %s\n", i, nrow(grid),
                    conditionMessage(e)))
        NULL
      }
    )

    if (is.null(fit_tune)) next

    oob_preds <- get_p1(fit_tune$predictions)
    n_valid   <- sum(!is.na(oob_preds))
    if (n_valid == 0L) {
      cat(sprintf("  [%2d/%d] mtry=%d  min.node=%2d  frac=%.1f  Brier=NA (skipping)\n",
                  i, nrow(grid), p_i$mtry, p_i$min.node.size, p_i$sample.fraction))
      next
    }

    err <- mean((Y_vec - oob_preds)^2, na.rm = TRUE)
    cat(sprintf("  [%2d/%d] mtry=%d  min.node=%2d  frac=%.1f  Brier=%.5f\n",
                i, nrow(grid), p_i$mtry, p_i$min.node.size, p_i$sample.fraction, err))

    if (!is.nan(err) && err < best_err) {
      best_err    <- err
      best_params <- p_i
    }
  }

  if (is.null(best_params))
    stop("Tuning failed: all combinations errored or produced NaN Brier scores.")

  cat(sprintf(">> Best: mtry=%d  min.node=%d  frac=%.1f  Brier=%.5f\n",
              best_params$mtry, best_params$min.node.size,
              best_params$sample.fraction, best_err))
  return(best_params)
}

# ---- Tuning or inject pre-validated parameters ------------------------------
if (TEST_MODE) {
  cat("\n--- Tuning SKIPPED (TEST_MODE) — using pre-validated parameters ---\n")
  cat(sprintf("    mtry=%d  min.node.size=%d  sample.fraction=%.1f\n",
              BEST_PARAMS_FIXED$mtry,
              BEST_PARAMS_FIXED$min.node.size,
              BEST_PARAMS_FIXED$sample.fraction))
  best_params <- BEST_PARAMS_FIXED
} else {
  cat("\n--- Tuning ---\n")
  best_params <- tune_prob_forest(X, Y, W, CL)
}

cat(sprintf("\nFitting final model (%d trees)...\n", NUM_TREES))
set.seed(20260202L)
fit <- probability_forest(
  X               = X,
  Y               = factor(Y, levels = c(0L, 1L)),
  num.trees       = NUM_TREES,
  sample.weights  = W,
  clusters        = CL,
  honesty         = FALSE,
  ci.group.size   = 1L,
  mtry            = best_params$mtry,
  min.node.size   = best_params$min.node.size,
  sample.fraction = best_params$sample.fraction,
  seed            = 20260202L
)
cat(sprintf("  OOB error: %.5f\n", oob_error(fit)))

# Extract OOB predictions
cv_data[, pred := get_p1(fit$predictions)]

cat("\nPrediction summary:\n")
print(summary(cv_data$pred))


#### S5: Variable Importance ##################################################
#
# GRF importance: weighted frequency each variable was chosen for a split,
# weighted by node size. Returned as unnamed vector ordered by X column
# position — attach names manually.

vi  <- variable_importance(fit)
imp <- data.table(
  variable      = FEATURES,
  importance    = as.numeric(vi)
)
imp[, importance_pct := importance / sum(importance) * 100]
setorder(imp, -importance_pct)

fwrite(imp, file.path(OUTPUT_TABLES, "Table_CV_VarImportance.csv"))
cat("\nVariable importance:\n")
print(imp)

# Appendix figure — horizontal bar, sorted by importance
imp_fig <- copy(imp)
imp_fig[, variable := factor(variable, levels = rev(variable))]

fig_imp <- ggplot(imp_fig, aes(x = importance_pct, y = variable)) +
  geom_col(fill = COL_TX, alpha = 0.85, width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", importance_pct)),
            hjust = -0.15, size = 3.0, color = "grey20") +
  scale_x_continuous(
    name   = "Importance (% of total split weight)",
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(y = NULL) +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "FigureA_CV_VarImportance.png"),
       fig_imp, width = 7, height = 4.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "FigureA_CV_VarImportance.pdf"),
       fig_imp, width = 7, height = 4.5, device = cairo_pdf)
cat("  FigureA_CV_VarImportance saved\n")


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
  geom_line(data  = roc_dt, aes(x = fpr, y = tpr),
            color = COL_TX, linewidth = 0.9) +
  geom_point(data = roc_dt, aes(x = fpr, y = tpr),
             shape = 1, size = 1.0, stroke = 0.4,
             color = COL_TX, fill = NA) +
  annotate("label",
           x = 0.65, y = 0.10,
           label         = sprintf("AUC = %.3f", auc_val),
           size          = 3.2,
           fill          = "white",
           color         = "grey20",
           label.size    = 0.25,
           label.padding = unit(0.22, "lines")) +
  coord_fixed() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.25)) +
  labs(x = "False Positive Rate (1 \u2212 Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_ROC.png"),
       fig_roc, width = 5.5, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_ROC.pdf"),
       fig_roc, width = 5.5, height = 5.5, device = cairo_pdf)
cat("  Figure_CV_ROC saved\n")


#### S7: Partial Dependence Figure ############################################
#
# Marginal PDP: for each (has_single_walled, age_bin_int) cell, override
# those two columns across ALL rows of the estimation sample and call
# predict() on the counterfactual dataset. All other covariates — including
# state_int, active_tanks, total_capacity, fuel type — remain at each
# facility's observed value. Average over the full empirical distribution.
#
# This answers the marginal question: "if every facility were forced to be
# single-walled and 6-8 years old, what would their average predicted risk
# be?" It avoids conflating the age/wall gradient with other covariates that
# happen to co-occur in those cells (which a simple group-and-average of OOB
# predictions would not).

# AGE_BIN_LABELS is defined in S1 of this script. The integer encoding fed
# to the model during training (age_bin_int) uses the same ordering, so
# pd_grid$age_bin_int indexes directly into AGE_BIN_LABELS.
#
# NOTE: estimate.variance = TRUE requires honesty = TRUE and ci.group.size > 1
# in GRF. We disabled both to avoid NaN OOB predictions from 20 state clusters.
# Variance bands are therefore not available for this PDP. Point estimates only.

pd_grid <- CJ(
  has_single_walled = c(0L, 1L),
  age_bin_int       = seq_along(AGE_BIN_LABELS)
)

cat(sprintf("\nComputing marginal PDP over %d cells x %d observations...\n",
    nrow(pd_grid), nrow(cv_data)))

pd_rows <- lapply(seq_len(nrow(pd_grid)), function(i) {
  # Counterfactual: fix wall and age bin; keep all other covariates observed
  tmp <- copy(cv_data[, FEATURES, with = FALSE])
  tmp[, has_single_walled := pd_grid$has_single_walled[i]]
  tmp[, age_bin_int       := pd_grid$age_bin_int[i]]
  X_tmp <- as.matrix(tmp)

  preds <- predict(fit, newdata = X_tmp)
  p_hat <- get_p1(preds$predictions)

  data.table(
    has_single_walled = pd_grid$has_single_walled[i],
    age_bin           = AGE_BIN_LABELS[pd_grid$age_bin_int[i]],
    pd_mean           = mean(p_hat, na.rm = TRUE),
    pd_per1k          = mean(p_hat, na.rm = TRUE) * 1000
  )
})
pd_dt <- rbindlist(pd_rows)

# Observed cell rates — saved for appendix, NOT shown in main figure
# (main figure shows marginal model predictions only)
obs_dt <- cv_data[, .(
  obs_mean    = mean(event_first_leak, na.rm = TRUE),
  obs_per1k   = mean(event_first_leak, na.rm = TRUE) * 1000,
  n_fac_years = .N,
  n_events    = sum(event_first_leak)
), by = .(has_single_walled, age_bin = as.character(age_bin))]

pd_dt <- merge(pd_dt, obs_dt,
               by = c("has_single_walled", "age_bin"), all.x = TRUE)

pd_dt[, wall_label := factor(
  fifelse(has_single_walled == 1L, "Single-Walled", "Double-Walled"),
  levels = c("Single-Walled", "Double-Walled")
)]
pd_dt[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]

fwrite(pd_dt, file.path(OUTPUT_TABLES, "Table_CV_CellRates.csv"))
cat("  Table_CV_CellRates saved\n")

# --- Figure ---
pd_colors <- c("Single-Walled" = COL_TX,   "Double-Walled" = COL_CTRL)
pd_lty    <- c("Single-Walled" = "solid",  "Double-Walled" = "dashed")

# Nudge per-1k labels above/below points to avoid overlap with each other
pd_dt[, label_vjust := fifelse(wall_label == "Single-Walled", -0.8, 1.6)]

fig_pd <- ggplot(
  pd_dt[!is.na(pd_mean)],
  aes(x        = age_bin,
      y        = pd_per1k,
      group    = wall_label,
      color    = wall_label,
      linetype = wall_label)
) +
  geom_line(linewidth = 0.9) +
  geom_point(shape = 19, size = 2.8) +
  geom_text(
    aes(label = sprintf("%.1f", pd_per1k), vjust = label_vjust),
    size        = 2.7,
    fontface    = "bold",
    show.legend = FALSE
  ) +
  scale_color_manual(values = pd_colors, name = "Wall Type") +
  scale_linetype_manual(values = pd_lty, name = "Wall Type") +
  scale_y_continuous(
    name   = "Predicted First-Leak Rate (per 1,000 Facility-Years)",
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  labs(
    title    = "Predicted Leak Risk by Tank Age and Wall Type",
    subtitle = sprintf(
      "%d\u2013%d. Never-leaked risk set. GRF probability forest, OOB predictions.",
      min(cv_data$panel_year), max(cv_data$panel_year)
    ),
    x = "Tank Age Bin (3-year intervals)"
  ) +
  guides(
    color    = guide_legend(
      override.aes   = list(shape = 19, linetype = c("solid", "dashed")),
      title.position = "top", title.hjust = 0.5
    ),
    linetype = "none"
  ) +
  theme_pub() +
  theme(
    axis.text.x      = element_text(angle = 30, hjust = 1),
    legend.position  = "bottom",
    legend.key.width = unit(1.8, "cm"),
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(size = rel(0.8), color = "grey30")
  )

ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_PartialDep.png"),
       fig_pd, width = 9, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_PartialDep.pdf"),
       fig_pd, width = 9, height = 6, device = cairo_pdf)
cat("  Figure_CV_PartialDep saved\n")


#### S8: Calibration ##########################################################
#
# Decile calibration: bin OOB predicted scores into deciles, plot mean
# observed release rate against mean predicted score within each bin.
# Points near the 45-degree line confirm the model produces reliable
# probability estimates, not just a ranking.

make_cal_dt <- function(pred_col) {
  dt     <- cv_data[!is.na(get(pred_col)) & !is.na(event_first_leak)]
  breaks <- unique(quantile(dt[[pred_col]],
                            probs = seq(0, 1, 0.1), na.rm = TRUE))
  if (length(breaks) < 3L) {
    warning("Insufficient score variation for calibration")
    return(NULL)
  }
  dt[, decile := cut(get(pred_col), breaks = breaks,
                     include.lowest = TRUE, labels = FALSE)]
  cal <- dt[!is.na(decile), .(
    mean_pred = mean(get(pred_col),    na.rm = TRUE),
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
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey55", linewidth = 0.5) +
    geom_line(color = COL_TX, linewidth = 0.6) +
    geom_point(aes(size = n_fac_yrs),
               color = COL_TX, shape = 19, alpha = 0.85) +
    geom_text(aes(label = decile),
              vjust = -0.9, size = 2.5, color = "grey30") +
    coord_fixed(xlim = c(0, xy_max), ylim = c(0, xy_max)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    scale_size_continuous(range = c(2, 7), guide = "none") +
    labs(x = "Mean Predicted Probability",
         y = "Mean Observed Release Rate") +
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
  dt[, cum_pct_events   := cumsum(event_first_leak) /
                            sum(event_first_leak, na.rm = TRUE)]

  top10 <- dt[cum_pct_screened <= 0.101, max(cum_pct_events, na.rm = TRUE)]
  top20 <- dt[cum_pct_screened <= 0.201, max(cum_pct_events, na.rm = TRUE)]

  cat(sprintf("  [%s] Top 10%%: %.0f%% events | Top 20%%: %.0f%% events\n",
              label, top10 * 100, top20 * 100))

  lift_line <- rbind(
    data.table(x = 0, y = 0, type = label),
    dt[, .(x = cum_pct_screened, y = cum_pct_events, type = label)],
    data.table(x = c(0, 1), y = c(0, 1), type = "Random")
  )
  lift_pts <- dt[, .(x = cum_pct_screened, y = cum_pct_events)]

  fig_lift <- ggplot() +
    geom_line(data = lift_line,
              aes(x = x, y = y, color = type, linetype = type),
              linewidth = 0.9) +
    geom_point(data  = lift_pts, aes(x = x, y = y),
               shape = 1, size = 1.6, stroke = 0.55,
               color = COL_TX, fill = NA) +
    geom_segment(aes(x = 0.10, xend = 0.10, y = 0, yend = top10),
                 linetype = "dotted", color = "grey50", linewidth = 0.4) +
    geom_segment(aes(x = 0, xend = 0.10, y = top10, yend = top10),
                 linetype = "dotted", color = "grey50", linewidth = 0.4) +
    annotate("text",
             x     = 0.12, y = top10 * 0.88, hjust = 0,
             size  = 2.8, color = "grey30",
             label = sprintf("Top 10%%:\n%.0f%% of releases captured",
                             top10 * 100)) +
    scale_color_manual(
      values = setNames(c(COL_TX, "grey55"), c(label, "Random"))) +
    scale_linetype_manual(
      values = setNames(c("solid", "dashed"), c(label, "Random"))) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x     = "Fraction of Facilities Screened (descending risk)",
         y     = "Cumulative Fraction of Releases Captured",
         color = NULL, linetype = NULL) +
    theme_pub() +
    theme(legend.position = "bottom")

  ggsave(file.path(OUTPUT_FIGURES,
                   sprintf("FigureA_CV_Lift_%s.png", file_suffix)),
         fig_lift, width = 6, height = 5.5, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES,
                   sprintf("FigureA_CV_Lift_%s.pdf", file_suffix)),
         fig_lift, width = 6, height = 5.5, device = cairo_pdf)

  dt[, outcome_label := fifelse(event_first_leak == 1L,
                                "Confirmed Release", "No Release")]
  x_cap <- quantile(dt[[pred_col]], 0.99, na.rm = TRUE)

  fig_sep <- ggplot(
    dt[get(pred_col) <= x_cap],
    aes(x = get(pred_col), fill = outcome_label, color = outcome_label)
  ) +
    geom_density(alpha = 0.35, linewidth = 0.7) +
    scale_fill_manual(values = c(
      "Confirmed Release" = COL_TX,
      "No Release"        = COL_CTRL)) +
    scale_color_manual(values = c(
      "Confirmed Release" = COL_TX,
      "No Release"        = COL_CTRL)) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 0.01),
      limits = c(0, x_cap)) +
    labs(x = "Predicted Annual Release Probability",
         y = "Density", fill = NULL, color = NULL) +
    theme_pub() +
    theme(legend.position = "bottom")

  ggsave(file.path(OUTPUT_FIGURES,
                   sprintf("FigureA_CV_ScoreSep_%s.png", file_suffix)),
         fig_sep, width = 6, height = 4.5, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES,
                   sprintf("FigureA_CV_ScoreSep_%s.pdf", file_suffix)),
         fig_sep, width = 6, height = 4.5, device = cairo_pdf)

  invisible(list(top10 = top10, top20 = top20))
}

cat("\nAppendix figures:\n")
app <- make_appendix_figs("pred", "GRF Probability Forest", "State")


#### S10: GoF Summary Table ###################################################

gof_summary <- data.table(
  Model            = "GRF Probability Forest (with State)",
  AUC_OOB          = round(auc_val,        3),
  Top10_pct_events = round(app$top10 * 100, 1),
  Top20_pct_events = round(app$top20 * 100, 1),
  N_fac_years      = n_total,
  N_events         = n_events,
  Base_rate_pct    = round(event_rate * 100, 3),
  Class_weight     = round(w_ratio, 1),
  Test_mode        = TEST_MODE,
  Num_trees        = NUM_TREES
)
fwrite(gof_summary, file.path(OUTPUT_TABLES, "Table_CV_GoF_Summary.csv"))
cat("\nGoF summary:\n")
print(gof_summary)


#### S11: Risk Score Outputs ##################################################
#
# Two objects saved:
#
# (A) analysis_cv_data_fac_year.rds
#     Grain:    panel_id x panel_year (pre-treatment years only)
#     Join key: panel_id + panel_year
#     Use:      year-by-year risk controls in 02a/02b analysis
#
# (B) analysis_cv_data_fac.rds
#     Grain:    panel_id (one row per facility)
#     Join key: panel_id
#     Use:      fixed pre-reform risk covariate in HTE analysis
#     Snapshot: SNAP_YEAR (1998). Facilities not observed in 1998 fall back
#               to the most recent available pre-treatment year.

# ---- (A) Facility-year ----
cv_scores_fy <- cv_data[, .(
  panel_id,
  panel_year,
  state,
  texas_treated,
  risk_score = pred
)]
saveRDS(cv_scores_fy,
        file.path(ANALYSIS_DIR, "analysis_cv_data_fac_year.rds"))
cat(sprintf("\nSaved: analysis_cv_data_fac_year.rds  (%s rows)\n",
    format(nrow(cv_scores_fy), big.mark = ",")))
cat("  Join key: panel_id + panel_year\n")

# ---- (B) Facility-level snapshot ----
snap <- cv_data[panel_year == SNAP_YEAR, .(
  panel_id,
  state,
  texas_treated,
  risk_score = pred,
  snap_year  = panel_year
)]

# Fallback for facilities not observed in SNAP_YEAR
missing_ids <- setdiff(unique(cv_data$panel_id), snap$panel_id)
if (length(missing_ids) > 0L) {
  cat(sprintf("  %s facilities missing in %d — using last pre-reform obs\n",
      format(length(missing_ids), big.mark = ","), SNAP_YEAR))
  fallback <- cv_data[
    panel_id %in% missing_ids,
    .SD[which.max(panel_year)],
    by      = panel_id,
    .SDcols = c("state", "texas_treated", "pred", "panel_year")
  ]
  setnames(fallback,
           c("pred", "panel_year"),
           c("risk_score", "snap_year"))
  snap <- rbindlist(list(snap, fallback), use.names = TRUE, fill = TRUE)
}

# Risk quartile (pooled) for convenience in downstream HTE
q_cuts <- quantile(snap$risk_score,
                   probs = c(0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE)
snap[, risk_quartile := cut(
  risk_score,
  breaks         = unique(q_cuts),
  include.lowest = TRUE,
  labels         = c("Q1-Low", "Q2", "Q3", "Q4-High")[
    seq_len(length(unique(q_cuts)) - 1L)]
)]

saveRDS(snap, file.path(ANALYSIS_DIR, "analysis_cv_data_fac.rds"))
cat(sprintf("Saved: analysis_cv_data_fac.rds         (%s rows)\n",
    format(nrow(snap), big.mark = ",")))
cat(sprintf("  Join key: panel_id  |  snapshot year: %d\n", SNAP_YEAR))
cat(sprintf("  TX: %s | Control: %s\n",
    format(sum(snap$texas_treated == 1L), big.mark = ","),
    format(sum(snap$texas_treated == 0L), big.mark = ",")))

# Distribution check: TX vs control
cat("\nRisk score by group (1998 snapshot):\n")
print(snap[, .(
  Mean   = round(mean(risk_score,           na.rm = TRUE), 4),
  Median = round(median(risk_score,         na.rm = TRUE), 4),
  P75    = round(quantile(risk_score, 0.75, na.rm = TRUE), 4),
  N      = .N
), by = .(Group = fifelse(texas_treated == 1L, "Texas", "Control"))])

ks <- ks.test(
  snap[texas_treated == 1L, risk_score],
  snap[texas_treated == 0L, risk_score]
)
cat(sprintf("KS test TX vs Control: D = %.4f, p = %.4f\n",
    ks$statistic, ks$p.value))


#### Summary ##################################################################

cat("\n========================================================\n")
if (TEST_MODE) {
  cat("01n COMPLETE  [TEST MODE — %d trees, tuning skipped]\n\n", NUM_TREES)
} else {
  cat("01n COMPLETE  [PRODUCTION — %d trees]\n\n", NUM_TREES)
}
cat(sprintf("  Sample:     %s facility-years | %s first-leak events (%.3f%% base rate)\n",
    format(n_total,  big.mark = ","),
    format(n_events, big.mark = ","),
    event_rate * 100))
cat(sprintf("  Class wt:   %.0f:1  (0s:1s) — minority class upweighted\n", w_ratio))
cat(sprintf("  Parameters: mtry=%d  min.node.size=%d  sample.fraction=%.1f\n",
    best_params$mtry, best_params$min.node.size, best_params$sample.fraction))
cat(sprintf("  Num trees:  %d\n", NUM_TREES))
cat(sprintf("  OOB AUC:    %.3f\n", auc_val))
cat(sprintf("  Top 10%%:    %.0f%% of releases captured\n", app$top10 * 100))
cat(sprintf("  Top 20%%:    %.0f%% of releases captured\n", app$top20 * 100))
cat("\n  Main figures:\n")
cat("    Figure_CV_ROC\n")
cat("    Figure_CV_PartialDep\n")
cat("    Figure_CV_Calibration\n")
cat("\n  Appendix figures:\n")
cat("    FigureA_CV_Lift_State\n")
cat("    FigureA_CV_ScoreSep_State\n")
cat("    FigureA_CV_VarImportance\n")
cat("\n  Risk score outputs (Data/Analysis/):\n")
cat("    analysis_cv_data_fac_year.rds   join on panel_id + panel_year\n")
cat("    analysis_cv_data_fac.rds        join on panel_id (1998 snapshot)\n")
cat("========================================================\n")