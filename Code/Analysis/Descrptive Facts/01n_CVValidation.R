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
# MODELS:
#   USE_GRF and USE_ELNET are independent switches. Either or both can be
#   set to TRUE. At least one must be TRUE.
#
#   USE_GRF:   GRF probability forest (grf package). Preferred for production.
#              Requires 2,000 trees for stable OOB predictions under clustered
#              sampling. NaN OOB errors occur at low tree counts because
#              cluster-based holdout leaves too few OOB trees per state.
#
#   USE_ELNET: Elastic net logistic regression (glmnet). Alpha and lambda
#              selected by grid search over k-fold CV deviance. OOS predictions
#              from cv.glmnet held-out folds (keep = TRUE). Stable at any
#              sample size. Use for smoke tests and specification checks.
#
#   When both are TRUE both models run in full. Each writes to its own
#   prediction column (pred_grf, pred_elnet). PRIMARY_MODEL then selects
#   which column feeds all downstream figures, tables, and risk score outputs.
#   Both prediction columns are saved in the risk score output files regardless
#   of which model is primary.
#
#   PRIMARY_MODEL: "GRF" or "ELNET". Must match an active model.
#
# CLASS IMBALANCE:
#   Release events are rare (~1-3% annual base rate). Without correction the
#   forest learns to predict the majority class (no release) everywhere.
#   Fix: case.weights = n_0/n_1 for all event == 1 rows, 1.0 for event == 0.
#   Each tree's bootstrap sample then sees roughly equal 0s and 1s, forcing
#   the forest to learn the discriminating signal in the minority class.
#   Note: this does NOT collapse predicted probabilities to 50/50.
#   The calibration plot (S8) verifies this empirically.
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
#   Set TEST_MODE = TRUE to skip GRF hyperparameter tuning and use
#   pre-validated parameters at a reduced tree count. The elastic net branch
#   is unaffected by TEST_MODE — it runs at full fidelity regardless.
#   Set TEST_MODE = FALSE for the full production GRF run (2,000 trees +
#   tuning).
#
# OUTPUTS
# -------
# Main figures (Output/Figures/):
#   Figure_CV_ROC               ROC curve
#   Figure_CV_PR                Precision-recall curve
#   Figure_CV_PartialDep        Marginal PD: predicted hazard by age x wall type
#   Figure_CV_Calibration       Calibration decile plot
#
# Appendix figures (Output/Figures/):
#   FigureA_CV_Lift_State       Lift curve
#   FigureA_CV_ScoreSep_State   Score separation density
#   FigureA_CV_VarImportance    Variable importance
#
# Tables (Output/Tables/):
#   Table_CV_GoF_Summary        AUC + lift summary (one row per active model)
#   Table_CV_Calibration        Decile calibration table
#   Table_CV_CellRates          Predicted + observed rates by (wall, age) cell
#   Table_CV_VarImportance      Variable importance scores
#
# Risk score outputs (Data/Analysis/):
#   analysis_cv_data_fac_year.rds   Facility-year predictions (panel_id x panel_year)
#   analysis_cv_data_fac.rds        Facility-level score at reform year (panel_id only)
#   Both files include pred_grf and/or pred_elnet columns for all active models,
#   plus risk_score pointing to the PRIMARY_MODEL column.
################################################################################


#### S1: Setup ################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(grf)
  library(glmnet)
  library(doParallel)
  library(PRROC)
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
POST_YEAR  <- 1999L
SNAP_YEAR  <- 1998L

# ---- State sample -----------------------------------------------------------
CONTROL_STATES <- c(
  "ME", "NM", "AR", "OK", "LA", "KS", "MT", "ID", "SD", "AL",
  "MN", "NC", "IL", "MA", "OH", "PA", "TN", "VA", "CO"
)
ALL_STUDY_STATES <- c("TX", CONTROL_STATES)

# ---- Make-model sample criteria ---------------------------------------------
MM_WALL_EXCLUDE <- "Unknown-Wall"
MM_FUEL_EXCLUDE <- "Unknown-Fuel"

# ---- Canonical age bins -----------------------------------------------------
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

# ---- Threading --------------------------------------------------------------
NUM_THREADS <- max(1L, parallel::detectCores() - 1L)

# ---- GRF constants ----------------------------------------------------------
NUM_TREES <- 2000L   # production forest size; TEST_MODE overrides to 200

# ==============================================================================
# MODEL SWITCHES — edit these three lines to control what runs
# ==============================================================================
USE_GRF       <- FALSE   # TRUE = run GRF probability forest
USE_ELNET     <- TRUE    # TRUE = run elastic net logistic regression
PRIMARY_MODEL <- "ELNET" # "GRF" or "ELNET" — feeds all figures and outputs
                         # must match an active model
# ==============================================================================

stopifnot(
  "At least one of USE_GRF / USE_ELNET must be TRUE" =
    USE_GRF || USE_ELNET,
  "PRIMARY_MODEL must be 'GRF' or 'ELNET'" =
    PRIMARY_MODEL %in% c("GRF", "ELNET"),
  "PRIMARY_MODEL = 'GRF' requires USE_GRF = TRUE" =
    !(PRIMARY_MODEL == "GRF"   && !USE_GRF),
  "PRIMARY_MODEL = 'ELNET' requires USE_ELNET = TRUE" =
    !(PRIMARY_MODEL == "ELNET" && !USE_ELNET)
)

# ---- Test mode --------------------------------------------------------------
# Affects GRF only: skips tuning and reduces tree count to 200.
# Elastic net is unaffected — it runs at full fidelity regardless.
TEST_MODE <- TRUE   # <-- flip to FALSE for production GRF run

BEST_PARAMS_FIXED <- list(
  mtry            = 7L,
  min.node.size   = 5L,
  sample.fraction = 0.7
)

if (TEST_MODE && USE_GRF) {
  NUM_TREES <- 200L
  cat("================================================================================\n")
  cat("*** TEST MODE ACTIVE (GRF) — tuning skipped, pre-validated parameters used ***\n")
  cat(sprintf("    mtry=%d  min.node.size=%d  sample.fraction=%.1f  Brier=%.5f\n",
              BEST_PARAMS_FIXED$mtry, BEST_PARAMS_FIXED$min.node.size,
              BEST_PARAMS_FIXED$sample.fraction, 0.18048))
  cat(sprintf("    Tree count reduced to %d.\n", NUM_TREES))
  cat("================================================================================\n\n")
}

cat(sprintf("Models active:   GRF=%s  ELNET=%s\n",
            USE_GRF, USE_ELNET))
cat(sprintf("Primary model:   %s\n", PRIMARY_MODEL))
cat(sprintf("Test mode (GRF): %s\n\n", TEST_MODE))

# ---- Safe GRF prediction extractor ------------------------------------------
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
  stop("Panel CSV not found: ", PANEL_CSV,
       "\nRun 10_Build_Annual_Panel_Optimized.R first.")

cat("=== 01n: Observable Risk — Actuarial Validation ===\n\n")
cat(sprintf("Loading: %s\n", PANEL_CSV))
annual_data <- fread(PANEL_CSV)
cat(sprintf("  Loaded: %s rows x %d cols | %d-%d\n",
    format(nrow(annual_data), big.mark = ","),
    ncol(annual_data),
    min(annual_data$panel_year),
    max(annual_data$panel_year)))


#### S2: Estimation Sample ####################################################

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
  stop("annual_data missing required columns: ",
       paste(missing_cols, collapse = ", "))

# ---- FILTER 1: State whitelist ----------------------------------------------
n_before_state  <- nrow(annual_data)
annual_data     <- annual_data[state %in% ALL_STUDY_STATES]
n_dropped_state <- n_before_state - nrow(annual_data)

cat(sprintf("\nState filter:\n"))
cat(sprintf("  Kept:    %s rows (%d states)\n",
    format(nrow(annual_data), big.mark = ","),
    uniqueN(annual_data$state)))
cat(sprintf("  Dropped: %s rows from states outside TX + controls\n",
    format(n_dropped_state, big.mark = ",")))
cat(sprintf("  States retained: %s\n",
    paste(sort(unique(annual_data$state)), collapse = ", ")))

missing_states <- setdiff(ALL_STUDY_STATES, unique(annual_data$state))
if (length(missing_states) > 0)
  warning("Expected states not found in panel: ",
          paste(missing_states, collapse = ", "))

# ---- Age bin ----------------------------------------------------------------
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

# ---- Derive is_make_model ---------------------------------------------------
annual_data[, is_make_model := as.integer(
  fac_wall != MM_WALL_EXCLUDE &
  fac_fuel != MM_FUEL_EXCLUDE &
  !is.na(age_bin)
)]
cat(sprintf("\nis_make_model: %s / %s facility-years classified (%.1f%%)\n",
    format(sum(annual_data$is_make_model == 1L), big.mark = ","),
    format(nrow(annual_data), big.mark = ","),
    mean(annual_data$is_make_model == 1L) * 100))

# ---- FILTERS 2-5 ------------------------------------------------------------
cv_data <- annual_data[
  panel_year        <  POST_YEAR    &
  has_previous_leak == 0L           &
  is_make_model     == 1L           &
  has_single_walled %in% c(0L, 1L)  &
  !is.na(active_tanks)              &
  !is.na(total_capacity)            &
  !is.na(event_first_leak)
]

cv_data[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
cv_data[, state   := factor(state)]

stopifnot(
  "has_previous_leak must be 0"       = all(cv_data$has_previous_leak == 0L),
  "is_make_model must be 1"           = all(cv_data$is_make_model == 1L),
  "No NA age_bin allowed"             = !anyNA(cv_data$age_bin),
  "Only study states should remain"   =
    all(as.character(cv_data$state) %in% ALL_STUDY_STATES)
)

n_events   <- sum(cv_data$event_first_leak)
n_total    <- nrow(cv_data)
event_rate <- mean(cv_data$event_first_leak)

cat("Estimation sample (all states, pre-treatment):\n")
cat(sprintf("  Facility-years:    %s\n", format(n_total,   big.mark = ",")))
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


#### S4: Model Training #######################################################
#
# USE_GRF and USE_ELNET are independent. Both can be TRUE simultaneously.
# Each active model writes to its own column: pred_grf or pred_elnet.
# PRIMARY_MODEL (set in S1) determines which column feeds all downstream
# figures, tables, and risk score outputs.
#
# When both are active, GoF statistics for both models are written to
# Table_CV_GoF_Summary (one row per model).

FEATURES <- c(
  "has_single_walled",
  "age_bin_int",
  "active_tanks",
  "total_capacity",
  "has_gasoline_year",
  "has_diesel_year",
  "state_int"
)

cv_data[, age_bin_int := as.integer(age_bin)]
cv_data[, state_int   := as.integer(factor(state))]

X  <- as.matrix(cv_data[, FEATURES, with = FALSE])
Y  <- as.integer(cv_data$event_first_leak)
W  <- cv_data$case_weight
CL <- cv_data$state_int

cat(sprintf("\nFeature matrix: %d rows x %d cols\n", nrow(X), ncol(X)))
cat(sprintf("Features: %s\n", paste(FEATURES, collapse = ", ")))

# Initialise prediction columns to NA so downstream code can always reference
# them even if a model is not active
cv_data[, pred_grf   := NA_real_]
cv_data[, pred_elnet := NA_real_]

# Storage for per-model metadata used in GoF table
model_meta <- list()

# ---- GRF branch -------------------------------------------------------------
if (USE_GRF) {

  cat(sprintf("\n--- GRF: Clusters (states): %d unique ---\n", uniqueN(CL)))

  oob_error <- function(fit) {
    if (!is.null(fit$debiased.error))   return(round(fit$debiased.error,   5))
    if (!is.null(fit$prediction.error)) return(round(fit$prediction.error, 5))
    return(NA_real_)
  }

  # ---- Manual tuning grid --------------------------------------------------
  # tune.parameters = "all" is NOT available for probability_forest.
  # 50-tree grid search — same cost as GRF's own internal tuner.
  # honesty = FALSE: avoids NaN OOB predictions from ~3 estimation-half
  #   states per tree when only 20 clusters are present.
  # ci.group.size = 1L: removes the sample.fraction < 0.5 constraint that
  #   would otherwise prevent testing frac = 0.7 in the grid.

  tune_prob_forest <- function(X_mat, Y_vec, W_vec, CL_vec,
                               n_trees_tune = 50L) {
    p <- ncol(X_mat)
    mtry_candidates <- unique(c(
      max(1L, floor(p / 3L)), floor(sqrt(p)), 3L, 4L, p
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
      fit_tune <- tryCatch({
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
          num.threads     = NUM_THREADS,
          seed            = 20260202L
        )
      }, error = function(e) {
        cat(sprintf("  [%2d/%d] ERROR: %s\n", i, nrow(grid),
                    conditionMessage(e)))
        NULL
      })
      if (is.null(fit_tune)) next
      oob_preds <- get_p1(fit_tune$predictions)
      if (sum(!is.na(oob_preds)) == 0L) {
        cat(sprintf(
          "  [%2d/%d] mtry=%d  min.node=%2d  frac=%.1f  Brier=NA (skipping)\n",
          i, nrow(grid), p_i$mtry, p_i$min.node.size, p_i$sample.fraction))
        next
      }
      err <- mean((Y_vec - oob_preds)^2, na.rm = TRUE)
      cat(sprintf(
        "  [%2d/%d] mtry=%d  min.node=%2d  frac=%.1f  Brier=%.5f\n",
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

  if (TEST_MODE) {
    cat("\n--- GRF tuning SKIPPED (TEST_MODE) — using pre-validated parameters ---\n")
    best_params_grf <- BEST_PARAMS_FIXED
  } else {
    cat("\n--- GRF tuning ---\n")
    best_params_grf <- tune_prob_forest(X, Y, W, CL)
  }

  cat(sprintf("\nFitting GRF probability forest (%d trees)...\n", NUM_TREES))
  set.seed(20260202L)
  fit_grf <- probability_forest(
    X               = X,
    Y               = factor(Y, levels = c(0L, 1L)),
    num.trees       = NUM_TREES,
    sample.weights  = W,
    clusters        = CL,
    honesty         = FALSE,
    ci.group.size   = 1L,
    mtry            = best_params_grf$mtry,
    min.node.size   = best_params_grf$min.node.size,
    sample.fraction = best_params_grf$sample.fraction,
    num.threads     = NUM_THREADS,
    seed            = 20260202L
  )
  cat(sprintf("  OOB error: %.5f\n", oob_error(fit_grf)))

  cv_data[, pred_grf := get_p1(fit_grf$predictions)]

  model_meta[["GRF"]] <- list(
    label = sprintf(
      "GRF Probability Forest (mtry=%d, min.node=%d, frac=%.1f, trees=%d)",
      best_params_grf$mtry, best_params_grf$min.node.size,
      best_params_grf$sample.fraction, NUM_TREES
    )
  )
  cat("  GRF predictions written to pred_grf\n")
}

# ---- Elastic net branch -----------------------------------------------------
if (USE_ELNET) {

  # SATURATED FEATURE MATRIX
  # The elastic net is fit on a rich, saturated model matrix. Regularisation
  # handles variable selection so there is no cost to including everything.
  # Includes:
  #   - Age bin as 8 dummies (9 bins, one absorbed as reference)
  #   - Full age_bin x has_single_walled interaction (different shape per wall)
  #   - State dummies (fixed effects, not integer encoding)
  #   - active_tanks, total_capacity, gasoline, diesel
  #   - All pairwise interactions among the continuous/binary controls
  # This lets the model recover the non-linear age gradient and wall-type
  # interaction from the data rather than imposing linearity.

  cat("\nBuilding saturated feature matrix for elastic net...\n")
  X_elnet <- model.matrix(
    ~ (factor(age_bin) + has_single_walled + active_tanks +
       total_capacity + has_gasoline_year + has_diesel_year)^2 +
      factor(state),
    data = as.data.frame(cv_data)
  )[, -1L]   # drop intercept — glmnet adds its own

  cat(sprintf("  Saturated matrix: %d rows x %d cols\n",
              nrow(X_elnet), ncol(X_elnet)))

  ELNET_FOLDS <- ifelse(TEST_MODE, 5L,  10L)
  ELNET_ALPHA <- if (TEST_MODE) c(0.0, 0.5, 1.0) else c(0.0, 0.25, 0.5, 0.75, 1.0)

  # Subsample for alpha tuning only in TEST_MODE.
  # 200k rows is enough to rank alpha values. Full data used for CV predictions.
  if (TEST_MODE) {
    set.seed(20260202L)
    tune_idx  <- sample(nrow(X_elnet), min(200000L, nrow(X_elnet)))
    X_tune    <- X_elnet[tune_idx, ]
    Y_tune    <- Y[tune_idx]
    W_tune    <- W[tune_idx]
    cat(sprintf(
      "  TEST_MODE: alpha tuning on %s-row subsample (full data used for predictions)\n",
      format(length(tune_idx), big.mark = ",")))
  } else {
    X_tune <- X_elnet
    Y_tune <- Y
    W_tune <- W
  }

  # Parallel CV across folds.
  elnet_cl <- makeCluster(NUM_THREADS)
  registerDoParallel(elnet_cl)
  on.exit(stopCluster(elnet_cl), add = TRUE)

  cat(sprintf(
    "\n--- Elastic net: tuning alpha over %d values, %d-fold CV, %d threads ---\n",
    length(ELNET_ALPHA), ELNET_FOLDS, NUM_THREADS))

  # Step 1: tune alpha by CV deviance on tuning sample
  alpha_results <- lapply(ELNET_ALPHA, function(a) {
    set.seed(20260202L)
    cv_fit <- cv.glmnet(
      x            = X_tune,
      y            = Y_tune,
      family       = "binomial",
      alpha        = a,
      weights      = W_tune,
      nfolds       = ELNET_FOLDS,
      type.measure = "deviance",
      parallel     = TRUE
    )
    best_dev <- min(cv_fit$cvm)
    cat(sprintf("  alpha=%.2f  lambda.min=%.6f  CV deviance=%.5f\n",
                a, cv_fit$lambda.min, best_dev))
    list(alpha = a, cv_fit = cv_fit, best_dev = best_dev)
  })

  best_alpha_idx <- which.min(sapply(alpha_results, `[[`, "best_dev"))
  best_alpha     <- alpha_results[[best_alpha_idx]]$alpha
  cat(sprintf("\n>> Best alpha: %.2f\n", best_alpha))

  # Step 2: full-data CV at best alpha for OOB predictions (keep = TRUE).
  # Every row gets a held-out linear predictor from the fold it was excluded
  # from. This is the elastic net analogue of GRF's OOB predictions.
  cat("  Fitting full-data CV at best alpha for OOS predictions...\n")
  set.seed(20260202L)
  best_cv_fit <- cv.glmnet(
    x            = X_elnet,
    y            = Y,
    family       = "binomial",
    alpha        = best_alpha,
    weights      = W,
    nfolds       = ELNET_FOLDS,
    type.measure = "deviance",
    keep         = TRUE,
    parallel     = TRUE
  )
  best_lambda <- best_cv_fit$lambda.min
  cat(sprintf("  lambda.min=%.6f\n", best_lambda))

  lambda_idx <- which.min(abs(best_cv_fit$lambda - best_lambda))
  lp_oos     <- best_cv_fit$fit.preval[, lambda_idx]
  pred_oos   <- 1 / (1 + exp(-lp_oos))

  # Platt scaling: inverse-frequency weighting shifts all probabilities toward
  # 0.5. Platt scaling fits an unweighted logistic regression of true labels on
  # the raw OOS linear predictor, mapping predictions back to the original
  # probability scale. Ranking is preserved exactly.
  cat("  Applying Platt scaling to correct probability scale...\n")
  platt_fit <- glm(Y ~ lp_oos, family = binomial)
  pred_cal  <- predict(platt_fit, type = "response")

  cv_data[, pred_elnet := pred_cal]

  cat(sprintf("  Pre-scaling  median: %.5f  (true rate: %.5f)\n",
              median(pred_oos), event_rate))
  cat(sprintf("  Post-scaling median: %.5f  (true rate: %.5f)\n",
              median(pred_cal), event_rate))

  # Step 3: refit on full data at best alpha + lambda for variable importance.
  set.seed(20260202L)
  fit_elnet <- glmnet(
    x       = X_elnet,
    y       = Y,
    family  = "binomial",
    alpha   = best_alpha,
    weights = W,
    lambda  = best_lambda
  )

  best_params_elnet <- list(alpha = best_alpha, lambda = best_lambda)

  model_meta[["ELNET"]] <- list(
    label = sprintf(
      "Elastic Net Logistic Saturated (alpha=%.2f, lambda=%.6f, folds=%d, cols=%d)",
      best_alpha, best_lambda, ELNET_FOLDS, ncol(X_elnet)
    )
  )
  cat("  Elastic net predictions written to pred_elnet\n")
}

# ---- Set primary prediction column ------------------------------------------
# cv_data$pred is the single column consumed by all downstream sections.
# PRIMARY_MODEL controls which model's predictions are used.

pred_col_primary <- ifelse(PRIMARY_MODEL == "GRF", "pred_grf", "pred_elnet")
cv_data[, pred := get(pred_col_primary)]

cat(sprintf("\nPrimary predictions: %s -> pred\n", pred_col_primary))
cat("Prediction summary:\n")
print(summary(cv_data$pred))
cat(sprintf("  NA predictions: %d\n", sum(is.na(cv_data$pred))))

# ---- Generic predict helper -------------------------------------------------
# Used by S7 (PDP). Dispatches to the PRIMARY_MODEL's fitted object.

predict_prob <- function(newdata_mat) {
  if (PRIMARY_MODEL == "GRF")
    return(get_p1(predict(fit_grf, newdata = newdata_mat)$predictions))
  if (PRIMARY_MODEL == "ELNET")
    return(as.numeric(predict(fit_elnet, newx = newdata_mat,
                              type = "response")))
}


#### S5: Variable Importance ##################################################
#
# Importance is computed for the PRIMARY_MODEL only.
# GRF:        weighted split frequency (variable_importance()).
# Elastic net: absolute standardised coefficient magnitude.

if (PRIMARY_MODEL == "GRF") {
  imp_vals <- as.numeric(variable_importance(fit_grf))
}

if (PRIMARY_MODEL == "ELNET") {
  # Standardise X_elnet for comparable coefficient magnitudes.
  # Importance = absolute standardised coefficient, summed to 100%.
  X_elnet_scaled <- scale(X_elnet)
  fit_scaled <- glmnet(
    x       = X_elnet_scaled,
    y       = Y,
    family  = "binomial",
    alpha   = best_params_elnet$alpha,
    weights = W,
    lambda  = best_params_elnet$lambda
  )
  imp_vals_raw <- abs(as.numeric(coef(fit_scaled))[-1L])
  # Summarise back to the original seven feature groups for readability.
  # Column names of X_elnet begin with the feature name — map by prefix.
  col_names <- colnames(X_elnet)
  imp_by_col <- data.table(col = col_names, imp = imp_vals_raw)
  imp_by_col[, feature := fcase(
    grepl("age_bin",          col), "age_bin",
    grepl("has_single_walled",col), "has_single_walled",
    grepl("active_tanks",     col), "active_tanks",
    grepl("total_capacity",   col), "total_capacity",
    grepl("has_gasoline_year",col), "has_gasoline_year",
    grepl("has_diesel_year",  col), "has_diesel_year",
    grepl("state",            col), "state",
    default = "other"
  )]
  imp_grouped <- imp_by_col[, .(importance = sum(imp)), by = feature]
  imp_vals    <- imp_grouped[match(
    c(FEATURES[FEATURES != "state_int"], "state"), imp_grouped$feature
  ), importance]
  imp_vals[is.na(imp_vals)] <- 0
}

imp <- data.table(variable = FEATURES, importance = imp_vals)
imp[, importance_pct := importance / sum(importance) * 100]
setorder(imp, -importance_pct)

fwrite(imp, file.path(OUTPUT_TABLES, "Table_CV_VarImportance.csv"))
cat("\nVariable importance:\n")
print(imp)

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
           size          = 3.2, fill = "white", color = "grey20",
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


#### S6b: Precision-Recall Curve ##############################################
#
# PR curve is preferred over ROC for rare-event settings. The no-skill
# baseline equals the marginal event rate, making lift above it directly
# interpretable. PRROC convention: scores.class0 = positive class scores,
# scores.class1 = negative class scores.

pr_obj <- pr.curve(
  scores.class0 = cv_data[event_first_leak == 1L, pred],
  scores.class1 = cv_data[event_first_leak == 0L, pred],
  curve         = TRUE
)
pr_auc <- pr_obj$auc.integral

pr_dt <- as.data.table(pr_obj$curve)
setnames(pr_dt, c("recall", "precision", "threshold"))

fig_pr <- ggplot(pr_dt, aes(x = recall, y = precision)) +
  geom_hline(yintercept = event_rate, linetype = "dashed",
             color = "grey55", linewidth = 0.5) +
  geom_line(color = COL_TX, linewidth = 0.9) +
  annotate("label",
           x             = 0.65,
           y             = event_rate +
                           (max(pr_dt$precision) - event_rate) * 0.12,
           label         = sprintf("No-skill baseline: %.2f%%",
                                   event_rate * 100),
           size          = 2.8, fill = "white", color = "grey40",
           label.size    = 0.2,
           label.padding = unit(0.2, "lines")) +
  annotate("label",
           x             = 0.65,
           y             = max(pr_dt$precision) * 0.88,
           label         = sprintf("PR-AUC = %.3f", pr_auc),
           size          = 3.2, fill = "white", color = "grey20",
           label.size    = 0.25,
           label.padding = unit(0.22, "lines")) +
  scale_x_continuous(name   = "Recall (Sensitivity)",
                     labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_y_continuous(name   = "Precision (Positive Predictive Value)",
                     labels = scales::percent_format(accuracy = 0.1),
                     limits = c(0, NA),
                     expand = expansion(mult = c(0.02, 0.08))) +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_PR.png"),
       fig_pr, width = 5.5, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_PR.pdf"),
       fig_pr, width = 5.5, height = 5.5, device = cairo_pdf)
cat(sprintf("  Figure_CV_PR saved  (PR-AUC = %.3f)\n", pr_auc))


#### S7: Model-Smoothed OOB Cell Risk #########################################
#
# Two figures are produced:
#
# Figure_CV_CellRisk
#   What the model says: average OOB predicted probability within each
#   (age bin, wall type) cell, from the PRIMARY_MODEL. This is covariate-
#   adjusted (all other features held at their observed values) and out-of-
#   sample (each row predicted from folds it was excluded from).
#
# Figure_CV_CellRisk_GoF
#   Model vs data: model-implied cell risk overlaid with the raw empirical
#   cell mean. Points close to the 45-degree line = model recovers the
#   pattern in the data. This is cell-level goodness of fit. Shows the
#   model is not just discriminating (lift/AUC) but also correctly ranking
#   and calibrating risk across the actual rate cells insurers use.
#
# For GRF: pred_grf is used directly (forest OOB predictions).
# For ELNET: pred_elnet comes from the saturated model matrix with full
#   age_bin x wall type interactions + Platt scaling.
# In both cases no additional fitting is needed here.

cell_colors <- c("Single-Walled" = COL_TX,  "Double-Walled" = COL_CTRL)
cell_lty    <- c("Single-Walled" = "solid", "Double-Walled" = "dashed")

# ---- Observed cell means (always computed) ----------------------------------
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
cat(sprintf("\nObserved cell rates computed: %d cells\n", nrow(obs_dt)))

# ---- Model-implied cell means from PRIMARY_MODEL OOB predictions -----------
cell_dt <- cv_data[!is.na(pred), .(
  model_per1k = mean(pred, na.rm = TRUE) * 1000,
  n_fac_years = .N,
  n_events    = sum(event_first_leak)
), by = .(has_single_walled, age_bin)]

cell_dt[, wall_label := factor(
  fifelse(has_single_walled == 1L, "Single-Walled", "Double-Walled"),
  levels = c("Single-Walled", "Double-Walled")
)]
cell_dt[, age_bin     := factor(age_bin, levels = AGE_BIN_LABELS)]
cell_dt[, label_vjust := fifelse(wall_label == "Single-Walled", -0.8, 1.6)]

fwrite(cell_dt, file.path(OUTPUT_TABLES, "Table_CV_CellRates_Model.csv"))

# ---- Figure 1: Model-implied OOB cell risk ----------------------------------
fig_cell_risk <- ggplot(
  cell_dt[!is.na(model_per1k)],
  aes(x = age_bin, y = model_per1k,
      group = wall_label, color = wall_label, linetype = wall_label)
) +
  geom_line(linewidth = 0.9) +
  geom_point(shape = 19, size = 2.8) +
  geom_text(aes(label = sprintf("%.1f", model_per1k), vjust = label_vjust),
            size = 2.7, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = cell_colors, name = "Wall Type") +
  scale_linetype_manual(values = cell_lty,  name = "Wall Type") +
  scale_y_continuous(
    name   = "OOB Predicted First-Leak Rate (per 1,000 Facility-Years)",
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  labs(x = "Tank Age Bin (3-year intervals)") +
  guides(
    color    = guide_legend(
      override.aes   = list(shape = 19, linetype = c("solid", "dashed")),
      title.position = "top", title.hjust = 0.5),
    linetype = "none"
  ) +
  theme_pub() +
  theme(axis.text.x      = element_text(angle = 30, hjust = 1),
        legend.position  = "bottom",
        legend.key.width = unit(1.8, "cm"))

ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_CellRisk.png"),
       fig_cell_risk, width = 9, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_CellRisk.pdf"),
       fig_cell_risk, width = 9, height = 6, device = cairo_pdf)
cat("  Figure_CV_CellRisk saved\n")

# ---- Figure 2: Model vs observed — cell-level GoF --------------------------
# Merge model and observed on the same cell keys.
# Each point is one (age bin, wall type) cell.
# Points on the 45-degree line = model cell risk matches observed cell rate.
gof_dt <- merge(
  cell_dt[, .(has_single_walled, age_bin, wall_label, model_per1k, n_fac_years)],
  obs_dt[ , .(has_single_walled, age_bin, obs_per1k)],
  by = c("has_single_walled", "age_bin")
)

xy_max_gof <- max(gof_dt$model_per1k, gof_dt$obs_per1k, na.rm = TRUE) * 1.08
xy_min_gof <- 0

fig_cell_gof <- ggplot(
  gof_dt,
  aes(x = obs_per1k, y = model_per1k,
      color = wall_label, shape = wall_label, size = n_fac_years)
) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey55", linewidth = 0.5) +
  geom_point(alpha = 0.85) +
  geom_text(aes(label = as.character(age_bin)),
            vjust = -0.9, size = 2.4, color = "grey30",
            show.legend = FALSE) +
  scale_color_manual(values = cell_colors, name = "Wall Type") +
  scale_shape_manual(values = c("Single-Walled" = 19, "Double-Walled" = 17),
                     name = "Wall Type") +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  coord_fixed(xlim = c(xy_min_gof, xy_max_gof),
              ylim = c(xy_min_gof, xy_max_gof)) +
  scale_x_continuous(
    name   = "Observed First-Leak Rate (per 1,000 Facility-Years)"
  ) +
  scale_y_continuous(
    name   = "OOB Predicted First-Leak Rate (per 1,000 Facility-Years)"
  ) +
  guides(
    color = guide_legend(
      override.aes   = list(shape = c(19, 17), size = 3),
      title.position = "top", title.hjust = 0.5)
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_CellRisk_GoF.png"),
       fig_cell_gof, width = 6, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_CellRisk_GoF.pdf"),
       fig_cell_gof, width = 6, height = 6, device = cairo_pdf)
cat("  Figure_CV_CellRisk_GoF saved\n")

# ---- Appendix: raw observed cell rates (no model) --------------------------
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
  scale_linetype_manual(values = cell_lty,  name = "Wall Type") +
  scale_y_continuous(
    name   = "Observed First-Leak Rate (per 1,000 Facility-Years)",
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  labs(x = "Tank Age Bin (3-year intervals)") +
  guides(
    color    = guide_legend(
      override.aes   = list(shape = 19, linetype = c("solid", "dashed")),
      title.position = "top", title.hjust = 0.5),
    linetype = "none"
  ) +
  theme_pub() +
  theme(axis.text.x      = element_text(angle = 30, hjust = 1),
        legend.position  = "bottom",
        legend.key.width = unit(1.8, "cm"))

ggsave(file.path(OUTPUT_FIGURES, "FigureA_CV_ObsCellRates.png"),
       fig_obs, width = 9, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "FigureA_CV_ObsCellRates.pdf"),
       fig_obs, width = 9, height = 6, device = cairo_pdf)
cat("  FigureA_CV_ObsCellRates saved (raw observed cell means, appendix)\n")


#### S8: Calibration ##########################################################

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
    geom_point(data = lift_pts, aes(x = x, y = y),
               shape = 1, size = 1.6, stroke = 0.55,
               color = COL_TX, fill = NA) +
    geom_segment(aes(x = 0.10, xend = 0.10, y = 0, yend = top10),
                 linetype = "dotted", color = "grey50", linewidth = 0.4) +
    geom_segment(aes(x = 0, xend = 0.10, y = top10, yend = top10),
                 linetype = "dotted", color = "grey50", linewidth = 0.4) +
    annotate("text",
             x = 0.12, y = top10 * 0.88, hjust = 0,
             size = 2.8, color = "grey30",
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
    scale_fill_manual(values = c("Confirmed Release" = COL_TX,
                                 "No Release"        = COL_CTRL)) +
    scale_color_manual(values = c("Confirmed Release" = COL_TX,
                                  "No Release"        = COL_CTRL)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.01),
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
app <- make_appendix_figs("pred", model_meta[[PRIMARY_MODEL]]$label, "State")


#### S10: GoF Summary Table ###################################################
#
# One row per active model. Primary model flagged explicitly.

compute_gof <- function(pred_col, label) {
  roc_i <- pROC::roc(cv_data$event_first_leak,
                     cv_data[[pred_col]], quiet = TRUE)
  auc_i <- as.numeric(pROC::auc(roc_i))
  pr_i  <- pr.curve(
    scores.class0 = cv_data[event_first_leak == 1L, get(pred_col)],
    scores.class1 = cv_data[event_first_leak == 0L, get(pred_col)],
    curve         = FALSE
  )
  dt_i <- cv_data[!is.na(get(pred_col)) & !is.na(event_first_leak)]
  setorderv(dt_i, pred_col, order = -1L)
  dt_i[, cum_pct_screened := seq_len(.N) / .N]
  dt_i[, cum_pct_events   := cumsum(event_first_leak) /
                              sum(event_first_leak, na.rm = TRUE)]
  t10 <- dt_i[cum_pct_screened <= 0.101, max(cum_pct_events, na.rm = TRUE)]
  t20 <- dt_i[cum_pct_screened <= 0.201, max(cum_pct_events, na.rm = TRUE)]
  data.table(
    Model            = label,
    Primary          = pred_col == pred_col_primary,
    AUC              = round(auc_i,    3),
    PR_AUC           = round(pr_i$auc.integral, 3),
    Top10_pct_events = round(t10 * 100, 1),
    Top20_pct_events = round(t20 * 100, 1),
    N_fac_years      = n_total,
    N_events         = n_events,
    Base_rate_pct    = round(event_rate * 100, 3),
    Class_weight     = round(w_ratio, 1),
    Test_mode        = TEST_MODE
  )
}

gof_rows <- list()
if (USE_GRF)   gof_rows[["GRF"]]   <- compute_gof("pred_grf",
                                                    model_meta$GRF$label)
if (USE_ELNET) gof_rows[["ELNET"]] <- compute_gof("pred_elnet",
                                                    model_meta$ELNET$label)

gof_summary <- rbindlist(gof_rows)
fwrite(gof_summary, file.path(OUTPUT_TABLES, "Table_CV_GoF_Summary.csv"))
cat("\nGoF summary:\n")
print(gof_summary)


#### S11: Risk Score Outputs ##################################################
#
# Both pred_grf and pred_elnet are saved in the output files when active,
# regardless of PRIMARY_MODEL. risk_score always points to PRIMARY_MODEL.

# ---- (A) Facility-year ----
score_cols <- c("panel_id", "panel_year", "state", "texas_treated")
if (USE_GRF)   score_cols <- c(score_cols, "pred_grf")
if (USE_ELNET) score_cols <- c(score_cols, "pred_elnet")

cv_scores_fy <- cv_data[, score_cols, with = FALSE]
cv_scores_fy[, risk_score := get(pred_col_primary)]

saveRDS(cv_scores_fy,
        file.path(ANALYSIS_DIR, "analysis_cv_data_fac_year.rds"))
cat(sprintf("\nSaved: analysis_cv_data_fac_year.rds  (%s rows)\n",
    format(nrow(cv_scores_fy), big.mark = ",")))
cat("  Join key: panel_id + panel_year\n")

# ---- (B) Facility-level snapshot ----
snap_cols <- c("panel_id", "state", "texas_treated")
if (USE_GRF)   snap_cols <- c(snap_cols, "pred_grf")
if (USE_ELNET) snap_cols <- c(snap_cols, "pred_elnet")

snap <- cv_data[panel_year == SNAP_YEAR,
                c(snap_cols, "panel_year"), with = FALSE]
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
    by      = panel_id,
    .SDcols = setdiff(fallback_cols, "panel_id")
  ]
  setnames(fallback, "panel_year", "snap_year")
  snap <- rbindlist(list(snap, fallback), use.names = TRUE, fill = TRUE)
}

snap[, risk_score := get(pred_col_primary)]

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
cat(sprintf("01n COMPLETE  [%s]\n\n",
            ifelse(TEST_MODE, "TEST MODE", "PRODUCTION")))
cat(sprintf("  Models run:     GRF=%s  ELNET=%s\n", USE_GRF, USE_ELNET))
cat(sprintf("  Primary model:  %s\n", PRIMARY_MODEL))
cat(sprintf("  Sample:         %s facility-years | %s events (%.3f%% base rate)\n",
    format(n_total,  big.mark = ","),
    format(n_events, big.mark = ","),
    event_rate * 100))
cat(sprintf("  Class wt:       %.0f:1  (0s:1s)\n", w_ratio))
cat(sprintf("  Primary AUC:    %.3f\n", auc_val))
cat(sprintf("  Primary PR-AUC: %.3f\n", pr_auc))
cat(sprintf("  Top 10%%:        %.0f%% of releases captured\n", app$top10 * 100))
cat(sprintf("  Top 20%%:        %.0f%% of releases captured\n", app$top20 * 100))
cat("\n  Main figures:\n")
cat("    Figure_CV_ROC\n")
cat("    Figure_CV_PR\n")
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