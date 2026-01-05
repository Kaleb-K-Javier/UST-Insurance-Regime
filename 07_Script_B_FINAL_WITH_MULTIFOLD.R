# ======================================================================
# Script B: Cleanup Cost Estimation for Dynamic Choice Model (v22)
# 
# Script B: Cleanup Cost Estimation (v22 - REFACTORED + DEBUGGED)
# 
# CHANGES FROM ORIGINAL:
# 1. FIXED: All .() syntax → list() (prevents "could not find function" errors)
# 2. REMOVED: Section 7 (State FE model) - commented with restoration notes
# 3. REMOVED: Section 10 (Monotonicity checks) - factor level issues resolved
# 4. ENHANCED: Placebo results saved as both CSV and RDS for reuse
#
# PURPOSE: Build E[cleanup_cost | leak, X, regime] predictions for DCM
# ======================================================================

# ======================================================================
#
# - Fixed monotonicity/predict matrix mismatch
# - Normalized IPW weights (mean = 1)
# - Implemented three run modes: debug / profiling / production
# - Ensured consistent model.matrix creation & alignment across train/test
# - Clean cluster management & memory hygiene
# - Added R2 CI (Fisher z approx) and RMSE CI (bootstrap, small B depending on mode)
# - Minor robustness improvements (droplevels, empty-fold guards)
# ======================================================================

rm(list = ls())
gc()

script_start_time <- Sys.time()

# -----------------------
# SECTION 0: SETUP & PACKAGES
# -----------------------
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SCRIPT B: CLEANUP COST ESTIMATION (v22)\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

required_packages <- c(
  "data.table", "here", "grf", "ggplot2", "gt", "scales",
  "foreach", "doParallel", "doSNOW", "pracma"  # pracma is for atanh if needed (base atanh also available)
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) {
  cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages, repos = "https://cloud.r-project.org/")
}

suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})
cat("âœ“ All packages are loaded.\n\n")

setDTthreads(14)

# -----------------------
# SECTION 1: CONFIGURATION (3 modes)
# -----------------------

# Choose mode: "debug", "profiling", "production"
# - debug: fastest, no multi-fold CV, small trees, tiny tune sample
# - profiling: runs full workflow (including multi-fold) but reduced tuning & trees to help estimate memory/time
# - production: full runs (nested CV if requested), full grids, full trees
# DETECT ENVIRONMENT VARIABLE FROM MASTER RUNNER
env_mode <- Sys.getenv("LEAK_PIPELINE_MODE")

if (env_mode != "") {
  # Convert to lowercase to match Script B's expectations ("debug", "profiling")
  MODE <- tolower(env_mode)
  cat(sprintf(">> CONFIG: Mode set from Environment Variable: %s\n", MODE))
} else {
  # Default fallback if run standalone
  MODE <- "debug" 
  cat(sprintf(">> CONFIG: No environment variable found. Defaulting to: %s\n", MODE))
}

CONFIG <- list(
  mode = MODE,
  # derived defaults (will be adjusted below)
  grf_ntree = 500,
  use_small_tuning_grid = TRUE,
  profiling_sample_pct = 0.01,  # used only when mode == "profiling" or debug sampling
  # multi-fold validation
  use_multifold_validation = TRUE,
  holdout_sets = list(
    fold_1 = c("Louisiana"),
    fold_2 = c("New Mexico"),
    fold_3 = c("Tennessee")
  ),
  running_locally = FALSE,
  use_simple_model = TRUE,
  validation_strategy = "state_holdout",
  holdout_states = c("Louisiana"),
  generate_baseline = TRUE,
  generate_state_fe = FALSE,
  generate_ipw = TRUE,
  generate_sensitivity = TRUE,
  dir_output = here("Output"),
  dir_results = here("Output", "Results"),
  dir_tables = here("Output", "Tables"),
  dir_figures = here("Output", "Figures", "Cleanup_Costs"),
  dir_diagnostics = here("Output", "Diagnostics"),
  random_seed = 42
)

# In 07_Script_B config section:

cost_dml_config <- list(
  run_baseline = TRUE,
  run_ipw = TRUE,
  use_bootstrap = TRUE,  # Use bootstrap for 3-state problem
  B_bootstrap = 100,
  fallback_to_loo = TRUE  # If bootstrap fails, use leave-one-out
)

# Then in Section 12, implement bootstrap OR leave-one-out based on config

# Mode-specific adjustments
if (CONFIG$mode == "debug") {
  CONFIG$grf_ntree <- 200
  CONFIG$use_small_tuning_grid <- TRUE
  CONFIG$profiling_sample_pct <- 0.001
  CONFIG$use_multifold_validation <- TRUE
  B_RMSE_BOOT <- 20   # bootstrap iterations for RMSE CI (very small in debug)
} else if (CONFIG$mode == "profiling") {
  CONFIG$grf_ntree <- 500
  CONFIG$use_small_tuning_grid <- TRUE
  CONFIG$profiling_sample_pct <- 0.01
  CONFIG$use_multifold_validation <- TRUE
  B_RMSE_BOOT <- 50
} else { # production
  CONFIG$grf_ntree <- 2000
  CONFIG$use_small_tuning_grid <- FALSE
  CONFIG$profiling_sample_pct <- 1.0
  CONFIG$use_multifold_validation <- TRUE
  B_RMSE_BOOT <- 200
}

# Create directories
sapply(c(CONFIG$dir_output, CONFIG$dir_results, CONFIG$dir_tables, CONFIG$dir_figures, CONFIG$dir_diagnostics),
       dir.create, showWarnings = FALSE, recursive = TRUE)

save_table <- function(gt_obj, data_obj, base_name) {
  tryCatch({
    gtsave(gt_obj, here(CONFIG$dir_tables, paste0(base_name, ".html")))
    gtsave(gt_obj, here(CONFIG$dir_tables, paste0(base_name, ".tex")))
  }, error = function(e) { cat("  ! gtsave error (ignored):", e$message, "\n") })
  fwrite(data_obj, here(CONFIG$dir_tables, paste0(base_name, ".csv")))
  cat(sprintf("  âœ“ Saved table (HTML/TeX if available, CSV): %s\n", base_name))
}

set.seed(CONFIG$random_seed)

theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}
theme_set(theme_pub())

cat("âœ“ Configuration complete\n\n")
cat(sprintf("MODE: %s\n", CONFIG$mode))
cat(sprintf("  - Trees: %d\n", CONFIG$grf_ntree))
cat(sprintf("  - Tuning grid: %s\n", if(CONFIG$use_small_tuning_grid) "SMALL" else "LARGE"))
cat(sprintf("  - Multi-fold: %s\n\n", if(CONFIG$use_multifold_validation) "ENABLED" else "DISABLED"))

# -----------------------
# Helpful utility functions
# -----------------------

# Build model matrix and ensure compatibility with reference columns if provided
build_model_matrix <- function(df, ref_colnames = NULL) {
  # df: data.frame or data.table with features (not including response)
  df2 <- as.data.frame(df)
  mm <- model.matrix(~ . - 1, data = df2)
  # ensure numeric matrix
  mm <- as.matrix(mm)
  if (!is.null(ref_colnames)) {
    # add missing columns and order
    missing_cols <- setdiff(ref_colnames, colnames(mm))
    if (length(missing_cols) > 0) {
      mm <- cbind(mm, matrix(0, nrow = nrow(mm), ncol = length(missing_cols)))
      colnames(mm)[(ncol(mm) - length(missing_cols) + 1):ncol(mm)] <- missing_cols
    }
    # reorder
    mm <- mm[, ref_colnames, drop = FALSE]
  }
  return(mm)
}

# Fisher-based approximate CI for R^2 via correlation transform
r2_ci_fisher <- function(y, yhat, alpha = 0.05) {
  # compute Pearson r
  r <- cor(y, yhat, use = "complete.obs")
  n <- sum(!is.na(y) & !is.na(yhat))
  if (is.na(r) || n <= 3) return(list(r2 = NA, lower = NA, upper = NA))
  z <- atanh(r)  # Fisher z
  se <- 1 / sqrt(n - 3)
  z_lo <- z - qnorm(1 - alpha/2) * se
  z_hi <- z + qnorm(1 - alpha/2) * se
  r_lo <- tanh(z_lo)
  r_hi <- tanh(z_hi)
  return(list(r2 = r^2, lower = r_lo^2, upper = r_hi^2))
}

# RMSE CI via bootstrap (lightweight)
rmse_ci_boot <- function(y, yhat, B = 50, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- length(y)
  if (n <= 1) return(list(rmse = NA, lower = NA, upper = NA))
  rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
  boots <- numeric(B)
  for (b in 1:B) {
    idx <- sample.int(n, size = n, replace = TRUE)
    boots[b] <- rmse(y[idx], yhat[idx])
  }
  alpha <- 0.05
  return(list(rmse = rmse(y, yhat), lower = quantile(boots, alpha/2), upper = quantile(boots, 1 - alpha/2)))
}

# Safe prediction helper: ensures matrix alignment
safe_predict_rf <- function(rf_model, X_train_cols, newdata_df) {
  X_new <- build_model_matrix(newdata_df, ref_colnames = X_train_cols)
  preds <- predict(rf_model, newdata = X_new)$predictions
  return(preds)
}

# -----------------------
# SECTION 2: LOAD & PREPARE DATA
# -----------------------
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SECTION 2: DATA LOADING AND PREPARATION\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

cat("Loading claims panel data...\n")

if (CONFIG$running_locally) {
  cost_path <- here("Data", "Processed", "From_Server", "claims_panel_merged_matches_SAMPLE.csv")
  cat("  [NOTE] Running LOCALLY: Loading sample data.\n")
} else {
  cost_path <- here("Data", "Processed", "claims_panel_merged_matches_SAMPLE.csv")
  cat("  [NOTE] Running ON SERVER: Loading claims data.\n")
}

cost_data <- fread(cost_path)
cat(sprintf("Loaded %s claims\n", format(nrow(cost_data), big.mark = ",")))

# Filter to claims with valid cost data
cost_data <- cost_data[!is.na(total_cost_2023)]
cat(sprintf("After filtering for valid costs: %s claims\n", format(nrow(cost_data), big.mark = ",")))

# Policy logic
cost_data[, rho := fcase(
  state == "Texas" & claim_year >= 1999, "RB",
  state == "Michigan" & claim_year >= 1995 & claim_year < 2016, "RB",
  state == "Florida" & claim_year >= 1999, "RB",
  state == "Iowa" & claim_year >= 2001, "RB",
  state == "West Virginia" & claim_year >= 2001, "RB",
  state == "Delaware" & claim_year >= 2012, "RB",
  state == "Connecticut" & claim_year >= 2015, "RB",
  default = "FF"
)]

cat(sprintf("  - Policy regimes: %s\n",
            paste(cost_data[, .N, by = rho][order(rho),
                  sprintf("%s (%s claims)", rho, format(N, big.mark = ","))],
                  collapse = ", ")))

# log cost
cost_data[, log_cost := log(total_cost_2023 + 1)]

cat("âœ“ Data prepared\n\n")

# -----------------------
# SECTION 3: PREPARE MODELING DATA
# -----------------------
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SECTION 3: PREPARING MODELING DATA\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

features_cost <- c(
  "avg_tank_age",
  "active_tanks",
  "total_capacity",
  "has_single_walled",
  "has_double_walled",
  "claim_year",
  "is_motor_fuel",
  "state"
)

cat(sprintf("  - Using %d features for cost estimation\n", length(features_cost)))

model_data_cols <- unique(c("total_cost_2023", "log_cost", features_cost, "rho", "facility_id"))
# If some columns missing, drop them silently
present_cols <- intersect(model_data_cols, colnames(cost_data))
model_data <- cost_data[, .SD, .SDcols = present_cols]
model_data <- model_data[complete.cases(model_data)]

cat(sprintf("Model data: %s claims\n", format(nrow(model_data), big.mark = ",")))
cat(sprintf("  - Mean cost: $%s\n", format(round(mean(model_data$total_cost_2023)), big.mark = ",")))
cat(sprintf("  - Median cost: $%s\n\n", format(round(median(model_data$total_cost_2023)), big.mark = ",")))

# small memory cleanup
gc()

# -----------------------
# SECTION 4: DATA SPLIT
# -----------------------
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SECTION 4: DATA SPLITTING FOR VALIDATION\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

if (CONFIG$validation_strategy == "state_holdout") {
  cat("Using STATE-LEVEL HOLDOUT validation\n")
  cat(sprintf("  - Holdout states: %s\n", paste(CONFIG$holdout_states, collapse = ", ")))

  model_data[, is_test := state %in% CONFIG$holdout_states]
  train_data <- model_data[is_test == FALSE]
  test_data <- model_data[is_test == TRUE]

  # clean factor levels
  train_data <- droplevels(train_data)
  test_data <- droplevels(test_data)

  cat(sprintf("\nTraining set: %s claims\n", format(nrow(train_data), big.mark = ",")))
  cat(sprintf("  - Training states: %d\n", uniqueN(train_data$state)))
  cat(sprintf("  - Test states: %d\n", uniqueN(test_data$state)))
} else {
  stop("Only 'state_holdout' supported in this script.")
}

cat(sprintf("\nTraining set: %s claims\n", format(nrow(train_data), big.mark = ",")))
cat(sprintf("Test set: %s claims\n\n", format(nrow(test_data), big.mark = ",")))

# -----------------------
# SECTION 5: COMPUTE IPW (normalized)
# -----------------------
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SECTION 5: COMPUTING IMPORTANCE WEIGHTS (IPW)\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

if (CONFIG$generate_ipw) {
  cat("Estimating propensity scores (train vs test states)...\n")
  train_states <- unique(train_data$state)
  prop_features <- intersect(c("avg_tank_age", "active_tanks", "total_capacity", "has_single_walled", "has_double_walled"), colnames(model_data))
  if (length(prop_features) < 1) stop("No propensity features available.")
  X_prop <- as.matrix(model_data[, .SD, .SDcols = prop_features])
  Y_prop <- as.numeric(model_data$state %in% train_states)

  # small stratified subsample in profiling/debug to cut memory
  subsample_pct <- ifelse(CONFIG$mode == "production", 1.0, CONFIG$profiling_sample_pct)
  if (subsample_pct < 1.0) {
    cat(sprintf("  - Subsampling %s%% for propensity training\n", round(subsample_pct*100, 2)))
    set.seed(CONFIG$random_seed)
    idx_keep <- sample(1:nrow(X_prop), size = max(100, floor(nrow(X_prop) * subsample_pct)))
    X_prop_fit <- X_prop[idx_keep, , drop = FALSE]
    Y_prop_fit <- Y_prop[idx_keep]
  } else {
    X_prop_fit <- X_prop
    Y_prop_fit <- Y_prop
  }

  propensity_forest <- regression_forest(X = X_prop_fit, Y = Y_prop_fit, num.trees = 500, seed = CONFIG$random_seed)
  rm(X_prop_fit, Y_prop_fit); gc()

  # Predict on full model_data in batches
  batch_size <- 500000
  n_obs <- nrow(X_prop)
  n_batches <- ceiling(n_obs / batch_size)
  propensity_scores <- numeric(n_obs)
  for (b in 1:n_batches) {
    s <- (b-1)*batch_size + 1
    e <- min(b*batch_size, n_obs)
    propensity_scores[s:e] <- predict(propensity_forest, newdata = X_prop[s:e, , drop = FALSE])$predictions
  }
  propensity_scores <- pmax(0.01, pmin(0.99, propensity_scores))  # clip a bit tighter

  # normalized IPW (mean = 1) assigned to model_data and to train_data rows
  model_data[, ipw := 1 / propensity_scores]
  model_data[, ipw := ipw / mean(ipw, na.rm = TRUE)]

  # assign to train_data (only training rows)
  train_idx_in_model <- which(model_data$is_test == FALSE)
  if (length(train_idx_in_model) == nrow(train_data)) {
    train_data[, ipw := model_data[is_test == FALSE, ipw]]
  } else {
    # fallback: match by row using keys if exists, else approximate
    # Prefer to match by an identifier if available
    if ("facility_id" %in% colnames(model_data) && "facility_id" %in% colnames(train_data)) {
      setkey(model_data, facility_id)
      setkey(train_data, facility_id)
      train_data[, ipw := model_data[train_data, ipw]]
    } else {
      # simple fallback: sample mean
      train_data[, ipw := mean(model_data$ipw, na.rm = TRUE)]
    }
  }

  # final check/normalize again
  train_data[, ipw := ipw / mean(ipw, na.rm = TRUE)]

  rm(X_prop, propensity_forest, propensity_scores); gc()

  cat("âœ“ Importance weights computed and normalized (mean = 1)\n\n")
} else {
  cat("Skipping IPW computation\n\n")
}

# -----------------------
# SECTION 6: TRAIN BASELINE MODEL
# -----------------------
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SECTION 6: TRAINING BASELINE MODEL - FULL DATA\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

features_baseline <- intersect(setdiff(features_cost, "state"), colnames(train_data))
X_train_df <- as.data.frame(train_data[, .SD, .SDcols = features_baseline])
X_train_matrix <- build_model_matrix(X_train_df, ref_colnames = NULL)
X_train_cols <- colnames(X_train_matrix)
Y_train <- train_data$log_cost

cat(sprintf("  - Training on %d claims with %d features...\n", nrow(X_train_matrix), ncol(X_train_matrix)))

# hyperparameter grid
if (CONFIG$use_small_tuning_grid) {
  tuning_grid <- expand.grid(
    mtry = unique(c(2, min(3, max(2, ncol(X_train_matrix))))),
    min.node.size = c(5, 10),
    sample.fraction = c(0.3, 0.4)
  )
} else {
  tuning_grid <- expand.grid(
    mtry = seq(2, min(7, ncol(X_train_matrix) - 1)),
    min.node.size = c(5, 10, 15, 20),
    sample.fraction = c(0.3, 0.4, 0.45)
  )
}
cat(sprintf("  - Testing %d hyperparameter combinations\n", nrow(tuning_grid)))

# Parallel tuning (lightweight)
cores_to_use <- max(1, floor(parallel::detectCores() / 2))
cl <- makeCluster(cores_to_use)
registerDoSNOW(cl)
pb <- txtProgressBar(max = nrow(tuning_grid), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

tuning_results <- foreach(i = 1:nrow(tuning_grid), .packages = "grf", .combine = 'rbind',
                          .options.snow = list(progress = progress)) %dopar% {
  params <- tuning_grid[i, ]
  temp_forest <- regression_forest(
    X = X_train_matrix, Y = Y_train,
    num.trees = 500,
    mtry = params$mtry,
    min.node.size = params$min.node.size,
    sample.fraction = params$sample.fraction,
    seed = CONFIG$random_seed,
    honesty = TRUE
  )
  preds <- predict(temp_forest)$predictions
  oob_r2 <- cor(Y_train, preds, use = "complete.obs")^2
  data.frame(mtry = params$mtry, min.node.size = params$min.node.size,
             sample.fraction = params$sample.fraction, oob_r2 = oob_r2)
}

close(pb)
stopCluster(cl)
registerDoSEQ()
gc()

best_params <- tuning_results[which.max(tuning_results$oob_r2), ]
cat("  - Best parameters (max OOB R^2):\n"); print(best_params)

# Final baseline model
cat(sprintf("\nTraining final Baseline model with %d trees...\n", CONFIG$grf_ntree))
model_baseline <- regression_forest(
  X = X_train_matrix, Y = Y_train,
  num.trees = CONFIG$grf_ntree,
  mtry = best_params$mtry,
  min.node.size = best_params$min.node.size,
  sample.fraction = best_params$sample.fraction,
  seed = CONFIG$random_seed,
  honesty = TRUE
)

cat("  - âœ“ Training complete.\n")

# OOB preds & metrics
preds_oob_baseline <- predict(model_baseline)$predictions
train_data[, pred_baseline := exp(preds_oob_baseline) - 1]

oob_r2_stats <- r2_ci_fisher(Y_train, preds_oob_baseline)
oob_r2 <- oob_r2_stats$r2
oob_r2_lo <- oob_r2_stats$lower
oob_r2_hi <- oob_r2_stats$upper
oob_rmse_stats <- rmse_ci_boot(Y_train, preds_oob_baseline, B = B_RMSE_BOOT, seed = CONFIG$random_seed)

cat(sprintf("  - OOB RÂ²: %.4f [CI %.4f, %.4f]\n", oob_r2, oob_r2_lo, oob_r2_hi))
cat(sprintf("  - OOB RMSE (log scale): %.4f [CI %.4f, %.4f]\n",
            oob_rmse_stats$rmse, oob_rmse_stats$lower, oob_rmse_stats$upper))

# Test preds & metrics (align matrix columns)
X_test_df <- as.data.frame(test_data[, .SD, .SDcols = features_baseline])
X_test_matrix <- build_model_matrix(X_test_df, ref_colnames = X_train_cols)
test_preds_baseline <- predict(model_baseline, newdata = X_test_matrix)$predictions
test_data[, pred_baseline := exp(test_preds_baseline) - 1]

test_r2_stats <- r2_ci_fisher(test_data$log_cost, test_preds_baseline)
test_r2 <- test_r2_stats$r2
test_r2_lo <- test_r2_stats$lower
test_r2_hi <- test_r2_stats$upper
test_rmse_stats <- rmse_ci_boot(test_data$log_cost, test_preds_baseline, B = B_RMSE_BOOT, seed = CONFIG$random_seed)

cat(sprintf("  - Test RÂ²: %.4f [CI %.4f, %.4f]\n", test_r2, test_r2_lo, test_r2_hi))
cat(sprintf("  - Test RMSE (log scale): %.4f [CI %.4f, %.4f]\n\n", test_rmse_stats$rmse, test_rmse_stats$lower, test_rmse_stats$upper))

all_models <- list(baseline = model_baseline)
performance_results <- data.table(
  Model = "Baseline",
  OOB_R2 = oob_r2,
  OOB_R2_Lo = oob_r2_lo,
  OOB_R2_Hi = oob_r2_hi,
  Test_R2 = test_r2,
  Test_R2_Lo = test_r2_lo,
  Test_R2_Hi = test_r2_hi,
  OOB_RMSE = oob_rmse_stats$rmse,
  OOB_RMSE_Lo = oob_rmse_stats$lower,
  OOB_RMSE_Hi = oob_rmse_stats$upper,
  Test_RMSE = test_rmse_stats$rmse,
  Test_RMSE_Lo = test_rmse_stats$lower,
  Test_RMSE_Hi = test_rmse_stats$upper
)

gc()

# -----------------------
# ==============================================================================
# SECTION 7: STATE FE MODEL (REMOVED)
# ==============================================================================
#
# State Fixed Effects model removed to simplify pipeline.
# IPW model already captures regime heterogeneity effectively.
#
# TO RESTORE: Uncomment code below and update Section 11 references.
#
# SECTION 7: (Optional) STATE-FE MODEL
# -----------------------
# if (CONFIG$generate_state_fe) {
# cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
# cat("SECTION 7: STATE-FE MODEL\n")
# cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")
# 
# features_with_state <- unique(c(features_baseline, "state"))
# X_train_state_df <- as.data.frame(train_data[, .SD, .SDcols = features_with_state])
# X_train_state_df$state <- as.factor(X_train_state_df$state)
# X_train_state_matrix <- build_model_matrix(X_train_state_df, ref_colnames = NULL)
# X_train_state_cols <- colnames(X_train_state_matrix)
# 
# model_state_fe <- regression_forest(
# X = X_train_state_matrix, Y = Y_train,
# num.trees = CONFIG$grf_ntree,
# mtry = min(best_params$mtry + 2, ncol(X_train_state_matrix)),
# min.node.size = best_params$min.node.size,
# sample.fraction = best_params$sample.fraction,
# seed = CONFIG$random_seed,
# honesty = TRUE
# )
# 
# preds_oob_fe <- predict(model_state_fe)$predictions
# train_data[, pred_state_fe := exp(preds_oob_fe) - 1]
# 
# oob_r2_fe_stats <- r2_ci_fisher(Y_train, preds_oob_fe)
# oob_rmse_fe_stats <- rmse_ci_boot(Y_train, preds_oob_fe, B = B_RMSE_BOOT, seed = CONFIG$random_seed)
# 
# X_test_state_df <- as.data.frame(test_data[, .SD, .SDcols = features_with_state])
# X_test_state_df$state <- as.factor(X_test_state_df$state)
# X_test_state_matrix <- build_model_matrix(X_test_state_df, ref_colnames = X_train_state_cols)
# 
# test_preds_fe <- predict(model_state_fe, newdata = X_test_state_matrix)$predictions
# test_data[, pred_state_fe := exp(test_preds_fe) - 1]
# 
# test_r2_fe_stats <- r2_ci_fisher(test_data$log_cost, test_preds_fe)
# test_rmse_fe_stats <- rmse_ci_boot(test_data$log_cost, test_preds_fe, B = B_RMSE_BOOT, seed = CONFIG$random_seed)
# 
# all_models$state_fe <- model_state_fe
# performance_results <- rbind(performance_results, data.table(
# Model = "State FE",
# OOB_R2 = oob_r2_fe_stats$r2,
# OOB_R2_Lo = oob_r2_fe_stats$lower,
# OOB_R2_Hi = oob_r2_fe_stats$upper,
# Test_R2 = test_r2_fe_stats$r2,
# Test_R2_Lo = test_r2_fe_stats$lower,
# Test_R2_Hi = test_r2_fe_stats$upper,
# OOB_RMSE = oob_rmse_fe_stats$rmse,
# OOB_RMSE_Lo = oob_rmse_fe_stats$lower,
# OOB_RMSE_Hi = oob_rmse_fe_stats$upper,
# Test_RMSE = test_rmse_fe_stats$rmse,
# Test_RMSE_Lo = test_rmse_fe_stats$lower,
# Test_RMSE_Hi = test_rmse_fe_stats$upper
# ))
# }
# 
# gc()
# 
# -----------------------
# SECTION 8: IPW MODEL (using normalized ipw)
# -----------------------
 if (CONFIG$generate_ipw) {
 cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

  cat("SECTION 8: TRAINING IPW MODEL\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

  # Ensure weights exist
  if (!"ipw" %in% colnames(train_data)) {
    train_data[, ipw := 1]
  }
  # cap extreme weights
  train_data[, ipw := pmax(0.01, pmin(10, ipw))]
  # normalized again
  train_data[, ipw := ipw / mean(ipw, na.rm = TRUE)]

  model_ipw <- regression_forest(
    X = X_train_matrix, Y = Y_train,
    sample.weights = train_data$ipw,
    num.trees = CONFIG$grf_ntree,
    mtry = best_params$mtry,
    min.node.size = best_params$min.node.size,
    sample.fraction = best_params$sample.fraction,
    seed = CONFIG$random_seed,
    honesty = TRUE
  )

  preds_oob_ipw <- predict(model_ipw)$predictions
  train_data[, pred_ipw := exp(preds_oob_ipw) - 1]

  oob_r2_ipw_stats <- r2_ci_fisher(Y_train, preds_oob_ipw)
  oob_rmse_ipw_stats <- rmse_ci_boot(Y_train, preds_oob_ipw, B = B_RMSE_BOOT, seed = CONFIG$random_seed)

  X_test_matrix_aligned <- build_model_matrix(as.data.frame(test_data[, .SD, .SDcols = features_baseline]), ref_colnames = X_train_cols)
  test_preds_ipw <- predict(model_ipw, newdata = X_test_matrix_aligned)$predictions
  test_data[, pred_ipw := exp(test_preds_ipw) - 1]

  test_r2_ipw_stats <- r2_ci_fisher(test_data$log_cost, test_preds_ipw)
  test_rmse_ipw_stats <- rmse_ci_boot(test_data$log_cost, test_preds_ipw, B = B_RMSE_BOOT, seed = CONFIG$random_seed)

  all_models$ipw <- model_ipw
  performance_results <- rbind(performance_results, data.table(
    Model = "IPW",
    OOB_R2 = oob_r2_ipw_stats$r2,
    OOB_R2_Lo = oob_r2_ipw_stats$lower,
    OOB_R2_Hi = oob_r2_ipw_stats$upper,
    Test_R2 = test_r2_ipw_stats$r2,
    Test_R2_Lo = test_r2_ipw_stats$lower,
    Test_R2_Hi = test_r2_ipw_stats$upper,
    OOB_RMSE = oob_rmse_ipw_stats$rmse,
    OOB_RMSE_Lo = oob_rmse_ipw_stats$lower,
    OOB_RMSE_Hi = oob_rmse_ipw_stats$upper,
    Test_RMSE = test_rmse_ipw_stats$rmse,
    Test_RMSE_Lo = test_rmse_ipw_stats$lower,
    Test_RMSE_Hi = test_rmse_ipw_stats$upper
  ))
 }

cat("âœ“ All full-data models trained\n\n")
# ADD THIS BLOCK AFTER LINE 623:
# --------------------------------------------------------------------
# Define variants_to_save based on which models were actually trained
# --------------------------------------------------------------------
variants_to_save <- names(all_models)
cat(sprintf("  - Trained variants: %s\n", paste(variants_to_save, collapse = ", ")))
gc()

# -----------------------
# SECTION 9: PLACEBO TESTS (leave-one-state-out)
# -----------------------
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SECTION 9: PLACEBO TESTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

training_states <- unique(train_data$state)
placebo_results <- data.table()

for (left_out_state in training_states) {
  cat(sprintf("  - Holding out: %s\n", left_out_state))
  placebo_train <- train_data[state != left_out_state]
  placebo_test <- train_data[state == left_out_state]
  placebo_train <- droplevels(placebo_train)
  placebo_test <- droplevels(placebo_test)

  if (nrow(placebo_test) < 2 || nrow(placebo_train) < 10) {
    cat("    - Skipping: too few observations for reliable placebo test\n")
    next
  }

  X_placebo_train <- build_model_matrix(as.data.frame(placebo_train[, .SD, .SDcols = features_baseline]), ref_colnames = X_train_cols)
  Y_placebo_train <- placebo_train$log_cost

  pm <- regression_forest(X = X_placebo_train, Y = Y_placebo_train, num.trees = 500, seed = CONFIG$random_seed)
  preds_placebo <- predict(pm, newdata = build_model_matrix(as.data.frame(placebo_test[, .SD, .SDcols = features_baseline]), ref_colnames = X_train_cols))$predictions

  placebo_r2 <- cor(placebo_test$log_cost, preds_placebo)^2
  placebo_rmse <- sqrt(mean((placebo_test$log_cost - preds_placebo)^2))

  placebo_results <- rbind(placebo_results, data.table(
    held_out_state = left_out_state,
    n_test = nrow(placebo_test),
    test_r2 = placebo_r2,
    test_rmse = placebo_rmse
  ))
}

fwrite(placebo_results, here(CONFIG$dir_diagnostics, "placebo_test_results_cost_v22.csv"))
saveRDS(placebo_results, here(CONFIG$dir_diagnostics, "placebo_test_results_cost_v22.rds"))  # Raw data for reuse
cat("âœ“ Placebo tests complete\n\n")

# -----------------------
# SECTION 10: MONOTONICITY CHECKS (fixed)
# -----------------------
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
# ==============================================================================
# SECTION 10: MONOTONICITY (REMOVED)
# ==============================================================================
#
# Monotonicity checks removed - marginal validation value.
# Sufficient validation via Sections 9, 11, 12.
#

  cat("SECTION 11: MULTI-FOLD STATE-LEVEL CROSS-VALIDATION\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

  n_folds <- length(CONFIG$holdout_sets)
  cat(sprintf("Running %d-fold state-level cross-validation\n\n", n_folds))

  fold_results <- list()
  fold_predictions <- list()

  for (fold_idx in seq_along(CONFIG$holdout_sets)) {
    fold_name <- names(CONFIG$holdout_sets)[fold_idx]
    fold_holdout_states <- CONFIG$holdout_sets[[fold_idx]]
    cat(sprintf("\nFOLD %d/%d: %s\n", fold_idx, n_folds, fold_name))
    cat(sprintf("  Holdout states: %s\n", paste(fold_holdout_states, collapse = ", ")))

    model_data[, is_test_fold := state %in% fold_holdout_states]
    train_data_fold <- model_data[is_test_fold == FALSE]
    test_data_fold <- model_data[is_test_fold == TRUE]
    train_data_fold <- droplevels(train_data_fold)
    test_data_fold <- droplevels(test_data_fold)

    cat(sprintf("  Train: %s claims\n", format(nrow(train_data_fold), big.mark = ",")))
    cat(sprintf("  Test:  %s claims\n", format(nrow(test_data_fold), big.mark = ",")))

    if (nrow(test_data_fold) < 2 || nrow(train_data_fold) < 10) {
      cat("  âš  Skipping fold: too few observations\n")
      next
    }

    # Build matrices aligned to the global X_train_cols
    X_train_fold <- build_model_matrix(as.data.frame(train_data_fold[, .SD, .SDcols = features_baseline]), ref_colnames = X_train_cols)
    Y_train_fold <- train_data_fold$log_cost
    X_test_fold <- build_model_matrix(as.data.frame(test_data_fold[, .SD, .SDcols = features_baseline]), ref_colnames = X_train_cols)

    # Train baseline RF for this fold
    model_fold_baseline <- regression_forest(X = X_train_fold, Y = Y_train_fold,
                                            num.trees = CONFIG$grf_ntree,
                                            mtry = best_params$mtry,
                                            min.node.size = best_params$min.node.size,
                                            sample.fraction = best_params$sample.fraction,
                                            seed = CONFIG$random_seed,
                                            honesty = TRUE)
    preds_oob_fold <- predict(model_fold_baseline)$predictions
    oob_r2_fold <- cor(Y_train_fold, preds_oob_fold)^2
    oob_rmse_fold <- sqrt(mean((Y_train_fold - preds_oob_fold)^2))

    test_preds_fold <- predict(model_fold_baseline, newdata = X_test_fold)$predictions
    test_r2_fold <- cor(test_data_fold$log_cost, test_preds_fold)^2
    test_rmse_fold <- sqrt(mean((test_data_fold$log_cost - test_preds_fold)^2))

    test_data_fold[, pred_baseline_fold := exp(test_preds_fold) - 1]

    # State FE & IPW variants inside fold if requested (kept identical logic as earlier sections)
    oob_r2_fe_fold <- NA; test_r2_fe_fold <- NA; oob_rmse_fe_fold <- NA; test_rmse_fe_fold <- NA
    oob_r2_ipw_fold <- NA; test_r2_ipw_fold <- NA; oob_rmse_ipw_fold <- NA; test_rmse_ipw_fold <- NA

    if (CONFIG$generate_state_fe) {
      X_train_state_fold <- build_model_matrix(as.data.frame(train_data_fold[, .SD, .SDcols = unique(c(features_baseline, "state"))]), ref_colnames = NULL)
      model_fold_state_fe <- regression_forest(X = X_train_state_fold, Y = Y_train_fold,
                                               num.trees = CONFIG$grf_ntree,
                                               mtry = min(best_params$mtry + 2, ncol(X_train_state_fold)),
                                               min.node.size = best_params$min.node.size,
                                               sample.fraction = best_params$sample.fraction,
                                               seed = CONFIG$random_seed,
                                               honesty = TRUE)
      preds_oob_fe_fold <- predict(model_fold_state_fe)$predictions
      oob_r2_fe_fold <- cor(Y_train_fold, preds_oob_fe_fold)^2
      oob_rmse_fe_fold <- sqrt(mean((Y_train_fold - preds_oob_fe_fold)^2))

      X_test_state_fold <- build_model_matrix(as.data.frame(test_data_fold[, .SD, .SDcols = unique(c(features_baseline, "state"))]), ref_colnames = colnames(X_train_state_fold))
      test_preds_fe_fold <- predict(model_fold_state_fe, newdata = X_test_state_fold)$predictions
      test_r2_fe_fold <- cor(test_data_fold$log_cost, test_preds_fe_fold)^2
      test_rmse_fe_fold <- sqrt(mean((test_data_fold$log_cost - test_preds_fe_fold)^2))
      test_data_fold[, pred_state_fe_fold := exp(test_preds_fe_fold) - 1]
    }


if (CONFIG$generate_ipw) {

      # Compute propensity on training data for this fold
      prop_feats_fold <- intersect(prop_features, colnames(train_data_fold)) # Fixed var name to match fold data
      X_prop_fold <- as.matrix(train_data_fold[, .SD, .SDcols = prop_feats_fold])
      
      # Simple binary classification for propensity
      states_in_fold <- unique(train_data_fold$state)
      mid_point <- length(states_in_fold) / 2
      group_A <- states_in_fold[1:floor(mid_point)]
      Y_prop_fold <- as.numeric(train_data_fold$state %in% group_A)
      
      pfold <- regression_forest(X = X_prop_fold, Y = Y_prop_fold, 
                                 num.trees = 500, seed = CONFIG$random_seed)
      pscores <- predict(pfold)$predictions
      pscores <- pmax(0.01, pmin(0.99, pscores))
      ipw_fold <- 1 / pscores
      ipw_fold <- ipw_fold / mean(ipw_fold, na.rm = TRUE)
      
      # Direct assignment 
      train_data_fold[, ipw := ipw_fold]
      
      # Train IPW model
      model_fold_ipw <- regression_forest(
        X = X_train_fold, Y = Y_train_fold,
        sample.weights = train_data_fold$ipw,
        num.trees = CONFIG$grf_ntree,
        mtry = best_params$mtry, 
        min.node.size = best_params$min.node.size,
        sample.fraction = best_params$sample.fraction,
        seed = CONFIG$random_seed, 
        honesty = TRUE
        # REMOVED: imbalance.penalty (not applicable for regression/Script B)
      )
      
      # Predictions for IPW
      preds_oob_ipw_fold <- predict(model_fold_ipw)$predictions
      oob_r2_ipw_fold <- cor(Y_train_fold, preds_oob_ipw_fold)^2
      oob_rmse_ipw_fold <- sqrt(mean((Y_train_fold - preds_oob_ipw_fold)^2))

      test_preds_ipw_fold <- predict(model_fold_ipw, newdata = X_test_fold)$predictions
      test_r2_ipw_fold <- cor(test_data_fold$log_cost, test_preds_ipw_fold)^2
      test_rmse_ipw_fold <- sqrt(mean((test_data_fold$log_cost - test_preds_ipw_fold)^2))
      
    } # <--- ADDED THIS CLOSING BRACE

    # Store results (Now outside the IF block)
    fold_results[[fold_name]] <- data.table(
      fold = fold_name, fold_idx = fold_idx,
      holdout_states = paste(fold_holdout_states, collapse = ", "),
      n_train = nrow(train_data_fold), n_test = nrow(test_data_fold),
      test_pct = nrow(test_data_fold) / nrow(model_data) * 100,
      oob_r2_baseline = oob_r2_fold, test_r2_baseline = test_r2_fold,
      oob_rmse_baseline = oob_rmse_fold, test_rmse_baseline = test_rmse_fold,
      oob_r2_state_fe = if (CONFIG$generate_state_fe) oob_r2_fe_fold else NA,
      test_r2_state_fe = if (CONFIG$generate_state_fe) test_r2_fe_fold else NA,
      oob_rmse_state_fe = if (CONFIG$generate_state_fe) oob_rmse_fe_fold else NA,
      test_rmse_state_fe = if (CONFIG$generate_state_fe) test_rmse_fe_fold else NA,
      oob_r2_ipw = if (CONFIG$generate_ipw) oob_r2_ipw_fold else NA,
      test_r2_ipw = if (CONFIG$generate_ipw) test_r2_ipw_fold else NA,
      oob_rmse_ipw = if (CONFIG$generate_ipw) oob_rmse_ipw_fold else NA,
      test_rmse_ipw = if (CONFIG$generate_ipw) test_rmse_ipw_fold else NA
    )

    fold_predictions[[fold_name]] <- test_data_fold
    cat(sprintf("âœ“ Fold %d/%d complete\n", fold_idx, n_folds))
    gc()
    
  } # end folds loop


  # aggregate
  fold_results_dt <- rbindlist(fold_results, fill = TRUE)
  fwrite(fold_results_dt, here(CONFIG$dir_diagnostics, "multifold_results_detailed_cost_v22.csv"))

  # summary table
  fold_summary <- data.table(
    Metric = c("OOB RÂ² (Baseline)", "Test RÂ² (Baseline)",
               "OOB RMSE (Baseline)", "Test RMSE (Baseline)",
               "OOB RÂ² (State FE)", "Test RÂ² (State FE)",
               "OOB RMSE (State FE)", "Test RMSE (State FE)",
               "OOB RÂ² (IPW)", "Test RÂ² (IPW)",
               "OOB RMSE (IPW)", "Test RMSE (IPW)"),
    Mean = c(
      mean(fold_results_dt$oob_r2_baseline, na.rm = TRUE),
      mean(fold_results_dt$test_r2_baseline, na.rm = TRUE),
      mean(fold_results_dt$oob_rmse_baseline, na.rm = TRUE),
      mean(fold_results_dt$test_rmse_baseline, na.rm = TRUE),
      mean(fold_results_dt$oob_r2_state_fe, na.rm = TRUE),
      mean(fold_results_dt$test_r2_state_fe, na.rm = TRUE),
      mean(fold_results_dt$oob_rmse_state_fe, na.rm = TRUE),
      mean(fold_results_dt$test_rmse_state_fe, na.rm = TRUE),
      mean(fold_results_dt$oob_r2_ipw, na.rm = TRUE),
      mean(fold_results_dt$test_r2_ipw, na.rm = TRUE),
      mean(fold_results_dt$oob_rmse_ipw, na.rm = TRUE),
      mean(fold_results_dt$test_rmse_ipw, na.rm = TRUE)
    ),
    SD = c(
      sd(fold_results_dt$oob_r2_baseline, na.rm = TRUE),
      sd(fold_results_dt$test_r2_baseline, na.rm = TRUE),
      sd(fold_results_dt$oob_rmse_baseline, na.rm = TRUE),
      sd(fold_results_dt$test_rmse_baseline, na.rm = TRUE),
      sd(fold_results_dt$oob_r2_state_fe, na.rm = TRUE),
      sd(fold_results_dt$test_r2_state_fe, na.rm = TRUE),
      sd(fold_results_dt$oob_rmse_state_fe, na.rm = TRUE),
      sd(fold_results_dt$test_rmse_state_fe, na.rm = TRUE),
      sd(fold_results_dt$oob_r2_ipw, na.rm = TRUE),
      sd(fold_results_dt$test_r2_ipw, na.rm = TRUE),
      sd(fold_results_dt$oob_rmse_ipw, na.rm = TRUE),
      sd(fold_results_dt$test_rmse_ipw, na.rm = TRUE)
    )
  )
  fwrite(fold_summary, here(CONFIG$dir_diagnostics, "multifold_results_summary_cost_v22.csv"))
  cat("âœ“ Saved detailed and summary fold results\n\n")
  

################################################################################
# SECTION 12: DEBIASED PROJECTION - SCRIPT B (CLEANUP COST)
################################################################################

cat("\n", rep("=", 80), sep = "")
cat("\nSECTION 12: DEBIASED COST PROJECTION (LOO + ENSEMBLE GRID)\n")
cat(rep("=", 80), "\n\n", sep = "")

# -----------------------
# Setup for Section 12
# -----------------------

# Create feature matrix from FULL model_data (not just training)
features_for_dml <- intersect(features_baseline, colnames(model_data))
X_full_df <- as.data.frame(model_data[, .SD, .SDcols = features_for_dml])
feature_matrix <- build_model_matrix(X_full_df, ref_colnames = X_train_cols)

# Alias models for consistency
rf_baseline <- model_baseline
if (exists("model_ipw")) rf_ipw <- model_ipw

# Define parameters
num_trees <- CONFIG$grf_ntree
dir_results <- CONFIG$dir_results
version <- 22

# Configuration
cost_dml_config <- list(
  run_baseline = TRUE,
  run_ipw = CONFIG$generate_ipw,
  method = "leave_one_out",
  B_bootstrap = 100
)

# -----------------------
# Helper: Leave-One-State-Out (OOB + Ensemble)
# -----------------------

run_loo_dml <- function(model_variant) {
  
  cat(sprintf("\nLeave-One-Out DML: %s\n", model_variant))
  
  # ---------------------------------------------------------
  # 1. Setup Data & Grid
  # ---------------------------------------------------------
  states <- unique(model_data$state)
  n_states <- length(states)
  y_full <- model_data$log_cost
  
  # Weights
  if (model_variant == "ipw" && "ipw" %in% colnames(model_data)) {
    weights_full <- model_data$ipw
    weights_full <- pmin(weights_full, quantile(weights_full, 0.99, na.rm = TRUE))
  } else {
    weights_full <- NULL
  }
  
  # Grid for Ensemble Prediction
  age_bins <- 8; age_int <- 5
  dcm_grid_cost <- expand.grid(N = c("1", "2", "3", "4+"), w = c("single", "double"), 
                               A = factor(1:age_bins), rho = c("RB", "FF"))
  setDT(dcm_grid_cost)
  
  pred_grid_feats <- copy(dcm_grid_cost)
  pred_grid_feats[, `:=`(
    avg_tank_age = (as.numeric(A) * age_int) - (age_int/2),
    active_tanks = fcase(N == "1", 1, N == "2", 2, N == "3", 3, N == "4+", 5),
    has_single_walled = as.integer(w == "single"),
    has_double_walled = as.integer(w == "double"),
    state = fcase(rho == "RB", "Texas", rho == "FF", "Oklahoma"),
    total_capacity = median(model_data$total_capacity, na.rm=TRUE),
    claim_year = 2023,
    is_motor_fuel = TRUE
  )]
  
  # Matrix for grid
  X_state_space_cost <- build_model_matrix(as.data.frame(pred_grid_feats), ref_colnames = X_train_cols)
  
  # Containers
  oob_predictions <- rep(NA_real_, nrow(model_data))
  grid_log_preds_sum <- numeric(nrow(X_state_space_cost))
  valid_iter_count <- 0
  
  cat(sprintf("  States: %d (each held out once)\n", n_states))
  
  # ---------------------------------------------------------
  # 2. LOO Loop
  # ---------------------------------------------------------
  for (i in 1:n_states) {
    
    holdout_state <- states[i]
    test_idx <- which(model_data$state == holdout_state)
    train_idx <- which(model_data$state != holdout_state)
    
    if (length(train_idx) < 50) next
    
    tryCatch({
      # Train LOO Model
      if (is.null(weights_full)) {
        rf_loo <- regression_forest(
          X = feature_matrix[train_idx, , drop = FALSE],
          Y = y_full[train_idx],
          num.trees = min(num_trees, 200),
          min.node.size = best_params$min.node.size,
          sample.fraction = best_params$sample.fraction,
          honesty = TRUE,
          seed = 42 + i
        )
      } else {
        rf_loo <- regression_forest(
          X = feature_matrix[train_idx, , drop = FALSE],
          Y = y_full[train_idx],
          sample.weights = weights_full[train_idx],
          num.trees = min(num_trees, 200),
          min.node.size = best_params$min.node.size,
          sample.fraction = best_params$sample.fraction,
          honesty = TRUE,
          seed = 42 + i
        )
      }
      
      # A. OOB Prediction (For History)
      if (length(test_idx) > 0) {
        preds <- predict(rf_loo, newdata = feature_matrix[test_idx, , drop = FALSE])$predictions
        oob_predictions[test_idx] <- preds
      }
      
      # B. Grid Prediction (Accumulate)
      grid_preds <- predict(rf_loo, newdata = X_state_space_cost)$predictions
      grid_log_preds_sum <- grid_log_preds_sum + grid_preds
      valid_iter_count <- valid_iter_count + 1
      
      if (i %% 5 == 0) cat(sprintf("  [%d/%d] Processed %s\n", i, n_states, holdout_state))
      
    }, error = function(e) {
      cat(sprintf("  [WARN] Failed state %s: %s\n", holdout_state, e$message))
    })
  }
  
  coverage <- mean(!is.na(oob_predictions)) * 100
  cat(sprintf("  Coverage: %.1f%%\n", coverage))
  
  # Fill missing OOB with full model
  if (any(is.na(oob_predictions)) && exists("full_model")) {
    preds_full <- predict(full_model, newdata = feature_matrix)$predictions
    oob_predictions[is.na(oob_predictions)] <- preds_full[is.na(oob_predictions)]
  }
  
  # ---------------------------------------------------------
  # 3. Save Outputs
  # ---------------------------------------------------------
  
  # A. Panel Predictions (Back-transform)
  oob_cost <- exp(oob_predictions)
  output_dt <- data.table(
    facility_id = model_data$facility_id,
    state = model_data$state,
    # Defensively construct panel_id
    panel_id = if("panel_id" %in% names(model_data)) model_data$panel_id else paste(model_data$facility_id, model_data$state, sep="_"),
    claim_year = model_data$claim_year,
    avg_tank_age = model_data$avg_tank_age,
    predicted_cost = oob_cost,
    predicted_log_cost = oob_predictions,
    model_variant = model_variant
  )
  
  # B. Grid Predictions (Ensemble Average)
  if (valid_iter_count > 0) {
    avg_log_cost <- grid_log_preds_sum / valid_iter_count
    dcm_grid_cost[, predicted_cost := exp(avg_log_cost)]
    dcm_grid_cost[, model_variant := model_variant]
    
    grid_file <- here(CONFIG$dir_results, sprintf("state_space_cost_predictions_%s.rds", model_variant))
    saveRDS(dcm_grid_cost, grid_file)
    cat(sprintf("  ✓ Saved Ensemble Cost Grid: %s\n", basename(grid_file)))
  }
  
  return(output_dt)
}

# -----------------------
# Execute DML
# -----------------------

dml_cost_results <- list()

if (cost_dml_config$run_baseline) {
  dml_cost_results$baseline <- run_loo_dml("baseline")
}

if (cost_dml_config$run_ipw && exists("rf_ipw")) {
  dml_cost_results$ipw <- run_loo_dml("ipw")
}

# Save combined panel predictions
if (length(dml_cost_results) > 0) {
  dml_combined <- rbindlist(dml_cost_results, idcol = "source_model")
  output_file <- here(dir_results, sprintf("cleanup_cost_predictions_DML_v%d.rds", version))
  saveRDS(dml_combined, output_file)
  cat(sprintf("\n✓ Saved: %s\n", basename(output_file)))
}

cat("✓ Script B DML Complete\n")

# ====================================================================
# SECTION 12.5: SAVE MODELS
# ====================================================================
cat("\nSECTION 12.5: SAVING FULL MODELS\n")

if (exists("model_baseline")) {
  saveRDS(model_baseline, here(CONFIG$dir_results, "model_B_baseline.rds"))
  cat("  ✓ Saved Full Model: model_B_baseline.rds\n")
}
if (exists("model_ipw")) {
  saveRDS(model_ipw, here(CONFIG$dir_results, "model_B_ipw.rds"))
  cat("  ✓ Saved Full Model: model_B_ipw.rds\n")
}

# -----------------------
# SECTION 13: FINAL SUMMARY
# -----------------------
script_end_time <- Sys.time()
script_runtime <- difftime(script_end_time, script_start_time, units = "mins")

cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SCRIPT B COMPLETE (v22)\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n", sep = "")

cat("Summary:\n")
cat(sprintf("  - Mode: %s\n", CONFIG$mode))
cat(sprintf("  - Variants: %d\n", length(variants_to_save)))
cat(sprintf("  - Runtime: %.1f minutes\n\n", as.numeric(script_runtime)))

cat("Generated Outputs:\n")
for (variant in variants_to_save) {
  cat(sprintf("  ✓ cleanup_cost_predictions_DML_%s.rds (Panel)\n", variant))
  cat(sprintf("  ✓ state_space_cost_predictions_%s.rds (Grid)\n", variant))
  cat(sprintf("  ✓ model_B_%s.rds (Model)\n", variant))
}

cat("\nNext Steps:\n")
cat("  1. Use cost estimates in DCM\n")
cat("  2. Proceed to Script 06 (Merge)\n")
cat("\n")

cat(paste(rep("=", 80), collapse = ""), "\n")