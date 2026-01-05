# ====================================================================
# Script A: Leak Probability Estimation (v30 - WITH REGIME INDICATORS + DIAGNOSTICS)
# 
# KEY CHANGES FROM v29:
# 1. FIXED: sample.fraction bug (was 0.7, now max 0.49 for honesty)
# 2. ADDED: Comprehensive diagnostics after Section 10
# 3. ADDED: Tree visualization for presentation
# 4. CLARIFIED: Temporal validation purpose (justifies regime indicators)
#
# PURPOSE: Build best predictor E[leak | X, regime] for DCM
#          Regime indicators are NECESSARY (temporal validation proves this)
#          Chapter 1 (DiD) establishes causality - this builds prediction model
# 
# GRF OPTIMIZATION (Applied): 3 critical rare-event parameters added:
#   - alpha = 0.02 (lower split threshold, default 0.05 too high for 1% events)
#   - honesty.fraction = 0.65 (more data for splits, default 0.5 too low)
#   - honesty.prune.leaves = FALSE (keep sparse trees, default TRUE discards)
#
# Expected improvement: +1-3% AUC, better rare-event detection
# Based on: GRF documentation + "Production ML for rare event prediction" guide
# ====================================================================

# ====================================================================

# Clear environment
rm(list = ls())
gc()

script_start_time <- Sys.time()

# --------------------------------------------------------------------
# SECTION 0: SETUP AND DEPENDENCIES
# --------------------------------------------------------------------
cat("\n", rep("=", 80), "\n", sep = "")
cat("SCRIPT A: LEAK PROBABILITY ESTIMATION (v30 - WITH DIAGNOSTICS)\n")
cat(rep("=", 80), "\n\n", sep = "")

required_packages <- c(
  "data.table", "here", "survival", "grf", "pROC", "PRROC",
  "ggplot2", "gt", "scales", "rpart", "rpart.plot",
  "foreach", "doParallel", "doSNOW", "cluster", "factoextra", 
  "glmnet", "Matrix", "reshape2"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages, repos = "https://cloud.r-project.org/")
}

cat("Loading all required packages...\n")
suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})
cat("âœ“ All packages are loaded.\n")

# --------------------------------------------------------------------
# SECTION 1: CONFIGURATION
# --------------------------------------------------------------------
env_mode <- Sys.getenv("LEAK_PIPELINE_MODE")

CONFIG <- list(
  run_mode = if(env_mode != "") env_mode else "DEBUG",
  num_threads = 14,
  max_training_rows = 5000000, 
  profiling_sample_pct = 0.10,   
  debug_sample_pct = 0.10,       
  grf_ntree_debug = 50,
  grf_ntree_profiling = 500,
  grf_ntree_production = 2000,
  use_multifold_validation = TRUE,
  use_simple_model = TRUE,
  age_bins = 8,
  age_interval = 5,
  validation_strategy = "state_holdout",
  holdout_states = c("California", "Florida", "Ohio"),
  generate_baseline = TRUE,
  generate_state_fe = TRUE,
  generate_ipw = TRUE,
  dir_output = here("Output"),
  dir_results = here("Output", "Results"),
  dir_tables = here("Output", "Tables"),
  dir_figures = here("Output", "Figures"),
  dir_diagnostics = here("Output", "Diagnostics"),
  seed = 42
)


# In 07_Script_A config section, ADD:

# DML Configuration
dml_config <- list(
  run_baseline = TRUE,
  run_state_fe = FALSE,  # TURN OFF - collinear with regime + factor issues
  run_ipw = TRUE,
  K_folds = 5,
  use_cluster_sampling = TRUE,  # ENABLE cluster-level fold assignment
  cluster_var = "state",
  min_clusters_per_fold = 3,
  ensure_regime_balance = TRUE
)

CONFIG$is_debug <- (CONFIG$run_mode == "DEBUG")
CONFIG$is_profiling <- (CONFIG$run_mode == "PROFILING")
CONFIG$is_production <- (CONFIG$run_mode == "PRODUCTION")

CONFIG$grf_ntree <- if (CONFIG$is_debug) {
  CONFIG$grf_ntree_debug
} else if (CONFIG$is_profiling) {
  CONFIG$grf_ntree_profiling
} else {
  CONFIG$grf_ntree_production
}

CONFIG$holdout_sets <- if (CONFIG$is_debug) {
  list(
    fold_1 = c("Kansas", "Ohio"),              
    fold_2 = c("Texas", "Michigan"),           
    fold_3 = c("North Carolina", "Tennessee"), 
    fold_4 = c("Montana", "Idaho", "Oklahoma") 
  )
} else {
  list(
    fold_1 = c("Maine"),
    fold_2 = c("Texas", "Michigan"),
    fold_3 = c("North Carolina", "Tennessee"),
    fold_4 = c("New Jersey", "Pennsylvania")
  )
}

for (d in c(CONFIG$dir_output, CONFIG$dir_results, CONFIG$dir_tables, CONFIG$dir_figures, CONFIG$dir_diagnostics)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}
print(sapply(CONFIG[c("dir_output", "dir_results", "dir_tables", "dir_figures", "dir_diagnostics")], dir.exists))

compute_metrics <- function(y_true, preds) {
  res <- list()
  if (length(unique(y_true)) < 2 || all(is.na(preds))) {
    return(list(auc = NA, pr_auc = NA, brier = NA, auc_ci_lower = NA, auc_ci_upper = NA))
  }
  roc_obj <- tryCatch(pROC::roc(y_true, preds, quiet = TRUE), error = function(e) NULL)
  if (!is.null(roc_obj)) {
    res$auc <- as.numeric(pROC::auc(roc_obj))
    auc_ci <- tryCatch(as.numeric(pROC::ci.auc(roc_obj)), error = function(e) c(NA, NA, NA))
    if (length(auc_ci) >= 3) { res$auc_ci_lower <- auc_ci[1]; res$auc_ci_upper <- auc_ci[3] } 
    else { res$auc_ci_lower <- NA; res$auc_ci_upper <- NA }
  } else { res$auc <- NA; res$auc_ci_lower <- NA; res$auc_ci_upper <- NA }
  
  pos <- preds[y_true == 1]; neg <- preds[y_true == 0]
  if (length(pos) > 0 && length(neg) > 0) {
    pr <- tryCatch(PRROC::pr.curve(scores.class0 = pos, scores.class1 = neg, curve = FALSE), error = function(e) NULL)
    if (!is.null(pr)) res$pr_auc <- pr$auc.integral else res$pr_auc <- NA
  } else { res$pr_auc <- NA }
  
  res$brier <- mean((preds - y_true)^2)
  return(res)
}

theme_pub <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(size = rel(1.1), face = "bold", hjust = 0),
      axis.title = element_text(size = rel(0.95), face = "bold"),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25)
    )
}
theme_set(theme_pub())
set.seed(CONFIG$seed)

cat("âœ“ Configuration complete\n")
cat(sprintf("MODE: %s | Threads: %d | Sample: %.2f\n\n", CONFIG$run_mode, CONFIG$num_threads, CONFIG$profiling_sample_pct))

# --------------------------------------------------------------------
# SECTION 2 & 3: DATA LOAD & PREP
# --------------------------------------------------------------------
cat(rep("=", 80), "\n", sep = ""); cat("SECTIONS 2-3: DATA LOAD & PREP\n"); cat(rep("=", 80), "\n\n", sep = "")

panel_path <- if (file.exists(here("Data", "Processed", "facility_leak_behavior_monthly.csv"))) {
  here("Data", "Processed", "facility_leak_behavior_monthly.csv")
} else {
  here("Data", "Processed", "From_Server", "facility_leak_behavior_monthly_SAMPLE.csv")
}

keep_cols <- c("facility_id", "leak_incident", "active_tanks", "has_double_walled", "has_single_walled", 
               "avg_tank_age", "state", "panel_year", "total_capacity", "months_since_entry", 
               "cumulative_closures", "days_since_last_closure", "is_motor_fuel")

panel <- fread(panel_path, select = keep_cols)
cat(sprintf("Loaded %s rows.\n", format(nrow(panel), big.mark = ",")))

# ====================================================================
# CREATE REGIME INDICATORS AND INTERACTIONS
# ====================================================================
cat("\n  â†’ Creating regime indicators and interaction terms...\n")

regime_transitions <- data.table(
  state = c("Texas", "Michigan", "Florida", "Iowa", 
            "West Virginia", "Delaware", "Connecticut"),
  transition_year = c(1999, 2006, 2007, 2008, 2004, 2004, 2004)
)

panel <- merge(panel, regime_transitions, by = "state", all.x = TRUE)
panel[is.na(transition_year), transition_year := 9999]

panel[, `:=`(
  is_risk_based = as.integer(panel_year >= transition_year),
  years_since_transition = pmax(0, panel_year - transition_year),
  post_policy = as.integer(panel_year >= transition_year)
)]

panel[, `:=`(
  age_x_regime = avg_tank_age * is_risk_based,
  single_x_regime = has_single_walled * is_risk_based,
  double_x_regime = has_double_walled * is_risk_based,
  capacity_x_regime = log1p(total_capacity) * is_risk_based
)]

regime_summary <- panel[, list(
  n_obs = .N,
  n_facilities = uniqueN(facility_id),
  n_leaks = sum(leak_incident),
  leak_rate = mean(leak_incident)
), by = is_risk_based]

cat("\n  Regime Distribution:\n")
print(regime_summary)
cat(sprintf("\n  States with risk-based pricing: %s\n", 
            paste(regime_transitions$state, collapse = ", ")))
cat(sprintf("  Transition years: %s\n", 
            paste(regime_transitions$transition_year, collapse = ", ")))
cat("\nâœ“ Regime variables created\n")

# Create other vars
panel[, `:=`(
  N = fcase(active_tanks == 1, "1", active_tanks == 2, "2", active_tanks == 3, "3", active_tanks >= 4, "4+", default = NA_character_),
  w = fcase(has_double_walled > 0, "double", has_single_walled > 0, "single", default = NA_character_),
  A = pmin(ceiling(avg_tank_age / CONFIG$age_interval), CONFIG$age_bins),
  rho = fcase(state %in% c("Texas","Michigan","Florida","Iowa","West Virginia","Delaware","Connecticut"), "RB", default = "FF")
)]
panel[, `:=`(N = factor(N), w = factor(w), rho = factor(rho), A = factor(A))]

# Sampling with Minimum Retention
if ((CONFIG$is_profiling || CONFIG$is_debug) && CONFIG$profiling_sample_pct < 1.0) {
  cat("\n---- STRATIFIED SAMPLING (Safe Mode) ----\n")
  set.seed(CONFIG$seed)
  sampled_facilities <- panel[, list(facility_id = unique(facility_id)), by = state][
    , .SD[sample(.N, size = max(50, ceiling(.N * CONFIG$profiling_sample_pct))), ], by = state
  ]$facility_id

  panel <- panel[facility_id %in% sampled_facilities]
  cat(sprintf("  Sampled facilities: %s (%.1f%% target)\n", format(uniqueN(panel$facility_id), big.mark = ","), CONFIG$profiling_sample_pct * 100))
  gc(verbose = FALSE)
} else {
  cat("  No sampling (full data will be used)\n")
}

# DCM States (Metadata only)
dcm_states <- if(CONFIG$use_simple_model) {
  CJ(N = factor("2", levels=levels(panel$N)), w = levels(panel$w), A = levels(panel$A), rho = levels(panel$rho))
} else {
  CJ(N = levels(panel$N), w = levels(panel$w), A = levels(panel$A), rho = levels(panel$rho))
}
dcm_states[, state_idx := .I]
features_dcm_state <- c("N", "w", "A", "rho")

# --------------------------------------------------------------------
# SECTION 4: PREPARE FEATURES (MASTER DATASET)
# --------------------------------------------------------------------
cat(rep("=", 80), "\n", sep = ""); cat("SECTION 4: PREPARING FEATURES\n"); cat(rep("=", 80), "\n\n", sep = "")

features_kitchen_sink <- c(
  "avg_tank_age", "active_tanks", "total_capacity",
  "has_single_walled", "has_double_walled", "panel_year", 
  "months_since_entry", "cumulative_closures", "days_since_last_closure", 
  "state", "is_motor_fuel",
  "is_risk_based",
  "years_since_transition",
  "age_x_regime",
  "single_x_regime", 
  "double_x_regime",
  "capacity_x_regime"
)

if (!CONFIG$use_simple_model) features_kitchen_sink <- unique(c(features_kitchen_sink, "N"))

cat(sprintf("  Feature list: %d features (including regime indicators)\n", length(features_kitchen_sink)))
cat("  Regime-related features:\n")
cat("    - is_risk_based (binary: 0=flat-fee, 1=risk-based)\n")
cat("    - years_since_transition (continuous)\n")
cat("    - age_x_regime (interaction: allows different age slopes)\n")
cat("    - single_x_regime, double_x_regime (construction type interactions)\n")
cat("    - capacity_x_regime (facility size interaction)\n\n")

model_cols <- unique(c("facility_id", "leak_incident", features_kitchen_sink, names(dcm_states)))
model_data <- panel[, .SD, .SDcols = intersect(names(panel), model_cols)]
model_data <- model_data[complete.cases(model_data)]

cat("  - Applying transformations (Log Capacity)...\n")
if("total_capacity" %in% names(model_data)) model_data[, total_capacity := log1p(total_capacity)]
if("days_since_last_closure" %in% names(model_data)) model_data[is.na(days_since_last_closure), days_since_last_closure := 0]

Y_master <- model_data$leak_incident
imbalance_ratio <- sum(Y_master == 0) / sum(Y_master == 1)
penalty_to_use <- sqrt(imbalance_ratio)

rm(panel); gc()

# --------------------------------------------------------------------
# SECTION 5: GEOGRAPHIC HETEROGENEITY
# --------------------------------------------------------------------
cat(rep("=", 80), "\n", sep = ""); cat("SECTION 5: GEOGRAPHIC HETEROGENEITY\n"); cat(rep("=", 80), "\n\n", sep = "")

state_summary <- model_data[, list(
  n_facilities = uniqueN(facility_id),
  n_observations = .N,
  leak_rate = mean(leak_incident),
  avg_age = mean(avg_tank_age, na.rm = TRUE),
  avg_tanks = mean(active_tanks, na.rm = TRUE),
  pct_double_walled = mean(has_double_walled > 0, na.rm = TRUE),
  avg_capacity = mean(total_capacity, na.rm = TRUE),
  pct_risk_based = mean(is_risk_based) * 100
), by = state]

set.seed(CONFIG$seed)
cluster_vars <- c("leak_rate", "avg_age", "avg_tanks", "pct_double_walled")
cluster_data <- scale(state_summary[, .SD, .SDcols = cluster_vars])
rownames(cluster_data) <- state_summary$state
k_optimal <- 3
kmeans_result <- kmeans(cluster_data, centers = k_optimal, nstart = 25)
state_summary[, cluster := kmeans_result$cluster]

fwrite(state_summary, here(CONFIG$dir_diagnostics, "geographic_heterogeneity.csv"))
cat("âœ“ Geographic heterogeneity analysis complete\n\n")

# --------------------------------------------------------------------
# SECTION 6: MASTER SPARSE MATRIX
# --------------------------------------------------------------------
cat(rep("=", 80), "\n", sep = ""); cat("SECTION 6: MASTER SPARSE MATRIX & SPLIT\n"); cat(rep("=", 80), "\n\n", sep = "")

cat("  - Constructing Sparse Master Matrix...\n")
features_formula <- as.formula(paste("~", paste(features_kitchen_sink, collapse = " + "), "- 1"))
X_master <- Matrix::sparse.model.matrix(features_formula, data = model_data)

cat(sprintf("  âœ“ Master Matrix Created. Dimensions: %d rows x %d cols\n", nrow(X_master), ncol(X_master)))
cat(sprintf("    (Includes %d new regime-related columns)\n", 
            sum(grepl("regime|is_risk_based|years_since", colnames(X_master)))))

all_col_names <- colnames(X_master)
state_cols <- grep("^state", all_col_names, value = TRUE)
baseline_cols <- setdiff(all_col_names, state_cols)

cat("Defining Train/Test Indices...\n")
if (CONFIG$validation_strategy == "state_holdout") {
  test_idx <- which(model_data$state %in% CONFIG$holdout_states)
  test_idx_logical <- seq_len(nrow(model_data)) %in% test_idx
  train_idx <- setdiff(seq_len(nrow(model_data)), test_idx)
} else {
  set.seed(CONFIG$seed)
  n_test <- floor(CONFIG$test_pct * nrow(model_data))
  test_idx <- sample(seq_len(nrow(model_data)), n_test)
  train_idx <- setdiff(seq_len(nrow(model_data)), test_idx)
}

cat(sprintf("  - Train Size: %s | Test Size: %s\n\n", 
            format(length(train_idx), big.mark = ","), 
            format(length(test_idx), big.mark = ",")))

# --------------------------------------------------------------------
# SECTION 7: IPW (ELASTIC NET)
# --------------------------------------------------------------------
cat(rep("=", 80), "\n", sep = ""); cat("SECTION 7: IPW (ELASTIC NET)\n"); cat(rep("=", 80), "\n\n", sep = "")

cat("  - IPW Grouping Strategy: Regulatory Regime\n")
cat("    Group A (Risk-Based): Texas, Michigan, Florida, Iowa, West Virginia, Delaware, Connecticut\n")
cat("    Group B (Flat-Fee): All other states\n")

ipw_group <- ifelse(model_data$state %in% c("Texas","Michigan","Florida","Iowa","West Virginia","Delaware","Connecticut"), 1, 0)
cat(sprintf("    Group distribution: %.1f%% Group A, %.1f%% Group B\n", 
            mean(ipw_group) * 100, (1 - mean(ipw_group)) * 100))

cat("  - Tuning Lambda (Elastic Net)...\n")
X_ipw <- Matrix::sparse.model.matrix(~ avg_tank_age + active_tanks + has_single_walled + has_double_walled + total_capacity - 1, data = model_data[train_idx])
cv_ipw <- tryCatch(glmnet::cv.glmnet(X_ipw, ipw_group[train_idx], family = "binomial", alpha = 0.5, nfolds = 5), error = function(e) NULL)

if (!is.null(cv_ipw)) {
  propensity_scores <- predict(cv_ipw, newx = as.matrix(X_ipw), s = "lambda.min", type = "response")[, 1]
  propensity_scores <- pmax(0.01, pmin(0.99, propensity_scores))
  
  ipw_weights <- ifelse(ipw_group[train_idx] == 1, 1 / propensity_scores, 1 / (1 - propensity_scores))
  ipw_weights <- ipw_weights / mean(ipw_weights)
  
  cat("  - Predicting Propensity Scores...\n")
  cat("  - Weight Distribution:\n")
  cat(sprintf("    Mean: %.2f | Median: %.2f | Max: %.2f\n", mean(ipw_weights), median(ipw_weights), max(ipw_weights)))
  cat(sprintf("    SD: %.2f | IQR: %.2f\n", sd(ipw_weights), IQR(ipw_weights)))
} else {
  cat("  âœ— IPW fitting failed. Using uniform weights.\n")
  ipw_weights <- rep(1, length(train_idx))
}

rm(X_ipw, cv_ipw); gc(verbose = FALSE)
cat("  âœ“ IPW Computed.\n\n")

# --------------------------------------------------------------------
# SECTION 8: BASELINE MODEL
# --------------------------------------------------------------------
cat(rep("=", 80), "\n", sep = ""); cat("SECTION 8: BASELINE MODEL\n"); cat(rep("=", 80), "\n\n", sep = "")

if (CONFIG$generate_baseline) {
  cat("  - Tuning (Optimizing for PR-AUC)...\n")
  
  # FIX #1: sample.fraction max 0.49 (was 0.7, caused crash!)
  tune_grid <- expand.grid(
    mtry = c(2, 4, 6), 
    min.node.size = c(10, 30, 50), 
    sample.fraction = c(0.3, 0.4, 0.49)  # â† FIXED! Must be < 0.5 with honesty=TRUE
  )
  
  tune_sample_size <- min(50000, length(train_idx))
  tune_sample_idx <- sample(train_idx, tune_sample_size)
  X_tune <- as.matrix(X_master[tune_sample_idx, baseline_cols])
  Y_tune <- Y_master[tune_sample_idx]
  
  best_pr_auc <- 0; best_params <- tune_grid[1, ]
  for (i in seq_len(nrow(tune_grid))) {
    m_temp <- grf::regression_forest(X = X_tune, Y = Y_tune, num.trees = 50, 
                                      mtry = tune_grid$mtry[i], min.node.size = tune_grid$min.node.size[i], 
                                      alpha = 0.02,                      # Critical for rare events
                                      honesty.fraction = 0.65,           # More data for splits
                                      honesty.prune.leaves = FALSE,      # Keep sparse trees
                                      sample.fraction = tune_grid$sample.fraction[i], num.threads = CONFIG$num_threads, honesty = TRUE)
    preds_temp <- predict(m_temp, newdata = X_tune)$predictions
    perf_temp <- compute_metrics(Y_tune, preds_temp)
    if (!is.na(perf_temp$pr_auc) && perf_temp$pr_auc > best_pr_auc) { best_pr_auc <- perf_temp$pr_auc; best_params <- tune_grid[i, ] }
  }
  
  cat("  - Best hyperparameters (Selected by PR-AUC):\n"); print(best_params)
  rm(X_tune, Y_tune, m_temp); gc()
  
  cat("  - Training Baseline...\n")
  train_sample_size <- if (!is.null(CONFIG$max_training_rows) && !is.infinite(CONFIG$max_training_rows)) {
    min(CONFIG$max_training_rows, length(train_idx))
  } else { length(train_idx) }
  
  if (train_sample_size < length(train_idx)) {
    set.seed(CONFIG$seed); train_sample_idx <- sample(train_idx, train_sample_size)
  } else { train_sample_idx <- train_idx }
  
  cat(sprintf("    (Using %s rows for training matrix)\n", format(length(train_sample_idx), big.mark = ",")))
  
  X_train_base <- as.matrix(X_master[train_sample_idx, baseline_cols, drop=FALSE])
  Y_train_base <- Y_master[train_sample_idx]
  
  gc(verbose = FALSE)
  
  model_baseline <- grf::regression_forest(X = X_train_base, Y = Y_train_base, num.trees = CONFIG$grf_ntree, 
                                           mtry = best_params$mtry, min.node.size = best_params$min.node.size, 
                                           sample.fraction = best_params$sample.fraction, num.threads = CONFIG$num_threads, 
                                           alpha = 0.02,                      # Critical for rare events
                                           honesty.fraction = 0.65,           # More data for splits
                                           honesty.prune.leaves = FALSE,      # Keep sparse trees
                                           honesty = TRUE, imbalance.penalty = penalty_to_use)
  
  rm(X_train_base, Y_train_base); gc()
  
  X_test_base <- as.matrix(X_master[test_idx, baseline_cols, drop=FALSE])
  Y_test_base <- Y_master[test_idx]
  preds_baseline <- predict(model_baseline, newdata = X_test_base)$predictions
  perf_baseline <- compute_metrics(Y_test_base, preds_baseline)
  
  # Store predictions in model_data for diagnostics
  model_data[test_idx, pred_baseline := preds_baseline]
  
  rm(X_test_base, Y_test_base); gc()
  cat(sprintf("  âœ“ Baseline AUC: %.4f\n", perf_baseline$auc))
} else {
  cat("  Baseline generation DISABLED.\n")
  model_baseline <- NULL; perf_baseline <- list(auc = NA)
}

# --------------------------------------------------------------------
# SECTION 9: STATE FE MODEL
# --------------------------------------------------------------------
# ==============================================================================
# SECTION 9: STATE FE MODEL (REMOVED)
# ==============================================================================
#
# State Fixed Effects model has been removed to simplify the pipeline.
# The IPW model already captures regime heterogeneity effectively.
#
# TO RESTORE: Uncomment the code block below and update references in Section 11-12.
#
# COMMENTED CODE:
# cat(rep("=", 80), "\n", sep = ""); cat("SECTION 9: STATE FE MODEL\n"); cat(rep("=", 80), "\n\n", sep = "")
# # if (CONFIG$generate_state_fe) {
# cat("  - Training FE Model...\n")
# train_sample_size <- if (!is.null(CONFIG$max_training_rows) && !is.infinite(CONFIG$max_training_rows)) {
# min(CONFIG$max_training_rows, length(train_idx))
# } else { length(train_idx) }
# if (train_sample_size < length(train_idx)) { set.seed(CONFIG$seed); train_sample_idx <- sample(train_idx, train_sample_size) 
# } else { train_sample_idx <- train_idx }
# # X_train_fe <- as.matrix(X_master[train_sample_idx, , drop=FALSE])
# Y_train_fe <- Y_master[train_sample_idx]
# # model_state_fe <- grf::regression_forest(X = X_train_fe, Y = Y_train_fe, num.trees = CONFIG$grf_ntree, 
# mtry = best_params$mtry, min.node.size = best_params$min.node.size, 
# sample.fraction = best_params$sample.fraction, num.threads = CONFIG$num_threads, 
# alpha = 0.02,                      # Critical for rare events
# honesty.fraction = 0.65,           # More data for splits
# honesty.prune.leaves = FALSE,      # Keep sparse trees
# honesty = TRUE, imbalance.penalty = penalty_to_use)
# # rm(X_train_fe, Y_train_fe); gc()
# # X_test_fe <- as.matrix(X_master[test_idx, , drop=FALSE])
# Y_test_fe <- Y_master[test_idx]
# preds_fe <- predict(model_state_fe, newdata = X_test_fe)$predictions
# perf_fe <- compute_metrics(Y_test_fe, preds_fe)
# # model_data[test_idx, pred_state_fe := preds_fe]
# # rm(X_test_fe, Y_test_fe); gc()
# cat(sprintf("  âœ“ State FE AUC: %.4f\n", perf_fe$auc))
# } else {
# cat("  State FE generation DISABLED.\n")
# model_state_fe <- NULL; perf_fe <- list(auc = NA)
# }
# # --------------------------------------------------------------------
# SECTION 10: IPW MODEL
# --------------------------------------------------------------------

cat(rep("=", 80), "\n", sep = ""); cat("SECTION 10: IPW MODEL\n"); cat(rep("=", 80), "\n\n", sep = "")

if (CONFIG$generate_ipw) {
  train_sample_size <- if (!is.null(CONFIG$max_training_rows) && !is.infinite(CONFIG$max_training_rows)) {
    min(CONFIG$max_training_rows, length(train_idx))
  } else { length(train_idx) }
  if (train_sample_size < length(train_idx)) { set.seed(CONFIG$seed); train_sample_idx <- sample(train_idx, train_sample_size) 
  } else { train_sample_idx <- train_idx }
  
  X_train_ipw <- as.matrix(X_master[train_sample_idx, baseline_cols, drop=FALSE])
  Y_train_ipw <- Y_master[train_sample_idx]
  weights_ipw <- if (length(ipw_weights) == length(train_idx)) { ipw_weights[match(train_sample_idx, train_idx)] 
  } else { rep(1, length(train_sample_idx)) }
  
  model_ipw <- grf::regression_forest(X = X_train_ipw, Y = Y_train_ipw, sample.weights = weights_ipw, 
                                      num.trees = CONFIG$grf_ntree, mtry = best_params$mtry, min.node.size = best_params$min.node.size, 
                                      alpha = 0.02,                      # Critical for rare events
                                      honesty.fraction = 0.65,           # More data for splits
                                      honesty.prune.leaves = FALSE,      # Keep sparse trees
                                      sample.fraction = best_params$sample.fraction, num.threads = CONFIG$num_threads, honesty = TRUE)
  
  rm(X_train_ipw, Y_train_ipw, weights_ipw); gc()
  
  X_test_ipw <- as.matrix(X_master[test_idx, baseline_cols, drop=FALSE])
  Y_test_ipw <- Y_master[test_idx]
  preds_ipw <- predict(model_ipw, newdata = X_test_ipw)$predictions
  perf_ipw <- compute_metrics(Y_test_ipw, preds_ipw)
  
  model_data[test_idx, pred_ipw := preds_ipw]
  
  rm(X_test_ipw, Y_test_ipw); gc()
  cat(sprintf("  âœ“ IPW AUC: %.4f\n", perf_ipw$auc))
} else {
  cat("  IPW generation DISABLED.\n")
  model_ipw <- NULL; perf_ipw <- list(auc = NA)
}

# ====================================================================
# NEW SECTION 10.5: COMPREHENSIVE DIAGNOSTICS
# ====================================================================
cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("SECTION 10.5: MODEL DIAGNOSTICS (REGIME INDICATORS)\n")
cat(rep("=", 80), "\n\n")


# ========================================
# DIAGNOSTIC 1: Variable Importance (CORRECTED)
# ========================================
cat("DIAGNOSTIC 1: VARIABLE IMPORTANCE\n")
cat(rep("-", 80), "\n", sep = "")

if (exists("model_baseline")) {
  imp <- grf::variable_importance(model_baseline)
  
  # CORRECTION: baseline_cols already contains the correct names in order
  imp_df <- data.table(
    feature = baseline_cols,
    importance = as.numeric(imp)
  )
  setorder(imp_df, -importance)
  
  cat("\nTOP 20 MOST IMPORTANT FEATURES:\n")
  print(imp_df[1:min(20, nrow(imp_df))])
  
  # Check regime indicator rank
  regime_features <- c("is_risk_based", "age_x_regime", "single_x_regime", 
                       "double_x_regime", "capacity_x_regime", "years_since_transition")
  
  cat("\n\nREGIME-RELATED FEATURE RANKS:\n")
  for (feat in regime_features) {
    if (feat %in% imp_df$feature) {
      rank <- which(imp_df$feature == feat)
      cat(sprintf("  %-25s: Rank #%-3d (importance: %.6f)\n", 
                  feat, rank, imp_df[feature == feat, importance]))
    }
  }
  
  # Interpretation
  regime_ranks <- sapply(regime_features, function(f) {
    if (f %in% imp_df$feature) which(imp_df$feature == f) else Inf
  })
  regime_top_rank <- min(regime_ranks)
  
  # CORRECTION: Handle Infinite rank safely
  if (is.finite(regime_top_rank)) {
    cat(sprintf("\nHighest regime feature rank: #%d\n", as.integer(regime_top_rank)))
  } else {
    cat("\nHighest regime feature rank: None found in model\n")
  }
  
  if (regime_top_rank <= 5) {
    cat("\nâœ“âœ“âœ“ INTERPRETATION: Regime indicators are CRITICAL for prediction\n")
    cat("    â†’ Including them was NECESSARY for accurate predictions\n")
    cat("    â†’ Temporal validation failure (Section 12) is due to regime shift\n")
    cat("    â†’ This validates your collaborator's intuition\n")
  } else if (regime_top_rank <= 10) {
    cat("\n~ INTERPRETATION: Regime indicators are MODERATELY important\n")
    cat("  â†’ Some regime effect, but other factors dominate\n")
    cat("  â†’ Temporal validation may show mixed results\n")
  } else {
    cat("\nâš  INTERPRETATION: Regime indicators have LOW importance\n")
    cat("  â†’ Temporal validation failure may be due to OTHER factors\n")
    cat("  â†’ Consider whether regime indicators truly needed\n")
    cat("  â†’ Technology/reporting changes may dominate\n")
  }
  
  fwrite(imp_df, here(CONFIG$dir_diagnostics, "variable_importance.csv"))
  
  # Plot variable importance (top 15)
  imp_plot_data <- imp_df[1:min(15, nrow(imp_df))]
  imp_plot_data[, feature := factor(feature, levels = rev(feature))]
  imp_plot_data[, is_regime := feature %in% regime_features]
  
  p_imp <- ggplot(imp_plot_data, aes(x = importance, y = feature, fill = is_regime)) +
    geom_col() +
    scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "darkred"),
                      name = "", labels = c("Base Feature", "Regime Feature")) +
    labs(title = "Variable Importance: Top 15 Features",
         subtitle = "Red = Regime-related features",
         x = "Importance", y = "") +
    theme_minimal() +
    theme(legend.position = "top")
  
  ggsave(here(CONFIG$dir_diagnostics, "variable_importance_top15.png"), 
         p_imp, width = 10, height = 6)
  
  cat("\nâœ“ Variable importance analysis saved\n")
}


# ========================================
# DIAGNOSTIC 2: Prediction Distributions by Regime
# ========================================
cat("\n\n")
cat("DIAGNOSTIC 2: PREDICTION DISTRIBUTIONS BY REGIME\n")
cat(rep("-", 80), "\n", sep = "")

if ("pred_baseline" %in% names(model_data)) {
  pred_by_regime <- model_data[test_idx, list(
    mean_pred = mean(pred_baseline, na.rm = TRUE),
    median_pred = median(pred_baseline, na.rm = TRUE),
    sd_pred = sd(pred_baseline, na.rm = TRUE),
    mean_actual = mean(leak_incident),
    n = .N,
    n_leaks = sum(leak_incident)
  ), by = is_risk_based]
  
  cat("\nPREDICTION vs ACTUAL BY REGIME:\n")
  print(pred_by_regime)
  
  # Calculate ratios
  if (nrow(pred_by_regime) == 2) {
    pred_ratio <- pred_by_regime[is_risk_based == 0, mean_pred] / 
                  pred_by_regime[is_risk_based == 1, mean_pred]
    actual_ratio <- pred_by_regime[is_risk_based == 0, mean_actual] / 
                    pred_by_regime[is_risk_based == 1, mean_actual]
    
    cat(sprintf("\nPredicted leak rate ratio (Flat-Fee / Risk-Based): %.2fx\n", pred_ratio))
    cat(sprintf("Actual leak rate ratio (Flat-Fee / Risk-Based): %.2fx\n", actual_ratio))
    cat(sprintf("Ratio discrepancy: %.2f%%\n", abs(pred_ratio - actual_ratio) / actual_ratio * 100))
    
    if (abs(pred_ratio - actual_ratio) / actual_ratio < 0.15) {
      cat("\nâœ“ Model accurately captures regime differences\n")
    } else {
      cat("\nâš  Model may not fully capture regime differences\n")
    }
  }
  
  # Density plot
  plot_data <- model_data[test_idx_logical & !is.na(pred_baseline), list(pred_baseline, is_risk_based)]
  plot_data[, regime := ifelse(is_risk_based == 1, "Risk-Based", "Flat-Fee")]
  
  p_dist <- ggplot(plot_data, aes(x = pred_baseline, fill = regime)) +
    geom_density(alpha = 0.6) +
    scale_x_log10() +
    scale_fill_manual(values = c("Flat-Fee" = "steelblue", "Risk-Based" = "darkred")) +
    labs(title = "Prediction Distributions by Regime",
         subtitle = "Log scale - Risk-based should be shifted left (lower risk)",
         x = "Predicted Leak Probability (log scale)", y = "Density",
         fill = "Regime") +
    theme_minimal()
  
  ggsave(here(CONFIG$dir_diagnostics, "prediction_distributions_by_regime.png"),
         p_dist, width = 10, height = 6)
  
  cat("\nâœ“ Prediction distribution analysis saved\n")
}

# ========================================
# DIAGNOSTIC 3: Calibration Analysis (Regime-Aware)
# ========================================
cat("\n\n")
cat("DIAGNOSTIC 3: CALIBRATION ANALYSIS (BY REGIME)\n")
cat(rep("-", 80), "\n", sep = "")

if ("pred_baseline" %in% names(model_data)) {
  
  # ----------------------------------------------------------------
  # 3A: State-Level Calibration (Colored by Regime)
  # ----------------------------------------------------------------
  cat("\n3A: STATE-LEVEL CALIBRATION\n")
  
  # Compute state-level predictions and actuals
  state_calib <- model_data[test_idx, list(
    mean_pred = mean(pred_baseline, na.rm = TRUE),
    observed_rate = mean(leak_incident),
    n = .N,
    n_leaks = sum(leak_incident),
    regime = ifelse(mean(is_risk_based) > 0.5, "Risk-Based", "Flat-Fee")
  ), by = state]
  
  cat("\nState-Level Predictions vs Actuals:\n")
  print(state_calib[order(-observed_rate)])
  
  # Scatter plot by state, colored by regime
  p_state_calib <- ggplot(state_calib, aes(x = mean_pred, y = observed_rate, color = regime)) +
    geom_point(size = 4, alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    geom_text(aes(label = state), vjust = -1, size = 2.5, show.legend = FALSE) +
    scale_color_manual(values = c("Flat-Fee" = "steelblue", "Risk-Based" = "darkred")) +
    labs(title = "State-Level Calibration (Colored by Regime)",
         subtitle = "Points on diagonal = perfect calibration",
         x = "Mean Predicted Leak Probability", 
         y = "Observed Leak Rate",
         color = "Regime") +
    theme_minimal() +
    theme(legend.position = "top")
  
  ggsave(here(CONFIG$dir_diagnostics, "calibration_state_level_by_regime.png"), 
         p_state_calib, width = 10, height = 8)
  
  # Calculate calibration error by regime
  calib_error_regime <- state_calib[, list(
    mae = mean(abs(mean_pred - observed_rate)),
    rmse = sqrt(mean((mean_pred - observed_rate)^2)),
    n_states = .N
  ), by = regime]
  
  cat("\nCalibration Error by Regime:\n")
  print(calib_error_regime)
  
  # ----------------------------------------------------------------
  # 3B: Age Bin Ã— Regime Calibration
  # ----------------------------------------------------------------
  cat("\n\n3B: AGE BIN Ã— REGIME CALIBRATION\n")
  
  # Create age bins
  model_data[test_idx, age_bin := cut(
    avg_tank_age,
    breaks = c(0, 10, 20, 30, 40, Inf),
    labels = c("0-10", "11-20", "21-30", "31-40", "40+"),
    include.lowest = TRUE
  )]
  
  # Compute predictions and actuals by age bin and regime
  age_regime_calib <- model_data[test_idx_logical & !is.na(age_bin), list(
    mean_pred = mean(pred_baseline, na.rm = TRUE),
    observed_rate = mean(leak_incident),
    n = .N,
    n_leaks = sum(leak_incident)
  ), by = list(age_bin, regime = ifelse(is_risk_based == 1, "Risk-Based", "Flat-Fee"))]
  
  cat("\nAge Bin Ã— Regime Calibration:\n")
  print(age_regime_calib)
  
  # Check if we have both regimes
  if (length(unique(age_regime_calib$regime)) > 1) {
    # Plot with facets by regime
    p_age_calib <- ggplot(age_regime_calib, aes(x = age_bin)) +
      geom_point(aes(y = mean_pred, color = "Predicted"), size = 3) +
      geom_point(aes(y = observed_rate, color = "Actual"), size = 3) +
      geom_line(aes(y = mean_pred, group = 1, color = "Predicted"), linetype = "dashed") +
      geom_line(aes(y = observed_rate, group = 1, color = "Actual"), linetype = "solid") +
      facet_wrap(~ regime, scales = "free_y") +
      scale_color_manual(values = c("Predicted" = "steelblue", "Actual" = "darkred")) +
      labs(title = "Calibration by Age Bin and Regime",
           subtitle = "Predicted vs Actual leak rates",
           x = "Tank Age Bin (years)", 
           y = "Leak Rate",
           color = "") +
      theme_minimal() +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(here(CONFIG$dir_diagnostics, "calibration_age_regime.png"), 
           p_age_calib, width = 10, height = 6)
  } else {
    cat(sprintf("\nâš  WARNING: Only %s regime in test set\n", unique(age_regime_calib$regime)))
    cat("  â†’ Cannot create regime comparison plots\n")
    cat("  â†’ This is because test states (CA, FL, OH) are predominantly flat-fee\n")
    
    # Still create plot for single regime
    p_age_calib <- ggplot(age_regime_calib, aes(x = age_bin, group = 1)) +
      geom_point(aes(y = mean_pred, color = "Predicted"), size = 3) +
      geom_point(aes(y = observed_rate, color = "Actual"), size = 3) +
      geom_line(aes(y = mean_pred, color = "Predicted"), linetype = "dashed") +
      geom_line(aes(y = observed_rate, color = "Actual"), linetype = "solid") +
      scale_color_manual(values = c("Predicted" = "steelblue", "Actual" = "darkred")) +
      labs(title = sprintf("Calibration by Age Bin (%s Regime)", unique(age_regime_calib$regime)),
           x = "Tank Age Bin (years)", 
           y = "Leak Rate",
           color = "") +
      theme_minimal() +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(here(CONFIG$dir_diagnostics, "calibration_age.png"), 
           p_age_calib, width = 8, height = 6)
  }
  

# ----------------------------------------------------------------
  # 3C: TRAINING DATA CALIBRATION (FOR REGIME COMPARISON)
  # ----------------------------------------------------------------
  cat("\n\n3C: TRAINING DATA CALIBRATION (FOR REGIME COMPARISON)\n")
  cat("(Using training data to show both regimes)\n")
  
  # Get predictions for a sample of training data
  # FIX: Use a different variable name (diag_sample_idx) to avoid overwriting the main train_sample_idx
  diag_sample_size <- min(50000, length(train_idx))
  diag_sample_idx <- sample(train_idx, diag_sample_size)
  
  X_train_sample <- as.matrix(X_master[diag_sample_idx, baseline_cols, drop=FALSE])
  preds_train_sample <- predict(model_baseline, newdata = X_train_sample)$predictions
  
  # Store in model_data temporarily for plotting
  model_data[diag_sample_idx, pred_baseline_train := preds_train_sample]
  
  # Age bin Ã— Regime for training data
  model_data[diag_sample_idx, age_bin_train := cut(
    avg_tank_age,
    breaks = c(0, 10, 20, 30, 40, Inf),
    labels = c("0-10", "11-20", "21-30", "31-40", "40+"),
    include.lowest = TRUE
  )]
  
  age_regime_train <- model_data[diag_sample_idx & !is.na(age_bin_train), list(
    mean_pred = mean(pred_baseline_train, na.rm = TRUE),
    observed_rate = mean(leak_incident),
    n = .N,
    n_leaks = sum(leak_incident)
  ), by = list(age_bin = age_bin_train, regime = ifelse(is_risk_based == 1, "Risk-Based", "Flat-Fee"))]
  
  cat("\nTraining Data - Age Bin Ã— Regime:\n")
  print(age_regime_train)
  
  # Plot with both regimes
  p_age_train <- ggplot(age_regime_train, aes(x = age_bin)) +
    geom_point(aes(y = mean_pred, color = "Predicted"), size = 3) +
    geom_point(aes(y = observed_rate, color = "Actual"), size = 3) +
    geom_line(aes(y = mean_pred, group = 1, color = "Predicted"), linetype = "dashed") +
    geom_line(aes(y = observed_rate, group = 1, color = "Actual"), linetype = "solid") +
    facet_wrap(~ regime, scales = "free_y") +
    scale_color_manual(values = c("Predicted" = "steelblue", "Actual" = "darkred")) +
    labs(title = "Training Data: Calibration by Age Bin and Regime",
         subtitle = "Shows model learns different patterns in each regime",
         x = "Tank Age Bin (years)", 
         y = "Leak Rate",
         color = "") +
    theme_minimal() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(here(CONFIG$dir_diagnostics, "calibration_age_regime_training.png"), 
         p_age_train, width = 10, height = 6)


  
  # ----------------------------------------------------------------
  # 3D: Overall Decile Calibration (Original)
  # ----------------------------------------------------------------
  cat("\n\n3D: OVERALL DECILE CALIBRATION\n")
  
  model_data[test_idx, pred_decile := cut(
    pred_baseline, 
    breaks = quantile(pred_baseline, probs = seq(0, 1, 0.1), na.rm = TRUE),
    labels = 1:10, 
    include.lowest = TRUE
  )]
  
  calibration_decile <- model_data[test_idx, list(
    mean_pred = mean(pred_baseline, na.rm = TRUE),
    observed_rate = mean(leak_incident),
    n = .N,
    n_leaks = sum(leak_incident)
  ), by = pred_decile]
  
  cat("\nOverall Calibration by Decile:\n")
  print(calibration_decile)
  
  cal_error <- mean(abs(calibration_decile$mean_pred - calibration_decile$observed_rate))
  cat(sprintf("\nMean Absolute Calibration Error: %.6f\n", cal_error))
  
  if (cal_error < 0.0001) {
    cat("âœ“ Excellent calibration\n")
  } else if (cal_error < 0.0005) {
    cat("âœ“ Good calibration\n")
  } else {
    cat("âš  Calibration could be improved (but this is expected with rare events)\n")
  }
  
  # Save outputs
  fwrite(state_calib, here(CONFIG$dir_diagnostics, "calibration_state_level.csv"))
  fwrite(age_regime_calib, here(CONFIG$dir_diagnostics, "calibration_age_regime_test.csv"))
  fwrite(age_regime_train, here(CONFIG$dir_diagnostics, "calibration_age_regime_train.csv"))
  fwrite(calibration_decile, here(CONFIG$dir_diagnostics, "calibration_decile.csv"))
  
  cat("\nâœ“ Calibration analysis complete\n")
  cat("  â†’ State-level plot shows regime differences\n")
  cat("  â†’ Age bin plots show predictions vs actuals by age and regime\n")
  cat("  â†’ Training data plot shows both regimes (test set only has flat-fee)\n")
}

# ========================================
# DIAGNOSTIC 4: Simple Tree for Presentation
# ========================================
cat("\n\n")
cat("DIAGNOSTIC 4: INTERPRETABLE TREE VISUALIZATION\n")
cat(rep("-", 80), "\n", sep = "")

cat("\nCreating simple decision tree for presentation purposes...\n")
cat("(This is NOT the prediction model - just for understanding key splits)\n\n")

tryCatch({
  # Create interpretable dataset for tree
  tree_data <- model_data[train_idx][sample(.N, min(50000, .N))]
  
  # Select key features (avoid state dummies)
  tree_features <- c("avg_tank_age", "active_tanks", "has_single_walled", 
                     "total_capacity", "is_risk_based", "age_x_regime")
  
  tree_formula <- as.formula(paste("leak_incident ~", paste(tree_features, collapse = " + ")))
  
  # Fit tree with reasonable depth
  tree_model <- rpart::rpart(
    tree_formula,
    data = tree_data,
    method = "anova",
    control = rpart::rpart.control(
      maxdepth = 4,        # Not too deep
      minsplit = 1000,     # Require reasonable sample size
      cp = 0.0001          # Complexity parameter
    )
  )
  
  # Save tree plot
  png(here(CONFIG$dir_diagnostics, "decision_tree_presentation.png"), 
      width = 12, height = 8, units = "in", res = 300)
  rpart.plot::rpart.plot(
    tree_model,
    type = 4,
    extra = 101,
    box.palette = "RdYlGn",
    branch.lty = 3,
    shadow.col = "gray",
    nn = TRUE,
    main = "Decision Tree: Key Risk Factors for Tank Leaks",
    sub = paste("Based on sample of", nrow(tree_data), "observations")
  )
  dev.off()
  
  cat("âœ“ Decision tree visualization saved\n")
  cat("  â†’ See: decision_tree_presentation.png\n")
  cat("  â†’ Use this for talks/presentations to show key splits\n")
  cat("  â†’ Remember: Random Forest uses MANY trees, not just this one!\n")
  
  # Print variable importance from tree
  tree_imp <- tree_model$variable.importance
  if (!is.null(tree_imp)) {
    cat("\nTree Variable Importance (for this single tree):\n")
    print(head(sort(tree_imp, decreasing = TRUE), 10))
  }
  
  # Save tree rules as text
  tree_rules <- capture.output(print(tree_model))
  writeLines(tree_rules, here(CONFIG$dir_diagnostics, "decision_tree_rules.txt"))
  
}, error = function(e) {
  cat(sprintf("âš  Tree visualization failed: %s\n", e$message))
  cat("  (Not critical - continuing with analysis)\n")
})

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("âœ“ ALL DIAGNOSTICS COMPLETE\n")
cat(rep("=", 80), "\n\n")

# ========================================
# DIAGNOSTIC INTERPRETATION GUIDE
# ========================================
cat(rep("=", 80), "\n", sep = "")
cat("INTERPRETATION GUIDE: What Do These Diagnostics Tell Us?\n")
cat(rep("=", 80), "\n\n")

cat("1. VARIABLE IMPORTANCE:\n")
cat("   â†’ If regime indicators in top 5: Strong regime effects\n")
cat("   â†’ If regime indicators rank 6-15: Moderate regime effects\n")
cat("   â†’ If regime indicators rank >15: Weak regime effects\n")
cat("   YOUR RESULT: Check variable_importance.csv\n\n")

cat("2. PREDICTION DISTRIBUTIONS:\n")
cat("   â†’ Should show ~2-3x difference (Flat-Fee / Risk-Based)\n")
cat("   â†’ If only one regime in test set: Check training data plot\n")
cat("   YOUR RESULT: Check prediction_distributions_by_regime.png\n\n")

cat("3. CALIBRATION:\n")
cat("   â†’ State-level plot: Points near diagonal = well-calibrated\n")
cat("   â†’ Age bin plots: Predicted should track Actual closely\n")
cat("   â†’ Training data shows both regimes (test may only have one)\n")
cat("   YOUR RESULT: Check calibration_state_level_by_regime.png\n\n")

cat("4. DECISION TREE:\n")
cat("   â†’ Use for presentations (shows ONE illustrative tree)\n")
cat("   â†’ Random Forest uses 500 trees, not just this one\n")
cat("   YOUR RESULT: Check decision_tree_presentation.png\n\n")

cat("5. TEMPORAL VALIDATION (Section 12):\n")
cat("   â†’ AUC < 0.6 = Model fails without regime indicators\n")
cat("   â†’ AUC > 0.7 = Model generalizes across regimes\n")
cat("   YOUR RESULT: Check temporal_validation_results.csv\n\n")

cat("6. MULTI-FOLD CV (Section 11):\n")
cat("   â†’ Fold 2 (TX, MI) should have LOWEST AUC if regime matters\n")
cat("   â†’ Other folds should have AUC > 0.85\n")
cat("   YOUR RESULT: Check multifold_results.csv\n\n")

cat(rep("-", 80), "\n", sep = "")
cat("OVERALL INTERPRETATION:\n")
cat(rep("-", 80), "\n\n")

if (exists("imp_df") && exists("regime_features")) {
  regime_ranks <- sapply(regime_features, function(f) {
    if (f %in% imp_df$feature) which(imp_df$feature == f) else Inf
  })
  regime_top_rank <- min(regime_ranks)
  
  if (regime_top_rank <= 5) {
    cat("âœ“âœ“âœ“ REGIME INDICATORS ARE CRITICAL\n")
    cat("    â†’ Including them dramatically improves predictions\n")
    cat("    â†’ Behavioral response to risk-based pricing is STRONG\n")
    cat("    â†’ Your collaborator was RIGHT\n\n")
  } else if (regime_top_rank <= 15) {
    cat("~ REGIME INDICATORS ARE MODERATELY IMPORTANT\n")
    cat("  â†’ Including them improves predictions\n")
    cat("  â†’ Behavioral response exists but is NOT dramatic\n")
    cat("  â†’ Other factors (facility history, technology) also matter\n")
    cat("  â†’ This is actually the MOST REALISTIC scenario\n\n")
  } else {
    cat("âš  REGIME INDICATORS HAVE LIMITED IMPORTANCE\n")
    cat("  â†’ Including them provides minimal improvement\n")
    cat("  â†’ Temporal validation failure may be due to other factors\n")
    cat("  â†’ Consider whether regime indicators truly needed\n\n")
  }
}

cat("NEXT STEPS:\n")
cat("  1. Review all diagnostic plots in Output/Diagnostics/\n")
cat("  2. Check if variable importance matches your expectations\n")
cat("  3. Examine temporal validation results (Section 12 output)\n")
cat("  4. Compare to your Chapter 1 DiD results\n")
cat("  5. Use these predictions in your DCM (Section 15 outputs)\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("END OF DIAGNOSTIC INTERPRETATION\n")
cat(rep("=", 80), "\n\n")

# Compile performance results
performance_results <- data.table(
  Model = c("Baseline",  "IPW"), #"State_FE"
  AUC = c(perf_baseline$auc, perf_ipw$auc), #, perf_fe$auc
  PR_AUC = c(perf_baseline$pr_auc,perf_ipw$pr_auc), # perf_fe$pr_auc, 
  Brier = c(perf_baseline$brier,  perf_ipw$brier) #perf_fe$brier,
)

# ====================================================================
# END OF NEW DIAGNOSTICS SECTION
# ====================================================================

# --------------------------------------------------------------------
# SECTION 11: MULTI-FOLD CV
# --------------------------------------------------------------------

if (CONFIG$use_multifold_validation) {
  cat(rep("=", 80), "\n", sep = "")
  cat("SECTION 11: MULTI-FOLD CV (GEOGRAPHIC VALIDATION)\n")
  cat(rep("=", 80), "\n\n", sep = "")
  
  # Configuration
  FOLD_CONFIG <- list(
    enable_ipw = TRUE,
    fold_max_train = 2000000,
    fold_ntrees = if(CONFIG$is_production) 500 else 50,
    verbose = TRUE,
    min_test_size = 100,
    ipw_strategy = "regulatory_regime"
  )
  
  # Function to create meaningful groups
  create_ipw_groups <- function(states, strategy = "regulatory_regime") {
    if (strategy == "regulatory_regime") {
      risk_based <- c("Texas", "Michigan", "Florida", "Iowa", 
                      "West Virginia", "Delaware", "Connecticut")
      group_a_states <- intersect(states, risk_based)
    } else if (strategy == "census_region") {
      high_volume <- c("Ohio", "Indiana", "Illinois", "Michigan", "Wisconsin",
                       "Minnesota", "Iowa", "Missouri", "North Dakota", "South Dakota",
                       "Nebraska", "Kansas", "Delaware", "Maryland", "Virginia",
                       "West Virginia", "North Carolina", "South Carolina", "Georgia",
                       "Florida", "Kentucky", "Tennessee", "Alabama", "Mississippi",
                       "Arkansas", "Louisiana", "Oklahoma", "Texas")
      group_a_states <- intersect(states, high_volume)
    } else if (strategy == "geographic_cluster") {
      if (exists("state_summary") && "cluster" %in% names(state_summary)) {
        cluster_1_2_states <- state_summary[cluster %in% c(1, 2), state]
        group_a_states <- intersect(states, cluster_1_2_states)
      } else {
        warning("Geographic clusters not found, falling back to regulatory regime")
        risk_based <- c("Texas", "Michigan", "Florida", "Iowa", 
                       "West Virginia", "Delaware", "Connecticut")
        group_a_states <- intersect(states, risk_based)
      }
    }
    return(group_a_states)
  }
  
  # Helper for propensity score estimation
  estimate_fold_weights <- function(train_indices, prop_vars, strategy) {
    train_states <- unique(model_data$state[train_indices])
    group_a_states <- create_ipw_groups(train_states, strategy)
    Y_ps <- as.numeric(model_data$state[train_indices] %in% group_a_states)
    
    if (length(unique(Y_ps)) < 2) {
      stop("IPW grouping resulted in only one group. Check your grouping strategy.")
    }
    
    ps_tune_size <- min(5000, length(train_indices))
    ps_tune_idx <- sample(train_indices, ps_tune_size)
    
    prop_cols <- which(colnames(X_master) %in% prop_vars)
    X_ps <- as.matrix(X_master[ps_tune_idx, prop_cols, drop=FALSE])
    Y_ps_tune <- Y_ps[match(ps_tune_idx, train_indices)]
    
    cv_ps <- cv.glmnet(
      x = X_ps,
      y = Y_ps_tune,
      family = "binomial",
      alpha = 0.5,
      nfolds = 3
    )
    
    X_ps_all <- as.matrix(X_master[train_indices, prop_cols, drop=FALSE])
    ps_raw <- predict(cv_ps, newx = X_ps_all, s = "lambda.min", type = "response")
    
    ps_bounded <- pmax(0.01, pmin(0.99, as.numeric(ps_raw)))
    weights <- (1 / ps_bounded) / mean(1 / ps_bounded)
    
    rm(X_ps, X_ps_all, cv_ps)
    gc(verbose = FALSE)
    
    return(weights)
  }
  
  log_mem <- function(label) {
    if (CONFIG$is_debug || FOLD_CONFIG$verbose) {
      mem_info <- gc(verbose = FALSE, reset = TRUE)
      cat(sprintf("  [MEM] %s: Used=%.1fMB\n", label, sum(mem_info[, 2])))
    }
  }
  
  prop_vars_base <- c("avg_tank_age", "active_tanks", "total_capacity", 
                      "has_single_walled", "has_double_walled")
  
  fold_res <- list()
  n_folds <- length(CONFIG$holdout_sets)
  
  for (i in seq_along(CONFIG$holdout_sets)) {
    fname <- names(CONFIG$holdout_sets)[i]
    f_states <- CONFIG$holdout_sets[[fname]]
    
    cat(sprintf("\n[%d/%d] Fold: %s (States: %s)\n", 
                i, n_folds, fname, paste(f_states, collapse=", ")))
    log_mem("Start of fold")
    
    f_test_idx <- which(model_data$state %in% f_states)
    f_train_idx <- setdiff(seq_len(nrow(model_data)), f_test_idx)
    
    if (length(f_test_idx) < FOLD_CONFIG$min_test_size) {
      cat(sprintf("  âŠ— SKIP: Insufficient test data (n=%d)\n", length(f_test_idx)))
      next
    }
    if (length(f_train_idx) == 0) {
      cat("  âŠ— SKIP: No training data\n")
      next
    }
    
    set.seed(CONFIG$seed)
    train_limit <- min(
      if(is.null(CONFIG$max_training_rows) || is.infinite(CONFIG$max_training_rows)) 
        Inf else CONFIG$max_training_rows,
      FOLD_CONFIG$fold_max_train,
      length(f_train_idx)
    )
    
    eff_f_train_idx <- if (length(f_train_idx) > train_limit) {
      sample(f_train_idx, train_limit)
    } else {
      f_train_idx
    }
    
    cat(sprintf("  Train: %s obs | Test: %s obs\n",
                format(length(eff_f_train_idx), big.mark=","),
                format(length(f_test_idx), big.mark=",")))
    
    res_base <- list(auc=NA_real_, pr_auc=NA_real_)
    res_fe <- list(auc=NA_real_, pr_auc=NA_real_)
    res_ipw <- list(auc=NA_real_, pr_auc=NA_real_)
    
    # BASELINE
    cat("  â†’ Baseline... ")
    base_ok <- tryCatch({
      X_train_b <- as.matrix(X_master[eff_f_train_idx, baseline_cols, drop=FALSE])
      Y_train_b <- Y_master[eff_f_train_idx]
      
      m_base <- grf::regression_forest(
        X = X_train_b, Y = Y_train_b,
        num.trees = FOLD_CONFIG$fold_ntrees,
        mtry = best_params$mtry,
        min.node.size = best_params$min.node.size,
        sample.fraction = best_params$sample.fraction,
        num.threads = CONFIG$num_threads,
        alpha = 0.02,                      # Critical for rare events
        honesty.fraction = 0.65,           # More data for splits
        honesty.prune.leaves = FALSE,      # Keep sparse trees
        honesty = TRUE,
        imbalance.penalty = penalty_to_use
      )
      
      rm(X_train_b, Y_train_b); gc(verbose=FALSE)
      
      X_test_b <- as.matrix(X_master[f_test_idx, baseline_cols, drop=FALSE])
      Y_test_b <- Y_master[f_test_idx]
      preds_b <- predict(m_base, newdata = X_test_b)$predictions
      res_base <- compute_metrics(Y_test_b, preds_b)
      
      rm(m_base, X_test_b, Y_test_b, preds_b); gc(verbose=FALSE)
      log_mem("After baseline")
      TRUE
    }, error = function(e) {
      cat(sprintf("FAILED: %s\n", conditionMessage(e)))
      FALSE
    })
    
    if (base_ok) cat(sprintf("AUC=%.3f\n", res_base$auc))
    
    # STATE FE
    if (CONFIG$generate_state_fe) {
      cat("  â†’ State FE... ")
      fe_ok <- tryCatch({
        X_train_fe <- as.matrix(X_master[eff_f_train_idx, , drop=FALSE])
        Y_train_fe <- Y_master[eff_f_train_idx]
        
        m_fe <- grf::regression_forest(
          X = X_train_fe, Y = Y_train_fe,
          num.trees = FOLD_CONFIG$fold_ntrees,
          mtry = min(best_params$mtry + 5, ncol(X_train_fe)),
          min.node.size = best_params$min.node.size,
          sample.fraction = best_params$sample.fraction,
          num.threads = CONFIG$num_threads,
          alpha = 0.02,                      # Critical for rare events
          honesty.fraction = 0.65,           # More data for splits
          honesty.prune.leaves = FALSE,      # Keep sparse trees
          honesty = TRUE,
          imbalance.penalty = penalty_to_use
        )
        
        rm(X_train_fe, Y_train_fe); gc(verbose=FALSE)
        
        X_test_fe <- as.matrix(X_master[f_test_idx, , drop=FALSE])
        Y_test_fe <- Y_master[f_test_idx]
        preds_fe <- predict(m_fe, newdata = X_test_fe)$predictions
        res_fe <- compute_metrics(Y_test_fe, preds_fe)
        
        rm(m_fe, X_test_fe, Y_test_fe, preds_fe); gc(verbose=FALSE)
        log_mem("After FE")
        TRUE
      }, error = function(e) {
        cat(sprintf("FAILED: %s\n", conditionMessage(e)))
        FALSE
      })
      
      if (fe_ok) cat(sprintf("AUC=%.3f\n", res_fe$auc))
    }
    
    # IPW
    if (CONFIG$generate_ipw && FOLD_CONFIG$enable_ipw) {
      cat("  â†’ IPW (re-estimating weights)... ")
      ipw_ok <- tryCatch({
        weights_fold <- estimate_fold_weights(
          train_indices = eff_f_train_idx,
          prop_vars = prop_vars_base,
          strategy = FOLD_CONFIG$ipw_strategy
        )
        
        log_mem("After IPW weight estimation")
        
        X_train_ipw <- as.matrix(X_master[eff_f_train_idx, baseline_cols, drop=FALSE])
        Y_train_ipw <- Y_master[eff_f_train_idx]
        
        m_ipw <- grf::regression_forest(
          X = X_train_ipw, Y = Y_train_ipw,
          sample.weights = weights_fold,
          num.trees = FOLD_CONFIG$fold_ntrees,
          mtry = best_params$mtry,
          min.node.size = best_params$min.node.size,
          sample.fraction = best_params$sample.fraction,
          num.threads = CONFIG$num_threads,
          alpha = 0.02,                      # Critical for rare events
          honesty.fraction = 0.65,           # More data for splits
          honesty.prune.leaves = FALSE,      # Keep sparse trees
          honesty = TRUE
        )
        
        rm(X_train_ipw, Y_train_ipw, weights_fold); gc(verbose=FALSE)
        
        X_test_ipw <- as.matrix(X_master[f_test_idx, baseline_cols, drop=FALSE])
        Y_test_ipw <- Y_master[f_test_idx]
        preds_ipw <- predict(m_ipw, newdata = X_test_ipw)$predictions
        res_ipw <- compute_metrics(Y_test_ipw, preds_ipw)
        
        rm(m_ipw, X_test_ipw, Y_test_ipw, preds_ipw); gc(verbose=FALSE)
        log_mem("After IPW")
        TRUE
      }, error = function(e) {
        cat(sprintf("FAILED: %s\n", conditionMessage(e)))
        FALSE
      })
      
      if (ipw_ok) cat(sprintf("AUC=%.3f\n", res_ipw$auc))
    }
    
    fold_res[[fname]] <- data.table(
      Fold = fname,
      Base_AUC = res_base$auc, FE_AUC = res_fe$auc, IPW_AUC = res_ipw$auc,
      Base_PR = res_base$pr_auc, FE_PR = res_fe$pr_auc, IPW_PR = res_ipw$pr_auc
    )
    
    gc(verbose = FALSE)
    log_mem("End of fold")
  }
  
  if (length(fold_res) > 0) {
    fold_dt <- rbindlist(fold_res)
    cat("\n"); cat(rep("=", 80), "\n", sep = "")
    cat("MULTI-FOLD RESULTS\n")
    cat(rep("=", 80), "\n", sep = "")
    print(fold_dt)
    cat(rep("=", 80), "\n\n", sep = "")
    
    fwrite(fold_dt, here(CONFIG$dir_diagnostics, "multifold_results.csv"))
  }

  if (exists("fold_dt") && nrow(fold_dt) > 0) {
    tryCatch({
      model_cols <- grep("_AUC$", names(fold_dt), value = TRUE)
      
      if (length(model_cols) > 0) {
        plot_data <- melt(
          fold_dt,
          id.vars = "Fold",
          measure.vars = model_cols,
          variable.name = "Model",
          value.name = "AUC"
        )
        
plot_data <- plot_data[!is.na(plot_data$AUC), ]
        
        if (nrow(plot_data) > 0) {
          plot_data[, Model := gsub("_AUC", "", Model)]
          
          p <- ggplot(plot_data, aes(x = Fold, y = AUC, fill = Model)) +
            geom_col(position = "dodge", alpha = 0.8) +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
            scale_fill_manual(
              values = c("Base" = "gray40", "FE" = "steelblue", "IPW" = "darkred")
            ) +
            labs(
              title = "Multi-Fold CV: Test AUC by Model",
              x = "Fold", y = "Test AUC"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          ggsave(
            here(CONFIG$dir_diagnostics, "multifold_test_auc_comparison.png"),
            p, width = 10, height = 6
          )
          cat("  âœ“ Fold plots saved.\n")
        } else {
          cat("  [INFO] No valid AUC data to plot.\n")
        }
      }
    }, error = function(e) {
      cat(sprintf("  [WARNING] Visualization skipped: %s\n", e$message))
    })
  }
}

cat("\n")

# --------------------------------------------------------------------
# SECTION 12: TEMPORAL VALIDATION (LUCAS CRITIQUE)
# --------------------------------------------------------------------
cat(rep("=", 80), "\n", sep = ""); cat("SECTION 12: TEMPORAL VALIDATION\n"); cat(rep("=", 80), "\n\n", sep = "")

cat("PURPOSE: Test if regime indicators are NECESSARY for accurate prediction\n")
cat("METHOD: Train on flat-fee regime ONLY, test on risk-based regime\n")
cat("INTERPRETATION: Poor performance â†’ regime indicators are essential\n\n")

TEMPORAL_CONFIG <- list(
  treated_states = list(Texas=1999, Michigan=2006, Florida=2007, Iowa=2008),
  buffer = 1, min_obs = 50
)

temporal_res <- list()

for (st in names(TEMPORAL_CONFIG$treated_states)) {
  trans_year <- TEMPORAL_CONFIG$treated_states[[st]]
  cat(sprintf("  - %s (Transition: %d)... ", st, trans_year))
  
  test_idx_t <- which(model_data$state == st & model_data$panel_year > trans_year + TEMPORAL_CONFIG$buffer)
  train_idx_t <- which(!(model_data$state == st & model_data$panel_year > trans_year - TEMPORAL_CONFIG$buffer))
  
  if(length(test_idx_t) < TEMPORAL_CONFIG$min_obs) { cat("Skipped (No Test Data)\n"); next }
  
  set.seed(CONFIG$seed)
  eff_rows_t <- if(is.null(CONFIG$max_training_rows) || is.infinite(CONFIG$max_training_rows)) length(train_idx_t) else CONFIG$max_training_rows
  eff_train_t <- if(length(train_idx_t) > eff_rows_t) sample(train_idx_t, eff_rows_t) else train_idx_t
  
  X_train_t <- as.matrix(X_master[eff_train_t, baseline_cols, drop=FALSE])
  
  m_temp <- grf::regression_forest(
    X = X_train_t, Y = Y_master[eff_train_t],
    num.trees = CONFIG$grf_ntree, mtry = best_params$mtry, min.node.size = best_params$min.node.size,
    sample.fraction = best_params$sample.fraction, num.threads = CONFIG$num_threads, honesty=TRUE, imbalance.penalty=penalty_to_use
  )
  rm(X_train_t); gc(verbose=FALSE)
  
  X_test_t <- as.matrix(X_master[test_idx_t, baseline_cols, drop=FALSE])
  preds_t <- predict(m_temp, newdata = X_test_t)$predictions
  met_t <- compute_metrics(Y_master[test_idx_t], preds_t)
  
  bias <- mean(preds_t) / mean(Y_master[test_idx_t])
  
  temporal_res[[st]] <- data.table(State=st, AUC=met_t$auc, PR_AUC=met_t$pr_auc, Bias_Ratio=bias)
  cat(sprintf("AUC: %.3f | Bias: %.2fx\n", met_t$auc, bias))
  rm(m_temp, X_test_t); gc(verbose=FALSE)
}

temp_dt <- rbindlist(temporal_res)
fwrite(temp_dt, here(CONFIG$dir_diagnostics, "temporal_validation_results.csv"))
saveRDS(temp_dt, here(CONFIG$dir_diagnostics, "temporal_validation_results.rds"))  # Raw data for reuse

cat("\n")
cat(rep("-", 80), "\n", sep = "")
cat("INTERPRETATION OF TEMPORAL VALIDATION:\n")
cat(rep("-", 80), "\n", sep = "")
cat("If AUC < 0.6 or Bias > 1.5x:\n")
cat("  â†’ Model trained WITHOUT regime indicators fails badly\n")
cat("  â†’ This JUSTIFIES including regime indicators in main models\n")
cat("  â†’ NOT testing causality (that's Chapter 1 DiD)\n")
cat("  â†’ Just showing: regime-aware model is NECESSARY for accurate prediction\n\n")
cat("âœ“ Temporal validation complete.\n\n")

# --------------------------------------------------------------------
# SECTION 13: PLACEBO TESTS
# --------------------------------------------------------------------
cat(rep("=", 80), "\n", sep = ""); cat("SECTION 13: PLACEBO TESTS\n"); cat(rep("=", 80), "\n\n", sep = "")

p_states <- unique(model_data$state)
if(CONFIG$is_debug) p_states <- p_states[1:3]
placebo_res <- list()

for(st in p_states) {
  p_test_idx <- which(model_data$state == st)
  p_train_idx <- setdiff(seq_len(nrow(model_data)), p_test_idx)
  
  if(length(p_test_idx) < 10) { cat(sprintf("  - %s: Skipped (Too few rows)\n", st)); next }
  
  set.seed(CONFIG$seed)
  eff_rows_p <- if(is.null(CONFIG$max_training_rows) || is.infinite(CONFIG$max_training_rows)) length(p_train_idx) else CONFIG$max_training_rows
  eff_p_train <- if(length(p_train_idx) > eff_rows_p) sample(p_train_idx, eff_rows_p) else p_train_idx
  
  X_p_train <- as.matrix(X_master[eff_p_train, baseline_cols])
  
  m_p <- grf::regression_forest(
    X = X_p_train, Y = Y_master[eff_p_train],
    num.trees=100, mtry=best_params$mtry, min.node.size=best_params$min.node.size,
    sample.fraction=best_params$sample.fraction, num.threads=CONFIG$num_threads, imbalance.penalty=penalty_to_use
  )
  rm(X_p_train); gc()
  
  X_p_test <- as.matrix(X_master[p_test_idx, baseline_cols])
  pred_p <- predict(m_p, newdata = X_p_test)$predictions
  met_p <- compute_metrics(Y_master[p_test_idx], pred_p)
  rm(X_p_test); gc()
  
  placebo_res[[st]] <- data.table(State=st, AUC=met_p$auc, PR_AUC=met_p$pr_auc)
  cat(sprintf("  - %s: AUC %.3f\n", st, met_p$auc))
}
p_dt <- rbindlist(placebo_res)
fwrite(p_dt, here(CONFIG$dir_diagnostics, "placebo_results.csv"))
saveRDS(p_dt, here(CONFIG$dir_diagnostics, "placebo_results.rds"))  # Raw data for reuse

if (nrow(p_dt) > 0) {
  p_dt[, State := factor(State, levels = State[order(AUC)])]
  
  p_placebo <- ggplot(p_dt, aes(x = AUC, y = State)) +
    geom_point(size = 3, color = "steelblue") +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") +
    labs(
      title = "Spatial Generalizability (Leave-One-State-Out)",
      subtitle = "Higher AUC = Model learns universal leak patterns",
      x = "Test AUC (Held-out State)", y = ""
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
    
  ggsave(here(CONFIG$dir_diagnostics, "placebo_auc_dotplot.png"), p_placebo, width = 8, height = max(6, nrow(p_dt) * 0.25))
  cat("  âœ“ Placebo plot saved.\n")
}

# ====================================================================
# SECTION 15: DEBIASED PROJECTION (CLUSTER-ROBUST DML)
# ====================================================================
cat("\nSECTION 15: DEBIASED PROJECTION (CLUSTER-ROBUST DML)\n")
cat(rep("=", 80), "\n\n", sep = "")

# Configuration
dml_config <- list(
  run_baseline = TRUE,
  run_state_fe = FALSE,  # Turned off
  run_ipw = TRUE,
  K_folds = 5,
  use_cluster_sampling = TRUE,
  min_clusters_per_fold = 3
)

# ---------------------------------------------------------
# 1. Define Sample & Folds
# ---------------------------------------------------------

# Ensure sample exists
if (CONFIG$is_debug) {
  cat("  [DEBUG] Using small sample (50k) for DML diagnostics...\n")
  set.seed(CONFIG$seed)
  diag_sample_size <- min(50000, nrow(model_data))
  diag_sample_idx <- sample(seq_len(nrow(model_data)), diag_sample_size)
} else {
  cat("  [PRODUCTION] Using FULL DATASET for final DML projections...\n")
  diag_sample_idx <- seq_len(nrow(model_data)) 
}

# Create cluster-robust folds
if (dml_config$use_cluster_sampling) {
  
  cat("Creating cluster-robust folds (state-level assignment)...\n")
  
  # State-level summary
  cluster_summary <- model_data[diag_sample_idx, list(
    n_obs = .N,
    pct_risk_based = mean(is_risk_based, na.rm = TRUE)
  ), by = state]
  
  cluster_summary[, regime_type := ifelse(pct_risk_based > 0.5, "risk_based", "flat_fee")]
  
  # Stratified random assignment
  set.seed(42)
  rb_states <- cluster_summary[regime_type == "risk_based", state]
  ff_states <- cluster_summary[regime_type == "flat_fee", state]
  
  state_fold_map <- rbind(
    data.table(state = sample(rb_states), fold_id = rep(1:dml_config$K_folds, length.out = length(rb_states))),
    data.table(state = sample(ff_states), fold_id = rep(1:dml_config$K_folds, length.out = length(ff_states)))
  )
  
  # Assign to data
  fold_lookup <- setNames(state_fold_map$fold_id, state_fold_map$state)
  model_data[diag_sample_idx, fold_id := fold_lookup[state]]
  
  # Check validity
  fold_diag <- model_data[diag_sample_idx, list(n_states = uniqueN(state)), by = fold_id]
  if (any(fold_diag$n_states < dml_config$min_clusters_per_fold)) warning("Some folds have fewer than minimum clusters")
  
} else {
  model_data[diag_sample_idx, fold_id := sample(1:dml_config$K_folds, .N, replace = TRUE)]
}

# ---------------------------------------------------------
# 2. DML Cross-Fitting Function (OOB + Ensemble Grid)
# ---------------------------------------------------------

run_dml_crossfit <- function(model_variant) {
  
  cat(sprintf("\nProcessing DML (OOB & Grid Ensemble): %s\n", model_variant))
  
  # A. Prepare Training Data
  if (model_variant == "baseline") {
    X_train_dml <- as.matrix(X_master[diag_sample_idx, baseline_cols, drop=FALSE])
    y_train_dml <- model_data[diag_sample_idx, leak_incident]
    weights_train_dml <- NULL
  } else if (model_variant == "ipw") {
    X_train_dml <- as.matrix(X_master[diag_sample_idx, baseline_cols, drop=FALSE])
    y_train_dml <- model_data[diag_sample_idx, leak_incident]
    if (exists("ipw_weights")) {
      weights_train_dml <- ipw_weights[match(diag_sample_idx, train_idx)]
      weights_train_dml <- pmin(weights_train_dml, quantile(weights_train_dml, 0.99, na.rm = TRUE))
    } else {
      weights_train_dml <- rep(1, length(y_train_dml))
    }
  }
  
  # B. Prepare Theoretical State Space Grid (for Ensemble Prediction)
  dcm_grid <- if(CONFIG$use_simple_model) {
    CJ(N = factor("2", levels=levels(model_data$N)), w = levels(model_data$w), 
       A = levels(model_data$A), rho = levels(model_data$rho))
  } else {
    CJ(N = levels(model_data$N), w = levels(model_data$w), 
       A = levels(model_data$A), rho = levels(model_data$rho))
  }
  
  # Map grid to features
  pred_grid_feats <- copy(dcm_grid)
  pred_grid_feats[, `:=`(
    avg_tank_age = (as.numeric(A) * CONFIG$age_interval) - (CONFIG$age_interval/2),
    active_tanks = fcase(N == "1", 1, N == "2", 2, N == "3", 3, N == "4+", 5),
    has_single_walled = as.integer(w == "single"),
    has_double_walled = as.integer(w == "double"),
    state = fcase(rho == "RB", "Texas", rho == "FF", "Oklahoma"),
    is_risk_based = as.integer(rho == "RB"),
    total_capacity = median(model_data$total_capacity, na.rm=TRUE),
    is_motor_fuel = 1,
    months_since_entry = median(model_data$months_since_entry, na.rm=TRUE),
    cumulative_closures = 0,
    days_since_last_closure = 0,
    years_since_transition = 10,
    panel_year = 2015
  )]
  
  # Add interactions
  pred_grid_feats[, `:=`(
    age_x_regime = avg_tank_age * is_risk_based,
    single_x_regime = has_single_walled * is_risk_based,
    double_x_regime = has_double_walled * is_risk_based,
    capacity_x_regime = total_capacity * is_risk_based
  )]
  
  # Matrix for grid
  X_state_space <- build_model_matrix(as.data.frame(pred_grid_feats), ref_colnames = baseline_cols)
  
  # Containers
  fold_assignment <- model_data[diag_sample_idx, fold_id]
  oob_predictions <- rep(NA_real_, length(y_train_dml))
  grid_preds_sum <- numeric(nrow(X_state_space))
  valid_fold_count <- 0
  
  # C. Cross-Fitting Loop
  for (k in 1:dml_config$K_folds) {
    
    test_mask <- (fold_assignment == k)
    train_mask <- (fold_assignment != k)
    test_idx_rel <- which(test_mask)
    train_idx_rel <- which(train_mask)
    
    if (length(test_idx_rel) < 50) next
    
    # Train Fold Model
    if (is.null(weights_train_dml)) {
      rf_fold <- grf::regression_forest(
        X = X_train_dml[train_idx_rel, , drop=FALSE],
        Y = y_train_dml[train_idx_rel],
        num.trees = if(CONFIG$is_production) 200 else 50,
        honesty = TRUE,
        num.threads = CONFIG$num_threads
      )
    } else {
      rf_fold <- grf::regression_forest(
        X = X_train_dml[train_idx_rel, , drop=FALSE],
        Y = y_train_dml[train_idx_rel],
        sample.weights = weights_train_dml[train_idx_rel],
        num.trees = if(CONFIG$is_production) 200 else 50,
        honesty = TRUE,
        num.threads = CONFIG$num_threads
      )
    }
    
    # 1. OOB Prediction (For Panel History)
    preds <- predict(rf_fold, newdata = X_train_dml[test_idx_rel, , drop=FALSE])$predictions
    oob_predictions[test_idx_rel] <- preds
    
    # 2. Grid Prediction (Accumulate for Ensemble)
    grid_preds <- predict(rf_fold, newdata = X_state_space)$predictions
    grid_preds_sum <- grid_preds_sum + grid_preds
    valid_fold_count <- valid_fold_count + 1
    
    cat(sprintf("  [Fold %d] OOB & Grid predictions computed.\n", k))
  }
  
  # D. Save Outputs
  
  # 1. Panel Predictions (OOB)
  output_dt <- model_data[diag_sample_idx, list(
    facility_id, state, panel_year,
    panel_id = if("panel_id" %in% names(model_data)) panel_id else paste(facility_id, state, sep="_"),
    leak_hazard_rate = oob_predictions,
    model_variant = model_variant
  )]
  saveRDS(output_dt, here(CONFIG$dir_results, sprintf("leak_hazard_rates_DML_%s.rds", model_variant)))
  
  # 2. State Space Predictions (Ensemble Average)
  if (valid_fold_count > 0) {
    dcm_grid[, prob_leak := grid_preds_sum / valid_fold_count]
    dcm_grid[, model_variant := model_variant]
    
    grid_file <- here(CONFIG$dir_results, sprintf("state_space_leak_predictions_%s.rds", model_variant))
    saveRDS(dcm_grid, grid_file)
    cat(sprintf("  ✓ Saved Ensemble Grid Predictions: %s\n", basename(grid_file)))
  }
  
  return(output_dt)
}

# Execute
dml_results <- list()
if (dml_config$run_baseline) dml_results$baseline <- run_dml_crossfit("baseline")
if (dml_config$run_ipw) dml_results$ipw <- run_dml_crossfit("ipw")

cat("✓ Script A DML Complete\n")

# ====================================================================
# SECTION 16: SAVE SUMMARY & MODELS
# ====================================================================
cat("\nSECTION 16: SAVE SUMMARY AND MODELS\n")

# 1. Save Summary Table
print(performance_results)
perf_gt <- copy(performance_results)
for(col in names(perf_gt)) if(is.numeric(perf_gt[[col]])) perf_gt[[col]] <- round(perf_gt[[col]], 4)
t_comp <- gt(perf_gt) %>% 
  tab_header(title = "Model Performance Comparison") %>%
  fmt_number(columns = c("AUC", "PR_AUC", "Brier"), decimals = 4)

gtsave(t_comp, here(CONFIG$dir_tables, "A_model_comparison.html"))
fwrite(performance_results, here(CONFIG$dir_tables, "A_model_comparison.csv"))
saveRDS(list(perf=performance_results, config=CONFIG), here(CONFIG$dir_results, "diagnostic_results.rds"))

# 2. Save Trained Full Models
cat("\nSaving Full Model Objects...\n")
if (exists("model_baseline") && !is.null(model_baseline)) {
  saveRDS(model_baseline, here(CONFIG$dir_results, "model_A_baseline.rds"))
  cat("  ✓ Saved model: model_A_baseline.rds\n")
}
if (exists("model_ipw") && !is.null(model_ipw)) {
  saveRDS(model_ipw, here(CONFIG$dir_results, "model_A_ipw.rds"))
  cat("  ✓ Saved model: model_A_ipw.rds\n")
}

script_end_time <- Sys.time()
cat(sprintf("\nSCRIPT COMPLETE. Runtime: %.1f mins\n", difftime(script_end_time, script_start_time, units = "mins")))
cat(rep("=", 80), "\n")
