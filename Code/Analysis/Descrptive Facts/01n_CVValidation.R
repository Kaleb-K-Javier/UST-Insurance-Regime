#==============================================================================
# 01n_CVValidation.R
# Cross-Validation: Predicting First Leak Incidence
# (formerly §9 of 01_Descriptive_Analysis_REFACTORED.R)
#
# Runs k-fold CV logistic regression predicting event_first_leak in
# the 1990-1998 pre-period risk set. Produces risk scores, ROC curves,
# lift charts, calibration plots, partial dependence figures, risk
# distribution figures, and raw closure rate figures by risk stratum.
#
# Controlled by RUN_FULL flag (from 01a_Setup.R):
#   RUN_FULL = TRUE  → runs full CV + county elastic net + all figures
#   RUN_FULL = FALSE → runs no-FE CV only, skips county model + PD
#
# Outputs (ANALYSIS_DIR):
#   analysis_cv_data.rds — facility-level risk scores + quartile + wall flags
#
# Figures (save_panels / save_fig):
#   Figure_CV_ROC_Combined         — ROC curves (no-FE + state FE)
#   Figure_CV_Lift_NoStateFE       — Lift curve, no FE
#   Figure_CV_Lift_SFE             — Lift curve, state FE
#   Figure_CV_ScoreSep_NoStateFE   — Score separation density, no FE
#   Figure_CV_ScoreSep_SFE         — Score separation density, state FE
#   Figure_CV_Calibration_NoStateFE— Calibration, no FE
#   Figure_CV_Calibration_SFE      — Calibration, state FE
#   Figure_CV_Separation           — Predicted score density by leaker status
#   Figure_leak_risk               — Predicted risk by wall type × age
#   Figure5A_CV_Partial_Dependence_FinalModel — True PD plots
#   Figure_EmpRisk_Score_Distribution         — Risk score histogram
#   Figure_EmpRisk_RawRates                   — Closure rates by risk stratum
#==============================================================================

source(here::here("Code", "Analysis", "Descrptive Facts", "01a_Setup.R"))
cat("=== 01n: CV VALIDATION ===\n")

if (!RUN_FULL) {
  cat("  RUN_FULL = FALSE → CV skipped. Set RUN_FULL = TRUE in 01a_Setup.R\n")
  cat("  analysis_cv_data.rds will NOT be written.\n")
  cat("  02_DiD will proceed without empirical risk scores.\n")
  cat("=== 01n SKIPPED ===\n")
  quit(save = "no")
}

suppressPackageStartupMessages({
  library(fixest)
  if (requireNamespace("pROC", quietly = TRUE)) library(pROC)
})

annual_data <- load_interim("annual_data")

#==============================================================================
# 9.1 BUILD ESTIMATION SAMPLE (RISK SET)
#==============================================================================
cat("\n--- 9.1: Build estimation sample ---\n")

stopifnot("pre_panel_leak" %in% names(annual_data))

# ── Construct past_replacement from closure history ──────────────────────────
# This must happen BEFORE subsetting, on the full annual_data.
annual_data[, past_replacement := as.integer(
  cumsum(shift(n_closures, 1L, fill = 0L)) > 0
), by = panel_id]

# ── Subset to never-yet-leaked, 1990-1998 pre-period ────────────────────────
cv_data <- annual_data[
  panel_year        >= 1990L          &
  panel_year        <= TREATMENT_YEAR &
  has_previous_leak == 0              &
  pre_panel_leak    == 0              &
  !is.na(has_single_walled)           &
  !is.na(age_bin)
]

# ── Verification checks ─────────────────────────────────────────────────────
stopifnot(cv_data[, sum(has_previous_leak)] == 0)
stopifnot(cv_data[, sum(pre_panel_leak)]    == 0)
stopifnot(cv_data[, sum(panel_year > year_of_first_leak, na.rm = TRUE)] == 0)

# ── 0-4 anomaly diagnostic ──────────────────────────────────────────────────
rate_04 <- cv_data[age_bin == "0-4" & past_replacement == 0,
                    mean(event_first_leak, na.rm = TRUE) * 1000]
rate_59 <- cv_data[age_bin == "5-9" & past_replacement == 0,
                    mean(event_first_leak, na.rm = TRUE) * 1000]
cat(sprintf("  VERIFICATION — 0-4 rate (no prior replacement): %.2f per 1,000\n", rate_04))
cat(sprintf("  VERIFICATION — 5-9 rate (no prior replacement): %.2f per 1,000\n", rate_59))
if (!is.na(rate_04) && !is.na(rate_59) && rate_04 <= rate_59 * 1.5)
  cat("  OK: 0-4 anomaly resolved — age gradient monotone as expected.\n") else
  cat("  WARNING: 0-4 rate still elevated — check for remaining contamination.\n")

cat(sprintf("  Facilities with past replacement in risk set: %s (%.1f%%)\n",
  format(uniqueN(cv_data[past_replacement == 1, panel_id]), big.mark = ","),
  100 * uniqueN(cv_data[past_replacement == 1, panel_id]) /
        uniqueN(cv_data$panel_id)))

# ── Construct rf_pre_1980 conditionally ──────────────────────────────────────
if ("install_year" %in% names(cv_data)) {
  cv_data[, rf_pre_1980 := as.integer(!is.na(install_year) & install_year < 1980)]
} else if ("pre1998_install" %in% names(cv_data)) {
  cv_data[, rf_pre_1980 := pre1998_install]
  cat("  WARNING: using pre1998_install proxy for rf_pre_1980\n")
} else {
  cv_data[, rf_pre_1980 := NA_integer_]
  cat("  WARNING: rf_pre_1980 set to NA (install_year absent)\n")
}

# ── Derived covariates ───────────────────────────────────────────────────────
# FIX: use total_capacity (the actual column name), not capacity
cap_col <- if ("total_capacity" %in% names(cv_data)) "total_capacity" else
           if ("capacity" %in% names(cv_data)) "capacity" else NULL

if (!is.null(cap_col)) {
  cv_data[, log_capacity := log(pmax(get(cap_col), 1, na.rm = TRUE))]
} else {
  cv_data[, log_capacity := NA_real_]
  cat("  WARNING: Neither total_capacity nor capacity found — log_capacity = NA\n")
}

# age_bin_for_reg() may return a factor; coerce to numeric so the
# interaction term (age_bin_reg:past_replacement) works correctly in glm/feglm.
cv_data[, age_bin_reg := age_bin_for_reg(age_bin)]
if (is.factor(cv_data$age_bin_reg)) {
  cat("  NOTE: age_bin_reg is factor — coercing to numeric for regression\n")
  cv_data[, age_bin_reg := as.numeric(age_bin_reg)]
}

# ── Fold assignment (stratified by state) ────────────────────────────────────
set.seed(20250128)
cv_data[, fold := sample(1:5, .N, replace = TRUE), by = state]

cat(sprintf("  CV data: %s facility-years | %s first-leak events (%.3f%%)\n",
  format(nrow(cv_data), big.mark = ","),
  format(sum(cv_data$event_first_leak, na.rm = TRUE), big.mark = ","),
  100 * mean(cv_data$event_first_leak, na.rm = TRUE)))
cat(sprintf("  Unique counties in CV data: %s\n",
  format(uniqueN(cv_data$county_fips), big.mark = ",")))

cv_data[, `:=`(pred_no_cfe   = NA_real_,
               pred_with_cfe = NA_real_)]
auc_no_cfe   <- NA_real_
auc_with_cfe <- NA_real_

#──────────────────────────────────────────────────────────────────────────────
# Model specification: elastic net (glmnet) with county FE
#
# Two models:
#   "base"   — facility covariates + year dummies (no geography)
#   "county" — base + county_fips dummies (subsumes state)
#
# Elastic net (alpha = ENET_ALPHA) handles the high-dimensional county
# dummies without separation/convergence issues. Lambda selected by
# inner 5-fold CV (type.measure = "auc") within each outer fold.
#──────────────────────────────────────────────────────────────────────────────
has_rf_pre_1980 <- !all(is.na(cv_data$rf_pre_1980))
has_log_cap     <- !all(is.na(cv_data$log_capacity))
has_county      <- "county_fips" %in% names(cv_data) && !all(is.na(cv_data$county_fips))
cat(sprintf("  rf_pre_1980 available: %s\n", has_rf_pre_1980))
cat(sprintf("  log_capacity available: %s\n", has_log_cap))
cat(sprintf("  county_fips available:  %s (%s unique)\n",
    has_county, if (has_county) format(uniqueN(cv_data$county_fips), big.mark = ",") else "0"))

suppressPackageStartupMessages({
  library(glmnet)
  library(Matrix)
})

ENET_ALPHA <- 0.5   # 0 = ridge, 0.5 = elastic net, 1 = lasso
cat(sprintf("  Elastic net alpha: %.2f\n", ENET_ALPHA))

# ── Build interaction term ───────────────────────────────────────────────────
cv_data[, age_x_repl := age_bin_reg * past_replacement]

# ── Complete-case filter ─────────────────────────────────────────────────────
cc_filter <- !is.na(cv_data$has_single_walled) &
             !is.na(cv_data$age_bin_reg) &
             !is.na(cv_data$is_motor_fuel) &
             !is.na(cv_data$past_replacement) &
             !is.na(cv_data$event_first_leak) &
             !is.na(cv_data$fold)
if (has_rf_pre_1980) cc_filter <- cc_filter & !is.na(cv_data$rf_pre_1980)
if (has_log_cap)     cc_filter <- cc_filter & !is.na(cv_data$log_capacity)
if (has_county)      cc_filter <- cc_filter & !is.na(cv_data$county_fips)

cc_idx <- which(cc_filter)
cat(sprintf("  Complete cases for glmnet: %s / %s (%.1f%%)\n",
    format(length(cc_idx), big.mark = ","),
    format(nrow(cv_data), big.mark = ","),
    100 * length(cc_idx) / nrow(cv_data)))

# ── Build sparse model matrices ONCE on full data ────────────────────────────
# Building once ensures consistent column structure across folds.
cc_dt <- cv_data[cc_idx]

base_rhs <- "has_single_walled + age_bin_reg + is_motor_fuel + past_replacement + age_x_repl"
if (has_rf_pre_1980) base_rhs <- paste(base_rhs, "+ rf_pre_1980")
if (has_log_cap)     base_rhs <- paste(base_rhs, "+ log_capacity")
base_rhs <- paste(base_rhs, "+ factor(panel_year)")

fml_base <- as.formula(paste("~", base_rhs, "- 1"))
X_base   <- sparse.model.matrix(fml_base, data = cc_dt)
cat(sprintf("  X_base: %s rows x %s columns\n",
    format(nrow(X_base), big.mark = ","), ncol(X_base)))

if (has_county) {
  cc_dt[, county_f := factor(county_fips)]
  fml_county <- as.formula(paste("~", base_rhs, "+ county_f - 1"))
  X_county   <- sparse.model.matrix(fml_county, data = cc_dt)
  cat(sprintf("  X_county: %s rows x %s columns (%s county dummies)\n",
      format(nrow(X_county), big.mark = ","), format(ncol(X_county), big.mark = ","),
      format(ncol(X_county) - ncol(X_base), big.mark = ",")))
} else {
  X_county <- NULL
  cat("  WARNING: county_fips not available — county model skipped\n")
}

y_vec    <- cc_dt$event_first_leak
fold_vec <- cc_dt$fold

base_colnames   <- colnames(X_base)
county_colnames <- if (!is.null(X_county)) colnames(X_county) else NULL

cat(sprintf("  Base RHS:   %s\n", base_rhs))
if (has_county) cat(sprintf("  County RHS: %s + county_f\n", base_rhs))


#==============================================================================
# 9.2 5-FOLD CV ELASTIC NET — BASE (NO GEOGRAPHY)
#==============================================================================
cat("\n--- 9.2: 5-fold CV elastic net (base, no geography) ---\n")

pred_base_vec <- rep(NA_real_, length(cc_idx))

for (k in 1:5) {
  train_k <- which(fold_vec != k)
  test_k  <- which(fold_vec == k)

  fit_base_k <- tryCatch(
    cv.glmnet(X_base[train_k, , drop = FALSE], y_vec[train_k],
              family   = "binomial",
              alpha    = ENET_ALPHA,
              nfolds   = 5,
              type.measure = "auc",
              parallel = FALSE),
    error = function(e) {
      cat(sprintf("  WARNING: Fold %d (base) failed: %s\n", k, e$message)); NULL
    }
  )

  if (!is.null(fit_base_k)) {
    pred_base_vec[test_k] <- as.numeric(
      predict(fit_base_k, X_base[test_k, , drop = FALSE],
              s = "lambda.min", type = "response"))
    cat(sprintf("  Fold %d (base): train N = %s | lambda.min = %.6f | inner AUC = %.3f\n",
                k, format(length(train_k), big.mark = ","),
                fit_base_k$lambda.min, max(fit_base_k$cvm)))
  }
}

cv_data[cc_idx, pred_no_cfe := pred_base_vec]


#==============================================================================
# 9.3 5-FOLD CV ELASTIC NET — WITH COUNTY FE
#==============================================================================
cat("\n--- 9.3: 5-fold CV elastic net (with county FE) ---\n")

pred_county_vec <- rep(NA_real_, length(cc_idx))

if (!is.null(X_county)) {
  for (k in 1:5) {
    train_k <- which(fold_vec != k)
    test_k  <- which(fold_vec == k)

    fit_county_k <- tryCatch(
      cv.glmnet(X_county[train_k, , drop = FALSE], y_vec[train_k],
                family   = "binomial",
                alpha    = ENET_ALPHA,
                nfolds   = 5,
                type.measure = "auc",
                parallel = FALSE),
      error = function(e) {
        cat(sprintf("  WARNING: Fold %d (county) failed: %s\n", k, e$message)); NULL
      }
    )

    if (!is.null(fit_county_k)) {
      pred_county_vec[test_k] <- as.numeric(
        predict(fit_county_k, X_county[test_k, , drop = FALSE],
                s = "lambda.min", type = "response"))
      cat(sprintf("  Fold %d (county): train N = %s | lambda.min = %.6f | inner AUC = %.3f | p = %s\n",
                  k, format(length(train_k), big.mark = ","),
                  fit_county_k$lambda.min, max(fit_county_k$cvm),
                  format(ncol(X_county), big.mark = ",")))
    }
  }
  cv_data[cc_idx, pred_with_cfe := pred_county_vec]
} else {
  cat("  Skipped — no county data\n")
}

# ── Full-sample county model (for PD in 9.8b and fallback) ───────────────────
cat("\n  Fitting full-sample elastic net models...\n")

final_model_sfe <- NULL
final_lambda    <- NULL

if (!is.null(X_county)) {
  final_fit_county <- tryCatch(
    cv.glmnet(X_county, y_vec,
              family   = "binomial",
              alpha    = ENET_ALPHA,
              nfolds   = 5,
              type.measure = "auc",
              parallel = FALSE),
    error = function(e) {
      cat(sprintf("  WARNING: Full-sample county model failed: %s\n", e$message)); NULL
    }
  )
  if (!is.null(final_fit_county)) {
    final_model_sfe <- final_fit_county
    final_lambda    <- final_fit_county$lambda.min
    n_nonzero <- sum(coef(final_fit_county, s = "lambda.min") != 0) - 1
    cat(sprintf("  Full-sample county: lambda.min = %.6f | %s non-zero / %s total\n",
                final_lambda, format(n_nonzero, big.mark = ","),
                format(ncol(X_county), big.mark = ",")))
  }
}

final_fit_base <- tryCatch(
  cv.glmnet(X_base, y_vec,
            family   = "binomial",
            alpha    = ENET_ALPHA,
            nfolds   = 5,
            type.measure = "auc",
            parallel = FALSE),
  error = function(e) {
    cat(sprintf("  WARNING: Full-sample base model failed: %s\n", e$message)); NULL
  }
)
if (!is.null(final_fit_base)) {
  n_nonzero_base <- sum(coef(final_fit_base, s = "lambda.min") != 0) - 1
  cat(sprintf("  Full-sample base: lambda.min = %.6f | %s non-zero / %s total\n",
              final_fit_base$lambda.min, n_nonzero_base, ncol(X_base)))
}


#==============================================================================
# 9.4 AUC-ROC + COMBINED ROC PLOT
#==============================================================================
cat("\n--- 9.4: AUC-ROC ---\n")

roc_no_cfe <- roc_with_cfe <- NULL

cv_complete_no <- cv_data[!is.na(pred_no_cfe) & !is.na(event_first_leak)]
if (nrow(cv_complete_no) > 100) {
  roc_no_cfe <- pROC::roc(cv_complete_no$event_first_leak,
                           cv_complete_no$pred_no_cfe, quiet = TRUE)
  auc_no_cfe <- as.numeric(pROC::auc(roc_no_cfe))
  cat(sprintf("  AUC-ROC (Base, No Geography): %.3f\n", auc_no_cfe))
  cat(sprintf("  Interpretation: %s\n",
    fcase(auc_no_cfe >= 0.80, "Good discrimination (>= 0.80)",
          auc_no_cfe >= 0.70, "Acceptable discrimination (0.70-0.79)",
          default            = "Modest discrimination (< 0.70 — expected for rare events)")))
} else {
  cat("  WARNING: Too few OOB predictions for AUC (no-FE model)\n")
}

cv_complete_cfe <- cv_data[!is.na(pred_with_cfe) & !is.na(event_first_leak)]
if (nrow(cv_complete_cfe) > 100) {
  roc_with_cfe <- pROC::roc(cv_complete_cfe$event_first_leak,
                             cv_complete_cfe$pred_with_cfe, quiet = TRUE)
  auc_with_cfe <- as.numeric(pROC::auc(roc_with_cfe))
  cat(sprintf("  AUC-ROC (County Elastic Net): %.3f\n", auc_with_cfe))
}

# Alias for any downstream reference
auc_no_sfe <- auc_no_cfe

if (!is.na(auc_no_cfe) && !is.na(auc_with_cfe)) {
  atten <- (1 - auc_with_cfe / auc_no_cfe) * 100
  cat(sprintf("  AUC improvement with county FE: %.1f%%\n", -atten))
  if (atten > 30) cat("  NOTE: >30% attenuation — substantial cross-county heterogeneity\n")
  else if (atten < 0) cat("  NOTE: County FE improves discrimination — local controls informative\n")
  else cat("  OK: Within-state variation sufficient.\n")
}

# ── Combined ROC figure ──────────────────────────────────────────────────────
roc_list <- list()
if (!is.null(roc_no_cfe))
  roc_list[["no_cfe"]] <- data.table(
    fpr   = 1 - roc_no_cfe$specificities,
    tpr   = roc_no_cfe$sensitivities,
    Model = sprintf("Base, No Geography  (AUC = %.3f)", auc_no_cfe))
if (!is.null(roc_with_cfe))
  roc_list[["with_cfe"]] <- data.table(
    fpr   = 1 - roc_with_cfe$specificities,
    tpr   = roc_with_cfe$sensitivities,
    Model = sprintf("County Elastic Net  (AUC = %.3f)", auc_with_cfe))

if (length(roc_list) > 0) {
  roc_combined <- rbindlist(roc_list)
  model_colors <- setNames(
    c(COL_TX, COL_CTRL)[seq_len(uniqueN(roc_combined$Model))],
    unique(roc_combined$Model))

  fig_roc <- ggplot(roc_combined, aes(x = fpr, y = tpr, color = Model)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "gray60", linewidth = 0.6) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = model_colors) +
    coord_fixed() +
    labs(title    = "ROC Curves: Out-of-Bag Predicted Leak Risk",
         subtitle = "5-Fold Cross-Validation. Dashed = random classifier.",
         x = "False Positive Rate (1 - Specificity)",
         y = "True Positive Rate (Sensitivity)",
         color = NULL) +
    theme(legend.position   = c(0.62, 0.12),
          legend.background = element_rect(fill = "white", color = "gray85",
                                           linewidth = 0.4))
  save_fig(fig_roc, "Figure_CV_ROC_Combined", width = 6, height = 6)
  save_table(roc_combined[, .(Model, fpr, tpr)], "Figure_data_ROC_Combined")
}


#==============================================================================
# 9.5 DISCRIMINATION PLOTS: LIFT, SCORE SEPARATION, CALIBRATION
#==============================================================================
cat("\n--- 9.5: Discrimination plots (lift, score separation, calibration) ---\n")

make_discrimination_plots <- function(pred_col, model_label, file_suffix,
                                      auc_val = NA_real_) {

  cv_cal <- cv_data[!is.na(get(pred_col)) & !is.na(event_first_leak)]
  if (nrow(cv_cal) <= 100) {
    cat(sprintf("  WARNING: Too few obs for discrimination plots (%s)\n", model_label))
    return(invisible(NULL))
  }

  # ---- (A) Lift curve -------------------------------------------------------
  cv_cal_lift <- copy(cv_cal)
  setorderv(cv_cal_lift, pred_col, order = -1L)
  cv_cal_lift[, cumulative_pct_screened := seq_len(.N) / .N]
  cv_cal_lift[, cumulative_pct_events   := cumsum(event_first_leak) /
                                            sum(event_first_leak, na.rm = TRUE)]

  idx_thin   <- unique(c(1L,
                         round(seq(1, nrow(cv_cal_lift), length.out = 600L)),
                         nrow(cv_cal_lift)))
  lift_model <- cv_cal_lift[idx_thin,
                             .(x    = cumulative_pct_screened,
                               y    = cumulative_pct_events,
                               type = model_label)]

  lift_random   <- data.table(x = c(0, 1), y = c(0, 1), type = "Random")
  lift_combined <- rbindlist(list(lift_random, lift_model), fill = TRUE)

  top10_lift <- cv_cal_lift[cumulative_pct_screened <= 0.101,
                             max(cumulative_pct_events, na.rm = TRUE)]
  top20_lift <- cv_cal_lift[cumulative_pct_screened <= 0.201,
                             max(cumulative_pct_events, na.rm = TRUE)]

  fig_lift <- ggplot(lift_combined,
                     aes(x = x, y = y, color = type, linetype = type)) +
    geom_line(linewidth = 1) +
    geom_segment(aes(x = 0.10, xend = 0.10, y = 0,          yend = top10_lift),
                 linetype = "dotted", color = "gray50", linewidth = 0.5,
                 inherit.aes = FALSE) +
    geom_segment(aes(x = 0,    xend = 0.10, y = top10_lift, yend = top10_lift),
                 linetype = "dotted", color = "gray50", linewidth = 0.5,
                 inherit.aes = FALSE) +
    annotate("text", x = 0.115, y = top10_lift - 0.02, hjust = 0, size = 2.8,
             color = "gray30",
             label = sprintf("Top 10%% screens\n%.0f%% of leaks", top10_lift * 100)) +
    annotate("text", x = 0.215, y = top20_lift + 0.025, hjust = 0, size = 2.8,
             color = "gray30",
             label = sprintf("Top 20%% → %.0f%% leaks", top20_lift * 100)) +
    scale_color_manual(values    = c("Random" = "gray60",
                                      setNames(COL_TX, model_label))) +
    scale_linetype_manual(values = c("Random" = "dashed",
                                      setNames("solid", model_label))) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title    = sprintf("Lift Curve: %s", model_label),
         subtitle = ifelse(!is.na(auc_val),
           sprintf("OOB predictions | AUC = %.3f", auc_val),
           "OOB predictions"),
         x     = "Fraction of Facilities Screened (descending risk order)",
         y     = "Cumulative Fraction of First-Leak Events Captured",
         color = NULL, linetype = NULL) +
    theme(legend.position = "bottom")

  # ---- (B) Score-separation density -----------------------------------------
  cv_cal[, outcome_label := fifelse(event_first_leak == 1L,
                                     "First Leak (event)", "No Leak")]
  x_cap_sep <- quantile(cv_cal[[pred_col]], 0.99, na.rm = TRUE)

  fig_sep <- ggplot(
    cv_cal[get(pred_col) <= x_cap_sep],
    aes(x = get(pred_col), fill = outcome_label, color = outcome_label)
  ) +
    geom_density(alpha = 0.35, linewidth = 0.7) +
    scale_fill_manual(values  = c("First Leak (event)" = COL_TX,
                                   "No Leak"           = COL_CTRL)) +
    scale_color_manual(values = c("First Leak (event)" = COL_TX,
                                   "No Leak"           = COL_CTRL)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title    = sprintf("Score Separation: %s", model_label),
         subtitle = sprintf(
           "OOB predicted probability by actual outcome | AUC = %.3f\n%s",
           ifelse(is.na(auc_val), 0, auc_val),
           "Good discrimination = little overlap between distributions."),
         x     = "Predicted P(First Leak)",
         y     = "Density",
         fill  = NULL, color = NULL) +
    theme(legend.position = "bottom")

  # ---- (C) Calibration check ------------------------------------------------
  breaks <- unique(quantile(cv_cal[[pred_col]],
                             probs = seq(0, 1, by = 0.1), na.rm = TRUE))
  if (length(breaks) >= 3) {
    cv_cal[, decile := as.integer(cut(get(pred_col), breaks = breaks,
                                       include.lowest = TRUE, labels = FALSE))]
    cal_tbl <- cv_cal[!is.na(decile), .(
      mean_predicted = round(mean(get(pred_col),    na.rm = TRUE), 4),
      mean_actual    = round(mean(event_first_leak, na.rm = TRUE), 4),
      mean_sw_share  = round(mean(has_single_walled, na.rm = TRUE), 3),
      n_fac_years    = .N,
      n_leaks        = sum(event_first_leak, na.rm = TRUE)
    ), by = decile][order(decile)]

    bottom_rate <- cal_tbl[decile == min(decile), mean_actual]
    top_rate    <- cal_tbl[decile == max(decile), mean_actual]
    cal_tbl[, lift := round(mean_actual / bottom_rate, 2)]
    if (!is.na(bottom_rate) && bottom_rate > 0)
      cat(sprintf("  [%s] Top decile: %.4f | Bottom: %.4f | Lift: %.1fx\n",
                  model_label, top_rate, bottom_rate, top_rate / bottom_rate))

    fig_cal <- ggplot(cal_tbl, aes(x = mean_predicted, y = mean_actual)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
      geom_line(color = COL_TX, linewidth = 0.7) +
      geom_point(aes(size = mean_sw_share), color = COL_TX, alpha = 0.85) +
      geom_text(aes(label = decile), vjust = -0.8, size = 2.6, color = "gray30") +
      scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      scale_size_continuous(range  = c(2, 8),
                            labels = scales::percent_format(accuracy = 1)) +
      labs(title    = sprintf("Calibration Check: %s", model_label),
           subtitle = "Decile bins of OOB predicted probability. Near-diagonal = well-calibrated.",
           x    = "Mean Predicted Probability",
           y    = "Mean Actual Incidence Rate",
           size = "Avg SW\nShare") +
      theme(legend.position = "right")

    save_fig(fig_cal, paste0("Figure_CV_Calibration_", file_suffix),
             width = 6, height = 4.5)
    save_table(cal_tbl, paste0("Table_CV_Calibration_", file_suffix))
  }

  save_fig(fig_lift, paste0("Figure_CV_Lift_",     file_suffix), width = 6,   height = 5.5)
  save_fig(fig_sep,  paste0("Figure_CV_ScoreSep_", file_suffix), width = 6,   height = 4.5)

  cat(sprintf("  [%s] Top 10%% screens %.0f%% of leaks | Top 20%% screens %.0f%%\n",
              model_label, top10_lift * 100, top20_lift * 100))

  invisible(NULL)
}

make_discrimination_plots("pred_no_cfe",   "Base (No Geography)", "Base",    auc_no_cfe)
make_discrimination_plots("pred_with_cfe", "County Elastic Net",  "County",  auc_with_cfe)


#==============================================================================
# 9.6 PARTIAL DEPENDENCE LEVEL ORDERING
#==============================================================================
cat("\n--- 9.6: Defining PD level ordering ---\n")

all_pd_levels <- c("Double-Walled", "Single-Walled",
                   "Pre-1980 = No", "Pre-1980 = Yes",
                   AGE_BIN_LABELS)


#==============================================================================
# 9.8 EMPIRICAL RISK SCORE — PREDICTED PROBABILITIES
#==============================================================================
cat("\n--- 9.8: Empirical risk score ---\n")

# ── Prefer OOB county-model predictions; fall back to base OOB ────────────────
if ("pred_with_cfe" %in% names(cv_data)) {
  oob_preds <- cv_data[!is.na(pred_with_cfe),
                        .(panel_id, panel_year, pred_oob = pred_with_cfe)]
  if ("pred_oob" %in% names(cv_data)) cv_data[, pred_oob := NULL]
  cv_data <- merge(cv_data, oob_preds, by = c("panel_id", "panel_year"), all.x = TRUE)
  n_matched <- sum(!is.na(cv_data$pred_oob))
  pct_oob   <- round(100 * n_matched / nrow(cv_data), 1)
  cat(sprintf("  OOB predictions matched: %s / %s (%.1f%%)\n",
              format(n_matched, big.mark = ","),
              format(nrow(cv_data), big.mark = ","), pct_oob))
  if (pct_oob < 80) cat("  WARNING: <80% OOB match rate\n")
} else {
  cv_data[, pred_oob := NA_real_]
  cat("  OOB predictions not available\n")
}

n_need_pred <- sum(is.na(cv_data$pred_oob))
cat(sprintf("  Facility-years needing final-model prediction: %s\n",
            format(n_need_pred, big.mark = ",")))

# ── Final (in-sample) fallback using full-sample base elastic net ─────────────
# Use the already-fitted final_fit_base to predict for rows that lack OOB
# predictions (e.g. rows outside complete-case set for county model).
if (!is.null(final_fit_base)) {
  # Predict for all complete-case rows using base model
  pred_final_vec <- rep(NA_real_, nrow(cv_data))
  pred_final_vec[cc_idx] <- as.numeric(
    predict(final_fit_base, X_base, s = "lambda.min", type = "response"))
  cv_data[, pred_final := pred_final_vec]

  complete_final <- cv_data[!is.na(pred_final) & !is.na(event_first_leak)]
  if (nrow(complete_final) > 100) {
    roc_final <- pROC::roc(complete_final$event_first_leak,
                            complete_final$pred_final, quiet = TRUE)
    auc_final <- as.numeric(pROC::auc(roc_final))
    cat(sprintf("  Final base model in-sample AUC: %.3f (optimistic upper bound)\n", auc_final))
    if (!is.na(auc_with_cfe))
      cat(sprintf("  CV OOB AUC was %.3f — gap %.3f = in-sample optimism\n",
                  auc_with_cfe, auc_final - auc_with_cfe))
  }
} else {
  cv_data[, pred_final := NA_real_]
}

# ── Combine: OOB preferred, final-model fallback ────────────────────────────
cv_data[, pred_emp := fcase(
  !is.na(pred_oob),   pred_oob,
  !is.na(pred_final), pred_final,
  default             = NA_real_
)]
cv_data[, pred_source := fcase(
  !is.na(pred_oob),                     "OOB (cross-validated)",
  is.na(pred_oob) & !is.na(pred_final), "Final model (in-sample)",
  default                               = "Missing"
)]
cat("\n  Prediction source breakdown:\n")
print(cv_data[, .N, by = pred_source])


#==============================================================================
# 9.7 FIGURE: PREDICTED LEAK RISK BY WALL TYPE × AGE
#==============================================================================
cat("\n--- 9.7: Figure leak_risk — predicted risk by wall type x age ---\n")

if (!"wall_label" %in% names(cv_data))
  cv_data[, wall_label := fifelse(has_single_walled == 1,
                                   "Single-Walled", "Double-Walled")]

# Aggregate: mean predicted probability + fold-based CV SE
# Each facility-year is OOB for exactly one fold. Compute the cell mean
# separately for each fold's OOB predictions, then use across-fold SD
# as the SE. This captures model estimation uncertainty (different
# training sets → different coefficients), not just within-cell
# composition heterogeneity — which with N~100K would be invisible.

# Step 1: fold-level cell means using OOB predictions ONLY
# In-sample fallback predictions (pred_source != "OOB") don't vary across
# folds, so including them would artificially shrink the across-fold SE.
fold_means <- cv_data[pred_source == "OOB (cross-validated)" & !is.na(fold), .(
  fold_mean = mean(pred_emp, na.rm = TRUE),
  fold_n    = .N
), by = .(age_bin, wall_label, fold)]

# Step 2: across-fold SE of the cell mean
fold_se <- fold_means[, .(
  se_risk = sd(fold_mean, na.rm = TRUE) / sqrt(.N),
  n_folds = .N
), by = .(age_bin, wall_label)]

# Step 3: point estimates from ALL pred_emp (OOB + fallback)
cell_means <- cv_data[!is.na(pred_emp), .(
  mean_risk   = mean(pred_emp, na.rm = TRUE),
  n_fac_years = .N
), by = .(age_bin, wall_label)]

# Merge
tbl3_leak_rates_pooled <- merge(cell_means, fold_se,
                                 by = c("age_bin", "wall_label"), all.x = TRUE)

tbl3_leak_rates_pooled[, `:=`(
  ci_lo   = mean_risk - 1.96 * se_risk,
  ci_hi   = mean_risk + 1.96 * se_risk,
  age_bin = factor(age_bin, levels = AGE_BIN_LABELS),
  lbl     = sprintf("%.1f%%", mean_risk * 100)
)]

fig5b_risk <- ggplot(
  tbl3_leak_rates_pooled[!is.na(mean_risk)],
  aes(x = age_bin, y = mean_risk,
      color = wall_label, fill = wall_label,
      linetype = wall_label, group = wall_label)
) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15,
              linetype = "blank", show.legend = FALSE) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_text(aes(label = lbl), vjust = -0.9, size = 2.5, show.legend = FALSE) +
  scale_x_discrete(limits = AGE_BIN_LABELS, drop = FALSE) +
  scale_color_manual(values    = c("Single-Walled" = COL_TX,
                                    "Double-Walled" = COL_CTRL)) +
  scale_fill_manual(values     = c("Single-Walled" = COL_TX,
                                    "Double-Walled" = COL_CTRL)) +
  scale_linetype_manual(values = c("Single-Walled" = "solid",
                                    "Double-Walled" = "dashed")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x        = "Tank Age Bin (5-year intervals)",
       y        = "Predicted First-Leak Probability",
       title    = "Predicted Leak Risk by Tank Age and Wall Type (Pooled)",
       subtitle = paste0(
         "1990-1998. Never-leaked risk set. Model-predicted probability from ",
         "5-fold CV logistic regression; age x replacement interaction removes ",
         "the spurious 0-4 year elevation driven by replacement-site observations. ",
         "Shaded bands = 95% CI from across-fold variation in cell means (K=5)."
       ),
       color    = "Wall Type",
       linetype = "Wall Type",
       fill     = "Wall Type") +
  guides(fill = "none") +
  theme(axis.text.x    = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.subtitle   = element_text(size = 8, color = "gray30"))

save_fig(fig5b_risk, "Figure_leak_risk", width = 8, height = 6)
cat("  OK: Figure_leak_risk saved\n")


#==============================================================================
# 9.8b TRUE PARTIAL DEPENDENCE PLOTS (uses final_model_sfe = glmnet)
#==============================================================================
cat("\n--- 9.8b: True partial dependence plots ---\n")

if (!is.null(final_model_sfe) && !is.null(X_county)) {

  # PD for glmnet: modify a column in the model matrix, re-predict.
  # X_county is the full-data matrix aligned with cc_idx rows.
  pd_predict <- function(X_mod) {
    mean(predict(final_model_sfe, newx = X_mod, s = final_lambda,
                 type = "response"), na.rm = TRUE)
  }

  # Helper: modify a named column in the sparse matrix
  pd_set_col <- function(X, col_name, val) {
    X_tmp <- X
    if (col_name %in% colnames(X_tmp)) {
      X_tmp[, col_name] <- val
    }
    X_tmp
  }

  age_reg_map <- unique(cv_data[!is.na(age_bin) & !is.na(age_bin_reg),
                                 .(age_bin = as.character(age_bin), age_bin_reg)])
  age_reg_map <- age_reg_map[age_bin %in% AGE_BIN_LABELS]
  setorder(age_reg_map, age_bin_reg)

  # Wall type PD
  pd_wall <- rbindlist(list(
    data.table(
      level       = "Double-Walled",
      mean_pred   = round(pd_predict(pd_set_col(X_county, "has_single_walled", 0)), 4),
      mean_actual = round(cv_data[has_single_walled == 0,
                                   mean(event_first_leak, na.rm = TRUE)], 4),
      n_fac_years = cv_data[has_single_walled == 0, .N],
      covariate   = "Wall Type"
    ),
    data.table(
      level       = "Single-Walled",
      mean_pred   = round(pd_predict(pd_set_col(X_county, "has_single_walled", 1)), 4),
      mean_actual = round(cv_data[has_single_walled == 1,
                                   mean(event_first_leak, na.rm = TRUE)], 4),
      n_fac_years = cv_data[has_single_walled == 1, .N],
      covariate   = "Wall Type"
    )
  ))

  # Vintage PD (conditional)
  if (has_rf_pre_1980 && "rf_pre_1980" %in% colnames(X_county)) {
    pd_vintage <- rbindlist(list(
      data.table(
        level       = "Pre-1980 = No",
        mean_pred   = round(pd_predict(pd_set_col(X_county, "rf_pre_1980", 0)), 4),
        mean_actual = round(cv_data[rf_pre_1980 == 0,
                                     mean(event_first_leak, na.rm = TRUE)], 4),
        n_fac_years = cv_data[rf_pre_1980 == 0, .N],
        covariate   = "Pre-1980 Vintage"
      ),
      data.table(
        level       = "Pre-1980 = Yes",
        mean_pred   = round(pd_predict(pd_set_col(X_county, "rf_pre_1980", 1)), 4),
        mean_actual = round(cv_data[rf_pre_1980 == 1,
                                     mean(event_first_leak, na.rm = TRUE)], 4),
        n_fac_years = cv_data[rf_pre_1980 == 1, .N],
        covariate   = "Pre-1980 Vintage"
      )
    ))
  } else {
    pd_vintage <- data.table()
    cat("  NOTE: rf_pre_1980 unavailable — Pre-1980 Vintage PD skipped\n")
  }

  # Age PD: hold past_replacement = 0 to isolate the pure age effect
  pd_age <- rbindlist(lapply(seq_len(nrow(age_reg_map)), function(i) {
    lbl     <- age_reg_map$age_bin[i]
    reg_val <- age_reg_map$age_bin_reg[i]
    # Modify age_bin_reg, zero out past_replacement and interaction
    X_tmp <- X_county
    X_tmp[, "age_bin_reg"]      <- reg_val
    X_tmp[, "past_replacement"] <- 0
    X_tmp[, "age_x_repl"]      <- 0   # reg_val * 0 = 0
    data.table(
      level       = lbl,
      mean_pred   = round(pd_predict(X_tmp), 4),
      mean_actual = round(cv_data[age_bin == lbl & past_replacement == 0,
                                   mean(event_first_leak, na.rm = TRUE)], 4),
      n_fac_years = cv_data[age_bin == lbl & past_replacement == 0, .N],
      covariate   = "Tank Age (5-yr bins)"
    )
  }))

  pd_combined <- rbindlist(list(pd_wall, pd_vintage, pd_age),
                            use.names = TRUE, fill = TRUE)
  save_table(pd_combined, "Table_CV_Partial_Dependence_FinalModel")

  cat("  Partial dependence estimates (final model):\n")
  print(pd_combined[, .(covariate, level, mean_pred, mean_actual, n_fac_years)])

  pd_long <- melt(pd_combined,
                  id.vars       = c("covariate", "level", "n_fac_years"),
                  measure.vars  = c("mean_pred", "mean_actual"),
                  variable.name = "type", value.name = "rate")

  pd_long[, type := factor(fcase(
    type == "mean_pred",   "Partial Dependence (PD)",
    type == "mean_actual", "Observed (marginal)",
    default = "Other"),
    levels = c("Partial Dependence (PD)", "Observed (marginal)"))]

  active_pd_levels <- all_pd_levels[all_pd_levels %in% pd_long$level]
  pd_long[, level := factor(as.character(level), levels = active_pd_levels)]

  fig_pd <- ggplot(pd_long, aes(x = level, y = rate, fill = type)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    facet_wrap(~ covariate, scales = "free_x", nrow = 1) +
    scale_fill_manual(values = c("Partial Dependence (PD)" = COL_TX,
                                 "Observed (marginal)"      = COL_CTRL)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    labs(x        = NULL,
         y        = "Predicted First-Leak Probability",
         title    = "Risk Factor Partial Dependence (Full-Sample County Elastic Net)",
         subtitle = paste0(
           "Each bar shows the mean predicted probability when every facility is ",
           "counterfactually assigned that level (other covariates held at observed). ",
           "Age PD computed holding past_replacement = 0 to isolate the pure age effect. ",
           "County + year effects absorbed via elastic net (alpha = ", ENET_ALPHA, "). ",
           "Observed bars = raw subgroup mean (replacement-free sites). ",
           "Pre-period 1990-1998 | Never-leaked risk set."
         ),
         fill = NULL) +
    theme(axis.text.x    = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.subtitle   = element_text(size = 8, color = "gray30"))

  save_fig(fig_pd, "Figure5A_CV_Partial_Dependence_FinalModel", width = 13, height = 5)
  cat("  OK: Figure5A_CV_Partial_Dependence_FinalModel saved (county elastic net)\n")

} else {
  cat("  WARNING: final_model_sfe is NULL — skipping PD plots\n")
}


#==============================================================================
# 9.9 AGGREGATE TO FACILITY LEVEL
#==============================================================================
cat("\n--- 9.9: Aggregate to facility level ---\n")

fac_risk_1998 <- cv_data[!is.na(pred_emp), .(
  fac_emp_risk             = mean(pred_emp, na.rm = TRUE),
  fac_emp_risk_sd          = sd(pred_emp,   na.rm = TRUE),
  fac_emp_risk_lastyear    = pred_emp[which.max(panel_year)],
  fac_emp_risk_lastyear_yr = max(panel_year),
  n_pred_years             = .N,
  pct_oob                  = mean(pred_source == "OOB (cross-validated)", na.rm = TRUE)
), by = .(panel_id, texas_treated)]

cat(sprintf("  Facilities with empirical risk score: %s\n",
            format(nrow(fac_risk_1998), big.mark = ",")))
cat(sprintf("  Facilities with >= 5 pre-period years: %s\n",
            format(sum(fac_risk_1998$n_pred_years >= 5), big.mark = ",")))
cat(sprintf("  Median n_pred_years: %.0f\n", median(fac_risk_1998$n_pred_years)))

n_no_score <- uniqueN(annual_data$panel_id) - nrow(fac_risk_1998)
if (n_no_score > 0)
  cat(sprintf("  NOTE: %s facilities have no empirical risk score\n",
              format(n_no_score, big.mark = ",")))


#==============================================================================
# 9.10 ALL-SINGLE-WALLED FLAG
#==============================================================================
cat("\n--- 9.10: All-single-walled flag (from tanks_1998) ---\n")

# tanks_1998 may not be in scope from 01a_Setup.R — try loading it
if (!exists("tanks_1998")) {
  tanks_1998 <- tryCatch(load_interim("tanks_1998"), error = function(e) NULL)
}

if (is.null(tanks_1998) || !exists("tanks_1998") || nrow(tanks_1998) == 0) {
  cat("  WARNING: tanks_1998 not available — constructing fac_all_sw from cv_data\n")
  # Fallback: derive all-SW flag from has_single_walled in cv_data at facility level
  fac_wall_fallback <- cv_data[panel_year == max(panel_year), .(
    fac_all_sw       = as.integer(all(has_single_walled == 1, na.rm = TRUE)),
    fac_all_sw_loose = as.integer(all(has_single_walled == 1, na.rm = TRUE)),
    n_tanks          = NA_integer_,
    n_sw             = NA_integer_,
    n_dw             = NA_integer_,
    n_unknown_wall   = NA_integer_
  ), by = panel_id]
  fac_risk_1998 <- merge(fac_risk_1998, fac_wall_fallback,
                          by = "panel_id", all.x = TRUE)
} else {

fac_wall_type <- tanks_1998[, .(
  n_tanks        = .N,
  n_sw           = sum(single_walled == 1,  na.rm = TRUE),
  n_dw           = sum(double_walled == 1,  na.rm = TRUE),
  n_unknown_wall = sum(is.na(single_walled) |
                       (single_walled == 0 & double_walled == 0), na.rm = TRUE)
), by = panel_id]

fac_wall_type[, fac_all_sw       := as.integer(n_sw == n_tanks &
                                                n_dw == 0 &
                                                n_unknown_wall == 0)]
fac_wall_type[, fac_all_sw_loose := as.integer(n_dw == 0 & (n_sw + n_dw) > 0)]

all_sw_counts <- fac_wall_type[
  panel_id %in% fac_risk_1998$panel_id,
  .(N_facilities    = .N,
    N_all_sw_strict = sum(fac_all_sw),
    N_all_sw_loose  = sum(fac_all_sw_loose),
    Pct_strict      = round(100 * mean(fac_all_sw), 1),
    Pct_loose       = round(100 * mean(fac_all_sw_loose), 1))
]
cat("  All-single-walled facilities (among risk-scored sample):\n")
print(all_sw_counts)

fac_risk_1998 <- merge(
  fac_risk_1998,
  fac_wall_type[, .(panel_id, fac_all_sw, fac_all_sw_loose,
                     n_tanks, n_sw, n_dw, n_unknown_wall)],
  by = "panel_id", all.x = TRUE
)
} # end if tanks_1998 available


#==============================================================================
# 9.11 RISK QUARTILES & TERTILES (POOLED CUTOFFS)
#==============================================================================
cat("\n--- 9.11: Risk quartiles & tertiles ---\n")

risk_dist_tbl <- fac_risk_1998[, .(
  Mean   = round(mean(fac_emp_risk,           na.rm = TRUE), 4),
  SD     = round(sd(fac_emp_risk,             na.rm = TRUE), 4),
  P10    = round(quantile(fac_emp_risk, 0.10, na.rm = TRUE), 4),
  P25    = round(quantile(fac_emp_risk, 0.25, na.rm = TRUE), 4),
  Median = round(quantile(fac_emp_risk, 0.50, na.rm = TRUE), 4),
  P75    = round(quantile(fac_emp_risk, 0.75, na.rm = TRUE), 4),
  P90    = round(quantile(fac_emp_risk, 0.90, na.rm = TRUE), 4)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

cat("  Empirical risk score distribution:\n")
print(risk_dist_tbl)

ks_test_risk <- ks.test(
  fac_risk_1998[texas_treated == 1, fac_emp_risk],
  fac_risk_1998[texas_treated == 0, fac_emp_risk]
)
cat(sprintf("  KS test (TX vs CTL): D = %.4f, p = %.4f %s\n",
            ks_test_risk$statistic, ks_test_risk$p.value,
            stars_fn(ks_test_risk$p.value)))
if (ks_test_risk$p.value < 0.05)
  cat("  NOTE: Distributions differ — report both raw and IPW-weighted DiD\n") else
  cat("  OK: Risk score distributions similar across groups\n")

# FIX: use cut() with explicit quantile breaks instead of dplyr::ntile
q_cuts_pooled  <- quantile(fac_risk_1998$fac_emp_risk,
                            probs = c(0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE)
q_cuts_tertile <- quantile(fac_risk_1998$fac_emp_risk,
                            probs = c(0, 1/3, 2/3, 1.0), na.rm = TRUE)

fac_risk_1998[, fac_emp_risk_quartile := cut(
  fac_emp_risk, breaks = unique(q_cuts_pooled), include.lowest = TRUE,
  labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")[
    seq_len(length(unique(q_cuts_pooled)) - 1)])]
fac_risk_1998[, fac_emp_risk_tertile := cut(
  fac_emp_risk, breaks = unique(q_cuts_tertile), include.lowest = TRUE,
  labels = c("T1 (Lowest)", "T2", "T3 (Highest)")[
    seq_len(length(unique(q_cuts_tertile)) - 1)])]

cat("\n  All-SW facilities by risk quartile:\n")
print(fac_risk_1998[!is.na(fac_emp_risk_quartile), .(
  N_facilities = .N,
  N_all_sw     = sum(fac_all_sw, na.rm = TRUE),
  Pct_all_sw   = round(100 * mean(fac_all_sw, na.rm = TRUE), 1)
), by = fac_emp_risk_quartile][order(fac_emp_risk_quartile)])


#==============================================================================
# 9.12 MERGE INTO annual_data
#==============================================================================
cat("\n--- 9.12: Merge into annual_data ---\n")

merge_cols <- c("panel_id", "fac_emp_risk", "fac_emp_risk_sd",
                "fac_emp_risk_lastyear", "fac_emp_risk_quartile",
                "fac_emp_risk_tertile", "n_pred_years", "pct_oob",
                "fac_all_sw", "fac_all_sw_loose",
                "n_tanks", "n_sw", "n_dw", "n_unknown_wall")

existing <- intersect(merge_cols[-1], names(annual_data))
if (length(existing) > 0) annual_data[, (existing) := NULL]

annual_data <- merge(
  annual_data, fac_risk_1998[, ..merge_cols],
  by = "panel_id", all.x = TRUE
)

cat(sprintf("  annual_data rows with fac_emp_risk: %s / %s (%.1f%%)\n",
  format(sum(!is.na(annual_data$fac_emp_risk)), big.mark = ","),
  format(nrow(annual_data), big.mark = ","),
  100 * mean(!is.na(annual_data$fac_emp_risk))))

n_no_score_ann <- annual_data[panel_year == TREATMENT_YEAR & is.na(fac_emp_risk), .N]
if (n_no_score_ann > 0)
  cat(sprintf("  Facilities at treatment year without score: %s\n",
              format(n_no_score_ann, big.mark = ",")))


#==============================================================================
# 9.13 SUMMARY TABLE
#==============================================================================
cat("\n--- 9.13: Summary table ---\n")

snap <- annual_data[panel_year == TREATMENT_YEAR & !is.na(fac_emp_risk_quartile)]

risk_summary <- snap[, .(
  N_fac         = .N,
  Mean_risk     = round(mean(fac_emp_risk, na.rm = TRUE), 4),
  SD_risk       = round(sd(fac_emp_risk,   na.rm = TRUE), 4),
  Pct_Q4        = round(100 * mean(fac_emp_risk_quartile == "Q4 (Highest)",
                                   na.rm = TRUE), 1),
  N_all_sw      = sum(fac_all_sw, na.rm = TRUE),
  Pct_all_sw    = round(100 * mean(fac_all_sw, na.rm = TRUE), 1),
  Pct_Q4_all_sw = round(100 * mean(
    fac_emp_risk_quartile == "Q4 (Highest)" & fac_all_sw == 1, na.rm = TRUE), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"),
           Spec  = fcase(spec_A_eligible == 1, "Spec A",
                         spec_B_eligible == 1, "Spec B",
                         default = "Mixed"))]

setorder(risk_summary, Spec, Group)
cat("  Risk summary at treatment year:\n")
print(risk_summary)
save_table(risk_summary, "Table9_13_FacRisk_Summary")

write_tex(
  kbl(risk_summary, format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      caption = paste("Facility-level empirical risk score at December 22, 1998.",
        "Risk score is the mean cross-validated predicted first-leak probability",
        "from the pre-period logistic regression (1990--1998 facility-years",
        "in the never-yet-leaked risk set). All-single-walled: every active",
        "tank is single-walled with no missing wall-type records.",
        "Q4 = top quartile of the pooled risk score distribution."),
      label = "tab:fac-risk-summary") |>
    kable_styling(latex_options = c("HOLD_position"), font_size = 10,
                  full_width = FALSE) |>
    footnote(general = paste(
      "Never-leaked risk set excludes facilities with any LUST report before 1990",
      "(pre\\_panel\\_leak) or any within-panel prior leak (has\\_previous\\_leak).",
      "County elastic net model used where available; base model as fallback.",
      "All models include age $\\times$ past-replacement interaction to absorb",
      "the spurious 0--4 year elevation from replacement-site observations."),
      general_title = "", threeparttable = TRUE),
  "Table9_13_FacRisk_Summary"
)


#==============================================================================
# 9.14 RISK SCORE DISTRIBUTION FIGURE
#==============================================================================
cat("\n--- 9.14: Risk score distribution figure ---\n")

risk_stats <- fac_risk_1998[!is.na(fac_emp_risk), .(
  mean_r   = mean(fac_emp_risk,   na.rm = TRUE),
  median_r = median(fac_emp_risk, na.rm = TRUE)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

x_cap <- quantile(fac_risk_1998$fac_emp_risk, 0.99, na.rm = TRUE)

annot_label <- paste0(
  sprintf("%-10s  %6s  %6s\n", "Group", "Mean", "Median"),
  paste(
    risk_stats[order(Group),
               sprintf("%-10s  %6s  %6s",
                       Group,
                       scales::percent(mean_r,   accuracy = 0.1),
                       scales::percent(median_r, accuracy = 0.1))],
    collapse = "\n"
  )
)

fig_risk_dist <- ggplot(
  fac_risk_1998[!is.na(fac_emp_risk)],
  aes(x    = fac_emp_risk,
      fill = fifelse(texas_treated == 1, "Texas", "Control"))
) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 0.005,
    alpha    = 0.45,
    position = "identity",
    color    = NA
  ) +
  geom_vline(xintercept = q_cuts_pooled[2:4],
             linetype = "dashed", color = "gray40", linewidth = 0.5) +
  annotate("text", x = q_cuts_pooled[2:4], y = Inf,
           vjust = 1.5, hjust = -0.1,
           label = c("Q1|Q2", "Q2|Q3", "Q3|Q4"),
           size = 2.8, color = "gray35") +
  geom_vline(data      = risk_stats,
             aes(xintercept = mean_r, color = Group),
             linetype  = "solid", linewidth = 1.0) +
  geom_vline(data      = risk_stats,
             aes(xintercept = median_r, color = Group),
             linetype  = "dotted", linewidth = 0.9) +
  annotate("label",
           x          = x_cap * 0.68,
           y          = Inf,
           vjust      = 1.4,
           hjust      = 0,
           size       = 2.5,
           fill       = "white",
           color      = "gray20",
           label      = annot_label) +
  scale_fill_manual(values  = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
  scale_color_manual(values = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    limits = c(0, x_cap),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(x        = "Empirical Risk Score (Predicted P(First Leak))",
       y        = "Density",
       title    = "Empirical Risk Score Distribution at Treatment Date",
       subtitle = paste0(
         "Facility mean of CV-predicted annual first-leak probability, 1990-1998. ",
         "Bars = 0.5 pp bins. X-axis capped at 99th pct (",
         scales::percent(x_cap, accuracy = 0.1), "). ",
         "Solid lines = group means; dotted = medians. Dashed grey = pooled quartile cutoffs."
       ),
       fill  = NULL,
       color = NULL) +
  guides(color = "none") +
  coord_cartesian(clip = "off")

save_fig(fig_risk_dist, "Figure_EmpRisk_Score_Distribution", width = 9, height = 5)


#==============================================================================
# 9.15 RAW CLOSURE RATE FIGURE BY RISK STRATUM
#==============================================================================
cat("\n--- 9.15: Raw closure rates by risk stratum ---\n")

build_rate_series <- function(dt, label) {
  dt[panel_year >= 1990 & panel_year <= 2015 & spec_A_eligible == 1, .(
    rate    = mean(closure_event, na.rm = TRUE),
    Group   = fifelse(texas_treated == 1, "Texas", "Control"),
    Stratum = label
  ), by = panel_year]
}

series_list <- list(
  build_rate_series(
    annual_data[fac_emp_risk_quartile == "Q1 (Lowest)"],
    "Placebo: Bottom-risk quartile (Q1)"
  ),
  build_rate_series(
    annual_data[fac_all_sw == 1],
    "All single-walled facilities"
  ),
  build_rate_series(
    annual_data[fac_emp_risk_quartile == "Q4 (Highest)"],
    "Highest-risk quartile (Q4)"
  )
)
raw_rates_combined <- rbindlist(series_list, fill = TRUE)

stratum_levels <- c(
  "Placebo: Bottom-risk quartile (Q1)",
  "All single-walled facilities",
  "Highest-risk quartile (Q4)"
)
raw_rates_combined[, Stratum := factor(Stratum, levels = stratum_levels)]

if (nrow(raw_rates_combined) > 0) {
  fig_risk_rates <- ggplot(raw_rates_combined,
    aes(x = panel_year, y = rate, color = Group, linetype = Group)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    geom_vline(xintercept = TREATMENT_YEAR + 0.5, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    annotate("text", x = TREATMENT_YEAR + 0.7, y = Inf,
             label = "Dec 1998\ndeadline", vjust = 1.4, hjust = 0,
             size = 2.6, color = "gray35") +
    facet_wrap(~ Stratum, nrow = 1, scales = "free_y") +
    scale_color_manual(values    = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
    scale_linetype_manual(values = c("Texas" = "solid", "Control" = "dashed")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_x_continuous(breaks = seq(1990, 2015, 5)) +
    labs(x        = "Year",
         y        = "Annual Tank-Closure Rate",
         title    = "Annual Tank-Closure Rates by Pre-Period Empirical Risk Stratum (Spec A)",
         subtitle = paste0(
           "Each panel is a risk sub-sample defined by pre-1999 characteristics. ",
           "Left panel (Q1) is a placebo — these facilities faced the same deadline ",
           "but had low predicted leak risk, so we expect little response. ",
           "Right panel (Q4) contains the highest-risk facilities, where the ",
           "deadline should bite hardest. Texas (orange) vs. control-state peers (blue)."
         ),
         color    = NULL,
         linetype = NULL) +
    theme(axis.text.x     = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text      = element_text(face = "bold", size = 9),
          plot.subtitle   = element_text(size = 8, color = "gray30")) +
    coord_cartesian(clip = "off")

  save_fig(fig_risk_rates, "Figure_EmpRisk_RawRates", width = 13, height = 5.5)
}


#==============================================================================
# SAVE ANALYSIS OUTPUT + INTERIM
#==============================================================================
cat("\n--- Saving CV data to ANALYSIS_DIR ---\n")

# Save facility-level file for 02_DiD
analysis_cv_data <- fac_risk_1998[, .(
  panel_id, fac_emp_risk, fac_emp_risk_sd,
  fac_emp_risk_lastyear, fac_emp_risk_quartile,
  fac_emp_risk_tertile, n_pred_years, pct_oob,
  fac_all_sw, fac_all_sw_loose,
  n_tanks, n_sw, n_dw, n_unknown_wall
)]

saveRDS(analysis_cv_data, file.path(ANALYSIS_DIR, "analysis_cv_data.rds"))
cat("  analysis_cv_data.rds saved\n")
save_interim(analysis_cv_data, "cv_data")

# Also save updated annual_data with risk scores merged
save_interim(annual_data, "annual_data")
cat("  annual_data (with risk scores) saved to interim\n")


#==============================================================================
# SUMMARY
#==============================================================================
cat("\n========================================\n")
cat("SECTION 9 (01n) COMPLETE\n")
cat(sprintf("  CV sample: %s facility-years | %s first-leak events\n",
  format(nrow(cv_data), big.mark = ","),
  format(sum(cv_data$event_first_leak, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  Empirical risk score built for %s facilities\n",
  format(nrow(fac_risk_1998), big.mark = ",")))
cat(sprintf("  All-SW: TX = %s | CTL = %s\n",
  format(fac_risk_1998[texas_treated == 1, sum(fac_all_sw, na.rm = TRUE)], big.mark = ","),
  format(fac_risk_1998[texas_treated == 0, sum(fac_all_sw, na.rm = TRUE)], big.mark = ",")))
cat(sprintf("  Q4 (Spec A, at treatment year): TX = %s | CTL = %s\n",
  format(annual_data[texas_treated == 1 & panel_year == TREATMENT_YEAR &
                     spec_A_eligible == 1 &
                     fac_emp_risk_quartile == "Q4 (Highest)", .N], big.mark = ","),
  format(annual_data[texas_treated == 0 & panel_year == TREATMENT_YEAR &
                     spec_A_eligible == 1 &
                     fac_emp_risk_quartile == "Q4 (Highest)", .N], big.mark = ",")))
cat(sprintf("  AUC: Base = %.3f | County Elastic Net = %.3f\n",
  ifelse(is.na(auc_no_cfe),   0, auc_no_cfe),
  ifelse(is.na(auc_with_cfe), 0, auc_with_cfe)))
cat("========================================\n\n")

rm(fac_wall_type, series_list, raw_rates_combined, snap)
gc()

cat("=== 01n COMPLETE ===\n")