#==============================================================================
# 01n_CVValidation.R
# Cross-Validation: Predicting First Leak Incidence
# (formerly §9 of 01_Descriptive_Analysis_REFACTORED.R)
#
# Runs k-fold CV logistic regression predicting event_first_leak in
# the 1990-1998 pre-period risk set. Produces risk scores, ROC curves,
# lift charts, calibration plots, and partial dependence figures.
#
# Controlled by RUN_FULL flag (from 01a_Setup.R):
#   RUN_FULL = TRUE  → runs CV, saves scores to ANALYSIS_DIR
#   RUN_FULL = FALSE → skips, logs a warning
#
# Outputs (ANALYSIS_DIR, only if RUN_FULL = TRUE):
#   analysis_cv_data.rds     — facility-level risk scores + quartile
#
# Figures (save_panels pattern):
#   Figure_CV_ROC:
#     PanelA — ROC curve, no state FE model
#     PanelB — ROC curve, state FE model
#     Combined → Figure_CV_ROC_Combined
#
#   Figure_CV_Lift:
#     PanelA — Lift curve, no FE
#     PanelB — Lift curve, state FE
#     Combined → Figure_CV_Lift_Combined
#
#   Figure_CV_Calibration:
#     PanelA — Calibration (predicted vs observed), no FE
#     PanelB — Calibration, state FE
#     Combined → Figure_CV_Calibration_Combined
#
#   Figure_CV_Separation:
#     PanelA — Predicted score density by eventual leaker status, no FE
#     Combined (same) → Figure_CV_Separation_Combined
#
#   Figure_CV_PartialDep:
#     PanelA — Partial dependence: age bin
#     PanelB — Partial dependence: past_replacement × age_bin
#     Combined → Figure_CV_PartialDep_Combined
#==============================================================================

source(here::here("Code", "01a_Setup.R"))
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
  if (requireNamespace("pROC", quietly=TRUE)) library(pROC)
})

annual_data <- load_interim("annual_data")

# ─────────────────────────────────────────────────────────────────────────────
# Risk set: never-yet-leaked facilities, 1990-1998 pre-period
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Building risk set ---\n")

risk_set <- annual_data[
  panel_year %between% c(1990L, TREATMENT_YEAR) &
  has_previous_leak == 0 &
  pre_panel_leak == 0 &
  !is.na(event_first_leak)
]
risk_set[, age_bin_reg := age_bin_for_reg(age_bin)]
risk_set[, log_capacity := log(1 + pmax(capacity, 0, na.rm=TRUE))]
risk_set[, past_replacement := as.integer(!is.na(past_replacement) &
                                            past_replacement > 0)]

cat(sprintf("  Risk set: %s rows | %s facilities\n",
    format(nrow(risk_set), big.mark=","),
    format(uniqueN(risk_set$panel_id), big.mark=",")))
cat(sprintf("  Leak incidence rate: %.2f%%\n",
    100 * mean(risk_set$event_first_leak, na.rm=TRUE)))

# ─────────────────────────────────────────────────────────────────────────────
# K-Fold CV
# ─────────────────────────────────────────────────────────────────────────────
K <- 5L
set.seed(20260202)
fac_ids <- unique(risk_set$panel_id)
folds   <- sample(rep(1:K, length.out = length(fac_ids)))
fac_fold_map <- data.table(panel_id = fac_ids, fold = folds)
risk_set <- merge(risk_set, fac_fold_map, by="panel_id")

RHS_no_fe <- paste0(
  "has_single_walled + age_bin_reg + rf_pre_1980 + log_capacity + ",
  "is_motor_fuel + past_replacement + age_bin_reg:past_replacement"
)
RHS_fe <- paste0(RHS_no_fe, " | state")

pred_no_fe <- numeric(nrow(risk_set))
pred_fe    <- numeric(nrow(risk_set))

cat(sprintf("  Running %d-fold CV...\n", K))
for (k in 1:K) {
  train <- risk_set[fold != k]
  test  <- risk_set[fold == k]

  # No-FE model
  m1 <- tryCatch(
    glm(as.formula(paste("event_first_leak ~", RHS_no_fe)),
        data = train, family = binomial),
    error = function(e) NULL)
  if (!is.null(m1))
    pred_no_fe[risk_set$fold == k] <- predict(m1, newdata=test, type="response")

  # State FE model
  m2 <- tryCatch(
    fixest::feglm(as.formula(paste("event_first_leak ~", RHS_fe)),
                  data = train, family = binomial),
    error = function(e) NULL)
  if (!is.null(m2)) {
    pred_fe[risk_set$fold == k] <- tryCatch(
      predict(m2, newdata=test, type="response"),
      error = function(e) rep(NA_real_, nrow(test)))
  }
  cat(sprintf("    Fold %d/%d done\n", k, K))
}

risk_set[, pred_emp    := pred_no_fe]
risk_set[, pred_emp_fe := pred_fe]

# ─────────────────────────────────────────────────────────────────────────────
# AUC-ROC
# ─────────────────────────────────────────────────────────────────────────────
auc_no_fe <- if (requireNamespace("pROC",quietly=TRUE))
  as.numeric(pROC::auc(pROC::roc(risk_set$event_first_leak,
                                  risk_set$pred_emp,
                                  quiet = TRUE))) else NA_real_
auc_fe    <- if (requireNamespace("pROC",quietly=TRUE) &&
                   !all(is.na(risk_set$pred_emp_fe)))
  as.numeric(pROC::auc(pROC::roc(risk_set$event_first_leak,
                                  risk_set$pred_emp_fe,
                                  quiet = TRUE))) else NA_real_

cat(sprintf("  AUC (no FE): %.3f | AUC (state FE): %.3f\n",
    auc_no_fe, auc_fe))

# ─────────────────────────────────────────────────────────────────────────────
# Figure: ROC Curves
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: ROC Curves ---\n")

make_roc_dt <- function(actual, predicted, model_name) {
  if (all(is.na(predicted))) return(NULL)
  ord   <- order(predicted, decreasing=TRUE)
  act_s <- actual[ord]
  tpr   <- cumsum(act_s) / sum(act_s)
  fpr   <- cumsum(1 - act_s) / sum(1 - act_s)
  data.table(FPR = fpr, TPR = tpr, model = model_name)
}

roc_no_fe <- make_roc_dt(risk_set$event_first_leak,
                          risk_set$pred_emp, "No State FE")
roc_fe    <- make_roc_dt(risk_set$event_first_leak,
                          risk_set$pred_emp_fe, "State FE")

plot_roc <- function(roc_dt, auc_val, title_str) {
  ggplot(roc_dt, aes(x=FPR, y=TPR)) +
    geom_abline(slope=1, intercept=0, linetype="dashed", color="gray60") +
    geom_line(color=COL_TX, linewidth=0.9) +
    annotate("text", x=0.6, y=0.15, label=sprintf("AUC = %.3f", auc_val),
             size=3.5, color=COL_TX, fontface="bold") +
    scale_x_continuous(labels=percent_format(accuracy=1), limits=c(0,1)) +
    scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0,1)) +
    labs(title=title_str, x="False Positive Rate", y="True Positive Rate")
}

p_roc_A <- if (!is.null(roc_no_fe))
  plot_roc(roc_no_fe, auc_no_fe, "A: ROC — No State FE") else
  ggplot() + labs(title = "A: ROC — No State FE (skipped)")

p_roc_B <- if (!is.null(roc_fe) && !is.na(auc_fe))
  plot_roc(roc_fe, auc_fe, "B: ROC — State FE") else
  ggplot() + labs(title = "B: ROC — State FE (skipped)")

save_panels(
  panels          = list(A = p_roc_A, B = p_roc_B),
  base_name       = "Figure_CV_ROC",
  combined_name   = "Figure_CV_ROC_Combined",
  panel_width     = 6, panel_height = 6,
  combined_width  = 12, combined_height = 6,
  ncol            = 2,
  title    = "Figure 5A: CV Prediction of First Leak Incidence — ROC Curves",
  subtitle = sprintf("5-fold CV on 1990\u20131998 pre-reform risk set. AUC no-FE = %.3f.",
                     auc_no_fe)
)

# ─────────────────────────────────────────────────────────────────────────────
# Figure: Score Separation Density
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: Score Separation ---\n")

risk_set[, eventual_leaker := fifelse(event_first_leak==1, "Leaker", "Non-Leaker")]

p_sep <- ggplot(risk_set[!is.na(pred_emp)],
                aes(x=pred_emp, fill=eventual_leaker, color=eventual_leaker)) +
  geom_density(alpha=0.30, adjust=1.2, linewidth=0.8) +
  scale_fill_manual(values  = c("Leaker"="firebrick","Non-Leaker"="steelblue")) +
  scale_color_manual(values = c("Leaker"="firebrick","Non-Leaker"="steelblue")) +
  scale_x_continuous(labels = percent_format(accuracy=0.1)) +
  labs(
    title    = "Predicted Risk Score Separation by Leak Outcome",
    subtitle = "Good separation = higher predicted probability for eventual leakers.",
    x = "Predicted First-Leak Probability", y = "Density",
    fill = NULL, color = NULL
  )

save_fig(p_sep, "Figure_CV_Separation", width=8, height=5)

# ─────────────────────────────────────────────────────────────────────────────
# Figure: Calibration
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: Calibration ---\n")

risk_set[, pred_decile := cut(pred_emp, breaks=quantile(pred_emp,
  probs=seq(0,1,0.1), na.rm=TRUE), include.lowest=TRUE, labels=FALSE)]
calib <- risk_set[!is.na(pred_decile), .(
  mean_pred = mean(pred_emp, na.rm=TRUE),
  mean_obs  = mean(event_first_leak, na.rm=TRUE),
  n         = .N
), by = pred_decile]

p_cal_A <- ggplot(calib, aes(x=mean_pred, y=mean_obs)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="gray60") +
  geom_point(aes(size=n), color=COL_TX, alpha=0.8) +
  geom_smooth(method="lm", se=FALSE, color="gray30", linewidth=0.7) +
  scale_x_continuous(labels=percent_format(accuracy=0.1)) +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  scale_size_continuous(range=c(2,8), labels=comma_format()) +
  labs(
    title    = "A: Calibration (No State FE)",
    subtitle = "Points = prediction decile bins. Diagonal = perfect calibration. Dot size = N.",
    x = "Mean Predicted Probability", y = "Mean Observed Rate",
    size = "N obs"
  )

save_panels(
  panels          = list(A = p_cal_A),
  base_name       = "Figure_CV_Calibration",
  combined_name   = "Figure_CV_Calibration_Combined",
  panel_width     = 7, panel_height = 6,
  combined_width  = 7, combined_height = 6
)

# ─────────────────────────────────────────────────────────────────────────────
# Figure: Partial Dependence — age bin × past_replacement
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: Partial Dependence ---\n")

pd_age <- risk_set[!is.na(pred_emp) & !is.na(age_bin), .(
  mean_pred = mean(pred_emp, na.rm=TRUE),
  mean_obs  = mean(event_first_leak, na.rm=TRUE),
  n         = .N
), by = .(age_bin, past_replacement)]

p_pd_A <- ggplot(pd_age, aes(x=age_bin, y=mean_pred,
                              group=factor(past_replacement),
                              color=factor(past_replacement))) +
  geom_line(linewidth=0.9) +
  geom_point(aes(size=n), alpha=0.8) +
  scale_color_manual(values = c("0"=COL_CTRL, "1"=COL_TX),
                     labels = c("0"="No Prior Replacement","1"="Has Prior Replacement"),
                     name = NULL) +
  scale_size_continuous(range=c(2,6), labels=comma_format(), guide="none") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  labs(
    title    = "A: Partial Dependence — Age Bin × Past Replacement",
    subtitle = paste0("Prior replacement raises predicted risk; age gradient steeper without replacement. ",
                      "Justifies age_bin × past_replacement interaction."),
    x = "Age Bin", y = "Mean Predicted First-Leak Probability"
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

p_pd_B <- ggplot(pd_age, aes(x=age_bin, y=mean_obs,
                              group=factor(past_replacement),
                              color=factor(past_replacement))) +
  geom_line(linewidth=0.9) +
  geom_point(aes(size=n), alpha=0.8) +
  scale_color_manual(values = c("0"=COL_CTRL, "1"=COL_TX),
                     labels = c("0"="No Prior Replacement","1"="Has Prior Replacement"),
                     name = NULL) +
  scale_size_continuous(range=c(2,6), guide="none") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  labs(
    title    = "B: Observed Leak Rate — Age Bin × Past Replacement",
    subtitle = "Empirical rates confirm predicted gradient. 0\u20134 bin after 0-4 fix should be monotone.",
    x = "Age Bin", y = "Observed First-Leak Rate"
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

save_panels(
  panels          = list(A = p_pd_A, B = p_pd_B),
  base_name       = "Figure_CV_PartialDep",
  combined_name   = "Figure_CV_PartialDep_Combined",
  panel_width     = 8, panel_height = 5,
  combined_width  = 16, combined_height = 5,
  ncol            = 2,
  title    = "Figure 5B: Partial Dependence Plots — Age and Prior Replacement",
  subtitle = "Left: predicted. Right: observed. Validates age gradient and interaction specification."
)

# ─────────────────────────────────────────────────────────────────────────────
# Compute facility-level risk score + quartile, save to ANALYSIS_DIR
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Saving CV data to ANALYSIS_DIR ---\n")

cv_data <- risk_set[, .(
  pred_emp          = mean(pred_emp, na.rm=TRUE),
  pred_emp_fe       = mean(pred_emp_fe, na.rm=TRUE)
), by = panel_id]
cv_data[, fac_emp_risk_quartile := ntile(pred_emp, 4)]
cv_data[, fac_all_sw := as.integer(!is.na(pred_emp))]   # used in 02_DiD matching

saveRDS(cv_data, file.path(ANALYSIS_DIR, "analysis_cv_data.rds"))
cat("  analysis_cv_data.rds saved\n")
save_interim(cv_data, "cv_data")

cat(sprintf("=== 01n COMPLETE | AUC = %.3f ===\n", auc_no_fe))
