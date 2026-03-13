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
#   Ranger probability forest (ranger package). Two specifications:
#     (1) No state feature  — purely facility-level observables
#     (2) With state feature — allows forest to learn state-specific baselines
#
#   OOB predicted probabilities are read directly from fit$predictions.
#   No separate CV loop is needed — ranger's bootstrap gives each observation
#   ~37% chance of being OOB per tree, and with 2,000 trees every observation
#   has a stable OOB prediction.
#
# CLASS IMBALANCE:
#   Release events are rare (~1-3% annual base rate). Without correction the
#   forest learns to predict the majority class (no release) everywhere.
#   Fix: case.weights = n_0/n_1 for all event == 1 rows, 1.0 for event == 0.
#   Each tree's bootstrap sample then sees roughly equal 0s and 1s, forcing
#   the forest to learn the discriminating signal in the minority class.
#
# COVARIATES (insurer's observable portfolio at start of policy year):
#   has_single_walled  — time-varying wall composition (binary)
#   age_bin            — current active stock age (3-year bins)
#   active_tanks       — portfolio size
#   total_capacity     — total capacity (gallons)
#   has_gasoline_year  — gasoline fuel indicator
#   has_diesel_year    — diesel fuel indicator
#   state              — included in Model 2 only
#
# ESTIMATION SAMPLE:
#   All states (TX + controls), pre-treatment (panel_year < POST_YEAR).
#   Never-yet-leaked risk set: has_previous_leak == 0.
#   Make-model primary sample: is_make_model == 1.
#
# OUTPUTS
# -------
# Main figures (Output/Figures/):
#   Figure_CV_ROC_Combined          2-panel ROC: no-state | with-state
#   Figure_CV_PartialDep            PD: predicted hazard by age bin x wall type
#   Figure_CV_Calibration_Combined  2-panel calibration decile plot
#
# Appendix figures (Output/Figures/):
#   FigureA_CV_Lift_NoState         Lift curve, no-state model
#   FigureA_CV_Lift_State           Lift curve, with-state model
#   FigureA_CV_ScoreSep_NoState     Score separation density, no-state
#   FigureA_CV_ScoreSep_State       Score separation density, with-state
#   FigureA_CV_VarImportance        Variable importance, both models
#
# Tables (Output/Tables/):
#   Table_CV_GoF_Summary            AUC + lift summary, both models
#   Table_CV_Calibration_NoState    Decile calibration, no-state model
#   Table_CV_Calibration_State      Decile calibration, with-state model
#   Table_CV_CellRates              Predicted + observed rates by (wall, age) cell
#   Table_CV_VarImportance          Variable importance scores
#
# Risk score outputs (Data/Analysis/):
#   analysis_cv_data_fac_year.rds   Facility-year predictions (panel_id x panel_year)
#   analysis_cv_data_fac.rds        Facility-level score at reform year (panel_id only)
################################################################################


#### S1: Setup ################################################################

source(here::here("Code", "Analysis", "Descrptive Facts", "01a_Setup.R"))
cat("=== 01n: Observable Risk — Actuarial Validation ===\n\n")

suppressPackageStartupMessages({
  library(grf)
  library(pROC)
  library(patchwork)
})

annual_data <- readRDS(file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))

# ---- Constants ----
NUM_TREES  <- 2000L   # final forest size; tuning uses fewer trees internally
SNAP_YEAR  <- POST_YEAR - 1L   # 1998 — reform snapshot year

WALL_COLS <- c(
  "All Single-Walled"  = COL_TX,
  "All Double-Walled"  = COL_CTRL
)


#### S2: Estimation Sample ####################################################

required_cols <- c(
  "is_make_model", "has_single_walled", "age_bin",
  "active_tanks", "total_capacity",
  "has_gasoline_year", "has_diesel_year",
  "event_first_leak", "has_previous_leak",
  "state", "panel_id", "panel_year", "texas_treated"
)
missing_cols <- setdiff(required_cols, names(annual_data))
if (length(missing_cols) > 0)
  stop("annual_data missing required columns: ",
       paste(missing_cols, collapse = ", "))

cv_data <- annual_data[
  panel_year        <  POST_YEAR    &   # pre-treatment only
  has_previous_leak == 0L           &   # never yet leaked within panel
  is_make_model     == 1L           &   # make-model primary sample
  has_single_walled %in% c(0L, 1L)  &   # classified wall only
  !is.na(age_bin)                   &
  !is.na(active_tanks)              &
  !is.na(total_capacity)            &
  !is.na(event_first_leak)
]

# Enforce factor levels
cv_data[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
cv_data[, state   := factor(state)]

# Sanity checks
stopifnot(all(cv_data$has_previous_leak == 0L))
stopifnot(all(cv_data$is_make_model     == 1L))

n_events   <- sum(cv_data$event_first_leak)
n_total    <- nrow(cv_data)
event_rate <- mean(cv_data$event_first_leak)

cat("Estimation sample (all states, pre-treatment):\n")
cat(sprintf("  Facility-years:    %s\n",    format(n_total, big.mark = ",")))
cat(sprintf("  Unique facilities: %s\n",
    format(uniqueN(cv_data$panel_id), big.mark = ",")))
cat(sprintf("  States:            %d (TX + %d controls)\n",
    uniqueN(cv_data$state),
    uniqueN(cv_data[texas_treated == 0L, state])))
cat(sprintf("  First-leak events: %s (%.3f%% base rate)\n",
    format(n_events, big.mark = ","), event_rate * 100))
cat(sprintf("  Class ratio (0:1): %.0f:1\n",
    (n_total - n_events) / n_events))


#### S3: Class Weights ########################################################
#
# Release events are rare. Without correction the forest learns to predict
# the majority class (no release) everywhere.
#
# Fix: weight each positive observation by n_0/n_1 so the expected number of
# 1s equals the expected number of 0s in each bootstrap sample. The forest
# is then forced to learn what distinguishes the 1s from the 0s.
#
# GRF's probability_forest accepts sample.weights during TRAINING (unlike
# survival_forest where weights only apply at prediction time).
#
# Note: this does NOT collapse predicted probabilities to 50/50.
# GRF outputs P(Y=1|X) on the original scale by averaging leaf proportions
# computed on weighted observations. Calibration plots in S8 verify this.

n_0     <- sum(cv_data$event_first_leak == 0L)
n_1     <- sum(cv_data$event_first_leak == 1L)
w_ratio <- n_0 / n_1

cv_data[, case_weight := fifelse(
  event_first_leak == 1L, w_ratio, 1.0
)]

cat(sprintf("\nClass weighting: w(1) = %.1f, w(0) = 1.0\n", w_ratio))
cat(sprintf("  Upweighting %s rare events by factor %.1fx\n",
    format(n_1, big.mark = ","), w_ratio))


#### S4: Fit GRF Probability Forests ##########################################
#
# Using grf::probability_forest with three key arguments:
#
#   clusters = state_int
#     State-level grouped bootstrap. Every tree holds out ALL observations
#     from one or more states together. OOB predictions for a state come only
#     from trees that never saw any observation from that state. With 20 state
#     clusters this is the correct analogue of leave-one-state-out CV, and it
#     prevents the forest from memorising state-level patterns in-sample.
#
#   sample.weights = case_weight
#     Applied during training (GRF probability_forest supports this fully,
#     unlike survival_forest which only applies weights at prediction time).
#
#   tune.parameters = "all"
#     GRF internally cross-validates to select: mtry, min.node.size,
#     sample.fraction, alpha, imbalance.penalty. The tuning CV also respects
#     the clusters argument so hyperparameter selection is state-level OOB.
#     Tuned values are logged after fitting.
#
# GRF requires numeric X. Encodings:
#   age_bin   -> age_bin_int: integer 1..K preserving bin order
#   state     -> state_int:   integer 1..20 (arbitrary; forest uses random
#                             splits so treats it as unordered categorical)

FEATURES <- c("has_single_walled", "age_bin_int",
               "active_tanks", "total_capacity",
               "has_gasoline_year", "has_diesel_year",
               "state_int")

# Encode ordered factor age_bin as integer (preserves ordinality)
cv_data[, age_bin_int := as.integer(age_bin)]
# Encode state as integer cluster ID
cv_data[, state_int   := as.integer(factor(state))]

# Build X matrix and Y/W vectors once
X    <- as.matrix(cv_data[, FEATURES, with = FALSE])
Y    <- as.integer(cv_data$event_first_leak)
W    <- cv_data$case_weight
CL   <- cv_data$state_int   # cluster vector — one integer per row

X_nostate <- X[, setdiff(FEATURES, "state_int"), drop = FALSE]

cat(sprintf("\nGRF feature matrix: %d rows x %d cols\n", nrow(X), ncol(X)))
cat(sprintf("Clusters (states): %d unique\n", uniqueN(CL)))

# Helper to safely extract and print tuned parameters
log_tuned_params <- function(fit, label) {
  if (!is.null(fit$tuning.output)) {
    tp <- fit$tuning.output$params
    cat(sprintf("  [%s] Tuned: mtry=%s  min.node=%s  sample.frac=%.3f  alpha=%.3f\n",
        label,
        ifelse(is.null(tp["mtry"]),          "?", tp["mtry"]),
        ifelse(is.null(tp["min.node.size"]),  "?", tp["min.node.size"]),
        ifelse(is.null(tp["sample.fraction"]), NA, as.numeric(tp["sample.fraction"])),
        ifelse(is.null(tp["alpha"]),           NA, as.numeric(tp["alpha"]))))
  } else {
    cat(sprintf("  [%s] No tuning output available\n", label))
  }
}

# Helper to safely read OOB error (field name varies by GRF version)
oob_error <- function(fit) {
  if (!is.null(fit$debiased.error))  return(round(fit$debiased.error, 5))
  if (!is.null(fit$prediction.error)) return(round(fit$prediction.error, 5))
  return(NA_real_)
}

# ---- Model 1: facility observables only (no state feature) ----
cat(sprintf("\nFitting no-state model (%d trees, state-level OOB, tuning all)...\n",
    NUM_TREES))
set.seed(20260202L)
fit_nostate <- probability_forest(
  X                = X_nostate,
  Y                = factor(Y, levels = c(0L, 1L)),
  num.trees        = NUM_TREES,
  sample.weights   = W,
  clusters         = CL,
  tune.parameters  = "all",
  seed             = 20260202L
)
cat(sprintf("  OOB error: %.5f\n", oob_error(fit_nostate)))
log_tuned_params(fit_nostate, "no-state")

# ---- Model 2: with state feature ----
cat(sprintf("\nFitting with-state model (%d trees, state-level OOB, tuning all)...\n",
    NUM_TREES))
set.seed(20260202L)
fit_state <- probability_forest(
  X                = X,
  Y                = factor(Y, levels = c(0L, 1L)),
  num.trees        = NUM_TREES,
  sample.weights   = W,
  clusters         = CL,
  tune.parameters  = "all",
  seed             = 20260202L
)
cat(sprintf("  OOB error: %.5f\n", oob_error(fit_state)))
log_tuned_params(fit_state, "with-state")

# OOB predicted P(Y=1) — GRF stores these in fit$predictions[, "1"]
cv_data[, pred_nostate := fit_nostate$predictions[, "1"]]
cv_data[, pred_state   := fit_state$predictions[,   "1"]]

cat(sprintf("\nOOB prediction coverage: %.1f%% (non-NA)\n",
    100 * mean(!is.na(cv_data$pred_nostate))))


#### S5: Variable Importance ##################################################
#
# GRF computes importance as the weighted frequency with which each variable
# was chosen for a split, weighted by node size. Returned by
# variable_importance(fit) as an unnamed numeric vector ordered by column
# position in X. We attach feature names manually.

vi_nostate <- variable_importance(fit_nostate)
vi_state   <- variable_importance(fit_state)

feat_nostate <- setdiff(FEATURES, "state_int")   # columns used in model 1

imp_ns <- data.table(
  variable           = feat_nostate,
  importance_nostate = as.numeric(vi_nostate)
)
imp_st <- data.table(
  variable         = FEATURES,
  importance_state = as.numeric(vi_state)
)
imp <- merge(imp_ns, imp_st, by = "variable", all = TRUE)
# Normalise to % of total within each model
imp[, importance_nostate := importance_nostate /
      sum(importance_nostate, na.rm = TRUE) * 100]
imp[, importance_state   := importance_state /
      sum(importance_state,   na.rm = TRUE) * 100]
setorder(imp, -importance_nostate)
fwrite(imp, file.path(OUTPUT_TABLES, "Table_CV_VarImportance.csv"))
cat("\nVariable importance saved.\n")

# Appendix figure
imp_long <- melt(imp,
  id.vars      = "variable",
  measure.vars = c("importance_nostate", "importance_state"),
  variable.name = "model", value.name = "importance"
)
imp_long[, model := fifelse(
  model == "importance_nostate", "No State Feature", "With State Feature"
)]
imp_long[, variable := factor(
  variable,
  levels = imp$variable[order(imp$importance_nostate)]
)]

fig_imp <- ggplot(imp_long, aes(x = importance, y = variable, fill = model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, alpha = 0.85) +
  scale_fill_manual(values = c(
    "No State Feature"   = COL_CTRL,
    "With State Feature" = COL_TX
  )) +
  labs(x = "Importance (% impurity reduction)", y = NULL, fill = NULL) +
  theme_pub() +
  theme(legend.position = "bottom")

ggsave(file.path(OUTPUT_FIGURES, "FigureA_CV_VarImportance.png"),
       fig_imp, width = 7, height = 4.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "FigureA_CV_VarImportance.pdf"),
       fig_imp, width = 7, height = 4.5, device = cairo_pdf)
cat("  FigureA_CV_VarImportance saved\n")


#### S6: ROC + Combined ROC Figure ############################################
#
# Each panel shows:
#   geom_line()  — smooth ROC curve using all threshold points
#   geom_point() — hollow open circles (shape = 1, fill = NA) at all points.
#                  Open circles allow full overlap without fill obscuring
#                  the line or each other underneath.

roc_ns <- pROC::roc(cv_data$event_first_leak, cv_data$pred_nostate, quiet = TRUE)
roc_st <- pROC::roc(cv_data$event_first_leak, cv_data$pred_state,   quiet = TRUE)
auc_ns <- as.numeric(pROC::auc(roc_ns))
auc_st <- as.numeric(pROC::auc(roc_st))

cat(sprintf("\nAUC — No State: %.3f | With State: %.3f\n", auc_ns, auc_st))

# Helper: build data table from a pROC object — all points, no thinning
make_roc_dt <- function(roc_obj, label) {
  data.table(
    fpr   = rev(1 - roc_obj$specificities),
    tpr   = rev(roc_obj$sensitivities),
    panel = label
  )
}

roc_ns_dt <- make_roc_dt(roc_ns, "No State Feature")
roc_st_dt <- make_roc_dt(roc_st, "With State Feature")

roc_all_dt <- rbindlist(list(roc_ns_dt, roc_st_dt))

auc_labels <- data.table(
  panel = c("No State Feature", "With State Feature"),
  label = c(sprintf("AUC = %.3f", auc_ns),
             sprintf("AUC = %.3f", auc_st)),
  x = 0.65, y = 0.10
)

fig_roc <- ggplot() +
  # 45-degree reference line
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey55", linewidth = 0.5) +
  # ROC curve — all points as continuous line
  geom_line(data = roc_all_dt,
            aes(x = fpr, y = tpr),
            color = COL_TX, linewidth = 0.9) +
  # Hollow scatter points at all thresholds — shape 1 = open circle, fill = NA
  # Open circles overlap cleanly without fill blocking each other
  geom_point(data = roc_all_dt,
             aes(x = fpr, y = tpr),
             shape  = 1,
             size   = 1.0,
             stroke = 0.4,
             color  = COL_TX,
             fill   = NA) +
  geom_label(data = auc_labels,
             aes(x = x, y = y, label = label),
             inherit.aes   = FALSE,
             size          = 3.2,
             fill          = "white",
             color         = "grey20",
             label.size    = 0.25,
             label.padding = unit(0.22, "lines")) +
  facet_wrap(~panel, nrow = 1) +
  coord_fixed() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.25)) +
  labs(x = "False Positive Rate (1 \u2212 Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_pub() +
  theme(strip.text = element_text(face = "bold", size = 10))

ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_ROC_Combined.png"),
       fig_roc, width = 9, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_ROC_Combined.pdf"),
       fig_roc, width = 9, height = 5, device = cairo_pdf)
cat("  Figure_CV_ROC_Combined saved\n")


#### S7: Partial Dependence Figure ############################################
#
# Marginal PD: for each (has_single_walled, age_bin) cell, override those
# two columns across ALL rows of the estimation sample and call predict() on
# the counterfactual dataset using the no-state model. Average over the full
# empirical distribution of all other covariates (size, capacity, fuel type).
#
# Y-axis is scaled to per-1,000 facility-years (pd_mean * 1000) for
# readability. Point labels show the per-1,000 rate to one decimal place.
#
# Observed cell rates are computed and saved to Table_CV_CellRates.csv for
# the appendix but are NOT shown in the main figure — the main figure is
# clean model predictions only.
#
# Age bins: 3-year intervals. Bin labels defined here to be self-contained
# and independent of AGE_BIN_LABELS (which may differ between 02a and 01n).
# The integer encoding fed to the GRF model during training uses the same
# ordering as these labels.

PD_BIN_LABELS <- c(
  "0-2yr", "3-5yr", "6-8yr", "9-11yr", "12-14yr",
  "15-17yr", "18-20yr", "21-23yr", "24-26yr", "27-29yr", "30yr+"
)

pd_grid <- CJ(
  has_single_walled = c(0L, 1L),
  age_bin_int       = seq_along(PD_BIN_LABELS)
)

feat_nostate <- setdiff(FEATURES, "state_int")

pd_rows <- lapply(seq_len(nrow(pd_grid)), function(i) {
  # Counterfactual dataset: fix wall and age, keep everything else observed
  tmp <- copy(cv_data[, feat_nostate, with = FALSE])
  tmp[, has_single_walled := pd_grid$has_single_walled[i]]
  tmp[, age_bin_int       := pd_grid$age_bin_int[i]]
  X_tmp <- as.matrix(tmp)
  preds <- predict(fit_nostate, newdata = X_tmp)$predictions[, "1"]
  data.table(
    has_single_walled = pd_grid$has_single_walled[i],
    age_bin           = PD_BIN_LABELS[pd_grid$age_bin_int[i]],
    pd_mean           = mean(preds, na.rm = TRUE),
    pd_per1k          = mean(preds, na.rm = TRUE) * 1000
  )
})
pd_dt <- rbindlist(pd_rows)

# Observed cell rates — saved to table for appendix, not shown in main figure
obs_dt <- cv_data[, .(
  obs_mean     = mean(event_first_leak, na.rm = TRUE),
  obs_per1k    = mean(event_first_leak, na.rm = TRUE) * 1000,
  n_fac_years  = .N,
  n_events     = sum(event_first_leak)
), by = .(has_single_walled, age_bin = as.character(age_bin))]

pd_dt <- merge(pd_dt, obs_dt,
               by = c("has_single_walled", "age_bin"), all.x = TRUE)

pd_dt[, wall_label := factor(
  fifelse(has_single_walled == 1L, "Single-Walled", "Double-Walled"),
  levels = c("Single-Walled", "Double-Walled")
)]
pd_dt[, age_bin := factor(age_bin, levels = PD_BIN_LABELS)]

fwrite(pd_dt, file.path(OUTPUT_TABLES, "Table_CV_CellRates.csv"))

# --- Figure: clean model predictions, per-1,000 scale, point labels ---
pd_colors <- c(
  "Single-Walled"  = COL_TX,
  "Double-Walled"  = COL_CTRL
)
pd_lty <- c(
  "Single-Walled"  = "solid",
  "Double-Walled"  = "dashed"
)

# Label nudge direction: push SW labels up, DW labels down to avoid overlap
pd_dt[, label_vjust := fifelse(wall_label == "Single-Walled", -0.8, 1.6)]

fig_pd <- ggplot(
  pd_dt[!is.na(pd_mean)],
  aes(x = age_bin, y = pd_per1k,
      group = wall_label, color = wall_label, linetype = wall_label)
) +
  geom_line(linewidth = 0.9) +
  geom_point(shape = 19, size = 2.8) +
  # Per-1,000 rate labels above/below each point
  geom_text(
    aes(label = sprintf("%.1f", pd_per1k), vjust = label_vjust),
    size      = 2.7,
    fontface  = "bold",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = pd_colors,
    name   = "Wall Type"
  ) +
  scale_linetype_manual(
    values = pd_lty,
    name   = "Wall Type"
  ) +
  scale_y_continuous(
    name   = "Predicted First-Leak Rate (per 1,000 Facility-Years)",
    expand = expansion(mult = c(0.12, 0.12))   # extra room for labels
  ) +
  labs(
    title    = "Predicted Leak Risk by Tank Age and Wall Type",
    subtitle = sprintf(
      "%d\u2013%d. Never-leaked risk set. GRF probability forest; state-level OOB.",
      min(cv_data$panel_year), max(cv_data$panel_year)
    ),
    x = "Tank Age Bin (3-year intervals)"
  ) +
  guides(
    color    = guide_legend(
      override.aes = list(shape = 19, linetype = c("solid", "dashed")),
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

make_cal_dt <- function(pred_col, file_suffix) {
  dt <- cv_data[!is.na(get(pred_col)) & !is.na(event_first_leak)]
  breaks <- unique(quantile(dt[[pred_col]],
                              probs = seq(0, 1, 0.1), na.rm = TRUE))
  if (length(breaks) < 3) {
    warning("Insufficient score variation for calibration (", pred_col, ")")
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
  fwrite(cal, file.path(OUTPUT_TABLES,
           sprintf("Table_CV_Calibration_%s.csv", file_suffix)))
  cal
}

make_cal_panel <- function(cal, panel_label) {
  if (is.null(cal)) return(NULL)
  xy_max <- max(cal$mean_obs, cal$mean_pred, na.rm = TRUE) * 1.08
  ggplot(cal, aes(x = mean_pred, y = mean_obs)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey55", linewidth = 0.5) +
    geom_line(color = COL_TX, linewidth = 0.6) +
    # Filled circles sized by n (one per decile, no overlap issue here)
    geom_point(aes(size = n_fac_yrs), color = COL_TX, shape = 19, alpha = 0.85) +
    geom_text(aes(label = decile), vjust = -0.9, size = 2.5, color = "grey30") +
    coord_fixed(xlim = c(0, xy_max), ylim = c(0, xy_max)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    scale_size_continuous(range = c(2, 7), guide = "none") +
    labs(x        = "Mean Predicted Probability",
         y        = "Mean Observed Release Rate",
         subtitle = panel_label) +
    theme_pub() +
    theme(plot.subtitle = element_text(face = "bold", size = 10))
}

cal_ns <- make_cal_dt("pred_nostate", "NoState")
cal_st <- make_cal_dt("pred_state",   "State")

p_cal_ns <- make_cal_panel(cal_ns, "No State Feature")
p_cal_st <- make_cal_panel(cal_st, "With State Feature")

if (!is.null(p_cal_ns) && !is.null(p_cal_st)) {
  fig_cal <- p_cal_ns + p_cal_st
  ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_Calibration_Combined.png"),
         fig_cal, width = 10, height = 5, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, "Figure_CV_Calibration_Combined.pdf"),
         fig_cal, width = 10, height = 5, device = cairo_pdf)
  cat("  Figure_CV_Calibration_Combined saved\n")
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

  # Lift figure — all points, no thinning
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
    # Hollow points on model curve only
    geom_point(data = lift_pts,
               aes(x = x, y = y),
               shape  = 1, size = 1.6, stroke = 0.55,
               color  = COL_TX, fill = NA) +
    geom_segment(aes(x = 0.10, xend = 0.10, y = 0, yend = top10),
                 linetype = "dotted", color = "grey50", linewidth = 0.4) +
    geom_segment(aes(x = 0, xend = 0.10, y = top10, yend = top10),
                 linetype = "dotted", color = "grey50", linewidth = 0.4) +
    annotate("text", x = 0.12, y = top10 * 0.88, hjust = 0,
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

  # Score separation density
  dt[, outcome_label := fifelse(event_first_leak == 1L,
                                 "Confirmed Release", "No Release")]
  x_cap <- quantile(dt[[pred_col]], 0.99, na.rm = TRUE)

  fig_sep <- ggplot(
    dt[get(pred_col) <= x_cap],
    aes(x = get(pred_col), fill = outcome_label, color = outcome_label)) +
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
app_ns <- make_appendix_figs("pred_nostate", "No State Feature", "NoState")
app_st <- make_appendix_figs("pred_state",   "With State Feature", "State")


#### S10: GoF Summary Table ###################################################

gof_summary <- data.table(
  Model            = c("No State Feature", "With State Feature"),
  AUC_OOB          = round(c(auc_ns,           auc_st),           3),
  Top10_pct_events = round(c(app_ns$top10,      app_st$top10) * 100, 1),
  Top20_pct_events = round(c(app_ns$top20,      app_st$top20) * 100, 1),
  N_fac_years      = n_total,
  N_events         = n_events,
  Base_rate_pct    = round(event_rate * 100, 3),
  Class_weight     = round(w_ratio, 1)
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
#     Use:      study pre-reform risk dynamics, year-by-year risk controls
#
# (B) analysis_cv_data_fac.rds
#     Grain:    panel_id (one row per facility)
#     Join key: panel_id
#     Use:      fixed pre-reform risk covariate in 02a/02b HTE analysis
#     Snapshot: SNAP_YEAR (1998). For facilities not observed in 1998,
#               falls back to the most recent available pre-treatment year.

# ---- (A) Facility-year ----
cv_scores_fy <- cv_data[, .(
  panel_id,
  panel_year,
  state,
  texas_treated,
  pred_nostate,
  pred_state
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
  risk_score_nostate = pred_nostate,
  risk_score_state   = pred_state,
  snap_year          = panel_year
)]

# Fallback for facilities not observed in SNAP_YEAR
missing_ids <- setdiff(unique(cv_data$panel_id), snap$panel_id)
if (length(missing_ids) > 0) {
  cat(sprintf("  %s facilities missing in %d — using last pre-reform obs\n",
      format(length(missing_ids), big.mark = ","), SNAP_YEAR))
  fallback <- cv_data[
    panel_id %in% missing_ids,
    .SD[which.max(panel_year)],
    by    = panel_id,
    .SDcols = c("state", "texas_treated", "pred_nostate", "pred_state", "panel_year")
  ]
  setnames(fallback,
           c("pred_nostate", "pred_state", "panel_year"),
           c("risk_score_nostate", "risk_score_state", "snap_year"))
  snap <- rbindlist(list(snap, fallback), use.names = TRUE, fill = TRUE)
}

# Risk quartile (pooled) for convenience in downstream HTE
q_cuts <- quantile(snap$risk_score_nostate,
                    probs = c(0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE)
snap[, risk_quartile := cut(
  risk_score_nostate,
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
cat("\nRisk score by group (no-state model, 1998 snapshot):\n")
print(snap[, .(
  Mean   = round(mean(risk_score_nostate,           na.rm = TRUE), 4),
  Median = round(median(risk_score_nostate,         na.rm = TRUE), 4),
  P75    = round(quantile(risk_score_nostate, 0.75, na.rm = TRUE), 4),
  N      = .N
), by = .(Group = fifelse(texas_treated == 1L, "Texas", "Control"))])

ks <- ks.test(
  snap[texas_treated == 1L, risk_score_nostate],
  snap[texas_treated == 0L, risk_score_nostate]
)
cat(sprintf("KS test TX vs Control: D = %.4f, p = %.4f\n",
    ks$statistic, ks$p.value))


#### Summary ##################################################################

cat("\n========================================================\n")
cat("01n COMPLETE\n\n")
cat(sprintf("  Sample:     %s facility-years | %s first-leak events (%.3f%% base rate)\n",
    format(n_total, big.mark = ","), format(n_events, big.mark = ","),
    event_rate * 100))
cat(sprintf("  Class wt:   %.0f:1  (0s:1s) — minority class upweighted\n", w_ratio))
cat(sprintf("  AUC:        No State = %.3f | With State = %.3f\n", auc_ns, auc_st))
cat("\n  Main figures:\n")
cat("    Figure_CV_ROC_Combined\n")
cat("    Figure_CV_PartialDep\n")
cat("    Figure_CV_Calibration_Combined\n")
cat("\n  Appendix figures:\n")
cat("    FigureA_CV_Lift_{NoState,State}\n")
cat("    FigureA_CV_ScoreSep_{NoState,State}\n")
cat("    FigureA_CV_VarImportance\n")
cat("\n  Risk score outputs (Data/Analysis/):\n")
cat("    analysis_cv_data_fac_year.rds   join on panel_id + panel_year\n")
cat("    analysis_cv_data_fac.rds        join on panel_id (1998 snapshot)\n")
cat("========================================================\n")