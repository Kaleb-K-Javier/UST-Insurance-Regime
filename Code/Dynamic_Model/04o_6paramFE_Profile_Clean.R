# ==============================================================================
# Code/Dynamic_Model/04o_6paramFE_Profile_Clean.R
# TICKET 007 Phase 3 — One 6p+stayFE re-estimation on the cleaned panel
#
# Inputs:  DCM_Primitives_Replacement_observed.rds (Phase 2-regenerated)
#          dcm_obs_panel_observed.csv               (Phase 2-regenerated)
#          Model_Replacement_6paramFE_profile_observed.rds (T005 warm-start)
#
# Outputs: Model_Replacement_6paramFE_profile_clean_observed.rds
#          Output/Tables/04o_Theta_Comparison_T005_vs_T007.csv + .tex
# ==============================================================================

# ==============================================================================
# SECTION 1 — LOGGING
# ==============================================================================
.log_path <- here::here("logs", paste0(
  "04o_6paramFE_Profile_Clean_",
  format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 04o_6paramFE_Profile_Clean.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# ==============================================================================
# SECTION 2 — PACKAGES
# ==============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(Rcpp)
  library(here)
  library(numDeriv)
})

SCALE_FACTOR <- 10000L
OUT_FIT <- here::here("Output", "Estimation_Results")
OUT_TAB <- here::here("Output", "Tables")
dir.create(OUT_FIT, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# SECTION 3 — SOURCE + SOURCECPP
# ==============================================================================
cat("=== SECTION 3: SOURCE + SOURCECPP ===\n")

source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))
tryCatch(
  Rcpp::sourceCpp(here::here("Code", "Helpers", "cpp_engine.cpp")),
  error = function(e) warning(sprintf(
    "cpp_engine.cpp could not compile — running in R fallback mode (~10x slower): %s",
    conditionMessage(e)))
)

stopifnot(exists("npl_estimator_replacement_6p_fe_profile",      mode = "function"))
stopifnot(exists("npl_likelihood_replacement_6p_fe_profile",     mode = "function"))
stopifnot(exists("flow_utilities_replacement_6p",                mode = "function"))
stopifnot(exists("solve_equilibrium_policy_replacement_6p",      mode = "function"))
cat("  All required 6p functions present\n")
if (exists("nll_replacement8pfe_profile_counts_cpp", mode = "function")) {
  cat("  C++ kernels loaded: Rcpp acceleration active\n")
} else {
  cat("  C++ kernels NOT loaded: using R fallback (slower but correct)\n")
}

# ==============================================================================
# SECTION 4 — LOAD CLEANED INPUTS (Phase 2 outputs)
# ==============================================================================
cat("=== SECTION 4: LOAD CLEANED INPUTS ===\n")

path_prims_clean <- here::here("Output", "Estimation_Results",
                                "DCM_Primitives_Replacement_observed.rds")
path_obs_clean   <- here::here("Data", "Analysis", "dcm_obs_panel_observed.csv")
path_warm_t005   <- here::here("Output", "Estimation_Results",
                                "Model_Replacement_6paramFE_profile_observed.rds")

stopifnot(file.exists(path_prims_clean),
          file.exists(path_obs_clean),
          file.exists(path_warm_t005))

primitives <- readRDS(path_prims_clean)
obs_panel  <- fread(path_obs_clean)
warm_fit   <- readRDS(path_warm_t005)

cat(sprintf("  Cleaned obs_panel: %s rows | %s facilities\n",
    format(nrow(obs_panel), big.mark = ","),
    format(data.table::uniqueN(obs_panel$panel_id), big.mark = ",")))
cat(sprintf("  T005 warm-start LL: %.3f | converged: %s\n",
    warm_fit$log_likelihood, warm_fit$converged))

# Validate panel row count (acceptance criterion)
stopifnot(nrow(obs_panel) >= 2e6, nrow(obs_panel) <= 2.5e6)
cat(sprintf("  Row count %.0f in [2M, 2.5M]: OK\n", nrow(obs_panel)))

# Validate action shares
act_shares <- obs_panel[, .(
  pct_maintain = mean(y_it == 0L, na.rm = TRUE),
  pct_exit     = mean(y_it == 1L & (is.na(I_replace) | I_replace == 0L), na.rm = TRUE),
  pct_replace  = mean(y_it == 1L & !is.na(I_replace) & I_replace == 1L, na.rm = TRUE)
)]
cat(sprintf("  Action shares — Maintain: %.2f%%  Exit: %.2f%%  Replace: %.2f%%\n",
    100 * act_shares$pct_maintain,
    100 * act_shares$pct_exit,
    100 * act_shares$pct_replace))
stopifnot(act_shares$pct_maintain >= 0.98)
stopifnot(act_shares$pct_exit    >= 0.0005, act_shares$pct_exit    <= 0.03)
stopifnot(act_shares$pct_replace >= 0.0005, act_shares$pct_replace <= 0.03)
cat("  Action share acceptance criteria: PASS\n")

# ==============================================================================
# SECTION 5 — CONFIG
# ==============================================================================
cat("=== SECTION 5: BUILD CONFIG ===\n")

cfg <- create_estimation_config_replacement_6p_fe_profile(
  beta                  = 0.95,
  sigma2                = 1.0,
  max_npl_iter          = 200L,
  feweightsource        = "controls",
  ccp_damping_lambda    = 0.6,
  alpha_bound           = 20,
  alpha_newton_tol      = 1e-10,
  alpha_newton_max_iter = 30L
)
cat(sprintf("  sigma2=%.1f  beta=%.2f  max_npl_iter=%d  tol_theta=%.0e  tol_P=%.0e\n",
    cfg$sigma2, cfg$beta, cfg$max_npl_iter, cfg$tol_theta, cfg$tol_P))

# ==============================================================================
# SECTION 6 — WARM START
# ==============================================================================
cat("=== SECTION 6: WARM START FROM T005 FE-PROFILE FIT ===\n")

# Use T005's contaminated-panel FE-profile theta_hat as warm-start (Q7 answer).
# Same structural form; avoids absorbing cross-state heterogeneity as T005 no-FE fit does.
pn <- cfg$struct_param_names
stopifnot(all(pn %in% names(warm_fit$theta_hat)))
thetastruct_init <- warm_fit$theta_hat[pn]

cat("  Warm-start theta (from T005 6p+FE profile, contaminated panel):\n")
print(round(thetastruct_init, 4))

# ==============================================================================
# SECTION 7 — ESTIMATE
# ==============================================================================
cat("=== SECTION 7: ESTIMATE ===\n")

fit_clean <- npl_estimator_replacement_6p_fe_profile(
  obs_panel        = obs_panel,
  primitives       = primitives,
  config           = cfg,
  thetastruct_init = thetastruct_init,
  verbose          = TRUE,
  sample_label     = "observed_clean"
)

cat(sprintf("\nEstimator: npl_estimator_replacement_6p_fe_profile | Sample: observed_clean\n"))
cat(sprintf("Converged: %s at iter %d | LL: %.3f | Elapsed: %.1fs\n",
    fit_clean$converged, fit_clean$n_iter,
    fit_clean$log_likelihood, fit_clean$elapsed_sec))
cat("theta_hat:\n"); print(round(fit_clean$theta_hat, 4))
cat("alpha_hat:\n"); print(round(fit_clean$alpha_hat, 3))

# Acceptance criteria
stopifnot(fit_clean$converged == TRUE)
stopifnot(length(fit_clean$theta_hat) == 6L)
stopifnot(all(names(fit_clean$theta_hat) == cfg$struct_param_names))
stopifnot(all(dim(fit_clean$P_hat) == c(32L, 3L)))
stopifnot(all(abs(rowSums(fit_clean$P_hat) - 1) < 1e-8))

kSW <- exp(fit_clean$theta_hat[["K_log_SW"]])
kDW <- exp(fit_clean$theta_hat[["K_log_DW"]])
cat(sprintf("  K_SW = %.1f ($%s/facility)  K_DW = %.1f ($%s/facility)\n",
    kSW, format(round(kSW * SCALE_FACTOR), big.mark = ","),
    kDW, format(round(kDW * SCALE_FACTOR), big.mark = ",")))
cat(sprintf("  gamma_price = %.4f  gamma_risk = %.4f\n",
    fit_clean$theta_hat[["gamma_price"]], fit_clean$theta_hat[["gamma_risk"]]))

if (fit_clean$theta_hat[["gamma_price"]] > -1 || fit_clean$theta_hat[["gamma_price"]] < -3)
  warning("gamma_price outside expected range [-3, -1]")
if (fit_clean$theta_hat[["gamma_risk"]] < 0 || fit_clean$theta_hat[["gamma_risk"]] > 0.15)
  warning("gamma_risk outside expected range [0, 0.15]")

cat("  Acceptance criteria: PASS\n")

# ==============================================================================
# SECTION 8 — SAVE RDS
# ==============================================================================
cat("=== SECTION 8: SAVE RDS ===\n")

fit_clean$sample_label <- "observed_clean"
path_out_rds <- here::here("Output", "Estimation_Results",
                            "Model_Replacement_6paramFE_profile_clean_observed.rds")
saveRDS(fit_clean, path_out_rds)
stopifnot(file.exists(path_out_rds))
cat(sprintf("  Saved: %s\n", path_out_rds))

# ==============================================================================
# SECTION 9 — COMPARISON TABLE: T005 vs T007
# ==============================================================================
cat("=== SECTION 9: THETA COMPARISON T005 vs T007 ===\n")

hat_t007 <- fit_clean$theta_hat
hat_t005 <- warm_fit$theta_hat[pn]

# Build comparison data.table
z95 <- qnorm(0.975)
se_t007 <- fit_clean$se_theta[pn]
se_t005 <- if (!is.null(warm_fit$se_theta)) warm_fit$se_theta[pn] else rep(NA_real_, 6L)

# Convert K_log to K levels for display
make_cmp_row <- function(pname, v_t005, v_t007) {
  if (grepl("^K_log_", pname)) {
    param_display <- sub("K_log_", "K_", pname)
    v5 <- exp(v_t005)
    v7 <- exp(v_t007)
  } else if (grepl("^kappa_", pname)) {
    param_display <- pname
    v5 <- v_t005 * SCALE_FACTOR
    v7 <- v_t007 * SCALE_FACTOR
  } else {
    param_display <- pname
    v5 <- v_t005
    v7 <- v_t007
  }
  dlt <- v7 - v5
  pct <- if (abs(v5) > 1e-10) 100 * dlt / abs(v5) else NA_real_
  data.table(parameter        = param_display,
             T005_contaminated = round(v5, 4),
             T007_clean        = round(v7, 4),
             delta             = round(dlt, 4),
             pct_change        = round(pct, 2))
}

cmp_rows <- lapply(pn, function(p) make_cmp_row(p, hat_t005[p], hat_t007[p]))

# Append logL and N_obs rows
cmp_rows[[length(cmp_rows) + 1L]] <- data.table(
  parameter         = "log_likelihood",
  T005_contaminated = round(warm_fit$log_likelihood, 3),
  T007_clean        = round(fit_clean$log_likelihood, 3),
  delta             = round(fit_clean$log_likelihood - warm_fit$log_likelihood, 3),
  pct_change        = NA_real_)
t005_obs_path <- here::here("Data", "Analysis", "_pre_T007", "dcm_obs_panel_observed.csv")
n_t005 <- if (file.exists(t005_obs_path)) {
  nrow(fread(t005_obs_path, select = "panel_id"))
} else {
  cat("  NOTE: T005 backup obs panel not found; using warm_fit N_obs if available\n")
  if (!is.null(warm_fit$n_obs)) warm_fit$n_obs else NA_integer_
}
cmp_rows[[length(cmp_rows) + 1L]] <- data.table(
  parameter         = "N_obs",
  T005_contaminated = n_t005,
  T007_clean        = nrow(obs_panel),
  delta             = if (!is.na(n_t005)) nrow(obs_panel) - n_t005 else NA_real_,
  pct_change        = NA_real_)

cmp_dt <- rbindlist(cmp_rows)

cat("  T005 vs T007 comparison:\n")
print(cmp_dt)

path_cmp_csv <- here::here("Output", "Tables", "04o_Theta_Comparison_T005_vs_T007.csv")
fwrite(cmp_dt, path_cmp_csv)
cat(sprintf("  Saved CSV: %s\n", path_cmp_csv))

# TEX table
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{6p+FE Structural Parameters: T005 (contaminated panel) vs T007 (cleaned panel)}",
  "\\label{tab:theta_t005_t007}",
  "\\begin{tabular}{lrrrr}",
  "\\hline",
  "Parameter & T005 & T007 & $\\Delta$ & \\% change \\\\",
  "\\hline"
)
for (i in seq_len(nrow(cmp_dt))) {
  r <- cmp_dt[i]
  tex_lines <- c(tex_lines,
    sprintf("%s & %.4g & %.4g & %.4g & %s \\\\",
      gsub("_", "\\_", r$parameter),
      r$T005_contaminated, r$T007_clean, r$delta,
      if (is.na(r$pct_change)) "---" else sprintf("%.1f\\%%", r$pct_change)))
}
tex_lines <- c(tex_lines, "\\hline", "\\end{tabular}", "\\end{table}")
path_cmp_tex <- here::here("Output", "Tables", "04o_Theta_Comparison_T005_vs_T007.tex")
writeLines(tex_lines, path_cmp_tex)
cat(sprintf("  Saved TEX: %s\n", path_cmp_tex))

cat("\n=== 04o_6paramFE_Profile_Clean COMPLETE ===\n")
cat(sprintf("  Fit:  %s\n", path_out_rds))
cat(sprintf("  Table:%s\n", path_cmp_csv))
