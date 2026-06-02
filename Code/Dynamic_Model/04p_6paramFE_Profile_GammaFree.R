# ==============================================================================
# Code/Dynamic_Model/04p_6paramFE_Profile_GammaFree.R
# TICKET 016 — Re-estimate the BOY 6p+FE fit with WIDE, SIGN-FREE gamma bounds.
#
# Cloned from 04o_6paramFE_Profile_Clean.R. The ONLY change vs the Ticket-013
# 04o run is the two gamma boxes, overridden at driver level to a wide
# sign-free range:
#     cfg$gamma_price_bounds <- c(-20, 20)
#     cfg$gamma_risk_bounds  <- c(-20, 20)
# Everything else (tol_theta, tol_P, npl_iter, sigma2, beta, ccp_damping,
# warm start, kappa/K bounds, F_replace, Bellman/CCP) is UNCHANGED.
#
# Inputs:  DCM_Primitives_Replacement_observed.rds   (BOY primitives, T013)
#          dcm_obs_panel_observed.csv                 (BOY panel, T013)
#          Model_Replacement_6paramFE_profile_observed.rds        (T005 warm-start)
#          Model_Replacement_6paramFE_profile_clean_observed.rds  (BOY_013 fit)
#
# Outputs: Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds
#          Output/Tables/T016_Gamma_BoundSensitivity.csv
# ==============================================================================

# ==============================================================================
# SECTION 1 — LOGGING
# ==============================================================================
.log_path <- here::here("logs", paste0(
  "04p_6paramFE_Profile_GammaFree_",
  format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 04p_6paramFE_Profile_GammaFree.R\nR: %s\nWD: %s\n\n",
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
# SECTION 4 — LOAD BOY INPUTS (Ticket-013 outputs)
# ==============================================================================
cat("=== SECTION 4: LOAD BOY INPUTS ===\n")

path_prims_clean <- here::here("Output", "Estimation_Results",
                                "DCM_Primitives_Replacement_observed.rds")
path_obs_clean   <- here::here("Data", "Analysis", "dcm_obs_panel_observed.csv")
path_warm_t005   <- here::here("Output", "Estimation_Results",
                                "Model_Replacement_6paramFE_profile_observed.rds")
path_boy_013     <- here::here("Output", "Estimation_Results",
                                "Model_Replacement_6paramFE_profile_clean_observed.rds")

stopifnot(file.exists(path_prims_clean),
          file.exists(path_obs_clean),
          file.exists(path_warm_t005),
          file.exists(path_boy_013))

primitives <- readRDS(path_prims_clean)
obs_panel  <- fread(path_obs_clean)
warm_fit   <- readRDS(path_warm_t005)
boy_013    <- readRDS(path_boy_013)

cat(sprintf("  BOY obs_panel: %s rows | %s facilities\n",
    format(nrow(obs_panel), big.mark = ","),
    format(data.table::uniqueN(obs_panel$panel_id), big.mark = ",")))
cat(sprintf("  T005 warm-start LL: %.3f | converged: %s\n",
    warm_fit$log_likelihood, warm_fit$converged))
cat(sprintf("  BOY_013 boxed fit LL: %.3f | converged: %s\n",
    boy_013$log_likelihood, boy_013$converged))

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
# SECTION 5 — CONFIG  (identical to 04o, then gamma boxes widened)
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

# --- TICKET 016: widen BOTH gamma boxes to a wide sign-free range ---
# Driver-level override ONLY; the shared config fn is NOT edited.
cat(sprintf("  Config-default gamma boxes (before override): price [%g, %g]  risk [%g, %g]\n",
    cfg$gamma_price_bounds[1], cfg$gamma_price_bounds[2],
    cfg$gamma_risk_bounds[1],  cfg$gamma_risk_bounds[2]))
cfg$gamma_price_bounds <- c(-20, 20)
cfg$gamma_risk_bounds  <- c(-20, 20)
cat(sprintf("  Ticket-016 gamma boxes (after override):     price [%g, %g]  risk [%g, %g]\n",
    cfg$gamma_price_bounds[1], cfg$gamma_price_bounds[2],
    cfg$gamma_risk_bounds[1],  cfg$gamma_risk_bounds[2]))
cat("  NOTE: npl_estimator_replacement_6p_fe_profile reads cfg$gamma_price_bounds\n")
cat("        and cfg$gamma_risk_bounds as the optim lower/upper for the two gammas\n")
cat("        (improved_estimator_OPTIMIZED.r:4787-4792).\n")

cat(sprintf("  sigma2=%.1f  beta=%.2f  max_npl_iter=%d  tol_theta=%.0e  tol_P=%.0e\n",
    cfg$sigma2, cfg$beta, cfg$max_npl_iter, cfg$tol_theta, cfg$tol_P))

# ==============================================================================
# SECTION 6 — WARM START (identical to 04o)
# ==============================================================================
cat("=== SECTION 6: WARM START FROM T005 FE-PROFILE FIT ===\n")

pn <- cfg$struct_param_names
stopifnot(all(pn %in% names(warm_fit$theta_hat)))
thetastruct_init <- warm_fit$theta_hat[pn]

cat("  Warm-start theta (from T005 6p+FE profile):\n")
print(round(thetastruct_init, 4))

# ==============================================================================
# SECTION 7 — ESTIMATE
# ==============================================================================
cat("=== SECTION 7: ESTIMATE ===\n")

fit_gf <- npl_estimator_replacement_6p_fe_profile(
  obs_panel        = obs_panel,
  primitives       = primitives,
  config           = cfg,
  thetastruct_init = thetastruct_init,
  verbose          = TRUE,
  sample_label     = "observed_gammafree"
)

cat(sprintf("\nEstimator: npl_estimator_replacement_6p_fe_profile | Sample: observed_gammafree\n"))
cat(sprintf("Converged: %s at iter %d | LL: %.3f | Elapsed: %.1fs\n",
    fit_gf$converged, fit_gf$n_iter,
    fit_gf$log_likelihood, fit_gf$elapsed_sec))
cat("theta_hat:\n"); print(round(fit_gf$theta_hat, 4))
cat("alpha_hat:\n"); print(round(fit_gf$alpha_hat, 3))

# Acceptance criteria (NO sign asserts, NO pinning gate — per Ticket 016)
stopifnot(fit_gf$converged == TRUE)
stopifnot(length(fit_gf$theta_hat) == 6L)
stopifnot(all(names(fit_gf$theta_hat) == cfg$struct_param_names))
stopifnot(all(dim(fit_gf$P_hat) == c(32L, 3L)))
stopifnot(all(abs(rowSums(fit_gf$P_hat) - 1) < 1e-8))

kSW <- exp(fit_gf$theta_hat[["K_log_SW"]])
kDW <- exp(fit_gf$theta_hat[["K_log_DW"]])
cat(sprintf("  K_SW = %.1f ($%s/facility)  K_DW = %.1f ($%s/facility)\n",
    kSW, format(round(kSW * SCALE_FACTOR), big.mark = ","),
    kDW, format(round(kDW * SCALE_FACTOR), big.mark = ",")))

# Neutral reporting of the gammas — no sign/range assumptions
cat(sprintf("  gamma_price = %.4f  (box [%g, %g])\n",
    fit_gf$theta_hat[["gamma_price"]],
    cfg$gamma_price_bounds[1], cfg$gamma_price_bounds[2]))
cat(sprintf("  gamma_risk  = %.4f  (box [%g, %g])\n",
    fit_gf$theta_hat[["gamma_risk"]],
    cfg$gamma_risk_bounds[1], cfg$gamma_risk_bounds[2]))
cat("  Acceptance criteria (converged + P_hat rowSums==1): PASS\n")

# ==============================================================================
# SECTION 8 — SAVE RDS (separate fit; 013 fit untouched)
# ==============================================================================
cat("=== SECTION 8: SAVE RDS ===\n")

fit_gf$sample_label <- "observed_gammafree"
path_out_rds <- here::here("Output", "Estimation_Results",
                           "Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds")
saveRDS(fit_gf, path_out_rds)
stopifnot(file.exists(path_out_rds))
cat(sprintf("  Saved: %s\n", path_out_rds))

# ==============================================================================
# SECTION 9 — DELIVERABLE: T016 bound-sensitivity comparison
# ==============================================================================
cat("=== SECTION 9: T016 GAMMA BOUND-SENSITIVITY TABLE ===\n")

hat_013 <- boy_013$theta_hat[pn]
hat_gf  <- fit_gf$theta_hat[pn]

# Convert each parameter to display scale:
#   kappa_* -> USD (*SCALE_FACTOR), K_log_* -> K=exp(.), gammas raw.
make_row <- function(pname, v013, vgf) {
  if (grepl("^K_log_", pname)) {
    disp <- sub("K_log_", "K_", pname)
    a <- exp(v013); b <- exp(vgf)
  } else if (grepl("^kappa_", pname)) {
    disp <- pname
    a <- v013 * SCALE_FACTOR; b <- vgf * SCALE_FACTOR
  } else {
    disp <- pname
    a <- v013; b <- vgf
  }
  data.table(parameter     = disp,
             BOY_013       = round(a, 4),
             BOY_gammafree = round(b, 4),
             delta         = round(b - a, 4))
}

rows <- lapply(pn, function(p) make_row(p, hat_013[[p]], hat_gf[[p]]))

# log_likelihood row
rows[[length(rows) + 1L]] <- data.table(
  parameter     = "log_likelihood",
  BOY_013       = round(boy_013$log_likelihood, 3),
  BOY_gammafree = round(fit_gf$log_likelihood, 3),
  delta         = round(fit_gf$log_likelihood - boy_013$log_likelihood, 3))

t016_dt <- rbindlist(rows)

cat("  T016 BOY_013 vs BOY_gammafree:\n")
print(t016_dt)

# One-line gamma movement summary (delta and % change)
for (g in c("gamma_price", "gamma_risk")) {
  r <- t016_dt[parameter == g]
  pct <- if (abs(r$BOY_013) > 1e-10) 100 * r$delta / abs(r$BOY_013) else NA_real_
  cat(sprintf("  %s: 013 = %.4f -> gammafree = %.4f  (delta = %.4f, %s)\n",
      g, r$BOY_013, r$BOY_gammafree, r$delta,
      if (is.na(pct)) "pct n/a" else sprintf("%.1f%%", pct)))
}

cat("  NOTE: the 013 fit used config-default boxes gamma_price [-20, 5],",
    "gamma_risk [-5, 10];\n        this run widened both to [-20, 20] (sign-free).\n")

path_t016_csv <- here::here("Output", "Tables", "T016_Gamma_BoundSensitivity.csv")
fwrite(t016_dt, path_t016_csv)
cat(sprintf("  Saved CSV: %s\n", path_t016_csv))

# ==============================================================================
# SECTION 10 — CANONICAL PROMOTION DECISION
# ==============================================================================
cat("=== SECTION 10: CANONICAL PROMOTION ===\n")
if (isTRUE(fit_gf$converged) && all(abs(rowSums(fit_gf$P_hat) - 1) < 1e-8)) {
  cat("  Step 2 acceptance met (converged + P_hat rowSums==1).\n")
  cat("  DECISION: the gammafree fit is the new canonical BOY fit.\n")
  cat(sprintf("    Canonical: %s\n", path_out_rds))
  cat("    Boxed 013 fit retained (NOT deleted):\n")
  cat(sprintf("    %s\n", path_boy_013))
} else {
  cat("  Step 2 acceptance NOT met — gammafree fit NOT promoted.\n")
}

cat("\n=== 04p_6paramFE_Profile_GammaFree COMPLETE ===\n")
cat(sprintf("  Fit:   %s\n", path_out_rds))
cat(sprintf("  Table: %s\n", path_t016_csv))
