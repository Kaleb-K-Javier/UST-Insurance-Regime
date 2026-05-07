# ==============================================================================
# 04c_Replacement_Estimation.R
# ==============================================================================
# PURPOSE
#   Phase 3 of the replacement-model pipeline. Loads the 32-state primitives +
#   observation panel produced by 04b for both the OBSERVED (TX 2006+) and
#   EXTENDED (TX 1999+ with engine-imputed pre-2006 premiums) samples, then
#   runs the 4-parameter NPL estimator on each sample and writes results.
#
# PARAMETERS:
#   theta = (kappa_exit, K_log, gamma_price, gamma_risk)
#   sigma calibrated at 1.0; beta = 0.95 (annual panel).
#
# OUTPUTS
#   Output/Estimation_Results/Model_Replacement_Estimates_observed.rds
#   Output/Estimation_Results/Model_Replacement_Estimates_extended.rds
#       Each: list(theta_hat, theta_raw, P_hat, V_hat, converged, n_iter,
#                  log_likelihood, ll_path, theta_path, sample_label).
#
# SANITY CHECKS
#   1. Side-by-side theta_hat across observed vs extended. Big shifts in
#      kappa_exit or gamma_price flag composition issues from extending TX
#      backward with engine-imputed premiums.
#   2. Absorbing-only sub-sample (Sample A1: I_replace = 0 only) — drops
#      every Replace observation so K_log becomes weakly identified;
#      kappa_exit / gamma_price / gamma_risk should pin down to roughly the
#      same place as the 3-parameter Model B on a comparable cell space.
# ==============================================================================

# Allow the worktree to find packages in the parent project's renv.
.PARENT_RENV_LIB <- "C:/Users/kaleb/Documents/ust_ins_move_to_github/renv/library/windows/R-4.5/x86_64-w64-mingw32"
if (dir.exists(.PARENT_RENV_LIB) && !(.PARENT_RENV_LIB %in% .libPaths())) {
  .libPaths(c(.PARENT_RENV_LIB, .libPaths()))
}

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(here)
})

source(here::here("Code", "Helpers", "data_paths.R"))
source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))

setDTthreads(0L)

OUT_DIR <- here::here("Output", "Estimation_Results")
PNL_DIR <- here::here("Data",  "Analysis")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Phase-1 outputs (primitives + obs panels) live in the PARENT project, not
# always in the worktree or on Z. Look there first when running from a
# worktree.
PARENT_PROJECT_ROOT <- "C:/Users/kaleb/Documents/ust_ins_move_to_github"
parent_in <- function(...) {
  parent_p <- file.path(PARENT_PROJECT_ROOT, ...)
  local_p  <- here::here(...)
  if (file.exists(local_p))  return(local_p)
  if (file.exists(parent_p)) return(parent_p)
  z_path(...)
}

cat("=================================================================\n")
cat("04c: Replacement-model NPL estimation (observed + extended)\n")
cat("=================================================================\n\n")


# ==============================================================================
# 1. Configuration (shared across both samples)
# ==============================================================================
config <- create_estimation_config_replacement(
  beta     = 0.95,   # annual discount factor
  sigma2   = 1.0,    # calibrated EV scale
  npl_iter = 200
)

theta_init <- c(kappa_exit = 20, K_log = log(20),
                gamma_price = -1.0, gamma_risk = 1.0)


# ==============================================================================
# 2. Helpers
# ==============================================================================
load_sample <- function(label, prims_filename, panel_filename) {
  prims_path <- parent_in("Output", "Estimation_Results", prims_filename)
  panel_path <- parent_in("Data",   "Analysis",            panel_filename)

  if (!file.exists(prims_path))
    stop("primitives missing: ", prims_path,
         "\n  Run 04b_Replacement_Panel_Prep.R first.")
  if (!file.exists(panel_path))
    stop("obs panel missing: ", panel_path,
         "\n  Run 04b_Replacement_Panel_Prep.R first.")

  prims <- readRDS(prims_path)
  obs   <- fread(panel_path)
  cat(sprintf("[%s] primitives <- %s\n             obs panel  <- %s\n",
              label, prims_path, panel_path))
  cat(sprintf("             rows = %s | facilities = %s | replace share = %.1f%%\n",
              format(nrow(obs), big.mark = ","),
              format(uniqueN(obs$panel_id), big.mark = ","),
              100 * prims$pct_replace))
  list(prims = prims, obs = obs)
}

run_estimation <- function(prims, obs, config, theta_init, label) {
  cat(sprintf("\n--- Estimating: %s ---\n", label))
  t_start <- Sys.time()
  fit <- npl_estimator_replacement(
    obs_panel  = obs,
    primitives = prims,
    config     = config,
    theta_init = theta_init,
    verbose    = TRUE
  )
  t_end <- Sys.time()
  fit$sample_label <- label
  fit$elapsed_sec  <- as.numeric(difftime(t_end, t_start, units = "secs"))
  cat(sprintf("    elapsed: %.1f sec\n", fit$elapsed_sec))
  fit
}

format_theta_row <- function(fit, label) {
  th <- fit$theta_hat
  data.table(
    sample      = label,
    converged   = fit$converged,
    n_iter      = fit$n_iter,
    log_lik     = round(fit$log_likelihood, 2),
    kappa_exit  = round(th[["kappa_exit"]],  4),
    K           = round(th[["K"]],           4),
    gamma_price = round(th[["gamma_price"]], 4),
    gamma_risk  = round(th[["gamma_risk"]],  4)
  )
}


# ==============================================================================
# 3. Load both samples
# ==============================================================================
cat("[1/3] Loading samples...\n")

obs_data <- load_sample(
  "observed",
  "DCM_Primitives_Replacement_observed.rds",
  "dcm_obs_panel_observed.csv"
)

ext_data <- load_sample(
  "extended",
  "DCM_Primitives_Replacement_extended.rds",
  "dcm_obs_panel_extended.csv"
)


# ==============================================================================
# 4. Run estimation on each main sample
# ==============================================================================
cat("\n[2/3] Running NPL estimator on each sample...\n")

fit_observed <- run_estimation(obs_data$prims, obs_data$obs,
                               config, theta_init,
                               "observed (TX 2006+, controls 1999+)")
fit_extended <- run_estimation(ext_data$prims, ext_data$obs,
                               config, theta_init,
                               "extended (TX 1999+ engine-imputed, controls 1999+)")


# ==============================================================================
# 5. Sanity 1 — side-by-side comparison
# ==============================================================================
cat("\n[3/3] Comparison and sanity checks\n")
cat("\n--- Theta comparison: observed vs extended ---\n")
cmp <- rbindlist(list(
  format_theta_row(fit_observed, "observed"),
  format_theta_row(fit_extended, "extended")
))
print(cmp)

big_shift <- function(a, b, tol_rel = 0.5) {
  if (any(!is.finite(c(a, b)))) return(TRUE)
  denom <- max(abs(a), abs(b), 1e-6)
  abs(a - b) / denom > tol_rel
}
shifts <- list(
  kappa_exit  = big_shift(fit_observed$theta_hat[["kappa_exit"]],
                          fit_extended$theta_hat[["kappa_exit"]]),
  gamma_price = big_shift(fit_observed$theta_hat[["gamma_price"]],
                          fit_extended$theta_hat[["gamma_price"]])
)
if (any(unlist(shifts))) {
  cat("\n  WARNING: large shift detected between samples — possible composition\n")
  cat("           issue from engine-imputed pre-2006 TX premiums.\n")
  print(shifts)
} else {
  cat("\n  OK: kappa_exit and gamma_price are stable across samples\n")
}


# ==============================================================================
# 6. Sanity 2 — absorbing-only robustness (Replace obs dropped)
# ==============================================================================
# Dropping every replacement observation makes K_log un-identified (no
# information enters the obs likelihood about reset-state continuation).
# kappa_exit, gamma_price, gamma_risk should still be recoverable; if they
# move dramatically from the full-sample fit, that's a red flag for
# misspecification rather than a sample-composition story.
cat("\n--- Sanity A1: absorbing-only sub-sample (drop Replace obs) ---\n")

obs_abs <- obs_data$obs[!(y_it == 1L & I_replace == 1L)]
cat(sprintf("    dropped %s of %s rows (%.1f%%)\n",
            format(nrow(obs_data$obs) - nrow(obs_abs), big.mark = ","),
            format(nrow(obs_data$obs),                 big.mark = ","),
            100 * (1 - nrow(obs_abs) / nrow(obs_data$obs))))

# Use a tiny K_log lower bound and tight upper bound to keep the optimizer
# from wandering on this un-identified parameter (effectively pins K).
config_abs <- config
config_abs$K_log_bounds <- c(log(20) - 0.01, log(20) + 0.01)
fit_absorbing <- run_estimation(obs_data$prims, obs_abs,
                                config_abs, theta_init,
                                "absorbing-only (Replace obs dropped)")
print(format_theta_row(fit_absorbing, "absorbing-only"))


# ==============================================================================
# 7. Save results
# ==============================================================================
saveRDS(fit_observed,
        file.path(OUT_DIR, "Model_Replacement_Estimates_observed.rds"))
saveRDS(fit_extended,
        file.path(OUT_DIR, "Model_Replacement_Estimates_extended.rds"))
saveRDS(fit_absorbing,
        file.path(OUT_DIR, "Model_Replacement_Estimates_absorbing.rds"))

cat("\nSaved:\n")
cat("  ", file.path(OUT_DIR, "Model_Replacement_Estimates_observed.rds"),  "\n")
cat("  ", file.path(OUT_DIR, "Model_Replacement_Estimates_extended.rds"),  "\n")
cat("  ", file.path(OUT_DIR, "Model_Replacement_Estimates_absorbing.rds"), "\n")

cat("\n04c complete.\n")
