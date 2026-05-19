# ==============================================================================
# 04i_smoke_test.R
# Stage A.5 of Ticket 001. No optim loop — pure plumbing verification:
#   cpp_engine compiled, FE config built, counts+weights built, cache merged,
#   equilibrium solved at structural sub-vector, ONE likelihood eval,
#   ONE geo-weighted CCP update. Exits 0 on success, non-zero on any failure.
# ==============================================================================

.PARENT_RENV_LIB <- "C:/Users/kaleb/Documents/ust_ins_move_to_github/renv/library/windows/R-4.5/x86_64-w64-mingw32"
if (dir.exists(.PARENT_RENV_LIB) && !(.PARENT_RENV_LIB %in% .libPaths())) {
  .libPaths(c(.PARENT_RENV_LIB, .libPaths()))
}

suppressPackageStartupMessages({
  library(data.table); library(Matrix); library(here)
})

.SCRIPT_BASENAME <- "04i_smoke_test"
.log_path <- here::here(
  "logs",
  paste0(.SCRIPT_BASENAME, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
    .log_path, .SCRIPT_BASENAME, R.version.string, getwd()))


# ---- Step 1. Source library; verify cpp_engine compiled. -------------------
source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))
if (!exists("nll_replacement8pfe_counts_cpp", mode = "function") ||
    !exists("update_ccps_geoweighted_cpp",    mode = "function")) {
  stop("cpp_engine.cpp did not compile; STOP")
}
cat("[smoke 1/9] cpp_engine functions available\n")


# ---- Step 2. Load primitives + obs panel. ----------------------------------
prims_obs <- readRDS(here::here("Output", "Estimation_Results",
                                "DCM_Primitives_Replacement_observed.rds"))
obs_obs   <- fread(here::here("Data", "Analysis",
                              "dcm_obs_panel_observed.csv"))
cat(sprintf("[smoke 2/9] loaded primitives + obs panel (n_obs=%d)\n",
            nrow(obs_obs)))


# ---- Step 3. Build FE config; assert structure. ----------------------------
cfg <- create_estimation_config_replacement_8p_fe(
  beta              = 0.95,
  sigma2            = 1.0,
  npl_iter          = 200,
  feweightsource    = "controls",
  ccp_damping_lambda = 0.6,
  epsprob           = 1e-12,
  alphabounds       = c(-20, 20),
  tol_theta         = 1e-5,
  tol_P             = 1e-5)
stopifnot(cfg$n_params == 25L)
stopifnot(length(cfg$param_names) == 25L)
stopifnot(all(cfg$fe_param_names == paste0("alpha",
  c("AR","CO","ID","KS","KY","LA","MA","MD","ME","MN",
    "MO","NC","OH","OK","SD","TN","VA"))))
cat("[smoke 3/9] cfg built: n_params=25, FE names match\n")


# ---- Step 4. Build cache + counts (mirrors npl_estimator_replacement_8p_fe).
cache <- prims_obs
cw    <- .build_counts_weights_8p_fe(obs_obs, prims_obs, cfg$feweightsource)
cache$countsdt8pfe <- cw$countsdt
cache$wsg8pfe      <- cw$wsg

config_4p <- create_estimation_config_replacement(
  beta = cfg$beta, sigma2 = cfg$sigma2, npl_iter = cfg$max_npl_iter)
std_cache <- create_estimation_cache_replacement_8p(prims_obs, obs_obs,
                                                    config_4p, cfg)
for (nm in names(std_cache)) {
  if (is.null(cache[[nm]])) cache[[nm]] <- std_cache[[nm]]
}
if (is.null(cache$F_maintain)) cache$F_maintain <- std_cache$F_maintain
if (is.null(cache$F_replace))  cache$F_replace  <- std_cache$F_replace

stopifnot(identical(dim(cache$wsg8pfe), c(32L, 18L)))
stopifnot(all(abs(rowSums(cache$wsg8pfe) - 1.0) < 1e-8))
stopifnot(nrow(cache$countsdt8pfe) == 576L)
stopifnot(nrow(unique(cache$countsdt8pfe[, .(sidx, graw)])) == 576L)
stopifnot(all(cache$countsdt8pfe$graw %in% 0:17))
stopifnot(all(cache$countsdt8pfe$sidx %in% 1:32))
cat("[smoke 4/9] cache built: wsg 32x18, counts 576 unique (sidx,graw)\n")


# ---- Step 5. Load 8p fit and build 25-vector theta_init_fe. ----------------
fit_8p <- readRDS(here::here("Output", "Estimation_Results",
                             "Model_Replacement_8param_observed.rds"))
theta_init_fe <- c(fit_8p$theta_raw,
                   setNames(rep(0, 17L), cfg$fe_param_names))
stopifnot(length(theta_init_fe) == 25L)
stopifnot(!anyNA(theta_init_fe))
cat("[smoke 5/9] theta_init_fe constructed (length 25, alphas = 0)\n")


# ---- Step 6. Solve equilibrium at structural sub-vector. -------------------
eq0 <- solve_equilibrium_policy_replacement_8p(fit_8p$theta_raw, cache, cfg,
                                               max_iter = 500, tol = 1e-7)
stopifnot(isTRUE(eq0$converged))
stopifnot(all(abs(rowSums(eq0$P) - 1.0) < 1e-8))
stopifnot(all(eq0$P >= 0 & eq0$P <= 1))
stopifnot(all(is.finite(eq0$V)))
cat(sprintf("[smoke 6/9] eq solved: converged=TRUE, P rowsum max-dev=%.2e\n",
            max(abs(rowSums(eq0$P) - 1.0))))


# ---- Step 7. ONE likelihood evaluation (no optim). -------------------------
nll0 <- npl_likelihood_replacement_8p_fe(theta_init_fe, eq0$P, cache, cfg)
stopifnot(is.finite(nll0) && nll0 > 0)
cat(sprintf("[smoke 7/9] nll0 = %.3f\n", nll0))


# ---- Step 8. ONE geo-weighted CCP update. ----------------------------------
P1 <- .update_ccps_geoweighted_8p_fe(theta_init_fe, eq0$P, cache, cfg)
stopifnot(identical(dim(P1), c(32L, 3L)))
stopifnot(all(abs(rowSums(P1) - 1.0) < 1e-8))
stopifnot(all(P1 >= 0 & P1 <= 1))
stopifnot(all(is.finite(P1)))
cat(sprintf("[smoke 8/9] CCP update: dim 32x3, rowsum max-dev=%.2e, max|dP|=%.2e\n",
            max(abs(rowSums(P1) - 1.0)), max(abs(P1 - eq0$P))))


# ---- Step 9. Summary. ------------------------------------------------------
cat("\n[smoke 9/9] ALL CHECKS PASSED\n")
cat(sprintf("            nll0 = %.3f\n", nll0))
cat(sprintf("            cfg$n_params = %d, theta_init_fe length = %d\n",
            cfg$n_params, length(theta_init_fe)))
cat("            04i_smoke_test complete.\n")
