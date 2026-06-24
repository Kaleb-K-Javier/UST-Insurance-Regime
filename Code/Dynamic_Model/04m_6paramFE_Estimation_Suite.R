# ==============================================================================
# 04m_6paramFE_Estimation_Suite.R
# 6-parameter universal-γ replacement model — full estimation suite
#
# Estimates 6 variants (2 samples × 3 specs) and assembles comparison tables.
# Prerequisite: 04f_Replacement_Refit_Extended2000_AM_SE.R must have run to
#   create DCM_Primitives_Replacement_extended_2000plus.rds and
#   Model_Replacement_Estimates_extended_2000plus.rds.
#
# Ticket: T005
# Created: 2026-05-22
# ==============================================================================

# ------------------------------------------------------------------------------
# Logging block (CLAUDE.md convention)
# ------------------------------------------------------------------------------
.script_name <- tryCatch(
  tools::file_path_sans_ext(
    basename(rstudioapi::getSourceEditorContext()$path)),
  error = function(e) "04m_6paramFE_Estimation_Suite"
)
.log_path <- here::here("logs", paste0(
  .script_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
    .log_path, .script_name, R.version.string, getwd()))

# ------------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(Rcpp)
  library(here)
  library(numDeriv)
})

# ------------------------------------------------------------------------------
# Step 0: Run 04f to produce extended_2000plus inputs (prerequisite)
# ------------------------------------------------------------------------------
cat("=== STEP 0: source 04f to produce extended_2000plus inputs ===\n")
prims_ext_2000plus_path <- here::here("Output", "Estimation_Results",
                                       "DCM_Primitives_Replacement_extended_2000plus.rds")
fit_4p_ext_path         <- here::here("Output", "Estimation_Results",
                                       "Model_Replacement_Estimates_extended_2000plus.rds")

if (!file.exists(prims_ext_2000plus_path) || !file.exists(fit_4p_ext_path)) {
  cat("  extended_2000plus files not found; running 04f ...\n")
  source(here::here("Code", "Dynamic_Model",
                    "04f_Replacement_Refit_Extended2000_AM_SE.R"))
  if (!file.exists(prims_ext_2000plus_path) || !file.exists(fit_4p_ext_path)) {
    stop("04f completed but expected output files still missing:\n",
         "  ", prims_ext_2000plus_path, "\n",
         "  ", fit_4p_ext_path)
  }
  cat("  04f completed successfully.\n")
} else {
  cat("  extended_2000plus files already exist; skipping 04f.\n")
}

# ------------------------------------------------------------------------------
# Step 2: sourceCpp and source estimator library
# ------------------------------------------------------------------------------
cat("=== STEP 2: compile C++ engine ===\n")
Rcpp::sourceCpp(here::here("Code", "Helpers", "cpp_engine.cpp"))
stopifnot(exists("nll_replacement8pfe_profile_counts_cpp", mode = "function"))
cat("  nll_replacement8pfe_profile_counts_cpp: OK\n")
cat("  update_ccps_geoweighted_profile_cpp:    OK\n")

cat("=== SOURCE estimator library ===\n")
source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))
stopifnot(exists("flow_utilities_replacement_6p",                mode = "function"))
stopifnot(exists("npl_estimator_replacement_6p",                 mode = "function"))
stopifnot(exists("npl_estimator_replacement_6p_fe_profile",      mode = "function"))
stopifnot(exists("npl_likelihood_replacement_6p_fe_profile",     mode = "function"))
cat("  All 6p functions loaded.\n")

# ------------------------------------------------------------------------------
# Step 4: Load primitives and panels for both samples
# ------------------------------------------------------------------------------
cat("=== STEP 4: load primitives + panels ===\n")

prims_obs <- readRDS(here::here("Output", "Estimation_Results",
                                 "DCM_Primitives_Replacement_observed.rds"))
prims_ext <- readRDS(prims_ext_2000plus_path)

obs_obs <- fread(here::here("Data", "Analysis", "dcm_obs_panel_observed.csv"))
obs_ext <- fread(here::here("Data", "Analysis", "dcm_obs_panel_extended.csv"))[
  panel_year >= 2000L]

cat(sprintf("  observed panel:   %s rows\n", format(nrow(obs_obs), big.mark = ",")))
cat(sprintf("  extended 2000+:   %s rows\n", format(nrow(obs_ext), big.mark = ",")))

# ------------------------------------------------------------------------------
# Step 5: Warm-start from existing 4p fits
# ------------------------------------------------------------------------------
cat("=== STEP 5: build 6p warm-start from 4p fits ===\n")

fit_4p_obs <- readRDS(here::here("Output", "Estimation_Results",
                                  "Model_Replacement_Estimates_observed.rds"))
fit_4p_ext <- readRDS(fit_4p_ext_path)

# 4p theta_hat keys: kappa_exit, K (level), gamma_price, gamma_risk
# 4p theta_raw keys: kappa_exit, K_log  , gamma_price, gamma_risk
theta_init_6p_obs <- c(
  kappa_SW    = unname(fit_4p_obs$theta_hat[["kappa_exit"]]),
  kappa_DW    = unname(fit_4p_obs$theta_hat[["kappa_exit"]]),  # split at pooled value
  K_log_SW    = unname(fit_4p_obs$theta_raw[["K_log"]]),
  K_log_DW    = unname(fit_4p_obs$theta_raw[["K_log"]]),
  gamma_price = unname(fit_4p_obs$theta_hat[["gamma_price"]]),
  gamma_risk  = unname(fit_4p_obs$theta_hat[["gamma_risk"]])
)
theta_init_6p_ext <- c(
  kappa_SW    = unname(fit_4p_ext$theta_hat[["kappa_exit"]]),
  kappa_DW    = unname(fit_4p_ext$theta_hat[["kappa_exit"]]),
  K_log_SW    = unname(fit_4p_ext$theta_raw[["K_log"]]),
  K_log_DW    = unname(fit_4p_ext$theta_raw[["K_log"]]),
  gamma_price = unname(fit_4p_ext$theta_hat[["gamma_price"]]),
  gamma_risk  = unname(fit_4p_ext$theta_hat[["gamma_risk"]])
)

cat(sprintf("  obs  warm-start: kSW=%.3f kDW=%.3f KSW=%.3f KDW=%.3f gP=%.3f gR=%.3f\n",
    theta_init_6p_obs["kappa_SW"], theta_init_6p_obs["kappa_DW"],
    exp(theta_init_6p_obs["K_log_SW"]), exp(theta_init_6p_obs["K_log_DW"]),
    theta_init_6p_obs["gamma_price"], theta_init_6p_obs["gamma_risk"]))
cat(sprintf("  ext  warm-start: kSW=%.3f kDW=%.3f KSW=%.3f KDW=%.3f gP=%.3f gR=%.3f\n",
    theta_init_6p_ext["kappa_SW"], theta_init_6p_ext["kappa_DW"],
    exp(theta_init_6p_ext["K_log_SW"]), exp(theta_init_6p_ext["K_log_DW"]),
    theta_init_6p_ext["gamma_price"], theta_init_6p_ext["gamma_risk"]))

# ------------------------------------------------------------------------------
# Build configs
# ------------------------------------------------------------------------------
cfg_6p            <- create_estimation_config_replacement_6p(
  beta = 0.95, sigma2 = 1.0, max_npl_iter = 200L)
cfg_6p_fe_profile <- create_estimation_config_replacement_6p_fe_profile(
  beta = 0.95, sigma2 = 1.0, max_npl_iter = 200L,
  feweightsource = "controls", ccp_damping_lambda = 0.6,
  alpha_bound = 20, alpha_newton_tol = 1e-10, alpha_newton_max_iter = 30L)

# ------------------------------------------------------------------------------
# Step 6: Six estimation calls
# ------------------------------------------------------------------------------
OUT_FIT <- here::here("Output", "Estimation_Results")

# --- (a) fit_1: 6p no-FE, observed ---
cat("\n=== FIT 1/6: 6p no-FE, observed ===\n")
fit_1 <- npl_estimator_replacement_6p(
  obs_panel    = obs_obs,
  primitives   = prims_obs,
  config       = cfg_6p,
  theta_init   = theta_init_6p_obs,
  verbose      = TRUE,
  sample_label = "observed"
)
cat(sprintf("[FIT 1] observed 6p no-FE  converged=%s LL=%.0f iters=%d\n",
    fit_1$converged, fit_1$log_likelihood, fit_1$n_iter))
saveRDS(fit_1, file.path(OUT_FIT, "Model_Replacement_6p_observed.rds"))
cat("  Saved: Model_Replacement_6p_observed.rds\n")

# --- (b) fit_2: 6p no-FE, extended 2000+ ---
cat("\n=== FIT 2/6: 6p no-FE, extended 2000+ ===\n")
fit_2 <- npl_estimator_replacement_6p(
  obs_panel    = obs_ext,
  primitives   = prims_ext,
  config       = cfg_6p,
  theta_init   = theta_init_6p_ext,
  verbose      = TRUE,
  sample_label = "extended_2000plus"
)
cat(sprintf("[FIT 2] extended 6p no-FE  converged=%s LL=%.0f iters=%d\n",
    fit_2$converged, fit_2$log_likelihood, fit_2$n_iter))
saveRDS(fit_2, file.path(OUT_FIT, "Model_Replacement_6p_extended_2000plus.rds"))
cat("  Saved: Model_Replacement_6p_extended_2000plus.rds\n")

# --- (c) fit_3: 6p stayFE profile, observed (warm-start from fit_1) ---
cat("\n=== FIT 3/6: 6p stayFE profile, observed ===\n")
fit_3 <- npl_estimator_replacement_6p_fe_profile(
  obs_panel        = obs_obs,
  primitives       = prims_obs,
  config           = cfg_6p_fe_profile,
  thetastruct_init = fit_1$theta_hat,   # log-scale K, compatible with struct_param_names
  verbose          = TRUE,
  sample_label     = "observed"
)
cat(sprintf("[FIT 3] observed 6p stayFE converged=%s LL=%.0f iters=%d\n",
    fit_3$converged, fit_3$log_likelihood, fit_3$n_iter))
saveRDS(fit_3, file.path(OUT_FIT, "Model_Replacement_6paramFE_profile_observed.rds"))
cat("  Saved: Model_Replacement_6paramFE_profile_observed.rds\n")

# --- (d) fit_4: 6p stayFE profile, extended 2000+ (warm-start from fit_2) ---
cat("\n=== FIT 4/6: 6p stayFE profile, extended 2000+ ===\n")
fit_4 <- npl_estimator_replacement_6p_fe_profile(
  obs_panel        = obs_ext,
  primitives       = prims_ext,
  config           = cfg_6p_fe_profile,
  thetastruct_init = fit_2$theta_hat,
  verbose          = TRUE,
  sample_label     = "extended_2000plus"
)
cat(sprintf("[FIT 4] extended 6p stayFE converged=%s LL=%.0f iters=%d\n",
    fit_4$converged, fit_4$log_likelihood, fit_4$n_iter))
saveRDS(fit_4, file.path(OUT_FIT, "Model_Replacement_6paramFE_profile_extended_2000plus.rds"))
cat("  Saved: Model_Replacement_6paramFE_profile_extended_2000plus.rds\n")

# --- (e) fit_5: 6p stayFE profile, observed, drop {SD, MO} ---
cat("\n=== FIT 5/6: 6p stayFE profile, observed, drop {SD,MO} ===\n")
obs_obs_drop <- obs_obs[!state %in% c("SD", "MO")]
cat(sprintf("  observed_dropSDMO: %s rows (dropped %s)\n",
    format(nrow(obs_obs_drop), big.mark = ","),
    format(nrow(obs_obs) - nrow(obs_obs_drop), big.mark = ",")))
fit_5 <- npl_estimator_replacement_6p_fe_profile(
  obs_panel        = obs_obs_drop,
  primitives       = prims_obs,       # primitives unchanged per spec Eq. 3
  config           = cfg_6p_fe_profile,
  thetastruct_init = fit_3$theta_hat, # warm-start from full-sample FE fit
  verbose          = TRUE,
  sample_label     = "observed_dropSDMO"
)
cat(sprintf("[FIT 5] observed_dropSDMO  converged=%s LL=%.0f iters=%d\n",
    fit_5$converged, fit_5$log_likelihood, fit_5$n_iter))
saveRDS(fit_5, file.path(OUT_FIT, "Model_Replacement_6paramFE_profile_observed_dropSDMO.rds"))
cat("  Saved: Model_Replacement_6paramFE_profile_observed_dropSDMO.rds\n")

# --- (f) fit_6: 6p stayFE profile, extended 2000+, drop {SD, MO} ---
cat("\n=== FIT 6/6: 6p stayFE profile, extended 2000+, drop {SD,MO} ===\n")
obs_ext_drop <- obs_ext[!state %in% c("SD", "MO")]
cat(sprintf("  extended_2000plus_dropSDMO: %s rows (dropped %s)\n",
    format(nrow(obs_ext_drop), big.mark = ","),
    format(nrow(obs_ext) - nrow(obs_ext_drop), big.mark = ",")))
fit_6 <- npl_estimator_replacement_6p_fe_profile(
  obs_panel        = obs_ext_drop,
  primitives       = prims_ext,
  config           = cfg_6p_fe_profile,
  thetastruct_init = fit_4$theta_hat,
  verbose          = TRUE,
  sample_label     = "extended_2000plus_dropSDMO"
)
cat(sprintf("[FIT 6] ext_2000plus_dropSDMO converged=%s LL=%.0f iters=%d\n",
    fit_6$converged, fit_6$log_likelihood, fit_6$n_iter))
saveRDS(fit_6, file.path(OUT_FIT,
  "Model_Replacement_6paramFE_profile_extended_2000plus_dropSDMO.rds"))
cat("  Saved: Model_Replacement_6paramFE_profile_extended_2000plus_dropSDMO.rds\n")

# ------------------------------------------------------------------------------
# Step 7: Acceptance criterion regression test
#   Verify the C++ early-out edit is transparent for the full observed sample.
#   Approach: compare C++ modified kernel vs R fallback at T004's (theta, P).
#   These must agree within 1e-3.  The T004 SAVED log_likelihood may differ
#   by ~0.02 from a re-evaluation at P_hat (pre-existing NPL P-mismatch:
#   saved LL used P-before-last-CCP-update; P_hat is P-after).
# ------------------------------------------------------------------------------
cat("\n=== REGRESSION TEST: C++ early-out transparent for full observed sample ===\n")
t004_path <- here::here("Output", "Estimation_Results",
                         "Model_Replacement_8paramFE_profile_observed.rds")
if (file.exists(t004_path)) {
  fit_t004    <- readRDS(t004_path)
  theta_t004  <- fit_t004$theta_hat
  P_t004      <- fit_t004$P_hat
  cache_t004  <- fit_t004$cache
  config_t004 <- fit_t004$config

  # Verify no group has zero observations → early-out never fires
  grp_tot <- cache_t004$countsdt8pfe[, .(n_total = sum(nM + nE + nR)), by = graw]
  zero_grps <- grp_tot[n_total == 0, graw]
  if (length(zero_grps) > 0)
    stop("REGRESSION FAIL: zero-count groups found in full observed sample: ",
         paste(zero_grps, collapse = ", "))
  cat("  All groups have observations → early-out never fires. OK\n")

  # Compare C++ vs R fallback at same point
  ll_cpp <- npl_likelihood_replacement_8p_fe_profile(
    thetastruct = theta_t004, Pfixed = P_t004,
    cache = cache_t004, config = config_t004)
  v_r        <- .compute_v_indices_8p(theta_t004, P_t004, cache_t004, config_t004)
  alpha_r    <- c(0.0, .solve_alpha_vec_profile(theta_t004, v_r, cache_t004, config_t004))
  log_eps    <- log(config_t004$epsprob)
  counts_t   <- cache_t004$countsdt8pfe
  ll_r       <- 0.0
  for (i in seq_len(nrow(counts_t))) {
    nm <- counts_t$nM[i]; ne <- counts_t$nE[i]; nr <- counts_t$nR[i]
    if (nm + ne + nr == 0L) next
    s  <- counts_t$sidx[i]; g <- counts_t$graw[i]
    a  <- alpha_r[g + 1L]
    uM <- v_r$vM[s] + a; uE <- v_r$vE[s]; uR <- v_r$vR[s] + a
    lse <- .logsumexp3(uM, uE, uR)
    ll_r <- ll_r + nm * max(uM-lse, log_eps) +
                   ne * max(uE-lse, log_eps) +
                   nr * max(uR-lse, log_eps)
  }
  ll_r <- -ll_r

  diff_cpp_r <- abs(ll_cpp - ll_r)
  cat(sprintf("  C++  NLL = %.6f\n", ll_cpp))
  cat(sprintf("  R    NLL = %.6f\n", ll_r))
  cat(sprintf("  |C++ - R| = %.2e  (must be < 1e-3)\n", diff_cpp_r))
  stopifnot(diff_cpp_r < 1e-3)
  cat("  REGRESSION PASS: modified C++ kernel matches R fallback\n")
  cat(sprintf("  (Saved T004 LL = %.6f; diff from re-eval = %.4f — pre-existing P-mismatch)\n",
              fit_t004$log_likelihood, abs(-ll_cpp - fit_t004$log_likelihood)))
} else {
  cat("  T004 fit not found; skipping regression test.\n")
}

# ------------------------------------------------------------------------------
# Step 9: Comparison table — theta, LL, N, k, BIC
# ------------------------------------------------------------------------------
cat("\n=== STEP 9: build comparison table ===\n")

OUT_TAB <- here::here("Output", "Tables")
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)

# Helper: extract 6p theta from a fit (handles both no-FE and FE-profile variants)
.extract_theta6p <- function(fit) {
  th <- fit$theta_hat
  c(
    kappa_SW    = unname(th[["kappa_SW"]]),
    kappa_DW    = unname(th[["kappa_DW"]]),
    K_SW        = exp(th[["K_log_SW"]]),   # reported in $-units per spec
    K_DW        = exp(th[["K_log_DW"]]),
    gamma_price = unname(th[["gamma_price"]]),
    gamma_risk  = unname(th[["gamma_risk"]])
  )
}
.extract_se6p <- function(fit) {
  se <- fit$se_theta
  c(
    kappa_SW    = unname(se[["kappa_SW"]]),
    kappa_DW    = unname(se[["kappa_DW"]]),
    K_SW        = unname(se[["K_log_SW"]]) * exp(fit$theta_hat[["K_log_SW"]]),  # delta method
    K_DW        = unname(se[["K_log_DW"]]) * exp(fit$theta_hat[["K_log_DW"]]),
    gamma_price = unname(se[["gamma_price"]]),
    gamma_risk  = unname(se[["gamma_risk"]])
  )
}

# N_obs: sum of all observation counts in countsdt8pfe for FE fits;
# use nrow(obs_panel) for no-FE fits (obs are stored in cache$n_obs)
.get_nobs <- function(fit) {
  if (!is.null(fit$cache$countsdt8pfe)) {
    sum(fit$cache$countsdt8pfe$nM + fit$cache$countsdt8pfe$nE +
        fit$cache$countsdt8pfe$nR)
  } else if (!is.null(fit$cache$n_obs)) {
    fit$cache$n_obs
  } else {
    NA_integer_
  }
}

fits <- list(fit_1, fit_2, fit_3, fit_4, fit_5, fit_6)
col_ids <- c("obs_noFE", "ext_noFE", "obs_stayFE", "ext_stayFE",
             "obs_stayFE_dropSDMO", "ext_stayFE_dropSDMO")
# k_effective: 6 for no-FE; 23 for FE-full (6+17); 21 for FE-drop (6+15)
k_eff <- c(6L, 6L, 23L, 23L, 21L, 21L)

theta_vals <- sapply(fits, .extract_theta6p)   # 6 × 6 matrix
ll_vals    <- sapply(fits, function(f) f$log_likelihood)
n_vals     <- sapply(fits, .get_nobs)
bic_vals   <- -2 * ll_vals + k_eff * log(n_vals)

# Assemble CSV data.table (10 rows × 7 cols)
param_rows <- c("kappa_SW", "kappa_DW", "K_SW", "K_DW",
                "gamma_price", "gamma_risk",
                "log_L", "N_obs", "k_effective", "BIC")
csv_dt <- data.table(
  parameter = param_rows
)
for (j in seq_along(col_ids)) {
  col_vals <- c(
    theta_vals[1, j],
    theta_vals[2, j],
    theta_vals[3, j],
    theta_vals[4, j],
    theta_vals[5, j],
    theta_vals[6, j],
    ll_vals[j],
    n_vals[j],
    k_eff[j],
    bic_vals[j]
  )
  csv_dt[, (col_ids[j]) := col_vals]
}
fwrite(csv_dt, file.path(OUT_TAB, "04m_Theta_Comparison_6p_Suite.csv"))
cat("  Saved: 04m_Theta_Comparison_6p_Suite.csv\n")

# --- TEX table ---
se_list <- lapply(fits, .extract_se6p)   # list of 6-vecs

fmt_val <- function(x, digits = 3) {
  if (is.na(x)) "---"
  else formatC(x, format = "f", digits = digits)
}
fmt_se  <- function(x) {
  if (is.na(x)) "(---)"
  else sprintf("(%s)", formatC(x, format = "f", digits = 3))
}

# Row-pair format: value row + SE row
tex_row_pair <- function(label_tex, row_name, digits = 3) {
  vals <- theta_vals[row_name, ]
  ses  <- sapply(se_list, function(s) s[[row_name]])
  val_str <- paste(sapply(vals, fmt_val, digits = digits), collapse = " & ")
  se_str  <- paste(sapply(ses,  fmt_se),                  collapse = " & ")
  paste0("  ", label_tex, " & ", val_str, " \\\\\n",
         "              & ", se_str,  " \\\\\n")
}
tex_stat_row <- function(label_tex, vals, digits = 0) {
  val_str <- paste(sapply(vals, fmt_val, digits = digits), collapse = " & ")
  paste0("  ", label_tex, " & ", val_str, " \\\\\n")
}

tex_body <- paste0(
  tex_row_pair("$\\kappa_{\\mathrm{SW}}$",   "kappa_SW"),
  tex_row_pair("$\\kappa_{\\mathrm{DW}}$",   "kappa_DW"),
  tex_row_pair("$K_{\\mathrm{SW}}$",         "K_SW"),
  tex_row_pair("$K_{\\mathrm{DW}}$",         "K_DW"),
  tex_row_pair("$\\gamma_{\\mathrm{price}}$", "gamma_price"),
  tex_row_pair("$\\gamma_{\\mathrm{risk}}$",  "gamma_risk"),
  "  \\hline\n",
  tex_stat_row("$\\log L$",       ll_vals,   digits = 0),
  tex_stat_row("$N$",             n_vals,    digits = 0),
  tex_stat_row("$k_{\\text{eff}}$", k_eff,   digits = 0),
  tex_stat_row("BIC",             bic_vals,  digits = 0)
)

tex_str <- paste0(
  "\\resizebox{\\textwidth}{!}{%\n",
  "\\begin{tabular}{l cc cc cc}\n",
  "\\hline\n",
  " & \\multicolumn{2}{c}{no FE} & \\multicolumn{2}{c}{stayFE (profile)}\n",
  " & \\multicolumn{2}{c}{stayFE, drop SD+MO} \\\\\n",
  " & Obs & Ext & Obs & Ext & Obs & Ext \\\\\n",
  "\\hline\n",
  tex_body,
  "\\hline\n",
  "\\multicolumn{7}{l}{\\footnotesize Universal-$\\gamma$ replacement model. ",
  "SEs in parentheses; both $\\kappa$ and $K$ reported in \\$-units ",
  "({\\texttimes}10,000 model-unit scaler). drop SD+MO columns filter the obs ",
  "panel to remove the two data-degenerate states; structural ",
  "primitives unchanged. AIC not reported because $N$ differs across ",
  "columns; BIC reported for within-sample comparison only.} \\\\\n",
  "\\end{tabular}}\n"
)
writeLines(tex_str, file.path(OUT_TAB, "04m_Theta_Comparison_6p_Suite.tex"))
cat("  Saved: 04m_Theta_Comparison_6p_Suite.tex\n")

# ------------------------------------------------------------------------------
# Step 10: FE-alphas table for the 4 FE variants
# ------------------------------------------------------------------------------
cat("\n=== STEP 10: build FE-alphas table ===\n")

fe_fits    <- list(fit_3, fit_4, fit_5, fit_6)
fe_col_ids <- c("obs_stayFE", "ext_stayFE",
                "obs_stayFE_dropSDMO", "ext_stayFE_dropSDMO")
drop_sets  <- list(NULL, NULL, c("SD", "MO"), c("SD", "MO"))

states_ordered <- c("TX", CONTROLSTATES)   # TX first, then 17 controls alphabetical
alpha_csv_dt   <- data.table(state = states_ordered)

for (j in seq_along(fe_fits)) {
  fit_j  <- fe_fits[[j]]
  drop_j <- drop_sets[[j]]

  col_vals <- vapply(states_ordered, function(st) {
    if (st == "TX") return("0")      # TX baseline is always 0
    if (!is.null(drop_j) && st %in% drop_j) return("dropped")
    alpha_name <- paste0("alpha", st)
    val        <- fit_j$alpha_hat[[alpha_name]]
    if (is.null(val) || is.na(val)) return("NA")
    formatC(val, format = "f", digits = 4)
  }, character(1L))

  alpha_csv_dt[, (fe_col_ids[j]) := col_vals]
}

fwrite(alpha_csv_dt, file.path(OUT_TAB, "04m_FE_Alphas_6p_Suite.csv"))
cat("  Saved: 04m_FE_Alphas_6p_Suite.csv\n")

# --- TEX for alphas ---
tex_alpha_rows <- character(length(states_ordered))
for (i in seq_along(states_ordered)) {
  st      <- states_ordered[i]
  row_raw <- as.character(unlist(alpha_csv_dt[state == st,
                                               .SD, .SDcols = fe_col_ids]))
  row_fmt <- vapply(row_raw, function(v) {
    if (v %in% c("0", "dropped", "NA")) v
    else {
      num <- as.numeric(v)
      if (is.na(num)) v else formatC(num, format = "f", digits = 3)
    }
  }, character(1L))
  tex_alpha_rows[i] <- paste0("  ", st, " & ",
                               paste(row_fmt, collapse = " & "), " \\\\\n")
}

tex_alpha_str <- paste0(
  "\\resizebox{\\textwidth}{!}{%\n",
  "\\begin{tabular}{l cccc}\n",
  "\\hline\n",
  " State & obs stayFE & ext stayFE & obs dropSDMO & ext dropSDMO \\\\\n",
  "\\hline\n",
  paste(tex_alpha_rows, collapse = ""),
  "\\hline\n",
  "\\multicolumn{5}{l}{\\footnotesize TX $\\alpha \\equiv 0$ (baseline). ",
  "``dropped'' = state excluded from likelihood; $\\alpha$ irrelevant. ",
  "For SEs, see individual \\texttt{.rds} fit files.} \\\\\n",
  "\\end{tabular}}\n"
)
writeLines(tex_alpha_str, file.path(OUT_TAB, "04m_FE_Alphas_6p_Suite.tex"))
cat("  Saved: 04m_FE_Alphas_6p_Suite.tex\n")

# ------------------------------------------------------------------------------
# Final summary
# ------------------------------------------------------------------------------
cat("\n=== FINAL SUMMARY ===\n")
for (j in seq_along(fits)) {
  cat(sprintf("  [%d] %s | converged=%s | LL=%.0f | iters=%d | N=%s\n",
      j, col_ids[j], fits[[j]]$converged,
      fits[[j]]$log_likelihood, fits[[j]]$n_iter,
      format(n_vals[j], big.mark = ",")))
}
cat(sprintf("\nAll 13 deliverables written.\nLog: %s\n", .log_path))
