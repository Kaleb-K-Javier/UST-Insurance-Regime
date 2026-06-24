# ==============================================================================
# Code/Dynamic_Model/04r_CF234_Welfare_6p.R
# TICKET 018 — CF2/CF3/CF4 welfare on the canonical 6p gammafree fit + bar chart
#
# Ports CF2 (replacement subsidy), CF3 (Pigouvian maintain penalty), and CF4
# (age mandate) onto the 04o 6p machinery (canonical gammafree fit, TX-empirical
# mu, Semantic-2 alphas-dropped, E_GRID {17k, 50k}), re-runs CF1 (TX flat-fee)
# fresh for cross-validation against T017, and produces the CF1-CF4 welfare
# summary (CSV + TeX) and the welfare bar chart (PNG + PDF).
#
# Clones the 04o scaffolding; does NOT edit 04o_CF_TX_FlatFee.R, 04j, or the
# shared estimator (improved_estimator_OPTIMIZED.r). CF3/CF4 hooks are added as
# driver-local *_6p_cf clones with optional (u_maintain_offset, mandate_idx)
# args; baseline/CF1/CF2 call them with defaults.
#
# Inputs:  Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds
#          DCM_Primitives_Replacement_observed.rds
#          dcm_obs_panel_observed.csv
#          Output/Tables/04o_CF_Welfare_Summary.csv  (T017 archived — CF1 check)
#
# Outputs: Output/Tables/04r_CF234_Welfare_Summary.csv  (40 rows)
#          Output/Tables/04r_CF234_Welfare_Summary.tex
#          Output/Figures/04r_CF234_Welfare_BarChart.png + .pdf
#          Output/Estimation_Results/CF234_Welfare_6p_results.rds
# ==============================================================================

# ==============================================================================
# SECTION 1 — LOGGING
# ==============================================================================
.log_path <- here::here("logs", paste0(
  "04r_CF234_Welfare_6p_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 04r_CF234_Welfare_6p.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# ==============================================================================
# SECTION 2 — PACKAGES + CONSTANTS
# ==============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(Rcpp)
  library(here)
  library(ggplot2)
  library(scales)
})
setDTthreads(0L)
set.seed(20260602L)

SCALE_FACTOR   <- 10000L
E_GRID_DOLLARS <- c(HEALTH_ONLY = 17000, HEALTH_PLUS_UNMEASURED = 50000)
E_GRID         <- E_GRID_DOLLARS / SCALE_FACTOR
SUBSIDY_BASE   <- 0.50      # CF2 replacement subsidy fraction
MANDATE_AGE_BIN <- 6L       # CF4: no maintain for A_bin >= 6 (tanks 25+ yr)
V_FORCE        <- -1e10     # CF4 maintain-off injection (see Section 5 note)

OUT_FIT <- here::here("Output", "Estimation_Results")
OUT_TAB <- here::here("Output", "Tables")
OUT_FIG <- here::here("Output", "Figures")
dir.create(OUT_FIT, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)

# Replace-margin caveat (carried from T017 / Ticket 014) — used in TeX, figure,
# and log per acceptance criterion C9.
CAVEAT_TXT <- paste0(
  "CF2 (subsidy) and CF4 (mandate) act on the replace margin, which carries ",
  "the F_replace single-tank-reset caveat (replace off-support ~98%, Ticket ",
  "014). Their welfare numbers are INDICATIVE, not validated. CF1 and CF3 ",
  "act through clean (premium / maintain) channels.")

# ==============================================================================
# SECTION 3 — SOURCE + SOURCECPP
# ==============================================================================
cat("=== SECTION 3: SOURCE + SOURCECPP ===\n")

source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))
tryCatch(
  Rcpp::sourceCpp(here::here("Code", "Helpers", "cpp_engine.cpp")),
  error = function(e) warning(sprintf(
    "cpp_engine.cpp could not compile — R fallback mode: %s",
    conditionMessage(e)))
)

stopifnot(exists("solve_equilibrium_policy_replacement_6p", mode = "function"))
stopifnot(exists("flow_utilities_replacement_6p",           mode = "function"))
stopifnot(exists("update_ccps_replacement_6p",              mode = "function"))
stopifnot(exists("invert_value_function_replacement",       mode = "function"))
cat("  All required 6p functions present\n")

# ==============================================================================
# SECTION 4 — LOAD INPUTS
# ==============================================================================
cat("=== SECTION 4: LOAD INPUTS ===\n")

fit_path   <- here::here("Output", "Estimation_Results",
                         "Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds")
prims_path <- here::here("Output", "Estimation_Results",
                         "DCM_Primitives_Replacement_observed.rds")
obs_path   <- here::here("Data", "Analysis", "dcm_obs_panel_observed.csv")
arch_path  <- here::here("Output", "Tables", "04o_CF_Welfare_Summary.csv")

stopifnot(file.exists(fit_path), file.exists(prims_path),
          file.exists(obs_path), file.exists(arch_path))

fit        <- readRDS(fit_path)
primitives <- readRDS(prims_path)
obs_panel  <- fread(obs_path)

cat(sprintf("  fit: LL=%.3f | converged=%s | theta[6]:\n",
    fit$log_likelihood, fit$converged))
print(round(fit$theta_hat, 4))
stopifnot(fit$converged == TRUE, length(fit$theta_hat) == 6L)

# beta read from the fit's own config — never hardcoded (Ticket 018 / spec).
BETA      <- fit$config$beta
stopifnot(is.finite(BETA), BETA > 0, BETA < 1)
PV_FACTOR <- 1 / (1 - BETA)
cat(sprintf("  beta (from fit$config$beta) = %.4f | PV_FACTOR = %.4f\n",
    BETA, PV_FACTOR))

# Structural params (no alphas: Semantic-2 — alphas don't enter the re-solve)
theta_struct <- fit$theta_hat

# ==============================================================================
# SECTION 5 — DRIVER-LOCAL 6p CLONES (*_6p_cf)
# ------------------------------------------------------------------------------
# Byte-for-byte clones of flow_utilities_replacement_6p, update_ccps_replacement_6p
# and solve_equilibrium_policy_replacement_6p (OPTIMIZED.r) PLUS two optional args:
#   u_maintain_offset : length-n_states additive term on u^M (default 0)  -> CF3
#   mandate_idx       : state indices forced to no-maintain (default none) -> CF4
# The cloned solver calls ONLY the cloned flow/ccp helpers, and reuses the
# original invert_value_function_replacement (unchanged) — preserving the exact
# internal path (incl. any C++ inside the inverter). OPTIMIZED.r is NOT edited.
# ==============================================================================
cat("=== SECTION 5: DRIVER-LOCAL 6p CLONES ===\n")

flow_utilities_6p_cf <- function(theta, cache,
                                 u_maintain_offset = rep(0, cache$n_states)) {
  required <- c("kappa_SW", "kappa_DW", "K_log_SW", "K_log_DW",
                "gamma_price", "gamma_risk")
  stopifnot(all(required %in% names(theta)))
  stopifnot(length(u_maintain_offset) == cache$n_states)

  K_vec     <- ifelse(cache$wall_idx == 1L,
                      exp(theta[["K_log_SW"]]), exp(theta[["K_log_DW"]]))
  kappa_vec <- ifelse(cache$wall_idx == 1L,
                      theta[["kappa_SW"]],      theta[["kappa_DW"]])
  gp <- theta[["gamma_price"]]   # scalar (universal-gamma)
  gr <- theta[["gamma_risk"]]    # scalar

  # CF3 enters here: exogenous additive penalty on the maintain payoff, with
  # coefficient EXACTLY 1 (NOT folded into hazard_loss, which gr would scale).
  u_M <- 1 + gp * cache$P_vec - gr * cache$hazard_loss + u_maintain_offset
  U   <- cbind(maintain = u_M, exit = kappa_vec, replace = -K_vec)
  stopifnot(all(dim(U) == c(32L, 3L)))
  stopifnot(all(colnames(U) == c("maintain", "exit", "replace")))
  U
}

update_ccps_6p_cf <- function(theta, V, cache, config,
                              u_maintain_offset = rep(0, cache$n_states),
                              mandate_idx = integer(0)) {
  sigma <- config$sigma2
  beta  <- cache$beta

  U   <- flow_utilities_6p_cf(theta, cache, u_maintain_offset)
  v_M <- U[, "maintain"] + beta * as.numeric(cache$F_maintain %*% V)
  v_E <- U[, "exit"]
  v_R <- U[, "replace"]  + beta * as.numeric(cache$F_replace  %*% V)

  # CF4 age-mandate: drive v_maintain to a large-negative value for mandated
  # cells BEFORE the softmax, then let the EXISTING softmax -> eps_prob floor ->
  # renormalize run untouched (NPL_REFERENCE §4 constrained action set; the
  # floor sends P_M -> eps_prob and P_M*log P_M -> 0). config$min_log_val in
  # this fit is a *probability* floor (1e-12), NOT a v-index floor, so it is
  # unusable here; V_FORCE = -1e10 is used per researcher Q2 fallback. -Inf is
  # deliberately avoided (risks log(0) downstream).
  if (length(mandate_idx) > 0L) v_M[mandate_idx] <- V_FORCE

  v_mat <- cbind(maintain = v_M, exit = v_E, replace = v_R)
  v_max <- pmax(v_M, v_E, v_R)
  z     <- exp((v_mat - v_max) / sigma)
  P     <- z / rowSums(z)
  P[P < config$eps_prob] <- config$eps_prob
  P <- P / rowSums(P)
  colnames(P) <- c("maintain", "exit", "replace")
  P
}

solve_equilibrium_6p_cf <- function(theta, cache, config,
                                    max_iter = 1000L, tol = 1e-8,
                                    u_maintain_offset = rep(0, cache$n_states),
                                    mandate_idx = integer(0)) {
  P <- matrix(1/3, cache$n_states, 3L,
              dimnames = list(NULL, c("maintain", "exit", "replace")))
  converged <- FALSE
  for (i in seq_len(max_iter)) {
    U     <- flow_utilities_6p_cf(theta, cache, u_maintain_offset)
    V     <- invert_value_function_replacement(P, U, cache, config)
    P_new <- update_ccps_6p_cf(theta, V, cache, config,
                               u_maintain_offset, mandate_idx)
    if (max(abs(P_new - P)) < tol) { converged <- TRUE; P <- P_new; break }
    P <- P_new
  }
  U <- flow_utilities_6p_cf(theta, cache, u_maintain_offset)
  V <- invert_value_function_replacement(P, U, cache, config)
  list(P = P, V = V, converged = converged, n_iter = i)
}

# ==============================================================================
# SECTION 6 — BUILD BASELINE CACHE (clone of 04o Section 5)
# ==============================================================================
cat("=== SECTION 6: BUILD BASELINE CACHE ===\n")

cfg <- create_estimation_config_replacement_6p_fe_profile(
  beta = BETA, sigma2 = 1.0, max_npl_iter = 200L)

config_4p <- create_estimation_config_replacement(
  beta = BETA, sigma2 = 1.0, npl_iter = cfg$max_npl_iter)

cache_baseline <- create_estimation_cache_replacement_8p(
  primitives, obs_panel, config_4p, cfg)

cat(sprintf("  Baseline P_vec range: [%.4f, %.4f] | n_states=%d\n",
    min(cache_baseline$P_vec), max(cache_baseline$P_vec),
    cache_baseline$n_states))
stopifnot(cache_baseline$n_states == 32L)

# ==============================================================================
# SECTION 7 — FAITHFULNESS ASSERTION + BASELINE EQUILIBRIUM
# ------------------------------------------------------------------------------
# Prove the clone did not drift: run the ORIGINAL solver and the *_6p_cf clone
# once at canonical theta with defaults; require P and V identical to 1e-10.
# ==============================================================================
cat("=== SECTION 7: FAITHFULNESS CHECK + BASELINE EQUILIBRIUM ===\n")

eq_orig <- solve_equilibrium_policy_replacement_6p(
  theta = theta_struct, cache = cache_baseline, config = cfg,
  max_iter = 500L, tol = 1e-7)
eq_clone <- solve_equilibrium_6p_cf(
  theta = theta_struct, cache = cache_baseline, config = cfg,
  max_iter = 500L, tol = 1e-7)

stopifnot(eq_orig$converged, eq_clone$converged)
d_P <- max(abs(eq_orig$P - eq_clone$P))
d_V <- max(abs(eq_orig$V - eq_clone$V))
cat(sprintf("  Clone faithfulness: max|dP|=%.3e  max|dV|=%.3e (require < 1e-10)\n",
    d_P, d_V))
stopifnot(d_P < 1e-10, d_V < 1e-10)
cat("  Faithfulness check: PASS\n")

# Baseline = clone with defaults (identical to original; verified above)
P_baseline <- eq_clone$P
V_baseline <- eq_clone$V
stopifnot(all(abs(rowSums(P_baseline) - 1) < 1e-6), all(is.finite(V_baseline)))

# ==============================================================================
# SECTION 8 — STATE LUT, mu, h_vec, K_base_vec
# ==============================================================================
cat("=== SECTION 8: STATE LUT / mu / h_vec / K_base_vec ===\n")

state_lut <- primitives$state_lut
stopifnot(!is.null(state_lut),
          all(c("s_idx", "A_bin", "w_state", "rho_state") %in% names(state_lut)))

# mu = fraction of TX obs_panel rows in each of the 32 state cells (04o Sec 8)
tx_obs    <- obs_panel[texas_treated == 1L]
mu_counts <- tx_obs[, .N, by = s_idx]
mu_dt     <- merge(data.table(s_idx = 1:32L), mu_counts, by = "s_idx", all.x = TRUE)
mu_dt[is.na(N), N := 0L]
setorder(mu_dt, s_idx)
mu <- mu_dt$N / sum(mu_dt$N)
stopifnot(abs(sum(mu) - 1) < 1e-10, all(mu >= 0), length(mu) == 32L)
cat(sprintf("  mu computed from %s TX observations\n",
    format(nrow(tx_obs), big.mark = ",")))

h_vec <- primitives$h_vec
stopifnot(length(h_vec) == 32L)

# K_BASELINE per wall (length-32 vector, NOT scalar) — aligned to the cache's
# wall convention (wall_idx == 1 => SW), matching flow_utilities' K_vec.
K_base_vec <- ifelse(cache_baseline$wall_idx == 1L,
                     exp(theta_struct[["K_log_SW"]]),
                     exp(theta_struct[["K_log_DW"]]))
stopifnot(length(K_base_vec) == 32L)
cat(sprintf("  K_base_vec: SW=%.4f DW=%.4f\n",
    exp(theta_struct[["K_log_SW"]]), exp(theta_struct[["K_log_DW"]])))

# ==============================================================================
# SECTION 9 — CF1: TX FLAT-FEE (re-solved fresh; port of 04o Sections 6-7)
# ==============================================================================
cat("=== SECTION 9: CF1 TX FLAT-FEE EQUILIBRIUM ===\n")

# Median FF premium per (A_bin, w_state) from control states, 2006+ only
ff_premiums <- obs_panel[texas_treated == 0L & panel_year >= 2006L & !is.na(premium),
                         .(P_FF = median(premium, na.rm = TRUE), n_obs = .N),
                         by = .(A_bin, w_state)]
ff_premiums_fallback <- obs_panel[texas_treated == 0L & panel_year >= 1999L & !is.na(premium),
                                  .(P_FF_fallback = median(premium, na.rm = TRUE)),
                                  by = .(A_bin, w_state)]
ff_premiums <- merge(ff_premiums, ff_premiums_fallback,
                     by = c("A_bin", "w_state"), all.x = TRUE)
ff_premiums[is.na(P_FF) | n_obs < 5L, P_FF := P_FF_fallback]
ff_premiums[is.na(P_FF), P_FF := median(obs_panel$premium, na.rm = TRUE)]

tx_cells <- state_lut[rho_state == 2L]
stopifnot(nrow(tx_cells) == 16L)

prem_sidecar <- merge(tx_cells, ff_premiums[, .(A_bin, w_state, P_FF)],
                      by = c("A_bin", "w_state"), all.x = TRUE)
prem_sidecar[, baseline_P := cache_baseline$P_vec[s_idx]]
prem_sidecar[, cf_P       := P_FF]
prem_sidecar[is.na(cf_P), cf_P := P_FF_fallback]
stopifnot(!anyNA(prem_sidecar$cf_P))

cf1_cache <- cache_baseline
cf1_cache$P_vec <- cache_baseline$P_vec   # copy (numeric vector, not reference)
for (i in seq_len(nrow(tx_cells))) {
  sidx <- tx_cells$s_idx[i]
  cf_P <- prem_sidecar[s_idx == sidx, cf_P]
  stopifnot(length(cf_P) == 1L, !is.na(cf_P))
  cf1_cache$P_vec[sidx] <- cf_P
}
cat(sprintf("  CF1 P_vec: %d TX cells updated (baseline mean %.4f -> CF mean %.4f)\n",
    nrow(tx_cells), mean(prem_sidecar$baseline_P), mean(prem_sidecar$cf_P)))

eq_cf1 <- solve_equilibrium_6p_cf(theta_struct, cf1_cache, cfg,
                                  max_iter = 500L, tol = 1e-7)
stopifnot(eq_cf1$converged)
P_cf1 <- eq_cf1$P; V_cf1 <- eq_cf1$V
stopifnot(all(abs(rowSums(P_cf1) - 1) < 1e-6), all(is.finite(V_cf1)))
cat(sprintf("  CF1: converged=%s | P_M range [%.4f, %.4f]\n",
    eq_cf1$converged, min(P_cf1[, "maintain"]), max(P_cf1[, "maintain"])))

# ==============================================================================
# SECTION 10 — CF2: REPLACEMENT SUBSIDY (s* = 0.50)
# ==============================================================================
cat("=== SECTION 10: CF2 REPLACEMENT SUBSIDY ===\n")

theta_cf2 <- theta_struct
theta_cf2[["K_log_SW"]] <- theta_cf2[["K_log_SW"]] + log(1 - SUBSIDY_BASE)
theta_cf2[["K_log_DW"]] <- theta_cf2[["K_log_DW"]] + log(1 - SUBSIDY_BASE)
cat(sprintf("  CF2 effective K: SW %.4f -> %.4f | DW %.4f -> %.4f (subsidy=%.2f)\n",
    exp(theta_struct[["K_log_SW"]]), exp(theta_cf2[["K_log_SW"]]),
    exp(theta_struct[["K_log_DW"]]), exp(theta_cf2[["K_log_DW"]]), SUBSIDY_BASE))

eq_cf2 <- solve_equilibrium_6p_cf(theta_cf2, cache_baseline, cfg,
                                  max_iter = 500L, tol = 1e-7)
stopifnot(eq_cf2$converged)
P_cf2 <- eq_cf2$P; V_cf2 <- eq_cf2$V
stopifnot(all(abs(rowSums(P_cf2) - 1) < 1e-6), all(is.finite(V_cf2)))
cat(sprintf("  CF2: converged=%s | P_R range [%.4f, %.4f]\n",
    eq_cf2$converged, min(P_cf2[, "replace"]), max(P_cf2[, "replace"])))

# ==============================================================================
# SECTION 11 — CF3: PIGOUVIAN (one equilibrium per E)
# ==============================================================================
cat("=== SECTION 11: CF3 PIGOUVIAN ===\n")

P_cf3 <- list(); V_cf3 <- list()
for (i in seq_along(E_GRID)) {
  e_label <- names(E_GRID)[i]
  E_val   <- E_GRID[[i]]
  offset  <- -h_vec * E_val            # penalty -h(s)*E with coefficient 1
  eq <- solve_equilibrium_6p_cf(theta_struct, cache_baseline, cfg,
                                max_iter = 500L, tol = 1e-7,
                                u_maintain_offset = offset)
  stopifnot(eq$converged)
  stopifnot(all(abs(rowSums(eq$P) - 1) < 1e-6), all(is.finite(eq$V)))
  P_cf3[[e_label]] <- eq$P; V_cf3[[e_label]] <- eq$V
  cat(sprintf("  CF3 [%s, E=$%d]: converged=%s | P_M range [%.4f, %.4f]\n",
      e_label, E_GRID_DOLLARS[[i]], eq$converged,
      min(eq$P[, "maintain"]), max(eq$P[, "maintain"])))
}

# ==============================================================================
# SECTION 12 — CF4: AGE MANDATE (no maintain for A_bin >= 6)
# ==============================================================================
cat("=== SECTION 12: CF4 AGE MANDATE ===\n")

# mandate_idx = s_idx of mandated cells (P rows are indexed 1..32 == s_idx)
mandate_idx <- sort(state_lut[A_bin >= MANDATE_AGE_BIN, s_idx])
# PRE-FLIGHT: A_bin in {6,7,8} x 2 wall x 2 regime = 12 cells (04j Sec 3 pattern)
cat(sprintf("  mandated cells: %d (A_bins %s)\n",
    length(mandate_idx),
    paste(sort(unique(state_lut[A_bin >= MANDATE_AGE_BIN, A_bin])), collapse = ",")))
stopifnot(length(mandate_idx) == 12L)
stopifnot(setequal(state_lut[A_bin >= MANDATE_AGE_BIN, A_bin], c(6L, 7L, 8L)))

eq_cf4 <- solve_equilibrium_6p_cf(theta_struct, cache_baseline, cfg,
                                  max_iter = 500L, tol = 1e-7,
                                  mandate_idx = mandate_idx)
stopifnot(eq_cf4$converged)
P_cf4 <- eq_cf4$P; V_cf4 <- eq_cf4$V
stopifnot(all(abs(rowSums(P_cf4) - 1) < 1e-6), all(is.finite(V_cf4)))

# Mandate enforcement assertions
eps_p <- cfg$eps_prob
stopifnot(all(P_cf4[mandate_idx, "maintain"] <= eps_p * 1.01))
stopifnot(all(abs((P_cf4[mandate_idx, "exit"] + P_cf4[mandate_idx, "replace"]) - 1) < 1e-6))
cat(sprintf("  CF4: converged=%s | mandated P_M max=%.2e (<= eps_prob*1.01=%.2e) PASS\n",
    eq_cf4$converged, max(P_cf4[mandate_idx, "maintain"]), eps_p * 1.01))

# ==============================================================================
# SECTION 13 — WELFARE ACCOUNTING (04o Section 9; NPL_REFERENCE §5)
# ==============================================================================
cat("=== SECTION 13: WELFARE ACCOUNTING ===\n")

welfare_one <- function(P, V, E_val, is_cf2) {
  producer <- sum(mu * V) * SCALE_FACTOR
  external <- sum(mu * P[, "maintain"] * h_vec) * E_val * PV_FACTOR * SCALE_FACTOR
  govt     <- if (is_cf2)
                sum(mu * P[, "replace"] * K_base_vec) * SUBSIDY_BASE *
                  PV_FACTOR * SCALE_FACTOR
              else 0
  social   <- producer - external - govt
  c(ProducerSurplus_USD = producer, ExternalDamage_USD = external,
    GovtOutlay_USD = govt, SocialWelfare_USD = social)
}

scenarios <- c("baseline", "CF1_flatfee", "CF2_subsidy", "CF3_pigou", "CF4_mandate")
get_PV <- function(scen, e_label) {
  switch(scen,
    baseline    = list(P = P_baseline, V = V_baseline, is_cf2 = FALSE),
    CF1_flatfee = list(P = P_cf1,      V = V_cf1,      is_cf2 = FALSE),
    CF2_subsidy = list(P = P_cf2,      V = V_cf2,      is_cf2 = TRUE),
    CF3_pigou   = list(P = P_cf3[[e_label]], V = V_cf3[[e_label]], is_cf2 = FALSE),
    CF4_mandate = list(P = P_cf4,      V = V_cf4,      is_cf2 = FALSE),
    stop("unknown scenario"))
}

rows <- list()
for (scen in scenarios) {
  for (i in seq_along(E_GRID)) {
    e_label <- names(E_GRID)[i]
    E_val   <- E_GRID[[i]]
    E_dol   <- E_GRID_DOLLARS[[i]]
    pv <- get_PV(scen, e_label)
    w  <- welfare_one(pv$P, pv$V, E_val, pv$is_cf2)
    for (comp in names(w)) {
      rows[[length(rows) + 1L]] <- data.table(
        scenario = scen, component = comp, E_label = e_label,
        E_external_USD = as.numeric(E_dol), level_full = as.numeric(w[[comp]]))
    }
  }
}
welf_full <- rbindlist(rows)
stopifnot(nrow(welf_full) == 40L)

# delta = level - baseline level (same component + E_label); 0 for baseline rows
base_levels <- welf_full[scenario == "baseline",
                         .(component, E_label, base_level = level_full)]
welf_full <- merge(welf_full, base_levels, by = c("component", "E_label"),
                   all.x = TRUE)
welf_full[, delta_full := level_full - base_level]

# ==============================================================================
# SECTION 14 — VALIDATION
# ==============================================================================
cat("=== SECTION 14: VALIDATION ===\n")

# (a) Welfare identity (full precision) for all 5 scenarios x 2 E
for (scen in scenarios) {
  for (e_label in names(E_GRID)) {
    sub  <- welf_full[scenario == scen & E_label == e_label]
    getv <- function(cc) sub[component == cc, level_full]
    chk  <- abs(getv("SocialWelfare_USD") -
                (getv("ProducerSurplus_USD") - getv("ExternalDamage_USD") -
                 getv("GovtOutlay_USD")))
    stopifnot(chk <= 1)
  }
}
cat("  Welfare identity (5 scenarios x 2 E): PASS\n")

# (b) CF1 reproduction vs T017 archived 04o_CF_Welfare_Summary.csv (within $1)
arch <- fread(arch_path)
cf1_delta <- welf_full[scenario == "CF1_flatfee",
                       .(component, E_label, my_delta = round(delta_full))]
cmp <- merge(cf1_delta, arch[, .(component, E_label, arch_delta = delta_USD)],
             by = c("component", "E_label"))
stopifnot(nrow(cmp) == 8L)
cmp[, abs_diff := abs(my_delta - arch_delta)]
cat("  CF1 vs T017 delta cross-check:\n")
print(cmp[order(E_label, component)])
stopifnot(all(cmp$abs_diff <= 1))
cat("  CF1 reproduction vs T017: PASS (all 8 cells within $1)\n")

# (c) Headline table (4 CFs x 4 deltas at E = 50k) to log
hl <- welf_full[E_label == "HEALTH_PLUS_UNMEASURED" & scenario != "baseline"]
hl_wide <- dcast(hl, scenario ~ component, value.var = "delta_full")
hl_wide <- hl_wide[match(c("CF1_flatfee", "CF2_subsidy", "CF3_pigou",
                           "CF4_mandate"), scenario)]
cat("\n  HEADLINE deltas ($/facility PV, E = $50k):\n")
print(hl_wide[, .(scenario,
                  dProducerSurplus = round(ProducerSurplus_USD),
                  dExternalDamage  = round(ExternalDamage_USD),
                  dGovtOutlay      = round(GovtOutlay_USD),
                  dSocialWelfare   = round(SocialWelfare_USD))])
cat(sprintf("\n  CAVEAT: %s\n", CAVEAT_TXT))

# ==============================================================================
# SECTION 15 — STEP 1 DELIVERABLE: WELFARE SUMMARY CSV + TEX
# ==============================================================================
cat("=== SECTION 15: WELFARE SUMMARY CSV + TEX ===\n")

out_csv <- welf_full[, .(scenario, component, E_label,
                         E_external_USD = as.numeric(E_external_USD),
                         level_USD = as.numeric(round(level_full)),
                         delta_USD = as.numeric(round(delta_full)))]
setorder(out_csv, scenario, E_external_USD, component)
stopifnot(nrow(out_csv) == 40L)
path_csv <- here::here("Output", "Tables", "04r_CF234_Welfare_Summary.csv")
fwrite(out_csv, path_csv)
cat(sprintf("  Saved CSV (40 rows): %s\n", path_csv))

# TeX: rows = 4 CFs; cols = dProducerSurplus, dExternalDamage, dGovtOutlay,
# dSocialWelfare (E = 50k headline). booktabs + \textit{Notes:} caveat line.
cf_pretty <- c(CF1_flatfee = "CF1: TX flat-fee",
               CF2_subsidy = "CF2: Replace subsidy",
               CF3_pigou   = "CF3: Pigouvian",
               CF4_mandate = "CF4: Age mandate")
fmt_d <- function(x) formatC(round(x), format = "d", big.mark = ",")
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Counterfactual welfare deltas (6p gammafree fit)}",
  "\\label{tab:cf234_welfare}",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  paste0("Counterfactual & $\\Delta$Firm surplus & $\\Delta$External damage & ",
         "$\\Delta$Govt outlay & $\\Delta$Social welfare \\\\"),
  "\\midrule")
for (s in c("CF1_flatfee", "CF2_subsidy", "CF3_pigou", "CF4_mandate")) {
  r <- hl_wide[scenario == s]
  tex_lines <- c(tex_lines,
    sprintf("%s & %s & %s & %s & %s \\\\",
      cf_pretty[[s]],
      fmt_d(r$ProducerSurplus_USD), fmt_d(r$ExternalDamage_USD),
      fmt_d(r$GovtOutlay_USD),      fmt_d(r$SocialWelfare_USD)))
}
tex_lines <- c(tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  sprintf(paste0("\\\\[4pt] \\textit{Notes:} All values $/facility present value, ",
                 "$E_{\\mathrm{ext}}=\\$50{,}000$, $\\beta=%.2f$, TX-empirical state ",
                 "distribution. %s"), BETA, CAVEAT_TXT),
  "\\end{table}")
path_tex <- here::here("Output", "Tables", "04r_CF234_Welfare_Summary.tex")
writeLines(tex_lines, path_tex)
cat(sprintf("  Saved TeX: %s\n", path_tex))

# ==============================================================================
# SECTION 16 — STEP 2 DELIVERABLE: WELFARE BAR CHART (PNG + PDF)
# ==============================================================================
cat("=== SECTION 16: WELFARE BAR CHART ===\n")

bar_dt <- copy(welf_full[E_label == "HEALTH_PLUS_UNMEASURED" & scenario != "baseline"])
bar_dt[, comp_label := fcase(
  component == "ProducerSurplus_USD", "Firm surplus",
  component == "ExternalDamage_USD",  "External damage",
  component == "GovtOutlay_USD",      "Govt outlay",
  component == "SocialWelfare_USD",   "Social welfare")]
bar_dt[, cf_label := fcase(
  scenario == "CF1_flatfee", "CF1",
  scenario == "CF2_subsidy", "CF2",
  scenario == "CF3_pigou",   "CF3",
  scenario == "CF4_mandate", "CF4")]
comp_levels <- c("Firm surplus", "External damage", "Govt outlay", "Social welfare")
bar_dt[, comp_label := factor(comp_label, levels = comp_levels)]
bar_dt[, cf_label   := factor(cf_label,   levels = c("CF1", "CF2", "CF3", "CF4"))]

pal <- c("Firm surplus" = "#003262", "External damage" = "#3B7A57",
         "Govt outlay" = "#FDB515", "Social welfare" = "#8B1A1A")

p_bar <- ggplot(bar_dt, aes(x = cf_label, y = delta_full, fill = comp_label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.6) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title    = "Counterfactual welfare deltas vs baseline (6p gammafree fit)",
    subtitle = "Change in welfare component, $/facility present value, E = $50k",
    x = NULL, y = expression(Delta~"vs baseline ($/facility PV)"),
    fill = "Component",
    caption = paste0(strwrap(CAVEAT_TXT, width = 110), collapse = "\n")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title   = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 8, color = "gray35"))

path_png <- here::here("Output", "Figures", "04r_CF234_Welfare_BarChart.png")
path_pdf <- here::here("Output", "Figures", "04r_CF234_Welfare_BarChart.pdf")
ggsave(path_png, p_bar, width = 10, height = 6, dpi = 150)
ggsave(path_pdf, p_bar, width = 10, height = 6)
cat(sprintf("  Saved figure: %s\n  Saved figure: %s\n", path_png, path_pdf))

# ==============================================================================
# SECTION 17 — SAVE RESULTS RDS
# ==============================================================================
cat("=== SECTION 17: SAVE RESULTS RDS ===\n")

cf_results <- list(
  beta = BETA, theta_struct = theta_struct,
  mu = mu, h_vec = h_vec, K_base_vec = K_base_vec,
  P_baseline = P_baseline, V_baseline = V_baseline,
  P_cf1 = P_cf1, V_cf1 = V_cf1,
  P_cf2 = P_cf2, V_cf2 = V_cf2,
  P_cf3 = P_cf3, V_cf3 = V_cf3,
  P_cf4 = P_cf4, V_cf4 = V_cf4,
  mandate_idx = mandate_idx,
  welfare_full = welf_full, welfare_summary = out_csv)
path_rds <- here::here("Output", "Estimation_Results", "CF234_Welfare_6p_results.rds")
saveRDS(cf_results, path_rds)
stopifnot(file.exists(path_rds))
cat(sprintf("  Saved: %s\n", path_rds))

cat("\n=== 04r_CF234_Welfare_6p COMPLETE ===\n")
cat(sprintf("  Summary CSV: %s\n", path_csv))
cat(sprintf("  Summary TeX: %s\n", path_tex))
cat(sprintf("  Bar chart:   %s (+ .pdf)\n", path_png))
cat(sprintf("  Results RDS: %s\n", path_rds))
