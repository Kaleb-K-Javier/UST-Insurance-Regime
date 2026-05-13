# ==============================================================================
# 04i_8param_FE_and_Welfare.R
# ==============================================================================
cat("04i: 8-param + 17 control-state FE (maintain-only) + 03-style SOCIAL welfare A/B/C/D\n")
# ==============================================================================
# Two atomic deliverables:
#
#   (1) Refit the 8-param replacement DCM on the observed sample with
#       17 control-state fixed effects in u_M (maintain utility only).
#       TX FE is fixed at 0 (no estimated TX parameter).
#       FE enters ONLY the maintain utility — NOT equilibrium inversion.
#       Compute AM-2002 profile-likelihood SEs over all 25 params.
#
#   (2) 03-style SOCIAL welfare counterfactuals (QSD-weighted, PV external
#       damages P_M * h * E):
#         A: TX stays under FF (swap RB gammas to FF gammas)
#         B: Pigouvian — internalise E via mFF/mRB scaling of gamma_risk
#         C: Retrofit subsidy (K_SW * 0.5)
#         D: Replacement tax (K_SW * 1.5, K_DW * 1.5)
#       Each: re-solve equilibrium with structural params only (FEs do not
#       enter the equilibrium); compare V(s), action shares, social welfare.
# ==============================================================================

.PARENT_RENV_LIB <- "C:/Users/kaleb/Documents/ust_ins_move_to_github/renv/library/windows/R-4.5/x86_64-w64-mingw32"
if (dir.exists(.PARENT_RENV_LIB) && !(.PARENT_RENV_LIB %in% .libPaths())) {
  .libPaths(c(.PARENT_RENV_LIB, .libPaths()))
}
suppressPackageStartupMessages({
  library(data.table); library(Matrix); library(here)
  library(ggplot2);    library(gridExtra)
})
source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))

OUT_FIT  <- here::here("Output", "Estimation_Results")
OUT_TAB  <- here::here("Output", "Tables")
OUT_FIG  <- here::here("Output", "Figures")
OUTTAB   <- OUT_TAB   # alias used in welfare section
OUTFIG   <- OUT_FIG   # alias used in welfare section
dir.create(OUT_FIT, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)
SCALE_FACTOR <- 10000
PARENT <- "C:/Users/kaleb/Documents/ust_ins_move_to_github"
parent_in <- function(...) {
  l <- here::here(...); p <- file.path(PARENT, ...)
  if (file.exists(l)) return(l); if (file.exists(p)) return(p)
  stop("not found: ", ...)
}

cat("======================================================================\n")
cat("04i: 8-param + 17 control-state FE + 03-style SOCIAL welfare A/B/C/D\n")
cat("======================================================================\n")

# ------------------------------------------------------------------------------
# C1: FE state lists (self-contained; authoritative for this script)
# ------------------------------------------------------------------------------
CONTROLSTATES <- c(
  "AR", "CO", "ID", "KS", "KY",
  "LA", "MA", "MD", "ME", "MN", "MO", "NC",
  "OH", "OK", "SD", "TN", "VA"
)
STUDYSTATES    <- c("TX", CONTROLSTATES)
FEPARAMNAMES   <- paste0("alpha", CONTROLSTATES)   # alphaAR..alphaVA
NFEPARAMS      <- length(FEPARAMNAMES)              # 17


# -- 1. Load primitives + observed obs panel + 8p (no-FE) fit ----------------
prims_obs <- readRDS(parent_in("Output", "Estimation_Results",
                               "DCM_Primitives_Replacement_observed.rds"))
obs_obs   <- fread(parent_in("Data", "Analysis",
                             "dcm_obs_panel_observed.csv"))
fit_8p    <- readRDS(file.path(OUT_FIT,
                               "Model_Replacement_8param_observed.rds"))


# -- 2. Fit 8p + FE (all 17 controls, maintain-only) -------------------------
# C2: config
config_8p_fe <- create_estimation_config_replacement_8p_fe(
  beta               = 0.95,
  sigma2             = 1.0,
  npl_iter           = 200,
  feweightsource     = "controls",
  ccp_damping_lambda = 0.6,
  epsprob            = 1e-12,
  alphabounds        = c(-20, 20),
  tol_theta          = 1e-5,
  tol_P              = 1e-5
)

# C3: theta init — 8 structural from no-FE fit + 17 alphas initialised to 0
theta_init_fe <- c(fit_8p$theta_raw,
                   setNames(rep(0, NFEPARAMS), FEPARAMNAMES))

cat("\n[2] 8p + 17-control FE fit on OBSERVED sample\n")
# C4: run FE estimator (aggregated likelihood + geo-weighted CCP)
fit_fe <- npl_estimator_replacement_8p_fe(
  obs_panel    = obs_obs,
  primitives   = prims_obs,
  config_8p_fe = config_8p_fe,
  theta_init   = theta_init_fe,
  verbose      = TRUE
)
stopifnot(isTRUE(fit_fe$converged))
saveRDS(fit_fe, file.path(OUT_FIT, "Model_Replacement_8paramFE_observed.rds"))

# E1: validation assertions
stopifnot(NFEPARAMS == 17L)
stopifnot(length(fit_fe$theta_raw) == 25L)
stopifnot(all(FEPARAMNAMES %in% names(fit_fe$theta_raw)))
stopifnot(isTRUE(fit_fe$converged))
stopifnot(all(dim(fit_fe$cache$wsg8pfe) == c(32L, 18L)))
stopifnot(nrow(fit_fe$cache$countsdt8pfe) == 576L)
cat("\n[E1] Validation assertions passed.\n")


# -- 3. AM-2002 SEs via memoised profile-likelihood Hessian (25 params) ------
# C5: memoised profile objective — equilibrium re-used for same structural theta

# C5 memoised profile objective
profile_neg_loglik_8p_fe <- local({
  eq_cache <- new.env(parent = emptyenv())

  function(theta_raw, cache, config_8p_fe) {
    if (is.null(names(theta_raw))) names(theta_raw) <- config_8p_fe$param_names

    struct_names <- c("kappa_SW", "kappa_DW", "K_log_SW", "K_log_DW",
                      "gamma_price_FF", "gamma_price_RB",
                      "gamma_risk_FF",  "gamma_risk_RB")
    theta_struct <- theta_raw[struct_names]

    key <- paste(sprintf("%.10f", as.numeric(theta_struct)), collapse = "|")
    eq  <- eq_cache[[key]]
    if (is.null(eq)) {
      eq <- tryCatch(
        solve_equilibrium_policy_replacement_8p(theta_struct, cache,
                                               config_8p_fe,
                                               max_iter = 500, tol = 1e-7),
        error = function(e) NULL)
      eq_cache[[key]] <- eq
    }
    if (is.null(eq) || !isTRUE(eq$converged)) return(1e10)

    npl_likelihood_replacement_8p_fe(theta_raw, eq$P, cache, config_8p_fe)
  }
})

finite_diff_hessian <- function(fn, x, h = 1e-3, ...) {
  k  <- length(x); H <- matrix(NA_real_, k, k)
  f0 <- fn(x, ...); if (!is.finite(f0)) return(H)
  fp <- numeric(k); fm <- numeric(k)
  for (i in seq_len(k)) {
    xp <- x; xp[i] <- xp[i] + h; fp[i] <- fn(xp, ...)
    xm <- x; xm[i] <- xm[i] - h; fm[i] <- fn(xm, ...)
  }
  for (i in seq_len(k)) H[i, i] <- (fp[i] - 2 * f0 + fm[i]) / (h^2)
  for (i in seq_len(k - 1L)) for (j in seq.int(i + 1L, k)) {
    xpp <- x; xpp[i] <- xpp[i] + h; xpp[j] <- xpp[j] + h
    xpm <- x; xpm[i] <- xpm[i] + h; xpm[j] <- xpm[j] - h
    xmp <- x; xmp[i] <- xmp[i] - h; xmp[j] <- xmp[j] + h
    xmm <- x; xmm[i] <- xmm[i] - h; xmm[j] <- xmm[j] - h
    fij <- (fn(xpp,...) - fn(xpm,...) - fn(xmp,...) + fn(xmm,...)) / (4 * h^2)
    H[i, j] <- fij; H[j, i] <- fij
  }
  H
}

cat("\n[3] AM-2002 profile-likelihood Hessian (25 params)\n")
t0 <- Sys.time()
H  <- finite_diff_hessian(profile_neg_loglik_8p_fe, fit_fe$theta_raw, h = 1e-3,
                           cache = fit_fe$cache, config_8p_fe = fit_fe$config)
cat(sprintf("    elapsed %.1f sec\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))

n_p <- length(fit_fe$theta_raw)
SE  <- if (anyNA(H)) rep(NA_real_, n_p) else {
  Vinv <- tryCatch(solve(H), error = function(e) NULL)
  if (is.null(Vinv)) rep(NA_real_, n_p)
  else { dv <- diag(Vinv); sqrt(ifelse(dv > 0, dv, NA_real_)) }
}


# -- 4. Build SE table + LaTeX (25 params) ------------------------------------
hat <- fit_fe$theta_hat   # natural-scale: K_SW, K_DW on level scale
th  <- fit_fe$theta_raw
z   <- qnorm(0.975)

se_tab <- data.table(
  parameter = names(th),
  estimate  = as.numeric(th),
  SE_raw    = SE
)
# Delta-method K_log -> K for positions 3, 4
se_tab[parameter == "K_log_SW", `:=`(
  estimate = hat[["K_SW"]],
  SE_raw   = if (!is.na(SE[3])) hat[["K_SW"]] * SE[3] else NA_real_)]
se_tab[parameter == "K_log_DW", `:=`(
  estimate = hat[["K_DW"]],
  SE_raw   = if (!is.na(SE[4])) hat[["K_DW"]] * SE[4] else NA_real_)]
se_tab[parameter %in% c("K_log_SW","K_log_DW"),
       parameter := sub("K_log_", "K_", parameter)]

se_tab[, CI_lo := estimate - z * SE_raw]
se_tab[, CI_hi := estimate + z * SE_raw]
print(se_tab)
fwrite(se_tab, file.path(OUT_TAB, "04i_Theta_Table_8paramFE_AM_SE.csv"))

# C6: FE table export (required)
hat_raw <- fit_fe$theta_raw
fe_table <- data.table(
  state = CONTROLSTATES,
  param = FEPARAMNAMES,
  alpha = sapply(CONTROLSTATES, function(s) hat_raw[[paste0("alpha", s)]])
)
fwrite(fe_table, file.path(OUTTAB, "04iFETableAllControls.csv"))
cat("\n[4] FE table written.\n")
print(fe_table)

# LaTeX
fmt_d  <- function(x) formatC(round(x), format = "d", big.mark = ",")
push_d <- function(label, est, se, lo, hi)
  sprintf("%s & \\$%s & (\\$%s) & [\\$%s, \\$%s] \\\\",
          label, fmt_d(est), fmt_d(se), fmt_d(lo), fmt_d(hi))
push_n <- function(label, est, se, lo, hi)
  sprintf("%s & %.3f & (%.3f) & [%.3f, %.3f] \\\\", label, est, se, lo, hi)

to_dollar <- function(r) list(
  est = r$estimate * SCALE_FACTOR, se = r$SE_raw * SCALE_FACTOR,
  lo  = r$CI_lo    * SCALE_FACTOR, hi = r$CI_hi  * SCALE_FACTOR)

tex <- c(
  "% Auto-generated by 04i_8param_FE_and_Welfare.R",
  "% AM (2002) profile-likelihood SEs in (parens); 95% Wald CIs in [brackets].",
  "% TX FE fixed at 0 (no estimated TX parameter). 17 control-state alphas estimated.",
  "\\begin{tabular}{lccc}", "\\hline",
  "Parameter & Estimate & (SE) & 95\\% CI \\\\", "\\hline"
)
for (lab in c("kappa_SW","kappa_DW","K_SW","K_DW")) {
  r <- se_tab[parameter == lab][1L]
  d <- to_dollar(r)
  pretty <- switch(lab,
    "kappa_SW" = "$\\kappa_{\\mathrm{SW}}$",
    "kappa_DW" = "$\\kappa_{\\mathrm{DW}}$",
    "K_SW"     = "$K_{\\mathrm{SW}}$",
    "K_DW"     = "$K_{\\mathrm{DW}}$")
  tex <- c(tex, push_d(pretty, d$est, d$se, d$lo, d$hi))
}
for (lab in c("gamma_price_FF","gamma_price_RB","gamma_risk_FF","gamma_risk_RB")) {
  r      <- se_tab[parameter == lab][1L]
  pretty <- switch(lab,
    "gamma_price_FF" = "$\\gamma_{\\mathrm{price,FF}}$",
    "gamma_price_RB" = "$\\gamma_{\\mathrm{price,RB}}$",
    "gamma_risk_FF"  = "$\\gamma_{\\mathrm{risk,FF}}$",
    "gamma_risk_RB"  = "$\\gamma_{\\mathrm{risk,RB}}$")
  tex <- c(tex, push_n(pretty, r$estimate, r$SE_raw, r$CI_lo, r$CI_hi))
}
for (lab in FEPARAMNAMES) {
  r      <- se_tab[parameter == lab][1L]
  state  <- sub("^alpha", "", lab)
  pretty <- sprintf("$\\alpha_{\\mathrm{%s}}$", state)
  tex    <- c(tex, push_n(pretty, r$estimate, r$SE_raw, r$CI_lo, r$CI_hi))
}
tex <- c(tex,
  "\\hline",
  sprintf("$\\log L$ & \\multicolumn{3}{c}{%s} \\\\",
          formatC(round(fit_fe$log_likelihood), format = "d", big.mark = ",")),
  sprintf("$N$ obs  & \\multicolumn{3}{c}{%s} \\\\",
          formatC(fit_fe$cache$n_obs %||% nrow(obs_obs),
                  format = "d", big.mark = ",")),
  "\\hline",
  "\\multicolumn{4}{l}{\\footnotesize Observed sample. TX FE fixed at 0.",
  "  17 control-state maintain-shifters estimated.}",
  "\\end{tabular}")
writeLines(tex, file.path(OUT_TAB, "04i_Theta_Table_8paramFE_AM_SE.tex"))


# -- 5. Welfare counterfactuals ----------------------------------------------
cat("\n[5] Welfare counterfactuals A/B/C/D\n")
config_8p   <- fit_fe$config
cache       <- fit_fe$cache
state_lut   <- prims_obs$state_lut

cell_n <- obs_obs[, .(n_cell = .N), by = s_idx][order(s_idx)]
all_s  <- data.table(s_idx = 1:32)
cell_n <- merge(all_s, cell_n, by = "s_idx", all.x = TRUE)
cell_n[is.na(n_cell), n_cell := 0L]
n_cell_vec <- cell_n$n_cell

# Baseline structural equilibrium (structural params only; FEs are nuisance)
theta_struct <- fit_fe$theta_raw[c("kappa_SW","kappa_DW","K_log_SW","K_log_DW",
                                   "gamma_price_FF","gamma_price_RB",
                                   "gamma_risk_FF","gamma_risk_RB")]
eq_base <- solve_equilibrium_policy_replacement_8p(theta_struct, cache, config_8p,
                                                   max_iter = 500, tol = 1e-8)

solve_cf <- function(modify_fn, label) {
  th_cf <- modify_fn(theta_struct)
  eq    <- solve_equilibrium_policy_replacement_8p(th_cf, cache, config_8p,
                                                   max_iter = 500, tol = 1e-8)
  list(label = label, theta = th_cf, eq = eq)
}

cf_A <- solve_cf(function(th) {
  th[["gamma_price_RB"]] <- th[["gamma_price_FF"]]
  th[["gamma_risk_RB"]]  <- th[["gamma_risk_FF"]]
  th
}, "A_TX_stays_FF")

# D0: constant E definition
Lvec  <- prims_obs$L_vec
hvec  <- prims_obs$h_vec

E_per_leak_event_dollars <- 0        # TODO: Marcus-based calibration later
E_per_leak_event         <- E_per_leak_event_dollars / 10000  # model unit = $10k
Evec                     <- rep(E_per_leak_event, length(Lvec))
pv_factor                <- 1 / (1 - config_8p$beta)

# D2: Pigouvian CF B — internalise E via mFF/mRB scaling of gamma_risk
FM <- cache$F_maintain %||% cache$FM
FR <- cache$F_replace  %||% cache$FR
stopifnot(!is.null(FM), !is.null(FR))

pop_weights <- prims_obs$ncell_vec %||% (n_cell_vec / sum(n_cell_vec))
if (sum(pop_weights) > 0) pop_weights <- pop_weights / sum(pop_weights)

# D1: QSD computation by wall x regime subchains
chain_id     <- paste0("w", state_lut$w_state, "rho", state_lut$rho_state)
chain_labels <- unique(chain_id)

compute_weighted_qsd_replacement <- function(P, FM, FR, chain_id, pop_weights,
                                             maxiter = 5000, tol = 1e-12) {
  S      <- nrow(P)
  pM     <- P[, "maintain"]
  pR     <- P[, "replace"]
  mu_full <- rep(0.0, S)

  for (cl in chain_labels) {
    idx         <- which(chain_id == cl)
    if (length(idx) == 0L) next
    chain_share <- sum(pop_weights[idx])
    if (chain_share < 1e-15) next

    Tsub <- as.matrix(
      Matrix::Diagonal(length(idx), x = pM[idx]) %*% FM[idx, idx] +
      Matrix::Diagonal(length(idx), x = pR[idx]) %*% FR[idx, idx]
    )
    mu_sub <- pop_weights[idx] / sum(pop_weights[idx])

    for (it in seq_len(maxiter)) {
      mu_new <- as.numeric(mu_sub %*% Tsub)
      s_tot  <- sum(mu_new)
      if (s_tot < 1e-15) { mu_sub <- rep(1 / length(idx), length(idx)); break }
      mu_new <- mu_new / s_tot
      if (max(abs(mu_new - mu_sub)) < tol) { mu_sub <- mu_new; break }
      mu_sub <- mu_new
    }
    mu_full[idx] <- mu_sub * chain_share
  }
  mu_full / sum(mu_full)
}

# Compute baseline QSD for Pigouvian scaling
mu0  <- compute_weighted_qsd_replacement(eq_base$P, FM, FR, chain_id, pop_weights)
PM0  <- eq_base$P[, "maintain"]
is_FF <- state_lut$rho_state == 1L
is_RB <- state_lut$rho_state == 0L %||% (state_lut$rho_state == 2L)
# safer: is_RB is complement of is_FF
is_RB <- !is_FF

den_FF <- sum(mu0[is_FF] * PM0[is_FF] * hvec[is_FF] * Lvec[is_FF])
num_FF <- sum(mu0[is_FF] * PM0[is_FF] * hvec[is_FF] * (Lvec[is_FF] + Evec[is_FF]))
m_FF   <- ifelse(den_FF > 0, num_FF / den_FF, 1.0)

den_RB <- sum(mu0[is_RB] * PM0[is_RB] * hvec[is_RB] * Lvec[is_RB])
num_RB <- sum(mu0[is_RB] * PM0[is_RB] * hvec[is_RB] * (Lvec[is_RB] + Evec[is_RB]))
m_RB   <- ifelse(den_RB > 0, num_RB / den_RB, 1.0)

cf_B <- solve_cf(function(th) {
  th[["gamma_risk_FF"]] <- th[["gamma_risk_FF"]] * m_FF
  th[["gamma_risk_RB"]] <- th[["gamma_risk_RB"]] * m_RB
  th
}, "B_Pigouvian_internalizeE")

cf_C <- solve_cf(function(th) {
  th[["K_log_SW"]] <- log(exp(th[["K_log_SW"]]) * 0.5)
  th
}, "C_KSW_subsidy_50pct")

cf_D <- solve_cf(function(th) {
  th[["K_log_SW"]] <- log(exp(th[["K_log_SW"]]) * 1.5)
  th[["K_log_DW"]] <- log(exp(th[["K_log_DW"]]) * 1.5)
  th
}, "D_K_tax_50pct")


# D1: Social welfare function
theta_ref_real <- fit_8p$theta_raw   # baseline valuation reference (no-FE fit)

compute_social_welfare_8p <- function(eq_obj, label, cache, config_8p) {
  Ppol       <- eq_obj$P
  V_perceived <- eq_obj$V
  mu         <- compute_weighted_qsd_replacement(Ppol, FM, FR, chain_id, pop_weights)

  U_real     <- flow_utilities_replacement_8p(theta_ref_real, cache)
  V_real     <- invert_value_function_replacement(Ppol, U_real, cache, config_8p)

  firm_perceived     <- sum(mu * V_perceived)
  firm_real          <- sum(mu * V_real)

  exp_private_loss_flow <- sum(mu * Ppol[, "maintain"] * hvec * Lvec)
  exp_external_flow     <- sum(mu * Ppol[, "maintain"] * hvec * Evec)

  social_welfare      <- firm_real - exp_external_flow * pv_factor
  govt_cost_implied   <- firm_perceived - firm_real

  list(
    tablerow = data.table(
      Scenario             = label,
      Converged            = isTRUE(eq_obj$converged),
      AvgP_M               = sum(mu * Ppol[, "maintain"]),
      AvgP_E               = sum(mu * Ppol[, "exit"]),
      AvgP_R               = sum(mu * Ppol[, "replace"]),
      ExpPrivateLossFlow   = exp_private_loss_flow,
      ExpExternalDamFlow   = exp_external_flow,
      FirmSurplusPerceived = firm_perceived,
      FirmSurplusReal      = firm_real,
      SocialWelfare        = social_welfare,
      GovtCostImplied      = govt_cost_implied
    ),
    mu = mu
  )
}

# D3: compute welfare for all scenarios
ws_base <- compute_social_welfare_8p(eq_base,    "BASELINE",           fit_fe$cache, config_8p)
ws_A    <- compute_social_welfare_8p(cf_A$eq,    cf_A$label,           fit_fe$cache, config_8p)
ws_B    <- compute_social_welfare_8p(cf_B$eq,    cf_B$label,           fit_fe$cache, config_8p)
ws_C    <- compute_social_welfare_8p(cf_C$eq,    cf_C$label,           fit_fe$cache, config_8p)
ws_D    <- compute_social_welfare_8p(cf_D$eq,    cf_D$label,           fit_fe$cache, config_8p)

welfare_social <- rbindlist(list(ws_base$tablerow, ws_A$tablerow,
                                 ws_B$tablerow, ws_C$tablerow, ws_D$tablerow),
                            fill = TRUE)
fwrite(welfare_social, file.path(OUTTAB, "04iWelfareSocial03Style.csv"))

mu_dt  <- function(mu, label) data.table(Scenario = label, sidx = seq_along(mu), mu = mu)
mu_all <- rbindlist(list(
  mu_dt(ws_base$mu, "BASELINE"),
  mu_dt(ws_A$mu,    cf_A$label),
  mu_dt(ws_B$mu,    cf_B$label),
  mu_dt(ws_C$mu,    cf_C$label),
  mu_dt(ws_D$mu,    cf_D$label)
))
fwrite(mu_all, file.path(OUTTAB, "04iQSDmubyScenario.csv"))

# Decomposition
base_row <- welfare_social[Scenario == "BASELINE"][1L]
dec      <- welfare_social[Scenario != "BASELINE"]
dec[, dFirmReal      := FirmSurplusReal  - base_row$FirmSurplusReal]
dec[, dExternalPV    := (ExpExternalDamFlow - base_row$ExpExternalDamFlow) * pv_factor]
dec[, dSocialWelfare := SocialWelfare    - base_row$SocialWelfare]
fwrite(dec, file.path(OUTTAB, "04iWelfareSocialDecomp.csv"))

dec_long <- melt(dec, id.vars = "Scenario",
                 measure.vars = c("dFirmReal", "dExternalPV"),
                 variable.name = "component", value.name = "value")
dec_long[, component := factor(component,
  levels = c("dFirmReal", "dExternalPV"),
  labels = c("\u0394 Firm surplus (real)", "\u0394 External damages (PV)"))]
dec_long[component == "\u0394 External damages (PV)", value := -value]

p_sw <- ggplot(dec_long, aes(x = Scenario, y = value, fill = component)) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_col(position = "stack") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(title    = "Social welfare decomposition vs baseline",
       subtitle = paste0("\u0394SW = \u0394 firm (real) \u2212 \u0394 external damages (PV);",
                         " external flow = PM\u00b7h\u00b7E"),
       x = NULL, y = "Change (model units)", fill = NULL)
ggsave(file.path(OUTFIG, "04iWelfareSocialDecomposition.png"),
       p_sw, width = 11, height = 6)


# -- 6. Per-cell welfare table (population-weighted, existing format) ---------
build_cell_welfare <- function(eq_base, eq_cf, label) {
  data.table(
    s_idx      = 1:32,
    A_bin      = state_lut$A_bin,
    w_state    = state_lut$w_state,
    rho_state  = state_lut$rho_state,
    n_cell     = n_cell_vec,
    V_baseline = round(eq_base$V, 4),
    V_cf       = round(eq_cf$V,   4),
    delta_V    = round(eq_cf$V - eq_base$V, 4),
    P_M_base   = round(eq_base$P[, "maintain"], 5),
    P_M_cf     = round(eq_cf$P[, "maintain"],   5),
    P_E_base   = round(eq_base$P[, "exit"],     5),
    P_E_cf     = round(eq_cf$P[, "exit"],       5),
    P_R_base   = round(eq_base$P[, "replace"],  5),
    P_R_cf     = round(eq_cf$P[, "replace"],    5),
    counterfactual = label
  )
}
welfare_long <- rbindlist(list(
  build_cell_welfare(eq_base, cf_A$eq, cf_A$label),
  build_cell_welfare(eq_base, cf_B$eq, cf_B$label),
  build_cell_welfare(eq_base, cf_C$eq, cf_C$label),
  build_cell_welfare(eq_base, cf_D$eq, cf_D$label)
))
fwrite(welfare_long, file.path(OUT_TAB, "04i_Welfare_PerCell.csv"))

agg <- welfare_long[, .(
  total_n           = sum(n_cell),
  pop_weighted_dV   = sum(n_cell * delta_V)   / sum(n_cell),
  pop_weighted_dP_M = sum(n_cell * (P_M_cf - P_M_base)) / sum(n_cell),
  pop_weighted_dP_E = sum(n_cell * (P_E_cf - P_E_base)) / sum(n_cell),
  pop_weighted_dP_R = sum(n_cell * (P_R_cf - P_R_base)) / sum(n_cell)
), by = counterfactual]

welfare_long[, hL := prims_obs$h_vec[s_idx] * prims_obs$L_vec[s_idx]]
leak_dt <- welfare_long[, .(
  d_leak_cost_per_facyear = sum(n_cell * (P_M_cf - P_M_base) * hL) / sum(n_cell),
  baseline_leak_cost      = sum(n_cell * P_M_base * hL) / sum(n_cell)
), by = counterfactual]
agg <- merge(agg, leak_dt, by = "counterfactual")
print(agg)
fwrite(agg, file.path(OUT_TAB, "04i_Welfare_Aggregate.csv"))

# Welfare summary LaTeX (per-cell format, kept for back-compat)
tex_w <- c(
  "% Welfare counterfactual aggregates (population-weighted by observed n_cell).",
  "% Delta V in model units (1 unit = $10K). Leak cost in same units.",
  "\\begin{tabular}{lccccc}", "\\hline",
  "Counterfactual & $\\Delta V$ & $\\Delta P_M$ & $\\Delta P_E$ & $\\Delta P_R$ & $\\Delta$ leak/facyr \\\\",
  "\\hline"
)
labels_pretty <- c(
  "A_TX_stays_FF"           = "A: TX stays FF",
  "B_Pigouvian_internalizeE" = "B: Pigouvian (internalise $E$)",
  "C_KSW_subsidy_50pct"     = "C: $K_{SW}$ $-50\\%$",
  "D_K_tax_50pct"           = "D: $K$ $+50\\%$"
)
for (lab in c("A_TX_stays_FF","B_Pigouvian_internalizeE",
              "C_KSW_subsidy_50pct","D_K_tax_50pct")) {
  r <- agg[counterfactual == lab][1L]
  tex_w <- c(tex_w,
    sprintf("%s & %+.3f & %+.4f & %+.4f & %+.4f & %+.4f \\\\",
            labels_pretty[lab],
            r$pop_weighted_dV, r$pop_weighted_dP_M,
            r$pop_weighted_dP_E, r$pop_weighted_dP_R,
            r$d_leak_cost_per_facyear))
}
tex_w <- c(tex_w, "\\hline",
  "\\multicolumn{6}{l}{\\footnotesize 1 model unit = \\$10,000.",
  "  Welfare uses 8-param + 17 control-state FE fit; equilibrium re-solved per CF.}",
  "\\end{tabular}")
writeLines(tex_w, file.path(OUT_TAB, "04i_Welfare_Summary.tex"))


# -- 7. Welfare figure: delta-V by cell, faceted by counterfactual -----------
welfare_long[, regime   := fifelse(rho_state == 1L, "FF", "RB")]
welfare_long[, wall     := fifelse(w_state   == 1L, "SW", "DW")]
welfare_long[, cf_label := factor(counterfactual,
  levels = c("A_TX_stays_FF","B_Pigouvian_internalizeE",
             "C_KSW_subsidy_50pct","D_K_tax_50pct"),
  labels = c("A: TX stays FF","B: Pigouvian (internalise E)",
             "C: K_SW -50%","D: K +50%"))]

p_dV <- ggplot(welfare_long,
               aes(x = A_bin, y = delta_V,
                   color = interaction(regime, wall),
                   group = interaction(regime, wall))) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  facet_wrap(~ cf_label, scales = "free_y", nrow = 2) +
  scale_x_continuous(breaks = c(1, 4, 8),
                     labels = c("0-5","15-20","35+")) +
  theme_minimal(base_size = 11) +
  labs(title    = "Welfare counterfactuals: delta V(s) by cell (model units, $10K)",
       subtitle = "Solved at the 8-param + FE structural equilibrium",
       x = "age bin", y = "delta V (counterfactual - baseline)",
       color = "regime x wall")
ggsave(file.path(OUT_FIG, "04i_Welfare_DeltaV_byCell.png"),
       p_dV, width = 12, height = 7)


# -- 8. Social welfare decomposition figure (D3, required) -------------------
# Already saved above as 04iWelfareSocialDecomposition.png


# -- 9. Compare 8p vs 8p+FE (gamma sanity) -----------------------------------
cmp <- data.table(
  param = c("gamma_price_FF","gamma_price_RB",
            "gamma_risk_FF", "gamma_risk_RB",
            "kappa_SW","kappa_DW","K_SW","K_DW"),
  no_FE_estimate = c(fit_8p$theta_raw[["gamma_price_FF"]],
                     fit_8p$theta_raw[["gamma_price_RB"]],
                     fit_8p$theta_raw[["gamma_risk_FF"]],
                     fit_8p$theta_raw[["gamma_risk_RB"]],
                     fit_8p$theta_raw[["kappa_SW"]],
                     fit_8p$theta_raw[["kappa_DW"]],
                     fit_8p$theta_hat[["K_SW"]],
                     fit_8p$theta_hat[["K_DW"]]),
  with_FE_estimate = c(fit_fe$theta_raw[["gamma_price_FF"]],
                       fit_fe$theta_raw[["gamma_price_RB"]],
                       fit_fe$theta_raw[["gamma_risk_FF"]],
                       fit_fe$theta_raw[["gamma_risk_RB"]],
                       fit_fe$theta_raw[["kappa_SW"]],
                       fit_fe$theta_raw[["kappa_DW"]],
                       fit_fe$theta_hat[["K_SW"]],
                       fit_fe$theta_hat[["K_DW"]])
)
print(cmp)
fwrite(cmp, file.path(OUT_TAB, "04i_Compare_8p_vs_8pFE.csv"))


# -- 10. Final summary --------------------------------------------------------
cat("\n--- 04i SUMMARY ---\n")
cat("25-param theta_hat (observed, 8p + 17-control FE):\n")
print(round(fit_fe$theta_hat, 4))
cat(sprintf("\nLog L: %s  (8p alone: %s)\n",
    formatC(round(fit_fe$log_likelihood),  big.mark = ","),
    formatC(round(fit_8p$log_likelihood),  big.mark = ",")))
cat("\nSocial welfare aggregates (model units; 1 unit = $10K):\n")
print(welfare_social)
cat("\nWelfare (per-cell, pop-weighted) aggregates:\n")
print(agg)

cat("\nSaved:\n")
for (f in c(
  file.path(OUT_FIT, "Model_Replacement_8paramFE_observed.rds"),
  file.path(OUT_TAB, "04i_Theta_Table_8paramFE_AM_SE.csv"),
  file.path(OUT_TAB, "04i_Theta_Table_8paramFE_AM_SE.tex"),
  file.path(OUTTAB,  "04iFETableAllControls.csv"),
  file.path(OUT_TAB, "04i_Welfare_PerCell.csv"),
  file.path(OUT_TAB, "04i_Welfare_Aggregate.csv"),
  file.path(OUT_TAB, "04i_Welfare_Summary.tex"),
  file.path(OUT_TAB, "04i_Compare_8p_vs_8pFE.csv"),
  file.path(OUTTAB,  "04iWelfareSocial03Style.csv"),
  file.path(OUTTAB,  "04iWelfareSocialDecomp.csv"),
  file.path(OUTTAB,  "04iQSDmubyScenario.csv"),
  file.path(OUT_FIG, "04i_Welfare_DeltaV_byCell.png"),
  file.path(OUTFIG,  "04iWelfareSocialDecomposition.png")
)) cat("  ", f, "\n")

cat("\n04i complete.\n")
