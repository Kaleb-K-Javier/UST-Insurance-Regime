# -- 5. Welfare counterfactuals (Marcus 2021 E sensitivity) -------------------
cat("\n[5] Welfare counterfactuals A/B/C/D with E in {HEALTH_ONLY=$17K, HEALTH_PLUS_UNMEASURED=$50K}\n")
config_8p   <- fit_fe$config
cache       <- fit_fe$cache
state_lut   <- prims_obs$state_lut

cell_n <- obs_obs[, .(n_cell = .N), by = s_idx][order(s_idx)]
all_s  <- data.table(s_idx = 1:32)
cell_n <- merge(all_s, cell_n, by = "s_idx", all.x = TRUE)
cell_n[is.na(n_cell), n_cell := 0L]
n_cell_vec <- cell_n$n_cell

# Marcus 2021 SVI. HEALTH_ONLY = $17K (infant LBW + preterm, conservative).
# HEALTH_PLUS_UNMEASURED = $50K (~3x for adult/cancer/ecological/cognitive-
# development channels not measured by Marcus).
E_GRID_DOLLARS <- c(HEALTH_ONLY = 17000, HEALTH_PLUS_UNMEASURED = 50000)
E_GRID         <- E_GRID_DOLLARS / SCALE_FACTOR   # c(HEALTH_ONLY = 1.7, HEALTH_PLUS_UNMEASURED = 5.0)

# E-independent setup: baseline structural equilibrium + CFs A, C, D
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

cf_C <- solve_cf(function(th) {
  th[["K_log_SW"]] <- log(exp(th[["K_log_SW"]]) * 0.5)
  th
}, "C_KSW_subsidy_50pct")

cf_D <- solve_cf(function(th) {
  th[["K_log_SW"]] <- log(exp(th[["K_log_SW"]]) * 1.5)
  th[["K_log_DW"]] <- log(exp(th[["K_log_DW"]]) * 1.5)
  th
}, "D_K_tax_50pct")

# Shared inputs for QSD and welfare
Lvec      <- prims_obs$L_vec
hvec      <- prims_obs$h_vec
pv_factor <- 1 / (1 - config_8p$beta)
FM        <- cache$F_maintain %||% cache$FM
FR        <- cache$F_replace  %||% cache$FR
stopifnot(!is.null(FM), !is.null(FR))

pop_weights <- prims_obs$ncell_vec %||% (n_cell_vec / sum(n_cell_vec))
if (sum(pop_weights) > 0) pop_weights <- pop_weights / sum(pop_weights)

chain_id     <- paste0("w", state_lut$w_state, "rho", state_lut$rho_state)
chain_labels <- unique(chain_id)

compute_weighted_qsd_replacement <- function(P, FM, FR, chain_id, pop_weights,
                                             maxiter = 5000, tol = 1e-12) {
  S       <- nrow(P)
  pM      <- P[, "maintain"]
  pR      <- P[, "replace"]
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

# Baseline QSD + Pigouvian scaling helper (re-evaluated per E)
mu0   <- compute_weighted_qsd_replacement(eq_base$P, FM, FR, chain_id, pop_weights)
PM0   <- eq_base$P[, "maintain"]
is_FF <- state_lut$rho_state == 1L
is_RB <- !is_FF

pigouvian_m_factors <- function(E_scalar) {
  Evec_E <- rep(E_scalar, length(Lvec))
  den_FF <- sum(mu0[is_FF] * PM0[is_FF] * hvec[is_FF] * Lvec[is_FF])
  num_FF <- sum(mu0[is_FF] * PM0[is_FF] * hvec[is_FF] * (Lvec[is_FF] + Evec_E[is_FF]))
  m_FF   <- if (den_FF > 0) num_FF / den_FF else 1.0
  den_RB <- sum(mu0[is_RB] * PM0[is_RB] * hvec[is_RB] * Lvec[is_RB])
  num_RB <- sum(mu0[is_RB] * PM0[is_RB] * hvec[is_RB] * (Lvec[is_RB] + Evec_E[is_RB]))
  m_RB   <- if (den_RB > 0) num_RB / den_RB else 1.0
  list(m_FF = m_FF, m_RB = m_RB)
}

solve_cf_B_at_E <- function(E_scalar) {
  ms <- pigouvian_m_factors(E_scalar)
  solve_cf(function(th) {
    th[["gamma_risk_FF"]] <- th[["gamma_risk_FF"]] * ms$m_FF
    th[["gamma_risk_RB"]] <- th[["gamma_risk_RB"]] * ms$m_RB
    th
  }, "B_Pigouvian_internalizeE")
}

# Social welfare function: E enters via Evec (length-32 vector); pv_factor explicit.
theta_ref_real <- fit_8p$theta_raw   # baseline valuation reference (no-FE 8p fit)

compute_social_welfare_8p <- function(eq_obj, label, cache, config_8p,
                                      Evec, pv_factor) {
  Ppol        <- eq_obj$P
  V_perceived <- eq_obj$V
  mu          <- compute_weighted_qsd_replacement(Ppol, FM, FR, chain_id, pop_weights)

  U_real      <- flow_utilities_replacement_8p(theta_ref_real, cache)
  V_real      <- invert_value_function_replacement(Ppol, U_real, cache, config_8p)

  firm_perceived        <- sum(mu * V_perceived)
  firm_real             <- sum(mu * V_real)
  exp_private_loss_flow <- sum(mu * Ppol[, "maintain"] * hvec * Lvec)
  exp_external_flow     <- sum(mu * Ppol[, "maintain"] * hvec * Evec)
  social_welfare        <- firm_real - exp_external_flow * pv_factor
  govt_cost_implied     <- firm_perceived - firm_real

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

# Loop over E values: collect welfare rows, decomposition rows, QSD mu rows.
welfare_rows <- list()
decomp_rows  <- list()
qsd_rows     <- list()
cf_B_by_E    <- list()

for (i in seq_along(E_GRID)) {
  E_lbl <- names(E_GRID)[i]
  E_val <- as.numeric(E_GRID[i])
  E_dol <- as.numeric(E_GRID_DOLLARS[i])
  Evec  <- rep(E_val, length(Lvec))

  cf_B <- solve_cf_B_at_E(E_val)
  cf_B_by_E[[E_lbl]] <- cf_B

  ws_base <- compute_social_welfare_8p(eq_base, "BASELINE",  cache, config_8p, Evec, pv_factor)
  ws_A    <- compute_social_welfare_8p(cf_A$eq, cf_A$label, cache, config_8p, Evec, pv_factor)
  ws_B    <- compute_social_welfare_8p(cf_B$eq, cf_B$label, cache, config_8p, Evec, pv_factor)
  ws_C    <- compute_social_welfare_8p(cf_C$eq, cf_C$label, cache, config_8p, Evec, pv_factor)
  ws_D    <- compute_social_welfare_8p(cf_D$eq, cf_D$label, cache, config_8p, Evec, pv_factor)

  this_w <- rbindlist(list(ws_base$tablerow, ws_A$tablerow,
                           ws_B$tablerow, ws_C$tablerow, ws_D$tablerow))
  this_w[, E_label   := E_lbl]
  this_w[, E_dollars := E_dol]
  setcolorder(this_w, c("E_label", "E_dollars", "Scenario"))
  welfare_rows[[E_lbl]] <- this_w

  base_row <- this_w[Scenario == "BASELINE"][1L]
  this_d   <- this_w[Scenario != "BASELINE",
                     .(E_label, E_dollars, Scenario,
                       dFirmReal      = FirmSurplusReal    - base_row$FirmSurplusReal,
                       dExternalPV    = (ExpExternalDamFlow - base_row$ExpExternalDamFlow) * pv_factor,
                       dSocialWelfare = SocialWelfare      - base_row$SocialWelfare)]
  decomp_rows[[E_lbl]] <- this_d

  # QSD rows: B is E-specific (suffixed by dollar value); others are E-independent
  # (record once on the first E iteration only).
  qsd_rows[[paste0("B_", E_lbl)]] <- data.table(
    Scenario = sprintf("B_Pigouvian_internalizeE_E%dK", round(E_dol / 1000)),
    sidx     = seq_along(ws_B$mu),
    mu       = ws_B$mu
  )
  if (i == 1L) {
    qsd_rows[["base"]] <- data.table(Scenario = "BASELINE",  sidx = seq_along(ws_base$mu), mu = ws_base$mu)
    qsd_rows[["A"]]    <- data.table(Scenario = cf_A$label,  sidx = seq_along(ws_A$mu),    mu = ws_A$mu)
    qsd_rows[["C"]]    <- data.table(Scenario = cf_C$label,  sidx = seq_along(ws_C$mu),    mu = ws_C$mu)
    qsd_rows[["D"]]    <- data.table(Scenario = cf_D$label,  sidx = seq_along(ws_D$mu),    mu = ws_D$mu)
  }

  sw_B    <- this_w[Scenario == "B_Pigouvian_internalizeE", SocialWelfare]
  sw_base <- base_row$SocialWelfare
  if (sw_B < sw_base) {
    warning(sprintf(
      "Pigouvian CF B did not raise social welfare at E_label=%s: SW_B=%.6f < SW_base=%.6f",
      E_lbl, sw_B, sw_base))
  }
}

welfare_social <- rbindlist(welfare_rows)
fwrite(welfare_social, file.path(OUTTAB, "04iWelfareSocial03Style.csv"))

dec <- rbindlist(decomp_rows)
fwrite(dec, file.path(OUTTAB, "04iWelfareSocialDecomp.csv"))

mu_all <- rbindlist(qsd_rows)
fwrite(mu_all, file.path(OUTTAB, "04iQSDmubyScenario.csv"))

# Sanity assertion: CF B at HEALTH_ONLY differs from baseline ONLY in gamma_risk_FF/RB.
cf_B_health_only <- cf_B_by_E[["HEALTH_ONLY"]]
th_diff_idx      <- which(abs(cf_B_health_only$theta - theta_struct) > 1e-14)
expected_diff    <- c("gamma_risk_FF", "gamma_risk_RB")
stopifnot(setequal(names(theta_struct)[th_diff_idx], expected_diff))

# Decomposition figure, faceted by E_label (one panel per E).
dec_long <- melt(dec,
                 id.vars       = c("E_label", "E_dollars", "Scenario"),
                 measure.vars  = c("dFirmReal", "dExternalPV"),
                 variable.name = "component",
                 value.name    = "value")
dec_long[, component := factor(component,
  levels = c("dFirmReal", "dExternalPV"),
  labels = c("Δ Firm surplus (real)", "Δ External damages (PV)"))]
dec_long[component == "Δ External damages (PV)", value := -value]
dec_long[, E_label := factor(E_label,
  levels = c("HEALTH_ONLY", "HEALTH_PLUS_UNMEASURED"),
  labels = c("E = $17K (Health only)", "E = $50K (Health + unmeasured)"))]

p_sw <- ggplot(dec_long, aes(x = Scenario, y = value, fill = component)) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_col(position = "stack") +
  facet_wrap(~ E_label, nrow = 1) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(title    = "Social welfare decomposition vs baseline (Marcus E sensitivity)",
       subtitle = paste0("ΔSW = Δ firm (real) − Δ external damages (PV);",
                         " external flow = PM·h·E"),
       x = NULL, y = "Change (model units)", fill = NULL)
ggsave(file.path(OUTFIG, "04iWelfareSocialDecomposition.png"),
       p_sw, width = 13, height = 6)

