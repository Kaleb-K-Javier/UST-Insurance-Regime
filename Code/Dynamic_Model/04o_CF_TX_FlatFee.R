# ==============================================================================
# Code/Dynamic_Model/04o_CF_TX_FlatFee.R
# TICKET 007 Phase 4 — "TX never had RB pricing" counterfactual
#
# At the cleaned 6p+FE structural θ̂ (Phase 3), re-solve equilibrium under
# a counterfactual where TX keeps the FF premium schedule in 2006+.
#
# Inputs:  Model_Replacement_6paramFE_profile_clean_observed.rds  (Phase 3)
#          DCM_Primitives_Replacement_observed.rds                 (Phase 2)
#          dcm_obs_panel_observed.csv                              (Phase 2)
#
# Outputs: CF_TX_FlatFee_results.rds
#          04o_CF_TX_FF_premiums.csv
#          04o_CF_RemovalAge_Distribution.csv
#          04o_CF_ActionShares_byAge.csv + .png
#          04o_CF_RemovalAge_Distribution_TX.png
#          04o_CF_Welfare_Summary.csv + .tex
# ==============================================================================

# ==============================================================================
# SECTION 1 — LOGGING
# ==============================================================================
.log_path <- here::here("logs", paste0(
  "04o_CF_TX_FlatFee_",
  format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 04o_CF_TX_FlatFee.R\nR: %s\nWD: %s\n\n",
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
})
setDTthreads(0L)
set.seed(20260527L)

SCALE_FACTOR  <- 10000L
BETA          <- 0.95
PV_FACTOR     <- 1 / (1 - BETA)
# External damage grid: $50K (headline) and $17K (sensitivity) per Q8
E_GRID_DOLLARS <- c(HEALTH_PLUS_UNMEASURED = 50000, HEALTH_ONLY = 17000)
E_GRID         <- E_GRID_DOLLARS / SCALE_FACTOR

OUT_FIT <- here::here("Output", "Estimation_Results")
OUT_TAB <- here::here("Output", "Tables")
OUT_FIG <- here::here("Output", "Figures")
dir.create(OUT_FIT, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)

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

stopifnot(file.exists(fit_path), file.exists(prims_path), file.exists(obs_path))

fit        <- readRDS(fit_path)
primitives <- readRDS(prims_path)
obs_panel  <- fread(obs_path)

cat(sprintf("  fit: LL=%.3f | converged=%s | theta[6]:\n",
    fit$log_likelihood, fit$converged))
print(round(fit$theta_hat, 4))
stopifnot(fit$converged == TRUE, length(fit$theta_hat) == 6L)

# Structural params (no alphas: per NPL_REFERENCE §4 Semantic 2, alphas don't
# enter the equilibrium re-solve)
theta_struct <- fit$theta_hat

# ==============================================================================
# SECTION 5 — BUILD BASELINE CACHE AND SOLVE EQUILIBRIUM
# ==============================================================================
cat("=== SECTION 5: BASELINE EQUILIBRIUM ===\n")

# Build the standard 6p cache (same as Phase 3 internally)
cfg <- create_estimation_config_replacement_6p_fe_profile(
  beta = BETA, sigma2 = 1.0, max_npl_iter = 200L)

config_4p <- create_estimation_config_replacement(
  beta = BETA, sigma2 = 1.0, npl_iter = cfg$max_npl_iter)

cache_baseline <- create_estimation_cache_replacement_8p(
  primitives, obs_panel, config_4p, cfg)

cat(sprintf("  Baseline P_vec range: [%.4f, %.4f]\n",
    min(cache_baseline$P_vec), max(cache_baseline$P_vec)))

eq_baseline <- solve_equilibrium_policy_replacement_6p(
  theta  = theta_struct,
  cache  = cache_baseline,
  config = cfg,
  max_iter = 500L,
  tol      = 1e-7
)
stopifnot(eq_baseline$converged)
P_baseline <- eq_baseline$P
V_baseline <- eq_baseline$V
cat(sprintf("  Baseline: converged=%s | P_M range [%.4f, %.4f]\n",
    eq_baseline$converged,
    min(P_baseline[, "maintain"]), max(P_baseline[, "maintain"])))
stopifnot(all(abs(rowSums(P_baseline) - 1) < 1e-6))

# ==============================================================================
# SECTION 6 — CONSTRUCT CF PREMIUMS AND CF CACHE
# ==============================================================================
cat("=== SECTION 6: CF PREMIUM CONSTRUCTION ===\n")

state_lut <- primitives$state_lut
stopifnot(!is.null(state_lut), all(c("s_idx", "A_bin", "w_state", "rho_state") %in%
                                    names(state_lut)))

# Median FF premium per (A_bin, w_state) from control states, 2006+ only
ff_premiums <- obs_panel[texas_treated == 0L & panel_year >= 2006L & !is.na(premium),
                          .(P_FF = median(premium, na.rm = TRUE),
                            n_obs = .N),
                          by = .(A_bin, w_state)]
cat(sprintf("  FF premium cells computed from control-state 2006+: %d cells\n",
    nrow(ff_premiums)))
print(ff_premiums[order(w_state, A_bin)])

# Fallback: use 1999+ if no 2006+ observations for a cell
ff_premiums_fallback <- obs_panel[texas_treated == 0L & panel_year >= 1999L & !is.na(premium),
                                   .(P_FF_fallback = median(premium, na.rm = TRUE)),
                                   by = .(A_bin, w_state)]

ff_premiums <- merge(ff_premiums, ff_premiums_fallback, by = c("A_bin", "w_state"), all.x = TRUE)
ff_premiums[is.na(P_FF) | n_obs < 5L, P_FF := P_FF_fallback]
ff_premiums[is.na(P_FF), P_FF := median(obs_panel$premium, na.rm = TRUE)]

# Identify TX RB cells (rho_state == 2) and their (A_bin, w_state)
tx_cells <- state_lut[rho_state == 2L]
stopifnot(nrow(tx_cells) == 16L)   # 8 A_bins × 2 w_states × 1 rho

# Build sidecar CF premium CSV
prem_sidecar <- merge(tx_cells, ff_premiums[, .(A_bin, w_state, P_FF)],
                       by = c("A_bin", "w_state"), all.x = TRUE)
prem_sidecar[, baseline_P := cache_baseline$P_vec[s_idx]]
prem_sidecar[, cf_P       := P_FF]
prem_sidecar[, source     := "median_FF_control_2006plus"]
prem_sidecar[is.na(cf_P), `:=`(cf_P = P_FF_fallback, source = "fallback_1999plus")]

cat(sprintf("  TX cell premium swap — baseline mean: %.4f  CF mean: %.4f\n",
    mean(prem_sidecar$baseline_P), mean(prem_sidecar$cf_P, na.rm = TRUE)))

stopifnot(!anyNA(prem_sidecar$cf_P))
fwrite(prem_sidecar[, .(s_idx, A_bin, w_state, baseline_P, cf_P, source)],
       here::here("Output", "Tables", "04o_CF_TX_FF_premiums.csv"))
cat(sprintf("  Saved: %s\n", here::here("Output", "Tables", "04o_CF_TX_FF_premiums.csv")))

# Build CF cache: deep copy and overwrite P_vec for TX cells
cf_cache <- cache_baseline
cf_cache$P_vec <- cache_baseline$P_vec   # copy (numeric vector, not reference)
for (i in seq_len(nrow(tx_cells))) {
  sidx <- tx_cells$s_idx[i]
  cf_P <- prem_sidecar[s_idx == sidx, cf_P]
  if (length(cf_P) == 1L && !is.na(cf_P)) {
    cf_cache$P_vec[sidx] <- cf_P
  } else {
    warning(sprintf("CF premium NA or missing for s_idx=%d (A_bin=%d, w_state=%d)",
                    sidx, tx_cells$A_bin[i], tx_cells$w_state[i]))
  }
}
cat(sprintf("  CF P_vec: %d TX cells updated\n", nrow(tx_cells)))

# ==============================================================================
# SECTION 7 — SOLVE CF EQUILIBRIUM
# ==============================================================================
cat("=== SECTION 7: CF EQUILIBRIUM ===\n")

eq_cf <- solve_equilibrium_policy_replacement_6p(
  theta  = theta_struct,
  cache  = cf_cache,
  config = cfg,
  max_iter = 500L,
  tol      = 1e-7
)
stopifnot(eq_cf$converged)
P_cf <- eq_cf$P
V_cf <- eq_cf$V
cat(sprintf("  CF: converged=%s | P_M range [%.4f, %.4f]\n",
    eq_cf$converged,
    min(P_cf[, "maintain"]), max(P_cf[, "maintain"])))
stopifnot(all(abs(rowSums(P_cf) - 1) < 1e-6))

# Verify CF changed something in TX cells
tx_sidx <- tx_cells$s_idx
delta_PM <- P_cf[tx_sidx, "maintain"] - P_baseline[tx_sidx, "maintain"]
cat(sprintf("  |Delta P_M| in TX cells: min=%.4f  max=%.4f  mean=%.4f\n",
    min(abs(delta_PM)), max(abs(delta_PM)), mean(abs(delta_PM))))
if (max(abs(delta_PM)) < 1e-6)
  warning("CF P_M for TX cells barely changed — verify premium override applied")

# ==============================================================================
# SECTION 8 — EMPIRICAL STATE DISTRIBUTION (mu)
# ==============================================================================
cat("=== SECTION 8: EMPIRICAL STATE DISTRIBUTION ===\n")

# mu = fraction of TX obs_panel rows in each state cell
tx_obs <- obs_panel[texas_treated == 1L]
mu_counts <- tx_obs[, .N, by = s_idx]
all_states <- data.table(s_idx = 1:32L)
mu_dt <- merge(all_states, mu_counts, by = "s_idx", all.x = TRUE)
mu_dt[is.na(N), N := 0L]
mu <- mu_dt$N / sum(mu_dt$N)
stopifnot(abs(sum(mu) - 1) < 1e-10, all(mu >= 0))
cat(sprintf("  mu computed from %s TX observations\n",
    format(nrow(tx_obs), big.mark = ",")))

# ==============================================================================
# SECTION 9 — WELFARE PV SUMMARY
# ==============================================================================
cat("=== SECTION 9: WELFARE PV SUMMARY ===\n")

h_vec <- primitives$h_vec
stopifnot(length(h_vec) == 32L)

compute_welfare <- function(P, V, label) {
  welfare_rows <- lapply(seq_along(E_GRID), function(i) {
    E_val <- E_GRID[i]
    E_dol <- E_GRID_DOLLARS[i]
    E_lbl <- names(E_GRID)[i]

    producer_surplus <- sum(mu * V) * SCALE_FACTOR
    ext_dam_flow     <- sum(mu * P[, "maintain"] * h_vec * E_val)
    external_damage  <- ext_dam_flow * PV_FACTOR * SCALE_FACTOR
    govt_outlay      <- 0
    social_welfare   <- producer_surplus - external_damage - govt_outlay

    data.table(
      scenario       = label,
      E_label        = E_lbl,
      E_external_USD = E_dol,
      ProducerSurplus_USD = round(producer_surplus),
      ExternalDamage_USD  = round(external_damage),
      GovtOutlay_USD      = govt_outlay,
      SocialWelfare_USD   = round(social_welfare)
    )
  })
  rbindlist(welfare_rows)
}

welf_baseline <- compute_welfare(P_baseline, V_baseline, "baseline")
welf_cf       <- compute_welfare(P_cf,       V_cf,       "cf_TX_FF")

welfare_long <- rbindlist(list(welf_baseline, welf_cf))
cat("  Welfare results:\n")
print(welfare_long[, .(scenario, E_label,
                        ProducerSurplus_USD, ExternalDamage_USD, SocialWelfare_USD)])

# Verify SocialWelfare = Producer - External - Govt (within $1)
welfare_long[, check := abs(SocialWelfare_USD - (ProducerSurplus_USD -
                                                   ExternalDamage_USD -
                                                   GovtOutlay_USD))]
stopifnot(all(welfare_long$check <= 1))
cat("  Welfare identity check: PASS\n")

# Build 4-row summary table (wide: baseline vs CF per component)
build_welfare_summary <- function(e_label) {
  bl <- welf_baseline[E_label == e_label]
  cf <- welf_cf[E_label == e_label]
  components <- c("ProducerSurplus_USD", "ExternalDamage_USD",
                  "GovtOutlay_USD", "SocialWelfare_USD")
  rows <- lapply(components, function(comp) {
    data.table(
      component    = comp,
      E_label      = e_label,
      E_external_USD = bl$E_external_USD,
      baseline_USD = bl[[comp]],
      cf_USD       = cf[[comp]],
      delta_USD    = cf[[comp]] - bl[[comp]]
    )
  })
  rbindlist(rows)
}

summary_50k <- build_welfare_summary("HEALTH_PLUS_UNMEASURED")
summary_17k <- build_welfare_summary("HEALTH_ONLY")
welfare_summary <- rbindlist(list(summary_50k, summary_17k))
setorder(welfare_summary, E_external_USD, component)

path_welf_csv <- here::here("Output", "Tables", "04o_CF_Welfare_Summary.csv")
fwrite(welfare_summary, path_welf_csv)
cat(sprintf("  Saved: %s\n", path_welf_csv))

# TEX table (headline: $50K E)
tex_welf <- summary_50k[, .(component, baseline_USD, cf_USD, delta_USD)]
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Welfare decomposition: TX never had RB pricing",
  "  ($E_{ext} = \\$50{,}000$, $\\beta=0.95$, observed sample)}",
  "\\label{tab:cf_welfare}",
  "\\begin{tabular}{lrrr}",
  "\\hline",
  "Component & Baseline (\\$) & CF: FF (\\$) & $\\Delta$ (\\$) \\\\",
  "\\hline"
)
for (i in seq_len(nrow(tex_welf))) {
  r <- tex_welf[i]
  label <- gsub("_USD$", "", r$component)
  tex_lines <- c(tex_lines,
    sprintf("%s & %s & %s & %s \\\\",
      gsub("_", " ", label),
      format(r$baseline_USD, big.mark = ","),
      format(r$cf_USD,       big.mark = ","),
      format(r$delta_USD,    big.mark = ",")))
}
tex_lines <- c(tex_lines, "\\hline", "\\end{tabular}", "\\end{table}")
path_welf_tex <- here::here("Output", "Tables", "04o_CF_Welfare_Summary.tex")
writeLines(tex_lines, path_welf_tex)
cat(sprintf("  Saved TEX: %s\n", path_welf_tex))

# ==============================================================================
# SECTION 10 — ACTION SHARES BY AGE (TX cells)
# ==============================================================================
cat("=== SECTION 10: ACTION SHARES BY AGE (TX) ===\n")

# Read off P(action | state) for TX cells (s_idx 17..32)
action_rows <- lapply(c("baseline", "cf_TX_FF"), function(scen) {
  P_mat <- if (scen == "baseline") P_baseline else P_cf
  lapply(seq_len(nrow(tx_cells)), function(i) {
    row <- tx_cells[i]
    data.table(
      A_bin    = row$A_bin,
      w_state  = row$w_state,
      scenario = scen,
      action   = c("maintain", "exit", "replace"),
      P        = as.numeric(P_mat[row$s_idx, c("maintain", "exit", "replace")])
    )
  }) |> rbindlist()
}) |> rbindlist()

stopifnot(nrow(action_rows) == 96L)   # 8 × 2 × 2 × 3 = 96

path_as_csv <- here::here("Output", "Tables", "04o_CF_ActionShares_byAge.csv")
fwrite(action_rows, path_as_csv)
cat(sprintf("  Saved: %s\n", path_as_csv))

# Figure: facet by action, x=A_bin, y=P, color=scenario, panel=w_state
action_rows[, w_label  := ifelse(w_state == 1L, "Single-Walled", "Double-Walled")]
action_rows[, scenario_label := fcase(
  scenario == "baseline", "Baseline (RB pricing)",
  scenario == "cf_TX_FF", "CF: TX keeps FF pricing")]

p_as <- ggplot(action_rows, aes(x = A_bin, y = P,
                                  color = scenario_label, linetype = scenario_label)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  facet_grid(action ~ w_label, scales = "free_y") +
  scale_x_continuous(breaks = 1:8, labels = c("0-5","5-10","10-15","15-20",
                                               "20-25","25-30","30-35","35+")) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  labs(title = "TX Action Shares by Age: Baseline vs CF (FF pricing)",
       x = "Age bin", y = "P(action | state)",
       color = "Scenario", linetype = "Scenario") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

path_as_fig <- here::here("Output", "Figures", "04o_CF_ActionShares_byAge_TX.png")
ggsave(path_as_fig, p_as, width = 8, height = 7, dpi = 150)
cat(sprintf("  Saved figure: %s\n", path_as_fig))

# ==============================================================================
# SECTION 11 — REMOVAL-AGE DISTRIBUTION (SIMULATION - FAST VECTORIZED)
# ==============================================================================
cat("=== SECTION 11: REMOVAL-AGE DISTRIBUTION SIMULATION ===\n")

# 4 start states: A_bin ∈ {1, 2} × w_state ∈ {1, 2} at rho_state = 2 (TX RB)
start_states <- tx_cells[A_bin %in% c(1L, 2L)]
N_SIM        <- 10000L
MAX_AGE      <- 50L    # cap simulation at 50 years

# ------------------------------------------------------------------------------
# 1. Pre-compute State Transitions
# ------------------------------------------------------------------------------
max_s <- max(state_lut$s_idx)

s_next_up   <- integer(max_s)
s_next_stay <- integer(max_s)
prob_up     <- numeric(max_s)

for (i in seq_len(nrow(state_lut))) {
  r <- state_lut[i]
  s <- r$s_idx
  a_curr <- r$A_bin
  
  s_next_stay[s] <- s
  
  if (a_curr < 8L) {
    pu <- primitives$age_trans[A_bin == a_curr, pi_up]
    prob_up[s] <- pu
    s_up <- state_lut[A_bin == (a_curr + 1L) & 
                      w_state == r$w_state & 
                      rho_state == r$rho_state, s_idx]
    s_next_up[s] <- s_up
  } else {
    prob_up[s] <- 0
    s_next_up[s] <- s
  }
}

# ------------------------------------------------------------------------------
# 2. Vectorized Simulation Function
# ------------------------------------------------------------------------------
simulate_removal_fast <- function(P_mat, start_sidx, n_sim = N_SIM) {
  
  prob_exit    <- as.numeric(P_mat[, "exit"])
  prob_replace <- as.numeric(P_mat[, "replace"])
  
  active_idx     <- seq_len(n_sim)
  current_states <- rep(start_sidx, n_sim)
  
  event_age    <- rep(MAX_AGE, n_sim)
  event_action <- rep("CensoredAtMax", n_sim)
  
  for (t in seq_len(MAX_AGE)) {
    if (length(active_idx) == 0L) break
    
    s   <- current_states
    p_e <- prob_exit[s]
    p_r <- prob_replace[s]
    
    runif_action <- stats::runif(length(active_idx))
    
    is_exit    <- runif_action < p_e
    is_replace <- !is_exit & (runif_action < (p_e + p_r))
    is_maintain <- !is_exit & !is_replace
    
    if (any(is_exit)) {
      idx_exit <- active_idx[is_exit]
      event_age[idx_exit]    <- t
      event_action[idx_exit] <- "Exit"
    }
    
    if (any(is_replace)) {
      idx_replace <- active_idx[is_replace]
      event_age[idx_replace]    <- t
      event_action[idx_replace] <- "Retrofit"
    }
    
    if (!any(is_maintain)) break
    
    active_idx     <- active_idx[is_maintain]
    current_states <- current_states[is_maintain]
    
    runif_age <- stats::runif(length(active_idx))
    s_m       <- current_states
    p_up_m    <- prob_up[s_m]
    
    is_up <- runif_age < p_up_m
    
    new_states <- s_next_stay[s_m]
    if (any(is_up)) {
      new_states[is_up] <- s_next_up[s_m][is_up]
    }
    current_states <- new_states
  }
  
  data.table(age_at_event = event_age, action = event_action)
}

# ------------------------------------------------------------------------------
# 3. Apply Simulation & Summarize
# ------------------------------------------------------------------------------
AGE_BREAKS <- c(0, 5, 10, 15, Inf)
AGE_LABELS <- c("[1-5]", "[6-10]", "[11-15]", "[16+]")

removal_rows <- lapply(seq_len(nrow(start_states)), function(i) {
  ss   <- start_states[i]
  sidx <- ss$s_idx
  label <- sprintf("A%d_W%d_TX", ss$A_bin, ss$w_state)

  lapply(c("baseline", "cf_TX_FF"), function(scen) {
    P_mat <- if (scen == "baseline") P_baseline else P_cf
    cat(sprintf("  Simulating: start=%s scenario=%s ...\n", label, scen))
    
    sims <- simulate_removal_fast(P_mat, sidx)
    sims <- sims[action != "CensoredAtMax"]
    if (nrow(sims) == 0L) return(NULL)
    
    sims[, age_bucket := cut(age_at_event, breaks = AGE_BREAKS, labels = AGE_LABELS,
                             include.lowest = TRUE)]
    density_dt <- sims[, .(density = .N / N_SIM), by = .(action, age_bucket)]
    density_dt[, `:=`(start_state = label, scenario = scen)]
    density_dt
  }) |> rbindlist()
}) |> rbindlist()

setorder(removal_rows, start_state, scenario, action, age_bucket)
cat(sprintf("  Removal-age distribution: %d rows\n", nrow(removal_rows)))

# ------------------------------------------------------------------------------
# 4. Save Data & Plot
# ------------------------------------------------------------------------------
path_rad_csv <- here::here("Output", "Tables", "04o_CF_RemovalAge_Distribution.csv")
fwrite(removal_rows[, .(start_state, scenario, action, age_at_event = age_bucket, density)],
       path_rad_csv)
cat(sprintf("  Saved: %s\n", path_rad_csv))

# Figure: density by age bucket, scenario, action
removal_rows[, scenario_label := fcase(
  scenario == "baseline", "Baseline (RB)",
  scenario == "cf_TX_FF", "CF: TX FF")]

p_rad <- ggplot(removal_rows,
                aes(x = age_bucket, y = density, fill = scenario_label)) +
  geom_col(position = "dodge") +
  facet_grid(action ~ start_state) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(title = "TX Removal-Age Distribution: Baseline vs CF",
       x = "Age at removal event", y = "Fraction of cohort",
       fill = "Scenario") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom")

path_rad_fig <- here::here("Output", "Figures",
                            "04o_CF_RemovalAge_Distribution_TX.png")
ggsave(path_rad_fig, p_rad, width = 10, height = 6, dpi = 150)
cat(sprintf("  Saved figure: %s\n", path_rad_fig))

# ==============================================================================
# SECTION 12 — SAVE RESULTS RDS
# ==============================================================================
cat("=== SECTION 12: SAVE RESULTS RDS ===\n")

cf_results <- list(
  P_baseline   = P_baseline,
  V_baseline   = V_baseline,
  P_cf         = P_cf,
  V_cf         = V_cf,
  cf_cache     = cf_cache,
  welfare_baseline = welf_baseline,
  welfare_cf       = welf_cf
)

path_rds <- here::here("Output", "Estimation_Results", "CF_TX_FlatFee_results.rds")
saveRDS(cf_results, path_rds)
stopifnot(file.exists(path_rds))
cat(sprintf("  Saved: %s\n", path_rds))

cat("\n=== 04o_CF_TX_FlatFee COMPLETE ===\n")
cat(sprintf("  Results RDS:      %s\n", path_rds))
cat(sprintf("  Welfare summary:  %s\n", path_welf_csv))
cat(sprintf("  Action shares:    %s\n", path_as_csv))
cat(sprintf("  Removal-age:      %s\n", path_rad_csv))

# ==============================================================================
# SECTION 13 — MODEL FIT FIGURES (6p+FE CLEAN PANEL)
# ==============================================================================
# PURPOSE
#   Produce the same style of model-vs-data goodness-of-fit figures that
#   04e / 04k make for the older 4p/8p/8p+FE fits, but using the T007
#   Phase 3 six-parameter profile-FE fit (fit$P_hat from Phase 3 = P_baseline
#   computed in Section 5 above).
#
#   Three figure families:
#     (A) Per-wall CCP-by-age:  model lines  + empirical points
#         — one panel per wall type (SW, DW), faceted by action
#         — mirrors 04k_Fit_8paramFE_{SW,DW}.png style
#
#     (B) Per-cell residual scatter: (empirical - model) vs log(n_cell)
#         — all 32 cells, faceted by action, colored by regime
#         — mirrors 04e_Cell_Fit_Residuals.png style
#
#     (C) Baseline vs CF overlay (TX cells only):
#         model lines for BOTH scenarios + empirical points
#         — lets a reader see simultaneously how well the model fits and
#           how large the CF shift is
#
# Outputs (all under OUT_FIG):
#   04o_Fit_6p_SW.png                 (family A, single-wall)
#   04o_Fit_6p_DW.png                 (family A, double-wall)
#   04o_Fit_6p_AllCells_Residuals.png (family B)
#   04o_Fit_6p_CF_Overlay_SW.png      (family C, single-wall TX cells)
#   04o_Fit_6p_CF_Overlay_DW.png      (family C, double-wall TX cells)
# ==============================================================================

cat("=== SECTION 13: MODEL FIT FIGURES (6p+FE) ===\n")

# ------------------------------------------------------------------------------
# 13.1  Build empirical shares from obs_panel (all cells, observed sample)
# ------------------------------------------------------------------------------
# obs_panel is already loaded (Section 4).
# We need the FULL panel (all states, all treated + control) for the
# empirical CCPs that the likelihood was fit to.
# P_baseline rows 1..32 index the canonical state ordering; s_idx is the key.

emp_all <- obs_panel[, .(
  emp_maintain = mean(y_it == 0L),
  emp_exit     = mean(y_it == 1L & I_replace == 0L, na.rm = TRUE),
  emp_replace  = mean(y_it == 1L & I_replace == 1L, na.rm = TRUE),
  n_cell       = .N
), by = s_idx][order(s_idx)]

stopifnot(nrow(emp_all) == 32L,
          all(abs(emp_all$emp_maintain + emp_all$emp_exit + emp_all$emp_replace - 1) < 1e-9))
cat(sprintf("  empirical shares: 32 cells, total N = %s\n",
    format(sum(emp_all$n_cell), big.mark = ",")))

# ------------------------------------------------------------------------------
# 13.2  Merge model CCPs (from P_baseline), state labels, and empirical shares
# ------------------------------------------------------------------------------

# P_baseline is a 32 x 3 matrix with columns "maintain","exit","replace"
# (verified by rowSums == 1 check in Section 5).
P_base_dt <- as.data.table(P_baseline)
setnames(P_base_dt, c("model_maintain", "model_exit", "model_replace"))
P_base_dt[, s_idx := seq_len(.N)]

r5 <- function(x) round(x, 5)

cell_6p <- merge(state_lut, merge(P_base_dt, emp_all, by = "s_idx"), by = "s_idx")
cell_6p[, regime    := fifelse(rho_state == 1L, "FF", "RB")]
cell_6p[, wall      := fifelse(w_state   == 1L, "SW", "DW")]
cell_6p[, age_label := c("0-5","5-10","10-15","15-20",
                          "20-25","25-30","30-35","35+")[A_bin]]
cell_6p[, `:=`(
  model_maintain = r5(model_maintain),  emp_maintain = r5(emp_maintain),
  res_maintain   = r5(emp_maintain - model_maintain),
  model_exit     = r5(model_exit),      emp_exit     = r5(emp_exit),
  res_exit       = r5(emp_exit - model_exit),
  model_replace  = r5(model_replace),   emp_replace  = r5(emp_replace),
  res_replace    = r5(emp_replace - model_replace)
)]
setorder(cell_6p, regime, wall, A_bin)
stopifnot(nrow(cell_6p) == 32L)

# Also write the per-cell fit CSV for paper appendix / supplementary material
out_cell_csv <- here::here("Output", "Tables", "04o_PerCell_Fit_6p_Wide.csv")
fwrite(cell_6p[, .(s_idx, regime, wall, age_bin = A_bin, age_label, n_cell,
                   model_maintain, emp_maintain, res_maintain,
                   model_exit, emp_exit, res_exit,
                   model_replace, emp_replace, res_replace)],
       out_cell_csv)
cat(sprintf("  saved per-cell fit CSV: %s\n", out_cell_csv))

# Quick RMSE summary
for (act in c("maintain", "exit", "replace")) {
  cat(sprintf("  RMSE %-8s : %.5f\n", act,
      sqrt(mean(cell_6p[[paste0("res_", act)]]^2))))
}

# ------------------------------------------------------------------------------
# 13.3  Helper: reshape to long (model + empirical, three actions)
# ------------------------------------------------------------------------------

build_fit_long <- function(dt) {
  rbindlist(list(
    dt[, .(s_idx, A_bin, regime, wall, n_cell, age_label,
           action = "Maintain", model = model_maintain, empirical = emp_maintain)],
    dt[, .(s_idx, A_bin, regime, wall, n_cell, age_label,
           action = "Exit",     model = model_exit,     empirical = emp_exit)],
    dt[, .(s_idx, A_bin, regime, wall, n_cell, age_label,
           action = "Replace",  model = model_replace,  empirical = emp_replace)]
  ))
}

# ------------------------------------------------------------------------------
# 13.4  FAMILY A — per-wall CCP-by-age figure  (mirrors 04k style)
# ------------------------------------------------------------------------------

fit_theme <- theme_minimal(base_size = 12) +
  theme(strip.text      = element_text(face = "bold", size = 12),
        legend.position = "top",
        plot.title      = element_text(face = "bold"),
        axis.text.x     = element_text(angle = 35, hjust = 1))

plot_fit_one_wall <- function(dt_wall, wall_label, outfile) {
  d <- build_fit_long(dt_wall)
  d[, action := factor(action, levels = c("Maintain", "Exit", "Replace"))]
  d[, regime := factor(regime, levels = c("FF", "RB"))]

  p <- ggplot(d, aes(x = A_bin, color = regime)) +
    geom_line(aes(y = model, linetype = regime), linewidth = 1.0) +
    geom_point(aes(y = empirical, size = n_cell),
               shape = 21, fill = "white", stroke = 1.1, alpha = 0.9) +
    facet_wrap(~ action, scales = "free_y", nrow = 1) +
    scale_x_continuous(breaks = 1:8,
                       labels = c("0-5","5-10","10-15","15-20",
                                  "20-25","25-30","30-35","35+")) +
    scale_color_manual(values = c(FF = "steelblue", RB = "firebrick")) +
    scale_size_area(max_size = 7, guide = "none") +
    fit_theme +
    labs(
      title    = sprintf("6p+FE fit — %s (T007 Phase 3, observed sample)", wall_label),
      subtitle = "Solid/dashed line = MODEL-IMPLIED P(action | state);  open circle = EMPIRICAL share  (size ∝ n obs)",
      x = "Age bin (5-yr)", y = "P(action | state)",
      color = "Regime", linetype = "Regime"
    )
  ggsave(outfile, p, width = 12, height = 4.5, dpi = 150)
  cat(sprintf("  saved: %s\n", outfile))
}

plot_fit_one_wall(cell_6p[wall == "SW"], "Single-Walled (SW)",
                  here::here("Output", "Figures", "04o_Fit_6p_SW.png"))
plot_fit_one_wall(cell_6p[wall == "DW"], "Double-Walled (DW)",
                  here::here("Output", "Figures", "04o_Fit_6p_DW.png"))



# ------------------------------------------------------------------------------
# 13.5  FAMILY B — per-cell residual scatter (all 32 cells, mirrors 04e style)
# ------------------------------------------------------------------------------

res_long <- rbindlist(list(
  cell_6p[, .(s_idx, A_bin, regime, wall, n_cell,
              action = "Maintain", residual = res_maintain)],
  cell_6p[, .(s_idx, A_bin, regime, wall, n_cell,
              action = "Exit",     residual = res_exit)],
  cell_6p[, .(s_idx, A_bin, regime, wall, n_cell,
              action = "Replace",  residual = res_replace)]
))
res_long[, action := factor(action, levels = c("Maintain", "Exit", "Replace"))]
res_long[, regime := factor(regime, levels = c("FF", "RB"))]

p_resid <- ggplot(res_long, aes(x = n_cell, y = residual,
                                  color = regime, shape = wall)) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", linewidth = 0.7) +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_x_log10(labels = scales::comma) +
  scale_color_manual(values = c(FF = "steelblue", RB = "firebrick")) +
  facet_wrap(~ action, scales = "free_y", nrow = 1) +
  fit_theme +
  labs(
    title    = "6p+FE per-cell residuals: empirical share − model-predicted P (T007 Phase 3)",
    subtitle = "Dashed = perfect fit;  x-axis = obs per cell (log scale)",
    x = "Obs per cell (log scale)", y = "Empirical − Model",
    color = "Regime", shape = "Wall type"
  )
ggsave(here::here("Output", "Figures", "04o_Fit_6p_AllCells_Residuals.png"),
       p_resid, width = 12, height = 4.5, dpi = 150)
cat(sprintf("  saved: %s\n",
    here::here("Output", "Figures", "04o_Fit_6p_AllCells_Residuals.png")))

# ------------------------------------------------------------------------------
# 13.6  FAMILY C — Baseline vs CF overlay (TX cells only, one panel per wall)
#        Model lines for BOTH scenarios + empirical points from obs_panel
# ------------------------------------------------------------------------------

# CF CCPs for TX cells
P_cf_dt <- as.data.table(P_cf)
setnames(P_cf_dt, c("cf_maintain", "cf_exit", "cf_replace"))
P_cf_dt[, s_idx := seq_len(.N)]

# Empirical shares for TX only (from obs_panel, same rows the model was fit to)
emp_tx <- obs_panel[texas_treated == 1L, .(
  emp_maintain = mean(y_it == 0L),
  emp_exit     = mean(y_it == 1L & I_replace == 0L, na.rm = TRUE),
  emp_replace  = mean(y_it == 1L & I_replace == 1L, na.rm = TRUE),
  n_cell       = .N
), by = s_idx][order(s_idx)]

# Merge everything onto TX cells only (rho_state == 2, s_idx 17..32)
cell_tx <- merge(state_lut[rho_state == 2L],
                  merge(P_base_dt, emp_tx, by = "s_idx"), by = "s_idx")
cell_tx <- merge(cell_tx, P_cf_dt, by = "s_idx")
cell_tx[, wall      := fifelse(w_state == 1L, "SW", "DW")]
cell_tx[, age_label := c("0-5","5-10","10-15","15-20",
                          "20-25","25-30","30-35","35+")[A_bin]]
stopifnot(nrow(cell_tx) == 16L)

# Build long: three "series" — baseline model, CF model, empirical
make_cf_long <- function(dt) {
  rbindlist(list(
    # Baseline model lines
    dt[, .(A_bin, wall, age_label, n_cell,
           action = "Maintain", series = "Baseline (RB)",
           value = model_maintain, is_model = TRUE)],
    dt[, .(A_bin, wall, age_label, n_cell,
           action = "Exit",     series = "Baseline (RB)",
           value = model_exit,     is_model = TRUE)],
    dt[, .(A_bin, wall, age_label, n_cell,
           action = "Replace",  series = "Baseline (RB)",
           value = model_replace,  is_model = TRUE)],
    # CF model lines
    dt[, .(A_bin, wall, age_label, n_cell,
           action = "Maintain", series = "CF: TX FF pricing",
           value = cf_maintain, is_model = TRUE)],
    dt[, .(A_bin, wall, age_label, n_cell,
           action = "Exit",     series = "CF: TX FF pricing",
           value = cf_exit,     is_model = TRUE)],
    dt[, .(A_bin, wall, age_label, n_cell,
           action = "Replace",  series = "CF: TX FF pricing",
           value = cf_replace,  is_model = TRUE)],
    # Empirical data points
    dt[, .(A_bin, wall, age_label, n_cell,
           action = "Maintain", series = "Empirical (TX data)",
           value = emp_maintain, is_model = FALSE)],
    dt[, .(A_bin, wall, age_label, n_cell,
           action = "Exit",     series = "Empirical (TX data)",
           value = emp_exit,     is_model = FALSE)],
    dt[, .(A_bin, wall, age_label, n_cell,
           action = "Replace",  series = "Empirical (TX data)",
           value = emp_replace,  is_model = FALSE)]
  ))
}

plot_cf_overlay_one_wall <- function(dt_wall, wall_label, outfile) {
  d <- make_cf_long(dt_wall)
  d[, action := factor(action, levels = c("Maintain", "Exit", "Replace"))]
  d[, series := factor(series, levels = c("Baseline (RB)", "CF: TX FF pricing",
                                           "Empirical (TX data)"))]
  col_vals  <- c("Baseline (RB)"      = "firebrick",
                 "CF: TX FF pricing"  = "steelblue",
                 "Empirical (TX data)"= "gray30")
  lty_vals  <- c("Baseline (RB)"      = "solid",
                 "CF: TX FF pricing"  = "dashed",
                 "Empirical (TX data)"= "blank")   # points only

  p <- ggplot(d, aes(x = A_bin, color = series)) +
    # Model lines (is_model == TRUE)
    geom_line(data  = d[is_model == TRUE],
              aes(y = value, linetype = series),
              linewidth = 1.0) +
    # Empirical points (is_model == FALSE)
    geom_point(data  = d[is_model == FALSE],
               aes(y = value, size = n_cell),
               shape = 21, fill = "white", stroke = 1.1, alpha = 0.9) +
    facet_wrap(~ action, scales = "free_y", nrow = 1) +
    scale_x_continuous(breaks = 1:8,
                       labels = c("0-5","5-10","10-15","15-20",
                                  "20-25","25-30","30-35","35+")) +
    scale_color_manual(values = col_vals) +
    scale_linetype_manual(values = lty_vals) +
    scale_size_area(max_size = 7, guide = "none") +
    fit_theme +
    labs(
      title    = sprintf("6p+FE model fit + CF overlay — %s (TX cells only)", wall_label),
      subtitle = paste0("Solid red = baseline (RB) model;  dashed blue = CF (FF) model;",
                        "  open circle = TX empirical  (size ∝ n obs)"),
      x = "Age bin (5-yr)", y = "P(action | state)",
      color = "Series", linetype = "Series"
    ) +
    guides(color    = guide_legend(override.aes = list(linetype = c("solid","dashed","blank"),
                                                       shape    = c(NA, NA, 21))),
           linetype = "none")

  ggsave(outfile, p, width = 12, height = 4.5, dpi = 150)
  cat(sprintf("  saved: %s\n", outfile))
}

plot_cf_overlay_one_wall(cell_tx[wall == "SW"], "Single-Walled (SW)",
                          here::here("Output", "Figures", "04o_Fit_6p_CF_Overlay_SW.png"))
plot_cf_overlay_one_wall(cell_tx[wall == "DW"], "Double-Walled (DW)",
                          here::here("Output", "Figures", "04o_Fit_6p_CF_Overlay_DW.png"))

# ------------------------------------------------------------------------------
# 13.7  Summary log
# ------------------------------------------------------------------------------
cat("\n  === Section 13 complete — fit figures ===\n")
for (f in c(
  here::here("Output", "Tables",  "04o_PerCell_Fit_6p_Wide.csv"),
  here::here("Output", "Figures", "04o_Fit_6p_SW.png"),
  here::here("Output", "Figures", "04o_Fit_6p_DW.png"),
  here::here("Output", "Figures", "04o_Fit_6p_AllCells_Residuals.png"),
  here::here("Output", "Figures", "04o_Fit_6p_CF_Overlay_SW.png"),
  here::here("Output", "Figures", "04o_Fit_6p_CF_Overlay_DW.png")
)) cat(sprintf("    %s\n", f))