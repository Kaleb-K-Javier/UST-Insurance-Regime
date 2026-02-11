# ==============================================================================
# 03_Welfare.R  —  Counterfactual Policy Analysis (Model B, 3-Parameter)
# ==============================================================================
# PURPOSE:
#   Compute policy counterfactuals and welfare from NPL-estimated structural
#   model of UST facility closure under heterogeneous insurance regimes.
#
#   Scenarios:
#     1. Baseline  (status quo estimated theta)
#     2. Social Optimum  (firm internalizes full externality -> gamma_risk scaled)
#     3. Closure Subsidy  (planner pays S to close -> kappa shifted)
#     4. Mandate  (old SW tanks forced to close)
#
#   WELFARE AGGREGATION:
#     The Markov chain decomposes into 4 disconnected components by
#     (wall x regime).  We compute the quasi-stationary distribution WITHIN
#     each component, then combine using observed population shares from
#     the estimation sample.
#
# DEPENDS:
#   improved_estimator_OPTIMIZED.r, Model_B_Estimates.rds, Estimated_Primitives.rds
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(ggplot2)
  library(here)
  library(scales)
  library(xtable) # Added for QME slide table generation
})

source(here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))


# Helper function for capitalization
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

# ==============================================================================
# 0. PATHS & CONSTANTS
# ==============================================================================

RESULTS_DIR <- here("Output", "Estimation_Results")
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR, recursive = TRUE)

SCALE_FACTOR <- 10000  # 1 model unit = $10,000

# Externality multiplier: social cost = ext_mult x private loss
EXT_MULT_DEFAULT <- 2.0

# ==============================================================================
# 1. LOAD ESTIMATION OUTPUT & PRIMITIVES
# ==============================================================================
cat("\n[1/7] Loading Estimation Results & Primitives...\n")

est_result <- readRDS(file.path(RESULTS_DIR, "Model_B_Estimates.rds"))
primitives <- readRDS(file.path(RESULTS_DIR, "Estimated_Primitives.rds"))

theta_hat <- est_result$theta_hat
config    <- est_result$config
if (is.null(config$n_actions)) config$n_actions <- 2

stopifnot(all(c("kappa", "gamma_price", "gamma_risk") %in% names(theta_hat)))
cat(sprintf("  theta_hat: kappa=%.3f  gamma_price=%.3f  gamma_risk=%.3f\n",
            theta_hat["kappa"], theta_hat["gamma_price"], theta_hat["gamma_risk"]))

# State space
states <- primitives$states
if (!"state_idx" %in% names(states)) states[, state_idx := .I]
setkey(states, state_idx)
n_states <- nrow(states)

# Transitions
cache_est <- est_result$cache
if (!is.null(cache_est$F_maintain)) {
  transitions <- list(maintain = cache_est$F_maintain, exit = cache_est$F_exit)
} else if (!is.null(primitives$transitions)) {
  transitions <- primitives$transitions
} else {
  stop("No transition matrices found.")
}

cat(sprintf("  State space: %d states\n", n_states))

# ==============================================================================
# 2. POPULATION WEIGHTS FROM DATA
# ==============================================================================
# Load panel to get cross-sectional composition for weighting.

panel_path <- here("Data", "Processed", "annual_facility_panel.csv")
if (file.exists(panel_path)) {
  panel_raw <- fread(panel_path)
  panel_raw[, A_idx := pmin(floor(age_start / 5) + 1, 9)]
  panel_raw[, w_idx := ifelse(wall_type == "single", 1L, 2L)]
  panel_raw[, r_idx := ifelse(regime == "RB", 2L, 1L)]
  panel_raw[, s_idx := (r_idx - 1) * 18 + (w_idx - 1) * 9 + A_idx]

  pop_counts <- panel_raw[, .N, by = s_idx]
  pop_weights <- rep(0, n_states)
  pop_weights[pop_counts$s_idx] <- pop_counts$N
  pop_weights <- pop_weights / sum(pop_weights)
  cat("  Population weights from panel data.\n")
} else {
  pop_weights <- rep(1 / n_states, n_states)
  cat("  WARNING: Panel not found, using uniform population weights.\n")
}

# Sub-chain membership (wall x regime)
chain_id <- paste(states$w, states$rho, sep = "_")
chain_labels <- unique(chain_id)

# ==============================================================================
# 3. ERGODIC DISTRIBUTION (DECOMPOSED BY SUB-CHAIN)
# ==============================================================================

#' Compute quasi-stationary distribution within each disconnected sub-chain,
#' then combine using observed population shares.
compute_weighted_ergodic <- function(P, F_maintain, states, pop_weights,
                                      max_iter = 5000, tol = 1e-10) {

  n <- nrow(P)
  p_m <- P[, "maintain"]
  mu_full <- rep(0, n)

  for (cl in chain_labels) {
    idx <- which(chain_id == cl)
    n_sub <- length(idx)

    # Sub-chain transition kernel
    T_sub <- as.matrix(Diagonal(n_sub, x = p_m[idx]) %*% F_maintain[idx, idx])

    # Population share for this sub-chain
    chain_share <- sum(pop_weights[idx])
    if (chain_share < 1e-15) next

    # Power iteration: init from data distribution within chain
    mu_sub <- pop_weights[idx] / sum(pop_weights[idx])

    for (iter in 1:max_iter) {
      mu_new <- as.numeric(mu_sub %*% T_sub)
      s <- sum(mu_new)
      if (s < 1e-15) {
        mu_sub <- rep(1 / n_sub, n_sub)
        break
      }
      mu_new <- mu_new / s
      if (max(abs(mu_new - mu_sub)) < tol) {
        mu_sub <- mu_new
        break
      }
      mu_sub <- mu_new
    }

    # Scale by chain's population share
    mu_full[idx] <- mu_sub * chain_share
  }

  if (sum(mu_full) > 0) mu_full <- mu_full / sum(mu_full)
  return(mu_full)
}

# ==============================================================================
# 4. COUNTERFACTUAL SOLVER
# ==============================================================================

solve_counterfactual <- function(scenario = c("baseline", "social", "subsidy", "mandate"),
                                  theta_base, states, premiums, hazards, losses,
                                  transitions, config,
                                  subsidy_amount = 0,
                                  externality_mult = 2.0,
                                  mandate_age = 7,
                                  mandate_wall = "single") {

  scenario <- match.arg(scenario)
  cat(sprintf("\n--- Solving: %s ---\n", toupper(scenario)))

  theta_cf <- theta_base

if (scenario == "social") {
    theta_cf["gamma_risk"] <- externality_mult
    cat(sprintf("  gamma_risk: %.3f -> %.3f\n",
                theta_base["gamma_risk"], theta_cf["gamma_risk"]))
  } else if (scenario == "subsidy") {
    theta_cf["kappa"] <- theta_base["kappa"] + subsidy_amount
    cat(sprintf("  kappa: %.3f -> %.3f  (subsidy ~$%.0f)\n",
                theta_base["kappa"], theta_cf["kappa"],
                subsidy_amount * SCALE_FACTOR))
  }

  cache_cf <- create_estimation_cache_model_b(
    states = states, premiums = premiums, hazards = hazards,
    losses = losses, transitions = transitions, config = config
  )

  if (scenario != "mandate") {
    eq <- solve_equilibrium_policy_model_b(
      theta = theta_cf, cache = cache_cf, config = config,
      check_bellman = TRUE
    )
  } else {
    eq <- solve_mandate_equilibrium(
      theta = theta_cf, cache = cache_cf, config = config,
      states = states, mandate_age = mandate_age, mandate_wall = mandate_wall
    )
  }

  cat(sprintf("  Converged: %s\n", eq$converged))
  list(scenario = scenario, theta = theta_cf,
       P = eq$P, V = eq$V, converged = eq$converged, cache = cache_cf)
}

solve_mandate_equilibrium <- function(theta, cache, config, states,
                                       mandate_age, mandate_wall) {

  mandate_mask <- (states$A >= mandate_age) & (states$w == mandate_wall)
  cat(sprintf("  Mandated states: %d / %d\n", sum(mandate_mask), nrow(states)))

  U <- calculate_flow_utilities_model_b(theta, cache)
  n_s <- cache$n_states

  P <- matrix(0.5, n_s, 2, dimnames = list(NULL, c("maintain", "close")))
  P[mandate_mask, "maintain"] <- config$eps_prob
  P[mandate_mask, "close"]    <- 1 - config$eps_prob

  for (iter in 1:5000) {
    V <- invert_value_function_model_b(P, U, config)
    if (any(is.na(V))) V[is.na(V)] <- -1e5
    P_new <- compute_ccps_model_b(U, V, cache, config)
    P_new[mandate_mask, "maintain"] <- config$eps_prob
    P_new[mandate_mask, "close"]    <- 1 - config$eps_prob
    if (max(abs(P_new - P), na.rm = TRUE) < 1e-9) { P <- P_new; break }
    P <- P_new
  }

  V_final <- invert_value_function_model_b(P, U, config)
  list(P = P, V = V_final, converged = (iter < 5000),
       n_iter = iter, n_mandated = sum(mandate_mask))
}

# ==============================================================================
# 5. WELFARE METRICS
# ==============================================================================

compute_welfare <- function(result, states, hazards, losses,
                            F_maintain, pop_weights,
                            theta_ref, cache_ref, config, 
                            externality_mult = EXT_MULT_DEFAULT) {

  P_policy <- result$P
  V_policy <- result$V 

  mu <- compute_weighted_ergodic(P_policy, F_maintain, states, pop_weights)

  # Re-evaluate Real Value using baseline primitives (No Transfers)
  U_real <- calculate_flow_utilities_model_b(theta_ref, cache_ref)
  V_real <- invert_value_function_model_b(P_policy, U_real, config)

  # Aggregation
  avg_firm_surplus_perceived <- sum(mu * V_policy)
  avg_firm_surplus_real      <- sum(mu * V_real)

  exp_leak_risk    <- sum(mu * P_policy[, "maintain"] * hazards)
  exp_private_loss <- sum(mu * P_policy[, "maintain"] * hazards * losses)
  exp_external_dam <- exp_private_loss * (externality_mult - 1)
  
  # Social Welfare = Real Private Value - External Damages
  pv_factor <- 1 / (1 - config$beta)
  social_welfare_val <- avg_firm_surplus_real - (exp_external_dam * pv_factor)

  govt_cost_implied <- avg_firm_surplus_perceived - avg_firm_surplus_real

  # Subgroup Closures
  subgroup_close <- function(mask) {
    w <- mu[mask]; s <- sum(w)
    if (s < 1e-15) return(NA_real_)
    sum(w * P_policy[mask, "close"]) / s
  }
  
  sw <- states$w == "single"; dw <- states$w == "double"
  young <- states$A <= 4;     old <- states$A >= 7
  ff <- states$rho == "FF";   rb <- states$rho == "RB"

  data.table(
    Scenario         = result$scenario,
    Converged        = result$converged,
    Avg_Close_Prob   = sum(mu * P_policy[, "close"]),
    Exp_Leak_Risk    = exp_leak_risk,
    Exp_External_Dam = exp_external_dam,         # Correctly exported for Fig 6
    Firm_Surplus     = avg_firm_surplus_perceived, 
    Real_Surplus     = avg_firm_surplus_real,      
    Social_Welfare   = social_welfare_val,         
    Govt_Cost        = govt_cost_implied,          
    
    Close_SW = subgroup_close(sw), Close_DW = subgroup_close(dw),
    Close_Young = subgroup_close(young), Close_Old = subgroup_close(old),
    Close_SW_RB = subgroup_close(sw & rb), Close_SW_FF = subgroup_close(sw & ff),
    Close_DW_RB = subgroup_close(dw & rb), Close_DW_FF = subgroup_close(dw & ff)
  )
}


# ==============================================================================
# 6. RUN COUNTERFACTUALS
# ==============================================================================
cat("\n[2/7] Solving Counterfactual Scenarios...\n")

cf_base <- list(states = states, premiums = primitives$premiums,
                hazards = primitives$hazards, losses = primitives$losses,
                transitions = transitions, config = config)

res_baseline <- do.call(solve_counterfactual,
  c(list(scenario = "baseline", theta_base = theta_hat), cf_base))

res_social <- do.call(solve_counterfactual,
  c(list(scenario = "social", theta_base = theta_hat,
         externality_mult = EXT_MULT_DEFAULT), cf_base))

res_subsidy <- do.call(solve_counterfactual,
  c(list(scenario = "subsidy", theta_base = theta_hat,
         subsidy_amount = 1.0), cf_base))

res_mandate <- do.call(solve_counterfactual,
  c(list(scenario = "mandate", theta_base = theta_hat,
         mandate_age = 7, mandate_wall = "single"), cf_base))


# ==============================================================================
# 7. WELFARE TABLE & LATEX EXPORT
# ==============================================================================
cat("\n[3/7] Computing Welfare (Standardized to Baseline Primitives)...\n")

welfare_args <- list(states = states, hazards = primitives$hazards,
                     losses = primitives$losses, F_maintain = transitions$maintain,
                     pop_weights = pop_weights,
                     theta_ref = theta_hat, 
                     cache_ref = res_baseline$cache, 
                     config = config)

welfare_results <- rbindlist(lapply(
  list(res_baseline, res_social, res_subsidy, res_mandate),
  function(r) do.call(compute_welfare, c(list(result = r), welfare_args))
))

# --- Calculate Deltas ---
bl <- welfare_results[Scenario == "baseline"]
welfare_results[, Delta_Close_pp := (Avg_Close_Prob - bl$Avg_Close_Prob) * 100]
welfare_results[, Delta_Risk_pct := (Exp_Leak_Risk - bl$Exp_Leak_Risk) / bl$Exp_Leak_Risk * 100]
welfare_results[, Delta_Social_W := Social_Welfare - bl$Social_Welfare]

# Avoided leaks & cost-effectiveness
welfare_results[, Avoided_Leaks := bl$Exp_Leak_Risk - Exp_Leak_Risk]
welfare_results[, Cost_Per_Avoided_Leak := fifelse(
  Avoided_Leaks > 0 & Govt_Cost > 0, Govt_Cost / Avoided_Leaks, NA_real_
)]

# --- Generate Production LaTeX Table ---
cat("\n[4/7] Generating LaTeX Table for QME Slides...\n")

# Format for display
tex_dt <- welfare_results[, .(
  Scenario = simpleCap(as.character(Scenario)),
  `Closure Rate` = sprintf("%.1f\\%%", Avg_Close_Prob * 100),
  `Leak Risk`    = sprintf("%.2f\\%%", Exp_Leak_Risk * 100),
  `Firm Surplus` = sprintf("%.2f", Real_Surplus),
  `Social Welfare` = sprintf("%.2f", Social_Welfare),
  `$\\Delta$ Welfare` = sprintf("%+.3f", Delta_Social_W)
)]

# Create xtable object
xt <- xtable(tex_dt, 
             caption = "Counterfactual Welfare Analysis (Model B)",
             label = "tab:welfare_cf",
             align = c("l", "l", "c", "c", "c", "c", "c"))

# Save to file
print(xt, 
      file = file.path(RESULTS_DIR, "Welfare_Summary.tex"),
      include.rownames = FALSE,
      sanitize.text.function = identity, # Allow LaTeX symbols in columns
      booktabs = TRUE)

cat(sprintf("  Saved LaTeX table to: %s\n", file.path(RESULTS_DIR, "Welfare_Summary.tex")))

# Console Print
print(welfare_results[, .(Scenario, Close=sprintf("%.1f%%", Avg_Close_Prob*100), 
                          dSocW=sprintf("%+.3f", Delta_Social_W))])

fwrite(welfare_results, file.path(RESULTS_DIR, "Counterfactual_Welfare.csv"))

# ==============================================================================
# 8. FIGURES
# ==============================================================================
cat("\n[5/7] Generating Figures...\n")

# FIX: Define pv_factor globally for this section to prevent scope errors in Fig 6 & Loop
pv_factor <- 1 / (1 - config$beta) 

theme_pub <- theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "grey30"),
        panel.grid.minor = element_blank())

scenario_colors <- c("Baseline" = "grey40", "Mandate" = "#D55E00",
                      "Closure Subsidy" = "#0072B2", "Social Optimum" = "#009E73")
scenario_labels <- c(baseline = "Baseline", social = "Social Optimum",
                      subsidy = "Closure Subsidy", mandate = "Mandate")

all_results <- list(res_baseline, res_social, res_subsidy, res_mandate)

plot_data <- rbindlist(lapply(all_results, function(res) {
  data.table(
    Scenario  = scenario_labels[res$scenario],
    A         = states$A,
    Age_Years = (states$A - 1) * 5 + 2.5,
    Wall      = as.character(states$w),
    # RENAME: FF -> Uniform Premium
    Regime    = ifelse(states$rho == "FF", "Uniform Premium", "Risk-Based"),
    P_Close   = res$P[, "close"],
    P_Maintain = res$P[, "maintain"],
    Value     = res$V
  )
}))
plot_data[, Scenario := factor(Scenario,
  levels = c("Baseline", "Mandate", "Closure Subsidy", "Social Optimum"))]

# ---------- FIGURE 1: SW Closure by Policy ----------
g1 <- ggplot(plot_data[Wall == "single"],
             aes(x = Age_Years, y = P_Close, color = Scenario)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8, alpha = 0.7) +
  facet_wrap(~ Regime) + # Labels already handled in data.table creation
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  scale_color_manual(values = scenario_colors) +
  labs(title = "Single-Wall Closure Probability by Policy Scenario",
       subtitle = "Structural counterfactuals, by insurance regime",
       x = "Tank Age (Years)", y = "P(Close)") +
  theme_pub

ggsave(file.path(RESULTS_DIR, "CF_Closure_SW.png"), g1, width = 10, height = 6, dpi = 300)

# ---------- FIGURE 2: Welfare Wedge ----------
idx_target <- which(states$w == "single" & states$rho == "RB")

df_wedge <- rbind(
  data.table(Age_Years = (states$A[idx_target] - 1) * 5 + 2.5,
             Prob = res_baseline$P[idx_target, "close"],
             Type = "Private (Baseline)"),
  data.table(Age_Years = (states$A[idx_target] - 1) * 5 + 2.5,
             Prob = res_social$P[idx_target, "close"],
             Type = "Social Optimum")
)
df_ribbon <- dcast(df_wedge, Age_Years ~ Type, value.var = "Prob")

g2 <- ggplot(df_wedge, aes(x = Age_Years, y = Prob, color = Type, linetype = Type)) +
  geom_line(linewidth = 1.3) +
  geom_ribbon(data = df_ribbon,
              aes(x = Age_Years,
                  ymin = `Private (Baseline)`, ymax = `Social Optimum`,
                  y = NULL, color = NULL, linetype = NULL),
              fill = "#D55E00", alpha = 0.12) +
  scale_color_manual(values = c("Private (Baseline)" = "grey30",
                                 "Social Optimum" = "#009E73")) +
  scale_linetype_manual(values = c("Private (Baseline)" = "solid",
                                    "Social Optimum" = "dashed")) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  labs(title = "The Welfare Wedge: Uninternalized Leak Externality",
       subtitle = "Single-wall tanks, risk-based insurance",
       x = "Tank Age (Years)", y = "P(Close)",
       color = NULL, linetype = NULL) +
  theme_pub +
  theme(legend.position = c(0.25, 0.85),
        legend.background = element_rect(fill = alpha("white", 0.8), color = NA))

ggsave(file.path(RESULTS_DIR, "Welfare_Wedge.png"), g2, width = 8, height = 6, dpi = 300)

# ---------- FIGURE 3: Insurance Regime Effect ----------
idx_RB <- which(states$w == "single" & states$rho == "RB")
idx_FF <- which(states$w == "single" & states$rho == "FF")

df_regime <- rbind(
  data.table(Age_Years = (states$A[idx_RB] - 1) * 5 + 2.5,
             Prob = res_baseline$P[idx_RB, "close"],
             Regime = "Risk-Based"),
  data.table(Age_Years = (states$A[idx_FF] - 1) * 5 + 2.5,
             Prob = res_baseline$P[idx_FF, "close"],
             Regime = "Uniform Premium") # RENAME: Uniform Premium
)

# Determine direction for subtitle
rb_old <- res_baseline$P[idx_RB[which.max(states$A[idx_RB])], "close"]
ff_old <- res_baseline$P[idx_FF[which.max(states$A[idx_FF])], "close"]

if (rb_old > ff_old) {
  regime_subtitle <- "Risk-based pricing increases exit incentives for high-risk tanks"
} else {
  regime_subtitle <- "Uniform premium pooling depresses exit rates for high-risk tanks"
}

g3 <- ggplot(df_regime, aes(x = Age_Years, y = Prob, color = Regime)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Risk-Based" = "#0072B2",
                                 "Uniform Premium" = "#E69F00")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Insurance Regime and Closure Incentives",
       subtitle = regime_subtitle,
       x = "Tank Age (Years)", y = "P(Close)", color = NULL) +
  theme_pub

ggsave(file.path(RESULTS_DIR, "Moral_Hazard.png"), g3, width = 8, height = 6, dpi = 300)

# ---------- FIGURE 4: Fleet Survival ----------
simulate_cohort_survival <- function(P_mat, F_maintain, start_state_idx, steps = 25) {
  n_s <- nrow(P_mat)
  mu <- rep(0, n_s)
  mu[start_state_idx] <- 1.0
  survival <- numeric(steps)

  for (t in 1:steps) {
    mu_active <- mu * P_mat[, "maintain"]
    mu <- as.numeric(t(mu_active) %*% F_maintain)
    survival[t] <- sum(mu)
  }
  return(survival)
}

start_idx <- which(states$A == 4 & states$w == "single" & states$rho == "RB")
T_sim <- 25

df_surv <- rbindlist(lapply(all_results, function(res) {
  surv <- simulate_cohort_survival(res$P, transitions$maintain, start_idx, T_sim)
  data.table(Year = 1:T_sim, Survival = surv,
             Scenario = scenario_labels[res$scenario])
}))
df_surv[, Scenario := factor(Scenario,
  levels = c("Baseline", "Mandate", "Closure Subsidy", "Social Optimum"))]

g4 <- ggplot(df_surv, aes(x = Year, y = Survival, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = scenario_colors) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Fleet Cleansing Dynamics",
       subtitle = "Survival of single-wall RB cohort (initial age 15 years)",
       x = "Years from Policy Implementation",
       y = "Fraction of Cohort Remaining",
       color = NULL, linetype = NULL) +
  theme_pub

ggsave(file.path(RESULTS_DIR, "Fleet_Survival.png"), g4, width = 8, height = 6, dpi = 300)

# ---------- FIGURE 5: Value Function ----------
g5 <- ggplot(plot_data[Wall == "single" & Regime == "Risk-Based"], # Using new label
             aes(x = Age_Years, y = Value, color = Scenario)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = scenario_colors) +
  labs(title = "Value Function: Single-Wall Tanks, Risk-Based Insurance",
       subtitle = "V(x) in annual revenue units",
       x = "Tank Age (Years)", y = expression(V(x))) +
  theme_pub

ggsave(file.path(RESULTS_DIR, "Value_Function.png"), g5, width = 8, height = 5, dpi = 300)

# ---------- FIGURE 6: WELFARE DECOMPOSITION (CORRECTED) ----------
bl_fw  <- welfare_results[Scenario == "baseline", Firm_Surplus]
# FIXED: Using correct column name Exp_External_Dam
bl_dam <- welfare_results[Scenario == "baseline", Exp_External_Dam] * pv_factor
bl_sw  <- welfare_results[Scenario == "baseline", Social_Welfare]

dt_delta <- welfare_results[Scenario != "baseline", .(
  Scenario = factor(Scenario, levels = c("social", "subsidy", "mandate"),
                    labels = c("Social Optimum", "Closure Subsidy", "Mandate")),
  
  `Change in Firm Surplus`   = Firm_Surplus - bl_fw,
  `Avoided External Damages` = -(Exp_External_Dam * pv_factor - bl_dam),
  `Government Cost`          = -Govt_Cost,
  `Net Social Welfare Gain`  = Social_Welfare - bl_sw
)]

dt_delta_long <- melt(dt_delta, id.vars = "Scenario",
                      variable.name = "Component", value.name = "Value")
dt_delta_long[, Value_dollars := Value * SCALE_FACTOR]

g6 <- ggplot(dt_delta_long, aes(x = Scenario, y = Value_dollars, fill = Component)) +
  geom_bar(data = dt_delta_long[Component != "Net Social Welfare Gain"],
           stat = "identity", position = "stack", width = 0.6) + 
  geom_point(data = dt_delta_long[Component == "Net Social Welfare Gain"],
             aes(y = Value_dollars, fill = Component),
             shape = 21, size = 4, color = "black", stroke = 1.5, show.legend = TRUE) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_fill_manual(values = c(
    "Change in Firm Surplus"   = "#0072B2", 
    "Avoided External Damages" = "#009E73", 
    "Government Cost"          = "#D55E00", 
    "Net Social Welfare Gain"  = "white"    
  )) +
  labs(title = "Welfare Decomposition (Standardized)",
       subtitle = "Components sum to Net Welfare (Dot). Subsidy treated as transfer.",
       x = NULL, y = "Change from Baseline ($)", fill = NULL) +
  scale_y_continuous(labels = dollar_format()) +
  theme_pub +
  theme(legend.position = "right")

ggsave(file.path(RESULTS_DIR, "Welfare_Decomposition.png"), g6, width = 9, height = 6, dpi = 300)

# ---------- FIGURE 7: POLICY HETEROGENEITY HEATMAP ----------
dt_heat <- plot_data[Scenario != "Baseline"]
dt_base_ref <- plot_data[Scenario == "Baseline", .(Age_Years, Wall, Regime,
                                                    P_Close_Base = P_Close)]
dt_heat <- merge(dt_heat, dt_base_ref, by = c("Age_Years", "Wall", "Regime"))
dt_heat[, Delta_Close := (P_Close - P_Close_Base) * 100]  # in pp

dt_heat[, State_Label := paste0(
  ifelse(Wall == "single", "SW", "DW"), ", ",
  Regime)] # Already "Uniform Premium"

COLOR_CAP <- 15 
dt_heat[, Delta_Capped := pmin(Delta_Close, COLOR_CAP)]
dt_heat[, Cell_Label := sprintf("%+.1f", Delta_Close)]
dt_heat[, Is_Capped := Delta_Close > COLOR_CAP]

g7 <- ggplot(dt_heat,
             aes(x = Age_Years, y = State_Label, fill = Delta_Capped)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = Cell_Label,
                fontface = ifelse(Is_Capped, "bold", "plain"),
                color = ifelse(Delta_Capped > COLOR_CAP * 0.7, "white", "black")),
            size = 2.6, show.legend = FALSE) +
  scale_color_identity() +
  facet_wrap(~ Scenario, ncol = 1) +
  scale_fill_gradient(low = "white", high = "#D55E00",
                       limits = c(0, COLOR_CAP),
                       name = expression(Delta*" P(Close) (pp)"),
                       breaks = seq(0, COLOR_CAP, 5)) +
  scale_x_continuous(breaks = seq(2.5, 42.5, 5)) +
  labs(title = "Policy Impact Heterogeneity",
       subtitle = paste0("Change in closure probability (pp) vs. baseline. ",
                          "Color capped at ", COLOR_CAP, "pp; bold = exceeds cap."),
       x = "Tank Age (Years)", y = NULL) +
  theme_pub +
  theme(legend.position = "right", panel.grid = element_blank())

ggsave(file.path(RESULTS_DIR, "Policy_Heterogeneity.png"), g7,
       width = 10, height = 8, dpi = 300)


# ---------- FIGURE 8: EXTERNALITY SENSITIVITY ----------
cat("\n[6/7] Computing externality sensitivity...\n")

ext_grid <- seq(1.0, 4.0, by = 0.25)
U_real_base <- calculate_flow_utilities_model_b(theta_hat, cf_base$cache)

# pv_factor is now defined globally at top of Section 8

sens_results <- rbindlist(lapply(ext_grid, function(em) {
  theta_s <- theta_hat
  theta_s["gamma_risk"] <- theta_hat["gamma_risk"] * em
  eq_s <- solve_equilibrium_policy_model_b(theta_s, cf_base$cache, config)
  mu_s <- compute_weighted_ergodic(eq_s$P, transitions$maintain, states, pop_weights)
  
  V_real_s <- invert_value_function_model_b(eq_s$P, U_real_base, config)
  firm_V_real <- sum(mu_s * V_real_s)

  leak_risk_s <- sum(mu_s * eq_s$P[, "maintain"] * primitives$hazards)
  priv_loss_s <- sum(mu_s * eq_s$P[, "maintain"] * primitives$hazards * primitives$losses)
  external_dam_s <- priv_loss_s * (em - 1)
  social_welfare  <- firm_V_real - (external_dam_s * pv_factor)

  data.table(
    Ext_Mult       = em,
    Close_Rate     = sum(mu_s * eq_s$P[, "close"]),
    Leak_Risk      = leak_risk_s,
    Firm_Surplus   = firm_V_real,
    Social_Loss    = priv_loss_s * em,
    Social_Welfare = social_welfare
  )
}))

# ==============================================================================
# 9. SAVE
# ==============================================================================
cat("\n[7/7] Saving all results...\n")

saveRDS(
  list(baseline = res_baseline, social = res_social,
       subsidy = res_subsidy, mandate = res_mandate,
       welfare = welfare_results, plot_data = plot_data,
       sensitivity = sens_results,
       theta_hat = theta_hat, config = config,
       pop_weights = pop_weights, scale_factor = SCALE_FACTOR),
  file.path(RESULTS_DIR, "Counterfactual_Results.rds")
)

# ==============================================================================
# 10. PREMIUM DIAGNOSTIC
# ==============================================================================
cat("\n[Final] Premium Diagnostic...\n")

prem_diag <- data.table(states[, .(A, w, rho, state_idx)],
                         premium = primitives$premiums)

cat("\nPremium structure (single-wall only):\n")
print(dcast(prem_diag[w == "single"], A ~ rho, value.var = "premium"))

ff_sw_prem <- prem_diag[rho == "FF" & w == "single", premium]
if (sd(ff_sw_prem) > 0.001) {
  cat("\n  WARNING: Uniform Premium (FF) premiums vary across age states (sd =",
      round(sd(ff_sw_prem), 4), ").\n")
}

cat(sprintf("\nDone. All outputs in: %s\n", RESULTS_DIR))
