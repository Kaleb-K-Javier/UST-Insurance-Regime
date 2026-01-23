# ==============================================================================
# 04_Counterfactual_Analysis_CORRECTED.R
# ==============================================================================
# PURPOSE: 
#   Run Policy Counterfactuals using Model B Estimates (3-Parameter Version)
#   
#   Scenarios:
#     1. Baseline (Status Quo)
#     2. Social Optimum (Firm internalizes externality via increased gamma_risk)
#     3. Tank Closure Subsidy (Planner pays $X to close)
#     4. Command & Control (Mandatory closure for old SW tanks)
#
# FIXES APPLIED:
#   - 3-parameter theta: (kappa, gamma_price, gamma_risk)
#   - Proper cache construction via create_estimation_cache_model_b()
#   - Mandate constraint applied WITHIN policy iteration
#   - Social optimum via gamma_risk modification OR cache rebuild
# ==============================================================================

library(data.table)
library(Matrix)
library(ggplot2)
library(here)

# Source helper functions (contains all Model B functions)
source(here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))

# ==============================================================================
# SETUP: LOAD ESTIMATION RESULTS
# ==============================================================================

RESULTS_DIR <- here("Output", "Estimation_Results")

# Load estimation output
est_result <- readRDS(file.path(RESULTS_DIR, "Model_B_Estimates.rds"))

# Extract components
theta_hat <- est_result$theta_hat
config <- est_result$config
cache_est <- est_result$cache

# NOTE: est_result$cache contains:
#   - n_states, premiums, hazards, losses, hazard_loss
#   - F_maintain, F_exit (transition matrices)
#   - beta

# We need the raw primitives and states to rebuild cache for counterfactuals
# If not saved separately, reconstruct from cache or load from primitives file
primitives <- readRDS(file.path(RESULTS_DIR, "Estimated_Primitives.rds"))

# Reconstruct state space (must match estimation)
states <- CJ(
  A = 1:9,
  w = factor(c("single", "double"), levels = c("single", "double")),
  rho = factor(c("FF", "RB"), levels = c("FF", "RB")),
  sorted = FALSE
)
states[, state_idx := .I]
setkey(states, state_idx)

# ==============================================================================
# HELPER: BUILD TRANSITIONS (if not available from est_result)
# ==============================================================================

build_transitions_model_b <- function(states, age_probs = NULL) {
  
  n_states <- nrow(states)
  
  # Default annual aging probabilities (5-year bins)
  if (is.null(age_probs)) {
    age_probs <- list(
      p_up   = c(0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.24, 0.25, 0.00),
      p_stay = c(0.82, 0.81, 0.80, 0.79, 0.78, 0.77, 0.76, 0.75, 1.00)
    )
  }
  
  state_map <- setNames(states$state_idx, 
                        paste(states$A, states$w, states$rho, sep = "_"))
  
  F_maintain <- Matrix(0, n_states, n_states, sparse = TRUE)
  
  for (i in 1:n_states) {
    A_i <- states$A[i]
    if (A_i < 9) {
      k_s <- paste(A_i, states$w[i], states$rho[i], sep = "_")
      k_u <- paste(A_i + 1, states$w[i], states$rho[i], sep = "_")
      F_maintain[i, state_map[k_s]] <- age_probs$p_stay[A_i]
      F_maintain[i, state_map[k_u]] <- age_probs$p_up[A_i]
    } else {
      F_maintain[i, i] <- 1.0
    }
  }
  
  list(
    maintain = F_maintain,
    exit = Diagonal(n_states)
  )
}

# Build or extract transitions
if (!is.null(cache_est$F_maintain)) {
  transitions <- list(
    maintain = cache_est$F_maintain,
    exit = cache_est$F_exit
  )
} else {
  transitions <- build_transitions_model_b(states)
}

# ==============================================================================
# COUNTERFACTUAL SOLVER: GENERAL INTERFACE
# ==============================================================================

#' Solve Counterfactual Equilibrium
#'
#' @param scenario Character: "baseline", "social", "subsidy", "mandate"
#' @param theta_base Estimated parameters from NPL
#' @param states State space data.table
#' @param premiums Premium vector
#' @param hazards Hazard rate vector
#' @param losses Loss vector (private)
#' @param transitions List with $maintain, $exit matrices
#' @param config Configuration list
#' @param subsidy_amount For "subsidy": payment to close (in revenue units)
#' @param externality_mult For "social": multiplier on gamma_risk
#' @param mandate_age For "mandate": age threshold (A_idx >=)
#' @param mandate_wall For "mandate": wall type to target ("single")
#' @return List with P (CCPs), V (values), scenario info
solve_counterfactual <- function(scenario = c("baseline", "social", "subsidy", "mandate"),
                                  theta_base,
                                  states,
                                  premiums,
                                  hazards,
                                  losses,
                                  transitions,
                                  config,
                                  subsidy_amount = 0,
                                  externality_mult = 2.0,
                                  mandate_age = 7,
                                  mandate_wall = "single") {
  

  scenario <- match.arg(scenario)
  cat(sprintf("\n--- Solving Counterfactual: %s ---\n", toupper(scenario)))
  
  # Initialize modified theta and primitives
 theta_cf <- theta_base
  premiums_cf <- premiums
  hazards_cf <- hazards
  losses_cf <- losses
  
  # ===========================================================================
  # SCENARIO-SPECIFIC MODIFICATIONS
  # ===========================================================================
  
  if (scenario == "baseline") {
    # No modifications - use estimated parameters as-is
    cat("  Using baseline (estimated) parameters\n")
    
  } else if (scenario == "social") {
    # Social Optimum: Firm internalizes full externality
    # 
    # APPROACH: Increase gamma_risk so firm "sees" social cost
    # If private gamma_risk = 1.0 and externality = 1x private loss,
    # then social gamma_risk = 2.0 (firm internalizes 2x)
    #
    # Alternative: Rebuild cache with losses_cf = losses * externality_mult
    # But modifying gamma_risk is cleaner and doesn't change state space
    
    theta_cf["gamma_risk"] <- theta_base["gamma_risk"] * externality_mult
    cat(sprintf("  gamma_risk: %.3f -> %.3f (externality mult = %.1f)\n",
                theta_base["gamma_risk"], theta_cf["gamma_risk"], externality_mult))
    
  } else if (scenario == "subsidy") {
    # Closure Subsidy: Planner pays subsidy_amount to close
    #
    # This increases the terminal payoff: u_close = kappa + subsidy
    # Equivalent to increasing kappa by subsidy_amount
    
    theta_cf["kappa"] <- theta_base["kappa"] + subsidy_amount
    cat(sprintf("  kappa: %.3f -> %.3f (subsidy = %.1f)\n",
                theta_base["kappa"], theta_cf["kappa"], subsidy_amount))
    
  } else if (scenario == "mandate") {
    # Command & Control: Mandatory closure for specific states
    # Handled via constrained policy iteration below
    cat(sprintf("  Mandate: Close if A >= %d and wall = %s\n", 
                mandate_age, mandate_wall))
  }
  
  # ===========================================================================
  # BUILD CACHE FOR COUNTERFACTUAL
  # ===========================================================================
  
  cache_cf <- create_estimation_cache_model_b(
    states = states,
    premiums = premiums_cf,
    hazards = hazards_cf,
    losses = losses_cf,
    transitions = transitions,
    config = config
  )
  
  # ===========================================================================
  # SOLVE EQUILIBRIUM
  # ===========================================================================
  
  if (scenario != "mandate") {
    # Standard policy iteration
    eq <- solve_equilibrium_policy_model_b(
      theta = theta_cf,
      cache = cache_cf,
      config = config,
      check_bellman = TRUE
    )
    
  } else {
    # Constrained policy iteration for mandate
    eq <- solve_mandate_equilibrium(
      theta = theta_cf,
      cache = cache_cf,
      config = config,
      states = states,
      mandate_age = mandate_age,
      mandate_wall = mandate_wall
    )
  }
  
  # ===========================================================================
  # RETURN RESULTS
  # ===========================================================================
  
  list(
    scenario = scenario,
    theta = theta_cf,
    P = eq$P,
    V = eq$V,
    converged = eq$converged,
    cache = cache_cf
  )
}

# ==============================================================================
# MANDATE SOLVER: CONSTRAINED POLICY ITERATION
# ==============================================================================

#' Solve Equilibrium with Mandate Constraint
#'
#' @description
#' For mandated states, P(close) = 1 is enforced WITHIN the policy iteration.
#' This ensures value function consistency (unlike post-hoc override).
solve_mandate_equilibrium <- function(theta, cache, config, states,
                                       mandate_age, mandate_wall) {
  
  # Identify mandated states
  mandate_mask <- (states$A >= mandate_age) & (states$w == mandate_wall)
  n_mandated <- sum(mandate_mask)
  cat(sprintf("  Mandated states: %d / %d\n", n_mandated, nrow(states)))
  
  # Compute flow utilities
  U <- calculate_flow_utilities_model_b(theta, cache)
  n_states <- cache$n_states
  
  # Initialize CCPs
  P <- matrix(0.5, n_states, 2, dimnames = list(NULL, c("maintain", "close")))
  
  # Apply mandate constraint to initial P
  P[mandate_mask, "maintain"] <- config$eps_prob
  P[mandate_mask, "close"] <- 1 - config$eps_prob
  
  max_iter <- 5000
  tol <- 1e-8
  
  for (iter in 1:max_iter) {
    
    # 1. Invert value function (Hotz-Miller)
    V <- invert_value_function_model_b(P, U, config)
    
    # Handle numerical issues
    if (any(is.na(V))) {
      V[is.na(V)] <- -1e5
    }
    
    # 2. Compute new CCPs
    P_new <- compute_ccps_model_b(U, V, cache, config)
    
    # 3. CRITICAL: Reapply mandate constraint
    P_new[mandate_mask, "maintain"] <- config$eps_prob
    P_new[mandate_mask, "close"] <- 1 - config$eps_prob
    
    # 4. Check convergence
    ccp_diff <- max(abs(P_new - P), na.rm = TRUE)
    
    if (!is.na(ccp_diff) && ccp_diff < tol) {
      P <- P_new
      break
    }
    
    P <- P_new
  }
  
  # Final value function
  V_final <- invert_value_function_model_b(P, U, config)
  
  list(
    P = P,
    V = V_final,
    converged = (iter < max_iter),
    n_iter = iter,
    n_mandated = n_mandated
  )
}

# ==============================================================================
# RUN ALL COUNTERFACTUALS
# ==============================================================================

cat("\n")
cat("==============================================================\n")
cat("          COUNTERFACTUAL POLICY ANALYSIS\n")
cat("==============================================================\n")
cat(sprintf("Estimated theta: kappa=%.3f, gamma_price=%.3f, gamma_risk=%.3f\n",
            theta_hat["kappa"], theta_hat["gamma_price"], theta_hat["gamma_risk"]))

# 1. Baseline
res_baseline <- solve_counterfactual(
  scenario = "baseline",
  theta_base = theta_hat,
  states = states,
  premiums = primitives$premiums,
  hazards = primitives$hazards,
  losses = primitives$losses,
  transitions = transitions,
  config = config
)

# 2. Social Optimum (2x risk internalization)
res_social <- solve_counterfactual(
  scenario = "social",
  theta_base = theta_hat,
  states = states,
  premiums = primitives$premiums,
  hazards = primitives$hazards,
  losses = primitives$losses,
  transitions = transitions,
  config = config,
  externality_mult = 2.0
)

# 3. Closure Subsidy ($1.0 in revenue units ~ 1 year of profit)
res_subsidy <- solve_counterfactual(
  scenario = "subsidy",
  theta_base = theta_hat,
  states = states,
  premiums = primitives$premiums,
  hazards = primitives$hazards,
  losses = primitives$losses,
  transitions = transitions,
  config = config,
  subsidy_amount = 1.0  # In annual revenue units
)

# 4. Mandate: SW tanks >= 30 years (A_idx >= 7 means age bin [30-35)+)
res_mandate <- solve_counterfactual(
  scenario = "mandate",
  theta_base = theta_hat,
  states = states,
  premiums = primitives$premiums,
  hazards = primitives$hazards,
  losses = primitives$losses,
  transitions = transitions,
  config = config,
  mandate_age = 7,
  mandate_wall = "single"
)

# ==============================================================================
# WELFARE COMPUTATION
# ==============================================================================

#' Compute Welfare Metrics for a Counterfactual
#'
#' @param result Output from solve_counterfactual()
#' @param states State space
#' @param hazards Hazard rates
#' @param losses Loss amounts
#' @param externality_mult Social cost multiplier on private loss
#' @return Data.table with welfare metrics
compute_welfare <- function(result, states, hazards, losses, 
                            externality_mult = 2.0) {
  
  P <- result$P
  V <- result$V
  n_states <- nrow(states)
  
  # Ergodic distribution (simplified: uniform over non-absorbing states)
  # For proper welfare, should compute stationary distribution under P
  # Here we use simple averages as approximation
  
  # Average closure probability
  avg_close_prob <- mean(P[, "close"])
  
  # Expected leak risk (weighted by maintain probability)
  # Risk only realized if facility maintains
  exp_leak_risk <- sum(hazards * P[, "maintain"]) / n_states
  
  # Expected private loss (conditional on leak)
  exp_private_loss <- sum(hazards * losses * P[, "maintain"]) / n_states
  
  # Expected social loss (includes externality)
  exp_social_loss <- exp_private_loss * externality_mult
  
  # Average value (firm welfare proxy)
  avg_value <- mean(V)
  
  # Closure rate by wall type
  sw_mask <- states$w == "single"
  dw_mask <- states$w == "double"
  
  avg_close_sw <- mean(P[sw_mask, "close"])
  avg_close_dw <- mean(P[dw_mask, "close"])
  
  # Closure rate by age (young vs old)
  young_mask <- states$A <= 4
  old_mask <- states$A >= 7
  
  avg_close_young <- mean(P[young_mask, "close"])
  avg_close_old <- mean(P[old_mask, "close"])
  
  data.table(
    Scenario = result$scenario,
    Converged = result$converged,
    Avg_Close_Prob = avg_close_prob,
    Exp_Leak_Risk = exp_leak_risk,
    Exp_Private_Loss = exp_private_loss,
    Exp_Social_Loss = exp_social_loss,
    Avg_Value = avg_value,
    Close_SW = avg_close_sw,
    Close_DW = avg_close_dw,
    Close_Young = avg_close_young,
    Close_Old = avg_close_old
  )
}

# Compute welfare for all scenarios
welfare_results <- rbindlist(list(
  compute_welfare(res_baseline, states, primitives$hazards, primitives$losses),
  compute_welfare(res_social, states, primitives$hazards, primitives$losses),
  compute_welfare(res_subsidy, states, primitives$hazards, primitives$losses),
  compute_welfare(res_mandate, states, primitives$hazards, primitives$losses)
))

cat("\n")
cat("==============================================================\n")
cat("                    WELFARE SUMMARY\n")
cat("==============================================================\n")
print(welfare_results, digits = 4)

# ==============================================================================
# VISUALIZATION: CLOSURE PROBABILITIES BY STATE
# ==============================================================================

# Prepare plotting data
plot_data <- data.table()

scenarios <- list(
  Baseline = res_baseline,
  Social = res_social,
  Subsidy = res_subsidy,
  Mandate = res_mandate
)

for (name in names(scenarios)) {
  res <- scenarios[[name]]
  
  # Extract CCPs with state info
  dt <- data.table(
    Scenario = name,
    state_idx = 1:nrow(states),
    A = states$A,
    Age_Years = (states$A - 1) * 5 + 2.5,  # Midpoint of age bin
    Wall = as.character(states$w),
    Regime = as.character(states$rho),
    P_Close = res$P[, "close"],
    P_Maintain = res$P[, "maintain"],
    Value = res$V
  )
  
  plot_data <- rbind(plot_data, dt)
}

# Plot 1: SW Closure by Age
g1 <- ggplot(plot_data[Wall == "single"], 
             aes(x = Age_Years, y = P_Close, color = Scenario, linetype = Regime)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~Regime) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "Single-Wall Tank Closure Probability by Policy",
    subtitle = "By age and insurance regime",
    x = "Tank Age (Years)",
    y = "P(Close)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Plot 2: DW Closure by Age
g2 <- ggplot(plot_data[Wall == "double"], 
             aes(x = Age_Years, y = P_Close, color = Scenario, linetype = Regime)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~Regime) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "Double-Wall Tank Closure Probability by Policy",
    subtitle = "By age and insurance regime",
    x = "Tank Age (Years)",
    y = "P(Close)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Plot 3: Value Function Comparison
g3 <- ggplot(plot_data[Wall == "single" & Regime == "RB"], 
             aes(x = Age_Years, y = Value, color = Scenario)) +
  geom_line(linewidth = 1.1) +
  labs(
    title = "Value Function: Single-Wall Tanks under Risk-Based Insurance",
    x = "Tank Age (Years)",
    y = "V(x)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# ==============================================================================
# SAVE OUTPUTS
# ==============================================================================

# Save welfare table
fwrite(welfare_results, file.path(RESULTS_DIR, "Counterfactual_Welfare.csv"))

# Save full results
saveRDS(
  list(
    baseline = res_baseline,
    social = res_social,
    subsidy = res_subsidy,
    mandate = res_mandate,
    welfare = welfare_results,
    plot_data = plot_data
  ),
  file.path(RESULTS_DIR, "Counterfactual_Results.rds")
)

# Save plots
ggsave(file.path(RESULTS_DIR, "CF_Closure_SW.png"), g1, width = 10, height = 6)
ggsave(file.path(RESULTS_DIR, "CF_Closure_DW.png"), g2, width = 10, height = 6)
ggsave(file.path(RESULTS_DIR, "CF_Value_Function.png"), g3, width = 8, height = 5)

cat("\n")
cat("==============================================================\n")
cat("Results saved to: ", RESULTS_DIR, "\n")
cat("==============================================================\n")

# ==============================================================================
# OPTIONAL: POLICY COMPARISON TABLE
# ==============================================================================

# Compute changes relative to baseline
baseline_close <- welfare_results[Scenario == "baseline", Avg_Close_Prob]
baseline_risk <- welfare_results[Scenario == "baseline", Exp_Leak_Risk]

policy_comparison <- welfare_results[, .(
  Scenario,
  Close_Rate = sprintf("%.1f%%", 100 * Avg_Close_Prob),
  Delta_Close = sprintf("%+.1f pp", 100 * (Avg_Close_Prob - baseline_close)),
  Leak_Risk = sprintf("%.2f%%", 100 * Exp_Leak_Risk),
  Delta_Risk = sprintf("%+.1f%%", 100 * (Exp_Leak_Risk - baseline_risk) / baseline_risk)
)]

cat("\n")
cat("==============================================================\n")
cat("              POLICY COMPARISON (vs Baseline)\n")
cat("==============================================================\n")
print(policy_comparison)

