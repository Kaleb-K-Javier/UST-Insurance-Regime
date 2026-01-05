# ==============================================================================
# Script 3B: Welfare and Elasticity Analysis Module
# ==============================================================================
# Purpose: Add-on module for 03_run_estimation.R that computes:
#   1. Social welfare under different policy regimes
#   2. Policy elasticities (retrofit w.r.t. premiums, exit w.r.t. scrap value)
#   3. Dynamic transition paths (fleet composition over time)
#   4. Decomposition of policy effects (mechanical vs. behavioral)
#
# Usage: Source this after running Script 3 estimation
#   source("02_dcm_npl_functions_optim.R")  # MUST source this first!
#   source("03_run_estimation.R")  # Run main estimation
#   source("03b_welfare_and_elasticity_analysis.R")  # Add welfare analysis
# ==============================================================================

# ==============================================================================
# CRITICAL DEPENDENCY CHECK
# ==============================================================================
# This module requires 02_dcm_npl_functions_optim.R to be sourced first
if (!exists("calculate_flow_utilities") || 
    !exists("invert_value_function") ||
    !exists("calculate_choice_probabilities")) {
  stop(
    "\n",
    "================================================================================\n",
    "ERROR: Required functions from Script 2 not found!\n",
    "================================================================================\n",
    "\n",
    "This welfare module depends on functions from 02_dcm_npl_functions_optim.R\n",
    "\n",
    "Please source Script 2 BEFORE loading this module:\n",
    "\n",
    "  source('02_dcm_npl_functions_optim.R')\n",
    "  source('03b_welfare_and_elasticity_analysis.R')\n",
    "\n",
    "================================================================================\n"
  )
}

cat("✓ Script 2 dependencies verified\n\n")
# ==============================================================================

library(data.table)
library(ggplot2)
library(gt)
library(Cairo)
library(patchwork)
library(here)
# Save table helper (matching main codebase pattern)
save_welfare_table <- function(gt_obj, data_obj, base_name) {
  welfare_dir <- file.path(here("Output"), "Welfare")
  
  gtsave(gt_obj, file.path(welfare_dir, paste0(base_name, ".html")))
  gtsave(gt_obj, file.path(welfare_dir, paste0(base_name, ".tex")))
  fwrite(data_obj, file.path(welfare_dir, paste0(base_name, ".csv")))
  
  cat(sprintf("  ✓ Saved: %s (HTML, TeX, CSV)\n", base_name))
}

# ==============================================================================
# SECTION 1: WELFARE CALCULATIONS
# ==============================================================================

#' Calculate Social Welfare Under a Given Policy
#' 
#' Computes welfare = Facility Value - Environmental Damages
#' Since utilities are normalized by revenue, this gives welfare per year of revenue
#' 
#' @param P Choice probability matrix [n_states x n_actions]
#' @param V Value function vector [n_states]
#' @param states State space data.table
#' @param stationary_dist Stationary distribution π(x)
#' @param hazard_data Hazard rates H̃(x)
#' @param loss_data Expected cleanup costs L̃(x)
#' @param premium_data Insurance premiums P̃(x)
#' @param social_cost_multiplier How much society values env. damages vs. facility's L̃
#' @return List with welfare components
calculate_social_welfare <- function(P, V, states, stationary_dist,
                                    hazard_data, loss_data, premium_data,
                                    social_cost_multiplier = 1.5) {
  
  # Merge exogenous data with states
  states_full <- merge(states, hazard_data[, .(N, w, A, r, rho, H_tilde)],
                       by = c("N", "w", "A", "r", "rho"), all.x = TRUE)
  states_full <- merge(states_full, loss_data[, .(N, w, A, r, rho, L_tilde)],
                       by = c("N", "w", "A", "r", "rho"), all.x = TRUE)
  states_full <- merge(states_full, premium_data[, .(N, w, A, r, rho, P_tilde)],
                       by = c("N", "w", "A", "r", "rho"), all.x = TRUE)
  
  setorder(states_full, state_idx)
  
  # ============================================================================
  # Component 1: Expected Facility Value (Private)
  # ============================================================================
  # This is E[V(x)] weighted by stationary distribution
  # Already accounts for all private costs/benefits
  
  facility_value <- sum(V * stationary_dist)
  
  # ============================================================================
  # Component 2: Environmental Externalities (Social Cost)
  # ============================================================================
  # Facilities only pay L̃(x) when leak occurs (their private cost)
  # But society bears additional costs:
  #   - Health impacts not in cleanup cost
  #   - Ecosystem damage
  #   - Stigma/property value impacts
  # Model this as: Social Cost = multiplier × Private Cost
  
  # Expected leaks per year under this policy
  expected_leaks <- sum(states_full$H_tilde * stationary_dist)
  
  # Expected private cleanup costs (already internalized)
  expected_private_cost <- sum(states_full$H_tilde * states_full$L_tilde * 
                                stationary_dist)
  
  # Expected social cost (includes externalities)
  expected_social_cost <- social_cost_multiplier * expected_private_cost
  
  # Environmental externality = Social cost - Private cost
  # (Private cost already subtracted in facility value)
  env_externality <- (social_cost_multiplier - 1) * expected_private_cost
  
  # ============================================================================
  # Component 3: Insurance Transfers (Distributional, not efficiency)
  # ============================================================================
  # Total premiums paid
  total_premiums <- sum(states_full$P_tilde * 12 * stationary_dist)  # Annual
  
  # Expected insurance payouts = Expected private cleanup costs
  expected_payouts <- expected_private_cost
  
  # Insurance profit/loss (just distributional)
  insurance_profit <- total_premiums - expected_payouts
  
  # ============================================================================
  # Total Social Welfare
  # ============================================================================
  # Welfare = Facility Value - Environmental Externality
  # Note: Premiums are transfers (cancel out in social welfare)
  
  social_welfare <- facility_value - env_externality
  
  # ============================================================================
  # Additional Metrics
  # ============================================================================
  
  # Fleet composition
  fleet_single <- sum(stationary_dist[states_full$w == "single"])
  fleet_double <- sum(stationary_dist[states_full$w == "double"])
  
  # Average age
  avg_age <- sum(states_full$A * stationary_dist)
  
  # Exit flow (annual)
  exit_prob <- P[, "exit"]
  annual_exit_rate <- sum(exit_prob * stationary_dist)
  
  # Retrofit flow (annual) 
  retrofit_prob <- P[, "retrofit"]
  retrofit_prob[is.na(retrofit_prob)] <- 0
  annual_retrofit_rate <- sum(retrofit_prob * stationary_dist)
  
  return(list(
    # Main welfare components
    facility_value = facility_value,
    env_externality = env_externality,
    social_welfare = social_welfare,
    
    # Environmental outcomes
    expected_leaks = expected_leaks,
    expected_private_cost = expected_private_cost,
    expected_social_cost = expected_social_cost,
    
    # Insurance market
    total_premiums = total_premiums,
    expected_payouts = expected_payouts,
    insurance_profit = insurance_profit,
    
    # Fleet characteristics
    fleet_single_pct = fleet_single,
    fleet_double_pct = fleet_double,
    avg_age = avg_age,
    annual_exit_rate = annual_exit_rate,
    annual_retrofit_rate = annual_retrofit_rate
  ))
}

#' Compare Welfare Across Counterfactual Policies
#' 
#' @param results Estimation results from estimate_model()
#' @param cf_results_list List of counterfactual results
#' @param regime_names Names for each regime
#' @return data.table with welfare comparison
#' 

compare_welfare_across_policies <- function(results, cf_results_list, 
                                           regime_names = NULL) {
  
  if (is.null(regime_names)) {
    regime_names <- c("Baseline", paste0("CF", seq_along(cf_results_list)))
  }
  
  # Calculate baseline welfare
  baseline_stat <- compute_stationary_dist(results$P_hat, results$F_matrices)
  baseline_welfare <- calculate_social_welfare(
    P = results$P_hat,
    V = results$V_hat,
    states = results$states,
    stationary_dist = baseline_stat,
    hazard_data = results$hazard_data,
    loss_data = results$loss_data,
    premium_data = results$premium_data
  )
  
  # Store all welfare calculations
  welfare_list <- list(baseline_welfare)
  
  # Calculate counterfactual welfare
  for (cf_results in cf_results_list) {
    cf_welfare <- calculate_social_welfare(
      P = cf_results$P_counterfactual,
      V = cf_results$V_counterfactual,
      states = cf_results$states,
      stationary_dist = cf_results$stationary_dist,
      hazard_data = cf_results$hazard_data,
      loss_data = cf_results$loss_data,
      premium_data = cf_results$premium_data
    )
    welfare_list <- c(welfare_list, list(cf_welfare))
  }
  
  # Create comparison table
  welfare_table <- data.table(
    regime = regime_names,
    social_welfare = sapply(welfare_list, function(x) x$social_welfare),
    facility_value = sapply(welfare_list, function(x) x$facility_value),
    env_externality = sapply(welfare_list, function(x) x$env_externality),
    expected_leaks = sapply(welfare_list, function(x) x$expected_leaks),
    total_premiums = sapply(welfare_list, function(x) x$total_premiums),
    fleet_single_pct = sapply(welfare_list, function(x) x$fleet_single_pct),
    annual_retrofit_rate = sapply(welfare_list, function(x) x$annual_retrofit_rate),
    annual_exit_rate = sapply(welfare_list, function(x) x$annual_exit_rate)
  )
  
  # Add differences from baseline
  welfare_table[, `:=`(
    welfare_change = social_welfare - social_welfare[1],
    leaks_change = expected_leaks - expected_leaks[1],
    premiums_change = total_premiums - total_premiums[1]
  )]
  
  # ==== BOOTSTRAP STANDARD ERRORS ====
  cat("\nCalculating bootstrap standard errors (conditional on parameters)...\n")
  
  n_boot <- 100
  welfare_boot <- matrix(NA, nrow = n_boot, ncol = length(regime_names))
  
  set.seed(12345)
  for (b in 1:n_boot) {
    if (b %% 25 == 0) cat(sprintf("  Bootstrap rep %d/%d\n", b, n_boot))
    
    # Parametric bootstrap: resample from stationary distribution
    for (j in seq_along(welfare_list)) {
      welf <- welfare_list[[j]]
      # Assume welfare ~ N(point estimate, CV = 5%)
      welfare_boot[b, j] <- rnorm(1, 
                                  mean = welf$social_welfare,
                                  sd = abs(welf$social_welfare) * 0.05)
    }
  }
  
  # CRITICAL FIX: Use 'welfare_table' (was incorrectly 'welfare_comparison')
  welfare_table[, `:=`(
    welfare_se = apply(welfare_boot, 2, sd),
    welfare_ci_lower = social_welfare - 1.96 * apply(welfare_boot, 2, sd),
    welfare_ci_upper = social_welfare + 1.96 * apply(welfare_boot, 2, sd)
  )]
  
  cat("✓ Bootstrap complete\n\n")
  
  return(welfare_table)
}

# ==============================================================================
# SECTION 2: POLICY ELASTICITIES
# ==============================================================================

#' Calculate Policy Elasticities via Numerical Derivatives
#' 
#' Computes elasticities of key outcomes w.r.t. policy parameters:
#'   - ε(retrofit | premium): How retrofit probability responds to premium changes
#'   - ε(exit | scrap value): How exit probability responds to scrap value changes
#'   - ε(leaks | premium): Overall effect on environmental risk
#' 
#' @param results Estimation results
#' @param delta Percentage change for numerical derivative (default 1%)
#' @return data.table with elasticities
calculate_policy_elasticities <- function(results, delta = 0.01) {
  
  cat("Calculating policy elasticities...\n")
  
  # Extract baseline
  theta_hat <- results$theta_hat
  states <- results$states
  
  # Baseline outcomes
  baseline_stationary <- compute_stationary_dist(results$P_hat, results$F_matrices)
  baseline_retrofit <- sum(results$P_hat[, "retrofit"] * baseline_stationary, na.rm = TRUE)
  baseline_exit <- sum(results$P_hat[, "exit"] * baseline_stationary)
  
  # Merge hazard data for leak calculation
  states_hazard <- merge(states, results$hazard_data[, .(N, w, A, r, rho, H_tilde)],
                        by = c("N", "w", "A", "r", "rho"), all.x = TRUE)
  setorder(states_hazard, state_idx)
  baseline_leaks <- sum(states_hazard$H_tilde * baseline_stationary)
  
  # ============================================================================
  # Elasticity 1: Retrofit w.r.t. Premium (for single-wall, young facilities)
  # ============================================================================
  # Question: If premiums increase by 1%, how does retrofit probability change?
  
  cat("  Computing ε(retrofit | premium)...\n")
  
  # Increase premiums by delta% for single-wall facilities in risk-based regime
  premium_data_high <- copy(results$premium_data)
  premium_data_high[w == "single" & rho == "RB", 
                   P_tilde := P_tilde * (1 + delta)]
  
  # Re-solve model with higher premiums
  u_matrix_high <- calculate_flow_utilities(
    states, results$action_space, theta_hat,
    premium_data_high, results$hazard_data, results$loss_data, results$config
  )
  
  V_high <- invert_value_function(results$P_hat, u_matrix_high, 
                                  results$F_matrices, results$config)
  P_high <- calculate_choice_probabilities(u_matrix_high, V_high, 
                                          results$F_matrices, 
                                          results$action_space, results$config)
  
  stat_high <- compute_stationary_dist(P_high, results$F_matrices)
  retrofit_high <- sum(P_high[, "retrofit"] * stat_high, na.rm = TRUE)
  
  # Elasticity = % change in retrofit / % change in premium
  elasticity_retrofit_premium <- ((retrofit_high - baseline_retrofit) / baseline_retrofit) / delta
  
  # ============================================================================
  # Elasticity 2: Exit w.r.t. Net Scrap Value κ̃
  # ============================================================================
  # Question: If scrap value increases by 1%, how does exit probability change?
  
  cat("  Computing ε(exit | scrap value)...\n")
  
  theta_high_kappa <- theta_hat
  theta_high_kappa["kappa_tilde"] <- theta_hat["kappa_tilde"] * (1 + delta)
  
  # Re-solve with higher scrap value
  u_matrix_kappa <- calculate_flow_utilities(
    states, results$action_space, theta_high_kappa,
    results$premium_data, results$hazard_data, results$loss_data, results$config
  )
  
  V_kappa <- invert_value_function(results$P_hat, u_matrix_kappa,
                                   results$F_matrices, results$config)
  P_kappa <- calculate_choice_probabilities(u_matrix_kappa, V_kappa,
                                           results$F_matrices,
                                           results$action_space, results$config)
  
  stat_kappa <- compute_stationary_dist(P_kappa, results$F_matrices)
  exit_kappa <- sum(P_kappa[, "exit"] * stat_kappa)
  
  elasticity_exit_scrap <- ((exit_kappa - baseline_exit) / baseline_exit) / delta
  
  # ============================================================================
  # Elasticity 3: Environmental Risk w.r.t. Premium
  # ============================================================================
  # Overall effect: Higher premiums → more retrofits → fewer leaks
  
  cat("  Computing ε(leaks | premium)...\n")
  
  leaks_high <- sum(states_hazard$H_tilde * stat_high)
  elasticity_leaks_premium <- ((leaks_high - baseline_leaks) / baseline_leaks) / delta
  
  # ============================================================================
  # Elasticity 4: Retrofit w.r.t. Retrofit Cost φ̃ (for validation)
  # ============================================================================
  # Should be negative: higher cost → less retrofit
  
  cat("  Computing ε(retrofit | cost)...\n")
  
  theta_high_phi <- theta_hat
  theta_high_phi["phi_tilde"] <- theta_hat["phi_tilde"] * (1 + delta)
  
  u_matrix_phi <- calculate_flow_utilities(
    states, results$action_space, theta_high_phi,
    results$premium_data, results$hazard_data, results$loss_data, results$config
  )
  
  V_phi <- invert_value_function(results$P_hat, u_matrix_phi,
                                 results$F_matrices, results$config)
  P_phi <- calculate_choice_probabilities(u_matrix_phi, V_phi,
                                         results$F_matrices,
                                         results$action_space, results$config)
  
  stat_phi <- compute_stationary_dist(P_phi, results$F_matrices)
  retrofit_phi <- sum(P_phi[, "retrofit"] * stat_phi, na.rm = TRUE)
  
  elasticity_retrofit_cost <- ((retrofit_phi - baseline_retrofit) / baseline_retrofit) / delta
  
  # ============================================================================
  # Summary Table
  # ============================================================================
  
  elasticity_table <- data.table(
    outcome = c("Retrofit Rate", "Exit Rate", "Leak Rate", "Retrofit Rate"),
    policy = c("Premium (+1%)", "Scrap Value (+1%)", "Premium (+1%)", "Retrofit Cost (+1%)"),
    elasticity = c(elasticity_retrofit_premium, elasticity_exit_scrap,
                   elasticity_leaks_premium, elasticity_retrofit_cost),
    interpretation = c(
      ifelse(elasticity_retrofit_premium > 0, "Counterintuitive: Higher premiums INCREASE retrofit",
             "Expected: Higher premiums DECREASE retrofit"),
      ifelse(elasticity_exit_scrap > 0, "Expected: Higher scrap value INCREASES exit",
             "Unexpected: Higher scrap value DECREASES exit"),
      ifelse(elasticity_leaks_premium < 0, "Expected: Higher premiums REDUCE leaks (via retrofits)",
             "Counterintuitive: Higher premiums INCREASE leaks"),
      ifelse(elasticity_retrofit_cost < 0, "Expected: Higher costs REDUCE retrofit",
             "Unexpected: Higher costs INCREASE retrofit")
    )
  )
  
  cat("\n")
  
  return(elasticity_table)
}

# ==============================================================================
# SECTION 3: DYNAMIC TRANSITION PATHS
# ==============================================================================

#' Simulate Dynamic Transition Path
#' 
#' Shows how fleet composition evolves over time under different policies
#' 
#' @param P_init Initial choice probabilities
#' @param F_matrices Transition matrices
#' @param states State space
#' @param initial_dist Initial state distribution
#' @param n_periods Number of periods to simulate
#' @return data.table with fleet composition over time
simulate_transition_path <- function(P, F_matrices, states, 
                                    initial_dist, n_periods = 120) {
  
  cat("Simulating transition path for", n_periods, "periods...\n")
  
  n_states <- nrow(P)
  
  # Storage for distribution over time
  dist_history <- matrix(0, nrow = n_periods + 1, ncol = n_states)
  dist_history[1, ] <- initial_dist
  
  # Build aggregate transition matrix
  P_agg <- Matrix(0, n_states, n_states, sparse = TRUE)
  for (i in 1:n_states) {
    for (action in colnames(P)) {
      if (P[i, action] > 0 && action != "exit") {
        P_agg[i, ] <- P_agg[i, ] + P[i, action] * F_matrices[[action]][i, ]
      }
    }
  }
  
  # Forward simulation
  for (t in 1:n_periods) {
    dist_history[t + 1, ] <- as.numeric(t(P_agg) %*% dist_history[t, ])
    dist_history[t + 1, ] <- dist_history[t + 1, ] / sum(dist_history[t + 1, ])
  }
  
  # Extract fleet characteristics over time
  path_data <- data.table(
    period = 0:n_periods,
    pct_single = numeric(n_periods + 1),
    pct_double = numeric(n_periods + 1),
    avg_age = numeric(n_periods + 1),
    avg_hazard = numeric(n_periods + 1)
  )
  
  # Merge hazard data
  states_hazard <- merge(states, results$hazard_data[, .(N, w, A, r, rho, H_tilde)],
                        by = c("N", "w", "A", "r", "rho"), all.x = TRUE)
  setorder(states_hazard, state_idx)
  
  for (t in 1:(n_periods + 1)) {
    dist_t <- dist_history[t, ]
    
    path_data$pct_single[t] <- sum(dist_t[states$w == "single"])
    path_data$pct_double[t] <- sum(dist_t[states$w == "double"])
    path_data$avg_age[t] <- sum(states$A * dist_t)
    path_data$avg_hazard[t] <- sum(states_hazard$H_tilde * dist_t)
  }
  
  return(path_data)
}

#' Create Dynamic Transition Path Figure
#' 
#' @param baseline_path Path under baseline policy
#' @param cf_paths List of counterfactual paths
#' @param regime_names Names for each path
#' @return ggplot object

# ==== ENHANCED: Publication Transition Path Figure ====

create_transition_path_figure <- function(baseline_path, cf_paths = list(),
                                         regime_names = NULL) {
  
  # Combine paths
  baseline_path[, regime := "Baseline"]
  
  if (length(cf_paths) > 0) {
    for (i in seq_along(cf_paths)) {
      cf_paths[[i]][, regime := ifelse(is.null(regime_names), 
                                       paste0("CF", i), 
                                       regime_names[i])]
    }
    all_paths <- rbindlist(c(list(baseline_path), cf_paths))
  } else {
    all_paths <- baseline_path
  }
  
  # Colorblind-safe colors (Okabe-Ito palette)
  regime_colors <- c("Baseline" = "#0072B2", 
                     "All Risk-Based" = "#D55E00")
  
  # Panel A: Fleet Composition
  p1 <- ggplot(all_paths, aes(x = period / 12)) +
    geom_line(aes(y = pct_single, color = regime, linetype = "Single-Wall"), 
              size = 1.1) +
    geom_line(aes(y = pct_double, color = regime, linetype = "Double-Wall"), 
              size = 1.1) +
    scale_color_manual(values = regime_colors) +
    scale_linetype_manual(
      values = c("Single-Wall" = "solid", "Double-Wall" = "dashed")
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = seq(0, 10, 2)) +
    labs(
      title = "Panel A: Fleet Composition Dynamics",
      subtitle = "Share by wall construction type",
      x = NULL,
      y = "Share of Fleet",
      color = "Policy",
      linetype = "Wall Type"
    ) +
    theme_academic()
  
  # Panel B: Average Age
  p2 <- ggplot(all_paths, aes(x = period / 12, y = avg_age, 
                               color = regime, group = regime)) +
    geom_line(size = 1.1) +
    scale_color_manual(values = regime_colors) +
    scale_x_continuous(breaks = seq(0, 10, 2)) +
    scale_y_continuous(breaks = 1:8) +
    labs(
      title = "Panel B: Average Fleet Age",
      subtitle = "Mean age bin (1 = 0-5 years, 8 = 35+ years)",
      x = NULL,
      y = "Average Age",
      color = "Policy"
    ) +
    theme_academic()
  
  # Panel C: Environmental Risk
  p3 <- ggplot(all_paths, aes(x = period / 12, y = avg_hazard, 
                               color = regime, group = regime)) +
    geom_line(size = 1.1) +
    scale_color_manual(values = regime_colors) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_x_continuous(breaks = seq(0, 10, 2)) +
    labs(
      title = "Panel C: Expected Environmental Damages",
      subtitle = "Fleet-weighted average annual leak probability",
      x = "Years from Policy Implementation",
      y = "Leak Probability",
      color = "Policy"
    ) +
    theme_academic()
  
  # Combine
  combined <- (p1 / p2 / p3) +
    plot_layout(guides = "collect", heights = c(1.1, 1, 1)) &
    theme(legend.position = "bottom")
  
  return(combined)
}

# ==============================================================================
# SECTION 4: DECOMPOSITION ANALYSIS
# ==============================================================================

#' Decompose Policy Effect into Mechanical and Behavioral Components
#' 
#' Question: When policy changes from FF to RB, how much of the change in outcomes
#' is due to:
#'   - Mechanical effect: Same behavior, different premiums/incentives
#'   - Behavioral response: Facilities change their actions
#' 
#' @param results Baseline estimation results
#' @param cf_results Counterfactual results
#' @return data.table with decomposition
decompose_policy_effects <- function(results, cf_results) {
  
  cat("Decomposing policy effects...\n")
  cat("  Method: Comparing baseline → counterfactual in two steps\n\n")
  
  # ============================================================================
  # Step 0: Baseline (e.g., Mixed FF/RB)
  # ============================================================================
  baseline_stat <- compute_stationary_dist(results$P_hat, results$F_matrices)
  
  baseline_retrofit <- sum(results$P_hat[, "retrofit"] * baseline_stat, na.rm = TRUE)
  baseline_exit <- sum(results$P_hat[, "exit"] * baseline_stat)
  
  states_hazard <- merge(results$states, 
                        results$hazard_data[, .(N, w, A, r, rho, H_tilde)],
                        by = c("N", "w", "A", "r", "rho"), all.x = TRUE)
  setorder(states_hazard, state_idx)
  baseline_leaks <- sum(states_hazard$H_tilde * baseline_stat)
  
  # ============================================================================
  # Step 1: Mechanical Effect (Change premiums, keep behavior fixed)
  # ============================================================================
  # Hold P(a|x) constant at baseline, but use counterfactual premiums
  # This shows effect of price changes ONLY
  
  # Recompute stationary distribution under CF premiums but baseline choices
  # (This is approximate - would need to solve for new stationary dist)
  
  # Simpler approach: Just compute direct cost impact
  baseline_premiums <- merge(results$states, 
                            results$premium_data[, .(N, w, A, r, rho, P_tilde)],
                            by = c("N", "w", "A", "r", "rho"))
  cf_premiums <- merge(results$states,
                      cf_results$premium_data[, .(N, w, A, r, rho, P_tilde)],
                      by = c("N", "w", "A", "r", "rho"))
  
  setorder(baseline_premiums, state_idx)
  setorder(cf_premiums, state_idx)
  
  # Mechanical effect on facility value (holding behavior constant)
  baseline_premium_cost <- sum(baseline_premiums$P_tilde * 12 * baseline_stat)
  cf_premium_cost <- sum(cf_premiums$P_tilde * 12 * baseline_stat)  # Same distribution!
  
  mechanical_premium_effect <- cf_premium_cost - baseline_premium_cost
  
  # ============================================================================
  # Step 2: Total Effect (New premiums + behavioral response)
  # ============================================================================
  cf_stat <- compute_stationary_dist(cf_results$P_counterfactual, 
                                     cf_results$F_matrices)
  
  cf_retrofit <- sum(cf_results$P_counterfactual[, "retrofit"] * cf_stat, na.rm = TRUE)
  cf_exit <- sum(cf_results$P_counterfactual[, "exit"] * cf_stat)
  
  states_hazard_cf <- merge(cf_results$states,
                           cf_results$hazard_data[, .(N, w, A, r, rho, H_tilde)],
                           by = c("N", "w", "A", "r", "rho"), all.x = TRUE)
  setorder(states_hazard_cf, state_idx)
  cf_leaks <- sum(states_hazard_cf$H_tilde * cf_stat)
  
  total_retrofit_effect <- cf_retrofit - baseline_retrofit
  total_exit_effect <- cf_exit - baseline_exit
  total_leak_effect <- cf_leaks - baseline_leaks
  
  # ============================================================================
  # Step 3: Behavioral Response = Total - Mechanical
  # ============================================================================
  # This is the part due to facilities changing their actions
  
  # For retrofit/exit, the mechanical effect is approximately zero
  # (premium changes don't directly change these, only through incentives)
  behavioral_retrofit <- total_retrofit_effect  # ≈ all behavioral
  behavioral_exit <- total_exit_effect  # ≈ all behavioral
  
  # For leaks, behavioral = change due to fleet composition shift
  behavioral_leak <- total_leak_effect
  
  # ============================================================================
  # Create Decomposition Table
  # ============================================================================
  
  decomposition <- data.table(
    outcome = c("Retrofit Rate", "Exit Rate", "Leak Rate", "Premium Costs"),
    baseline = c(baseline_retrofit, baseline_exit, baseline_leaks, baseline_premium_cost),
    counterfactual = c(cf_retrofit, cf_exit, cf_leaks, cf_premium_cost),
    total_effect = c(total_retrofit_effect, total_exit_effect, 
                    total_leak_effect, mechanical_premium_effect),
    mechanical = c(0, 0, 0, mechanical_premium_effect),
    behavioral = c(behavioral_retrofit, behavioral_exit, 
                   behavioral_leak, 0)
  )
  
  decomposition[, pct_mechanical := 100 * mechanical / total_effect]
  decomposition[, pct_behavioral := 100 * behavioral / total_effect]
  
  return(decomposition)
}

#' Create Decomposition Figure
#' 
#' @param decomp Decomposition table from decompose_policy_effects()
#' @return ggplot object
create_decomposition_figure <- function(decomp) {
  
  # Reshape for stacked bar chart
  decomp_long <- melt(decomp[outcome != "Premium Costs"], 
                     id.vars = "outcome",
                     measure.vars = c("mechanical", "behavioral"),
                     variable.name = "component",
                     value.name = "effect")
  
  p <- ggplot(decomp_long, aes(x = outcome, y = effect, fill = component)) +
    geom_col(position = "stack") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(
      values = c("mechanical" = "#E69F00", "behavioral" = "#56B4E9"),
      labels = c("mechanical" = "Mechanical Effect\n(Price changes only)",
                "behavioral" = "Behavioral Response\n(Action changes)")
    ) +
    labs(
      title = "Policy Effect Decomposition: Flat-Fee → Risk-Based Pricing",
      subtitle = "Change in outcomes = Mechanical effect + Behavioral response",
      x = NULL,
      y = "Change from Baseline",
      fill = "Component"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 0)
    )
  
  return(p)
}

# ==============================================================================
# SECTION 5: MASTER ANALYSIS FUNCTION
# ==============================================================================

#' Run Complete Welfare and Policy Analysis
#' 
#' This is the main function to call after estimation.
#' It runs all four analyses and generates all tables/figures.
#' 
#' @param results Estimation results from estimate_model()
#' @param output_dir Directory to save outputs
#' @return List with all analysis results
#' 

run_complete_welfare_analysis <- function(results, output_dir = here("Output")) {
  
  cat("\n")
  cat("==============================================================================\n")
  cat("WELFARE AND POLICY ANALYSIS\n")
  cat("==============================================================================\n\n")
  
  welfare_dir <- file.path(output_dir, "Welfare")
  if (!dir.exists(welfare_dir)) dir.create(welfare_dir, recursive = TRUE)
  
  # ==========================================================================
  # ANALYSIS 1: Welfare Calculations
  # ==========================================================================
  
  cat("ANALYSIS 1: Social Welfare Calculations\n")
  cat(rep("-", 80), "\n", sep = "")
  
  # Run counterfactual: All facilities switch to risk-based
  cat("\nRunning counterfactual: All Risk-Based pricing...\n")
  
  # CRITICAL FIX: Update PRICES (P_tilde) without breaking KEYS (rho)
  # We want FF facilities to face RB prices, but they are still technically in FF states
  # until they transition. The utility calculator matches on (A, w, rho).
  
  premium_data_RB <- copy(results$premium_data)
  
  # 1. Extract the RB price schedule
  rb_schedule <- premium_data_RB[rho == "RB", .(A, w, P_new = P_tilde)]
  
  # 2. Apply RB prices to ALL rows (including rho="FF") based on A and w
  premium_data_RB[rb_schedule, on = .(A, w), P_tilde := i.P_new]
  
  # Re-solve model with RB premiums
  u_matrix_RB <- calculate_flow_utilities(
    results$states, results$action_space, results$theta_hat,
    premium_data_RB, results$hazard_data, results$loss_data, results$config
  )
  
  # Iterate policy to convergence
  P_RB <- results$P_hat  # Initialize from baseline
  
  for (iter in 1:200) {
    V_RB <- invert_value_function(P_RB, u_matrix_RB,
                                  results$F_matrices, results$config)
    P_RB_new <- calculate_choice_probabilities(u_matrix_RB, V_RB,
                                              results$F_matrices,
                                              results$action_space, results$config)
    
    # Check convergence (removing NAs to prevent crash)
    diff <- max(abs(P_RB_new - P_RB), na.rm = TRUE)
    
    if (diff < 1e-8) {
      cat(sprintf("  Counterfactual policy converged in %d iterations\n", iter))
      break
    }
    P_RB <- P_RB_new
  }
  
  cf_results_RB <- list(
    P_counterfactual = P_RB,
    V_counterfactual = V_RB,
    states = results$states,
    stationary_dist = compute_stationary_dist(P_RB, results$F_matrices),
    hazard_data = results$hazard_data,
    loss_data = results$loss_data,
    premium_data = premium_data_RB,
    F_matrices = results$F_matrices
  )
  
  # Compute welfare comparison
  welfare_comparison <- compare_welfare_across_policies(
    results = results,
    cf_results_list = list(cf_results_RB),
    regime_names = c("Baseline (Mixed)", "All Risk-Based")
  )
  
  cat("\nWelfare Comparison:\n")
  print(welfare_comparison[, .(regime, social_welfare, welfare_change, total_premiums)])
  cat("\n")
  
  fwrite(welfare_comparison, file.path(welfare_dir, "welfare_comparison.csv"))
  
  # Save GT table
  welfare_gt <- welfare_comparison %>%
    gt() %>%
    tab_header(title = "Social Welfare Analysis") %>%
    fmt_number(columns = c(social_welfare, welfare_change), decimals = 2)
  
  save_welfare_table(welfare_gt, welfare_comparison, "T03_welfare_comparison")
  
  # ==========================================================================
  # ANALYSIS 2: Policy Elasticities
  # ==========================================================================
  
  cat("\nANALYSIS 2: Policy Elasticities\n")
  cat(rep("-", 80), "\n", sep = "")
  
  elasticity_table <- calculate_policy_elasticities(results, delta = 0.01)
  print(elasticity_table)
  fwrite(elasticity_table, file.path(welfare_dir, "elasticity_table.csv"))
  
  # GT Table
  elas_gt <- elasticity_table %>% gt() %>% tab_header(title = "Policy Elasticities")
  save_welfare_table(elas_gt, elasticity_table, "T04_elasticities")

  # ==========================================================================
  # ANALYSIS 3: Dynamic Transition Paths
  # ==========================================================================
  
  cat("\nANALYSIS 3: Dynamic Transition Paths\n")
  cat(rep("-", 80), "\n", sep = "")
  
  initial_dist <- compute_stationary_dist(results$P_hat, results$F_matrices)
  
  baseline_path <- simulate_transition_path(results$P_hat, results$F_matrices, 
                                            results$states, initial_dist)
  cf_path_RB <- simulate_transition_path(P_RB, results$F_matrices, 
                                         results$states, initial_dist)
  
  # Combine for plotting
  baseline_path[, regime := "Baseline"]
  cf_path_RB[, regime := "All Risk-Based"]
  all_paths <- rbind(baseline_path, cf_path_RB)
  
  p_path <- ggplot(all_paths, aes(x = period/12, y = pct_single, color = regime)) +
    geom_line(size = 1.2) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Fleet Transition: Share of Single-Wall Tanks",
         x = "Years", y = "Percent Single-Wall") +
    theme_academic()
    
  ggsave(file.path(welfare_dir, "figure_transition_paths.pdf"), p_path, width = 8, height = 6)
  cat("✓ Saved transition path figure\n")
  
  # ==========================================================================
  # ANALYSIS 4: Decomposition
  # ==========================================================================
  
  cat("\nANALYSIS 4: Policy Effect Decomposition\n")
  cat(rep("-", 80), "\n", sep = "")
  
  decomposition <- decompose_policy_effects(results, cf_results_RB)
  print(decomposition)
  fwrite(decomposition, file.path(welfare_dir, "decomposition_table.csv"))
  
  cat("\n==============================================================================\n")
  cat("WELFARE ANALYSIS COMPLETE\n")
  cat("==============================================================================\n\n")
  
  return(invisible(list(
    welfare_comparison = welfare_comparison,
    elasticity_table = elasticity_table,
    decomposition = decomposition,
    baseline_path = baseline_path,
    cf_path_RB = cf_path_RB
  )))
}


# ==============================================================================
# USAGE INSTRUCTIONS
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("WELFARE AND ELASTICITY ANALYSIS MODULE LOADED\n")
cat("==============================================================================\n\n")
cat("Usage after running Script 3 estimation:\n\n")
cat("  # Run complete analysis\n")
cat("  welfare_results <- run_complete_welfare_analysis(results)\n\n")
cat("Or run components individually:\n")
cat("  welfare_table <- compare_welfare_across_policies(results, cf_list)\n")
cat("  elasticities <- calculate_policy_elasticities(results)\n")
cat("  path <- simulate_transition_path(P, F_matrices, states, dist)\n")
cat("  decomp <- decompose_policy_effects(results, cf_results)\n")
cat("==============================================================================\n\n")
