# ==============================================================================
# improved_generator_OPTIMIZED.r - PERFORMANCE-ENHANCED DATA GENERATION
# ==============================================================================
# PURPOSE: Generate synthetic panel data with Rcpp acceleration
#
# KEY OPTIMIZATIONS:
#   - Rcpp implementation for simulation loop
#   - Vectorized state transition maps
#   - Pre-computed transition lookups
#   - Reduced cache lookups in hot loops
#
# DEPENDENCIES: improved_estimator_OPTIMIZED.r, structural_estimation.cpp
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(here)
  library(Rcpp)
})

# ==============================================================================
# 1. SOURCE PRODUCTION ESTIMATOR
# ==============================================================================

source(here("Code", "Public_to_Private", "improved_estimator_OPTIMIZED.r"))

# Rcpp::sourceCpp(here("src", "structural_estimation.cpp"))

cat("✓ Sourced optimized estimator functions.\n")

# ==============================================================================
# 2. ENVIRONMENT & PHYSICS
# ==============================================================================

set_exogenous_parameters <- function() {
  list(h0=0.02, h_single=0.09, h_age=0.008, ell0=1.0, ell_age=0.08,
       p_FF_annual=0.08, p0_RB_annual=0.03, p_single_RB_annual=0.1, 
       p_age_RB_annual=0.0055)
}

set_age_transitions <- function() {
  list(p_stay=c(0.985, 0.982, 0.978, 0.974, 0.970, 0.965, 0.960, 0.955, 0.950, 1.00),
       p_up=c(0.015, 0.018, 0.022, 0.026, 0.030, 0.035, 0.040, 0.045, 0.050, 0.00))
}

create_transition_matrices <- function(states, age_probs) {
  n <- nrow(states)
  keys <- paste(states$A, states$w, states$rho, sep="_")
  state_map <- setNames(states$state_idx, keys)
  
  trans <- list(maintain=Matrix(0,n,n,sparse=TRUE), exit=Diagonal(n), retrofit=Matrix(0,n,n,sparse=TRUE))
  
  for(i in 1:n) {
    if(states$A[i] < 10) {
      k_s <- paste(states$A[i], states$w[i], states$rho[i], sep="_")
      k_u <- paste(states$A[i]+1, states$w[i], states$rho[i], sep="_")
      trans$maintain[i, state_map[k_s]] <- age_probs$p_stay[states$A[i]]
      trans$maintain[i, state_map[k_u]] <- age_probs$p_up[states$A[i]]
    } else {
      trans$maintain[i,i] <- 1.0
    }
  }
  
  idx_s <- which(states$w=="single")
  if(length(idx_s)>0) {
    k_d <- paste(1, "double", states$rho[idx_s], sep="_")
    trans$retrofit[idx_s, state_map[k_d]] <- 1.0
  }
  trans
}

calculate_hazard_rates <- function(states, params) {
  h <- params$h0 + (states$w == "single")*params$h_single + params$h_age*states$A
  pmin(pmax(h, 0.001), 0.50)
}

calculate_expected_losses <- function(states, params) { 
  pmax(params$ell0 + params$ell_age*states$A, 0.1) 
}

calculate_premiums <- function(states, params) {
  P_RB <- params$p0_RB_annual + (states$w=="single")*params$p_single_RB_annual + params$p_age_RB_annual*states$A
  ifelse(states$rho == "FF", params$p_FF_annual, P_RB)
}

# ==============================================================================
# 3. SOLVER WRAPPER
# ==============================================================================

solve_equilibrium_policy <- function(theta_vec, sigma1, model_type, cache, config, 
                                     profit_mult=1.0) {
  
  U <- calculate_flow_utilities_fast(theta_vec, cache, profit_mult)
  
  P <- matrix(0, cache$n_states, 3, dimnames=list(NULL, c("maintain", "exit", "retrofit")))
  for(i in 1:cache$n_states) P[i, cache$feasibility[i,]] <- 1/sum(cache$feasibility[i,])
  
  tol <- 1e-6
  max_iter <- 1000
  
  if (model_type == "standard") {
    for (iter in 1:max_iter) {
      V <- invert_value_function_standard(P, U, config)
      P_new <- compute_ccps_standard(U, V, cache, config)
      
      if (max(abs(P_new - P)) < tol) break
      P <- P_new
    }
    
    V_final <- invert_value_function_standard(P, U, config)
    
  } else if (model_type == "nested") {
    for (iter in 1:max_iter) {
      V <- invert_value_function_nested(P, U, sigma1, config)
      P_new <- compute_ccps_nested_forward(U, V, cache, config, sigma1)
      
      if (max(abs(P_new - P)) < tol) break
      P <- P_new
    }
    
    V_final <- invert_value_function_nested(P, U, sigma1, config)
    
  } else {
    stop("Unknown model_type. Choose 'standard' or 'nested'.")
  }
  
  return(list(P = P, V = V_final))
}

# ==============================================================================
# 4. PRE-BUILD TRANSITION MAPS (OPTIMIZATION)
# ==============================================================================

build_transition_maps <- function(states, cache) {
  n_states <- nrow(states)
  next_state_maintain <- integer(n_states)
  next_state_retrofit <- integer(n_states)
  
  for (s in 1:n_states) {
    curr_state <- states[s, ]
    
    # Maintain transition: age+1, same wall, same regime (deterministic)
    next_age <- min(curr_state$A + 1, 10)
    key_maintain <- paste(next_age, curr_state$w, curr_state$rho, sep="_")
    next_state_maintain[s] <- cache$state_map[key_maintain]
    
    # Retrofit transition: age=1, double, same regime
    if (curr_state$w == "single") {
      key_retrofit <- paste(1, "double", curr_state$rho, sep="_")
      next_state_retrofit[s] <- cache$state_map[key_retrofit]
    } else {
      next_state_retrofit[s] <- NA  # Not valid from double-walled
    }
  }
  
  list(
    next_state_maintain = next_state_maintain,
    next_state_retrofit = next_state_retrofit
  )
}

# ==============================================================================
# 5. MAIN DATA GENERATION FUNCTION (WITH RCPP OPTION)
# ==============================================================================

generate_production_data <- function(N_facilities, T_periods, model_type = "standard",
                                     phi_true = NULL, kappa_true = NULL, sigma1_true = NULL,
                                     K = NULL, phi_mix = NULL, kappa_mix = NULL, 
                                     pi_weights = NULL, type_profit_mult = NULL,
                                     seed = NULL, use_rcpp = TRUE) {
  
  if (!is.null(seed)) set.seed(seed)
  
  config <- create_estimation_config()
  states <- create_state_space_est()
  n_states <- nrow(states)
  
  params <- set_exogenous_parameters()
  age_probs <- set_age_transitions()
  
  premiums <- calculate_premiums(states, params)
  hazards <- calculate_hazard_rates(states, params)
  losses <- calculate_expected_losses(states, params)
  transitions <- create_transition_matrices(states, age_probs)
  
  cache <- create_estimation_cache(states, premiums, hazards, losses, transitions, config)
  
  # Build transition maps for fast lookup
  trans_maps <- build_transition_maps(states, cache)
  
  # A. Determine Model Configuration
  if (model_type %in% c("standard", "nested")) {
    K <- 1
    if (is.null(phi_true)) phi_true <- 0.5
    if (is.null(kappa_true)) kappa_true <- 1.0
    phi_vec <- phi_true
    kappa_vec <- kappa_true
    pi_weights <- 1.0
    if (is.null(type_profit_mult)) type_profit_mult <- 1.0
    
  } else if (model_type %in% c("mix_standard", "mix_nested")) {
    if (is.null(K)) stop("K must be specified for mixture models")
    if (is.null(phi_mix)) stop("phi_mix must be specified")
    if (is.null(kappa_mix)) stop("kappa_mix must be specified")
    if (is.null(pi_weights)) pi_weights <- rep(1/K, K)
    if (is.null(type_profit_mult)) type_profit_mult <- rep(1.0, K)
    
    phi_vec <- phi_mix
    kappa_vec <- kappa_mix
    
  } else {
    stop("Invalid model_type")
  }
  
  # B. Assign Types
  true_types <- sample(1:K, N_facilities, replace = TRUE, prob = pi_weights)
  
  # C. Solve Equilibrium for Each Type
  sol_list <- vector("list", K)
  
  for (k in 1:K) {
    theta_k <- c(phi_tilde = phi_vec[k], kappa_tilde = kappa_vec[k])
    
    if (model_type %in% c("nested", "mix_nested")) {
      if (is.null(sigma1_true)) sigma1_true <- 0.5
      sol_list[[k]] <- solve_equilibrium_policy(theta_k, sigma1_true, "nested", 
                                                 cache, config, type_profit_mult[k])
    } else {
      sol_list[[k]] <- solve_equilibrium_policy(theta_k, NULL, "standard", 
                                                 cache, config, type_profit_mult[k])
    }
  }
  
  # D. Initial States (random)
  initial_states <- sample(1:n_states, N_facilities, replace = TRUE)
  facility_types <- true_types
  
  # E. Simulate Panel Data
  if (use_rcpp && exists("simulate_panel_cpp")) {
    # Use Rcpp version
    P_list_R <- lapply(sol_list, function(x) x$P)
    
    panel_list <- simulate_panel_cpp(
      initial_states = as.integer(initial_states),
      facility_types = as.integer(facility_types),
      P_list = P_list_R,
      next_state_maintain = as.integer(trans_maps$next_state_maintain),
      next_state_retrofit = as.integer(trans_maps$next_state_retrofit),
      T_periods = as.integer(T_periods),
      seed = if (is.null(seed)) sample.int(1e6, 1) else as.integer(seed)
    )
    
    full_panel <- data.table(
      facility_id = panel_list$facility_id,
      period = panel_list$period,
      state_idx = panel_list$state_idx,
      action = c("maintain", "exit", "retrofit")[panel_list$action],
      true_type = panel_list$true_type
    )
    
  } else {
    # R fallback (vectorized as much as possible)
    current_states <- initial_states
    active_mask <- rep(TRUE, N_facilities)
    
    panel_dt_list <- vector("list", T_periods)
    
    for (t in 1:T_periods) {
      if (sum(active_mask) == 0) break
      
      current_ids <- which(active_mask)
      n_active <- length(current_ids)
      
      actions_t <- integer(n_active)
      
      for (k in 1:K) {
        type_mask <- (true_types[current_ids] == k)
        if (!any(type_mask)) next
        
        idx_subset <- current_ids[type_mask]
        state_idx_subset <- current_states[idx_subset]
        
        P_k <- sol_list[[k]]$P
        probs <- P_k[state_idx_subset, , drop=FALSE]
        
        u <- runif(length(idx_subset))
        act_k <- ifelse(u < probs[,1], 1L,
                        ifelse(u < probs[,1] + probs[,2], 2L, 3L))
        
        actions_t[type_mask] <- act_k
      }
      
      action_labels <- c("maintain", "exit", "retrofit")[actions_t]
      
      panel_dt_list[[t]] <- data.table(
        facility_id = current_ids,
        period = t,
        state_idx = current_states[current_ids],
        action = action_labels,
        true_type = true_types[current_ids]
      )
      
      # Vectorized state updates
      exited_local_idx <- which(actions_t == 2L)
      if (length(exited_local_idx) > 0) {
        exited_ids <- current_ids[exited_local_idx]
        active_mask[exited_ids] <- FALSE
      }
      
      maint_local_idx <- which(actions_t == 1L)
      if (length(maint_local_idx) > 0) {
        ids_m <- current_ids[maint_local_idx]
        current_states[ids_m] <- trans_maps$next_state_maintain[current_states[ids_m]]
      }
      
      retro_local_idx <- which(actions_t == 3L)
      if (length(retro_local_idx) > 0) {
        ids_r <- current_ids[retro_local_idx]
        current_states[ids_r] <- trans_maps$next_state_retrofit[current_states[ids_r]]
      }
    }
    
    full_panel <- rbindlist(panel_dt_list)
  }
  
  # G. Create Counts Vector
  counts_tab <- table(factor(full_panel$state_idx, levels=1:cache$n_states), 
                      factor(full_panel$action, levels=c("maintain", "exit", "retrofit")))
  counts_vec <- as.vector(counts_tab)
  
  cat(sprintf("Generated %s model (%d facilities). Exit Rate: %.1f%%\n", 
              model_type, N_facilities, 100*mean(full_panel$action == "exit")))
  
  return(list(
    panel = full_panel,
    counts_vec = counts_vec,
    states = states,
    premiums = premiums,
    hazards = hazards,
    losses = losses,
    transitions = transitions,
    config = config,
    true_params = list(phi=phi_true, kappa=kappa_true, sigma1=sigma1_true,
                       phi_mix=phi_mix, kappa_mix=kappa_mix, pi=pi_weights),
    trans_maps = trans_maps
  ))
}

# ==============================================================================
# 6. DIAGNOSTIC RUN
# ==============================================================================

cat("\n--- RUNNING DIAGNOSTIC TEST ---\n")

test_sim_nested <- generate_production_data(
  N_facilities = 400,
  T_periods = 500,
  model_type = "nested",
  phi_true = 3.5,
  kappa_true = 75,
  sigma1_true = .5,
  use_rcpp = FALSE  # Test R fallback
)

cat("Action Shares:\n")
print(round(prop.table(table(test_sim_nested$panel$action)), 4))

test_sim_standard <- generate_production_data(
  N_facilities = 400,
  T_periods = 500,
  model_type = "standard",
  phi_true = 3.5,
  kappa_true = 75,
  use_rcpp = FALSE
)

cat("Action Shares:\n")
print(round(prop.table(table(test_sim_standard$panel$action)), 4))

cat("\n--- TESTING STANDARD MIXTURE ---\n")
test_sim_mix <- generate_production_data(
  N_facilities = 400,
  T_periods = 500,
  model_type = "mix_standard",
  K = 2,
  phi_mix = c(3.5, 4.5),
  kappa_mix = c(69, 75),
  pi_weights = c(0.6, 0.4),
  type_profit_mult = c(1.0, 1.0),
  use_rcpp = FALSE
)

cat("Aggregate Action Shares:\n")
print(round(prop.table(table(test_sim_mix$panel$action)), 4))
cat("Action Shares by True Type:\n")
print(round(prop.table(table(test_sim_mix$panel$true_type, test_sim_mix$panel$action), 1), 4))

cat("\n--- TESTING NESTED MIXTURE ---\n")
test_sim_mix_nst <- generate_production_data(
  N_facilities = 400,
  T_periods = 500,
  model_type = "mix_nested",
  K = 2,
  phi_mix = c(3.5, 4.5),
  kappa_mix = c(69, 75),
  pi_weights = c(0.6, 0.4),
  sigma1_true = 0.7,
  type_profit_mult = c(1.0, 1.0),
  use_rcpp = FALSE
)

cat("Aggregate Action Shares:\n")
print(round(prop.table(table(test_sim_mix_nst$panel$action)), 4))
cat("Action Shares by True Type:\n")
print(round(prop.table(table(test_sim_mix_nst$panel$true_type, test_sim_mix_nst$panel$action), 1), 4))

cat("\n✓ improved_generator_OPTIMIZED.r loaded\n")
cat("  - Vectorized transition maps\n")
cat("  - Rcpp simulation ready (use_rcpp=TRUE)\n")
cat("  - Reduced cache lookups\n")
