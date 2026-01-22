# ==============================================================================
# improved_generator_FINAL.r - FULLY CORRECTED DATA GENERATION
# ==============================================================================
# PURPOSE: Generate synthetic panel data with all audit fixes applied
#
# FIXES APPLIED:
#   - Bellman residual check in policy iteration
#   - Sparse matrices maintained throughout (no densification)
#   - Explicit failure point comments for interactive debugging
#   - All trycatch blocks removed
#
# DEPENDENCIES: improved_estimator_FINAL.r, structural_estimation.cpp
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(here)
})

# ==============================================================================
# 1. SOURCE PRODUCTION ESTIMATOR
# ==============================================================================

# POTENTIAL FAILURE: Check that this path is correct for your system
# If file not found, you'll get an error immediately
source(here("Code", "Public_to_Private", "improved_estimator_OPTIMIZED.r"))

# POTENTIAL FAILURE: Rcpp compilation may fail if:
#   - No C++ compiler installed (need Rtools on Windows, gcc on Linux)
#   - RcppArmadillo package not installed
#   - Incompatible Rcpp/Matrix versions
# If compilation fails, simulation will use R fallback (slower but functional)
if (requireNamespace("Rcpp", quietly = TRUE)) {
  # User should uncomment and set correct path:
  # Rcpp::sourceCpp(here("Code", "Public_to_Private", "structural_estimation.cpp"))
  
  if (exists("simulate_panel_cpp", mode = "function")) {
    cat("✓ Rcpp simulation functions available\n")
  } else {
    cat("ℹ Rcpp simulation not loaded - will use R fallback\n")
  }
}

cat("✓ Sourced estimator functions.\n")

# ==============================================================================
# 2. ENVIRONMENT & PHYSICS
# ==============================================================================

set_exogenous_parameters <- function() {
  list(h0=0.02, h_single=0.09, h_age=0.008, ell0=1.0, ell_age=0.08,
       p_FF_annual=0.08, p0_RB_annual=0.03, p_single_RB_annual=0.1, 
       p_age_RB_annual=0.0055)
}

set_age_transitions <- function() {
  # NOTE: These must sum to 1.0 for ages 1-9
  # p_stay[i] + p_up[i] = 1.0 for i in 1:9
  list(p_stay=c(0.985, 0.982, 0.978, 0.974, 0.970, 0.965, 0.960, 0.955, 0.950, 1.00),
       p_up=c(0.015, 0.018, 0.022, 0.026, 0.030, 0.035, 0.040, 0.045, 0.050, 0.00))
}

create_transition_matrices <- function(states, age_probs) {
  n <- nrow(states)
  keys <- paste(states$A, states$w, states$rho, sep="_")
  state_map <- setNames(states$state_idx, keys)
  
  # FIX: Keep as sparse matrices throughout
  trans <- list(
    maintain = Matrix(0, n, n, sparse = TRUE), 
    exit = Diagonal(n), 
    retrofit = Matrix(0, n, n, sparse = TRUE)
  )
  
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
  
  # CORRECTED: Retrofit transitions apply to ALL states (production readiness)
  # All states retrofit to (1, double, rho) - regime is preserved
  for(i in 1:n) {
    k_retrofit <- paste(1, "double", states$rho[i], sep="_")
    trans$retrofit[i, state_map[k_retrofit]] <- 1.0
  }
  
  return(trans)
}

calculate_hazard_rates <- function(states, params) {
  h <- params$h0 + (states$w == "single")*params$h_single + params$h_age*states$A
  # Clip to reasonable bounds to prevent numerical issues
  pmin(pmax(h, 0.001), 0.50)
}

calculate_expected_losses <- function(states, params) { 
  # Floor at 0.1 to prevent negative or zero losses
  pmax(params$ell0 + params$ell_age*states$A, 0.1) 
}

calculate_premiums <- function(states, params) {
  base <- params$p_FF_annual
  regime_adj <- ifelse(states$rho == "RB", params$p0_RB_annual, 0)
  wall_adj <- ifelse(states$w == "single", params$p_single_RB_annual, 0)
  age_adj <- (states$A - 1) * params$p_age_RB_annual
  
  annual_premiums <- base + regime_adj + wall_adj + age_adj
  return(annual_premiums)
}
# ==============================================================================
# 3. SOLVER WRAPPER WITH BELLMAN RESIDUAL CHECK
# ==============================================================================

solve_equilibrium_policy <- function(theta_vec, sigma1, model_type, cache, config, 
                                     profit_mult=1.0, check_bellman=TRUE) {
  
  # POTENTIAL FAILURE: theta_vec must have names "phi_tilde" and "kappa_tilde"
  # If unnamed, calculation_flow_utilities_fast will fail
  U <- calculate_flow_utilities_fast(theta_vec, cache, profit_mult)
  
  # Initialize uniform probabilities over feasible actions
  P <- matrix(0, cache$n_states, 3, dimnames=list(NULL, c("maintain", "exit", "retrofit")))
  for(i in 1:cache$n_states) P[i, cache$feasibility[i,]] <- 1/sum(cache$feasibility[i,])
  
  tol <- 1e-6
  max_iter <- 5000
  
  # Normalize model_type (mixture models use underlying structure)
  base_model <- if (grepl("nested", model_type)) "nested" else "standard"
  
  if (base_model == "standard") {
    for (iter in 1:max_iter) {
      V_old <- invert_value_function_standard(P, U, config)
      P_new <- compute_ccps_standard(U, V_old, cache, config)
      
      # FIX: ADD BELLMAN RESIDUAL CHECK
      if (check_bellman) {
        V_new <- invert_value_function_standard(P_new, U, config)
        bellman_error <- max(abs(V_new - V_old))
      }
      
      # Check CCP convergence
      diff <- abs(P_new - P)
      diff[!is.finite(diff)] <- 0
      ccp_converged <- max(diff, na.rm = TRUE) < tol
      
      # Check Bellman convergence
      bellman_converged <- if (check_bellman) bellman_error < tol else TRUE
      
      if (ccp_converged && bellman_converged) break
      
      P <- P_new
    }
    
    V_final <- invert_value_function_standard(P, U, config)
    
  } else if (base_model == "nested") {
    # POTENTIAL FAILURE: sigma1 must be positive and within bounds
    # If sigma1 <= 0, inclusive value computation will fail
    if (is.null(sigma1) || sigma1 <= 0) {
      stop("sigma1 must be positive for nested logit model")
    }
    
    for (iter in 1:max_iter) {
      V_old <- invert_value_function_nested(P, U, sigma1, config)
      P_new <- compute_ccps_nested_forward(U, V_old, cache, config, sigma1)
      
      # FIX: ADD BELLMAN RESIDUAL CHECK
      if (check_bellman) {
        V_new <- invert_value_function_nested(P_new, U, sigma1, config)
        bellman_error <- max(abs(V_new - V_old))
      }
      
      # Check CCP convergence
      diff <- abs(P_new - P)
      diff[!is.finite(diff)] <- 0
      ccp_converged <- max(diff, na.rm = TRUE) < tol
      
      # Check Bellman convergence
      bellman_converged <- if (check_bellman) bellman_error < tol else TRUE
      
      if (ccp_converged && bellman_converged) break
      
      P <- P_new
    }
    
    V_final <- invert_value_function_nested(P, U, sigma1, config)
    
  } else {
    stop("Unknown model_type. Choose 'standard', 'nested', 'mix_standard', or 'mix_nested'.")
  }
  
  # POTENTIAL FAILURE: If max_iter reached without convergence
  # This typically indicates numerical issues or misspecified parameters
  if (iter >= max_iter) {
    warning(sprintf("Policy iteration reached max_iter=%d without convergence", max_iter))
  }
  
  return(list(P = P, V = V_final, converged = (iter < max_iter)))
}


# ==============================================================================
# FINAL RUST-CONSISTENT TRANSITION MAP BUILDER
# ==============================================================================
build_transition_maps <- function() {
  # CORRECTED: State space is (A, w, rho) where:
  #   A: Age bins 1-10
  #   w: Wall type (single, double)
  #   rho: Policy regime (FF, RB) - FIXED, never changes
  
  # Define state space components
  A_vals <- 1:10
  w_types <- c("single", "double")
  rho_types <- c("FF", "RB")
  
  # Build full 3D state space: 10 × 2 × 2 = 40 states
  states <- data.table::CJ(A = A_vals, w = w_types, rho = rho_types, sorted = FALSE)
  states[, state_id := .I]
  
  # Build lookup map using standardized key format
  state_map <- list()
  for (i in seq_len(nrow(states))) {
    key <- paste(states$A[i], states$w[i], states$rho[i], sep = "_")
    state_map[[key]] <- states$state_id[i]
  }
  
  N_states <- nrow(states)
  
  # Initialize transition vectors
  next_state_stay     <- integer(N_states)
  next_state_up       <- integer(N_states)
  next_state_retrofit <- integer(N_states)
  
  # Loop across all states
  for (s in seq_len(N_states)) {
    A   <- states$A[s]
    w   <- states$w[s]
    rho <- states$rho[s]
    
    #--------------------------
    # 1. STAY: (A, w, rho) → (A, w, rho)
    #--------------------------
    key_stay <- paste(A, w, rho, sep = "_")
    next_state_stay[s] <- state_map[[key_stay]]
    
    #--------------------------
    # 2. AGE UP: (A, w, rho) → (A+1, w, rho)
    #    Capped at A=10
    #--------------------------
    A_next <- min(A + 1L, 10L)
    key_up <- paste(A_next, w, rho, sep = "_")
    next_state_up[s] <- state_map[[key_up]]
    
    #--------------------------
    # 3. RETROFIT: (A, w, rho) → (1, double, rho)
    #    CRITICAL: Regime is PRESERVED
    #    Applies to ALL states (production readiness)
    #--------------------------
    key_retrofit <- paste(1, "double", rho, sep = "_")
    next_state_retrofit[s] <- state_map[[key_retrofit]]
  }
  
  # Safety checks
  if (any(is.na(next_state_stay)) ||
      any(is.na(next_state_up)) ||
      any(is.na(next_state_retrofit))) {
    stop("Invalid transition map: NA values present")
  }
  
  # Return maps required by R + Rcpp
  list(
    next_state_maintain = next_state_up,   # C++ expects this name
    next_state_stay     = next_state_stay,
    next_state_up       = next_state_up,
    next_state_retrofit = next_state_retrofit,
    state_map           = state_map,
    states              = states,
    N_states            = N_states
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
trans_maps <- build_transition_maps()

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
    # POTENTIAL FAILURE: Must specify K, phi_mix, kappa_mix for mixtures
    if (is.null(K)) stop("K must be specified for mixture models")
    if (is.null(phi_mix)) stop("phi_mix must be specified for mixture models")
    if (is.null(kappa_mix)) stop("kappa_mix must be specified for mixture models")
    if (length(phi_mix) != K) stop(sprintf("phi_mix length (%d) must equal K (%d)", length(phi_mix), K))
    if (length(kappa_mix) != K) stop(sprintf("kappa_mix length (%d) must equal K (%d)", length(kappa_mix), K))
    
    if (is.null(pi_weights)) pi_weights <- rep(1/K, K)
    if (is.null(type_profit_mult)) type_profit_mult <- rep(1.0, K)
    
    # POTENTIAL FAILURE: pi_weights must sum to 1.0
    if (abs(sum(pi_weights) - 1.0) > 1e-10) {
      stop(sprintf("pi_weights must sum to 1.0 (currently sums to %.10f)", sum(pi_weights)))
    }
    
    phi_vec <- phi_mix
    kappa_vec <- kappa_mix
    
  } else {
    stop(sprintf("Invalid model_type: '%s'. Choose 'standard', 'nested', 'mix_standard', or 'mix_nested'.", 
                 model_type))
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
    
    # POTENTIAL FAILURE: Policy iteration may not converge
    # Check sol_list[[k]]$converged
    if (!sol_list[[k]]$converged) {
      warning(sprintf("Policy iteration for type %d did not converge", k))
    }
  }
  
  # D. Initial States (random)
  initial_states <- sample(1:n_states, N_facilities, replace = TRUE)
  facility_types <- true_types
  
  # E. Simulate Panel Data
  if (use_rcpp && exists("simulate_panel_cpp", mode = "function")) {
    # Use Rcpp version
    P_list_R <- lapply(sol_list, function(x) x$P)
    
    # CORRECTED: Construct probs_up vector aligned to state space
    # Map age-based probabilities to each state based on its A component
    n_states <- trans_maps$N_states
    probs_up_vec <- numeric(n_states)
    
    for (i in 1:n_states) {
      age_idx <- trans_maps$states$A[i]  # Extract age from state
      probs_up_vec[i] <- age_probs$p_up[age_idx]
    }
    
    # POTENTIAL FAILURE: Rcpp function may crash if:
    #   - Integer overflow (very large N_facilities or T_periods)
    #   - Invalid state indices in transition maps
    #   - P_list contains NaN or Inf values
    panel_list <- simulate_panel_cpp(
      initial_states = as.integer(initial_states),
      facility_types = as.integer(facility_types),
      P_list = P_list_R,
      next_state_maintain = as.integer(trans_maps$next_state_maintain),
      next_state_stay = as.integer(trans_maps$next_state_stay),  # NEW: Stochastic aging
      probs_up = as.numeric(probs_up_vec),  # NEW: Age transition probabilities
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
    if (use_rcpp) {
      cat("ℹ simulate_panel_cpp not available, using R fallback\n")
    }
    
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
        
        # POTENTIAL FAILURE: If probs contain NaN or don't sum to 1
        # This indicates numerical issues in CCP computation
        if (any(is.nan(probs))) {
          stop(sprintf("NaN in CCPs for type %d at period %d", k, t))
        }
        
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
        current_indices <- current_states[ids_m]
        
        # Get current ages to determine probabilities
        curr_ages <- states$A[current_indices]
        
        # Look up p_up for these ages
        # age_probs$p_up is a vector where index = age
        probs_up <- age_probs$p_up[curr_ages]
        
        # Coin flip
        is_aging <- runif(length(ids_m)) < probs_up
        
        # Assign new state based on flip
        # If aging: use "up" map. If not: use "stay" map.
        new_states <- ifelse(is_aging, 
                             trans_maps$next_state_up[current_indices],
                             trans_maps$next_state_stay[current_indices])
                             
        current_states[ids_m] <- new_states
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
  
  cat(sprintf("Generated %s model (%d facilities, %d periods). Exit Rate: %.1f%%\n", 
              model_type, N_facilities, T_periods, 100*mean(full_panel$action == "exit")))
  
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
# # ==============================================================================
# #  DIAGNOSTICS FOR ALL NPL MODEL TYPES
# # ==============================================================================

# cat("\n============================================================\n")
# cat("        NPL MODEL DATA-GENERATION DIAGNOSTICS\n")
# cat("============================================================\n")

# # --------------------------------------------------------------
# # 1. STANDARD MODEL — R version
# # --------------------------------------------------------------
# cat("\n--- TESTING STANDARD MODEL (R VERSION) ---\n")

# test_standard_R <- generate_production_data(
#   N_facilities = 400,
#   T_periods   = 500,
#   model_type  = "standard",
#   phi_true    = .5,
#   kappa_true  = 69,
#   use_rcpp    = FALSE,
#   seed        = 1111
# )

# cat("Action Shares:\n")
# print(round(prop.table(table(test_standard_R$panel$action)), 4))


# # --------------------------------------------------------------
# # 2. STANDARD MODEL — RCPP version
# # --------------------------------------------------------------
# cat("\n--- TESTING STANDARD MODEL (RCPP VERSION) ---\n")

# test_standard_RCPP <- generate_production_data(
#   N_facilities = 400,
#   T_periods   = 500,
#   model_type  = "standard",
#   phi_true    = .5,
#   kappa_true  = 69,
#   use_rcpp    = TRUE,
#   seed        = 1111
# )

# cat("Action Shares:\n")
# print(round(prop.table(table(test_standard_RCPP$panel$action)), 4))



# # --------------------------------------------------------------
# # 2. Nested MODEL — RCPP version
# # --------------------------------------------------------------
# cat("\n--- TESTING STANDARD MODEL (RCPP VERSION) ---\n")

test_nested_RCPP <- generate_production_data(
  N_facilities = 400,
  T_periods   = 500,
  model_type  = "nested",
  phi_true    = .5,
  kappa_true  = 91,
  sigma1_true = 1.5,
  use_rcpp    = TRUE,
  seed        = 1111
)

cat("Action Shares:\n")
print(round(prop.table(table(test_nested_RCPP$panel$action)), 4))



# # --------------------------------------------------------------
# # 3. STANDARD MIXTURE MODEL
# # --------------------------------------------------------------
# cat("\n--- TESTING STANDARD MIXTURE MODEL ---\n")

# test_mix <- generate_production_data(
#   N_facilities     = 400,
#   T_periods        = 500,
#   model_type       = "mix_standard",
#   K                = 2,
#   phi_mix          = c(.5, 1.5),
#   kappa_mix        = c(69, 75),
#   pi_weights       = c(0.6, 0.4),
#   type_profit_mult = c(1.0, 1.0),
#   use_rcpp         = FALSE
# )

# cat("Aggregate Action Shares:\n")
# print(round(prop.table(table(test_mix$panel$action)), 4))

# cat("Action Shares by True Type:\n")
# print(round(prop.table(table(test_mix$panel$true_type, 
#                              test_mix$panel$action), 1), 4))

                             
# test_mixrcp <- generate_production_data(
#   N_facilities     = 400,
#   T_periods        = 500,
#   model_type       = "mix_standard",
#   K                = 2,
#   phi_mix          = c(.5, 1.5),
#   kappa_mix        = c(69, 75),
#   pi_weights       = c(0.6, 0.4),
#   type_profit_mult = c(1.0, 1.0),
#   use_rcpp         = TRUE
# )

# cat("Aggregate Action Shares:\n")
# print(round(prop.table(table(test_mixrcp$panel$action)), 4))

# cat("Action Shares by True Type:\n")
# print(round(prop.table(table(test_mixrcp$panel$true_type, 
#                              test_mixrcp$panel$action), 1), 4))



# # --------------------------------------------------------------
# # 4. NESTED MIXTURE MODEL
# # --------------------------------------------------------------
# cat("\n--- TESTING NESTED MIXTURE MODEL ---\n")

# test_mix_nested <- generate_production_data(
#   N_facilities     = 400,
#   T_periods        = 500,
#   model_type       = "mix_nested",
#   K                = 2,
#   phi_mix          = c(.4, .75),
#   kappa_mix        = c(95, 82),
#   pi_weights       = c(0.6, 0.4),
#   sigma1_true      = 1.5,
#   type_profit_mult = c(1.0, 1.0),
#   use_rcpp         = FALSE,
#   seed = 12246
# )

# cat("Aggregate Action Shares:\n")
# print(round(prop.table(table(test_mix_nested$panel$action)), 4))

# cat("Action Shares by True Type:\n")
# print(round(prop.table(table(test_mix_nested$panel$true_type, 
#                              test_mix_nested$panel$action), 1), 4))


# cat("\n============================================================\n")
# cat("             ALL DIAGNOSTIC TESTS COMPLETE\n")
# cat("============================================================\n\n")

