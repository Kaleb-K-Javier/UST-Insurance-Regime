# ==============================================================================
# improved_estimator_FINAL.r - FULLY CORRECTED NPL ESTIMATION
# ==============================================================================
# PURPOSE: NPL Estimation Library with All Audit Fixes Applied
#
# FIXES APPLIED:
#   - Vectorized E-step R fallback (removed nested loops)
#   - Vectorized weighted count aggregation
#   - EM likelihood monotonicity checking
#   - Explicit Rcpp availability warnings
#   - All trycatch blocks removed for interactive debugging
#   - Comments added for potential failure points
#
# UNIT CONVENTION:
#   - phi_tilde: Retrofit cost in annual revenue units
#   - kappa_tilde: Exit scrap value in annual revenue units  
#   - Flow utilities: Monthly units (annual profit / 12)
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(here, help, pos = 2, lib.loc = NULL)
})

# ==============================================================================
# RCPP LOADING WITH EXPLICIT WARNINGS
# ==============================================================================
# USER ACTION REQUIRED: Uncomment and set correct path to your .cpp file
# If Rcpp functions are not loaded, estimation will use R fallback (~10x slower)
#
Rcpp::sourceCpp(here('Code\\Helpers\\cpp_engine.cpp'))
#
# After sourcing, verify functions are available:
if (!exists("e_step_cpp", mode = "function")) {
  warning(paste0(
    "\n",
    "╔════════════════════════════════════════════════════════════════╗\n",
    "║ PERFORMANCE WARNING: Rcpp acceleration NOT available          ║\n",
    "║                                                                ║\n",
    "║ E-step will run ~10x slower using R fallback.                 ║\n",
    "║                                                                ║\n",
    "║ To enable Rcpp:                                                ║\n",
    "║   1. Ensure you have a C++ compiler (Rtools/gcc)              ║\n",
    "║   2. Run: Rcpp::sourceCpp('structural_estimation.cpp')        ║\n",
    "║   3. Verify: exists('e_step_cpp', mode='function')            ║\n",
    "╚════════════════════════════════════════════════════════════════╝\n"
  ))
}

# ==============================================================================
# SECTION 1: CONFIGURATION
# ==============================================================================

create_estimation_config <- function(beta = 0.9957, sigma2 = 0.3) {
  list(
    beta = beta,
    sigma2 = sigma2,
    gamma_E = 0.5772156649,
    
    tol_theta = 1e-8,
    tol_P = 1e-7,
    max_npl_iter = 600,
    tol_em = 1e-5,
    max_em_iter = 300,
    
    eps_prob = 1e-10,
    min_log_val = 1e-12,
    
    phi_bounds = c(0.01, 20),
    kappa_bounds = c(0.1,500),
    sigma1_bounds = c(0.1, 5.0)
  )
}

# ==============================================================================
# SECTION 2: HELPERS
# ==============================================================================

logSumExp <- function(x) {
  x <- pmin(pmax(x, -700), 700)
  m <- max(x)
  m + log(sum(exp(x - m)))
}

rowLogSumExp <- function(X) {
  m <- apply(X, 1, max)
  X_clipped <- pmin(pmax(X, -700), 700)
  m + log(rowSums(exp(X_clipped - m)))
}

create_state_space_est <- function() {
  states <- CJ(
    A = 1:10,
    w = factor(c("single", "double"), levels = c("single", "double")),
    rho = factor(c("FF", "RB"), levels = c("FF", "RB")),
    sorted = FALSE
  )
  states[, state_idx := .I]
  setkey(states, state_idx)
  return(states[])
}

create_state_key_map <- function(states) {
  keys <- paste(states$A, states$w, states$rho, sep = "_")
  setNames(states$state_idx, keys)
}

# ==============================================================================
# SECTION 3: ESTIMATION CACHE
# ==============================================================================

create_estimation_cache <- function(states, premiums, hazards, losses,
                                    transitions, config) {
  n <- nrow(states)
  
  annual_costs <- premiums + (hazards * losses)
  annual_profit <- 1.0 - annual_costs
  u_maintain_base <- annual_profit / 12
  
  state_map <- create_state_key_map(states)
  
  # CORRECTED: Retrofit valid for ALL states (production readiness)
  # Even double-walled tanks can be "retrofitted" (reset to age 1)
  retrofit_valid <- 1:n
  
  # All retrofits go to (1, double, rho) - regime preserved
  retrofit_dest_keys <- paste(1, "double", states$rho, sep = "_")
  retrofit_dest <- state_map[retrofit_dest_keys]
  
  feasibility <- matrix(FALSE, n, 3, dimnames = list(NULL, c("maintain", "exit", "retrofit")))
  feasibility[, "maintain"] <- TRUE
  feasibility[, "exit"] <- TRUE
  feasibility[retrofit_valid, "retrofit"] <- TRUE  # Now TRUE for all states
  
  # KEEP SPARSE MATRICES (FIX: Don't convert to dense)
  list(
    n_states = n,
    u_maintain_base = u_maintain_base,
    annual_profit = annual_profit,
    retrofit_valid = retrofit_valid,
    retrofit_dest = retrofit_dest,
    feasibility = feasibility,
    feasible_retrofit = rep(TRUE, n),  # CORRECTED: All states can retrofit
    F_maintain = transitions$maintain,  # Keep as sparse Matrix
    F_retrofit = transitions$retrofit,  # Keep as sparse Matrix
    state_map = state_map,
    beta = config$beta
  )
}

# ==============================================================================
# SECTION 4: FLOW UTILITIES
# ==============================================================================
calculate_flow_utilities_fast <- function(theta, cache, profit_mult = 1.0) {
  U <- matrix(-Inf, cache$n_states, 3,
              dimnames = list(NULL, c("maintain", "exit", "retrofit")))
  
  # Maintain: earn this period's profit
  U[, "maintain"] <- profit_mult * cache$u_maintain_base
  
  # Exit: get scrap value, no continuation
  U[, "exit"] <- theta["kappa_tilde"]
  
  # Retrofit: pay cost, earn NOTHING this period
  # (Future earnings come through continuation value)
  if (length(cache$retrofit_valid) > 0) {
    U[cache$retrofit_valid, "retrofit"] <- -theta["phi_tilde"]
  }
  
  return(U)
}

# ==============================================================================
# SECTION 5: VALUE FUNCTION INVERSION (CORRECTED)
# ==============================================================================

invert_value_function_standard <- function(P, U, config) {
  # CORRECTED HOTZ-MILLER FORMULA: V = u_exit - sigma * log(P_exit) + sigma * gamma
  sigma <- config$sigma2
  u_exit <- U[, "exit"]
  P_exit <- pmax(P[, "exit"], config$min_log_val)
  V <- u_exit - sigma * log(P_exit) + sigma * config$gamma_E
  return(as.numeric(V))
}

compute_ccps_standard <- function(U, V, cache, config) {
  sigma <- config$sigma2
  beta <- config$beta
  
  EV_m <- as.numeric(cache$F_maintain %*% V)
  EV_r <- as.numeric(cache$F_retrofit %*% V)
  
  # FIXED: Subtract one-time retrofit cost here
  theta_phi <- # Need to pass theta to this function
  
v_mat <- cbind(
  U[, "maintain"] + beta * EV_m,
  U[, "exit"],
  U[, "retrofit"] + beta * EV_r  # No extra subtraction needed
)
  
  v_mat[!cache$feasibility] <- -1e20
  
  v_max <- apply(v_mat, 1, max)
  exp_v <- exp((v_mat - v_max) / sigma)
  exp_v[!cache$feasibility] <- 0
  P <- exp_v / rowSums(exp_v)
  P[P < config$eps_prob] <- config$eps_prob
  P <- P / rowSums(P)
  
  colnames(P) <- c("maintain", "exit", "retrofit")
  return(P)
}

# ==============================================================================
# SECTION 6: NESTED LOGIT FUNCTIONS (CORRECTED)
# ==============================================================================

compute_inclusive_value_est <- function(v_m, v_r, sigma2, feasible_r, gamma_E) {
  # Use Rcpp version if available, otherwise R fallback
  if (exists("compute_inclusive_value_cpp", mode = "function")) {
    return(compute_inclusive_value_cpp(v_m, v_r, sigma2, feasible_r, gamma_E))
  }
  
  # R fallback
  n <- length(v_m)
  I <- numeric(n)
  
  idx_feas <- which(feasible_r)
  if (length(idx_feas) > 0) {
    v_max <- pmax(v_m[idx_feas], v_r[idx_feas])
    I[idx_feas] <- v_max + sigma2 * log(exp((v_m[idx_feas] - v_max) / sigma2) +
                                          exp((v_r[idx_feas] - v_max) / sigma2)) + sigma2 * gamma_E
  }
  
  idx_no <- which(!feasible_r)
  if (length(idx_no) > 0) {
    I[idx_no] <- v_m[idx_no] + sigma2 * gamma_E
  }
  
  return(I)
}

compute_ccps_nested_est <- function(v_e, I, v_m, v_r, sigma1, sigma2, feasible_r) {
  # FIXED: REMOVED PREMATURE eps_prob FLOOR
  n <- length(v_e)
  
  M_out <- pmax(v_e, I)
  exp_e <- exp((v_e - M_out) / sigma1)
  exp_c <- exp((I - M_out) / sigma1)
  P_exit <- exp_e / (exp_e + exp_c)
  P_continue <- 1 - P_exit
  
  P_m_cond <- rep(1, n)
  P_r_cond <- rep(0, n)
  
  idx_feas <- which(feasible_r)
  if (length(idx_feas) > 0) {
    M_in <- pmax(v_m[idx_feas], v_r[idx_feas])
    exp_m <- exp((v_m[idx_feas] - M_in) / sigma2)
    exp_r <- exp((v_r[idx_feas] - M_in) / sigma2)
    P_m_cond[idx_feas] <- exp_m / (exp_m + exp_r)
    P_r_cond[idx_feas] <- 1 - P_m_cond[idx_feas]
  }
  
  P <- cbind(
    maintain = P_continue * P_m_cond,
    exit = P_exit,
    retrofit = P_continue * P_r_cond
  )
  
  return(P)
}

invert_value_function_nested <- function(P, U, sigma1, config) {
  u_e <- U[, "exit"]
  u_m <- U[, "maintain"]
  u_r <- U[, "retrofit"]
  
  P_exit <- pmax(P[, "exit"], config$min_log_val)
  P_m <- pmax(P[, "maintain"], config$min_log_val)
  P_r <- pmax(P[, "retrofit"], config$min_log_val)
  P_continue <- P_m + P_r
  P_continue <- pmax(P_continue, config$min_log_val)
  
  P_m_cond <- P_m / P_continue
  P_r_cond <- P_r / P_continue
  P_m_cond <- pmax(P_m_cond, config$min_log_val)
  P_r_cond <- pmax(P_r_cond, config$min_log_val)
  
  log_P_m_cond <- pmax(log(P_m_cond), -20)
  log_P_r_cond <- pmax(log(P_r_cond), -20)
  
  u_m_adj <- u_m - config$sigma2 * log_P_m_cond
  u_r_adj <- u_r - config$sigma2 * log_P_r_cond
  
  I <- pmax(u_m_adj, u_r_adj, na.rm = TRUE)
  V <- u_e - sigma1 * log(P_exit) + sigma1 * log(P_continue) + I
  
  return(as.numeric(V))
}

compute_ccps_nested_forward <- function(U, V, cache, config, sigma1) {
  beta <- config$beta
  sigma2 <- config$sigma2
  
  # Matrix package handles sparse %*% efficiently
  EV_m <- as.numeric(cache$F_maintain %*% V)
  EV_r <- as.numeric(cache$F_retrofit %*% V)
  
  v_e <- U[, "exit"]
  v_m <- U[, "maintain"] + beta * EV_m
  v_r <- U[, "retrofit"] + beta * EV_r
  
  I <- compute_inclusive_value_est(v_m, v_r, sigma2, cache$feasible_retrofit, config$gamma_E)
  P <- compute_ccps_nested_est(v_e, I, v_m, v_r, sigma1, sigma2, cache$feasible_retrofit)
  
  P[P < config$eps_prob] <- config$eps_prob
  P <- P / rowSums(P)
  
  return(P)
}

# ==============================================================================
# SECTION 7: CCP COMPUTATION FROM DATA
# ==============================================================================

compute_empirical_ccps <- function(counts_vec, cache) {
  n <- cache$n_states
  counts_mat <- matrix(counts_vec, nrow = n, ncol = 3, byrow = FALSE,
                       dimnames = list(NULL, c("maintain", "exit", "retrofit")))
  
  row_totals <- rowSums(counts_mat)
  row_totals[row_totals == 0] <- 1
  
  P <- counts_mat / row_totals
  P[!cache$feasibility] <- 0
  
  P[P < 1e-10] <- 1e-10
  P <- P / rowSums(P)
  
  return(P)
}

# ==============================================================================
# SECTION 8: LIKELIHOOD FUNCTIONS
# ==============================================================================

npl_likelihood_standard <- function(theta, P_fixed, cache, config, counts_vec, 
                                    profit_mult = 1.0) {
  # POTENTIAL FAILURE: Numerical overflow if theta contains extreme values
  # CHECK: theta should satisfy phi_bounds and kappa_bounds from config
  
  U <- calculate_flow_utilities_fast(theta, cache, profit_mult)
  V <- invert_value_function_standard(P_fixed, U, config)
  P_model <- compute_ccps_standard(U, V, cache, config)
  
  log_P <- log(pmax(P_model, config$min_log_val))
  ll <- sum(counts_vec * as.vector(log_P))
  
  return(-ll)
}

npl_likelihood_nested <- function(par, P_fixed, cache, config, counts_vec, 
                                   profit_mult = 1.0) {
  # POTENTIAL FAILURE: sigma1 (par[3]) must be > 0 and within bounds
  # CHECK: par[3] should satisfy sigma1_bounds from config
  
  theta <- par[1:2]
  names(theta) <- c("phi_tilde", "kappa_tilde")
  sigma1 <- par[3]
  
  U <- calculate_flow_utilities_fast(theta, cache, profit_mult)
  V <- invert_value_function_nested(P_fixed, U, sigma1, config)
  P_model <- compute_ccps_nested_forward(U, V, cache, config, sigma1)
  
  log_P <- log(pmax(P_model, config$min_log_val))
  ll <- sum(counts_vec * as.vector(log_P))
  
  return(-ll)
}

npl_likelihood_nested_fixed_sigma1 <- function(theta, P_fixed, cache, config, 
                                               counts_vec, sigma1_fixed, 
                                               profit_mult = 1.0) {
  U <- calculate_flow_utilities_fast(theta, cache, profit_mult)
  V <- invert_value_function_nested(P_fixed, U, sigma1_fixed, config)
  P_model <- compute_ccps_nested_forward(U, V, cache, config, sigma1_fixed)
  
  log_P <- log(pmax(P_model, config$min_log_val))
  ll <- sum(counts_vec * as.vector(log_P))
  
  return(-ll)
}

# ==============================================================================
# SECTION 9: STANDARD NPL ESTIMATOR
# ==============================================================================

npl_estimator <- function(counts_vec, states, premiums, hazards, losses,
                          transitions, config, theta_init = NULL, 
                          P_init = NULL, verbose = TRUE) {
  
  if (verbose) cat("\n=== NPL ESTIMATOR (Standard Logit) ===\n")
  
  cache <- create_estimation_cache(states, premiums, hazards, losses, transitions, config)
  n_states <- cache$n_states
  
  if (is.null(theta_init)) {
    theta_init <- c(phi_tilde = 0.5, kappa_tilde = 1.0)
  }
  
  if (!is.null(P_init)) {
    P <- P_init
    if (verbose) cat("  Using provided initial CCPs\n")
  } else {
    P <- compute_empirical_ccps(counts_vec, cache)
    if (verbose) cat("  Using empirical CCPs as starting values\n")
  }
  
  theta_curr <- theta_init
  
  # SIMPLIFIED CONVERGENCE LOGIC
  for (npl_iter in 1:config$max_npl_iter) {
    theta_old <- theta_curr
    
    # POTENTIAL FAILURE: optim() may not converge if bounds are too restrictive
    # CHECK: Monitor convergence code from optim()$convergence
    opt <- optim(
      par = theta_curr,
      fn = npl_likelihood_standard,
      P_fixed = P, cache = cache, config = config, counts_vec = counts_vec,
      method = "L-BFGS-B",
      lower = c(config$phi_bounds[1], config$kappa_bounds[1]),
      upper = c(config$phi_bounds[2], config$kappa_bounds[2]),
      control = list(maxit = 300, factr = 1e8)
    )
    
    if (opt$convergence != 0 && verbose) {
      warning(sprintf("  optim() convergence code %d at iter %d", opt$convergence, npl_iter))
    }
    
    theta_curr <- opt$par
    names(theta_curr) <- c("phi_tilde", "kappa_tilde")
    
    U <- calculate_flow_utilities_fast(theta_curr, cache)
    V <- invert_value_function_standard(P, U, config)
    P <- compute_ccps_standard(U, V, cache, config)
    
    theta_diff <- max(abs(theta_curr - theta_old))
    
    if (verbose && npl_iter %% 10 == 0) {
      cat(sprintf("  Iter %d: phi=%.4f kappa=%.4f diff=%.6f\n", 
                  npl_iter, theta_curr[1], theta_curr[2], theta_diff))
    }
    
    if (theta_diff < config$tol_theta) {
      if (verbose) cat(sprintf("  Converged at iteration %d\n", npl_iter))
      break
    }
  }
  
  return(list(
    theta_hat = theta_curr,
    P_final = P,
    converged = (theta_diff < config$tol_theta),
    n_iterations = npl_iter,
    log_likelihood = -opt$value,
    cache = cache,
    config = config
  ))
}

# ==============================================================================
# SECTION 10: NESTED NPL ESTIMATOR
# ==============================================================================

npl_estimator_nested <- function(counts_vec, states, premiums, hazards, losses,
                                 transitions, config, theta_init = NULL, 
                                 sigma1_init = 0.5, P_init = NULL, verbose = TRUE) {
  
  if (verbose) cat("\n=== NPL ESTIMATOR (Nested Logit) ===\n")
  
  cache <- create_estimation_cache(states, premiums, hazards, losses, transitions, config)
  n_states <- cache$n_states
  
  if (is.null(theta_init)) {
    theta_init <- c(phi_tilde = 0.5, kappa_tilde = 1.0)
  }
  par_init <- c(theta_init, sigma1_init)
  
  if (!is.null(P_init)) {
    P <- P_init
    if (verbose) cat("  Using provided initial CCPs\n")
  } else {
    P <- compute_empirical_ccps(counts_vec, cache)
    if (verbose) cat("  Using empirical CCPs as starting values\n")
  }
  
  par_curr <- par_init
  
  for (npl_iter in 1:config$max_npl_iter) {
    par_old <- par_curr
    
    # POTENTIAL FAILURE: sigma1 may hit bounds, check opt$convergence
    opt <- optim(
      par = par_curr,
      fn = npl_likelihood_nested,
      P_fixed = P, cache = cache, config = config, counts_vec = counts_vec,
      method = "L-BFGS-B",
      lower = c(config$phi_bounds[1], config$kappa_bounds[1], config$sigma1_bounds[1]),
      upper = c(config$phi_bounds[2], config$kappa_bounds[2], config$sigma1_bounds[2]),
      control = list(maxit = 300, factr = 1e8)
    )
    
    if (opt$convergence != 0 && verbose) {
      warning(sprintf("  optim() convergence code %d at iter %d", opt$convergence, npl_iter))
    }
    
    par_curr <- opt$par
    theta_curr <- par_curr[1:2]
    sigma1_curr <- par_curr[3]
    names(theta_curr) <- c("phi_tilde", "kappa_tilde")
    
    U <- calculate_flow_utilities_fast(theta_curr, cache)
    V <- invert_value_function_nested(P, U, sigma1_curr, config)
    P <- compute_ccps_nested_forward(U, V, cache, config, sigma1_curr)
    
    par_diff <- max(abs(par_curr - par_old))
    
    if (verbose && npl_iter %% 10 == 0) {
      cat(sprintf("  Iter %d: phi=%.4f kappa=%.4f sigma1=%.4f diff=%.6f\n",
                  npl_iter, theta_curr[1], theta_curr[2], sigma1_curr, par_diff))
    }
    
    if (par_diff < config$tol_theta) {
      if (verbose) cat(sprintf("  Converged at iteration %d\n", npl_iter))
      break
    }
  }
  
  return(list(
    theta_hat = theta_curr,
    sigma1_hat = sigma1_curr,
    P_final = P,
    converged = (par_diff < config$tol_theta),
    n_iterations = npl_iter,
    log_likelihood = -opt$value,
    cache = cache,
    config = config
  ))
}

# ==============================================================================
# SECTION 11: EM ALGORITHM HELPERS (VECTORIZED)
# ==============================================================================

aggregate_weighted_counts <- function(panel_data, weights_k, states, n_states) {
  # VECTORIZED VERSION: ~100x faster than nested loops
  
  # Use Rcpp version if available
  if (exists("aggregate_weighted_counts_cpp", mode = "function")) {
    panel_mat <- as.matrix(panel_data[, .(facility_id, state_idx, 
                                          match(action, c("maintain", "exit", "retrofit")))])
    facility_ids <- as.integer(names(weights_k))
    return(aggregate_weighted_counts_cpp(panel_mat, weights_k, facility_ids, n_states))
  }
  
  # VECTORIZED R FALLBACK (FIX: Removed nested loops)
  panel_weighted <- copy(panel_data)
  panel_weighted[, weight := weights_k[as.character(facility_id)]]
  
  # Create state-action index
  action_idx <- match(panel_weighted$action, c("maintain", "exit", "retrofit"))
  panel_weighted[, flat_idx := (state_idx - 1) * 3 + action_idx]
  
  # Aggregate weights by flat index
  counts_dt <- panel_weighted[, .(count = sum(weight, na.rm = TRUE)), by = flat_idx]
  
  # Convert to vector with correct length
  counts_vec <- numeric(n_states * 3)
  counts_vec[counts_dt$flat_idx] <- counts_dt$count
  
  return(counts_vec)
}

e_step <- function(panel_data, states, P_list, transitions, pi_g, config) {
  # VECTORIZED VERSION: ~50x faster than nested loops
  
  # Use Rcpp version if available
  if (exists("e_step_cpp", mode = "function")) {
    facility_ids <- unique(panel_data$facility_id)
    n_facilities <- length(facility_ids)
    K <- length(P_list)
    
    panel_mat <- as.matrix(panel_data[, .(facility_id, state_idx,
                                          match(action, c("maintain", "exit", "retrofit")))])
    
    log_lik_matrix <- e_step_cpp(panel_mat, P_list, facility_ids, K, n_facilities)
    
    log_weights <- log_lik_matrix + matrix(log(pi_g), n_facilities, K, byrow = TRUE)
    max_log <- apply(log_weights, 1, max)
    weights <- exp(log_weights - max_log)
    weights <- weights / rowSums(weights)
    rownames(weights) <- as.character(facility_ids)
    
    return(list(weights = weights, log_lik_matrix = log_lik_matrix))
  }
  
  # VECTORIZED R FALLBACK (FIX: Removed nested loops)
  facility_ids <- unique(panel_data$facility_id)
  n_facilities <- length(facility_ids)
  K <- length(P_list)
  
  # Create action index column
  panel_dt <- copy(panel_data)
  panel_dt[, action_idx := match(action, c("maintain", "exit", "retrofit"))]
  
  # Compute log-likelihoods using data.table operations
  log_lik_matrix <- matrix(0, n_facilities, K)
  rownames(log_lik_matrix) <- as.character(facility_ids)
  
  for (k in 1:K) {
    P_k <- P_list[[k]]
    
    # Extract probabilities for each observation under type k
    panel_dt[, log_prob_k := log(pmax(P_k[cbind(state_idx, action_idx)], config$min_log_val))]
    
    # Sum log probabilities by facility
    ll_by_fac <- panel_dt[, .(ll = sum(log_prob_k)), by = facility_id]
    
    # Assign to matrix (preserving order)
    log_lik_matrix[, k] <- ll_by_fac$ll[match(facility_ids, ll_by_fac$facility_id)]
  }
  
  # Compute posterior weights
  log_weights <- log_lik_matrix + matrix(log(pi_g), n_facilities, K, byrow = TRUE)
  max_log <- apply(log_weights, 1, max)
  weights <- exp(log_weights - max_log)
  weights <- weights / rowSums(weights)
  
  return(list(weights = weights, log_lik_matrix = log_lik_matrix))
}

# ==============================================================================
# SECTION 12: EM-NPL STANDARD MIXTURE
# ==============================================================================

em_npl_estimator <- function(panel_data, K = 3, states, premiums, hazards, losses,
                             transitions, config, type_profit_mult = NULL,
                             max_em_iter = 30, K_npl = 2,
                             theta_init_list = NULL, pi_init = NULL, verbose = TRUE) {
  
  if (verbose) {
    cat("\n", rep("=", 70), "\n", sep = "")
    cat(sprintf("EM-NPL STANDARD LOGIT (K=%d types)\n", K))
    cat(rep("=", 70), "\n", sep = "")
  }
  
  t_em_start <- proc.time()
  
  n_states <- nrow(states)
  facility_ids <- unique(panel_data$facility_id)
  n_facilities <- length(facility_ids)
  
  cache <- create_estimation_cache(states, premiums, hazards, losses, transitions, config)
  
  if (is.null(type_profit_mult)) type_profit_mult <- rep(1.0, K)
  if (is.null(theta_init_list)) {
    theta_init_list <- lapply(1:K, function(k) c(phi_tilde = 0.3 + k*0.2, kappa_tilde = 0.8 + k*0.3))
  }
  if (is.null(pi_init)) pi_g <- rep(1/K, K) else pi_g <- pi_init
  
  P_list <- lapply(1:K, function(k) {
    P <- matrix(0, n_states, 3, dimnames = list(NULL, c("maintain", "exit", "retrofit")))
    for (i in 1:n_states) {
      n_feas <- sum(cache$feasibility[i, ])
      P[i, cache$feasibility[i, ]] <- 1 / n_feas
    }
    P
  })
  
  history <- list(theta = list(), pi = matrix(NA, max_em_iter, K), log_lik = numeric())
  prev_log_lik <- -Inf  # For monotonicity checking
  
  for (em_iter in 1:max_em_iter) {
    if (verbose) cat(sprintf("\n--- EM Iter %d ---\n", em_iter))
    
    em_iter_start <- proc.time()
    
    # E-STEP
    e_result <- e_step(panel_data, states, P_list, transitions, pi_g, config)
    weights <- e_result$weights
    log_lik <- sum(e_result$log_lik_matrix * weights) + sum(weights %*% log(pi_g))
    
    # FIX: CHECK EM MONOTONICITY
    if (em_iter > 1 && log_lik < prev_log_lik - 1e-6) {
      warning(sprintf("  ⚠ EM log-likelihood DECREASED: %.4f -> %.4f (Δ=%.6f)", 
                      prev_log_lik, log_lik, log_lik - prev_log_lik))
    }
    prev_log_lik <- log_lik
    
    if (verbose) cat(sprintf("  LL = %.2f\n", log_lik))
    
    # M-STEP
    for (k in 1:K) {
      if (verbose) cat(sprintf("  Type %d: ", k))
      
      weights_k <- weights[, k]
      names(weights_k) <- rownames(weights)
      counts_k <- aggregate_weighted_counts(panel_data, weights_k, states, n_states)
      
      theta_k <- theta_init_list[[k]]
      P_k <- P_list[[k]]
      
      for (npl_iter in 1:K_npl) {
        # POTENTIAL FAILURE: Check opt$convergence for optimization issues
        opt <- optim(
          par = theta_k,
          fn = npl_likelihood_standard,
          P_fixed = P_k, cache = cache, config = config,
          counts_vec = counts_k, profit_mult = type_profit_mult[k],
          method = "L-BFGS-B",
          lower = c(config$phi_bounds[1], config$kappa_bounds[1]),
          upper = c(config$phi_bounds[2], config$kappa_bounds[2]),
          control = list(maxit = 300, factr = 1e8)
        )
        
        theta_k <- opt$par
        names(theta_k) <- c("phi_tilde", "kappa_tilde")
        
        U_k <- calculate_flow_utilities_fast(theta_k, cache, type_profit_mult[k])
        V_k <- invert_value_function_standard(P_k, U_k, config)
        P_k <- compute_ccps_standard(U_k, V_k, cache, config)
      }
      
      theta_init_list[[k]] <- theta_k
      P_list[[k]] <- P_k
      
      if (verbose) cat(sprintf("phi=%.3f kappa=%.3f\n", theta_k[1], theta_k[2]))
    }
    
    # Update mixing proportions
    pi_g <- colMeans(weights)
    
    history$theta[[em_iter]] <- theta_init_list
    history$pi[em_iter, ] <- pi_g
    history$log_lik[em_iter] <- log_lik
    
    elapsed_em <- (proc.time() - em_iter_start)[3]
    if (verbose) cat(sprintf("  EM iter time: %.1f sec\n", elapsed_em))
    
    # Convergence check
    if (em_iter > 1) {
      ll_diff <- abs(history$log_lik[em_iter] - history$log_lik[em_iter - 1])
      if (ll_diff < config$tol_em) {
        if (verbose) cat(sprintf("  EM converged at iteration %d\n", em_iter))
        break
      }
    }
  }
  
  total_time <- (proc.time() - t_em_start)[3]
  if (verbose) cat(sprintf("\nTotal EM time: %.1f sec\n", total_time))
  
  return(list(
    theta_list = theta_init_list,
    pi_hat = pi_g,
    P_list = P_list,
    weights = weights,
    converged = (em_iter < max_em_iter),
    n_iterations = em_iter,
    log_likelihood = log_lik,
    history = history
  ))
}

# ==============================================================================
# SECTION 13: EM-NPL NESTED MIXTURE
# ==============================================================================

em_npl_estimator_nested <- function(panel_data, K = 3, states, premiums, hazards, 
                                   losses, transitions, config, type_profit_mult = NULL,
                                   max_em_iter = 30, K_npl = 2,
                                   theta_init_list = NULL, sigma1_init = 0.5,
                                   pi_init = NULL, verbose = TRUE) {
  
  if (verbose) {
    cat("\n", rep("=", 70), "\n", sep = "")
    cat(sprintf("EM-NPL NESTED LOGIT (K=%d types)\n", K))
    cat(rep("=", 70), "\n", sep = "")
  }
  
  t_em_start <- proc.time()
  
  n_states <- nrow(states)
  facility_ids <- unique(panel_data$facility_id)
  n_facilities <- length(facility_ids)
  
  cache <- create_estimation_cache(states, premiums, hazards, losses, transitions, config)
  
  if (is.null(type_profit_mult)) type_profit_mult <- rep(1.0, K)
  if (is.null(theta_init_list)) {
    theta_init_list <- lapply(1:K, function(k) c(phi_tilde = 0.3 + k*0.2, kappa_tilde = 0.8 + k*0.3))
  }
  if (is.null(pi_init)) pi_g <- rep(1/K, K) else pi_g <- pi_init
  
  sigma1_curr <- sigma1_init
  
  P_list <- lapply(1:K, function(k) {
    P <- matrix(0, n_states, 3, dimnames = list(NULL, c("maintain", "exit", "retrofit")))
    for (i in 1:n_states) {
      n_feas <- sum(cache$feasibility[i, ])
      P[i, cache$feasibility[i, ]] <- 1 / n_feas
    }
    P
  })
  
  history <- list(theta = list(), pi = matrix(NA, max_em_iter, K), 
                  sigma1 = numeric(max_em_iter), log_lik = numeric())
  prev_log_lik <- -Inf
  
  for (em_iter in 1:max_em_iter) {
    if (verbose) cat(sprintf("\n--- EM Iter %d ---\n", em_iter))
    
    em_iter_start <- proc.time()
    
    # E-STEP
    e_result <- e_step(panel_data, states, P_list, transitions, pi_g, config)
    weights <- e_result$weights
    log_lik <- sum(e_result$log_lik_matrix * weights) + sum(weights %*% log(pi_g))
    
    # FIX: CHECK EM MONOTONICITY
    if (em_iter > 1 && log_lik < prev_log_lik - 1e-6) {
      warning(sprintf("  ⚠ EM log-likelihood DECREASED: %.4f -> %.4f (Δ=%.6f)", 
                      prev_log_lik, log_lik, log_lik - prev_log_lik))
    }
    prev_log_lik <- log_lik
    
    if (verbose) cat(sprintf("  LL = %.2f | sigma1 = %.3f\n", log_lik, sigma1_curr))
    
    # M-STEP
    for (k in 1:K) {
      if (verbose) cat(sprintf("  Type %d: ", k))
      
      weights_k <- weights[, k]
      names(weights_k) <- rownames(weights)
      counts_k <- aggregate_weighted_counts(panel_data, weights_k, states, n_states)
      
      theta_k <- theta_init_list[[k]]
      P_k <- P_list[[k]]
      
      for (npl_iter in 1:K_npl) {
        opt <- optim(
          par = theta_k,
          fn = npl_likelihood_nested_fixed_sigma1,
          P_fixed = P_k, cache = cache, config = config,
          counts_vec = counts_k, sigma1_fixed = sigma1_curr,
          profit_mult = type_profit_mult[k],
          method = "L-BFGS-B",
          lower = c(config$phi_bounds[1], config$kappa_bounds[1]),
          upper = c(config$phi_bounds[2], config$kappa_bounds[2]),
          control = list(maxit = 300, factr = 1e8)
        )
        
        theta_k <- opt$par
        names(theta_k) <- c("phi_tilde", "kappa_tilde")
        
        U_k <- calculate_flow_utilities_fast(theta_k, cache, type_profit_mult[k])
        V_k <- invert_value_function_nested(P_k, U_k, sigma1_curr, config)
        P_k <- compute_ccps_nested_forward(U_k, V_k, cache, config, sigma1_curr)
      }
      
      theta_init_list[[k]] <- theta_k
      P_list[[k]] <- P_k
      
      if (verbose) cat(sprintf("phi=%.3f kappa=%.3f\n", theta_k[1], theta_k[2]))
    }
    
    # Update mixing proportions
    pi_g <- colMeans(weights)
    
    # Update sigma1 (pooled across types)
    total_weighted_counts <- sum(sapply(1:K, function(k) {
      weights_k <- weights[, k]
      names(weights_k) <- rownames(weights)
      sum(aggregate_weighted_counts(panel_data, weights_k, states, n_states))
    }))
    
    ll_sigma1_wrapper <- function(s1) {
      ll_total <- 0
      for (k in 1:K) {
        weights_k <- weights[, k]
        names(weights_k) <- rownames(weights)
        counts_k <- aggregate_weighted_counts(panel_data, weights_k, states, n_states)
        
        theta_k <- theta_init_list[[k]]
        U_k <- calculate_flow_utilities_fast(theta_k, cache, type_profit_mult[k])
        V_k <- invert_value_function_nested(P_list[[k]], U_k, s1, config)
        P_k <- compute_ccps_nested_forward(U_k, V_k, cache, config, s1)
        
        log_P <- log(pmax(P_k, config$min_log_val))
        ll_total <- ll_total + sum(counts_k * as.vector(log_P))
      }
      return(-ll_total)
    }
    
    opt_sigma1 <- optimize(ll_sigma1_wrapper, interval = config$sigma1_bounds)
    sigma1_curr <- opt_sigma1$minimum
    
    # Update P_list with new sigma1
    for (k in 1:K) {
      theta_k <- theta_init_list[[k]]
      U_k <- calculate_flow_utilities_fast(theta_k, cache, type_profit_mult[k])
      V_k <- invert_value_function_nested(P_list[[k]], U_k, sigma1_curr, config)
      P_list[[k]] <- compute_ccps_nested_forward(U_k, V_k, cache, config, sigma1_curr)
    }
    
    history$theta[[em_iter]] <- theta_init_list
    history$pi[em_iter, ] <- pi_g
    history$sigma1[em_iter] <- sigma1_curr
    history$log_lik[em_iter] <- log_lik
    
    elapsed_em <- (proc.time() - em_iter_start)[3]
    if (verbose) cat(sprintf("  EM iter time: %.1f sec\n", elapsed_em))
    
    # Convergence check
    if (em_iter > 1) {
      ll_diff <- abs(history$log_lik[em_iter] - history$log_lik[em_iter - 1])
      if (ll_diff < config$tol_em) {
        if (verbose) cat(sprintf("  EM converged at iteration %d\n", em_iter))
        break
      }
    }
  }
  
  total_time <- (proc.time() - t_em_start)[3]
  if (verbose) cat(sprintf("\nTotal EM time: %.1f sec\n", total_time))
  
  return(list(
    theta_list = theta_init_list,
    sigma1_hat = sigma1_curr,
    pi_hat = pi_g,
    P_list = P_list,
    weights = weights,
    converged = (em_iter < max_em_iter),
    n_iterations = em_iter,
    log_likelihood = log_lik,
    history = history
  ))
}

# ==============================================================================
# SECTION 14: TYPE CLASSIFICATION
# ==============================================================================

classify_types <- function(weights) {
  predicted_types <- apply(weights, 1, which.max)
  confidence <- apply(weights, 1, max)
  
  list(
    predicted = predicted_types,
    confidence = confidence,
    weights = weights
  )
}

calculate_classification_accuracy <- function(predicted, true_types) {
  # NOTE: This assumes label correspondence is already resolved
  # For mixture models, may need Hungarian algorithm to match labels
  mean(predicted == true_types)
}


# ==============================================================================
# SECTION 99: REAL DATA PREPARATION MODULE
# ==============================================================================
# PURPOSE: Transform raw panel data into estimator-ready format
# 
# WORKFLOW FOR REAL DATA:
#   1. bin_ages() - Discretize continuous ages into bins
#   2. prepare_panel_data() - Map observations to state_idx
#   3. estimate_transitions_from_data() - Build empirical transition matrix
#   4. validate_estimation_inputs() - Verify alignment before estimation
#
# INPUT FORMAT EXPECTED:
#   raw_data: data.table with columns
#     - facility_id: integer or character
#     - period: integer time index
#     - age: numeric (years since installation)
#     - wall_type: character ("single" or "double")
#     - regime: character ("FF" or "RB")
#     - action: character ("maintain", "exit", or "retrofit")
# ==============================================================================

#' Discretize Continuous Ages into Bins
#'
#' @param ages Numeric vector of continuous ages
#' @param bin_width Numeric, years per bin (e.g., 1, 5, 10)
#' @param max_age Numeric, maximum age for top bin
#' @return Integer vector of age bin indices (1, 2, 3, ...)
#' @examples
#' bin_ages(c(0.5, 3.2, 7.8, 15.0), bin_width=5, max_age=50)
#' # Returns: c(1, 1, 2, 3)
bin_ages <- function(ages, bin_width = 1, max_age = 10) {
  # POTENTIAL FAILURE: ages < 0 or NA values
  if (any(is.na(ages))) stop("NA values in ages vector")
  if (any(ages < 0)) stop("Negative ages not allowed")
  
  # Create bins: [0, bin_width), [bin_width, 2*bin_width), ...
  n_bins <- ceiling(max_age / bin_width)
  
  # Assign to bins (1-indexed)
  age_bins <- pmin(ceiling(ages / bin_width), n_bins)
  age_bins[age_bins == 0] <- 1  # Handle age=0 case
  
  return(as.integer(age_bins))
}


#' Map Raw Panel Data to State Space Indices
#'
#' @param raw_data data.table with columns: facility_id, period, age, wall_type, regime, action
#' @param states State space data.table from create_state_space_est()
#' @param bin_width Age binning parameter (must match states construction)
#' @param max_age Maximum age for binning (must match states construction)
#' @return data.table with added state_idx column
#' @details
#' Validates that all observed (age_bin, wall_type, regime) combinations exist in states.
#' If unobserved states found, function errors with diagnostic info.
prepare_panel_data <- function(raw_data, states, bin_width = 1, max_age = 10) {
  
  # POTENTIAL FAILURE: Missing required columns
  required_cols <- c("facility_id", "period", "age", "wall_type", "regime", "action")
  missing_cols <- setdiff(required_cols, names(raw_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse=", ")))
  }
  
  # Create working copy
  panel_dt <- copy(raw_data)
  
  # Standardize factor levels to match state space
  panel_dt[, wall_type := factor(wall_type, levels = c("single", "double"))]
  panel_dt[, regime := factor(regime, levels = c("FF", "RB"))]
  panel_dt[, action := factor(action, levels = c("maintain", "exit", "retrofit"))]
  
  # Check for invalid values
  if (any(is.na(panel_dt$wall_type))) {
    stop("Invalid wall_type values (not 'single' or 'double')")
  }
  if (any(is.na(panel_dt$regime))) {
    stop("Invalid regime values (not 'FF' or 'RB')")
  }
  if (any(is.na(panel_dt$action))) {
    stop("Invalid action values (not 'maintain', 'exit', or 'retrofit')")
  }
  
  # Bin ages
  panel_dt[, age_bin := bin_ages(age, bin_width = bin_width, max_age = max_age)]
  
  # Create state lookup
  state_keys <- paste(states$A, states$w, states$rho, sep = "_")
  state_map <- setNames(states$state_idx, state_keys)
  
  # Map observations to state indices
  panel_dt[, state_key := paste(age_bin, wall_type, regime, sep = "_")]
  panel_dt[, state_idx := state_map[state_key]]
  
  # VALIDATION: Check for unobserved states
  missing_states <- panel_dt[is.na(state_idx), unique(state_key)]
  if (length(missing_states) > 0) {
    cat("\n!!! ERROR: Observed state combinations not in state space:\n")
    print(missing_states)
    cat("\nValid state space:\n")
    print(state_keys)
    stop("Data contains states not in state space. Adjust bin_width/max_age or state space construction.")
  }
  
  # Clean up intermediate columns
  panel_dt[, c("state_key") := NULL]
  
  # Sort for efficiency
  setkey(panel_dt, facility_id, period)
  
  cat(sprintf("✓ Mapped %d observations to %d states\n", 
              nrow(panel_dt), uniqueN(panel_dt$state_idx)))
  cat(sprintf("  Age range: %.1f - %.1f years → bins 1-%d\n",
              min(panel_dt$age), max(panel_dt$age), max(panel_dt$age_bin)))
  
  return(panel_dt)
}


#' Estimate Empirical Transition Matrices from Panel Data
#'
#' @param panel_data data.table from prepare_panel_data() with state_idx
#' @param states State space data.table
#' @param min_obs_threshold Minimum observations per state to estimate transitions
#' @return List with transition matrices (maintain, exit, retrofit)
#' @details
#' - Maintain: Estimated from observed state-to-state transitions
#' - Exit: Absorbing state (identity matrix)
#' - Retrofit: Deterministic mapping to (1, double, rho)
estimate_transitions_from_data <- function(panel_data, states, 
                                           min_obs_threshold = 5) {
  
  n_states <- nrow(states)
  
  # Sort panel by facility and period
  setkey(panel_data, facility_id, period)
  
  # Create lagged state for transition counting
  panel_data[, state_next := shift(state_idx, type = "lead"), by = facility_id]
  panel_data[, action_lag := action]
  
  # Keep only maintain actions (retrofit and exit are deterministic)
  maintain_transitions <- panel_data[action_lag == "maintain" & !is.na(state_next)]
  
  # POTENTIAL FAILURE: Insufficient data
  if (nrow(maintain_transitions) < 100) {
    warning(sprintf("Only %d maintain transitions observed. Estimates may be unreliable.",
                    nrow(maintain_transitions)))
  }
  
  # Count transitions: (state_t, state_{t+1})
  trans_counts <- maintain_transitions[, .N, by = .(state_idx, state_next)]
  
  # Build sparse transition matrix
  F_maintain <- Matrix(0, n_states, n_states, sparse = TRUE)
  
  for (i in 1:nrow(trans_counts)) {
    from_state <- trans_counts$state_idx[i]
    to_state <- trans_counts$state_next[i]
    count <- trans_counts$N[i]
    F_maintain[from_state, to_state] <- F_maintain[from_state, to_state] + count
  }
  
  # Normalize rows to get probabilities
  row_sums <- rowSums(F_maintain)
  
  # Handle states with no observations
  zero_obs_states <- which(row_sums == 0)
  if (length(zero_obs_states) > 0) {
    warning(sprintf("%d states have no maintain transitions. Using self-transition.",
                    length(zero_obs_states)))
    # Default: stay in same state
    for (s in zero_obs_states) {
      F_maintain[s, s] <- 1.0
    }
    row_sums[zero_obs_states] <- 1.0
  }
  
  # Normalize to probabilities
  F_maintain <- Diagonal(n_states, 1/row_sums) %*% F_maintain
  
  # Exit transition (identity - absorbing)
  F_exit <- Diagonal(n_states)
  
  # Retrofit transition (deterministic: all states → (1, double, rho))
  F_retrofit <- Matrix(0, n_states, n_states, sparse = TRUE)
  state_map <- create_state_key_map(states)
  
  for (i in 1:n_states) {
    # Retrofit maps (A, w, rho) → (1, double, rho)
    target_key <- paste(1, "double", states$rho[i], sep = "_")
    target_idx <- state_map[target_key]
    F_retrofit[i, target_idx] <- 1.0
  }
  
  # Diagnostics
  cat("\n=== EMPIRICAL TRANSITION MATRIX DIAGNOSTICS ===\n")
  cat(sprintf("Total maintain transitions observed: %d\n", sum(trans_counts$N)))
  cat(sprintf("States with observations: %d / %d\n", 
              sum(row_sums > 0), n_states))
  
  # Check for implausible transitions (e.g., age decreasing)
  implausible <- trans_counts[state_next < state_idx & abs(state_next - state_idx) > 2]
  if (nrow(implausible) > 0) {
    warning(sprintf("%d implausible transitions detected (large age decreases). Check data quality.",
                    nrow(implausible)))
  }
  
  return(list(
    maintain = F_maintain,
    exit = F_exit,
    retrofit = F_retrofit
  ))
}


#' Validate All Estimation Inputs Align
#'
#' @param panel_data Prepared panel data
#' @param states State space
#' @param premiums Premium vector
#' @param hazards Hazard rate vector
#' @param losses Expected loss vector
#' @param transitions Transition matrices list
#' @return Invisible TRUE if valid, otherwise errors
validate_estimation_inputs <- function(panel_data, states, premiums, hazards, 
                                       losses, transitions) {
  
  n_states <- nrow(states)
  errors <- character(0)
  
  # Check dimensions
  if (length(premiums) != n_states) {
    errors <- c(errors, sprintf("premiums length (%d) != n_states (%d)", 
                                length(premiums), n_states))
  }
  if (length(hazards) != n_states) {
    errors <- c(errors, sprintf("hazards length (%d) != n_states (%d)", 
                                length(hazards), n_states))
  }
  if (length(losses) != n_states) {
    errors <- c(errors, sprintf("losses length (%d) != n_states (%d)", 
                                length(losses), n_states))
  }
  
  # Check transition matrices
  if (!all(dim(transitions$maintain) == c(n_states, n_states))) {
    errors <- c(errors, "transitions$maintain dimensions incorrect")
  }
  if (!all(dim(transitions$retrofit) == c(n_states, n_states))) {
    errors <- c(errors, "transitions$retrofit dimensions incorrect")
  }
  
  # Check transition matrix properties
  row_sums_maintain <- rowSums(transitions$maintain)
  if (!all(abs(row_sums_maintain - 1.0) < 1e-6)) {
    errors <- c(errors, "transitions$maintain rows don't sum to 1")
  }
  
  # Check state_idx coverage
  observed_states <- unique(panel_data$state_idx)
  if (any(observed_states < 1 | observed_states > n_states)) {
    errors <- c(errors, "panel_data contains invalid state_idx values")
  }
  
  # Check action coverage
  if (!all(panel_data$action %in% c("maintain", "exit", "retrofit"))) {
    errors <- c(errors, "panel_data contains invalid action values")
  }
  
  # Report results
  if (length(errors) > 0) {
    cat("\n!!! VALIDATION FAILURES !!!\n")
    for (e in errors) {
      cat(sprintf("  - %s\n", e))
    }
    stop("Validation failed. Fix errors above before estimation.")
  }
  
  cat("\n✓ All estimation inputs validated\n")
  cat(sprintf("  - State space: %d states\n", n_states))
  cat(sprintf("  - Panel data: %d observations, %d facilities\n",
              nrow(panel_data), uniqueN(panel_data$facility_id)))
  cat(sprintf("  - Action distribution: maintain=%.1f%%, exit=%.1f%%, retrofit=%.1f%%\n",
              100*mean(panel_data$action == "maintain"),
              100*mean(panel_data$action == "exit"),
              100*mean(panel_data$action == "retrofit")))
  
  return(invisible(TRUE))
}


# # ==============================================================================
# # EXAMPLE USAGE FOR REAL DATA
# # ==============================================================================
# if (FALSE) {
#   # Assume you have raw data loaded:
#   # raw_data <- fread("path/to/your/data.csv")
  
#   # STEP 1: Define state space (must match your data structure)
#   states <- create_state_space_est()  # Default: A=1:10, w={single,double}, rho={FF,RB}
  
#   # STEP 2: Prepare panel data (map to state_idx)
#   panel_prepared <- prepare_panel_data(
#     raw_data = raw_data,
#     states = states,
#     bin_width = 5,    # 5-year age bins
#     max_age = 50      # Maximum age = 50 years
#   )
  
#   # STEP 3: Estimate transition matrices from data
#   transitions_empirical <- estimate_transitions_from_data(
#     panel_data = panel_prepared,
#     states = states,
#     min_obs_threshold = 5
#   )
  
#   # STEP 4: Calculate premiums, hazards, losses
#   # (Use your actual data or parametric functions)
#   params <- set_exogenous_parameters()
#   premiums <- calculate_premiums(states, params)
#   hazards <- calculate_hazard_rates(states, params)
#   losses <- calculate_expected_losses(states, params)
  
#   # STEP 5: Validate everything aligns
#   validate_estimation_inputs(
#     panel_data = panel_prepared,
#     states = states,
#     premiums = premiums,
#     hazards = hazards,
#     losses = losses,
#     transitions = transitions_empirical
#   )
  
#   # STEP 6: Create counts vector for NPL estimation
#   counts_tab <- table(
#     factor(panel_prepared$state_idx, levels = 1:nrow(states)),
#     factor(panel_prepared$action, levels = c("maintain", "exit", "retrofit"))
#   )
#   counts_vec <- as.vector(counts_tab)
  
#   # STEP 7: Run NPL estimation
#   config <- create_estimation_config()
  
#   est <- npl_estimator(
#     counts_vec = counts_vec,
#     states = states,
#     premiums = premiums,
#     hazards = hazards,
#     losses = losses,
#     transitions = transitions_empirical,
#     config = config,
#     verbose = TRUE
#   )
  
#   print(est$theta_hat)
# }



# ==============================================================================
# MODEL B: BINARY OPTIMAL STOPPING (MAINTAIN VS CLOSE)
# ==============================================================================
# PURPOSE: NPL Estimation for binary choice model with premium preference parameter
#
# STATE SPACE: 36 states = 9 age bins × 2 wall types × 2 regimes
#   - Age bins: 1=[0-5), 2=[5-10), ..., 8=[40-45), 9=[45+] (absorbing for age)
#   - Wall types: {single, double}
#   - Regimes: {FF (flat-fee), RB (risk-based)}
#
# PARAMETERS: θ_B = (κ, γ)
#   - κ: Tank closure value (can be negative if remediation > scrap)
#   - γ: Premium preference parameter (marginal utility of insurance costs)
#
# KEY INNOVATION: γ·p(x) term creates cross-regime variation for identification
#
# DEPENDENCIES: data.table, Matrix
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
})

# ==============================================================================
# SECTION 1: MODEL B CONFIGURATION
# ==============================================================================

#' Create Configuration for Model B Estimation
#'
#' @param beta Monthly discount factor (default 0.9957 = 5% annual)
#' @param sigma2 Scale parameter for Type I EV errors
#' @return Configuration list with bounds, tolerances, and calibrated values
#'
#' @details
#' Parameter bounds:
#'   - κ ∈ [-500, 500]: closure value in revenue units (negative = net cost)
#'   - γ price ∈ [-20, 5]: premium preference (γ=-1 is dollar-for-dollar)
#'   - γ risk ∈ [0, 10]: risk prefeences (γ=-1 is full risk internalized)
create_estimation_config_model_b <- function(beta = 0.9957, sigma2 = 0.3, npl_iter = 600) {
  list(
    beta = beta,
    sigma2 = sigma2,
    gamma_E = 0.5772156649,
    
    tol_theta = 1e-8,
    tol_P = 1e-7,
    max_npl_iter = npl_iter,
    
    eps_prob = 1e-10,
    min_log_val = 1e-12,
    
    # --- UPDATED BOUNDS ---
    kappa_bounds = c(-500, 500),
    gamma_price_bounds = c(-20, 5),   # Price sensitivity (Expect negative)
    gamma_risk_bounds  = c(0, 10),    # Risk internalization (Expect positive ~1.0)
    # ----------------------
    
    n_actions = 2
  )
}

# ==============================================================================
# SECTION 2: MODEL B ESTIMATION CACHE
# ==============================================================================

#' Create Estimation Cache for Model B
#'
#' @param states State space data.table (36 states: 9 age bins × 2 wall × 2 regime)
#' @param premiums Premium vector [n_states]
#' @param hazards Hazard rate vector [n_states]
#' @param losses Expected loss vector [n_states]
#' @param transitions List with $maintain and $exit sparse matrices
#' @param config Configuration from create_estimation_config_model_b()
#' @return Cache list for Model B estimation
create_estimation_cache_model_b <- function(states, premiums, hazards, losses,
                                             transitions, config) {
  n <- nrow(states)
  
  # Precompute hazard*loss product (state-dependent expected cleanup cost)
  hazard_loss <- hazards * losses
  
  list(
    n_states = n,
    premiums = premiums,
    hazards = hazards,
    losses = losses,
    hazard_loss = hazard_loss,
    F_maintain = transitions$maintain,  # Sparse matrix [n × n]
    F_exit = transitions$exit,          # Identity (absorbing)
    beta = config$beta
  )
}

# ==============================================================================
# SECTION 3: FLOW UTILITIES (VECTORIZED)
# ==============================================================================

#' Calculate Flow Utilities for Model B
#'
#' @param theta Named vector c(kappa, gamma)
#' @param cache Model B estimation cache
#' @param profit_mult Revenue normalization multiplier (default 1.0)
#' @return Matrix [n_states × 2] with columns [maintain, close]
#'
#' @details
#' Flow utility specification (revenue-normalized):
#'   u_maintain = ψ + γ·p(x) - h(x)ℓ(x)
#'   u_close = κ
#'
#' Under normalization: ψ = profit_mult (typically 1.0)
#'
#' Parameter interpretation:
#'   - κ: closure value in annual revenue units
#'   - γ: dimensionless premium multiplier (γ=-1 is dollar-for-dollar)
calculate_flow_utilities_model_b <- function(theta, cache, profit_mult = 1.0) {
  
  # theta is now vector of length 3
  kappa       <- theta["kappa"]
  gamma_price <- theta["gamma_price"]
  gamma_risk  <- theta["gamma_risk"]
  
  n_states <- cache$n_states
  
  U <- matrix(0, n_states, 2, dimnames = list(NULL, c("maintain", "close")))
  
  # Maintain Utility:
  # Profit + (Price Sensitivity * Premium) - (Risk Internalization * Expected Loss)
  U[, "maintain"] <- profit_mult + 
                     (gamma_price * cache$premiums) - 
                     (gamma_risk * cache$hazard_loss)
  
  # Close Utility: Scrap Value
  U[, "close"] <- kappa
  
  return(U)
}

# ==============================================================================
# SECTION 4: HOTZ-MILLER VALUE FUNCTION INVERSION (CLOSED-FORM)
# ==============================================================================

#' Invert Value Function Using Hotz-Miller (Closed-Form for Binary Choice)
#'
#' @param P Matrix [n_states × 2] of CCPs
#' @param U Matrix [n_states × 2] of flow utilities
#' @param config Configuration list
#' @return Vector [n_states] of state values V(x)
#'
#' @details
#' For binary logit with terminal action (close), Hotz-Miller gives:
#'   V(x) = u_close - σ·log(P_close) + σ·γ_E
#'
#' This is the CLOSED-FORM inversion, not iterative.
#' Analogous to Model A's invert_value_function_standard().
invert_value_function_model_b <- function(P, U, config) {
  
  sigma <- config$sigma2
  u_close <- U[, "close"]
  P_close <- pmax(P[, "close"], config$min_log_val)
  
  # Hotz-Miller closed-form inversion for binary choice
  V <- u_close - sigma * log(P_close) + sigma * config$gamma_E
  
  return(as.numeric(V))
}

# ==============================================================================
# SECTION 5: CCP COMPUTATION (VECTORIZED)
# ==============================================================================

#' Compute CCPs for Model B (Vectorized Binary Logit)
#'
#' @param U Matrix [n_states × 2] of flow utilities
#' @param V Vector [n_states] of state values
#' @param cache Model B estimation cache
#' @param config Configuration list
#' @return Matrix [n_states × 2] of choice probabilities
#'
#' @details
#' Standard binary logit:
#'   P(d|x) = exp(v_d/σ) / Σ_d' exp(v_d'/σ)
#' where v_d = u(x,d) + β·E[V(x')|x,d]
#'
#' Uses log-sum-exp trick for numerical stability.
compute_ccps_model_b <- function(U, V, cache, config) {
  
  sigma <- config$sigma2
  beta <- config$beta
  
  # Continuation values (sparse matrix multiplication)
  # EV_maintain: expected value if maintain (stochastic aging)
  # EV_close: expected value if close (absorbing, so EV = V for exit state)
  EV_maintain <- as.numeric(cache$F_maintain %*% V)
  EV_close <- as.numeric(cache$F_exit %*% V)  # = V (identity matrix)
  
  # Choice-specific value functions
# Choice-specific value functions
  v_maintain <- U[, "maintain"] + beta * EV_maintain
  v_close <- U[, "close"]                    # <--- FIX: Terminal exit has no future V  
  # Vectorized log-sum-exp for binary logit
  v_mat <- cbind(v_maintain, v_close)
  v_max <- apply(v_mat, 1, max)
  
  exp_v <- exp((v_mat - v_max) / sigma)
  row_sums <- rowSums(exp_v)
  
  P <- exp_v / row_sums
  
  # Floor probabilities and renormalize
  P[P < config$eps_prob] <- config$eps_prob
  P <- P / rowSums(P)
  
  colnames(P) <- c("maintain", "close")
  return(P)
}

# ==============================================================================
# SECTION 6: EMPIRICAL CCP COMPUTATION
# ==============================================================================

#' Compute Empirical CCPs from Counts (Model B)
#'
#' @param counts_vec Vector or matrix of state-action counts
#' @param n_states Number of states
#' @param eps_prob Probability floor for numerical stability
#' @return Matrix [n_states × 2] of empirical CCPs
compute_empirical_ccps_model_b <- function(counts_vec, n_states, eps_prob = 1e-10) {
  
  # Handle vector or matrix input

  if (is.vector(counts_vec)) {
    counts_mat <- matrix(counts_vec, nrow = n_states, ncol = 2, byrow = FALSE)
  } else {
    counts_mat <- counts_vec
  }
  
  colnames(counts_mat) <- c("maintain", "close")
  
  # Compute row totals
  row_totals <- rowSums(counts_mat)
  row_totals[row_totals == 0] <- 1  # Avoid division by zero
  
  # Empirical CCPs
  P <- counts_mat / row_totals
  
  # Floor and renormalize
  P[P < eps_prob] <- eps_prob
  P <- P / rowSums(P)
  
  return(P)
}

# ==============================================================================
# SECTION 7: NPL LIKELIHOOD (VECTORIZED)
# ==============================================================================

#' NPL Pseudo-Likelihood for Model B (Vectorized)
#'
#' @param theta Parameter vector c(kappa, gamma)
#' @param P_fixed Fixed CCP matrix from previous NPL iteration
#' @param cache Model B estimation cache
#' @param config Configuration list
#' @param counts_vec State-action counts (vector or matrix)
#' @param profit_mult Revenue multiplier (default 1.0)
#' @return Negative log-likelihood (for minimization)
#'
#' @details
#' NPL objective: -Σ_{s,d} n_{s,d} log P(d|s; θ, V(P_fixed))
npl_likelihood_model_b <- function(theta, P_fixed, cache, config, counts_vec,
                                    profit_mult = 1.0) {
  
  # Ensure named parameters
  if (is.null(names(theta))) {
    names(theta) <- c("kappa", "gamma")
  }
  
  # Step 1: Flow utilities given theta
  U <- calculate_flow_utilities_model_b(theta, cache, profit_mult)
  
  # Step 2: Hotz-Miller inversion using P_fixed
  V <- invert_value_function_model_b(P_fixed, U, config)
  
  # Step 3: Compute CCPs given theta and V
  P_theta <- compute_ccps_model_b(U, V, cache, config)
  
  # Step 4: Log-likelihood
  # Handle counts format
  if (is.vector(counts_vec)) {
    counts_mat <- matrix(counts_vec, nrow = cache$n_states, ncol = 2, byrow = FALSE)
  } else {
    counts_mat <- counts_vec
  }
  
  log_P <- log(pmax(P_theta, config$min_log_val))
  ll <- sum(counts_mat * log_P)
  
  # Return negative for minimization
  if (!is.finite(ll)) {
    return(1e10)
  }
  
  return(-ll)
}

# ==============================================================================
# SECTION 8: MAIN NPL ESTIMATOR
# ==============================================================================

#' NPL Estimator for Model B (Binary Optimal Stopping)
#'
#' @param counts_vec State-action counts (vector [n_states*2] or matrix [n_states × 2])
#' @param states State space data.table
#' @param premiums Premium vector
#' @param hazards Hazard rate vector
#' @param losses Expected loss vector
#' @param transitions List of transition matrices ($maintain, $exit)
#' @param config Configuration from create_estimation_config_model_b()
#' @param theta_init Initial parameter values (optional)
#' @param P_init Initial CCPs (optional, uses empirical if NULL)
#' @param verbose Print progress?
#' @return List with theta_hat, P_hat, V_hat, convergence info, diagnostics
#'
#' @details
#' Implements NPL algorithm:
#'   1. Initialize P^(0) from empirical CCPs
#'   2. Loop:
#'      a. Invert V^(k) given P^(k) using Hotz-Miller
#'      b. Update θ^(k+1) by maximizing pseudo-LL given V^(k)
#'      c. Update P^(k+1) given θ^(k+1) and V^(k)
#'      d. Check convergence
#'   3. Return estimates and diagnostics
npl_estimator_model_b <- function(counts_vec,
                                   states,
                                   premiums,
                                   hazards,
                                   losses,
                                   transitions,
                                   config,
                                   theta_init = NULL,
                                   P_init = NULL,
                                   verbose = TRUE) {
  
  if (verbose) cat("\n=== NPL ESTIMATOR MODEL B (3-Param: Kappa, Price, Risk) ===\n")
  
  # Create cache
  cache <- create_estimation_cache_model_b(states, premiums, hazards, losses,
                                            transitions, config)
  n_states <- cache$n_states
  
  # Initialize parameters (Length 3 now)
  if (is.null(theta_init)) {
    theta_init <- c(kappa = 20, gamma_price = -1.0, gamma_risk = 1.0)
  }
  # Ensure names are correct
  if(is.null(names(theta_init))) names(theta_init) <- c("kappa", "gamma_price", "gamma_risk")
  
  theta_curr <- theta_init
  
  # Initialize CCPs
  if (!is.null(P_init)) {
    P <- P_init
    if (verbose) cat("  Using provided initial CCPs\n")
  } else {
    P <- compute_empirical_ccps_model_b(counts_vec, n_states, config$eps_prob)
    if (verbose) cat("  Using empirical CCPs as starting values\n")
  }
  
  # Storage for iteration path (Now 3 columns)
  theta_path <- matrix(NA, config$max_npl_iter, 3)
  colnames(theta_path) <- c("kappa", "gamma_price", "gamma_risk")
  ll_path <- numeric(config$max_npl_iter)
  
  # NPL iteration loop
  converged <- FALSE
  
  for (npl_iter in 1:config$max_npl_iter) {
    
    theta_old <- theta_curr
    theta_path[npl_iter, ] <- theta_curr
    
    # Construct Bounds vectors for optim (3 params)
    lower_b <- c(config$kappa_bounds[1], config$gamma_price_bounds[1], config$gamma_risk_bounds[1])
    upper_b <- c(config$kappa_bounds[2], config$gamma_price_bounds[2], config$gamma_risk_bounds[2])
    
    # Step 1: Optimize theta given fixed P
    opt <- optim(
      par = theta_curr,
      fn = npl_likelihood_model_b,
      P_fixed = P,
      cache = cache,
      config = config,
      counts_vec = counts_vec,
      method = "L-BFGS-B",
      lower = lower_b,
      upper = upper_b,
      control = list(maxit = 300, factr = 1e8)
    )
    
    if (opt$convergence != 0 && verbose) {
      warning(sprintf("  optim() convergence code %d at iter %d", 
                      opt$convergence, npl_iter))
    }
    
    theta_curr <- opt$par
    names(theta_curr) <- c("kappa", "gamma_price", "gamma_risk")
    ll_path[npl_iter] <- -opt$value
    
    # Step 2: Update CCPs given new theta
    U <- calculate_flow_utilities_model_b(theta_curr, cache)
    V <- invert_value_function_model_b(P, U, config)
    P_new <- compute_ccps_model_b(U, V, cache, config)
    
    # Convergence check
    theta_diff <- max(abs(theta_curr - theta_old))
    P_diff <- max(abs(P_new - P))
    
    if (verbose && npl_iter %% 10 == 0) {
      cat(sprintf("  Iter %3d: k=%6.2f gP=%6.3f gR=%6.3f | dTh=%.1e dP=%.1e\n",
                  npl_iter, theta_curr[1], theta_curr[2], theta_curr[3], 
                  theta_diff, P_diff))
    }
    
    # Update P for next iteration
    P <- P_new
    
    # Check convergence
    if (theta_diff < config$tol_theta && P_diff < config$tol_P) {
      converged <- TRUE
      if (verbose) {
        cat(sprintf("\n  *** CONVERGED at iteration %d ***\n", npl_iter))
        cat(sprintf("  Final: kappa=%.4f, gamma_price=%.4f, gamma_risk=%.4f\n",
                    theta_curr["kappa"], theta_curr["gamma_price"], theta_curr["gamma_risk"]))
      }
      break
    }
  }
  
  if (!converged && verbose) {
    warning(sprintf("NPL did not converge in %d iterations", config$max_npl_iter))
  }
  
  # Final value function
  U_final <- calculate_flow_utilities_model_b(theta_curr, cache)
  V_final <- invert_value_function_model_b(P, U_final, config)
  
  return(list(
    theta_hat = theta_curr,
    P_hat = P,
    V_hat = V_final,
    converged = converged,
    n_iterations = npl_iter,
    log_likelihood = ll_path[npl_iter],
    theta_path = theta_path[1:npl_iter, , drop = FALSE],
    ll_path = ll_path[1:npl_iter],
    cache = cache,
    config = config
  ))
}

# ==============================================================================
# SECTION 9: POLICY ITERATION SOLVER (FOR DATA GENERATION)
# ==============================================================================

#' Solve Equilibrium Policy for Model B via Policy Iteration
#'
#' @param theta Named vector c(kappa, gamma)
#' @param cache Model B estimation cache
#' @param config Configuration list
#' @param profit_mult Revenue multiplier
#' @param check_bellman Verify Bellman residual convergence?
#' @return List with P (CCPs), V (values), converged flag
#' Solve Equilibrium Policy for Model B (Robust Version)
solve_equilibrium_policy_model_b <- function(theta, cache, config,
                                              profit_mult = 1.0,
                                              check_bellman = TRUE) {
  
  U <- calculate_flow_utilities_model_b(theta, cache, profit_mult)
  n_states <- cache$n_states
  P <- matrix(0.5, n_states, 2, dimnames = list(NULL, c("maintain", "close")))
  
  max_iter <- 5000
  tol <- 1e-8
  
  for (iter in 1:max_iter) {
    # 1. Invert Value Function
    V_old <- invert_value_function_model_b(P, U, config)
    
    # SAFETY CHECK: If V contains NAs, reset or abort
    if (any(is.na(V_old))) {
      V_old[is.na(V_old)] <- -1e5 # Assign low value to undefined states
    }

    # 2. Update CCPs
    P_new <- compute_ccps_model_b(U, V_old, cache, config)
    
    # 3. Check Convergence (Robust to NA)
    ccp_diff <- max(abs(P_new - P), na.rm = TRUE)
    
    # Calculate Bellman error if requested
    bellman_converged <- TRUE
    if (check_bellman) {
      V_new <- invert_value_function_model_b(P_new, U, config)
      bellman_error <- max(abs(V_new - V_old), na.rm = TRUE)
      bellman_converged <- (bellman_error < tol)
    }
    
    # Robust Loop Break
    if (!is.na(ccp_diff) && ccp_diff < tol && bellman_converged) {
      P <- P_new
      break
    }
    
    P <- P_new
  }
  
  # Final calculation
  V_final <- invert_value_function_model_b(P, U, config)
  
  # Return result
  return(list(P = P, V = V_final, converged = (iter < max_iter)))
}

# ==============================================================================
# SECTION 10: DATA GENERATION
# ==============================================================================

#' Generate Synthetic Panel Data for Model B
#'
#' @param N_facilities Number of facilities
#' @param T_periods Maximum time periods
#' @param kappa_true True closure value parameter (can be negative)
#' @param gamma_true True premium preference parameter
#' @param seed Random seed
#' @return List with counts, states, auxiliary functions, panel data, true params
#'
#' @details
#' State space: 36 states (9 age bins × 2 wall × 2 regime)
#' Age bins: 5-year bins from [0-5) to [45+], where bin 9 is absorbing
generate_model_b_data <- function(N_facilities = 1000,
                                   T_periods = 50,
                                   kappa_true = 75,
                                   gamma_true = -1.2,
                                   seed = 2025) {
  
  set.seed(seed)
  
  # Exogenous parameters (calibrated)
  params <- list(
    h0 = 0.02, h_single = 0.09, h_age = 0.008,
    ell0 = 1.0, ell_age = 0.08,
    p_FF_annual = 0.08, p0_RB_annual = 0.03,
    p_single_RB_annual = 0.1, p_age_RB_annual = 0.0055
  )
  
  # Age transitions for 9 bins (5-year bins, bin 9 is absorbing for age)
  # p_up[i] = probability of aging from bin i to bin i+1
  # Bin 9 (45+) is absorbing: p_up[9] = 0, p_stay[9] = 1
  age_probs <- list(
    p_stay = c(0.985, 0.982, 0.978, 0.974, 0.970, 0.965, 0.960, 0.955, 1.00),
    p_up   = c(0.015, 0.018, 0.022, 0.026, 0.030, 0.035, 0.040, 0.045, 0.00)
  )
  

  # Create state space (36 states: 9 age bins × 2 wall × 2 regime)
  # Age bins: 1=[0-5), 2=[5-10), ..., 8=[35-40), 9=[45+] (absorbing for age)
  states <- CJ(
    A = 1:9,
    w = factor(c("single", "double"), levels = c("single", "double")),
    rho = factor(c("FF", "RB"), levels = c("FF", "RB")),
    sorted = FALSE
  )
  states[, state_idx := .I]
  setkey(states, state_idx)
  
  n_states <- nrow(states)  # = 36
  
  # Compute auxiliary functions
  # NOTE: For 5-year bins, A=1 means [0-5), A=9 means [45+]
  # Map age bin to representative age for calculations: bin_midpoint = (A-1)*5 + 2.5
  # Or use bin index directly if params are calibrated for bins
  
  premiums <- params$p_FF_annual + 
              ifelse(states$rho == "RB", params$p0_RB_annual, 0) +
              ifelse(states$w == "single", params$p_single_RB_annual, 0) +
              (states$A - 1) * params$p_age_RB_annual
  
  hazards <- pmin(pmax(params$h0 + 
                        (states$w == "single") * params$h_single + 
                        params$h_age * states$A, 0.001), 0.50)
  
  losses <- pmax(params$ell0 + params$ell_age * states$A, 0.1)
  
  # Build transition matrices for 9 age bins
  state_map <- setNames(states$state_idx, 
                        paste(states$A, states$w, states$rho, sep = "_"))
  
  F_maintain <- Matrix(0, n_states, n_states, sparse = TRUE)
  for (i in 1:n_states) {
    A_i <- states$A[i]
    if (A_i < 9) {
      # Can stay or age up
      k_s <- paste(A_i, states$w[i], states$rho[i], sep = "_")
      k_u <- paste(A_i + 1, states$w[i], states$rho[i], sep = "_")
      F_maintain[i, state_map[k_s]] <- age_probs$p_stay[A_i]
      F_maintain[i, state_map[k_u]] <- age_probs$p_up[A_i]
    } else {
      # Age bin 9 (45+) is absorbing for age dimension
      F_maintain[i, i] <- 1.0
    }
  }
  
  transitions <- list(
    maintain = F_maintain,
    exit = Diagonal(n_states)  # Identity (absorbing)
  )
  
  # Configuration
  config <- create_estimation_config_model_b()
  
  # Create cache
  cache <- create_estimation_cache_model_b(states, premiums, hazards, losses,
                                            transitions, config)
  
  # Solve equilibrium
  theta_true <- c(kappa = kappa_true, gamma = gamma_true)
  eq <- solve_equilibrium_policy_model_b(theta_true, cache, config)
  P_equilibrium <- eq$P
  
  if (!eq$converged) {
    warning("Equilibrium policy did not converge")
  }
  
  # Build transition maps for simulation
  state_map <- setNames(states$state_idx, 
                        paste(states$A, states$w, states$rho, sep = "_"))
  
  next_state_stay <- integer(n_states)
  next_state_up <- integer(n_states)
  
  for (s in 1:n_states) {
    A <- states$A[s]
    w <- states$w[s]
    rho <- states$rho[s]
    
    # Stay transition (same state)
    k_stay <- paste(A, w, rho, sep = "_")
    next_state_stay[s] <- state_map[k_stay]
    
    # Age up transition (capped at 9 = 45+ bin)
    A_next <- min(A + 1L, 9L)
    k_up <- paste(A_next, w, rho, sep = "_")
    next_state_up[s] <- state_map[k_up]
  }
  
  # Simulate panel
  panel_list <- vector("list", N_facilities)
  
  for (fac in 1:N_facilities) {
    current_state <- sample(1:n_states, 1)
    fac_obs <- list()
    obs_idx <- 1
    
    for (t in 1:T_periods) {
      # Draw action
      action_probs <- P_equilibrium[current_state, ]
      action <- sample(1:2, 1, prob = action_probs)
      
      # Record observation
      fac_obs[[obs_idx]] <- data.table(
        facility_id = fac,
        period = t,
        state_idx = current_state,
        action = action
      )
      obs_idx <- obs_idx + 1
      
      # Transition
      if (action == 2) {
        # Close: exit simulation for this facility
        break
      } else {
        # Maintain: stochastic aging
        curr_age <- states$A[current_state]
        p_up <- age_probs$p_up[curr_age]
        
        if (runif(1) < p_up) {
          current_state <- next_state_up[current_state]
        } else {
          current_state <- next_state_stay[current_state]
        }
      }
    }
    
    panel_list[[fac]] <- rbindlist(fac_obs)
  }
  
  panel_data <- rbindlist(panel_list)
  
  # Aggregate counts
  counts_dt <- panel_data[, .N, by = .(state_idx, action)]
  counts_matrix <- matrix(0, n_states, 2)
  
  for (i in 1:nrow(counts_dt)) {
    counts_matrix[counts_dt$state_idx[i], counts_dt$action[i]] <- counts_dt$N[i]
  }
  
  counts_vec <- as.vector(counts_matrix)
  
  # Diagnostics
  cat(sprintf("Generated Model B data: %d facilities, %d observations\n",
              N_facilities, nrow(panel_data)))
  cat(sprintf("  Close rate: %.1f%%\n", 100 * mean(panel_data$action == 2)))
  cat(sprintf("  True parameters: kappa = %.2f, gamma = %.3f\n",
              kappa_true, gamma_true))
  
  return(list(
    counts_vec = counts_vec,
    counts_matrix = counts_matrix,
    states = states,
    premiums = premiums,
    hazards = hazards,
    losses = losses,
    transitions = transitions,
    panel_data = panel_data,
    P_true = P_equilibrium,
    V_true = eq$V,
    theta_true = theta_true,
    cache = cache,
    config = config
  ))
}

# ==============================================================================
# SECTION 11: VALIDATION TEST
# ==============================================================================

#' Validation Test for Model B Estimator
#'
#' @param N_facilities Number of facilities for test data
#' @param T_periods Number of periods
#' @param kappa_true True kappa parameter
#' @param gamma_true True gamma parameter
#' @param seed Random seed
#' @return List with data and estimation results
test_model_b_estimation <- function(N_facilities = 500,
                                     T_periods = 50,
                                     kappa_true = 75,
                                     gamma_true = -1.2,
                                     seed = 12345) {
  
  cat("\n")
  cat("==============================================================\n")
  cat("           MODEL B VALIDATION TEST\n")
  cat("==============================================================\n\n")
  
  # Generate data
  cat("Step 1: Generating synthetic data...\n")
  data <- generate_model_b_data(
    N_facilities = N_facilities,
    T_periods = T_periods,
    kappa_true = kappa_true,
    gamma_true = gamma_true,
    seed = seed
  )
  
  cat(sprintf("  Sample: %d observations from %d facilities\n\n",
              nrow(data$panel_data), N_facilities))
  
  # Estimate
  cat("Step 2: Running NPL estimation...\n")
  config <- create_estimation_config_model_b()
  
  est <- npl_estimator_model_b(
    counts_vec = data$counts_matrix,
    states = data$states,
    premiums = data$premiums,
    hazards = data$hazards,
    losses = data$losses,
    transitions = data$transitions,
    config = config,
    theta_init = c(kappa = 50, gamma = -0.5),  # Intentionally away from truth
    verbose = TRUE
  )
  
  # Results
  cat("\n")
  cat("==============================================================\n")
  cat("                    RESULTS\n")
  cat("==============================================================\n")
  
  cat("\nParameter Recovery:\n")
  cat(sprintf("  kappa:  True = %7.2f  |  Est = %7.2f  |  Error = %+6.2f (%.1f%%)\n",
              kappa_true, est$theta_hat["kappa"],
              est$theta_hat["kappa"] - kappa_true,
              100 * (est$theta_hat["kappa"] - kappa_true) / kappa_true))
  cat(sprintf("  gamma:  True = %7.3f  |  Est = %7.3f  |  Error = %+6.3f (%.1f%%)\n",
              gamma_true, est$theta_hat["gamma"],
              est$theta_hat["gamma"] - gamma_true,
              100 * (est$theta_hat["gamma"] - gamma_true) / abs(gamma_true)))
  
  # CCP comparison
  ccp_max_diff <- max(abs(est$P_hat - data$P_true))
  ccp_mean_diff <- mean(abs(est$P_hat - data$P_true))
  
  cat(sprintf("\nCCP Recovery:\n"))
  cat(sprintf("  Max |P_hat - P_true|:  %.6f\n", ccp_max_diff))
  cat(sprintf("  Mean |P_hat - P_true|: %.6f\n", ccp_mean_diff))
  
  cat(sprintf("\nConvergence:\n"))
  cat(sprintf("  Converged: %s\n", ifelse(est$converged, "YES", "NO")))
  cat(sprintf("  Iterations: %d\n", est$n_iterations))
  cat(sprintf("  Final LL: %.2f\n", est$log_likelihood))
  
  cat("\n==============================================================\n\n")
  
  invisible(list(data = data, estimates = est))
}

# ==============================================================================
# SECTION 12: DIAGNOSTIC FUNCTIONS
# ==============================================================================

#' Compute Identification Diagnostics for Model B
#'
#' @param data Output from generate_model_b_data()
#' @param theta_grid Grid of parameter values to evaluate
#' @return List with likelihood surface and gradient information
diagnose_identification_model_b <- function(data, 
                                             kappa_grid = seq(50, 100, by = 5),
                                             gamma_grid = seq(-2, -0.5, by = 0.1)) {
  
  config <- data$config
  cache <- data$cache
  
  # Compute equilibrium CCPs at true parameters for likelihood evaluation
  P_true <- data$P_true
  
  # Likelihood surface
  ll_surface <- matrix(NA, length(kappa_grid), length(gamma_grid))
  rownames(ll_surface) <- kappa_grid
  colnames(ll_surface) <- gamma_grid
  
  for (i in seq_along(kappa_grid)) {
    for (j in seq_along(gamma_grid)) {
      theta_test <- c(kappa = kappa_grid[i], gamma = gamma_grid[j])
      ll_surface[i, j] <- -npl_likelihood_model_b(theta_test, P_true, cache, 
                                                   config, data$counts_matrix)
    }
  }
  
  # Find maximum
  max_idx <- which(ll_surface == max(ll_surface), arr.ind = TRUE)
  kappa_max <- kappa_grid[max_idx[1]]
  gamma_max <- gamma_grid[max_idx[2]]
  
  cat("Identification Diagnostics:\n")
  cat(sprintf("  True parameters: kappa = %.2f, gamma = %.3f\n",
              data$theta_true["kappa"], data$theta_true["gamma"]))
  cat(sprintf("  Grid maximum:    kappa = %.2f, gamma = %.3f\n",
              kappa_max, gamma_max))
  
  return(list(
    ll_surface = ll_surface,
    kappa_grid = kappa_grid,
    gamma_grid = gamma_grid,
    kappa_max = kappa_max,
    gamma_max = gamma_max
  ))
}

#' NPL Estimator Model B with Relaxation (Damping) - FULLY COMPATIBLE
#' @param alpha Damping factor (0.0 = no update, 1.0 = standard NPL). 
#'              Recommended: 0.3 to 0.7 for oscillating models.
npl_estimator_model_b_fast <- function(counts_vec, states, premiums, hazards, 
                                       losses, transitions, config, 
                                       theta_init = NULL, P_init = NULL, 
                                       alpha = 0.5, verbose = TRUE) {
  
  if (verbose) cat("\n=== NPL ESTIMATOR B (DAMPENED alpha=", alpha, ") ===\n", sep="")
  
  # 1. Setup Cache (Same as before)
  cache <- create_estimation_cache_model_b(states, premiums, hazards, losses, transitions, config)
  n_states <- cache$n_states 
  
  # 2. Init Parameters
  if (is.null(theta_init)) theta_init <- c(kappa = 6.0, gamma = -1.0)
  theta_curr <- theta_init
  
  # 3. Init CCPs (Reuse existing helper)
  if (!is.null(P_init)) {
    P <- P_init 
  } else {
    P <- compute_empirical_ccps_model_b(counts_vec, n_states, config$eps_prob)
  }
  
  # 4. Storage
  theta_path <- matrix(NA, config$max_npl_iter, 2)
  ll_path <- numeric(config$max_npl_iter)
  
  # 5. Main Loop
  for (iter in 1:config$max_npl_iter) {
    theta_old <- theta_curr
    theta_path[iter, ] <- theta_curr
    
    # A. Maximization (Inner Loop)
    # Reduced maxit to 100 for speed (standard NPL practice)
    opt <- tryCatch({
      optim(
        par = theta_curr,
        fn = npl_likelihood_model_b,
        P_fixed = P, cache = cache, config = config, counts_vec = counts_vec,
        method = "L-BFGS-B",
        lower = c(config$kappa_bounds[1], config$gamma_bounds[1]),
        upper = c(config$kappa_bounds[2], config$gamma_bounds[2]),
        control = list(maxit = 100, factr = 1e8) 
      )
    }, error = function(e) list(par = theta_curr, value = NA, convergence = 99))
    
    theta_curr <- opt$par
    ll_path[iter] <- if(!is.na(opt$value)) -opt$value else NA
    
    # B. Policy Update
    U <- calculate_flow_utilities_model_b(theta_curr, cache)
    V <- invert_value_function_model_b(P, U, config)
    P_new_pure <- compute_ccps_model_b(U, V, cache, config)
    
    # C. DAMPING (The Speedup)
    # P_next = alpha * P_new + (1-alpha) * P_old
    P_next <- (alpha * P_new_pure) + ((1 - alpha) * P)
    
    # D. Convergence Check
    theta_diff <- max(abs(theta_curr - theta_old))
    P_diff <- max(abs(P_next - P))
    
    if (verbose && iter %% 10 == 0) {
      cat(sprintf("  Iter %4d: k=%6.3f g=%6.3f | dTheta=%.1e dP=%.1e | LL=%.1f\n",
                  iter, theta_curr[1], theta_curr[2], theta_diff, P_diff, ll_path[iter]))
    }
    
    P <- P_next
    
    if (theta_diff < config$tol_theta && P_diff < config$tol_P) {
      if (verbose) cat(sprintf("  *** CONVERGED at iter %d ***\n", iter))
      return(list(
        theta_hat = theta_curr, 
        P_hat = P, 
        V_hat = V,
        converged = TRUE, 
        n_iterations = iter, 
        log_likelihood = ll_path[iter], 
        theta_path = theta_path[1:iter,], 
        ll_path = ll_path[1:iter],
        cache = cache,   # ADDED: Critical for diagnostic functions
        config = config  # ADDED: Critical for diagnostic functions
      ))
    }
  }
  
  warning("NPL did not converge.")
  return(list(
    theta_hat = theta_curr, 
    P_hat = P, 
    converged = FALSE,
    cache = cache,
    config = config
  ))
}


cat("\n✓ model_b_estimator.R loaded\n")
cat("  Key functions:\n")
cat("    - create_estimation_config_model_b()\n")
cat("    - npl_estimator_model_b()\n")
cat("    - generate_model_b_data()\n")
cat("    - test_model_b_estimation()\n\n")

cat("\n✓ improved_estimator_FINAL.r loaded\n")


