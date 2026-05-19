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
  # 9 age bins x 2 wall types x 2 regimes = 36 states.
  # Bin 9 is absorbing within Maintain (oldest age).
  states <- CJ(
    A = 1:9,
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
  
v_mat <- cbind(
  U[, "maintain"] + beta * EV_m,
  U[, "exit"],
  U[, "retrofit"] + beta * EV_r
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
#   states <- create_state_space_est()  # Default: A=1:9, w={single,double}, rho={FF,RB}
  
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
create_estimation_config_model_b <- function(beta = 0.9957, sigma2 = 0.3, npl_iter = 5000) {
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
    kappa_bounds = c(-999999, 9999999),
    gamma_price_bounds = c(-20, 20),   # Price sensitivity (Expect negative)
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
  
 # Validate theta has required names
 if (!all(c("kappa", "gamma_price", "gamma_risk") %in% names(theta))) {
    stop("theta must be named vector with: kappa, gamma_price, gamma_risk")
  }
  


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
invert_value_function_model_b <- function(P, U, cache, config) {
  # AM (2002) Hotz-Miller policy-marginal inversion for Maintain/Close.
  # Close is terminal => continuation only flows through Maintain.
  # Solve linear system (I - beta * P_m * F_maintain) V = R.
  sigma   <- config$sigma2
  beta    <- cache$beta
  gamma_E <- config$gamma_E
  n       <- cache$n_states

  P_m <- pmax(P[, "maintain"], config$min_log_val)
  P_c <- pmax(P[, "close"],    config$min_log_val)

  R <- P_m * (U[, "maintain"] + sigma * (gamma_E - log(P_m))) +
       P_c * (U[, "close"]    + sigma * (gamma_E - log(P_c)))

  F_P <- Diagonal(x = as.numeric(P_m)) %*% cache$F_maintain
  V   <- as.numeric(solve(Diagonal(n) - beta * F_P, R))

  return(V)
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

  # Choice-specific value functions (Close is terminal => no continuation)
  v_maintain <- U[, "maintain"] + beta * EV_maintain
  v_close    <- U[, "close"]

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
    names(theta) <- c("kappa", "gamma_price", "gamma_risk")
}
  # Step 1: Flow utilities given theta
  U <- calculate_flow_utilities_model_b(theta, cache, profit_mult)
  
  # Step 2: Hotz-Miller inversion using P_fixed
  V <- invert_value_function_model_b(P_fixed, U, cache, config)
  
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
    
# Add after this line:
if (npl_iter > 1 && ll_path[npl_iter] < ll_path[npl_iter - 1] - 1e-6) {
  if (verbose) warning(sprintf("  LL decreased at iter %d: %.2f -> %.2f", 
                                npl_iter, ll_path[npl_iter-1], ll_path[npl_iter]))
}


    # Step 2: Update CCPs given new theta
    U <- calculate_flow_utilities_model_b(theta_curr, cache)
    V <- invert_value_function_model_b(P, U, cache, config)
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
  V_final <- invert_value_function_model_b(P, U_final, cache, config)
  
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
    V_old <- invert_value_function_model_b(P, U, cache, config)
    
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
      V_new <- invert_value_function_model_b(P_new, U, cache, config)
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
  V_final <- invert_value_function_model_b(P, U, cache, config)

  # Return result
  return(list(P = P, V = V_final, converged = (iter < max_iter)))
}

# ==============================================================================
# SECTION 10: DATA GENERATION
# ==============================================================================


# ==============================================================================
# 2. OVERRIDES: DATA GENERATOR FOR ANNUAL TRANSITIONS
# ==============================================================================

generate_model_b_data_annual <- function(N_facilities = 1000,
                                         T_periods = 50,  # 50 Years
                                         kappa_true = 6.25, # ~6.25 years of profit
                                         gamma_price_true = -1.2, # Price Sensitivity
                                         gamma_risk_true = 1.0,   # Risk Internalization
                                         seed = 2025,
                                         P_manual = NULL) { # <--- NEW ARGUMENT for Calibration
  
  set.seed(seed)
  
  # ----------------------------------------------------------------------------
  # A. Exogenous Parameters (Annual Rates)
  # ----------------------------------------------------------------------------
  params <- list(
    # INCREASED HAZARDS (Annual probabilities of leak/failure)
    h0 = 0.12,          # Base 12% annual failure risk
    h_single = 0.05,    # Extra risk for single wall
    h_age = 0.005,      # Risk increase per age bin
    
    # LOSS SEVERITY (Conditional on failure)
    ell0 = 1.0,         # Base loss size
    ell_age = 0.05,     
    
    # INCREASED PREMIUMS (Annual insurance cost)
    p_FF_annual = 0.20,        # Base cost higher
    p0_RB_annual = 0.10,       # Base for Risk-Based
    p_single_RB_annual = 0.10, # Penalty for single wall
    p_age_RB_annual = 0.02     # Penalty per age bin
  )  
  
  # ----------------------------------------------------------------------------
  # B. Annual Transition Probabilities
  # ----------------------------------------------------------------------------
  age_probs_annual <- list(
    p_up   = c(0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.24, 0.25, 0.00)
  )
  age_probs_annual$p_stay <- 1.0 - age_probs_annual$p_up
  
  # ----------------------------------------------------------------------------
  # C. State Space Construction
  # ----------------------------------------------------------------------------
  states <- CJ(
    A = 1:9,
    w = factor(c("single", "double"), levels = c("single", "double")),
    rho = factor(c("FF", "RB"), levels = c("FF", "RB")),
    sorted = FALSE
  )
  states[, state_idx := .I]
  setkey(states, state_idx)
  
  n_states <- nrow(states)
  
  # ----------------------------------------------------------------------------
  # D. Auxiliary Variables (Flow Utility Inputs)
  # ----------------------------------------------------------------------------
 #FF: flat premium (pooling contract, no risk variation)
  # RB: base + wall-type surcharge + age surcharge (risk-priced)
  is_RB <- (states$rho == "RB")

  premiums <- ifelse(is_RB,
    params$p0_RB_annual +
      (states$w == "single") * params$p_single_RB_annual +
      (states$A - 1) * params$p_age_RB_annual,
    params$p_FF_annual
  )

  hazards <- pmin(pmax(params$h0 + 
                         (states$w == "single") * params$h_single + 
                         params$h_age * states$A, 0.001), 0.60) # Cap at 60%
  
  losses <- pmax(params$ell0 + params$ell_age * states$A, 0.1)
  
  # ----------------------------------------------------------------------------
  # E. Transition Matrices (Annual)
  # ----------------------------------------------------------------------------
  state_map <- setNames(states$state_idx, 
                        paste(states$A, states$w, states$rho, sep = "_"))
  
  F_maintain <- Matrix(0, n_states, n_states, sparse = TRUE)
  
  for (i in 1:n_states) {
    A_i <- states$A[i]
    if (A_i < 9) {
      k_s <- paste(A_i, states$w[i], states$rho[i], sep = "_")
      k_u <- paste(A_i + 1, states$w[i], states$rho[i], sep = "_")
      F_maintain[i, state_map[k_s]] <- age_probs_annual$p_stay[A_i]
      F_maintain[i, state_map[k_u]] <- age_probs_annual$p_up[A_i]
    } else {
      # Absorbing age state
      F_maintain[i, i] <- 1.0
    }
  }
  
  transitions <- list(
    maintain = F_maintain,
    exit = Diagonal(n_states)
  )
  
  # ----------------------------------------------------------------------------
  # F. Solve & Simulate
  # ----------------------------------------------------------------------------
  config <- create_estimation_config_model_b(beta = 0.95, sigma2 = 1) 
  cache <- create_estimation_cache_model_b(states, premiums, hazards, losses, transitions, config)
  
  theta_true <- c(kappa = kappa_true, 
                  gamma_price = gamma_price_true, 
                  gamma_risk = gamma_risk_true)
  
  # --- CRITICAL FIX: Use manual policy if provided (prevents re-solving crash) ---
  if (!is.null(P_manual)) {
    P_equilibrium <- P_manual
    # Compute V for consistency (optional)
    U <- calculate_flow_utilities_model_b(theta_true, cache)
    V_equilibrium <- invert_value_function_model_b(P_equilibrium, U, cache, config)
  } else {
    # Standard: Solve internally
    eq <- solve_equilibrium_policy_model_b(theta_true, cache, config)
    if (!eq$converged) warning("Equilibrium policy did not converge")
    P_equilibrium <- eq$P
    V_equilibrium <- eq$V
  }
  
  # Simulate Panel
  # Pre-calculate transition destinations for speed
  next_state_stay <- integer(n_states)
  next_state_up <- integer(n_states)
  
  for (s in 1:n_states) {
    A <- states$A[s]
    w <- states$w[s]
    rho <- states$rho[s]
    next_state_stay[s] <- state_map[paste(A, w, rho, sep = "_")]
    next_state_up[s]   <- state_map[paste(min(A + 1L, 9L), w, rho, sep = "_")]
  }
  
  # Vectorized Simulation Loop
  panel_list <- vector("list", N_facilities)
  
  for (fac in 1:N_facilities) {
    # Random initial state
    current_state <- sample(1:n_states, 1)
    
    # Pre-allocate facility vectors
    fac_states <- integer(T_periods)
    fac_actions <- integer(T_periods)
    active <- TRUE
    
    for (t in 1:T_periods) {
      if (!active) break
      
      # Decision
      prob_maintain <- P_equilibrium[current_state, "maintain"]
      
      # --- ROBUSTNESS FIX: Handle NA probabilities ---
      if (is.na(prob_maintain)) {
        prob_maintain <- 1.0 # Default to maintain if solver failed for this state
      }
      
      action <- if (runif(1) < prob_maintain) 1L else 2L # 1=Maintain, 2=Close
      
      fac_states[t] <- current_state
      fac_actions[t] <- action
      
      if (action == 2) {
        active <- FALSE
      } else {
        # Annual Transition: Age up or Stay
        p_up <- age_probs_annual$p_up[states$A[current_state]]
        if (runif(1) < p_up) {
          current_state <- next_state_up[current_state]
        } else {
          current_state <- next_state_stay[current_state]
        }
      }
    }
    
    # Trim to actual observed periods
    n_obs <- which(fac_actions == 0)[1] - 1
    if (is.na(n_obs)) n_obs <- T_periods
    
    if (n_obs > 0) {
      panel_list[[fac]] <- data.table(
        facility_id = fac,
        period = 1:n_obs,
        state_idx = fac_states[1:n_obs],
        action = fac_actions[1:n_obs]
      )
    }
  }
  
  panel_data <- rbindlist(panel_list)
  
  # Create Counts Matrix
  counts_dt <- panel_data[, .N, by = .(state_idx, action)]
  counts_matrix <- matrix(0, n_states, 2)
  for (i in 1:nrow(counts_dt)) {
    counts_matrix[counts_dt$state_idx[i], counts_dt$action[i]] <- counts_dt$N[i]
  }
  
  return(list(
    counts_vec = as.vector(counts_matrix),
    counts_matrix = counts_matrix,
    states = states,
    premiums = premiums,
    hazards = hazards,
    losses = losses,
    transitions = transitions,
    panel_data = panel_data,
    P_true = P_equilibrium,
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
    V <- invert_value_function_model_b(P, U, cache, config)
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

# ==============================================================================
# DAMPED NPL ESTIMATOR (Add to improved_estimator_OPTIMIZED.r)
# ==============================================================================
# Purpose: Solves NPL fixed point with relaxation to prevent oscillations.
#          P_{t+1} = alpha * P_{new} + (1 - alpha) * P_{t}
# Params:
#   alpha: Step size (0 to 1). 
#          1.0 = Standard NPL (Fast, unstable)
#          0.5 = Damped (Stable, slower)
# ==============================================================================

npl_estimator_damped <- function(counts_vec, states, premiums, hazards, losses, 
                                 transitions, config, theta_init, alpha=0.5, verbose=TRUE) {
  
  # 1. Build Cache
  cache <- create_estimation_cache_model_b(states, premiums, hazards, losses, transitions, config)
  n_states <- cache$n_states
  
  # 2. Init P (Empirical)
  P <- compute_empirical_ccps_model_b(counts_vec, n_states, config$eps_prob)
  theta_curr <- theta_init
  
  # History
  theta_path <- matrix(NA, nrow=config$max_npl_iter, ncol=length(theta_init))
  
  for (iter in 1:config$max_npl_iter) {
    theta_old <- theta_curr
    
    # 3. Maximization (M-Step)
    # Using L-BFGS-B with wide bounds to prevent runaway parameters
    opt <- optim(par = theta_curr, fn = npl_likelihood_model_b,
                 P_fixed = P, cache = cache, config = config, counts_vec = counts_vec,
                 method = "L-BFGS-B", 
                 lower = c(0, -10, 0),   # Kappa>0, Price>-10, Risk>0
                 upper = c(100, 10, 10), # Upper bounds
                 control = list(maxit = 100))
    
    theta_curr <- opt$par
    names(theta_curr) <- names(theta_init)
    theta_path[iter, ] <- theta_curr
    
    # 4. Policy Update (With Damping)
    U <- calculate_flow_utilities_model_b(theta_curr, cache)
    V <- invert_value_function_model_b(P, U, cache, config)
    P_new_pure <- compute_ccps_model_b(U, V, cache, config)
    
    # DAMPING: Mix new policy with old policy
    P_next <- (alpha * P_new_pure) + ((1 - alpha) * P)
    
    # 5. Convergence Check
    theta_diff <- max(abs(theta_curr - theta_old))
    P_diff <- max(abs(P_next - P))
    
    if (verbose && iter %% 5 == 0) {
      cat(sprintf("  Iter %3d: k=%5.2f gP=%5.2f gR=%5.2f | dTh=%.1e dP=%.1e | LL=%.1f\n",
                  iter, theta_curr[1], theta_curr[2], theta_curr[3], theta_diff, P_diff, -opt$value))
    }
    
    P <- P_next
    
    if (theta_diff < config$tol_theta && P_diff < config$tol_P) {
      if (verbose) cat(sprintf("  *** CONVERGED at iter %d ***\n", iter))
      return(list(
        theta_hat = theta_curr, 
        P = P, 
        V = V, 
        converged = TRUE,
        theta_path = theta_path[1:iter,],
        cache = cache
      ))
    }
  }
  
  if (verbose) cat("  *** MAX ITERATIONS REACHED ***\n")
  return(list(
    theta_hat = theta_curr,
    P = P,
    V = V,
    converged = FALSE,
    theta_path = theta_path,
    cache = cache
  ))
}


# ==============================================================================
# ==============================================================================
# REPLACEMENT MODEL — OPTION A: 3-ACTION MULTINOMIAL LOGIT
# ==============================================================================
# ==============================================================================
# Three actions at the state level (Maintain, Exit, Replace). K enters the
# value function through proper continuation via F_R, not only through the
# observation-level likelihood.
#
# STATE SPACE: 32 states = 8 age bins x 2 wall types x 2 regimes
#   s_idx = (rho - 1) * 16 + (w - 1) * 8 + A_bin    in 1..32
#
# CHOICE-SPECIFIC VALUES (state-level, given V):
#   v_M(s) = u_M(s) + beta * (F_M %*% V)[s]
#   v_E(s) = kappa_exit                          (terminal: no continuation)
#   v_R(s) = -K + beta * (F_R %*% V)[s]          (F_R[s, reset(s)] = 1)
#
# AM-2002 inversion remains LINEAR in V given P_fixed:
#   V = R(P) + beta * (diag(P_M) %*% F_M + diag(P_R) %*% F_R) %*% V
# ==============================================================================


# ==============================================================================
# REPLACEMENT - SECTION 1: CONFIGURATION
# ==============================================================================
create_estimation_config_replacement <- function(beta     = 0.95,
                                                 sigma2   = 1.0,
                                                 npl_iter = 200) {
  list(
    beta    = beta,
    sigma2  = sigma2,
    gamma_E = 0.5772156649,

    # tol_theta = 1e-5 (not 1e-6) because optim's factr=1e8 leaves ~1e-6
    # noise in theta; a tighter outer-loop tol just spins iterations.
    # tol_P matched to tol_theta for the same reason (P_diff floor is the
    # binding constraint when theta has already settled).
    tol_theta = 1e-5,
    tol_P     = 1e-5,
    max_npl_iter = npl_iter,

    eps_prob    = 1e-12,
    min_log_val = 1e-12,
    v_clip      = 700,

    kappa_exit_bounds  = c(-200,           500),
    K_log_bounds       = c(log(0.01),      log(1000)),
    gamma_price_bounds = c(-20,            5),
    gamma_risk_bounds  = c(-5,             10),

    n_actions = 3L,
    n_params  = 4L,

    action_labels = c("maintain", "exit", "replace")
  )
}


# ==============================================================================
# REPLACEMENT - SECTION 2: STATE-LEVEL HELPERS
# ==============================================================================
build_state_lut_replacement <- function() {
  lut <- data.table::CJ(rho_state = 1:2, w_state = 1:2, A_bin = 1:8)
  lut[, s_idx := (rho_state - 1L) * 16L + (w_state - 1L) * 8L + A_bin]
  data.table::setorder(lut, s_idx)
  lut
}

build_reset_state_lut <- function(state_lut) {
  reset_idx <- integer(nrow(state_lut))
  for (s in seq_len(nrow(state_lut))) {
    reset_idx[s] <- (state_lut$rho_state[s] - 1L) * 16L +
                    (state_lut$w_state[s]   - 1L) * 8L  + 1L
  }
  reset_idx
}

build_F_replace <- function(reset_state_lut) {
  n <- length(reset_state_lut)
  Matrix::sparseMatrix(i = seq_len(n),
                       j = reset_state_lut,
                       x = rep.int(1, n),
                       dims = c(n, n))
}


# ==============================================================================
# REPLACEMENT - SECTION 3: ESTIMATION CACHE
# ==============================================================================
create_estimation_cache_replacement <- function(primitives, obs_panel, config) {

  n_states <- 32L
  stopifnot(length(primitives$h_vec)  == n_states,
            length(primitives$L_vec)  == n_states,
            length(primitives$P_vec)  == n_states,
            nrow(primitives$F_maintain) == n_states,
            ncol(primitives$F_maintain) == n_states)

  state_lut <- if (!is.null(primitives$state_lut)) {
    data.table::as.data.table(primitives$state_lut)
  } else {
    build_state_lut_replacement()
  }
  data.table::setorder(state_lut, s_idx)

  reset_state_lut <- build_reset_state_lut(state_lut)
  F_replace       <- build_F_replace(reset_state_lut)

  hazard_loss <- primitives$h_vec * primitives$L_vec

  s_obs <- as.integer(obs_panel$s_idx)
  y_it  <- as.integer(obs_panel$y_it)
  I_rep <- as.integer(obs_panel$I_replace)

  a_obs <- rep(1L, length(s_obs))     # default: Maintain
  is_exit    <- y_it == 1L & (!is.na(I_rep)) & I_rep == 0L
  is_replace <- y_it == 1L & (!is.na(I_rep)) & I_rep == 1L
  a_obs[is_exit]    <- 2L
  a_obs[is_replace] <- 3L
  ambiguous <- y_it == 1L & is.na(I_rep)
  if (any(ambiguous)) a_obs[ambiguous] <- 2L

  stopifnot(all(s_obs %in% seq_len(n_states)),
            all(a_obs %in% 1:3))

  list(
    n_states  = n_states,
    n_actions = 3L,
    n_obs     = length(s_obs),

    h_vec       = primitives$h_vec,
    L_vec       = primitives$L_vec,
    P_vec       = primitives$P_vec,
    hazard_loss = hazard_loss,
    F_maintain  = primitives$F_maintain,
    F_replace   = F_replace,
    reset_state_lut = reset_state_lut,
    state_lut   = state_lut,

    s_obs = s_obs,
    a_obs = a_obs,
    y_obs = y_it,
    I_replace = I_rep,

    beta = config$beta
  )
}


make_P0_mat_3action <- function(P0_mat, obs_panel, prior_alpha = 1.0,
                                eps_prob = 1e-12) {

  if (is.null(colnames(P0_mat))) colnames(P0_mat) <- c("maintain", "close")
  if (ncol(P0_mat) == 3L) {
    colnames(P0_mat) <- c("maintain", "exit", "replace")
    return(P0_mat)
  }
  stopifnot(ncol(P0_mat) == 2L)

  ob <- data.table::as.data.table(obs_panel)
  closures <- ob[y_it == 1L]
  global_share <- if (nrow(closures) > 0) {
    mean(closures$I_replace == 1L, na.rm = TRUE)
  } else 0.5

  cell <- closures[, .(
    n_close   = .N,
    n_replace = sum(I_replace == 1L, na.rm = TRUE)
  ), by = s_idx]
  cell[, pi_replace :=
         (n_replace + prior_alpha * global_share) /
         (n_close   + prior_alpha)]

  pi_vec <- rep(global_share, 32L)
  pi_vec[cell$s_idx] <- cell$pi_replace

  P3 <- cbind(
    maintain = P0_mat[, "maintain"],
    exit     = P0_mat[, "close"] * (1 - pi_vec),
    replace  = P0_mat[, "close"] *      pi_vec
  )
  P3[P3 < eps_prob] <- eps_prob
  P3 <- P3 / rowSums(P3)
  P3
}


# ==============================================================================
# REPLACEMENT - SECTION 4: FLOW UTILITIES
# ==============================================================================
flow_utilities_replacement <- function(theta, cache) {
  if (!all(c("kappa_exit", "K_log", "gamma_price", "gamma_risk") %in%
           names(theta))) {
    stop("theta must be named with: kappa_exit, K_log, gamma_price, gamma_risk")
  }
  K <- exp(theta[["K_log"]])

  u_M <- 1 + theta[["gamma_price"]] * cache$P_vec -
             theta[["gamma_risk"]]  * cache$hazard_loss
  u_E <- rep.int(theta[["kappa_exit"]], cache$n_states)
  u_R <- rep.int(-K,                    cache$n_states)

  cbind(maintain = u_M, exit = u_E, replace = u_R)
}


# ==============================================================================
# REPLACEMENT - SECTION 5: VALUE FUNCTION INVERSION (LINEAR)
# ==============================================================================
invert_value_function_replacement <- function(P, U, cache, config) {
  sigma   <- config$sigma2
  beta    <- cache$beta
  gamma_E <- config$gamma_E
  n       <- cache$n_states

  P_M <- pmax(P[, "maintain"], config$min_log_val)
  P_E <- pmax(P[, "exit"],     config$min_log_val)
  P_R <- pmax(P[, "replace"],  config$min_log_val)

  R <- P_M * (U[, "maintain"] + sigma * (gamma_E - log(P_M))) +
       P_E * (U[, "exit"]     + sigma * (gamma_E - log(P_E))) +
       P_R * (U[, "replace"]  + sigma * (gamma_E - log(P_R)))

  M <- Matrix::Diagonal(x = as.numeric(P_M)) %*% cache$F_maintain +
       Matrix::Diagonal(x = as.numeric(P_R)) %*% cache$F_replace

  V <- as.numeric(Matrix::solve(Matrix::Diagonal(n) - beta * M, R))
  V
}


# ==============================================================================
# REPLACEMENT - SECTION 6: CCP COMPUTATION
# ==============================================================================
update_ccps_replacement <- function(theta, V, cache, config) {
  sigma <- config$sigma2
  beta  <- cache$beta

  U <- flow_utilities_replacement(theta, cache)

  v_M <- U[, "maintain"] + beta * as.numeric(cache$F_maintain %*% V)
  v_E <- U[, "exit"]
  v_R <- U[, "replace"]  + beta * as.numeric(cache$F_replace  %*% V)

  v_mat <- cbind(maintain = v_M, exit = v_E, replace = v_R)
  v_max <- pmax(v_M, v_E, v_R)
  z     <- exp((v_mat - v_max) / sigma)
  P     <- z / rowSums(z)

  P[P < config$eps_prob] <- config$eps_prob
  P <- P / rowSums(P)
  colnames(P) <- c("maintain", "exit", "replace")
  P
}


# ==============================================================================
# REPLACEMENT - SECTION 7: NPL PSEUDO-LIKELIHOOD
# ==============================================================================
npl_likelihood_replacement <- function(theta, P_fixed, cache, config) {

  if (is.null(names(theta))) {
    names(theta) <- c("kappa_exit", "K_log", "gamma_price", "gamma_risk")
  }

  sigma <- config$sigma2
  beta  <- cache$beta

  U <- flow_utilities_replacement(theta, cache)
  V <- tryCatch(
    invert_value_function_replacement(P_fixed, U, cache, config),
    error = function(e) rep(NA_real_, cache$n_states)
  )
  if (anyNA(V)) return(1e10)

  v_M <- U[, "maintain"] + beta * as.numeric(cache$F_maintain %*% V)
  v_E <- U[, "exit"]
  v_R <- U[, "replace"]  + beta * as.numeric(cache$F_replace  %*% V)

  v_mat <- cbind(v_M, v_E, v_R)
  v_max <- pmax(v_M, v_E, v_R)
  v_diff <- (v_mat - v_max) / sigma
  cl <- config$v_clip
  v_diff[v_diff >  cl] <-  cl
  v_diff[v_diff < -cl] <- -cl
  log_Z <- log(rowSums(exp(v_diff)))     # length 32

  log_P_state <- v_diff - log_Z          # 32 x 3 matrix

  idx <- cbind(cache$s_obs, cache$a_obs)
  ll <- sum(log_P_state[idx])

  if (!is.finite(ll)) return(1e10)
  -ll
}


# ==============================================================================
# REPLACEMENT - SECTION 8: EQUILIBRIUM POLICY SOLVER
# ==============================================================================
solve_equilibrium_policy_replacement <- function(theta, cache, config,
                                                 max_iter = 1000, tol = 1e-8) {
  P <- matrix(1/3, cache$n_states, 3,
              dimnames = list(NULL, c("maintain", "exit", "replace")))

  converged <- FALSE
  for (i in seq_len(max_iter)) {
    U <- flow_utilities_replacement(theta, cache)
    V <- invert_value_function_replacement(P, U, cache, config)
    P_new <- update_ccps_replacement(theta, V, cache, config)
    if (max(abs(P_new - P)) < tol) { converged <- TRUE; P <- P_new; break }
    P <- P_new
  }

  U <- flow_utilities_replacement(theta, cache)
  V <- invert_value_function_replacement(P, U, cache, config)

  list(P = P, V = V, converged = converged, n_iter = i)
}


# ==============================================================================
# REPLACEMENT - SECTION 9: MAIN NPL ESTIMATOR
# ==============================================================================
npl_estimator_replacement <- function(obs_panel,
                                      primitives,
                                      config,
                                      theta_init = NULL,
                                      P_init     = NULL,
                                      verbose    = TRUE) {

  if (verbose) {
    cat("\n=== NPL ESTIMATOR — REPLACEMENT MODEL (Option A, 4-Param) ===\n")
    cat(sprintf("    sample = %s | n_obs = %s | n_facilities = %s\n",
                primitives$sample_label %||% "?",
                format(nrow(obs_panel), big.mark = ","),
                format(data.table::uniqueN(obs_panel$panel_id),
                       big.mark = ",")))
  }

  cache <- create_estimation_cache_replacement(primitives, obs_panel, config)

  if (is.null(theta_init)) {
    theta_init <- c(kappa_exit = 20, K_log = log(20),
                    gamma_price = -1.0, gamma_risk = 1.0)
  }
  if (is.null(names(theta_init))) {
    names(theta_init) <- c("kappa_exit", "K_log", "gamma_price", "gamma_risk")
  }
  theta_curr <- theta_init

  if (is.null(P_init)) {
    P <- make_P0_mat_3action(primitives$P0_mat, obs_panel,
                             eps_prob = config$eps_prob)
    if (verbose) cat("    initial CCPs: split of primitives$P0_mat\n")
  } else {
    P <- P_init
    if (verbose) cat("    initial CCPs: user-supplied\n")
  }
  stopifnot(nrow(P) == cache$n_states, ncol(P) == 3L)
  if (is.null(colnames(P))) {
    colnames(P) <- c("maintain", "exit", "replace")
  }

  lower_b <- c(config$kappa_exit_bounds[1], config$K_log_bounds[1],
               config$gamma_price_bounds[1], config$gamma_risk_bounds[1])
  upper_b <- c(config$kappa_exit_bounds[2], config$K_log_bounds[2],
               config$gamma_price_bounds[2], config$gamma_risk_bounds[2])

  theta_path <- matrix(NA_real_, config$max_npl_iter, config$n_params,
                       dimnames = list(NULL, names(theta_init)))
  ll_path    <- numeric(config$max_npl_iter)
  converged  <- FALSE

  for (npl_iter in seq_len(config$max_npl_iter)) {

    theta_old <- theta_curr
    theta_path[npl_iter, ] <- theta_curr

    opt <- optim(
      par     = theta_curr,
      fn      = npl_likelihood_replacement,
      P_fixed = P,
      cache   = cache,
      config  = config,
      method  = "L-BFGS-B",
      lower   = lower_b,
      upper   = upper_b,
      control = list(maxit = 300, factr = 1e8)
    )
    if (opt$convergence != 0 && verbose) {
      warning(sprintf("    optim() returned code %d at NPL iter %d",
                      opt$convergence, npl_iter))
    }

    theta_curr <- opt$par
    names(theta_curr) <- names(theta_init)
    ll_path[npl_iter] <- -opt$value

    U <- flow_utilities_replacement(theta_curr, cache)
    V <- invert_value_function_replacement(P, U, cache, config)
    P_new <- update_ccps_replacement(theta_curr, V, cache, config)

    theta_diff <- max(abs(theta_curr - theta_old))
    P_diff     <- max(abs(P_new - P))

    if (verbose && (npl_iter %% 5 == 0 || npl_iter <= 3)) {
      cat(sprintf(
        "    iter %3d: kappa=%7.3f K=%8.3f gP=%6.3f gR=%6.3f | dTh=%.1e dP=%.1e LL=%.1f\n",
        npl_iter,
        theta_curr[["kappa_exit"]], exp(theta_curr[["K_log"]]),
        theta_curr[["gamma_price"]], theta_curr[["gamma_risk"]],
        theta_diff, P_diff, ll_path[npl_iter]))
    }

    P <- P_new

    if (theta_diff < config$tol_theta && P_diff < config$tol_P) {
      converged <- TRUE
      if (verbose) {
        cat(sprintf("\n    *** CONVERGED at iter %d ***\n", npl_iter))
        cat(sprintf("    theta_hat: kappa_exit=%.4f K=%.4f gP=%.4f gR=%.4f\n",
                    theta_curr[["kappa_exit"]],
                    exp(theta_curr[["K_log"]]),
                    theta_curr[["gamma_price"]],
                    theta_curr[["gamma_risk"]]))
      }
      break
    }
  }
  if (!converged && verbose) {
    warning(sprintf("NPL did not converge in %d iterations",
                    config$max_npl_iter))
  }

  U_final <- flow_utilities_replacement(theta_curr, cache)
  V_final <- invert_value_function_replacement(P, U_final, cache, config)

  theta_hat <- c(kappa_exit  = unname(theta_curr[["kappa_exit"]]),
                 K           = exp(theta_curr[["K_log"]]),
                 gamma_price = unname(theta_curr[["gamma_price"]]),
                 gamma_risk  = unname(theta_curr[["gamma_risk"]]))

  list(
    theta_hat      = theta_hat,
    theta_raw      = theta_curr,
    P_hat          = P,
    V_hat          = V_final,
    converged      = converged,
    n_iter         = npl_iter,
    log_likelihood = ll_path[npl_iter],
    ll_path        = ll_path[seq_len(npl_iter)],
    theta_path     = theta_path[seq_len(npl_iter), , drop = FALSE],
    cache          = cache,
    config         = config
  )
}


# ==============================================================================
# REPLACEMENT - SECTION 10: DATA GENERATOR (FOR MONTE CARLO)
# ==============================================================================
generate_replacement_data <- function(N_facilities    = 1000L,
                                      T_periods       = 30L,
                                      theta_true      = c(kappa_exit = 22,
                                                          K_log = log(15),
                                                          gamma_price = -1.0,
                                                          gamma_risk  = 0.6),
                                      seed            = 2025L,
                                      P_manual        = NULL,
                                      hazard_params   = NULL,
                                      loss_params     = NULL,
                                      premium_params  = NULL,
                                      age_probs       = NULL,
                                      config          = NULL,
                                      verbose         = FALSE) {

  set.seed(seed)
  if (is.null(names(theta_true))) {
    names(theta_true) <- c("kappa_exit", "K_log", "gamma_price", "gamma_risk")
  }

  state_lut <- build_state_lut_replacement()
  n_states  <- nrow(state_lut)

  hp <- modifyList(list(h0 = 0.04, h_single = 0.02, h_age = 0.004,
                        cap_lo = 1e-3, cap_hi = 0.40),
                   hazard_params %||% list())
  lp <- modifyList(list(ell0 = 12, ell_age = 1.0, ell_single = 4),
                   loss_params %||% list())
  pp <- modifyList(list(p_FF = 0.10, p0_RB = 0.04,
                        p_single_RB = 0.06, p_age_RB = 0.012),
                   premium_params %||% list())
  ap <- modifyList(list(p_up = c(0.18, 0.19, 0.20, 0.21,
                                 0.22, 0.23, 0.24, 0.0)),
                   age_probs %||% list())

  s     <- seq_len(n_states)
  rho_v <- state_lut$rho_state[s]
  w_v   <- state_lut$w_state[s]
  A_v   <- state_lut$A_bin[s]

  h_vec <- pmin(pmax(hp$h0 + hp$h_single * (w_v == 1L) + hp$h_age * A_v,
                     hp$cap_lo), hp$cap_hi)
  L_vec <- pmax(lp$ell0 + lp$ell_age * A_v + lp$ell_single * (w_v == 1L), 1)
  P_vec <- pp$p_FF + pp$p0_RB * (rho_v == 2L) +
           pp$p_single_RB * (w_v == 1L) * (rho_v == 2L) +
           pp$p_age_RB    * (A_v - 1L)  * (rho_v == 2L)

  N_AGE <- 8L
  F_maintain <- Matrix::Matrix(0, n_states, n_states, sparse = TRUE)
  for (rho in 1:2) for (w in 1:2) for (a in 1:N_AGE) {
    s_from <- (rho - 1L) * 16L + (w - 1L) * 8L + a
    pu <- ap$p_up[a]
    F_maintain[s_from, s_from] <- 1 - pu
    if (a < N_AGE) {
      s_up <- (rho - 1L) * 16L + (w - 1L) * 8L + (a + 1L)
      F_maintain[s_from, s_up] <- pu
    }
  }
  stopifnot(all(abs(rowSums(F_maintain) - 1) < 1e-10))

  P0_close_init <- 0.10
  P0_mat <- matrix(c(1 - P0_close_init, P0_close_init), nrow = n_states,
                   ncol = 2, byrow = TRUE,
                   dimnames = list(NULL, c("maintain", "close")))

  primitives_stub <- list(
    state_lut    = state_lut,
    h_vec        = h_vec,
    L_vec        = L_vec,
    P_vec        = P_vec,
    F_maintain   = F_maintain,
    P0_mat       = P0_mat,
    sample_label = "synthetic"
  )

  if (is.null(config)) {
    config <- create_estimation_config_replacement(beta = 0.95, sigma2 = 1.0,
                                                   npl_iter = 200)
  }

  obs_stub <- data.table::data.table(s_idx = 1L, y_it = 0L,
                                     I_replace = NA_integer_)
  cache <- create_estimation_cache_replacement(primitives_stub, obs_stub,
                                               config)

  if (!is.null(P_manual)) {
    P_eq <- P_manual
    U_eq <- flow_utilities_replacement(theta_true, cache)
    V_eq <- invert_value_function_replacement(P_eq, U_eq, cache, config)
    converged <- TRUE
  } else {
    eq <- solve_equilibrium_policy_replacement(theta_true, cache, config)
    P_eq <- eq$P; V_eq <- eq$V; converged <- eq$converged
    if (!converged && verbose) warning("equilibrium did not converge")
  }
  stopifnot(nrow(P_eq) == n_states, ncol(P_eq) == 3L)

  panel_list <- vector("list", N_facilities)

  for (fac in seq_len(N_facilities)) {
    s_curr <- sample.int(n_states, 1L)
    rec_s <- integer(T_periods)
    rec_y <- integer(T_periods)
    rec_I <- rep(NA_integer_, T_periods)
    rec_reset <- rep(NA_integer_, T_periods)
    n_t <- 0L
    active <- TRUE

    for (t in seq_len(T_periods)) {
      if (!active) break
      n_t <- t

      probs <- P_eq[s_curr, ]
      a <- sample.int(3L, 1L, prob = probs)

      rec_s[t] <- s_curr
      if (a == 1L) {
        rec_y[t] <- 0L
        a_idx <- state_lut$A_bin[s_curr]
        pu <- ap$p_up[a_idx]
        if (runif(1) < pu && a_idx < N_AGE) {
          s_curr <- (state_lut$rho_state[s_curr] - 1L) * 16L +
                    (state_lut$w_state[s_curr]   - 1L) * 8L  +
                    (a_idx + 1L)
        }
      } else if (a == 2L) {
        rec_y[t] <- 1L
        rec_I[t] <- 0L
        active <- FALSE
      } else {
        rec_y[t] <- 1L
        rec_I[t] <- 1L
        s_reset <- (state_lut$rho_state[s_curr] - 1L) * 16L +
                   (state_lut$w_state[s_curr]   - 1L) * 8L  + 1L
        rec_reset[t] <- s_reset
        s_curr <- s_reset
      }
    }

    if (n_t > 0L) {
      panel_list[[fac]] <- data.table::data.table(
        panel_id    = fac,
        panel_year  = seq_len(n_t),
        s_idx       = rec_s[seq_len(n_t)],
        A_bin       = state_lut$A_bin     [rec_s[seq_len(n_t)]],
        w_state     = state_lut$w_state   [rec_s[seq_len(n_t)]],
        rho_state   = state_lut$rho_state [rec_s[seq_len(n_t)]],
        premium     = P_vec[rec_s[seq_len(n_t)]],
        y_it        = rec_y[seq_len(n_t)],
        I_replace   = rec_I[seq_len(n_t)],
        reset_state_index = rec_reset[seq_len(n_t)]
      )
    }
  }
  obs_panel <- data.table::rbindlist(panel_list)

  closures <- obs_panel[y_it == 1L]
  pi_global <- if (nrow(closures)) mean(closures$I_replace == 1L,
                                        na.rm = TRUE) else 0.5
  P0_close <- mean(obs_panel$y_it == 1L)
  P0_mat_2 <- matrix(c(1 - P0_close, P0_close), nrow = n_states,
                     ncol = 2, byrow = TRUE,
                     dimnames = list(NULL, c("maintain", "close")))

  primitives <- list(
    state_lut    = state_lut,
    h_vec        = h_vec,
    L_vec        = L_vec,
    P_vec        = P_vec,
    F_maintain   = F_maintain,
    P0_mat       = P0_mat_2,
    n_exit       = sum(obs_panel$y_it == 1L &
                       obs_panel$I_replace == 0L, na.rm = TRUE),
    n_replace    = sum(obs_panel$y_it == 1L &
                       obs_panel$I_replace == 1L, na.rm = TRUE),
    pct_replace  = pi_global,
    sample_label = "synthetic",
    n_obs        = nrow(obs_panel),
    n_facilities = N_facilities
  )

  list(
    obs_panel    = obs_panel,
    primitives   = primitives,
    theta_true   = theta_true,
    P_true       = P_eq,
    V_true       = V_eq,
    cache        = cache,
    config       = config,
    converged_eq = converged
  )
}


if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}


# ==============================================================================
# ==============================================================================
# REPLACEMENT MODEL — 8-PARAMETER EXTENSION
# ==============================================================================
# ==============================================================================
# Wall-specific (kappa_SW, kappa_DW, K_SW, K_DW) and regime-specific
# (gamma_price_FF, gamma_price_RB, gamma_risk_FF, gamma_risk_RB).
#
# theta_raw = (kappa_SW, kappa_DW, K_log_SW, K_log_DW,
#              gamma_price_FF, gamma_price_RB, gamma_risk_FF, gamma_risk_RB)
#
# All downstream solver code is identical to the 4-param version EXCEPT for
# the flow utility, which is now state-varying via wall and regime indices.
# We therefore reuse `invert_value_function_replacement` directly (it takes
# U as input, not theta) and write thin _8p wrappers for the rest.
# ==============================================================================


create_estimation_config_replacement_8p <- function(beta = 0.95, sigma2 = 1.0,
                                                    npl_iter = 200) {
  list(
    beta    = beta,
    sigma2  = sigma2,
    gamma_E = 0.5772156649,

    tol_theta = 1e-5,
    tol_P     = 1e-5,
    max_npl_iter = npl_iter,

    eps_prob    = 1e-12,
    min_log_val = 1e-12,
    v_clip      = 700,

    # Bounds — duplicate the 4-param bounds onto each wall- or regime-specific param.
    kappa_SW_bounds       = c(-200, 500),
    kappa_DW_bounds       = c(-200, 500),
    K_log_SW_bounds       = c(log(0.01), log(1000)),
    K_log_DW_bounds       = c(log(0.01), log(1000)),
    gamma_price_FF_bounds = c(-20, 5),
    gamma_price_RB_bounds = c(-20, 5),
    gamma_risk_FF_bounds  = c(-5, 10),
    gamma_risk_RB_bounds  = c(-5, 10),

    n_actions = 3L,
    n_params  = 8L,

    param_names = c("kappa_SW","kappa_DW","K_log_SW","K_log_DW",
                    "gamma_price_FF","gamma_price_RB",
                    "gamma_risk_FF","gamma_risk_RB"),

    action_labels = c("maintain","exit","replace")
  )
}


create_estimation_cache_replacement_8p <- function(primitives, obs_panel,
                                                   config_4p, config_8p) {
  # Build the standard cache via the 4-param helper, then add wall_idx and
  # regime_idx (length-32 integer vectors) for state-varying flow utilities.
  cache <- create_estimation_cache_replacement(primitives, obs_panel, config_4p)
  cache$wall_idx   <- as.integer(cache$state_lut$w_state)     # 1 = SW, 2 = DW
  cache$regime_idx <- as.integer(cache$state_lut$rho_state)   # 1 = FF, 2 = RB
  stopifnot(length(cache$wall_idx)   == cache$n_states,
            length(cache$regime_idx) == cache$n_states)
  cache
}


flow_utilities_replacement_8p <- function(theta, cache) {
  required <- c("kappa_SW","kappa_DW","K_log_SW","K_log_DW",
                "gamma_price_FF","gamma_price_RB",
                "gamma_risk_FF","gamma_risk_RB")
  if (!all(required %in% names(theta))) {
    stop("theta is missing one of: ", paste(required, collapse = ", "))
  }
  K_vec     <- ifelse(cache$wall_idx == 1L,
                      exp(theta[["K_log_SW"]]), exp(theta[["K_log_DW"]]))
  kappa_vec <- ifelse(cache$wall_idx == 1L,
                      theta[["kappa_SW"]],   theta[["kappa_DW"]])
  gp_vec <- ifelse(cache$regime_idx == 1L,
                   theta[["gamma_price_FF"]], theta[["gamma_price_RB"]])
  gr_vec <- ifelse(cache$regime_idx == 1L,
                   theta[["gamma_risk_FF"]],  theta[["gamma_risk_RB"]])

  u_M <- 1 + gp_vec * cache$P_vec - gr_vec * cache$hazard_loss
  U <- cbind(maintain = u_M, exit = kappa_vec, replace = -K_vec)
  U
}


update_ccps_replacement_8p <- function(theta, V, cache, config) {
  sigma <- config$sigma2
  beta  <- cache$beta

  U  <- flow_utilities_replacement_8p(theta, cache)
  v_M <- U[, "maintain"] + beta * as.numeric(cache$F_maintain %*% V)
  v_E <- U[, "exit"]
  v_R <- U[, "replace"]  + beta * as.numeric(cache$F_replace  %*% V)

  v_mat <- cbind(maintain = v_M, exit = v_E, replace = v_R)
  v_max <- pmax(v_M, v_E, v_R)
  z     <- exp((v_mat - v_max) / sigma)
  P     <- z / rowSums(z)
  P[P < config$eps_prob] <- config$eps_prob
  P <- P / rowSums(P)
  colnames(P) <- c("maintain","exit","replace")
  P
}


npl_likelihood_replacement_8p <- function(theta, P_fixed, cache, config) {
  if (is.null(names(theta))) names(theta) <- config$param_names
  sigma <- config$sigma2
  beta  <- cache$beta

  U <- flow_utilities_replacement_8p(theta, cache)
  V <- tryCatch(
    invert_value_function_replacement(P_fixed, U, cache, config),
    error = function(e) rep(NA_real_, cache$n_states)
  )
  if (anyNA(V)) return(1e10)

  v_M <- U[, "maintain"] + beta * as.numeric(cache$F_maintain %*% V)
  v_E <- U[, "exit"]
  v_R <- U[, "replace"]  + beta * as.numeric(cache$F_replace  %*% V)

  v_mat <- cbind(v_M, v_E, v_R)
  v_max <- pmax(v_M, v_E, v_R)
  v_diff <- (v_mat - v_max) / sigma
  cl <- config$v_clip
  v_diff[v_diff >  cl] <-  cl
  v_diff[v_diff < -cl] <- -cl
  log_Z <- log(rowSums(exp(v_diff)))

  log_P_state <- v_diff - log_Z
  idx <- cbind(cache$s_obs, cache$a_obs)
  ll  <- sum(log_P_state[idx])
  if (!is.finite(ll)) return(1e10)
  -ll
}


solve_equilibrium_policy_replacement_8p <- function(theta, cache, config,
                                                    max_iter = 1000,
                                                    tol = 1e-8) {
  P <- matrix(1/3, cache$n_states, 3,
              dimnames = list(NULL, c("maintain","exit","replace")))
  converged <- FALSE
  for (i in seq_len(max_iter)) {
    U <- flow_utilities_replacement_8p(theta, cache)
    V <- invert_value_function_replacement(P, U, cache, config)
    P_new <- update_ccps_replacement_8p(theta, V, cache, config)
    if (max(abs(P_new - P)) < tol) { converged <- TRUE; P <- P_new; break }
    P <- P_new
  }
  U <- flow_utilities_replacement_8p(theta, cache)
  V <- invert_value_function_replacement(P, U, cache, config)
  list(P = P, V = V, converged = converged, n_iter = i)
}


npl_estimator_replacement_8p <- function(obs_panel,
                                         primitives,
                                         config_8p,
                                         theta_init = NULL,
                                         P_init     = NULL,
                                         verbose    = TRUE) {

  # Need a 4-param config to share with the cache builder (uses same
  # numerical constants — gamma_E, eps_prob, etc.).
  config_4p <- create_estimation_config_replacement(
    beta = config_8p$beta, sigma2 = config_8p$sigma2,
    npl_iter = config_8p$max_npl_iter)

  if (verbose) {
    cat("\n=== NPL ESTIMATOR — REPLACEMENT MODEL (8-Param spec) ===\n")
    cat(sprintf("    sample = %s | n_obs = %s | n_facilities = %s\n",
                primitives$sample_label %||% "?",
                format(nrow(obs_panel), big.mark = ","),
                format(data.table::uniqueN(obs_panel$panel_id), big.mark = ",")))
  }

  cache <- create_estimation_cache_replacement_8p(primitives, obs_panel,
                                                   config_4p, config_8p)

  if (is.null(theta_init)) {
    # Default: duplicate of 4-param defaults
    theta_init <- c(kappa_SW = 20, kappa_DW = 20,
                    K_log_SW = log(20), K_log_DW = log(20),
                    gamma_price_FF = -1.0, gamma_price_RB = -1.0,
                    gamma_risk_FF  =  1.0, gamma_risk_RB  =  1.0)
  }
  if (is.null(names(theta_init))) names(theta_init) <- config_8p$param_names
  theta_curr <- theta_init

  if (is.null(P_init)) {
    P <- make_P0_mat_3action(primitives$P0_mat, obs_panel,
                             eps_prob = config_8p$eps_prob)
    if (verbose) cat("    initial CCPs: split of primitives$P0_mat\n")
  } else {
    P <- P_init
    if (verbose) cat("    initial CCPs: user-supplied\n")
  }
  stopifnot(nrow(P) == cache$n_states, ncol(P) == 3L)
  if (is.null(colnames(P))) colnames(P) <- c("maintain","exit","replace")

  pn <- config_8p$param_names
  lower_b <- c(config_8p$kappa_SW_bounds[1],       config_8p$kappa_DW_bounds[1],
               config_8p$K_log_SW_bounds[1],       config_8p$K_log_DW_bounds[1],
               config_8p$gamma_price_FF_bounds[1], config_8p$gamma_price_RB_bounds[1],
               config_8p$gamma_risk_FF_bounds[1],  config_8p$gamma_risk_RB_bounds[1])
  upper_b <- c(config_8p$kappa_SW_bounds[2],       config_8p$kappa_DW_bounds[2],
               config_8p$K_log_SW_bounds[2],       config_8p$K_log_DW_bounds[2],
               config_8p$gamma_price_FF_bounds[2], config_8p$gamma_price_RB_bounds[2],
               config_8p$gamma_risk_FF_bounds[2],  config_8p$gamma_risk_RB_bounds[2])

  theta_path <- matrix(NA_real_, config_8p$max_npl_iter, config_8p$n_params,
                       dimnames = list(NULL, pn))
  ll_path    <- numeric(config_8p$max_npl_iter)
  converged  <- FALSE

  for (npl_iter in seq_len(config_8p$max_npl_iter)) {
    theta_old <- theta_curr
    theta_path[npl_iter, ] <- theta_curr

    opt <- optim(
      par     = theta_curr,
      fn      = npl_likelihood_replacement_8p,
      P_fixed = P, cache = cache, config = config_8p,
      method  = "L-BFGS-B",
      lower   = lower_b, upper = upper_b,
      control = list(maxit = 300, factr = 1e8)
    )
    if (opt$convergence != 0 && verbose) {
      warning(sprintf("    optim() returned code %d at NPL iter %d",
                      opt$convergence, npl_iter))
    }
    theta_curr <- opt$par
    names(theta_curr) <- pn
    ll_path[npl_iter] <- -opt$value

    U <- flow_utilities_replacement_8p(theta_curr, cache)
    V <- invert_value_function_replacement(P, U, cache, config_8p)
    P_new <- update_ccps_replacement_8p(theta_curr, V, cache, config_8p)
    theta_diff <- max(abs(theta_curr - theta_old))
    P_diff     <- max(abs(P_new - P))

    if (verbose && (npl_iter %% 5 == 0 || npl_iter <= 3)) {
      cat(sprintf("    iter %3d: kSW=%5.2f kDW=%5.2f KSW=%5.2f KDW=%5.2f gpFF=%5.2f gpRB=%5.2f grFF=%5.3f grRB=%5.3f | dTh=%.1e dP=%.1e LL=%.1f\n",
        npl_iter,
        theta_curr[["kappa_SW"]], theta_curr[["kappa_DW"]],
        exp(theta_curr[["K_log_SW"]]), exp(theta_curr[["K_log_DW"]]),
        theta_curr[["gamma_price_FF"]], theta_curr[["gamma_price_RB"]],
        theta_curr[["gamma_risk_FF"]],  theta_curr[["gamma_risk_RB"]],
        theta_diff, P_diff, ll_path[npl_iter]))
    }
    P <- P_new

    if (theta_diff < config_8p$tol_theta && P_diff < config_8p$tol_P) {
      converged <- TRUE
      if (verbose) cat(sprintf("\n    *** CONVERGED at iter %d ***\n", npl_iter))
      break
    }
  }
  if (!converged && verbose) {
    warning(sprintf("8-param NPL did not converge in %d iterations",
                    config_8p$max_npl_iter))
  }

  U_final <- flow_utilities_replacement_8p(theta_curr, cache)
  V_final <- invert_value_function_replacement(P, U_final, cache, config_8p)

  theta_hat <- c(
    kappa_SW       = unname(theta_curr[["kappa_SW"]]),
    kappa_DW       = unname(theta_curr[["kappa_DW"]]),
    K_SW           = exp(theta_curr[["K_log_SW"]]),
    K_DW           = exp(theta_curr[["K_log_DW"]]),
    gamma_price_FF = unname(theta_curr[["gamma_price_FF"]]),
    gamma_price_RB = unname(theta_curr[["gamma_price_RB"]]),
    gamma_risk_FF  = unname(theta_curr[["gamma_risk_FF"]]),
    gamma_risk_RB  = unname(theta_curr[["gamma_risk_RB"]])
  )

  list(
    theta_hat      = theta_hat,
    theta_raw      = theta_curr,
    P_hat          = P,
    V_hat          = V_final,
    converged      = converged,
    n_iter         = npl_iter,
    log_likelihood = ll_path[npl_iter],
    ll_path        = ll_path[seq_len(npl_iter)],
    theta_path     = theta_path[seq_len(npl_iter), , drop = FALSE],
    cache          = cache,
    config         = config_8p
  )
}




# ==============================================================================


# ==============================================================================
# APPEND (2026-05-08): 8p Replacement + nuisance FE for ALL 17 control states
# ------------------------------------------------------------------------------
# • Maintain-only FE: alpha enters uM only (not exit/replace)
# • Aggregated likelihood by (sidx, graw, action) — O(32×18) evals
# • Geo-weighted CCP update: sum_g w(g|s) * softmax(vM+alpha_g, vE, vR)
# • TX FE fixed at 0; estimate alpha for each of the 17 control states
# • alpha does NOT enter Bellman inversion / equilibrium mapping (Option B)
# • PR/PE ratio does not vary with geo g conditional on state s — intentional
#   restriction to keep gamma parameters stable.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(here)
})

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}

# ------------------------------------------------------------------------------
# FE STATE LISTS (authoritative — overrides earlier 5-FE / AR-base definitions)
# ------------------------------------------------------------------------------
CONTROLSTATES <- c(
  "AR", "CO", "ID", "KS", "KY",
  "LA", "MA", "MD", "ME", "MN", "MO", "NC",
  "OH", "OK", "SD", "TN", "VA"
)
STUDYSTATES    <- c("TX", CONTROLSTATES)
FETXSTATE      <- "TX"
FEPARAMNAMES   <- paste0("alpha", CONTROLSTATES)   # alphaAR..alphaVA (17 params)
NFEPARAMS      <- length(FEPARAMNAMES)              # 17

# ------------------------------------------------------------------------------
# Recompile cpp_engine.cpp so the new exports are available
# ------------------------------------------------------------------------------
.try_source_cpp_engine <- function() {
  if (!requireNamespace("Rcpp", quietly = TRUE)) return(invisible(FALSE))
  cpppath <- tryCatch(here::here("Code", "Helpers", "cpp_engine.cpp"),
                      error = function(e) NULL)
  if (is.null(cpppath) || !file.exists(cpppath)) return(invisible(FALSE))
  ok <- TRUE
  tryCatch(Rcpp::sourceCpp(cpppath),
           error = function(e) {
             ok <<- FALSE
             warning("Failed to compile cpp_engine.cpp: ", conditionMessage(e))
           })
  invisible(ok)
}
.try_source_cpp_engine()

# ------------------------------------------------------------------------------
# Stable logsumexp for 3 alternatives (R fallback)
# ------------------------------------------------------------------------------
.logsumexp3 <- function(a, b, c) {
  m <- pmax(a, pmax(b, c))
  m + log(exp(a - m) + exp(b - m) + exp(c - m))
}

# ------------------------------------------------------------------------------
# Build aggregated counts by (sidx, graw) and geo-weight matrix w(g|s)
#
# feweightsource = "controls" : weight matrix uses controls-only observations
#                               for FF (rho=1) states; all-sample for RB.
# feweightsource = "all"      : use all-sample everywhere.
#
# Required obs_panel columns: sidx (1..32), state (in STUDYSTATES),
#                              yit (0=maintain, 1=not-maintain), Ireplace
# ------------------------------------------------------------------------------
.build_counts_weights_8p_fe <- function(obs_panel, primitives,
                                        feweightsource = c("controls", "all")) {
  feweightsource <- match.arg(feweightsource)
  dt <- data.table::as.data.table(obs_panel)

  # Obs panel CSVs use underscored names (s_idx, y_it, I_replace); the rest of
  # this function uses internal no-underscore names. Bridge with a setnames.
  data.table::setnames(dt, c("s_idx", "y_it", "I_replace"),
                           c("sidx",  "yit",  "Ireplace"), skip_absent = TRUE)

  stopifnot(all(c("sidx", "state", "yit", "Ireplace") %in% names(dt)))

  # action index: 1=maintain, 2=exit, 3=replace
  dt[, aidx := data.table::fifelse(
    yit == 0L, 1L,
    data.table::fifelse(!is.na(Ireplace) & Ireplace == 1L, 3L, 2L)
  )]

  # graw mapping: 0=TX; 1..17 in CONTROLSTATES order
  mctrl <- match(dt$state, CONTROLSTATES)
  dt[, graw := data.table::fifelse(
    state == FETXSTATE, 0L,
    data.table::fifelse(!is.na(mctrl), as.integer(mctrl), NA_integer_)
  )]

  if (anyNA(dt$graw)) {
    bad <- unique(dt[is.na(graw), state])
    stop("States in obs_panel not in STUDYSTATES: ", paste(bad, collapse = ", "))
  }

  # aggregate counts by (sidx, graw)
  counts <- dt[, .(
    nM = sum(aidx == 1L),
    nE = sum(aidx == 2L),
    nR = sum(aidx == 3L),
    nT = .N
  ), by = .(sidx, graw)]

  S       <- 32L
  Grawmax <- 17L
  grid    <- data.table::CJ(sidx = 1:S, graw = 0:Grawmax)
  counts  <- merge(grid, counts, by = c("sidx", "graw"), all.x = TRUE)
  for (v in c("nM", "nE", "nR", "nT")) {
    counts[is.na(get(v)), (v) := 0L]
  }
  data.table::setorder(counts, sidx, graw)

  # rho_state for FF vs RB weight-source logic (1=FF, 2=RB)
  rho <- NULL
  if (!is.null(primitives$statelut) && "rhostate" %in% names(primitives$statelut)) {
    rho <- primitives$statelut$rhostate
  } else if (!is.null(primitives$state_lut) && "rho_state" %in% names(primitives$state_lut)) {
    rho <- primitives$state_lut$rho_state
  } else {
    rho <- rep(1L, S)  # fallback: treat all as FF
  }

  # weight matrix 32 x 18 (column j corresponds to graw = j-1, R 1-indexed)
  wsg <- matrix(0.0, nrow = S, ncol = Grawmax + 1L)

  countsc <- NULL
  if (feweightsource == "controls") {
    dtc    <- dt[state != FETXSTATE]
    ctmp   <- dtc[, .(nT = .N), by = .(sidx, graw)]
    ctmp   <- merge(grid, ctmp, by = c("sidx", "graw"), all.x = TRUE)
    ctmp[is.na(nT), nT := 0L]
    data.table::setorder(ctmp, sidx, graw)
    countsc <- ctmp
  }

  for (s in seq_len(S)) {
    use_controls <- (feweightsource == "controls") && (rho[s] == 1L)
    src <- if (use_controls) countsc else counts
    nTvec <- src[sidx == s, nT]
    denom <- sum(nTvec)
    if (denom > 0) {
      wsg[s, ] <- nTvec / denom
    } else {
      # no observations for this state: all weight on TX (graw=0)
      wsg[s, ]    <- 0.0
      wsg[s, 1L]  <- 1.0
    }
  }

  stopifnot(nrow(wsg) == 32L, ncol(wsg) == 18L)
  stopifnot(all(abs(rowSums(wsg) - 1.0) < 1e-8))

  list(countsdt = counts, wsg = wsg)
}

# ------------------------------------------------------------------------------
# Build alphacpp vector (length 18, R-indexed 1..18)
#   alphacpp[1]  = 0       (graw=0, TX, fixed)
#   alphacpp[j+1] = alpha for CONTROLSTATES[j], j=1..17
# C++ accesses as alphacpp[g] (0-based), so alphacpp[graw+1] in R.
# ------------------------------------------------------------------------------
.alphacpp_from_theta <- function(thetaraw) {
  a       <- numeric(18L)
  a[1L]   <- 0.0   # TX fixed at 0
  for (j in seq_along(CONTROLSTATES)) {
    nm <- paste0("alpha", CONTROLSTATES[j])
    if (is.null(thetaraw[[nm]])) stop("Missing FE param: ", nm)
    a[j + 1L] <- thetaraw[[nm]]
  }
  a
}

# ------------------------------------------------------------------------------
# Transition matrix accessors
# ------------------------------------------------------------------------------
.get_F_mats <- function(cache) {
  FM <- cache$Fmaintain %||% cache$F_maintain %||% cache$FM %||% cache$F0
  FE <- cache$Fexit     %||% cache$F_exit     %||% cache$FE %||% cache$F1
  FR <- cache$Freplace  %||% cache$F_replace  %||% cache$FR %||% cache$F2
  if (is.null(FM) || is.null(FR))
    stop("Missing transition matrices in cache. Need F_maintain + F_replace.")
  list(FM = FM, FE = FE, FR = FR)
}

# ------------------------------------------------------------------------------
# Compute v-indices (vM, vE, vR) given structural theta + fixed P
# Uses existing flow utility + inversion functions from the base 8p model.
# ------------------------------------------------------------------------------
.compute_v_indices_8p <- function(thetastruct, P, cache, config) {
  U  <- flow_utilities_replacement_8p(thetastruct, cache)
  V  <- invert_value_function_replacement(P, U, cache, config)
  Fs <- .get_F_mats(cache)
  beta <- config$beta
  vM <- as.numeric(U[, "maintain"] + beta * (Fs$FM %*% V))
  vE <- as.numeric(U[, "exit"])
  vR <- as.numeric(U[, "replace"]  + beta * (Fs$FR %*% V))
  list(U = U, V = V, vM = vM, vE = vE, vR = vR)
}

# ------------------------------------------------------------------------------
# FE config creator — full 17-control version
# ------------------------------------------------------------------------------
create_estimation_config_replacement_8p_fe <- function(
    beta              = 0.95,
    sigma2            = 1.0,
    npl_iter          = 200,
    feweightsource    = c("controls", "all"),
    ccp_damping_lambda = 0.6,
    epsprob           = 1e-12,
    alphabounds       = c(-20, 20),
    tol_theta         = 1e-5,
    tol_P             = 1e-5) {

  feweightsource <- match.arg(feweightsource)

  # Start from the base 8p config to inherit all numerical constants + bounds
  cfg_8p <- create_estimation_config_replacement_8p(beta = beta,
                                                    sigma2 = sigma2,
                                                    npl_iter = npl_iter)

  cfg <- cfg_8p
  cfg$feweightsource     <- feweightsource
  cfg$ccp_damping_lambda <- ccp_damping_lambda
  cfg$epsprob            <- epsprob
  cfg$alpha_bounds       <- alphabounds      # kept for back-compat with old code
  cfg$alphabounds        <- alphabounds
  cfg$tol_theta          <- tol_theta
  cfg$tol_P              <- tol_P
  cfg$max_npl_iter       <- npl_iter
  cfg$struct_param_names <- c("kappa_SW", "kappa_DW", "K_log_SW", "K_log_DW",
                               "gamma_price_FF", "gamma_price_RB",
                               "gamma_risk_FF",  "gamma_risk_RB")
  cfg$fe_param_names     <- FEPARAMNAMES
  cfg$n_params           <- 8L + NFEPARAMS   # 25
  cfg$param_names        <- c(cfg$struct_param_names, cfg$fe_param_names)
  cfg
}

# ------------------------------------------------------------------------------
# Aggregated FE negative log-likelihood (maintain-only FE)
#   uM(s,g) = vM(s) + alpha_g;  uE = vE(s);  uR = vR(s)
# Iterates over <= 576 aggregated (sidx, graw) rows.
# ------------------------------------------------------------------------------
npl_likelihood_replacement_8p_fe <- function(thetaraw, Pfixed, cache, config) {
  if (is.null(names(thetaraw))) names(thetaraw) <- config$param_names

  thetastruct <- thetaraw[config$struct_param_names]
  v           <- .compute_v_indices_8p(thetastruct, Pfixed, cache, config)
  alphacpp    <- .alphacpp_from_theta(thetaraw)

  counts <- cache$countsdt8pfe
  stopifnot(!is.null(counts))

  # Prefer compiled C++ path
  if (exists("nll_replacement8pfe_counts_cpp", mode = "function")) {
    return(nll_replacement8pfe_counts_cpp(
      sidx     = counts$sidx,
      graw     = counts$graw,
      nM       = counts$nM,
      nE       = counts$nE,
      nR       = counts$nR,
      vM       = v$vM,
      vE       = v$vE,
      vR       = v$vR,
      alphacpp = alphacpp,
      epsprob  = config$epsprob
    ))
  }

  # R fallback
  ll     <- 0.0
  logeps <- log(config$epsprob)
  for (i in seq_len(nrow(counts))) {
    s  <- counts$sidx[i]
    g  <- counts$graw[i]
    nm <- counts$nM[i]; ne <- counts$nE[i]; nr <- counts$nR[i]
    if (nm + ne + nr == 0L) next

    uM  <- v$vM[s] + alphacpp[g + 1L]   # R is 1-indexed; graw+1 gives correct slot
    uE  <- v$vE[s]
    uR  <- v$vR[s]
    lse <- .logsumexp3(uM, uE, uR)
    lpM <- uM - lse; lpE <- uE - lse; lpR <- uR - lse
    lpM <- max(lpM, logeps); lpE <- max(lpE, logeps); lpR <- max(lpR, logeps)
    ll  <- ll + nm * lpM + ne * lpE + nr * lpR
  }
  -ll
}

# ------------------------------------------------------------------------------
# Geo-weighted CCP update (maintain-only FE)
#   Pnew(s,.) = sum_g w(g|s) * softmax(vM(s)+alpha_g, vE(s), vR(s))
# Then apply damping: P <- (1-lam)*Pold + lam*Pnew
# ------------------------------------------------------------------------------
.update_ccps_geoweighted_8p_fe <- function(thetaraw, Pold, cache, config) {
  thetastruct <- thetaraw[config$struct_param_names]
  v           <- .compute_v_indices_8p(thetastruct, Pold, cache, config)
  alphacpp    <- .alphacpp_from_theta(thetaraw)
  wsg         <- cache$wsg8pfe

  # Prefer compiled C++ path
  if (exists("update_ccps_geoweighted_cpp", mode = "function")) {
    Pnew <- update_ccps_geoweighted_cpp(
      vM       = v$vM,
      vE       = v$vE,
      vR       = v$vR,
      alphacpp = alphacpp,
      wsg      = wsg,
      epsprob  = config$epsprob
    )
  } else {
    # R fallback
    S    <- 32L
    G    <- 18L
    Pnew <- matrix(0.0, nrow = S, ncol = 3)
    colnames(Pnew) <- c("maintain", "exit", "replace")
    eps  <- config$epsprob
    for (s in seq_len(S)) {
      pmix <- c(0.0, 0.0, 0.0)
      for (g in 0:(G - 1L)) {
        w <- wsg[s, g + 1L]
        if (w <= 0.0) next
        uM  <- v$vM[s] + alphacpp[g + 1L]
        uE  <- v$vE[s]
        uR  <- v$vR[s]
        lse <- .logsumexp3(uM, uE, uR)
        p   <- exp(c(uM - lse, uE - lse, uR - lse))
        p   <- pmax(p, eps)
        p   <- p / sum(p)
        pmix <- pmix + w * p
      }
      pmix        <- pmax(pmix, eps)
      Pnew[s, ]   <- pmix / sum(pmix)
    }
  }

  # CCP damping
  lam <- config$ccp_damping_lambda %||% 1.0
  if (is.finite(lam) && lam < 1.0) {
    Pnew <- (1.0 - lam) * Pold + lam * Pnew
    Pnew <- Pnew / rowSums(Pnew)
  }
  Pnew
}

# ------------------------------------------------------------------------------
# NPL estimator for 8p + ALL-17-controls FE
# ------------------------------------------------------------------------------
npl_estimator_replacement_8p_fe <- function(obs_panel,
                                            primitives,
                                            config_8p_fe,
                                            theta_init,
                                            verbose = TRUE) {
  stopifnot(!is.null(names(theta_init)))

  # Build cache from primitives; augment with aggregated counts + weights
  cache <- primitives
  cw    <- .build_counts_weights_8p_fe(obs_panel, primitives,
                                       config_8p_fe$feweightsource)
  cache$countsdt8pfe <- cw$countsdt
  cache$wsg8pfe      <- cw$wsg

  # Also build the standard replacement cache (for vM/vE/vR computation)
  config_4p  <- create_estimation_config_replacement(
    beta = config_8p_fe$beta, sigma2 = config_8p_fe$sigma2,
    npl_iter = config_8p_fe$max_npl_iter)
  std_cache  <- create_estimation_cache_replacement_8p(primitives, obs_panel,
                                                       config_4p, config_8p_fe)
  # Merge std_cache fields needed by .compute_v_indices_8p into cache
  for (nm in names(std_cache)) {
    if (is.null(cache[[nm]])) cache[[nm]] <- std_cache[[nm]]
  }
  # Ensure F_maintain and F_replace are present under canonical names
  if (is.null(cache$F_maintain)) cache$F_maintain <- std_cache$F_maintain
  if (is.null(cache$F_replace))  cache$F_replace  <- std_cache$F_replace

  # Initialise P from structural equilibrium if possible, else uniform
  thetastruct_init <- theta_init[config_8p_fe$struct_param_names]
  P <- NULL
  if (exists("solve_equilibrium_policy_replacement_8p", mode = "function")) {
    eq0 <- tryCatch(
      solve_equilibrium_policy_replacement_8p(thetastruct_init, cache,
                                              config_8p_fe,
                                              max_iter = 500, tol = 1e-7),
      error = function(e) NULL)
    if (!is.null(eq0) && isTRUE(eq0$converged)) P <- eq0$P
  }
  if (is.null(P)) {
    P <- matrix(1/3, nrow = 32L, ncol = 3L)
    colnames(P) <- c("maintain", "exit", "replace")
  }

  pn      <- config_8p_fe$param_names
  stopifnot(all(pn %in% names(theta_init)))
  theta   <- theta_init[pn]

  # Bounds: structural params unbounded (inherit from config); alphas bounded
  lower <- setNames(rep(-Inf, length(pn)), pn)
  upper <- setNames(rep( Inf, length(pn)), pn)
  lower[config_8p_fe$fe_param_names] <- config_8p_fe$alphabounds[1]
  upper[config_8p_fe$fe_param_names] <- config_8p_fe$alphabounds[2]
  # Also apply structural bounds from config
  lower["kappa_SW"]       <- config_8p_fe$kappa_SW_bounds[1]
  upper["kappa_SW"]       <- config_8p_fe$kappa_SW_bounds[2]
  lower["kappa_DW"]       <- config_8p_fe$kappa_DW_bounds[1]
  upper["kappa_DW"]       <- config_8p_fe$kappa_DW_bounds[2]
  lower["K_log_SW"]       <- config_8p_fe$K_log_SW_bounds[1]
  upper["K_log_SW"]       <- config_8p_fe$K_log_SW_bounds[2]
  lower["K_log_DW"]       <- config_8p_fe$K_log_DW_bounds[1]
  upper["K_log_DW"]       <- config_8p_fe$K_log_DW_bounds[2]
  lower["gamma_price_FF"] <- config_8p_fe$gamma_price_FF_bounds[1]
  upper["gamma_price_FF"] <- config_8p_fe$gamma_price_FF_bounds[2]
  lower["gamma_price_RB"] <- config_8p_fe$gamma_price_RB_bounds[1]
  upper["gamma_price_RB"] <- config_8p_fe$gamma_price_RB_bounds[2]
  lower["gamma_risk_FF"]  <- config_8p_fe$gamma_risk_FF_bounds[1]
  upper["gamma_risk_FF"]  <- config_8p_fe$gamma_risk_FF_bounds[2]
  lower["gamma_risk_RB"]  <- config_8p_fe$gamma_risk_RB_bounds[1]
  upper["gamma_risk_RB"]  <- config_8p_fe$gamma_risk_RB_bounds[2]

  ll_path   <- numeric(0)
  converged <- FALSE

  for (it in seq_len(config_8p_fe$max_npl_iter)) {
    if (verbose) cat(sprintf("\n[NPL 8p+FE] iter %d\n", it))

    opt <- optim(
      par    = theta,
      fn     = function(x) {
        names(x) <- pn
        npl_likelihood_replacement_8p_fe(x, P, cache, config_8p_fe)
      },
      method  = "L-BFGS-B",
      lower   = lower,
      upper   = upper,
      control = list(maxit = 200, factr = 1e8)
    )
    theta_new        <- opt$par
    names(theta_new) <- pn

    Pnew   <- .update_ccps_geoweighted_8p_fe(theta_new, P, cache, config_8p_fe)
    dtheta <- max(abs(theta_new - theta))
    dP     <- max(abs(Pnew - P))
    ll     <- -opt$value
    ll_path <- c(ll_path, ll)

    if (verbose) cat(sprintf(
      "  logLik=%.3f  max|dtheta|=%.3e  max|dP|=%.3e\n", ll, dtheta, dP))

    theta <- theta_new
    P     <- Pnew

    if (dtheta < config_8p_fe$tol_theta && dP < config_8p_fe$tol_P) {
      converged <- TRUE
      break
    }
  }
  if (verbose && !converged)
    warning("8p+FE NPL did not converge in ", config_8p_fe$max_npl_iter, " iterations")

  # Final V under converged theta
  thetastruct_fin <- theta[config_8p_fe$struct_param_names]
  U_fin <- flow_utilities_replacement_8p(thetastruct_fin, cache)
  V_fin <- invert_value_function_replacement(P, U_fin, cache, config_8p_fe)

  # theta_hat: structural on natural scale, FE on raw scale
  theta_hat <- c(
    kappa_SW       = unname(theta[["kappa_SW"]]),
    kappa_DW       = unname(theta[["kappa_DW"]]),
    K_SW           = exp(theta[["K_log_SW"]]),
    K_DW           = exp(theta[["K_log_DW"]]),
    gamma_price_FF = unname(theta[["gamma_price_FF"]]),
    gamma_price_RB = unname(theta[["gamma_price_RB"]]),
    gamma_risk_FF  = unname(theta[["gamma_risk_FF"]]),
    gamma_risk_RB  = unname(theta[["gamma_risk_RB"]])
  )
  for (s in CONTROLSTATES) {
    theta_hat[paste0("alpha", s)] <- unname(theta[[paste0("alpha", s)]])
  }

  list(
    theta_raw      = theta,
    theta_hat      = theta_hat,
    Phat           = P,
    V_hat          = V_fin,
    cache          = cache,
    config         = config_8p_fe,
    converged      = converged,
    logLik_path    = ll_path,
    logLik         = tail(ll_path, 1L),
    log_likelihood = tail(ll_path, 1L)   # alias for back-compat
  )
}
