# ==============================================================================
# 03_Model_B_Rescaled_Recovery_Test_FIXED.R
# ==============================================================================
# PURPOSE: 
#   Verify parameter recovery with:
#   1. "Competitive" True Parameters (Exits ~5-10% to ensure identification)
#   2. Damped NPL Estimator (To prevent numerical oscillation)
# ==============================================================================

library(data.table)
library(Matrix)
library(here)

# Source standard helpers
source(here("Code", "Helpers", "improved_estimator_OPTIMIZED.r")) 

# ==============================================================================
# 1. PARAMETERS (ADJUSTED FOR IDENTIFICATION)
# ==============================================================================
SCALE <- 10000 
N_FACILITIES <- 2000
T_PERIODS <- 50

# ADJUSTMENT: Kappa must be comparable to V_maintain (~20) to generate signal
# V_maint approx 1.0 / (1-0.95) = 20.
# If Kappa is 2.5, exit is impossible. If Kappa is 15, exit is a real choice.
theta_true <- c(
  kappa = 18.54,        # Increased from 2.5 to generate active sorting
  gamma_price = -1.73,  
  gamma_risk = 1.23     
)

# Economic Primitives
params <- list(
  h0 = 0.02, h_single = 0.03, h_age = 0.002,
  ell0 = 50000 / SCALE, ell_age = 5000 / SCALE, 
  p_FF = 500 / SCALE, p_RB_base = 1200 / SCALE, p_age = 100 / SCALE
)

# ==============================================================================
# 2. DAMPED ESTIMATOR (INLINE DEFINITION FOR SAFETY)
# ==============================================================================
npl_estimator_damped <- function(counts_vec, states, premiums, hazards, losses, 
                                 transitions, config, theta_init, alpha=0.35, verbose=TRUE) {
  
  cache <- create_estimation_cache_model_b(states, premiums, hazards, losses, transitions, config)
  n_states <- cache$n_states
  
  # 1. Init P (Empirical)
  P <- compute_empirical_ccps_model_b(counts_vec, n_states, config$eps_prob)
  theta_curr <- theta_init
  
  for (iter in 1:config$max_npl_iter) {
    theta_old <- theta_curr
    
    # 2. Maximization (M-Step)
    # Using small maxit for inner loop speed
    opt <- optim(par = theta_curr, fn = npl_likelihood_model_b,
                 P_fixed = P, cache = cache, config = config, counts_vec = counts_vec,
                 method = "L-BFGS-B",
                 lower = c(-50, -10, 0), upper = c(50, 5, 10),
                 control = list(maxit = 50))
    theta_curr <- opt$par
    names(theta_curr) <- c("kappa", "gamma_price", "gamma_risk")
    
    # 3. Policy Update (With Damping)
    U <- calculate_flow_utilities_model_b(theta_curr, cache)
    V <- invert_value_function_model_b(P, U, config)
    P_new_pure <- compute_ccps_model_b(U, V, cache, config)
    
    # DAMPING: P_next = alpha*P_new + (1-alpha)*P_old
    P_next <- (alpha * P_new_pure) + ((1 - alpha) * P)
    
    # Check Convergence
    theta_diff <- max(abs(theta_curr - theta_old))
    P_diff <- max(abs(P_next - P))
    
    if (verbose && iter %% 5 == 0) {
      cat(sprintf("  Iter %3d: k=%5.2f gP=%5.2f gR=%5.2f | dTh=%.1e dP=%.1e | LL=%.1f\n",
                  iter, theta_curr[1], theta_curr[2], theta_curr[3], theta_diff, P_diff, -opt$value))
    }
    
    P <- P_next
    
    if (theta_diff < 1e-6 && P_diff < 1e-6) return(list(theta_hat=theta_curr, converged=TRUE))
  }
  return(list(theta_hat=theta_curr, converged=FALSE))
}

# ==============================================================================
# 3. GENERATE & RUN
# ==============================================================================
run_test <- function() {
  cat("\n[1/3] Generating Data (Targeting ~10-15% Exits)...\n")
  
  # Build Cache
  states <- CJ(A = 1:9, w = factor(c("single", "double")), rho = factor(c("FF", "RB")))
  states[, state_idx := .I]; setkey(states, state_idx)
  n <- nrow(states)
  
  premiums <- params$p_FF + ifelse(states$rho=="RB", params$p_RB_base-params$p_FF, 0) + 
              ifelse(states$rho=="RB", (states$A-1)*params$p_age, 0)
  hazards <- pmin(params$h0 + (states$w=="single")*params$h_single + params$h_age*states$A, 0.40)
  losses <- params$ell0 + params$ell_age * states$A
  
  T_mat <- Matrix(0, n, n, sparse = TRUE)
  for (i in 1:n) {
    if (states$A[i] < 9) {
      next_s <- which(states$A == states$A[i]+1 & states$w == states$w[i] & states$rho == states$rho[i])
      T_mat[i, next_s] <- 1.0
    } else { T_mat[i, i] <- 1.0 }
  }
  
  config <- create_estimation_config_model_b(beta = 0.95, sigma2 = 1)
  cache <- list(n_states = n, states = states, premiums = premiums, hazards = hazards, 
                losses = losses, hazard_loss = hazards*losses, 
                F_maintain = T_mat, F_exit = Diagonal(n), config = config,
                transitions = list(maintain = T_mat, exit = Diagonal(n)))
  
  # Solve Equilibrium
  eq <- solve_equilibrium_policy_model_b(theta_true, cache, config)
  P_true <- eq$P
  cat(sprintf("      True Exit Rate: %.2f%%\n", 100 * mean(P_true[,2])))
  
  # Simulate Counts (Fast Vectorized)
  counts_mat <- matrix(0, n, 2)
  state_vec <- sample(1:n, N_FACILITIES, replace=TRUE)
  
  for(t in 1:T_PERIODS) {
    probs_close <- P_true[state_vec, 2]
    actions <- rbinom(N_FACILITIES, 1, probs_close) + 1
    
    # Tally
    idx_counts <- cbind(state_vec, actions)
    # Simple aggregation loop
    for(i in 1:N_FACILITIES) {
      counts_mat[state_vec[i], actions[i]] <- counts_mat[state_vec[i], actions[i]] + 1
    }
    
    # Transition
    keep <- (actions == 1)
    if(sum(keep) == 0) break
    
    # Age active
    active_idx <- which(keep)
    for(k in active_idx) {
      s <- state_vec[k]
      if(states$A[s] < 9) state_vec[k] <- s + 1
    }
    
    # Replace exits (keep N constant for stable stats)
    exit_idx <- which(!keep)
    if(length(exit_idx) > 0) state_vec[exit_idx] <- sample(1:n, length(exit_idx), replace=TRUE)
  }
  
  cat("\n[2/3] Running Damped Estimator...\n")
  # Start further away to prove convergence
  theta_init <- c(kappa=10.0, gamma_price=0.0, gamma_risk=0.0) 
  
  est <- npl_estimator_damped(
    counts_vec = counts_mat, states = states, premiums = premiums, 
    hazards = hazards, losses = losses, transitions = list(maintain=T_mat, exit=Diagonal(n)),
    config = config, theta_init = theta_init, alpha = 0.5
  )
  
  cat("\n[3/3] Results:\n")
  res <- data.table(Parameter=names(theta_true), True=theta_true, Est=est$theta_hat)
  res[, Bias := Est - True]
  res[, Pct_Error := sprintf("%.1f%%", 100*abs(Bias)/abs(True))]
  print(res)
}

run_test()