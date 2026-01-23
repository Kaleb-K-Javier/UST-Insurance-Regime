# ==============================================================================
# 03_Model_B_Annual_Refactored.R
# ==============================================================================
# PURPOSE: Full parameter recovery test for Model B in ANNUAL time steps.
# UNIT CONVENTION: 
#   - Time: 1 period = 1 Year
#   - Flow Utility: Annual Profits (Normalized to ~1.0)
#   - Kappa: Scrap value in Annual Profits (e.g., 6.0 = 6 years of profit)
# ==============================================================================

library(data.table)
library(Matrix)
library(here)
library(numDeriv)
library(ggplot2)
library(gridExtra)
library(foreach)
library(doParallel)

# Define Output Paths using 'here' for repo portability
FIG_DIR <- here::here("Output", "Figures")
TAB_DIR <- here::here("Output", "Tables")

# Ensure directories exist
if (!dir.exists(FIG_DIR)) dir.create(FIG_DIR, recursive = TRUE)
if (!dir.exists(TAB_DIR)) dir.create(TAB_DIR, recursive = TRUE)

# Load your estimator library (assuming it's in the standard path)
# If running locally without the file structure, you can source the file manually
source(here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))


# ==============================================================================
# 1. OVERRIDES: CONFIGURATION FOR ANNUAL DATA
# ==============================================================================

create_estimation_config_model_b <- function(beta = 0.95, sigma2 = 1, npl_iter =5000 ) { 
  # NOTE: Increased sigma2 slightly to account for larger variance in annual shocks
  list(
    beta = beta,           # Annual Discount Factor (0.95 = 5% interest)
    sigma2 = sigma2,       # Scale parameter for Type I EV errors
    gamma_E = 0.5772156649,
    
    tol_theta = 1e-8,
    tol_P = 1e-7,
    max_npl_iter = npl_iter,
    
    eps_prob = 1e-10,
    min_log_val = 1e-12,
    
    # Bounds appropriate for Annual Units
    # Kappa 6.0 = 6 years of profit. Bounds [-50, 50] cover reasonable range.
    kappa_bounds = c(-50, 50), 
    gamma_bounds = c(-10, 5),
    
    n_actions = 2
  )
}


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
  premiums <- params$p_FF_annual + 
    ifelse(states$rho == "RB", params$p0_RB_annual, 0) +
    ifelse(states$w == "single", params$p_single_RB_annual, 0) +
    (states$A - 1) * params$p_age_RB_annual
  
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
    V_equilibrium <- invert_value_function_model_b(P_equilibrium, U, config)
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
# 3. TEST EXECUTION
# ==============================================================================
run_annual_test <- function() {
  cat("\n==============================================================\n")
  cat("           MODEL B: ANNUAL PANEL VALIDATION TEST\n")
  cat("==============================================================\n")
  
  # 1. Generate Data (Annual)
  cat("\nStep 1: Generating Annual Data (T=50 Years)...\n")
  
  # UPDATED: Calling with 3 parameters
  data <- generate_model_b_data_annual(
    N_facilities = 1500, 
  kappa_true = 22, 
  gamma_price_true = -1,
  gamma_risk_true = 0.6
  )
  
  cat(sprintf("  Facilities: %d\n", uniqueN(data$panel_data$facility_id)))
  cat(sprintf("  Observations: %d\n", nrow(data$panel_data)))
  cat(sprintf("  Annual Close Rate: %.2f%%\n", 100 * mean(data$panel_data$action == 2)))
  
  # 2. Estimation
  cat("\nStep 2: Running NPL Estimation (Annual Config)...\n")
  
  # Ensure we use the ANNUAL config
  config_est <- create_estimation_config_model_b(beta = 0.95, sigma2 = 1)
  
  est <- npl_estimator_model_b(
    counts_vec = data$counts_matrix,
    states = data$states,
    premiums = data$premiums,
    hazards = data$hazards,
    losses = data$losses,
    transitions = data$transitions,
    config = config_est,
    # UPDATED: Initializing 3 parameters
    theta_init = c(kappa = 0.0, gamma_price = -0.5, gamma_risk = 0.5), 
    verbose = TRUE
  )
  
  # 3. Results
  cat("\n==============================================================\n")
  cat("                    FINAL RESULTS\n")
  cat("==============================================================\n")
  
  # Extract True Parameters
  k_true  <- data$theta_true["kappa"]
  gp_true <- data$theta_true["gamma_price"]
  gr_true <- data$theta_true["gamma_risk"]
  
  # Extract Estimated Parameters
  k_est  <- est$theta_hat["kappa"]
  gp_est <- est$theta_hat["gamma_price"]
  gr_est <- est$theta_hat["gamma_risk"]
  
  cat(sprintf("\nParameter Recovery:\n"))
  cat(sprintf("  Kappa (Scrap): True = %6.2f | Est = %6.2f | Error = %.2f\n", 
              k_true, k_est, k_est - k_true))
  cat(sprintf("  Gamma (Price): True = %6.2f | Est = %6.2f | Error = %.2f\n", 
              gp_true, gp_est, gp_est - gp_true))
  cat(sprintf("  Gamma (Risk):  True = %6.2f | Est = %6.2f | Error = %.2f\n", 
              gr_true, gr_est, gr_est - gr_true))
  
  cat(sprintf("\nDiagnostics:\n"))
  cat(sprintf("  Converged: %s\n", est$converged))
  cat(sprintf("  Iterations: %d\n", est$n_iterations))
  cat(sprintf("  CCP Max Error: %.6f\n", max(abs(est$P_hat - data$P_true))))
}

# Run the test
run_annual_test()


calibrate_model_b_wide <- function() {
  
  # 1. Setup Environment
  config <- create_estimation_config_model_b(beta = 0.95, sigma2 = 1)
  # Dummy call to get structure
  dummy <- generate_model_b_data_annual(N_facilities = 1, T_periods = 1) 
  cache <- dummy$cache
  
  # 2. Define Wide Search Grid
  # We search a broader space since we aren't forcing Kappa
  grid <- expand.grid(
    kappa = seq(0, 80, by = 2),         # Wide range for Scrap Value
    gamma_price = seq(-3.0, -0.5, by = 0.5), # Wide range for Price Sensitivity
    gamma_risk = seq(0, 2,by=.2)          # Test Low, Standard, High Risk Aversion
  )
  
  results <- list()
  cat(sprintf("Scanning %d parameter combinations for ~10%% exit rate...\n", nrow(grid)))
  
  # Progress bar counter
  counter <- 0
  
  for(i in 1:nrow(grid)) {
    counter <- counter + 1
    if(counter %% 10 == 0) cat(sprintf("  ...checked %d/%d combinations\n", counter, nrow(grid)))

    theta_test <- c(
      kappa = grid$kappa[i], 
      gamma_price = grid$gamma_price[i], 
      gamma_risk = grid$gamma_risk[i]
    )
    
    # Solve Equilibrium Policy
    eq <- solve_equilibrium_policy_model_b(theta_test, cache, config)
    
    # Skip if solver failed (Numerical Instability)
    if (any(is.na(eq$P))) next
    
    # Check Proxy Exit Rate (Steady State) before running expensive simulation
    P_maintain <- eq$P[, "maintain"]
    proxy_exit <- mean(1 - P_maintain)
    
    # Filter: Only simulate if proxy is roughly between 5% and 20%
    # This saves huge amounts of time by ignoring 0% or 99% exit scenarios
    if (proxy_exit > 0.05 && proxy_exit < 0.20) {
      
      # Use MANUAL POLICY to ensure stability
      sim <- generate_model_b_data_annual(
        N_facilities = 1000, T_periods = 30,
        kappa_true = theta_test["kappa"],
        gamma_price_true = theta_test["gamma_price"],
        gamma_risk_true = theta_test["gamma_risk"],
        P_manual = eq$P
      )
      
      actual_rate <- mean(sim$panel_data$action == 2)
      
      results[[i]] <- data.table(
        kappa = theta_test["kappa"],
        gamma_price = theta_test["gamma_price"],
        gamma_risk = theta_test["gamma_risk"],
        exit_rate = actual_rate
      )
      
      # Visual flag for good hits
      star <- ifelse(actual_rate >= 0.08 & actual_rate <= 0.12, "*** HIT ***", "")
      cat(sprintf("  K=%4.0f GP=%4.1f GR=%3.1f -> Exit: %5.1f%% %s\n", 
                  theta_test[1], theta_test[2], theta_test[3], actual_rate * 100, star))
    }
  }
  
  if (length(results) > 0) {
    final_tab <- rbindlist(results)
    
    # Sort by distance to 10%
    final_tab[, diff := abs(exit_rate - 0.10)]
    best_candidates <- final_tab[order(diff)]
    
    print("\n--- TOP 5 PARAMETER CANDIDATES ---")
    print(best_candidates[1:5])
    
    return(best_candidates[1]) # Return the single best row
  } else {
    print("No parameters found in the 5-20% range. Try widening the grid further.")
    return(NULL)
  }
}

# Run it
# candidates <- calibrate_model_b_wide()



# ==============================================================================
# SECTION 4: MONTE CARLO DIAGNOSTICS & IDENTIFICATION SUITE (FINAL)
# ==============================================================================


plot_model_b_surface_3param <- function(fig_dir = FIG_DIR,
                                        kappa_true = 22.0,
                                        gp_true = -1.0,
                                        gr_true = 0.6) {
  
  cat("\n[Surface Scan] Generating 3-Parameter Likelihood Slices...\n")
  
  # 1. Generate Reference Data (Rich enough to identify all 3)
  #    Assumes generator now accepts gamma_price and gamma_risk
  data <- generate_model_b_data_annual(
    N_facilities = 2000, 
    kappa_true = kappa_true, 
    gamma_price_true = gp_true,
    gamma_risk_true = gr_true
  )
  
  P_emp <- compute_empirical_ccps_model_b(data$counts_matrix, nrow(data$states))
  
  # --- SLICE A: KAPPA vs GAMMA_PRICE (Fixing Risk) ---
  cat("  Scanning Slice A: Kappa vs Price Sensitivity...\n")
  grid_a <- expand.grid(
    kappa = seq(15, 30, length.out = 20),
    gp = seq(-2.0, -0.2, length.out = 20)
  )
  grid_a$ll <- NA
  
  for(i in 1:nrow(grid_a)) {
    theta_test <- c(kappa = grid_a$kappa[i], gamma_price = grid_a$gp[i], gamma_risk = gr_true)
    grid_a$ll[i] <- -npl_likelihood_model_b(
      theta = theta_test, P_fixed = P_emp,
      cache = data$cache, config = data$config, counts_vec = data$counts_matrix
    )
  }
  
  p1 <- ggplot(grid_a, aes(x = kappa, y = gp, z = ll)) +
    geom_contour_filled(bins = 20) +
    geom_point(aes(x = kappa_true, y = gp_true), color = "red", shape = 4, size = 5, stroke = 2) +
    labs(title = "Slice A: Scrap vs Price", x = "Kappa", y = "Gamma Price") + theme_minimal() + theme(legend.position="none")

  # --- SLICE B: GAMMA_PRICE vs GAMMA_RISK (Fixing Kappa) ---
  cat("  Scanning Slice B: Price vs Risk Sensitivity...\n")
  grid_b <- expand.grid(
    gp = seq(-2.0, -0.2, length.out = 20),
    gr = seq(0.0, 1.2, length.out = 20)
  )
  grid_b$ll <- NA
  
  for(i in 1:nrow(grid_b)) {
    theta_test <- c(kappa = kappa_true, gamma_price = grid_b$gp[i], gamma_risk = grid_b$gr[i])
    grid_b$ll[i] <- -npl_likelihood_model_b(
      theta = theta_test, P_fixed = P_emp,
      cache = data$cache, config = data$config, counts_vec = data$counts_matrix
    )
  }
  
  p2 <- ggplot(grid_b, aes(x = gp, y = gr, z = ll)) +
    geom_contour_filled(bins = 20) +
    geom_point(aes(x = gp_true, y = gr_true), color = "red", shape = 4, size = 5, stroke = 2) +
    labs(title = "Slice B: Price vs Risk", x = "Gamma Price", y = "Gamma Risk") + theme_minimal() + theme(legend.position="none")

  # Combine and Save
  outfile <- file.path(fig_dir, "03_ModelB_3Param_Surface.png")
  ggsave(outfile, arrangeGrob(p1, p2, ncol = 2), width = 12, height = 6)
  cat(sprintf("[Surface Scan] Saved plot to: %s\n", outfile))
}

# ------------------------------------------------------------------------------
# 4.2 Monte Carlo Engine
# ------------------------------------------------------------------------------
run_model_b_mc <- function(N_sim = 5, 
                           run_parallel = FALSE, 
                           n_cores = 4,
                           kappa_true = 17.0,
                           gamma_price_true = -1.2,
                           gamma_risk_true = 1.0) {
  
  cat(sprintf("\n[Monte Carlo] Starting %d simulations (Parallel=%s)...\n", N_sim, run_parallel))
  
  if (run_parallel) {
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    clusterEvalQ(cl, { library(data.table); library(Matrix); library(numDeriv) })
    clusterExport(cl, c("generate_model_b_data_annual", "create_estimation_config_model_b", 
                        "create_estimation_cache_model_b", "solve_equilibrium_policy_model_b",
                        "npl_estimator_model_b", "npl_likelihood_model_b",
                        "calculate_flow_utilities_model_b", "invert_value_function_model_b",
                        "compute_ccps_model_b", "compute_empirical_ccps_model_b"))
  }
  
  `%op%` <- if (run_parallel) `%dopar%` else `%do%`
  
  results <- foreach(i = 1:N_sim, .combine = rbind, 
                     .packages = c('data.table', 'Matrix', 'numDeriv')) %op% {
    
    # Generate
    sim_data <- generate_model_b_data_annual(
      N_facilities = 1000, 
      kappa_true = kappa_true, 
      gamma_price_true = gamma_price_true,
      gamma_risk_true = gamma_risk_true,
      seed = 1000 + i
    )
    
    # Estimate
    est_config <- create_estimation_config_model_b(beta = 0.95, sigma2 = 1)
    
    # Start slightly off-truth
    theta_start <- c(
      kappa = kappa_true * 0.9, 
      gamma_price = gamma_price_true * 0.9,
      gamma_risk = gamma_risk_true * 1.1
    )
    
    est <- npl_estimator_model_b(
      counts_vec = sim_data$counts_matrix, states = sim_data$states,
      premiums = sim_data$premiums, hazards = sim_data$hazards,
      losses = sim_data$losses, transitions = sim_data$transitions,
      config = est_config, theta_init = theta_start, verbose = FALSE
    )
    
    # Hessian / Condition Number
    hess_func <- function(theta_val) {
      names(theta_val) <- c("kappa", "gamma_price", "gamma_risk")
      npl_likelihood_model_b(theta = theta_val, P_fixed = est$P_hat,
                             cache = sim_data$cache, config = est_config, 
                             counts_vec = sim_data$counts_matrix)
    }
    
    H <- tryCatch(hessian(hess_func, est$theta_hat), error = function(e) matrix(NA,3,3))
    
    cond_num <- NA
    if (!any(is.na(H))) {
      eigs <- eigen(H)$values
      min_eig <- min(abs(eigs))
      if(min_eig > 1e-10) cond_num <- max(abs(eigs)) / min_eig
    }
    
    # Return row
    data.table(
      rep = i,
      kappa_est = est$theta_hat["kappa"], 
      gamma_price_est = est$theta_hat["gamma_price"],
      gamma_risk_est = est$theta_hat["gamma_risk"],
      
      kappa_true = kappa_true, 
      gamma_price_true = gamma_price_true,
      gamma_risk_true = gamma_risk_true,
      
      converged = est$converged,
      iters = est$n_iterations, 
      cond_num = cond_num
    )
  }
  
  if (run_parallel) stopCluster(cl)
  cat("[Monte Carlo] Completed.\n")
  return(results)
}

# ------------------------------------------------------------------------------
# 4.3 Reporting
# ------------------------------------------------------------------------------
generate_diagnostic_report <- function(results, fig_dir = FIG_DIR, tab_dir = TAB_DIR) {
  
  valid <- results[converged == TRUE]
  cat(sprintf("\n[Report] Analyzing %d converged runs...\n", nrow(valid)))
  
  # Get Truth
  k_true  <- valid$kappa_true[1]
  gp_true <- valid$gamma_price_true[1]
  gr_true <- valid$gamma_risk_true[1]
  
  # Histogram 1: Kappa
  p1 <- ggplot(valid, aes(x = kappa_est)) +
    geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha=0.7) +
    geom_vline(xintercept = k_true, color = "red", linetype = "dashed", linewidth=1) +
    labs(title = "Kappa (Scrap)", x="Estimate") + theme_minimal()
  
  # Histogram 2: Gamma Price
  p2 <- ggplot(valid, aes(x = gamma_price_est)) +
    geom_histogram(bins = 15, fill = "forestgreen", color = "white", alpha=0.7) +
    geom_vline(xintercept = gp_true, color = "red", linetype = "dashed", linewidth=1) +
    labs(title = "Gamma Price (Cost)", x="Estimate") + theme_minimal()
  
  # Histogram 3: Gamma Risk
  p3 <- ggplot(valid, aes(x = gamma_risk_est)) +
    geom_histogram(bins = 15, fill = "firebrick", color = "white", alpha=0.7) +
    geom_vline(xintercept = gr_true, color = "red", linetype = "dashed", linewidth=1) +
    labs(title = "Gamma Risk (Internalization)", x="Estimate") + theme_minimal()
  
  # Scatter: Price vs Risk (The Critical Identification Margin)
  truth_df <- data.frame(x = gp_true, y = gr_true)
  p4 <- ggplot(valid, aes(x = gamma_price_est, y = gamma_risk_est)) +
    geom_point(alpha = 0.6, size = 3) +
    geom_point(data = truth_df, aes(x = x, y = y), 
               color = "red", shape = 4, size = 5, stroke = 2) +
    labs(title = "Joint ID: Price vs Risk", subtitle = "Red X = Truth", 
         x = "Gamma Price", y = "Gamma Risk") + theme_minimal()
  
  # Save Plot
  outfile <- file.path(fig_dir, "03_ModelB_MC_Recovery_3Param.png")
  ggsave(outfile, arrangeGrob(p1, p2, p3, p4, ncol = 2), width = 10, height = 8)
  cat(sprintf("[Report] Saved plot to: %s\n", outfile))
  
  # Save Stats
  stats <- valid[, .(
    N = .N,
    # Kappa Stats
    Mean_K = mean(kappa_est), Bias_K = mean(kappa_est) - k_true, RMSE_K = sqrt(mean((kappa_est - k_true)^2)),
    # Gamma Price Stats
    Mean_GP = mean(gamma_price_est), Bias_GP = mean(gamma_price_est) - gp_true, RMSE_GP = sqrt(mean((gamma_price_est - gp_true)^2)),
    # Gamma Risk Stats
    Mean_GR = mean(gamma_risk_est), Bias_GR = mean(gamma_risk_est) - gr_true, RMSE_GR = sqrt(mean((gamma_risk_est - gr_true)^2)),
    
    Avg_Cond_Num = mean(cond_num, na.rm=TRUE)
  )]
  
  print(stats)
  fwrite(stats, file.path(tab_dir, "03_ModelB_Production_Stats.csv"))
  fwrite(valid, file.path(tab_dir, "03_ModelB_Production_Raw.csv"))
}

# ==============================================================================
# 5. EXECUTION BLOCK
# ==============================================================================
 # K=  22 GP=-1.0 GR=0.6 -> Exit:  15.2% 
# 1. Run Surface Scan (K vs Gamma_Price slice)
plot_model_b_surface_3param(kappa_true = 22.0, gp_true = -1.0, gr_true = 0.6)

# 2. Run Production Monte Carlo
mc_results <- run_model_b_mc(
  N_sim = 100,             # Production Size
  run_parallel = TRUE,    # Enable Speed
  n_cores = detectCores()/2,
  kappa_true = 22, 
  gamma_price_true = -1,
  gamma_risk_true = 0.6
)

# 3. Generate Final Report
if (nrow(mc_results) > 0) {
  generate_diagnostic_report(mc_results)
}
