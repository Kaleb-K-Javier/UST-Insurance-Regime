# ==============================================================================
# 00_Generate_Believable_Data.R (WIDE GRID + FIXES)
# ==============================================================================
# PURPOSE: 
#   1. Define Scaled Environment (1 unit = $10,000)
#   2. CALIBRATE parameters with a WIDE GRID to find realistic exit rates (~12%)
#   3. Generate full dataset using those calibrated parameters.
# ==============================================================================

library(data.table)
library(Matrix)
library(here)

# Assumes you have updated this file with npl_estimator_damped
source(here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))

# PATHS
DATA_DIR <- here("Data", "Processed")
RESULTS_DIR <- here("Output", "Estimation_Results")
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR, recursive = TRUE)

# ==============================================================================
# 1. SETUP: SCALED PRIMITIVES
# ==============================================================================
SCALE <- 10000 

# Base Economic Primitives (Scaled)
params <- list(
  h0 = 0.02, h_single = 0.03, h_age = 0.002,   
  ell0 = 50000 / SCALE, 
  ell_age = 5000 / SCALE, 
  p_FF = 500 / SCALE,
  p_RB_base = 1200 / SCALE,
  p_age = 100 / SCALE
)

# Cache Builder
build_cache <- function() {
  states <- CJ(A = 1:9, w = factor(c("single", "double")), rho = factor(c("FF", "RB")))
  states[, state_idx := .I]; setkey(states, state_idx)
  n <- nrow(states)
  
  premiums <- params$p_FF + 
    ifelse(states$rho == "RB", params$p_RB_base - params$p_FF, 0) + 
    ifelse(states$rho == "RB", (states$A - 1) * params$p_age, 0)
  
  hazards <- pmin(params$h0 + (states$w == "single")*params$h_single + params$h_age*states$A, 0.40)
  losses <- params$ell0 + params$ell_age * states$A
  
  T_mat <- Matrix(0, n, n, sparse = TRUE)
  for (i in 1:n) {
    if (states$A[i] < 9) {
      next_s <- which(states$A == states$A[i] + 1 & states$w == states$w[i] & states$rho == states$rho[i])
      T_mat[i, next_s] <- 1.0
    } else { T_mat[i, i] <- 1.0 }
  }
  
  config <- create_estimation_config_model_b(beta = 0.95, sigma2 = 1)
  
  list(
    n_states = n,           
    states = states, premiums = premiums, hazards = hazards, losses = losses,
    hazard_loss = hazards * losses, # CRITICAL FIX for Model B
    F_maintain = T_mat, F_exit = Diagonal(n),
    config = config, transitions = list(maintain = T_mat, exit = Diagonal(n))
  )
}

# ==============================================================================
# 2. CALIBRATION LOOP (WIDE GRID)
# ==============================================================================
find_calibrated_parameters <- function() {
  cat("\n[1/3] Calibrating Parameters (Wide Grid)...\n")
  
  cache <- build_cache()
  
  # --- UPDATED GRID ---
  grid <- expand.grid(
    kappa = seq(1.0, 10.0, by = 0.5),       
    gamma_price = seq(-2.0, 2.0, by = 0.5), 
    gamma_risk = seq(0.0, 1.6, by = 0.2)    
  )
  
  cat(sprintf("      Scanning %d parameter combinations...\n", nrow(grid)))
  
  best_theta <- NULL
  best_diff <- Inf
  
  pb <- txtProgressBar(min = 0, max = nrow(grid), style = 3)
  
  for(i in 1:nrow(grid)) {
    setTxtProgressBar(pb, i)
    
    theta_try <- c(
      kappa = grid$kappa[i], 
      gamma_price = grid$gamma_price[i], 
      gamma_risk = grid$gamma_risk[i]
    )
    
    # Solve Equilibrium (Try/Catch for stability)
    eq <- tryCatch({
      solve_equilibrium_policy_model_b(theta_try, cache, cache$config)
    }, error = function(e) list(converged=FALSE))
    
    if(eq$converged) {
      # Target ~0.5% annual closure (corresponds to ~15% lifetime)
      avg_exit <- mean(eq$P[,2])
      dist <- abs(avg_exit - 0.005) 
      
      if(dist < best_diff) {
        best_diff <- dist
        best_theta <- theta_try
      }
    }
  }
  close(pb)
  
  if(is.null(best_theta)) stop("Calibration failed to find valid parameters.")
  
  cat(sprintf("\n>>> WINNER (Diff=%.5f):\n", best_diff))
  print(best_theta)
  return(best_theta)
}

# ==============================================================================
# 3. GENERATION
# ==============================================================================
generate_final_data <- function() {
  
  # 1. Run Calibration
  theta_star <- find_calibrated_parameters()
  
  # 2. Build Cache & Solve Final Policy
  cat("\n[2/3] Solving Final Equilibrium...\n")
  cache <- build_cache()
  eq <- solve_equilibrium_policy_model_b(theta_star, cache, cache$config)
  P_opt <- eq$P
  
  # 3. Simulate Full Panel
  cat("\n[3/3] Simulating Panel (N=2500, T=40)...\n")
  N_fac <- 2500; T_per <- 40
  panel_list <- vector("list", N_fac)
  
  for (i in 1:N_fac) {
    w <- sample(c("single", "double"), 1, prob=c(0.6, 0.4))
    r <- sample(c("FF", "RB"), 1, prob=c(0.5, 0.5))
    a <- sample(1:4, 1) # Start young
    
    active <- TRUE
    hist_list <- vector("list", T_per)
    
    for (t in 1:T_per) {
      if (!active) break
      
      # State Index
      s_idx <- which(cache$states$A == a & cache$states$w == w & cache$states$rho == r)
      
      # Action
      prob_close <- P_opt[s_idx, 2]
      act <- if(runif(1) < prob_close) 2L else 1L
      
      # Outcome
      leak <- as.integer(runif(1) < cache$hazards[s_idx])
      cost <- if(leak) rlnorm(1, log(cache$losses[s_idx]), 0.5) else NA
      
      # Store (RAW DOLLARS for realism, we will scale in estimation)
      hist_list[[t]] <- data.table(
        facility_id = i, year = 1990 + t,
        age_start = (a-1)*5, wall_type = w, regime = r,
        action_idx = act,
        leaked_annual = leak,
        total_cleanup_cost = cost * SCALE, 
        premium_annual = cache$premiums[s_idx] * SCALE
      )
      
      if (act == 2) active <- FALSE else a <- min(a + 1, 9)
    }
    panel_list[[i]] <- rbindlist(hist_list)
  }
  
  dt <- rbindlist(panel_list)
  
  # Stats
  exit_rate <- mean(dt[, .(closed = max(action_idx==2)), by=facility_id]$closed)
  cat(sprintf("\nFinal Stats:\n  Total Obs: %d\n  Unique Facilities: %d\n  Lifetime Exit Rate: %.1f%%\n", 
              nrow(dt), length(unique(dt$facility_id)), exit_rate*100))
  
  # SAVE
  fwrite(dt, file.path(DATA_DIR, "annual_facility_panel.csv"))
  saveRDS(list(theta = theta_star, scale = SCALE, params = params), 
          file.path(RESULTS_DIR, "true_parameters.rds"))
  
  cat(sprintf("\nSaved to %s\n", DATA_DIR))
}

# RUN EVERYTHING
generate_final_data()