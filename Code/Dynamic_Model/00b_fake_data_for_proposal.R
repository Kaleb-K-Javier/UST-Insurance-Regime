# ==============================================================================
# 00b_fake_data_for_proposal.R (FIXED - MONTE CARLO PARAMS)
# ==============================================================================
# PURPOSE: 
#   1. Generate synthetic data using the SUCCESSFUL Monte Carlo parameters:
#      - Kappa (Scrap) = 22.0
#      - Gamma Price   = -1.0
#      - Gamma Risk    = 0.6
#   2. Use the exact primitives (high hazards/losses) from the MC test
#      to ensure the data is identified and non-degenerate.
#   3. Output 'annual_facility_panel.csv' for estimation.
# ==============================================================================

library(data.table)
library(Matrix)
library(here)

# Load estimator helpers
source(here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))


# PATHS
DATA_DIR <- here("Data", "Processed")
RESULTS_DIR <- here("Output", "Estimation_Results")
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR, recursive = TRUE)


# ==============================================================================
# 1. SETUP: PRIMITIVES FROM SUCCESSFUL MONTE CARLO
# ==============================================================================
# Scale Factor: 1 unit = $10,000 (implied by ell0=1.0 ~ $10k)
SCALE <- 10000 

# Primitives from 03_Model_B_Binary_test.R (The "Working" Set)
params <- list(
  # Annual Probabilities of leak/failure
  h0 = 0.12,          # Base 12% risk
  h_single = 0.05,    # +5% for single wall
  h_age = 0.005,      # +0.5% per age bin
  
  # Loss Severity (Model Units: 1.0 = $10,000)
  ell0 = 1.0,         
  ell_age = 0.05,     
  
  # Annual Premiums (Model Units)
  p_FF_annual = 0.20,        # $2,000
  p0_RB_annual = 0.10,       # $1,000 base
  p_single_RB_annual = 0.10, # +$1,000 for single wall
  p_age_RB_annual = 0.02     # +$200 per age bin
)

# Transition Probabilities (Stochastic Aging from MC)
age_probs <- list(
  p_up = c(0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.24, 0.25, 0.00)
)
age_probs$p_stay <- 1.0 - age_probs$p_up

# ==============================================================================
# 2. TRUE PARAMETERS (MONTE CARLO TARGETS)
# ==============================================================================
theta_true <- c(
  kappa = 22.0,       # High scrap value required for this environment
  gamma_price = -1.0, # Standard price sensitivity
  gamma_risk = 0.6    # Partial internalization (60%)
)

cat("\n==============================================================\n")
cat(" GENERATING DATA: MONTE CARLO SPECIFICATION\n")
cat("==============================================================\n")
cat(sprintf("Target Parameters:\n  Kappa: %.2f\n  Gamma Price: %.2f\n  Gamma Risk: %.2f\n", 
            theta_true["kappa"], theta_true["gamma_price"], theta_true["gamma_risk"]))

# ==============================================================================
# 3. BUILD CACHE & SOLVE EQUILIBRIUM
# ==============================================================================
build_cache_and_solve <- function() {
  
  # 1. State Space
  states <- CJ(A = 1:9, w = factor(c("single", "double")), rho = factor(c("FF", "RB")))
  states[, state_idx := .I]
  setkey(states, state_idx)
  n <- nrow(states)
  
  # 2. Exogenous Vectors
# FF: flat premium (pooling contract, no risk variation)
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
                         params$h_age * states$A, 0.001), 0.60)
  
  losses <- pmax(params$ell0 + params$ell_age * states$A, 0.1)
  
  # 3. Transitions (Probabilistic Aging)
  state_map <- setNames(states$state_idx, paste(states$A, states$w, states$rho, sep="_"))
  F_maintain <- Matrix(0, n, n, sparse = TRUE)
  
  for (i in 1:n) {
    A_i <- states$A[i]
    if (A_i < 9) {
      k_stay <- paste(A_i, states$w[i], states$rho[i], sep = "_")
      k_up   <- paste(A_i + 1, states$w[i], states$rho[i], sep = "_")
      
      F_maintain[i, state_map[k_stay]] <- age_probs$p_stay[A_i]
      F_maintain[i, state_map[k_up]]   <- age_probs$p_up[A_i]
    } else {
      F_maintain[i, i] <- 1.0 # Absorbing
    }
  }
  
  transitions <- list(maintain = F_maintain, exit = Diagonal(n))
  
  # 4. Config & Solve
  config <- create_estimation_config_model_b(beta = 0.95, sigma2 = 1.0)
  cache <- create_estimation_cache_model_b(states, premiums, hazards, losses, transitions, config)
  
  eq <- solve_equilibrium_policy_model_b(theta_true, cache, config)
  
  return(list(cache = cache, eq = eq, states = states, transitions = transitions))
}

setup <- build_cache_and_solve()
P_opt <- setup$eq$P
cache <- setup$cache
states <- setup$states

# Diagnostic
avg_exit <- sum(P_opt[,"close"] * (1/nrow(states))) # Crude average
cat(sprintf("Average Model Exit Probability: %.2f%%\n", avg_exit * 100))

# ==============================================================================
# 4. SIMULATE PANEL DATA
# ==============================================================================
generate_panel <- function(N_fac = 3000, T_per = 50) {
  
  cat(sprintf("\nSimulating %d Facilities over %d Years...\n", N_fac, T_per))
  
  # Next state lookups for speed
  n_states <- nrow(states)
  state_map <- setNames(states$state_idx, paste(states$A, states$w, states$rho, sep="_"))
  next_stay <- integer(n_states)
  next_up   <- integer(n_states)
  
  for(i in 1:n_states) {
    k_stay <- paste(states$A[i], states$w[i], states$rho[i], sep="_")
    k_up   <- paste(min(states$A[i]+1, 9), states$w[i], states$rho[i], sep="_")
    next_stay[i] <- state_map[k_stay]
    next_up[i]   <- state_map[k_up]
  }
  
  panel_list <- vector("list", N_fac)
  
  for (i in 1:N_fac) {
    # Initial: 50/50 split on wall/regime, young age
    w <- sample(c("single", "double"), 1)
    r <- sample(c("FF", "RB"), 1)
    current_state <- state_map[paste(sample(1:3,1), w, r, sep="_")]
    
    active <- TRUE
    hist_list <- vector("list", T_per)
    
    for (t in 1:T_per) {
      if (!active) break
      
      # 1. Decision
      prob_close <- P_opt[current_state, "close"]
      action <- if(runif(1) < prob_close) 2L else 1L
      
      # 2. Outcomes (Leak & Cost)
      is_leaked <- 0L
      cleanup_cost <- 0.0
      
      if (action == 1L) {
        if(runif(1) < cache$hazards[current_state]) {
          is_leaked <- 1L
          # Generate cost in RAW DOLLARS (Model unit * SCALE)
          mu_cost <- cache$losses[current_state] * SCALE
          cleanup_cost <- rlnorm(1, meanlog = log(mu_cost), sdlog = 0.5)
        }
      }
      
      # 3. Record
      s_row <- states[current_state]
      hist_list[[t]] <- data.table(
        facility_id = i,
        year = 1990 + t,
        age_start = (s_row$A-1)*5, # Approx years
        wall_type = s_row$w,
        regime = s_row$rho,
        action_idx = action,
        action = ifelse(action==1, "maintain", "exit"),
        leaked_annual = is_leaked,
        total_cleanup_cost = cleanup_cost,
        premium_annual = cache$premiums[current_state] * SCALE # Raw Dollars
      )
      
      # 4. Transition
      if (action == 2L) {
        active <- FALSE
      } else {
        # Stochastic Aging
        p_up_val <- age_probs$p_up[s_row$A]
        if(runif(1) < p_up_val) {
          current_state <- next_up[current_state]
        } else {
          current_state <- next_stay[current_state]
        }
      }
    }
    panel_list[[i]] <- rbindlist(hist_list)
  }
  
  return(rbindlist(panel_list))
}

final_df <- generate_panel(N_fac = 10000)

# ==============================================================================
# 5. SAVE OUTPUTS
# ==============================================================================
cat(sprintf("\nFinal Dataset: %d rows, %.1f%% exit rate\n", 
            nrow(final_df), 
            100 * mean(final_df[, .(exit=max(action_idx==2)), by=facility_id]$exit)))

fwrite(final_df, file.path(DATA_DIR, "annual_facility_panel.csv"))

# Save Primitives for Welfare Script (Critical for consistency)
saveRDS(list(
  states = states,
  hazards = cache$hazards,
  losses = cache$losses,
  premiums = cache$premiums,
  transitions = setup$transitions, # Save PROBABILISTIC transitions
  scale = SCALE,
  theta_true = theta_true,
  params = params
), file.path(RESULTS_DIR, "Estimated_Primitives.rds"))

cat(sprintf("\nDONE: Data and primitives saved to %s\n", DATA_DIR))