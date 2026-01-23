# ==============================================================================
# 03_Production_Estimation.R
# ==============================================================================
# PURPOSE: 
#   Run NPL Estimation on the full Annual Facility Panel.
#   - Scales data (1 unit = $10,000) for numerical stability.
#   - Uses Damped NPL (alpha=0.5) to ensure convergence.
#   - Estimates structural primitives (h, L, p) from data.
#   - Saves all inputs required for Counterfactual Analysis.
# ==============================================================================

library(data.table)
library(Matrix)
library(here)
library(caret) 

# Source helper functions (Must contain npl_estimator_damped from previous step)
source(here("Code", "Helpers", "improved_estimator_OPTIMIZED.r")) 

# OUTPUT PATHS
RESULTS_DIR <- here("Output", "Estimation_Results")
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR, recursive = TRUE)

# CONFIGURATION
SCALE_FACTOR <- 10000 

# ==============================================================================
# 1. LOAD & PREP DATA
# ==============================================================================
cat("\n[1/4] Loading Data...\n")
annual_panel <- fread(here("Data", "Processed", "annual_facility_panel.csv"))

# Create State Indices
annual_panel[, A_idx := pmin(floor(age_start / 5) + 1, 9)]
annual_panel[, w_idx := ifelse(wall_type == "single", 1L, 2L)]
annual_panel[, r_idx := ifelse(regime == "RB", 2L, 1L)]

# ==============================================================================
# 2. ESTIMATE PRIMITIVES (DATA -> STATE SPACE)
# ==============================================================================
cat("\n[2/4] Estimating & Scaling Primitives...\n")

# Define Canonical State Space
state_space <- CJ(A_idx = 1:9, w_idx = 1:2, r_idx = 1:2)
pred_grid <- copy(state_space)
pred_grid[, age_start := (A_idx - 1) * 5 + 2.5] 
pred_grid[, wall_type := ifelse(w_idx == 1, "single", "double")]
pred_grid[, regime := ifelse(r_idx == 1, "FF", "RB")]

# --- A. Hazards (Logit) ---
# Probability of leak (not scaled)
h_model <- glm(leaked_annual ~ poly(age_start, 2) * wall_type, 
               data = annual_panel, family = binomial)
h_vec <- predict(h_model, newdata = pred_grid, type = "response")

# --- B. Losses (GLM) - SCALED ---
# Use subset with leaks
# We divide by SCALE_FACTOR to convert $ -> Utility Units
l_model <- glm(total_cleanup_cost ~ wall_type + age_start, 
               data = annual_panel[leaked_annual == 1], family = gaussian)
l_vec_raw <- predict(l_model, newdata = pred_grid, type = "response")
l_vec <- l_vec_raw / SCALE_FACTOR

# --- C. Premiums - SCALED ---
if ("premium_annual" %in% names(annual_panel)) {
  p_model <- glm(premium_annual ~ regime * age_start, 
                 data = annual_panel, family = gaussian)
  p_vec_raw <- predict(p_model, newdata = pred_grid, type = "response")
} else {
  # Fallback if no premium col
  p_vec_raw <- ifelse(state_space$r_idx==1, 500, 1200 + (state_space$A_idx-1)*100)
}
p_vec <- p_vec_raw / SCALE_FACTOR

# --- D. SAVE PRIMITIVES (CRITICAL STEP) ---
# We verify 'states' is included so 04_Counterfactuals works
saveRDS(list(
  states = state_space,   # Required to rebuild matrices
  hazards = h_vec,
  losses = l_vec,
  premiums = p_vec,
  scale = SCALE_FACTOR
), file.path(RESULTS_DIR, "Estimated_Primitives.rds"))

# ==============================================================================
# 3. BUILD MATRICES
# ==============================================================================
cat("\n[3/4] Building Matrices...\n")

# Counts
annual_panel[, s_idx := (r_idx - 1)*18 + (w_idx - 1)*9 + A_idx]
counts_dt <- annual_panel[, .N, by = .(s_idx, action_idx)]
counts_mat <- matrix(0, 36, 2)
for(i in 1:nrow(counts_dt)) counts_mat[counts_dt$s_idx[i], counts_dt$action_idx[i]] <- counts_dt$N[i]

# Transitions
T_mat <- Matrix(0, 36, 36, sparse = TRUE)
for(s in 1:36) {
  row <- state_space[s]
  next_A <- min(row$A_idx + 1, 9)
  next_s <- which(state_space$A_idx == next_A & 
                  state_space$w_idx == row$w_idx & 
                  state_space$r_idx == row$r_idx)
  T_mat[s, next_s] <- 1.0
}

# ==============================================================================
# 4. RUN DAMPED ESTIMATION
# ==============================================================================
cat("\n[4/4] Running Damped NPL Estimator...\n")

config <- create_estimation_config_model_b(beta = 0.95, sigma2 = 1, npl_iter = 5000)

# Init: Start near Calibration Winner to avoid local optima
theta_init <- c(kappa = 18.0, gamma_price = -1.7, gamma_risk = 1.2)

# Call the DAMPED estimator (alpha=0.5)
est_result <- npl_estimator_damped(
  counts_vec = counts_mat, 
  states = state_space,
  premiums = p_vec, 
  hazards = h_vec, 
  losses = l_vec,
  transitions = list(maintain = T_mat, exit = Diagonal(36)),
  config = config, 
  theta_init = theta_init, 
  alpha = 0.5 # Damping reduces oscillation
)

# Save
names(est_result$theta_hat) <- c("kappa", "gamma_price", "gamma_risk")
saveRDS(est_result, file = file.path(RESULTS_DIR, "Model_B_Estimates.rds"))

cat("\nFinal Estimates:\n")
print(est_result$theta_hat)
cat(sprintf("\nImplied Scrap Value: $%.0f\n", est_result$theta_hat["kappa"] * SCALE_FACTOR))