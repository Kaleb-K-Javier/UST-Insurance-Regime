# ==============================================================================
# 03_Production_Estimation.R (SCALED VERSION)
# ==============================================================================
# PURPOSE: 
#   1. Load Annual Panel
#   2. Estimate Primitives (Hazards, Costs, Premiums)
#   3. SCALE INPUTS ($ -> Utility Units) to prevent numerical overflow
#   4. Run Model B Estimation
# ==============================================================================

library(data.table)
library(Matrix)
library(here)
library(caret) 

# Source helper functions
source(here("Code", "Helpers", "improved_estimator_OPTIMIZED.r")) 

# OUTPUT PATHS
RESULTS_DIR <- here("Output", "Estimation_Results")
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR, recursive = TRUE)

# ==============================================================================
# PART 1: LOAD ANNUAL DATA
# ==============================================================================
cat("\n[1/3] Loading Annual Panel...\n")

annual_panel <- fread(here("Data", "Processed", "annual_facility_panel.csv"))

# Construct State Indices
# Age Bins: 1=0-5, ... 9=40+
annual_panel[, A_idx := pmin(floor(age_start / 5) + 1, 9)]
annual_panel[, w_idx := ifelse(wall_type == "single", 1L, 2L)]
annual_panel[, r_idx := ifelse(regime == "RB", 2L, 1L)]

# ==============================================================================
# PART 2: PRIMITIVE ESTIMATION & SCALING
# ==============================================================================
cat("\n[2/3] Estimating Primitives & SCALING...\n")

# A. Define Scale Factor (Critical for Convergence)
# We convert dollars to "units of $10,000"
# This keeps utility differences in the range of [-10, 10] rather than [-100k, 100k]
SCALE_FACTOR <- 10000 

state_space <- CJ(A_idx = 1:9, w_idx = 1:2, r_idx = 1:2)
pred_grid <- copy(state_space)
pred_grid[, age_start := (A_idx - 1) * 5 + 2.5] 
pred_grid[, wall_type := ifelse(w_idx == 1, "single", "double")]
pred_grid[, regime := ifelse(r_idx == 1, "FF", "RB")]

# B. Estimate Primitives (Raw Dollars)

# 1. Hazards (Probability, no scaling needed)
h_model <- glm(leaked_annual ~ poly(age_start, 2) * wall_type, 
               data = annual_panel, family = binomial(link = "logit"))
h_vec <- predict(h_model, newdata = pred_grid, type = "response")

# 2. Losses (Dollars)
leak_obs <- annual_panel[leaked_annual == 1 & !is.na(total_cleanup_cost)]
l_model <- glm(total_cleanup_cost ~ wall_type + age_start, 
               data = leak_obs, family = gaussian(link = "identity"))
l_vec_raw <- predict(l_model, newdata = pred_grid, type = "response")

# 3. Premiums (Dollars)
if ("premium_annual" %in% names(annual_panel)) {
  p_model <- glm(premium_annual ~ regime * age_start, 
                 data = annual_panel, family = gaussian())
  p_vec_raw <- predict(p_model, newdata = pred_grid, type = "response")
} else {
  # Fallback Rule (matches your generator)
  p_vec_raw <- numeric(nrow(state_space))
  for(i in 1:nrow(state_space)) {
    if (state_space$r_idx[i] == 1) { 
      p_vec_raw[i] <- 500 
    } else {
      p_vec_raw[i] <- 1200 + (state_space$A_idx[i]-1)*100 
    }
  }
}

# C. APPLY SCALING
cat(sprintf("  - Scaling monetary units by factor: 1 / %d\n", SCALE_FACTOR))
l_vec_scaled <- l_vec_raw / SCALE_FACTOR
p_vec_scaled <- p_vec_raw / SCALE_FACTOR

primitives <- list(
  hazards = h_vec,
  losses = l_vec_scaled,   # SCALED
  premiums = p_vec_scaled, # SCALED
  scale_factor = SCALE_FACTOR # Save for reference
)

# ==============================================================================
# PART 3: NPL ESTIMATION
# ==============================================================================
cat("\n[3/3] Running Model B Estimation...\n")

# 1. Counts Matrix
annual_panel[, state_linear_idx := (r_idx - 1) * 18 + (w_idx - 1) * 9 + A_idx]
counts_dt <- annual_panel[, .N, by = .(state_linear_idx, action_idx)]
counts_matrix <- matrix(0, nrow = 36, ncol = 2)
for(i in 1:nrow(counts_dt)) {
  counts_matrix[counts_dt$state_linear_idx[i], counts_dt$action_idx[i]] <- counts_dt$N[i]
}

# 2. Transitions
T_mat <- Matrix(0, 36, 36, sparse = TRUE)
for(s in 1:36) {
  row <- state_space[s]
  next_A <- min(row$A_idx + 1, 9)
  next_s <- which(state_space$A_idx == next_A & state_space$w_idx == row$w_idx & state_space$r_idx == row$r_idx)
  T_mat[s, next_s] <- 1.0
}
transitions <- list(maintain = T_mat, exit = Diagonal(36))

# 3. Configure & Run
config <- create_estimation_config_model_b(beta = 0.95, sigma2 = 1, npl_iter = 100)

# SCALED INITIALIZATION
# If Kappa was $50,000, scaled it is 5.0
theta_init <- c(
  kappa = 5.0,         # Scaled from 50,000
  gamma_price = -1.0,  # Price sensitivity (should be negative)
  gamma_risk = 1.0     # Risk internalization (positive)
) 

est_result <- npl_estimator_model_b(
  counts_vec = counts_matrix, 
  states = state_space,
  premiums = primitives$premiums, # SCALED
  hazards = primitives$hazards,
  losses = primitives$losses,     # SCALED
  transitions = transitions,
  config = config,
  theta_init = theta_init,
  verbose = TRUE
)

# 4. Save
saveRDS(est_result, file = file.path(RESULTS_DIR, "Model_B_Estimates.rds"))
saveRDS(primitives, file = file.path(RESULTS_DIR, "Estimated_Primitives.rds"))

cat("\nEstimates (Scaled):\n")
print(est_result$theta_hat)
cat(sprintf("\nImplied Real Values (approx):\nKappa: $%.0f\n", est_result$theta_hat["kappa"] * SCALE_FACTOR))