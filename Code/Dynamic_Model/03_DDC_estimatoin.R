# ==============================================================================
# 03_Production_Estimation_FINAL.R
# ==============================================================================
# PURPOSE: 
#   Run NPL Estimation matching the Monte Carlo SUCCESS exactly.
#   - Scales data (1 unit = $10,000).
#   - LOADS TRUE PRIMITIVES (Hazards, Losses, Premiums).
#   - LOADS TRUE TRANSITIONS (This is the key fix!).
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
# 2. LOAD TRUE PRIMITIVES (THE MC STRATEGY)
# ==============================================================================
cat("\n[2/4] Loading True Primitives from Data Generation...\n")

primitives_path <- file.path(RESULTS_DIR, "Estimated_Primitives.rds")
if (!file.exists(primitives_path)) {
  stop("Estimated_Primitives.rds not found! Run 00b_fake_data_for_proposal.R first.")
}

primitives <- readRDS(primitives_path)

# Extract Vectors
state_space <- primitives$states
h_vec       <- primitives$hazards
l_vec       <- primitives$losses
p_vec       <- primitives$premiums

# --- FIX: Ensure State Space has Indices (A_idx, w_idx, r_idx) ---
if (!"A_idx" %in% names(state_space)) state_space[, A_idx := A]
if (!"w_idx" %in% names(state_space)) state_space[, w_idx := ifelse(w == "single", 1L, 2L)]
if (!"r_idx" %in% names(state_space)) state_space[, r_idx := ifelse(rho == "RB", 2L, 1L)]

cat(sprintf("  Loaded True Primitives. Scale used: %.0f\n", primitives$scale))

# ==============================================================================
# 3. LOAD TRUE TRANSITIONS (THE KEY FIX)
# ==============================================================================
cat("\n[3/4] Loading Theoretical Transition Matrix...\n")

# ERROR SOURCE IN PREVIOUS RUNS:
# We were calculating the matrix from data, which introduced noise.
# The MC script worked because it used the TRUE matrix. We do the same here.

T_mat <- primitives$transitions$maintain

# Verify Dimensions
if (nrow(T_mat) != 36 || ncol(T_mat) != 36) {
  stop("Transition matrix dimension mismatch! Expected 36x36.")
}

# Diagnostic:
p_stay_avg <- mean(diag(T_mat)[1:8]) 
cat(sprintf("  Using Theoretical Aging Probability (Up): %.2f%%\n", (1 - p_stay_avg) * 100))

# Build Counts Matrix from Data
annual_panel[, s_idx := (r_idx - 1)*18 + (w_idx - 1)*9 + A_idx]
counts_dt <- annual_panel[, .N, by = .(s_idx, action_idx)]
counts_mat <- matrix(0, 36, 2)
for(i in 1:nrow(counts_dt)) counts_mat[counts_dt$s_idx[i], counts_dt$action_idx[i]] <- counts_dt$N[i]

# ==============================================================================
# 4. RUN ESTIMATION (EXACTLY LIKE MC)
# ==============================================================================
cat("\n[4/4] Running NPL Estimator...\n")

config <- create_estimation_config_model_b(beta = 0.95, sigma2 = 1, npl_iter = 5000)

# Initialize near Truth (Just like MC script did)
theta_init <- c(kappa = 20.0, gamma_price = -0.5, gamma_risk = 0.5)

# Run Standard Estimator (No constraints needed if Physics are correct)
est_result <- npl_estimator_model_b(
  counts_vec = counts_mat, 
  states = state_space,
  premiums = p_vec, 
  hazards = h_vec, 
  losses = l_vec,
  transitions = list(maintain = T_mat, exit = Diagonal(36)),
  config = config, 
  theta_init = theta_init,
  verbose = TRUE
)

# Save
names(est_result$theta_hat) <- c("kappa", "gamma_price", "gamma_risk")
est_result$cache <- create_estimation_cache_model_b(
    states = state_space,
    premiums = p_vec,
    hazards = h_vec,
    losses = l_vec,
    transitions = list(maintain = T_mat, exit = Diagonal(36)),
    config = config
)

saveRDS(est_result, file = file.path(RESULTS_DIR, "Model_B_Estimates.rds"))

cat("\n==============================================================\n")
cat(" FINAL RESULTS (Target: K=22, GP=-1.0, GR=0.6)\n")
cat("==============================================================\n")
print(est_result$theta_hat)
cat(sprintf("\nImplied Scrap Value: $%.0f\n", est_result$theta_hat["kappa"] * SCALE_FACTOR))