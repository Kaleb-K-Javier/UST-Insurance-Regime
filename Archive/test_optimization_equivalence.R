# ==============================================================================
# test_optimization_equivalence.R - VERIFICATION SCRIPT
# ==============================================================================
# PURPOSE: Verify that optimized code produces estimates within tolerance of
#          original implementation
#
# TOLERANCE: |theta_opt - theta_orig| < 1e-10
# ==============================================================================

library(data.table)
library(here)
library(testthat)

cat("\n========================================\n")
cat("OPTIMIZATION EQUIVALENCE VERIFICATION\n")
cat("========================================\n\n")

# Source both versions
cat("Loading original estimator...\n")
source(here("Code", "Public_to_Private", "improved_estimator_FINAL (1).r"))
fns_orig <- list(
  npl_estimator = npl_estimator,
  npl_estimator_nested = npl_estimator_nested,
  em_npl_estimator = em_npl_estimator,
  em_npl_estimator_nested = em_npl_estimator_nested
)

cat("Loading optimized estimator...\n")
source(here("Code", "Public_to_Private", "improved_estimator_OPTIMIZED.r"))
fns_opt <- list(
  npl_estimator = npl_estimator,
  npl_estimator_nested = npl_estimator_nested,
  em_npl_estimator = em_npl_estimator,
  em_npl_estimator_nested = em_npl_estimator_nested
)

# Generate test data
cat("Generating test data...\n")
set.seed(12345)

source(here("Code", "Public_to_Private", "improved_generator_FINAL (1).r"))

test_data_std <- generate_production_data(
  N_facilities = 200,
  T_periods = 300,
  model_type = "standard",
  phi_true = 3.5,
  kappa_true = 75,
  seed = 12345
)

test_data_nested <- generate_production_data(
  N_facilities = 200,
  T_periods = 300,
  model_type = "nested",
  phi_true = 3.5,
  kappa_true = 75,
  sigma1_true = 0.5,
  seed = 22345
)

test_data_mix <- generate_production_data(
  N_facilities = 200,
  T_periods = 300,
  model_type = "mix_standard",
  K = 2,
  phi_mix = c(3.5, 4.5),
  kappa_mix = c(69, 75),
  pi_weights = c(0.6, 0.4),
  seed = 32345
)

# Test configuration
config <- create_estimation_config()

# ==============================================================================
# TEST 1: STANDARD NPL ESTIMATOR
# ==============================================================================

cat("\n--- TEST 1: Standard NPL ---\n")

cat("Running original estimator...\n")
est_orig_std <- fns_orig$npl_estimator(
  test_data_std$counts_vec, test_data_std$states, test_data_std$premiums,
  test_data_std$hazards, test_data_std$losses, test_data_std$transitions,
  config, verbose = FALSE
)

cat("Running optimized estimator...\n")
est_opt_std <- fns_opt$npl_estimator(
  test_data_std$counts_vec, test_data_std$states, test_data_std$premiums,
  test_data_std$hazards, test_data_std$losses, test_data_std$transitions,
  config, verbose = FALSE
)

theta_diff_std <- max(abs(est_orig_std$theta_hat - est_opt_std$theta_hat))
cat(sprintf("Max parameter difference: %.2e\n", theta_diff_std))
cat(sprintf("Original: phi=%.6f, kappa=%.6f\n", 
            est_orig_std$theta_hat[1], est_orig_std$theta_hat[2]))
cat(sprintf("Optimized: phi=%.6f, kappa=%.6f\n",
            est_opt_std$theta_hat[1], est_opt_std$theta_hat[2]))

test_that("Standard NPL produces equivalent estimates", {
  expect_lt(theta_diff_std, 1e-10)
})

if (theta_diff_std < 1e-10) {
  cat("✓ PASS: Estimates within tolerance\n")
} else {
  cat("✗ FAIL: Estimates differ beyond tolerance\n")
}

# ==============================================================================
# TEST 2: NESTED NPL ESTIMATOR
# ==============================================================================

cat("\n--- TEST 2: Nested NPL ---\n")

cat("Running original estimator...\n")
est_orig_nst <- fns_orig$npl_estimator_nested(
  test_data_nested$counts_vec, test_data_nested$states, test_data_nested$premiums,
  test_data_nested$hazards, test_data_nested$losses, test_data_nested$transitions,
  config, verbose = FALSE
)

cat("Running optimized estimator...\n")
est_opt_nst <- fns_opt$npl_estimator_nested(
  test_data_nested$counts_vec, test_data_nested$states, test_data_nested$premiums,
  test_data_nested$hazards, test_data_nested$losses, test_data_nested$transitions,
  config, verbose = FALSE
)

theta_diff_nst <- max(abs(c(est_orig_nst$theta_hat, est_orig_nst$sigma1_hat) - 
                          c(est_opt_nst$theta_hat, est_opt_nst$sigma1_hat)))
cat(sprintf("Max parameter difference: %.2e\n", theta_diff_nst))
cat(sprintf("Original: phi=%.6f, kappa=%.6f, sigma1=%.6f\n",
            est_orig_nst$theta_hat[1], est_orig_nst$theta_hat[2], est_orig_nst$sigma1_hat))
cat(sprintf("Optimized: phi=%.6f, kappa=%.6f, sigma1=%.6f\n",
            est_opt_nst$theta_hat[1], est_opt_nst$theta_hat[2], est_opt_nst$sigma1_hat))

test_that("Nested NPL produces equivalent estimates", {
  expect_lt(theta_diff_nst, 1e-10)
})

if (theta_diff_nst < 1e-10) {
  cat("✓ PASS: Estimates within tolerance\n")
} else {
  cat("✗ FAIL: Estimates differ beyond tolerance\n")
}

# ==============================================================================
# TEST 3: EM-NPL STANDARD MIXTURE
# ==============================================================================

cat("\n--- TEST 3: EM-NPL Standard Mixture ---\n")

cat("Running original estimator...\n")
est_orig_mix <- fns_orig$em_npl_estimator(
  test_data_mix$panel, K = 2, test_data_mix$states, test_data_mix$premiums,
  test_data_mix$hazards, test_data_mix$losses, test_data_mix$transitions,
  config, max_em_iter = 20, verbose = FALSE
)

cat("Running optimized estimator...\n")
est_opt_mix <- fns_opt$em_npl_estimator(
  test_data_mix$panel, K = 2, test_data_mix$states, test_data_mix$premiums,
  test_data_mix$hazards, test_data_mix$losses, test_data_mix$transitions,
  config, max_em_iter = 20, verbose = FALSE
)

theta_diff_mix <- max(abs(unlist(est_orig_mix$theta_list) - unlist(est_opt_mix$theta_list)))
pi_diff_mix <- max(abs(est_orig_mix$pi_hat - est_opt_mix$pi_hat))

cat(sprintf("Max parameter difference: %.2e\n", theta_diff_mix))
cat(sprintf("Max mixing proportion difference: %.2e\n", pi_diff_mix))

cat("Original Type 1: phi=%.6f, kappa=%.6f\n",
    est_orig_mix$theta_list[[1]][1], est_orig_mix$theta_list[[1]][2])
cat("Optimized Type 1: phi=%.6f, kappa=%.6f\n",
    est_opt_mix$theta_list[[1]][1], est_opt_mix$theta_list[[1]][2])

test_that("EM-NPL Standard produces equivalent estimates", {
  expect_lt(theta_diff_mix, 1e-8)  # Slightly relaxed due to EM randomness
  expect_lt(pi_diff_mix, 1e-8)
})

if (theta_diff_mix < 1e-8 && pi_diff_mix < 1e-8) {
  cat("✓ PASS: Estimates within tolerance\n")
} else {
  cat("✗ FAIL: Estimates differ beyond tolerance\n")
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n========================================\n")
cat("VERIFICATION SUMMARY\n")
cat("========================================\n\n")

all_tests_pass <- (theta_diff_std < 1e-10) && 
                  (theta_diff_nst < 1e-10) && 
                  (theta_diff_mix < 1e-8)

if (all_tests_pass) {
  cat("✓ ALL TESTS PASSED\n")
  cat("Optimized code produces equivalent estimates to original.\n")
  cat("Safe to use optimized version for production runs.\n")
} else {
  cat("✗ SOME TESTS FAILED\n")
  cat("Review differences before using optimized version.\n")
}

cat("\nTest Results:\n")
cat(sprintf("  Standard NPL: %s (diff: %.2e)\n", 
            ifelse(theta_diff_std < 1e-10, "PASS", "FAIL"), theta_diff_std))
cat(sprintf("  Nested NPL: %s (diff: %.2e)\n",
            ifelse(theta_diff_nst < 1e-10, "PASS", "FAIL"), theta_diff_nst))
cat(sprintf("  EM-NPL Mix: %s (diff: %.2e)\n",
            ifelse(theta_diff_mix < 1e-8, "PASS", "FAIL"), theta_diff_mix))

cat("\n")
