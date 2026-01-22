# ==============================================================================
# benchmark_speedup.R - PERFORMANCE BENCHMARK SCRIPT
# ==============================================================================
# PURPOSE: Measure speedup of optimized code vs. original implementation
#
# OUTPUTS: Timing comparison table with speedup factors
# ==============================================================================

library(data.table)
library(here)
library(microbenchmark)

cat("\n========================================\n")
cat("PERFORMANCE BENCHMARK\n")
cat("========================================\n\n")

# ==============================================================================
# SETUP
# ==============================================================================

cat("Loading implementations...\n")

# Original
source(here("Code", "Public_to_Private", "improved_estimator_FINAL (1).r"))
source(here("Code", "Public_to_Private", "improved_generator_FINAL (1).r"))
fns_orig <- list(
  npl_estimator = npl_estimator,
  npl_estimator_nested = npl_estimator_nested,
  em_npl_estimator = em_npl_estimator,
  generate_data = generate_production_data
)

# Optimized
rm(list = c("npl_estimator", "npl_estimator_nested", "em_npl_estimator", 
            "generate_production_data"))
source(here("Code", "Public_to_Private", "improved_estimator_OPTIMIZED.r"))
source(here("Code", "Public_to_Private", "improved_generator_OPTIMIZED.r"))
fns_opt <- list(
  npl_estimator = npl_estimator,
  npl_estimator_nested = npl_estimator_nested,
  em_npl_estimator = em_npl_estimator,
  generate_data = generate_production_data
)

# Test configuration
config <- create_estimation_config()

# Generate benchmark datasets
cat("Generating benchmark data...\n")
set.seed(99999)

# Small dataset (quick benchmark)
data_small_std <- fns_orig$generate_data(
  N_facilities = 100, T_periods = 200,
  model_type = "standard",
  phi_true = 3.5, kappa_true = 75,
  seed = 11111
)

data_small_nst <- fns_orig$generate_data(
  N_facilities = 100, T_periods = 200,
  model_type = "nested",
  phi_true = 3.5, kappa_true = 75, sigma1_true = 0.5,
  seed = 22222
)

data_small_mix <- fns_orig$generate_data(
  N_facilities = 100, T_periods = 200,
  model_type = "mix_standard",
  K = 2, phi_mix = c(3.5, 4.5), kappa_mix = c(69, 75),
  pi_weights = c(0.6, 0.4),
  seed = 33333
)

# Medium dataset (realistic benchmark)
data_med_std <- fns_orig$generate_data(
  N_facilities = 300, T_periods = 500,
  model_type = "standard",
  phi_true = 3.5, kappa_true = 75,
  seed = 44444
)

data_med_mix <- fns_orig$generate_data(
  N_facilities = 300, T_periods = 500,
  model_type = "mix_standard",
  K = 2, phi_mix = c(3.5, 4.5), kappa_mix = c(69, 75),
  pi_weights = c(0.6, 0.4),
  seed = 55555
)

# ==============================================================================
# BENCHMARK FUNCTIONS
# ==============================================================================

benchmark_npl <- function(data, estimator_fn, config, n_runs = 3) {
  times <- numeric(n_runs)
  
  for (i in 1:n_runs) {
    t_start <- proc.time()
    est <- estimator_fn(
      data$counts_vec, data$states, data$premiums, data$hazards,
      data$losses, data$transitions, config, verbose = FALSE
    )
    t_end <- proc.time()
    times[i] <- (t_end - t_start)[3]
  }
  
  list(
    mean = mean(times),
    sd = sd(times),
    min = min(times),
    max = max(times)
  )
}

benchmark_em <- function(data, estimator_fn, K, config, max_em_iter, n_runs = 3) {
  times <- numeric(n_runs)
  
  for (i in 1:n_runs) {
    t_start <- proc.time()
    est <- estimator_fn(
      data$panel, K = K, data$states, data$premiums, data$hazards,
      data$losses, data$transitions, config, max_em_iter = max_em_iter,
      verbose = FALSE
    )
    t_end <- proc.time()
    times[i] <- (t_end - t_start)[3]
  }
  
  list(
    mean = mean(times),
    sd = sd(times),
    min = min(times),
    max = max(times)
  )
}

# ==============================================================================
# RUN BENCHMARKS
# ==============================================================================

results_table <- data.table(
  Component = character(),
  Dataset = character(),
  Original_Mean = numeric(),
  Original_SD = numeric(),
  Optimized_Mean = numeric(),
  Optimized_SD = numeric(),
  Speedup = numeric()
)

# --- STANDARD NPL (SMALL) ---
cat("\n--- Benchmarking Standard NPL (Small) ---\n")
cat("Original: ")
time_orig <- benchmark_npl(data_small_std, fns_orig$npl_estimator, config, n_runs = 3)
cat(sprintf("%.2f sec (sd=%.2f)\n", time_orig$mean, time_orig$sd))

cat("Optimized: ")
time_opt <- benchmark_npl(data_small_std, fns_opt$npl_estimator, config, n_runs = 3)
cat(sprintf("%.2f sec (sd=%.2f)\n", time_opt$mean, time_opt$sd))

speedup <- time_orig$mean / time_opt$mean
cat(sprintf("Speedup: %.2fx\n", speedup))

results_table <- rbind(results_table, data.table(
  Component = "Standard NPL",
  Dataset = "Small (100 fac, 200 periods)",
  Original_Mean = time_orig$mean,
  Original_SD = time_orig$sd,
  Optimized_Mean = time_opt$mean,
  Optimized_SD = time_opt$sd,
  Speedup = speedup
))

# --- STANDARD NPL (MEDIUM) ---
cat("\n--- Benchmarking Standard NPL (Medium) ---\n")
cat("Original: ")
time_orig <- benchmark_npl(data_med_std, fns_orig$npl_estimator, config, n_runs = 2)
cat(sprintf("%.2f sec (sd=%.2f)\n", time_orig$mean, time_orig$sd))

cat("Optimized: ")
time_opt <- benchmark_npl(data_med_std, fns_opt$npl_estimator, config, n_runs = 2)
cat(sprintf("%.2f sec (sd=%.2f)\n", time_opt$mean, time_opt$sd))

speedup <- time_orig$mean / time_opt$mean
cat(sprintf("Speedup: %.2fx\n", speedup))

results_table <- rbind(results_table, data.table(
  Component = "Standard NPL",
  Dataset = "Medium (300 fac, 500 periods)",
  Original_Mean = time_orig$mean,
  Original_SD = time_orig$sd,
  Optimized_Mean = time_opt$mean,
  Optimized_SD = time_opt$sd,
  Speedup = speedup
))

# --- NESTED NPL (SMALL) ---
cat("\n--- Benchmarking Nested NPL (Small) ---\n")
cat("Original: ")
time_orig <- benchmark_npl(data_small_nst, fns_orig$npl_estimator_nested, config, n_runs = 3)
cat(sprintf("%.2f sec (sd=%.2f)\n", time_orig$mean, time_orig$sd))

cat("Optimized: ")
time_opt <- benchmark_npl(data_small_nst, fns_opt$npl_estimator_nested, config, n_runs = 3)
cat(sprintf("%.2f sec (sd=%.2f)\n", time_opt$mean, time_opt$sd))

speedup <- time_orig$mean / time_opt$mean
cat(sprintf("Speedup: %.2fx\n", speedup))

results_table <- rbind(results_table, data.table(
  Component = "Nested NPL",
  Dataset = "Small (100 fac, 200 periods)",
  Original_Mean = time_orig$mean,
  Original_SD = time_orig$sd,
  Optimized_Mean = time_opt$mean,
  Optimized_SD = time_opt$sd,
  Speedup = speedup
))

# --- EM-NPL STANDARD (SMALL) ---
cat("\n--- Benchmarking EM-NPL Standard (Small) ---\n")
cat("Original: ")
time_orig <- benchmark_em(data_small_mix, fns_orig$em_npl_estimator, K = 2, 
                          config, max_em_iter = 15, n_runs = 2)
cat(sprintf("%.2f sec (sd=%.2f)\n", time_orig$mean, time_orig$sd))

cat("Optimized: ")
time_opt <- benchmark_em(data_small_mix, fns_opt$em_npl_estimator, K = 2,
                         config, max_em_iter = 15, n_runs = 2)
cat(sprintf("%.2f sec (sd=%.2f)\n", time_opt$mean, time_opt$sd))

speedup <- time_orig$mean / time_opt$mean
cat(sprintf("Speedup: %.2fx\n", speedup))

results_table <- rbind(results_table, data.table(
  Component = "EM-NPL Standard",
  Dataset = "Small (100 fac, 200 periods)",
  Original_Mean = time_orig$mean,
  Original_SD = time_orig$sd,
  Optimized_Mean = time_opt$mean,
  Optimized_SD = time_opt$sd,
  Speedup = speedup
))

# --- EM-NPL STANDARD (MEDIUM) ---
cat("\n--- Benchmarking EM-NPL Standard (Medium) ---\n")
cat("Original: ")
time_orig <- benchmark_em(data_med_mix, fns_orig$em_npl_estimator, K = 2,
                          config, max_em_iter = 15, n_runs = 1)
cat(sprintf("%.2f sec\n", time_orig$mean))

cat("Optimized: ")
time_opt <- benchmark_em(data_med_mix, fns_opt$em_npl_estimator, K = 2,
                         config, max_em_iter = 15, n_runs = 1)
cat(sprintf("%.2f sec\n", time_opt$mean))

speedup <- time_orig$mean / time_opt$mean
cat(sprintf("Speedup: %.2fx\n", speedup))

results_table <- rbind(results_table, data.table(
  Component = "EM-NPL Standard",
  Dataset = "Medium (300 fac, 500 periods)",
  Original_Mean = time_orig$mean,
  Original_SD = 0,
  Optimized_Mean = time_opt$mean,
  Optimized_SD = 0,
  Speedup = speedup
))

# ==============================================================================
# RESULTS SUMMARY
# ==============================================================================

cat("\n========================================\n")
cat("BENCHMARK RESULTS\n")
cat("========================================\n\n")

print(results_table)

# Calculate aggregate statistics
avg_speedup <- mean(results_table$Speedup)
min_speedup <- min(results_table$Speedup)
max_speedup <- max(results_table$Speedup)

cat("\n--- Summary Statistics ---\n")
cat(sprintf("Average speedup: %.2fx\n", avg_speedup))
cat(sprintf("Min speedup: %.2fx (%s)\n", min_speedup, 
            results_table[Speedup == min_speedup, Component]))
cat(sprintf("Max speedup: %.2fx (%s)\n", max_speedup,
            results_table[Speedup == max_speedup, Component]))

# Calculate time saved for full MC run
time_saved_per_rep <- sum(results_table[Dataset == "Medium (300 fac, 500 periods)", 
                                        Original_Mean - Optimized_Mean])
cat(sprintf("\nTime saved per MC replication (4 models): %.1f sec (%.1f min)\n",
            time_saved_per_rep, time_saved_per_rep / 60))

mc_reps <- 100
total_time_saved <- time_saved_per_rep * mc_reps
cat(sprintf("Projected savings for %d replications: %.1f hours\n",
            mc_reps, total_time_saved / 3600))

# Save results
fwrite(results_table, here("Output", "benchmark_results.csv"))
cat("\n✓ Results saved to: Output/benchmark_results.csv\n")

# Generate markdown table
md_lines <- c(
  "# Performance Benchmark Results",
  "",
  sprintf("Benchmark Date: %s", Sys.time()),
  "",
  "## Timing Comparison",
  "",
  "| Component | Dataset | Original (s) | Optimized (s) | Speedup |",
  "|-----------|---------|--------------|---------------|---------|"
)

for (i in 1:nrow(results_table)) {
  row <- results_table[i, ]
  md_lines <- c(md_lines,
                sprintf("| %s | %s | %.2f ± %.2f | %.2f ± %.2f | **%.2fx** |",
                        row$Component, row$Dataset,
                        row$Original_Mean, row$Original_SD,
                        row$Optimized_Mean, row$Optimized_SD,
                        row$Speedup))
}

md_lines <- c(md_lines,
              "",
              "## Summary",
              "",
              sprintf("- Average speedup: **%.2fx**", avg_speedup),
              sprintf("- Min speedup: %.2fx", min_speedup),
              sprintf("- Max speedup: %.2fx", max_speedup),
              "",
              sprintf("### Monte Carlo Savings (4 models, medium dataset)"),
              sprintf("- Time saved per replication: %.1f minutes", time_saved_per_rep / 60),
              sprintf("- Projected savings for %d replications: **%.1f hours**", mc_reps, total_time_saved / 3600),
              "",
              "## Optimizations Implemented",
              "- Analytical gradients for L-BFGS-B",
              "- Warm-starting in NPL and EM iterations",
              "- Adaptive convergence detection",
              "- Rcpp acceleration (E-step, aggregation)",
              "- Memoization/caching for utilities",
              "- Enhanced numerical stability"
)

writeLines(md_lines, here("Output", "benchmark_results.md"))
cat("✓ Markdown report saved to: Output/benchmark_results.md\n")

cat("\n========================================\n")
cat("BENCHMARK COMPLETE\n")
cat("========================================\n\n")
