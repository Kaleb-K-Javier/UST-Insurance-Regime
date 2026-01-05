#!/usr/bin/env Rscript
# ==============================================================================
# mc_master_REFACTORED.r - MODULAR MONTE CARLO VALIDATION FRAMEWORK
# ==============================================================================
# ARCHITECTURE:
#   1. Environment Setup: Dependencies, paths, function loading
#   2. Unit Testing: Parameter recovery validation on test datasets
#   3. Simulation Encapsulation: Scalable MC wrapper with profiling
#
# IMPROVEMENTS OVER ORIGINAL:
#   - Strict modularity: testing separated from production simulation
#   - Automated dependency verification with informative error messages
#   - Unit tests validate estimator functionality before expensive MC runs
#   - Performance baseline established via single-pass profiling
#   - Checkpoint/resume logic externalized to utility functions
#   - Function discovery automated via namespace inspection
# ==============================================================================

# ==============================================================================
# SECTION 1: ENVIRONMENT SETUP
# ==============================================================================

# 1.1 CLEAN WORKSPACE
rm(list = ls()); gc(verbose = FALSE)

# 1.2 CRITICAL PATHS
REPO_ROOT <- here::here()
CODE_DIR <- file.path(REPO_ROOT, "Code", "Public_to_Private")
OUTPUT_DIR <- file.path(REPO_ROOT, "Output", "Monte_Carlo_REFACTORED")
CHECKPOINT_DIR <- file.path(OUTPUT_DIR, "checkpoints")
PROFILING_DIR <- file.path(OUTPUT_DIR, "profiling")
UNIT_TEST_DIR <- file.path(OUTPUT_DIR, "unit_tests")


# 1.3 ENSURE DIRECTORIES EXIST
for (dir_path in c(OUTPUT_DIR, CHECKPOINT_DIR, PROFILING_DIR, UNIT_TEST_DIR)) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat(sprintf("Created directory: %s\n", dir_path))
  }
}

# 1.4 LOAD CORE LIBRARIES WITH VERSION CHECKING
required_packages <- c(
  "data.table" = "1.14.0",
  "Matrix" = "1.4.0",
  "here" = "1.0.0",
  "parallel" = NA,
  "foreach" = "1.5.0",
  "doParallel" = "1.0.16",
  "progressr" = "0.10.0",
  "Rcpp" = "1.0.0"
)

cat("\n=== LOADING DEPENDENCIES ===\n")
for (pkg_name in names(required_packages)) {
  min_version <- required_packages[pkg_name]
  
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    stop(sprintf("FATAL: Package '%s' not installed. Run: install.packages('%s')", 
                 pkg_name, pkg_name))
  }
  
  if (!is.na(min_version)) {
    pkg_version <- as.character(packageVersion(pkg_name))
    if (compareVersion(pkg_version, min_version) < 0) {
      warning(sprintf("Package '%s' version %s < required %s. Update recommended.", 
                      pkg_name, pkg_version, min_version))
    }
  }
  
  library(pkg_name, character.only = TRUE)
  cat(sprintf("  ✓ %s [v%s]\n", pkg_name, packageVersion(pkg_name)))
}

# 1.5 SOURCE ESTIMATION LIBRARIES
estimator_path <- file.path(CODE_DIR, "improved_estimator_OPTIMIZED.r")
generator_path <- file.path(CODE_DIR, "improved_generator_OPTIMIZED.r")

if (!file.exists(estimator_path)) {
  stop(sprintf("FATAL: Estimator not found at %s", estimator_path))
}
if (!file.exists(generator_path)) {
  stop(sprintf("FATAL: Generator not found at %s", generator_path))
}

cat("\n=== SOURCING CORE FUNCTIONS ===\n")
source(estimator_path)
cat("  ✓ Estimator functions loaded\n")

# Generator sources estimator, so only source generator
source(generator_path)
cat("  ✓ Generator functions loaded (includes estimator)\n")

# 1.6 ATTEMPT RCPP COMPILATION
cpp_path <- here('Code','Public_to_Private', "structural_estimation.cpp")
if (file.exists(cpp_path)) {
  # POTENTIAL FAILURE: Rcpp compilation may fail if:
  #   - No C++ compiler installed (need Rtools on Windows, gcc on Linux/Mac)
  #   - RcppArmadillo not installed: install.packages("RcppArmadillo")
  #   - Incompatible compiler version
  # If compilation fails, comment out the next line and set USE_RCPP <- FALSE
  # The code will run correctly but ~10x slower using R fallback
  
  Rcpp::sourceCpp(cpp_path)
  cat("  ✓ Rcpp acceleration loaded\n")
  USE_RCPP <- TRUE
} else {
  warning(sprintf("Rcpp file not found at %s. Using R fallback.", cpp_path))
  USE_RCPP <- FALSE
}

# 1.7 DISCOVER EXPORTED FUNCTIONS (AUTOMATED)
cat("\n=== FUNCTION DISCOVERY ===\n")

# Get all functions from global environment after sourcing
all_fns <- ls(envir = .GlobalEnv)
fn_objects <- mget(all_fns, envir = .GlobalEnv)
fn_names <- all_fns[sapply(fn_objects, is.function)]

# Categorize by naming convention
estimator_fns <- grep("^(npl|em_npl|compute|invert|create_estimation|calculate_flow|e_step|aggregate_weighted|classify|rowLogSumExp|logSumExp)", 
                      fn_names, value = TRUE)
generator_fns <- grep("^(generate|solve_equilibrium|set_|build_transition|create_state_key|create_state_space)", 
                      fn_names, value = TRUE)
helper_fns <- setdiff(fn_names, c(estimator_fns, generator_fns))

cat(sprintf("  Estimator functions: %d\n", length(estimator_fns)))
cat(sprintf("  Generator functions: %d\n", length(generator_fns)))
cat(sprintf("  Helper functions: %d\n", length(helper_fns)))
cat(sprintf("  Total exported: %d\n", length(fn_names)))
# 2.3 UNIT TEST EXECUTOR



run_unit_test <- function(model_name, params, config, verbose = TRUE) {
  if (verbose) {
    cat("\n", rep("-", 60), "\n", sep = "")
    cat(sprintf("UNIT TEST: %s\n", toupper(model_name)))
    cat(rep("-", 60), "\n", sep = "")
  }
  
  test_start <- proc.time()
  
  # POTENTIAL FAILURE: Data generation may fail if:
  #   - Invalid parameter values (e.g., negative phi/kappa)
  #   - K/phi_mix/kappa_mix mismatch for mixture models
  #   - Policy iteration doesn't converge
  # If error occurs, check function arguments and parameter bounds
  
  # Generate synthetic data
    sim_args <- list(
      N_facilities = params$N,
      T_periods = params$T,
      model_type = model_name,
      seed = 99999,
      use_rcpp = USE_RCPP
    )
    
    if (model_name == "standard") {
      sim_args$phi_true <- params$phi_true
      sim_args$kappa_true <- params$kappa_true
      true_theta <- c(params$phi_true, params$kappa_true)
      
    } else if (model_name == "nested") {
      sim_args$phi_true <- params$phi_true
      sim_args$kappa_true <- params$kappa_true
      sim_args$sigma1_true <- params$sigma1_true
      true_theta <- c(params$phi_true, params$kappa_true, params$sigma1_true)
      
    } else if (model_name == "mix_standard") {
      sim_args$K <- params$K
      sim_args$phi_mix <- params$phi_mix
      sim_args$kappa_mix <- params$kappa_mix
      sim_args$pi_weights <- params$pi_weights
      sim_args$type_profit_mult <- params$type_profit_mult
      true_theta <- c(params$phi_mix[1], params$kappa_mix[1])
      
    } else if (model_name == "mix_nested") {
      sim_args$K <- params$K
      sim_args$phi_mix <- params$phi_mix
      sim_args$kappa_mix <- params$kappa_mix
      sim_args$pi_weights <- params$pi_weights
      sim_args$sigma1_true <- params$sigma1_true
      sim_args$type_profit_mult <- params$type_profit_mult
      true_theta <- c(params$phi_mix[1], params$kappa_mix[1], params$sigma1_true)
    }
    
    sim <- do.call(generate_production_data, sim_args)
    
    if (verbose) {
      action_props <- prop.table(table(sim$panel$action))
      cat(sprintf("  Data generated: %d facilities, %d periods\n", 
                  params$N, params$T))
      cat(sprintf("  Action shares: maintain=%.3f exit=%.3f retrofit=%.3f\n",
                  action_props["maintain"], action_props["exit"], action_props["retrofit"]))
    }
    
    # Run estimation
    est_start <- proc.time()
    
    if (model_name == "standard") {
      cache <- create_estimation_cache(sim$states, sim$premiums, sim$hazards,
                                       sim$losses, sim$transitions, config)
      P_init <- compute_empirical_ccps(sim$counts_vec, cache)
      
      est <- npl_estimator(sim$counts_vec, sim$states, sim$premiums, sim$hazards,
                          sim$losses, sim$transitions, config, P_init = P_init,
                          verbose = FALSE)
      est_theta <- est$theta_hat
      
    } else if (model_name == "nested") {
      cache <- create_estimation_cache(sim$states, sim$premiums, sim$hazards,
                                       sim$losses, sim$transitions, config)
      P_init <- compute_empirical_ccps(sim$counts_vec, cache)
      
      est <- npl_estimator_nested(sim$counts_vec, sim$states, sim$premiums,
                                 sim$hazards, sim$losses, sim$transitions, config,
                                 P_init = P_init, verbose = FALSE)
      est_theta <- c(est$theta_hat, est$sigma1_hat)
      
    } else if (model_name == "mix_standard") {
      est <- em_npl_estimator(sim$panel, K = params$K, sim$states, sim$premiums,
                             sim$hazards, sim$losses, sim$transitions, config,
                             type_profit_mult = params$type_profit_mult,
                             max_em_iter = 20, verbose = FALSE)
      est_theta <- est$theta_list[[1]]
      
    } else if (model_name == "mix_nested") {
      est <- em_npl_estimator_nested(sim$panel, K = params$K, sim$states,
                                    sim$premiums, sim$hazards, sim$losses,
                                    sim$transitions, config,
                                    type_profit_mult = params$type_profit_mult,
                                    max_em_iter = 20, verbose = FALSE)
      est_theta <- c(est$theta_list[[1]], sigma1 = est$sigma1_hat)
    }
    
    est_time <- (proc.time() - est_start)[3]
    total_time <- (proc.time() - test_start)[3]
    
    # Compute recovery metrics
    abs_error <- abs(est_theta - true_theta)
    pct_error <- 100 * abs_error / abs(true_theta)
    
    if (verbose) {
      cat(sprintf("\n  Estimation completed in %.2f sec\n", est_time))
      cat(sprintf("  Converged: %s\n", est$converged))
      cat("\n  PARAMETER RECOVERY:\n")
      
      param_names <- if (model_name %in% c("standard", "mix_standard")) {
        c("phi", "kappa")
      } else {
        c("phi", "kappa", "sigma1")
      }
      
      for (i in seq_along(true_theta)) {
        cat(sprintf("    %-8s True=%-7.3f  Est=%-7.3f  AbsErr=%-7.4f  PctErr=%-6.2f%%\n",
                    paste0(param_names[i], ":"), true_theta[i], est_theta[i],
                    abs_error[i], pct_error[i]))
      }
    }
    # For mix models, check each type
if (grepl("mix", model_name)) {
  cat("  TYPE-SPECIFIC RECOVERY:\n")
  for (k in 1:params$K) {
    cat(sprintf("  Type %d:\n", k))
    cat(sprintf("    phi:     True=%.3f    Est=%.3f    AbsErr=%.4f   PctErr=%.2f %%\n",
                params$phi_mix[k], est$theta_list[[k]][1],
                abs(est$theta_list[[k]][1] - params$phi_mix[k]),
                100 * abs(est$theta_list[[k]][1] - params$phi_mix[k]) / params$phi_mix[k]))
    cat(sprintf("    kappa:   True=%.3f   Est=%.3f   AbsErr=%.4f   PctErr=%.2f %%\n",
                params$kappa_mix[k], est$theta_list[[k]][2],
                abs(est$theta_list[[k]][2] - params$kappa_mix[k]),
                100 * abs(est$theta_list[[k]][2] - params$kappa_mix[k]) / params$kappa_mix[k]))
  }
}
    # Test passed if all parameters recovered within 15% (generous for single run)
    test_passed <- all(pct_error < 15)
    
    result <- list(
      model = model_name,
      passed = test_passed,
      converged = est$converged,
      true_theta = true_theta,
      est_theta = est_theta,
      abs_error = abs_error,
      pct_error = pct_error,
      est_time_sec = est_time,
      total_time_sec = total_time,
      timestamp = Sys.time()
    )
    
    # Save test results
    saveRDS(result, file.path(UNIT_TEST_DIR, sprintf("test_%s.rds", model_name)))
    
    if (verbose) {
      status_symbol <- if (test_passed) "✓ PASS" else "✗ FAIL"
      cat(sprintf("\n  %s (%.1f sec total)\n", status_symbol, total_time))
    }
    
    return(result)
  
  # NOTE: No error handler - errors will propagate to caller for inspection
  # This allows interactive debugging of failure points
}


cat("\n", rep("=", 70), "\n", sep = "")
cat("SECTION 3: MONTE CARLO SIMULATION FRAMEWORK\n")
cat(rep("=", 70), "\n", sep = "")

# 3.1 CHECKPOINT UTILITIES (EXTERNALIZED)
checkpoint_ops <- list(
  get_path = function(rep_id, model_name) {
    file.path(CHECKPOINT_DIR, sprintf("rep%03d_%s.rds", rep_id, model_name))
  },
  
  exists = function(rep_id, model_name) {
    file.exists(checkpoint_ops$get_path(rep_id, model_name))
  },
  
  save = function(rep_id, model_name, result) {
    path <- checkpoint_ops$get_path(rep_id, model_name)
    saveRDS(result, path)
    invisible(path)
  },
  
  load = function(rep_id, model_name) {
    if (checkpoint_ops$exists(rep_id, model_name)) {
      path <- checkpoint_ops$get_path(rep_id, model_name)
      return(readRDS(path))
    }
    return(NULL)
  }
)

# 3.2 SINGLE MODEL ESTIMATION WRAPPER
estimate_single_model <- function(rep_id, model_name, params, config, 
                                 checkpoint_ops, verbose = FALSE) {
  
  # Check for existing checkpoint
  checkpoint <- checkpoint_ops$load(rep_id, model_name)
  if (!is.null(checkpoint)) {
    if (verbose) cat(sprintf("  [%s] Loaded from checkpoint\n", toupper(model_name)))
    return(checkpoint)
  }
  
  if (verbose) cat(sprintf("  [%s] Starting estimation...\n", toupper(model_name)))
  
  t_start <- proc.time()
  
  # Generate data
  sim_args <- list(
    N_facilities = params$N,
    T_periods = params$T,
    model_type = model_name,
    seed = params$seed_base + rep_id,
    use_rcpp = USE_RCPP
  )
  
  if (model_name == "standard") {
    sim_args$phi_true <- params$phi_true
    sim_args$kappa_true <- params$kappa_true
    true_vec <- c(params$phi_true, params$kappa_true)
    
  } else if (model_name == "nested") {
    sim_args$phi_true <- params$phi_true
    sim_args$kappa_true <- params$kappa_true
    sim_args$sigma1_true <- params$sigma1_true
    true_vec <- c(params$phi_true, params$kappa_true, params$sigma1_true)
    
  } else if (model_name == "mix_standard") {
    sim_args$K <- params$K
    sim_args$phi_mix <- params$phi_mix
    sim_args$kappa_mix <- params$kappa_mix
    sim_args$pi_weights <- params$pi_weights
    sim_args$type_profit_mult <- params$type_profit_mult
    true_vec <- c(params$phi_mix[1], params$kappa_mix[1])
    
  } else if (model_name == "mix_nested") {
    sim_args$K <- params$K
    sim_args$phi_mix <- params$phi_mix
    sim_args$kappa_mix <- params$kappa_mix
    sim_args$pi_weights <- params$pi_weights
    sim_args$sigma1_true <- params$sigma1_true
    sim_args$type_profit_mult <- params$type_profit_mult
    true_vec <- c(params$phi_mix[1], params$kappa_mix[1], params$sigma1_true)
  }
  
  sim <- do.call(generate_production_data, sim_args)
  
  # Estimate
  if (model_name == "standard") {
    cache <- create_estimation_cache(sim$states, sim$premiums, sim$hazards,
                                    sim$losses, sim$transitions, config)
    P_init <- compute_empirical_ccps(sim$counts_vec, cache)
    
    est <- npl_estimator(sim$counts_vec, sim$states, sim$premiums, sim$hazards,
                        sim$losses, sim$transitions, config, P_init = P_init,
                        verbose = FALSE)
    est_vec <- est$theta_hat
    
  } else if (model_name == "nested") {
    cache <- create_estimation_cache(sim$states, sim$premiums, sim$hazards,
                                    sim$losses, sim$transitions, config)
    P_init <- compute_empirical_ccps(sim$counts_vec, cache)
    
    est <- npl_estimator_nested(sim$counts_vec, sim$states, sim$premiums,
                               sim$hazards, sim$losses, sim$transitions, config,
                               P_init = P_init, verbose = FALSE)
    est_vec <- c(est$theta_hat, est$sigma1_hat)
    
  } else if (model_name == "mix_standard") {
    est <- em_npl_estimator(sim$panel, K = params$K, sim$states, sim$premiums,
                           sim$hazards, sim$losses, sim$transitions, config,
                           type_profit_mult = params$type_profit_mult,K_npl = 100,
                           max_em_iter = params$max_em_iter, verbose = FALSE)
    est_vec <- est$theta_list[[1]]
    
  } else if (model_name == "mix_nested") {
    est <- em_npl_estimator_nested(sim$panel, K = params$K, sim$states,
                                  sim$premiums, sim$hazards, sim$losses,
                                  sim$transitions, config,K_npl = 100,
                                  type_profit_mult = params$type_profit_mult,
                                  max_em_iter = params$max_em_iter, verbose = FALSE)
    est_vec <- c(est$theta_list[[1]], sigma1 = est$sigma1_hat)
  }
  
  elapsed <- (proc.time() - t_start)[3]
  
  result <- list(
    est = est_vec,
    true = true_vec,
    converged = est$converged,
    time_sec = elapsed,
    model = model_name,
    rep_id = rep_id
  )
  
  # Save checkpoint
  checkpoint_ops$save(rep_id, model_name, result)
  
  if (verbose) {
    cat(sprintf("  [%s] Complete in %.1f sec | Converged: %s\n",
                toupper(model_name), elapsed, est$converged))
  }
  
  return(result)
}



# 3.3 FULL REPLICATION WRAPPER
run_full_replication <- function(rep_id, param_config, est_config, 
                                checkpoint_ops, verbose = FALSE) {
  
  if (verbose) cat(sprintf("\n=== REPLICATION %d ===\n", rep_id))
  
  results <- list(rep_id = rep_id)
  
  for (model_name in names(param_config)) {
    # POTENTIAL FAILURE: Each model estimation may fail due to:
    #   - Data generation issues
    #   - Optimization convergence failures
    #   - Numerical instability
    # Errors will propagate to foreach loop and be caught there
    
    results[[model_name]] <- estimate_single_model(
      rep_id, model_name, param_config[[model_name]],
      est_config, checkpoint_ops, verbose
    )
  }
  
  return(results)
}



# ==============================================================================
# SECTION 2: UNIT TESTING & BENCHMARKING
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("SECTION 2: UNIT TESTING & PARAMETER RECOVERY VALIDATION\n")
cat(rep("=", 70), "\n", sep = "")

# 2.1 DEFINE GROUND TRUTH PARAMETERS
# CALIBRATED to generate sufficient action variation for parameter recovery
THETA_VALIDATION <- list(
  standard = list(
    phi_true = 0.5,      # Monthly: $0.5 retrofit cost
    kappa_true = 69,     # Monthly: $69 exit value
    N = 1000,
    T = 500
  ),
  nested = list(
  phi_true    = .5,
  kappa_true  = 91,
  sigma1_true = 1.5,
    N = 1000,
    T = 500
  ),
  mix_standard = list(
    K = 2,
    phi_mix = c(0.5, 1.5),      # Monthly: Type 1 = $0.5, Type 2 = $1.5
    kappa_mix = c(69, 75),      # Monthly: Type 1 = $69, Type 2 = $75
    pi_weights = c(0.6, 0.4),
    type_profit_mult = c(1.0, 1.0),
    N = 1000,
    T = 500
  ),
  mix_nested = list(
    K = 2,
    phi_mix = c(0.4, 0.75),     # Monthly: Type 1 = $0.4, Type 2 = $0.75
    kappa_mix = c(95, 82),      # Monthly: Type 1 = $95, Type 2 = $82
    pi_weights = c(0.6, 0.4),
    sigma1_true = 1.5,          # Nest parameter
    type_profit_mult = c(1.0, 1.0),
    N = 1000,
    T = 500
  )
)

# 2.2 ESTIMATION CONFIGURATION
EST_CONFIG <- create_estimation_config(beta = 0.9957, sigma2 = 0.3)



#2.3


# 2.4 RUN ALL UNIT TESTS
cat("\nExecuting unit tests on all model types...\n")

unit_test_results <- list()
for (model_name in names(THETA_VALIDATION)) {
  unit_test_results[[model_name]] <- run_unit_test(
    model_name,
    THETA_VALIDATION[[model_name]],
    EST_CONFIG,
    verbose = TRUE
  )
}

# 2.5 SUMMARIZE UNIT TEST RESULTS
cat("\n", rep("=", 70), "\n", sep = "")
cat("UNIT TEST SUMMARY\n")
cat(rep("=", 70), "\n", sep = "")

all_passed <- all(sapply(unit_test_results, function(x) x$passed))

for (model_name in names(unit_test_results)) {
  result <- unit_test_results[[model_name]]
  status <- if (result$passed) "✓ PASS" else "✗ FAIL"
  cat(sprintf("  %-15s %s  (%.1f sec)\n", 
              toupper(model_name), status, result$total_time_sec))
}

cat(sprintf("\nOverall status: %s\n", if (all_passed) "✓ ALL TESTS PASSED" else "✗ SOME TESTS FAILED"))

if (!all_passed) {
  cat("\n⚠ WARNING: Not all unit tests passed. Review estimation logic before MC run.\n")
  cat("Continue? (y/n): ")
  user_input <- readline()
  if (tolower(user_input) != "y") {
    stop("Monte Carlo simulation aborted by user.")
  }
}

# 2.6 PERFORMANCE BASELINE
baseline_perf <- data.table(
  model = names(unit_test_results),
  est_time_sec = sapply(unit_test_results, function(x) x$est_time_sec),
  total_time_sec = sapply(unit_test_results, function(x) x$total_time_sec)
)

cat("\nPERFORMANCE BASELINE (single replication):\n")
print(baseline_perf)

fwrite(baseline_perf, file.path(PROFILING_DIR, "baseline_performance.csv"))
# Compile C++ on Master (to ensure it works locally first)
if(file.exists(cpp_path)) {
  Rcpp::sourceCpp(cpp_path)
} else {
  stop("FATAL: structural_estimation.cpp not found.")
}
#!/usr/bin/env Rscript
# ==============================================================================
# mc_standard_final_FIXED.r
# ==============================================================================
# PURPOSE: Monte Carlo simulation for Standard Model + Full Diagnostic Suite.
# FIXES APPLIED:
#   1. Explicit CPP_PATH definition and worker export.
#   2. Local worker compilation to fix null pointer errors.
#   3. Robust parameter sweep logic (safe loops + error catching).
#   4. Re-compilation on Master before diagnostics to prevent environment errors.
# ==============================================================================

# 1. ENVIRONMENT & PATHS (MUST RUN FIRST)
# ------------------------------------------------------------------------------
rm(list = ls()); gc(verbose = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(here)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(Rcpp)
  library(ggplot2)
})

# DEFINITIONS - CRITICAL STEP
# Ensure these exist in the Global Environment BEFORE cluster setup
CODE_DIR <- here("Code", "Public_to_Private")
CPP_PATH <- file.path(CODE_DIR, "structural_estimation.cpp")

# Validation
if (!file.exists(CPP_PATH)) stop(paste("FATAL: C++ file not found at:", CPP_PATH))
if (!file.exists(file.path(CODE_DIR, "improved_generator_OPTIMIZED.r"))) stop("FATAL: Generator R script not found.")

# Load Core Functions Locally (Master Node)
source(file.path(CODE_DIR, "improved_estimator_OPTIMIZED.r"))
source(file.path(CODE_DIR, "improved_generator_OPTIMIZED.r"))
if(file.exists(CPP_PATH)) Rcpp::sourceCpp(CPP_PATH)

# 2. CONFIGURATION
# ------------------------------------------------------------------------------
MC_CONFIG <- list(
  N_SIM = 50,
  N_CORES = detectCores(logical=FALSE) - 1, 
  N_FACILITIES = 1000,
  T_PERIODS = 500,
  SEED_BASE = 1000
)

# Standard Model Parameters (Phi=0.5, Kappa=69)
PARAMS <- list(
  phi_true = 0.5,
  kappa_true = 69,
  beta = 0.9957,
  sigma2 = 0.3
)

# 3. CLUSTER SETUP & WORKER INITIALIZATION
# ------------------------------------------------------------------------------
cat(sprintf("\n=== LAUNCHING MC: %d Sims on %d Cores ===\n", MC_CONFIG$N_SIM, MC_CONFIG$N_CORES))

cl <- makeCluster(MC_CONFIG$N_CORES)
registerDoParallel(cl)

# A. Export PATH variables to all workers explicitly
clusterExport(cl, c("CODE_DIR", "CPP_PATH"))

# B. Initialize Workers: Compile C++ LOCALLY
clusterEvalQ(cl, {
  library(data.table)
  library(Matrix)
  library(Rcpp)
  
  # 1. Source R Functions
  source(file.path(CODE_DIR, "improved_estimator_OPTIMIZED.r"))
  source(file.path(CODE_DIR, "improved_generator_OPTIMIZED.r"))
  
  # 2. Compile C++ locally on this worker
  Rcpp::sourceCpp(CPP_PATH)
})

# C. Export Data Configs
clusterExport(cl, c("MC_CONFIG", "PARAMS"))

# 4. EXECUTION
# ------------------------------------------------------------------------------
results <- foreach(i = 1:MC_CONFIG$N_SIM, .combine = rbind, .packages = c("data.table", "Matrix")) %dopar% {
  
  # --- Step A: Data Generation ---
  sim_args <- list(
    N_facilities = MC_CONFIG$N_FACILITIES,
    T_periods = MC_CONFIG$T_PERIODS,
    model_type = "standard",
    phi_true = PARAMS$phi_true,
    kappa_true = PARAMS$kappa_true,
    seed = MC_CONFIG$SEED_BASE + i,
    use_rcpp = TRUE
  )
  
  # Generate Data (Uses worker's local C++ compilation)
  sim <- do.call(generate_production_data, sim_args)
  
  # --- Step B: Estimation ---
  est_config <- create_estimation_config(beta = PARAMS$beta, sigma2 = PARAMS$sigma2)
  
  # Create Cache
  cache <- create_estimation_cache(sim$states, sim$premiums, sim$hazards,
                                   sim$losses, sim$transitions, est_config)
  
  # Initial CCPs
  P_init <- compute_empirical_ccps(sim$counts_vec, cache)
  
  # Run Optimization
  t_start <- proc.time()
  est <- npl_estimator(
    counts_vec = sim$counts_vec,
    states = sim$states,
    premiums = sim$premiums,
    hazards = sim$hazards,
    losses = sim$losses,
    transitions = sim$transitions,
    config = est_config,
    P_init = P_init,
    verbose = FALSE
  )
  elapsed <- (proc.time() - t_start)[3]
  
  # --- Step C: Return Result ---
  data.table(
    rep_id = i,
    phi_est = est$theta_hat[1],
    kappa_est = est$theta_hat[2],
    phi_true = PARAMS$phi_true,
    kappa_true = PARAMS$kappa_true,
    converged = est$converged,
    time_sec = elapsed
  )
}

stopCluster(cl)

# 5. ANALYSIS
# ------------------------------------------------------------------------------
cat("\n=== RESULTS ===\n")
if (exists("results") && nrow(results) > 0) {
  valid <- results[converged == TRUE]
  
  if (nrow(valid) > 0) {
    stats <- valid[, .(
      "True" = c(mean(phi_true), mean(kappa_true)),
      "Est"  = c(mean(phi_est), mean(kappa_est)),
      "Bias" = c(mean(phi_est) - mean(phi_true), mean(kappa_est) - mean(kappa_true)),
      "RMSE" = c(sqrt(mean((phi_est - phi_true)^2)), sqrt(mean((kappa_est - kappa_true)^2)))
    )]
    row.names(stats) <- c("phi", "kappa")
    print(stats, digits=4)
    cat(sprintf("\nConvergence: %.1f%%\n", 100 * nrow(valid)/nrow(results)))
  } else {
    cat("No converged replications.\n")
  }
} else {
  cat("Simulation failed to produce results.\n")
}

# ==============================================================================
# DIAGNOSTICS PREP: RE-ESTABLISH MASTER ENVIRONMENT
# ==============================================================================
cat("\n=== PREPARING DIAGNOSTICS ===\n")
# Ensure Master session has compiled C++ functions for local runs
if(file.exists(CPP_PATH)) {
  Rcpp::sourceCpp(CPP_PATH)
  cat("✓ Master session C++ re-compiled.\n")
}

# 1. FAILURE FORENSICS
# ------------------------------------------------------------------------------
analyze_failures <- function(results, config) {
  cat("\n=== DIAGNOSTIC 1: FAILURE FORENSICS ===\n")
  
  phi_bounds <- config$phi_bounds
  kappa_bounds <- config$kappa_bounds
  
  failed <- results[converged == FALSE]
  n_fail <- nrow(failed)
  
  if (n_fail == 0) {
    cat("✓ No failures to analyze.\n")
    return(NULL)
  }
  
  cat(sprintf("Analyzing %d non-converged runs:\n", n_fail))
  
  hit_phi_low  <- sum(abs(failed$phi_est - phi_bounds[1]) < 0.01)
  hit_phi_high <- sum(abs(failed$phi_est - phi_bounds[2]) < 0.01)
  hit_kap_low  <- sum(abs(failed$kappa_est - kappa_bounds[1]) < 0.01)
  hit_kap_high <- sum(abs(failed$kappa_est - kappa_bounds[2]) < 0.01)
  
  cat(sprintf("  - Phi Low/High Hits:   %d / %d\n", hit_phi_low, hit_phi_high))
  cat(sprintf("  - Kappa Low/High Hits: %d / %d\n", hit_kap_low, hit_kap_high))
  
  interior_fails <- n_fail - (hit_phi_low + hit_phi_high + hit_kap_low + hit_kap_high)
  cat(sprintf("  - Interior Oscillations: %d\n", max(0, interior_fails)))
}

if(exists("results")) analyze_failures(results, create_estimation_config())

# 2. ROBUST PARAMETER SWEEP FUNCTION
# ------------------------------------------------------------------------------
run_robust_sweep <- function() {
  cat("\n=== DIAGNOSTIC: ROBUST PARAMETER SENSITIVITY SWEEP ===\n")
  
  scenarios <- list(
    "Baseline"      = c(phi=0.5, kappa=69),
    "High Cost"     = c(phi=2.0, kappa=69),
    "Low Cost"      = c(phi=0.1, kappa=69),
    "High Scrap"    = c(phi=0.5, kappa=200),
    "Low Scrap"     = c(phi=0.5, kappa=10)
  )
  
  for(name in names(scenarios)) {
    p <- scenarios[[name]]
    cat(sprintf("Testing %-12s (phi=%.1f, kappa=%.1f)... ", name, p['phi'], p['kappa']))
    
    # Use standard matrix pre-allocation (Safe & Sequential)
    reps <- matrix(NA, nrow=5, ncol=3)
    
    for(r in 1:5) {
      tryCatch({
        # 1. Data Generation
        sim <- generate_production_data(
          N_facilities = 1000, 
          T_periods = 500, 
          model_type = "standard", 
          phi_true = p['phi'], 
          kappa_true = p['kappa'], 
          use_rcpp = TRUE
        )
        
        # 2. Estimation
        conf <- create_estimation_config(beta=0.9957, sigma2=0.3)
        cache <- create_estimation_cache(sim$states, sim$premiums, sim$hazards, 
                                         sim$losses, sim$transitions, conf)
        P_init <- compute_empirical_ccps(sim$counts_vec, cache)
        
        est <- npl_estimator(sim$counts_vec, sim$states, sim$premiums, sim$hazards, 
                             sim$losses, sim$transitions, conf, P_init=P_init, verbose=FALSE)
        
        # Store result
        reps[r, ] <- c(est$theta_hat[1], est$theta_hat[2], as.integer(est$converged))
        
      }, error = function(e) {
        # Leave row as NA or mark specific error code if desired
      })
    }
    
    # Robust filtering
    valid_idx <- which(reps[,3] == 1)
    valid <- reps[valid_idx, , drop=FALSE]
    
    if (nrow(valid) > 0) {
      avg_phi <- mean(valid[,1])
      avg_kap <- mean(valid[,2])
      
      bias_phi <- 100 * (avg_phi - p['phi']) / p['phi']
      bias_kap <- 100 * (avg_kap - p['kappa']) / p['kappa']
      conv_rate <- nrow(valid)/5
      
      cat(sprintf("Conv: %.0f%% | Bias: Phi %+.1f%%, Kap %+.1f%%\n", 
                  conv_rate*100, bias_phi, bias_kap))
    } else {
      cat("Conv: 0% (All Failed)\n")
    }
  }
}

# 3. LIKELIHOOD SURFACE SCAN (Visual Check)
# ------------------------------------------------------------------------------
scan_surface <- function() {
  cat("\n=== DIAGNOSTIC: LIKELIHOOD SURFACE SCAN ===\n")
  cat("Generating reference data...\n")
  
  sim <- generate_production_data(1000, 500, "standard", 
                                  phi_true=0.5, kappa_true=69, use_rcpp=TRUE)
  
  conf <- create_estimation_config(beta=0.9957, sigma2=0.3)
  cache <- create_estimation_cache(sim$states, sim$premiums, sim$hazards, 
                                   sim$losses, sim$transitions, conf)
  P_emp <- compute_empirical_ccps(sim$counts_vec, cache)
  
  # Grid
  phi_grid <- seq(0.1, 1.5, length.out = 15)
  kappa_grid <- seq(40, 100, length.out = 15)
  grid <- expand.grid(phi = phi_grid, kappa = kappa_grid)
  
  cat("Scanning grid...\n")
  vals <- numeric(nrow(grid))
  for(i in 1:nrow(grid)) {
    theta <- c(phi_tilde = grid$phi[i], kappa_tilde = grid$kappa[i])
    vals[i] <- -npl_likelihood_standard(theta, P_fixed = P_emp, 
                                        cache = cache, config = conf, 
                                        counts_vec = sim$counts_vec)
  }
  grid$ll <- vals
  
  # Plot
  p <- ggplot(grid, aes(x=phi, y=kappa, z=ll)) +
    geom_contour_filled(bins = 15) +
    geom_point(aes(x=0.5, y=69), color="red", shape=4, size=5, stroke=2) +
    labs(title = "NPL Objective Surface", x = "Phi", y = "Kappa") +
    theme_minimal()
  
  print(p)
  cat("✓ Surface plot generated.\n")
}

# EXECUTE
run_robust_sweep()
scan_surface()
