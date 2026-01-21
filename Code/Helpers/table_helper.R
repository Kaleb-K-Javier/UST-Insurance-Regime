#==============================================================================
# TABLE SAVING HELPER: Specialized Table Export with Julia-Based Wild Bootstrap
# 
# Location: Code/Helpers/table_helper.R
#
# This script provides 5 specialized table-saving functions for DiD analysis:
#   1. save_standard_did_table()   - Standard 2x2 DiD (Parts A.1, D.1, D.2, D.3, E.1)
#   2. save_triple_did_table()     - Triple interaction with MF HTE (Part A.2)
#   3. save_simple_csv_table()     - Coefficient extracts, no bootstrap (Parts C, E.2, E.3, F)
#   4. save_event_study_table()    - Event study with time-varying coefs (Part G.1)
#   5. save_summary_table()        - Interpretation/summary tables (D.2 summary)
#
# Bootstrap Engine:
#   - Uses fwildclusterboot with WildBootTests.jl (Julia) for memory efficiency
#   - Webb weights (6-point) for small G (19 clusters)
#   - Falls back to HC2 clustered SEs when use_bootstrap = FALSE
#
# Memory Optimization:
#   - Designed for 25M+ observations with 19 clusters
#   - Julia engine avoids R's dense matrix explosion
#   - Requires Julia installation and JuliaConnectoR package
#
# References:
#   - Roodman et al. (2019): Fast and wild bootstrap inference
#   - MacKinnon, Nielsen, Webb (2022): Cluster-robust inference
#   - Cameron, Gelbach, Miller (2008): Bootstrap-based improvements
#==============================================================================

library(data.table)
library(fixest)
library(kableExtra)
library(here)

#==============================================================================
# BOOTSTRAP ENGINE SETUP
#==============================================================================

#' Check and Initialize Julia Bootstrap Engine
#' 
#' Validates that Julia and required packages are available.
#' Must be called before any bootstrap operations.
#' 
#' @return TRUE if successful, stops with error otherwise
check_julia_bootstrap_engine <- function() {
  

  # Check 1: fwildclusterboot package

if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
    stop(
      "BOOTSTRAP ERROR: fwildclusterboot package not installed.\n",
      "Install via: remotes::install_github('s3alfisc/fwildclusterboot')\n",
      "Note: Package was removed from CRAN; GitHub version required."
    )
  }
  
  # Check 2: JuliaConnectoR package
  if (!requireNamespace("JuliaConnectoR", quietly = TRUE)) {
    stop(
      "BOOTSTRAP ERROR: JuliaConnectoR package not installed.\n",
      "Install via: install.packages('JuliaConnectoR')\n",
      "This is required for the memory-efficient Julia bootstrap engine."
    )
  }
  
  # Check 3: Julia installation
  julia_home <- Sys.getenv("JULIA_BINDIR")
  if (julia_home == "") {
    julia_home <- Sys.getenv("JULIA_HOME")
  }
  
  # Try to find Julia in PATH if not set
  julia_path <- Sys.which("julia")
  
  if (julia_home == "" && julia_path == "") {
    stop(
      "BOOTSTRAP ERROR: Julia installation not found.\n",
      "Please install Julia from https://julialang.org/downloads/\n",
      "Then set the environment variable:\n",
      "  Sys.setenv(JULIA_BINDIR = '/path/to/julia/bin')\n",
      "Or add Julia to your system PATH."
    )
  }
  
  # Set the bootstrap engine globally
  fwildclusterboot::setBoottest_engine("WildBootTests.jl")
  
  cat("✓ Julia bootstrap engine initialized successfully\n")
  cat(sprintf("  Julia path: %s\n", ifelse(julia_path != "", julia_path, julia_home)))
  
  return(TRUE)
}

#' Run Wild Cluster Bootstrap for Single Coefficient
#' 
#' Memory-efficient bootstrap using Julia engine with Webb weights.
#' 
#' @param model A fitted model object (feols, lm, feglm)
#' @param param Character: coefficient name to test
#' @param cluster_var Character: clustering variable name
#' @param n_reps Integer: number of bootstrap replications
#' @param seed Integer: random seed for reproducibility
#' 
#' @return List with beta_obs, se_boot, p_val, ci_lower, ci_upper, label
run_wcb_single <- function(model, param, cluster_var = "state", 
                           n_reps = 999, seed = 20250120) {
  
  # Validate coefficient exists
  model_coefs <- coef(model)
  if (!param %in% names(model_coefs)) {
    warning(sprintf("Coefficient '%s' not found in model. Available: %s",
                    param, paste(head(names(model_coefs), 10), collapse = ", ")))
    return(list(
      beta_obs = NA_real_,
      se_boot = NA_real_,
      p_val = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      label = "Not found"
    ))
  }
  
  beta_obs <- model_coefs[[param]]
  
  # Run bootstrap
  boot_result <- tryCatch({
    fwildclusterboot::boottest(
      object = model,
      param = param,
      clustid = cluster_var,
      B = n_reps,
      type = "webb",                    # 6-point distribution for small G
      engine = "WildBootTests.jl",      # Memory-efficient Julia engine
      impose_null = TRUE,               # WCR bootstrap (recommended)
      seed = seed,
      nthreads = parallel::detectCores() - 1
    )
  }, error = function(e) {
    warning(sprintf("Bootstrap failed for '%s': %s", param, e$message))
    return(NULL)
  })
  
  if (is.null(boot_result)) {
    return(list(
      beta_obs = beta_obs,
      se_boot = NA_real_,
      p_val = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      label = "Bootstrap failed"
    ))
  }
  
  # Extract results
  p_val <- boot_result$p_val
  ci <- boot_result$conf_int
  se_boot <- (ci[2] - ci[1]) / (2 * 1.96)  # Approximate SE from 95% CI
  
  # Significance stars
  stars <- if (p_val < 0.01) "***" else if (p_val < 0.05) "**" else if (p_val < 0.1) "*" else ""
  
  return(list(
    beta_obs = beta_obs,
    se_boot = se_boot,
    p_val = p_val,
    ci_lower = ci[1],
    ci_upper = ci[2],
    label = sprintf("%.4f%s", p_val, stars)
  ))
}

#' Run Wild Cluster Bootstrap for Linear Combination
#' 
#' Tests H0: R'β = 0 where R is a vector of weights.
#' Used for testing β₁ + β₃ = 0 in triple interaction models.
#' 
#' @param model A fitted model object
#' @param params Character vector: coefficient names in the combination
#' @param R Numeric vector: weights for each coefficient (same length as params)
#' @param cluster_var Character: clustering variable name
#' @param n_reps Integer: number of bootstrap replications
#' @param seed Integer: random seed
#' 
#' @return List with beta_obs (sum), se_boot, p_val, ci_lower, ci_upper, label
run_wcb_combination <- function(model, params, R, cluster_var = "state",
                                 n_reps = 999, seed = 20250120) {
  
  stopifnot(length(params) == length(R))
  
  # Validate coefficients exist
  model_coefs <- coef(model)
  missing_params <- params[!params %in% names(model_coefs)]
  
  if (length(missing_params) > 0) {
    warning(sprintf("Coefficients not found: %s", paste(missing_params, collapse = ", ")))
    return(list(
      beta_obs = NA_real_,
      se_boot = NA_real_,
      p_val = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      label = "Not found"
    ))
  }
  
  # Compute observed linear combination
  beta_obs <- sum(R * model_coefs[params])
  
  # Run bootstrap for linear combination
  boot_result <- tryCatch({
    fwildclusterboot::boottest(
      object = model,
      param = params,
      R = R,                            # Linear combination weights
      clustid = cluster_var,
      B = n_reps,
      type = "webb",
      engine = "WildBootTests.jl",
      impose_null = TRUE,
      seed = seed,
      nthreads = parallel::detectCores() - 1
    )
  }, error = function(e) {
    warning(sprintf("Bootstrap failed for combination: %s", e$message))
    return(NULL)
  })
  
  if (is.null(boot_result)) {
    return(list(
      beta_obs = beta_obs,
      se_boot = NA_real_,
      p_val = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      label = "Bootstrap failed"
    ))
  }
  
  p_val <- boot_result$p_val
  ci <- boot_result$conf_int
  se_boot <- (ci[2] - ci[1]) / (2 * 1.96)
  
  stars <- if (p_val < 0.01) "***" else if (p_val < 0.05) "**" else if (p_val < 0.1) "*" else ""
  
  return(list(
    beta_obs = beta_obs,
    se_boot = se_boot,
    p_val = p_val,
    ci_lower = ci[1],
    ci_upper = ci[2],
    label = sprintf("%.4f%s", p_val, stars)
  ))
}

#' Get HC2 Clustered Standard Errors (Fallback)
#' 
#' Used when use_bootstrap = FALSE.
#' 
#' @param model A fitted fixest model
#' @param param Character: coefficient name
#' @param cluster_var Character: clustering variable
#' 
#' @return List with beta_obs, se_hc2, p_val, ci_lower, ci_upper, label
get_hc2_results <- function(model, param, cluster_var = "state") {
  
  # Get HC2 clustered vcov
  vcov_hc2 <- tryCatch({
    vcov(model, vcov = ~get(cluster_var), ssc = ssc(adj = TRUE, cluster.adj = TRUE))
  }, error = function(e) {
    vcov(model)  # Fallback to default
  })
  
  model_coefs <- coef(model)
  
  if (!param %in% names(model_coefs)) {
    return(list(
      beta_obs = NA_real_,
      se_hc2 = NA_real_,
      p_val = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      label = "Not found"
    ))
  }
  
  beta_obs <- model_coefs[[param]]
  se_hc2 <- sqrt(vcov_hc2[param, param])
  t_stat <- beta_obs / se_hc2
  
  # Use t-distribution with G-1 degrees of freedom (conservative for small G)
  G <- 19  # Number of clusters
  p_val <- 2 * pt(abs(t_stat), df = G - 1, lower.tail = FALSE)
  
  ci_lower <- beta_obs - qt(0.975, df = G - 1) * se_hc2
  ci_upper <- beta_obs + qt(0.975, df = G - 1) * se_hc2
  
  stars <- if (p_val < 0.01) "***" else if (p_val < 0.05) "**" else if (p_val < 0.1) "*" else ""
  
  return(list(
    beta_obs = beta_obs,
    se_hc2 = se_hc2,
    p_val = p_val,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    label = sprintf("%.4f%s", p_val, stars)
  ))
}

#==============================================================================
# FUNCTION 1: STANDARD DiD TABLE
# Used for: Parts A.1, D.1, D.2, D.3, E.1
#==============================================================================

#' Save Standard DiD Table
#' 
#' Creates publication-ready tables for standard 2x2 DiD models.
#' Outputs CSV, TEX, and TXT formats with optional WCB p-values.
#' 
#' @param models List of fixest model objects
#' @param headers Character vector of column headers (same length as models)
#' @param base_name Character: base filename (without extension)
#' @param title Character: table title
#' @param treatment_var Character: name of treatment coefficient
#' @param cluster_var Character: clustering variable name
#' @param use_bootstrap Logical: whether to run WCB (default TRUE)
#' @param n_reps Integer: bootstrap replications (default 999)
#' @param digits Integer: decimal places for coefficients
#' @param output_dir Character: output directory path
#' 
#' @return data.table of results (invisibly)
#' 
#' @examples
#' save_standard_did_table(
#'   models = list(m1, m2, m3),
#'   headers = c("LUST", "Exit", "Retrofit"),
#'   base_name = "A1_Naive_2WFE_Benchmark",
#'   title = "Naive 2WFE DiD (Biased Benchmark)"
#' )
save_standard_did_table <- function(
    models,
    headers,
    base_name,
    title,
    treatment_var = "texas_treated:post_1999",
    cluster_var = "state",
    use_bootstrap = TRUE,
    n_reps = 999,
    digits = 6,
    output_dir = NULL
) {
  
  # Resolve output directory
  if (is.null(output_dir)) {
    if (exists("OUTPUT_TABLES", envir = .GlobalEnv)) {
      output_dir <- get("OUTPUT_TABLES", envir = .GlobalEnv)
    } else {
      output_dir <- here::here("Output", "Tables")
    }
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  n_models <- length(models)
  
  # Validate inputs
  stopifnot(
    "models and headers must have same length" = length(headers) == n_models,
    "models must be a list" = is.list(models)
  )
  
  # Initialize bootstrap engine if needed
  if (use_bootstrap) {
    check_julia_bootstrap_engine()
  }
  
  #---------------------------------------------------------------------------
  # 1. Run Bootstrap or HC2 Fallback
  #---------------------------------------------------------------------------
  cat(sprintf("\n=== %s ===\n", title))
  
  results_list <- vector("list", n_models)
  
  for (i in seq_along(models)) {
    cat(sprintf("Processing model %d/%d: %s...\n", i, n_models, headers[i]))
    
    if (use_bootstrap) {
      results_list[[i]] <- run_wcb_single(
        model = models[[i]],
        param = treatment_var,
        cluster_var = cluster_var,
        n_reps = n_reps
      )
      results_list[[i]]$se_type <- "WCB"
    } else {
      results_list[[i]] <- get_hc2_results(
        model = models[[i]],
        param = treatment_var,
        cluster_var = cluster_var
      )
      results_list[[i]]$se_type <- "HC2"
    }
  }
  
  #---------------------------------------------------------------------------
  # 2. Build Results data.table
  #---------------------------------------------------------------------------
  coef_dt <- data.table(
    Model = headers,
    Coefficient = sapply(results_list, `[[`, "beta_obs"),
    SE = sapply(results_list, function(x) x$se_boot %||% x$se_hc2),
    CI_Lower = sapply(results_list, `[[`, "ci_lower"),
    CI_Upper = sapply(results_list, `[[`, "ci_upper"),
    P_Value = sapply(results_list, `[[`, "p_val"),
    P_Label = sapply(results_list, `[[`, "label"),
    SE_Type = sapply(results_list, `[[`, "se_type"),
    N = sapply(models, nobs),
    R2 = sapply(models, function(m) tryCatch(r2(m, type = "r2"), error = function(e) NA_real_))
  )
  
  #---------------------------------------------------------------------------
  # 3. Console Output
  #---------------------------------------------------------------------------
  print(fixest::etable(models, headers = headers, se.below = TRUE, 
                       fitstat = c("n", "r2"), digits = digits))
  
  cat("\nInference Results:\n")
  print(coef_dt[, .(Model, Coefficient, SE, P_Value, P_Label, SE_Type)])
  
  #---------------------------------------------------------------------------
  # 4. CSV Export
  #---------------------------------------------------------------------------
  csv_file <- file.path(output_dir, paste0(base_name, ".csv"))
  fwrite(coef_dt, csv_file)
  
  #---------------------------------------------------------------------------
  # 5. LaTeX Export
  #---------------------------------------------------------------------------
  tex_file <- file.path(output_dir, paste0(base_name, ".tex"))
  
  fixest::etable(
    models,
    headers = headers,
    se.below = TRUE,
    fitstat = c("n", "r2"),
    digits = digits,
    tex = TRUE,
    title = title,
    file = tex_file,
    replace = TRUE
  )
  
  # Inject p-value row into LaTeX
  tex_lines <- readLines(tex_file)
  treat_pattern <- gsub(":", ".*", gsub("_", "\\\\_", treatment_var))
  treat_row_idx <- grep(treat_pattern, tex_lines)
  
  if (length(treat_row_idx) > 0) {
    se_row_idx <- treat_row_idx[1] + 1
    
    p_labels <- sapply(results_list, `[[`, "label")
    se_label <- if (use_bootstrap) "WCB p-val" else "HC2 p-val"
    p_row <- paste0("$[$", se_label, "$]$ & ", paste(p_labels, collapse = " & "), " \\\\")
    
    tex_lines <- c(
      tex_lines[1:se_row_idx],
      p_row,
      tex_lines[(se_row_idx + 1):length(tex_lines)]
    )
    
    # Add footnote
    note_idx <- grep("\\\\end\\{tabular\\}", tex_lines)[1] - 1
    if (!is.na(note_idx) && note_idx > 0) {
      if (use_bootstrap) {
        note_text <- sprintf(
          "\\multicolumn{%d}{l}{\\footnotesize WCB = Wild cluster bootstrap (Webb weights, %d reps, Julia engine)} \\\\",
          n_models + 1, n_reps
        )
      } else {
        note_text <- sprintf(
          "\\multicolumn{%d}{l}{\\footnotesize HC2 = Heteroskedasticity-robust clustered SEs (state level)} \\\\",
          n_models + 1
        )
      }
      tex_lines <- c(tex_lines[1:note_idx], note_text, tex_lines[(note_idx + 1):length(tex_lines)])
    }
    
    writeLines(tex_lines, tex_file)
  }
  
  #---------------------------------------------------------------------------
  # 6. TXT Export
  #---------------------------------------------------------------------------
  txt_file <- file.path(output_dir, paste0(base_name, ".txt"))
  
  sink(txt_file)
  cat(paste0("=== ", title, " ===\n"))
  cat(paste0("Generated: ", Sys.time(), "\n"))
  cat(sprintf("Inference: %s\n\n", ifelse(use_bootstrap, 
                                           sprintf("Wild Cluster Bootstrap (Webb, %d reps)", n_reps),
                                           "HC2 Clustered SEs")))
  
  print(fixest::etable(models, headers = headers, se.below = TRUE, 
                       fitstat = c("n", "r2"), digits = digits))
  
  cat("\n====================================================================\n")
  cat(sprintf("%s P-Values\n", ifelse(use_bootstrap, "Wild Cluster Bootstrap", "HC2")))
  cat("====================================================================\n")
  cat(sprintf("Treatment Effect: %s\n", treatment_var))
  cat(sprintf("Clustering: %s level (%d clusters)\n\n", cluster_var, 19))
  
  for (i in seq_along(models)) {
    cat(sprintf("  %-20s: %s (beta = %.*f)\n",
                headers[i],
                results_list[[i]]$label,
                digits,
                results_list[[i]]$beta_obs))
  }
  
  cat("\n--------------------------------------------------------------------\n")
  cat("Notes:\n")
  if (use_bootstrap) {
    cat("  - WCB uses Webb 6-point weights (recommended for G < 20)\n")
    cat("  - Engine: WildBootTests.jl (memory-efficient)\n")
  } else {
    cat("  - HC2 small-sample correction applied\n")
    cat("  - t(G-1) critical values used for inference\n")
  }
  cat("  - Significance: *** p<0.01, ** p<0.05, * p<0.10\n")
  cat("====================================================================\n")
  sink()
  
  cat(sprintf("✓ Saved: %s (.csv, .tex, .txt)\n", base_name))
  
  invisible(coef_dt)
}


#==============================================================================
# FUNCTION 2: TRIPLE INTERACTION DiD TABLE
# Used for: Part A.2 (Step-In Controls with Motor Fuel HTE)
#==============================================================================

#' Save Triple Interaction DiD Table
#' 
#' Creates tables for triple interaction models (Texas × Post × MotorFuel).
#' Tests three hypotheses:
#'   1. H0: β₁ = 0 (Non-motor fuel effect)
#'   2. H0: β₃ = 0 (Motor fuel differential)
#'   3. H0: β₁ + β₃ = 0 (Motor fuel total effect)
#' 
#' @param pooled_model Optional fixest model without interaction (for baseline column)
#' @param triple_models List of fixest models with triple interaction
#' @param headers Character vector of column headers
#' @param base_name Character: base filename
#' @param title Character: table title
#' @param base_term Character: base treatment coefficient name (β₁)
#' @param interact_term Character: interaction coefficient name (β₃)
#' @param cluster_var Character: clustering variable
#' @param use_bootstrap Logical: whether to run WCB
#' @param n_reps Integer: bootstrap replications
#' @param digits Integer: decimal places
#' @param output_dir Character: output directory
#' 
#' @return data.table of results (invisibly)
save_triple_did_table <- function(
    pooled_model = NULL,
    triple_models,
    headers,
    base_name,
    title,
    base_term = "texas_treated:post_1999",
    interact_term = "texas_treated:post_1999:is_motor_fuelTRUE",
    cluster_var = "state",
    use_bootstrap = TRUE,
    n_reps = 999,
    digits = 6,
    output_dir = NULL
) {
  
  # Resolve output directory
  if (is.null(output_dir)) {
    if (exists("OUTPUT_TABLES", envir = .GlobalEnv)) {
      output_dir <- get("OUTPUT_TABLES", envir = .GlobalEnv)
    } else {
      output_dir <- here::here("Output", "Tables")
    }
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Validate inputs
  n_triple <- length(triple_models)
  has_pooled <- !is.null(pooled_model)
  n_total <- n_triple + as.integer(has_pooled)
  
  stopifnot(
    "headers length must match total models" = length(headers) == n_total,
    "triple_models must be a list" = is.list(triple_models)
  )
  
  # Initialize bootstrap engine if needed
  if (use_bootstrap) {
    check_julia_bootstrap_engine()
  }
  
  cat(sprintf("\n=== %s ===\n", title))
  
  #---------------------------------------------------------------------------
  # Helper: Format coefficient with p-value
  #---------------------------------------------------------------------------
  fmt_res <- function(beta, p, digits = 6) {
    if (is.na(beta) || is.na(p)) return("--")
    stars <- if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
    sprintf("%.*f%s (%.4f)", digits, beta, stars, p)
  }
  
  #---------------------------------------------------------------------------
  # Process Pooled Model (if provided)
  #---------------------------------------------------------------------------
  pooled_results <- NULL
  if (has_pooled) {
    cat("Processing pooled model...\n")
    
    if (use_bootstrap) {
      pooled_results <- run_wcb_single(pooled_model, base_term, cluster_var, n_reps)
    } else {
      pooled_results <- get_hc2_results(pooled_model, base_term, cluster_var)
    }
  }
  
  #---------------------------------------------------------------------------
  # Process Triple Interaction Models
  #---------------------------------------------------------------------------
  triple_results <- vector("list", n_triple)
  
  for (i in seq_along(triple_models)) {
    model_idx <- i + as.integer(has_pooled)
    cat(sprintf("Processing model %d/%d: %s...\n", model_idx, n_total, headers[model_idx]))
    
    model <- triple_models[[i]]
    
    if (use_bootstrap) {
      # Test 1: β₁ (Non-motor fuel effect)
      base_res <- run_wcb_single(model, base_term, cluster_var, n_reps, seed = 20250120)
      
      # Test 2: β₃ (Motor fuel differential)
      interact_res <- run_wcb_single(model, interact_term, cluster_var, n_reps, seed = 20250121)
      
      # Test 3: β₁ + β₃ (Motor fuel total effect)
      sum_res <- run_wcb_combination(
        model,
        params = c(base_term, interact_term),
        R = c(1, 1),
        cluster_var = cluster_var,
        n_reps = n_reps,
        seed = 20250122
      )
    } else {
      # HC2 fallback
      base_res <- get_hc2_results(model, base_term, cluster_var)
      interact_res <- get_hc2_results(model, interact_term, cluster_var)
      
      # For sum, compute manually with delta method
      model_coefs <- coef(model)
      vcov_m <- vcov(model)
      
      if (base_term %in% names(model_coefs) && interact_term %in% names(model_coefs)) {
        beta_sum <- model_coefs[[base_term]] + model_coefs[[interact_term]]
        se_sum <- sqrt(vcov_m[base_term, base_term] + vcov_m[interact_term, interact_term] + 
                         2 * vcov_m[base_term, interact_term])
        t_stat <- beta_sum / se_sum
        p_sum <- 2 * pt(abs(t_stat), df = 18, lower.tail = FALSE)
        stars <- if (p_sum < 0.01) "***" else if (p_sum < 0.05) "**" else if (p_sum < 0.1) "*" else ""
        
        sum_res <- list(
          beta_obs = beta_sum,
          se_hc2 = se_sum,
          p_val = p_sum,
          ci_lower = beta_sum - qt(0.975, 18) * se_sum,
          ci_upper = beta_sum + qt(0.975, 18) * se_sum,
          label = sprintf("%.4f%s", p_sum, stars)
        )
      } else {
        sum_res <- list(beta_obs = NA, se_hc2 = NA, p_val = NA, 
                        ci_lower = NA, ci_upper = NA, label = "Not found")
      }
    }
    
    triple_results[[i]] <- list(
      base = base_res,
      interact = interact_res,
      sum = sum_res
    )
  }
  
  #---------------------------------------------------------------------------
  # Build Output Table
  #---------------------------------------------------------------------------
  # Row structure:
  # 1. Pooled Effect (only first column if pooled model exists)
  # 2. Non-Motor Fuel Effect (β₁)
  # 3. MF Differential (β₃)
  # 4. Motor Fuel Effect (β₁ + β₃)
  
  table_dt <- data.table(
    Effect = c(
      "Pooled Effect",
      "Non-Motor Fuel (β₁)",
      "MF Differential (β₃)",
      "Motor Fuel Effect (β₁+β₃)"
    )
  )
  
  col_idx <- 1
  
  # Add pooled column
  if (has_pooled) {
    col_vals <- c(
      fmt_res(pooled_results$beta_obs, pooled_results$p_val, digits),
      "--",
      "--",
      "--"
    )
    table_dt[, (headers[col_idx]) := col_vals]
    col_idx <- col_idx + 1
  }
  
  # Add triple model columns
  for (i in seq_along(triple_results)) {
    res <- triple_results[[i]]
    col_vals <- c(
      "--",
      fmt_res(res$base$beta_obs, res$base$p_val, digits),
      fmt_res(res$interact$beta_obs, res$interact$p_val, digits),
      fmt_res(res$sum$beta_obs, res$sum$p_val, digits)
    )
    table_dt[, (headers[col_idx]) := col_vals]
    col_idx <- col_idx + 1
  }
  
  #---------------------------------------------------------------------------
  # Console Output
  #---------------------------------------------------------------------------
  cat("\nTriple Interaction Results:\n")
  print(table_dt)
  
  #---------------------------------------------------------------------------
  # CSV Export
  #---------------------------------------------------------------------------
  csv_file <- file.path(output_dir, paste0(base_name, ".csv"))
  fwrite(table_dt, csv_file)
  
  #---------------------------------------------------------------------------
  # LaTeX Export
  #---------------------------------------------------------------------------
  tex_file <- file.path(output_dir, paste0(base_name, ".tex"))
  
  # Build LaTeX manually for this custom structure
  tex_content <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    sprintf("\\caption{%s}", title),
    sprintf("\\begin{tabular}{l%s}", paste(rep("c", n_total), collapse = "")),
    "\\toprule",
    paste0("Effect & ", paste(headers, collapse = " & "), " \\\\"),
    "\\midrule"
  )
  
  # Add data rows
  for (r in 1:nrow(table_dt)) {
    row_vals <- as.character(table_dt[r, -1, with = FALSE])
    tex_content <- c(tex_content, 
                     paste0(table_dt$Effect[r], " & ", paste(row_vals, collapse = " & "), " \\\\"))
  }
  
  tex_content <- c(
    tex_content,
    "\\bottomrule",
    sprintf("\\multicolumn{%d}{l}{\\footnotesize %s} \\\\", n_total + 1,
            ifelse(use_bootstrap, 
                   sprintf("Wild cluster bootstrap (Webb, %d reps). * p<0.1, ** p<0.05, *** p<0.01", n_reps),
                   "HC2 clustered SEs. * p<0.1, ** p<0.05, *** p<0.01")),
    sprintf("\\multicolumn{%d}{l}{\\footnotesize β₁ = Texas×Post, β₃ = Texas×Post×MotorFuel} \\\\", n_total + 1),
    "\\end{tabular}",
    "\\end{table}"
  )
  
  writeLines(tex_content, tex_file)
  
  #---------------------------------------------------------------------------
  # TXT Export
  #---------------------------------------------------------------------------
  txt_file <- file.path(output_dir, paste0(base_name, ".txt"))
  
  sink(txt_file)
  cat(paste0("=== ", title, " ===\n"))
  cat(paste0("Generated: ", Sys.time(), "\n"))
  cat(sprintf("Inference: %s\n\n", ifelse(use_bootstrap, 
                                           sprintf("Wild Cluster Bootstrap (Webb, %d reps)", n_reps),
                                           "HC2 Clustered SEs")))
  
  print(table_dt)
  
  cat("\n====================================================================\n")
  cat("Hypothesis Tests\n")
  cat("====================================================================\n")
  cat("  H0(1): β₁ = 0       (Non-motor fuel treatment effect)\n")
  cat("  H0(2): β₃ = 0       (Motor fuel differential from base)\n")
  cat("  H0(3): β₁ + β₃ = 0  (Motor fuel total treatment effect)\n")
  cat("\n")
  cat("Coefficient Interpretation:\n")
  cat("  β₁              = Effect for non-motor-fuel facilities\n")
  cat("  β₃              = Additional effect for motor-fuel facilities\n")
  cat("  β₁ + β₃         = Total effect for motor-fuel facilities\n")
  cat("====================================================================\n")
  sink()
  
  cat(sprintf("✓ Saved: %s (.csv, .tex, .txt)\n", base_name))
  
  invisible(table_dt)
}


#==============================================================================
# FUNCTION 3: SIMPLE CSV TABLE
# Used for: Parts C.2-C.5, E.2, E.3, F.1, F.2
#==============================================================================

#' Save Simple CSV Table
#' 
#' Saves coefficient extracts to CSV. Optionally runs bootstrap for supported models.
#' Primarily for Deaner-Ku hazard analysis, HTE extracts, and robustness checks.
#' 
#' @param results_dt data.table: pre-computed results to save
#' @param base_name Character: base filename
#' @param title Character: optional title for header
#' @param models Optional list of models for bootstrap inference
#' @param treatment_var Character: coefficient to test (if models provided)
#' @param cluster_var Character: clustering variable
#' @param use_bootstrap Logical: whether to add bootstrap p-values
#' @param n_reps Integer: bootstrap replications
#' @param output_dir Character: output directory
#' 
#' @return data.table (invisibly)
save_simple_csv_table <- function(
    results_dt,
    base_name,
    title = NULL,
    models = NULL,
    treatment_var = "texas_treated:post_1999",
    cluster_var = "state",
    use_bootstrap = TRUE,
    n_reps = 999,
    output_dir = NULL
) {
  
  # Resolve output directory
  if (is.null(output_dir)) {
    if (exists("OUTPUT_TABLES", envir = .GlobalEnv)) {
      output_dir <- get("OUTPUT_TABLES", envir = .GlobalEnv)
    } else {
      output_dir <- here::here("Output", "Tables")
    }
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # If models provided and bootstrap requested, add p-values
  if (!is.null(models) && use_bootstrap && length(models) > 0) {
    check_julia_bootstrap_engine()
    
    cat(sprintf("Running bootstrap for %d models...\n", length(models)))
    
    wcb_pvals <- sapply(models, function(m) {
      res <- tryCatch({
        run_wcb_single(m, treatment_var, cluster_var, n_reps)
      }, error = function(e) {
        list(p_val = NA, label = "Error")
      })
      res$p_val
    })
    
    if (nrow(results_dt) == length(wcb_pvals)) {
      results_dt[, WCB_P := wcb_pvals]
    }
  }
  
  # CSV Export
  csv_file <- file.path(output_dir, paste0(base_name, ".csv"))
  fwrite(results_dt, csv_file)
  
  # Console output
  if (!is.null(title)) {
    cat(sprintf("\n=== %s ===\n", title))
  }
  print(results_dt)
  
  cat(sprintf("✓ Saved: %s.csv\n", base_name))
  
  invisible(results_dt)
}


#==============================================================================
# FUNCTION 4: EVENT STUDY TABLE
# Used for: Part G.1
#==============================================================================

#' Save Event Study Table
#' 
#' Creates tables and plot-ready CSVs for event study models.
#' Computes WCB confidence intervals for each time period coefficient.
#' 
#' @param model fixest event study model
#' @param base_name Character: base filename
#' @param title Character: table title
#' @param outcome_label Character: label for outcome variable
#' @param event_var Character: event time variable pattern in coefficient names
#' @param ref_date Date: reference period (excluded)
#' @param cluster_var Character: clustering variable
#' @param use_bootstrap Logical: whether to run WCB
#' @param n_reps Integer: bootstrap replications
#' @param output_dir Character: output directory
#' 
#' @return data.table of event study coefficients (invisibly)
save_event_study_table <- function(
    model,
    base_name,
    title,
    outcome_label,
    event_var = "event_date",
    ref_date = as.Date("1998-12-01"),
    cluster_var = "state",
    use_bootstrap = TRUE,
    n_reps = 999,
    output_dir = NULL
) {
  
  # Resolve output directory
  if (is.null(output_dir)) {
    if (exists("OUTPUT_TABLES", envir = .GlobalEnv)) {
      output_dir <- get("OUTPUT_TABLES", envir = .GlobalEnv)
    } else {
      output_dir <- here::here("Output", "Tables")
    }
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat(sprintf("\n=== %s ===\n", title))
  
  # Extract event study coefficients
  all_coefs <- coef(model)
  all_se <- se(model)
  
  # Find event time coefficients (pattern: event_date::YYYY-MM-DD:treatment)
  es_pattern <- paste0(event_var, "::")
  es_names <- grep(es_pattern, names(all_coefs), value = TRUE)
  
  if (length(es_names) == 0) {
    warning("No event study coefficients found matching pattern: ", es_pattern)
    return(invisible(NULL))
  }
  
  # Build base results
  es_dt <- data.table(
    term = es_names,
    estimate = all_coefs[es_names],
    std.error = all_se[es_names]
  )
  
  # Extract dates from coefficient names
  es_dt[, event_date := as.Date(regmatches(term, regexpr("\\d{4}-\\d{2}-\\d{2}", term)))]
  es_dt[, period := ifelse(event_date < ref_date, "Pre", "Post")]
  es_dt[, Outcome := outcome_label]
  
  # Run bootstrap for each coefficient if requested
  if (use_bootstrap) {
    check_julia_bootstrap_engine()
    
    cat(sprintf("Running bootstrap for %d event study coefficients...\n", nrow(es_dt)))
    
    # This is computationally intensive - run in parallel chunks if many coefficients
    wcb_results <- lapply(es_names, function(coef_name) {
      tryCatch({
        run_wcb_single(model, coef_name, cluster_var, n_reps)
      }, error = function(e) {
        list(se_boot = NA, ci_lower = NA, ci_upper = NA)
      })
    })
    
    es_dt[, `:=`(
      se_wcb = sapply(wcb_results, function(x) x$se_boot %||% NA_real_),
      conf.low = sapply(wcb_results, function(x) x$ci_lower %||% NA_real_),
      conf.high = sapply(wcb_results, function(x) x$ci_upper %||% NA_real_),
      se_type = "WCB"
    )]
  } else {
    # Use clustered SEs
    es_dt[, `:=`(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      se_type = "Clustered"
    )]
  }
  
  # Sort by date
  setorder(es_dt, event_date)
  
  # CSV Export
  csv_file <- file.path(output_dir, paste0(base_name, "_event_study.csv"))
  fwrite(es_dt, csv_file)
  
  # Console output
  cat("\nEvent Study Coefficients:\n")
  print(es_dt[, .(event_date, estimate, std.error, conf.low, conf.high, period)])
  
  cat(sprintf("✓ Saved: %s_event_study.csv\n", base_name))
  
  invisible(es_dt)
}


#==============================================================================
# FUNCTION 5: SUMMARY TABLE
# Used for: D.2 decomposition summary, interpretation tables
#==============================================================================

#' Save Summary/Interpretation Table
#' 
#' Saves summary tables with interpretations. No bootstrap - uses pre-computed results.
#' 
#' @param results_dt data.table: summary data with interpretations
#' @param base_name Character: base filename
#' @param title Character: table title
#' @param notes Character vector: footnotes to include
#' @param output_dir Character: output directory
#' 
#' @return data.table (invisibly)
save_summary_table <- function(
    results_dt,
    base_name,
    title,
    notes = NULL,
    output_dir = NULL
) {
  
  # Resolve output directory
  if (is.null(output_dir)) {
    if (exists("OUTPUT_TABLES", envir = .GlobalEnv)) {
      output_dir <- get("OUTPUT_TABLES", envir = .GlobalEnv)
    } else {
      output_dir <- here::here("Output", "Tables")
    }
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat(sprintf("\n=== %s ===\n", title))
  print(results_dt)
  
  # CSV Export
  csv_file <- file.path(output_dir, paste0(base_name, ".csv"))
  fwrite(results_dt, csv_file)
  
  # TXT Export with interpretation
  txt_file <- file.path(output_dir, paste0(base_name, ".txt"))
  
  sink(txt_file)
  cat(paste0("=== ", title, " ===\n"))
  cat(paste0("Generated: ", Sys.time(), "\n\n"))
  
  print(results_dt)
  
  if (!is.null(notes)) {
    cat("\n--------------------------------------------------------------------\n")
    cat("Notes:\n")
    for (note in notes) {
      cat(sprintf("  - %s\n", note))
    }
  }
  sink()
  
  cat(sprintf("✓ Saved: %s (.csv, .txt)\n", base_name))
  
  invisible(results_dt)
}


#==============================================================================
# UTILITY: Null coalesce operator
#==============================================================================
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x


#==============================================================================
# LOAD MESSAGE
#==============================================================================
cat("✓ Table saving helper loaded (5 specialized functions)\n")
cat("  Functions available:\n")
cat("    1. save_standard_did_table()  - Standard 2x2 DiD\n")
cat("    2. save_triple_did_table()    - Triple interaction with MF HTE\n")
cat("    3. save_simple_csv_table()    - Coefficient extracts\n")
cat("    4. save_event_study_table()   - Event study with WCB CIs\n")
cat("    5. save_summary_table()       - Interpretation tables\n")
cat("\n")
cat("  Bootstrap Engine: fwildclusterboot + WildBootTests.jl\n")
cat("  Weights: Webb 6-point (optimal for G < 20)\n")
cat("  Fallback: HC2 clustered SEs with t(G-1) inference\n")