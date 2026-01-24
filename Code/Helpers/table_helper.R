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
# Compatibility Updates (2025):
#   - Added 'Smart Coefficient Resolution' to handle aliases (did_term vs interaction)
#   - Added handling for 'TRUE/FALSE' suffix mismatches in integer interactions
#   - Updated Event Study extractor for fixest::i() syntax
#   - FIXED: vcov() calls now accept string arguments directly to prevent scoping errors
#==============================================================================

library(data.table)
library(fixest)
library(kableExtra)
library(here)

#==============================================================================
# INTERNAL UTILS: Coefficient Resolution
#==============================================================================

#' Resolve Coefficient Name
#' 
#' Handles mismatches between requested parameter names and model names.
#' Critical for handling 'did_term' aliases and 'is_motor_fuelTRUE' suffixes.
#' 
#' @param model fixest model object
#' @param target_name Requested coefficient name
#' @return The actual coefficient name in the model, or NA if not found
resolve_coef_name <- function(model, target_name) {
  model_names <- names(coef(model))
  
  # 1. Exact match
  if (target_name %in% model_names) return(target_name)
  
  # 2. Check for "did_term" alias (Common in Part A.1 vs A.2)
  if (target_name == "texas_treated:post_1999" && "did_term" %in% model_names) {
    return("did_term")
  }
  
  # 3. Check for "TRUE/FALSE" suffix mismatch (Integer vs Logical interactions)
  #    Target: "...:is_motor_fuelTRUE", Model: "...:is_motor_fuel"
  stripped_true <- gsub("TRUE$", "", target_name)
  if (stripped_true %in% model_names) return(stripped_true)
  
  stripped_false <- gsub("FALSE$", "", target_name)
  if (stripped_false %in% model_names) return(stripped_false)
  
  # 4. Interaction Order Swap (a:b vs b:a)
  #    Only handles simple 2-way swaps for now
  if (grepl(":", target_name)) {
    parts <- unlist(strsplit(target_name, ":"))
    if (length(parts) == 2) {
      swapped <- paste(parts[2], parts[1], sep = ":")
      if (swapped %in% model_names) return(swapped)
    }
  }
  
  # Not found
  return(NA_character_)
}

#==============================================================================
# BOOTSTRAP ENGINE SETUP
#==============================================================================

#' Check and Initialize Julia Bootstrap Engine
check_julia_bootstrap_engine <- function() {
  
  # Check 1: fwildclusterboot package
  if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
    stop("BOOTSTRAP ERROR: fwildclusterboot package not installed.\n")
  }
  
  # Check 2: JuliaConnectoR package
  if (!requireNamespace("JuliaConnectoR", quietly = TRUE)) {
    stop("BOOTSTRAP ERROR: JuliaConnectoR package not installed.\n")
  }
  
  # Check 3: Julia installation
  julia_home <- Sys.getenv("JULIA_BINDIR")
  if (julia_home == "") julia_home <- Sys.getenv("JULIA_HOME")
  julia_path <- Sys.which("julia")
  
  if (julia_home == "" && julia_path == "") {
    stop("BOOTSTRAP ERROR: Julia installation not found.\n")
  }
  
  # Set the bootstrap engine globally
  fwildclusterboot::setBoottest_engine("WildBootTests.jl")
  
  cat("✓ Julia bootstrap engine initialized successfully\n")
  return(TRUE)
}

#' Run Wild Cluster Bootstrap for Single Coefficient
run_wcb_single <- function(model, param, cluster_var = "state", n_reps = 999) {
  
  # Resolve Alias
  actual_param <- resolve_coef_name(model, param)
  
  if (is.na(actual_param)) {
    warning(sprintf("Coefficient '%s' not found in model.", param))
    return(list(beta_obs = NA, se_boot = NA, p_val = NA, label = "Not found"))
  }
  
  beta_obs <- coef(model)[[actual_param]]
  
  boot_result <- tryCatch({
    fwildclusterboot::boottest(
      object = model,
      param = actual_param,
      clustid = as.formula(paste0("~", cluster_var)),
      B = n_reps,
      type = "webb",
      engine = "WildBootTests.jl",
      impose_null = TRUE
    )
  }, error = function(e) {
    warning(sprintf("Bootstrap failed for '%s': %s", actual_param, e$message))
    return(NULL)
  })
  
  if (is.null(boot_result)) return(list(beta_obs = beta_obs, label = "Boot Fail"))
  
  p_val <- boot_result$p_val
  ci <- boot_result$conf_int
  se_boot <- (ci[2] - ci[1]) / (2 * 1.96)
  stars <- if (p_val < 0.01) "***" else if (p_val < 0.05) "**" else if (p_val < 0.1) "*" else ""
  
  return(list(
    beta_obs = beta_obs, se_boot = se_boot, p_val = p_val,
    ci_lower = ci[1], ci_upper = ci[2], label = sprintf("%.4f%s", p_val, stars)
  ))
}

#' Run Wild Cluster Bootstrap for Linear Combination (Triple Interaction Sums)
run_wcb_combination <- function(model, params, R, cluster_var = "state", n_reps = 999) {
  
  # Resolve all params
  resolved_params <- sapply(params, function(p) resolve_coef_name(model, p))
  
  if (any(is.na(resolved_params))) {
    warning("One or more coefficients not found for linear combination.")
    return(list(beta_obs = NA, label = "Not found"))
  }
  
  # Use the resolved names
  actual_params <- unname(resolved_params)
  beta_obs <- sum(R * coef(model)[actual_params])
  
  boot_result <- tryCatch({
    fwildclusterboot::boottest(
      object = model,
      param = actual_params,
      R = R,
      clustid = as.formula(paste0("~", cluster_var)),
      B = n_reps,
      type = "webb",
      engine = "WildBootTests.jl",
      impose_null = TRUE
    )
  }, error = function(e) {
    warning(sprintf("Bootstrap failed for combination: %s", e$message))
    return(NULL)
  })
  
  if (is.null(boot_result)) return(list(beta_obs = beta_obs, label = "Boot Fail"))
  
  p_val <- boot_result$p_val
  ci <- boot_result$conf_int
  se_boot <- (ci[2] - ci[1]) / (2 * 1.96)
  stars <- if (p_val < 0.01) "***" else if (p_val < 0.05) "**" else if (p_val < 0.1) "*" else ""
  
  return(list(
    beta_obs = beta_obs, se_boot = se_boot, p_val = p_val,
    ci_lower = ci[1], ci_upper = ci[2], label = sprintf("%.4f%s", p_val, stars)
  ))
}

#' Get HC2 Clustered Standard Errors (Fallback)
get_hc2_results <- function(model, param, cluster_var = "state") {
  
  actual_param <- resolve_coef_name(model, param)
  
  if (is.na(actual_param)) return(list(beta_obs = NA, label = "Not found"))
  
  vcov_hc2 <- tryCatch({
    # CRITICAL FIX: Pass 'cluster_var' directly as a string or formula, not using 'get()'
    # fixest handles string input for vcov correctly (e.g., vcov = "state")
    vcov(model, vcov = cluster_var, ssc = ssc(adj = TRUE, cluster.adj = TRUE))
  }, error = function(e) {
    # If the string fails, try as a formula without get()
    tryCatch({
      vcov(model, vcov = as.formula(paste0("~", cluster_var)), ssc = ssc(adj = TRUE, cluster.adj = TRUE))
    }, error = function(e2) {
      warning("HC2 clustering failed, falling back to default SEs. Error: ", e2$message)
      vcov(model)
    })
  })
  
  beta_obs <- coef(model)[[actual_param]]
  se_hc2 <- sqrt(vcov_hc2[actual_param, actual_param])
  t_stat <- beta_obs / se_hc2
  
  G <- 19 # Clusters
  p_val <- 2 * pt(abs(t_stat), df = G - 1, lower.tail = FALSE)
  
  ci_lower <- beta_obs - qt(0.975, df = G - 1) * se_hc2
  ci_upper <- beta_obs + qt(0.975, df = G - 1) * se_hc2
  
  stars <- if (p_val < 0.01) "***" else if (p_val < 0.05) "**" else if (p_val < 0.1) "*" else ""
  
  return(list(
    beta_obs = beta_obs, se_hc2 = se_hc2, p_val = p_val,
    ci_lower = ci_lower, ci_upper = ci_upper, label = sprintf("%.4f%s", p_val, stars)
  ))
}

#==============================================================================
# FUNCTION 1: STANDARD DiD TABLE
#==============================================================================
save_standard_did_table <- function(models, headers, base_name, title, 
                                    treatment_var = "texas_treated:post_1999", 
                                    cluster_var = "state", 
                                    use_bootstrap = TRUE, n_reps = 999, 
                                    digits = 6, output_dir = NULL) {
  
  if (is.null(output_dir)) output_dir <- here::here("Output", "Tables")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (use_bootstrap) check_julia_bootstrap_engine()
  
  cat(sprintf("\n=== %s ===\n", title))
  
  results_list <- vector("list", length(models))
  
  for (i in seq_along(models)) {
    if (use_bootstrap) {
      results_list[[i]] <- run_wcb_single(models[[i]], treatment_var, cluster_var, n_reps)
      results_list[[i]]$se_type <- "WCB"
    } else {
      results_list[[i]] <- get_hc2_results(models[[i]], treatment_var, cluster_var)
      results_list[[i]]$se_type <- "HC2"
    }
  }
  
  # Build Data Table
  safe_ext <- function(l, f) if(is.null(l[[f]])) NA_real_ else l[[f]]
  
  coef_dt <- data.table(
    Model = headers,
    Coefficient = sapply(results_list, safe_ext, "beta_obs"),
    SE = sapply(results_list, function(x) x$se_boot %||% x$se_hc2 %||% NA),
    P_Value = sapply(results_list, safe_ext, "p_val"),
    P_Label = sapply(results_list, function(x) x$label %||% "NA"),
    N = sapply(models, nobs),
    R2 = sapply(models, function(m) tryCatch(r2(m, "r2"), error=function(e) NA))
  )
  
  # Output
  csv_file <- file.path(output_dir, paste0(base_name, ".csv"))
  fwrite(coef_dt, csv_file)
  
  print(coef_dt[, .(Model, Coefficient, P_Label, N)])
  cat(sprintf("✓ Saved: %s.csv\n", base_name))
  invisible(coef_dt)
}

#==============================================================================
# FUNCTION 2: TRIPLE INTERACTION DiD TABLE
#==============================================================================
save_triple_did_table <- function(pooled_model = NULL, triple_models, headers, 
                                  base_name, title, 
                                  base_term = "texas_treated:post_1999",
                                  interact_term = "texas_treated:post_1999:is_motor_fuelTRUE",
                                  cluster_var = "state", use_bootstrap = TRUE, 
                                  n_reps = 999, digits = 6, output_dir = NULL) {
  
  if (is.null(output_dir)) output_dir <- here::here("Output", "Tables")
  if (use_bootstrap) check_julia_bootstrap_engine()
  
  cat(sprintf("\n=== %s ===\n", title))
  
  fmt <- function(b, p) if(is.na(b)) "--" else sprintf("%.*f%s", digits, b, 
         if(p < 0.01) "***" else if(p < 0.05) "**" else if(p < 0.1) "*" else "")
  
  # Process Pooled
  pooled_res <- if (!is.null(pooled_model)) {
    if (use_bootstrap) run_wcb_single(pooled_model, base_term, cluster_var, n_reps)
    else get_hc2_results(pooled_model, base_term, cluster_var)
  } else NULL
  
  # Process Triples
  triple_res <- lapply(triple_models, function(m) {
    if (use_bootstrap) {
      list(
        base = run_wcb_single(m, base_term, cluster_var, n_reps),
        interact = run_wcb_single(m, interact_term, cluster_var, n_reps),
        sum = run_wcb_combination(m, c(base_term, interact_term), c(1,1), cluster_var, n_reps)
      )
    } else {
      # Manual HC2 Sum
      base <- get_hc2_results(m, base_term, cluster_var)
      interact <- get_hc2_results(m, interact_term, cluster_var)
      
      # Determine Actual Names
      bn <- resolve_coef_name(m, base_term)
      in_nm <- resolve_coef_name(m, interact_term)
      
      sum_res <- list(beta_obs=NA, p_val=1)
      if(!is.na(bn) && !is.na(in_nm)) {
        # CRITICAL FIX: Pass 'cluster_var' directly as string for vcov()
        vc <- tryCatch({
          vcov(m, vcov = cluster_var)
        }, error = function(e) {
          vcov(m, vcov = as.formula(paste0("~", cluster_var)))
        })
        
        est <- coef(m)[[bn]] + coef(m)[[in_nm]]
        se <- sqrt(vc[bn,bn] + vc[in_nm,in_nm] + 2*vc[bn,in_nm])
        p <- 2*pt(abs(est/se), 18, lower.tail=FALSE)
        sum_res <- list(beta_obs=est, p_val=p, se_hc2=se)
      }
      list(base=base, interact=interact, sum=sum_res)
    }
  })
  
  # Build Table
  dt <- data.table(Effect = c("Pooled Effect", "Non-MF (B1)", "MF Diff (B3)", "MF Total (B1+B3)"))
  
  if(!is.null(pooled_res)) {
    dt[, (headers[1]) := c(fmt(pooled_res$beta_obs, pooled_res$p_val), "--", "--", "--")]
  }
  
  start_idx <- if(is.null(pooled_res)) 1 else 2
  for(i in seq_along(triple_res)) {
    r <- triple_res[[i]]
    dt[, (headers[start_idx + i - 1]) := c("--",
         fmt(r$base$beta_obs, r$base$p_val),
         fmt(r$interact$beta_obs, r$interact$p_val),
         fmt(r$sum$beta_obs, r$sum$p_val))]
  }
  
  fwrite(dt, file.path(output_dir, paste0(base_name, ".csv")))
  print(dt)
  cat(sprintf("✓ Saved: %s.csv\n", base_name))
}

#==============================================================================
# FUNCTION 3: SIMPLE CSV TABLE
#==============================================================================
save_simple_csv_table <- function(results_dt, base_name, title = NULL, 
                                  models = NULL, treatment_var = "texas_treated:post_1999", 
                                  cluster_var = "state", use_bootstrap = TRUE, 
                                  n_reps = 999, output_dir = NULL) {
  
  if (is.null(output_dir)) output_dir <- here::here("Output", "Tables")
  if (!is.null(models) && use_bootstrap && length(models) > 0) {
    check_julia_bootstrap_engine()
    results_dt[, WCB_P := sapply(models, function(m) {
      run_wcb_single(m, treatment_var, cluster_var, n_reps)$p_val
    })]
  }
  
  fwrite(results_dt, file.path(output_dir, paste0(base_name, ".csv")))
  if(!is.null(title)) cat(sprintf("\n=== %s ===\n", title))
  print(results_dt)
  cat(sprintf("✓ Saved: %s.csv\n", base_name))
}

#==============================================================================
# FUNCTION 4: EVENT STUDY TABLE
#==============================================================================
save_event_study_table <- function(model, base_name, title, outcome_label, 
                                   event_var = "panel_year", ref_date = 1998, 
                                   cluster_var = "state", use_bootstrap = TRUE, 
                                   n_reps = 999, output_dir = NULL) {
  
  if (is.null(output_dir)) output_dir <- here::here("Output", "Tables")
  cat(sprintf("\n=== %s ===\n", title))
  
  # Detect Coefficients
  all_coefs <- coef(model)
  # Look for "panel_year::XXXX:texas_treated" (fixest::i syntax)
  # or "event_date::" (old syntax)
  
  if (any(grepl(paste0(event_var, "::"), names(all_coefs)))) {
    es_names <- grep(paste0(event_var, "::"), names(all_coefs), value = TRUE)
  } else {
    warning("No event study coefficients found.")
    return(NULL)
  }
  
  es_dt <- data.table(term = es_names, estimate = all_coefs[es_names], std.error = se(model)[es_names])
  es_dt[, year := as.numeric(gsub("[^0-9]", "", term))]
  es_dt[, period := ifelse(year < ref_date, "Pre", "Post")]
  es_dt[, Outcome := outcome_label]
  
  if (use_bootstrap) {
    check_julia_bootstrap_engine()
    cat(sprintf("Running bootstrap for %d periods...\n", nrow(es_dt)))
    
    # Run WCB for each period
    wcb <- lapply(es_names, function(p) run_wcb_single(model, p, cluster_var, n_reps))
    es_dt[, `:=`(
      se_wcb = sapply(wcb, function(x) x$se_boot),
      conf.low = sapply(wcb, function(x) x$ci_lower),
      conf.high = sapply(wcb, function(x) x$ci_upper)
    )]
  } else {
    es_dt[, `:=`(conf.low = estimate - 1.96*std.error, conf.high = estimate + 1.96*std.error)]
  }
  
  setorder(es_dt, year)
  fwrite(es_dt, file.path(output_dir, paste0(base_name, "_event_study.csv")))
  print(es_dt[, .(year, estimate, conf.low, conf.high)])
  cat(sprintf("✓ Saved: %s_event_study.csv\n", base_name))
}

#==============================================================================
# FUNCTION 5: SUMMARY TABLE
#==============================================================================
save_summary_table <- function(results_dt, base_name, title, notes = NULL, output_dir = NULL) {
  if (is.null(output_dir)) output_dir <- here::here("Output", "Tables")
  cat(sprintf("\n=== %s ===\n", title))
  print(results_dt)
  fwrite(results_dt, file.path(output_dir, paste0(base_name, ".csv")))
  cat(sprintf("✓ Saved: %s.csv\n", base_name))
}

# Load Message
cat("✓ Table helper loaded (Smart Aliasing + Fixed vcov Scoping)\n")