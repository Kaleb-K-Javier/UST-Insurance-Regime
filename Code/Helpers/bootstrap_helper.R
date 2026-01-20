#==============================================================================
# BOOTSTRAP HELPER: Fast Matrix-Algebra Wild Cluster Bootstrap
# 
# Location: Code/Helpers/bootstrap_helper.R
#
# Implements Roodman et al. (2019) Score Bootstrap via raw matrix multiplication.
# Uses fixest_data() to correctly recover estimation sample (handles dropped obs/NAs).
#
# References:
#   Roodman, D., et al. (2019). Fast and wild: Bootstrap inference in Stata 
#   using boottest. The Stata Journal, 19(1), 4-60.
#
# Dependencies: 
#   - fixest (for fixest_data, model.matrix, resid)
#   - Parent script must load: library(fixest)
#
# Usage:
#   source("Code/Helpers/bootstrap_helper.R")
#   wcb_result <- fast_wild_bootstrap(model, "texas_treated:post_1999", B = 9999)
#   wcb_result$p_val   # Numeric p-value
#   wcb_result$label   # Formatted string with stars
#==============================================================================

#------------------------------------------------------------------------------
# fast_wild_bootstrap()
# 
# Arguments:
#   model       - fixest model object (from feols/feglm)
#   param       - Character: name of coefficient to test (e.g., "texas_treated:post_1999")
#   B           - Integer: number of bootstrap replications (default 9999)
#   seed        - Integer: random seed for reproducibility (default 12345)
#   cluster_var - Character: name of cluster variable (default "state")
#
# Returns:
#   List with elements:
#     $p_val     - Two-sided p-value
#     $label     - Formatted p-value with significance stars
#     $beta_obs  - Point estimate of the coefficient
#     $boot_dist - Vector of bootstrapped coefficient values (length B)
#------------------------------------------------------------------------------
fast_wild_bootstrap <- function(model, param, B = 9999, seed = 12345, cluster_var = "state") {
  
  # 1. Setup
  set.seed(seed)
  
  # 2. Recover the exact data used in estimation (excluding NAs, singletons, etc.)
  #    fixest::fixest_data(..., sample = "estimation") handles obs_selection correctly.
  obs_data <- tryCatch({
    fixest::fixest_data(model, sample = "estimation")
  }, error = function(e) {
    # Fallback for older fixest versions or edge cases:
    # model$obs_selection is a list of REMOVED indices (negative logic)
    d <- model$data
    if (!is.null(model$obs_selection)) {
      d <- d[-unlist(model$obs_selection), ]
    }
    return(d)
  })
  
  # 3. Extract De-meaned X (Projected out Fixed Effects)
  X <- model.matrix(model, type = "rhs")
  
  # 4. Identify the parameter index
  if (!param %in% colnames(X)) {
    stop(sprintf("Parameter '%s' not found in model matrix. Available: %s",
                 param, paste(colnames(X), collapse = ", ")))
  }
  param_idx <- which(colnames(X) == param)
  
  # 5. Extract Residuals
  u_hat <- resid(model)
  
  # 6. Extract Cluster IDs
  if (!cluster_var %in% names(obs_data)) {
    stop(sprintf("Cluster variable '%s' not found in estimation data. Available: %s",
                 cluster_var, paste(names(obs_data), collapse = ", ")))
  }
  
  clusters <- as.character(obs_data[[cluster_var]])
  unique_clusters <- unique(clusters)
  G <- length(unique_clusters)
  
  # 7. VALIDATION CHECK - Critical for correctness
  if (length(clusters) != length(u_hat)) {
    stop(sprintf(
      "Length mismatch: Clusters (%d) vs Residuals (%d). fixest_data() failed to align.",
      length(clusters), length(u_hat)
    ))
  }
  
  # 8. Map rows to clusters for fast aggregation
  cluster_map <- match(clusters, unique_clusters)
  
  # 9. Pre-Calculate Bread: (X'X)^-1
  XtX_inv <- solve(crossprod(X))
  
  # 10. Pre-Calculate Scores: X_i * u_i
  scores_raw <- X * u_hat
  
  # 11. Aggregate Scores by Cluster: S_g = Sum(X_i * u_i) for i in g
  scores_cluster <- rowsum(scores_raw, cluster_map)
  
  # 12. The Matrix Trick: Weight Matrix (G x B)
  #     Rademacher weights: +1 or -1 with prob 0.5
  weights_mat <- matrix(sample(c(-1, 1), G * B, replace = TRUE), nrow = G, ncol = B)
  
  # 13. Calculate Bootstrapped Coefficients
  #     Beta_boot = Beta_hat + (X'X)^-1 * (Scores_Cluster_Transposed %*% Weights)
  delta_mat <- XtX_inv %*% t(scores_cluster) %*% weights_mat
  
  # 14. Extract distribution and P-Value
  beta_obs <- coef(model)[[param]]
  beta_boot_dist <- beta_obs + delta_mat[param_idx, ]
  
  dist_centered <- beta_boot_dist - beta_obs
  p_val <- mean(abs(dist_centered) >= abs(beta_obs))
  
  # 15. Format with significance stars
  stars <- ""
  if (p_val < 0.01) stars <- "***"
  else if (p_val < 0.05) stars <- "**"
  else if (p_val < 0.1) stars <- "*"
  
  return(list(
    p_val = p_val,
    label = sprintf("%.4f%s", p_val, stars),
    beta_obs = beta_obs,
    boot_dist = beta_boot_dist
  ))
}

#------------------------------------------------------------------------------
# get_unified_wcb()
#
# Extended version that returns both plotting data AND covariance matrix.
# Useful for HonestDiD sensitivity analysis with WCB-based standard errors.
#
# Arguments:
#   model - fixest model object with event study terms
#   label - Character: label for the outcome (used in plot_data)
#   B     - Number of bootstrap replications
#   seed  - Random seed
#   cluster_var - Name of cluster variable
#
# Returns:
#   List with:
#     $plot_data - data.table with event_date, estimate, std.error, conf.low, conf.high
#     $sigma     - Full WCB covariance matrix (named rows/cols match X columns)
#------------------------------------------------------------------------------
get_unified_wcb <- function(model, label, B = 9999, seed = 12345, cluster_var = "state") {
  set.seed(seed)
  
  # A. Setup Bootstrap Components
  obs_data <- tryCatch({
    fixest::fixest_data(model, sample = "estimation")
  }, error = function(e) {
    model$data[-unlist(model$obs_selection), ]
  })
  
  clusters <- as.character(obs_data[[cluster_var]])
  unique_clusters <- unique(clusters)
  G <- length(unique_clusters)
  
  X <- model.matrix(model, type = "rhs")
  u_hat <- resid(model)
  XtX_inv <- solve(crossprod(X))
  scores_raw <- X * u_hat
  
  cluster_map <- match(clusters, unique_clusters)
  scores_cluster <- rowsum(scores_raw, cluster_map)
  
  # B. Run Matrix Bootstrap
  cat(sprintf("Running Unified WCB Simulation for %s (B=%d)...\n", label, B))
  weights_mat <- matrix(sample(c(-1, 1), G * B, replace = TRUE), nrow = G, ncol = B)
  delta_mat <- XtX_inv %*% t(scores_cluster) %*% weights_mat
  
  # C. Calculate Covariance Matrix (For HonestDiD)
  sigma_wild <- cov(t(delta_mat))
  rownames(sigma_wild) <- colnames(X)
  colnames(sigma_wild) <- colnames(X)
  
  # D. Calculate Standard Errors
  se_wild <- sqrt(diag(sigma_wild))
  
  # E. Create Plotting Data for Event Study coefficients
  beta_obs <- coef(model)
  target_idx <- grep("event_date::.*:texas_treated", names(beta_obs))
  
  if (length(target_idx) == 0) {
    # Fallback: return NULL for plot_data if no event study terms
    return(list(plot_data = NULL, sigma = sigma_wild))
  }
  
  plot_dt <- data.table::data.table(
    term = names(beta_obs)[target_idx],
    estimate = beta_obs[target_idx],
    std.error = se_wild[target_idx]
  )
  
  # Add CIs and Date parsing
  plot_dt[, `:=`(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    event_date = as.Date(regmatches(term, regexpr("\\d{4}-\\d{2}-\\d{2}", term))),
    Outcome = label
  )]
  
  return(list(plot_data = plot_dt, sigma = sigma_wild))
}

cat("✓ Bootstrap helper loaded (fast_wild_bootstrap, get_unified_wcb)\n")


#' Bootstrap for triple interaction models
#' Returns: base effect, interaction, and SUM with proper WCB p-values
fast_wild_bootstrap_triple <- function(model, base_coef, interaction_coef, B = 999, 
                                        cluster_var = "state") {
  
  # Get observed coefficients
  beta_base <- coef(model)[base_coef]
  beta_interact <- coef(model)[interaction_coef]
  beta_sum <- beta_base + beta_interact  # Motor fuel full effect
  
  # Extract model components (same as fast_wild_bootstrap)
  model_data <- tryCatch(model$model_matrix_info, error = function(e) NULL)
  if (is.null(model_data)) {
    model_data <- tryCatch(fixest::fixest_data(model), error = function(e) NULL)
  }
  if (is.null(model_data)) {
    model_data <- tryCatch(model.frame(model), error = function(e) NULL)
  }
  
  if (is.null(model_data)) {
    warning("Could not extract model data for bootstrap")
    return(list(
      base = list(beta_obs = beta_base, p_val = NA),
      interact = list(beta_obs = beta_interact, p_val = NA),
      sum = list(beta_obs = beta_sum, p_val = NA)
    ))
  }
  
  # Get clusters
  clusters <- model_data[[cluster_var]]
  unique_clusters <- unique(clusters)
  G <- length(unique_clusters)
  
  # Get residuals and design matrix
  u_hat <- residuals(model)
  X <- model.matrix(model)
  
  # Ensure dimensions match
  if (length(u_hat) != nrow(X) || length(clusters) != length(u_hat)) {
    warning("Dimension mismatch in bootstrap data")
    return(list(
      base = list(beta_obs = beta_base, p_val = NA),
      interact = list(beta_obs = beta_interact, p_val = NA),
      sum = list(beta_obs = beta_sum, p_val = NA)
    ))
  }
  
  # Compute (X'X)^{-1}
  XtX_inv <- solve(crossprod(X))
  
  # Pre-compute cluster scores: X_g' * u_g for each cluster
  cluster_idx <- split(seq_along(clusters), clusters)
  scores_cluster <- do.call(rbind, lapply(cluster_idx, function(idx) {
    colSums(X[idx, , drop = FALSE] * u_hat[idx])
  }))
  
  # Coefficient indices
  base_idx <- which(colnames(X) == base_coef)
  interact_idx <- which(colnames(X) == interaction_coef)
  
  if (length(base_idx) == 0 || length(interact_idx) == 0) {
    warning("Could not find coefficients in design matrix")
    return(list(
      base = list(beta_obs = beta_base, p_val = NA),
      interact = list(beta_obs = beta_interact, p_val = NA),
      sum = list(beta_obs = beta_sum, p_val = NA)
    ))
  }
  
  # Generate Rademacher weights and compute bootstrap distribution
  set.seed(20250120 + 12345)
  weights_mat <- matrix(sample(c(-1, 1), G * B, replace = TRUE), nrow = G, ncol = B)
  
  # Bootstrap coefficients: delta = (X'X)^{-1} * sum_g(w_g * score_g)
  delta_mat <- XtX_inv %*% t(scores_cluster) %*% weights_mat
  
  # Extract bootstrap distributions
  delta_base <- delta_mat[base_idx, ]
  delta_interact <- delta_mat[interact_idx, ]
  delta_sum <- delta_base + delta_interact  # Sum distribution
  
  # Two-sided p-values (proportion of |t*| >= |t_obs|)
  t_base <- beta_base / sd(delta_base)
  t_interact <- beta_interact / sd(delta_interact)
  t_sum <- beta_sum / sd(delta_sum)
  
  t_star_base <- delta_base / sd(delta_base)
  t_star_interact <- delta_interact / sd(delta_interact)
  t_star_sum <- delta_sum / sd(delta_sum)
  
  p_base <- mean(abs(t_star_base) >= abs(t_base))
  p_interact <- mean(abs(t_star_interact) >= abs(t_interact))
  p_sum <- mean(abs(t_star_sum) >= abs(t_sum))
  
  return(list(
    base = list(
      beta_obs = beta_base,
      se_boot = sd(delta_base),
      p_val = p_base,
      label = "Non-Motor Fuel Effect"
    ),
    interact = list(
      beta_obs = beta_interact,
      se_boot = sd(delta_interact),
      p_val = p_interact,
      label = "MF Differential"
    ),
    sum = list(
      beta_obs = beta_sum,
      se_boot = sd(delta_sum),
      p_val = p_sum,
      label = "Motor Fuel Effect"
    )
  ))
}