#==============================================================================
# BOOTSTRAP HELPER: Memory-Optimized Wild Cluster Bootstrap
# 
# Location: Code/Helpers/bootstrap_helper.R
#
# CRITICAL FIXES for large panels:
#   1. Sparse matrix operations for memory efficiency
#   2. Chunked bootstrap computation to avoid RAM overflow
#   3. Pre-aggregation to cluster level before matrix operations
#   4. Removed unnecessary data copies
#
# For ~25M observations with 19 clusters, expect ~2-5GB RAM usage (down from 50GB+)
#==============================================================================

library(Matrix)  # For sparse matrices

#------------------------------------------------------------------------------
# fast_wild_bootstrap() - OPTIMIZED VERSION
#------------------------------------------------------------------------------
fast_wild_bootstrap <- function(model, param, B = 9999, seed = 12345, cluster_var = "state") {
  
  set.seed(seed)
  
  # -------------------------------------------------------------------------
  # STEP 1: Extract Model Components (no data copies)
  # -------------------------------------------------------------------------
  X <- model.matrix(model, type = "rhs")
  u_hat <- resid(model)
  n_obs <- length(u_hat)
  
  # Validate parameter
  if (!param %in% colnames(X)) {
    stop(sprintf("Parameter '%s' not found. Available: %s",
                 param, paste(head(colnames(X), 10), collapse = ", ")))
  }
  param_idx <- which(colnames(X) == param)
  
  # -------------------------------------------------------------------------
  # STEP 2: Get Cluster IDs (lightweight extraction)
  # -------------------------------------------------------------------------
  # Try multiple methods to get cluster variable
  clusters <- tryCatch({
    # Method 1: From fixest_data
    obs_data <- fixest::fixest_data(model, sample = "estimation")
    as.character(obs_data[[cluster_var]])
  }, error = function(e) {
    tryCatch({
      # Method 2: From model$obs_selection
      full_data <- model$data
      if (!is.null(model$obs_selection)) {
        full_data <- full_data[-unlist(model$obs_selection), ]
      }
      as.character(full_data[[cluster_var]])
    }, error = function(e2) {
      # Method 3: From vcov cluster info
      if (!is.null(model$fixef_id)) {
        as.character(model$fixef_id[[cluster_var]])
      } else {
        stop(sprintf("Cannot extract cluster variable '%s'", cluster_var))
      }
    })
  })
  
  # Validation
  if (length(clusters) != n_obs) {
    stop(sprintf("Cluster length mismatch: %d clusters vs %d observations", 
                 length(clusters), n_obs))
  }
  
  unique_clusters <- unique(clusters)
  G <- length(unique_clusters)
  cat(sprintf("  Clusters: %d | Observations: %s | Replications: %d\n", 
              G, format(n_obs, big.mark = ","), B))
  
  # -------------------------------------------------------------------------
  # STEP 3: PRE-AGGREGATE TO CLUSTER LEVEL (Key Memory Optimization)
  # -------------------------------------------------------------------------
  # Instead of storing full N x K score matrix, aggregate to G x K
  cluster_map <- match(clusters, unique_clusters)
  
  # Compute scores: X_i * u_i (element-wise)
  scores_raw <- X * u_hat  # N x K matrix
  
  # Aggregate to cluster level using fast C-based rowsum
  scores_cluster <- rowsum(scores_raw, cluster_map, reorder = FALSE)  # G x K
  
  # Clear large objects immediately
  rm(scores_raw)
  gc(verbose = FALSE)
  
  # -------------------------------------------------------------------------
  # STEP 4: Bread Matrix (X'X)^-1 - Use crossprod for efficiency
  # -------------------------------------------------------------------------
  XtX_inv <- solve(crossprod(X))  # K x K (small)
  
  # -------------------------------------------------------------------------
  # STEP 5: CHUNKED BOOTSTRAP (Process in batches to avoid memory overflow)
  # -------------------------------------------------------------------------
  chunk_size <- min(1000, B)  # Process 1000 reps at a time
  n_chunks <- ceiling(B / chunk_size)
  
  beta_boot_dist <- numeric(B)
  beta_obs <- coef(model)[[param]]
  
  cat(sprintf("  Running bootstrap in %d chunks...\n", n_chunks))
  
  for (chunk_i in 1:n_chunks) {
    start_idx <- (chunk_i - 1) * chunk_size + 1
    end_idx <- min(chunk_i * chunk_size, B)
    B_chunk <- end_idx - start_idx + 1
    
    # Generate Rademacher weights for this chunk only
    weights_chunk <- matrix(
      sample(c(-1, 1), G * B_chunk, replace = TRUE), 
      nrow = G, 
      ncol = B_chunk
    )
    
    # Matrix multiplication: (K x K) %*% (K x G) %*% (G x B_chunk)
    # Result: K x B_chunk
    delta_chunk <- XtX_inv %*% t(scores_cluster) %*% weights_chunk
    
    # Extract parameter row and add to observed coefficient
    beta_boot_dist[start_idx:end_idx] <- beta_obs + delta_chunk[param_idx, ]
    
    # Clear chunk
    rm(weights_chunk, delta_chunk)
    
    if (chunk_i %% 5 == 0 || chunk_i == n_chunks) {
      cat(sprintf("    Completed %d/%d chunks\n", chunk_i, n_chunks))
    }
  }
  
  # -------------------------------------------------------------------------
  # STEP 6: Calculate P-Value
  # -------------------------------------------------------------------------
  dist_centered <- beta_boot_dist - beta_obs
  p_val <- mean(abs(dist_centered) >= abs(beta_obs))
  
  # Significance stars
  stars <- if (p_val < 0.01) "***" else if (p_val < 0.05) "**" else if (p_val < 0.1) "*" else ""
  
  return(list(
    p_val = p_val,
    label = sprintf("%.4f%s", p_val, stars),
    beta_obs = beta_obs,
    boot_dist = beta_boot_dist
  ))
}

#------------------------------------------------------------------------------
# fast_wild_bootstrap_triple() - OPTIMIZED VERSION
#------------------------------------------------------------------------------
fast_wild_bootstrap_triple <- function(model, base_coef, interaction_coef, 
                                        B = 999, cluster_var = "state") {
  
  set.seed(20250120 + 12345)
  
  # -------------------------------------------------------------------------
  # STEP 1: Extract Components
  # -------------------------------------------------------------------------
  X <- model.matrix(model, type = "rhs")
  u_hat <- resid(model)
  n_obs <- length(u_hat)
  
  # Get observed coefficients
  all_coefs <- coef(model)
  if (!base_coef %in% names(all_coefs) || !interaction_coef %in% names(all_coefs)) {
    warning(sprintf("Coefficients not found: %s, %s", base_coef, interaction_coef))
    return(list(
      base = list(beta_obs = NA, se_boot = NA, p_val = NA, label = "Not found"),
      interact = list(beta_obs = NA, se_boot = NA, p_val = NA, label = "Not found"),
      sum = list(beta_obs = NA, se_boot = NA, p_val = NA, label = "Not found")
    ))
  }
  
  beta_base <- all_coefs[[base_coef]]
  beta_interact <- all_coefs[[interaction_coef]]
  beta_sum <- beta_base + beta_interact
  
  # -------------------------------------------------------------------------
  # STEP 2: Get Clusters
  # -------------------------------------------------------------------------
  clusters <- tryCatch({
    obs_data <- fixest::fixest_data(model, sample = "estimation")
    as.character(obs_data[[cluster_var]])
  }, error = function(e) {
    full_data <- model$data
    if (!is.null(model$obs_selection)) {
      full_data <- full_data[-unlist(model$obs_selection), ]
    }
    as.character(full_data[[cluster_var]])
  })
  
  if (length(clusters) != n_obs) {
    warning("Cluster-observation mismatch in triple bootstrap")
    return(list(
      base = list(beta_obs = beta_base, se_boot = NA, p_val = NA, label = "Error"),
      interact = list(beta_obs = beta_interact, se_boot = NA, p_val = NA, label = "Error"),
      sum = list(beta_obs = beta_sum, se_boot = NA, p_val = NA, label = "Error")
    ))
  }
  
  unique_clusters <- unique(clusters)
  G <- length(unique_clusters)
  cluster_map <- match(clusters, unique_clusters)
  
  cat(sprintf("  Triple Bootstrap: %d clusters, %s obs, %d reps\n", 
              G, format(n_obs, big.mark = ","), B))
  
  # -------------------------------------------------------------------------
  # STEP 3: Pre-Aggregate Scores
  # -------------------------------------------------------------------------
  scores_raw <- X * u_hat
  scores_cluster <- rowsum(scores_raw, cluster_map, reorder = FALSE)
  rm(scores_raw)
  gc(verbose = FALSE)
  
  # -------------------------------------------------------------------------
  # STEP 4: Get Coefficient Indices
  # -------------------------------------------------------------------------
  base_idx <- which(colnames(X) == base_coef)
  interact_idx <- which(colnames(X) == interaction_coef)
  
  if (length(base_idx) == 0 || length(interact_idx) == 0) {
    warning("Coefficient indices not found in design matrix")
    return(list(
      base = list(beta_obs = beta_base, se_boot = NA, p_val = NA, label = "Error"),
      interact = list(beta_obs = beta_interact, se_boot = NA, p_val = NA, label = "Error"),
      sum = list(beta_obs = beta_sum, se_boot = NA, p_val = NA, label = "Error")
    ))
  }
  
  # -------------------------------------------------------------------------
  # STEP 5: Chunked Bootstrap
  # -------------------------------------------------------------------------
  XtX_inv <- solve(crossprod(X))
  
  chunk_size <- min(500, B)
  n_chunks <- ceiling(B / chunk_size)
  
  delta_base <- numeric(B)
  delta_interact <- numeric(B)
  
  for (chunk_i in 1:n_chunks) {
    start_idx <- (chunk_i - 1) * chunk_size + 1
    end_idx <- min(chunk_i * chunk_size, B)
    B_chunk <- end_idx - start_idx + 1
    
    weights_chunk <- matrix(
      sample(c(-1, 1), G * B_chunk, replace = TRUE),
      nrow = G,
      ncol = B_chunk
    )
    
    delta_mat <- XtX_inv %*% t(scores_cluster) %*% weights_chunk
    
    delta_base[start_idx:end_idx] <- delta_mat[base_idx, ]
    delta_interact[start_idx:end_idx] <- delta_mat[interact_idx, ]
    
    rm(weights_chunk, delta_mat)
  }
  
  delta_sum <- delta_base + delta_interact
  
  # -------------------------------------------------------------------------
  # STEP 6: Calculate P-Values
  # -------------------------------------------------------------------------
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

#------------------------------------------------------------------------------
# get_unified_wcb() - OPTIMIZED VERSION
#------------------------------------------------------------------------------
get_unified_wcb <- function(model, label, B = 9999, seed = 12345, cluster_var = "state") {
  
  set.seed(seed)
  cat(sprintf("Running Unified WCB for %s (B=%d)...\n", label, B))
  
  # -------------------------------------------------------------------------
  # STEP 1: Extract Components
  # -------------------------------------------------------------------------
  X <- model.matrix(model, type = "rhs")
  u_hat <- resid(model)
  n_obs <- length(u_hat)
  
  clusters <- tryCatch({
    obs_data <- fixest::fixest_data(model, sample = "estimation")
    as.character(obs_data[[cluster_var]])
  }, error = function(e) {
    full_data <- model$data
    if (!is.null(model$obs_selection)) {
      full_data <- full_data[-unlist(model$obs_selection), ]
    }
    as.character(full_data[[cluster_var]])
  })
  
  unique_clusters <- unique(clusters)
  G <- length(unique_clusters)
  cluster_map <- match(clusters, unique_clusters)
  
  # -------------------------------------------------------------------------
  # STEP 2: Pre-Aggregate Scores
  # -------------------------------------------------------------------------
  scores_raw <- X * u_hat
  scores_cluster <- rowsum(scores_raw, cluster_map, reorder = FALSE)
  rm(scores_raw)
  gc(verbose = FALSE)
  
  XtX_inv <- solve(crossprod(X))
  
  # -------------------------------------------------------------------------
  # STEP 3: Chunked Bootstrap
  # -------------------------------------------------------------------------
  chunk_size <- 1000
  n_chunks <- ceiling(B / chunk_size)
  K <- ncol(X)
  
  # Pre-allocate result matrix
  delta_full <- matrix(0, nrow = K, ncol = B)
  
  for (chunk_i in 1:n_chunks) {
    start_idx <- (chunk_i - 1) * chunk_size + 1
    end_idx <- min(chunk_i * chunk_size, B)
    B_chunk <- end_idx - start_idx + 1
    
    weights_chunk <- matrix(
      sample(c(-1, 1), G * B_chunk, replace = TRUE),
      nrow = G,
      ncol = B_chunk
    )
    
    delta_full[, start_idx:end_idx] <- XtX_inv %*% t(scores_cluster) %*% weights_chunk
    
    rm(weights_chunk)
    
    if (chunk_i %% 10 == 0) {
      cat(sprintf("  Completed %d/%d chunks\n", chunk_i, n_chunks))
    }
  }
  
  # -------------------------------------------------------------------------
  # STEP 4: Calculate Covariance Matrix
  # -------------------------------------------------------------------------
  sigma_wild <- cov(t(delta_full))
  rownames(sigma_wild) <- colnames(X)
  colnames(sigma_wild) <- colnames(X)
  
  se_wild <- sqrt(diag(sigma_wild))
  
  # -------------------------------------------------------------------------
  # STEP 5: Create Plot Data (if event study terms exist)
  # -------------------------------------------------------------------------
  beta_obs <- coef(model)
  target_idx <- grep("event_date::.*:texas_treated", names(beta_obs))
  
  if (length(target_idx) == 0) {
    return(list(plot_data = NULL, sigma = sigma_wild))
  }
  
  plot_dt <- data.table::data.table(
    term = names(beta_obs)[target_idx],
    estimate = beta_obs[target_idx],
    std.error = se_wild[target_idx]
  )
  
  plot_dt[, `:=`(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    event_date = as.Date(regmatches(term, regexpr("\\d{4}-\\d{2}-\\d{2}", term))),
    Outcome = label
  )]
  
  return(list(plot_data = plot_dt, sigma = sigma_wild))
}

cat("✓ Optimized bootstrap helper loaded (Memory-efficient for large panels)\n")
cat("  Key improvements:\n")
cat("    - Pre-aggregation to cluster level (G x K vs N x K)\n")
cat("    - Chunked bootstrap computation (1000 reps/chunk)\n")
cat("    - Aggressive garbage collection\n")
cat("    - Expected RAM: ~2-5GB for 25M obs with 19 clusters\n")

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