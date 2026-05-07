# ==============================================================================
# 04_Replacement_MC.R
# ==============================================================================
# PURPOSE
#   Monte Carlo recovery test for the 4-parameter replacement model
#   (Option A — 3-action multinomial logit). Mirrors 03_Model_B_Binary_test.R.
#
#   Stages:
#     1. Single fit sanity check (one synthetic draw).
#     2. Likelihood surface slices (kappa_exit vs K, K vs gamma_price).
#     3. Monte Carlo recovery (N_sim replications) + Hessian condition number.
#     4. Diagnostic figures (histograms, joint scatter) and CSV stats.
#
# OUTPUTS
#   Output/Figures/04_Replacement_MC_Surface.png
#   Output/Figures/04_Replacement_MC_Recovery.png
#   Output/Tables/04_Replacement_MC_Stats.csv
#   Output/Tables/04_Replacement_MC_Raw.csv
# ==============================================================================

# Allow this script to find packages installed under the parent project's
# renv library when run from a git worktree (where renv is not auto-activated).
.PARENT_RENV_LIB <- "C:/Users/kaleb/Documents/ust_ins_move_to_github/renv/library/windows/R-4.5/x86_64-w64-mingw32"
if (dir.exists(.PARENT_RENV_LIB) && !(.PARENT_RENV_LIB %in% .libPaths())) {
  .libPaths(c(.PARENT_RENV_LIB, .libPaths()))
}

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(here)
  library(numDeriv)
  library(ggplot2)
  library(gridExtra)
  library(foreach)
  library(doParallel)
})

source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))

FIG_DIR <- here::here("Output", "Figures")
TAB_DIR <- here::here("Output", "Tables")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# 1. Single-fit sanity
# ------------------------------------------------------------------------------
single_fit_sanity <- function(theta_true, N_facilities = 2000L,
                              T_periods = 30L, seed = 2025L) {

  cat("\n[Stage 1] Single-fit sanity check\n")
  cat("---------------------------------\n")

  data <- generate_replacement_data(
    N_facilities = N_facilities, T_periods = T_periods,
    theta_true   = theta_true, seed = seed
  )

  # Quick sample diagnostics
  ob <- data$obs_panel
  cat(sprintf("  obs = %s | facilities = %s\n",
              format(nrow(ob),         big.mark = ","),
              format(uniqueN(ob$panel_id), big.mark = ",")))
  cat(sprintf("  Maintain share = %.2f%% | Exit share = %.2f%% | Replace share = %.2f%%\n",
              100 * mean(ob$y_it == 0L),
              100 * mean(ob$y_it == 1L & ob$I_replace == 0L, na.rm = TRUE),
              100 * mean(ob$y_it == 1L & ob$I_replace == 1L, na.rm = TRUE)))

  est <- npl_estimator_replacement(
    obs_panel  = data$obs_panel,
    primitives = data$primitives,
    config     = data$config,
    theta_init = c(kappa_exit = theta_true[["kappa_exit"]] * 0.7,
                   K_log      = theta_true[["K_log"]]      + 0.5,
                   gamma_price = theta_true[["gamma_price"]] * 0.7,
                   gamma_risk  = theta_true[["gamma_risk"]]  * 1.3),
    verbose    = TRUE
  )

  K_true <- exp(theta_true[["K_log"]])
  cat(sprintf(
    "\n  Truth:    kappa_exit=%.3f  K=%.3f  gamma_price=%.3f  gamma_risk=%.3f\n",
    theta_true[["kappa_exit"]], K_true,
    theta_true[["gamma_price"]], theta_true[["gamma_risk"]]))
  cat(sprintf(
    "  Estimate: kappa_exit=%.3f  K=%.3f  gamma_price=%.3f  gamma_risk=%.3f\n",
    est$theta_hat[["kappa_exit"]], est$theta_hat[["K"]],
    est$theta_hat[["gamma_price"]], est$theta_hat[["gamma_risk"]]))
  cat(sprintf("  CCP max abs error vs truth: %.4f\n",
              max(abs(est$P_hat - data$P_true))))

  invisible(list(data = data, est = est))
}


# ------------------------------------------------------------------------------
# 2. Likelihood surface slices
# ------------------------------------------------------------------------------
plot_replacement_surface <- function(theta_true, fig_dir = FIG_DIR,
                                     N_facilities = 2000L, T_periods = 30L,
                                     seed = 2025L, n_grid = 18L) {

  cat("\n[Stage 2] Likelihood surface slices\n")
  cat("-----------------------------------\n")

  data <- generate_replacement_data(
    N_facilities = N_facilities, T_periods = T_periods,
    theta_true   = theta_true, seed = seed
  )
  cache  <- create_estimation_cache_replacement(data$primitives,
                                                data$obs_panel, data$config)
  P_emp  <- make_P0_mat_3action(data$primitives$P0_mat, data$obs_panel,
                                eps_prob = data$config$eps_prob)

  K_true <- exp(theta_true[["K_log"]])
  ke_true <- theta_true[["kappa_exit"]]
  gp_true <- theta_true[["gamma_price"]]
  gr_true <- theta_true[["gamma_risk"]]

  # --- Slice A: kappa_exit vs K (fix gamma's at truth) ---
  cat("  Slice A: kappa_exit vs K\n")
  grid_a <- expand.grid(
    kappa_exit = seq(max(0.5 * ke_true, ke_true - 10),
                     ke_true + 10, length.out = n_grid),
    K          = seq(max(0.3 * K_true, 1),
                     K_true * 2.5, length.out = n_grid)
  )
  grid_a$ll <- NA_real_
  for (i in seq_len(nrow(grid_a))) {
    th <- c(kappa_exit = grid_a$kappa_exit[i],
            K_log      = log(grid_a$K[i]),
            gamma_price = gp_true,
            gamma_risk  = gr_true)
    grid_a$ll[i] <- -npl_likelihood_replacement(th, P_emp, cache, data$config)
  }

  p1 <- ggplot(grid_a, aes(x = kappa_exit, y = K, z = ll)) +
    geom_contour_filled(bins = 18) +
    annotate("point", x = ke_true, y = K_true, color = "red",
             shape = 4, size = 5, stroke = 2) +
    labs(title = "Slice A: kappa_exit vs K",
         x = "kappa_exit (scrap value at exit)",
         y = "K (replacement cost)") +
    theme_minimal() + theme(legend.position = "none")

  # --- Slice B: K vs gamma_price (fix kappa, gamma_risk at truth) ---
  cat("  Slice B: K vs gamma_price\n")
  grid_b <- expand.grid(
    K  = seq(max(0.3 * K_true, 1), K_true * 2.5, length.out = n_grid),
    gp = seq(gp_true - 1.0, min(gp_true + 1.0, 2.0), length.out = n_grid)
  )
  grid_b$ll <- NA_real_
  for (i in seq_len(nrow(grid_b))) {
    th <- c(kappa_exit = ke_true,
            K_log      = log(grid_b$K[i]),
            gamma_price = grid_b$gp[i],
            gamma_risk  = gr_true)
    grid_b$ll[i] <- -npl_likelihood_replacement(th, P_emp, cache, data$config)
  }

  p2 <- ggplot(grid_b, aes(x = K, y = gp, z = ll)) +
    geom_contour_filled(bins = 18) +
    annotate("point", x = K_true, y = gp_true, color = "red",
             shape = 4, size = 5, stroke = 2) +
    labs(title = "Slice B: K vs gamma_price",
         x = "K (replacement cost)",
         y = "gamma_price") +
    theme_minimal() + theme(legend.position = "none")

  outfile <- file.path(fig_dir, "04_Replacement_MC_Surface.png")
  ggsave(outfile, arrangeGrob(p1, p2, ncol = 2), width = 12, height = 6)
  cat(sprintf("  saved: %s\n", outfile))

  invisible(list(grid_a = grid_a, grid_b = grid_b))
}


# ------------------------------------------------------------------------------
# 3. Monte Carlo recovery
# ------------------------------------------------------------------------------
run_replacement_mc <- function(N_sim = 100,
                               run_parallel = TRUE,
                               n_cores = max(1, parallel::detectCores() %/% 2),
                               theta_true = c(kappa_exit = 22,
                                              K_log = log(15),
                                              gamma_price = -1.0,
                                              gamma_risk  = 0.6),
                               N_facilities = 2000L,
                               T_periods    = 30L) {

  cat(sprintf("\n[Stage 3] Monte Carlo recovery (%d sims, parallel=%s, cores=%d)\n",
              N_sim, run_parallel, n_cores))
  cat("--------------------------------------------------------\n")

  if (run_parallel) {
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    parent_lib <- .PARENT_RENV_LIB
    clusterCall(cl, function(lp) {
      if (dir.exists(lp) && !(lp %in% .libPaths())) {
        .libPaths(c(lp, .libPaths()))
      }
    }, parent_lib)
    clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(data.table); library(Matrix); library(numDeriv)
      })
    })
    clusterExport(cl, varlist = c(
      "build_state_lut_replacement", "build_reset_state_lut",
      "build_F_replace", "create_estimation_config_replacement",
      "create_estimation_cache_replacement", "make_P0_mat_3action",
      "flow_utilities_replacement", "invert_value_function_replacement",
      "update_ccps_replacement", "npl_likelihood_replacement",
      "solve_equilibrium_policy_replacement",
      "npl_estimator_replacement", "generate_replacement_data"),
      envir = environment())
  }

  `%op%` <- if (run_parallel) `%dopar%` else `%do%`

  t_start <- Sys.time()
  raw_list <- foreach(i = 1:N_sim,
                      .packages = c("data.table", "Matrix", "numDeriv"),
                      .errorhandling = "pass") %op% {

    sim <- generate_replacement_data(
      N_facilities = N_facilities, T_periods = T_periods,
      theta_true   = theta_true, seed = 1000L + i
    )

    theta_start <- c(
      kappa_exit  = theta_true[["kappa_exit"]]  * 0.8,
      K_log       = theta_true[["K_log"]]       + 0.3,
      gamma_price = theta_true[["gamma_price"]] * 0.8,
      gamma_risk  = theta_true[["gamma_risk"]]  * 1.2
    )

    est <- npl_estimator_replacement(
      obs_panel  = sim$obs_panel, primitives = sim$primitives,
      config     = sim$config,    theta_init = theta_start,
      verbose    = FALSE
    )

    # Hessian / condition number on the FINAL pseudo-likelihood. Wrap in
    # tryCatch so a Hessian failure doesn't drop the whole replication —
    # we still want the point estimate + paths.
    H <- tryCatch(
      hessian(function(th) {
        names(th) <- c("kappa_exit","K_log","gamma_price","gamma_risk")
        npl_likelihood_replacement(th, est$P_hat, est$cache, est$config)
      }, est$theta_raw),
      error = function(e) matrix(NA_real_, 4L, 4L))

    cond_num <- NA_real_
    if (!any(is.na(H))) {
      eg <- eigen(H, only.values = TRUE)$values
      m  <- min(abs(eg))
      if (m > 1e-10) cond_num <- max(abs(eg)) / m
    }

    summary_row <- data.table(
      rep         = i,
      kappa_exit_est  = est$theta_hat[["kappa_exit"]],
      K_est           = est$theta_hat[["K"]],
      K_log_est       = est$theta_raw[["K_log"]],
      gamma_price_est = est$theta_hat[["gamma_price"]],
      gamma_risk_est  = est$theta_hat[["gamma_risk"]],

      kappa_exit_true  = theta_true[["kappa_exit"]],
      K_true           = exp(theta_true[["K_log"]]),
      gamma_price_true = theta_true[["gamma_price"]],
      gamma_risk_true  = theta_true[["gamma_risk"]],

      converged = est$converged,
      n_iter    = est$n_iter,
      log_lik   = est$log_likelihood,
      cond_num  = cond_num,

      n_obs       = nrow(sim$obs_panel),
      maintain_sh = mean(sim$obs_panel$y_it == 0L),
      exit_sh     = mean(sim$obs_panel$y_it == 1L &
                         sim$obs_panel$I_replace == 0L, na.rm = TRUE),
      replace_sh  = mean(sim$obs_panel$y_it == 1L &
                         sim$obs_panel$I_replace == 1L, na.rm = TRUE)
    )

    # Return both summary row and full iteration paths so we can
    # diagnose convergence behavior after the fact.
    list(
      summary    = summary_row,
      theta_path = est$theta_path,
      ll_path    = est$ll_path
    )
  }
  if (run_parallel) stopCluster(cl)
  cat(sprintf("  elapsed: %.1f sec\n",
              as.numeric(difftime(Sys.time(), t_start, units = "secs"))))

  # Drop reps that errored (foreach .errorhandling = "pass" wraps errors
  # as the rep's value; valid reps are the named-list shape).
  ok <- vapply(raw_list, function(x) is.list(x) && !is.null(x$summary),
               logical(1))
  if (any(!ok)) {
    cat(sprintf("  reps that errored: %d / %d\n", sum(!ok), length(raw_list)))
  }
  raw_list <- raw_list[ok]
  results  <- rbindlist(lapply(raw_list, `[[`, "summary"))
  paths    <- list(
    theta_paths = lapply(raw_list, `[[`, "theta_path"),
    ll_paths    = lapply(raw_list, `[[`, "ll_path"),
    rep_ids     = results$rep
  )
  attr(results, "paths") <- paths
  results
}


# ------------------------------------------------------------------------------
# 4. Reporting
# ------------------------------------------------------------------------------
report_replacement_mc <- function(results, fig_dir = FIG_DIR,
                                  tab_dir = TAB_DIR) {

  cat("\n[Stage 4] Diagnostic report\n")
  cat("---------------------------\n")

  valid <- results[converged == TRUE]
  cat(sprintf("  converged runs: %d / %d\n", nrow(valid), nrow(results)))
  cat(sprintf("  Hessian succeeded on: %d / %d converged\n",
              sum(!is.na(valid$cond_num)), nrow(valid)))
  if (nrow(valid) == 0) {
    cat("  no converged runs — nothing to report\n")
    return(invisible(NULL))
  }

  ke_true <- valid$kappa_exit_true [1]
  K_true  <- valid$K_true          [1]
  gp_true <- valid$gamma_price_true[1]
  gr_true <- valid$gamma_risk_true [1]

  # ---- Pairwise correlation matrix across reps ----
  cor_mat <- cor(valid[, .(kappa_exit_est, K_est, gamma_price_est,
                           gamma_risk_est)])
  cat("\n  Pairwise correlation of estimates across reps:\n")
  print(round(cor_mat, 3))

  thm <- theme_minimal(base_size = 10)
  vline_truth <- function(x) geom_vline(xintercept = x, color = "red",
                                        linetype = "dashed", linewidth = 0.8)

  p1 <- ggplot(valid, aes(x = kappa_exit_est)) +
    geom_histogram(bins = 18, fill = "steelblue", color = "white", alpha = 0.7) +
    vline_truth(ke_true) + thm +
    labs(title = "kappa_exit recovery", x = "estimate", y = "count")

  p2 <- ggplot(valid, aes(x = K_est)) +
    geom_histogram(bins = 18, fill = "darkorange", color = "white", alpha = 0.7) +
    vline_truth(K_true) + thm +
    labs(title = "K (replacement cost) recovery", x = "estimate", y = "count")

  p3 <- ggplot(valid, aes(x = gamma_price_est)) +
    geom_histogram(bins = 18, fill = "forestgreen", color = "white", alpha = 0.7) +
    vline_truth(gp_true) + thm +
    labs(title = "gamma_price recovery", x = "estimate", y = "count")

  p4 <- ggplot(valid, aes(x = gamma_risk_est)) +
    geom_histogram(bins = 18, fill = "firebrick", color = "white", alpha = 0.7) +
    vline_truth(gr_true) + thm +
    labs(title = "gamma_risk recovery", x = "estimate", y = "count")

  truth_df <- data.frame(x = ke_true, y = K_true)
  p5 <- ggplot(valid, aes(x = kappa_exit_est, y = K_est)) +
    geom_point(alpha = 0.55, size = 2) +
    geom_point(data = truth_df, aes(x = x, y = y),
               color = "red", shape = 4, size = 5, stroke = 2,
               inherit.aes = FALSE) +
    thm +
    labs(title = "Joint: kappa_exit vs K",
         subtitle = "Red X = truth",
         x = "kappa_exit", y = "K")

  truth_df2 <- data.frame(x = gp_true, y = gr_true)
  p6 <- ggplot(valid, aes(x = gamma_price_est, y = gamma_risk_est)) +
    geom_point(alpha = 0.55, size = 2) +
    geom_point(data = truth_df2, aes(x = x, y = y),
               color = "red", shape = 4, size = 5, stroke = 2,
               inherit.aes = FALSE) +
    thm +
    labs(title = "Joint: gamma_price vs gamma_risk",
         x = "gamma_price", y = "gamma_risk")

  outfile <- file.path(fig_dir, "04_Replacement_MC_Recovery.png")
  ggsave(outfile,
         arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 2),
         width = 11, height = 12)
  cat(sprintf("  saved figure: %s\n", outfile))

  # ---- Iteration-trace plot (theta paths + LL paths across reps) ----
  paths <- attr(results, "paths")
  if (!is.null(paths) && length(paths$theta_paths) > 0L) {
    rep_ids <- paths$rep_ids
    n_keep  <- min(40L, length(rep_ids))    # show up to 40 reps
    keep_idx <- seq_len(n_keep)

    trace_dt <- rbindlist(lapply(keep_idx, function(k) {
      tp <- paths$theta_paths[[k]]
      lp <- paths$ll_paths   [[k]]
      if (is.null(tp) || nrow(tp) == 0) return(NULL)
      data.table(
        rep        = rep_ids[k],
        iter       = seq_len(nrow(tp)),
        kappa_exit = tp[, "kappa_exit"],
        K          = exp(tp[, "K_log"]),
        gamma_price = tp[, "gamma_price"],
        gamma_risk  = tp[, "gamma_risk"],
        ll          = lp[seq_len(nrow(tp))]
      )
    }), fill = TRUE)

    trace_long <- melt(trace_dt,
                       id.vars = c("rep", "iter"),
                       measure.vars = c("kappa_exit", "K",
                                        "gamma_price", "gamma_risk", "ll"),
                       variable.name = "param", value.name = "value")
    truth_df3 <- data.table(
      param = factor(c("kappa_exit", "K", "gamma_price", "gamma_risk"),
                     levels = levels(trace_long$param)),
      truth = c(ke_true, K_true, gp_true, gr_true)
    )

    p_trace <- ggplot(trace_long, aes(x = iter, y = value, group = rep)) +
      geom_line(alpha = 0.25, color = "steelblue") +
      geom_hline(data = truth_df3, aes(yintercept = truth),
                 color = "red", linetype = "dashed") +
      facet_wrap(~ param, scales = "free_y", ncol = 1) +
      thm +
      labs(title = "NPL outer-loop iteration paths (one line per rep)",
           subtitle = sprintf("Showing %d of %d reps; red dashed = truth (LL has no truth)",
                              n_keep, length(rep_ids)),
           x = "NPL outer iteration", y = "value")

    outfile_tr <- file.path(fig_dir, "04_Replacement_MC_Traces.png")
    ggsave(outfile_tr, p_trace, width = 9, height = 12)
    cat(sprintf("  saved iteration traces: %s\n", outfile_tr))
  }

  stats <- valid[, .(
    N = .N,

    Mean_kappa = mean(kappa_exit_est),
    Bias_kappa = mean(kappa_exit_est) - ke_true,
    SD_kappa   = sd(kappa_exit_est),
    RMSE_kappa = sqrt(mean((kappa_exit_est - ke_true)^2)),

    Mean_K     = mean(K_est),
    Bias_K     = mean(K_est) - K_true,
    SD_K       = sd(K_est),
    RMSE_K     = sqrt(mean((K_est - K_true)^2)),

    Mean_gp    = mean(gamma_price_est),
    Bias_gp    = mean(gamma_price_est) - gp_true,
    SD_gp      = sd(gamma_price_est),
    RMSE_gp    = sqrt(mean((gamma_price_est - gp_true)^2)),

    Mean_gr    = mean(gamma_risk_est),
    Bias_gr    = mean(gamma_risk_est) - gr_true,
    SD_gr      = sd(gamma_risk_est),
    RMSE_gr    = sqrt(mean((gamma_risk_est - gr_true)^2)),

    Avg_cond_num = mean(cond_num, na.rm = TRUE),
    Med_cond_num = median(cond_num, na.rm = TRUE),

    Avg_n_obs       = mean(n_obs),
    Avg_maintain_sh = mean(maintain_sh),
    Avg_exit_sh     = mean(exit_sh),
    Avg_replace_sh  = mean(replace_sh)
  )]

  print(stats)

  fwrite(stats,  file.path(tab_dir, "04_Replacement_MC_Stats.csv"))
  fwrite(valid,  file.path(tab_dir, "04_Replacement_MC_Raw.csv"))
  cat(sprintf("  saved tables: %s, %s\n",
              file.path(tab_dir, "04_Replacement_MC_Stats.csv"),
              file.path(tab_dir, "04_Replacement_MC_Raw.csv")))

  # Save full iteration paths + correlation matrix to RDS for later inspection.
  paths <- attr(results, "paths")
  bundle <- list(
    stats   = stats,
    raw     = valid,
    paths   = paths,
    cor_mat = cor_mat
  )
  rds_path <- file.path(tab_dir, "04_Replacement_MC_Bundle.rds")
  saveRDS(bundle, rds_path)
  cat(sprintf("  saved RDS bundle (paths + cor_mat): %s\n", rds_path))

  invisible(stats)
}


# ==============================================================================
# EXECUTION BLOCK
# ==============================================================================
if (sys.nframe() == 0) {

  # Truth chosen to give a realistic 3-action mix: ~85% Maintain, ~14% Exit,
  # ~1.2% Replace (matches the real-data closure composition where Exit
  # dominates and Replace is the thinner margin — worst-case identification
  # for K, by design).
  THETA_TRUE <- c(kappa_exit  = 15,
                  K_log       = log(6),
                  gamma_price = -1.0,
                  gamma_risk  = 0.6)

  N_FAC <- 2000L
  T_P   <- 30L

  # Stage 1: single-fit sanity
  single_fit_sanity(THETA_TRUE, N_facilities = N_FAC, T_periods = T_P)

  # Stage 2: likelihood surface
  plot_replacement_surface(THETA_TRUE, N_facilities = N_FAC, T_periods = T_P,
                           n_grid = 18L)

  # Stage 3: Monte Carlo
  mc <- run_replacement_mc(
    N_sim         = 100,
    run_parallel  = TRUE,
    n_cores       = max(1, parallel::detectCores() %/% 2),
    theta_true    = THETA_TRUE,
    N_facilities  = N_FAC,
    T_periods     = T_P
  )

  # Stage 4: report
  report_replacement_mc(mc)
}
