# ==============================================================================
# 04b_Replacement_MC_Robustness.R
# ==============================================================================
# PURPOSE
#   Comprehensive robustness battery for the Option-A replacement-model
#   estimator. The original 04_Replacement_MC.R tested a single truth with
#   a single (warm) starting point — useful for a smoke check, not for
#   defending the estimator. This script adds:
#
#     (1) Truth grid       — 5 truth vectors spanning identification regimes
#                            (Replace share ~6% down to ~0.6%).
#     (2) Multi-start      — 3 starts per truth (warm, mid, cold).
#                            Detects local optima and weak global identification.
#     (3) Sample-size scan — recovery at N_facilities ∈ {1000, 2000, 5000, 20000}
#                            at one central truth, one warm start. Verifies
#                            sqrt(n) scaling to project SE onto the real-data
#                            sample size.
#
# OUTPUTS
#   Output/Figures/04b_Robustness_Truth_x_Start.png
#   Output/Figures/04b_Robustness_Sample_Size.png
#   Output/Figures/04b_Robustness_CorrByTruth.png
#   Output/Tables/04b_Robustness_Raw.csv
#   Output/Tables/04b_Robustness_Summary.csv
#   Output/Tables/04b_Robustness_Bundle.rds
# ==============================================================================

# Allow this script to find packages installed under the parent project's renv
.PARENT_RENV_LIB <- "C:/Users/kaleb/Documents/ust_ins_move_to_github/renv/library/windows/R-4.5/x86_64-w64-mingw32"
if (dir.exists(.PARENT_RENV_LIB) && !(.PARENT_RENV_LIB %in% .libPaths())) {
  .libPaths(c(.PARENT_RENV_LIB, .libPaths()))
}

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(here)
  library(ggplot2)
  library(gridExtra)
  library(foreach)
  library(doParallel)
  library(numDeriv)
})

source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))

FIG_DIR <- here::here("Output", "Figures")
TAB_DIR <- here::here("Output", "Tables")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# 1. Truth grid + start menu
# ==============================================================================
TRUTHS <- list(
  truth1 = c(kappa_exit =  8, K_log = log(4),  gamma_price = -1.0, gamma_risk = 0.6),
  truth2 = c(kappa_exit = 10, K_log = log(5),  gamma_price = -1.0, gamma_risk = 0.6),
  truth3 = c(kappa_exit = 12, K_log = log(6),  gamma_price = -1.0, gamma_risk = 0.6),
  truth4 = c(kappa_exit = 15, K_log = log(6),  gamma_price = -1.0, gamma_risk = 0.6),
  truth5 = c(kappa_exit = 15, K_log = log(8),  gamma_price = -1.0, gamma_risk = 0.6)
)

# Starting-point strategies. "warm" is close to truth (current default).
# "mid" is a fixed economically-sensible interior point. "cold" draws random
# within sensible economic ranges on each rep — tests global identification.
START_RANGES <- list(
  cold_kappa = c(-10, 50),
  cold_K     = c(  1, 50),     # natural scale; converted to K_log
  cold_gp    = c( -5,  1),
  cold_gr    = c( -1,  3)
)

build_starts <- function(theta_true, rep_seed, ranges = START_RANGES) {
  set.seed(rep_seed)
  cold <- c(
    kappa_exit  = runif(1, ranges$cold_kappa[1], ranges$cold_kappa[2]),
    K_log       = log(runif(1, ranges$cold_K[1], ranges$cold_K[2])),
    gamma_price = runif(1, ranges$cold_gp[1], ranges$cold_gp[2]),
    gamma_risk  = runif(1, ranges$cold_gr[1], ranges$cold_gr[2])
  )
  list(
    warm = c(kappa_exit  = theta_true[["kappa_exit"]]  * 0.8,
             K_log       = theta_true[["K_log"]]       + 0.3,
             gamma_price = theta_true[["gamma_price"]] * 0.8,
             gamma_risk  = theta_true[["gamma_risk"]]  * 1.2),
    mid  = c(kappa_exit  = 10,
             K_log       = log(10),
             gamma_price = -1.0,
             gamma_risk  = 1.0),
    cold = cold
  )
}


# ==============================================================================
# 2. Pre-flight: equilibrium shares per truth (sanity)
# ==============================================================================
preflight_shares <- function(truths, N_facilities = 800L, T_periods = 30L) {
  cat("[Pre-flight] Equilibrium action shares per truth\n")
  out <- rbindlist(lapply(names(truths), function(nm) {
    th <- truths[[nm]]
    sim <- generate_replacement_data(N_facilities = N_facilities,
                                     T_periods = T_periods,
                                     theta_true = th, seed = 9999L)
    ob <- sim$obs_panel
    data.table(
      label    = nm,
      kappa    = th[["kappa_exit"]],
      K        = exp(th[["K_log"]]),
      maintain = mean(ob$y_it == 0L),
      exit     = mean(ob$y_it == 1L & ob$I_replace == 0L, na.rm = TRUE),
      replace  = mean(ob$y_it == 1L & ob$I_replace == 1L, na.rm = TRUE),
      n_obs    = nrow(ob)
    )
  }))
  print(out)
  out
}


# ==============================================================================
# 3. Cluster setup (shared by truth-grid and sample-size runs)
# ==============================================================================
.start_cluster <- function(n_cores) {
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
    # Estimator + generator (from improved_estimator_OPTIMIZED.r)
    "build_state_lut_replacement", "build_reset_state_lut",
    "build_F_replace", "create_estimation_config_replacement",
    "create_estimation_cache_replacement", "make_P0_mat_3action",
    "flow_utilities_replacement", "invert_value_function_replacement",
    "update_ccps_replacement", "npl_likelihood_replacement",
    "solve_equilibrium_policy_replacement",
    "npl_estimator_replacement", "generate_replacement_data",
    # Helpers from THIS script (without these workers can't run a cell)
    "estimate_one", "build_starts", "TRUTHS", "START_RANGES"),
    envir = globalenv())
  cl
}


# ==============================================================================
# 4. Single-cell estimation worker (truth, start, seed) -> data.table row
# ==============================================================================
estimate_one <- function(theta_true, theta_start, seed,
                         N_facilities, T_periods, config) {
  sim <- generate_replacement_data(
    N_facilities = N_facilities, T_periods = T_periods,
    theta_true   = theta_true,   seed = seed,
    config       = config
  )
  est <- npl_estimator_replacement(
    obs_panel  = sim$obs_panel, primitives = sim$primitives,
    config     = sim$config,    theta_init = theta_start,
    verbose    = FALSE
  )
  data.table(
    seed            = seed,
    N_facilities    = N_facilities,
    T_periods       = T_periods,
    n_obs           = nrow(sim$obs_panel),

    kappa_start     = theta_start[["kappa_exit"]],
    K_log_start     = theta_start[["K_log"]],
    gp_start        = theta_start[["gamma_price"]],
    gr_start        = theta_start[["gamma_risk"]],

    kappa_true      = theta_true[["kappa_exit"]],
    K_true          = exp(theta_true[["K_log"]]),
    gp_true         = theta_true[["gamma_price"]],
    gr_true         = theta_true[["gamma_risk"]],

    kappa_est       = est$theta_hat[["kappa_exit"]],
    K_est           = est$theta_hat[["K"]],
    K_log_est       = est$theta_raw[["K_log"]],
    gp_est          = est$theta_hat[["gamma_price"]],
    gr_est          = est$theta_hat[["gamma_risk"]],

    converged       = est$converged,
    n_iter          = est$n_iter,
    log_lik         = est$log_likelihood,

    maintain_sh     = mean(sim$obs_panel$y_it == 0L),
    exit_sh         = mean(sim$obs_panel$y_it == 1L &
                           sim$obs_panel$I_replace == 0L, na.rm = TRUE),
    replace_sh      = mean(sim$obs_panel$y_it == 1L &
                           sim$obs_panel$I_replace == 1L, na.rm = TRUE)
  )
}


# ==============================================================================
# 5. Truth-grid x multi-start
# ==============================================================================
run_truth_start_grid <- function(truths, N_sim = 200L,
                                 N_facilities = 2000L, T_periods = 30L,
                                 n_cores = max(1, parallel::detectCores() %/% 2)) {

  cat(sprintf("\n[Robustness] Truth-grid x multi-start (%d truths x 3 starts x %d reps)\n",
              length(truths), N_sim))
  cat(sprintf("            N_facilities=%d, cores=%d\n", N_facilities, n_cores))

  cl <- .start_cluster(n_cores)
  on.exit(stopCluster(cl), add = TRUE)
  config <- create_estimation_config_replacement(beta = 0.95, sigma2 = 1.0,
                                                 npl_iter = 200)

  cells <- expand.grid(
    truth_label = names(truths),
    start_label = c("warm", "mid", "cold"),
    rep         = seq_len(N_sim),
    stringsAsFactors = FALSE
  )
  cat(sprintf("            total cells: %d\n", nrow(cells)))

  t_start <- Sys.time()
  raw_list <- foreach(k = seq_len(nrow(cells)),
                      .packages = c("data.table", "Matrix"),
                      .errorhandling = "pass") %dopar% {
    truth_label <- cells$truth_label[k]
    start_label <- cells$start_label[k]
    rep_id      <- cells$rep[k]
    theta_true  <- truths[[truth_label]]
    starts      <- build_starts(theta_true, rep_seed = 50000L + rep_id)
    theta_start <- starts[[start_label]]
    out <- tryCatch(
      estimate_one(theta_true, theta_start, seed = 1000L + rep_id,
                   N_facilities = N_facilities, T_periods = T_periods,
                   config = config),
      error = function(e) NULL
    )
    if (is.null(out)) return(NULL)
    out[, `:=`(truth_label = truth_label, start_label = start_label,
               rep = rep_id)]
    out
  }
  ok      <- vapply(raw_list, function(x) is.data.table(x), logical(1))
  results <- rbindlist(raw_list[ok], fill = TRUE)
  cat(sprintf("            elapsed: %.1f sec | OK %d / %d (failed=%d)\n",
              as.numeric(difftime(Sys.time(), t_start, units = "secs")),
              sum(ok), length(raw_list), sum(!ok)))
  if (sum(!ok) > 0L) {
    fail_idx <- which(!ok)
    cat("            first failure messages (up to 3):\n")
    for (i in head(fail_idx, 3L)) {
      msg <- if (inherits(raw_list[[i]], "condition"))
                conditionMessage(raw_list[[i]]) else "(non-condition NULL)"
      cat(sprintf("              cell %d: %s\n", i, msg))
    }
  }
  results
}


# ==============================================================================
# 6. Sample-size scan (1 truth x 1 start x N_fac grid)
# ==============================================================================
run_sample_size_scan <- function(theta_true, N_grid = c(1000L, 2000L, 5000L, 20000L),
                                 N_sim = 100L, T_periods = 30L,
                                 n_cores = max(1, parallel::detectCores() %/% 2)) {
  cat(sprintf("\n[Robustness] Sample-size scan (N_fac in %s, %d reps each)\n",
              paste(N_grid, collapse = ","), N_sim))
  cat(sprintf("            cores=%d\n", n_cores))

  cl <- .start_cluster(n_cores)
  on.exit(stopCluster(cl), add = TRUE)
  config <- create_estimation_config_replacement(beta = 0.95, sigma2 = 1.0,
                                                 npl_iter = 200)

  cells <- expand.grid(N_fac = N_grid, rep = seq_len(N_sim),
                       stringsAsFactors = FALSE)

  t_start <- Sys.time()
  raw_list <- foreach(k = seq_len(nrow(cells)),
                      .packages = c("data.table", "Matrix"),
                      .errorhandling = "pass") %dopar% {
    N_fac  <- cells$N_fac[k]
    rep_id <- cells$rep[k]
    starts <- build_starts(theta_true, rep_seed = 80000L + rep_id)
    out <- tryCatch(
      estimate_one(theta_true, starts$warm, seed = 2000L + rep_id,
                   N_facilities = N_fac, T_periods = T_periods,
                   config = config),
      error = function(e) NULL
    )
    if (is.null(out)) return(NULL)
    out[, `:=`(truth_label = "central", start_label = "warm", rep = rep_id)]
    out
  }
  ok      <- vapply(raw_list, function(x) is.data.table(x), logical(1))
  results <- rbindlist(raw_list[ok], fill = TRUE)
  cat(sprintf("            elapsed: %.1f sec | OK %d / %d (failed=%d)\n",
              as.numeric(difftime(Sys.time(), t_start, units = "secs")),
              sum(ok), length(raw_list), sum(!ok)))
  results
}


# ==============================================================================
# 7. Reporting
# ==============================================================================
summarize_truth_start <- function(grid_dt, truths) {
  truth_tab <- rbindlist(lapply(names(truths), function(nm) {
    th <- truths[[nm]]
    data.table(truth_label = nm,
               kappa_true  = th[["kappa_exit"]],
               K_true      = exp(th[["K_log"]]),
               gp_true     = th[["gamma_price"]],
               gr_true     = th[["gamma_risk"]])
  }))
  setkey(truth_tab, truth_label)
  setkey(grid_dt, truth_label)

  s <- grid_dt[converged == TRUE,
               .(N           = .N,
                 mean_kappa  = mean(kappa_est),
                 bias_kappa  = mean(kappa_est) - kappa_true[1],
                 sd_kappa    = sd(kappa_est),
                 rmse_kappa  = sqrt(mean((kappa_est - kappa_true)^2)),
                 mean_K      = mean(K_est),
                 bias_K      = mean(K_est) - K_true[1],
                 sd_K        = sd(K_est),
                 rmse_K      = sqrt(mean((K_est - K_true)^2)),
                 mean_gp     = mean(gp_est),
                 bias_gp     = mean(gp_est) - gp_true[1],
                 sd_gp       = sd(gp_est),
                 rmse_gp     = sqrt(mean((gp_est - gp_true)^2)),
                 mean_gr     = mean(gr_est),
                 bias_gr     = mean(gr_est) - gr_true[1],
                 sd_gr       = sd(gr_est),
                 rmse_gr     = sqrt(mean((gr_est - gr_true)^2)),
                 cor_kappa_K = cor(kappa_est, K_est),
                 cor_kappa_gr= cor(kappa_est, gr_est),
                 mean_replace_sh = mean(replace_sh),
                 mean_exit_sh    = mean(exit_sh),
                 conv_rate    = mean(converged),
                 mean_n_iter = mean(n_iter)),
               by = .(truth_label, start_label)]
  s
}

plot_truth_x_start <- function(grid_dt, truth_tab, fig_dir = FIG_DIR) {
  dt <- merge(grid_dt[converged == TRUE], truth_tab,
              by = "truth_label", suffixes = c("", "_t"))
  dt[, truth_label := factor(truth_label, levels = names(TRUTHS))]
  dt[, start_label := factor(start_label, levels = c("warm", "mid", "cold"))]
  thm <- theme_minimal(base_size = 11) +
         theme(strip.text = element_text(face = "bold"))

  truth_lines <- function(varname, values_per_truth) {
    geom_hline(data = data.table(truth_label = factor(names(values_per_truth),
                                                       levels = names(TRUTHS)),
                                 truth = values_per_truth),
               aes(yintercept = truth), color = "red", linetype = "dashed")
  }

  K_truth_vec <- setNames(sapply(TRUTHS, function(x) exp(x["K_log"])),
                          names(TRUTHS))
  k_truth_vec <- setNames(sapply(TRUTHS, function(x) x["kappa_exit"]),
                          names(TRUTHS))

  p_K <- ggplot(dt, aes(x = start_label, y = K_est, fill = start_label)) +
    geom_boxplot(outlier.size = 0.5) +
    facet_wrap(~ truth_label, scales = "free_y", nrow = 1) +
    truth_lines("K", K_truth_vec) +
    thm + theme(legend.position = "none") +
    labs(title = "K (replacement cost) recovery by truth x start",
         subtitle = "Red dashed = truth K",
         x = NULL, y = "K_est")

  p_kappa <- ggplot(dt, aes(x = start_label, y = kappa_est, fill = start_label)) +
    geom_boxplot(outlier.size = 0.5) +
    facet_wrap(~ truth_label, scales = "free_y", nrow = 1) +
    truth_lines("kappa", k_truth_vec) +
    thm + theme(legend.position = "none") +
    labs(title = "kappa_exit recovery by truth x start",
         subtitle = "Red dashed = truth kappa",
         x = NULL, y = "kappa_est")

  outfile <- file.path(fig_dir, "04b_Robustness_Truth_x_Start.png")
  ggsave(outfile, arrangeGrob(p_K, p_kappa, ncol = 1),
         width = 14, height = 9)
  cat(sprintf("  saved: %s\n", outfile))
  outfile
}

plot_correlations_by_truth <- function(grid_dt, fig_dir = FIG_DIR) {
  dt <- grid_dt[converged == TRUE & start_label == "warm"]
  cor_dt <- dt[, .(
    cor_kK   = cor(kappa_est, K_est),
    cor_kgp  = cor(kappa_est, gp_est),
    cor_kgr  = cor(kappa_est, gr_est),
    cor_Kgp  = cor(K_est,     gp_est),
    cor_Kgr  = cor(K_est,     gr_est),
    cor_gpgr = cor(gp_est,    gr_est)
  ), by = truth_label]

  cor_long <- melt(cor_dt, id.vars = "truth_label",
                   variable.name = "pair", value.name = "r")
  cor_long[, truth_label := factor(truth_label, levels = names(TRUTHS))]
  cor_long[, pair := factor(pair, levels = c("cor_kK", "cor_kgr", "cor_kgp",
                                             "cor_Kgp", "cor_Kgr", "cor_gpgr"))]

  p <- ggplot(cor_long, aes(x = truth_label, y = r, fill = pair)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, color = "black") +
    coord_cartesian(ylim = c(-1, 1)) +
    theme_minimal(base_size = 11) +
    labs(title = "Pairwise correlations of estimates across reps (warm start)",
         subtitle = "Persistence of identification trade-offs across truths",
         x = "truth", y = "Pearson r")

  outfile <- file.path(fig_dir, "04b_Robustness_CorrByTruth.png")
  ggsave(outfile, p, width = 11, height = 6)
  cat(sprintf("  saved: %s\n", outfile))
  outfile
}

plot_sample_size <- function(scan_dt, theta_true, fig_dir = FIG_DIR) {
  dt <- scan_dt[converged == TRUE]
  K_true <- exp(theta_true[["K_log"]])
  k_true <- theta_true[["kappa_exit"]]
  thm <- theme_minimal(base_size = 11)

  dt[, N_facilities_lab := factor(N_facilities,
                                   levels = sort(unique(N_facilities)))]

  p_K <- ggplot(dt, aes(x = N_facilities_lab, y = K_est)) +
    geom_boxplot(fill = "darkorange", alpha = 0.7) +
    geom_hline(yintercept = K_true, color = "red", linetype = "dashed") +
    thm +
    labs(title = "K_est vs sample size",
         x = "N_facilities", y = "K_est")

  p_kappa <- ggplot(dt, aes(x = N_facilities_lab, y = kappa_est)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    geom_hline(yintercept = k_true, color = "red", linetype = "dashed") +
    thm +
    labs(title = "kappa_est vs sample size",
         x = "N_facilities", y = "kappa_est")

  # SD as a function of N_fac, for sqrt(n) check
  sd_dt <- dt[, .(sd_K = sd(K_est), sd_kappa = sd(kappa_est),
                  N = .N, mean_n_obs = mean(n_obs)),
              by = N_facilities]
  setorder(sd_dt, N_facilities)

  p_sd <- ggplot(sd_dt, aes(x = N_facilities)) +
    geom_point(aes(y = sd_K), color = "darkorange", size = 3) +
    geom_line(aes(y = sd_K),  color = "darkorange") +
    geom_point(aes(y = sd_kappa), color = "steelblue", size = 3) +
    geom_line(aes(y = sd_kappa),  color = "steelblue") +
    scale_x_log10() + scale_y_log10() +
    thm +
    labs(title = "log(SD) vs log(N_fac) — slope = -0.5 if sqrt(n) holds",
         subtitle = "orange = SD(K), blue = SD(kappa)",
         x = "N_facilities (log)", y = "SD across reps (log)")

  outfile <- file.path(fig_dir, "04b_Robustness_Sample_Size.png")
  ggsave(outfile, arrangeGrob(p_K, p_kappa, p_sd, ncol = 1),
         width = 11, height = 12)
  cat(sprintf("  saved: %s\n", outfile))

  # Also fit sqrt(n) regression: log(SD) = a + b * log(N)
  fit_K     <- lm(log(sd_K)     ~ log(N_facilities), data = sd_dt)
  fit_kappa <- lm(log(sd_kappa) ~ log(N_facilities), data = sd_dt)
  cat(sprintf("  sqrt(n) check: slope_K     = %.3f (target = -0.5)\n",
              coef(fit_K)[2]))
  cat(sprintf("                 slope_kappa = %.3f (target = -0.5)\n",
              coef(fit_kappa)[2]))

  list(figure = outfile, sd_dt = sd_dt,
       slope_K = unname(coef(fit_K)[2]),
       slope_kappa = unname(coef(fit_kappa)[2]))
}


# ==============================================================================
# 8. EXECUTION BLOCK
# ==============================================================================
if (sys.nframe() == 0) {

  N_CORES <- max(1L, parallel::detectCores() %/% 2L)

  # ---- Pre-flight ----
  preflight <- preflight_shares(TRUTHS, N_facilities = 800L)

  # ---- Truth grid x multi-start ----
  grid_dt <- run_truth_start_grid(TRUTHS, N_sim = 200L,
                                  N_facilities = 2000L,
                                  T_periods = 30L,
                                  n_cores = N_CORES)

  truth_tab <- rbindlist(lapply(names(TRUTHS), function(nm) {
    th <- TRUTHS[[nm]]
    data.table(truth_label = nm,
               kappa_true  = th[["kappa_exit"]],
               K_true      = exp(th[["K_log"]]),
               gp_true     = th[["gamma_price"]],
               gr_true     = th[["gamma_risk"]])
  }))

  s_grid <- summarize_truth_start(grid_dt, TRUTHS)
  cat("\n[Truth x Start] summary table:\n")
  print(s_grid)

  plot_truth_x_start(grid_dt, truth_tab)
  plot_correlations_by_truth(grid_dt)

  # ---- Sample-size scan at the central truth (truth4 = real-data-like) ----
  scan_dt <- run_sample_size_scan(
    theta_true = TRUTHS$truth4,
    N_grid     = c(1000L, 2000L, 5000L, 20000L),
    N_sim      = 100L,
    T_periods  = 30L,
    n_cores    = N_CORES
  )
  scan_summary <- plot_sample_size(scan_dt, TRUTHS$truth4)

  # ---- Save outputs ----
  fwrite(grid_dt, file.path(TAB_DIR, "04b_Robustness_Raw.csv"))
  fwrite(s_grid,  file.path(TAB_DIR, "04b_Robustness_Summary.csv"))

  bundle <- list(
    truths       = TRUTHS,
    preflight    = preflight,
    grid_raw     = grid_dt,
    grid_summary = s_grid,
    scan_raw     = scan_dt,
    scan_summary = scan_summary
  )
  saveRDS(bundle, file.path(TAB_DIR, "04b_Robustness_Bundle.rds"))

  cat("\n[Robustness] done.\n")
}
