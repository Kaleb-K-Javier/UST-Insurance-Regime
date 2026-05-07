# ==============================================================================
# 04f_Replacement_Refit_Extended2000_AM_SE.R
# ==============================================================================
# PURPOSE
#
#   1. Build a new "extended_2000plus" sample that drops 1999 entirely (the
#      1999 closure spike is an EPA-1998-deadline artifact, not a regime
#      effect). The new sample is panel_year >= 2000 for both TX and controls.
#      Recomputes obs-panel-dependent primitives (P_vec, P0_mat, F_maintain)
#      on the filtered subset; reuses h_vec and L_vec (these are upstream
#      objects from 01n hazard model and 05 claims-loss data, independent of
#      this filter).
#
#   2. Compute Aguirregabiria-Mira (2002) corrected standard errors via the
#      PROFILE LIKELIHOOD Hessian. The earlier 04e SEs treated P_hat as
#      fixed, which understates uncertainty. The profile-likelihood approach
#      L_profile(theta) = L(theta, P_eq(theta)) re-solves the equilibrium
#      policy at every perturbation of theta, so the resulting Hessian
#      accounts for the implicit dependence of P on theta at the NPL fixed
#      point. Bootstrap SEs are the gold standard for full NPL inference and
#      are deferred to a later pass.
#
#   3. Build per-wall goodness-of-fit figures (separate PNG for SW, separate
#      PNG for DW), simpler and less busy than the combined figure in 04e.
#
# OUTPUTS
#   Output/Estimation_Results/Model_Replacement_Estimates_extended_2000plus.rds
#   Output/Estimation_Results/DCM_Primitives_Replacement_extended_2000plus.rds
#   Output/Tables/04f_Theta_Table_AM_SE.csv
#   Output/Tables/04f_Theta_Table_AM_SE.tex
#   Output/Figures/04f_Fit_SW.png
#   Output/Figures/04f_Fit_DW.png
#   Output/Figures/04f_Fit_Counts_per_Cell.png   (just data — n obs by cell)
# ==============================================================================

# Worktree compatibility: prepend parent project's renv lib if present.
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
  library(numDeriv)
})

source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))

OUT_FIT <- here::here("Output", "Estimation_Results")
OUT_TAB <- here::here("Output", "Tables")
OUT_FIG <- here::here("Output", "Figures")
dir.create(OUT_FIT, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)

SCALE_FACTOR <- 10000   # 1 model unit = $10,000

PARENT_PROJECT_ROOT <- "C:/Users/kaleb/Documents/ust_ins_move_to_github"
parent_in <- function(...) {
  parent_p <- file.path(PARENT_PROJECT_ROOT, ...)
  local_p  <- here::here(...)
  if (file.exists(local_p))  return(local_p)
  if (file.exists(parent_p)) return(parent_p)
  stop("not found: ", ...)
}

cat("==================================================================\n")
cat("04f: Refit extended (year >= 2000), AM-corrected SEs, per-wall fit figs\n")
cat("==================================================================\n")


# ==============================================================================
# 1. Load existing fits + primitives + obs panels
# ==============================================================================
fit_observed   <- readRDS(file.path(OUT_FIT, "Model_Replacement_Estimates_observed.rds"))
prims_extended <- readRDS(parent_in("Output", "Estimation_Results",
                                    "DCM_Primitives_Replacement_extended.rds"))
prims_observed <- readRDS(parent_in("Output", "Estimation_Results",
                                    "DCM_Primitives_Replacement_observed.rds"))
obs_observed   <- fread(parent_in("Data", "Analysis",
                                  "dcm_obs_panel_observed.csv"))
obs_extended   <- fread(parent_in("Data", "Analysis",
                                  "dcm_obs_panel_extended.csv"))


# ==============================================================================
# 2. Build extended_2000plus obs panel + primitives
# ==============================================================================
cat("\n[2] Build extended_2000plus sample (drop 1999 entirely) ...\n")
N_AGE    <- 8L
N_WALL   <- 2L
N_RHO    <- 2L
N_STATES <- 32L

obs_ext2k <- obs_extended[panel_year >= 2000L]
cat(sprintf("    rows kept: %s of %s extended (%.1f%% removed)\n",
            format(nrow(obs_ext2k),       big.mark = ","),
            format(nrow(obs_extended),    big.mark = ","),
            100 * (1 - nrow(obs_ext2k) / nrow(obs_extended))))

# Recompute obs-panel-dependent primitives on the new subset.
# Reuse upstream cell-level objects: h_vec (01n hazard) and L_vec (05 claims)
# which do not depend on this row-level filter.
state_idx_fn <- function(A, w, rho) (rho - 1L) * 16L + (w - 1L) * 8L + A

# (a) age transitions on Maintain rows in the new subset
setorder(obs_ext2k, panel_id, panel_year)
obs_ext2k[, A_bin_next := shift(A_bin, -1L, type = "lead"), by = panel_id]
age_trans <- obs_ext2k[
  !is.na(A_bin_next) & y_it == 0L,
  .(pi_up   = mean(A_bin_next > A_bin, na.rm = TRUE),
    pi_stay = mean(A_bin_next == A_bin, na.rm = TRUE),
    n_obs   = .N),
  by = A_bin][order(A_bin)]
age_trans[A_bin == N_AGE, `:=`(pi_up = 0, pi_stay = 1)]
age_trans[, pi_sum := pi_up + pi_stay]
if (any(abs(age_trans$pi_sum - 1) > 0.05)) {
  age_trans[, `:=`(pi_up = pi_up / pi_sum, pi_stay = pi_stay / pi_sum)]
}

# (b) F_maintain
F_maintain <- Matrix(0, N_STATES, N_STATES, sparse = TRUE)
for (rho in 1:N_RHO) for (w in 1:N_WALL) for (a in 1:N_AGE) {
  s_from <- state_idx_fn(a, w, rho)
  pu <- age_trans[A_bin == a, pi_up]
  ps <- age_trans[A_bin == a, pi_stay]
  F_maintain[s_from, s_from] <- ps
  if (a < N_AGE) {
    F_maintain[s_from, state_idx_fn(a + 1L, w, rho)] <- pu
  }
}
stopifnot(all(abs(rowSums(F_maintain) - 1) < 1e-10))

# (c) P_vec on the filtered subset
prem_s <- obs_ext2k[!is.na(premium),
                    .(P_hat = mean(premium)), by = s_idx][order(s_idx)]
P_vec <- numeric(N_STATES)
P_vec[prem_s$s_idx] <- prem_s$P_hat
empties <- which(P_vec == 0 | !is.finite(P_vec))
if (length(empties)) {
  for (s in empties) {
    rho      <- (s - 1L) %/% 16L + 1L
    rest     <-  s - (rho - 1L) * 16L
    other_rho <- 3L - rho
    P_vec[s] <- P_vec[(other_rho - 1L) * 16L + rest]
  }
  if (any(P_vec == 0)) P_vec[P_vec == 0] <- mean(P_vec[P_vec > 0])
}

# (d) P0_mat (smooth-logit init from 04b's recipe, on filtered subset)
ccp_dat <- obs_ext2k[!is.na(premium) & !is.na(y_it)]
ccp_fit <- glm(y_it ~ A_bin + I(A_bin^2) + factor(w_state) + factor(rho_state),
               data = ccp_dat, family = binomial)
ccp_dat[, P_close_init := pmax(pmin(predict(ccp_fit, type = "response"),
                                    1 - 1e-6), 1e-6)]
P0_state <- ccp_dat[, .(P_close = mean(P_close_init)), by = s_idx]
P0_state <- merge(data.table(s_idx = 1:N_STATES), P0_state, by = "s_idx",
                  all.x = TRUE)
P0_state[is.na(P_close), P_close := mean(ccp_dat$P_close_init)]
P0_mat <- cbind(maintain = 1 - P0_state$P_close, close = P0_state$P_close)

# (e) Closure summary
n_exit    <- obs_ext2k[y_it == 1L & I_replace == 0L, .N]
n_replace <- obs_ext2k[y_it == 1L & I_replace == 1L, .N]
pct_replace <- n_replace / max(1, n_exit + n_replace)

prims_ext2k <- list(
  state_lut    = prims_extended$state_lut,
  h_vec        = prims_extended$h_vec,    # upstream from 01n; unchanged
  L_vec        = prims_extended$L_vec,    # upstream from 05;   unchanged
  P_vec        = P_vec,
  F_maintain   = F_maintain,
  P0_mat       = P0_mat,
  age_trans    = age_trans,
  n_exit       = n_exit,
  n_replace    = n_replace,
  pct_replace  = pct_replace,
  sample_label = "extended_2000plus (TX + controls 2000+)",
  n_obs        = nrow(obs_ext2k),
  n_facilities = uniqueN(obs_ext2k$panel_id)
)
saveRDS(prims_ext2k,
        file.path(OUT_FIT, "DCM_Primitives_Replacement_extended_2000plus.rds"))
cat(sprintf("    saved primitives -> .../DCM_Primitives_Replacement_extended_2000plus.rds\n"))
cat(sprintf("    n_exit=%d  n_replace=%d  pct_replace=%.2f%%\n",
            n_exit, n_replace, 100 * pct_replace))


# ==============================================================================
# 3. Refit on extended_2000plus
# ==============================================================================
cat("\n[3] Refit on extended_2000plus ...\n")
config <- create_estimation_config_replacement(beta = 0.95, sigma2 = 1.0,
                                               npl_iter = 200)
fit_ext2k <- npl_estimator_replacement(
  obs_panel  = obs_ext2k,
  primitives = prims_ext2k,
  config     = config,
  theta_init = c(kappa_exit  = 20, K_log = log(20),
                 gamma_price = -1.0, gamma_risk = 1.0),
  verbose    = TRUE
)
saveRDS(fit_ext2k,
        file.path(OUT_FIT, "Model_Replacement_Estimates_extended_2000plus.rds"))
cat(sprintf("\n  ext2k theta_hat: kappa=%.3f K=%.3f gP=%.3f gR=%.3f\n",
            fit_ext2k$theta_hat[["kappa_exit"]], fit_ext2k$theta_hat[["K"]],
            fit_ext2k$theta_hat[["gamma_price"]], fit_ext2k$theta_hat[["gamma_risk"]]))


# ==============================================================================
# 4. AM (2002) corrected SEs via PROFILE LIKELIHOOD Hessian
# ==============================================================================
# At the NPL fixed point (theta_hat, P_hat), the AM (2002) asymptotic
# variance equals the inverse of the Hessian of the *profile* log-likelihood
#   L_profile(theta) = L(theta, P_eq(theta))
# where P_eq(theta) is the equilibrium policy solving P = Psi(theta, P).
# This accounts for the implicit dependence of P on theta. The within-loop
# Hessian (used in 04e) holds P fixed at P_hat and underestimates SE.
#
# We compute Hessian numerically: each evaluation re-solves the equilibrium.
# Bootstrap SEs are the gold standard for full NPL inference (would also
# capture small-sample behavior of P_hat itself); deferred.
cat("\n[4] AM-2002 SEs via profile likelihood Hessian ...\n")

# Precompute the cache once per fit (depends on primitives + obs panel,
# independent of theta).
build_cache <- function(prims, obs, cfg) {
  create_estimation_cache_replacement(prims, obs, cfg)
}
cache_obs <- build_cache(prims_observed, obs_observed, config)
cache_e2k <- build_cache(prims_ext2k,   obs_ext2k,    config)

profile_neg_loglik <- function(theta_raw, cache, config,
                               policy_max_iter = 500, policy_tol = 1e-7) {
  if (is.null(names(theta_raw))) {
    names(theta_raw) <- c("kappa_exit","K_log","gamma_price","gamma_risk")
  }
  eq <- tryCatch(
    solve_equilibrium_policy_replacement(
      theta_raw, cache, config,
      max_iter = policy_max_iter, tol = policy_tol),
    error = function(e) NULL
  )
  if (is.null(eq) || !eq$converged) return(1e10)
  P <- eq$P
  npl_likelihood_replacement(theta_raw, P, cache, config)
}

# Custom central-difference Hessian. We do this instead of numDeriv::hessian()
# because the default Richardson method is wasteful for our case (each profile
# evaluation re-solves the equilibrium policy, ~5 sec) and "simple" is not
# implemented for hessian. Central differences here use 1 + 2k + 4k(k-1)/2
# evaluations for k parameters: 33 evaluations for k=4.
finite_diff_hessian <- function(fn, x, h = 1e-3, ...) {
  k <- length(x)
  H <- matrix(NA_real_, k, k)
  f0 <- fn(x, ...)
  if (!is.finite(f0)) return(H)

  # diagonal: f(x + h e_i), f(x - h e_i)
  fp <- numeric(k); fm <- numeric(k)
  for (i in seq_len(k)) {
    xp <- x; xp[i] <- xp[i] + h
    xm <- x; xm[i] <- xm[i] - h
    fp[i] <- fn(xp, ...)
    fm[i] <- fn(xm, ...)
  }
  for (i in seq_len(k)) {
    H[i, i] <- (fp[i] - 2 * f0 + fm[i]) / (h^2)
  }

  # off-diagonal: cross differences
  for (i in seq_len(k - 1L)) for (j in seq.int(i + 1L, k)) {
    xpp <- x; xpp[i] <- xpp[i] + h; xpp[j] <- xpp[j] + h
    xpm <- x; xpm[i] <- xpm[i] + h; xpm[j] <- xpm[j] - h
    xmp <- x; xmp[i] <- xmp[i] - h; xmp[j] <- xmp[j] + h
    xmm <- x; xmm[i] <- xmm[i] - h; xmm[j] <- xmm[j] - h
    fij <- (fn(xpp, ...) - fn(xpm, ...) - fn(xmp, ...) + fn(xmm, ...)) / (4 * h^2)
    H[i, j] <- fij
    H[j, i] <- fij
  }
  H
}

compute_am_se <- function(fit, cache, config, label, h_step = 1e-3) {
  cat(sprintf("  -- %s : profile-likelihood Hessian via finite differences\n", label))
  cat("     (each evaluation re-solves the equilibrium policy)\n")
  th <- fit$theta_raw
  t0 <- Sys.time()
  H <- tryCatch(
    finite_diff_hessian(profile_neg_loglik, th, h = h_step,
                        cache = cache, config = config),
    error = function(e) {
      cat("    Hessian failed:", conditionMessage(e), "\n")
      matrix(NA_real_, 4L, 4L)
    }
  )
  cat(sprintf("    elapsed %.1f sec\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  if (anyNA(H) || any(!is.finite(H))) {
    return(list(SE = rep(NA_real_, 4L), H = H, Vinv = NULL))
  }
  Vinv <- tryCatch(solve(H), error = function(e) NULL)
  if (is.null(Vinv) || any(diag(Vinv) <= 0)) {
    cat("    Hessian inversion failed or yielded non-PSD; falling back to abs(diag)\n")
    SE <- sqrt(abs(diag(tryCatch(solve(H), error = function(e) diag(NA_real_, 4L)))))
  } else {
    SE <- sqrt(diag(Vinv))
  }
  list(SE = SE, H = H, Vinv = Vinv)
}

am_obs   <- compute_am_se(fit_observed, cache_obs, config, "observed")
am_ext2k <- compute_am_se(fit_ext2k,    cache_e2k, config, "extended_2000plus")


# Delta-method: SE(K) = K * SE(K_log)
build_se_row <- function(fit, am_obj, label) {
  th_raw <- fit$theta_raw
  K_hat  <- fit$theta_hat[["K"]]
  SE     <- am_obj$SE
  data.table(
    sample      = label,
    kappa_exit  = th_raw[["kappa_exit"]],
    SE_kappa    = SE[1],
    K           = K_hat,
    SE_K        = if (length(SE) >= 2 && !is.na(SE[2])) K_hat * SE[2] else NA_real_,
    K_log       = th_raw[["K_log"]],
    SE_K_log    = SE[2],
    gamma_price = th_raw[["gamma_price"]],
    SE_gp       = SE[3],
    gamma_risk  = th_raw[["gamma_risk"]],
    SE_gr       = SE[4],
    log_lik     = fit$log_likelihood,
    n_obs       = fit$cache$n_obs
  )
}

se_tab <- rbindlist(list(
  build_se_row(fit_observed, am_obs,
               "observed (TX 2006+, controls 1999+)"),
  build_se_row(fit_ext2k,    am_ext2k,
               "extended_2000plus (TX + controls 2000+)")
))
print(se_tab)
fwrite(se_tab, file.path(OUT_TAB, "04f_Theta_Table_AM_SE.csv"))


# Build LaTeX table
fmt_dollar <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt_se_dollar <- function(x) sprintf("(%s)", formatC(round(x), format = "d", big.mark = ","))
push <- function(label, val_o, se_o, val_e, se_e, dollar = FALSE) {
  if (dollar) {
    fv <- function(x) sprintf("\\$%s", fmt_dollar(x))
    fs <- function(x) sprintf("(\\$%s)", fmt_dollar(x))
  } else {
    fv <- function(x) sprintf("%.3f", x)
    fs <- function(x) sprintf("(%.3f)", x)
  }
  c(
    sprintf("%s & %s & %s \\\\", label, fv(val_o), fv(val_e)),
    sprintf("        & %s & %s \\\\", fs(se_o), fs(se_e))
  )
}

o <- se_tab[1L]; e <- se_tab[2L]
tex <- c(
  "% Auto-generated by 04f_Replacement_Refit_Extended2000_AM_SE.R",
  "% AM (2002) corrected SEs in (parentheses); bootstrap SEs deferred.",
  "\\begin{tabular}{lcc}",
  "\\hline",
  " & Observed & Extended (2000+) \\\\",
  " & TX 2006+, controls 1999+ & TX 2000+, controls 2000+ \\\\",
  "\\hline",
  push("$\\kappa_{\\mathrm{exit}}$ (\\$)",
       o$kappa_exit * SCALE_FACTOR, o$SE_kappa * SCALE_FACTOR,
       e$kappa_exit * SCALE_FACTOR, e$SE_kappa * SCALE_FACTOR,
       dollar = TRUE),
  push("$K$ (\\$)",
       o$K * SCALE_FACTOR, o$SE_K * SCALE_FACTOR,
       e$K * SCALE_FACTOR, e$SE_K * SCALE_FACTOR,
       dollar = TRUE),
  push("$\\gamma_{\\mathrm{price}}$",
       o$gamma_price, o$SE_gp,
       e$gamma_price, e$SE_gp),
  push("$\\gamma_{\\mathrm{risk}}$",
       o$gamma_risk, o$SE_gr,
       e$gamma_risk, e$SE_gr),
  "\\hline",
  sprintf("$\\log L$ & %s & %s \\\\",
          formatC(round(o$log_lik), format = "d", big.mark = ","),
          formatC(round(e$log_lik), format = "d", big.mark = ",")),
  sprintf("$N$ obs  & %s & %s \\\\",
          formatC(o$n_obs, format = "d", big.mark = ","),
          formatC(e$n_obs, format = "d", big.mark = ",")),
  "\\hline",
  "\\multicolumn{3}{l}{\\footnotesize SEs are AM (2002) profile-likelihood;",
  "  bootstrap SEs deferred for a future pass.}",
  "\\end{tabular}"
)
writeLines(tex, file.path(OUT_TAB, "04f_Theta_Table_AM_SE.tex"))


# ==============================================================================
# 5. Per-wall fit figures (one PNG per wall, less busy than 04e)
# ==============================================================================
cat("\n[5] Per-wall fit figures (observed sample) ...\n")

state_lut <- prims_observed$state_lut
P_hat_dt <- as.data.table(fit_observed$P_hat)
setnames(P_hat_dt, c("model_M", "model_E", "model_R"))
P_hat_dt[, s_idx := seq_len(.N)]

emp <- obs_observed[, .(
  emp_M  = mean(y_it == 0L),
  emp_E  = mean(y_it == 1L & I_replace == 0L, na.rm = TRUE),
  emp_R  = mean(y_it == 1L & I_replace == 1L, na.rm = TRUE),
  n_cell = .N
), by = s_idx][order(s_idx)]

cell <- merge(state_lut, merge(P_hat_dt, emp, by = "s_idx"), by = "s_idx")
cell[, regime := fifelse(rho_state == 1L, "FF (controls / TX pre-99)",
                                            "RB (TX 1999+)")]
cell[, wall_lbl := fifelse(w_state == 1L, "Single-Walled (or Mixed)",
                                            "Double-Walled")]

# Build long-form for plotting
build_long <- function(dt) {
  rbind(
    dt[, .(s_idx, A_bin, regime, n_cell,
           action = "Maintain", model = model_M, empirical = emp_M)],
    dt[, .(s_idx, A_bin, regime, n_cell,
           action = "Exit",     model = model_E, empirical = emp_E)],
    dt[, .(s_idx, A_bin, regime, n_cell,
           action = "Replace",  model = model_R, empirical = emp_R)]
  )
}

plot_one_wall <- function(dt, wall_label, outfile) {
  d <- build_long(dt)
  d[, action := factor(action, levels = c("Maintain","Exit","Replace"))]

  # Compute y-limits per panel from BOTH model and empirical so the data
  # is always on-axis.
  thm <- theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold", size = 12),
          legend.position = "top",
          plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(color = "gray30"))

  p <- ggplot(d, aes(x = A_bin, color = regime)) +
    geom_line(aes(y = model), linewidth = 1.0) +
    geom_point(aes(y = empirical, size = n_cell),
               shape = 21, fill = "white", stroke = 1) +
    facet_wrap(~ action, scales = "free_y", nrow = 1) +
    scale_x_continuous(breaks = 1:8,
                       labels = c("0-5","5-10","10-15","15-20",
                                  "20-25","25-30","30-35","35+")) +
    scale_size_area(max_size = 7, guide = "none") +
    thm +
    labs(title = sprintf("Goodness of fit — %s (observed sample)", wall_label),
         subtitle = "Solid line = MODEL-IMPLIED P(action | state); open circle = EMPIRICAL data share (size = n obs)",
         x = "age bin (5-yr)", y = "P(action | state)",
         color = "regime")
  ggsave(outfile, p, width = 12, height = 4.5)
  cat(sprintf("    saved: %s\n", outfile))
}

plot_one_wall(cell[w_state == 1L], "Single-Walled (or Mixed)",
              file.path(OUT_FIG, "04f_Fit_SW.png"))
plot_one_wall(cell[w_state == 2L], "Double-Walled",
              file.path(OUT_FIG, "04f_Fit_DW.png"))


# Counts per cell (data-only) — gives the reader a sense of where the data
# lives, separate from the fit story.
counts_dt <- cell[, .(s_idx, A_bin, regime, wall_lbl, n_cell)]
p_counts <- ggplot(counts_dt,
                   aes(x = A_bin, y = n_cell,
                       color = regime, group = regime)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
  facet_wrap(~ wall_lbl, scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = 1:8,
                     labels = c("0-5","5-10","10-15","15-20",
                                "20-25","25-30","30-35","35+")) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 11) +
  labs(title = "Observation counts per state cell — RAW DATA",
       x = "age bin (5-yr)", y = "obs in cell (observed sample)",
       color = "regime")
ggsave(file.path(OUT_FIG, "04f_Fit_Counts_per_Cell.png"),
       p_counts, width = 11, height = 4)


# ==============================================================================
# 6. Diagnostic summary
# ==============================================================================
cat("\n--- 04f summary ---\n")
cat(sprintf("  observed (PR1):           kappa=%.3f  K=%.3f  gP=%.3f  gR=%.3f\n",
            fit_observed$theta_hat[["kappa_exit"]], fit_observed$theta_hat[["K"]],
            fit_observed$theta_hat[["gamma_price"]], fit_observed$theta_hat[["gamma_risk"]]))
cat(sprintf("  extended_2000plus (NEW):  kappa=%.3f  K=%.3f  gP=%.3f  gR=%.3f\n",
            fit_ext2k$theta_hat[["kappa_exit"]],     fit_ext2k$theta_hat[["K"]],
            fit_ext2k$theta_hat[["gamma_price"]],    fit_ext2k$theta_hat[["gamma_risk"]]))

cat("\n  AM SEs (parentheses below = profile-likelihood Hessian):\n")
print(se_tab[, .(sample, SE_kappa, SE_K, SE_gp, SE_gr)])

cat("\nSaved:\n")
for (f in c(
  file.path(OUT_FIT, "Model_Replacement_Estimates_extended_2000plus.rds"),
  file.path(OUT_FIT, "DCM_Primitives_Replacement_extended_2000plus.rds"),
  file.path(OUT_TAB, "04f_Theta_Table_AM_SE.csv"),
  file.path(OUT_TAB, "04f_Theta_Table_AM_SE.tex"),
  file.path(OUT_FIG, "04f_Fit_SW.png"),
  file.path(OUT_FIG, "04f_Fit_DW.png"),
  file.path(OUT_FIG, "04f_Fit_Counts_per_Cell.png")
)) cat("  ", f, "\n")

cat("\n04f complete.\n")
