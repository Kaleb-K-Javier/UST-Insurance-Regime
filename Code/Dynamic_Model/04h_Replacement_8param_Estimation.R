# ==============================================================================
# 04h_Replacement_8param_Estimation.R
# ==============================================================================
# PURPOSE
#   Fit the 8-parameter replacement DCM:
#     theta = (kappa_SW, kappa_DW, K_SW, K_DW,
#              gamma_price_FF, gamma_price_RB,
#              gamma_risk_FF,  gamma_risk_RB)
#   on the observed sample (TX 2006+, controls 1999+) and the
#   extended_2000+ sample. Computes AM (2002) profile-likelihood SEs.
#   Builds per-cell fit numbers (5 decimals), per-wall fit figures, and a
#   headline LaTeX table with point estimates + AM SEs + 95% CIs.
#
#   MC robustness on the 8-param spec is a follow-up PR.
# ==============================================================================

.PARENT_RENV_LIB <- "C:/Users/kaleb/Documents/ust_ins_move_to_github/renv/library/windows/R-4.5/x86_64-w64-mingw32"
if (dir.exists(.PARENT_RENV_LIB) && !(.PARENT_RENV_LIB %in% .libPaths())) {
  .libPaths(c(.PARENT_RENV_LIB, .libPaths()))
}

suppressPackageStartupMessages({
  library(data.table); library(Matrix); library(here)
  library(ggplot2);    library(gridExtra)
})

.SCRIPT_BASENAME <- "04h"
.log_path <- here::here(
  "logs",
  paste0(.SCRIPT_BASENAME, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
    .log_path, .SCRIPT_BASENAME, R.version.string, getwd()))

# ============================================================================
# SAMPLE GATES — flip to TRUE to enable a sample branch.
# extended_2000plus is currently FALSE because its upstream prereqs
# (DCM_Primitives_Replacement_extended_2000plus.rds and
#  Model_Replacement_Estimates_extended_2000plus.rds) are produced by 04f,
# which is not part of Ticket 001's pipeline. Flip to TRUE once 04f outputs
# exist locally or on Z.
# ============================================================================
RUN_OBSERVED          <- TRUE
RUN_EXTENDED_2000PLUS <- FALSE

source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))

OUT_FIT <- here::here("Output", "Estimation_Results")
OUT_TAB <- here::here("Output", "Tables")
OUT_FIG <- here::here("Output", "Figures")
dir.create(OUT_FIT, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)

SCALE_FACTOR <- 10000
PARENT <- "C:/Users/kaleb/Documents/ust_ins_move_to_github"
parent_in <- function(...) {
  p <- file.path(PARENT, ...); l <- here::here(...)
  if (file.exists(l)) return(l); if (file.exists(p)) return(p)
  stop("not found: ", ...)
}

cat("======================================================================\n")
cat("04h: 8-parameter replacement DCM fit on observed + extended_2000+\n")
cat("======================================================================\n")


# ---- 1. Load primitives + obs panels + 4-param fits (for theta_init) ------
prims_obs   <- readRDS(parent_in("Output", "Estimation_Results",
                                 "DCM_Primitives_Replacement_observed.rds"))
obs_obs     <- fread(parent_in("Data", "Analysis",
                               "dcm_obs_panel_observed.csv"))
fit_4p_obs  <- readRDS(file.path(OUT_FIT,
                                 "Model_Replacement_Estimates_observed.rds"))

if (RUN_EXTENDED_2000PLUS) {
  prims_e2k  <- readRDS(file.path(OUT_FIT,
                                  "DCM_Primitives_Replacement_extended_2000plus.rds"))
  obs_e2k    <- fread(parent_in("Data", "Analysis",
                                "dcm_obs_panel_extended.csv"))[panel_year >= 2000L]
  fit_4p_e2k <- readRDS(file.path(OUT_FIT,
                                  "Model_Replacement_Estimates_extended_2000plus.rds"))
}


# ---- 2. theta_init for 8-param: copy from 4-param fit (duplicate) ---------
build_theta_init_from_4p <- function(fit_4p) {
  th <- fit_4p$theta_raw
  c(kappa_SW       = th[["kappa_exit"]],
    kappa_DW       = th[["kappa_exit"]],
    K_log_SW       = th[["K_log"]],
    K_log_DW       = th[["K_log"]],
    gamma_price_FF = th[["gamma_price"]],
    gamma_price_RB = th[["gamma_price"]],
    gamma_risk_FF  = th[["gamma_risk"]],
    gamma_risk_RB  = th[["gamma_risk"]])
}


# ---- 3. Fit 8-param on each sample ----------------------------------------
config_8p <- create_estimation_config_replacement_8p(beta = 0.95,
                                                     sigma2 = 1.0,
                                                     npl_iter = 300)

cat("\n[3a] 8-param fit on OBSERVED sample\n")
fit_8p_obs <- npl_estimator_replacement_8p(
  obs_panel  = obs_obs,
  primitives = prims_obs,
  config_8p  = config_8p,
  theta_init = build_theta_init_from_4p(fit_4p_obs),
  verbose    = TRUE
)
saveRDS(fit_8p_obs,
        file.path(OUT_FIT, "Model_Replacement_8param_observed.rds"))

if (RUN_EXTENDED_2000PLUS) {
  cat("\n[3b] 8-param fit on EXTENDED_2000plus sample\n")
  fit_8p_e2k <- npl_estimator_replacement_8p(
    obs_panel  = obs_e2k,
    primitives = prims_e2k,
    config_8p  = config_8p,
    theta_init = build_theta_init_from_4p(fit_4p_e2k),
    verbose    = TRUE
  )
  saveRDS(fit_8p_e2k,
          file.path(OUT_FIT, "Model_Replacement_8param_extended_2000plus.rds"))
}


# ---- 4. AM-2002 SEs via profile-likelihood Hessian (custom finite diff) ---
finite_diff_hessian <- function(fn, x, h = 1e-3, ...) {
  k <- length(x); H <- matrix(NA_real_, k, k)
  f0 <- fn(x, ...); if (!is.finite(f0)) return(H)
  fp <- numeric(k); fm <- numeric(k)
  for (i in seq_len(k)) {
    xp <- x; xp[i] <- xp[i] + h
    xm <- x; xm[i] <- xm[i] - h
    fp[i] <- fn(xp, ...); fm[i] <- fn(xm, ...)
  }
  for (i in seq_len(k)) H[i, i] <- (fp[i] - 2 * f0 + fm[i]) / (h^2)
  for (i in seq_len(k - 1L)) for (j in seq.int(i + 1L, k)) {
    xpp <- x; xpp[i] <- xpp[i] + h; xpp[j] <- xpp[j] + h
    xpm <- x; xpm[i] <- xpm[i] + h; xpm[j] <- xpm[j] - h
    xmp <- x; xmp[i] <- xmp[i] - h; xmp[j] <- xmp[j] + h
    xmm <- x; xmm[i] <- xmm[i] - h; xmm[j] <- xmm[j] - h
    fij <- (fn(xpp, ...) - fn(xpm, ...) - fn(xmp, ...) + fn(xmm, ...)) / (4 * h^2)
    H[i, j] <- fij; H[j, i] <- fij
  }
  H
}

profile_neg_loglik_8p <- function(theta_raw, cache, config_8p,
                                  policy_max_iter = 500, policy_tol = 1e-7) {
  if (is.null(names(theta_raw))) names(theta_raw) <- config_8p$param_names
  eq <- tryCatch(
    solve_equilibrium_policy_replacement_8p(theta_raw, cache, config_8p,
                                            max_iter = policy_max_iter,
                                            tol = policy_tol),
    error = function(e) NULL)
  if (is.null(eq) || !eq$converged) return(1e10)
  npl_likelihood_replacement_8p(theta_raw, eq$P, cache, config_8p)
}

compute_am_se_8p <- function(fit, label) {
  cat(sprintf("\n[4] AM-SE for %s ...\n", label))
  t0 <- Sys.time()
  H <- tryCatch(
    finite_diff_hessian(profile_neg_loglik_8p, fit$theta_raw, h = 1e-3,
                        cache = fit$cache, config_8p = fit$config),
    error = function(e) { cat("    Hessian failed:", conditionMessage(e), "\n");
                          matrix(NA_real_, 8L, 8L) }
  )
  cat(sprintf("    elapsed %.1f sec\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (anyNA(H)) return(list(SE = rep(NA_real_, 8L)))
  Vinv <- tryCatch(solve(H), error = function(e) NULL)
  if (is.null(Vinv)) return(list(SE = rep(NA_real_, 8L)))
  diag_v <- diag(Vinv)
  SE <- sqrt(ifelse(diag_v > 0, diag_v, NA_real_))
  list(SE = SE, H = H, Vinv = Vinv)
}

am_obs <- compute_am_se_8p(fit_8p_obs, "observed")
if (RUN_EXTENDED_2000PLUS) {
  am_e2k <- compute_am_se_8p(fit_8p_e2k, "extended_2000plus")
}


# ---- 5. Build SE table with 95% CIs ---------------------------------------
build_row <- function(fit, am, label) {
  th_raw <- fit$theta_raw; th_hat <- fit$theta_hat; SE <- am$SE
  z <- qnorm(0.975)
  data.table(
    sample          = label,
    kappa_SW        = th_raw[["kappa_SW"]],       SE_kappa_SW    = SE[1],
    kappa_DW        = th_raw[["kappa_DW"]],       SE_kappa_DW    = SE[2],
    K_SW            = th_hat[["K_SW"]],
    SE_K_SW         = if (!is.na(SE[3])) th_hat[["K_SW"]] * SE[3] else NA_real_,
    K_DW            = th_hat[["K_DW"]],
    SE_K_DW         = if (!is.na(SE[4])) th_hat[["K_DW"]] * SE[4] else NA_real_,
    K_log_SW        = th_raw[["K_log_SW"]],       SE_K_log_SW    = SE[3],
    K_log_DW        = th_raw[["K_log_DW"]],       SE_K_log_DW    = SE[4],
    gamma_price_FF  = th_raw[["gamma_price_FF"]], SE_gamma_pF    = SE[5],
    gamma_price_RB  = th_raw[["gamma_price_RB"]], SE_gamma_pR    = SE[6],
    gamma_risk_FF   = th_raw[["gamma_risk_FF"]],  SE_gamma_rF    = SE[7],
    gamma_risk_RB   = th_raw[["gamma_risk_RB"]],  SE_gamma_rR    = SE[8],
    log_lik         = fit$log_likelihood,
    n_obs           = fit$cache$n_obs
  )
}
rows <- list()
if (RUN_OBSERVED) {
  rows[["observed"]] <- build_row(fit_8p_obs, am_obs,
                                  "observed (TX 2006+, controls 1999+)")
}
if (RUN_EXTENDED_2000PLUS) {
  rows[["extended_2000plus"]] <- build_row(fit_8p_e2k, am_e2k,
                                           "extended_2000plus (TX + controls 2000+)")
}
se_tab <- rbindlist(rows)
print(se_tab)
fwrite(se_tab, file.path(OUT_TAB, "04h_Theta_Table_8param_AM_SE.csv"))


# Build LaTeX table with AM SEs in (parentheses) and 95% CIs in [brackets].
# Sample columns are conditional on RUN_OBSERVED / RUN_EXTENDED_2000PLUS.
fmt_d <- function(x) formatC(round(x), format = "d", big.mark = ",")
cell_d <- function(p, se) {
  z <- qnorm(0.975)
  c(sprintf("\\$%s", fmt_d(p)),
    sprintf("(\\$%s)", fmt_d(se)),
    sprintf("[\\$%s, \\$%s]", fmt_d(p - z * se), fmt_d(p + z * se)))
}
cell_n <- function(p, se) {
  z <- qnorm(0.975)
  c(sprintf("%.3f", p),
    sprintf("(%.3f)", se),
    sprintf("[%.3f, %.3f]", p - z * se, p + z * se))
}
push <- function(label, cell_fn, get_p, get_se) {
  cols <- list()
  if (RUN_OBSERVED)          cols[["o"]] <- cell_fn(get_p(o), get_se(o))
  if (RUN_EXTENDED_2000PLUS) cols[["e"]] <- cell_fn(get_p(e), get_se(e))
  c(sprintf("%s & %s \\\\", label,
            paste(sapply(cols, `[`, 1), collapse = " & ")),
    sprintf("        & %s \\\\",
            paste(sapply(cols, `[`, 2), collapse = " & ")),
    sprintf("        & %s \\\\",
            paste(sapply(cols, `[`, 3), collapse = " & ")))
}
o <- if (RUN_OBSERVED)          se_tab[sample == "observed (TX 2006+, controls 1999+)"]   else NULL
e <- if (RUN_EXTENDED_2000PLUS) se_tab[sample == "extended_2000plus (TX + controls 2000+)"] else NULL

n_samp     <- as.integer(RUN_OBSERVED) + as.integer(RUN_EXTENDED_2000PLUS)
col_spec   <- paste0("l", paste(rep("c", n_samp), collapse = ""))
hdr_titles <- c(if (RUN_OBSERVED) "Observed (headline)" else NULL,
                if (RUN_EXTENDED_2000PLUS) "Extended (2000+)" else NULL)
hdr_sub    <- c(if (RUN_OBSERVED) "TX 2006+, controls 1999+" else NULL,
                if (RUN_EXTENDED_2000PLUS) "TX 2000+, controls 2000+" else NULL)
ll_cells   <- c(if (RUN_OBSERVED) formatC(round(o$log_lik), format = "d", big.mark = ",") else NULL,
                if (RUN_EXTENDED_2000PLUS) formatC(round(e$log_lik), format = "d", big.mark = ",") else NULL)
n_cells    <- c(if (RUN_OBSERVED) formatC(o$n_obs, format = "d", big.mark = ",") else NULL,
                if (RUN_EXTENDED_2000PLUS) formatC(e$n_obs, format = "d", big.mark = ",") else NULL)

tex <- c(
  "% Auto-generated by 04h_Replacement_8param_Estimation.R",
  "% AM (2002) profile-likelihood SEs in parentheses; 95% Wald CIs in brackets.",
  "% Bootstrap SEs and MC robustness on 8-param spec deferred.",
  sprintf("\\begin{tabular}{%s}", col_spec), "\\hline",
  sprintf(" & %s \\\\", paste(hdr_titles, collapse = " & ")),
  sprintf(" & %s \\\\", paste(hdr_sub,    collapse = " & ")),
  "\\hline",
  push("$\\kappa_{\\mathrm{SW}}$", cell_d,
       function(d) d$kappa_SW * SCALE_FACTOR, function(d) d$SE_kappa_SW * SCALE_FACTOR),
  push("$\\kappa_{\\mathrm{DW}}$", cell_d,
       function(d) d$kappa_DW * SCALE_FACTOR, function(d) d$SE_kappa_DW * SCALE_FACTOR),
  push("$K_{\\mathrm{SW}}$", cell_d,
       function(d) d$K_SW * SCALE_FACTOR, function(d) d$SE_K_SW * SCALE_FACTOR),
  push("$K_{\\mathrm{DW}}$", cell_d,
       function(d) d$K_DW * SCALE_FACTOR, function(d) d$SE_K_DW * SCALE_FACTOR),
  push("$\\gamma_{\\mathrm{price,FF}}$", cell_n,
       function(d) d$gamma_price_FF, function(d) d$SE_gamma_pF),
  push("$\\gamma_{\\mathrm{price,RB}}$", cell_n,
       function(d) d$gamma_price_RB, function(d) d$SE_gamma_pR),
  push("$\\gamma_{\\mathrm{risk,FF}}$",  cell_n,
       function(d) d$gamma_risk_FF, function(d) d$SE_gamma_rF),
  push("$\\gamma_{\\mathrm{risk,RB}}$",  cell_n,
       function(d) d$gamma_risk_RB, function(d) d$SE_gamma_rR),
  "\\hline",
  sprintf("$\\log L$ & %s \\\\", paste(ll_cells, collapse = " & ")),
  sprintf("$N$ obs  & %s \\\\", paste(n_cells,  collapse = " & ")),
  "\\hline",
  sprintf("\\multicolumn{%d}{l}{\\footnotesize Point estimates with AM (2002)", n_samp + 1L),
  "  profile-likelihood SEs in parentheses and 95\\% CIs in brackets.",
  "  MC robustness battery on 8-param spec deferred.}",
  "\\end{tabular}"
)
writeLines(tex, file.path(OUT_TAB, "04h_Theta_Table_8param_AM_SE.tex"))


# ---- 6. Per-cell fit numbers (high precision, observed sample) ------------
state_lut <- prims_obs$state_lut
P_hat <- as.data.table(fit_8p_obs$P_hat)
setnames(P_hat, c("model_M", "model_E", "model_R")); P_hat[, s_idx := seq_len(.N)]
emp <- obs_obs[, .(emp_M = mean(y_it == 0L),
                   emp_E = mean(y_it == 1L & I_replace == 0L, na.rm = TRUE),
                   emp_R = mean(y_it == 1L & I_replace == 1L, na.rm = TRUE),
                   n_cell = .N), by = s_idx][order(s_idx)]
cell <- merge(state_lut, merge(P_hat, emp, by = "s_idx"), by = "s_idx")
cell[, regime := fifelse(rho_state == 1L, "FF", "RB")]
cell[, wall   := fifelse(w_state == 1L, "SW", "DW")]
cell[, age_label := c("0-5","5-10","10-15","15-20",
                       "20-25","25-30","30-35","35+")[A_bin]]

r5 <- function(x) round(x, 5)
cell_wide <- cell[, .(s_idx, regime, wall, age_bin = A_bin, age_label, n_cell,
  model_maintain = r5(model_M), emp_maintain = r5(emp_M),
  res_maintain   = r5(emp_M - model_M),
  model_exit     = r5(model_E), emp_exit     = r5(emp_E),
  res_exit       = r5(emp_E - model_E),
  model_replace  = r5(model_R), emp_replace  = r5(emp_R),
  res_replace    = r5(emp_R - model_R)
)]
setorder(cell_wide, regime, wall, age_bin)
fwrite(cell_wide, file.path(OUT_TAB, "04h_8param_PerCell_Fit_Wide.csv"))


# Fit-quality summary by wall x regime x action
cell_long <- rbindlist(list(
  cell_wide[, .(s_idx, regime, wall, age_bin, n_cell, action = "Maintain",
                model = model_maintain, empirical = emp_maintain,
                residual = res_maintain)],
  cell_wide[, .(s_idx, regime, wall, age_bin, n_cell, action = "Exit",
                model = model_exit, empirical = emp_exit,
                residual = res_exit)],
  cell_wide[, .(s_idx, regime, wall, age_bin, n_cell, action = "Replace",
                model = model_replace, empirical = emp_replace,
                residual = res_replace)]
))
fit_quality <- cell_long[, .(
  N_cells     = .N, total_n = sum(n_cell),
  weighted_RMSE = sqrt(sum((residual^2) * n_cell) / sum(n_cell)),
  weighted_mean_residual = sum(residual * n_cell) / sum(n_cell),
  max_abs_residual = max(abs(residual))
), by = .(wall, regime, action)]
setorder(fit_quality, action, wall, regime)
print(fit_quality)
fwrite(fit_quality,
       file.path(OUT_TAB, "04h_8param_FitQuality_byWallRegimeAction.csv"))


# ---- 7. Per-wall fit figures (8-param) ------------------------------------
build_long <- function(dt) {
  rbind(
    dt[, .(s_idx, A_bin = age_bin, regime, n_cell,
           action = "Maintain", model = model_maintain, empirical = emp_maintain)],
    dt[, .(s_idx, A_bin = age_bin, regime, n_cell,
           action = "Exit",     model = model_exit,     empirical = emp_exit)],
    dt[, .(s_idx, A_bin = age_bin, regime, n_cell,
           action = "Replace",  model = model_replace,  empirical = emp_replace)]
  )
}
plot_one_wall <- function(dt, wall_label, outfile) {
  d <- build_long(dt)
  d[, action := factor(action, levels = c("Maintain","Exit","Replace"))]
  d[, regime := factor(regime, levels = c("FF","RB"))]
  thm <- theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold", size = 12),
          legend.position = "top",
          plot.title = element_text(face = "bold"))
  p <- ggplot(d, aes(x = A_bin, color = regime)) +
    geom_line(aes(y = model), linewidth = 1.0) +
    geom_point(aes(y = empirical, size = n_cell),
               shape = 21, fill = "white", stroke = 1) +
    facet_wrap(~ action, scales = "free_y", nrow = 1) +
    scale_x_continuous(breaks = 1:8,
                       labels = c("0-5","5-10","10-15","15-20",
                                  "20-25","25-30","30-35","35+")) +
    scale_size_area(max_size = 7, guide = "none") +
    thm + labs(
      title = sprintf("8-param fit — %s (observed sample)", wall_label),
      subtitle = "Solid line = MODEL-IMPLIED P(action | state); open circle = EMPIRICAL share (size = n obs)",
      x = "age bin (5-yr)", y = "P(action | state)", color = "regime")
  ggsave(outfile, p, width = 12, height = 4.5)
  cat(sprintf("    saved: %s\n", outfile))
}
cat("\n[7] Per-wall fit figures (8-param) ...\n")
plot_one_wall(cell_wide[wall == "SW"], "Single-Walled (or Mixed)",
              file.path(OUT_FIG, "04h_8param_Fit_SW.png"))
plot_one_wall(cell_wide[wall == "DW"], "Double-Walled",
              file.path(OUT_FIG, "04h_8param_Fit_DW.png"))


# ---- 8. Comparison vs 4-param (LL improvement, RMSE reduction) ------------
ll_compare <- data.table(spec = c("4-param", "8-param"))
if (RUN_OBSERVED) {
  ll_compare[, log_lik_obs := c(fit_4p_obs$log_likelihood,
                                fit_8p_obs$log_likelihood)]
  ll_compare[, LR_obs      := 2 * (log_lik_obs[2] - log_lik_obs[1])]
}
if (RUN_EXTENDED_2000PLUS) {
  ll_compare[, log_lik_e2k := c(fit_4p_e2k$log_likelihood,
                                fit_8p_e2k$log_likelihood)]
  ll_compare[, LR_e2k      := 2 * (log_lik_e2k[2] - log_lik_e2k[1])]
}
cat("\n[8] Likelihood comparison (4-param vs 8-param):\n")
print(ll_compare)
fwrite(ll_compare, file.path(OUT_TAB, "04h_LL_Compare_4p_vs_8p.csv"))
# LR test: 4 extra params, chi-sq(4) critical at 0.05 = 9.49
cat(sprintf("\n  LR test (chi-sq(4) crit at 0.05 = 9.49):\n"))
if (RUN_OBSERVED) {
  cat(sprintf("    observed:           LR = %.1f\n", unique(ll_compare$LR_obs)))
}
if (RUN_EXTENDED_2000PLUS) {
  cat(sprintf("    extended_2000plus:  LR = %.1f\n", unique(ll_compare$LR_e2k)))
}


# ---- 9. Summary ------------------------------------------------------------
if (RUN_OBSERVED) {
  stopifnot(file.exists(file.path(OUT_FIT,
    "Model_Replacement_8param_observed.rds")))
}

cat("\n--- 04h SUMMARY ---\n")
if (RUN_OBSERVED) {
  cat("8-param theta_hat (observed):\n")
  print(round(fit_8p_obs$theta_hat, 4))
}
if (RUN_EXTENDED_2000PLUS) {
  cat("\n8-param theta_hat (extended_2000plus):\n")
  print(round(fit_8p_e2k$theta_hat, 4))
}

cat("\nSaved:\n")
saved_files <- c(
  if (RUN_OBSERVED)
    file.path(OUT_FIT, "Model_Replacement_8param_observed.rds")
  else NULL,
  if (RUN_EXTENDED_2000PLUS)
    file.path(OUT_FIT, "Model_Replacement_8param_extended_2000plus.rds")
  else NULL,
  file.path(OUT_TAB, "04h_Theta_Table_8param_AM_SE.csv"),
  file.path(OUT_TAB, "04h_Theta_Table_8param_AM_SE.tex"),
  file.path(OUT_TAB, "04h_8param_PerCell_Fit_Wide.csv"),
  file.path(OUT_TAB, "04h_8param_FitQuality_byWallRegimeAction.csv"),
  file.path(OUT_TAB, "04h_LL_Compare_4p_vs_8p.csv"),
  file.path(OUT_FIG, "04h_8param_Fit_SW.png"),
  file.path(OUT_FIG, "04h_8param_Fit_DW.png")
)
for (f in saved_files) cat("  ", f, "\n")

cat("\n04h complete.\n")
