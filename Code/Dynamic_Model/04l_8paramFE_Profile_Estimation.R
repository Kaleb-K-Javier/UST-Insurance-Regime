# Code/Dynamic_Model/04l_8paramFE_Profile_Estimation.R
# T004: 8-param profile-likelihood estimator with state FE on {Maintain, Replace}

# ==============================================================================
# SECTION 1 — LOGGING
# ==============================================================================
.log_path <- here::here("logs", paste0(
  "04l_8paramFE_Profile_Estimation_",
  format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: Code/Dynamic_Model/04l_8paramFE_Profile_Estimation.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# ==============================================================================
# SECTION 2 — PACKAGES
# ==============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(Rcpp)
  library(here)
  library(ggplot2)
})
if (!requireNamespace("numDeriv", quietly = TRUE))
  stop("Package numDeriv required. Install with: install.packages('numDeriv')")

SCALE_FACTOR <- 10000L

OUT_FIT <- here::here("Output", "Estimation_Results")
OUT_TAB <- here::here("Output", "Tables")
OUT_FIG <- here::here("Output", "Figures")
dir.create(OUT_FIT, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# SECTION 3 — LOAD DATA AND FITS
# ==============================================================================
cat("=== SECTION 3: LOAD ===\n")

path_prims    <- here::here("Output", "Estimation_Results",
                            "DCM_Primitives_Replacement_observed.rds")
path_8p       <- here::here("Output", "Estimation_Results",
                            "Model_Replacement_8param_observed.rds")
path_fe_old   <- here::here("Output", "Estimation_Results",
                            "Model_Replacement_8paramFE_observed.rds")
path_obs      <- here::here("Data", "Analysis", "dcm_obs_panel_observed.csv")
path_cmp3     <- here::here("Output", "Tables", "04k_Model_Comparison_3way.csv")

stopifnot(file.exists(path_prims), file.exists(path_8p),
          file.exists(path_fe_old), file.exists(path_obs),
          file.exists(path_cmp3))

primitives  <- readRDS(path_prims)
fit_8p      <- readRDS(path_8p)
fit_fe_old  <- readRDS(path_fe_old)
obs_panel   <- fread(path_obs)
cmp3        <- fread(path_cmp3)

cat(sprintf("  obs_panel: %d rows\n", nrow(obs_panel)))
cat(sprintf("  fit_8p LL: %.3f\n", fit_8p$log_likelihood))
cat(sprintf("  fit_fe_old LL: %.3f\n", fit_fe_old$log_likelihood))
stopifnot(nrow(cmp3) == 3L, ncol(cmp3) == 9L)

# ==============================================================================
# SECTION 4 — SOURCE HELPERS + COMPILE CPP
# ==============================================================================
cat("=== SECTION 4: SOURCE + SOURCECPP ===\n")

source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))
Rcpp::sourceCpp(here::here("Code", "Helpers", "cpp_engine.cpp"))

stopifnot(exists("nll_replacement8pfe_profile_counts_cpp",   mode = "function"))
stopifnot(exists("update_ccps_geoweighted_profile_cpp",      mode = "function"))
stopifnot(exists("nll_replacement8pfe_counts_cpp",           mode = "function"))
stopifnot(exists("update_ccps_geoweighted_cpp",              mode = "function"))
cat("  All four expected C++ kernels present\n")

# ==============================================================================
# SECTION 5 — REGRESSION TEST (Step 7 spec): existing 8p+FE LL reproduced
# ==============================================================================
cat("=== SECTION 5: REGRESSION TEST — existing 8p+FE LL ===\n")

# Use saved cache directly: isolates the kernel-mod check from cache-rebuild
cache_old <- fit_fe_old$cache
stopifnot(!is.null(cache_old$countsdt8pfe), !is.null(cache_old$wsg8pfe))

ll_check <- -npl_likelihood_replacement_8p_fe(
  fit_fe_old$theta_raw,
  fit_fe_old$Phat,
  cache_old,
  fit_fe_old$config,
  alpha_in_R = FALSE
)
delta_ll <- abs(ll_check - fit_fe_old$log_likelihood)
cat(sprintf("  ll_check=%.6f  stored=%.6f  |delta|=%.3e\n",
            ll_check, fit_fe_old$log_likelihood, delta_ll))
# The stored log_likelihood was recorded at ll_path[iter] = -opt$value using
# Pfixed = P_{k-1}, while fit_fe_old$Phat = P_k (the updated CCP).
# These differ by the final NPL CCP step; delta up to ~1e-3 is expected.
stopifnot(delta_ll < 1e-3)
cat("  REGRESSION OK: existing 8p+FE LL reproduced to within 1e-3 (P-step gap expected)\n")

# ==============================================================================
# SECTION 6 — CONFIG
# ==============================================================================
cat("=== SECTION 6: BUILD CONFIG ===\n")

cfg <- create_estimation_config_replacement_8p_fe_profile()
cat(sprintf("  sigma2=%.1f  beta=%.2f  max_npl_iter=%d  tol_theta=%.0e  tol_P=%.0e\n",
            cfg$sigma2, cfg$beta, cfg$max_npl_iter, cfg$tol_theta, cfg$tol_P))
cat(sprintf("  epsprob=%.0e  alpha_bound=%g  alpha_newton_tol=%.0e\n",
            cfg$epsprob, cfg$alpha_bound, cfg$alpha_newton_tol))

# ==============================================================================
# SECTION 7 — ESTIMATE
# ==============================================================================
cat("=== SECTION 7: ESTIMATE (warm-start from 8p fit) ===\n")

thetastruct_init <- fit_8p$theta_raw[cfg$struct_param_names]   # theta_raw has K_log_*
cat("  Warm-start theta (from 8p):\n")
print(round(thetastruct_init, 4))

fit_new <- npl_estimator_replacement_8p_fe_profile(
  obs_panel        = obs_panel,
  primitives       = primitives,
  config           = cfg,
  thetastruct_init = thetastruct_init,
  verbose          = TRUE
)

cat(sprintf("\nEstimator: npl_estimator_replacement_8p_fe_profile | Sample: observed\n"))
cat(sprintf("Converged: %s at iter %d | LL: %.3f | Elapsed: %.1fs\n",
            fit_new$converged, fit_new$n_iter,
            fit_new$log_likelihood, fit_new$elapsed_sec))
cat("theta_hat:\n"); print(round(fit_new$theta_hat, 4))
cat("alpha_hat:\n"); print(round(fit_new$alpha_hat, 3))

# Sanity checks
stopifnot(length(fit_new$theta_hat) == 8L)
stopifnot(all(names(fit_new$theta_hat) == cfg$struct_param_names))
stopifnot(length(fit_new$alpha_hat) == 17L)
stopifnot(all(names(fit_new$alpha_hat) == paste0("alpha", CONTROLSTATES)))
stopifnot(all(dim(fit_new$P_hat) == c(32L, 3L)))
stopifnot(all(abs(rowSums(fit_new$P_hat) - 1) < 1e-8))
stopifnot(fit_new$log_likelihood > fit_8p$log_likelihood)
stopifnot(!any(is.nan(fit_new$se_theta)))
stopifnot(!any(is.nan(fit_new$se_alpha)))

# ==============================================================================
# SECTION 8 — SAVE RDS
# ==============================================================================
cat("=== SECTION 8: SAVE RDS ===\n")

path_out_rds <- here::here("Output", "Estimation_Results",
                           "Model_Replacement_8paramFE_profile_observed.rds")
saveRDS(fit_new, path_out_rds)
stopifnot(file.exists(path_out_rds))
cat(sprintf("  Saved: %s\n", path_out_rds))

# ==============================================================================
# SECTION 9 — THETA TABLE CSV + TEX (deliverables #5 and #6)
# ==============================================================================
cat("=== SECTION 9: THETA TABLE ===\n")

hat <- fit_new$theta_hat   # named 8-vec; K_log_SW, K_log_DW on log scale
SE  <- fit_new$se_theta    # named 8-vec
z   <- qnorm(0.975)

K_SW <- exp(hat[["K_log_SW"]])
K_DW <- exp(hat[["K_log_DW"]])

se_tab <- data.table(
  parameter = names(hat),
  estimate  = as.numeric(hat),
  SE_raw    = as.numeric(SE)
)

# Delta-method K_log -> K
se_tab[parameter == "K_log_SW", `:=`(
  estimate = K_SW,
  SE_raw   = if (!is.na(SE[["K_log_SW"]])) K_SW * SE[["K_log_SW"]] else NA_real_)]
se_tab[parameter == "K_log_DW", `:=`(
  estimate = K_DW,
  SE_raw   = if (!is.na(SE[["K_log_DW"]])) K_DW * SE[["K_log_DW"]] else NA_real_)]
se_tab[parameter %in% c("K_log_SW","K_log_DW"),
       parameter := sub("K_log_", "K_", parameter)]

# Scale kappa_* and K_* to $-units
se_tab[parameter %in% c("kappa_SW","kappa_DW","K_SW","K_DW"), `:=`(
  estimate = estimate * SCALE_FACTOR,
  SE_raw   = SE_raw   * SCALE_FACTOR)]

se_tab[, CI_lo := estimate - z * SE_raw]
se_tab[, CI_hi := estimate + z * SE_raw]

# Enforce row order per spec
param_order <- c("kappa_SW","kappa_DW","K_SW","K_DW",
                 "gamma_price_FF","gamma_price_RB","gamma_risk_FF","gamma_risk_RB")
se_tab <- se_tab[match(param_order, parameter)]

# Add logL and N_obs rows
N_obs <- nrow(obs_panel)
se_tab <- rbind(se_tab,
  data.table(parameter = "logL",  estimate = fit_new$log_likelihood,
             SE_raw = NA_real_, CI_lo = NA_real_, CI_hi = NA_real_),
  data.table(parameter = "N_obs", estimate = N_obs,
             SE_raw = NA_real_, CI_lo = NA_real_, CI_hi = NA_real_),
  fill = TRUE)

stopifnot(nrow(se_tab) == 10L)
print(se_tab)

# CSV
path_csv5 <- here::here("Output","Tables","04l_Theta_Table_8paramFE_profile_AM_SE.csv")
fwrite(se_tab, path_csv5)
stopifnot(file.exists(path_csv5))
tmp <- fread(path_csv5)
stopifnot(nrow(tmp) == 10L)
cat(sprintf("  CSV written: %s\n", path_csv5))

# TeX
fmt_d  <- function(x) formatC(round(x), format = "d", big.mark = ",")
push_d <- function(label, est, se, lo, hi)
  sprintf("%s & \\$%s & (\\$%s) & [\\$%s, \\$%s] \\\\",
          label, fmt_d(est), fmt_d(se), fmt_d(lo), fmt_d(hi))
push_n <- function(label, est, se, lo, hi)
  sprintf("%s & %.3f & (%.3f) & [%.3f, %.3f] \\\\", label, est, se, lo, hi)

tex6 <- c(
  "% Auto-generated by 04l_8paramFE_Profile_Estimation.R",
  "% Profile-likelihood SEs; alpha on {Maintain, Replace}; 95% Wald CIs in [brackets].",
  "\\begin{tabular}{lccc}", "\\hline",
  "Parameter & Estimate & (SE) & 95\\% CI \\\\", "\\hline"
)

dollar_params <- c("kappa_SW","kappa_DW","K_SW","K_DW")
label_map <- list(
  kappa_SW       = "$\\kappa_{\\mathrm{SW}}$",
  kappa_DW       = "$\\kappa_{\\mathrm{DW}}$",
  K_SW           = "$K_{\\mathrm{SW}}$",
  K_DW           = "$K_{\\mathrm{DW}}$",
  gamma_price_FF = "$\\gamma_{\\mathrm{price,FF}}$",
  gamma_price_RB = "$\\gamma_{\\mathrm{price,RB}}$",
  gamma_risk_FF  = "$\\gamma_{\\mathrm{risk,FF}}$",
  gamma_risk_RB  = "$\\gamma_{\\mathrm{risk,RB}}$"
)

for (lab in param_order) {
  r      <- se_tab[parameter == lab]
  pretty <- label_map[[lab]]
  if (lab %in% dollar_params) {
    tex6 <- c(tex6, push_d(pretty, r$estimate, r$SE_raw, r$CI_lo, r$CI_hi))
  } else {
    tex6 <- c(tex6, push_n(pretty, r$estimate, r$SE_raw, r$CI_lo, r$CI_hi))
  }
}

tex6 <- c(tex6,
  "\\hline",
  sprintf("$\\log L$ & \\multicolumn{3}{c}{%s} \\\\",
          formatC(round(fit_new$log_likelihood), format = "d", big.mark = ",")),
  sprintf("$N$ obs  & \\multicolumn{3}{c}{%s} \\\\",
          formatC(N_obs, format = "d", big.mark = ",")),
  "\\hline",
  "\\multicolumn{4}{l}{\\footnotesize 8-parameter structural fit with state FE on",
  "  \\{Maintain, Replace\\} profiled out via 1-D FOC at each $\\theta$-evaluation.",
  "  SEs are profile-likelihood AM (2002) form (Schur complement of joint 25-dim",
  "  Hessian). State-FE coefficients $\\alpha_g$ are reported in",
  "  Table 04l\\_FE\\_Alphas\\_profile\\_Compact.tex.}",
  "\\end{tabular}"
)

path_tex6 <- here::here("Output","Tables","04l_Theta_Table_8paramFE_profile_AM_SE.tex")
writeLines(tex6, path_tex6)
stopifnot(file.exists(path_tex6))
cat(sprintf("  TeX written: %s\n", path_tex6))

# ==============================================================================
# SECTION 10 — FE ALPHAS TABLE TEX (deliverable #7)
# ==============================================================================
cat("=== SECTION 10: FE ALPHAS TEX ===\n")

alpha_hat <- fit_new$alpha_hat   # named 17-vec
se_alpha  <- fit_new$se_alpha    # named 17-vec

tex7_rows <- vapply(seq_along(CONTROLSTATES), function(j) {
  state <- CONTROLSTATES[j]
  a_val <- alpha_hat[paste0("alpha", state)]
  a_se  <- se_alpha[paste0("alpha", state)]
  if (is.na(a_se)) {
    sprintf("%s & %.3f (NA) \\\\", state, a_val)
  } else {
    sprintf("%s & %.3f (%.3f) \\\\", state, a_val, a_se)
  }
}, character(1L))

tex7 <- c(
  "\\resizebox{\\textwidth}{!}{",
  "\\begin{tabular}{lc}",
  "\\hline",
  "State & $\\hat\\alpha_g$ (SE) \\\\",
  "\\hline",
  "TX & 0 (baseline) \\\\",
  tex7_rows,
  "\\hline",
  "\\multicolumn{2}{l}{\\footnotesize Texas is the baseline (alpha set to 0). The other 17 alphas are}",
  "\\multicolumn{2}{l}{\\footnotesize recovered at the converged $\\hat\\theta$ as $\\alpha_g(\\hat\\theta)$ from the profile-FOC}",
  "\\multicolumn{2}{l}{\\footnotesize (Eq. 3 of the spec); SEs from joint Hessian sub-block.}",
  "\\end{tabular}",
  "}"
)

path_tex7 <- here::here("Output","Tables","04l_FE_Alphas_profile_Compact.tex")
writeLines(tex7, path_tex7)
stopifnot(file.exists(path_tex7))
stopifnot(grepl("\\resizebox", paste(tex7, collapse = "\n"), fixed = TRUE))
stopifnot(sum(grepl("\\\\\\\\", tex7)) >= 18L)   # TX + 17 controls + hline rows
cat(sprintf("  TeX written: %s\n", path_tex7))

# ==============================================================================
# SECTION 11 — PER-CELL FIT TABLE + FIT QUALITY + FIGURES
#              (deliverables #8, #9, #12, #13)
# ==============================================================================
cat("=== SECTION 11: PER-CELL FIT + FIGURES ===\n")

state_lut <- as.data.table(primitives$state_lut)
stopifnot("s_idx" %in% names(state_lut), nrow(state_lut) == 32L)
stopifnot(all(c("A_bin","w_state","rho_state") %in% names(state_lut)))

# Empirical shares per state cell
dt_obs <- data.table::as.data.table(obs_panel)
data.table::setnames(dt_obs, c("s_idx","y_it","I_replace"),
                              c("sidx",  "yit",  "Ireplace"),
                     skip_absent = TRUE)
stopifnot(all(c("yit","Ireplace","sidx") %in% names(dt_obs)))

emp <- dt_obs[, .(
  emp_M  = mean(yit == 0L, na.rm = TRUE),
  emp_E  = mean(yit == 1L & Ireplace == 0L, na.rm = TRUE),
  emp_R  = mean(yit == 1L & Ireplace == 1L, na.rm = TRUE),
  n_cell = .N
), by = .(s_idx = sidx)][order(s_idx)]
stopifnot(nrow(emp) == 32L)
stopifnot(all(abs(emp$emp_M + emp$emp_E + emp$emp_R - 1) < 1e-10))

# Model shares from profile fit
r5 <- function(x) round(x, 5)
P_hat_dt <- as.data.table(fit_new$P_hat)
setnames(P_hat_dt, c("model_M","model_E","model_R"))
P_hat_dt[, s_idx := seq_len(.N)]
cell_new <- merge(state_lut, merge(P_hat_dt, emp, by = "s_idx"), by = "s_idx")

cell_new[, regime    := fifelse(rho_state == 1L, "FF", "RB")]
cell_new[, wall      := fifelse(w_state   == 1L, "SW", "DW")]
cell_new[, age_label := c("0-5","5-10","10-15","15-20",
                          "20-25","25-30","30-35","35+")[A_bin]]

cell_wide <- cell_new[, .(
  s_idx, regime, wall, age_bin = A_bin, age_label, n_cell,
  model_maintain = r5(model_M), emp_maintain = r5(emp_M),
  res_maintain   = r5(emp_M - model_M),
  model_exit     = r5(model_E), emp_exit     = r5(emp_E),
  res_exit       = r5(emp_E - model_E),
  model_replace  = r5(model_R), emp_replace  = r5(emp_R),
  res_replace    = r5(emp_R - model_R)
)]
setorder(cell_wide, regime, wall, age_bin)
stopifnot(nrow(cell_wide) == 32L)
stopifnot(all(abs(cell_wide$model_maintain + cell_wide$model_exit +
                   cell_wide$model_replace - 1) < 2e-5))

path_csv8 <- here::here("Output","Tables","04l_PerCell_Fit_8paramFE_profile_Wide.csv")
fwrite(cell_wide[, .(s_idx, regime, wall, age_bin, age_label, n_cell,
                     model_maintain, emp_maintain, res_maintain,
                     model_exit, emp_exit, res_exit,
                     model_replace, emp_replace, res_replace)],
       path_csv8)
stopifnot(file.exists(path_csv8))
tmp8 <- fread(path_csv8)
stopifnot(nrow(tmp8) == 32L, ncol(tmp8) == 15L)
cat(sprintf("  per-cell CSV written: %s\n", path_csv8))

# Fit quality by wall × regime × action
cell_long <- rbindlist(list(
  cell_wide[, .(s_idx, regime, wall, age_bin = age_bin, n_cell,
                action = "Maintain", model = model_maintain, empirical = emp_maintain,
                residual = res_maintain)],
  cell_wide[, .(s_idx, regime, wall, age_bin = age_bin, n_cell,
                action = "Exit",     model = model_exit,     empirical = emp_exit,
                residual = res_exit)],
  cell_wide[, .(s_idx, regime, wall, age_bin = age_bin, n_cell,
                action = "Replace",  model = model_replace,  empirical = emp_replace,
                residual = res_replace)]
))

fit_quality <- cell_long[, .(
  N_cells              = .N,
  total_n              = sum(n_cell),
  weighted_RMSE        = sqrt(sum(n_cell * residual^2) / sum(n_cell)),
  weighted_mean_resid  = sum(n_cell * residual) / sum(n_cell),
  max_abs_residual     = max(abs(residual))
), by = .(wall, regime, action)]
setnames(fit_quality, "weighted_mean_resid", "weighted_mean_residual")
setorder(fit_quality, action, wall, regime)
stopifnot(nrow(fit_quality) == 12L)

path_csv9 <- here::here("Output","Tables",
                        "04l_FitQuality_8paramFE_profile_byWallRegimeAction.csv")
fwrite(fit_quality, path_csv9)
stopifnot(file.exists(path_csv9))
cat(sprintf("  fit quality CSV written: %s\n", path_csv9))

# Figures (reuse plot_one_wall pattern from 04k)
build_long <- function(dt) {
  rbind(
    dt[, .(A_bin = age_bin, regime, n_cell,
           action = "Maintain", model = model_maintain, empirical = emp_maintain)],
    dt[, .(A_bin = age_bin, regime, n_cell,
           action = "Exit",     model = model_exit,     empirical = emp_exit)],
    dt[, .(A_bin = age_bin, regime, n_cell,
           action = "Replace",  model = model_replace,  empirical = emp_replace)]
  )
}

plot_one_wall <- function(dt, wall_label, outfile, model_tag) {
  d <- build_long(dt)
  d[, action := factor(action, levels = c("Maintain","Exit","Replace"))]
  d[, regime := factor(regime, levels = c("FF","RB"))]
  thm <- theme_minimal(base_size = 12) +
    theme(strip.text      = element_text(face = "bold", size = 12),
          legend.position = "top",
          plot.title      = element_text(face = "bold"))
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
      title    = sprintf("%s fit — %s (observed sample)", model_tag, wall_label),
      subtitle = "Solid line = MODEL-IMPLIED P(action | state); open circle = EMPIRICAL share (size = n obs)",
      x = "age bin (5-yr)", y = "P(action | state)", color = "regime")
  ggsave(outfile, p, width = 12, height = 4.5)
  cat(sprintf("    saved: %s\n", outfile))
}

path_png_sw <- here::here("Output","Figures","04l_Fit_8paramFE_profile_SW.png")
path_png_dw <- here::here("Output","Figures","04l_Fit_8paramFE_profile_DW.png")

plot_one_wall(cell_wide[wall == "SW"], "Single-Walled (or Mixed)",
              path_png_sw, "8p + stayFE (profile)")
plot_one_wall(cell_wide[wall == "DW"], "Double-Walled",
              path_png_dw, "8p + stayFE (profile)")

stopifnot(file.exists(path_png_sw), file.exists(path_png_dw))
cat("  both PNGs written\n")

# ==============================================================================
# SECTION 12 — 4-WAY COMPARISON TABLE (deliverables #10 and #11)
# ==============================================================================
cat("=== SECTION 12: 4-WAY COMPARISON TABLE ===\n")

calc_aic <- function(ll, k)    -2 * ll + 2 * k
calc_bic <- function(ll, k, n) -2 * ll + k * log(n)
fmt_int  <- function(x) if (is.na(x)) "---" else
  formatC(round(x), format = "d", big.mark = ",")
fmt_lr   <- function(x) if (is.na(x)) "---" else sprintf("%.1f", x)
fmt_df   <- function(x) if (is.na(x)) "---" else as.character(x)
fmt_p    <- function(x) if (is.na(x)) "---" else sprintf("%.3g", x)

# Rows 1-3: read from existing 3-way CSV; rename row 3 label for clarity
N       <- cmp3[model == "8-param"]$n_obs
ll_4p   <- cmp3[model == "4-param"]$log_lik
ll_8p   <- cmp3[model == "8-param"]$log_lik
ll_feM  <- cmp3[grepl("FE", model)]$log_lik   # maintain-only FE
stopifnot(!is.na(N), !is.na(ll_4p), !is.na(ll_8p), !is.na(ll_feM))

ll_new  <- fit_new$log_likelihood
k_4     <- 4L; k_8 <- 8L; k_fe <- 25L; k_new <- 25L

lr_8v4     <- 2 * (ll_8p  - ll_4p)
lr_feMv8   <- 2 * (ll_feM - ll_8p)
lr_newv8   <- 2 * (ll_new - ll_8p)
p_8v4      <- 1 - pchisq(lr_8v4,   df = k_8  - k_4)
p_feMv8    <- 1 - pchisq(lr_feMv8, df = k_fe - k_8)
p_newv8    <- 1 - pchisq(lr_newv8, df = k_new - k_8)

cmp4 <- data.table(
  model        = c("4-param",
                   "8-param",
                   "8-param + FE (M-only, joint)",
                   "8-param + FE (stay, profile)"),
  k_params     = c(k_4,   k_8,   k_fe,   k_new),
  n_obs        = c(N,     N,     N,      N),
  log_lik      = c(ll_4p, ll_8p, ll_feM, ll_new),
  AIC          = c(calc_aic(ll_4p, k_4),
                   calc_aic(ll_8p, k_8),
                   calc_aic(ll_feM, k_fe),
                   calc_aic(ll_new, k_new)),
  BIC          = c(calc_bic(ll_4p, k_4,  N),
                   calc_bic(ll_8p, k_8,  N),
                   calc_bic(ll_feM, k_fe, N),
                   calc_bic(ll_new, k_new, N)),
  LR_vs_nested = c(NA_real_,  lr_8v4, lr_feMv8, lr_newv8),
  df_LR        = c(NA_integer_, k_8 - k_4, k_fe - k_8, k_new - k_8),
  pvalue_LR    = c(NA_real_,  p_8v4, p_feMv8, p_newv8)
)

stopifnot(nrow(cmp4) == 4L, ncol(cmp4) == 9L)
stopifnot(length(unique(cmp4$n_obs)) == 1L)
print(cmp4)

path_csv10 <- here::here("Output","Tables","04l_Model_Comparison_4way.csv")
fwrite(cmp4, path_csv10)
stopifnot(file.exists(path_csv10))
tmp10 <- fread(path_csv10)
stopifnot(nrow(tmp10) == 4L, ncol(tmp10) == 9L)
cat(sprintf("  4-way CSV written: %s\n", path_csv10))

# TeX
tex11_rows <- vapply(seq_len(nrow(cmp4)), function(i) {
  r <- cmp4[i]
  sprintf("%s & %d & %s & %s & %s & %s & %s & %s & %s \\\\",
    r$model, r$k_params, fmt_int(r$n_obs),
    fmt_int(r$log_lik),  fmt_int(r$AIC), fmt_int(r$BIC),
    fmt_lr(r$LR_vs_nested), fmt_df(r$df_LR), fmt_p(r$pvalue_LR))
}, character(1L))

tex11 <- c(
  "\\resizebox{\\textwidth}{!}{",
  "\\begin{tabular}{lrrrrrrrr}",
  "\\hline",
  "Model & $k$ & $N$ & $\\log L$ & AIC & BIC & LR vs nested & df & $p$-value \\\\",
  "\\hline",
  tex11_rows,
  "\\hline",
  "\\multicolumn{9}{l}{\\footnotesize LR tests: 8-param vs 4-param (df=4);",
  "  8p+FE (M-only, joint) vs 8-param (df=17);",
  "  8p+FE (stay, profile) vs 8-param (df=17).",
  "  Rows 3 and 4 are non-nested with each other. Both report k=25 (profile-out",
  "  does not reduce free parameters of the model, only optimizer dimensionality).}",
  "\\end{tabular}",
  "}"
)

path_tex11 <- here::here("Output","Tables","04l_Model_Comparison_4way.tex")
writeLines(tex11, path_tex11)
stopifnot(file.exists(path_tex11))
stopifnot(grepl("\\resizebox", paste(tex11, collapse = "\n"), fixed = TRUE))
cat(sprintf("  4-way TeX written: %s\n", path_tex11))

# ==============================================================================
# SECTION 13 — SUMMARY
# ==============================================================================
cat("\n=== SECTION 13: SUMMARY ===\n")
cat(sprintf("Estimator: npl_estimator_replacement_8p_fe_profile | Sample: observed | Converged: %s at iter %d | LL: %.3f\n",
            fit_new$converged, fit_new$n_iter, fit_new$log_likelihood))
cat(sprintf("theta_hat: %s\n",
            paste(names(fit_new$theta_hat),
                  sprintf("%.4f", fit_new$theta_hat), sep = "=", collapse = "  ")))
cat(sprintf("Elapsed: %.1fs | Saved: %s\n", fit_new$elapsed_sec, path_out_rds))
cat(sprintf("LL improvement vs 8p: %.1f  vs 8p+FE(M-only): %.1f\n",
            ll_new - ll_8p, ll_new - ll_feM))

cat("\nDeliverables written:\n")
for (f in c(path_out_rds, path_csv5, path_tex6, path_tex7,
            path_csv8, path_csv9, path_png_sw, path_png_dw,
            path_csv10, path_tex11)) {
  cat(sprintf("  [%s] %s\n", if (file.exists(f)) "OK" else "MISSING", f))
}
stopifnot(all(file.exists(c(path_out_rds, path_csv5, path_tex6, path_tex7,
                             path_csv8, path_csv9, path_png_sw, path_png_dw,
                             path_csv10, path_tex11))))
cat("\n04l complete.\n")
