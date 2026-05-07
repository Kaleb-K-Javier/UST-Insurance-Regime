# ==============================================================================
# 04g_Fit_Numbers_and_CIs.R
# ==============================================================================
# PURPOSE
#   1. Export the high-precision (4-6 decimal) numbers behind the per-wall
#      fit figures so the reader does not have to eyeball ggplot output.
#      One row per state cell x action with model_P, empirical_P, residual,
#      and n_cell. Wide and long versions both saved.
#
#   2. Update the headline LaTeX results table to use the AM (2002)
#      profile-likelihood SEs as canonical (per user direction) and add
#      95% confidence intervals (theta_hat +/- 1.96 * SE). Bootstrap SEs
#      remain a deferred follow-up.
#
# OUTPUTS
#   Output/Tables/04g_Per_Cell_Fit_Numbers.csv          (long, all cells x actions)
#   Output/Tables/04g_Per_Cell_Fit_Numbers_Wide.csv     (one row per cell)
#   Output/Tables/04g_Theta_Table_AM_with_CIs.csv
#   Output/Tables/04g_Theta_Table_AM_with_CIs.tex
# ==============================================================================

# Worktree compatibility
.PARENT_RENV_LIB <- "C:/Users/kaleb/Documents/ust_ins_move_to_github/renv/library/windows/R-4.5/x86_64-w64-mingw32"
if (dir.exists(.PARENT_RENV_LIB) && !(.PARENT_RENV_LIB %in% .libPaths())) {
  .libPaths(c(.PARENT_RENV_LIB, .libPaths()))
}

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(here)
})

OUT_FIT <- here::here("Output", "Estimation_Results")
OUT_TAB <- here::here("Output", "Tables")
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)

SCALE_FACTOR <- 10000
PARENT_PROJECT_ROOT <- "C:/Users/kaleb/Documents/ust_ins_move_to_github"
parent_in <- function(...) {
  parent_p <- file.path(PARENT_PROJECT_ROOT, ...)
  local_p  <- here::here(...)
  if (file.exists(local_p))  return(local_p)
  if (file.exists(parent_p)) return(parent_p)
  stop("not found: ", ...)
}

cat("======================================================================\n")
cat("04g: high-precision per-cell fit numbers + AM-SE table with 95%% CIs\n")
cat("======================================================================\n")


# ==============================================================================
# 1. Load fits + primitives + observed obs panel
# ==============================================================================
fit_observed <- readRDS(file.path(OUT_FIT, "Model_Replacement_Estimates_observed.rds"))
fit_ext2k    <- readRDS(file.path(OUT_FIT, "Model_Replacement_Estimates_extended_2000plus.rds"))

prims_observed <- readRDS(parent_in("Output", "Estimation_Results",
                                    "DCM_Primitives_Replacement_observed.rds"))
obs_observed   <- fread(parent_in("Data", "Analysis",
                                  "dcm_obs_panel_observed.csv"))

# AM SE table from 04f
am_se_tab <- fread(file.path(OUT_TAB, "04f_Theta_Table_AM_SE.csv"))


# ==============================================================================
# 2. Per-cell fit numbers (high precision, observed sample)
# ==============================================================================
cat("\n[2] Building per-cell fit table (observed sample) ...\n")

state_lut <- prims_observed$state_lut
P_hat     <- as.data.table(fit_observed$P_hat)
setnames(P_hat, c("model_maintain", "model_exit", "model_replace"))
P_hat[, s_idx := seq_len(.N)]

emp <- obs_observed[, .(
  emp_maintain = mean(y_it == 0L),
  emp_exit     = mean(y_it == 1L & I_replace == 0L, na.rm = TRUE),
  emp_replace  = mean(y_it == 1L & I_replace == 1L, na.rm = TRUE),
  n_cell       = .N
), by = s_idx][order(s_idx)]

cell <- merge(state_lut, merge(P_hat, emp, by = "s_idx"), by = "s_idx")
cell[, regime    := fifelse(rho_state == 1L, "FF", "RB")]
cell[, wall      := fifelse(w_state == 1L, "SW", "DW")]
cell[, age_label := c("0-5","5-10","10-15","15-20",
                       "20-25","25-30","30-35","35+")[A_bin]]

# Rounded to 5 decimals (matters for Replace which is ~0.001-0.025)
round5 <- function(x) round(x, 5)
cell_wide <- cell[, .(
  s_idx, regime, wall, age_bin = A_bin, age_label, n_cell,
  model_maintain = round5(model_maintain), emp_maintain = round5(emp_maintain),
  res_maintain   = round5(emp_maintain - model_maintain),
  model_exit     = round5(model_exit),     emp_exit     = round5(emp_exit),
  res_exit       = round5(emp_exit - model_exit),
  model_replace  = round5(model_replace),  emp_replace  = round5(emp_replace),
  res_replace    = round5(emp_replace - model_replace)
)]
setorder(cell_wide, regime, wall, age_bin)
fwrite(cell_wide, file.path(OUT_TAB, "04g_Per_Cell_Fit_Numbers_Wide.csv"))
cat(sprintf("    saved (wide): %s\n",
            file.path(OUT_TAB, "04g_Per_Cell_Fit_Numbers_Wide.csv")))

# Long form: one row per (cell, action)
cell_long <- rbindlist(list(
  cell_wide[, .(s_idx, regime, wall, age_bin, age_label, n_cell,
                action = "Maintain",
                model = model_maintain, empirical = emp_maintain,
                residual = res_maintain)],
  cell_wide[, .(s_idx, regime, wall, age_bin, age_label, n_cell,
                action = "Exit",
                model = model_exit, empirical = emp_exit,
                residual = res_exit)],
  cell_wide[, .(s_idx, regime, wall, age_bin, age_label, n_cell,
                action = "Replace",
                model = model_replace, empirical = emp_replace,
                residual = res_replace)]
))
setorder(cell_long, action, regime, wall, age_bin)
fwrite(cell_long, file.path(OUT_TAB, "04g_Per_Cell_Fit_Numbers.csv"))
cat(sprintf("    saved (long): %s\n",
            file.path(OUT_TAB, "04g_Per_Cell_Fit_Numbers.csv")))


# ==============================================================================
# 3. Print summary fit-quality numbers by wall x regime x action
# ==============================================================================
cat("\n[3] Mean / weighted residuals by wall x regime x action:\n")

fit_quality <- cell_long[, .(
  N_cells          = .N,
  total_n          = sum(n_cell),
  unweighted_mean_residual    = mean(residual),
  weighted_mean_residual      = sum(residual * n_cell) / sum(n_cell),
  unweighted_RMSE             = sqrt(mean(residual^2)),
  weighted_RMSE               = sqrt(sum((residual^2) * n_cell) / sum(n_cell)),
  max_abs_residual            = max(abs(residual))
), by = .(wall, regime, action)]
setorder(fit_quality, action, wall, regime)
print(fit_quality)
fwrite(fit_quality, file.path(OUT_TAB, "04g_Fit_Quality_byWallRegimeAction.csv"))


# ==============================================================================
# 4. AM-SE results table with point estimates + AM SEs + 95% CIs
# ==============================================================================
cat("\n[4] Building AM-SE table with 95%% CIs ...\n")

# Helpers: convert raw-scale row from 04f's CSV into a CI-augmented row.
build_ci <- function(point_estimate, se, alpha = 0.05) {
  z <- qnorm(1 - alpha / 2)
  list(lower = point_estimate - z * se, upper = point_estimate + z * se)
}

build_ci_row <- function(row, label) {
  k_ci  <- build_ci(row$kappa_exit,  row$SE_kappa)
  K_ci  <- build_ci(row$K,           row$SE_K)
  gp_ci <- build_ci(row$gamma_price, row$SE_gp)
  gr_ci <- build_ci(row$gamma_risk,  row$SE_gr)
  data.table(
    sample          = label,
    kappa_exit      = row$kappa_exit,
    SE_kappa        = row$SE_kappa,
    CI_kappa_lo     = k_ci$lower,
    CI_kappa_hi     = k_ci$upper,
    K               = row$K,
    SE_K            = row$SE_K,
    CI_K_lo         = K_ci$lower,
    CI_K_hi         = K_ci$upper,
    gamma_price     = row$gamma_price,
    SE_gp           = row$SE_gp,
    CI_gp_lo        = gp_ci$lower,
    CI_gp_hi        = gp_ci$upper,
    gamma_risk      = row$gamma_risk,
    SE_gr           = row$SE_gr,
    CI_gr_lo        = gr_ci$lower,
    CI_gr_hi        = gr_ci$upper,
    log_lik         = row$log_lik,
    n_obs           = row$n_obs
  )
}

obs_row <- am_se_tab[grepl("^observed",          sample)][1L]
ext_row <- am_se_tab[grepl("^extended_2000plus", sample)][1L]

ci_tab <- rbindlist(list(
  build_ci_row(obs_row, "observed (TX 2006+, controls 1999+)"),
  build_ci_row(ext_row, "extended_2000plus (TX + controls 2000+)")
))
fwrite(ci_tab, file.path(OUT_TAB, "04g_Theta_Table_AM_with_CIs.csv"))


# LaTeX: point estimate, SE in parentheses below, 95% CI [lo, hi] beneath.
fmt_dollar <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt_num    <- function(x, d = 3) sprintf(paste0("%.", d, "f"), x)

push_dollar <- function(label, p_o, se_o, lo_o, hi_o,
                                 p_e, se_e, lo_e, hi_e) {
  c(
    sprintf("%s & \\$%s & \\$%s \\\\", label,
            fmt_dollar(p_o), fmt_dollar(p_e)),
    sprintf("        & (\\$%s) & (\\$%s) \\\\",
            fmt_dollar(se_o), fmt_dollar(se_e)),
    sprintf("        & [\\$%s, \\$%s] & [\\$%s, \\$%s] \\\\",
            fmt_dollar(lo_o), fmt_dollar(hi_o),
            fmt_dollar(lo_e), fmt_dollar(hi_e))
  )
}
push_num <- function(label, p_o, se_o, lo_o, hi_o,
                              p_e, se_e, lo_e, hi_e) {
  c(
    sprintf("%s & %s & %s \\\\", label,
            fmt_num(p_o), fmt_num(p_e)),
    sprintf("        & (%s) & (%s) \\\\",
            fmt_num(se_o), fmt_num(se_e)),
    sprintf("        & [%s, %s] & [%s, %s] \\\\",
            fmt_num(lo_o), fmt_num(hi_o),
            fmt_num(lo_e), fmt_num(hi_e))
  )
}

o <- ci_tab[1L]; e <- ci_tab[2L]
tex <- c(
  "% Auto-generated by 04g_Fit_Numbers_and_CIs.R",
  "% Standard errors are AM (2002) profile-likelihood, computed via",
  "% finite-difference Hessian over the equilibrium-policy fixed point.",
  "% Bootstrap SEs deferred for a future pass.",
  "\\begin{tabular}{lcc}",
  "\\hline",
  " & Observed (headline) & Extended (2000+) \\\\",
  " & TX 2006+, controls 1999+ & TX 2000+, controls 2000+ \\\\",
  "\\hline",
  push_dollar("$\\kappa_{\\mathrm{exit}}$",
    p_o  = o$kappa_exit  * SCALE_FACTOR, se_o = o$SE_kappa  * SCALE_FACTOR,
    lo_o = o$CI_kappa_lo * SCALE_FACTOR, hi_o = o$CI_kappa_hi * SCALE_FACTOR,
    p_e  = e$kappa_exit  * SCALE_FACTOR, se_e = e$SE_kappa  * SCALE_FACTOR,
    lo_e = e$CI_kappa_lo * SCALE_FACTOR, hi_e = e$CI_kappa_hi * SCALE_FACTOR),
  push_dollar("$K$",
    p_o = o$K  * SCALE_FACTOR, se_o = o$SE_K  * SCALE_FACTOR,
    lo_o = o$CI_K_lo * SCALE_FACTOR, hi_o = o$CI_K_hi * SCALE_FACTOR,
    p_e = e$K  * SCALE_FACTOR, se_e = e$SE_K  * SCALE_FACTOR,
    lo_e = e$CI_K_lo * SCALE_FACTOR, hi_e = e$CI_K_hi * SCALE_FACTOR),
  push_num("$\\gamma_{\\mathrm{price}}$",
    p_o = o$gamma_price, se_o = o$SE_gp,
    lo_o = o$CI_gp_lo,   hi_o = o$CI_gp_hi,
    p_e = e$gamma_price, se_e = e$SE_gp,
    lo_e = e$CI_gp_lo,   hi_e = e$CI_gp_hi),
  push_num("$\\gamma_{\\mathrm{risk}}$",
    p_o = o$gamma_risk, se_o = o$SE_gr,
    lo_o = o$CI_gr_lo,  hi_o = o$CI_gr_hi,
    p_e = e$gamma_risk, se_e = e$SE_gr,
    lo_e = e$CI_gr_lo,  hi_e = e$CI_gr_hi),
  "\\hline",
  sprintf("$\\log L$ & %s & %s \\\\",
          formatC(round(o$log_lik), format = "d", big.mark = ","),
          formatC(round(e$log_lik), format = "d", big.mark = ",")),
  sprintf("$N$ obs  & %s & %s \\\\",
          formatC(o$n_obs, format = "d", big.mark = ","),
          formatC(e$n_obs, format = "d", big.mark = ",")),
  "\\hline",
  "\\multicolumn{3}{l}{\\footnotesize Point estimates with AM (2002)",
  "  profile-likelihood standard errors in parentheses and 95\\% CIs in",
  "  brackets. Bootstrap SEs deferred.}",
  "\\end{tabular}"
)
writeLines(tex, file.path(OUT_TAB, "04g_Theta_Table_AM_with_CIs.tex"))


# ==============================================================================
# 5. Diagnostic preview
# ==============================================================================
cat("\n--- 04g preview: a few representative rows ---\n")
cat("\nReplace fit by wall x regime x age (rounded to 5 decimals):\n")
print(cell_wide[, .(regime, wall, age_label, n_cell,
                    model_replace, emp_replace, res_replace)][order(wall, regime, age_label)])

cat("\nFit quality (weighted RMSE) by wall x regime x action:\n")
print(fit_quality[, .(wall, regime, action, weighted_RMSE = round(weighted_RMSE, 5),
                      weighted_mean_residual = round(weighted_mean_residual, 5))])

cat("\nResults table (AM SEs + 95%% CIs):\n")
print(ci_tab[, .(sample,
                 kappa_exit, SE_kappa,
                 K,           SE_K,
                 gamma_price, SE_gp,
                 gamma_risk,  SE_gr)])

cat("\nSaved:\n")
for (f in c(
  file.path(OUT_TAB, "04g_Per_Cell_Fit_Numbers.csv"),
  file.path(OUT_TAB, "04g_Per_Cell_Fit_Numbers_Wide.csv"),
  file.path(OUT_TAB, "04g_Fit_Quality_byWallRegimeAction.csv"),
  file.path(OUT_TAB, "04g_Theta_Table_AM_with_CIs.csv"),
  file.path(OUT_TAB, "04g_Theta_Table_AM_with_CIs.tex")
)) cat("  ", f, "\n")

cat("\n04g complete.\n")
