# ==============================================================================
# 04e_Replacement_ModelFit_DeepDive.R
# ==============================================================================
# PURPOSE
#   Three things in one place:
#
#     (1) Compute Hessian-based standard errors on the observed and extended
#         real-data fits and rebuild a presentation-ready LaTeX table.
#
#     (2) Investigate the gamma_price discontinuity (observed = -0.29 vs
#         extended = -1.14) by inspecting year x treatment empirical patterns.
#         The extended sample adds engine-imputed Mid-Continent premiums for
#         TX 1999-2005; if those imputed premiums correlate with anomalous
#         closure / replacement rates, gamma_price is mechanically pulled
#         more negative.
#
#     (3) Build model-fit figures and tables that clearly separate raw-data
#         empirical patterns from model-implied predictions, so a reader can
#         judge how well the structural model fits the data.
#
# OUTPUTS
#   Output/Tables/04e_Theta_Table_with_SE.csv
#   Output/Tables/04e_Theta_Table_with_SE.tex
#   Output/Tables/04e_Aggregate_Fit.csv
#   Output/Tables/04e_Cell_Residuals.csv
#   Output/Tables/04e_Action_Shares_by_Year_Treatment.csv
#   Output/Tables/04e_Premium_by_Year_Treatment.csv
#   Output/Figures/04e_Action_Shares_by_Year.png
#   Output/Figures/04e_Premium_by_Year_Treatment.png
#   Output/Figures/04e_CCPs_byAge_DataAndModel.png
#   Output/Figures/04e_Cell_Fit_Residuals.png
# ==============================================================================

# Allow the worktree to find packages in the parent project's renv.
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
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)

SCALE_FACTOR <- 10000   # 1 model unit = $10,000

# Helper: parent-project lookup so this works run from a worktree too.
PARENT_PROJECT_ROOT <- "C:/Users/kaleb/Documents/ust_ins_move_to_github"
parent_in <- function(...) {
  parent_p <- file.path(PARENT_PROJECT_ROOT, ...)
  local_p  <- here::here(...)
  if (file.exists(local_p))  return(local_p)
  if (file.exists(parent_p)) return(parent_p)
  stop("not found: ", ...)
}


# ==============================================================================
# 1. Load fits + primitives + obs panels
# ==============================================================================
cat("==================================================================\n")
cat("04e: Model-fit deep dive (SEs, gamma_price diagnostic, raw-vs-model)\n")
cat("==================================================================\n")

fit_observed  <- readRDS(file.path(OUT_FIT, "Model_Replacement_Estimates_observed.rds"))
fit_extended  <- readRDS(file.path(OUT_FIT, "Model_Replacement_Estimates_extended.rds"))

prims_observed <- readRDS(parent_in("Output", "Estimation_Results",
                                    "DCM_Primitives_Replacement_observed.rds"))
prims_extended <- readRDS(parent_in("Output", "Estimation_Results",
                                    "DCM_Primitives_Replacement_extended.rds"))

obs_observed <- fread(parent_in("Data", "Analysis", "dcm_obs_panel_observed.csv"))
obs_extended <- fread(parent_in("Data", "Analysis", "dcm_obs_panel_extended.csv"))


# ==============================================================================
# 2. Hessian-based standard errors (on raw scale, then delta to K)
# ==============================================================================
# Note: this is the pseudo-likelihood Hessian at converged (theta, P_hat),
# treating P as fixed. For a tighter NPL SE that accounts for sampling
# variation in P_hat, apply the AM (2002) correction; sufficient as a first
# pass given the MC robustness battery already showed tiny SEs at this n.
cat("\n[2] Computing Hessian SEs ...\n")

compute_se <- function(fit, label) {
  cat(sprintf("  -- %s\n", label))
  cache  <- fit$cache
  config <- fit$config
  P_hat  <- fit$P_hat
  theta_raw <- fit$theta_raw

  ll_fn <- function(th) {
    names(th) <- c("kappa_exit","K_log","gamma_price","gamma_risk")
    npl_likelihood_replacement(th, P_hat, cache, config)
  }
  t0 <- Sys.time()
  H <- tryCatch(numDeriv::hessian(ll_fn, theta_raw),
                error = function(e) {
                  cat("    Hessian failed:", conditionMessage(e), "\n")
                  matrix(NA_real_, 4L, 4L)
                })
  cat(sprintf("    elapsed %.1f sec\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  if (anyNA(H) || any(!is.finite(H))) {
    return(list(SE = rep(NA_real_, 4L),
                cor_mat = matrix(NA_real_, 4L, 4L),
                H = H))
  }
  Vinv <- tryCatch(solve(H), error = function(e) matrix(NA_real_, 4L, 4L))
  if (anyNA(Vinv)) {
    return(list(SE = rep(NA_real_, 4L),
                cor_mat = matrix(NA_real_, 4L, 4L),
                H = H))
  }
  SE <- sqrt(diag(Vinv))
  D  <- diag(1 / SE)
  cor_mat <- D %*% Vinv %*% D
  rownames(cor_mat) <- colnames(cor_mat) <- c("kappa_exit","K_log","gamma_price","gamma_risk")
  list(SE = SE, cor_mat = cor_mat, H = H, Vinv = Vinv)
}

se_observed <- compute_se(fit_observed, "observed")
se_extended <- compute_se(fit_extended, "extended")


# Delta-method: SE(K) = K * SE(K_log)
build_se_row <- function(fit, se_obj, label) {
  th_raw <- fit$theta_raw
  th_hat <- fit$theta_hat
  SE     <- se_obj$SE
  K_hat  <- th_hat[["K"]]

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
  build_se_row(fit_observed, se_observed, "observed (TX 2006+, controls 1999+)"),
  build_se_row(fit_extended, se_extended, "extended (TX 1999+ engine-imputed, controls 1999+)")
))
print(se_tab)
fwrite(se_tab, file.path(OUT_TAB, "04e_Theta_Table_with_SE.csv"))

# LaTeX with SE in (parentheses) under each estimate.
fmt_dollar <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt_se_dollar <- function(x) sprintf("(%s)", formatC(round(x), format = "d", big.mark = ","))
fmt_num <- function(x, d = 3) sprintf(paste0("%.", d, "f"), x)
fmt_se  <- function(x, d = 3) sprintf("(%.*f)", d, x)

tex <- c(
  "% Auto-generated by 04e_Replacement_ModelFit_DeepDive.R",
  "\\begin{tabular}{lcc}",
  "\\hline",
  " & Observed (headline) & Extended (robustness) \\\\",
  " & TX 2006+, controls 1999+ & TX 1999+ imputed pre-2006 \\\\",
  "\\hline"
)
push <- function(label, val_o, se_o, val_e, se_e, fmt_v, fmt_s, dollar = FALSE) {
  if (dollar) {
    fv <- function(x) sprintf("\\$%s", fmt_dollar(x))
    fs <- function(x) sprintf("(\\$%s)", fmt_dollar(x))
  } else {
    fv <- function(x) fmt_num(x, 3)
    fs <- function(x) fmt_se(x, 3)
  }
  c(
    sprintf("%s & %s & %s \\\\", label, fv(val_o), fv(val_e)),
    sprintf("        & %s & %s \\\\", fs(se_o), fs(se_e))
  )
}

# Pull values
o <- se_tab[1L]; e <- se_tab[2L]
tex <- c(tex,
  push("$\\kappa_{\\mathrm{exit}}$ (\\$)",
       o$kappa_exit * SCALE_FACTOR, o$SE_kappa * SCALE_FACTOR,
       e$kappa_exit * SCALE_FACTOR, e$SE_kappa * SCALE_FACTOR,
       NULL, NULL, dollar = TRUE),
  push("$K$ (\\$)",
       o$K * SCALE_FACTOR, o$SE_K * SCALE_FACTOR,
       e$K * SCALE_FACTOR, e$SE_K * SCALE_FACTOR,
       NULL, NULL, dollar = TRUE),
  push("$\\gamma_{\\mathrm{price}}$",
       o$gamma_price, o$SE_gp,
       e$gamma_price, e$SE_gp,
       NULL, NULL),
  push("$\\gamma_{\\mathrm{risk}}$",
       o$gamma_risk, o$SE_gr,
       e$gamma_risk, e$SE_gr,
       NULL, NULL),
  "\\hline",
  sprintf("$\\log L$ & %s & %s \\\\",
          formatC(round(o$log_lik), format = "d", big.mark = ","),
          formatC(round(e$log_lik), format = "d", big.mark = ",")),
  sprintf("$N$ obs  & %s & %s \\\\",
          formatC(o$n_obs, format = "d", big.mark = ","),
          formatC(e$n_obs, format = "d", big.mark = ",")),
  "\\hline",
  "\\end{tabular}"
)
writeLines(tex, file.path(OUT_TAB, "04e_Theta_Table_with_SE.tex"))


# ==============================================================================
# 3. gamma_price discontinuity diagnostic — empirical patterns by year x treatment
# ==============================================================================
cat("\n[3] gamma_price diagnostic ...\n")

# Tag rows by treatment status group:
#   "TX_imputed_pre2006" = texas_treated==1 & panel_year < 2006
#   "TX_observed_2006+"  = texas_treated==1 & panel_year >= 2006
#   "control"            = texas_treated==0
ext <- copy(obs_extended)
ext[, group := fcase(
  texas_treated == 1L & panel_year <  2006L, "TX_imputed_pre2006",
  texas_treated == 1L & panel_year >= 2006L, "TX_observed_2006+",
  texas_treated == 0L,                       "control",
  default = "other"
)]

action_by_year <- ext[, .(
  n_obs        = .N,
  maintain_sh  = mean(y_it == 0L),
  exit_sh      = mean(y_it == 1L & I_replace == 0L, na.rm = TRUE),
  replace_sh   = mean(y_it == 1L & I_replace == 1L, na.rm = TRUE),
  mean_premium = mean(premium, na.rm = TRUE)
), by = .(panel_year, group)][order(group, panel_year)]
fwrite(action_by_year,
       file.path(OUT_TAB, "04e_Action_Shares_by_Year_Treatment.csv"))
fwrite(action_by_year[, .(panel_year, group, mean_premium, n_obs)],
       file.path(OUT_TAB, "04e_Premium_by_Year_Treatment.csv"))

# Aggregate (one row per group)
agg_by_group <- ext[, .(
  n_obs        = .N,
  n_facilities = uniqueN(panel_id),
  maintain_sh  = mean(y_it == 0L),
  exit_sh      = mean(y_it == 1L & I_replace == 0L, na.rm = TRUE),
  replace_sh   = mean(y_it == 1L & I_replace == 1L, na.rm = TRUE),
  mean_premium = mean(premium, na.rm = TRUE)
), by = group]
cat("\n  Action shares by treatment group (extended panel):\n")
print(agg_by_group)


# Figure: action shares by year, faceted by action, colored by group.
# Empirical only — no model overlay since model is time-invariant and the
# point of this figure is to expose the time pattern that varies by group.
shares_long <- melt(action_by_year,
  id.vars      = c("panel_year", "group", "n_obs"),
  measure.vars = c("maintain_sh", "exit_sh", "replace_sh"),
  variable.name = "action", value.name = "share")
shares_long[, action := factor(action,
                               levels = c("maintain_sh","exit_sh","replace_sh"),
                               labels = c("Maintain (data)","Exit (data)","Replace (data)"))]
shares_long[, group := factor(group,
                              levels = c("control","TX_imputed_pre2006","TX_observed_2006+"))]

p_yrly <- ggplot(shares_long[action != "Maintain (data)"],
                 aes(x = panel_year, y = share, color = group)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  facet_wrap(~ action, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "gray30") +
  geom_vline(xintercept = 1999, linetype = "dashed", color = "gray30") +
  theme_minimal(base_size = 11) +
  labs(title = "Empirical Exit and Replace shares by year — RAW DATA",
       subtitle = "Vertical lines: 1999 (RB regulation) and 2006 (TX premium observation start)",
       x = "panel year", y = "share of obs in group-year",
       color = "treatment group")
ggsave(file.path(OUT_FIG, "04e_Action_Shares_by_Year.png"),
       p_yrly, width = 11, height = 5)


# Figure: mean premium by year x group
p_prem <- ggplot(action_by_year[!is.na(mean_premium)],
                 aes(x = panel_year, y = mean_premium * SCALE_FACTOR,
                     color = group)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.5) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "gray30") +
  geom_vline(xintercept = 1999, linetype = "dashed", color = "gray30") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal(base_size = 11) +
  labs(title = "Mean per-tank premium by year x treatment group — RAW DATA",
       subtitle = "TX_imputed_pre2006 = engine-imputed Mid-Continent premium for 1999-2005",
       x = "panel year", y = "mean premium per tank-year ($, USD)",
       color = "treatment group")
ggsave(file.path(OUT_FIG, "04e_Premium_by_Year_Treatment.png"),
       p_prem, width = 10, height = 5)


# ==============================================================================
# 4. Refit on extended panel WITHOUT pre-2006 TX rows (sanity counterfactual)
# ==============================================================================
# This isolates the obs-level driver: if the gamma_price shift comes purely
# from the imputed-pre-2006-TX rows, removing them from the extended panel
# should recover gamma_price ~ -0.29 (matching observed).
cat("\n[4] Refit on extended-minus-(TX pre-2006) ...\n")

ext_filtered <- ext[!(texas_treated == 1L & panel_year < 2006L)]
cat(sprintf("    rows kept: %s of %s (%.1f%% removed)\n",
            format(nrow(ext_filtered),     big.mark = ","),
            format(nrow(ext),              big.mark = ","),
            100 * (1 - nrow(ext_filtered) / nrow(ext))))

config <- create_estimation_config_replacement(beta = 0.95, sigma2 = 1.0,
                                               npl_iter = 200)
fit_ext_filtered <- npl_estimator_replacement(
  obs_panel  = ext_filtered,
  primitives = prims_extended,
  config     = config,
  theta_init = c(kappa_exit = 20, K_log = log(20),
                 gamma_price = -1.0, gamma_risk = 1.0),
  verbose    = TRUE
)
saveRDS(fit_ext_filtered,
        file.path(OUT_FIT, "Model_Replacement_Estimates_ext_no_pre2006TX.rds"))

cat("\n  Comparison: observed vs extended vs extended-without-pre2006TX\n")
cmp <- data.table(
  sample = c("observed", "extended", "extended_minus_preTX2006"),
  kappa_exit  = c(fit_observed$theta_hat[["kappa_exit"]],
                  fit_extended$theta_hat[["kappa_exit"]],
                  fit_ext_filtered$theta_hat[["kappa_exit"]]),
  K           = c(fit_observed$theta_hat[["K"]],
                  fit_extended$theta_hat[["K"]],
                  fit_ext_filtered$theta_hat[["K"]]),
  gamma_price = c(fit_observed$theta_hat[["gamma_price"]],
                  fit_extended$theta_hat[["gamma_price"]],
                  fit_ext_filtered$theta_hat[["gamma_price"]]),
  gamma_risk  = c(fit_observed$theta_hat[["gamma_risk"]],
                  fit_extended$theta_hat[["gamma_risk"]],
                  fit_ext_filtered$theta_hat[["gamma_risk"]])
)
print(cmp)
fwrite(cmp, file.path(OUT_TAB, "04e_Refit_Comparison.csv"))


# ==============================================================================
# 5. Model-vs-data figures with explicit RAW vs MODEL labeling
# ==============================================================================
# Headline fit object = OBSERVED (per user direction).
cat("\n[5] Model-vs-data figures (observed fit) ...\n")

state_lut <- prims_observed$state_lut
P_hat <- as.data.table(fit_observed$P_hat)
setnames(P_hat, c("model_maintain", "model_exit", "model_replace"))
P_hat[, s_idx := seq_len(.N)]

# Empirical CCPs from the observed obs panel
emp <- obs_observed[, .(
  emp_maintain = mean(y_it == 0L),
  emp_exit     = mean(y_it == 1L & I_replace == 0L, na.rm = TRUE),
  emp_replace  = mean(y_it == 1L & I_replace == 1L, na.rm = TRUE),
  n_cell       = .N
), by = s_idx][order(s_idx)]

cell_dt <- merge(state_lut, P_hat, by = "s_idx")
cell_dt <- merge(cell_dt, emp, by = "s_idx")
cell_dt[, regime := fifelse(rho_state == 1L, "FF (controls / TX pre-99)",
                                              "RB (TX 1999+)")]
cell_dt[, wall   := fifelse(w_state == 1L, "Single / Mixed", "Double-Walled")]

# Per-cell residuals
cell_dt[, res_maintain := emp_maintain - model_maintain]
cell_dt[, res_exit     := emp_exit     - model_exit]
cell_dt[, res_replace  := emp_replace  - model_replace]
fwrite(cell_dt, file.path(OUT_TAB, "04e_Cell_Residuals.csv"))


# CCP-by-age with empirical points overlaid on model lines
make_long <- function(dt, action_lbl, model_col, emp_col) {
  rbind(
    dt[, .(A_bin, regime, wall, n_cell,
           action = action_lbl, kind = "Model-implied",
           value = get(model_col))],
    dt[, .(A_bin, regime, wall, n_cell,
           action = action_lbl, kind = "Empirical (data)",
           value = get(emp_col))])
}
mfit_long <- rbindlist(list(
  make_long(cell_dt, "Maintain", "model_maintain", "emp_maintain"),
  make_long(cell_dt, "Exit",     "model_exit",     "emp_exit"),
  make_long(cell_dt, "Replace",  "model_replace",  "emp_replace")
))
mfit_long[, action := factor(action, levels = c("Maintain","Exit","Replace"))]
mfit_long[, kind   := factor(kind,   levels = c("Model-implied","Empirical (data)"))]

# Plot model lines (kind = Model-implied) and empirical points (kind = Empirical)
# faceted by action; color = regime, linetype = wall.
p_ccp <- ggplot() +
  geom_line(data = mfit_long[kind == "Model-implied"],
            aes(x = A_bin, y = value, color = regime,
                linetype = wall, group = interaction(regime, wall)),
            linewidth = 0.9) +
  geom_point(data = mfit_long[kind == "Empirical (data)"],
             aes(x = A_bin, y = value, color = regime,
                 shape = wall, size = n_cell),
             alpha = 0.85) +
  facet_wrap(~ action, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = c(1,4,8),
                     labels = c("0-5","15-20","35+")) +
  scale_size(range = c(1.5, 5), guide = "none") +
  theme_minimal(base_size = 11) +
  labs(title = "Model-implied (lines) vs empirical (points) CCPs by age",
       subtitle = "Observed sample fit; point size = obs in cell",
       x = "age bin", y = "P(action | state)",
       color = "regime", linetype = "wall (model)", shape = "wall (data)")
ggsave(file.path(OUT_FIG, "04e_CCPs_byAge_DataAndModel.png"),
       p_ccp, width = 12, height = 4.5)


# Cell-level residuals scatter (empirical - model) vs n_cell, faceted by action
res_long <- melt(
  cell_dt[, .(s_idx, A_bin, w_state, rho_state, regime, wall, n_cell,
              res_maintain, res_exit, res_replace)],
  id.vars = c("s_idx","A_bin","w_state","rho_state","regime","wall","n_cell"),
  measure.vars = c("res_maintain","res_exit","res_replace"),
  variable.name = "action", value.name = "residual")
res_long[, action := factor(action,
                            levels = c("res_maintain","res_exit","res_replace"),
                            labels = c("Maintain","Exit","Replace"))]

p_resid <- ggplot(res_long, aes(x = n_cell, y = residual,
                                color = regime, shape = wall)) +
  geom_hline(yintercept = 0, color = "gray40", linetype = "dashed") +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_x_log10(labels = scales::comma) +
  facet_wrap(~ action, scales = "free_y", ncol = 3) +
  theme_minimal(base_size = 11) +
  labs(title = "Per-cell residuals: empirical share - model-predicted P",
       subtitle = "Dashed = perfect fit; x = obs per cell (log scale)",
       x = "obs per cell", y = "empirical - model",
       color = "regime", shape = "wall")
ggsave(file.path(OUT_FIG, "04e_Cell_Fit_Residuals.png"),
       p_resid, width = 12, height = 4.5)


# ==============================================================================
# 6. Aggregate fit table — model vs empirical at totals + by regime + by wall
# ==============================================================================
weighted_share <- function(dt, model_col, emp_col, label) {
  data.table(
    group        = label,
    model_share  = sum(dt$n_cell * dt[[model_col]]) / sum(dt$n_cell),
    empirical_sh = sum(dt$n_cell * dt[[emp_col]])   / sum(dt$n_cell)
  )
}
agg_fit_one <- function(dt, group_label) {
  rbind(
    weighted_share(dt, "model_maintain", "emp_maintain",
                   paste0(group_label, " | Maintain")),
    weighted_share(dt, "model_exit",     "emp_exit",
                   paste0(group_label, " | Exit")),
    weighted_share(dt, "model_replace",  "emp_replace",
                   paste0(group_label, " | Replace"))
  )
}
agg_fit <- rbindlist(list(
  agg_fit_one(cell_dt,                                   "All cells"),
  agg_fit_one(cell_dt[rho_state == 1L],                  "FF regime"),
  agg_fit_one(cell_dt[rho_state == 2L],                  "RB regime"),
  agg_fit_one(cell_dt[w_state == 1L],                    "Single-walled"),
  agg_fit_one(cell_dt[w_state == 2L],                    "Double-walled")
))
agg_fit[, gap := empirical_sh - model_share]
print(agg_fit)
fwrite(agg_fit, file.path(OUT_TAB, "04e_Aggregate_Fit.csv"))


# ==============================================================================
# 7. Diagnostics summary
# ==============================================================================
cat("\n--- 04e diagnostic summary ---\n")
cat(sprintf("  observed gamma_price : %+.4f  (SE %.4f)\n",
            o$gamma_price, o$SE_gp))
cat(sprintf("  extended gamma_price : %+.4f  (SE %.4f)\n",
            e$gamma_price, e$SE_gp))
cat(sprintf("  ext-minus-preTX2006 gp: %+.4f\n",
            cmp$gamma_price[3]))
cat("\n  --> if ext-minus-preTX2006 ~~ observed, the discontinuity is\n")
cat("      driven entirely by the imputed-pre-2006 TX block.\n")

cat(sprintf("\n  RMSE of cell-level residuals (observed fit):\n"))
cat(sprintf("    Maintain : %.4f\n", sqrt(mean(cell_dt$res_maintain^2))))
cat(sprintf("    Exit     : %.4f\n", sqrt(mean(cell_dt$res_exit^2))))
cat(sprintf("    Replace  : %.4f\n", sqrt(mean(cell_dt$res_replace^2))))

cat("\nSaved:\n")
for (f in c(
  file.path(OUT_TAB, "04e_Theta_Table_with_SE.csv"),
  file.path(OUT_TAB, "04e_Theta_Table_with_SE.tex"),
  file.path(OUT_TAB, "04e_Action_Shares_by_Year_Treatment.csv"),
  file.path(OUT_TAB, "04e_Premium_by_Year_Treatment.csv"),
  file.path(OUT_TAB, "04e_Refit_Comparison.csv"),
  file.path(OUT_TAB, "04e_Cell_Residuals.csv"),
  file.path(OUT_TAB, "04e_Aggregate_Fit.csv"),
  file.path(OUT_FIG, "04e_Action_Shares_by_Year.png"),
  file.path(OUT_FIG, "04e_Premium_by_Year_Treatment.png"),
  file.path(OUT_FIG, "04e_CCPs_byAge_DataAndModel.png"),
  file.path(OUT_FIG, "04e_Cell_Fit_Residuals.png")
)) cat("  ", f, "\n")

cat("\n04e complete.\n")
