# Code/Dynamic_Model/04k_Fit_Report_Artifacts.R
# T003: Harmonized goodness-of-fit artifacts + 3-way model comparison

# ---- Logging ---------------------------------------------------------------
.log_path <- here::here("logs", paste0(
  "04k_Fit_Report_Artifacts_",
  format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: Code/Dynamic_Model/04k_Fit_Report_Artifacts.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# ---- Packages --------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(here)
})

# ---- Paths -----------------------------------------------------------------
OUT_FIG <- here::here("Output", "Figures")
OUT_TAB <- here::here("Output", "Tables")
EST_DIR <- here::here("Output", "Estimation_Results")
DATA_DIR <- here::here("Data", "Analysis")


# ============================================================
cat("=== STEP 1: Load fits, primitives, observed panel ===\n")
# ============================================================

path_prims <- file.path(EST_DIR, "DCM_Primitives_Replacement_observed.rds")
path_4p    <- file.path(EST_DIR, "Model_Replacement_Estimates_observed.rds")
path_8p    <- file.path(EST_DIR, "Model_Replacement_8param_observed.rds")
path_fe    <- file.path(EST_DIR, "Model_Replacement_8paramFE_observed.rds")
path_obs   <- file.path(DATA_DIR, "dcm_obs_panel_observed.csv")

stopifnot(file.exists(path_prims), file.exists(path_4p),
          file.exists(path_8p),    file.exists(path_fe),
          file.exists(path_obs))

prims_obs <- readRDS(path_prims)
fit_4p    <- readRDS(path_4p)
fit_8p    <- readRDS(path_8p)
fit_fe    <- readRDS(path_fe)
fit_fe$P_hat <- fit_fe$Phat   # field name differs from 4p/8p convention
obs_obs   <- fread(path_obs)

stopifnot(is.numeric(fit_4p$log_likelihood), length(fit_4p$log_likelihood) == 1L)
stopifnot(is.numeric(fit_8p$log_likelihood), length(fit_8p$log_likelihood) == 1L)
stopifnot(is.numeric(fit_fe$log_likelihood), length(fit_fe$log_likelihood) == 1L)
stopifnot(is.numeric(fit_4p$theta_hat), is.numeric(fit_8p$theta_hat),
          is.numeric(fit_fe$theta_hat))
stopifnot(!is.null(fit_4p$P_hat), !is.null(fit_8p$P_hat), !is.null(fit_fe$P_hat))
stopifnot(identical(dim(fit_4p$P_hat), c(32L, 3L)))
stopifnot(identical(dim(fit_8p$P_hat), c(32L, 3L)))
stopifnot(identical(dim(fit_fe$P_hat), c(32L, 3L)))
stopifnot(all(abs(rowSums(fit_fe$P_hat) - 1) < 1e-6))

alpha_names <- grep("^alpha", names(fit_fe$theta_hat), value = TRUE)
stopifnot(length(alpha_names) > 0)
cat(sprintf("  fit_fe alpha fields in theta_hat: %d (%s .. %s)\n",
    length(alpha_names), alpha_names[1], alpha_names[length(alpha_names)]))

state_lut <- as.data.table(prims_obs$state_lut)
stopifnot("s_idx" %in% names(state_lut))
stopifnot(nrow(state_lut) == 32L)
stopifnot(all(c("A_bin", "w_state", "rho_state") %in% names(state_lut)))

stopifnot(all(c("y_it", "I_replace", "s_idx") %in% names(obs_obs)))

cat(sprintf("  prims_obs: %d states\n", nrow(state_lut)))
cat(sprintf("  obs panel: %d rows\n", nrow(obs_obs)))
cat(sprintf("  ll — 4p: %.1f  8p: %.1f  8p+FE: %.1f\n",
    fit_4p$log_likelihood, fit_8p$log_likelihood, fit_fe$log_likelihood))


# ============================================================
cat("=== STEP 2: Build empirical cell shares ===\n")
# ============================================================

emp <- obs_obs[, .(
  emp_M  = mean(y_it == 0L),
  emp_E  = mean(y_it == 1L & I_replace == 0L, na.rm = TRUE),
  emp_R  = mean(y_it == 1L & I_replace == 1L, na.rm = TRUE),
  n_cell = .N
), by = s_idx][order(s_idx)]

stopifnot(nrow(emp) == 32L)
stopifnot(all(abs(emp$emp_M + emp$emp_E + emp$emp_R - 1) < 1e-10))
stopifnot(all(emp$n_cell > 0L))
cat(sprintf("  emp: %d cells, total N=%d\n", nrow(emp), sum(emp$n_cell)))


# ============================================================
cat("=== STEP 3: Build per-fit cell tables ===\n")
# ============================================================

r5 <- function(x) round(x, 5)

build_cell_dt <- function(fit, emp, state_lut) {
  P_hat_dt <- as.data.table(fit$P_hat)
  setnames(P_hat_dt, c("model_M", "model_E", "model_R"))
  stopifnot(all(abs(P_hat_dt$model_M + P_hat_dt$model_E + P_hat_dt$model_R - 1) < 1e-6))
  P_hat_dt[, s_idx := seq_len(.N)]
  cell <- merge(state_lut, merge(P_hat_dt, emp, by = "s_idx"), by = "s_idx")
  cell[, regime    := fifelse(rho_state == 1L, "FF", "RB")]
  cell[, wall      := fifelse(w_state   == 1L, "SW", "DW")]
  cell[, age_label := c("0-5","5-10","10-15","15-20",
                        "20-25","25-30","30-35","35+")[A_bin]]
  cell_wide <- cell[, .(s_idx, regime, wall, age_bin = A_bin, age_label, n_cell,
    model_maintain = r5(model_M), emp_maintain = r5(emp_M),
    res_maintain   = r5(emp_M - model_M),
    model_exit     = r5(model_E), emp_exit     = r5(emp_E),
    res_exit       = r5(emp_E - model_E),
    model_replace  = r5(model_R), emp_replace  = r5(emp_R),
    res_replace    = r5(emp_R - model_R)
  )]
  setorder(cell_wide, regime, wall, age_bin)
  cell_wide
}

cell_4p <- build_cell_dt(fit_4p, emp, state_lut)
cell_8p <- build_cell_dt(fit_8p, emp, state_lut)
cell_fe <- build_cell_dt(fit_fe, emp, state_lut)

for (tag in c("cell_4p", "cell_8p", "cell_fe")) {
  ct <- get(tag)
  stopifnot(nrow(ct) == 32L)
  stopifnot(all(ct$model_maintain >= 0), all(ct$model_maintain <= 1))
  stopifnot(all(ct$model_exit     >= 0), all(ct$model_exit     <= 1))
  stopifnot(all(ct$model_replace  >= 0), all(ct$model_replace  <= 1))
  stopifnot(all(ct$emp_maintain   >= 0), all(ct$emp_maintain   <= 1))
  stopifnot(all(ct$emp_exit       >= 0), all(ct$emp_exit       <= 1))
  stopifnot(all(ct$emp_replace    >= 0), all(ct$emp_replace    <= 1))
  stopifnot(all(abs(ct$model_maintain + ct$model_exit + ct$model_replace - 1) < 2e-5))
  sar <- sum(abs(ct$res_maintain) + abs(ct$res_exit) + abs(ct$res_replace))
  stopifnot(is.finite(sar), sar < 6)
  cat(sprintf("  %s: 32 rows OK, sum|res|=%.4f\n", tag, sar))
}


# ============================================================
cat("=== STEP 4: Write 8p+FE per-cell fit CSV (deliverable #7) ===\n")
# ============================================================

out4 <- here::here("Output", "Tables", "04k_PerCell_Fit_8paramFE_Wide.csv")
fwrite(cell_fe[, .(s_idx, regime, wall, age_bin, age_label, n_cell,
                   model_maintain, emp_maintain, res_maintain,
                   model_exit, emp_exit, res_exit,
                   model_replace, emp_replace, res_replace)],
       out4)
stopifnot(file.exists(out4))
tmp4 <- fread(out4)
stopifnot(nrow(tmp4) == 32L, ncol(tmp4) == 15L)
cat(sprintf("  written: %s (32 rows, 15 cols)\n", out4))


# ============================================================
cat("=== STEP 5: 8p+FE fit-quality summary (deliverable #8) ===\n")
# ============================================================

cell_long_fe <- rbindlist(list(
  cell_fe[, .(s_idx, regime, wall, age_bin, n_cell, action = "Maintain",
              model = model_maintain, empirical = emp_maintain,
              residual = res_maintain)],
  cell_fe[, .(s_idx, regime, wall, age_bin, n_cell, action = "Exit",
              model = model_exit,     empirical = emp_exit,
              residual = res_exit)],
  cell_fe[, .(s_idx, regime, wall, age_bin, n_cell, action = "Replace",
              model = model_replace,  empirical = emp_replace,
              residual = res_replace)]
))
fit_quality_fe <- cell_long_fe[, .(
  N_cells                = .N,
  total_n                = sum(n_cell),
  weighted_RMSE          = sqrt(sum(residual^2 * n_cell) / sum(n_cell)),
  weighted_mean_residual = sum(residual * n_cell) / sum(n_cell),
  max_abs_residual       = max(abs(residual))
), by = .(wall, regime, action)]
setorder(fit_quality_fe, action, wall, regime)

out5 <- here::here("Output", "Tables", "04k_FitQuality_8paramFE_byWallRegimeAction.csv")
fwrite(fit_quality_fe, out5)
stopifnot(file.exists(out5))
stopifnot(nrow(fit_quality_fe) == 12L)
stopifnot(all(is.finite(fit_quality_fe$weighted_RMSE)))
stopifnot(all(fit_quality_fe$weighted_RMSE >= 0))
cat(sprintf("  written: %s (%d rows, %d cols)\n",
    out5, nrow(fit_quality_fe), ncol(fit_quality_fe)))


# ============================================================
cat("=== STEP 6: Harmonized per-wall fit figures (deliverables #1-#6) ===\n")
# ============================================================

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

plot_one_wall <- function(dt, wall_label, outfile, model_tag) {
  d <- build_long(dt)
  d[, action := factor(action, levels = c("Maintain", "Exit", "Replace"))]
  d[, regime := factor(regime, levels = c("FF", "RB"))]
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

plot_one_wall(cell_4p[wall == "SW"], "Single-Walled (or Mixed)",
              here::here("Output", "Figures", "04k_Fit_4param_SW.png"),   "4-param")
plot_one_wall(cell_4p[wall == "DW"], "Double-Walled",
              here::here("Output", "Figures", "04k_Fit_4param_DW.png"),   "4-param")
plot_one_wall(cell_8p[wall == "SW"], "Single-Walled (or Mixed)",
              here::here("Output", "Figures", "04k_Fit_8param_SW.png"),   "8-param")
plot_one_wall(cell_8p[wall == "DW"], "Double-Walled",
              here::here("Output", "Figures", "04k_Fit_8param_DW.png"),   "8-param")
plot_one_wall(cell_fe[wall == "SW"], "Single-Walled (or Mixed)",
              here::here("Output", "Figures", "04k_Fit_8paramFE_SW.png"), "8-param + FE")
plot_one_wall(cell_fe[wall == "DW"], "Double-Walled",
              here::here("Output", "Figures", "04k_Fit_8paramFE_DW.png"), "8-param + FE")

png_files <- here::here("Output", "Figures",
  c("04k_Fit_4param_SW.png",   "04k_Fit_4param_DW.png",
    "04k_Fit_8param_SW.png",   "04k_Fit_8param_DW.png",
    "04k_Fit_8paramFE_SW.png", "04k_Fit_8paramFE_DW.png"))
stopifnot(all(file.exists(png_files)))
cat("  all 6 PNGs written\n")


# ============================================================
cat("=== STEP 7: 3-way model comparison table (deliverables #9 and #10) ===\n")
# ============================================================

N    <- nrow(obs_obs)
ll_4  <- fit_4p$log_likelihood
ll_8  <- fit_8p$log_likelihood
ll_fe <- fit_fe$log_likelihood
k_4  <- 4L;  k_8 <- 8L;  k_fe <- 25L

calc_aic <- function(ll, k)    -2 * ll + 2 * k
calc_bic <- function(ll, k, n) -2 * ll + k * log(n)

lr_8v4  <- 2 * (ll_8  - ll_4)
lr_fev8 <- 2 * (ll_fe - ll_8)
p_8v4   <- 1 - pchisq(lr_8v4,  df = k_8  - k_4)
p_fev8  <- 1 - pchisq(lr_fev8, df = k_fe - k_8)

model_cmp <- data.table(
  model        = c("4-param", "8-param", "8-param+FE"),
  k_params     = c(k_4,  k_8,  k_fe),
  n_obs        = c(N,    N,    N),
  log_lik      = c(ll_4, ll_8, ll_fe),
  AIC          = c(calc_aic(ll_4, k_4),
                   calc_aic(ll_8, k_8),
                   calc_aic(ll_fe, k_fe)),
  BIC          = c(calc_bic(ll_4, k_4, N),
                   calc_bic(ll_8, k_8, N),
                   calc_bic(ll_fe, k_fe, N)),
  LR_vs_nested = c(NA_real_,  lr_8v4, lr_fev8),
  df_LR        = c(NA_integer_, k_8 - k_4, k_fe - k_8),
  pvalue_LR    = c(NA_real_,  p_8v4, p_fev8)
)

out9_csv <- here::here("Output", "Tables", "04k_Model_Comparison_3way.csv")
fwrite(model_cmp, out9_csv)
stopifnot(file.exists(out9_csv))
tmp9 <- fread(out9_csv)
stopifnot(nrow(tmp9) == 3L, ncol(tmp9) == 9L)
stopifnot(length(unique(tmp9$n_obs)) == 1L)
stopifnot(is.na(tmp9$LR_vs_nested[1L]))
stopifnot(is.na(tmp9$df_LR[1L]))
stopifnot(is.na(tmp9$pvalue_LR[1L]))
stopifnot(all(tmp9$log_lik < 0))
cat(sprintf("  CSV written: %s\n", out9_csv))

fmt_int <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_p   <- function(x) if (is.na(x)) "--" else sprintf("%.3g", x)
fmt_lr  <- function(x) if (is.na(x)) "--" else fmt_int(x)
fmt_df  <- function(x) if (is.na(x)) "--" else as.character(as.integer(x))

tex7_rows <- vapply(seq_len(3L), function(i) {
  r <- model_cmp[i]
  sprintf("%s & %d & %s & %s & %s & %s & %s & %s & %s \\\\",
    r$model, r$k_params, fmt_int(r$n_obs),
    fmt_int(r$log_lik), fmt_int(r$AIC), fmt_int(r$BIC),
    fmt_lr(r$LR_vs_nested), fmt_df(r$df_LR), fmt_p(r$pvalue_LR))
}, character(1L))

tex7 <- c(
  "\\resizebox{\\textwidth}{!}{",
  "\\begin{tabular}{lrrrrrrrr}",
  "\\hline",
  "Model & $k$ & $N$ & $\\log L$ & AIC & BIC & LR vs nested & df & $p$-value \\\\",
  "\\hline",
  tex7_rows,
  "\\hline",
  "\\multicolumn{9}{l}{\\footnotesize LR tests: 8p vs 4p, then 8p+FE vs 8p.}",
  "\\end{tabular}",
  "}"
)

out9_tex <- here::here("Output", "Tables", "04k_Model_Comparison_3way.tex")
writeLines(tex7, out9_tex)
stopifnot(file.exists(out9_tex))
stopifnot(grepl("\\resizebox", paste(tex7, collapse = "\n"), fixed = TRUE))
stopifnot(length(tex7_rows) == 3L)
cat(sprintf("  TeX written: %s\n", out9_tex))


# ============================================================
cat("=== STEP 8: Compact FE alphas table (deliverable #11) ===\n")
# ============================================================

path_alpha_csv <- here::here("Output", "Tables", "04iFETableAllControls.csv")
path_theta_csv <- here::here("Output", "Tables", "04i_Theta_Table_8paramFE_AM_SE.csv")
stopifnot(file.exists(path_alpha_csv), file.exists(path_theta_csv))

alpha_dt <- fread(path_alpha_csv)
theta_dt <- fread(path_theta_csv)

cat(sprintf("  04iFETableAllControls cols: %s\n",  paste(names(alpha_dt), collapse = ", ")))
cat(sprintf("  04i_Theta_Table cols:       %s\n",  paste(names(theta_dt), collapse = ", ")))
cat(sprintf("  alpha_dt head(1): %s\n", paste(unlist(alpha_dt[1L]), collapse = ", ")))
cat(sprintf("  theta_dt head(1): %s\n", paste(unlist(theta_dt[1L]), collapse = ", ")))

se_lookup <- theta_dt[, .(param = parameter, SE_raw)]
alpha_se  <- merge(alpha_dt, se_lookup, by = "param", all.x = TRUE)
stopifnot(!anyNA(alpha_se$SE_raw))
setorder(alpha_se, state)

stopifnot(nrow(alpha_se) == 17L)
body_rows    <- character(18L)
body_rows[1] <- "TX & 0 (baseline) \\\\"
for (i in seq_len(17L)) {
  body_rows[i + 1L] <- sprintf("%s & %s \\\\",
    alpha_se$state[i],
    sprintf("%.3f (%.3f)", alpha_se$alpha[i], alpha_se$SE_raw[i]))
}

tex8 <- c(
  "\\resizebox{\\textwidth}{!}{",
  "\\begin{tabular}{lc}",
  "\\hline",
  "State & $\\hat\\alpha_g$ (SE) \\\\",
  "\\hline",
  body_rows,
  "\\hline",
  "\\multicolumn{2}{l}{\\footnotesize Texas is the baseline (alpha set to 0).",
  "  Other states are estimated jointly with structural theta in the",
  "  25-parameter 8p+FE specification.}",
  "\\end{tabular}",
  "}"
)

out8 <- here::here("Output", "Tables", "04k_FE_Alphas_Compact.tex")
writeLines(tex8, out8)
stopifnot(file.exists(out8))
stopifnot(grepl("\\resizebox", paste(tex8, collapse = "\n"), fixed = TRUE))
stopifnot(length(body_rows) == 18L)
stopifnot(grepl("^TX", body_rows[1L]))
cat(sprintf("  TeX written: %s (18 body rows)\n", out8))


# ============================================================
cat("=== STEP 9: Summary ===\n")
# ============================================================

cat("== 04k Fit Report Artifacts ==\n")
cat(sprintf("  4p:   k=4   logL=%.0f  AIC=%.0f  BIC=%.0f\n",
    ll_4, calc_aic(ll_4, k_4), calc_bic(ll_4, k_4, N)))
cat(sprintf("  8p:   k=8   logL=%.0f  AIC=%.0f  BIC=%.0f  LRvs4p=%.1f (p=%.3g)\n",
    ll_8, calc_aic(ll_8, k_8), calc_bic(ll_8, k_8, N), lr_8v4, p_8v4))
cat(sprintf("  8pFE: k=25  logL=%.0f  AIC=%.0f  BIC=%.0f  LRvs8p=%.1f (p=%.3g)\n",
    ll_fe, calc_aic(ll_fe, k_fe), calc_bic(ll_fe, k_fe, N), lr_fev8, p_fev8))
cat("  Wrote 11 artifacts under Output/Figures and Output/Tables (04k_* prefix)\n")
