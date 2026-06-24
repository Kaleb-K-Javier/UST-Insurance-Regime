# 04q_Export_Identification_Figures.R
# Polish-direct (presentation): export the identification-appendix ggplots as
# standalone PNGs for the talk deck. Pure downstream consumer of the canonical
# gammafree fit + the observed estimation panel. No estimation logic.
# Mirrors the chunks in Reports/Paper/Identification_6pFE_Appendix.qmd
# (fig-Ph, fig-logodds-P, fig-exit, fig-replace), sized for slides.

library(data.table)
library(ggplot2)
library(here)

cat("=== 04q: EXPORT IDENTIFICATION FIGURES ===\n")

theme_set(theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        strip.text    = element_text(face = "bold")))

fig_dir <- here("Output", "Figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# --- load canonical gammafree fit; build cell_dt (primitives) -------------
fit_path <- here("Output", "Estimation_Results",
                 "Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds")
stopifnot(file.exists(fit_path))
fit <- readRDS(fit_path)
ca  <- fit$cache
lut <- ca$state_lut
cell_dt <- data.table(
  s_idx  = lut$s_idx,
  A_bin  = lut$A_bin,
  wall   = ifelse(lut$w_state   == 1L, "SW", "DW"),
  regime = ifelse(lut$rho_state == 1L, "FF", "RB"),
  P      = ca$P_vec       * 10000,
  h      = ca$h_vec,
  haz    = ca$hazard_loss * 10000
)
cat(sprintf("  cell_dt rows: %d\n", nrow(cell_dt)))

# --- load observed panel; empirical CCPs by cell --------------------------
obs_path <- here("Data", "Analysis", "dcm_obs_panel_observed.csv")
stopifnot(file.exists(obs_path))
obs <- fread(obs_path)
emp <- obs[, .(
  n_cell  = .N,
  P_M     = mean(y_it == 0L),
  P_E     = mean(y_it == 1L & (is.na(I_replace) | I_replace == 0L)),
  P_R     = mean(y_it == 1L & !is.na(I_replace) & I_replace == 1L),
  P_close = mean(y_it == 1L)
), by = s_idx]
emp[, P_R_cond := P_R / pmax(P_close, 1e-12)]
emp[, P_E_cond := P_E / pmax(P_close, 1e-12)]
emp <- merge(cell_dt, emp, by = "s_idx", all.x = TRUE)
cat(sprintf("  emp cells with data: %d\n", emp[!is.na(P_M), .N]))

age_labs <- c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+")

save_png <- function(p, file, w = 8, h = 5) {
  ggsave(file.path(fig_dir, file), p, width = w, height = h, dpi = 150)
  cat(sprintf("  saved %s\n", file))
}

# --- (1) premium x hazard geometry (gamma_price identification) -----------
p_ph <- ggplot(cell_dt, aes(x = h, y = P, color = regime, shape = wall)) +
  geom_point(size = 4, alpha = 0.85) +
  scale_color_manual(values = c(FF = "#E76F51", RB = "#2A9D8F")) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Annual leak probability  h(s)",
       y = "Annual premium  P(s)  ($/tank-year)",
       color = "Regime", shape = "Wall",
       title = "Price-vs-risk identification geometry",
       subtitle = "FF cells: h varies at ~fixed P (pins gamma_price). RB cells: sloped locus.")
save_png(p_ph, "04q_Identif_PremiumHazard.png")

# --- (2) maintain log-odds vs premium within FF (gamma_price) -------------
d <- emp[regime == "FF" & !is.na(P_M) & n_cell > 0]
d[, logodds_M := log(pmax(P_M, 1e-6) / pmax(1 - P_M, 1e-6))]
d[, h_tercile := cut(h, breaks = quantile(h, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                     include.lowest = TRUE, labels = c("low h", "mid h", "high h"))]
p_lo <- ggplot(d, aes(x = P, y = logodds_M, color = h_tercile)) +
  geom_point(aes(size = n_cell), alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  scale_x_continuous(labels = scales::dollar) +
  scale_size_continuous(guide = "none") +
  labs(x = "Annual premium  P(s)  ($/tank-year)",
       y = "Empirical maintain log-odds",
       color = "Hazard tercile",
       title = "Maintain log-odds vs premium, FF cells",
       subtitle = "FF cells span a premium range at given hazard; structural gamma_price pools all cells")
save_png(p_lo, "04q_Identif_LogOdds_P.png")

# --- (3) exit share among closures (kappa) --------------------------------
de <- emp[!is.na(P_E_cond)]
p_ex <- ggplot(de, aes(x = A_bin, y = P_E_cond, color = wall, linetype = regime)) +
  geom_line(linewidth = 0.9) + geom_point(aes(size = n_cell), alpha = 0.7) +
  scale_x_continuous(breaks = 1:8, labels = age_labs) +
  scale_size_continuous(guide = "none") +
  labs(x = "Age bin", y = "P(Exit | closure)", color = "Wall", linetype = "Regime",
       title = "Exit share among closures, by age and wall",
       subtitle = "Level and age-profile identify kappa_SW, kappa_DW")
save_png(p_ex, "04q_Identif_ExitShare.png")

# --- (4) conditional replace share among closures (K) ---------------------
dr <- emp[!is.na(P_R_cond)]
p_rp <- ggplot(dr, aes(x = A_bin, y = P_R_cond, color = wall, linetype = regime)) +
  geom_line(linewidth = 0.9) + geom_point(aes(size = n_cell), alpha = 0.7) +
  scale_x_continuous(breaks = 1:8, labels = age_labs) +
  scale_size_continuous(guide = "none") +
  labs(x = "Age bin", y = "P(Replace | closure)", color = "Wall", linetype = "Regime",
       title = "Conditional replace share among closures",
       subtitle = "Both walls carry exit-vs-replace variation -> identifies K")
save_png(p_rp, "04q_Identif_ReplaceShare.png")

# --- (5) portfolio: replace share among closures by age x wall (pooled regime)
rep_age <- emp[!is.na(P_R) & n_cell > 0, .(
  R  = sum(P_R * n_cell),
  Cl = sum(P_close * n_cell)
), by = .(A_bin, wall)]
rep_age[, P_R_cond := R / pmax(Cl, 1e-9)]
p_rep <- ggplot(rep_age, aes(x = A_bin, y = P_R_cond, color = wall)) +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  scale_x_continuous(breaks = 1:8, labels = age_labs) +
  scale_color_manual(values = c(SW = "#2A9D8F", DW = "#E76F51")) +
  labs(x = "Tank age", y = "Replace share among closures", color = "Wall",
       title = "Young double-walled tanks \"replace\" the most",
       subtitle = "New DW tanks shouldn't need replacing -- it's portfolio churn, not real replacement")
save_png(p_rep, "04q_Replace_Portfolio.png")

cat("=== 04q DONE ===\n")
