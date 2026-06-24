# 04ae_Identification_Figures_v2.R
# Presentation-polish: redesigned gamma-identification figures for the talk.
# The current 04q scatter shows (hazard, premium) GEOMETRY but not BEHAVIOR, so
# the audience can't see how the two gammas map to the closure decision. These
# candidates plot the RESPONSE and map each gamma to one visual feature:
#   gamma_risk  = slope of the flat-fee line (closing rises with risk at flat price)
#   gamma_price = vertical gap flat-fee -> risk-based at the same risk
# Pure downstream consumer of the canonical gammafree fit + observed panel. No
# estimation logic. Mirrors 04q's cell-level data assembly.

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
cat("=== 04ae: IDENTIFICATION FIGURES v2 ===\n")

FF_COL <- "#E76F51"   # flat-fee (orange) -- matches 04q
RB_COL <- "#2A9D8F"   # risk-based (teal) -- matches 04q
INK    <- "#1A1A1A"

base_theme <- theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        strip.text    = element_text(face = "bold"),
        panel.grid.minor = element_blank())

fig_dir <- here("Output", "Figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
save_png <- function(p, file, w = 8.5, h = 5.2) {
  ggsave(file.path(fig_dir, file), p, width = w, height = h, dpi = 150)
  cat(sprintf("  saved %s\n", file))
}

# ---- cell-level data (reuse 04q assembly) --------------------------------
fit <- readRDS(here("Output","Estimation_Results",
                    "Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds"))
ca <- fit$cache; lut <- ca$state_lut
cell_dt <- data.table(s_idx = lut$s_idx, A_bin = lut$A_bin,
  wall   = ifelse(lut$w_state == 1L, "SW", "DW"),
  regime = ifelse(lut$rho_state == 1L, "FF", "RB"),
  P = ca$P_vec * 10000, h = ca$h_vec)
obs <- fread(here("Data","Analysis","dcm_obs_panel_observed.csv"))
emp <- obs[, .(n_cell = .N, P_close = mean(y_it == 1L)), by = s_idx]
emp <- merge(cell_dt, emp, by = "s_idx", all.x = TRUE)
age_labs <- c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+")
emp[, regime := factor(regime, levels = c("FF","RB"))]

# ==========================================================================
# CANDIDATE 1 (DATA) -- behavioral "two worlds": empirical close rate by age,
# flat-fee vs risk-based, single-walled (the cells that drive the gradient).
# ==========================================================================
d1 <- emp[wall == "SW" & !is.na(P_close) & n_cell > 50]
p1 <- ggplot(d1, aes(x = A_bin, y = P_close, color = regime)) +
  geom_line(linewidth = 1.1) +
  geom_point(aes(size = n_cell), alpha = 0.85) +
  scale_color_manual(values = c(FF = FF_COL, RB = RB_COL),
                     labels = c("Flat-fee (control)", "Risk-based (Texas)")) +
  scale_size_area(max_size = 6, guide = "none") +
  scale_x_continuous(breaks = 1:8, labels = age_labs) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Tank age", y = "Annual closure rate  P(close)",
       color = NULL,
       title = "Two worlds, one tank: where the two responses live in the data",
       subtitle = "Single-walled cells. Flat-fee slope = risk response; flat-fee→risk-based gap = price response") +
  base_theme
save_png(p1, "04ae_Identif_TwoWorlds_Data.png")

# 2-panel variant (SW + DW)
d1b <- emp[!is.na(P_close) & n_cell > 50]
d1b[, wall := factor(wall, levels = c("SW","DW"))]
p1b <- ggplot(d1b, aes(x = A_bin, y = P_close, color = regime)) +
  geom_line(linewidth = 1.0) + geom_point(aes(size = n_cell), alpha = 0.85) +
  facet_wrap(~ wall) +
  scale_color_manual(values = c(FF = FF_COL, RB = RB_COL),
                     labels = c("Flat-fee (control)", "Risk-based (Texas)")) +
  scale_size_area(max_size = 5, guide = "none") +
  scale_x_continuous(breaks = 1:8, labels = age_labs) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Tank age", y = "Annual closure rate  P(close)", color = NULL,
       title = "Two worlds, by wall type",
       subtitle = "Flat-fee slope identifies gamma_risk; the across-regime gap identifies gamma_price") +
  base_theme
save_png(p1b, "04ae_Identif_TwoWorlds_Data_2panel.png", w = 9.5, h = 5.0)

# ==========================================================================
# CANDIDATE 2 (SCHEMATIC) -- clean cartoon, no data noise. Both lines share the
# gamma_risk slope; risk-based diverges as the premium prices the risk.
# ==========================================================================
xx <- seq(0, 10, length.out = 100)
sched <- rbind(
  data.table(x = xx, y = 0.5 + 0.15 * xx,                 regime = "Flat-fee"),
  data.table(x = xx, y = 0.5 + 0.15 * xx + 0.017 * xx^2,  regime = "Risk-based"))
sched[, regime := factor(regime, levels = c("Flat-fee","Risk-based"))]
xstar <- 7
ff_y <- 0.5 + 0.15 * xstar
rb_y <- 0.5 + 0.15 * xstar + 0.017 * xstar^2

p2 <- ggplot(sched, aes(x, y, color = regime)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("Flat-fee" = FF_COL, "Risk-based" = RB_COL)) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  labs(x = "Tank risk  (age →)", y = "P(close)", color = NULL,
       title = "How the two responses are identified",
       subtitle = "Same tank, same risk in both worlds — only the premium schedule differs") +
  base_theme +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(),
        axis.line = element_line(color = "grey70"))
save_png(p2, "04ae_Identif_TwoWorlds_Schematic.png")

# ==========================================================================
# CANDIDATE 4 (DATA) -- the CROSSOVER as the headline fact. RB re-times exit:
# discounts young (fewer exits) and penalizes old (more exits). The crossover
# IS gamma_price in age-space.
# ==========================================================================
d4 <- emp[wall == "SW" & !is.na(P_close) & n_cell > 50]
p4 <- ggplot(d4, aes(A_bin, P_close, color = regime)) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(size = n_cell), alpha = 0.85) +
  scale_color_manual(values = c(FF = FF_COL, RB = RB_COL),
                     labels = c("Flat-fee (control)", "Risk-based (Texas)")) +
  scale_size_area(max_size = 6, guide = "none") +
  scale_x_continuous(breaks = 1:8, labels = age_labs) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Tank age", y = "Annual closure rate  P(close)", color = NULL,
       title = "Risk-based pricing re-times exit: keep the young, retire the old",
       subtitle = "Single-walled closure rate. The crossover is the price effect — RB tilts the whole age profile.") +
  base_theme
save_png(p4, "04ae_Identif_Crossover.png")

# ==========================================================================
# CANDIDATE 5 (DATA) -- the two regressors, one per gamma. Reduced-form native:
# each gamma is a bivariate slope.
#   5a price channel : closure vs PREMIUM (the variation RB pricing creates)
#   5b risk  channel : closure vs HAZARD within FF (premium held flat)
# ==========================================================================
d5 <- emp[!is.na(P_close) & n_cell > 50]
d5[, wall := factor(wall, levels = c("SW","DW"))]
p5a <- ggplot(d5, aes(P, P_close)) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              color = "grey45", linetype = "dashed", linewidth = 0.8) +
  geom_point(aes(color = regime, size = n_cell, shape = wall), alpha = 0.8) +
  scale_color_manual(values = c(FF = FF_COL, RB = RB_COL),
                     labels = c("Flat-fee", "Risk-based")) +
  scale_size_area(max_size = 6, guide = "none") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Annual premium  P(s)  ($/tank-year)", y = "Annual closure rate",
       color = NULL, shape = "Wall",
       title = "Price channel  ->  gamma_price",
       subtitle = "Each point a state cell. RB pricing spreads the premium; closure rises across it.") +
  base_theme
save_png(p5a, "04ae_Identif_PriceChannel.png", w = 7.2, h = 5.0)

d5b <- emp[regime == "FF" & !is.na(P_close) & n_cell > 50]
d5b[, wall := factor(wall, levels = c("SW","DW"))]
p5b <- ggplot(d5b, aes(h, P_close)) +
  geom_smooth(method = "lm", se = FALSE, color = FF_COL,
              linetype = "dashed", linewidth = 0.8) +
  geom_point(aes(size = n_cell, shape = wall), color = FF_COL, alpha = 0.8) +
  scale_size_area(max_size = 6, guide = "none") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Annual leak probability  h(s)", y = "Annual closure rate",
       shape = "Wall",
       title = "Risk channel  ->  gamma_risk  (flat-fee world)",
       subtitle = "Premium ~flat here, so the closure-vs-risk slope is the own-risk response.") +
  base_theme
save_png(p5b, "04ae_Identif_RiskChannel.png", w = 7.2, h = 5.0)

cat("=== 04ae DONE ===\n")
