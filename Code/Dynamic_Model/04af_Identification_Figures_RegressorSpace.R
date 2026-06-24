# 04af_Identification_Figures_RegressorSpace.R
# Presentation-polish: identification figures that put the MODEL'S REGRESSORS on
# the axes (premium and expected loss), answering "what variation fits the gammas".
# The structural model is a nonlinear regression of the keep/close choice on
#   u_M = 1 + gamma_price * P(s) - gamma_risk * (h(s)*L)   [L = cleanup today; -> D after OOP respec]
# so gamma_price / gamma_risk are slopes. These figures show where P and (h*L)
# vary independently enough to pin each slope:
#   - PRICE channel: premium gap at matched risk (FF vs RB at same age/wall)
#   - RISK  channel: risk gradient at flat premium (flat-fee world)
# Plus added-variable (partial-regression) plots = the exact orthogonal variation.
# Pure downstream consumer of the canonical gammafree fit + observed panel.

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
cat("=== 04af: IDENTIFICATION FIGURES (regressor space) ===\n")

FF_COL <- "#E76F51"; RB_COL <- "#2A9D8F"; INK <- "#1A1A1A"
base_theme <- theme_minimal(base_size = 13) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())
fig_dir <- here("Output", "Figures"); dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
save_png <- function(p, file, w = 8.3, h = 5.1) {
  ggsave(file.path(fig_dir, file), p, width = w, height = h, dpi = 150)
  cat(sprintf("  saved %s\n", file)) }

# ---- cell-level data (reuse 04q assembly; add expected-loss regressor) ----
fit <- readRDS(here("Output","Estimation_Results",
                    "Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds"))
ca <- fit$cache; lut <- ca$state_lut
cell_dt <- data.table(s_idx = lut$s_idx, A_bin = lut$A_bin,
  wall = ifelse(lut$w_state == 1L, "SW", "DW"),
  regime = ifelse(lut$rho_state == 1L, "FF", "RB"),
  P = ca$P_vec * 10000, h = ca$h_vec, haz = ca$hazard_loss * 10000)  # haz = h * L_cleanup ($)
obs <- fread(here("Data","Analysis","dcm_obs_panel_observed.csv"))
emp <- obs[, .(n_cell = .N, P_close = mean(y_it == 1L)), by = s_idx]
emp <- merge(cell_dt, emp, by = "s_idx", all.x = TRUE)
emp[, regime := factor(regime, levels = c("FF","RB"))]
emp[, wall   := factor(wall,   levels = c("SW","DW"))]
age_labs <- c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+")
d <- emp[!is.na(P_close) & n_cell > 50]

# ==========================================================================
# A2 -- SCHEMATIC, pivot-faithful (RB starts below FF, crosses, ends above).
# ==========================================================================
xx <- seq(0, 10, length.out = 120)
sched <- rbind(
  data.table(x = xx, y = 1.0 + 0.16 * xx,                  regime = "Flat-fee"),
  data.table(x = xx, y = 1.0 + 0.16 * xx + 0.18 * (xx - 5), regime = "Risk-based"))
sched[, regime := factor(regime, levels = c("Flat-fee","Risk-based"))]
gap_at <- function(x) 0.18 * (x - 5)
p_a2 <- ggplot(sched, aes(x, y, color = regime)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("Flat-fee" = FF_COL, "Risk-based" = RB_COL)) +
  labs(x = "Tank risk  (age ->)", y = "P(close)", color = NULL,
       title = "How the two responses are identified (schematic, pivot)",
       subtitle = "Same risk in both worlds; only the premium schedule differs. RB tilts the profile.") +
  base_theme +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(),
        axis.line = element_line(color = "grey70"))
save_png(p_a2, "04af_Identif_Schematic_Pivot.png")

# ==========================================================================
# B1b -- CROSSOVER with explicit gamma readings (FF slope = risk; gap = price).
# ==========================================================================
d_sw <- d[wall == "SW"]
p_b1b <- ggplot(d_sw, aes(A_bin, P_close, color = regime)) +
  geom_line(linewidth = 1.2) + geom_point(aes(size = n_cell), alpha = 0.85) +
  scale_color_manual(values = c(FF = FF_COL, RB = RB_COL),
                     labels = c("Flat-fee (control)","Risk-based (Texas)")) +
  scale_size_area(max_size = 6, guide = "none") +
  scale_x_continuous(breaks = 1:8, labels = age_labs) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Tank age", y = "Annual closure rate", color = NULL,
       title = "Identification, read off the data (single-walled)",
       subtitle = "Same age = same risk: flat-fee slope identifies gamma_risk; the FF<->RB gap identifies gamma_price") +
  base_theme
save_png(p_b1b, "04af_Identif_Crossover_Annotated.png")

# ==========================================================================
# C-price -- closure vs PREMIUM within leak-risk bands (hazard ~held fixed).
# Within each band, the premium slope is gamma_price.
# ==========================================================================
d[, h_tier := cut(h, breaks = quantile(h, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                  include.lowest = TRUE, labels = c("low risk","mid risk","high risk"))]
p_cp <- ggplot(d, aes(P, P_close, color = h_tier)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  geom_point(aes(size = n_cell), alpha = 0.8) +
  scale_color_manual(values = c("low risk" = "#A8DADC","mid risk" = "#457B9D","high risk" = "#1D3557")) +
  scale_size_area(max_size = 6, guide = "none") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Annual premium  P(s)  ($/tank-year)", y = "Annual closure rate",
       color = "Leak-risk band",
       title = "Price channel: closure vs premium, within risk bands",
       subtitle = "Hold risk ~fixed (color), and higher premium -> more closing. That within-band slope is gamma_price.") +
  base_theme
save_png(p_cp, "04af_Identif_PriceChannel_RiskBands.png")

# ==========================================================================
# C-risk -- closure vs EXPECTED LOSS, FF vs RB. The FF panel (flat premium) is
# the clean gamma_risk read.
# ==========================================================================
p_cr <- ggplot(d, aes(haz, P_close, color = wall)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  geom_point(aes(size = n_cell), alpha = 0.8) +
  facet_wrap(~ regime, labeller = as_labeller(c(FF = "Flat-fee (premium ~flat)", RB = "Risk-based"))) +
  scale_color_manual(values = c(SW = "#1D3557", DW = "#E76F51")) +
  scale_size_area(max_size = 5, guide = "none") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Expected loss  h(s) x L  ($/tank-year)", y = "Annual closure rate", color = "Wall",
       title = "Risk channel: closure vs expected loss",
       subtitle = "In the flat-fee panel the premium is ~constant, so the slope is the pure risk response = gamma_risk") +
  base_theme
save_png(p_cr, "04af_Identif_RiskChannel_byRegime.png", w = 9.3, h = 5.0)

# ==========================================================================
# D -- ADDED-VARIABLE (partial-regression) plots: the exact orthogonal
# variation each gamma uses. Linear analog of the choice model (intuition only).
# ==========================================================================
rg <- copy(d); rg[, w := n_cell]
flin <- lm(P_close ~ P + haz, data = rg, weights = w)
bp <- coef(flin)[["P"]]; br <- coef(flin)[["haz"]]
rg[, P_resid   := resid(lm(P ~ haz,        data = rg, weights = w))]
rg[, Yp_resid  := resid(lm(P_close ~ haz,  data = rg, weights = w))]
rg[, haz_resid := resid(lm(haz ~ P,        data = rg, weights = w))]
rg[, Yr_resid  := resid(lm(P_close ~ P,    data = rg, weights = w))]

p_dp <- ggplot(rg, aes(P_resid, Yp_resid)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linewidth = 0.9) +
  geom_point(aes(color = regime, size = w), alpha = 0.8) +
  scale_color_manual(values = c(FF = FF_COL, RB = RB_COL), labels = c("Flat-fee","Risk-based")) +
  scale_size_area(max_size = 4, guide = "none") + scale_x_continuous(labels = scales::dollar) +
  labs(x = "Premium  |  risk held fixed   (residual $)",
       y = "Closure  |  risk held fixed   (residual)", color = NULL,
       title = "Added-variable plot: gamma_price",
       subtitle = sprintf("Premium variation orthogonal to risk = the FF<->RB gap. Linear-analog slope = %.2e per $", bp),
       caption = "Each point = one age x wall x regime state cell (28 with data), sized by facility-years.") +
  base_theme
save_png(p_dp, "04af_Identif_AVP_Price.png", w = 7.4, h = 5.0)

p_dr <- ggplot(rg, aes(haz_resid, Yr_resid)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linewidth = 0.9) +
  geom_point(aes(color = regime, size = w), alpha = 0.8) +
  scale_color_manual(values = c(FF = FF_COL, RB = RB_COL), labels = c("Flat-fee","Risk-based")) +
  scale_size_area(max_size = 4, guide = "none") + scale_x_continuous(labels = scales::dollar) +
  labs(x = "Expected loss  |  premium held fixed   (residual $)",
       y = "Closure  |  premium held fixed   (residual)", color = NULL,
       title = "Added-variable plot: gamma_risk",
       subtitle = sprintf("Risk variation orthogonal to premium = the flat-fee age gradient. Slope = %.2e per $", br),
       caption = "Each point = one age x wall x regime state cell (28 with data), sized by facility-years.") +
  base_theme
save_png(p_dr, "04af_Identif_AVP_Risk.png", w = 7.4, h = 5.0)

# ==========================================================================
# E -- the two regressors and the response in ONE plot: premium (x) x expected
# loss (y), colored by closure rate. Shows the joint variation the model fits.
# ==========================================================================
p_e <- ggplot(d, aes(haz, P)) +
  geom_point(aes(color = P_close, size = n_cell, shape = regime), alpha = 0.9) +
  scale_color_gradient(low = "#FFE08A", high = "#8B1A1A",
                       labels = scales::percent_format(accuracy = 0.1)) +
  scale_size_area(max_size = 6, guide = "none") +
  scale_x_continuous(labels = scales::dollar) + scale_y_continuous(labels = scales::dollar) +
  guides(color = guide_colorbar(barwidth = 12, barheight = 0.6)) +
  labs(x = "Expected loss  h(s) x L  ($/tank-year)", y = "Annual premium  P(s)  ($/tank-year)",
       color = "P(close)", shape = "Regime",
       title = "The two regressors and the response",
       subtitle = "FF cells: flat low premium (bottom), risk varies left-right. RB cells: premium (top) tracks risk.") +
  base_theme
save_png(p_e, "04af_Identif_2D_Regressors.png")

cat(sprintf("  [linear-analog coefs] gamma_price~%.3e/$  gamma_risk~%.3e/$\n", bp, br))
cat("=== 04af DONE ===\n")
