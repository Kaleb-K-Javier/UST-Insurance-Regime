#==============================================================================
# 01f_CapitalStock.R
# Capital Stock Characterization Figures (Dec 22 1998 cross-section)
#
# Figure 1 — Tank Age Density
#   PanelA: density plot TX vs Control
#   PanelB: ECDF TX vs Control  (quantile comparison)
#   Combined → Figure1_AgeDensity_Combined
#
# Figure 2 — Installation Vintage Composition
#   PanelA: stacked bar (share) TX vs Control
#   PanelB: diverging bar (TX minus Control share gap)
#   Combined → Figure2_VintageComposition_Combined
#
# Figure 3 — Single-Wall Share by Vintage
#   PanelA: grouped bar TX vs Control
#   PanelB: SW share line over detailed install cohort (5-yr bins)
#   Combined → Figure3_WallByVintage_Combined
#==============================================================================

source(here::here("Code", "01a_Setup.R"))
cat("=== 01f: CAPITAL STOCK FIGURES ===\n")

tanks_1998 <- load_interim("tanks_1998")

# Vintage bins (3-level summary for Figures 2-3)
tanks_1998[, vintage_bin := factor(fcase(
  install_year < 1980,                        "Pre-1980",
  install_year %between% c(1980L, 1988L),     "1980\u20131988",
  install_year %between% c(1989L, 1998L),     "1989\u20131998",
  default = NA_character_
), levels = c("Pre-1980","1980\u20131988","1989\u20131998"))]

# Vintage breaks for Panel 3B (5-yr install cohort bins)
tanks_1998[, vintage_5yr := factor(fcase(
  install_year < 1970,                        "< 1970",
  install_year %between% c(1970L, 1974L),     "1970\u201374",
  install_year %between% c(1975L, 1979L),     "1975\u201379",
  install_year %between% c(1980L, 1984L),     "1980\u201384",
  install_year %between% c(1985L, 1988L),     "1985\u201388",
  install_year %between% c(1989L, 1992L),     "1989\u201392",
  install_year %between% c(1993L, 1998L),     "1993\u201398",
  default = NA_character_
), levels = c("< 1970","1970\u201374","1975\u201379","1980\u201384",
              "1985\u201388","1989\u201392","1993\u201398"))]

# ─────────────────────────────────────────────────────────────────────────────
# Figure 1: Tank Age Density
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure 1: Tank Age Density ---\n")

mean_age <- tanks_1998[!is.na(tank_age_1998),
  .(mean_age = mean(tank_age_1998, na.rm=TRUE)), by = Group]

p1_A <- ggplot(tanks_1998[!is.na(tank_age_1998)],
               aes(x = tank_age_1998, fill = Group, color = Group)) +
  geom_density(alpha = 0.30, adjust = 1.4, linewidth = 0.7) +
  geom_vline(data = mean_age,
             aes(xintercept = mean_age, color = Group),
             linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = AGE_BIN_BREAKS[is.finite(AGE_BIN_BREAKS)],
             linetype = "dotted", color = "gray70", linewidth = 0.3) +
  geom_vline(xintercept = 20,
             linetype = "dashed", color = "gray30", linewidth = 0.6) +
  annotate("text", x = 20.8, y = Inf, vjust = 1.5, hjust = 0,
           size = 2.6, color = "gray30",
           label = "20 yrs\n(elevated risk)") +
  scale_fill_manual(values  = COL_PAIR) +
  scale_color_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = AGE_BIN_BREAKS[is.finite(AGE_BIN_BREAKS)],
                     limits = c(0, 42)) +
  labs(title    = "A: Tank Age Density",
       subtitle = "Active tanks Dec 22, 1998. Dotted = 5-yr bin bounds. Dashed = group means.",
       x = "Tank Age (years)", y = "Density",
       fill = NULL, color = NULL)

p1_B <- ggplot(tanks_1998[!is.na(tank_age_1998)],
               aes(x = tank_age_1998, color = Group)) +
  stat_ecdf(linewidth = 0.9, geom = "step") +
  geom_vline(xintercept = 10, linetype = "dotted",
             color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 20, linetype = "dashed",
             color = "gray30", linewidth = 0.6) +
  scale_color_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = seq(0, 40, 5), limits = c(0, 42)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title    = "B: Cumulative Age Distribution (ECDF)",
       subtitle = "Vertical gap = share of tanks in that age range TX vs Control.",
       x = "Tank Age (years)", y = "Cumulative Share",
       color = NULL)

save_panels(
  panels        = list(A = p1_A, B = p1_B),
  base_name     = "Figure1_AgeDensity",
  combined_name = "Figure1_AgeDensity_Combined",
  panel_width   = 8, panel_height = 5,
  combined_width = 16, combined_height = 5,
  ncol = 2,
  title    = "Figure 1: Tank Age Distribution at Treatment Date (Dec 22, 1998)",
  subtitle = "Incumbent tanks active December 22, 1998. Pre-1989 vintage tanks dominate the older tail."
)

# ─────────────────────────────────────────────────────────────────────────────
# Figure 2: Vintage Composition
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure 2: Vintage Composition ---\n")

vintage_shares <- tanks_1998[!is.na(vintage_bin),
  .(N = .N), by = .(Group, vintage_bin)]
vintage_shares[, share := N / sum(N), by = Group]

p2_A <- ggplot(vintage_shares, aes(x = Group, y = share, fill = vintage_bin)) +
  geom_col(position = "stack", width = 0.65, color = "white", linewidth = 0.3) +
  geom_text(aes(label = percent(share, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("Pre-1980"         = "#1a1a2e",
               "1980\u20131988"   = "#4a4a8a",
               "1989\u20131998"   = "#9090cc"),
    name = "Vintage"
  ) +
  labs(title    = "A: Installation Vintage (Stacked Share)",
       subtitle = "Active tanks Dec 22, 1998.",
       x = NULL, y = "Share of Incumbent Tanks") +
  theme(legend.position = "right")

# Diverging bar: TX − Control share gap per vintage bin
vintage_wide <- dcast(vintage_shares, vintage_bin ~ Group, value.var = "share")
vintage_wide[, gap := Texas - Control]

p2_B <- ggplot(vintage_wide,
               aes(x = vintage_bin, y = gap,
                   fill = fifelse(gap > 0, "TX Higher", "CTL Higher"))) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.1f pp", gap * 100),
                vjust = fifelse(gap >= 0, -0.4, 1.2)),
            size = 3.2) +
  scale_fill_manual(values = c("TX Higher" = COL_TX, "CTL Higher" = COL_CTRL),
                    guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title    = "B: Vintage Share Gap (Texas \u2212 Control)",
       subtitle = "Positive = TX overrepresented in that vintage. Drives pre-trend confound.",
       x = "Vintage Bin", y = "Share Gap (TX \u2212 CTL, pp)")

save_panels(
  panels        = list(A = p2_A, B = p2_B),
  base_name     = "Figure2_VintageComposition",
  combined_name = "Figure2_VintageComposition_Combined",
  panel_width   = 7, panel_height = 5,
  combined_width = 14, combined_height = 5,
  ncol = 2,
  title    = "Figure 2: Installation Vintage Composition at Treatment Date",
  subtitle = "Active tanks December 22, 1998. Darkest = oldest vintage cohort."
)

# ─────────────────────────────────────────────────────────────────────────────
# Figure 3: Wall Type by Vintage
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure 3: Wall Type by Vintage ---\n")

wall_by_vintage <- tanks_1998[!is.na(vintage_bin) & !is.na(single_walled), .(
  pct_sw = 100 * mean(single_walled == 1, na.rm = TRUE),
  n      = .N
), by = .(vintage_bin, Group)]

p3_A <- ggplot(wall_by_vintage,
               aes(x = vintage_bin, y = pct_sw, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%", pct_sw)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3.1, fontface = "bold") +
  scale_fill_manual(values = COL_PAIR) +
  scale_y_continuous(limits = c(0, 110),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "A: Single-Walled Share by Vintage",
       subtitle = "Expected: SW% declines in newer vintages (post-mandate DW requirements).",
       x = "Installation Vintage", y = "Share Single-Walled (%)",
       fill = NULL)

# Panel B: 5-yr cohort level detail
wall_by_5yr <- tanks_1998[!is.na(vintage_5yr) & !is.na(single_walled), .(
  pct_sw = 100 * mean(single_walled == 1, na.rm = TRUE),
  n      = .N
), by = .(vintage_5yr, Group)]

p3_B <- ggplot(wall_by_5yr,
               aes(x = vintage_5yr, y = pct_sw,
                   color = Group, group = Group)) +
  geom_line(linewidth = 0.9) +
  geom_point(aes(size = n), alpha = 0.8) +
  scale_color_manual(values = COL_PAIR) +
  scale_size_continuous(range = c(2, 7),
                        labels = comma_format(),
                        guide  = guide_legend(title = "N Tanks")) +
  scale_y_continuous(limits = c(0, 110),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "B: SW Share by 5-Year Install Cohort",
       subtitle = "Dot size = N tanks. Convergence post-1989 reflects DW mandate compliance.",
       x = "5-Year Install Cohort", y = "Share Single-Walled (%)",
       color = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_panels(
  panels        = list(A = p3_A, B = p3_B),
  base_name     = "Figure3_WallByVintage",
  combined_name = "Figure3_WallByVintage_Combined",
  panel_width   = 8, panel_height = 5,
  combined_width = 16, combined_height = 5,
  ncol = 2,
  title    = "Figure 3: Single-Walled Share by Installation Vintage",
  subtitle = "Active tanks December 22, 1998. Post-1989 cohorts converge as DW became standard."
)

# ─────────────────────────────────────────────────────────────────────────────
# Summary table: vintage x wall x group
# ─────────────────────────────────────────────────────────────────────────────
vintage_wall_summary <- tanks_1998[!is.na(vintage_bin), .(
  n_tanks     = .N,
  pct_sw      = round(100 * mean(single_walled == 1, na.rm=TRUE), 1),
  pct_dw      = round(100 * mean(double_walled == 1, na.rm=TRUE), 1),
  mean_age    = round(mean(tank_age_1998, na.rm=TRUE), 1)
), by = .(vintage_bin, Group)]
save_table(vintage_wall_summary, "Data_VintageWall_Summary")

cat("=== 01f COMPLETE ===\n")
