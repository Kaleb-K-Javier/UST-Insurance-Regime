#==============================================================================
# 01e_InstallTrends.R
# Installation Trend Figures
#
# Figures produced (each as individual panel + combined patchwork):
#
#  Figure InstallTanks:
#    PanelA — New tanks installed by year, TX vs Control (count)
#    PanelB — New tanks installed by year, index (TX = 100 in 1990)
#    Combined → Figure_InstallTanks_Combined
#
#  Figure InstallFacilities:
#    PanelA — New facilities by year, TX vs Control (count)
#    PanelB — TX share of new facilities by year
#    Combined → Figure_InstallFacilities_Combined
#
#  Figure SWShare:
#    PanelA — Mean facility-level SW share of ACTIVE tanks by year (line)
#    PanelB — Tank-level SW share (pooled) by year
#    PanelC — SW share gap (TX − Control) by year
#    Combined → Figure_SWShare_Combined
#
# All bar figures shade the 1990–1997 analysis cohort window in light blue
# and mark Dec 1998 with a dashed vertical line.
#==============================================================================

source(here::here("Code",'Analysis','Descrptive Facts', "01a_Setup.R"))
cat("=== 01e: INSTALL TRENDS FIGURES ===\n")

tanks    <- load_interim("tanks")
annual_data <- load_interim("annual_data")

# Ensure date columns are IDate
for (col in c("tank_installed_date","tank_closed_date"))
  if (col %in% names(tanks) && !inherits(tanks[[col]], "IDate"))
    tanks[, (col) := as.IDate(get(col))]

tanks[, install_year := year(tank_installed_date)]
tanks[, Group        := fifelse(state == "TX", "Texas", "Control")]
tanks[, texas_treated := as.integer(state == "TX")]

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE: New Tanks Installed by Year
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: New Tanks Installed by Year ---\n")

new_tanks_yr <- tanks[
  !is.na(install_year) & install_year >= PANEL_START & install_year <= PANEL_END,
  .N, by = .(install_year, Group)
]
setorder(new_tanks_yr, install_year, Group)

# Reference counts for indexing (mean of 1990-1992 to smooth noise)
ref_counts <- new_tanks_yr[install_year %between% c(1990L, 1992L),
  .(ref_N = mean(N)), by = Group]
new_tanks_yr <- merge(new_tanks_yr, ref_counts, by = "Group")
new_tanks_yr[, index_N := 100 * N / ref_N]

# ── Panel A: raw counts ──
p_it_A <- ggplot(new_tanks_yr,
                 aes(x = install_year, y = N, fill = Group)) +
  cohort_shade_layer(ymin = 0) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, alpha = 0.9) +
  treatment_vline() +
  scale_fill_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title    = "New Tank Installations by Year",
    subtitle = "Count of tanks with install date in year. Blue band = analysis cohort window (1990\u20131997).",
    x = "Installation Year", y = "Number of Tanks", fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ── Panel B: indexed (TX = 100 in 1990-1992 average) ──
p_it_B <- ggplot(new_tanks_yr,
                 aes(x = install_year, y = index_N, fill = Group)) +
  cohort_shade_layer(ymin = 0) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, alpha = 0.9) +
  treatment_vline() +
  geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
  scale_fill_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  labs(
    title    = "New Tank Installations by Year (Indexed)",
    subtitle = "Index = 100 at group mean of 1990\u20131992. Reveals relative installation pace.",
    x = "Installation Year", y = "Installation Index (1990\u201392 avg = 100)", fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_panels(
  panels        = list(A = p_it_A, B = p_it_B),
  base_name     = "Figure_InstallTanks",
  combined_name = "Figure_InstallTanks_Combined",
  panel_width   = 9, panel_height = 5,
  combined_width = 18, combined_height = 5,
  ncol          = 2,
  title    = "New Tank Installations by Year: Texas vs. Control",
  subtitle = "Blue band = analysis cohort install window (1990\u20131997). Dashed line = Dec 1998 reform."
)
save_table(new_tanks_yr, "Data_InstallTanks_ByYear")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE: New Facilities by Year
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: New Facilities by Year ---\n")

# "New facility" = first year any tank is installed at that panel_id
fac_first_install <- tanks[!is.na(install_year),
  .(first_install_yr = min(install_year)), by = .(panel_id, Group)]

new_fac_yr <- fac_first_install[
  first_install_yr >= PANEL_START & first_install_yr <= PANEL_END,
  .N, by = .(install_year = first_install_yr, Group)
]
setorder(new_fac_yr, install_year, Group)
new_fac_yr[, total_yr := sum(N), by = install_year]
new_fac_yr[, tx_share_yr := N / total_yr]   # TX share of all new entrants

# ── Panel A: raw facility counts ──
p_if_A <- ggplot(new_fac_yr,
                 aes(x = install_year, y = N, fill = Group)) +
  cohort_shade_layer(ymin = 0) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, alpha = 0.9) +
  treatment_vline() +
  scale_fill_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title    = "New Facility Openings by Year",
    subtitle = "Facility defined by first tank installation at a panel_id. Blue band = analysis cohort window.",
    x = "Year of First Installation", y = "Number of New Facilities", fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ── Panel B: TX share of new entrants ──
tx_share_fac <- new_fac_yr[Group == "Texas",
  .(install_year, tx_share_yr)]

p_if_B <- ggplot(tx_share_fac,
                 aes(x = install_year, y = tx_share_yr)) +
  cohort_shade_layer(ymin = 0) +
  geom_col(fill = COL_TX, alpha = 0.85, width = 0.7) +
  geom_hline(yintercept = mean(tx_share_fac$tx_share_yr, na.rm = TRUE),
             linetype = "dashed", color = "gray40") +
  treatment_vline() +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, NA)) +
  labs(
    title    = "Texas Share of New Facility Openings by Year",
    subtitle = "Dashed = overall TX share. Stable share supports compositional balance of new entrants.",
    x = "Year of First Installation", y = "Texas Share of New Facilities"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_panels(
  panels        = list(A = p_if_A, B = p_if_B),
  base_name     = "Figure_InstallFacilities",
  combined_name = "Figure_InstallFacilities_Combined",
  panel_width   = 9, panel_height = 5,
  combined_width = 18, combined_height = 5,
  ncol          = 2,
  title    = "New Facility Openings by Year: Texas vs. Control",
  subtitle = "Blue band = analysis cohort install window (1990\u20131997). Dashed line = Dec 1998 reform."
)
save_table(new_fac_yr, "Data_InstallFacilities_ByYear")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE: Single-Wall Share of Active Tanks Over Time
#
# Construction:
#   For each year in PANEL_START:PANEL_END, identify active tanks on Dec 22 of
#   that year. Compute facility-level SW share = n_sw / n_active per panel_id.
#   Average that share across TX and Control groups.
#   This is the correct unit: facility average, not a tank-count-weighted mean,
#   because the DiD is at the facility level.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: SW Share of Active Tanks Over Time ---\n")

years_sw <- PANEL_START:PANEL_END

sw_share_by_year <- rbindlist(lapply(years_sw, function(yr) {
  ref_date <- as.IDate(sprintf("%d-12-22", yr))
  active   <- tanks[
    !is.na(tank_installed_date) &
    tank_installed_date <= ref_date &
    (is.na(tank_closed_date) | tank_closed_date > ref_date) &
    !is.na(single_walled)
  ]
  if (nrow(active) == 0) return(NULL)

  # Facility-level SW share
  fac_sw <- active[, .(
    fac_sw_share  = mean(single_walled == 1, na.rm = TRUE),
    n_active      = .N,
    texas_treated = as.integer(first(state) == "TX")
  ), by = panel_id]

  # Group averages with SE for ribbon
  fac_sw[, .(
    year          = yr,
    mean_sw_share = mean(fac_sw_share, na.rm = TRUE),
    se_sw_share   = sd(fac_sw_share, na.rm = TRUE) / sqrt(.N),
    n_facilities  = .N,
    # Tank-level pooled share (for Panel B)
    tank_sw_share = sum(active$single_walled == 1, na.rm = TRUE) / nrow(active)
  ), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]
}))

# Tank-level pooled share (Panel B computed from same loop data)
sw_share_by_year[, ci_lo := mean_sw_share - 1.96 * se_sw_share]
sw_share_by_year[, ci_hi := mean_sw_share + 1.96 * se_sw_share]

# SW share gap TX - Control (Panel C)
sw_gap <- dcast(sw_share_by_year, year ~ Group,
                value.var = c("mean_sw_share","se_sw_share"))
sw_gap[, gap     := mean_sw_share_Texas - mean_sw_share_Control]
sw_gap[, gap_se  := sqrt(se_sw_share_Texas^2 + se_sw_share_Control^2)]
sw_gap[, gap_lo  := gap - 1.96 * gap_se]
sw_gap[, gap_hi  := gap + 1.96 * gap_se]

# ── Panel A: facility-level mean SW share (preferred measure) ──
p_sw_A <- ggplot(sw_share_by_year,
                 aes(x = year, y = mean_sw_share,
                     color = Group, fill = Group)) +
  cohort_shade_layer() +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  treatment_vline() +
  scale_color_manual(values = COL_PAIR) +
  scale_fill_manual(values  = COL_PAIR) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title    = "A: Mean Facility SW Share of Active Tanks",
    subtitle = "Mean of (n_sw / n_active) across facilities per year. Ribbon = \u00b11.96 SE.",
    x = "Year", y = "Mean Facility Single-Wall Share",
    color = NULL, fill = NULL
  )

# ── Panel B: tank-level pooled share ──
p_sw_B <- ggplot(sw_share_by_year,
                 aes(x = year, y = tank_sw_share, color = Group)) +
  cohort_shade_layer() +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  treatment_vline() +
  scale_color_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title    = "B: Pooled Tank-Level SW Share of Active Tanks",
    subtitle = "Share of all active tanks that are single-walled (tank-count weighted).",
    x = "Year", y = "Tank-Level Single-Wall Share",
    color = NULL
  )

# ── Panel C: gap TX − Control ──
p_sw_C <- ggplot(sw_gap, aes(x = year, y = gap)) +
  cohort_shade_layer() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(ymin = gap_lo, ymax = gap_hi),
              fill = COL_TX, alpha = 0.15) +
  geom_line(color = COL_TX, linewidth = 1) +
  geom_point(color = COL_TX, size = 1.8) +
  treatment_vline() +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "C: SW Share Gap (Texas \u2212 Control)",
    subtitle = "Positive = TX has higher SW share. Ribbon = \u00b11.96 SE.",
    x = "Year", y = "Facility SW Share Gap (TX \u2212 CTL)"
  )

save_panels(
  panels          = list(A = p_sw_A, B = p_sw_B, C = p_sw_C),
  base_name       = "Figure_SWShare",
  combined_name   = "Figure_SWShare_Combined",
  panel_width     = 8, panel_height = 5,
  combined_width  = 24, combined_height = 5,
  ncol            = 3,
  title    = "Single-Wall Share of Active Tanks Over Time: Texas vs. Control",
  subtitle = paste0(
    "Panel A (preferred): facility-level mean of SW share. ",
    "Panel B: pooled tank-level share. ",
    "Panel C: gap Texas \u2212 Control (positive = TX more single-walled). ",
    "Blue band = analysis cohort install window (1990\u20131997)."
  )
)

save_table(sw_share_by_year, "Data_SWShare_ByYear")
save_table(sw_gap,           "Data_SWShare_Gap_ByYear")

# ─────────────────────────────────────────────────────────────────────────────
# Bonus: Double-wall adoption figure (mirror of SW share)
# Shows the post-mandate acceleration of DW adoption
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: Wall Type Composition of Active Tanks Over Time ---\n")

# Compute annual active tank wall-type breakdown for stacked area
wall_yr <- rbindlist(lapply(years_sw, function(yr) {
  ref_date <- as.IDate(sprintf("%d-12-22", yr))
  active <- tanks[
    !is.na(tank_installed_date) &
    tank_installed_date <= ref_date &
    (is.na(tank_closed_date) | tank_closed_date > ref_date)
  ]
  if (nrow(active) == 0) return(NULL)
  active[, .(
    year         = yr,
    n_sw         = sum(single_walled == 1, na.rm = TRUE),
    n_dw         = sum(double_walled == 1, na.rm = TRUE),
    n_unk        = sum(is.na(single_walled) | (single_walled==0 & double_walled==0)),
    n_total      = .N
  ), by = .(Group = fifelse(state == "TX", "Texas", "Control"))]
}))

wall_yr_long <- melt(
  wall_yr[, .(year, Group,
              `Single-Walled`    = n_sw / n_total,
              `Double-Walled`    = n_dw / n_total,
              `Unknown / Other`  = n_unk / n_total)],
  id.vars = c("year","Group"),
  variable.name = "Wall_Type", value.name = "share"
)
wall_yr_long[, Wall_Type := factor(Wall_Type,
  levels = c("Single-Walled","Double-Walled","Unknown / Other"))]

p_wall_yr <- ggplot(wall_yr_long,
                    aes(x = year, y = share, fill = Wall_Type)) +
  cohort_shade_layer() +
  geom_area(position = "stack", alpha = 0.85) +
  treatment_vline(color = "white", lwd = 0.9) +
  facet_wrap(~ Group, nrow = 1) +
  scale_fill_manual(values = c("Single-Walled"   = "#D55E00",
                                "Double-Walled"   = "#0072B2",
                                "Unknown / Other" = "gray75")) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Wall Type Composition of Active Tanks Over Time",
    subtitle = "Blue band = analysis cohort install window (1990\u20131997). Post-1989 shift to DW reflects mandate compliance.",
    x = "Year", y = "Share of Active Tanks", fill = "Wall Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(p_wall_yr, "Figure_WallType_Composition_OverTime", width = 12, height = 5)
save_table(wall_yr, "Data_WallType_ByYear")

cat("=== 01e COMPLETE ===\n")
