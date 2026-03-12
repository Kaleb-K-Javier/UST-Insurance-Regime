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
#    PanelA — Share of active facilities with ANY single-walled tank (line)
#    PanelB — Tank-level SW share (pooled) by year
#    PanelC — SW facility share gap (TX − Control) by year
#    Combined → Figure_SWShare_Combined
#
# All bar figures shade the 1989–1997 analysis cohort window in light blue
# and mark Dec 1998 with a dashed vertical line.
#==============================================================================

source(here::here("Code", "Analysis", "Descrptive Facts", "01a_Setup.R"))
open_log("01e_InstallTrends")
log_cat("=== 01e: INSTALL TRENDS FIGURES ===\n")

tanks       <- load_interim("tanks")
annual_data <- load_interim("annual_data")

# Ensure date columns are IDate
for (col in c("tank_installed_date", "tank_closed_date"))
  if (col %in% names(tanks) && !inherits(tanks[[col]], "IDate"))
    tanks[, (col) := as.IDate(get(col))]

tanks[, install_year  := year(tank_installed_date)]
tanks[, Group         := fifelse(state == "TX", "Texas", "Control")]
tanks[, texas_treated := as.integer(state == "TX")]

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE: New Tanks Installed by Year
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Figure: New Tanks Installed by Year ---\n")

new_tanks_yr <- tanks[
  !is.na(install_year) & install_year >= PANEL_START & install_year <= PANEL_END,
  .N, by = .(install_year, Group)
]
setorder(new_tanks_yr, install_year, Group)

ref_counts <- new_tanks_yr[install_year %between% c(1990L, 1992L),
  .(ref_N = mean(N)), by = Group]
new_tanks_yr <- merge(new_tanks_yr, ref_counts, by = "Group")
new_tanks_yr[, index_N := 100 * N / ref_N]

p_it_A <- ggplot(new_tanks_yr,
                 aes(x = install_year, y = N, fill = Group)) +
  cohort_shade_layer(ymin = 0) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, alpha = 0.9) +
  treatment_vline() +
  scale_fill_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title    = NULL,
    subtitle = "Count of tanks with install date in year. Blue band = analysis cohort window (1989\u20131997).",
    x = "Installation Year", y = "Number of Tanks", fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_it_B <- ggplot(new_tanks_yr,
                 aes(x = install_year, y = index_N, fill = Group)) +
  cohort_shade_layer(ymin = 0) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, alpha = 0.9) +
  treatment_vline() +
  geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
  scale_fill_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  labs(
    title    = NULL,
    subtitle = "Index = 100 at group mean of 1990\u20131992.",
    x = "Installation Year", y = "Installation Index (1990\u201392 avg = 100)", fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(p_it_A, "Figure_InstallTanks_ByYear", width = 9, height = 5)
save_fig(p_it_B, "Figure_InstallTanks_ByYear_Indexed", width = 9, height = 5)
save_table(new_tanks_yr, "Data_InstallTanks_ByYear")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE: New Facilities by Year
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Figure: New Facilities by Year ---\n")

fac_first_install <- tanks[!is.na(install_year),
  .(first_install_yr = min(install_year)), by = .(panel_id, Group)]

new_fac_yr <- fac_first_install[
  first_install_yr >= PANEL_START & first_install_yr <= PANEL_END,
  .N, by = .(install_year = first_install_yr, Group)
]
setorder(new_fac_yr, install_year, Group)
new_fac_yr[, total_yr    := sum(N), by = install_year]
new_fac_yr[, tx_share_yr := N / total_yr]

p_if_A <- ggplot(new_fac_yr,
                 aes(x = install_year, y = N, fill = Group)) +
  cohort_shade_layer(ymin = 0) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, alpha = 0.9) +
  treatment_vline() +
  scale_fill_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title    = NULL,
    subtitle = "Facility defined by first tank installation at a panel_id.",
    x = "Year of First Installation", y = "Number of New Facilities", fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tx_share_fac <- new_fac_yr[Group == "Texas", .(install_year, tx_share_yr)]

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
    title    = NULL,
    subtitle = "Dashed = overall TX share.",
    x = "Year of First Installation", y = "Texas Share of New Facilities"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(p_if_A, "Figure_InstallFacilities_ByYear", width = 9, height = 5)
save_fig(p_if_B, "Figure_InstallFacilities_TXShare", width = 9, height = 5)
save_table(new_fac_yr, "Data_InstallFacilities_ByYear")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE: Single-Wall Share & Capital Mix of Active Facilities Over Time
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Figure: SW Share & Capital Mix of Active Tanks Over Time ---\n")

years_sw <- PANEL_START:PANEL_END

sw_share_by_year <- rbindlist(lapply(years_sw, function(yr) {
  ref_date <- as.IDate(sprintf("%d-12-22", yr))
  
  active <- tanks[
    !is.na(tank_installed_date) &
    tank_installed_date <= ref_date &
    (is.na(tank_closed_date) | tank_closed_date > ref_date)
  ]
  if (nrow(active) == 0) return(NULL)

  # Calculate facility-level risk profiles using extensive margin indicators
  fac_sw <- active[, .(
    has_any_sw    = as.integer(sum(single_walled == 1, na.rm = TRUE) > 0),
    is_all_sw     = as.integer(sum(single_walled == 1, na.rm = TRUE) == sum(!is.na(single_walled)) & sum(!is.na(single_walled)) > 0),
    is_mixed      = as.integer(sum(single_walled == 1, na.rm = TRUE) > 0 & 
                               sum(double_walled == 1, na.rm = TRUE) > 0),
    n_active      = .N,
    texas_treated = as.integer(first(state) == "TX")
  ), by = panel_id]

  # Aggregate probabilities (shares) at the group level
  fac_sw[, .(
    year               = yr,
    share_any_sw       = mean(has_any_sw, na.rm = TRUE),
    share_all_sw       = mean(is_all_sw, na.rm = TRUE),
    share_mixed        = mean(is_mixed, na.rm = TRUE),
    se_any_sw          = sd(has_any_sw, na.rm = TRUE) / sqrt(.N),
    n_facilities       = .N,
    tank_level_sw_prop = sum(active$single_walled == 1, na.rm = TRUE) / nrow(active)
  ), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]
}))

# Calculate CI for the primary manuscript metric
sw_share_by_year[, ci_lo := share_any_sw - 1.96 * se_any_sw]
sw_share_by_year[, ci_hi := share_any_sw + 1.96 * se_any_sw]

# Gap calculation for the primary metric
sw_gap <- dcast(sw_share_by_year, year ~ Group,
                value.var = c("share_any_sw", "se_any_sw"))
sw_gap[, gap    := share_any_sw_Texas - share_any_sw_Control]
sw_gap[, gap_se := sqrt(se_any_sw_Texas^2 + se_any_sw_Control^2)]
sw_gap[, gap_lo := gap - 1.96 * gap_se]
sw_gap[, gap_hi := gap + 1.96 * gap_se]

# Panel A: Corrected Manuscript Figure (Share with ANY single-walled tank)
p_sw_A <- ggplot(sw_share_by_year,
                 aes(x = year, y = share_any_sw,
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
    title    = NULL,
    subtitle = "Share of active facilities with \u22651 single-walled tank. Ribbon = \u00b11.96 SE.",
    x = "Year", y = "Facilities with Any Single-Walled Tank",
    color = NULL, fill = NULL
  )

# Panel B: Tank-Level Proportion
p_sw_B <- ggplot(sw_share_by_year,
                 aes(x = year, y = tank_level_sw_prop, color = Group)) +
  cohort_shade_layer() +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  treatment_vline() +
  scale_color_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title    = NULL,
    subtitle = "Share of all active tanks that are single-walled.",
    x = "Year", y = "Tank-Level Single-Wall Share",
    color = NULL
  )

# Panel C: Gap
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
    title    = NULL,
    subtitle = "Positive = TX has higher SW facility exposure. Ribbon = \u00b11.96 SE.",
    x = "Year", y = "Facility SW Share Gap (TX \u2212 CTL)"
  )

# NOTE: File name mapped explicitly to the LaTeX/Quarto include path provided previously
save_fig(p_sw_A, "Figure_SW_Share_Over_Time", width = 8, height = 5)
save_fig(p_sw_B, "Figure_SWShare_TankLevel", width = 8, height = 5)
save_fig(p_sw_C, "Figure_SWShare_Gap", width = 8, height = 5)

save_table(sw_share_by_year, "Data_SWShare_ByYear")
save_table(sw_gap,           "Data_SWShare_Gap_ByYear")

# ─────────────────────────────────────────────────────────────────────────────
# Wall Type Composition Figure
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Figure: Wall Type Composition of Active Tanks Over Time ---\n")

wall_yr <- rbindlist(lapply(years_sw, function(yr) {
  ref_date <- as.IDate(sprintf("%d-12-22", yr))
  active <- tanks[
    !is.na(tank_installed_date) &
    tank_installed_date <= ref_date &
    (is.na(tank_closed_date) | tank_closed_date > ref_date)
  ]
  if (nrow(active) == 0) return(NULL)
  active[, .(
    year    = yr,
    n_sw    = sum(single_walled == 1, na.rm = TRUE),
    n_dw    = sum(double_walled == 1, na.rm = TRUE),
    n_unk   = sum(is.na(single_walled) |
                    (single_walled == 0 & double_walled == 0)),
    n_total = .N
  ), by = .(Group = fifelse(state == "TX", "Texas", "Control"))]
}))

wall_yr_long <- melt(
  wall_yr[, .(year, Group,
              `Single-Walled`   = n_sw  / n_total,
              `Double-Walled`   = n_dw  / n_total,
              `Unknown / Other` = n_unk / n_total)],
  id.vars = c("year", "Group"),
  variable.name = "Wall_Type", value.name = "share"
)
wall_yr_long[, Wall_Type := factor(Wall_Type,
  levels = c("Single-Walled", "Double-Walled", "Unknown / Other"))]

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
    title    = NULL,
    subtitle = "Post-1989 shift to DW reflects mandate compliance.",
    x = "Year", y = "Share of Active Tanks", fill = "Wall Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(p_wall_yr, "Figure_WallType_Composition_OverTime", width = 12, height = 5)
save_table(wall_yr, "Data_WallType_ByYear")

# ─────────────────────────────────────────────────────────────────────────────
# CORRECTED AGGREGATION: Tank-Level and Facility-Level Metrics
# ─────────────────────────────────────────────────────────────────────────────
sw_share_by_year <- rbindlist(lapply(years_sw, function(yr) {
  ref_date <- as.IDate(sprintf("%d-12-22", yr))
  
  active <- tanks[
    !is.na(tank_installed_date) &
    tank_installed_date <= ref_date &
    (is.na(tank_closed_date) | tank_closed_date > ref_date)
  ]
  if (nrow(active) == 0) return(NULL)

  # 1. Tank-Level Aggregation (Intensive Margin)
  tank_metrics <- active[, .(
    n_tanks            = .N,
    tank_level_sw_prop = sum(single_walled == 1, na.rm = TRUE) / .N,
    tank_level_dw_prop = sum(double_walled == 1, na.rm = TRUE) / .N
  ), by = .(Group = fifelse(state == "TX", "Texas", "Control"))]

  # 2. Facility-Level Aggregation (Extensive Margin)
  fac_sw <- active[, .(
    has_any_sw    = as.integer(sum(single_walled == 1, na.rm = TRUE) > 0),
    is_all_sw     = as.integer(sum(single_walled == 1, na.rm = TRUE) == sum(!is.na(single_walled)) & sum(!is.na(single_walled)) > 0),
    is_mixed      = as.integer(sum(single_walled == 1, na.rm = TRUE) > 0 & 
                               sum(double_walled == 1, na.rm = TRUE) > 0),
    texas_treated = as.integer(first(state) == "TX")
  ), by = panel_id]

  fac_metrics <- fac_sw[, .(
    share_any_sw       = mean(has_any_sw, na.rm = TRUE),
    share_all_sw       = mean(is_all_sw, na.rm = TRUE),
    share_mixed        = mean(is_mixed, na.rm = TRUE),
    se_any_sw          = sd(has_any_sw, na.rm = TRUE) / sqrt(.N),
    n_facilities       = .N
  ), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

  # Merge metrics and append iteration year
  res <- merge(fac_metrics, tank_metrics, by = "Group")
  res[, year := yr]
  return(res)
}))

# ─────────────────────────────────────────────────────────────────────────────
# PANEL B REVISION: Dual Wall-Type Stock Level Plot
# ─────────────────────────────────────────────────────────────────────────────
# Isolate and melt tank-level proportions for multi-series rendering
stock_long <- melt(
  sw_share_by_year[, .(year, Group, 
                       `Single-Walled` = tank_level_sw_prop, 
                       `Double-Walled` = tank_level_dw_prop)],
  id.vars       = c("year", "Group"),
  variable.name = "Wall_Type",
  value.name    = "share"
)

p_sw_stock <- ggplot(stock_long,
                 aes(x = year, y = share, color = Group, linetype = Wall_Type, shape = Wall_Type)) +
  cohort_shade_layer() +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.8) +
  treatment_vline() +
  scale_color_manual(values = COL_PAIR) +
  scale_linetype_manual(values = c("Single-Walled" = "solid", "Double-Walled" = "dashed")) +
  scale_shape_manual(values = c("Single-Walled" = 16, "Double-Walled" = 17)) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title    = NULL,
    subtitle = "Aggregate active capital stock categorized by primary wall type.",
    x = "Year", y = "Aggregate Share (Fleet-Level)",
    color = "State Group", linetype = "Wall Type", shape = "Wall Type"
  )

save_fig(p_sw_stock, "Figure_SWShare_StockLevel_Dual", width = 8, height = 5)


# ─────────────────────────────────────────────────────────────────────────────
# FIGURE: 1990 Cohort Survival Curve by Wall Type
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Figure: 1990 Cohort Survival Curve ---\n")

# 1. Define 1990 baseline reference date and isolate the cohort
ref_1990 <- as.IDate("1990-12-22")

cohort_1990 <- tanks[
  !is.na(tank_installed_date) &
  tank_installed_date <= ref_1990 &
  (is.na(tank_closed_date) | tank_closed_date > ref_1990)
]

# 2. Categorize static wall types for the cohort
cohort_1990[, wall_cat := fcase(
  single_walled == 1, "Single-Walled",
  double_walled == 1, "Double-Walled",
  default = "Unknown/Other"
)]

# 3. Calculate baseline denominators (N active in 1990)
baseline_counts <- cohort_1990[, .(base_n = .N), by = .(Group, wall_cat)]

# 4. Iterate forward to track attrition
years_surv <- 1990:2020

survival_curve <- rbindlist(lapply(years_surv, function(yr) {
  ref_date <- as.IDate(sprintf("%d-12-22", yr))
  
  # Subset the fixed 1990 cohort to those STILL active in 'yr'
  alive <- cohort_1990[is.na(tank_closed_date) | tank_closed_date > ref_date]
  
  if (nrow(alive) == 0) return(NULL)
  
  alive_counts <- alive[, .(alive_n = .N), by = .(Group, wall_cat)]
  alive_counts[, year := yr]
  return(alive_counts)
}))

# 5. Compute survival shares
survival_curve <- merge(survival_curve, baseline_counts, by = c("Group", "wall_cat"), all.x = TRUE)
survival_curve[, survival_share := alive_n / base_n]

# Filter to specified technologies for visualization
survival_plot_data <- survival_curve[wall_cat %in% c("Single-Walled", "Double-Walled")]

p_survival <- ggplot(survival_plot_data,
                     aes(x = year, y = survival_share, color = Group, linetype = wall_cat, shape = wall_cat)) +
  cohort_shade_layer() +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  treatment_vline() +
  scale_color_manual(values = COL_PAIR) +
  scale_linetype_manual(values = c("Single-Walled" = "solid", "Double-Walled" = "dashed")) +
  scale_shape_manual(values = c("Single-Walled" = 16, "Double-Walled" = 17)) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title    = NULL,
    subtitle = "Survival curve of the active capital stock established by 1990.",
    x = "Year", y = "Share of 1990 Cohort Still Active",
    color = "State Group", linetype = "Wall Type", shape = "Wall Type"
  )

save_fig(p_survival, "Figure_1990_Cohort_Survival", width = 8, height = 5)
save_table(survival_curve, "Data_1990_Cohort_Survival")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE: Pooled Active Capital Stock by Wall Type (Full Sample)
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Figure: Pooled Stock Level by Wall Type ---\n")

years_sw <- PANEL_START:PANEL_END

pooled_stock_yr <- rbindlist(lapply(years_sw, function(yr) {
  ref_date <- as.IDate(sprintf("%d-12-22", yr))
  
  active <- tanks[
    !is.na(tank_installed_date) &
    tank_installed_date <= ref_date &
    (is.na(tank_closed_date) | tank_closed_date > ref_date)
  ]
  if (nrow(active) == 0) return(NULL)

  data.table(
    year               = yr,
    n_tanks            = nrow(active),
    tank_level_sw_prop = sum(active$single_walled == 1, na.rm = TRUE) / nrow(active),
    tank_level_dw_prop = sum(active$double_walled == 1, na.rm = TRUE) / nrow(active)
  )
}))

pooled_stock_long <- melt(
  pooled_stock_yr,
  id.vars       = c("year", "n_tanks"),
  measure.vars  = c("tank_level_sw_prop", "tank_level_dw_prop"),
  variable.name = "Wall_Type",
  value.name    = "share"
)

pooled_stock_long[, Wall_Type := fcase(
  Wall_Type == "tank_level_sw_prop", "Single-Walled",
  Wall_Type == "tank_level_dw_prop", "Double-Walled"
)]
pooled_stock_long[, Wall_Type := factor(Wall_Type, levels = c("Single-Walled", "Double-Walled"))]

p_pooled_stock <- ggplot(pooled_stock_long,
                 aes(x = year, y = share, color = Wall_Type, linetype = Wall_Type, shape = Wall_Type)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  treatment_vline() +
  scale_color_manual(values = c("Single-Walled" = "#D55E00", "Double-Walled" = "#0072B2")) +
  scale_linetype_manual(values = c("Single-Walled" = "solid", "Double-Walled" = "dashed")) +
  scale_shape_manual(values = c("Single-Walled" = 16, "Double-Walled" = 17)) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title    = NULL,
    subtitle = "Aggregate active capital stock (pooled sample) categorized by primary wall type.",
    x = "Year", y = "Aggregate Share (Fleet-Level)",
    color = "Wall Type", linetype = "Wall Type", shape = "Wall Type"
  )

save_fig(p_pooled_stock, "Figure_SW_DW_StockLevel_Pooled", width = 8, height = 5)
save_table(pooled_stock_yr, "Data_StockLevel_Pooled")


log_cat("=== 01e COMPLETE ===\n")
close_log("01e_InstallTrends")