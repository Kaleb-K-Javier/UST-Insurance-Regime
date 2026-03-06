#==============================================================================
# 01i_CompositionBalance.R
# Composition Comparison: Full Sample vs. Make-Model Sample
# 
# Central argument: the make-model restriction produces TX/Control groups
# with identical observable risk profiles (SW share, fuel type, tank count),
# while the full sample shows substantial composition differences.
#
# Figure CompositionBalance:
#   PanelA — SW share: 4 bars (Full-TX, Full-CTL, MM-TX, MM-CTL)
#   PanelB — Motor fuel share
#   PanelC — Single-tank operator share
#   PanelD — Mean age at treatment
#   Combined → Figure_CompositionBalance_Combined
#
# Figure CompositionBalance_Radar (optional spider chart for visual summary)
#
# Table_CompositionBalance (Table 2 equivalent)
#==============================================================================

source(here::here("Code",'Analysis','Descrptive Facts', "01a_Setup.R"))
cat("=== 01i: COMPOSITION BALANCE ===\n")

annual_data <- load_interim("annual_data")
tanks_1998  <- load_interim("tanks_1998")

# ─────────────────────────────────────────────────────────────────────────────
# Build: facility-level characteristic snapshot at 1998
# ─────────────────────────────────────────────────────────────────────────────
fac_chars <- tanks_1998[, .(
  sw_share      = mean(single_walled == 1, na.rm = TRUE),
  dw_share      = mean(double_walled == 1, na.rm = TRUE),
  mean_age_1998 = mean(tank_age_1998, na.rm = TRUE),
  texas_treated = as.integer(first(state) == "TX")
), by = panel_id]

# Merge annual-level characteristics (fuel, tank count)
fac_annual_chars <- annual_data[
  panel_year == min(panel_year[panel_year >= 1995], na.rm=TRUE),
  .(has_gasoline     = first(has_gasoline_year),
    single_tank_op   = as.integer(first(single_tanks) == first(active_tanks)),
    install_year     = first(install_year)),
  by = panel_id]
fac_chars <- merge(fac_chars, fac_annual_chars, by="panel_id", all.x=TRUE)

# Sample flags
fac_chars[, is_mm := as.integer(
  sw_share == 1 &
  !is.na(has_gasoline) & has_gasoline == 1 &
  !is.na(single_tank_op) & single_tank_op == 1 &
  !is.na(install_year) &
  install_year > MM_INSTALL_START - 1 & install_year <= MM_INSTALL_END
)]
fac_chars[, Sample := fifelse(is_mm == 1, "Make-Model", "Full")]
fac_chars[, Group  := fifelse(texas_treated == 1, "Texas", "Control")]
fac_chars[, SampleGroup := paste0(Sample, " — ", Group)]
fac_chars[, SampleGroup := factor(SampleGroup, levels = c(
  "Full — Texas", "Full — Control",
  "Make-Model — Texas", "Make-Model — Control"
))]

# ─────────────────────────────────────────────────────────────────────────────
# Summary table per SampleGroup
# ─────────────────────────────────────────────────────────────────────────────
comp_summary <- fac_chars[, .(
  N_facilities    = .N,
  SW_share        = mean(sw_share, na.rm=TRUE),
  Motor_fuel_share = mean(!is.na(has_gasoline) & has_gasoline==1, na.rm=TRUE),
  Single_tank_share = mean(!is.na(single_tank_op) & single_tank_op==1, na.rm=TRUE),
  Mean_age_1998   = mean(mean_age_1998, na.rm=TRUE)
), by = SampleGroup]

print(comp_summary)
save_table(comp_summary, "Table2_CompositionBalance")

# ─────────────────────────────────────────────────────────────────────────────
# Figure Panel factory
# ─────────────────────────────────────────────────────────────────────────────

COL_4 <- c(
  "Full — Texas"         = scales::alpha(COL_TX,   0.65),
  "Full — Control"       = scales::alpha(COL_CTRL,  0.65),
  "Make-Model — Texas"   = COL_TX,
  "Make-Model — Control" = COL_CTRL
)

make_comp_bar <- function(var, ylabel, title, pct_fmt = TRUE,
                           pct_threshold = 0.05) {
  dt <- comp_summary[, .(SampleGroup, Value = get(var))]
  dt[, fill_grp := SampleGroup]

  p <- ggplot(dt, aes(x = SampleGroup, y = Value, fill = fill_grp)) +
    geom_col(width = 0.65, alpha = 0.95) +
    geom_hline(data = dt[grepl("Full", SampleGroup)],
               aes(yintercept = Value, color = fill_grp),
               linetype = "dashed", linewidth = 0.5, show.legend = FALSE) +
    geom_text(aes(label = if (pct_fmt) percent(Value, accuracy=0.1)
                          else round(Value, 1)),
              vjust = -0.5, size = 3.2, fontface = "bold") +
    scale_fill_manual(values = COL_4, guide = "none") +
    scale_color_manual(
      values = c("Full — Texas" = COL_TX, "Full — Control" = COL_CTRL,
                 "Make-Model — Texas" = COL_TX, "Make-Model — Control" = COL_CTRL),
      guide = "none") +
    scale_x_discrete(labels = function(x)
      gsub(" — ", "\n", x)) +
    scale_y_continuous(
      labels = if (pct_fmt) percent_format(accuracy = 1) else waiver(),
      limits = if (pct_fmt) c(0, 1.15) else NULL
    ) +
    labs(title = title, x = NULL, y = ylabel) +
    theme(axis.text.x = element_text(size = 8))
  p
}

p_sw   <- make_comp_bar("SW_share",           "Single-Walled Share",    "A: Single-Walled Share")
p_mf   <- make_comp_bar("Motor_fuel_share",   "Motor Fuel / Gasoline",  "B: Motor Fuel Share")
p_st   <- make_comp_bar("Single_tank_share",  "Single-Tank Operators",  "C: Single-Tank Share")
p_age  <- make_comp_bar("Mean_age_1998",      "Mean Tank Age (yrs)",    "D: Mean Age at Treatment",
                        pct_fmt = FALSE)

save_panels(
  panels          = list(A = p_sw, B = p_mf, C = p_st, D = p_age),
  base_name       = "Figure_CompositionBalance",
  combined_name   = "Figure_CompositionBalance_Combined",
  panel_width     = 7, panel_height = 5,
  combined_width  = 14, combined_height = 10,
  ncol            = 2, nrow = 2,
  title    = "Composition Comparison: Full Sample vs. Make-Model Sample",
  subtitle = paste0(
    "Faded bars = full sample; solid bars = make-model sample. ",
    "Dashed lines carry full-sample values forward for reference. ",
    "Make-model restriction eliminates TX/Control composition gaps in A\u2013C by construction."
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# LaTeX table
# ─────────────────────────────────────────────────────────────────────────────
write_tex(
  kbl(comp_summary[, .(
        `Sample — Group`      = as.character(SampleGroup),
        N                     = comma(N_facilities),
        `SW Share`            = percent(SW_share, accuracy=0.1),
        `Motor Fuel Share`    = percent(Motor_fuel_share, accuracy=0.1),
        `Single-Tank Share`   = percent(Single_tank_share, accuracy=0.1),
        `Mean Age 1998 (yrs)` = round(Mean_age_1998, 1)
      )],
      format="latex", booktabs=TRUE, linesep="", escape=FALSE,
      caption="Observable risk profile: full vs. make-model sample, Texas vs. control. Make-model restriction equalizes all three observable risk dimensions (columns 3--5) across TX and Control by construction.",
      label="tab:composition-balance") |>
    kable_styling(latex_options=c("scale_down","hold_position"), font_size=9),
  "Table2_CompositionBalance"
)

cat("=== 01i COMPLETE ===\n")
