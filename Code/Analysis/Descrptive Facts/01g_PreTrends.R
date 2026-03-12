#==============================================================================
# 01g_PreTrends.R
# Pre-Period Trend Figures and Parallel Trends Tests
#
# Figure PreTrends_SpecA  (appendix)
#   PanelA: Closure rate trend — Spec A only, TX vs Control
#   PanelB: Difference (TX − CTL) — Spec A
#   Combined → FigureAppx_PreTrends_SpecA_Combined
#
# Figure PreTrends_SpecB  (appendix — shows mandate spike)
#   PanelA: Closure rate — Spec B, TX vs Control, mandate shading
#   PanelB: Difference — Spec B
#   Combined → FigureAppx_PreTrends_SpecB_Combined
#
# Figure PreTrends_Pooled (appendix — contaminated)
#   PanelA: Closure rate — Pooled
#   PanelB: Difference — Pooled
#   Combined → FigureAppx_PreTrends_Pooled_Combined
#
# Figure 4 PRIMARY: Make-Model Parallel Trends (main text)
#   PanelA: Closure rate — Make-Model Primary Sample
#   PanelB: Difference — Make-Model Primary Sample
#   Combined → Figure4_PreTrends_MakeModel_Combined
#
# Figure MandateSpike (focused mandate contamination illustration)
#   → Figure_MandateSpike
#
# Table B.4: Parallel trends Wald tests (5 specs)
#==============================================================================

source(here::here("Code", "Analysis", "Descrptive Facts", "01a_Setup.R"))
open_log("01g_PreTrends")
log_cat("=== 01g: PRE-TRENDS FIGURES ===\n")

annual_data <- load_interim("annual_data")

# ─────────────────────────────────────────────────────────────────────────────
# Make-model primary sample flag
# Mirrors mm_fac_primary in 02a S4: exclude Unknown cells, require
# reform-date classification and age bin.
# ─────────────────────────────────────────────────────────────────────────────
annual_data[, is_mm_primary := as.integer(
  !is.na(make_model_fac)       &
  fac_wall != "Unknown-Wall"   &
  fac_fuel != "Unknown-Fuel"   &
  !is.na(fac_oldest_age_bin)
)]
log_cat(sprintf("  mm_fac_primary: %s facility-years (%s facilities)\n",
  format(sum(annual_data$is_mm_primary, na.rm = TRUE), big.mark = ","),
  format(uniqueN(annual_data[is_mm_primary == 1, panel_id]), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# Helpers
# ─────────────────────────────────────────────────────────────────────────────
build_rate_series <- function(dt, yr_start = PANEL_START, yr_end = PANEL_END) {
  dt[panel_year %between% c(yr_start, yr_end), .(
    closure_rate = mean(closure_event, na.rm = TRUE),
    n_fac_yr     = .N,
    se           = sqrt(mean(closure_event, na.rm = TRUE) *
                        (1 - mean(closure_event, na.rm = TRUE)) / .N)
  ), by = .(panel_year,
             Group = fifelse(texas_treated == 1, "Texas", "Control"))]
}

build_diff_series <- function(rate_dt) {
  wide <- dcast(rate_dt, panel_year ~ Group,
                value.var = c("closure_rate", "se", "n_fac_yr"))
  wide[, `:=`(
    diff    = closure_rate_Texas - closure_rate_Control,
    diff_se = sqrt(se_Texas^2 + se_Control^2)
  )]
  wide[, `:=`(diff_lo = diff - 1.96 * diff_se,
               diff_hi = diff + 1.96 * diff_se)]
  wide
}

make_trend_panels <- function(dt, yr_start, yr_end,
                               title_A, subtitle_A,
                               title_B, subtitle_B,
                               add_mandate = FALSE) {
  rates <- build_rate_series(dt, yr_start, yr_end)
  diffs <- build_diff_series(rates)

  p_A <- ggplot(rates,
                aes(x = panel_year, y = closure_rate,
                    color = Group, fill = Group)) +
    geom_ribbon(aes(ymin = closure_rate - 1.96 * se,
                    ymax = closure_rate + 1.96 * se), alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    treatment_vline()

  if (add_mandate) p_A <- p_A + mandate_shade_layer()

  p_A <- p_A +
    scale_color_manual(values = COL_PAIR) +
    scale_fill_manual(values  = COL_PAIR) +
    scale_x_continuous(breaks = seq(yr_start, yr_end, 3)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(title = title_A, subtitle = subtitle_A,
         x = "Year", y = "Annual Closure Rate",
         color = NULL, fill = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p_B <- ggplot(diffs[!is.na(diff)],
                aes(x = panel_year, y = diff)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    geom_ribbon(aes(ymin = diff_lo, ymax = diff_hi),
                fill = COL_TX, alpha = 0.18) +
    geom_line(color = COL_TX, linewidth = 1) +
    geom_point(color = COL_TX, size = 2) +
    treatment_vline()

  if (add_mandate) p_B <- p_B + mandate_shade_layer()

  p_B <- p_B +
    scale_x_continuous(breaks = seq(yr_start, yr_end, 3)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(title = title_B, subtitle = subtitle_B,
         x = "Year", y = "Closure Rate Difference (TX \u2212 CTL)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  list(A = p_A, B = p_B, rates = rates, diffs = diffs)
}

#==============================================================================
# Figure 01: Full Sample Tank Closure Rates — Revised
#==============================================================================

# 1. Prep -----------------------------------------------------------------------
annual_data <- annual_data[panel_year >= 1985 & panel_year <= 2020]
if (!"Group" %in% names(annual_data)) {
  annual_data[, Group := fifelse(state == "TX", "Texas", "Control")]
}

tank_rates <- annual_data[, .(
  closures   = sum(n_closures,      na.rm = TRUE),
  active_eoy = sum(active_tanks_dec, na.rm = TRUE)
), by = .(panel_year, Group)]

tank_rates[, tank_closure_rate := closures / (closures + active_eoy)]

# Wide form for ribbon
tank_wide <- dcast(tank_rates, panel_year ~ Group, value.var = "tank_closure_rate")

y_max <- max(tank_rates$tank_closure_rate, na.rm = TRUE)

# 2. Label placement (end of series) -------------------------------------------
label_data <- tank_rates[panel_year == 2019]

# 3. Plot -----------------------------------------------------------------------
p_full_closures <- ggplot() +
  
  # --- TX phased mandate band ---------------------------------------------------
  annotate("rect",
           xmin = 1989, xmax = 1993,
           ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "#d95f02") +
  # Label anchored BELOW the lines in the shaded region
  annotate("text",
           x = 1991, y = 0.01,
           label = "TX Phased\nMandate",
           size = 3, color = "#d95f02", fontface = "bold", vjust = 0) +

# --- Fix ribbon: only shade where Texas > Control ----------------------------
geom_ribbon(
  data = tank_wide[panel_year %between% c(1989, 1993)],
  aes(x    = panel_year,
      ymin = pmin(Texas, Control),
      ymax = pmax(Texas, Control)),
  fill = "#d95f02", alpha = 0.25
) +
  # --- Federal deadline line ----------------------------------------------------
  geom_vline(xintercept = 1998, linetype = "dashed",
             color = "grey30", linewidth = 0.7) +
# --- Federal deadline annotation: right of line, low in plot -----------------
annotate("text",
         x = 1998.4, y = 0.124,
         label = "Federal Deadline, Dec 1998",
         hjust = 0, vjust = 1, size = 3, 
         color = "grey30", fontface = "bold") +
           # --- Main lines ---------------------------------------------------------------
  geom_line(data  = tank_rates,
            aes(x = panel_year, y = tank_closure_rate, color = Group),
            linewidth = 1) +
  geom_point(data = tank_rates,
             aes(x = panel_year, y = tank_closure_rate, color = Group),
             size = 1.8) +

  # --- Direct end labels (replaces legend) --------------------------------------
  geom_text(data = label_data,
            aes(x = panel_year + 0.3, y = tank_closure_rate,
                label = Group, color = Group),
            hjust = 0, size = 3.5, fontface = "bold") +

  # --- Scales -------------------------------------------------------------------
  scale_color_manual(values = c("Texas" = "#d95f02", "Control" = "#1f78b4")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.05))) +
  scale_x_continuous(breaks = seq(1985, 2020, by = 5),
                     expand = expansion(add = c(0.5, 2.5))) +  # room for end labels

  # --- Labels -------------------------------------------------------------------
  labs(
    # title    = "Annual Tank Closure Rates, Full Sample",
    # subtitle = "Both states spike at the 1998 federal deadline; Texas elevated earlier under its phased state mandate.",
    x        = "Calendar Year",
    y        = "Annual Tank Closure Rate"
  ) +

  # --- Theme --------------------------------------------------------------------
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    plot.title       = element_text(face = "bold")
  )

save_fig(p_full_closures, "Figure_01_Full_Sample_closure_rates", width = 8, height = 5)


# ─────────────────────────────────────────────────────────────────────────────
# Spec A (moved to appendix)
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- FigureAppx: Spec A Pre-Trends (Appendix) ---\n")

specA_panels <- make_trend_panels(
  dt         = annual_data[spec_A_eligible == 1],
  yr_start   = 1990L, yr_end = PANEL_END,
  title_A    = "A: Closure Rate — Post-1988 Cohort (Spec A)",
  subtitle_A = "Mandate-free sample. Shaded = \u00b11.96 SE.",
  title_B    = "B: Closure Rate Difference (TX \u2212 Control) — Spec A",
  subtitle_B = "Pre-period near zero supports parallel trends for Spec A.",
  add_mandate = FALSE
)

save_panels(
  panels        = list(A = specA_panels$A, B = specA_panels$B),
  base_name     = "FigureAppx_PreTrends_SpecA",
  combined_name = "FigureAppx_PreTrends_SpecA_Combined",
  panel_width   = 9, panel_height = 5,
  combined_width = 18, combined_height = 5,
  ncol = 2,
  title    = "Appendix: Pre-Period Closure Rate Trends — Spec A (Post-1988, Mandate-Free)",
  subtitle = "Appendix figure. Primary identification uses make-model cell FE. Dashed = Dec 1998 reform."
)
save_table(specA_panels$rates, "Data_PreTrends_SpecA_Rates")
save_table(specA_panels$diffs, "Data_PreTrends_SpecA_Diff")

# ─────────────────────────────────────────────────────────────────────────────
# Spec B (appendix)
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- FigureAppx: Spec B Pre-Trends (Appendix) ---\n")

specB_panels <- make_trend_panels(
  dt          = annual_data[spec_B_eligible == 1],
  yr_start    = PANEL_START, yr_end = PANEL_END,
  title_A     = "A: Closure Rate — Pre-1988 Cohort (Spec B)",
  subtitle_A  = "Mandate-exposed. Gold = TX phased mandate window (1989\u20131993).",
  title_B     = "B: Closure Rate Difference (TX \u2212 Control) — Spec B",
  subtitle_B  = "TX spike during 1989\u20131993 = mandate effect, not reform effect.",
  add_mandate = TRUE
)

save_panels(
  panels        = list(A = specB_panels$A, B = specB_panels$B),
  base_name     = "FigureAppx_PreTrends_SpecB",
  combined_name = "FigureAppx_PreTrends_SpecB_Combined",
  panel_width   = 9, panel_height = 5,
  combined_width = 18, combined_height = 5,
  ncol = 2,
  title    = "Appendix: Pre-Period Closure Rate Trends — Spec B (Pre-1988, Mandate-Exposed)",
  subtitle = "Gold band = TX phased mandate window. Spike violates parallel trends for pooled/Spec B sample."
)

# ─────────────────────────────────────────────────────────────────────────────
# Mandate Spike figure
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Figure: Mandate Spike ---\n")

specB_pre <- annual_data[
  spec_B_eligible == 1 &
  panel_year %between% c(1985L, 1998L), .(
    closure_rate = mean(closure_event, na.rm = TRUE),
    se = sqrt(mean(closure_event, na.rm = TRUE) *
              (1 - mean(closure_event, na.rm = TRUE)) / .N)
  ), by = .(panel_year, Group = fifelse(texas_treated == 1, "Texas", "Control"))
]

p_mandate <- ggplot(specB_pre,
                    aes(x = panel_year, y = closure_rate,
                        color = Group)) +
  mandate_shade_layer() +
  geom_ribbon(aes(ymin = closure_rate - 1.96 * se,
                  ymax = closure_rate + 1.96 * se,
                  fill = Group), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = COL_PAIR) +
  scale_fill_manual(values  = COL_PAIR) +
  scale_x_continuous(breaks = seq(1985, 1998, 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title    = "Pre-1988 Tanks: Texas Mandate Spike (1989\u20131993)",
    subtitle = paste0(
      "Gold band = TX phased compliance window (30 TAC Ch. 334). ",
      "TX closure rate spikes during mandate, then converges. ",
      "This pre-trend violation motivates make-model cell FE identification."
    ),
    x = "Year", y = "Annual Closure Rate", color = NULL, fill = NULL
  )

save_fig(p_mandate, "Figure_MandateSpike_SpecB", width = 9, height = 5)
save_table(specB_pre, "Data_MandateSpike_SpecB")

# ─────────────────────────────────────────────────────────────────────────────
# Figure 4 PRIMARY: Make-Model Parallel Trends
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Figure 4 PRIMARY: Make-Model Pre-Trends ---\n")

mm_panels <- make_trend_panels(
  dt         = annual_data[is_mm_primary == 1],
  yr_start   = 1990L, yr_end = PANEL_END,
  title_A    = "A: Closure Rate — Make-Model Primary Sample",
  subtitle_A = "Facilities with classified wall/fuel/age cell. Excludes Unknown cells.",
  title_B    = "B: Closure Rate Difference (TX \u2212 Control) — Make-Model Sample",
  subtitle_B = "Pre-period near zero supports parallel trends within make-model cells.",
  add_mandate = FALSE
)

save_panels(
  panels        = list(A = mm_panels$A, B = mm_panels$B),
  base_name     = "Figure4_PreTrends_MakeModel",
  combined_name = "Figure4_PreTrends_MakeModel_Combined",
  panel_width   = 9, panel_height = 5,
  combined_width = 18, combined_height = 5,
  ncol = 2,
  title    = "Figure 4: Pre-Period Closure Rate Trends — Make-Model Primary Sample",
  subtitle = "Primary identification sample. Cell FE absorb wall/fuel/age heterogeneity. Dashed = Dec 1998 reform."
)
save_table(mm_panels$rates, "Data_PreTrends_MakeModel_Rates")
save_table(mm_panels$diffs, "Data_PreTrends_MakeModel_Diff")

# ─────────────────────────────────────────────────────────────────────────────
# Table B.4: Parallel trends Wald tests
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Table B.4: Parallel Trends Tests ---\n")

run_pt_test <- function(dt, label, extra_rhs = "", mandate_ctrl = "None") {
  rhs <- paste0(
    "i(rel_year_1999, texas_treated, ref = -1)",
    if (nchar(extra_rhs) > 0) paste0(" + ", extra_rhs) else "",
    " | panel_id + panel_year"
  )
  m <- feols(as.formula(paste("closure_event ~", rhs)),
             data    = dt[panel_year %between% c(1990L, TREATMENT_YEAR)],
             cluster = ~state)
  w <- fixest::wald(m, "rel_year_1999")
  data.table(
    Specification     = label,
    `Mandate Control` = mandate_ctrl,
    `F-statistic`     = round(w$stat, 3),
    `p-value`         = round(w$p,    4),
    Conclusion        = fifelse(w$p > 0.10, "NOT rejected", "REJECTED")
  )
}

# Make-model Wald test uses cell FE instead of panel_year alone
run_pt_test_mm <- function(dt, label) {
  pre <- dt[panel_year %between% c(1990L, TREATMENT_YEAR) &
              !is.na(make_model_fac) &
              fac_wall != "Unknown-Wall" &
              fac_fuel != "Unknown-Fuel"]
  if (uniqueN(pre$panel_id) < 50)
    stop(sprintf("Too few facilities (%d) in make-model pre-period for Wald test.",
                 uniqueN(pre$panel_id)))
  m <- feols(closure_event ~
               i(rel_year_1999, texas_treated, ref = -1) |
               panel_id + make_model_fac^panel_year,
             data = pre, cluster = ~state)
  w <- fixest::wald(m, "rel_year_1999")
  data.table(
    Specification     = label,
    `Mandate Control` = "make_model_fac^panel_year FE",
    `F-statistic`     = round(w$stat, 3),
    `p-value`         = round(w$p,    4),
    Conclusion        = fifelse(w$p > 0.10, "NOT rejected", "REJECTED")
  )
}

pt_results <- rbindlist(list(
  run_pt_test(annual_data,
              "1. Pooled (all facilities)"),
  run_pt_test(annual_data[spec_A_eligible == 1],
              "2. Spec A (post-1988) — appendix"),
  run_pt_test(annual_data[spec_B_eligible == 1],
              "3. Spec B (pre-1988), no mandate control"),
  run_pt_test(annual_data[spec_B_eligible == 1],
              "4. Spec B + mandate_active control",
              extra_rhs = "mandate_active", mandate_ctrl = "mandate_active"),
  run_pt_test_mm(annual_data,
              "5. Make-Model Primary Sample (PRIMARY IDENTIFICATION)")
))

print(pt_results[, .(Specification, `F-statistic`, `p-value`, Conclusion)])
save_table(pt_results, "TableB4_Parallel_Trends_Validation")

write_tex(
  kbl(pt_results, format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      caption = paste("Parallel trends Wald F-tests, 1990\u20131997 pre-reform window.",
        "Each row is a TWFE regression of annual closure on Texas",
        "$\\times$ relative-year interactions with facility and year FEs.",
        "Row 5 uses make-model cell-by-year FE as the primary specification."),
      label = "tab:pt-validation") |>
    kable_styling(latex_options = c("scale_down", "hold_position"), font_size = 9),
  "TableB4_Parallel_Trends_Validation"
)

save_interim(pt_results, "pt_results")

log_cat("=== 01g COMPLETE ===\n")
close_log("01g_PreTrends")