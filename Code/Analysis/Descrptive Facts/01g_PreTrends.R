#==============================================================================
# 01g_PreTrends.R
# Pre-Period Trend Figures and Parallel Trends Tests
#
# Figure PreTrends_SpecA  (main text)
#   PanelA: Closure rate trend — Spec A only, TX vs Control
#   PanelB: Difference (TX − CTL) — Spec A
#   Combined → Figure4_PreTrends_SpecA_Combined
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
# Figure MandateSpike (focused mandate contamination illustration)
#   Single panel: Spec B pre-period closure rate with gold mandate shading
#   → Figure_MandateSpike
#
# Table B.4: Parallel trends Wald tests (4 specs)
#==============================================================================

source(here::here("Code",'Analysis','Descrptive Facts', "01a_Setup.R"))

cat("=== 01g: PRE-TRENDS FIGURES ===\n")

annual_data <- load_interim("annual_data")

# ─────────────────────────────────────────────────────────────────────────────
# Helper: build annual closure rate by group for a given sub-sample
# ─────────────────────────────────────────────────────────────────────────────
build_rate_series <- function(dt, yr_start = PANEL_START, yr_end = PANEL_END) {
  dt[panel_year %between% c(yr_start, yr_end), .(
    closure_rate = mean(closure_event, na.rm = TRUE),
    n_fac_yr     = .N,
    se           = sqrt(mean(closure_event, na.rm=TRUE) *
                        (1 - mean(closure_event, na.rm=TRUE)) / .N)
  ), by = .(panel_year,
             Group = fifelse(texas_treated == 1, "Texas", "Control"))]
}

build_diff_series <- function(rate_dt) {
  wide <- dcast(rate_dt, panel_year ~ Group,
                value.var = c("closure_rate","se","n_fac_yr"))
  wide[, `:=`(
    diff   = closure_rate_Texas - closure_rate_Control,
    diff_se = sqrt(se_Texas^2 + se_Control^2)
  )]
  wide[, `:=`(diff_lo = diff - 1.96 * diff_se,
               diff_hi = diff + 1.96 * diff_se)]
  wide
}

# ─────────────────────────────────────────────────────────────────────────────
# Helper: rate panel + diff panel (4 panels for full/specA/specB)
# ─────────────────────────────────────────────────────────────────────────────
make_trend_panels <- function(dt, yr_start, yr_end,
                               title_A, subtitle_A,
                               title_B, subtitle_B,
                               add_mandate = FALSE) {

  rates <- build_rate_series(dt, yr_start, yr_end)
  diffs <- build_diff_series(rates)

  # Panel A: raw rates
  p_A <- ggplot(rates,
                aes(x = panel_year, y = closure_rate,
                    color = Group, fill = Group)) +
    geom_ribbon(aes(ymin = closure_rate - 1.96*se,
                    ymax = closure_rate + 1.96*se), alpha = 0.12, color = NA) +
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

  # Panel B: difference
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

# ─────────────────────────────────────────────────────────────────────────────
# Figure 4: Spec A pre-trends (PRIMARY — main text)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure 4: Spec A Pre-Trends ---\n")

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
  base_name     = "Figure4_PreTrends_SpecA",
  combined_name = "Figure4_PreTrends_SpecA_Combined",
  panel_width   = 9, panel_height = 5,
  combined_width = 18, combined_height = 5,
  ncol = 2,
  title    = "Figure 4: Pre-Period Closure Rate Trends — Spec A (Post-1988, Mandate-Free)",
  subtitle = "Primary identification sample. Dashed = Dec 1998 reform."
)
save_table(specA_panels$rates, "Data_PreTrends_SpecA_Rates")
save_table(specA_panels$diffs, "Data_PreTrends_SpecA_Diff")

# ─────────────────────────────────────────────────────────────────────────────
# Figure: Spec B (appendix — mandate contamination visible)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: Spec B Pre-Trends (Appendix) ---\n")

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
# Figure: Mandate Spike — focused single-panel illustration
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: Mandate Spike ---\n")

specB_pre <- annual_data[
  spec_B_eligible == 1 &
  panel_year %between% c(1985L, 1998L), .(
    closure_rate = mean(closure_event, na.rm = TRUE),
    se = sqrt(mean(closure_event, na.rm=TRUE) *
              (1 - mean(closure_event, na.rm=TRUE)) / .N)
  ), by = .(panel_year, Group = fifelse(texas_treated==1,"Texas","Control"))
]

p_mandate <- ggplot(specB_pre,
                    aes(x = panel_year, y = closure_rate,
                        color = Group)) +
  mandate_shade_layer() +
  geom_ribbon(aes(ymin = closure_rate - 1.96*se,
                  ymax = closure_rate + 1.96*se,
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
      "This pre-trend violation is the primary motivation for restricting to ",
      "post-1988 (Spec A) tanks."
    ),
    x = "Year", y = "Annual Closure Rate", color = NULL, fill = NULL
  )

save_fig(p_mandate, "Figure_MandateSpike_SpecB",  width = 9, height = 5)
save_table(specB_pre, "Data_MandateSpike_SpecB")

# ─────────────────────────────────────────────────────────────────────────────
# Table B.4: Parallel trends Wald tests
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Table B.4: Parallel Trends Tests ---\n")

run_pt_test <- function(dt, label, extra_rhs = "", mandate_ctrl = "None") {
  rhs <- paste0(
    "i(rel_year_1999, texas_treated, ref = -1)",
    if (nchar(extra_rhs) > 0) paste0(" + ", extra_rhs) else "",
    " | panel_id + panel_year"
  )
  m <- tryCatch(
    feols(as.formula(paste("closure_event ~", rhs)),
          data    = dt[panel_year %between% c(1990L, TREATMENT_YEAR)],
          cluster = ~state),
    error = function(e) NULL)
  if (is.null(m)) return(data.table(
    Specification = label, `Mandate Control` = mandate_ctrl,
    `F-statistic` = NA_real_, `p-value` = NA_real_, Conclusion = "FAILED"))
  w <- tryCatch(fixest::wald(m, "rel_year_1999"), error = function(e) NULL)
  if (is.null(w)) return(data.table(
    Specification = label, `Mandate Control` = mandate_ctrl,
    `F-statistic` = NA_real_, `p-value` = NA_real_, Conclusion = "FAILED"))
  data.table(
    Specification     = label,
    `Mandate Control` = mandate_ctrl,
    `F-statistic`     = round(w$stat, 3),
    `p-value`         = round(w$p,    4),
    Conclusion        = fifelse(w$p > 0.10, "NOT rejected", "REJECTED")
  )
}

pt_results <- rbindlist(list(
  run_pt_test(annual_data,
              "1. Pooled (all facilities)"),
  run_pt_test(annual_data[spec_A_eligible == 1],
              "2. Spec A (post-1988) — PRIMARY IDENTIFICATION"),
  run_pt_test(annual_data[spec_B_eligible == 1],
              "3. Spec B (pre-1988), no mandate control"),
  run_pt_test(annual_data[spec_B_eligible == 1],
              "4. Spec B + mandate_active control",
              extra_rhs = "mandate_active", mandate_ctrl = "mandate_active")
))

print(pt_results[, .(Specification, `F-statistic`, `p-value`, Conclusion)])
save_table(pt_results, "TableB4_Parallel_Trends_Validation")

write_tex(
  kbl(pt_results, format="latex", booktabs=TRUE, linesep="", escape=FALSE,
      caption=paste("Parallel trends Wald F-tests, 1990\u20131997 pre-reform window.",
        "Each row is a TWFE regression of annual closure on Texas",
        "$\\times$ relative-year interactions with facility and year FEs."),
      label="tab:pt-validation") |>
    kable_styling(latex_options=c("scale_down","hold_position"), font_size=9),
  "TableB4_Parallel_Trends_Validation"
)

# Save for metadata
save_interim(pt_results, "pt_results")

cat("=== 01g COMPLETE ===\n")
