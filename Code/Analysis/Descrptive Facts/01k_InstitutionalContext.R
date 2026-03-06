#==============================================================================
# 01k_InstitutionalContext.R
# Institutional Context Figures (Texas FR Market)
#
# Requires: fr_year_raw, rate_data_raw, contract_month_raw  (from 01b)
# All figures are skipped gracefully if source files absent.
#
# Figures:
#   Figure_FR_Coverage:
#     PanelA — FR coverage rate by year (TX and comparison states)
#     PanelB — Number of FR-covered facilities by year
#     Combined → Figure_FR_Coverage_Combined
#
#   Figure_FR_Pricing:
#     PanelA — Median annual premium TIME SERIES (line, not bar)
#     PanelB — Premium index (1995 = 100)
#     PanelC — Log premium scatter with reform cutoff
#     Combined → Figure_FR_Pricing_Combined
#
#   Figure_FR_Switching:
#     PanelA — Monthly FR provider switching rate (contract panel)
#     PanelB — Cumulative fraction having switched ≥ once (by year)
#     Combined → Figure_FR_Switching_Combined
#==============================================================================

source(here::here("Code", "Analysis", "Descrptive Facts", "01a_Setup.R"))
cat("=== 01k: INSTITUTIONAL CONTEXT FIGURES ===\n")

fr_year        <- load_interim("fr_year_raw")
rate_data      <- load_interim("rate_data_raw")
contract_month <- load_interim("contract_month_raw")


# ─────────────────────────────────────────────────────────────────────────────
# Figure: FR Coverage
# ─────────────────────────────────────────────────────────────────────────────
if (!is.null(fr_year)) {
  cat("\n--- Figure: FR Coverage ---\n")

  # Diagnostic: show actual column names so mismatches are visible in log
  cat("  fr_year columns:", paste(names(fr_year), collapse = ", "), "\n")

  # --- Coerce year -------------------------------------------------------
  # year may be read as character/factor from CSV; force to integer
  year_col <- intersect(c("year", "panel_year", "YEAR", "Year"), names(fr_year))[1]
  if (is.na(year_col)) stop("fr_year: cannot find a year column. Columns are: ",
                             paste(names(fr_year), collapse = ", "))
  if (year_col != "year") {
    fr_year[, year := get(year_col)]
    cat(sprintf("  NOTE: using '%s' as year column\n", year_col))
  }
  fr_year[, year := as.integer(year)]
  fr_year[, post := as.integer(year >= POST_YEAR)]

  # --- Identify coverage column ------------------------------------------
  # This panel is Texas-only — no state column expected
  cov_col <- intersect(
    c("fr_covered", "has_fr_coverage", "fr_coverage", "covered",
      "fr_active", "has_coverage"),
    names(fr_year)
  )[1]
  if (is.na(cov_col)) stop("fr_year: cannot find an FR coverage column. Columns are: ",
                            paste(names(fr_year), collapse = ", "))
  cat(sprintf("  NOTE: using '%s' as coverage column (Texas-only panel)\n", cov_col))

  # --- Aggregate by year (Texas only) ------------------------------------
  fr_yr_agg <- fr_year[, .(
    n_covered = sum(get(cov_col) == 1, na.rm = TRUE),
    n_total   = .N,
    cov_rate  = mean(get(cov_col) == 1, na.rm = TRUE),
    post      = max(post)
  ), by = year]

  p_cov_A <- ggplot(fr_yr_agg, aes(x = year, y = cov_rate)) +
    treatment_vline() +
    geom_line(color = COL_TX, linewidth = 1) +
    geom_point(aes(color = factor(post)), size = 2.5) +
    scale_color_manual(values = c("0" = "grey50", "1" = COL_TX),
                       labels = c("Pre-Reform", "Post-Reform"), name = NULL) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(1990, PANEL_END, 4)) +
    labs(
      title    = "A: FR Coverage Rate by Year (Texas)",
      subtitle = "Share of TX UST facilities with active financial responsibility coverage.",
      x = "Year", y = "FR Coverage Rate"
    )

  p_cov_B <- ggplot(fr_yr_agg, aes(x = year, y = n_covered)) +
    treatment_vline() +
    geom_line(color = COL_TX, linewidth = 1) +
    geom_point(aes(color = factor(post)), size = 2.5) +
    scale_color_manual(values = c("0" = "grey50", "1" = COL_TX),
                       labels = c("Pre-Reform", "Post-Reform"), name = NULL) +
    scale_y_continuous(labels = comma_format()) +
    scale_x_continuous(breaks = seq(1990, PANEL_END, 4)) +
    labs(
      title    = "B: Number of FR-Covered Facilities by Year (Texas)",
      subtitle = "Count of TX facilities with active FR coverage.",
      x = "Year", y = "FR-Covered Facilities"
    )

  save_panels(
    panels         = list(A = p_cov_A, B = p_cov_B),
    base_name      = "Figure_FR_Coverage",
    combined_name  = "Figure_FR_Coverage_Combined",
    panel_width    = 9,  panel_height   = 5,
    combined_width = 18, combined_height = 5,
    ncol           = 2,
    title    = "Financial Responsibility Coverage: Texas vs. Control States",
    subtitle = "Dashed line = Dec 1998 reform. Coverage rate contextualizes reform penetration."
  )
  save_table(fr_yr_agg, "Data_FR_Coverage_ByYear")

} else {
  cat("  SKIPPED: fr_year_raw not found\n")
}


# ─────────────────────────────────────────────────────────────────────────────
# Figure: FR Pricing  (Mid-Continent rate filings)
# Panel A replaced with time-series line plot — bar charts obscure trend
# ─────────────────────────────────────────────────────────────────────────────
if (!is.null(rate_data)) {
  cat("\n--- Figure: FR Pricing ---\n")
  cat("  rate_data columns:", paste(names(rate_data), collapse = ", "), "\n")

  # Identify premium column (mean_tank_premium confirmed in 01b output)
  prem_col <- intersect(
    c("mean_tank_premium", "ANNUAL_PREMIUM", "annual_premium",
      "median_premium", "base_premium", "premium"),
    names(rate_data)
  )[1]
  if (is.na(prem_col)) stop("rate_data: cannot find a premium column. Columns are: ",
                             paste(names(rate_data), collapse = ", "))
  cat(sprintf("  NOTE: using '%s' as premium column\n", prem_col))

  price_yr <- rate_data[
    !is.na(get(prem_col)) & get(prem_col) > 0,
    .(
      median_premium = median(get(prem_col), na.rm = TRUE),
      mean_premium   = mean(get(prem_col),   na.rm = TRUE),
      p25_premium    = quantile(get(prem_col), 0.25, na.rm = TRUE),
      p75_premium    = quantile(get(prem_col), 0.75, na.rm = TRUE),
      n              = .N
    ),
    by = YEAR
  ]
  setorder(price_yr, YEAR)

  ref_yr  <- 1995L
  ref_val <- price_yr[YEAR == ref_yr, median_premium]
  # If 1995 is outside the filing range, fall back to first available year
  if (length(ref_val) == 0 || is.na(ref_val)) {
    ref_yr  <- price_yr[1, YEAR]
    ref_val <- price_yr[1, median_premium]
    cat(sprintf("  NOTE: 1995 not in rate data; using %d as index base\n", ref_yr))
  }
  price_yr[, index := 100 * median_premium / ref_val]
  price_yr[, post  := as.integer(YEAR >= POST_YEAR)]

  # ── Panel A: TIME SERIES line plot (replaces bar chart) ────────────────
  p_pr_A <- ggplot(price_yr, aes(x = YEAR, y = median_premium)) +
    treatment_vline() +
    # IQR ribbon for spread
    geom_ribbon(aes(ymin = p25_premium, ymax = p75_premium),
                fill = COL_TX, alpha = 0.15) +
    # Mean line
    geom_line(aes(y = mean_premium), color = "grey60",
              linewidth = 0.6, linetype = "dashed") +
    # Median line (primary)
    geom_line(color = COL_TX, linewidth = 1.2) +
    geom_point(aes(color = factor(post)), size = 2.5) +
    scale_color_manual(values = c("0" = "grey50", "1" = COL_TX),
                       labels = c("Pre-Reform", "Post-Reform"), name = NULL) +
    scale_y_continuous(labels = dollar_format()) +
    scale_x_continuous(breaks = seq(min(price_yr$YEAR), max(price_yr$YEAR), 3)) +
    labs(
      title    = "A: Median Annual FR Premium Over Time",
      subtitle = "Mid-Continent Casualty rate filings. Ribbon = IQR. Dashed = mean.",
      x = "Year", y = "Median Annual Premium (USD)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # ── Panel B: Premium index ──────────────────────────────────────────────
  p_pr_B <- ggplot(price_yr[!is.na(index)], aes(x = YEAR, y = index)) +
    treatment_vline() +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    geom_line(color = COL_TX, linewidth = 1.1) +
    geom_point(color = COL_TX, size = 2.5) +
    scale_x_continuous(breaks = seq(min(price_yr$YEAR), max(price_yr$YEAR), 3)) +
    labs(
      title    = sprintf("B: Premium Index (Base = %d)", ref_yr),
      subtitle = sprintf("Index 100 = %d median premium. Relative price change around reform.", ref_yr),
      x = "Year", y = sprintf("Premium Index (%d = 100)", ref_yr)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # ── Panel C: Log premium scatter ────────────────────────────────────────
  p_pr_C <- ggplot(
    rate_data[!is.na(get(prem_col)) & get(prem_col) > 0],
    aes(x     = YEAR,
        y     = log(get(prem_col)),
        color = as.factor(sign(YEAR - POST_YEAR)))
  ) +
    treatment_vline() +
    geom_jitter(width = 0.2, height = 0, alpha = 0.15, size = 0.8) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.9,
                aes(group = as.factor(YEAR >= POST_YEAR))) +
    scale_color_manual(values = c("-1" = COL_CTRL, "0" = COL_CTRL, "1" = COL_TX),
                       guide  = "none") +
    scale_x_continuous(breaks = seq(min(rate_data$YEAR, na.rm = TRUE),
                                    max(rate_data$YEAR, na.rm = TRUE), 3)) +
    labs(
      title    = "C: Log Annual Premiums (Scatter + Linear Fit)",
      subtitle = "Pre/post fitted separately. Slope break indicates re-pricing post-reform.",
      x = "Year", y = "Log Annual Premium"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  save_panels(
    panels         = list(A = p_pr_A, B = p_pr_B, C = p_pr_C),
    base_name      = "Figure_FR_Pricing",
    combined_name  = "Figure_FR_Pricing_Combined",
    panel_width    = 8,  panel_height    = 5,
    combined_width = 24, combined_height = 5,
    ncol           = 3,
    title    = "FR Insurance Pricing: Mid-Continent Casualty Rate Filings",
    subtitle = "Dashed line = Dec 1998 reform. Panel C fits linear trends pre/post separately."
  )
  save_table(price_yr, "Data_FR_Pricing_ByYear")

} else {
  cat("  SKIPPED: rate_data_raw not found\n")
}


#==============================================================================
# SECTION 11: TX FR Market Figures
# Uses contract_month already loaded above via load_interim()
#==============================================================================

CONTRACT_PATH <- here("Data", "Processed", "texas_fr_contract_month_panel.csv")

if (!is.null(contract_month)) {
  panel_data_inst <- contract_month[YEAR >= 2007 & YEAR <= 2020]

  # 11.1 Figure [A]: TX FR Coverage Composition
  cat("--- 11.1: TX FR Coverage Composition ---\n")

  panel_data_inst[, plot_category := fcase(
    CATEGORY == "Insurance",      "Private Insurance",
    CATEGORY == "State Fund",     "State Fund",
    CATEGORY == "Self-Insurance", "Self-Insurance",
    CATEGORY == "NO COVERAGE",    "No Coverage",
    default = "Other"
  )]

  fac_categories <- unique(panel_data_inst[, .(FACILITY_ID, YEAR, MONTH, plot_category)])
  fac_categories[, weight := 1.0 / .N, by = .(FACILITY_ID, YEAR, MONTH)]
  regime_shares <- fac_categories[, .(N = sum(weight) / 12), by = .(YEAR, plot_category)]
  regime_shares[, share := N / sum(N), by = YEAR]

  fig_coverage <- ggplot(regime_shares, aes(x = YEAR, y = share, fill = plot_category)) +
    geom_col(position = "fill", width = 0.8) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "Year", y = "Share of Facilities",
         title = "Primary Financial Responsibility Mechanism (2007-2020)",
         fill = "Mechanism") +
    theme(legend.position = "bottom")

  save_fig(fig_coverage, "FigureA_TX_FR_Coverage_Composition", width = 8, height = 6)
  save_table(regime_shares, "FigureA_data_regime_shares")

  # 11.2: TX Private Insurance Market - Dynamic Top 5 & HHI Trend
  cat("--- 11.2: TX Private Insurance Market - Dynamic Top 5 & HHI Trend ---\n")

  ins_panel <- panel_data_inst[plot_category == "Private Insurance" &
                                !is.na(ISSUER_NAME) & ISSUER_NAME != "NO COVERAGE"]
  ins_panel[toupper(ISSUER_NAME) == "OTHER", ISSUER_NAME := "Other Private Insurers"]

  ins_panel[, weight := 1.0 / .N, by = .(FACILITY_ID, YEAR, MONTH)]
  insurer_exposure <- ins_panel[, .(fac_months = sum(weight)), by = .(YEAR, ISSUER_NAME)]
  insurer_exposure[, annual_total := sum(fac_months), by = YEAR]
  insurer_exposure[, market_share := fac_months / annual_total]

  hhi_trend <- insurer_exposure[, .(HHI = sum((market_share * 100)^2)), by = YEAR]

  setorder(insurer_exposure, YEAR, -fac_months)
  insurer_exposure[, annual_rank := frank(-fac_months, ties.method = "first"), by = YEAR]

  ever_top5_names <- unique(insurer_exposure[annual_rank <= 5 & ISSUER_NAME != "Other Private Insurers", ISSUER_NAME])
  cat(sprintf("  Insurers appearing in any annual Top 5: %d firms\n", length(ever_top5_names)))

  historical_volume <- insurer_exposure[ISSUER_NAME %in% ever_top5_names,
                                        .(total_vol = sum(fac_months)), by = ISSUER_NAME]
  setorder(historical_volume, -total_vol)

  insurer_exposure[, Plot_Issuer := fifelse(
    ISSUER_NAME %in% ever_top5_names, ISSUER_NAME, "Other Private Insurers"
  )]

  plot_data_inst <- insurer_exposure[, .(market_share = sum(market_share)), by = .(YEAR, Plot_Issuer)]
  plot_data_inst <- merge(plot_data_inst, hhi_trend, by = "YEAR", all.x = TRUE)

  factor_levels_inst <- c("Other Private Insurers", historical_volume$ISSUER_NAME)
  plot_data_inst[, Plot_Issuer := factor(Plot_Issuer, levels = factor_levels_inst)]

  named_colors  <- scales::hue_pal()(length(ever_top5_names))
  custom_colors <- setNames(c("gray80", named_colors), factor_levels_inst)

  HHI_SCALE_FACTOR <- 5000

  fig_top5 <- ggplot(plot_data_inst, aes(x = as.factor(YEAR))) +
    geom_col(aes(y = market_share, fill = Plot_Issuer),
             width = 0.85, color = "white", linewidth = 0.2) +
    geom_line(aes(y = HHI / HHI_SCALE_FACTOR, group = 1),
              color = "black", linewidth = 1.2, linetype = "dashed") +
    geom_point(aes(y = HHI / HHI_SCALE_FACTOR), color = "black", size = 2.5) +
    scale_y_continuous(
      labels   = scales::percent_format(accuracy = 1),
      expand   = c(0, 0),
      sec.axis = sec_axis(~ . * HHI_SCALE_FACTOR, name = "Herfindahl-Hirschman Index (HHI)")
    ) +
    scale_fill_manual(values = custom_colors) +
    labs(x = "Year", y = "Market Share (by Facility-Months)",
         title = "Private Insurance Market Consolidation",
         subtitle = "Market Share of All Firms Reaching Top 5 in Any Year (Bars) + Per-Year HHI (Dashed)",
         fill = "Insurer") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right") +
    guides(fill = guide_legend(ncol = 1, reverse = TRUE))

  save_fig(fig_top5, "Figure_TX_FR_Top5_Dominance_HHI", width = 12, height = 6)
  save_table(plot_data_inst, "Figure_data_Top5_HHI")

  # 11.3: Contract Switching Rate
  cat("--- 11.3: Contract Switching Rate ---\n")

  fac_yr_primary <- ins_panel[, .(months = .N), by = .(FACILITY_ID, YEAR, ISSUER_NAME)]
  setorder(fac_yr_primary, FACILITY_ID, YEAR, -months)
  fac_yr_primary <- fac_yr_primary[, .SD[1], by = .(FACILITY_ID, YEAR)]

  setorder(fac_yr_primary, FACILITY_ID, YEAR)
  fac_yr_primary[, prev_issuer := shift(ISSUER_NAME, 1L), by = FACILITY_ID]
  fac_yr_primary[, prev_year   := shift(YEAR,         1L), by = FACILITY_ID]
  fac_yr_primary[, switched    := as.integer(
    !is.na(prev_issuer) & prev_year == YEAR - 1L & ISSUER_NAME != prev_issuer
  )]

  annual_switch_rate <- fac_yr_primary[!is.na(switched), .(switch_rate = mean(switched)), by = YEAR]
  setorder(annual_switch_rate, YEAR)

  cat(sprintf("  Overall annual switch rate: %.1f%%\n",
              100 * mean(fac_yr_primary$switched, na.rm = TRUE)))

  fig_churn <- ggplot(annual_switch_rate, aes(x = YEAR, y = switch_rate)) +
    geom_line(color = "#264653", linewidth = 1.2) +
    geom_point(color = "#e76f51", size = 3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                       limits = c(0, max(annual_switch_rate$switch_rate) * 1.2)) +
    scale_x_continuous(breaks = min(annual_switch_rate$YEAR):max(annual_switch_rate$YEAR)) +
    labs(x = "Year", y = "Annual Switch Rate",
         title = "Facility Insurance Contract Switching Rate",
         subtitle = "Share of facilities changing primary insurer year-over-year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  save_fig(fig_churn, "Figure_TX_FR_Annual_Switch_Rate", width = 8, height = 5)
  save_table(annual_switch_rate, "Figure_data_Annual_Switch_Rate")

  # 11.4: Statutory Coverage Limits
  cat("--- 11.4: Statutory Coverage Limits ---\n")

  limits_data <- unique(ins_panel[!is.na(COVER_OCC) & COVER_OCC >= 10000,
                                   .(FIN_ASSUR_ID, YEAR, COVER_OCC)])
  limits_data[, coverage_tier := fcase(
    COVER_OCC == 1000000, "$1M (Statutory)",
    COVER_OCC  < 1000000, "Sub-$1M",
    COVER_OCC  > 1000000, "Excess (>$1M)"
  )]
  tier_shares <- limits_data[, .N, by = .(YEAR, coverage_tier)]
  tier_shares[, share := N / sum(N), by = YEAR]
  tier_shares[, coverage_tier := factor(coverage_tier,
                                        levels = c("Sub-$1M", "$1M (Statutory)", "Excess (>$1M)"))]

  fig_limits <- ggplot(tier_shares, aes(x = YEAR, y = share, fill = coverage_tier)) +
    geom_col(position = "stack", width = 0.7) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c("Sub-$1M"        = "firebrick",
                                 "$1M (Statutory)" = "gray60",
                                 "Excess (>$1M)"   = COL_CTRL)) +
    labs(x = "Year", y = "Share of Contracts",
         title = "Per-Occurrence Coverage Limits",
         subtitle = "Adherence to the standard $1M statutory minimum vs. excess coverage",
         fill = "Occurrence Limit") +
    theme(legend.position = "bottom")

  save_fig(fig_limits, "Figure_TX_FR_Coverage_Limits", width = 8, height = 5)
  save_table(tier_shares, "Figure_data_Coverage_Limits")

} else {
  cat("  WARNING: Skipped Section 11 — contract_month_raw not found\n")
}


# ─────────────────────────────────────────────────────────────────────────────
# Figure: Risk-Differentiated Pricing (Mid-Continent Rate Filings)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: Mid-Continent Risk-Differentiated Pricing ---\n")

if (!is.null(rate_data)) {
  pricing_range <- rate_data[, .(
    mean_base    = mean(base_premium, na.rm = TRUE),
    mean_min     = mean(sched_min,    na.rm = TRUE),
    mean_max     = mean(sched_max,    na.rm = TRUE),
    sched_spread = mean(sched_max - sched_min, na.rm = TRUE),
    n_fac        = uniqueN(FACILITY_ID)
  ), by = YEAR]
  setorder(pricing_range, YEAR)

  fig_b <- ggplot(pricing_range, aes(x = YEAR)) +
    geom_ribbon(aes(ymin = mean_min, ymax = mean_max),
                fill = COL_TX, alpha = 0.2) +
    geom_line(aes(y = mean_base), color = COL_TX, linewidth = 1) +
    geom_point(aes(y = mean_base), color = COL_TX, size = 2) +
    labs(x = "Year", y = "Premium ($)",
         title = "Risk-Differentiated Pricing: Mid-Continent Rate Filings",
         subtitle = "Shaded band = filed scheduling range (min to max across facilities)",
         caption = sprintf("N facilities: %s\u2013%s per year",
                           format(min(pricing_range$n_fac), big.mark = ","),
                           format(max(pricing_range$n_fac), big.mark = ","))) +
    scale_y_continuous(labels = scales::dollar_format())

  save_fig(fig_b, "FigureB_MidContinent_Pricing", width = 8, height = 5)
  save_table(pricing_range, "FigureB_data_pricing_range")
} else {
  cat("  SKIPPED: rate_data not found\n")
}


# ─────────────────────────────────────────────────────────────────────────────
# Figure: Regulatory & Data Availability Timeline
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: Regulatory Timeline ---\n")

timeline_events <- data.table(
  date  = as.numeric(c(1988, 1989, 1993, 1995, 1998, 2005, 2007, 2020)),
  label = c(
    "EPA final technical\nstandards (40 CFR §280)",
    "TX phased mandates\nbegin (earliest cohort)",
    "TX phased mandates\nend (latest cohort)",
    "H.B. 2587 enacted\n(PSTRF cutoff set)",
    "Federal upgrade deadline\n& PSTRF cutoff",
    "EPAct 2005\n(database funding)",
    "TCEQ electronic FR\nrecording begins",
    "End of study\nwindow"
  ),
  type  = c("federal", "texas", "texas", "texas", "both", "federal", "data", "data"),
  y_pos = c(0.8, 0.5, 0.5, 0.65, 0.9, 0.7, 0.55, 0.55)
)

data_bars <- data.table(
  source = c("Control States Panel", "Texas Tank & LUST Panel",
             "Texas FR Panel", "Mid-Continent Rates"),
  start  = c(1985, 1985, 2007,
             ifelse(!is.null(rate_data), min(rate_data$YEAR, na.rm = TRUE), 2006)),
  end    = c(2020, 2020, 2020,
             ifelse(!is.null(rate_data), max(rate_data$YEAR, na.rm = TRUE), 2024)),
  y_bar  = c(0.20, 0.15, 0.10, 0.05)
)

fig_t <- ggplot() +
  geom_segment(data = data_bars,
               aes(x = start, xend = end, y = y_bar, yend = y_bar),
               linewidth = 4, color = "gray70") +
  geom_text(data = data_bars,
            aes(x = (start + end) / 2, y = y_bar + 0.03, label = source),
            size = 2.8, fontface = "italic") +
  geom_segment(data = timeline_events,
               aes(x = date, xend = date, y = 0.28, yend = y_pos - 0.03),
               linetype = "dotted", color = "gray50") +
  geom_point(data = timeline_events, aes(x = date, y = y_pos),
             size = 3, shape = 18,
             color = fifelse(timeline_events$type == "texas",   COL_TX,
                     fifelse(timeline_events$type == "federal", COL_CTRL,
                             "gray40"))) +
  geom_text(data = timeline_events,
            aes(x = date, y = y_pos + 0.06, label = label),
            size = 2.5, lineheight = 0.85) +
  annotate("rect", xmin = 1997.5, xmax = 1998.5, ymin = 0, ymax = 1,
           fill = "gold", alpha = 0.1) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), limits = c(1984, 2022)) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(title = "Regulatory and Data Availability Timeline", x = "Year", y = NULL) +
  theme(axis.text.y  = element_blank(), axis.ticks.y = element_blank(),
        panel.grid   = element_blank())

save_fig(fig_t, "FigureT_Regulatory_Timeline", width = 14, height = 6)

cat("=== 01k COMPLETE ===\n")