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
#     PanelA — Median annual premium by year (from Mid-Continent filings)
#     PanelB — Premium index (1995 = 100)
#     PanelC — Log premium scatter with reform cutoff
#     Combined → Figure_FR_Pricing_Combined
#
#   Figure_FR_Switching:
#     PanelA — Monthly FR provider switching rate (contract panel)
#     PanelB — Cumulative fraction having switched ≥ once (by year)
#     Combined → Figure_FR_Switching_Combined
#==============================================================================

source(here::here("Code", "01a_Setup.R"))
cat("=== 01k: INSTITUTIONAL CONTEXT FIGURES ===\n")

fr_year       <- load_interim("fr_year_raw")
rate_data     <- load_interim("rate_data_raw")
contract_month <- load_interim("contract_month_raw")

# ─────────────────────────────────────────────────────────────────────────────
# Figure: FR Coverage
# ─────────────────────────────────────────────────────────────────────────────
if (!is.null(fr_year)) {
  cat("\n--- Figure: FR Coverage ---\n")

  fr_year[, post := as.integer(year >= POST_YEAR)]
  fr_yr_agg <- fr_year[, .(
    n_covered    = sum(has_fr_coverage == 1, na.rm = TRUE),
    n_total      = .N,
    cov_rate     = mean(has_fr_coverage == 1, na.rm = TRUE)
  ), by = .(year, Group = fifelse(state == "TX", "Texas", "Control"))]

  p_cov_A <- ggplot(fr_yr_agg,
                    aes(x = year, y = cov_rate, color = Group)) +
    treatment_vline() +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = COL_PAIR) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(1990, PANEL_END, 4)) +
    labs(
      title    = "A: FR Coverage Rate by Year",
      subtitle = "Share of UST facilities with active financial responsibility coverage.",
      x = "Year", y = "FR Coverage Rate", color = NULL
    )

  p_cov_B <- ggplot(fr_yr_agg,
                    aes(x = year, y = n_covered, fill = Group)) +
    treatment_vline() +
    geom_col(position = position_stack(), width = 0.8, alpha = 0.85) +
    scale_fill_manual(values = COL_PAIR) +
    scale_y_continuous(labels = comma_format()) +
    scale_x_continuous(breaks = seq(1990, PANEL_END, 4)) +
    labs(
      title    = "B: Number of FR-Covered Facilities by Year",
      subtitle = "Stacked: TX (bottom) + Control states.",
      x = "Year", y = "FR-Covered Facilities", fill = NULL
    )

  save_panels(
    panels          = list(A = p_cov_A, B = p_cov_B),
    base_name       = "Figure_FR_Coverage",
    combined_name   = "Figure_FR_Coverage_Combined",
    panel_width     = 9, panel_height = 5,
    combined_width  = 18, combined_height = 5,
    ncol            = 2,
    title    = "Financial Responsibility Coverage: Texas vs. Control States",
    subtitle = "Dashed line = Dec 1998 reform. Coverage rate contextualizes reform penetration."
  )
  save_table(fr_yr_agg, "Data_FR_Coverage_ByYear")
} else {
  cat("  SKIPPED: fr_year_raw not found\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# Figure: FR Pricing (Mid-Continent rate filings)
# ─────────────────────────────────────────────────────────────────────────────
if (!is.null(rate_data)) {
  cat("\n--- Figure: FR Pricing ---\n")

  price_yr <- rate_data[!is.na(ANNUAL_PREMIUM) & ANNUAL_PREMIUM > 0, .(
    median_premium = median(ANNUAL_PREMIUM, na.rm=TRUE),
    mean_premium   = mean(ANNUAL_PREMIUM,   na.rm=TRUE),
    n              = .N
  ), by = YEAR]
  setorder(price_yr, YEAR)

  ref_yr   <- 1995L
  ref_val  <- price_yr[YEAR == ref_yr, median_premium]
  if (length(ref_val) > 0 && !is.na(ref_val))
    price_yr[, index := 100 * median_premium / ref_val]
  else
    price_yr[, index := NA_real_]

  price_yr[, post := as.integer(YEAR >= POST_YEAR)]

  p_pr_A <- ggplot(price_yr, aes(x = YEAR, y = median_premium)) +
    treatment_vline() +
    geom_col(aes(fill = factor(post)), width = 0.75, alpha = 0.85) +
    scale_fill_manual(values = c("0" = "gray60", "1" = COL_TX),
                      labels = c("Pre-Reform","Post-Reform"), name = NULL) +
    scale_y_continuous(labels = dollar_format()) +
    scale_x_continuous(breaks = seq(min(price_yr$YEAR), max(price_yr$YEAR), 3)) +
    labs(
      title    = "A: Median Annual FR Premium by Year",
      subtitle = "Mid-Continent Casualty rate filings. Post-reform = red.",
      x = "Year", y = "Median Annual Premium (USD)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p_pr_B <- ggplot(price_yr[!is.na(index)],
                   aes(x = YEAR, y = index)) +
    treatment_vline() +
    geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
    geom_line(color = COL_TX, linewidth = 1.1) +
    geom_point(color = COL_TX, size = 2.5) +
    scale_x_continuous(breaks = seq(min(price_yr$YEAR), max(price_yr$YEAR), 3)) +
    labs(
      title    = sprintf("B: Premium Index (Base = %d)", ref_yr),
      subtitle = sprintf("Index 100 = %d median premium. Shows relative price change around reform.", ref_yr),
      x = "Year", y = sprintf("Premium Index (%d = 100)", ref_yr)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p_pr_C <- ggplot(rate_data[!is.na(ANNUAL_PREMIUM) & ANNUAL_PREMIUM > 0],
                   aes(x = YEAR, y = log(ANNUAL_PREMIUM),
                       color = as.factor(sign(YEAR - POST_YEAR)))) +
    treatment_vline() +
    geom_jitter(width = 0.2, height = 0, alpha = 0.15, size = 0.8) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.9,
                aes(group = as.factor(YEAR >= POST_YEAR))) +
    scale_color_manual(values = c("-1" = COL_CTRL, "0" = COL_CTRL, "1" = COL_TX),
                       guide = "none") +
    scale_x_continuous(breaks = seq(min(rate_data$YEAR, na.rm=TRUE),
                                    max(rate_data$YEAR, na.rm=TRUE), 3)) +
    labs(
      title    = "C: Log Annual Premiums (Scatter + Linear Fit)",
      subtitle = "Pre/post fitted separately. Slope break indicates re-pricing post-reform.",
      x = "Year", y = "Log Annual Premium"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  save_panels(
    panels          = list(A = p_pr_A, B = p_pr_B, C = p_pr_C),
    base_name       = "Figure_FR_Pricing",
    combined_name   = "Figure_FR_Pricing_Combined",
    panel_width     = 8, panel_height = 5,
    combined_width  = 24, combined_height = 5,
    ncol            = 3,
    title    = "FR Insurance Pricing: Mid-Continent Casualty Rate Filings",
    subtitle = "Dashed line = Dec 1998 reform. Panel C fits linear trends pre/post separately."
  )
  save_table(price_yr, "Data_FR_Pricing_ByYear")
} else {
  cat("  SKIPPED: rate_data_raw not found\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# Figure: FR Provider Switching
# ─────────────────────────────────────────────────────────────────────────────
if (!is.null(contract_month)) {
  cat("\n--- Figure: FR Switching ---\n")

  if ("year_month" %in% names(contract_month) && !inherits(contract_month$year_month, "Date"))
    contract_month[, year_month := as.Date(year_month)]
  contract_month[, year := year(year_month)]

  switch_monthly <- contract_month[!is.na(provider_switch), .(
    switch_rate = mean(provider_switch == 1, na.rm = TRUE),
    n_contracts = .N
  ), by = .(year_month, Group = fifelse(state_std == "TX", "Texas", "Control"))]
  setorder(switch_monthly, year_month)

  p_sw_A <- ggplot(switch_monthly,
                   aes(x = year_month, y = switch_rate, color = Group)) +
    geom_vline(xintercept = as.numeric(as.Date("1999-01-01")),
               linetype = "dashed", color = "gray30", linewidth = 0.6) +
    annotate("text", x = as.Date("1999-03-01"), y = Inf,
             vjust = 1.5, hjust = 0, size = 2.5, color = "gray30",
             label = "Dec 1998 reform") +
    geom_line(linewidth = 0.6, alpha = 0.6) +
    geom_smooth(method = "loess", span = 0.2, se = FALSE,
                linewidth = 1.1) +
    scale_color_manual(values = COL_PAIR) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(
      title    = "A: Monthly Provider Switching Rate",
      subtitle = "Fraction of active contracts switching FR provider. Smooth = LOESS.",
      x = "Month", y = "Provider Switching Rate", color = NULL
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Cumulative fraction having switched ≥ once (by year)
  switch_annual <- contract_month[, .(
    ever_switched = as.integer(any(provider_switch == 1, na.rm=TRUE))
  ), by = .(panel_id, year, Group = fifelse(state_std=="TX","Texas","Control"))]
  cum_switch <- switch_annual[, .(
    frac_switched = mean(ever_switched, na.rm=TRUE)
  ), by = .(year, Group)]
  setorder(cum_switch, year)

  p_sw_B <- ggplot(cum_switch,
                   aes(x = year, y = frac_switched, color = Group)) +
    treatment_vline() +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = COL_PAIR) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title    = "B: Annual Share Having Switched Provider \u22651 Times",
      subtitle = "Cumulative within each year. Post-reform increase consistent with market re-pricing.",
      x = "Year", y = "Share That Switched Provider", color = NULL
    )

  save_panels(
    panels          = list(A = p_sw_A, B = p_sw_B),
    base_name       = "Figure_FR_Switching",
    combined_name   = "Figure_FR_Switching_Combined",
    panel_width     = 9, panel_height = 5,
    combined_width  = 18, combined_height = 5,
    ncol            = 2,
    title    = "FR Provider Switching: Texas vs. Control",
    subtitle = "Panel A: monthly rate (LOESS smoothed). Panel B: annual share that switched ≥1 times."
  )
} else {
  cat("  SKIPPED: contract_month_raw not found\n")
}

cat("=== 01k COMPLETE ===\n")
