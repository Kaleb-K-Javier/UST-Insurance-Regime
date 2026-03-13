#==============================================================================
# 01l_DataQuality.R
# Data Quality Tables + "Restriction Earns Parallel Trends" 2x2 Figure
#
# This script produces the most important motivating figure in the paper:
# a 2x2 grid showing that the make-model restriction is NECESSARY
# to achieve parallel pre-trends.
#
# Figure_RestrictionEarnsParallelTrends (2x2):
#   Row 1 (Full sample):     Panel 1A closure rate  |  Panel 1B difference
#   Row 2 (Make-Model):      Panel 2A closure rate  |  Panel 2B difference
#   Combined → Figure_RestrictionEarnsParallelTrends_Combined
#   Individual panels: _Panel1A, _Panel1B, _Panel2A, _Panel2B
#
# Data quality:
#   Table_DataQuality      — state-level completeness
#   Table_AttritionLog     — sequential filter counts  (Table B.1)
#   Table_MissingBalance   — missing-date balance test (Table B.3)
#==============================================================================

source(here::here("Code",'Analysis','Descrptive Facts', "01a_Setup.R"))
cat("=== 01l: DATA QUALITY + KEY MOTIVATION FIGURE ===\n")

annual_data         <- load_interim("annual_data")
data_quality_report <- load_interim("data_quality_report")
attrition_log       <- load_interim("attrition_log")
balance_glm         <- load_interim("balance_glm")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE: Restriction Earns Parallel Trends (2x2) ← MOST IMPORTANT FIGURE
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: Restriction Earns Parallel Trends (2x2) ---\n")

make_rate_panel <- function(dt, yr_start, yr_end, label,
                             add_mandate = FALSE, show_label = TRUE) {
  rates <- dt[panel_year %between% c(yr_start, yr_end), .(
    closure_rate = mean(closure_event, na.rm=TRUE),
    se = sqrt(mean(closure_event, na.rm=TRUE) *
              (1 - mean(closure_event, na.rm=TRUE)) / .N)
  ), by = .(panel_year,
             Group = fifelse(texas_treated==1,"Texas","Control"))]

  p <- ggplot(rates,
              aes(x = panel_year, y = closure_rate,
                  color = Group, fill = Group)) +
    geom_ribbon(aes(ymin = closure_rate - 1.96*se,
                    ymax = closure_rate + 1.96*se), alpha=0.12, color=NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    treatment_vline()

  if (add_mandate) p <- p + mandate_shade_layer()

  if (show_label)
    p <- p + annotate("label", x = yr_start + 1, y = Inf,
                      vjust = 1.3, hjust = 0, size = 2.8,
                      label = label, fill = "white", label.size = 0.3)

  p + scale_color_manual(values = COL_PAIR) +
      scale_fill_manual(values  = COL_PAIR) +
      scale_x_continuous(breaks = seq(yr_start, yr_end, 4)) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Year", y = "Annual Closure Rate", color = NULL, fill = NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

make_diff_panel <- function(dt, yr_start, yr_end, wald_p = NULL,
                             add_mandate = FALSE, show_label = TRUE) {
  rates <- dt[panel_year %between% c(yr_start, yr_end), .(
    closure_rate = mean(closure_event, na.rm=TRUE),
    se = sqrt(mean(closure_event, na.rm=TRUE) *
              (1 - mean(closure_event, na.rm=TRUE)) / .N)
  ), by = .(panel_year,
             Group = fifelse(texas_treated==1,"Texas","Control"))]

  wide <- dcast(rates, panel_year ~ Group,
                value.var = c("closure_rate","se"))
  wide[, `:=`(
    diff    = closure_rate_Texas - closure_rate_Control,
    diff_se = sqrt(se_Texas^2 + se_Control^2)
  )]
  wide[, `:=`(diff_lo = diff - 1.96*diff_se,
               diff_hi = diff + 1.96*diff_se)]

  p <- ggplot(wide[!is.na(diff)], aes(x = panel_year, y = diff)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    geom_ribbon(aes(ymin=diff_lo, ymax=diff_hi), fill=COL_TX, alpha=0.18) +
    geom_line(color = COL_TX, linewidth = 1) +
    geom_point(color = COL_TX, size = 1.8) +
    treatment_vline()

  if (add_mandate) p <- p + mandate_shade_layer()

  if (!is.null(wald_p) && show_label)
    p <- p + annotate("label",
                      x = yr_start + 1, y = Inf, vjust = 1.3, hjust = 0,
                      size = 2.8, fill = "white", label.size = 0.3,
                      label = sprintf("Pre-trend p = %.3f", wald_p))

  p + scale_x_continuous(breaks = seq(yr_start, yr_end, 4)) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Year", y = "Closure Rate Diff (TX \u2212 CTL)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Get pre-trend p-values from 01g
pt_results <- tryCatch(load_interim("pt_results"), error = function(e) NULL)
p_full_wald <- if (!is.null(pt_results))
  pt_results[grepl("Pooled", Specification), `p-value`][1] else NULL
p_mm_wald   <- if (!is.null(pt_results))
  pt_results[grepl("Spec A", Specification), `p-value`][1] else NULL

# Make-model sample flag on annual_data
annual_data[, is_mm := as.integer(
  has_single_walled == 1 &
  has_gasoline_year == 1 &
  single_tanks == active_tanks &
  install_year > MM_INSTALL_START - 1 &
  install_year <= MM_INSTALL_END
)]

yr_start <- 1990L; yr_end <- PANEL_END

# Row 1: Full sample
p_1A <- make_rate_panel(annual_data, yr_start, yr_end,
                         label = "Full Sample",
                         add_mandate = TRUE) +
  labs(title = "Full Sample: Closure Rate",
       subtitle = "All incumbent facilities. Gold = TX mandate window (1989\u20131993).")

p_1B <- make_diff_panel(annual_data, yr_start, yr_end,
                         wald_p = p_full_wald,
                         add_mandate = TRUE) +
  labs(title = "Full Sample: TX \u2212 Control Difference",
       subtitle = "Mandate-driven TX spike violates parallel trends (p < 0.05).")

# Row 2: Make-model sample
p_2A <- make_rate_panel(annual_data[is_mm == 1], yr_start, yr_end,
                         label = "Make-Model Sample") +
  labs(title = "Make-Model Sample: Closure Rate",
       subtitle = "Post-1989 SW, gasoline, single-tank, 1990\u20131997 cohort.")

p_2B <- make_diff_panel(annual_data[is_mm == 1], yr_start, yr_end,
                         wald_p = p_mm_wald) +
  labs(title = "Make-Model Sample: TX \u2212 Control Difference",
       subtitle = "Pre-period near zero. Parallel trends NOT rejected.")

save_panels(
  panels          = list(`1A` = p_1A, `1B` = p_1B, `2A` = p_2A, `2B` = p_2B),
  base_name       = "Figure_RestrictionEarnsParallelTrends",
  combined_name   = "Figure_RestrictionEarnsParallelTrends_Combined",
  panel_width     = 9, panel_height = 5,
  combined_width  = 18, combined_height = 10,
  ncol            = 2, nrow = 2,
  title    = "The Make-Model Restriction Earns Parallel Trends",
  subtitle = paste0(
    "Row 1: full sample fails parallel trends due to TX mandate spike (gold shading). ",
    "Row 2: make-model sample passes. Restriction is empirically justified, not arbitrary."
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# Table B.1: Attrition log
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Table B.1: Attrition Log ---\n")

attrition_dt <- rbindlist(lapply(seq_along(attrition_log), function(i) {
  e <- attrition_log[[i]]
  data.table(
    Step        = i - 1L,
    Stage       = e$stage,
    Filter      = e$filter,
    Facilities  = e$facilities,
    `Fac-Years` = e$fac_years
  )
}))
attrition_dt[, `:=`(
  `Δ Facilities` = c(NA_integer_, diff(Facilities)),
  `Δ Fac-Years`  = c(NA_integer_, diff(`Fac-Years`))
)]
print(attrition_dt)
save_table(attrition_dt, "TableB1_Attrition_Log")

write_tex(
  kbl(attrition_dt, format="latex", booktabs=TRUE, linesep="",
      caption="Sequential attrition log. Each row applies one additional filter to the facility-year panel. $\\Delta$ columns show marginal change in observations.",
      label="tab:attrition") |>
    kable_styling(latex_options = "scale_down", font_size = 9),
  "TableB1_Attrition_Log"
)

# ─────────────────────────────────────────────────────────────────────────────
# Table B.3: Missing-date balance
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Table B.3: Missing Date Balance ---\n")

if (!is.null(balance_glm)) {
  broom_balance <- broom::tidy(balance_glm, conf.int = TRUE)
  save_table(as.data.table(broom_balance), "TableB3_Missing_Date_Balance")

  write_tex(
    kbl(broom_balance[, c("term","estimate","std.error","statistic","p.value")],
        format="latex", booktabs=TRUE, linesep="", digits=4,
        caption="Balance test for missing date exclusion. Logistic regression of \\textit{any missing date} indicator on Texas dummy. $p < 0.05$ would indicate differential exclusion.",
        label="tab:missing-balance") |>
      kable_styling(latex_options = "scale_down", font_size = 9),
    "TableB3_Missing_Date_Balance"
  )
}
# ─────────────────────────────────────────────────────────────────────────────
# Table: State Data Quality
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Table: State Data Quality ---\n")

# Read directly from processed CSV (interim object has different column schema)
dq <- fread(here::here("Data", "Processed", "Master_Data_Quality_Report.csv"))

# Print actual column names so you can verify
cat("Columns in Master_Data_Quality_Report.csv:\n")
print(names(dq))

# Compute LUST missing-date share
dq[, lust_missing_pct := fifelse(
  !is.na(total_lusts) & total_lusts > 0,
  round(n_missing_report_date / total_lusts * 100, 1),
  NA_real_
)]

# Clean group label
dq[, group_clean := fcase(
  study_group %like% "Target",          "Target",
  study_group %like% "Control Tier 1",  "Control Tier 1",
  study_group %like% "Control Tier 2",  "Control Tier 2",
  study_group %like% "Excluded.*Treat", "Excl. (Treated)",
  study_group %like% "Excluded.*Other", "Excl. (No Fund)",
  study_group %like% "Remainder",       "Remainder",
  default = "Other"
)]

group_order <- c("Target", "Control Tier 1", "Control Tier 2",
                 "Excl. (Treated)", "Excl. (No Fund)", "Remainder")
dq[, group_clean := factor(group_clean, levels = group_order)]
setorder(dq, group_clean, state)

# Format numeric columns: NA → "---", 0 → "0.0"
fmt_pct <- function(x) {
  fifelse(is.na(x), "---",
    fifelse(x == 0,  "0.0",
      formatC(x, format = "f", digits = 1)))
}

dq[, pct_closed_missing_date  := fmt_pct(pct_closed_missing_date)]
dq[, pct_missing_install_date := fmt_pct(pct_missing_install_date)]
dq[, pct_miss_tank_type       := fmt_pct(pct_miss_tank_type)]
dq[, lust_missing_pct         := fmt_pct(lust_missing_pct)]
dq[, total_tanks_fmt          := formatC(total_tanks, format = "d", big.mark = ",")]

# Subset columns for print
dq_print <- dq[, .(
  State          = state,
  group_clean,
  Tanks          = total_tanks_fmt,
  `Close Date`   = pct_closed_missing_date,
  `Install Date` = pct_missing_install_date,
  `Wall Type`    = pct_miss_tank_type,
  `LUST Date`    = lust_missing_pct
)]

# Build pack_rows indices before dropping group col
grp_idx <- function(g) {
  rows <- which(dq_print$group_clean == g)
  if (length(rows) == 0) return(c(1L, 1L))
  c(min(rows), max(rows))
}

dq_kbl <- dq_print[, !"group_clean"]

dq_tex <- kbl(
  dq_kbl,
  format   = "latex",
  booktabs = TRUE,
  linesep  = "",
  align    = c("l", "r", "r", "r", "r", "r"),
  caption  = paste(
    "State-level data completeness on the four dimensions required for panel",
    "construction. Each cell reports the percentage of records missing the",
    "indicated field. Remainder states are excluded due to pervasive missingness.",
    "Washington and Oregon (Excl., No Fund) are excluded on institutional grounds",
    "regardless of data quality. ``---'' indicates field not applicable or",
    "denominator is zero."
  ),
  label = "tab:data-quality"
) |>
  add_header_above(
    c(" " = 2,
      "\\% Missing (tanks)" = 3,
      "\\% Missing (LUSTs)" = 1),
    escape = FALSE
  ) |>
  pack_rows("Target",          grp_idx("Target")[1],          grp_idx("Target")[2]) |>
  pack_rows("Control Tier 1",  grp_idx("Control Tier 1")[1],  grp_idx("Control Tier 1")[2]) |>
  pack_rows("Control Tier 2",  grp_idx("Control Tier 2")[1],  grp_idx("Control Tier 2")[2]) |>
  pack_rows("Excl. (Treated)", grp_idx("Excl. (Treated)")[1], grp_idx("Excl. (Treated)")[2]) |>
  pack_rows("Excl. (No Fund)", grp_idx("Excl. (No Fund)")[1], grp_idx("Excl. (No Fund)")[2]) |>
  pack_rows("Remainder",       grp_idx("Remainder")[1],       grp_idx("Remainder")[2]) |>
  kable_styling(latex_options = "scale_down", font_size = 9)

save_table(dq_print[, !"group_clean"], "TableA0_DataQuality_ByState")
write_tex(dq_tex, "TableA0_DataQuality_ByState")

cat("=== 01l COMPLETE ===\n")


#==============================================================================
# 01n_DescriptiveTable.R
# Table 0: Sample Characteristics — Full Sample vs MM Comparison Sample
#
# Two-panel descriptive statistics table comparing:
#   Panel A: Full analysis sample (all states, 1990-1997 pre-period)
#   Panel B: MM comparison sample (All-SW, Gasoline-Only, 1990-1997)
#
# Each panel shows TX mean | CTL mean | Diff (TX - CTL) | p-value
#
# Rows (pre-period annual means unless noted):
#   --- Facility Scale ---
#   Active tanks (Dec)
#   Total capacity (gallons, Dec)
#   Mean tank age (years, Dec)
#   --- Portfolio Composition ---
#   Single-walled share (%)        [degenerate = 100% in MM by construction]
#   Gasoline-only share (%)        [degenerate = 100% in MM by construction]
#   Pre-1989 install (%)
#   --- Outcomes (pre-reform) ---
#   Annual closure rate (%)
#   Annual leak rate (%)
#   --- Sample Counts ---
#   N facilities
#   N facility-years
#==============================================================================

source(here::here("Code", "Analysis", "Descrptive Facts", "01a_Setup.R"))
cat("=== 01n: SAMPLE DESCRIPTION TABLE ===\n")

# ── 0. Load ───────────────────────────────────────────────────────────────────
annual_data <- load_interim("annual_data")

PRE_START <- 1990L
PRE_END   <- 1997L

# ── 1. Define samples ─────────────────────────────────────────────────────────
pre_full <- annual_data[panel_year %between% c(PRE_START, PRE_END)]

pre_mm <- annual_data[
  fac_wall == "All-SW"        &
  fac_fuel == "Gasoline-Only" &
  !is.na(make_model_fac)      &
  !is.na(fac_oldest_age_bin)  &
  panel_year %between% c(PRE_START, PRE_END)
]

# ── 2. Helper: compute one row of stats ──────────────────────────────────────
# Returns a list: tx_mean, ctl_mean, diff, p_value
# Uses a two-sample t-test (facility-year observations) for the p-value.
# For count rows (N fac, N fac-years) p_value is NA.
desc_row <- function(dt, var_expr, label, pct = FALSE, fmt_digits = 2) {
  tx_vals  <- dt[texas_treated == 1, eval(parse(text = var_expr))]
  ctl_vals <- dt[texas_treated == 0, eval(parse(text = var_expr))]

  tx_vals  <- tx_vals[!is.na(tx_vals)]
  ctl_vals <- ctl_vals[!is.na(ctl_vals)]

  tx_mean  <- mean(tx_vals)
  ctl_mean <- mean(ctl_vals)
  diff_val <- tx_mean - ctl_mean

  # Welch t-test; suppress warning when variance is zero (degenerate vars)
  p_val <- tryCatch(
    suppressWarnings(t.test(tx_vals, ctl_vals)$p.value),
    error = function(e) NA_real_
  )

  mult <- if (pct) 100 else 1
  list(
    label    = label,
    tx_mean  = tx_mean  * mult,
    ctl_mean = ctl_mean * mult,
    diff     = diff_val * mult,
    p_val    = p_val,
    fmt_digits = fmt_digits,
    pct      = pct
  )
}

# ── 3. Compute all rows for each sample ──────────────────────────────────────
make_panel <- function(dt, sample_label) {

  rows <- list(
    # Facility scale
    desc_row(dt, "active_tanks",      "Active tanks (Dec.)",          fmt_digits = 1),
    desc_row(dt, "total_capacity",    "Total capacity (gal., Dec.)",  fmt_digits = 0),
    desc_row(dt, "avg_tank_age",      "Mean tank age (years, Dec.)",  fmt_digits = 1),
    # Portfolio composition
    desc_row(dt, "has_single_walled", "Single-walled share",  pct = TRUE, fmt_digits = 1),
    desc_row(dt, "has_gasoline_year", "Gasoline-only share",  pct = TRUE, fmt_digits = 1),
    desc_row(dt, "as.integer(install_year < 1989)",
                                      "Pre-1989 install",     pct = TRUE, fmt_digits = 1),
    # Outcomes
    desc_row(dt, "closure_event",     "Annual closure rate",  pct = TRUE, fmt_digits = 2),
    desc_row(dt, "as.integer(n_leaks > 0)",
                                      "Annual leak rate",     pct = TRUE, fmt_digits = 2)
  )

  # Format each row
  panel_dt <- rbindlist(lapply(rows, function(r) {
    fmt <- function(x) formatC(x, format = "f", digits = r$fmt_digits, big.mark = ",")
    stars <- function(p) {
      if (is.na(p))    return("")
      if (p < 0.01)    return("$^{***}$")
      if (p < 0.05)    return("$^{**}$")
      if (p < 0.10)    return("$^{*}$")
      return("")
    }
    diff_fmt <- sprintf("%s%s%s",
      ifelse(r$diff > 0, "+", ""),
      fmt(r$diff),
      stars(r$p_val))

    data.table(
      Variable = r$label,
      TX       = fmt(r$tx_mean),
      CTL      = fmt(r$ctl_mean),
      Diff     = diff_fmt
    )
  }))

  # Count rows (no t-test)
  n_fac    <- data.table(
    Variable = "\\quad $N$ facilities",
    TX  = format(dt[texas_treated == 1, uniqueN(panel_id)], big.mark = ","),
    CTL = format(dt[texas_treated == 0, uniqueN(panel_id)], big.mark = ","),
    Diff = "---"
  )
  n_fac_yr <- data.table(
    Variable = "\\quad $N$ facility-years",
    TX  = format(nrow(dt[texas_treated == 1]), big.mark = ","),
    CTL = format(nrow(dt[texas_treated == 0]), big.mark = ","),
    Diff = "---"
  )

  rbind(panel_dt, n_fac, n_fac_yr)
}

panel_full <- make_panel(pre_full, "Full Sample")
panel_mm   <- make_panel(pre_mm,   "MM Sample")

cat(sprintf("Full sample rows: %d | MM sample rows: %d\n",
  nrow(panel_full), nrow(panel_mm)))

# ── 4. Combine into wide table ────────────────────────────────────────────────
# Side-by-side: Variable | Full TX | Full CTL | Full Diff | MM TX | MM CTL | MM Diff
stopifnot(identical(panel_full$Variable, panel_mm$Variable))

combined <- data.table(
  Variable     = panel_full$Variable,
  Full_TX      = panel_full$TX,
  Full_CTL     = panel_full$CTL,
  Full_Diff    = panel_full$Diff,
  MM_TX        = panel_mm$TX,
  MM_CTL       = panel_mm$CTL,
  MM_Diff      = panel_mm$Diff
)

setnames(combined, c(
  "Variable",
  "TX", "CTL", "Diff.",
  "TX ", "CTL ", "Diff. "
))

cat("\n=== Sample Description Preview ===\n")
print(combined)

# ── 5. Section separators for LaTeX ──────────────────────────────────────────
# We want thin rules between groups of rows.
# Groups: Scale (rows 1-3) | Composition (4-6) | Outcomes (7-8) | Counts (9-10)
# In kableExtra, use pack_rows() for section headers.
n_scale  <- 3L
n_comp   <- 3L
n_out    <- 2L
n_counts <- 2L

# ── 6. Render LaTeX ───────────────────────────────────────────────────────────
desc_tex <- kbl(
  combined,
  format    = "latex",
  booktabs  = TRUE,
  linesep   = "",
  escape    = FALSE,
  align     = c("l","r","r","r","r","r","r"),
  caption   = paste(
    "Sample characteristics: full analysis sample versus make-model comparison sample.",
    "The full sample comprises all analysis-state facilities observed in at least one",
    "pre-reform year (1990--1997); the make-model (MM) sample further restricts to",
    "facilities whose reform-date portfolio was entirely single-walled and gasoline-only.",
    "All means are facility-year averages over 1990--1997 (1998 excluded).",
    "Stock variables (active tanks, capacity, tank age) use the December end-of-year snapshot.",
    "Single-walled share and gasoline-only share are degenerate in the MM sample",
    "by construction (100\\%), confirming the restriction homogenises the comparison",
    "group on the dimensions along which the treatment effect is theoretically heterogeneous.",
    "Diff.\\ $=$ Texas minus control; significance stars from two-sample Welch $t$-tests.",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  label = "tab:descriptive"
) |>
  add_header_above(c(
    " "                   = 1,
    "Full Sample"         = 3,
    "MM Comparison Sample"= 3
  ), escape = FALSE) |>
  pack_rows("Facility Scale",       start_row = 1,
            end_row = n_scale,                         bold = FALSE, italic = TRUE) |>
  pack_rows("Portfolio Composition", start_row = n_scale + 1,
            end_row = n_scale + n_comp,                bold = FALSE, italic = TRUE) |>
  pack_rows("Pre-Reform Outcomes",   start_row = n_scale + n_comp + 1,
            end_row = n_scale + n_comp + n_out,        bold = FALSE, italic = TRUE) |>
  pack_rows("Sample Size",           start_row = n_scale + n_comp + n_out + 1,
            end_row = n_scale + n_comp + n_out + n_counts,
                                                       bold = FALSE, italic = TRUE) |>
  kable_styling(latex_options = "scale_down", font_size = 10)

# ── 7. Save ───────────────────────────────────────────────────────────────────
save_table(combined, "Table0_Descriptive")
write_tex(desc_tex, "Table0_Descriptive")

cat("\n=== 01n COMPLETE -> Table0_Descriptive.tex ===\n")
#==============================================================================
# 01m_BalanceTable.R
# Table 1: Progressive Balance — Naive → MM Pooled → MM Cell-Weighted
#
# The table has exactly THREE comparison rows, showing how the TX/CTL gap
# shrinks as the identification strategy tightens:
#
#   Row 1: Full Sample (Naive)
#          Raw TX vs CTL across all analysis facilities.
#          Equivalent to panel_id + year FE only. The benchmark.
#
#   Row 2: MM Sample (Pooled)
#          Raw TX vs CTL restricted to overlap-cell facilities
#          (All-SW, Gasoline-Only, both TX and CTL present in cell).
#          Shows whether sample restriction alone moves the gap.
#
#   Row 3: MM Sample (Cell-Weighted)
#          Cell-size-weighted mean of within-cell TX and CTL rates.
#          This is what make_model_fac^panel_year actually absorbs.
#          Shrinkage from Row 1 to Row 3 = composition removed by design.
#
# Columns: Comparison | N TX | N CTL | Closure TX | Closure CTL | Diff
#                                    | Leak TX    | Leak CTL    | Diff
#
# Individual cell-level diagnostics are saved separately as a CSV
# (Table1_Balance_CellDiag.csv) for referee appendix use if needed.
#==============================================================================

source(here::here("Code", "Analysis", "Descrptive Facts", "01a_Setup.R"))
cat("=== 01m: PROGRESSIVE BALANCE TABLE ===\n")

# ── 0. Load ───────────────────────────────────────────────────────────────────
annual_data <- load_interim("annual_data")

PRE_START <- 1990L
PRE_END   <- 1997L

# ── 1. Three samples ──────────────────────────────────────────────────────────

# Row 1: full analysis sample pre-period
pre_full <- annual_data[panel_year %between% c(PRE_START, PRE_END)]

# Row 2 & 3: MM slice → overlap restriction
pre_mm <- annual_data[
  fac_wall == "All-SW"        &
  fac_fuel == "Gasoline-Only" &
  !is.na(make_model_fac)      &
  !is.na(fac_oldest_age_bin)  &
  panel_year %between% c(PRE_START, PRE_END)
]

cell_support  <- pre_mm[, .(
  has_tx  = any(texas_treated == 1),
  has_ctl = any(texas_treated == 0)
), by = make_model_fac]

overlap_cells <- cell_support[has_tx == TRUE & has_ctl == TRUE, make_model_fac]
n_tx_only     <- cell_support[has_tx == TRUE  & has_ctl == FALSE, .N]
n_ctl_only    <- cell_support[has_tx == FALSE & has_ctl == TRUE,  .N]
pre_ov        <- pre_mm[make_model_fac %in% overlap_cells]

cat(sprintf("Row 1 (Naive):       %s fac | %s TX | %s CTL\n",
  format(uniqueN(pre_full$panel_id),             big.mark = ","),
  format(pre_full[texas_treated==1, uniqueN(panel_id)], big.mark = ","),
  format(pre_full[texas_treated==0, uniqueN(panel_id)], big.mark = ",")))
cat(sprintf("Row 2/3 (MM overlap):%s fac | %s TX | %s CTL | %d cells\n",
  format(uniqueN(pre_ov$panel_id),               big.mark = ","),
  format(pre_ov[texas_treated==1, uniqueN(panel_id)],   big.mark = ","),
  format(pre_ov[texas_treated==0, uniqueN(panel_id)],   big.mark = ","),
  length(overlap_cells)))
cat(sprintf("  Cells dropped (no overlap): %d TX-only, %d CTL-only\n",
  n_tx_only, n_ctl_only))

# ── 2. Per-cell stats (for cell-weighted row and diagnostic CSV) ──────────────
age_levels <- c("0-2yr","3-5yr","6-8yr","9-11yr","12-14yr","15-19yr","20yr-Plus")

cell_stats <- pre_ov[, .(
  n_tx             = uniqueN(panel_id[texas_treated == 1]),
  n_ctl            = uniqueN(panel_id[texas_treated == 0]),
  closure_TX       = mean(closure_event[texas_treated == 1], na.rm = TRUE) * 100,
  closure_CTL      = mean(closure_event[texas_treated == 0], na.rm = TRUE) * 100,
  leak_TX          = mean((n_leaks > 0)[texas_treated == 1], na.rm = TRUE) * 100,
  leak_CTL         = mean((n_leaks > 0)[texas_treated == 0], na.rm = TRUE) * 100
), by = .(make_model_fac, fac_oldest_age_bin)]

cell_stats[, `:=`(
  closure_diff = closure_TX - closure_CTL,
  leak_diff    = leak_TX    - leak_CTL,
  age_ord      = factor(fac_oldest_age_bin, levels = age_levels)
)]
setorder(cell_stats, age_ord)

# Save cell-level detail as diagnostic CSV (not printed in main table)
save_table(cell_stats, "Table1_Balance_CellDiag")
cat(sprintf("  Cell diagnostics saved: Table1_Balance_CellDiag.csv\n"))

# ── 3. Build three summary rows ───────────────────────────────────────────────
w <- cell_stats$n_tx + cell_stats$n_ctl

summary_rows <- rbind(

  # Row 1: Naive
  data.table(
    Comparison   = "Full Sample (Naive)",
    n_tx         = pre_full[texas_treated == 1, uniqueN(panel_id)],
    n_ctl        = pre_full[texas_treated == 0, uniqueN(panel_id)],
    closure_TX   = pre_full[texas_treated == 1, mean(closure_event, na.rm=TRUE)*100],
    closure_CTL  = pre_full[texas_treated == 0, mean(closure_event, na.rm=TRUE)*100],
    leak_TX      = pre_full[texas_treated == 1, mean(n_leaks > 0,   na.rm=TRUE)*100],
    leak_CTL     = pre_full[texas_treated == 0, mean(n_leaks > 0,   na.rm=TRUE)*100]
  ),

  # Row 2: MM Pooled
  data.table(
    Comparison   = "MM Sample (Pooled)",
    n_tx         = pre_ov[texas_treated == 1, uniqueN(panel_id)],
    n_ctl        = pre_ov[texas_treated == 0, uniqueN(panel_id)],
    closure_TX   = pre_ov[texas_treated == 1, mean(closure_event, na.rm=TRUE)*100],
    closure_CTL  = pre_ov[texas_treated == 0, mean(closure_event, na.rm=TRUE)*100],
    leak_TX      = pre_ov[texas_treated == 1, mean(n_leaks > 0,   na.rm=TRUE)*100],
    leak_CTL     = pre_ov[texas_treated == 0, mean(n_leaks > 0,   na.rm=TRUE)*100]
  ),

  # Row 3: MM Cell-Weighted
  data.table(
    Comparison   = "MM Sample (Cell-Weighted)",
    n_tx         = sum(cell_stats$n_tx),
    n_ctl        = sum(cell_stats$n_ctl),
    closure_TX   = weighted.mean(cell_stats$closure_TX,  w = w, na.rm = TRUE),
    closure_CTL  = weighted.mean(cell_stats$closure_CTL, w = w, na.rm = TRUE),
    leak_TX      = weighted.mean(cell_stats$leak_TX,     w = w, na.rm = TRUE),
    leak_CTL     = weighted.mean(cell_stats$leak_CTL,    w = w, na.rm = TRUE)
  )
)

summary_rows[, `:=`(
  closure_diff = closure_TX - closure_CTL,
  leak_diff    = leak_TX    - leak_CTL
)]

# ── 4. Format ─────────────────────────────────────────────────────────────────
fmt_n    <- function(x) formatC(as.integer(x), format = "d", big.mark = ",")
fmt_rate <- function(x) sprintf("%.2f", x)
fmt_diff <- function(x) fifelse(x > 0, sprintf("+%.2f", x), sprintf("%.2f", x))

print_dt <- summary_rows[, .(
  Comparison   = Comparison,
  `$N_{TX}$`  = fmt_n(n_tx),
  `$N_{CTL}$` = fmt_n(n_ctl),
  `TX`          = fmt_rate(closure_TX),
  `CTL`         = fmt_rate(closure_CTL),
  `Diff.`       = fmt_diff(closure_diff),
  `TX `         = fmt_rate(leak_TX),
  `CTL `        = fmt_rate(leak_CTL),
  `Diff. `      = fmt_diff(leak_diff)
)]

cat("\n=== Balance Table ===\n")
print(print_dt)
cat(sprintf("\nClosure gap shrinkage: Naive %+.2fpp → Pooled %+.2fpp → Cell-Wt %+.2fpp\n",
  summary_rows[1, closure_diff],
  summary_rows[2, closure_diff],
  summary_rows[3, closure_diff]))
cat(sprintf("Leak gap shrinkage:    Naive %+.2fpp → Pooled %+.2fpp → Cell-Wt %+.2fpp\n",
  summary_rows[1, leak_diff],
  summary_rows[2, leak_diff],
  summary_rows[3, leak_diff]))

# ── 5. Render LaTeX ───────────────────────────────────────────────────────────
overlap_note <- if (n_tx_only + n_ctl_only > 0)
  sprintf(
    "%d cell(s) lacking common support (TX-only: %d, CTL-only: %d) are excluded from MM rows.",
    n_tx_only + n_ctl_only, n_tx_only, n_ctl_only)
  else
    "All MM cells have common support."

balance_tex <- kbl(
  print_dt,
  format    = "latex",
  booktabs  = TRUE,
  linesep   = "",
  escape    = FALSE,
  align     = c("l","r","r","r","r","r","r","r","r"),
  caption   = paste(
    "Progressive balance: naive comparison, MM pooled, and MM cell-weighted.",
    "Each row tightens the comparison group;",
    "shrinkage in the Diff.\\ columns measures how much of the raw",
    "Texas versus control gap is attributable to compositional sorting",
    "that the make-model design removes.",
    "\\textit{Full Sample (Naive)} is the unconditional pre-reform difference",
    "across all analysis facilities, equivalent to a specification with",
    "\\texttt{panel\\_id + year} fixed effects only.",
    "\\textit{MM Sample (Pooled)} restricts to facilities in the",
    "All-SW, Gasoline-Only sub-sample with common cell support",
    "(both Texas and control-state facilities present in the same age-bin cell)",
    "but still computes a raw pooled TX vs.\\ CTL mean.",
    "\\textit{MM Sample (Cell-Weighted)} is the cell-size-weighted mean of",
    "within-cell Texas and control rates across all",
    sprintf("%d", length(overlap_cells)),
    "identified cells --- the quantity directly absorbed by the",
    "\\texttt{make\\_model\\_fac\\^{}panel\\_year} fixed effects in equation~(1).",
    "$N$ counts are unique facilities observed in at least one pre-reform year",
    "(1990--1997; 1998 excluded).",
    "Cell-level diagnostics are reported in Table~\\ref{tab:balance_cells}",
    "of the online appendix.",
    overlap_note
  ),
  label = "tab:balance"
) |>
  add_header_above(c(
    " "                  = 3,
    "Closure Rate (\\%)" = 3,
    "Leak Rate (\\%)"    = 3
  ), escape = FALSE) |>
  row_spec(1, italic = TRUE)  |>   # Naive row — italic to signal it's the benchmark
  row_spec(3, bold   = TRUE)  |>   # Cell-Weighted row — bold as primary result
  kable_styling(latex_options = "scale_down", font_size = 10)

# ── 6. Save ───────────────────────────────────────────────────────────────────
save_table(print_dt, "Table1_Balance")
write_tex(balance_tex, "Table1_Balance")

cat(sprintf("\n=== 01m COMPLETE -> Table1_Balance.tex (3 rows) ===\n"))
cat(sprintf("    Cell-level detail -> Table1_Balance_CellDiag.csv\n"))
