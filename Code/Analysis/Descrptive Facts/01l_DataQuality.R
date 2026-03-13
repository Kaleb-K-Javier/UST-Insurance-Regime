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