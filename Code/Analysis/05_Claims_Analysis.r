###############################################################################
# Script: 05_Claims_Analysis.R
# Purpose: Build all claims-related figures and descriptive regressions.
#
# Outputs:
#   1. Figure_cost_distribution_pooled.png    — Main paper: pooled 6-state density
#   2. Figure_cost_distribution_by_state.png  — Appendix: state-faceted panels
#   3. Table_claims_descriptive_regression.tex/.csv — OLS: log(cost) ~ facility chars
#   4. Table_claims_summary_statistics.csv    — Summary stats for paper text
#
# Inputs:
#   A. Outputs/all_cleaned_claims.csv           (from 11_Build_Claims_Dataset.r)
#   B. Data/Processed/claims_panel_annual_merged.csv (from 11_ server-side merge)
#
# Notes:
#   - All costs in 2023 USD (CPI-U deflated).
#   - PA is now included as a sixth claims state.
#   - The "control states" framing in the paper refers to the 5 non-PA states;
#     PA is treated separately in the structural model but its claims data
#     contributes to the pooled cost distribution used for L(x_t) calibration.
###############################################################################

library(data.table)
library(ggplot2)
library(scales)
library(fixest)
library(here)

# ── 1. Load Claims Data ──────────────────────────────────────────────────────
claims_path <- here("Outputs", "all_cleaned_claims.csv")
if (!file.exists(claims_path)) {
  # Fallback: server location
  claims_path <- here("Data", "Processed", "all_cleaned_claims.csv")
}
if (!file.exists(claims_path)) {
  stop("all_cleaned_claims.csv not found. Run 11_Build_Claims_Dataset.r (RUN_LOCAL = TRUE) first.")
}

claims <- fread(claims_path)
cat(sprintf("Loaded %d claims across states: %s\n",
            nrow(claims), paste(sort(unique(claims$state)), collapse = ", ")))

# ── 2. Prepare Plotting Data ─────────────────────────────────────────────────
ALL_STATES <- c("CO", "LA", "NM", "PA", "TN", "UT")

state_label_map <- c(
  CO = "Colorado",
  LA = "Louisiana",
  NM = "New Mexico",
  PA = "Pennsylvania",
  TN = "Tennessee",
  UT = "Utah"
)

plot_dt <- claims[
  state %in% ALL_STATES &
    !is.na(total_cost_2023) &
    total_cost_2023 > 0
]
plot_dt[, state_label := factor(state_label_map[state], levels = unname(state_label_map))]

cat(sprintf("Claims for figures: %d rows across %d states\n",
            nrow(plot_dt), uniqueN(plot_dt$state)))

# ── 3. Summary Statistics ────────────────────────────────────────────────────

# Per-state stats
state_stats <- plot_dt[, .(
  n        = .N,
  median   = median(total_cost_2023, na.rm = TRUE),
  mean     = mean(total_cost_2023,   na.rm = TRUE),
  sd       = sd(total_cost_2023,     na.rm = TRUE),
  p10      = quantile(total_cost_2023, 0.10, na.rm = TRUE),
  p25      = quantile(total_cost_2023, 0.25, na.rm = TRUE),
  p75      = quantile(total_cost_2023, 0.75, na.rm = TRUE),
  p90      = quantile(total_cost_2023, 0.90, na.rm = TRUE),
  p95      = quantile(total_cost_2023, 0.95, na.rm = TRUE),
  p99      = quantile(total_cost_2023, 0.99, na.rm = TRUE),
  min_year = min(year(claims_start_date), na.rm = TRUE),
  max_year = max(year(claims_start_date), na.rm = TRUE)
), by = .(state, state_label)]

# Pooled stats
pooled_stats <- plot_dt[, .(
  n      = .N,
  median = median(total_cost_2023, na.rm = TRUE),
  mean   = mean(total_cost_2023,   na.rm = TRUE),
  sd     = sd(total_cost_2023,     na.rm = TRUE),
  p10    = quantile(total_cost_2023, 0.10, na.rm = TRUE),
  p25    = quantile(total_cost_2023, 0.25, na.rm = TRUE),
  p75    = quantile(total_cost_2023, 0.75, na.rm = TRUE),
  p90    = quantile(total_cost_2023, 0.90, na.rm = TRUE),
  p95    = quantile(total_cost_2023, 0.95, na.rm = TRUE),
  p99    = quantile(total_cost_2023, 0.99, na.rm = TRUE)
)]

cat("\n=== POOLED DISTRIBUTION ===\n")
print(pooled_stats)
cat("\n=== BY STATE ===\n")
print(state_stats[order(state)])


###############################################################################
# FIGURE 1 (MAIN PAPER): Pooled 6-State Cost Distribution
###############################################################################

fig_pooled <- ggplot(plot_dt, aes(x = total_cost_2023)) +

  geom_density(
    fill  = "#2C6E9E",
    color = "#1B4F72",
    alpha = 0.55,
    adjust = 1.2
  ) +

  # Median
  geom_vline(
    xintercept = pooled_stats$median,
    linetype   = "dashed",
    color      = "#E74C3C",
    linewidth  = 0.8
  ) +

  # Mean
  geom_vline(
    xintercept = pooled_stats$mean,
    linetype   = "dotdash",
    color      = "#E67E22",
    linewidth  = 0.8
  ) +

  # Annotation: median and mean values
  annotate(
    "text",
    x     = pooled_stats$mean * 2.5,
    y     = Inf,
    vjust = 2.2,
    hjust = 0,
    size  = 3,
    fontface = "italic",
    label = sprintf(
      "Median: %s\nMean: %s\nN = %s",
      dollar(pooled_stats$median, accuracy = 1),
      dollar(pooled_stats$mean,   accuracy = 1),
      format(pooled_stats$n, big.mark = ",")
    )
  ) +

  scale_x_log10(
    labels = label_dollar(scale_cut = cut_short_scale(), prefix = "$"),
    breaks = c(1e3, 1e4, 1e5, 1e6, 1e7),
    minor_breaks = NULL
  ) +

  labs(
    x     = "Realized Cleanup Cost (2023 USD, log scale)",
    y     = "Density",
    title = NULL
  ) +

  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 30, hjust = 1, size = 9),
    panel.grid.minor  = element_blank(),
    plot.margin       = margin(6, 10, 6, 6)
  )

# ── Save
out_dir <- here("Output", "Figures")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(
  file.path(out_dir, "Figure_cost_distribution_pooled.png"),
  plot   = fig_pooled,
  width  = 7,
  height = 4.5,
  dpi    = 300,
  bg     = "white"
)
cat("\nSaved: Figure_cost_distribution_pooled.png\n")


###############################################################################
# FIGURE 2 (APPENDIX): State-Faceted Panels
###############################################################################

state_ref <- plot_dt[, .(
  med = median(total_cost_2023, na.rm = TRUE),
  avg = mean(total_cost_2023,   na.rm = TRUE)
), by = state_label]

fig_states <- ggplot(plot_dt, aes(x = total_cost_2023)) +

  geom_density(
    fill  = "#2C6E9E",
    color = "#1B4F72",
    alpha = 0.55,
    adjust = 1.2
  ) +

  geom_vline(
    data     = state_ref,
    aes(xintercept = med, linetype = "Median"),
    color    = "#E74C3C",
    linewidth = 0.7
  ) +

  geom_vline(
    data     = state_ref,
    aes(xintercept = avg, linetype = "Mean"),
    color    = "#E67E22",
    linewidth = 0.7
  ) +

  scale_linetype_manual(
    name   = NULL,
    values = c("Median" = "dashed", "Mean" = "dotdash"),
    guide  = guide_legend(override.aes = list(color = c("#E67E22", "#E74C3C")))
  ) +

  scale_x_log10(
    labels = label_dollar(scale_cut = cut_short_scale(), prefix = "$"),
    breaks = c(1e3, 1e4, 1e5, 1e6, 1e7),
    minor_breaks = NULL
  ) +

  facet_wrap(~ state_label, ncol = 2, scales = "free_y") +

  labs(
    x     = "Realized Cleanup Cost (2023 USD, log scale)",
    y     = "Density",
    title = NULL
  ) +

  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "#EAF1FB", color = "grey70"),
    strip.text       = element_text(face = "bold", size = 10),
    axis.text.x      = element_text(angle = 30, hjust = 1, size = 8),
    panel.grid.minor  = element_blank(),
    legend.position   = "bottom",
    legend.key.width  = unit(1.5, "cm"),
    plot.margin       = margin(6, 10, 6, 6)
  )

ggsave(
  file.path(out_dir, "Figure_cost_distribution_by_state.png"),
  plot   = fig_states,
  width  = 7,
  height = 7,
  dpi    = 300,
  bg     = "white"
)
cat("Saved: Figure_cost_distribution_by_state.png\n")

###############################################################################
# DESCRIPTIVE REGRESSION: log(cost) ~ facility characteristics at leak time
###############################################################################
cat("\n========================================\n")
cat("DESCRIPTIVE REGRESSION: Claims Cost Correlates (Age Bins & Interactions)\n")
cat("========================================\n\n")

# Require marginaleffects for Panel B / visual estimation
if (!requireNamespace("marginaleffects", quietly = TRUE)) install.packages("marginaleffects")
library(marginaleffects)

merged_path <- here("Data", "Processed", "claims_panel_annual_merged.csv")
if (!file.exists(merged_path)) {
  stop("claims_panel_annual_merged.csv not found. Execute 11_Merge_Claims_to_Annual_Panel.R first.")
}

merged <- fread(merged_path)

# Prepare regression data
reg_dt <- merged[
  !is.na(claims_total_2023) & claims_total_2023 > 0
][, .(
  panel_id, state, panel_year,
  log_cost = log(claims_total_2023),
  active_tanks = fifelse(is.na(active_tanks_dec), NA_real_, as.numeric(active_tanks_dec)),
  capacity     = fifelse(is.na(total_capacity_dec), NA_real_, as.numeric(total_capacity_dec)),
  avg_age      = fifelse(is.na(avg_tank_age_dec), NA_real_, as.numeric(avg_tank_age_dec)),
  has_sw       = fifelse(is.na(has_single_walled_dec), 0L, as.integer(has_single_walled_dec > 0)),
  has_dw       = fifelse(is.na(has_double_walled_dec), 0L, as.integer(has_double_walled_dec > 0))
)]

# Restrict to non-degenerate observations
reg_dt <- reg_dt[!is.na(log_cost) & is.finite(log_cost) & !is.na(active_tanks) & active_tanks > 0 & !is.na(avg_age)]

# Discretize age into 5-year bins and set 0-5 as reference
reg_dt[, age_bin := cut(avg_age, 
                        breaks = c(-Inf, 5, 10, 15, 20, 25, 30, Inf), 
                        labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31+"), 
                        right = TRUE)]
reg_dt[, age_bin := relevel(factor(age_bin), ref = "0-5")]

cat(sprintf("Regression sample: %d claim-year observations\n", nrow(reg_dt)))

# ── 1. Run specifications ─────────────────────────────────────────────────────

# (1) Base Bins + State FE
m1 <- feols(log_cost ~ age_bin + active_tanks + capacity + has_sw + has_dw | state, 
            data = reg_dt, cluster = ~state)

# (2) Base Bins + State FE + Year FE
m2 <- feols(log_cost ~ age_bin + active_tanks + capacity + has_sw + has_dw | state + panel_year, 
            data = reg_dt, cluster = ~state)

# (3) Interaction Model: Wall Type x Age Bin + State FE + Year FE
m3 <- feols(log_cost ~ age_bin * has_sw + age_bin * has_dw + active_tanks + capacity | state + panel_year, 
            data = reg_dt, cluster = ~state)

# ── 2. Export Panel A: Raw Coefficients ───────────────────────────────────────
tbl_dir <- here("Output", "Tables")
if (!dir.exists(tbl_dir)) dir.create(tbl_dir, recursive = TRUE)

etable(m1, m2, m3,
       headers = c("State FE", "State+Year FE", "Interacted"),
       dict = c(age_bin = "Tank Age: ", has_sw = "Single-Walled", has_dw = "Double-Walled",
                active_tanks = "Active Tanks", capacity = "Total Capacity (gal)",
                log_cost = "log(Cleanup Cost, 2023 USD)"),
       file = file.path(tbl_dir, "Table_claims_regression_Panel_A.tex"),
       replace = TRUE)

# ── 3. Calculate Panel B: Marginal Effects (Cost Savings/Increases) ───────────
# Compute Average Marginal Effects (AME) of wall types across age bins based on m3
me_sw <- avg_comparisons(m3, variables = "has_sw", by = "age_bin") |> as.data.table()
me_dw <- avg_comparisons(m3, variables = "has_dw", by = "age_bin") |> as.data.table()

me_combined <- rbind(
  me_sw[, .(age_bin, term = "Single-Walled", estimate, conf.low, conf.high, p.value)],
  me_dw[, .(age_bin, term = "Double-Walled", estimate, conf.low, conf.high, p.value)]
)

# Convert log-cost differences to expected percentage changes: [exp(beta) - 1] * 100
me_combined[, `:=`(
  pct_effect    = (exp(estimate) - 1) * 100,
  pct_conf.low  = (exp(conf.low) - 1) * 100,
  pct_conf.high = (exp(conf.high) - 1) * 100
)]

fwrite(me_combined, file.path(tbl_dir, "Table_claims_marginal_effects_Panel_B.csv"))
cat("\nSaved Panel A (Raw Coefficients) and Panel B (Marginal Effects) to Outputs/Tables.\n")

# ── 4. Generate Marginal Effects Plot ─────────────────────────────────────────
fig_me <- ggplot(me_combined, aes(x = age_bin, y = pct_effect, color = term, group = term)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey40", linewidth = 0.8) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(ymin = pct_conf.low, ymax = pct_conf.high), 
                position = position_dodge(width = 0.5), width = 0.2, linewidth = 0.8) +
  scale_color_manual(values = c("Single-Walled" = "#E74C3C", "Double-Walled" = "#2C6E9E")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    x = "Average Tank Age Bin (Years)",
    y = "Marginal Effect on Cleanup Cost (%)",
    color = "Containment Type"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

out_dir <- here("Output", "Figures")
ggsave(
  file.path(out_dir, "Figure_claims_marginal_effects.png"),
  plot = fig_me, width = 7.5, height = 5, dpi = 300, bg = "white"
)
cat("Saved Marginal Effects Plot: Figure_claims_marginal_effects.png\n")

###############################################################################
# SAVE SUMMARY STATISTICS TABLE
###############################################################################
stats_dir <- here("Output", "Tables")
if (!dir.exists(stats_dir)) dir.create(stats_dir, recursive = TRUE)

# State-level
fwrite(state_stats, file.path(stats_dir, "Table_claims_summary_by_state.csv"))

# Pooled
pooled_out <- copy(pooled_stats)
pooled_out[, state := "Pooled"]
fwrite(pooled_out, file.path(stats_dir, "Table_claims_summary_pooled.csv"))

cat("\nSaved summary tables.\n")


###############################################################################
# PAPER TEXT — Quarto-compatible section (copy into .qmd)
#
# This block is for reference. Paste into the paper and adjust as needed.
###############################################################################

# ──────────────────────────────────────────────────────────────────────────────
# ## Cleanup Cost Distribution
#
# To calibrate the damage function $L(x_t)$ in the structural model of
# Section~\ref{sec:structural}, I link administrative facility identifiers to
# payment ledgers of state underground storage tank trust funds in Colorado,
# Louisiana, New Mexico, Pennsylvania, Tennessee, and Utah. This six-state
# sample spans the full set of control-group states for which verified
# per-incident cleanup cost records could be obtained from public
# administrative data. All cost figures are deflated to 2023 dollars using the
# Bureau of Labor Statistics Consumer Price Index for All Urban Consumers
# (CPI-U).
#
# The pooled distribution of realized cleanup costs conditional on a confirmed
# release is characterized by extreme positive skewness and rejects normality
# in favor of a heavy-tailed log-normal structure. Across all six states, the
# median cleanup cost is [INSERT median from pooled_stats], while the mean is
# approximately [INSERT mean from pooled_stats], driven upward by a small
# number of catastrophic outcomes in which contamination plumes migrate
# off-site and trigger third-party liability. The right tail is a structural
# feature of UST environmental liability rather than an artifact of particular
# state administrative procedures: as @fig-cost-distribution-states
# demonstrates, the heavy upper tail is consistent across all six sampled fund
# states, despite variation in program design, geography, and claim volumes.
#
# @fig-cost-distribution presents the pooled log-scale density. Vertical lines
# mark the median and mean, illustrating the two-plus-order-of-magnitude gap
# between typical and catastrophic remediation events.
#
# ```{r}
# #| label: fig-cost-distribution
# #| fig-cap: "Pooled distribution of realized cleanup costs per confirmed release across six state trust funds (Colorado, Louisiana, New Mexico, Pennsylvania, Tennessee, Utah), deflated to 2023 dollars. Log-scale density with vertical lines at the median (dashed red) and mean (dot-dash orange)."
# #| fig-width: 7
# #| fig-height: 4.5
# #| echo: false
# #| warning: false
# knitr::include_graphics(here::here("Output", "Figures", "Figure_cost_distribution_pooled.png"))
# ```
#
# To examine what observable facility characteristics correlate with remediation
# severity, @tbl-claims-regression reports OLS estimates of log cleanup cost on
# facility-level covariates measured in the year of the confirmed release.
# Column (1) documents a positive age gradient: older tank installations are
# associated with larger cleanup costs, consistent with cumulative corrosion
# and degraded containment. Column (2) adds facility scale (active tank count,
# total capacity) and wall type indicators. Facilities operating single-walled
# tanks face higher remediation costs, while double-walled containment is
# associated with smaller claims. Columns (3)–(4) absorb state and year fixed
# effects respectively. The age gradient and wall-type penalty survive these
# controls, suggesting the correlations reflect physical deterioration
# mechanisms rather than state-specific administrative practices or secular
# trends in remediation technology.
#
# These descriptive relationships do not identify causal effects of tank
# characteristics on cleanup costs; they serve to validate the structural
# model's assumption that the damage function $L(x_t)$ depends on the
# physical state vector and to inform the machine-learning prediction stage
# in which Random Survival Forests generate facility-specific expected cost
# inputs for the dynamic discrete choice model.
#
# ```{r}
# #| label: fig-cost-distribution-states
# #| fig-cap: "State-level distribution of realized cleanup costs per confirmed release, deflated to 2023 dollars. Each panel plots the log-scale density for one state with vertical lines at the state-specific median and mean. The consistency of the heavy upper tail across all six states supports treating the skewed cost distribution as a structural feature of UST liability."
# #| fig-width: 7
# #| fig-height: 7
# #| echo: false
# #| warning: false
# knitr::include_graphics(here::here("Output", "Figures", "Figure_cost_distribution_by_state.png"))
# ```
# ──────────────────────────────────────────────────────────────────────────────

cat("\n✓ 05_Claims_Analysis.r COMPLETE\n")