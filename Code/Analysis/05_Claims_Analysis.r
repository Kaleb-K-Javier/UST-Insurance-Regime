###############################################################################
# Script: 05_Claims_Analysis.R
# Purpose: Build all claims-related figures and descriptive regressions.
#
# Outputs:
#   1. Figure_cost_distribution_pooled     — Main paper: pooled 6-state density
#   2. Figure_cost_distribution_by_state   — Appendix: state-faceted panels
#   3. Table_Claims_Regression_Main.tex    — OLS: 4-Column Panel Regression
#   4. Table_Claims_Marginal_Effects_PanelB — Marginal effects (cost savings)
#   5. Figure_Claims_Marginal_Effects      — Accompanying marginal effects plot
#   6. Table_claims_summary_*.csv          — Summary stats for paper text
#   7. Table_Claims_Count_Decomposition    — Appendix: N observations per category
###############################################################################

library(data.table)
library(ggplot2)
library(scales)
library(fixest)
library(marginaleffects)
library(here)

cat("=================================================================\n")
cat("05_Claims_Analysis.R — Cleanup Cost Distributions & Correlates\n")
cat("=================================================================\n\n")

# ── 1. Formatting Helpers ────────────────────────────────────────────────────
theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(size = rel(1.1), face = "bold", margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_text(size = rel(0.85), margin = margin(0, 0, 10, 0), color = "gray30"),
      axis.title    = element_text(face = "bold", size = rel(0.9)),
      legend.title  = element_text(face = "bold", size = rel(0.9)),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.border  = element_rect(fill = NA, color = "gray85"),
      strip.text    = element_text(face = "bold")
    )
}
theme_set(theme_pub())

save_fig <- function(plot_obj, filename, width = 10, height = 6, dpi = 300) {
  out_dir <- here("Output", "Figures")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(out_dir, paste0(filename, ".png")), plot_obj, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(file.path(out_dir, paste0(filename, ".pdf")), plot_obj, width = width, height = height, device = grDevices::cairo_pdf)
  cat(sprintf("✓ Saved Figure: %s (.png + .pdf)\n", filename))
}

# ── 2. Load Raw Claims (For Distribution Density) ────────────────────────────
claims_path <- here("Data", "Processed", "all_cleaned_claims.csv")
if (!file.exists(claims_path)) stop("all_cleaned_claims.csv not found.")

claims <- fread(claims_path)

ALL_STATES <- c("CO", "LA", "NM", "PA", "TN", "UT")
state_label_map <- c(
  CO = "Colorado", LA = "Louisiana", NM = "New Mexico",
  PA = "Pennsylvania", TN = "Tennessee", UT = "Utah"
)

plot_dt <- claims[state %in% ALL_STATES & !is.na(total_cost_2023) & total_cost_2023 > 0]
plot_dt[, state_label := factor(state_label_map[state], levels = unname(state_label_map))]

cat(sprintf("Claims for figures: %s rows across %d states\n", 
            format(nrow(plot_dt), big.mark=","), uniqueN(plot_dt$state)))

# ── 3. Summary Statistics ────────────────────────────────────────────────────
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
  p99      = quantile(total_cost_2023, 0.99, na.rm = TRUE)
), by = .(state, state_label)]

pooled_stats <- plot_dt[, .(
  n        = .N,
  median   = median(total_cost_2023, na.rm = TRUE),
  mean     = mean(total_cost_2023,   na.rm = TRUE),
  sd       = sd(total_cost_2023,     na.rm = TRUE),
  p10      = quantile(total_cost_2023, 0.10, na.rm = TRUE),
  p25      = quantile(total_cost_2023, 0.25, na.rm = TRUE),
  p75      = quantile(total_cost_2023, 0.75, na.rm = TRUE),
  p90      = quantile(total_cost_2023, 0.90, na.rm = TRUE),
  p95      = quantile(total_cost_2023, 0.95, na.rm = TRUE),
  p99      = quantile(total_cost_2023, 0.99, na.rm = TRUE)
)]

stats_dir <- here("Output", "Tables")
dir.create(stats_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(state_stats, file.path(stats_dir, "Table_claims_summary_by_state.csv"))
pooled_out <- copy(pooled_stats)
pooled_out[, state := "Pooled"]
fwrite(pooled_out, file.path(stats_dir, "Table_claims_summary_pooled.csv"))

# ── 4. Figure 1: Pooled Cost Distribution ────────────────────────────────────
fig_pooled <- ggplot(plot_dt, aes(x = total_cost_2023)) +
  geom_density(fill = "#0072B2", color = "#004D7A", alpha = 0.55, adjust = 1.2) +
  geom_vline(xintercept = pooled_stats$median, linetype = "dashed", color = "#D55E00", linewidth = 0.8) +
  geom_vline(xintercept = pooled_stats$mean, linetype = "dotdash", color = "#E69F00", linewidth = 0.8) +
  annotate("text", x = pooled_stats$mean * 2.5, y = Inf, vjust = 2.2, hjust = 0, size = 3.5, fontface = "italic",
           label = sprintf("Median: %s\nMean: %s\nN = %s",
                           dollar(pooled_stats$median, accuracy = 1),
                           dollar(pooled_stats$mean,   accuracy = 1),
                           format(pooled_stats$n, big.mark = ","))) +
  scale_x_log10(labels = label_dollar(scale_cut = cut_short_scale(), prefix = "$"),
                breaks = c(1e3, 1e4, 1e5, 1e6, 1e7), minor_breaks = NULL) +
  labs(x = "Realized Cleanup Cost (2023 USD, log scale)", y = "Density", title = "Pooled Cleanup Cost Distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_fig(fig_pooled, "Figure_cost_distribution_pooled", width = 8, height = 5)

# ── 5. Figure 2: State-Faceted Panels (With Observation Counts) ──────────────
state_ref <- plot_dt[, .(med = median(total_cost_2023, na.rm = TRUE),
                         avg = mean(total_cost_2023,   na.rm = TRUE),
                         n   = .N), by = state_label]

state_ref[, facet_label := sprintf("%s (N: %s)", state_label, format(n, big.mark = ","))]
plot_dt <- merge(plot_dt, state_ref[, .(state_label, facet_label)], by = "state_label")

ordered_labels <- state_ref[order(state_label)]$facet_label
plot_dt[, facet_label := factor(facet_label, levels = ordered_labels)]
state_ref[, facet_label := factor(facet_label, levels = ordered_labels)]

fig_states <- ggplot(plot_dt, aes(x = total_cost_2023)) +
  geom_density(fill = "#0072B2", color = "#004D7A", alpha = 0.55, adjust = 1.2) +
  geom_vline(data = state_ref, aes(xintercept = med, linetype = "Median"), color = "#D55E00", linewidth = 0.7) +
  geom_vline(data = state_ref, aes(xintercept = avg, linetype = "Mean"), color = "#E69F00", linewidth = 0.7) +
  scale_linetype_manual(name = NULL, values = c("Median" = "dashed", "Mean" = "dotdash"),
                        guide = guide_legend(override.aes = list(color = c("#E69F00", "#D55E00")))) +
  scale_x_log10(labels = label_dollar(scale_cut = cut_short_scale(), prefix = "$"),
                breaks = c(1e3, 1e4, 1e5, 1e6, 1e7), minor_breaks = NULL) +
  facet_wrap(~ facet_label, ncol = 2, scales = "free_y") +
  labs(x = "Realized Cleanup Cost (2023 USD, log scale)", y = "Density", title = "Cleanup Cost Distribution by State") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "bottom")

save_fig(fig_states, "Figure_cost_distribution_by_state", width = 8, height = 7)

# ── 6. Descriptive Regressions (Merged Panel) ────────────────────────────────
cat("\n--- Running Descriptive Correlates ---\n")
merged_path <- here("Data", "Processed", "claims_panel_annual_merged.csv")
if (!file.exists(merged_path)) stop("claims_panel_annual_merged.csv missing.")

reg_dt <- fread(merged_path)

# Restrict to valid positive claims using native panel aliases
reg_dt <- reg_dt[
  !is.na(claims_total_2023) & claims_total_2023 > 0 & 
  !is.na(active_tanks) & active_tanks > 0 & 
  !is.na(age_bins)
]

reg_dt[, log_cost := log(claims_total_2023)]

# Fix factor levels for age bins to ensure chronological plotting
bin_levels <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35+")
reg_dt[, age_bins := factor(age_bins, levels = bin_levels)]

# Create Single-Walled Share Terciles + NM Missing Control
# Inferred from single_tanks/double_tanks
reg_dt[, sw_share := single_tanks / active_tanks]
reg_dt[, sw_tercile := fcase(
  single_tanks == 0 & double_tanks == 0, "Unknown",
  sw_share <= 0.33, "0-33%",
  sw_share > 0.33 & sw_share <= 0.66, "34-66%",
  sw_share > 0.66, "67-100%"
)]
reg_dt[, sw_tercile := factor(sw_tercile, levels = c("0-33%", "34-66%", "67-100%", "Unknown"))]

# Rescale Total Capacity to units of 1,000 Gallons
reg_dt[, capacity_1k := total_capacity / 1000]

cat(sprintf("Regression sample: %s claim-year observations\n", format(nrow(reg_dt), big.mark=",")))

# ── 6.1 Appendix Table: Claim Counts by Category
age_counts <- reg_dt[, .(Category = "Age Bin", Level = as.character(age_bins), N_Claims = .N), by = age_bins][order(age_bins)][, age_bins := NULL]
sw_counts  <- reg_dt[, .(Category = "SW Share Tercile", Level = as.character(sw_tercile), N_Claims = .N), by = sw_tercile][order(sw_tercile)][, sw_tercile := NULL]
count_decomp <- rbind(age_counts, sw_counts)
fwrite(count_decomp, file.path(stats_dir, "Table_Claims_Count_Decomposition.csv"))
cat("✓ Saved Claim Count Decomposition Table (Appendix)\n")

# ── Main Paper Regression Specifications (4-Column)
# NOTE: Has Double-Walled dropped to resolve multicollinearity with SW Terciles
m1 <- feols(log_cost ~ age_bins, 
            data = reg_dt, cluster = ~state)

m2 <- feols(log_cost ~ age_bins + active_tanks + capacity_1k + sw_tercile, 
            data = reg_dt, cluster = ~state)

m3 <- feols(log_cost ~ age_bins + active_tanks + capacity_1k + sw_tercile | state, 
            data = reg_dt, cluster = ~state)

m4 <- feols(log_cost ~ age_bins + active_tanks + capacity_1k + sw_tercile | state + panel_year, 
            data = reg_dt, cluster = ~state)

etable(m1, m2, m3, m4,
       headers = c("Bivariate", "Controls", "State FE", "State+Year FE"),
       dict = c(age_bins = "Tank Age: ", 
                "sw_tercile34-66%" = "SW Share: 34-66%", 
                "sw_tercile67-100%" = "SW Share: 67-100%",
                "sw_tercileUnknown" = "SW Share: Unknown (NM)",
                active_tanks = "Active Tanks", 
                capacity_1k = "Total Capacity (1,000 gal)", 
                log_cost = "log(Cleanup Cost, 2023 USD)"),
       tex = TRUE, file = file.path(stats_dir, "Table_Claims_Regression_Main.tex"), replace = TRUE)
cat("✓ Saved Table: Table_Claims_Regression_Main.tex\n")

# ── 7. Marginal Effects Calculation & Plot
# Interaction model feeding plot geometry maintaining binary structural factors
m_int <- feols(log_cost ~ age_bins * has_single_walled + age_bins * has_double_walled + active_tanks + capacity_1k | state + panel_year, 
            data = reg_dt, cluster = ~state)

me_sw <- avg_comparisons(m_int, variables = "has_single_walled", by = "age_bins") |> as.data.table()
me_dw <- avg_comparisons(m_int, variables = "has_double_walled", by = "age_bins") |> as.data.table()

me_combined <- rbind(
  me_sw[, .(age_bins, term = "Single-Walled", estimate, conf.low, conf.high, p.value)],
  me_dw[, .(age_bins, term = "Double-Walled", estimate, conf.low, conf.high, p.value)]
)

me_combined[, age_bins := factor(age_bins, levels = bin_levels)]

# Halvorsen-Palmquist transformation
me_combined[, `:=`(pct_effect = (exp(estimate) - 1) * 100,
                   pct_conf.low = (exp(conf.low) - 1) * 100,
                   pct_conf.high = (exp(conf.high) - 1) * 100)]

fwrite(me_combined, file.path(stats_dir, "Table_Claims_Marginal_Effects_PanelB.csv"))

fig_me <- ggplot(me_combined, aes(x = age_bins, y = pct_effect, color = term, group = term)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_line(position = position_dodge(width = 0.5), linewidth = 0.8, alpha = 0.7) +
  geom_errorbar(aes(ymin = pct_conf.low, ymax = pct_conf.high), 
                position = position_dodge(width = 0.5), width = 0.2, linewidth = 0.8) +
  scale_color_manual(values = c("Single-Walled" = "#D55E00", "Double-Walled" = "#0072B2")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Marginal Effect of Containment Type on Cleanup Costs",
       subtitle = "Estimated percentage cost difference relative to mixed/unknown containment within each age cohort",
       x = "Average Tank Age Bin (Years)", y = "Marginal Effect on Cleanup Cost (%)", color = "Containment Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(fig_me, "Figure_Claims_Marginal_Effects_PanelB", width = 8, height = 5)

cat("\n✓ 05_Claims_Analysis.R COMPLETE\n")