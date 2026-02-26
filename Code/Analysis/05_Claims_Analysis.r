###############################################################################
# Script: build_cost_distribution_figure.R
# Purpose: Build Figure — Empirical distribution of realized cleanup costs
#          for the five control-state trust funds (CO, LA, NM, TN, UT).
#          Deflated to 2023 dollars.  Log-scale density faceted by state.
#          Vertical dashed lines mark the state-specific median and mean.
#
# Input:  Outputs/all_cleaned_claims.csv  (produced by 05_Claims_Analysis.r,
#         RUN_LOCAL = TRUE)
# Output: Output/Figures/Figure_cost_distribution.png  (7 × 5 inches, 300 dpi)
#
# Called from: 05_Claims_Analysis.r (after Section H) OR directly from the
#              Quarto paper document.
###############################################################################

library(data.table)
library(ggplot2)
library(scales)
library(here)

# ── 1. Load data ──────────────────────────────────────────────────────────────
claims_path <- here("Outputs", "all_cleaned_claims.csv")
if (!file.exists(claims_path)) {
  stop("all_cleaned_claims.csv not found. Run 05_Claims_Analysis.r with RUN_LOCAL = TRUE first.")
}

claims <- fread(claims_path)

# ── 2. Subset to the five control states only ─────────────────────────────────
# Pennsylvania (PA) is the treatment group and is intentionally excluded from
# the control-group cost distribution figure.
CONTROL_STATES <- c("CO", "LA", "NM", "TN", "UT")

state_label_map <- c(
  CO = "Colorado",
  LA = "Louisiana",
  NM = "New Mexico",
  TN = "Tennessee",
  UT = "Utah"
)

plot_dt <- claims[
  state %in% CONTROL_STATES &
    !is.na(total_cost_2023) &
    total_cost_2023 > 0
][,
  state_label := factor(state_label_map[state], levels = unname(state_label_map))
]

cat(sprintf("Control-state claims available for figure: %d rows across %d states\n",
            nrow(plot_dt), uniqueN(plot_dt$state)))
print(plot_dt[, .(n = .N, median = median(total_cost_2023), mean = mean(total_cost_2023)), by = state_label])

# ── 3. Compute per-state summary statistics for reference lines ───────────────
state_stats <- plot_dt[,
  .(
    med  = median(total_cost_2023, na.rm = TRUE),
    avg  = mean(total_cost_2023,   na.rm = TRUE)
  ),
  by = state_label
]

# ── 4. Build figure ───────────────────────────────────────────────────────────
fig <- ggplot(plot_dt, aes(x = total_cost_2023)) +

  # Log-scale kernel density
  geom_density(
    fill  = "#2C6E9E",
    color = "#1B4F72",
    alpha = 0.55,
    adjust = 1.2   # slight smoothing to reduce noise in thin tails
  ) +

  # Median line
  geom_vline(
    data     = state_stats,
    aes(xintercept = med, linetype = "Median"),
    color    = "#E74C3C",
    linewidth = 0.75
  ) +

  # Mean line
  geom_vline(
    data     = state_stats,
    aes(xintercept = avg, linetype = "Mean"),
    color    = "#E67E22",
    linewidth = 0.75
  ) +

  scale_linetype_manual(
    name   = NULL,
    values = c("Median" = "dashed", "Mean" = "dotdash"),
    guide  = guide_legend(override.aes = list(color = c("#E67E22", "#E74C3C")))
  ) +

  # Log scale on x-axis
  scale_x_log10(
    labels = label_dollar(
      scale_cut = cut_short_scale(),
      prefix    = "$"
    ),
    breaks = c(1e3, 1e4, 1e5, 1e6, 1e7),
    minor_breaks = NULL
  ) +

  # Facet by state, free y-scale so each panel's density is readable
  facet_wrap(~ state_label, ncol = 2, scales = "free_y") +

  labs(
    x     = "Realized Cleanup Cost (2023 USD, log scale)",
    y     = "Density",
    title = NULL
  ) +

  theme_bw(base_size = 11) +
  theme(
    strip.background   = element_rect(fill = "#EAF1FB", color = "grey70"),
    strip.text         = element_text(face = "bold", size = 10),
    axis.text.x        = element_text(angle = 30, hjust = 1, size = 8),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    legend.key.width   = unit(1.5, "cm"),
    plot.margin        = margin(6, 10, 6, 6)
  )

# ── 5. Save ───────────────────────────────────────────────────────────────────
out_dir <- here("Output", "Figures")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_path <- file.path(out_dir, "Figure_cost_distribution.png")
ggsave(
  filename = out_path,
  plot     = fig,
  width    = 7,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)

cat(sprintf("\nFigure saved to: %s\n", out_path))