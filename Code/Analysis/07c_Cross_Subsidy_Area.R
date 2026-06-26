################################################################################
# 07c_Cross_Subsidy_Area.R
#
# Cross-subsidy as the AREA between the fair-premium curve and the break-even fee.
# Facilities ordered by fair premium PP (x = percentile). Break-even fee τ = mean PP.
#   - BLUE   band between PP and τ where PP < τ : low-risk firms OVERPAY (subsidizers)
#   - RED    band between τ and PP where PP > τ : high-risk firms are SUBSIDIZED
#   - blue area == red area == the cross-subsidy (balanced at break-even)
#   - BLACK thin band [0, fee] : the share each firm actually pays (gas-tax-funded fee)
#
# INPUTS  : Data/Analysis/cross_subsidy_facility.csv  (from 07)
#           Data/Analysis/facility_panel.csv          (active_tanks, fr_premium_per_tank_yr)
# OUTPUTS : Output/Figures/Fig_CrossSub_Area_<ST>.{png,pdf}   (4 individual)
#           Output/Figures/Fig_CrossSub_Area_Panel.{png,pdf}  (2x2 combined)
################################################################################

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(scales); library(here)
})
source(here::here("Code", "Helpers", "data_paths.R"))
cat("=== 07c: Cross-subsidy AREA figure ===\n")

OUTPUT_FIGURES <- here("Output", "Figures")
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)
FIG_STATES <- c("CO", "LA", "NM", "TN")
STATE_LAB  <- c(CO = "Colorado", LA = "Louisiana", NM = "New Mexico", TN = "Tennessee")

COL_OVER <- "#3B7DD8"  # blue  : overpay (subsidizers)
COL_SUB  <- "#C0392B"  # red   : subsidized
COL_PAID <- "#2B2B2B"  # black : share actually paid (fee)

# ── 1. Data: PP + per-facility fee ───────────────────────────────────────────
fac <- fread(here("Data", "Analysis", "cross_subsidy_facility.csv"))[state %in% FIG_STATES]
fp  <- fread(data_in("Data", "Analysis", "facility_panel.csv"),
             select = c("panel_id", "panel_year", "active_tanks", "fr_premium_per_tank_yr"))
fp[, fee_fac := active_tanks * fr_premium_per_tank_yr]
fac <- merge(fac, fp[, .(panel_id, panel_year, fee_fac)],
             by = c("panel_id", "panel_year"), all.x = TRUE)
fac[is.na(fee_fac), fee_fac := 0]

# ── 2. Per-state: order by PP, percentile x, cross-subsidy stat ───────────────
setorder(fac, state, PP)
fac[, pct := (seq_len(.N) - 0.5) / .N * 100, by = state]
fac[, tau := mean(PP), by = state]

stat <- fac[, .(tau = mean(PP),
                xfer_pct = sum(pmax(PP - mean(PP), 0)) / sum(PP),
                over_pct = mean(PP <= mean(PP))), by = state]
stat[, lab := sprintf("Cross-subsidy = %.0f%% of premiums\n%.0f%% overpay → %.0f%% subsidized",
                      xfer_pct * 100, over_pct * 100, (1 - over_pct) * 100)]
cat("Per-state cross-subsidy (% of premiums):\n"); print(stat[, .(state, tau = round(tau), xfer_pct = round(xfer_pct,3))])

mk_lab <- function(d) { d <- copy(d); d[, state_lab := factor(STATE_LAB[state], levels = STATE_LAB[FIG_STATES])]; d }
fac  <- mk_lab(fac); stat <- mk_lab(stat)

# ── 3. Plot builder ──────────────────────────────────────────────────────────
theme_pub <- function(base = 12) theme_minimal(base_size = base) +
  theme(plot.title = element_blank(), plot.subtitle = element_blank(), plot.caption = element_blank(),  # titles/notes live in the LaTeX caption
        legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))

build <- function(d, s, facet = TRUE) {
  p <- ggplot(d, aes(x = pct)) +
    # cross-subsidy band: blue where PP<tau (overpay), red where PP>tau (subsidized)
    geom_ribbon(data = d[PP <= tau], aes(ymin = PP, ymax = tau, fill = "Overpay (subsidizers)")) +
    geom_ribbon(data = d[PP >= tau], aes(ymin = tau, ymax = PP, fill = "Subsidized")) +
    # share actually paid (fee) — thin black band at the bottom
    geom_ribbon(aes(ymin = 0, ymax = fee_fac, fill = "Share firm pays (fee)")) +
    geom_line(aes(y = PP), color = "#1a1a1a", linewidth = 0.6) +
    geom_line(aes(y = tau), color = "#1a1a1a", linetype = "dashed", linewidth = 0.45) +
    geom_text(data = s, aes(x = 1, y = tau, label = "break-even fee τ"),
              inherit.aes = FALSE, hjust = 0, vjust = -0.5, size = 2.9) +
    geom_text(data = s, aes(x = 2, y = Inf, label = lab), inherit.aes = FALSE,
              hjust = 0, vjust = 1.3, size = 2.85, color = "grey20", lineheight = 1.05) +
    scale_fill_manual(values = c("Overpay (subsidizers)" = COL_OVER,
                                 "Subsidized" = COL_SUB,
                                 "Share firm pays (fee)" = COL_PAID),
                      breaks = c("Share firm pays (fee)", "Overpay (subsidizers)", "Subsidized")) +
    scale_x_continuous("Facilities, ranked by fair premium (within state)",
                       labels = function(x) paste0(x, "%")) +
    scale_y_continuous("Fair premium PP (2023 USD / facility-yr)",
                       labels = dollar_format(accuracy = 1, big.mark = ",")) +
    theme_pub()
  if (facet) p + facet_wrap(~ state_lab, scales = "free_y", ncol = 2) else p
}

save_fig <- function(p, name, w, h) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".pdf")), p, width = w, height = h, device = grDevices::cairo_pdf)
  cat(sprintf("  Saved: %s\n", name))
}

# ── 4. Combined + individual ─────────────────────────────────────────────────
p_panel <- build(fac, stat, facet = TRUE) +
  labs(title = "The cross-subsidy is the gap between fair cost and a flat fee",
       subtitle = "Blue = low-risk firms overpaying; red = high-risk firms subsidized. The two areas are equal — that balance is the transfer.",
       caption = "PP = λ·S̄ (01n hazard × pooled severity). τ = break-even flat fee = mean PP. Black = actual fee paid (fr_premium_per_tank_yr × tanks; NM gas-tax, ≈0).")
save_fig(p_panel, "Fig_CrossSub_Area_Panel", 10, 7.5)

for (st in FIG_STATES) {
  p <- build(fac[state == st], stat[state == st], facet = FALSE) +
    labs(title = sprintf("%s: cross-subsidy under a flat fee (2005)", STATE_LAB[st]),
         subtitle = "Blue = overpayment (subsidizers); red = subsidy received. Equal areas.")
  save_fig(p, sprintf("Fig_CrossSub_Area_%s", st), 7, 5)
}
cat("\n07c complete.\n")
