################################################################################
# 07d_Cross_Subsidy_PaidShare.R   (alternative framing to 07c)
#
# "Firms pay almost none of their fair cost." Facilities ordered by fair premium PP.
#   - BLACK [0, fee]  : the share the firm actually pays (gas-tax-funded flat fee)
#   - BLUE  [fee, PP] : the rest of the fair premium — the subsidized share
#   - faint dashed break-even τ as a reference (no red shading here)
# Compare against 07c (which shades the redistribution blue=red around τ).
#
# INPUTS  : Data/Analysis/cross_subsidy_facility.csv ; Data/Analysis/facility_panel.csv
# OUTPUTS : Output/Figures/Fig_CrossSub_Paid_<ST>.{png,pdf} + _Panel
################################################################################

if (!requireNamespace("ggpattern", quietly = TRUE))
  install.packages("ggpattern", repos = "https://cloud.r-project.org", quiet = TRUE)
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(ggpattern); library(scales); library(here)
})
source(here::here("Code", "Helpers", "data_paths.R"))
cat("=== 07d: paid-share framing ===\n")

OUTPUT_FIGURES <- here("Output", "Figures")
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)
FIG_STATES <- c("CO", "LA", "NM", "TN")
STATE_LAB  <- c(CO = "Colorado", LA = "Louisiana", NM = "New Mexico", TN = "Tennessee")
COL_PAID <- "#2B2B2B"; COL_SUB <- "#3B7DD8"

fac <- fread(here("Data", "Analysis", "cross_subsidy_facility.csv"))[state %in% FIG_STATES]
fp  <- fread(data_in("Data", "Analysis", "facility_panel.csv"),
             select = c("panel_id", "panel_year", "active_tanks", "fr_premium_per_tank_yr"))
fp[, fee_fac := active_tanks * fr_premium_per_tank_yr]
fac <- merge(fac, fp[, .(panel_id, panel_year, fee_fac)], by = c("panel_id", "panel_year"), all.x = TRUE)
fac[is.na(fee_fac), fee_fac := 0]

setorder(fac, state, PP)
fac[, pct := (seq_len(.N) - 0.5) / .N * 100, by = state]
fac[, tau := mean(PP), by = state]
YEAR <- fac$panel_year[1L]   # snapshot year (from 07's output)

stat <- fac[, .(tau = mean(PP), paid_pct = sum(fee_fac) / sum(PP)), by = state]
stat[, lab := sprintf("%d · firms pay %.0f%% of fair cost\n(flat fee ÷ fair premium)\n%.0f%% is subsidized", YEAR, paid_pct * 100, (1 - paid_pct) * 100)]
cat("Paid share of fair cost by state:\n"); print(stat[, .(state, paid_pct = round(paid_pct, 3))])

mk <- function(d){ d <- copy(d); d[, state_lab := factor(STATE_LAB[state], levels = STATE_LAB[FIG_STATES])]; d }
fac <- mk(fac); stat <- mk(stat)

theme_pub <- function(base = 12) theme_minimal(base_size = base) +
  theme(plot.title = element_blank(), plot.subtitle = element_blank(), plot.caption = element_blank(),  # titles/notes live in the LaTeX caption
        legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))

build <- function(d, s, facet = TRUE) {
  p <- ggplot(d, aes(x = pct)) +
    geom_ribbon(aes(ymin = fee_fac, ymax = PP, fill = "Subsidized share")) +
    # Hatch the wedge above break-even τ: cross-subsidy that persists even under a
    # neutral (break-even) flat fee — high-risk firms still underpay relative to it.
    ggpattern::geom_ribbon_pattern(
      data = d[PP >= tau], aes(ymin = tau, ymax = PP),
      fill = NA, pattern = "stripe", pattern_fill = "grey12", pattern_colour = "grey12",
      pattern_angle = 45, pattern_density = 0.07, pattern_spacing = 0.017) +
    geom_ribbon(aes(ymin = 0, ymax = fee_fac, fill = "Share firm pays (fee)")) +
    geom_line(aes(y = PP), color = "#1a1a1a", linewidth = 0.6) +
    geom_line(aes(y = tau), color = "grey45", linetype = "dashed", linewidth = 0.4) +
    geom_text(data = s, aes(x = 1, y = tau, label = "break-even τ"), inherit.aes = FALSE,
              hjust = 0, vjust = -0.5, size = 2.7, color = "grey45") +
    geom_text(data = s, aes(x = 2, y = Inf, label = lab), inherit.aes = FALSE,
              hjust = 0, vjust = 1.3, size = 2.9, color = "grey20", lineheight = 1.05) +
    scale_fill_manual(values = c("Share firm pays (fee)" = COL_PAID, "Subsidized share" = COL_SUB),
                      breaks = c("Share firm pays (fee)", "Subsidized share")) +
    scale_x_continuous("Facilities, ranked by fair premium (within state)", labels = function(x) paste0(x, "%")) +
    scale_y_continuous("Fair premium (2023 USD / facility-yr)", labels = dollar_format(accuracy = 1, big.mark = ",")) +
    theme_pub()
  if (facet) p + facet_wrap(~ state_lab, scales = "free_y", ncol = 2) else p
}
save_fig <- function(p, name, w, h) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  # PDF can fail if the file is open in a viewer (file lock). Surface it with a
  # message and keep going rather than halting the whole figure set.
  pdf_ok <- tryCatch({
    ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".pdf")), p, width = w, height = h, device = grDevices::cairo_pdf)
    TRUE
  }, error = function(e) { message("  PDF locked/skipped: ", name, ".pdf"); FALSE })
  cat(sprintf("  Saved: %s (%s)\n", name, if (pdf_ok) ".png + .pdf" else ".png only"))
}

# Individual figures first (so an open viewer locking the panel PDF can't block them).
for (st in FIG_STATES) {
  p <- build(fac[state == st], stat[state == st], FALSE) +
    labs(title = sprintf("%s: share paid vs subsidized (2005)", STATE_LAB[st]),
         subtitle = "Black = fee paid; blue = subsidized; hatched = cross-subsidy above a neutral break-even fee.")
  save_fig(p, sprintf("Fig_CrossSub_Paid_%s", st), 7, 5)
}
p_panel <- build(fac, stat, TRUE) +
  labs(title = "Firms pay a sliver of their fair cost — and a neutral flat fee still cross-subsidizes",
       subtitle = "Black = fee paid; blue = subsidized share. Hatched (above τ) = the wedge that stays cross-subsidy even under a neutral break-even fee.",
       caption = "PP = λ·S̄. Fee = fr_premium_per_tank_yr × tanks (NM gas-tax, ≈0). Dashed = break-even fee τ = mean PP.")
save_fig(p_panel, "Fig_CrossSub_Paid_Panel", 10, 7.5)
cat("\n07d complete.\n")
