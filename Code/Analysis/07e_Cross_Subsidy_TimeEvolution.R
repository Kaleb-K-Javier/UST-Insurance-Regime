################################################################################
# 07e_Cross_Subsidy_TimeEvolution.R
#
# Cross-subsidy time evolution: does the redistribution move? (2000/05/10/15)
# Extends the 07c area cross-subsidy design to four year cross-sections per
# state and adds a patchwork trend summary (% transfer + break-even tau in $).
#
# INPUTS  : Data/Analysis/cross_subsidy_facility.csv          (from 07)
#           Data/Analysis/analysis_hazard_predictions_full.csv (from 01n)
#           Data/Analysis/facility_panel.csv
# OUTPUTS : Data/Analysis/cross_subsidy_timeevolution.csv
#           Output/Tables/cross_subsidy_timeevolution_summary.csv
#           Output/Figures/Fig_CrossSub_AreaTime_{CO,LA,NM,TN}.{png,pdf}
#           Output/Figures/Fig_CrossSub_TransferTrend.{png,pdf}
################################################################################

if (!requireNamespace("patchwork", quietly = TRUE))
  install.packages("patchwork", repos = "https://cloud.r-project.org", quiet = TRUE)

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(patchwork); library(scales); library(here)
})
source(here::here("Code", "Helpers", "data_paths.R"))
cat("=== 07e: Cross-subsidy time evolution ===\n")

OUTPUT_FIGURES <- here("Output", "Figures")
OUTPUT_TABLES  <- here("Output", "Tables")
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)

YEARS      <- c(2000L, 2005L, 2010L, 2015L)
FIG_STATES <- c("CO", "LA", "NM", "TN")
STATE_LAB  <- c(CO = "Colorado", LA = "Louisiana", NM = "New Mexico", TN = "Tennessee")
STATE_COL  <- c(CO = "#1B6CA8", LA = "#C94B4B", NM = "#4E9A06", TN = "#E88B00")

COL_OVER <- "#3B7DD8"  # blue  : overpay (subsidizers)
COL_SUB  <- "#C0392B"  # red   : subsidized
COL_PAID <- "#2B2B2B"  # black : share actually paid (fee)

# ── Step 1: Severity constant S̄ ──────────────────────────────────────────────
cat("=== STEP 1: Severity constant ===\n")
fac0  <- fread(here("Data", "Analysis", "cross_subsidy_facility.csv"))
S_bar <- mean(fac0$PP / fac0$lambda)
stopifnot(is.finite(S_bar), S_bar > 0)
cat(sprintf("  S_bar = %.0f  (sd of PP/lambda = %.6f)\n", S_bar, sd(fac0$PP / fac0$lambda)))

# ── Step 2: Hazard cross-sections ─────────────────────────────────────────────
cat("=== STEP 2: Hazard cross-sections ===\n")
haz <- fread(data_in("Data", "Analysis", "analysis_hazard_predictions_full.csv"),
             select = c("panel_id", "panel_year", "state", "pred_elnet_full"))
haz <- haz[state %in% FIG_STATES & panel_year %in% YEARS]
setnames(haz, "pred_elnet_full", "lambda")
stopifnot(all(haz$lambda >= 0), all(haz$lambda <= 1), !anyNA(haz$lambda))
cell_n <- haz[, .N, by = .(state, panel_year)]
stopifnot(nrow(cell_n) == length(FIG_STATES) * length(YEARS), all(cell_n$N > 0))
cat("N by state x year:\n")
print(dcast(cell_n, state ~ panel_year, value.var = "N"))

# ── Step 3: Per-facility fee ───────────────────────────────────────────────────
cat("=== STEP 3: Per-facility fee ===\n")
fp <- fread(data_in("Data", "Analysis", "facility_panel.csv"),
            select = c("panel_id", "panel_year", "active_tanks", "fr_premium_per_tank_yr"))
fp[, fee_fac := active_tanks * fr_premium_per_tank_yr]
n_pre <- nrow(haz)
haz <- merge(haz, fp[, .(panel_id, panel_year, fee_fac)],
             by = c("panel_id", "panel_year"), all.x = TRUE)
n_nafill <- sum(is.na(haz$fee_fac))
haz[is.na(fee_fac), fee_fac := 0]
cat(sprintf("  Rows: %d -> %d after merge; %d fee_fac NA -> 0 (NM gas-tax / unmatched)\n",
            n_pre, nrow(haz), n_nafill))

# ── Step 4: PP, tau, transfer, percentile ─────────────────────────────────────
cat("=== STEP 4: PP, tau, transfer, percentile ===\n")
haz[, PP  := lambda * S_bar]
haz[, tau := mean(PP), by = .(state, panel_year)]
setorder(haz, state, panel_year, PP)
haz[, pct := (seq_len(.N) - 0.5) / .N * 100, by = .(state, panel_year)]

cell_dev <- haz[, .(dev = abs(sum(PP - tau))), by = .(state, panel_year)]
stopifnot(all(cell_dev$dev < 1e-6))

stat <- haz[, .(N        = .N,
                tau      = mean(PP),
                xfer_pct = sum(pmax(PP - mean(PP), 0)) / sum(PP),
                over_pct = mean(PP <= mean(PP))),
            by = .(state, panel_year)]

# strip labels carry N and xfer% into each year panel
stat[, year_lab := sprintf("%d  (N=%d, xfer=%.0f%%)", panel_year, N, xfer_pct * 100)]
haz <- merge(haz, stat[, .(state, panel_year, year_lab)], by = c("state", "panel_year"))

fwrite(haz[, .(panel_id, state, panel_year, lambda, PP, tau, fee_fac)],
       here("Data", "Analysis", "cross_subsidy_timeevolution.csv"))
fwrite(stat[, .(state, panel_year, N, tau, xfer_pct, over_pct)],
       here("Output", "Tables", "cross_subsidy_timeevolution_summary.csv"))

cat("Cross-subsidy summary:\n")
print(stat[order(state, panel_year),
           .(state, panel_year, N, tau = round(tau), xfer_pct = round(xfer_pct, 4))])

# ── Shared helpers ─────────────────────────────────────────────────────────────
theme_pub <- function(base = 12) theme_minimal(base_size = base) +
  theme(plot.title = element_blank(), plot.subtitle = element_blank(), plot.caption = element_blank(),  # titles/notes live in the LaTeX caption
        legend.position  = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold"))

save_fig <- function(p, name, w, h) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".png")), p,
         width = w, height = h, dpi = 300, bg = "white")
  pdf_ok <- tryCatch({
    ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".pdf")), p,
           width = w, height = h, device = grDevices::cairo_pdf)
    TRUE
  }, error = function(e) { message("  PDF locked/skipped: ", name, ".pdf"); FALSE })
  cat(sprintf("  Saved: %s (%s)\n", name, if (pdf_ok) ".png + .pdf" else ".png only"))
}

# ── Step 5: F1 — per-state area plots faceted by year ─────────────────────────
cat("=== STEP 5: Figure F1 (area x year, per state) ===\n")

build_time <- function(d, s) {
  yr_levels <- s[order(panel_year), year_lab]
  d <- copy(d); d[, year_lab := factor(year_lab, levels = yr_levels)]
  s <- copy(s); s[, year_lab := factor(year_lab, levels = yr_levels)]

  ggplot(d, aes(x = pct)) +
    geom_ribbon(data = d[PP <= tau],
                aes(ymin = PP, ymax = tau, fill = "Overpay (subsidizers)")) +
    geom_ribbon(data = d[PP >= tau],
                aes(ymin = tau, ymax = PP, fill = "Subsidized")) +
    geom_ribbon(aes(ymin = 0, ymax = fee_fac, fill = "Share firm pays (fee)")) +
    geom_line(aes(y = PP),  color = "#1a1a1a", linewidth = 0.6) +
    geom_line(aes(y = tau), color = "#1a1a1a", linetype = "dashed", linewidth = 0.45) +
    geom_text(data = s, aes(x = 1, y = tau),
              label = "break-even fee τ",
              inherit.aes = FALSE, hjust = 0, vjust = -0.5, size = 2.6) +
    scale_fill_manual(
      values = c("Overpay (subsidizers)" = COL_OVER,
                 "Subsidized"            = COL_SUB,
                 "Share firm pays (fee)" = COL_PAID),
      breaks = c("Share firm pays (fee)", "Overpay (subsidizers)", "Subsidized")) +
    scale_x_continuous("Facilities, ranked by fair premium (within state / year)",
                       labels = function(x) paste0(x, "%")) +
    scale_y_continuous("Fair premium PP (2023 USD / facility-yr)",
                       labels = dollar_format(accuracy = 1, big.mark = ",")) +
    facet_wrap(~ year_lab, nrow = 1, scales = "free_y") +
    theme_pub()
}

for (st in FIG_STATES) {
  d_st   <- haz[state == st]
  s_st   <- stat[state == st]
  cap_nm <- if (st == "NM")
    "\nNote: NM sample is small (N ≈ 70 in 2000, ≈ 150 in 2005); interpret with caution."
  else ""
  p <- build_time(d_st, s_st) +
    labs(title    = sprintf("%s: does the cross-subsidy move? (2000–2015)", STATE_LAB[st]),
         subtitle = "Blue = low-risk firms overpaying; red = high-risk firms subsidized. Areas equal (balanced at break-even τ).",
         caption  = paste0("PP = λ·S̄ (01n hazard × pooled severity). τ = break-even flat fee = mean PP. Black = actual fee paid.", cap_nm))
  save_fig(p, sprintf("Fig_CrossSub_AreaTime_%s", st), 11, 3.6)
}

# ── Step 6: F2 — transfer trend (patchwork) ───────────────────────────────────
cat("=== STEP 6: Figure F2 (transfer trend) ===\n")

stat_p <- copy(stat)

p_xfer <- ggplot(stat_p, aes(x = panel_year, y = xfer_pct * 100,
                              color = state, group = state)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_manual(values = STATE_COL, labels = STATE_LAB) +
  scale_x_continuous(breaks = YEARS) +
  scale_y_continuous("% of premiums",
                     labels = function(x) paste0(x, "%")) +
  labs(title = "Cross-subsidy over time: dollars vs share of premiums", x = NULL) +
  theme_pub() +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p_tau <- ggplot(stat_p, aes(x = panel_year, y = tau,
                             color = state, group = state)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_manual(values = STATE_COL, labels = STATE_LAB) +
  scale_x_continuous(breaks = YEARS) +
  scale_y_continuous("Break-even τ ($/facility-yr)",
                     labels = dollar_format(accuracy = 1, big.mark = ",")) +
  labs(x = "Year") +
  theme_pub() + theme(legend.position = "bottom", legend.title = element_blank())

p_trend <- p_xfer / p_tau
save_fig(p_trend, "Fig_CrossSub_TransferTrend", 9, 6)

# ── Step 7: Summary print ─────────────────────────────────────────────────────
cat("=== STEP 7: Summary ===\n")
cat(sprintf("  S_bar = %.0f\n", S_bar))
cat("  N by state x year:\n")
print(dcast(cell_n, state ~ panel_year, value.var = "N"))
cat("  Transfer summary by state x year:\n")
print(stat[order(state, panel_year),
           .(state, panel_year, N,
             tau      = round(tau),
             xfer_pct = round(xfer_pct, 4),
             over_pct = round(over_pct, 3))])
thin <- cell_n[N < 200]
if (nrow(thin) > 0) {
  cat("  THIN CELLS (N < 200):\n"); print(thin)
} else {
  cat("  All cells: N >= 200.\n")
}
cat("\n07e complete.\n")
