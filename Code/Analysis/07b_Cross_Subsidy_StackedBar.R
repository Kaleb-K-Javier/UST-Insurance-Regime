################################################################################
# 07b_Cross_Subsidy_StackedBar.R
#
# Stacked-bar cross-subsidy figure (consumes 07's cross_subsidy_facility.csv).
# Facilities ordered by fair premium PP, binned into ventiles; each bar = mean PP
# in that bin, stacked into three economically meaningful layers:
#   1. fee paid            : the actual flat fee (fee_per_tank x n_tanks)
#   2. fee -> tau          : underpayment relative to the break-even fee
#   3. tau -> PP (if >tau) : the cross-subsidy received (subsidized beyond break-even)
# tau = break-even flat fee = mean PP within state. Per-state facets + individuals.
#
# Real per-facility fee joined from facility_panel (CO/LA/TN). NM is gas-tax funded
# (no flat fee) -> fee = 0, footnoted.
#
# INPUTS  : Data/Analysis/cross_subsidy_facility.csv  (from 07)
#           Data/Analysis/facility_panel.csv          (active_tanks, fr_premium_per_tank_yr)
# OUTPUTS : Output/Figures/Fig_CrossSub_Stacked_<ST>.{png,pdf}   (4 individual)
#           Output/Figures/Fig_CrossSub_Stacked_Panel.{png,pdf}  (2x2 combined)
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
  library(here)
})
source(here::here("Code", "Helpers", "data_paths.R"))

cat("=== 07b: Cross-subsidy stacked-bar figure ===\n")

OUTPUT_FIGURES <- here("Output", "Figures")
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

FIG_STATES <- c("CO", "LA", "NM", "TN")
N_BINS     <- 20L   # ventiles
STATE_LAB  <- c(CO = "Colorado", LA = "Louisiana", NM = "New Mexico", TN = "Tennessee")

# Segment colors (deep red = cross-subsidy received; light = gap to break-even; grey = paid)
SEG_LEVELS <- c("Fee paid", "Underpays break-even (fee → τ)",
                "Subsidized above break-even (τ → PP)")
SEG_COLORS <- c("Fee paid" = "#6E6E6E",
                "Underpays break-even (fee → τ)" = "#F2C2B0",
                "Subsidized above break-even (τ → PP)" = "#C0392B")

# ── 1. Load facility-level PP (from 07) ──────────────────────────────────────
fac <- fread(here("Data", "Analysis", "cross_subsidy_facility.csv"))
fac <- fac[state %in% FIG_STATES]
cat(sprintf("Facilities: %s across %d states\n",
            format(nrow(fac), big.mark = ","), uniqueN(fac$state)))

# ── 2. Join the real per-facility fee (fee_per_tank x n_tanks) ───────────────
fp <- fread(data_in("Data", "Analysis", "facility_panel.csv"),
            select = c("panel_id", "panel_year", "active_tanks", "fr_premium_per_tank_yr"))
fp[, fee_fac := active_tanks * fr_premium_per_tank_yr]
fac <- merge(fac, fp[, .(panel_id, panel_year, fee_fac)],
             by = c("panel_id", "panel_year"), all.x = TRUE)
# NM (and any unmatched) = gas-tax funded, no flat fee
fac[is.na(fee_fac), fee_fac := 0]
cat("Median per-facility fee by state:\n")
print(fac[, .(fee_med = round(median(fee_fac)), PP_med = round(median(PP))), by = state][order(state)])

# ── 3. Bin by PP within state; build stacked segments ────────────────────────
fac[, ventile := cut(frank(PP, ties.method = "first"),
                     breaks = N_BINS, labels = FALSE), by = state]

bins <- fac[, .(
  mean_PP  = mean(PP),
  mean_fee = mean(fee_fac),
  tau      = mean(tau_s),       # tau_s is constant within state (= mean PP)
  n        = .N
), by = .(state, ventile)]

# Three stacked layers (sum to mean_PP)
bins[, `:=`(
  seg_fee = mean_fee,
  seg_mid = pmax(pmin(mean_PP, tau) - mean_fee, 0),
  seg_top = pmax(mean_PP - tau, 0)
)]
stopifnot(all(abs((bins$seg_fee + bins$seg_mid + bins$seg_top) - bins$mean_PP) < 1e-6))

long <- melt(bins,
             id.vars = c("state", "ventile", "tau", "mean_PP"),
             measure.vars = c("seg_fee", "seg_mid", "seg_top"),
             variable.name = "seg", value.name = "value")
long[, segment := factor(fcase(
  seg == "seg_fee", SEG_LEVELS[1],
  seg == "seg_mid", SEG_LEVELS[2],
  seg == "seg_top", SEG_LEVELS[3]
), levels = SEG_LEVELS)]
long[, state_lab := factor(STATE_LAB[state], levels = STATE_LAB[FIG_STATES])]

tau_dt <- unique(bins[, .(state, tau)])
tau_dt[, state_lab := factor(STATE_LAB[state], levels = STATE_LAB[FIG_STATES])]

# ── 4. Theme + plot builder ──────────────────────────────────────────────────
theme_pub <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.title    = element_text(face = "bold", size = rel(1.05)),
          plot.subtitle = element_text(color = "grey40", size = rel(0.8)),
          plot.caption  = element_text(color = "grey45", size = rel(0.7), hjust = 0),
          legend.position = "bottom", legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"))
}

build_plot <- function(dat, taus, facet = TRUE) {
  p <- ggplot(dat, aes(x = ventile, y = value, fill = segment)) +
    geom_col(width = 0.92, position = position_stack(reverse = TRUE)) +
    geom_hline(data = taus, aes(yintercept = tau),
               linetype = "dashed", color = "#1a1a1a", linewidth = 0.5) +
    geom_text(data = taus, aes(x = 0.5, y = tau, label = "break-even τ"),
              inherit.aes = FALSE, hjust = 0, vjust = -0.4, size = 2.9, color = "#1a1a1a") +
    scale_fill_manual(values = SEG_COLORS, drop = FALSE) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
    scale_y_continuous(labels = dollar_format(accuracy = 1, big.mark = ",")) +
    labs(x = "Facilities, ranked by fair premium (within state)",
         y = "Fair premium PP (2023 USD / facility-yr)") +
    theme_pub()
  if (facet) p <- p + facet_wrap(~ state_lab, scales = "free_y", ncol = 2)
  p
}

save_fig <- function(p, name, w, h) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".pdf")), p, width = w, height = h, device = grDevices::cairo_pdf)
  cat(sprintf("  Saved: %s (.png + .pdf)\n", name))
}

# ── 5. Combined 2x2 panel ────────────────────────────────────────────────────
p_panel <- build_plot(long, tau_dt, facet = TRUE) +
  labs(title = "Flat-fee funds cross-subsidize risk",
       subtitle = paste0("Facilities ranked by actuarially fair premium (2005). Bars stack: fee paid, ",
                         "the gap to break-even, and the cross-subsidy received above it."),
       caption = paste0("PP = λ · S̄ (01n hazard × pooled severity). τ = break-even flat fee = mean PP. ",
                       "Fee = fr_premium_per_tank_yr × tanks; NM is gas-tax funded (no flat fee)."))
save_fig(p_panel, "Fig_CrossSub_Stacked_Panel", w = 10, h = 7.5)

# ── 6. Individual state figures ──────────────────────────────────────────────
for (st in FIG_STATES) {
  dd <- long[state == st]
  tt <- tau_dt[state == st]
  note <- if (st == "NM") "  Gas-tax funded: no flat fee (fee = 0)." else ""
  p <- build_plot(dd, tt, facet = FALSE) +
    labs(title = sprintf("%s: cross-subsidy under a flat fee (2005)", STATE_LAB[st]),
         subtitle = sprintf("Break-even τ = %s/yr.%s",
                            dollar(unique(tt$tau), accuracy = 1), note))
  save_fig(p, sprintf("Fig_CrossSub_Stacked_%s", st), w = 7, h = 5)
}

cat("\n07b complete.\n")
