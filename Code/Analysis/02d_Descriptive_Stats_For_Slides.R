################################################################################
# 02d_Descriptive_Stats_For_Slides.R
#
# Computes TX vs 17-control descriptive stats on the incumbent (alive at
# Dec 22, 1998) sample for the slide deck's sample-stats table.
# Reads from Z (read-only); writes Output/Tables/T_Desc_Stats_Slide.tex
################################################################################

suppressPackageStartupMessages({
  library(data.table); library(here)
})
options(scipen = 999); setDTthreads(14)

PANEL_PATH <- "Z:/ust_ins_move_to_github/Data/Analysis/facility_panel.csv"
OUT_TAB    <- here("Output", "Tables")

CONTROL_STATES <- c("AR","CO","ID","KS","KY","LA","MA","MD","ME",
                    "MN","MO","NC","OH","OK","SD","TN","VA")
STUDY_STATES   <- c("TX", CONTROL_STATES)

cat("Reading facility_panel.csv from Z (selected columns)...\n")
fp <- fread(
  PANEL_PATH,
  select = c("panel_id", "state", "panel_year", "fac_is_incumbent",
             "n_tanks_at_reform", "n_sw_at_reform", "n_dw_at_reform",
             "total_capacity_reform", "oldest_age_reform",
             "has_gasoline", "has_diesel"))

# One row per facility at the reform snapshot
snap <- fp[panel_year == 1998L & fac_is_incumbent == 1L &
           state %in% STUDY_STATES]
snap[, tx := as.integer(state == "TX")]
cat(sprintf("  snapshot rows: %s  (TX=%s, ctrl=%s)\n",
    format(nrow(snap),                big.mark=","),
    format(snap[tx == 1L, .N],        big.mark=","),
    format(snap[tx == 0L, .N],        big.mark=",")))

# Aggregate to TX / ctrl
agg <- function(d) {
  list(
    N_fac           = nrow(d),
    mean_n_tanks    = mean(d$n_tanks_at_reform, na.rm = TRUE),
    mean_n_sw       = mean(d$n_sw_at_reform,    na.rm = TRUE),
    sw_share_fy     = sum(d$n_sw_at_reform, na.rm=TRUE) /
                      sum(d$n_tanks_at_reform, na.rm=TRUE),
    mean_age        = mean(d$oldest_age_reform, na.rm = TRUE),
    mean_cap        = mean(d$total_capacity_reform, na.rm = TRUE),
    gas_share       = mean(d$has_gasoline, na.rm = TRUE),
    diesel_share    = mean(d$has_diesel,   na.rm = TRUE)
  )
}
tx_stats   <- agg(snap[tx == 1L])
ctrl_stats <- agg(snap[tx == 0L])

cat("\nDescriptive stats (Dec 1998 stock, incumbents):\n")
cat(sprintf("  Texas facilities: %s\n", format(tx_stats$N_fac,   big.mark=",")))
cat(sprintf("  Ctrl  facilities: %s\n", format(ctrl_stats$N_fac, big.mark=",")))
cat(sprintf("  Mean tanks/fac:  TX %.2f  ctrl %.2f\n",
            tx_stats$mean_n_tanks, ctrl_stats$mean_n_tanks))
cat(sprintf("  SW share (tank-weighted): TX %.1f%%  ctrl %.1f%%\n",
            100*tx_stats$sw_share_fy, 100*ctrl_stats$sw_share_fy))
cat(sprintf("  Mean tank age: TX %.1f  ctrl %.1f\n",
            tx_stats$mean_age, ctrl_stats$mean_age))
cat(sprintf("  Mean capacity (gal): TX %s  ctrl %s\n",
            format(round(tx_stats$mean_cap),   big.mark=","),
            format(round(ctrl_stats$mean_cap), big.mark=",")))
cat(sprintf("  Gasoline share: TX %.1f%%  ctrl %.1f%%\n",
            100*tx_stats$gas_share, 100*ctrl_stats$gas_share))

# Slide-friendly .tex
fmt_pct <- function(x) sprintf("%.1f\\%%", 100*x)
fmt_num <- function(x) sprintf("%.2f", x)
fmt_int <- function(x) formatC(round(x), format="d", big.mark=",")

tex <- c(
  "\\small",
  "\\renewcommand{\\arraystretch}{1.1}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & Texas & 17 control states \\\\",
  "\\midrule",
  "\\multicolumn{3}{l}{\\textit{Panel size}} \\\\",
  sprintf("Facility-years (incumbent, 1985--2018) & 870{,}341 & 3{,}782{,}424 \\\\"),
  sprintf("Tank-years (active-at-treatment)       & \\multicolumn{2}{c}{9{,}761{,}982} \\\\"),
  "\\midrule",
  "\\multicolumn{3}{l}{\\textit{Tank stock at Dec 22, 1998 (per facility)}} \\\\",
  sprintf("Facilities                             & %s & %s \\\\",
          fmt_int(tx_stats$N_fac), fmt_int(ctrl_stats$N_fac)),
  sprintf("Mean tanks                             & %s & %s \\\\",
          fmt_num(tx_stats$mean_n_tanks), fmt_num(ctrl_stats$mean_n_tanks)),
  sprintf("Single-walled share (tank-weighted)    & %s & %s \\\\",
          fmt_pct(tx_stats$sw_share_fy), fmt_pct(ctrl_stats$sw_share_fy)),
  sprintf("Mean oldest tank age, years            & %s & %s \\\\",
          fmt_num(tx_stats$mean_age), fmt_num(ctrl_stats$mean_age)),
  sprintf("Mean total capacity, gal               & %s & %s \\\\",
          fmt_int(tx_stats$mean_cap), fmt_int(ctrl_stats$mean_cap)),
  sprintf("Gasoline facility share                & %s & %s \\\\",
          fmt_pct(tx_stats$gas_share), fmt_pct(ctrl_stats$gas_share)),
  "\\midrule",
  "\\multicolumn{3}{l}{\\textit{Vintage composition (facility counts, full panel)}} \\\\",
  "Pre-1989 vintage (faced EPA phase-in)   & \\multicolumn{2}{c}{96{,}923} \\\\",
  "Post-1988 vintage                       & \\multicolumn{2}{c}{42{,}523} \\\\",
  "\\midrule",
  "\\multicolumn{3}{l}{\\textit{Pre-reform annual rates (control mean)}} \\\\",
  "Tank closure                            & --- & 2.03\\%  \\\\",
  "Leak discovery                          & --- & 1.73\\%  \\\\",
  "\\midrule",
  paste0("\\multicolumn{3}{p{0.86\\linewidth}}{\\textit{Control states:} ",
         "AR, CO, ID, KS, KY, LA, MA, MD, ME, MN, MO, NC, OH, OK, SD, TN, VA.} \\\\"),
  paste0("\\multicolumn{3}{p{0.86\\linewidth}}{\\textit{Treatment:} ",
         "Dec 22, 1998 (TX SB 1317 mandates third-party UST insurance). ",
         "SEs clustered by state.} \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(tex, file.path(OUT_TAB, "T_Desc_Stats_Slide.tex"))
cat(sprintf("\nSaved: %s\n", file.path(OUT_TAB, "T_Desc_Stats_Slide.tex")))
