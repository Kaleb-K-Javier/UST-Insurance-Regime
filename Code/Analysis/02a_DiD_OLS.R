################################################################################
# 02a_DiD_OLS.R
# Texas UST Insurance Reform — Facility-Level OLS Difference-in-Differences
#
# ANALYSIS LEVEL: Facility-year panel
#
# IDENTIFICATION:
#   Treatment indicator: did_term = texas_treated x I(panel_year >= 1999)
#   Unit FE:            panel_id  (facility intercept)
#   Cell-year FE:       make_model_fac^panel_year
#     Within each (portfolio wall x portfolio fuel x oldest-tank age bin) cell,
#     the estimator compares Texas facilities to control-state facilities in the
#     same calendar year. Parallel trends is localised within cells.
#   Clustering: state level (20 clusters: 1 TX + 19 controls)
#
# PRIMARY SAMPLE: mm_fac_primary
#   Facilities with a valid make_model_fac cell, classified wall and fuel,
#   non-missing oldest-tank age bin.
#   Built from facility_make_model.rds (Section 2.5 of the panel builder).
#
# DATA INPUTS:
#   Data/Analysis/analysis_annual_data.rds   — facility-year panel
#   Data/Analysis/analysis_closed_tanks.rds  — closed-tank records
#   Data/Analysis/facility_make_model.rds    — facility cell assignments
#   Data/Analysis/facility_biography.rds     — facility install history
#
# SECTIONS:
#   S1   Setup and Data Loading
#   S2   Variable Construction
#   S3   Helper Functions
#   S4   Primary Sample Construction and Cell Coverage
#   S5   Descriptive Figures
#   S6   Parallel Trends Validation
#   S7   Headline DiD Estimates
#   S8   HTE by Portfolio Wall Composition
#   S9   HTE by Oldest-Tank Age at Reform
#   S10  Event Study Figures
#   S11  Reported Leak DiD
#   S12  Age at Closure
#   S13  Robustness Checks
#   S14  Save Samples and Model Objects
################################################################################


#### S1: Setup and Data Loading ####

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(here)
  library(ggplot2)
  library(broom)
  library(patchwork)
  library(scales)
})

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
ANALYSIS_DIR   <- here("Data",   "Analysis")

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# ---- Study parameters ----
POST_YEAR   <- 1999L
ES_END      <- 2018L
PANEL_START <- 1990L
PANEL_END   <- 2018L

# Facility-level age bins: used as time-varying control only (not cell assignment)
# These map onto the 3-year bins from Section 6.4 of the design document.
AGE_BIN_BREAKS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
AGE_BIN_LABELS <- c("0-2", "3-5", "6-8", "9-11",
                     "12-14", "15-17", "18-20", "21-23", "24+")
AGE_BIN_REF    <- "0-2"

COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"

# ---- Load data ----
annual_data  <- readRDS(file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))
closed_tanks <- readRDS(file.path(ANALYSIS_DIR, "analysis_closed_tanks.rds"))

# ---- Merge facility make-model cell (Section 2.5 of panel builder) ----
fac_mm <- readRDS(file.path(ANALYSIS_DIR, "facility_make_model.rds"))
mm_merge_cols <- c("panel_id", "make_model_fac", "fac_wall", "fac_fuel",
                   "fac_oldest_age_bin", "n_tanks_at_reform")
drop_cols <- intersect(setdiff(mm_merge_cols, "panel_id"), names(annual_data))
if (length(drop_cols) > 0) annual_data[, (drop_cols) := NULL]
annual_data <- merge(annual_data, fac_mm[, ..mm_merge_cols],
                     by = "panel_id", all.x = TRUE)

# ---- Merge facility biography (Section 2.6 of panel builder) ----
fac_bio <- readRDS(file.path(ANALYSIS_DIR, "facility_biography.rds"))
bio_merge_cols <- c("panel_id",
                    "facility_first_install_yr", "facility_last_install_yr",
                    "total_tanks_ever", "had_pre1989_tanks",
                    "all_tanks_post1988", "fac_is_incumbent")
drop_cols_bio <- intersect(setdiff(bio_merge_cols, "panel_id"), names(annual_data))
if (length(drop_cols_bio) > 0) annual_data[, (drop_cols_bio) := NULL]
annual_data <- merge(annual_data, fac_bio[, ..bio_merge_cols],
                     by = "panel_id", all.x = TRUE)

rm(fac_mm, fac_bio)

cat(sprintf("annual_data: %s rows, %s facilities\n",
            format(nrow(annual_data),            big.mark = ","),
            format(uniqueN(annual_data$panel_id), big.mark = ",")))
cat(sprintf("make_model_fac coverage: %.1f%%\n",
            100 * mean(!is.na(annual_data$make_model_fac))))


#### S2: Variable Construction ####

# Time-varying facility age bin — used as control in regressions, NOT for
# cell assignment. avg_tank_age is the facility-level average across active
# tanks in each year.
annual_data[, age_bin := factor(
  cut(avg_tank_age,
      breaks         = AGE_BIN_BREAKS,
      labels         = AGE_BIN_LABELS,
      right          = FALSE,
      include.lowest = TRUE),
  levels = AGE_BIN_LABELS)]
annual_data[, age_bin := relevel(age_bin, ref = AGE_BIN_REF)]

# Event-study relative year (capped ±8 for the facility panel)
rel_max <- ES_END - POST_YEAR
annual_data[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max), -8L)]

# Ordered factor levels for fac_oldest_age_bin — used in HTE plots
age_bin_levels <- c("0-2yr", "3-5yr", "6-8yr", "9-11yr",
                    "12-14yr", "15-19yr", "20yr-Plus")

# Wall-type factor for HTE — restricted to the three classified categories
wall_levels <- c("All-SW", "Mixed-Wall", "All-DW")


#### S3: Helper Functions ####

theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title         = element_blank(),
      plot.subtitle      = element_blank(),
      plot.caption       = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      axis.title         = element_text(size = base_size),
      axis.text          = element_text(size = base_size - 1),
      strip.text         = element_text(face = "bold", size = base_size)
    )
}
theme_set(theme_pub())

stars_fn <- function(p) {
  fcase(is.na(p), "",
        p < 0.01, "$^{***}$",
        p < 0.05, "$^{**}$",
        p < 0.10, "$^{*}$",
        default = "")
}

extract_did <- function(m, tvar = "did_term") {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(tvar, rownames(ct), fixed = TRUE)[1]
  list(beta = ct[idx, "Estimate"],
       se   = ct[idx, "Std. Error"],
       p    = ct[idx, "Pr(>|t|)"],
       n    = nobs(m))
}

fmt_est <- function(beta, p) sprintf("%.4f%s", beta, stars_fn(p))
fmt_se  <- function(se)       sprintf("(%.4f)", se)
fmt_n   <- function(n)        format(n, big.mark = ",", scientific = FALSE)

pull_coef <- function(m, pattern) {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(pattern, rownames(ct))
  list(b  = fmt_est(ct[idx[1], "Estimate"],   ct[idx[1], "Pr(>|t|)"]),
       se = fmt_se( ct[idx[1], "Std. Error"]))
}

pt_pval <- function(m) {
  nms <- names(coef(m))
  pre <- nms[grepl("::-[2-9]|::-1[0-9]", nms)]
  suppressWarnings(wald(m, keep = pre)$p)
}

write_tex <- function(lines, filename) {
  writeLines(lines, file.path(OUTPUT_TABLES, filename))
}

# plot_es: standard event-study figure (OLS coefficients ± 95% CI)
plot_es <- function(model,
                    ylab     = "Effect on Annual Closure Probability",
                    xlim_lo  = -8L,
                    xlim_hi  = NULL,
                    filename = NULL) {
  ct <- as.data.table(tidy(model, conf.int = TRUE))
  ct <- ct[grepl("rel_year", term)]
  ct[, rel_year := as.numeric(gsub(".*::(-?[0-9]+).*", "\\1", term))]
  ct <- rbind(ct,
    data.table(term = "ref", estimate = 0, std.error = 0,
               conf.low = 0, conf.high = 0, rel_year = -1L),
    fill = TRUE)
  setorder(ct, rel_year)
  ct[, period := fcase(rel_year < -1L, "Pre",
                       rel_year == -1L, "Ref",
                       default = "Post")]
  xh <- if (is.null(xlim_hi)) max(ct$rel_year) else xlim_hi

  p <- ggplot(ct[rel_year %between% c(xlim_lo, xh)],
              aes(x = rel_year, y = estimate)) +
    annotate("rect", xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.6) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.12, fill = "grey40") +
    geom_line(color = "grey40", linewidth = 0.4, alpha = 0.6) +
    geom_point(aes(color = period), size = 2.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = period),
                  width = 0.25, linewidth = 0.5) +
    scale_color_manual(
      values = c(Pre = COL_CTRL, Post = COL_TX, Ref = "black"),
      guide  = "none") +
    labs(x = "Years Relative to Reform (1999)", y = ylab) +
    theme_pub()

  if (!is.null(filename)) {
    ggsave(filename, p, width = 10, height = 5.5, dpi = 300, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p,
           width = 10, height = 5.5, device = cairo_pdf)
  }
  invisible(list(plot = p, data = ct))
}


#### S4: Primary Sample Construction and Cell Coverage ####

# mm_fac_primary: facilities with a valid, classified make-model cell.
# Exclusion criteria (per design document Part 3.5):
#   - make_model_fac is NA         → facility had no tanks at reform date
#   - fac_wall == "Unknown-Wall"   → insurer cannot determine wall composition
#   - fac_fuel == "Unknown-Fuel"   → fuel type unclassifiable
#   - fac_oldest_age_bin is NA     → oldest tank age unavailable

mm_fac_primary <- annual_data[
  !is.na(make_model_fac)       &
  fac_wall != "Unknown-Wall"   &
  fac_fuel != "Unknown-Fuel"   &
  !is.na(fac_oldest_age_bin)
]

cat(sprintf("mm_fac_primary: %s rows, %s facilities, %s unique cells\n",
            format(nrow(mm_fac_primary),                 big.mark = ","),
            format(uniqueN(mm_fac_primary$panel_id),     big.mark = ","),
            format(uniqueN(mm_fac_primary$make_model_fac), big.mark = ",")))

# ---- Unknown-cell exclusion report ----
excl_base <- annual_data[!is.na(make_model_fac) | fac_wall == "Unknown-Wall" |
                           fac_fuel == "Unknown-Fuel"]
fwrite(
  annual_data[, .(
    N_total        = .N,
    N_unknown_wall = sum(fac_wall == "Unknown-Wall",   na.rm = TRUE),
    N_unknown_fuel = sum(fac_fuel == "Unknown-Fuel",   na.rm = TRUE),
    N_na_cell      = sum(is.na(make_model_fac)),
    pre_closure_uw = mean(closure_event[fac_wall == "Unknown-Wall"],   na.rm = TRUE),
    pre_closure_cl = mean(closure_event[fac_wall != "Unknown-Wall" &
                                          !is.na(fac_wall)], na.rm = TRUE)
  ), by = .(state_group = fifelse(texas_treated == 1, "Texas", "Control"))],
  file.path(OUTPUT_TABLES, "Diag_FacExclusion_UnknownCells.csv")
)

# ---- Cell coverage diagnostic (design document Part 5.2) ----
# Target: pct_fac_years_identified >= 70%.
fac_cell_diag <- mm_fac_primary[, .(
  n_total  = .N,
  n_tx     = sum(texas_treated == 1L),
  n_ctl    = sum(texas_treated == 0L),
  has_both = as.integer(
    sum(texas_treated == 1L) > 0L & sum(texas_treated == 0L) > 0L)
), by = .(make_model_fac, panel_year)]

cell_coverage <- fac_cell_diag[, .(
  total_cell_years         = .N,
  identified_cell_years    = sum(has_both),
  pct_identified           = round(mean(has_both) * 100, 1),
  fac_years_identified     = sum(n_total * has_both),
  pct_fac_years_identified = round(
    sum(n_total * has_both) / sum(n_total) * 100, 1)
)]

cat("Facility cell coverage:\n")
print(cell_coverage)

if (cell_coverage$pct_fac_years_identified < 70)
  warning(sprintf(
    "Only %.1f%% of facility-years in identified cells (target: 70%%).\n  Consider coarsening fac_oldest_age_bin to wider intervals.",
    cell_coverage$pct_fac_years_identified))

fwrite(cell_coverage, file.path(OUTPUT_TABLES, "Diag_FacCell_Coverage.csv"))

# Cell size table at 1998 (reform baseline year)
cell_sizes_98 <- mm_fac_primary[panel_year == 1998L, .(
  n_tx     = sum(texas_treated == 1L),
  n_ctl    = sum(texas_treated == 0L),
  n_total  = .N,
  has_both = as.integer(
    any(texas_treated == 1L) & any(texas_treated == 0L))
), by = make_model_fac][order(-n_total)]

cat(sprintf("Cells at 1998: %d total | %d identified | %d well-populated (>=5 each)\n",
            nrow(cell_sizes_98),
            cell_sizes_98[has_both == 1L, .N],
            cell_sizes_98[n_tx >= 5L & n_ctl >= 5L, .N]))

fwrite(cell_sizes_98, file.path(OUTPUT_TABLES, "Diag_FacCell_Sizes_1998.csv"))

# Pre-reform balance within cells
pre_balance <- mm_fac_primary[panel_year < POST_YEAR, .(
  closure_rate = mean(closure_event, na.rm = TRUE),
  n            = .N
), by = .(make_model_fac,
          group = fifelse(texas_treated == 1L, "Texas", "Control"))]

bal_wide <- dcast(pre_balance, make_model_fac ~ group,
                  value.var = c("closure_rate", "n"))
bal_wide[, pre_gap := closure_rate_Texas - closure_rate_Control]

n_flagged <- bal_wide[abs(pre_gap) > 0.02 |
                        n_Texas < 10L | n_Control < 10L, .N]
cat(sprintf("Cells flagged (|pre-gap| > 2pp or n < 10): %d\n", n_flagged))
fwrite(bal_wide[order(-abs(pre_gap))],
       file.path(OUTPUT_TABLES, "Diag_FacPreBalance_ByCell.csv"))

rm(fac_cell_diag, excl_base)


#### S5: Descriptive Figures ####

# Raw closure rates by cell wall composition and period
raw_trends_dt <- mm_fac_primary[
  panel_year %between% c(1992L, 2008L),
  .(closure_rate = mean(closure_event, na.rm = TRUE),
    n_fac        = uniqueN(panel_id)),
  by = .(
    panel_year,
    group    = fifelse(texas_treated == 1L, "Texas", "Control States"),
    fac_wall = fac_wall)
][n_fac >= 50]

# Keep only classified wall categories for the trend plot
raw_trends_dt <- raw_trends_dt[fac_wall %in% wall_levels]
raw_trends_dt[, fac_wall := factor(fac_wall, levels = wall_levels)]

p_raw_trends <- ggplot(
  raw_trends_dt,
  aes(x = panel_year, y = closure_rate, color = group)) +
  annotate("rect", xmin = -Inf, xmax = 1998.5,
           ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.5) +
  geom_vline(xintercept = 1998.5,
             linetype = "dashed", color = "grey30", linewidth = 0.6) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.0, fill = "white", stroke = 0.8) +
  facet_wrap(~fac_wall, ncol = 3) +
  scale_color_manual(values = c("Control States" = COL_CTRL,
                                "Texas"          = COL_TX)) +
  scale_x_continuous(breaks = seq(1992, 2008, 4)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Year", y = "Mean Annual Closure Rate") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_Raw_ClosureRates_ByWall.png"),
       p_raw_trends, width = 11, height = 4.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Raw_ClosureRates_ByWall.pdf"),
       p_raw_trends, width = 11, height = 4.5, device = cairo_pdf)

# Distribution of oldest-tank age bin at reform (1998 cross-section)
age_dist_dt <- mm_fac_primary[
  panel_year == 1998L & !is.na(fac_oldest_age_bin),
  .(panel_id, fac_oldest_age_bin,
    group = fifelse(texas_treated == 1L, "Texas", "Control States"))
]
age_dist_dt[, fac_oldest_age_bin := factor(fac_oldest_age_bin,
                                            levels = age_bin_levels)]

p_age_dist <- ggplot(age_dist_dt,
                     aes(x = fac_oldest_age_bin, fill = group)) +
  geom_bar(aes(y = after_stat(prop), group = group),
           position = position_dodge(width = 0.8),
           width = 0.7, alpha = 0.85) +
  scale_fill_manual(values = c("Texas" = COL_TX,
                                "Control States" = COL_CTRL)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Age of Oldest Tank at Reform (1998)",
       y = "Share of Facilities") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_OldestTankAge_Distribution.png"),
       p_age_dist, width = 9, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_OldestTankAge_Distribution.pdf"),
       p_age_dist, width = 9, height = 5, device = cairo_pdf)


#### S6: Parallel Trends Validation ####

# Estimate in the pre-period only; reference year = -1.
# make_model_fac^panel_year absorbs any common time trend within each cell,
# so identification comes only from within-cell TX vs CTL variation.

m_pt_main <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary[panel_year < POST_YEAR],
  cluster = ~state)

pt_pval_joint <- pt_pval(m_pt_main)
cat(sprintf("Pre-trend joint F-test p-value: %.3f\n", pt_pval_joint))

saveRDS(list(pval    = pt_pval_joint,
             n_pre   = nrow(mm_fac_primary[panel_year < POST_YEAR])),
        file.path(ANALYSIS_DIR, "ols_pt_validation.rds"))


#### S7: Headline DiD Estimates ####

# Primary specification (eq-ols-did in the empirical framework):
#   closure_event ~ alpha_i + gamma_{m(i),t} + beta * did_term + eps
# where alpha_i = facility FE, gamma = make_model_fac^panel_year FE.

m_did_main <- feols(
  closure_event ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# With time-varying age control (facility avg tank age, 3-year bins)
m_did_agectrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# Facility exit (permanent cessation of operations)
m_exit_main <- feols(
  exit_flag ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# Net tank change (disinvestment margin)
m_nettank_main <- feols(
  net_tank_change ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

d_main      <- extract_did(m_did_main)
d_agectrl   <- extract_did(m_did_agectrl)
d_exit      <- extract_did(m_exit_main)
d_nettank   <- extract_did(m_nettank_main)

pre_mean_cl <- mean(mm_fac_primary[panel_year < POST_YEAR, closure_event],
                    na.rm = TRUE)
pre_mean_ex <- mean(mm_fac_primary[panel_year < POST_YEAR, exit_flag],
                    na.rm = TRUE)
pre_mean_nt <- mean(mm_fac_primary[panel_year < POST_YEAR, net_tank_change],
                    na.rm = TRUE)

fwrite(data.table(
  Specification = c("Closure (main)",
                    "Closure (with age control)",
                    "Exit (permanent closure)",
                    "Net tank change"),
  Estimate      = round(c(d_main$beta, d_agectrl$beta,
                           d_exit$beta, d_nettank$beta), 4),
  SE            = round(c(d_main$se,   d_agectrl$se,
                           d_exit$se,   d_nettank$se), 4),
  P_value       = round(c(d_main$p,    d_agectrl$p,
                           d_exit$p,    d_nettank$p), 4),
  Pre_mean      = round(c(pre_mean_cl, pre_mean_cl,
                           pre_mean_ex, pre_mean_nt), 4),
  N             = c(d_main$n, d_agectrl$n, d_exit$n, d_nettank$n)),
  file.path(OUTPUT_TABLES, "Table_Main_DiD_Summary.csv"))

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Effect of Experience Rating on Facility Outcomes: DiD Estimates}",
  "\\label{tbl:main_did}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) \\\\",
  " & Closure & Closure + age & Exit & Net tank change \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s & %s & %s & %s \\\\",
          fmt_est(d_main$beta,    d_main$p),
          fmt_est(d_agectrl$beta, d_agectrl$p),
          fmt_est(d_exit$beta,    d_exit$p),
          fmt_est(d_nettank$beta, d_nettank$p)),
  sprintf(" & %s & %s & %s & %s \\\\",
          fmt_se(d_main$se), fmt_se(d_agectrl$se),
          fmt_se(d_exit$se), fmt_se(d_nettank$se)),
  "\\midrule",
  sprintf("Pre-reform mean & %.4f & %.4f & %.4f & %.4f \\\\",
          pre_mean_cl, pre_mean_cl, pre_mean_ex, pre_mean_nt),
  "Age control           & No  & Yes & No  & No  \\\\",
  "Facility FE           & Yes & Yes & Yes & Yes \\\\",
  "Cell $\\times$ Year FE & Yes & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
          fmt_n(d_main$n), fmt_n(d_agectrl$n),
          fmt_n(d_exit$n), fmt_n(d_nettank$n)),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.95\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Facility-level primary sample.",
  "Cell $\\times$ year fixed effects absorb common trends within each",
  "(portfolio wall $\\times$ portfolio fuel $\\times$ oldest-tank age bin) cell.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_Main_DiD.tex")


#### S8: HTE by Portfolio Wall Composition ####

# The theory predicts that All-SW facilities face the largest per-tank premium
# increase under experience rating and should show the strongest closure response.
# All-DW facilities face the smallest premium gradient.
# Per design document Section 4.4: did_term:fac_wall interaction.

m_hte_wall <- feols(
  closure_event ~ did_term:fac_wall | panel_id + make_model_fac^panel_year,
  mm_fac_primary[fac_wall %in% wall_levels],
  cluster = ~state)

m_exit_hte_wall <- feols(
  exit_flag ~ did_term:fac_wall | panel_id + make_model_fac^panel_year,
  mm_fac_primary[fac_wall %in% wall_levels],
  cluster = ~state)

# Extract HTE coefficients
wall_ct_cl <- coeftable(summary(m_hte_wall,      cluster = ~state))
wall_ct_ex <- coeftable(summary(m_exit_hte_wall, cluster = ~state))

wall_rows_cl <- lapply(wall_levels, function(w) {
  idx <- grep(paste0("fac_wall", w), rownames(wall_ct_cl), fixed = TRUE)
  if (length(idx) == 0) return(NULL)
  data.table(wall = w,
             outcome = "Closure",
             est = wall_ct_cl[idx[1], "Estimate"],
             se  = wall_ct_cl[idx[1], "Std. Error"],
             p   = wall_ct_cl[idx[1], "Pr(>|t|)"])
})
wall_rows_ex <- lapply(wall_levels, function(w) {
  idx <- grep(paste0("fac_wall", w), rownames(wall_ct_ex), fixed = TRUE)
  if (length(idx) == 0) return(NULL)
  data.table(wall = w,
             outcome = "Exit",
             est = wall_ct_ex[idx[1], "Estimate"],
             se  = wall_ct_ex[idx[1], "Std. Error"],
             p   = wall_ct_ex[idx[1], "Pr(>|t|)"])
})

wall_hte_dt <- rbindlist(c(wall_rows_cl, wall_rows_ex), fill = TRUE)
wall_hte_dt[, `:=`(
  ci_lo = est - 1.96 * se,
  ci_hi = est + 1.96 * se,
  wall  = factor(wall, levels = rev(wall_levels)),
  outcome = factor(outcome, levels = c("Closure", "Exit"))
)]

p_wall_hte <- ggplot(wall_hte_dt[!is.na(est)],
  aes(x = est, y = wall, xmin = ci_lo, xmax = ci_hi,
      color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.5),
                  size = 0.5, linewidth = 0.6) +
  scale_color_manual(values = c("Closure" = COL_TX,
                                "Exit"    = COL_CTRL)) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Effect on Annual Probability (Texas x Post)",
       y = "Portfolio Wall Composition") +
  theme_pub() +
  theme(panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3))

ggsave(file.path(OUTPUT_FIGURES, "Figure_HTE_WallType.png"),
       p_wall_hte, width = 8, height = 4, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_HTE_WallType.pdf"),
       p_wall_hte, width = 8, height = 4, device = cairo_pdf)

fwrite(wall_hte_dt[order(outcome, wall)],
       file.path(OUTPUT_TABLES, "Table_HTE_WallType.csv"))

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Reform Effects by Portfolio Wall Composition}",
  "\\label{tbl:wall_hte}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & (1) Closure & (2) Exit \\\\",
  "\\midrule",
  unlist(lapply(seq_along(wall_levels), function(i) {
    w   <- wall_levels[i]
    rcl <- wall_hte_dt[wall == w & outcome == "Closure"]
    rex <- wall_hte_dt[wall == w & outcome == "Exit"]
    c(
      sprintf("\\textit{Texas $\\times$ Post $\\times$} %s & %s & %s \\\\",
              w,
              if (nrow(rcl) > 0) fmt_est(rcl$est, rcl$p) else "---",
              if (nrow(rex) > 0) fmt_est(rex$est, rex$p) else "---"),
      sprintf(" & %s & %s \\\\",
              if (nrow(rcl) > 0) fmt_se(rcl$se) else "",
              if (nrow(rex) > 0) fmt_se(rex$se) else "")
    )
  })),
  "\\midrule",
  "Facility FE            & Yes & Yes \\\\",
  "Cell $\\times$ Year FE  & Yes & Yes \\\\",
  sprintf("Observations & %s & %s \\\\",
          fmt_n(nobs(m_hte_wall)), fmt_n(nobs(m_exit_hte_wall))),
  "\\bottomrule",
  "\\multicolumn{3}{p{0.75\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Facility-level primary sample.",
  "Mixed-Wall facilities serve as the implicit baseline in the interaction.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_HTE_WallType.tex")


#### S9: HTE by Oldest-Tank Age at Reform ####

# The premium gradient under experience rating is steepest for the oldest
# tank. Facilities whose oldest tank was near the end of its actuarial life
# at reform should show the strongest closure response.
# Per design document Section 4.4: did_term:fac_oldest_age_bin interaction.

mm_fac_primary[, fac_oldest_age_bin := factor(fac_oldest_age_bin,
                                               levels = age_bin_levels)]

m_hte_age <- feols(
  closure_event ~ did_term:fac_oldest_age_bin |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary,
  cluster = ~state)

m_exit_hte_age <- feols(
  exit_flag ~ did_term:fac_oldest_age_bin |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary,
  cluster = ~state)

age_ct_cl <- coeftable(summary(m_hte_age,      cluster = ~state))
age_ct_ex <- coeftable(summary(m_exit_hte_age, cluster = ~state))

age_rows_cl <- lapply(age_bin_levels, function(b) {
  idx <- grep(b, rownames(age_ct_cl), fixed = TRUE)
  if (length(idx) == 0) return(NULL)
  data.table(age_bin = b, outcome = "Closure",
             est = age_ct_cl[idx[1], "Estimate"],
             se  = age_ct_cl[idx[1], "Std. Error"],
             p   = age_ct_cl[idx[1], "Pr(>|t|)"])
})
age_rows_ex <- lapply(age_bin_levels, function(b) {
  idx <- grep(b, rownames(age_ct_ex), fixed = TRUE)
  if (length(idx) == 0) return(NULL)
  data.table(age_bin = b, outcome = "Exit",
             est = age_ct_ex[idx[1], "Estimate"],
             se  = age_ct_ex[idx[1], "Std. Error"],
             p   = age_ct_ex[idx[1], "Pr(>|t|)"])
})

age_hte_dt <- rbindlist(c(age_rows_cl, age_rows_ex), fill = TRUE)
age_hte_dt[, `:=`(
  ci_lo   = est - 1.96 * se,
  ci_hi   = est + 1.96 * se,
  age_bin = factor(age_bin, levels = age_bin_levels),
  outcome = factor(outcome, levels = c("Closure", "Exit"))
)]

p_age_hte <- ggplot(age_hte_dt[!is.na(est)],
  aes(x = age_bin, y = est,
      ymin = ci_lo, ymax = ci_hi,
      color = outcome, group = outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 2.5) +
  geom_errorbar(width = 0.25, linewidth = 0.5) +
  scale_color_manual(values = c("Closure" = COL_TX,
                                "Exit"    = COL_CTRL)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Age of Oldest Tank at Reform",
       y = "Effect on Annual Probability (Texas x Post)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_HTE_OldestTankAge.png"),
       p_age_hte, width = 9, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_HTE_OldestTankAge.pdf"),
       p_age_hte, width = 9, height = 5, device = cairo_pdf)

fwrite(age_hte_dt[order(outcome, age_bin)],
       file.path(OUTPUT_TABLES, "Table_HTE_OldestTankAge.csv"))


#### S10: Event Study Figures ####

# Full post-period event study — closure outcome
m_es_main <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary,
  cluster = ~state)

plot_es(
  model    = m_es_main,
  ylab     = "Effect on Annual Closure Probability",
  xlim_lo  = -8L,
  xlim_hi  = rel_max,
  filename = file.path(OUTPUT_FIGURES, "Figure_ES_Closure_FacMM.png"))

# Exit outcome event study
m_es_exit <- feols(
  exit_flag ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary,
  cluster = ~state)

plot_es(
  model    = m_es_exit,
  ylab     = "Effect on Annual Facility Exit Probability",
  xlim_lo  = -8L,
  xlim_hi  = rel_max,
  filename = file.path(OUTPUT_FIGURES, "Figure_ES_Exit_FacMM.png"))

# Combined panel figure
es_cl_out   <- plot_es(m_es_main, xlim_lo = -8L, xlim_hi = rel_max)
es_exit_out <- plot_es(m_es_exit, xlim_lo = -8L, xlim_hi = rel_max)

p_es_combined <- (
  es_cl_out$plot +
    labs(y = "Closure probability") +
    annotate("text", x = -Inf, y = Inf, label = "A",
             hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5)
) / (
  es_exit_out$plot +
    labs(y = "Exit probability") +
    annotate("text", x = -Inf, y = Inf, label = "B",
             hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5)
)
ggsave(file.path(OUTPUT_FIGURES, "Figure_ES_Combined_FacMM.png"),
       p_es_combined, width = 10, height = 10, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_ES_Combined_FacMM.pdf"),
       p_es_combined, width = 10, height = 10, device = cairo_pdf)


#### S11: Reported Leak DiD ####

# LUST reports are filed at the facility level (no tank-level attribution
# exists in the administrative data). The facility is the appropriate unit.

m_leak_main <- feols(
  leak_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_leak_agectrl <- feols(
  leak_year ~ did_term + age_bin | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_es_leak <- feols(
  leak_year ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

plot_es(
  model    = m_es_leak,
  ylab     = "Effect on Annual Reported Leak Probability",
  xlim_lo  = -8L,
  xlim_hi  = rel_max,
  filename = file.path(OUTPUT_FIGURES, "Figure_ES_Leak_FacMM.png"))

d_leak    <- extract_did(m_leak_main)
d_leak_ac <- extract_did(m_leak_agectrl)

pre_mean_leak <- mean(mm_fac_primary[panel_year < POST_YEAR, leak_year],
                      na.rm = TRUE)

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Annual Reported Leak Probability: DiD Estimates}",
  "\\label{tbl:leak_did}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & (1) & (2) \\\\",
  " & Leak DiD & Leak DiD + age control \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s & %s \\\\",
          fmt_est(d_leak$beta, d_leak$p),
          fmt_est(d_leak_ac$beta, d_leak_ac$p)),
  sprintf(" & %s & %s \\\\",
          fmt_se(d_leak$se), fmt_se(d_leak_ac$se)),
  "\\midrule",
  sprintf("Pre-reform mean & \\multicolumn{2}{c}{%.4f} \\\\", pre_mean_leak),
  "Age control            & No  & Yes \\\\",
  "Facility FE            & Yes & Yes \\\\",
  "Cell $\\times$ Year FE  & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s \\\\",
          fmt_n(d_leak$n), fmt_n(d_leak_ac$n)),
  "\\bottomrule",
  "\\multicolumn{3}{p{0.75\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Facility-level primary sample.",
  "Outcome is an indicator for a confirmed underground storage tank",
  "release in the facility-year. LUST data are reported at the",
  "facility level; no tank-level attribution is available.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_Leak_DiD.tex")


#### S12: Age at Closure ####

# Uses the closed-tank cross-section. Outcome = age_at_closure (years).
# County and year fixed effects absorb local market conditions.
# The DiD estimand here is: did Texas close tanks at a different age
# post-reform relative to control states, over and above any pre-reform gap?

closed_tanks <- merge(
  closed_tanks,
  unique(mm_fac_primary[, .(panel_id)]),
  by = "panel_id")   # restrict to primary-sample facilities

acl_data <- closed_tanks[
  !is.na(age_at_closure) &
  !is.na(county_fips_fac) &
  closure_year %between% c(PANEL_START, PANEL_END),
  .(panel_id, age_at_closure, closure_year, state, county_fips_fac)
][, `:=`(
  texas      = as.integer(state == "TX"),
  post       = as.integer(closure_year >= POST_YEAR),
  texas_post = as.integer(state == "TX") *
               as.integer(closure_year >= POST_YEAR)
)]

m_acl_main <- feols(
  age_at_closure ~ texas_post | county_fips_fac + closure_year,
  acl_data, cluster = ~state)

m_acl_pre <- feols(
  age_at_closure ~ texas | county_fips_fac + closure_year,
  acl_data[post == 0L], cluster = ~state)

d_acl_main <- extract_did(m_acl_main, tvar = "texas_post")
d_acl_pre  <- extract_did(m_acl_pre,  tvar = "texas")

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Tank Age at Closure: DiD Estimates}",
  "\\label{tbl:age_at_closure}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & (1) & (2) \\\\",
  " & Post-reform DiD & Pre-reform falsification \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s & --- \\\\",
          fmt_est(d_acl_main$beta, d_acl_main$p)),
  sprintf(" & %s & \\\\", fmt_se(d_acl_main$se)),
  sprintf("Texas (pre-reform) & --- & %s \\\\",
          fmt_est(d_acl_pre$beta, d_acl_pre$p)),
  sprintf(" & & %s \\\\", fmt_se(d_acl_pre$se)),
  "\\midrule",
  "County FE          & Yes & Yes \\\\",
  "Year FE            & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s \\\\",
          fmt_n(d_acl_main$n), fmt_n(d_acl_pre$n)),
  "\\bottomrule",
  "\\multicolumn{3}{p{0.75\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Closed-tank records matched to primary-sample facilities.",
  "Outcome is tank age in years at the time of closure.",
  "Column~(2) uses pre-reform years only as a falsification check.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_AgeAtClosure_DiD.tex")

# Age-at-closure time series (raw means with 3-year moving average)
age_ts_dt <- acl_data[
  closure_year %between% c(PANEL_START, PANEL_END) & !is.na(age_at_closure),
  .(mean_age = mean(age_at_closure, na.rm = TRUE), n = .N),
  by = .(closure_year,
         group = fifelse(state == "TX", "Texas", "Control States"))
]
setorder(age_ts_dt, group, closure_year)
age_ts_dt[, mean_age_smooth := frollmean(mean_age, n = 3, align = "center"),
           by = group]

p_acl_ts <- ggplot(age_ts_dt,
  aes(x = closure_year, y = mean_age, color = group)) +
  geom_vline(xintercept = POST_YEAR - 0.5,
             linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.45, size = 1.8) +
  geom_line(aes(y = mean_age_smooth), linewidth = 1, na.rm = TRUE) +
  scale_color_manual(values = c("Texas" = COL_TX,
                                "Control States" = COL_CTRL)) +
  scale_x_continuous(breaks = seq(PANEL_START, PANEL_END, 4)) +
  labs(x = "Year of Closure",
       y = "Mean Tank Age at Closure (years)") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_AgeAtClosure_TimeSeries.png"),
       p_acl_ts, width = 9, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_AgeAtClosure_TimeSeries.pdf"),
       p_acl_ts, width = 9, height = 5, device = cairo_pdf)


#### S13: Robustness Checks ####

# 13a: Drop border states (Threat 6 in design document)
# OK, AR, LA, NM border Texas and may be contaminated via insurer entry
# or contractor market spillovers.
BORDER_STATES <- c("OK", "AR", "LA", "NM")

m_rob_noborder <- feols(
  closure_event ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary[!state %in% BORDER_STATES],
  cluster = ~state)

# 13b: Add mandate-period control (Threat 2 in design document)
# Texas imposed a tank upgrade mandate 1989-1993; mandate_active marks
# facility-years where this was operative for pre-1989 vintage tanks.
m_rob_mandate <- feols(
  closure_event ~ did_term + mandate_active |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# 13c: 3-dimension cell (drop oldest-tank age bin; Threat 7)
# If the age-bin measurement is noisy, the 3-dim cell (wall x fuel only)
# and 4-dim estimates should be similar. Divergence flags measurement error.
mm_fac_primary[, make_model_3dim := paste(fac_wall, fac_fuel, sep = "_")]

m_rob_3dim <- feols(
  closure_event ~ did_term | panel_id + make_model_3dim^panel_year,
  mm_fac_primary, cluster = ~state)

# 13d: Tighter installation window (1992-1997 only)
m_rob_tight <- feols(
  closure_event ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary[!is.na(facility_first_install_yr) &
                   facility_first_install_yr %between% c(1992L, 1997L)],
  cluster = ~state)

d_main      <- extract_did(m_did_main)
d_noborder  <- extract_did(m_rob_noborder)
d_mandate   <- extract_did(m_rob_mandate)
d_3dim      <- extract_did(m_rob_3dim)
d_tight     <- extract_did(m_rob_tight)

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Annual Closure Probability: Robustness to Alternative Specifications}",
  "\\label{tbl:robustness_ols}",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) & (5) \\\\",
  " & Baseline & No border & Mandate ctrl & 3-dim cell & Install 1992--1997 \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s & %s & %s & %s & %s \\\\",
          fmt_est(d_main$beta,     d_main$p),
          fmt_est(d_noborder$beta, d_noborder$p),
          fmt_est(d_mandate$beta,  d_mandate$p),
          fmt_est(d_3dim$beta,     d_3dim$p),
          fmt_est(d_tight$beta,    d_tight$p)),
  sprintf(" & %s & %s & %s & %s & %s \\\\",
          fmt_se(d_main$se),    fmt_se(d_noborder$se),
          fmt_se(d_mandate$se), fmt_se(d_3dim$se),
          fmt_se(d_tight$se)),
  "\\midrule",
  "Facility FE           & Yes & Yes & Yes & Yes & Yes \\\\",
  "Cell $\\times$ Year FE & Yes & Yes & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s & %s \\\\",
          fmt_n(d_main$n),    fmt_n(d_noborder$n),
          fmt_n(d_mandate$n), fmt_n(d_3dim$n),
          fmt_n(d_tight$n)),
  "\\bottomrule",
  "\\multicolumn{6}{p{0.98\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Facility-level primary sample unless noted.",
  "Column~(2) drops the four states bordering Texas.",
  "Column~(3) adds an indicator for active state upgrade mandate periods.",
  "Column~(4) uses a 3-dimension cell (wall $\\times$ fuel only, no age bin).",
  "Column~(5) restricts to facilities first installed between 1992 and 1997.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_Robustness_OLS.tex")

fwrite(data.table(
  Specification = c("Baseline",
                    "Drop border states",
                    "Add mandate control",
                    "3-dimension cell",
                    "Install 1992-1997"),
  Estimate  = round(c(d_main$beta, d_noborder$beta, d_mandate$beta,
                       d_3dim$beta, d_tight$beta), 4),
  SE        = round(c(d_main$se,   d_noborder$se,   d_mandate$se,
                       d_3dim$se,   d_tight$se), 4),
  P_value   = round(c(d_main$p,    d_noborder$p,    d_mandate$p,
                       d_3dim$p,    d_tight$p), 4),
  N_obs     = c(d_main$n, d_noborder$n, d_mandate$n, d_3dim$n, d_tight$n)),
  file.path(OUTPUT_TABLES, "Table_Robustness_OLS.csv"))


#### S14: Save Samples and Model Objects ####

saveRDS(mm_fac_primary, file.path(ANALYSIS_DIR, "mm_fac_primary.rds"))

saveRDS(m_did_main,    file.path(ANALYSIS_DIR, "ols_headline_did.rds"))
saveRDS(m_es_main,     file.path(ANALYSIS_DIR, "ols_headline_es.rds"))
saveRDS(m_exit_main,   file.path(ANALYSIS_DIR, "ols_exit_did.rds"))
saveRDS(m_leak_main,   file.path(ANALYSIS_DIR, "ols_leak_did.rds"))
saveRDS(m_hte_wall,    file.path(ANALYSIS_DIR, "ols_hte_wall.rds"))
saveRDS(m_hte_age,     file.path(ANALYSIS_DIR, "ols_hte_age.rds"))

cat("02a_DiD_OLS.R complete.\n")
cat(sprintf("  Primary sample: %s facility-years, %s facilities\n",
            fmt_n(nrow(mm_fac_primary)),
            fmt_n(uniqueN(mm_fac_primary$panel_id))))
cat(sprintf("  Make-model cells: %s unique\n",
            fmt_n(uniqueN(mm_fac_primary$make_model_fac))))