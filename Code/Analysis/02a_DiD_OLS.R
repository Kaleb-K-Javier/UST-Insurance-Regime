################################################################################
# 02a_DiD_OLS.R
# Texas UST Insurance Reform — OLS Difference-in-Differences
#
# PREREQUISITES: 01_BuildSamples.R
#
# SECTIONS:
#   S1   Setup and Data Loading
#   S2   Variable Construction
#   S3   Helper Functions
#   S4   Sample Construction
#   S5   Descriptive Figures
#   S6   Parallel Trends Validation
#   S7   Main DiD Estimates
#   S8   Age-at-Treatment Heterogeneous Effects
#   S9   HTE Event Study and Figures
#   S10  Age-Bin Coefficient Plot
#   S11  Robustness: Group-Specific Year Fixed Effects
#   S12  Youngest Subsample
#   S13  Oldest Subsample
#   S14  Combined Event Study Figure
#   S15  Reported Leak DiD
#   S16  Wall Type Heterogeneity
#   S17  Age at Closure
#   S18  Theory-Evidence Summary
#   S19  Robustness Checks
#   S20  Save Samples and Model Objects
################################################################################


#### S1 Setup and Data Loading ####

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

POST_YEAR      <- 1999L
TREATMENT_YEAR <- 1999L
ES_END         <- 2018L
PANEL_START    <- 1990L
PANEL_END      <- 2018L
AGE_BIN_BREAKS <- c(0, 5, 10, 15, 20, 25, Inf)
AGE_BIN_LABELS <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25+")
AGE_BIN_REF    <- "0-4"

COL_TX    <- "#D55E00"
COL_CTRL  <- "#0072B2"
COL_YOUNG <- "#009E73"
COL_OLD   <- "#CC79A7"

annual_data  <- readRDS(file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))
closed_tanks <- readRDS(file.path(ANALYSIS_DIR, "analysis_closed_tanks.rds"))
tanks_1999   <- readRDS(file.path(ANALYSIS_DIR, "analysis_tanks_1999.rds"))


#### S2 Variable Construction ####

tanks_1999[, vc := fcase(
  install_year < 1965,                  "Pre-1965",
  install_year %between% c(1965, 1974), "1965-1974",
  install_year %between% c(1975, 1979), "1975-1979",
  install_year %between% c(1980, 1984), "1980-1984",
  install_year %between% c(1985, 1988), "1985-1988",
  default =                              "Post-1988")]

fac_vc <- tanks_1999[,
  .(vintage_cohort = names(which.max(table(vc)))),
  by = panel_id]
annual_data <- merge(annual_data, fac_vc, by = "panel_id", all.x = TRUE)
annual_data[is.na(vintage_cohort), vintage_cohort := "Post-1988"]

vc_levels <- c("Pre-1965","1965-1974","1975-1979",
               "1980-1984","1985-1988","Post-1988")
annual_data[, vintage_cohort := factor(vintage_cohort, levels = vc_levels)]

annual_data[, age_bin := factor(
  cut(avg_tank_age, breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS,
      right = FALSE, include.lowest = TRUE),
  levels = AGE_BIN_LABELS)]
annual_data[, age_bin := relevel(age_bin, ref = AGE_BIN_REF)]

annual_data[, age_treat_bin := cut(
  mean_age_1998,
  breaks = c(0, 5, 6, 9, Inf),
  labels = c("1-4 years", "5 years", "6-8 years", "9+ years"),
  right  = FALSE, include.lowest = TRUE)]
annual_data[, age_treat_bin := relevel(factor(age_treat_bin),
                                        ref = "1-4 years")]

wall_col <- intersect(
  c("pct_single_wall", "has_single_walled", "any_single_walled"),
  names(annual_data))[1]

rel_max_full     <- ES_END - POST_YEAR
rel_min_youngest <- 1994L - POST_YEAR
rel_min_oldest   <- -5L


#### S3 Helper Functions ####

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
        p < 0.01,  "$^{***}$",
        p < 0.05,  "$^{**}$",
        p < 0.10,  "$^{*}$",
        default =  "")
}

extract_did <- function(m, tvar = "did_term") {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(tvar, rownames(ct), fixed = TRUE)[1]
  list(beta = ct[idx, "Estimate"],
       se   = ct[idx, "Std. Error"],
       p    = ct[idx, "Pr(>|t|)"],
       n    = nobs(m))
}

fmt_est <- function(beta, p)  sprintf("%.4f%s", beta, stars_fn(p))
fmt_se  <- function(se)       sprintf("(%.4f)", se)
fmt_n   <- function(n)        format(n, big.mark = ",", scientific = FALSE)

pull_coef <- function(m, pattern) {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(pattern, rownames(ct))
  list(b  = fmt_est(ct[idx[1], "Estimate"], ct[idx[1], "Pr(>|t|)"]),
       se = fmt_se(ct[idx[1], "Std. Error"]))
}

extract_att_from_es <- function(m) {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep("rel_year_bin::[^-][0-9]*:texas_treated", rownames(ct))
  list(beta = mean(ct[idx, "Estimate"],   na.rm = TRUE),
       se   = mean(ct[idx, "Std. Error"], na.rm = TRUE),
       n    = nobs(m))
}

pt_pval <- function(m) {
  nms <- names(coef(m))
  pre <- nms[grepl("::-[2-9]|::-1[0-9]", nms)]
  suppressWarnings(wald(m, keep = pre)$p)
}

texas_share_diag <- function(dt) {
  dt[, .(texas_share = mean(texas_treated, na.rm = TRUE)), by = rel_year_bin]
}

plot_es <- function(model, ylab = "Effect on Annual Closure Probability",
                    xlim_lo = NULL, xlim_hi = NULL, filename = NULL) {
  ct <- as.data.table(tidy(model, conf.int = TRUE))
  ct <- ct[grepl("rel_year|event_time", term)]
  ct[, rel_year := as.numeric(gsub(".*::(-?[0-9]+).*", "\\1", term))]
  ct <- rbind(ct,
    data.table(term = "ref", estimate = 0, std.error = 0,
               conf.low = 0, conf.high = 0, rel_year = -1L),
    fill = TRUE)
  setorder(ct, rel_year)
  ct[, period := fcase(rel_year < 0,   "Pre",
                       rel_year == -1, "Ref",
                       default =        "Post")]
  xl <- if (is.null(xlim_lo)) min(ct$rel_year) else xlim_lo
  xh <- if (is.null(xlim_hi)) max(ct$rel_year) else xlim_hi

  p <- ggplot(ct[rel_year %between% c(xl, xh)],
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

plot_es_hte <- function(model, xlim_lo = -8, xlim_hi = NULL,
                        filename = NULL) {
  ct <- as.data.table(tidy(model, conf.int = TRUE))

  young <- ct[grepl(":texas_treated$", term) & grepl("rel_year_bin", term)]
  young[, rel_year := as.numeric(
    gsub("rel_year_bin::(-?[0-9]+):.*", "\\1", term))]
  young[, group := "Young (5 years or less at reform)"]
  young[, `:=`(conf.low  = estimate - 1.96 * std.error,
               conf.high = estimate + 1.96 * std.error)]

  diff_dt <- ct[grepl(":tx_old$", term) & grepl("rel_year_bin", term)]
  diff_dt[, rel_year := as.numeric(
    gsub("rel_year_bin::(-?[0-9]+):.*", "\\1", term))]

  old <- merge(
    young[,   .(rel_year, base = estimate, base_se = std.error)],
    diff_dt[, .(rel_year, diff = estimate, diff_se = std.error)],
    by = "rel_year", all.x = TRUE)
  old[is.na(diff), `:=`(diff = 0, diff_se = 0)]
  old[, `:=`(
    estimate  = base + diff,
    conf.low  = (base + diff) - 1.96 * sqrt(base_se^2 + diff_se^2),
    conf.high = (base + diff) + 1.96 * sqrt(base_se^2 + diff_se^2),
    group     = "Old (over 5 years at reform)")]

  ref_rows <- data.table(
    rel_year = -1L, estimate = 0, conf.low = 0, conf.high = 0,
    group = c("Young (5 years or less at reform)",
              "Old (over 5 years at reform)"))

  plot_dt <- rbind(
    young[, .(rel_year, estimate, conf.low, conf.high, group)],
    old[,   .(rel_year, estimate, conf.low, conf.high, group)],
    ref_rows, fill = TRUE)
  setorder(plot_dt, group, rel_year)
  xh <- if (is.null(xlim_hi)) max(plot_dt$rel_year) else xlim_hi

  p <- ggplot(plot_dt[rel_year %between% c(xlim_lo, xh)],
              aes(x = rel_year, y = estimate,
                  color = group, fill = group)) +
    annotate("rect", xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    scale_color_manual(values = c(
      "Young (5 years or less at reform)" = COL_YOUNG,
      "Old (over 5 years at reform)"      = COL_OLD)) +
    scale_fill_manual(values = c(
      "Young (5 years or less at reform)" = COL_YOUNG,
      "Old (over 5 years at reform)"      = COL_OLD)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(x = "Years Relative to Reform (1999)",
         y = "Effect on Annual Closure Probability") +
    theme_pub()

  if (!is.null(filename)) {
    ggsave(filename, p, width = 10, height = 5.5, dpi = 300, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p,
           width = 10, height = 5.5, device = cairo_pdf)
  }
  invisible(list(plot = p, data = plot_dt))
}

plot_es_split <- function(m_young, m_old, xlim_lo = -8,
                          xlim_hi = NULL, filename = NULL) {
  extract_path <- function(m, grp_label) {
    ct <- as.data.table(tidy(m, conf.int = TRUE))
    ct <- ct[grepl("rel_year_bin", term)]
    ct[, rel_year := as.numeric(
      gsub("rel_year_bin::(-?[0-9]+):.*", "\\1", term))]
    ct[, group    := grp_label]
    ct[, `:=`(conf.low  = estimate - 1.96 * std.error,
              conf.high = estimate + 1.96 * std.error)]
    ct[, .(rel_year, estimate, conf.low, conf.high, group)]
  }
  plot_dt <- rbind(
    extract_path(m_young, "Young (5 years or less at reform)"),
    extract_path(m_old,   "Old (over 5 years at reform)"),
    data.table(rel_year = -1L, estimate = 0, conf.low = 0, conf.high = 0,
               group = c("Young (5 years or less at reform)",
                         "Old (over 5 years at reform)")),
    fill = TRUE)
  setorder(plot_dt, group, rel_year)
  xh <- if (is.null(xlim_hi)) max(plot_dt$rel_year) else xlim_hi

  p <- ggplot(plot_dt[rel_year %between% c(xlim_lo, xh)],
              aes(x = rel_year, y = estimate,
                  color = group, fill = group)) +
    annotate("rect", xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    scale_color_manual(values = c(
      "Young (5 years or less at reform)" = COL_YOUNG,
      "Old (over 5 years at reform)"      = COL_OLD)) +
    scale_fill_manual(values = c(
      "Young (5 years or less at reform)" = COL_YOUNG,
      "Old (over 5 years at reform)"      = COL_OLD)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(x = "Years Relative to Reform (1999)",
         y = "Effect on Annual Closure Probability") +
    theme_pub()

  if (!is.null(filename)) {
    ggsave(filename, p, width = 10, height = 5.5, dpi = 300, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p,
           width = 10, height = 5.5, device = cairo_pdf)
  }
  invisible(list(plot = p, data = plot_dt))
}

write_tex <- function(lines, filename) {
  writeLines(lines, file.path(OUTPUT_TABLES, filename))
}


#### S4 Sample Construction ####

main_sample <- annual_data[
  single_tanks      == active_tanks &
  has_gasoline_year == 1            &
  install_year      >  1989         &
  install_year      <= 1997
]

sw_broader_sample <- annual_data[
  has_single_walled == 1 &
  install_year      >  1989 &
  install_year      <= 1997
]

main_sample[, old_at_treat := as.integer(mean_age_1998 > 5)]
main_sample[, tx_old       := texas_treated * old_at_treat]
main_sample[, age_grp      := fifelse(mean_age_1998 <= 5, "Young", "Old")]

youngest_sample <- main_sample[mean_age_1998 <= 5]
oldest_sample   <- main_sample[mean_age_1998 >  5]

main_sample[,     rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), -8L)]
youngest_sample[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full),
                                        rel_min_youngest)]
oldest_sample[,   rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full),
                                        rel_min_oldest)]
sw_broader_sample[, rel_year_bin :=
                    pmax(pmin(rel_year_1999, rel_max_full), -8L)]


#### S5 Descriptive Figures ####

age_dist_dt <- main_sample[
  panel_year == 1998 & !is.na(mean_age_1998),
  .(panel_id, mean_age_1998,
    group = fifelse(texas_treated == 1, "Texas", "Control States"))
]

p_age_dist <- ggplot(age_dist_dt,
                     aes(x = mean_age_1998, fill = group)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("Texas" = COL_TX,
                                "Control States" = COL_CTRL)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Mean Tank Age at Reform (years)", y = "Density") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_Age_Distribution_1998.png"),
       p_age_dist, width = 8, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Age_Distribution_1998.pdf"),
       p_age_dist, width = 8, height = 5, device = cairo_pdf)

raw_trends_dt <- main_sample[
  panel_year %between% c(1992L, 2005L) & !is.na(mean_age_1998),
  .(closure_rate = mean(closure_event, na.rm = TRUE),
    n_fac        = uniqueN(panel_id)),
  by = .(
    panel_year,
    group   = fifelse(texas_treated == 1, "Texas", "Control States"),
    age_grp = fifelse(mean_age_1998 <= 5,
                      "Young (5 years or less at reform)",
                      "Old (over 5 years at reform)"))
]

p_raw_trends <- ggplot(
  raw_trends_dt[n_fac >= 200],
  aes(x = panel_year, y = closure_rate,
      color = group, linetype = age_grp, shape = age_grp)) +
  annotate("rect", xmin = -Inf, xmax = 1998.5,
           ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.5) +
  geom_vline(xintercept = 1998.5, linetype = "dashed",
             color = "grey30", linewidth = 0.6) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2, fill = "white", stroke = 0.8) +
  scale_color_manual(values = c("Control States" = COL_CTRL,
                                "Texas"          = COL_TX)) +
  scale_linetype_manual(values = c(
    "Young (5 years or less at reform)" = "dashed",
    "Old (over 5 years at reform)"      = "solid")) +
  scale_shape_manual(values = c(
    "Young (5 years or less at reform)" = 21,
    "Old (over 5 years at reform)"      = 19)) +
  scale_x_continuous(breaks = seq(1992, 2005, 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Year", y = "Mean Annual Closure Rate") +
  theme_pub() +
  theme(legend.key.width = unit(1.5, "cm"))

ggsave(file.path(OUTPUT_FIGURES, "Figure_Raw_ClosureRates_ByAge.png"),
       p_raw_trends, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Raw_ClosureRates_ByAge.pdf"),
       p_raw_trends, width = 9, height = 5.5, device = cairo_pdf)


#### S6 Parallel Trends Validation ####

m_pt_main <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  main_sample[panel_year < POST_YEAR],
  cluster = ~state)

pt_pval_joint <- pt_pval(m_pt_main)
pt_n_pre_obs  <- nrow(main_sample[panel_year < POST_YEAR])

saveRDS(list(pval = pt_pval_joint, n_pre_obs = pt_n_pre_obs),
        file.path(ANALYSIS_DIR, "ols_pt_validation.rds"))


#### S7 Main DiD Estimates ####

m_did_pooled  <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  main_sample, cluster = ~state)

m_did_agectrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  main_sample, cluster = ~state)

d_pooled  <- extract_did(m_did_pooled)
d_agectrl <- extract_did(m_did_agectrl)

pre_mean_full <- mean(
  main_sample[panel_year < TREATMENT_YEAR, closure_event], na.rm = TRUE)

fwrite(data.table(
  Specification = c("TWFE DiD", "TWFE DiD with age controls"),
  Estimate      = round(c(d_pooled$beta,  d_agectrl$beta), 4),
  SE            = round(c(d_pooled$se,    d_agectrl$se),   4),
  P_value       = round(c(d_pooled$p,     d_agectrl$p),    4),
  Pre_mean      = round(pre_mean_full, 4),
  N             = c(d_pooled$n, d_agectrl$n)),
  file.path(OUTPUT_TABLES, "Table_Main_DiD_Summary.csv"))


#### S8 Age-at-Treatment Heterogeneous Effects ####

m_did_young <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  main_sample[old_at_treat == 0], cluster = ~state)

m_did_old <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  main_sample[old_at_treat == 1], cluster = ~state)

m_did_interact <- feols(
  closure_event ~ did_term + did_term:old_at_treat | panel_id + panel_year,
  main_sample, cluster = ~state)

m_did_interact_agectrl <- feols(
  closure_event ~ did_term + did_term:old_at_treat + age_bin |
    panel_id + panel_year,
  main_sample, cluster = ~state)

m_did_4bin_hte <- feols(
  closure_event ~ did_term:age_treat_bin + age_bin | panel_id + panel_year,
  main_sample, cluster = ~state)

d_young <- extract_did(m_did_young)
d_old   <- extract_did(m_did_old)

pre_mean_young <- mean(
  main_sample[old_at_treat == 0 & panel_year < TREATMENT_YEAR, closure_event],
  na.rm = TRUE)
pre_mean_old <- mean(
  main_sample[old_at_treat == 1 & panel_year < TREATMENT_YEAR, closure_event],
  na.rm = TRUE)

young_est     <- fmt_est(d_young$beta, d_young$p)
young_se      <- fmt_se(d_young$se)
old_est       <- fmt_est(d_old$beta,   d_old$p)
old_se        <- fmt_se(d_old$se)
int_did_b     <- pull_coef(m_did_interact,         "^did_term$")$b
int_did_se    <- pull_coef(m_did_interact,         "^did_term$")$se
intage_did_b  <- pull_coef(m_did_interact_agectrl, "^did_term$")$b
intage_did_se <- pull_coef(m_did_interact_agectrl, "^did_term$")$se
int_old_b     <- pull_coef(m_did_interact,         "old_at_treat")$b
int_old_se    <- pull_coef(m_did_interact,         "old_at_treat")$se
intage_old_b  <- pull_coef(m_did_interact_agectrl, "old_at_treat")$b
intage_old_se <- pull_coef(m_did_interact_agectrl, "old_at_treat")$se
n_young       <- fmt_n(d_young$n)
n_old_dt      <- fmt_n(d_old$n)
n_interact    <- fmt_n(nobs(m_did_interact))
n_intage      <- fmt_n(nobs(m_did_interact_agectrl))
pre_y_fmt     <- sprintf("%.4f", pre_mean_young)
pre_o_fmt     <- sprintf("%.4f", pre_mean_old)

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Annual Closure Probability: Treatment Effects by Age at Reform}",
  "\\label{tbl:old_young_did}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) \\\\",
  " & Young only & Old only & Interaction & Interaction + age \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s & %s & %s & %s \\\\",
          young_est, old_est, int_did_b, intage_did_b),
  sprintf(" & %s & %s & %s & %s \\\\",
          young_se, old_se, int_did_se, intage_did_se),
  sprintf("\\quad $\\times$ Old at reform & & & %s & %s \\\\",
          int_old_b, intage_old_b),
  sprintf(" & & & %s & %s \\\\", int_old_se, intage_old_se),
  "\\midrule",
  sprintf("Pre-reform mean & %s & %s & \\multicolumn{2}{c}{---} \\\\",
          pre_y_fmt, pre_o_fmt),
  "Facility fixed effects & Yes & Yes & Yes & Yes \\\\",
  "Year fixed effects     & Yes & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
          n_young, n_old_dt, n_interact, n_intage),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.95\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Make-model sample: single-product gasoline facilities",
  "installed 1990--1997, excluding mandate cohorts.",
  "Columns~(1) and (2) estimate the model on split subsamples.",
  "Columns~(3) and (4) interact the treatment indicator with an indicator",
  "for mean tank age greater than five years at reform.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_OldYoung_DiD.tex")

bin_rows   <- lapply(
  c("1-4 years", "5 years", "6-8 years", "9\\+"),
  function(lbl) pull_coef(m_did_4bin_hte,
                           sprintf("did_term:age_treat_bin%s", lbl)))
bin_labels <- c("1--4 years", "5 years", "6--8 years", "9 or more years")
n_4bin     <- fmt_n(nobs(m_did_4bin_hte))

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Annual Closure Probability: Treatment Effects by Age-at-Reform Group}",
  "\\label{tbl:age_treat_bins}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "Mean tank age at reform & Estimate & Standard error \\\\",
  "\\midrule",
  unlist(mapply(function(lbl, row) {
    c(sprintf("%s & %s & %s \\\\", lbl, row$b, row$se))
  }, bin_labels, bin_rows, SIMPLIFY = FALSE)),
  "\\midrule",
  "Age controls           & \\multicolumn{2}{c}{Yes} \\\\",
  "Facility fixed effects & \\multicolumn{2}{c}{Yes} \\\\",
  "Year fixed effects     & \\multicolumn{2}{c}{Yes} \\\\",
  sprintf("Observations & \\multicolumn{2}{c}{%s} \\\\", n_4bin),
  "\\bottomrule",
  "\\multicolumn{3}{p{0.60\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Make-model sample.",
  "Each row reports the average treatment effect for facilities",
  "in that age-at-reform group.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_AgeBin_HTE.tex")


#### S9 HTE Event Study and Figures ####

m_es_hte <- feols(
  closure_event ~
    i(rel_year_bin, texas_treated, ref = -1) +
    i(rel_year_bin, tx_old,        ref = -1) |
    panel_id + panel_year,
  main_sample, cluster = ~state)

plot_es_hte(
  model    = m_es_hte,
  xlim_lo  = -8,
  xlim_hi  = rel_max_full,
  filename = file.path(OUTPUT_FIGURES, "Figure_ES_HTE_OldYoung.png"))


#### S10 Age-Bin Coefficient Plot ####

ct_bins  <- as.data.table(tidy(m_did_4bin_hte, conf.int = TRUE))
hte_bins <- ct_bins[grepl("age_treat_bin", term) & grepl("did_term", term)]
hte_bins[, bin := gsub(".*age_treat_bin", "", term)]
hte_bins[, bin := factor(bin,
  levels = c("1-4 years","5 years","6-8 years","9+ years"))]

p_hte_bins <- ggplot(hte_bins[!is.na(bin)],
                     aes(x = bin, y = estimate, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.6, color = COL_TX, alpha = 0.7) +
  geom_point(size = 3, color = COL_TX) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.25, linewidth = 0.5, color = COL_TX) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Mean Tank Age at Reform",
       y = "Effect on Annual Closure Probability") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_HTE_AgeBin.png"),
       p_hte_bins, width = 8, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_HTE_AgeBin.pdf"),
       p_hte_bins, width = 8, height = 5, device = cairo_pdf)


#### S11 Robustness: Group-Specific Year Fixed Effects ####

models_split <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  data    = main_sample,
  split   = ~age_grp,
  cluster = ~state)

m_split_old   <- models_split[[grep("Old",   names(models_split))]]
m_split_young <- models_split[[grep("Young", names(models_split))]]

d_split_young <- extract_att_from_es(m_split_young)
d_split_old   <- extract_att_from_es(m_split_old)

m_es_hte_grpfe <- feols(
  closure_event ~
    i(rel_year_bin, texas_treated, ref = -1) +
    i(rel_year_bin, tx_old,        ref = -1) |
    panel_id + panel_year^age_grp,
  main_sample, cluster = ~state)

plot_es_split(
  m_young  = m_split_young, m_old = m_split_old,
  xlim_lo  = -8, xlim_hi = rel_max_full,
  filename = file.path(OUTPUT_FIGURES,
                        "FigureB_Robustness_SplitSample.png"))

plot_es_hte(
  model    = m_es_hte_grpfe,
  xlim_lo  = -8, xlim_hi = rel_max_full,
  filename = file.path(OUTPUT_FIGURES,
                        "FigureB_Robustness_GrpYearFE.png"))

cn_grpfe <- names(coef(m_es_hte_grpfe))
cn_hte   <- names(coef(m_es_hte))

att_young_grpfe <- mean(coef(m_es_hte_grpfe)[
  grepl(":texas_treated$", cn_grpfe) &
  grepl("rel_year_bin::[^-]", cn_grpfe)], na.rm = TRUE)
att_old_grpfe   <- att_young_grpfe + mean(coef(m_es_hte_grpfe)[
  grepl(":tx_old$", cn_grpfe) &
  grepl("rel_year_bin::[^-]", cn_grpfe)], na.rm = TRUE)

att_young_primary <- mean(coef(m_es_hte)[
  grepl(":texas_treated$", cn_hte) &
  grepl("rel_year_bin::[^-]", cn_hte)], na.rm = TRUE)
att_old_primary   <- att_young_primary + mean(coef(m_es_hte)[
  grepl(":tx_old$", cn_hte) &
  grepl("rel_year_bin::[^-]", cn_hte)], na.rm = TRUE)

fwrite(data.table(
  Specification         = c("Shared year fixed effects",
                             "Split sample (group-specific year FE)",
                             "Group-time interacted year FE"),
  ATT_young             = round(c(att_young_primary,
                                   d_split_young$beta,
                                   att_young_grpfe), 5),
  ATT_old               = round(c(att_old_primary,
                                   d_split_old$beta,
                                   att_old_grpfe), 5),
  Old_larger_than_Young = c(att_old_primary > att_young_primary,
                             d_split_old$beta > d_split_young$beta,
                             att_old_grpfe    > att_young_grpfe)),
  file.path(OUTPUT_TABLES, "TableB_YearFE_Robustness.csv"))


#### S12 Youngest Subsample ####

m_youngest_simple   <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  youngest_sample, cluster = ~state)
m_youngest_age_ctrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  youngest_sample, cluster = ~state)

model_es_youngest <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  youngest_sample, cluster = ~state)

es_youngest_out <- plot_es(model_es_youngest,
                            xlim_lo = rel_min_youngest,
                            xlim_hi = rel_max_full)

d_youngest_simple  <- extract_did(m_youngest_simple)
d_youngest_agectrl <- extract_did(m_youngest_age_ctrl)


#### S13 Oldest Subsample ####

m_oldest_simple   <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  oldest_sample, cluster = ~state)
m_oldest_age_ctrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  oldest_sample, cluster = ~state)

model_es_oldest <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  oldest_sample, cluster = ~state)

es_oldest_out <- plot_es(model_es_oldest,
                          xlim_lo = rel_min_oldest,
                          xlim_hi = rel_max_full)

d_oldest_simple  <- extract_did(m_oldest_simple)
d_oldest_agectrl <- extract_did(m_oldest_age_ctrl)

yng_s_est <- fmt_est(d_youngest_simple$beta,  d_youngest_simple$p)
yng_s_se  <- fmt_se(d_youngest_simple$se)
yng_a_est <- fmt_est(d_youngest_agectrl$beta, d_youngest_agectrl$p)
yng_a_se  <- fmt_se(d_youngest_agectrl$se)
old_s_est <- fmt_est(d_oldest_simple$beta,    d_oldest_simple$p)
old_s_se  <- fmt_se(d_oldest_simple$se)
old_a_est <- fmt_est(d_oldest_agectrl$beta,   d_oldest_agectrl$p)
old_a_se  <- fmt_se(d_oldest_agectrl$se)
n_yng_s   <- fmt_n(d_youngest_simple$n)
n_yng_a   <- fmt_n(d_youngest_agectrl$n)
n_old_s   <- fmt_n(d_oldest_simple$n)
n_old_a   <- fmt_n(d_oldest_agectrl$n)

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Annual Closure Probability: Young and Old Tank Subsamples}",
  "\\label{tbl:age_split_did}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{\\textbf{Young ($\\leq$5 years at reform)}}",
  " & \\multicolumn{2}{c}{\\textbf{Old ($>$5 years at reform)}} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  " & (1) & (2) & (3) & (4) \\\\",
  " & Simple & With age control & Simple & With age control \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s & %s & %s & %s \\\\",
          yng_s_est, yng_a_est, old_s_est, old_a_est),
  sprintf(" & %s & %s & %s & %s \\\\",
          yng_s_se, yng_a_se, old_s_se, old_a_se),
  "\\midrule",
  "Age controls           & No  & Yes & No  & Yes \\\\",
  "Facility fixed effects & Yes & Yes & Yes & Yes \\\\",
  "Year fixed effects     & Yes & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
          n_yng_s, n_yng_a, n_old_s, n_old_a),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.95\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Make-model sample split at mean tank age of 5 years",
  "in 1998. Young: mean tank age 5 years or less.",
  "Old: mean tank age greater than 5 years.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_AgeSplit_DiD.tex")


#### S14 Combined Event Study Figure ####

p_combined_es <- (
  es_youngest_out$plot +
    labs(y = "Effect on Annual Closure Probability") +
    annotate("text", x = -Inf, y = Inf, label = "A",
             hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5)
) / (
  es_oldest_out$plot +
    labs(y = "Effect on Annual Closure Probability") +
    annotate("text", x = -Inf, y = Inf, label = "B",
             hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5)
)

ggsave(file.path(OUTPUT_FIGURES, "Figure_ES_YoungOld_Combined.png"),
       p_combined_es, width = 10, height = 10, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_ES_YoungOld_Combined.pdf"),
       p_combined_es, width = 10, height = 10, device = cairo_pdf)


#### S15 Reported Leak DiD ####

m_leak_main     <- feols(leak_year ~ did_term | panel_id + panel_year,
                         main_sample,     cluster = ~state)
m_leak_youngest <- feols(leak_year ~ did_term | panel_id + panel_year,
                         youngest_sample, cluster = ~state)
m_leak_oldest   <- feols(leak_year ~ did_term | panel_id + panel_year,
                         oldest_sample,   cluster = ~state)

m_es_leak <- feols(
  leak_year ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  main_sample, cluster = ~state)

plot_es(m_es_leak,
        ylab     = "Effect on Annual Reported Leak Probability",
        xlim_lo  = -8, xlim_hi = rel_max_full,
        filename = file.path(OUTPUT_FIGURES, "Figure_Leak_ES.png"))

d_leak_m <- extract_did(m_leak_main)
d_leak_y <- extract_did(m_leak_youngest)
d_leak_o <- extract_did(m_leak_oldest)

lk_m_est <- fmt_est(d_leak_m$beta, d_leak_m$p)
lk_m_se  <- fmt_se(d_leak_m$se)
lk_y_est <- fmt_est(d_leak_y$beta, d_leak_y$p)
lk_y_se  <- fmt_se(d_leak_y$se)
lk_o_est <- fmt_est(d_leak_o$beta, d_leak_o$p)
lk_o_se  <- fmt_se(d_leak_o$se)
n_lk_m   <- fmt_n(d_leak_m$n)
n_lk_y   <- fmt_n(d_leak_y$n)
n_lk_o   <- fmt_n(d_leak_o$n)

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Annual Reported Leak Probability: Difference-in-Differences Estimates}",
  "\\label{tbl:leak_did}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & (1) & (2) & (3) \\\\",
  " & Full sample & Young only & Old only \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s & %s & %s \\\\",
          lk_m_est, lk_y_est, lk_o_est),
  sprintf(" & %s & %s & %s \\\\", lk_m_se, lk_y_se, lk_o_se),
  "\\midrule",
  "Facility fixed effects & Yes & Yes & Yes \\\\",
  "Year fixed effects     & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s \\\\", n_lk_m, n_lk_y, n_lk_o),
  "\\bottomrule",
  "\\multicolumn{4}{p{0.75\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Make-model sample.",
  "Outcome is an indicator for a confirmed underground storage tank",
  "release in the facility-year.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_Leak_DiD.tex")


#### S16 Wall Type Heterogeneity ####

fml_wall_did <- as.formula(sprintf(
  "closure_event ~ did_term + did_term:%s + %s | panel_id + panel_year",
  wall_col, wall_col))
m_hte_wall_broad <- feols(fml_wall_did, sw_broader_sample, cluster = ~state)

fml_wall_pre <- as.formula(sprintf(
  "closure_event ~ texas_treated + texas_treated:%s + %s | panel_id + panel_year",
  wall_col, wall_col))
m_wall_pre <- feols(fml_wall_pre,
                    sw_broader_sample[panel_year < POST_YEAR],
                    cluster = ~state)

int_pattern <- sprintf("did_term:%s",      wall_col)
pre_pattern <- sprintf("texas_treated:%s", wall_col)

w_did_b   <- pull_coef(m_hte_wall_broad, "^did_term$")$b
w_did_se  <- pull_coef(m_hte_wall_broad, "^did_term$")$se
w_int_b   <- pull_coef(m_hte_wall_broad, int_pattern)$b
w_int_se  <- pull_coef(m_hte_wall_broad, int_pattern)$se
w_pre_b   <- pull_coef(m_wall_pre, "^texas_treated$")$b
w_pre_se  <- pull_coef(m_wall_pre, "^texas_treated$")$se
w_pint_b  <- pull_coef(m_wall_pre, pre_pattern)$b
w_pint_se <- pull_coef(m_wall_pre, pre_pattern)$se
n_wall_d  <- fmt_n(nobs(m_hte_wall_broad))
n_wall_p  <- fmt_n(nobs(m_wall_pre))

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Annual Closure Probability: Heterogeneity by Tank Wall Construction}",
  "\\label{tbl:wall_hte}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & (1) & (2) \\\\",
  " & Post-reform & Pre-reform falsification \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s & --- \\\\", w_did_b),
  sprintf(" & %s & \\\\", w_did_se),
  sprintf("Texas (pre-reform) & --- & %s \\\\", w_pre_b),
  sprintf(" & & %s \\\\", w_pre_se),
  "\\addlinespace",
  sprintf("\\quad $\\times$ Single-walled & %s & %s \\\\",
          w_int_b, w_pint_b),
  sprintf(" & %s & %s \\\\", w_int_se, w_pint_se),
  "\\midrule",
  "Facility fixed effects & Yes & Yes \\\\",
  "Year fixed effects     & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s \\\\", n_wall_d, n_wall_p),
  "\\bottomrule",
  "\\multicolumn{3}{p{0.75\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Broader sample including single- and double-walled",
  "facilities installed 1990--1997.",
  "Column~(2) uses only pre-reform years as a falsification check.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_WallType_HTE.tex")


#### S17 Age at Closure ####

closed_tanks <- merge(
  closed_tanks,
  unique(annual_data[, .(panel_id, spec_A_eligible)]),
  by = "panel_id", all.x = TRUE)
closed_tanks[is.na(spec_A_eligible), spec_A_eligible := 0L]

sw_flag_1998 <- unique(annual_data[
  panel_year == TREATMENT_YEAR - 1L,
  .(panel_id,
    fac_all_sw_1998 = as.integer(get(wall_col) >= 0.5))])
closed_tanks <- merge(closed_tanks, sw_flag_1998,
                      by = "panel_id", all.x = TRUE)

mm_ids <- unique(main_sample$panel_id)
closed_tanks[, in_make_model := as.integer(panel_id %in% mm_ids)]

acl_data <- closed_tanks[
  !is.na(age_at_closure)  &
  !is.na(county_fips_fac) &
  spec_A_eligible == 1    &
  closure_year %between% c(PANEL_START, PANEL_END),
  .(panel_id, age_at_closure, closure_year, state,
    county_fips_fac, in_make_model)
][, `:=`(
  texas      = as.integer(state == "TX"),
  post       = as.integer(closure_year >= POST_YEAR),
  texas_post = as.integer(state == "TX") *
               as.integer(closure_year >= POST_YEAR))]

m_acl_mm    <- feols(
  age_at_closure ~ texas_post | county_fips_fac + closure_year,
  acl_data[in_make_model == 1], cluster = ~state)
m_acl_specA <- feols(
  age_at_closure ~ texas_post | county_fips_fac + closure_year,
  acl_data, cluster = ~state)
m_acl_pre   <- feols(
  age_at_closure ~ texas | county_fips_fac + closure_year,
  acl_data[post == 0], cluster = ~state)

d_acl_mm    <- extract_did(m_acl_mm,    tvar = "texas_post")
d_acl_specA <- extract_did(m_acl_specA, tvar = "texas_post")
d_acl_pre   <- extract_did(m_acl_pre,   tvar = "texas")

acl_mm_est  <- fmt_est(d_acl_mm$beta,    d_acl_mm$p)
acl_mm_se   <- fmt_se(d_acl_mm$se)
acl_sA_est  <- fmt_est(d_acl_specA$beta, d_acl_specA$p)
acl_sA_se   <- fmt_se(d_acl_specA$se)
acl_pre_est <- fmt_est(d_acl_pre$beta,   d_acl_pre$p)
acl_pre_se  <- fmt_se(d_acl_pre$se)
n_acl_mm    <- fmt_n(d_acl_mm$n)
n_acl_sA    <- fmt_n(d_acl_specA$n)
n_acl_pre   <- fmt_n(d_acl_pre$n)

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Tank Age at Closure: Difference-in-Differences Estimates}",
  "\\label{tbl:age_at_closure}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & (1) & (2) & (3) \\\\",
  " & Make-model closures & Spec-A closures & Pre-reform only \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s & %s & --- \\\\",
          acl_mm_est, acl_sA_est),
  sprintf(" & %s & %s & \\\\", acl_mm_se, acl_sA_se),
  sprintf("Texas (pre-reform) & & & %s \\\\", acl_pre_est),
  sprintf(" & & & %s \\\\", acl_pre_se),
  "\\midrule",
  "County fixed effects & Yes & Yes & Yes \\\\",
  "Year fixed effects   & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s \\\\",
          n_acl_mm, n_acl_sA, n_acl_pre),
  "\\bottomrule",
  "\\multicolumn{4}{p{0.85\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Outcome is tank age in years at the time of closure.",
  "Column~(1) restricts to make-model sample closures.",
  "Column~(2) uses all Spec-A eligible closures.",
  "Column~(3) uses pre-reform years only as a falsification check.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_AgeAtClosure_DiD.tex")

age_ts_dt <- closed_tanks[
  spec_A_eligible == 1 &
  closure_year %between% c(1990, 2018) &
  !is.na(age_at_closure),
  .(mean_age = mean(age_at_closure, na.rm = TRUE), n = .N),
  by = .(closure_year,
         group = fifelse(state == "TX", "Texas", "Control States"))
]
setorder(age_ts_dt, group, closure_year)
age_ts_dt[, mean_age_smooth :=
            frollmean(mean_age, n = 3, align = "center"), by = group]

p_acl_ts <- ggplot(age_ts_dt,
  aes(x = closure_year, y = mean_age, color = group, shape = group)) +
  geom_vline(xintercept = POST_YEAR - 0.5,
             linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_line(aes(y = mean_age_smooth), linewidth = 1, na.rm = TRUE) +
  scale_color_manual(values = c("Texas" = COL_TX,
                                "Control States" = COL_CTRL)) +
  scale_x_continuous(breaks = seq(1990, 2018, 4)) +
  labs(x = "Year of Closure",
       y = "Mean Tank Age at Closure (years)") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_AgeAtClosure_TimeSeries.png"),
       p_acl_ts, width = 9, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_AgeAtClosure_TimeSeries.pdf"),
       p_acl_ts, width = 9, height = 5, device = cairo_pdf)


#### S18 Theory-Evidence Summary ####

extract_h_row <- function(m, pattern, label, prediction,
                           p_col = "Pr(>|t|)") {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- tail(grep(pattern, rownames(ct)), 1)
  data.table(Hypothesis = label,
             Prediction = prediction,
             Estimate   = round(ct[idx, "Estimate"],   5),
             SE         = round(ct[idx, "Std. Error"], 5),
             P_value    = round(ct[idx, p_col],        4))
}

theory_tbl <- rbindlist(list(
  extract_h_row(m_did_4bin_hte,  "did_term:age_treat_bin",
    "Age gradient: oldest age-at-reform group",
    "Average treatment effect increasing in tank age at reform"),
  extract_h_row(m_youngest_simple, "did_term",
    "Treatment effect: young group (5 years or less at reform)",
    "Effect near zero for young facilities"),
  extract_h_row(m_oldest_simple, "did_term",
    "Treatment effect: old group (over 5 years at reform)",
    "Effect for old group exceeds effect for young group"),
  extract_h_row(m_acl_mm, "texas_post",
    "Age at closure: make-model closures",
    "Texas post-reform closures are older on average"),
  extract_h_row(m_acl_pre, "texas",
    "Pre-reform falsification: age at closure",
    "No Texas--control difference in closure age before reform"),
  extract_h_row(m_hte_wall_broad, int_pattern,
    "Wall type heterogeneity: broader sample",
    "Single-walled facilities show larger response")))

fwrite(theory_tbl,
       file.path(OUTPUT_TABLES, "Table_Theory_Evidence_Summary.csv"))


#### S19 Robustness Checks ####

m_noMD_main     <- feols(closure_event ~ did_term | panel_id + panel_year,
                         main_sample[state != "MD"],     cluster = ~state)
m_noMD_youngest <- feols(closure_event ~ did_term | panel_id + panel_year,
                         youngest_sample[state != "MD"], cluster = ~state)
m_noMD_oldest   <- feols(closure_event ~ did_term | panel_id + panel_year,
                         oldest_sample[state != "MD"],   cluster = ~state)

m_mandate_main  <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  main_sample, cluster = ~state)
m_mandate_young <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  youngest_sample, cluster = ~state)

main_tight <- annual_data[
  single_tanks      == active_tanks &
  has_gasoline_year == 1            &
  install_year      %between% c(1992L, 1997L)]
main_tight[, rel_year_bin :=
             pmax(pmin(rel_year_1999, rel_max_full), -7L)]

m_tight <- feols(closure_event ~ did_term | panel_id + panel_year,
                 main_tight, cluster = ~state)

d_base    <- extract_did(m_did_pooled)
d_noMD    <- extract_did(m_noMD_main)
d_mandate <- extract_did(m_mandate_main)
d_tight   <- extract_did(m_tight)

rob_b_est  <- fmt_est(d_base$beta,    d_base$p)
rob_b_se   <- fmt_se(d_base$se)
rob_m_est  <- fmt_est(d_noMD$beta,    d_noMD$p)
rob_m_se   <- fmt_se(d_noMD$se)
rob_mn_est <- fmt_est(d_mandate$beta, d_mandate$p)
rob_mn_se  <- fmt_se(d_mandate$se)
rob_t_est  <- fmt_est(d_tight$beta,   d_tight$p)
rob_t_se   <- fmt_se(d_tight$se)
n_rob_b    <- fmt_n(d_base$n)
n_rob_m    <- fmt_n(d_noMD$n)
n_rob_mn   <- fmt_n(d_mandate$n)
n_rob_t    <- fmt_n(d_tight$n)

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Annual Closure Probability: Robustness to Alternative Specifications}",
  "\\label{tbl:robustness_ols}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) \\\\",
  " & Baseline & Exclude Maryland & Mandate control & Install 1992--1997 \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s & %s & %s & %s \\\\",
          rob_b_est, rob_m_est, rob_mn_est, rob_t_est),
  sprintf(" & %s & %s & %s & %s \\\\",
          rob_b_se, rob_m_se, rob_mn_se, rob_t_se),
  "\\midrule",
  "Facility fixed effects & Yes & Yes & Yes & Yes \\\\",
  "Year fixed effects     & Yes & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
          n_rob_b, n_rob_m, n_rob_mn, n_rob_t),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.98\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Make-model sample unless noted.",
  "Column~(3) adds an indicator for active state upgrade mandate periods.",
  "Column~(4) restricts to facilities installed between 1992 and 1997.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "TableB_Robustness_OLS.tex")

fwrite(data.table(
  Specification = c("Baseline",
                     "Exclude Maryland",
                     "Add mandate activity control",
                     "Install years 1992--1997"),
  Estimate      = round(c(d_base$beta, d_noMD$beta,
                           d_mandate$beta, d_tight$beta), 4),
  SE            = round(c(d_base$se, d_noMD$se,
                           d_mandate$se, d_tight$se), 4),
  P_value       = round(c(d_base$p, d_noMD$p,
                           d_mandate$p, d_tight$p), 4),
  N             = c(d_base$n, d_noMD$n, d_mandate$n, d_tight$n)),
  file.path(OUTPUT_TABLES, "TableB_Robustness_OLS.csv"))


#### S20 Save Samples and Model Objects ####

saveRDS(main_sample,       file.path(ANALYSIS_DIR, "main_sample.rds"))
saveRDS(youngest_sample,   file.path(ANALYSIS_DIR, "youngest_sample.rds"))
saveRDS(oldest_sample,     file.path(ANALYSIS_DIR, "oldest_sample.rds"))
saveRDS(sw_broader_sample, file.path(ANALYSIS_DIR, "sw_broader_sample.rds"))

saveRDS(m_did_pooled,      file.path(ANALYSIS_DIR, "ols_headline_did.rds"))
saveRDS(m_es_hte,          file.path(ANALYSIS_DIR, "ols_hte_event_study.rds"))
saveRDS(model_es_youngest, file.path(ANALYSIS_DIR, "ols_youngest_es.rds"))
saveRDS(model_es_oldest,   file.path(ANALYSIS_DIR, "ols_oldest_es.rds"))

fwrite(texas_share_diag(main_sample),
       file.path(OUTPUT_TABLES, "Diag_TXShare_Main.csv"))
fwrite(texas_share_diag(youngest_sample),
       file.path(OUTPUT_TABLES, "Diag_TXShare_Youngest.csv"))
fwrite(texas_share_diag(oldest_sample),
       file.path(OUTPUT_TABLES, "Diag_TXShare_Oldest.csv"))
