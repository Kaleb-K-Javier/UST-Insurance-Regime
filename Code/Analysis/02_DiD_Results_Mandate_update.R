################################################################################
# 02_DiD_Main_MakeModel.R
# Texas UST Insurance Reform -- Causal DiD Estimates
#
# PREREQUISITES: Run 00_RunAll.R (or 01m_MakeModelSample.R) first.
#
# SECTIONS:
#   S1   Setup and Data Loading
#   S2   Helper Functions
#   S3   Sample Construction and Diagnostics
#   S4   Parallel Trends Validation
#   S5   Age-at-Treatment Heterogeneous Effects
#   S6   Age-Bin Coefficient Plot
#   S7   Robustness: Age-Group-Specific Year Fixed Effects
#   S8   Duration and Survival Models
#   S9   Youngest Subsample
#   S10  Oldest Subsample
#   S11  Reported Leak DiD
#   S12  Wall Type Heterogeneity (Broader Sample)
#   S13  Age at Closure (Descriptive OLS)
#   S14  Theory-Evidence Summary
#   S15  Robustness Checks
#   S16  Cox Proportional Hazard Models
#   S17  Diagnostic Data Export
#   S18  Publication LaTeX Tables
################################################################################


#### S1 Setup and Data Loading ####

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(here)
  library(ggplot2)
  library(broom)
  library(patchwork)
})

if (requireNamespace("fwildclusterboot", quietly = TRUE))
  library(fwildclusterboot)

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

USE_BOOTSTRAP <- FALSE
N_BOOTSTRAP   <- 9999
OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
ANALYSIS_DIR   <- here("Data", "Analysis")

meta <- readRDS(file.path(ANALYSIS_DIR, "analysis_metadata.rds"))
list2env(meta, envir = .GlobalEnv)

# Constants (fallback if not in metadata)
if (!exists("OUTPUT_TABLES"))  OUTPUT_TABLES  <- here("Output", "Tables")
if (!exists("OUTPUT_FIGURES")) OUTPUT_FIGURES <- here("Output", "Figures")
if (!exists("POST_YEAR"))      POST_YEAR      <- 1999L
if (!exists("TREATMENT_YEAR")) TREATMENT_YEAR <- 1999L
if (!exists("ES_END"))         ES_END         <- 2018L
if (!exists("PANEL_START"))    PANEL_START    <- 1990L
if (!exists("PANEL_END"))      PANEL_END      <- 2018L
if (!exists("AGE_BIN_BREAKS")) AGE_BIN_BREAKS <- c(0, 5, 10, 15, 20, 25, Inf)
if (!exists("AGE_BIN_LABELS")) AGE_BIN_LABELS <- c("0-4","5-9","10-14","15-19","20-24","25+")
if (!exists("AGE_BIN_REF"))    AGE_BIN_REF    <- "0-4"

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# Color palette
COL_TX    <- "#D55E00"
COL_CTRL  <- "#0072B2"
COL_YOUNG <- "#009E73"
COL_OLD   <- "#CC79A7"

# Publication theme: minimal, no title/subtitle (those go in LaTeX captions)
theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_blank(),
      plot.subtitle    = element_blank(),
      plot.caption     = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      axis.title       = element_text(size = base_size),
      axis.text        = element_text(size = base_size - 1),
      strip.text       = element_text(face = "bold", size = base_size)
    )
}
theme_set(theme_pub())

# Load datasets
annual_data    <- readRDS(file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))
tank_inventory <- readRDS(file.path(ANALYSIS_DIR, "analysis_tank_inventory.rds"))
closed_tanks   <- readRDS(file.path(ANALYSIS_DIR, "analysis_closed_tanks.rds"))
tanks_1999     <- readRDS(file.path(ANALYSIS_DIR, "analysis_tanks_1999.rds"))

# Variable construction
annual_data[, any_closure   := closure_event]
annual_data[, any_leak      := leak_year]
annual_data[, replace_event := as.integer(closure_event == 1 & exit_flag == 0)]

if (!"vintage_cohort" %in% names(annual_data)) {
  if (!"install_year" %in% names(tanks_1999)) {
    if ("install_date" %in% names(tanks_1999)) {
      tanks_1999[, install_year := year(install_date)]
    } else {
      stop("tanks_1999 has neither 'install_year' nor 'install_date'.")
    }
  }
  tanks_1999[, vc := fcase(
    install_year < 1965,                  "Pre-1965",
    install_year %between% c(1965, 1974), "1965-1974",
    install_year %between% c(1975, 1979), "1975-1979",
    install_year %between% c(1980, 1984), "1980-1984",
    install_year %between% c(1985, 1988), "1985-1988",
    default =                              "Post-1988"
  )]
  fac_vc <- tanks_1999[, .(vintage_cohort = names(which.max(table(vc)))), by = panel_id]
  annual_data <- merge(annual_data, fac_vc, by = "panel_id", all.x = TRUE)
  annual_data[is.na(vintage_cohort), vintage_cohort := "Post-1988"]
}

vc_levels <- c("Pre-1965","1965-1974","1975-1979","1980-1984","1985-1988","Post-1988")
annual_data[, vintage_cohort := factor(vintage_cohort, levels = vc_levels)]

annual_data[, mandate_window_3yr := as.integer(
  state == "TX" & spec_B_eligible == 1 & panel_year %between% c(1988L, 1994L))]

annual_data[, age_bin := factor(
  cut(avg_tank_age, breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS,
      right = FALSE, include.lowest = TRUE),
  levels = AGE_BIN_LABELS)]
annual_data[, age_bin := relevel(age_bin, ref = AGE_BIN_REF)]

wall_col <- intersect(
  c("pct_single_wall","has_single_walled","any_single_walled"),
  names(annual_data))[1]

rel_min_full     <- 1990L - POST_YEAR
rel_max_full     <- ES_END - POST_YEAR
rel_min_youngest <- 1994L - POST_YEAR
rel_min_oldest   <- -5L

annual_data[, age_treat_bin := cut(
  mean_age_1998,
  breaks = c(0, 5, 6, 9, Inf),
  labels = c("1-2 yrs", "3-5 yrs", "6-8 yrs", "9+ yrs"),
  right  = FALSE, include.lowest = TRUE
)]
annual_data[, age_treat_bin := relevel(factor(age_treat_bin), ref = "1-2 yrs")]


#### S2 Helper Functions ####

save_did_table <- function(models, headers, base_name, title,
                           tvar = "did_term", digits = 4) {
  results <- mapply(function(m, h) {
    ct  <- coeftable(summary(m, cluster = ~state))
    idx <- grep(tvar, rownames(ct))[1]
    data.frame(Model    = h,
               Estimate = round(ct[idx, "Estimate"],   digits),
               SE       = round(ct[idx, "Std. Error"], digits),
               t_stat   = round(ct[idx, "t value"],    3),
               p_value  = round(ct[idx, "Pr(>|t|)"],   4),
               N_obs    = nobs(m))
  }, models, headers, SIMPLIFY = FALSE)
  dt <- rbindlist(results)
  fwrite(dt, file.path(OUTPUT_TABLES, paste0(base_name, ".csv")))
  invisible(dt)
}

extract_did <- function(m, tvar = "did_term") {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(tvar, rownames(ct), fixed = TRUE)[1]
  list(beta = ct[idx,"Estimate"], se = ct[idx,"Std. Error"],
       p    = ct[idx,"Pr(>|t|)"], n  = nobs(m))
}

extract_att_from_es <- function(m) {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep("rel_year_bin::[^-][0-9]*:texas_treated", rownames(ct))
  if (length(idx) == 0) return(list(beta = NA, se = NA, p = NA_real_, n = nobs(m)))
  list(beta = mean(ct[idx, "Estimate"], na.rm = TRUE),
       se   = mean(ct[idx, "Std. Error"], na.rm = TRUE),
       p    = NA_real_, n = nobs(m))
}

stars_fn <- function(p) {
  if (is.na(p))  return("")
  if (p < 0.01)  return("$^{***}$")
  if (p < 0.05)  return("$^{**}$")
  if (p < 0.10)  return("$^{*}$")
  ""
}

pt_pval <- function(m) {
  nms <- names(coef(m))
  pre <- nms[grepl("::-[2-9]|::-1[0-9]", nms)]
  if (length(pre) == 0) return(NA_real_)
  suppressWarnings(wald(m, keep = pre)$p)
}

pull_coef <- function(m, pattern) {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(pattern, rownames(ct))
  if (length(idx) == 0) return(list(b = "---", se = "---"))
  list(
    b  = sprintf("%.4f%s", ct[idx[1], "Estimate"], stars_fn(ct[idx[1], "Pr(>|t|)"])),
    se = sprintf("(%.4f)", ct[idx[1], "Std. Error"])
  )
}

extract_cox_coef <- function(m, pattern) {
  s   <- summary(m)$coefficients
  idx <- grep(pattern, rownames(s))
  if (length(idx) == 0) return(NULL)
  data.table(
    term = rownames(s)[idx],
    hr   = s[idx, "exp(coef)"],
    coef = s[idx, "coef"],
    se   = s[idx, "se(coef)"],
    z    = s[idx, "z"],
    p    = s[idx, "Pr(>|z|)"]
  )
}

texas_share_by_period <- function(dt, rel_year_col = "rel_year_bin",
                                  weight_col = NULL) {
  if (is.null(weight_col)) {
    dt[, .(texas_share = mean(texas_treated)), by = get(rel_year_col)]
  } else {
    dt[, .(texas_share = weighted.mean(texas_treated, get(weight_col))),
       by = get(rel_year_col)]
  }
}

# Publication event study plot: clean, no titles (LaTeX handles captions)
plot_es <- function(model, ylab = "Effect on Closure Probability",
                    ref_period = -1, xlim_lo = NULL, xlim_hi = NULL,
                    filename = NULL) {
  ct <- as.data.table(tidy(model, conf.int = TRUE))
  ct <- ct[grepl("rel_year|event_time", term)]
  ct[, rel_year := as.numeric(gsub(".*::(-?[0-9]+).*", "\\1", term))]
  ct <- rbind(ct,
    data.table(term = "ref", estimate = 0, std.error = 0,
               conf.low = 0, conf.high = 0, rel_year = ref_period),
    fill = TRUE)
  setorder(ct, rel_year)
  ct[, period := fcase(rel_year < 0, "Pre", rel_year == ref_period, "Ref",
                        default = "Post")]

  xl <- if (is.null(xlim_lo)) min(ct$rel_year) else xlim_lo
  xh <- if (is.null(xlim_hi)) max(ct$rel_year) else xlim_hi

  p <- ggplot(ct[rel_year %between% c(xl, xh)],
              aes(x = rel_year, y = estimate)) +
    annotate("rect", xmin = -Inf, xmax = -0.5, ymin = -Inf, ymax = Inf,
             fill = "grey90", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50",
               linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.6) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12,
                fill = "grey40") +
    geom_line(color = "grey40", linewidth = 0.4, alpha = 0.6) +
    geom_point(aes(color = period), size = 2.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = period),
                  width = 0.25, linewidth = 0.5) +
    scale_color_manual(values = c(Pre = COL_CTRL, Post = COL_TX, Ref = "black"),
                       guide = "none") +
    labs(x = "Years Relative to Treatment (1999)", y = ylab) +
    theme_pub()

  if (!is.null(filename)) {
    ggsave(filename, p, width = 10, height = 5.5, dpi = 300, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p, width = 10, height = 5.5,
           device = cairo_pdf)
  }
  invisible(list(plot = p, data = ct))
}

# Publication HTE event study plot: old vs young, no titles
plot_es_hte <- function(model, filename = NULL,
                        xlim_lo = -8, xlim_hi = NULL,
                        col_young = COL_YOUNG, col_old = COL_OLD) {
  ct <- as.data.table(tidy(model, conf.int = TRUE))

  young <- ct[grepl(":texas_treated$", term) & grepl("rel_year_bin", term)]
  young[, rel_year := as.numeric(gsub("rel_year_bin::(-?[0-9]+):.*", "\\1", term))]
  young[, group := "Young (age 5 or less in 1998)"]
  young[, `:=`(conf.low  = estimate - 1.96 * std.error,
               conf.high = estimate + 1.96 * std.error)]

  diff_dt <- ct[grepl(":tx_old$", term) & grepl("rel_year_bin", term)]
  diff_dt[, rel_year := as.numeric(gsub("rel_year_bin::(-?[0-9]+):.*", "\\1", term))]

  old <- merge(
    young[,   .(rel_year, base = estimate, base_se = std.error)],
    diff_dt[, .(rel_year, diff = estimate, diff_se = std.error)],
    by = "rel_year", all.x = TRUE)
  old[is.na(diff), `:=`(diff = 0, diff_se = 0)]
  old[, `:=`(
    estimate  = base + diff,
    conf.low  = (base + diff) - 1.96 * sqrt(base_se^2 + diff_se^2),
    conf.high = (base + diff) + 1.96 * sqrt(base_se^2 + diff_se^2),
    group     = "Old (age over 5 in 1998)"
  )]

  ref_rows <- data.table(
    rel_year = -1L, estimate = 0, conf.low = 0, conf.high = 0,
    group = c("Young (age 5 or less in 1998)", "Old (age over 5 in 1998)"))

  plot_dt <- rbind(
    young[, .(rel_year, estimate, conf.low, conf.high, group)],
    old[,   .(rel_year, estimate, conf.low, conf.high, group)],
    ref_rows, fill = TRUE)
  setorder(plot_dt, group, rel_year)

  xh <- if (is.null(xlim_hi)) max(plot_dt$rel_year) else xlim_hi

  p <- ggplot(plot_dt[rel_year %between% c(xlim_lo, xh)],
              aes(x = rel_year, y = estimate, color = group, fill = group)) +
    annotate("rect", xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12,
                color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    scale_color_manual(values = c(
      "Young (age 5 or less in 1998)" = col_young,
      "Old (age over 5 in 1998)"      = col_old)) +
    scale_fill_manual(values = c(
      "Young (age 5 or less in 1998)" = col_young,
      "Old (age over 5 in 1998)"      = col_old)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(x = "Years Relative to Treatment (1999)",
         y = "Effect on Closure Probability") +
    theme_pub()

  if (!is.null(filename)) {
    ggsave(filename, p, width = 10, height = 5.5, dpi = 300, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p, width = 10, height = 5.5,
           device = cairo_pdf)
  }
  invisible(list(plot = p, data = plot_dt))
}


#### S3 Sample Construction and Diagnostics ####

main_sample <- annual_data[
  single_tanks  == active_tanks  &
  has_gasoline_year == 1         &
  install_year  >  1989          &
  install_year  <= 1997
]

sw_broader_sample <- annual_data[
  has_single_walled == 1 &
  install_year > 1989    &
  install_year <= 1997
]

youngest_sample <- main_sample[mean_age_1998 <= 5]
oldest_sample   <- main_sample[mean_age_1998 >  5]

main_sample[,       rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), -8L)]
youngest_sample[,   rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), rel_min_youngest)]
oldest_sample[,     rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), rel_min_oldest)]
sw_broader_sample[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), -8L)]

# Age distribution histogram (publication version)
age_dist_1998 <- main_sample[panel_year == 1998,
  .(panel_id, mean_age_1998,
    group = fifelse(texas_treated == 1, "Texas", "Control"))
]

p_age_dist <- ggplot(age_dist_1998[!is.na(mean_age_1998)],
                     aes(x = mean_age_1998, fill = group)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c(Texas = COL_TX, Control = COL_CTRL)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Mean Tank Age in 1998 (years)", y = "Density") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_9_Age_Distribution_1998.png"),
       p_age_dist, width = 8, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_9_Age_Distribution_1998.pdf"),
       p_age_dist, width = 8, height = 5, device = cairo_pdf)

# Raw closure rates by group and age split
raw_trends <- main_sample[
  panel_year %between% c(1992L, 2005L) & !is.na(mean_age_1998),
  .(closure_rate = mean(closure_event, na.rm = TRUE),
    n_fac        = uniqueN(panel_id)),
  by = .(panel_year,
         group   = fifelse(texas_treated == 1, "Texas", "Control"),
         age_grp = fifelse(mean_age_1998 <= 5,
                           "Young (age 5 or less in 1998)",
                           "Old (age over 5 in 1998)"))
][!is.na(age_grp)]

raw_trends[, group   := factor(group,   levels = c("Control", "Texas"))]

p_raw <- ggplot(
  raw_trends[n_fac >= 200],
  aes(x = panel_year, y = closure_rate, color = group,
      linetype = age_grp, shape = age_grp)
) +
  annotate("rect", xmin = -Inf, xmax = 1998.5,
           ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.5) +
  geom_vline(xintercept = 1998.5, linetype = "dashed",
             color = "grey30", linewidth = 0.6) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2, fill = "white", stroke = 0.8) +
  scale_color_manual(values = c(Control = COL_CTRL, Texas = COL_TX)) +
  scale_linetype_manual(values = c(
    "Young (age 5 or less in 1998)" = "dashed",
    "Old (age over 5 in 1998)"      = "solid")) +
  scale_shape_manual(values = c(
    "Young (age 5 or less in 1998)" = 21,
    "Old (age over 5 in 1998)"      = 19)) +
  scale_x_continuous(breaks = seq(1992, 2005, 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Year", y = "Mean Annual Closure Rate") +
  theme_pub() +
  theme(legend.key.width = unit(1.5, "cm"))

ggsave(file.path(OUTPUT_FIGURES, "Figure_Raw_ClosureRates_OldYoung.png"),
       p_raw, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Raw_ClosureRates_OldYoung.pdf"),
       p_raw, width = 9, height = 5.5, device = cairo_pdf)


#### S4 Parallel Trends Validation ####

m_pt_main <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  main_sample[panel_year < POST_YEAR], cluster = ~state
)

pt_results <- list(main = pt_pval(m_pt_main),
                   n_pre = nrow(main_sample[panel_year < POST_YEAR]))


#### S5 Age-at-Treatment Heterogeneous Effects ####

# Binary classification: old (>5 yrs in 1998) vs young (<=5 yrs)
main_sample[, old_at_treat := as.integer(mean_age_1998 > 5)]
main_sample[, tx_old       := texas_treated * old_at_treat]

# Split-sample DiD
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

d_young <- extract_did(m_did_young)
d_old   <- extract_did(m_did_old)

pre_mean_young <- mean(
  main_sample[old_at_treat == 0 & panel_year < TREATMENT_YEAR, closure_event],
  na.rm = TRUE)
pre_mean_old <- mean(
  main_sample[old_at_treat == 1 & panel_year < TREATMENT_YEAR, closure_event],
  na.rm = TRUE)

# Triple-interaction event study (Figure 6 in paper)
m_es_hte <- feols(
  closure_event ~
    i(rel_year_bin, texas_treated, ref = -1) +
    i(rel_year_bin, tx_old,        ref = -1) |
    panel_id + panel_year,
  main_sample, cluster = ~state)

es_hte_out <- plot_es_hte(
  model   = m_es_hte,
  xlim_lo = -8,
  xlim_hi = rel_max_full,
  filename = file.path(OUTPUT_FIGURES, "Figure_6_ES_HTE_OldYoung.png"))

# Four-specification DiD table
m_did_pooled <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  main_sample, cluster = ~state)

m_did_agectrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  main_sample, cluster = ~state)

m_did_binary_hte <- feols(
  closure_event ~ did_term + did_term:old_at_treat + age_bin |
    panel_id + panel_year,
  main_sample, cluster = ~state)

m_did_4bin_hte <- feols(
  closure_event ~ did_term:age_treat_bin + age_bin |
    panel_id + panel_year,
  main_sample, cluster = ~state)

save_did_table(
  models    = list(m_did_pooled, m_did_agectrl, m_did_binary_hte, m_did_4bin_hte),
  headers   = c("Pooled DiD", "+ Age Control", "Binary HTE", "4-Bin HTE"),
  base_name = "Table4_AgeTreat_HTE_MakeModel",
  title     = "Age-at-Treatment Heterogeneous Effects")

# LaTeX: Table 3 (Old vs Young DiD)
writeLines(c(
  "\\begin{table}[htbp]\\centering",
  "\\caption{Treatment Effect by Age at Reform: Old vs.\\ Young Facilities}",
  "\\label{tbl:old_young_did}",
  "\\begin{tabular}{lcccc}\\toprule",
  " & (1) & (2) & (3) & (4) \\\\",
  " & Young only & Old only & Interact & Interact+Age \\\\\\midrule",
  sprintf("Texas $\\times$ Post & %s & %s & %s & %s \\\\",
    sprintf("%.4f%s", d_young$beta, stars_fn(d_young$p)),
    sprintf("%.4f%s", d_old$beta,   stars_fn(d_old$p)),
    pull_coef(m_did_interact,         "^did_term$")$b,
    pull_coef(m_did_interact_agectrl, "^did_term$")$b),
  sprintf(" & (%.4f) & (%.4f) & %s & %s \\\\",
    d_young$se, d_old$se,
    pull_coef(m_did_interact,         "^did_term$")$se,
    pull_coef(m_did_interact_agectrl, "^did_term$")$se),
  sprintf("$\\times$ Old ($>$5 in 1998) & & & %s & %s \\\\",
    pull_coef(m_did_interact,         "old_at_treat")$b,
    pull_coef(m_did_interact_agectrl, "old_at_treat")$b),
  sprintf(" & & & %s & %s \\\\",
    pull_coef(m_did_interact,         "old_at_treat")$se,
    pull_coef(m_did_interact_agectrl, "old_at_treat")$se),
  "\\midrule",
  sprintf("Pre-reform mean & %.4f & %.4f & \\multicolumn{2}{c}{---} \\\\",
    pre_mean_young, pre_mean_old),
  "Facility + Year FE & Yes & Yes & Yes & Yes \\\\\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
    format(d_young$n, big.mark=","), format(d_old$n, big.mark=","),
    format(nobs(m_did_interact), big.mark=","),
    format(nobs(m_did_interact_agectrl), big.mark=",")),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:} ",
  "Make-model sample. SE clustered at state. ",
  "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}\\end{table}"
), file.path(OUTPUT_TABLES, "Table3_OldYoung_DiD_MakeModel.tex"))

# LaTeX: Table 4 (4-spec HTE)
specs       <- list(m_did_pooled, m_did_agectrl, m_did_binary_hte, m_did_4bin_hte)
spec_labels <- c("Pooled", "+Age Ctrl", "Binary HTE", "4-Bin HTE")
did_rows    <- lapply(specs, pull_coef, pattern = "^did_term$")
old_rows    <- lapply(specs, pull_coef, pattern = "did_term:old_at_treat")

writeLines(c(
  "\\begin{table}[htbp]\\centering",
  "\\caption{Age-at-Treatment Heterogeneous Treatment Effects}",
  "\\label{tbl:age_treat_hte}",
  "\\begin{tabular}{lcccc}\\toprule",
  " & (1) & (2) & (3) & (4)\\\\",
  sprintf(" & %s \\\\", paste(spec_labels, collapse = " & ")),
  "\\midrule",
  "\\textit{Panel A: Young group (age $\\leq$5 in 1998)} & & & & \\\\",
  sprintf("Texas $\\times$ Post & %s \\\\",
    paste(sapply(did_rows, `[[`, "b"), collapse = " & ")),
  sprintf(" & %s \\\\",
    paste(sapply(did_rows, `[[`, "se"), collapse = " & ")),
  "\\addlinespace",
  "\\textit{Panel B: Differential for old group ($>$5 yrs)} & & & & \\\\",
  sprintf("$\\times$ Old at treatment & %s \\\\",
    paste(sapply(old_rows, `[[`, "b"), collapse = " & ")),
  sprintf(" & %s \\\\",
    paste(sapply(old_rows, `[[`, "se"), collapse = " & ")),
  "\\midrule",
  "Age control (panel bins) & No & Yes & Yes & Yes \\\\",
  "Facility FE & Yes & Yes & Yes & Yes \\\\",
  "Year FE & Yes & Yes & Yes & Yes \\\\\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
    format(nobs(specs[[1]]), big.mark = ","),
    format(nobs(specs[[2]]), big.mark = ","),
    format(nobs(specs[[3]]), big.mark = ","),
    format(nobs(specs[[4]]), big.mark = ",")),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:} ",
  "Make-model sample. Spec 4: each bin coefficient is the total ATT for that bin. ",
  "SE clustered at state. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}\\end{table}"
), file.path(OUTPUT_TABLES, "Table4_AgeTreat_HTE_MakeModel.tex"))


#### S6 Age-Bin Coefficient Plot ####

build_hte_dt <- function(m, base_tvar, label,
                         age_var    = "age_treat_bin",
                         bin_levels = levels(main_sample$age_treat_bin)) {
  ct       <- as.data.table(tidy(m, conf.int = TRUE))
  int_rows <- ct[grepl(age_var,   term, fixed = TRUE) &
                 grepl(base_tvar, term, fixed = TRUE)]
  int_rows[, bin := gsub(paste0(".*", age_var), "", term)]
  base_row <- ct[term == base_tvar]
  bc  <- if (nrow(base_row) > 0) base_row$estimate[1]  else 0
  bse <- if (nrow(base_row) > 0) base_row$std.error[1] else 0
  if (nrow(base_row) > 0 && nrow(int_rows) > 0) {
    int_rows[, `:=`(estimate  = estimate  + bc,
                    conf.low  = conf.low  + bc,
                    conf.high = conf.high + bc)]
  }
  int_rows <- int_rows[, .(bin, estimate, conf.low, conf.high)]
  int_rows[, `:=`(sample = label, bin = factor(bin, levels = bin_levels))]
  int_rows
}

hte_main <- build_hte_dt(m_did_4bin_hte, base_tvar = "did_term",
                         label = "Post-Reform")

p_hte <- ggplot(hte_main[!is.na(bin)], aes(x = bin, y = estimate, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.6, color = COL_TX, alpha = 0.7) +
  geom_point(size = 3, color = COL_TX) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.25, linewidth = 0.5, color = COL_TX) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Mean Tank Age in 1998", y = "Effect on Closure Probability") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_8_HTE_AgeBin_MakeModel.png"),
       p_hte, width = 8, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_8_HTE_AgeBin_MakeModel.pdf"),
       p_hte, width = 8, height = 5, device = cairo_pdf)


#### S7 Robustness: Age-Group-Specific Year Fixed Effects ####

main_sample[, age_grp := fifelse(mean_age_1998 <= 5, "Young", "Old")]

# Method A: split sample with group-specific year FEs
models_split <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  data = main_sample, split = ~age_grp, cluster = ~state)

nm_old   <- grep("Old",   names(models_split), value = TRUE)
nm_young <- grep("Young", names(models_split), value = TRUE)
m_split_old   <- models_split[[nm_old]]
m_split_young <- models_split[[nm_young]]

d_split_young <- extract_att_from_es(m_split_young)
d_split_old   <- extract_att_from_es(m_split_old)

# Method B: group-time interacted FEs (single model)
m_es_hte_grpfe <- feols(
  closure_event ~
    i(rel_year_bin, texas_treated, ref = -1) +
    i(rel_year_bin, tx_old,        ref = -1) |
    panel_id + panel_year^age_grp,
  main_sample, cluster = ~state)

# Split-sample event study plot helper
plot_es_split <- function(m_young, m_old, filename = NULL,
                          xlim_lo = -8, xlim_hi = NULL,
                          col_young = COL_YOUNG, col_old = COL_OLD) {
  extract_path <- function(m, grp_label) {
    ct <- as.data.table(tidy(m, conf.int = TRUE))
    ct <- ct[grepl("rel_year_bin", term)]
    ct[, rel_year := as.numeric(gsub("rel_year_bin::(-?[0-9]+):.*", "\\1", term))]
    ct[, group    := grp_label]
    ct[, `:=`(conf.low  = estimate - 1.96 * std.error,
              conf.high = estimate + 1.96 * std.error)]
    ct[, .(rel_year, estimate, conf.low, conf.high, group)]
  }
  plot_dt <- rbind(
    extract_path(m_young, "Young (age 5 or less in 1998)"),
    extract_path(m_old,   "Old (age over 5 in 1998)"),
    data.table(rel_year = -1L, estimate = 0, conf.low = 0, conf.high = 0,
               group = c("Young (age 5 or less in 1998)",
                         "Old (age over 5 in 1998)")),
    fill = TRUE)
  setorder(plot_dt, group, rel_year)
  xh <- if (is.null(xlim_hi)) max(plot_dt$rel_year) else xlim_hi

  p <- ggplot(plot_dt[rel_year %between% c(xlim_lo, xh)],
              aes(x = rel_year, y = estimate, color = group, fill = group)) +
    annotate("rect", xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12,
                color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5) +
    scale_color_manual(values = c(
      "Young (age 5 or less in 1998)" = col_young,
      "Old (age over 5 in 1998)"      = col_old)) +
    scale_fill_manual(values = c(
      "Young (age 5 or less in 1998)" = col_young,
      "Old (age over 5 in 1998)"      = col_old)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(x = "Years Relative to Treatment (1999)",
         y = "Effect on Closure Probability") +
    theme_pub()

  if (!is.null(filename)) {
    ggsave(filename, p, width = 10, height = 5.5, dpi = 300, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p, width = 10, height = 5.5,
           device = cairo_pdf)
  }
  invisible(list(plot = p, data = plot_dt))
}

es_split_out <- plot_es_split(
  m_young  = m_split_young, m_old = m_split_old,
  xlim_lo  = -8, xlim_hi = rel_max_full,
  filename = file.path(OUTPUT_FIGURES, "FigureB1_Robustness_SplitSample.png"))

es_hte_grpfe_out <- plot_es_hte(
  model   = m_es_hte_grpfe,
  xlim_lo = -8, xlim_hi = rel_max_full,
  filename = file.path(OUTPUT_FIGURES, "FigureB2_Robustness_GrpYearFE.png"))

# Comparison table
cn_b <- names(coef(m_es_hte_grpfe))
post_young_b <- mean(coef(m_es_hte_grpfe)[
  grepl(":texas_treated$", cn_b) & grepl("rel_year_bin::[^-]", cn_b)],
  na.rm = TRUE)
post_old_b <- post_young_b + mean(coef(m_es_hte_grpfe)[
  grepl(":tx_old$", cn_b) & grepl("rel_year_bin::[^-]", cn_b)],
  na.rm = TRUE)

post_young_primary <- mean(coef(m_es_hte)[
  grepl(":texas_treated$", names(coef(m_es_hte))) &
  grepl("rel_year_bin::[^-]", names(coef(m_es_hte)))], na.rm = TRUE)
post_old_primary <- post_young_primary + mean(coef(m_es_hte)[
  grepl(":tx_old$", names(coef(m_es_hte))) &
  grepl("rel_year_bin::[^-]", names(coef(m_es_hte)))], na.rm = TRUE)

comparison_tbl <- data.table(
  Spec      = c("Primary (shared year FE)", "Split sample", "Group-time FE"),
  ATT_young = round(c(post_young_primary, d_split_young$beta, post_young_b), 5),
  ATT_old   = round(c(post_old_primary,   d_split_old$beta,   post_old_b),   5),
  Old_gt_Young = c(post_old_primary > post_young_primary,
                   d_split_old$beta > d_split_young$beta,
                   post_old_b       > post_young_b))

fwrite(comparison_tbl,
  file.path(OUTPUT_TABLES, "TableB_Robustness_YearFE_Comparison.csv"))


#### S8 Duration and Survival Models ####

library(survival)

setorder(main_sample, panel_id, panel_year)
main_sample[, surv_time := seq_len(.N), by = panel_id]

# --- S8a: Survivorship diagnostic ---

surv_diag <- main_sample[,
  .(exited_pre99  = as.integer(any(closure_event == 1 & panel_year < POST_YEAR)),
    survived_1999 = as.integer(any(panel_year >= POST_YEAR)),
    age_treat_bin = first(age_treat_bin),
    texas         = first(texas_treated)),
  by = panel_id]

surv_summary <- surv_diag[!is.na(age_treat_bin),
  .(n_facilities = .N,
    n_exited_pre = sum(exited_pre99,  na.rm = TRUE),
    pct_exit_pre = mean(exited_pre99, na.rm = TRUE)),
  by = .(age_treat_bin,
         texas = fifelse(texas == 1, "Texas", "Control"))
][order(texas, age_treat_bin)]

fwrite(surv_summary,
  file.path(OUTPUT_TABLES, "Table_Survivorship_Diagnostic.csv"))


# --- S8b: Binned scatter -- closure rate vs age bin by group and period ---

h1_scatter_dt <- main_sample[!is.na(age_treat_bin),
  .(closure_rate = mean(closure_event, na.rm = TRUE), n = .N),
  by = .(age_treat_bin,
         state_group = fifelse(texas_treated == 1, "Texas", "Control"),
         period      = fifelse(panel_year < POST_YEAR, "Pre-Reform", "Post-Reform"))
]
h1_scatter_dt[, period_group := factor(
  paste(state_group, period),
  levels = c("Control Pre-Reform","Control Post-Reform",
             "Texas Pre-Reform","Texas Post-Reform"))]

p_h1_scatter <- ggplot(
  h1_scatter_dt[!is.na(age_treat_bin)],
  aes(x = age_treat_bin, y = closure_rate,
      color = period_group, linetype = period_group, group = period_group)) +
  geom_line(linewidth = 0.9) + geom_point(size = 3) +
  scale_color_manual(
    values = c("Control Pre-Reform"=COL_CTRL, "Control Post-Reform"="#56B4E9",
               "Texas Pre-Reform"=COL_TX,     "Texas Post-Reform"="#E69F00")) +
  scale_linetype_manual(
    values = c("Control Pre-Reform"="dashed", "Control Post-Reform"="solid",
               "Texas Pre-Reform"="dashed",   "Texas Post-Reform"="solid")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Age at Treatment (years)", y = "Mean Annual Closure Rate") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_BinnedScatter_AgeClosure.png"),
       p_h1_scatter, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_BinnedScatter_AgeClosure.pdf"),
       p_h1_scatter, width = 9, height = 5.5, device = cairo_pdf)


# --- S8c: Cox proportional hazard -- age gradient ---
# Three specs: base (did_term only), age interactions, full with age controls

cox_base <- coxph(
  Surv(surv_time, closure_event) ~ did_term + strata(panel_id),
  data = main_sample, cluster = main_sample$state, ties = "efron")

cox_age_interact <- coxph(
  Surv(surv_time, closure_event) ~
    did_term + age_bin + did_term:age_bin + strata(panel_id),
  data = main_sample, cluster = main_sample$state, ties = "efron")

cox_h1_coefs <- extract_cox_coef(cox_age_interact, "did_term")
interact_hrs <- cox_h1_coefs[grepl("did_term:age_bin", term)]

# Forest plot: base HR + age-bin interaction HRs
base_hr_dt <- extract_cox_coef(cox_base, "did_term")
base_hr_dt[, label := "Pooled"]

if (nrow(interact_hrs) > 0) {
  interact_hrs[, label := gsub("did_term:age_bin", "", term)]

  forest_dt <- rbind(
    base_hr_dt[, .(label, hr, coef, se)],
    interact_hrs[, .(label, hr, coef, se)]
  )
  forest_dt[, `:=`(
    ci_lo = exp(coef - 1.96 * se),
    ci_hi = exp(coef + 1.96 * se)
  )]
  forest_dt[, label := factor(label, levels = rev(forest_dt$label))]

  p_forest <- ggplot(forest_dt,
    aes(x = hr, y = label, xmin = ci_lo, xmax = ci_hi)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_pointrange(size = 0.5, linewidth = 0.6, color = COL_TX) +
    labs(x = "Hazard Ratio", y = NULL) +
    theme_pub() +
    theme(panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3))

  ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_ForestPlot.png"),
         p_forest, width = 7, height = 4.5, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_ForestPlot.pdf"),
         p_forest, width = 7, height = 4.5, device = cairo_pdf)
}


# --- S8d: Duration models on full at-risk panel ---
# Three estimators: cloglog (discrete PH), LPM with facility FE, Cox with age scale

h3_dt <- main_sample[!is.na(avg_tank_age) & !is.na(did_term)]

h3_cloglog <- glm(
  closure_event ~ did_term + age_bin + texas_treated,
  family = binomial(link = "cloglog"), data = h3_dt)

h3_lpm <- feols(
  closure_event ~ did_term + age_bin | panel_id,
  h3_dt, cluster = ~state)

h3_cox_age <- coxph(
  Surv(avg_tank_age, closure_event) ~ did_term + strata(panel_id),
  data = h3_dt, cluster = h3_dt$state, ties = "efron")


# --- S8d-fig: Discrete-time hazard profile by age bin ---
# Predicted closure probability from cloglog, by age bin x TX/Control x period
# This is the reduced-form analog of the structural Figure 14

pred_dt <- CJ(
  age_bin     = levels(main_sample$age_bin),
  did_term    = c(0L, 1L),
  texas_treated = c(0L, 1L)
)
# Keep only: control pre (did=0, tx=0), control post (did=0, tx=0 but post),
#            TX pre (did=0, tx=1), TX post (did=1, tx=1)
pred_dt <- pred_dt[
  (texas_treated == 0 & did_term == 0) |
  (texas_treated == 1 & did_term == 0) |
  (texas_treated == 1 & did_term == 1)
]
pred_dt[, age_bin := factor(age_bin, levels = levels(main_sample$age_bin))]
pred_dt[, group := fcase(
  texas_treated == 0 & did_term == 0, "Control (uniform premium)",
  texas_treated == 1 & did_term == 0, "Texas pre-reform",
  texas_treated == 1 & did_term == 1, "Texas post-reform (risk-based)"
)]

pred_dt[, pred_prob := predict(h3_cloglog, newdata = pred_dt, type = "response")]
pred_dt[, group := factor(group, levels = c(
  "Control (uniform premium)", "Texas pre-reform",
  "Texas post-reform (risk-based)"))]

p_hazard_profile <- ggplot(pred_dt,
  aes(x = age_bin, y = pred_prob, color = group, group = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "Control (uniform premium)"        = COL_CTRL,
    "Texas pre-reform"                 = "#E69F00",
    "Texas post-reform (risk-based)"   = COL_TX)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Tank Age Bin (years)", y = "Predicted Closure Probability") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_DiscreteHazard_AgeProfile.png"),
       p_hazard_profile, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_DiscreteHazard_AgeProfile.pdf"),
       p_hazard_profile, width = 9, height = 5.5, device = cairo_pdf)


# --- S8d-tab: Duration model comparison table (LaTeX) ---

cll_coefs  <- coef(summary(h3_cloglog))
cll_did    <- cll_coefs["did_term", ]
lpm_ct     <- coeftable(summary(h3_lpm, cluster = ~state))
lpm_did    <- lpm_ct["did_term", ]
cox_s      <- summary(h3_cox_age)$coefficients
cox_did    <- cox_s["did_term", ]

dur_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  ""
}

writeLines(c(
  "\\begin{table}[htbp]\\centering",
  "\\caption{Duration Model Estimates: Effect of Risk-Based Pricing on Closure Hazard}",
  "\\label{tbl:duration_models}",
  "\\begin{tabular}{lccc}\\toprule",
  " & (1) & (2) & (3) \\\\",
  " & Cloglog & LPM & Cox PH \\\\\\midrule",
  sprintf("Texas $\\times$ Post & %.4f%s & %.4f%s & \\\\",
    cll_did["Estimate"], dur_stars(cll_did["Pr(>|z|)"]),
    lpm_did["Estimate"], dur_stars(lpm_did["Pr(>|t|)"])),
  sprintf(" & (%.4f) & (%.4f) & \\\\",
    cll_did["Std. Error"], lpm_did["Std. Error"]),
  "\\addlinespace",
  sprintf("Hazard ratio & & & %.4f%s \\\\",
    exp(cox_did["coef"]), dur_stars(cox_did["Pr(>|z|)"])),
  sprintf("Log coefficient & & & %.4f \\\\", cox_did["coef"]),
  sprintf(" & & & (%.4f) \\\\", cox_did["se(coef)"]),
  "\\midrule",
  "Age bin controls & Yes & Yes & --- \\\\",
  "Facility FE / Strata & No & Yes & Strata \\\\",
  "Time scale & Calendar & Calendar & Tank age \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s \\\\",
    format(nrow(h3_dt), big.mark = ","),
    format(nobs(h3_lpm), big.mark = ","),
    format(h3_cox_age$n, big.mark = ",")),
  sprintf("Events & %s & & %s \\\\",
    format(sum(h3_dt$closure_event), big.mark = ","),
    format(h3_cox_age$nevent, big.mark = ",")),
  "\\bottomrule",
  "\\multicolumn{4}{p{0.92\\textwidth}}{\\footnotesize \\textit{Notes:} ",
  "Full at-risk panel (make-model sample). Column 1: complementary log-log ",
  "(discrete proportional hazard). Column 2: linear probability model with ",
  "facility fixed effects and state-clustered SE. Column 3: Cox proportional ",
  "hazard with tank age as the time axis and facility strata. ",
  "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}\\end{table}"
), file.path(OUTPUT_TABLES, "Table_Duration_Models.tex"))

fwrite(
  as.data.table(coef(summary(h3_cloglog)), keep.rownames = "term"),
  file.path(OUTPUT_TABLES, "Table_Duration_ClogLog.csv"))


# --- S8e: Cox event study (time-varying HRs parallel to OLS event study) ---
# Manual dummies excluding reference period, interacted with treatment

es_years <- sort(unique(main_sample$rel_year_bin))
es_years <- es_years[es_years != -1L]  # drop reference

for (yr in es_years) {
  vname <- paste0("ry_", ifelse(yr < 0, paste0("m", abs(yr)), yr))
  main_sample[, (vname) := as.integer(rel_year_bin == yr) * texas_treated]
}

ry_vars <- grep("^ry_", names(main_sample), value = TRUE)
cox_fml <- as.formula(paste(
  "Surv(surv_time, closure_event) ~",
  paste(ry_vars, collapse = " + "),
  "+ strata(panel_id)"
))

cox_es_manual <- coxph(cox_fml, data = main_sample,
                       cluster = main_sample$state, ties = "efron")

cox_es_dt <- as.data.table(
  summary(cox_es_manual)$coefficients, keep.rownames = "term")
setnames(cox_es_dt, c("term","coef","exp_coef","se","z","p"))

# Parse relative year from variable names
cox_es_dt[, rel_year := as.numeric(gsub("ry_m", "-", gsub("ry_", "", term)))]
cox_es_dt <- cox_es_dt[!is.na(rel_year)]
cox_es_dt[, `:=`(
  hr    = exp_coef,
  ci_lo = exp(coef - 1.96 * se),
  ci_hi = exp(coef + 1.96 * se),
  period = fifelse(rel_year < 0, "Pre", "Post")
)]

# Add reference point (normalized to HR = 1)
cox_es_dt <- rbind(cox_es_dt,
  data.table(term = "ref", coef = 0, exp_coef = 1, se = 0, z = 0, p = 1,
             rel_year = -1, hr = 1, ci_lo = 1, ci_hi = 1, period = "Ref"),
  fill = TRUE)
setorder(cox_es_dt, rel_year)

p_cox_es <- ggplot(cox_es_dt,
  aes(x = rel_year, y = hr)) +
  annotate("rect", xmin = -Inf, xmax = -0.5, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50",
             linewidth = 0.5) +
  geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.6) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, fill = "grey40") +
  geom_line(color = "grey40", linewidth = 0.4, alpha = 0.6) +
  geom_point(aes(color = period), size = 2.5) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi, color = period),
                width = 0.25, linewidth = 0.5) +
  scale_color_manual(values = c(Pre = COL_CTRL, Post = COL_TX, Ref = "black"),
                     guide = "none") +
  labs(x = "Years Relative to Treatment (1999)", y = "Hazard Ratio") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_EventStudy.png"),
       p_cox_es, width = 10, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_EventStudy.pdf"),
       p_cox_es, width = 10, height = 5.5, device = cairo_pdf)

# Clean up temporary dummy variables
main_sample[, (ry_vars) := NULL]


# --- S8f: Kaplan-Meier survival curves ---
# Four panels: TX pre, TX post, Control pre, Control post
# Clean faceted KM showing survival probability over panel time

main_sample[, km_group := factor(paste0(
  fifelse(texas_treated == 1, "Texas", "Control"), ", ",
  fifelse(panel_year < POST_YEAR, "Pre-Reform", "Post-Reform")),
  levels = c("Control, Pre-Reform", "Control, Post-Reform",
             "Texas, Pre-Reform",   "Texas, Post-Reform"))]

# Compute per-group KM fits from the first observation year of each panel spell
km_fit <- survfit(Surv(surv_time, closure_event) ~ km_group, data = main_sample)
km_tidy <- as.data.table(broom::tidy(km_fit))
km_tidy[, group := gsub("km_group=", "", strata)]
km_tidy[, group := factor(group, levels = c(
  "Control, Pre-Reform", "Control, Post-Reform",
  "Texas, Pre-Reform",   "Texas, Post-Reform"))]

p_km <- ggplot(km_tidy, aes(x = time, y = estimate, color = group)) +
  geom_step(linewidth = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.08, color = NA, stat = "identity") +
  scale_color_manual(values = c(
    "Control, Pre-Reform"  = COL_CTRL,
    "Control, Post-Reform" = "#56B4E9",
    "Texas, Pre-Reform"    = "#E69F00",
    "Texas, Post-Reform"   = COL_TX)) +
  scale_fill_manual(values = c(
    "Control, Pre-Reform"  = COL_CTRL,
    "Control, Post-Reform" = "#56B4E9",
    "Texas, Pre-Reform"    = "#E69F00",
    "Texas, Post-Reform"   = COL_TX)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(x = "Years in Panel", y = "Survival Probability") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_KM_Survival.png"),
       p_km, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_KM_Survival.pdf"),
       p_km, width = 9, height = 5.5, device = cairo_pdf)


# --- S8g: Age-at-closure histogram ---

h3_closed <- main_sample[
  closure_event == 1 & !is.na(avg_tank_age),
  .(avg_tank_age,
    state_group = fifelse(texas_treated == 1, "Texas", "Control"),
    period      = fifelse(panel_year < POST_YEAR, "Pre-Reform", "Post-Reform"))
]
h3_closed[, group := factor(paste(state_group, period), levels = c(
  "Control Pre-Reform","Control Post-Reform",
  "Texas Pre-Reform","Texas Post-Reform"))]

h3_stats <- h3_closed[,
  .(n = .N, mean = mean(avg_tank_age), median = median(avg_tank_age)),
  by = group][order(group)]

fwrite(h3_stats, file.path(OUTPUT_TABLES, "Table_AgeAtClosure_Stats.csv"))

p_h3_hist <- ggplot(h3_closed, aes(x = avg_tank_age, fill = group)) +
  geom_histogram(binwidth = 1, alpha = 0.85) +
  facet_wrap(~group, ncol = 2, scales = "free_y") +
  geom_vline(data = h3_stats, aes(xintercept = median),
             linetype = "dashed", linewidth = 0.5) +
  scale_fill_manual(values = c(
    "Control Pre-Reform" = COL_CTRL, "Control Post-Reform" = "#56B4E9",
    "Texas Pre-Reform"   = "#E69F00", "Texas Post-Reform"   = COL_TX),
    guide = "none") +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  labs(x = "Tank Age at Closure (years)", y = "Count") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_Histogram_AgeAtClosure.png"),
       p_h3_hist, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Histogram_AgeAtClosure.pdf"),
       p_h3_hist, width = 10, height = 6, device = cairo_pdf)


# --- S8h: Wall type Cox (requires sw_broader_sample) ---

if (exists("sw_broader_sample") && wall_col %in% names(sw_broader_sample)) {
  if (!"did_term" %in% names(sw_broader_sample))
    sw_broader_sample[,
      did_term := as.integer(texas_treated == 1 & panel_year >= POST_YEAR)]

  setorder(sw_broader_sample, panel_id, panel_year)
  sw_broader_sample[, surv_time := seq_len(.N), by = panel_id]

  cox_wall_base <- coxph(as.formula(sprintf(
    "Surv(surv_time, closure_event) ~ did_term + %s + strata(panel_id)",
    wall_col)),
    data = sw_broader_sample, cluster = sw_broader_sample$state, ties = "efron")

  cox_wall_interact <- coxph(as.formula(sprintf(
    "Surv(surv_time, closure_event) ~ did_term + %s + did_term:%s + strata(panel_id)",
    wall_col, wall_col)),
    data = sw_broader_sample, cluster = sw_broader_sample$state, ties = "efron")

  # Wall type Cox table (LaTeX)
  cox_w_base_s <- summary(cox_wall_base)$coefficients
  cox_w_int_s  <- summary(cox_wall_interact)$coefficients

  writeLines(c(
    "\\begin{table}[htbp]\\centering",
    "\\caption{Cox Proportional Hazard: Wall Construction Heterogeneity}",
    "\\label{tbl:cox_wall}",
    "\\begin{tabular}{lcc}\\toprule",
    " & (1) & (2) \\\\",
    " & Base & Interaction \\\\\\midrule",
    sprintf("Texas $\\times$ Post (HR) & %.3f%s & %.3f%s \\\\",
      exp(cox_w_base_s["did_term","coef"]),
      dur_stars(cox_w_base_s["did_term","Pr(>|z|)"]),
      exp(cox_w_int_s["did_term","coef"]),
      dur_stars(cox_w_int_s["did_term","Pr(>|z|)"])),
    sprintf(" & (%.4f) & (%.4f) \\\\",
      cox_w_base_s["did_term","se(coef)"],
      cox_w_int_s["did_term","se(coef)"]),
    {
      int_row <- grep(paste0("did_term:", wall_col), rownames(cox_w_int_s))
      if (length(int_row) > 0) {
        c(sprintf("$\\times$ Single-walled (HR) & & %.3f%s \\\\",
          exp(cox_w_int_s[int_row,"coef"]),
          dur_stars(cox_w_int_s[int_row,"Pr(>|z|)"])),
          sprintf(" & & (%.4f) \\\\", cox_w_int_s[int_row,"se(coef)"]))
      } else character(0)
    },
    "\\midrule",
    "Facility strata & Yes & Yes \\\\",
    sprintf("Observations & %s & %s \\\\",
      format(cox_wall_base$n, big.mark = ","),
      format(cox_wall_interact$n, big.mark = ",")),
    sprintf("Events & %s & %s \\\\",
      format(cox_wall_base$nevent, big.mark = ","),
      format(cox_wall_interact$nevent, big.mark = ",")),
    "\\bottomrule",
    "\\multicolumn{3}{p{0.85\\textwidth}}{\\footnotesize \\textit{Notes:} ",
    "Broader sample including single- and double-walled facilities. ",
    "Hazard ratios reported. SE (of log-HR) in parentheses. ",
    "Clustered at state. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
    "\\end{tabular}\\end{table}"
  ), file.path(OUTPUT_TABLES, "Table_Cox_WallType.tex"))

  fwrite(
    as.data.table(cox_w_int_s, keep.rownames = "term"),
    file.path(OUTPUT_TABLES, "Table_Cox_WallType.csv"))
}


#### S9 Youngest Subsample ####

m_youngest_simple <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  youngest_sample, cluster = ~state)
m_youngest_age_ctrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  youngest_sample, cluster = ~state)

save_did_table(
  models    = list(m_youngest_simple, m_youngest_age_ctrl),
  headers   = c("Simple DiD", "+ Age Control"),
  base_name = "Table5_Youngest_MakeModel",
  title     = "Youngest Subsample (age 5 or less)")

model_es_youngest <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  youngest_sample, cluster = ~state)

es_youngest_out <- plot_es(
  model_es_youngest,
  xlim_lo = rel_min_youngest, xlim_hi = rel_max_full)


#### S10 Oldest Subsample ####

m_oldest_simple <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  oldest_sample, cluster = ~state)
m_oldest_age_ctrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  oldest_sample, cluster = ~state)

save_did_table(
  models    = list(m_oldest_simple, m_oldest_age_ctrl),
  headers   = c("Simple DiD", "+ Age Control"),
  base_name = "Table6_Oldest_MakeModel",
  title     = "Oldest Subsample (age over 5)")

model_es_oldest <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  oldest_sample, cluster = ~state)

es_oldest_out <- plot_es(
  model_es_oldest,
  xlim_lo = rel_min_oldest, xlim_hi = rel_max_full)

# Figure 7: combined youngest vs oldest event studies
p_combined_es <- (es_youngest_out$plot +
    labs(y = "Effect on Closure Probability") +
    annotate("text", x = -Inf, y = Inf, label = "A",
             hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5)) /
  (es_oldest_out$plot +
    labs(y = "Effect on Closure Probability") +
    annotate("text", x = -Inf, y = Inf, label = "B",
             hjust = -0.5, vjust = 1.5, fontface = "bold", size = 5))

ggsave(file.path(OUTPUT_FIGURES, "Figure_7_ES_Youngest_Oldest.png"),
       p_combined_es, width = 10, height = 10, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_7_ES_Youngest_Oldest.pdf"),
       p_combined_es, width = 10, height = 10, device = cairo_pdf)


#### S11 Reported Leak DiD ####

m_leak_main     <- feols(leak_year ~ did_term | panel_id + panel_year,
                         main_sample,     cluster = ~state)
m_leak_youngest <- feols(leak_year ~ did_term | panel_id + panel_year,
                         youngest_sample, cluster = ~state)
m_leak_oldest   <- feols(leak_year ~ did_term | panel_id + panel_year,
                         oldest_sample,   cluster = ~state)

save_did_table(
  models    = list(m_leak_main, m_leak_youngest, m_leak_oldest),
  headers   = c("Full Make-Model", "Youngest", "Oldest"),
  base_name = "Table7_Leak_MakeModel",
  title     = "Reported Leak Probability")

model_es_leak <- feols(
  leak_year ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  main_sample, cluster = ~state)

plot_es(model_es_leak,
  ylab    = "Effect on Reported Leak Probability",
  xlim_lo = -8, xlim_hi = rel_max_full,
  filename = file.path(OUTPUT_FIGURES, "Figure_Leak_ES_MakeModel.png"))


#### S12 Wall Type Heterogeneity (Broader Sample) ####

fml_wall_broad <- as.formula(sprintf(
  "closure_event ~ did_term + did_term:%s + %s | panel_id + panel_year",
  wall_col, wall_col))
m_hte_wall_broad <- feols(fml_wall_broad, sw_broader_sample, cluster = ~state)

sw_broad_pre <- sw_broader_sample[panel_year < POST_YEAR]
m_h4_pre <- feols(as.formula(sprintf(
  "closure_event ~ texas_treated + texas_treated:%s + %s | panel_id + panel_year",
  wall_col, wall_col)), sw_broad_pre, cluster = ~state)

save_did_table(
  models    = list(m_hte_wall_broad, m_h4_pre),
  headers   = c("Broader Sample", "Pre-Period Falsification"),
  base_name = "Table9_WallType_BroaderSW",
  title     = "Wall Type Heterogeneity")


#### S13 Age at Closure (Descriptive OLS) ####

if (!"spec_A_eligible" %in% names(closed_tanks)) {
  closed_tanks <- merge(closed_tanks,
    unique(annual_data[, .(panel_id, spec_A_eligible)]),
    by = "panel_id", all.x = TRUE)
  closed_tanks[is.na(spec_A_eligible), spec_A_eligible := 0L]
}

sw_flag_1998 <- unique(annual_data[panel_year == TREATMENT_YEAR - 1L,
  .(panel_id, fac_all_sw_1998 = as.integer(get(wall_col) >= 0.5))])
if (!"fac_all_sw_1998" %in% names(closed_tanks))
  closed_tanks <- merge(closed_tanks, sw_flag_1998, by = "panel_id", all.x = TRUE)

mm_ids <- unique(main_sample$panel_id)
closed_tanks[, in_make_model := as.integer(panel_id %in% mm_ids)]

h3_data <- closed_tanks[
  !is.na(age_at_closure) & !is.na(county_fips_fac) &
  spec_A_eligible == 1   &
  closure_year %between% c(PANEL_START, PANEL_END)
][, `:=`(
  texas      = as.integer(state == "TX"),
  post       = as.integer(closure_year >= POST_YEAR),
  texas_post = as.integer(state == "TX") * as.integer(closure_year >= POST_YEAR)
)]

h3_mm    <- h3_data[in_make_model == 1]
h3_specA <- h3_data

m_h3_mm    <- feols(age_at_closure ~ texas_post | county_fips_fac + closure_year,
                    h3_mm, cluster = ~state)
m_h3_specA <- feols(age_at_closure ~ texas_post | county_fips_fac + closure_year,
                    h3_specA, cluster = ~state)
m_h3_pre   <- feols(age_at_closure ~ texas | county_fips_fac + closure_year,
                    h3_specA[post == 0], cluster = ~state)

save_did_table(
  models    = list(m_h3_mm, m_h3_specA, m_h3_pre),
  headers   = c("Make-Model Closures", "Spec A Closures", "Pre-Period"),
  base_name = "Table8_AgeAtClosure",
  title     = "Age at Closure (Descriptive OLS)",
  tvar      = "texas_post")

# Age at closure time series
age_ts <- closed_tanks[
  spec_A_eligible == 1 & closure_year %between% c(1990, 2018) & !is.na(age_at_closure),
  .(mean_age = mean(age_at_closure, na.rm = TRUE), n = .N),
  by = .(closure_year, group = fifelse(state == "TX", "Texas", "Control"))
]
setorder(age_ts, group, closure_year)
age_ts[, mean_age_smooth := frollmean(mean_age, n = 3, align = "center"), by = group]

p_h3 <- ggplot(age_ts, aes(x = closure_year, y = mean_age,
                            color = group, shape = group)) +
  geom_vline(xintercept = POST_YEAR - 0.5, linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_line(aes(y = mean_age_smooth), linewidth = 1, na.rm = TRUE) +
  scale_color_manual(values = c(Texas = COL_TX, Control = COL_CTRL)) +
  scale_x_continuous(breaks = seq(1990, 2018, 4)) +
  labs(x = "Year of Closure", y = "Mean Tank Age at Closure (years)") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_AgeAtClosure_TimeSeries.png"),
       p_h3, width = 9, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_AgeAtClosure_TimeSeries.pdf"),
       p_h3, width = 9, height = 5, device = cairo_pdf)


#### S14 Theory-Evidence Summary ####

extract_h <- function(m, pattern, label, prediction, null_expected = FALSE) {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(pattern, rownames(ct))
  if (length(idx) == 0)
    return(data.table(Hypothesis = label, Prediction = prediction,
                      Estimate = NA, SE = NA, P = NA, Verdict = "Term not found"))
  r <- ct[tail(idx, 1), ]
  p <- r["Pr(>|t|)"]
  verdict <- if (null_expected) {
    fifelse(p > 0.10, "Supported (null as expected)", "FAILED (pre-trend)")
  } else {
    fcase(is.na(p), "N/A",
          p < 0.05 & r["Estimate"] > 0, "Supported",
          p >= 0.10, "Not rejected (null)",
          default = "Partial")
  }
  data.table(Hypothesis = label, Prediction = prediction,
             Estimate = round(r["Estimate"], 5),
             SE = round(r["Std. Error"], 5),
             P = round(p, 4), Verdict = verdict)
}

theory_tbl <- rbindlist(list(
  extract_h(m_did_4bin_hte, "did_term:age_treat_bin",
            "Age gradient (4-bin, oldest bin)",
            "ATT rising in age"),
  extract_h(m_youngest_simple, "did_term",
            "ATT(young, age 5 or less)",
            "ATT small or zero"),
  extract_h(m_oldest_simple, "did_term",
            "ATT(old, age over 5)",
            "ATT(old) > ATT(young)"),
  extract_h(m_h3_mm, "texas_post",
            "Age-at-closure shift (make-model OLS)",
            "TX post-reform closures older"),
  extract_h(m_h3_pre, "texas",
            "Pre-period falsification",
            "No TX-control age gap pre-reform", null_expected = TRUE),
  extract_h(m_hte_wall_broad, paste0("did_term:", wall_col),
            "Wall type sensitivity (broader sample)",
            "Single-wall responds more than double-wall")
))

fwrite(theory_tbl, file.path(OUTPUT_TABLES, "Table10_Theory_Evidence.csv"))


#### S15 Robustness Checks ####

m_noMD_main     <- feols(closure_event ~ did_term | panel_id + panel_year,
                         main_sample[state != "MD"],     cluster = ~state)
m_noMD_youngest <- feols(closure_event ~ did_term | panel_id + panel_year,
                         youngest_sample[state != "MD"], cluster = ~state)
m_noMD_oldest   <- feols(closure_event ~ did_term | panel_id + panel_year,
                         oldest_sample[state != "MD"],   cluster = ~state)

save_did_table(
  models    = list(m_noMD_main, m_noMD_youngest, m_noMD_oldest),
  headers   = c("Full (no MD)", "Youngest (no MD)", "Oldest (no MD)"),
  base_name = "TableB5_MD_Excluded",
  title     = "Maryland-Excluded Robustness")

m_mandate_main <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  main_sample, cluster = ~state)
m_mandate_youngest <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  youngest_sample, cluster = ~state)

save_did_table(
  models    = list(m_did_pooled, m_mandate_main, m_youngest_simple, m_mandate_youngest),
  headers   = c("Main", "Main + mandate", "Youngest", "Youngest + mandate"),
  base_name = "TableB6_Mandate_Sensitivity",
  title     = "Mandate Control Sensitivity")

main_tight <- annual_data[
  single_tanks == active_tanks & has_gasoline_year == 1 &
  install_year %between% c(1992L, 1997L)]
main_tight[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), -7L)]

m_tight <- feols(closure_event ~ did_term | panel_id + panel_year,
                 main_tight, cluster = ~state)

save_did_table(
  models    = list(m_did_pooled, m_tight),
  headers   = c("1990-1997 install", "1992-1997 install"),
  base_name = "TableB7_InstallWindow",
  title     = "Install Year Window Sensitivity")


#### S16 Cox Proportional Hazard Models ####

# Calendar-time and age-time Cox on make-model sample
# These complement the S8 duration models with alternative time origins

cox_main <- copy(main_sample)
cox_main[, `:=`(tstart    = panel_year - 1L,
                tstop     = panel_year,
                age_start = avg_tank_age - 1,
                age_stop  = avg_tank_age)]
cox_main <- cox_main[tstop > tstart]

m_cox_cal <- coxph(
  Surv(tstart, tstop, closure_event) ~ did_term + mandate_active,
  data = cox_main, cluster = state)

cox_main_age <- cox_main[age_start >= 0 & age_stop > age_start]
m_cox_age <- coxph(
  Surv(age_start, age_stop, closure_event) ~ did_term + mandate_active + panel_year,
  data = cox_main_age, cluster = state)

cox_results <- rbindlist(lapply(list(
  list(m = m_cox_cal, label = "Calendar time"),
  list(m = m_cox_age, label = "Tank age")
), function(x) {
  s <- summary(x$m)$coefficients
  data.table(Model    = x$label,
             HR       = round(exp(s["did_term","coef"]), 4),
             coef     = round(s["did_term","coef"], 4),
             se       = round(s["did_term","se(coef)"], 4),
             p        = round(s["did_term","Pr(>|z|)"], 4),
             n_events = x$m$nevent)
}))

fwrite(cox_results, file.path(OUTPUT_TABLES, "Table_Cox_DiD_MakeModel.csv"))

# LaTeX table: Cox DiD comparison
cal_s <- summary(m_cox_cal)$coefficients
age_s <- summary(m_cox_age)$coefficients

writeLines(c(
  "\\begin{table}[htbp]\\centering",
  "\\caption{Cox Proportional Hazard Estimates: Calendar Time vs.\\ Tank Age Origin}",
  "\\label{tbl:cox_did}",
  "\\begin{tabular}{lcc}\\toprule",
  " & (1) & (2) \\\\",
  " & Calendar time & Tank age \\\\\\midrule",
  sprintf("Texas $\\times$ Post (HR) & %.3f%s & %.3f%s \\\\",
    exp(cal_s["did_term","coef"]),
    stars_fn(cal_s["did_term","Pr(>|z|)"]),
    exp(age_s["did_term","coef"]),
    stars_fn(age_s["did_term","Pr(>|z|)"])),
  sprintf("Log coefficient & %.4f & %.4f \\\\",
    cal_s["did_term","coef"], age_s["did_term","coef"]),
  sprintf(" & (%.4f) & (%.4f) \\\\",
    cal_s["did_term","se(coef)"], age_s["did_term","se(coef)"]),
  "\\addlinespace",
  sprintf("Mandate control & %.3f%s & %.3f%s \\\\",
    exp(cal_s["mandate_active","coef"]),
    stars_fn(cal_s["mandate_active","Pr(>|z|)"]),
    exp(age_s["mandate_active","coef"]),
    stars_fn(age_s["mandate_active","Pr(>|z|)"])),
  "\\midrule",
  "Time origin & Calendar year & Tank age \\\\",
  sprintf("Observations & %s & %s \\\\",
    format(m_cox_cal$n, big.mark = ","),
    format(m_cox_age$n, big.mark = ",")),
  sprintf("Events & %s & %s \\\\",
    format(m_cox_cal$nevent, big.mark = ","),
    format(m_cox_age$nevent, big.mark = ",")),
  "\\bottomrule",
  "\\multicolumn{3}{p{0.85\\textwidth}}{\\footnotesize \\textit{Notes:} ",
  "Make-model sample. Counting-process Cox models. Hazard ratios reported; ",
  "log-HR SE in parentheses. Clustered at state. ",
  "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}\\end{table}"
), file.path(OUTPUT_TABLES, "Table_Cox_DiD_MakeModel.tex"))


#### S17 Diagnostic Data Export ####

saveRDS(model_es_youngest, file.path(ANALYSIS_DIR, "mm_youngest_event_study.rds"))
saveRDS(model_es_oldest,   file.path(ANALYSIS_DIR, "mm_oldest_event_study.rds"))
saveRDS(m_did_pooled,      file.path(ANALYSIS_DIR, "mm_headline_did_model.rds"))
saveRDS(pt_results,        file.path(ANALYSIS_DIR, "mm_pt_validation_results.rds"))
saveRDS(m_es_hte,          file.path(ANALYSIS_DIR, "mm_hte_event_study.rds"))

fwrite(texas_share_by_period(main_sample),
  file.path(OUTPUT_TABLES, "Diag_TXShare_Main.csv"))
fwrite(texas_share_by_period(youngest_sample),
  file.path(OUTPUT_TABLES, "Diag_TXShare_Youngest.csv"))
fwrite(texas_share_by_period(oldest_sample),
  file.path(OUTPUT_TABLES, "Diag_TXShare_Oldest.csv"))


#### S18 Publication LaTeX Tables ####

# Cross-spec summary
spec_summary <- data.table(
  Model = c("Make-Model Full","Youngest","Oldest",
            "Leak (Main)","Leak (Youngest)","Leak (Oldest)"),
  beta  = sapply(list(m_did_pooled, m_youngest_simple, m_oldest_simple,
                      m_leak_main, m_leak_youngest, m_leak_oldest),
                 function(m) extract_did(m)$beta),
  se    = sapply(list(m_did_pooled, m_youngest_simple, m_oldest_simple,
                      m_leak_main, m_leak_youngest, m_leak_oldest),
                 function(m) extract_did(m)$se),
  p     = sapply(list(m_did_pooled, m_youngest_simple, m_oldest_simple,
                      m_leak_main, m_leak_youngest, m_leak_oldest),
                 function(m) extract_did(m)$p))

fwrite(spec_summary, file.path(OUTPUT_TABLES, "Cross_Spec_Summary_MakeModel.csv"))

# Age split LaTeX table (youngest vs oldest)
cy <- lapply(list(m_youngest_simple, m_youngest_age_ctrl), extract_did)
co <- lapply(list(m_oldest_simple,   m_oldest_age_ctrl),   extract_did)

writeLines(c(
  "\\begin{table}[htbp]\\centering",
  "\\caption{Age Heterogeneity: Youngest vs.\\ Oldest Subsamples}",
  "\\label{tbl:mm_age_hte}",
  "\\begin{tabular}{lcccc}\\toprule",
  " & \\multicolumn{2}{c}{\\textbf{Youngest ($\\leq$5 yrs)}} & \\multicolumn{2}{c}{\\textbf{Oldest ($>$5 yrs)}}\\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  " & (1) & (2) & (3) & (4)\\\\ & Simple & +Age & Simple & +Age\\\\\\midrule",
  sprintf("Texas $\\times$ Post & %s%s & %s%s & %s%s & %s%s\\\\",
    sprintf("%.4f", cy[[1]]$beta), stars_fn(cy[[1]]$p),
    sprintf("%.4f", cy[[2]]$beta), stars_fn(cy[[2]]$p),
    sprintf("%.4f", co[[1]]$beta), stars_fn(co[[1]]$p),
    sprintf("%.4f", co[[2]]$beta), stars_fn(co[[2]]$p)),
  sprintf(" & (%.4f) & (%.4f) & (%.4f) & (%.4f)\\\\",
    cy[[1]]$se, cy[[2]]$se, co[[1]]$se, co[[2]]$se),
  "\\midrule",
  "Age bin control & No & Yes & No & Yes\\\\",
  "Facility + Year FE & Yes & Yes & Yes & Yes\\\\\\midrule",
  sprintf("Observations & %s & %s & %s & %s\\\\",
    format(cy[[1]]$n,big.mark=","), format(cy[[2]]$n,big.mark=","),
    format(co[[1]]$n,big.mark=","), format(co[[2]]$n,big.mark=",")),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:} ",
  "Make-model sample split at mean tank age in 1998 = 5 years. ",
  "SE clustered at state. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}\\end{table}"
), file.path(OUTPUT_TABLES, "JMP_Table56_AgeSplit_MakeModel.tex"))

message(sprintf("02_DiD_Main_MakeModel.R complete | %s", Sys.time()))
message(sprintf("  Tables: %s", OUTPUT_TABLES))
message(sprintf("  Figures: %s", OUTPUT_FIGURES))