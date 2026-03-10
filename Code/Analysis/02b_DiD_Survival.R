################################################################################
# 02b_DiD_Survival.R
# Texas UST Insurance Reform — Survival and Duration Analysis
#
# PREREQUISITES: 02a_DiD_OLS.R
#   (saves main_sample.rds, youngest_sample.rds, oldest_sample.rds,
#    sw_broader_sample.rds to Data/Analysis)
#
# SECTIONS:
#   S1   Setup
#   S2   Helper Functions
#   S3   Load Analysis Samples
#   S4   Counting-Process Time Variables
#   S5   Survivorship Diagnostic
#   S6   Binned Scatter: Mean Closure Rate by Age Group and Period
#   S7   Cox Age Gradient and Forest Plot
#   S8   Headline Cloglog Fixed-Effects Model
#   S9   Cloglog HTE and Subsample Models
#   S10  Discrete Hazard Profile Figure
#   S11  Duration Model Comparison Table
#   S12  Cox Event Study
#   S13  Kaplan-Meier Survival Curves
#   S14  Age-at-Closure Histogram
#   S15  Wall Type Cox Models
#   S16  Additional Cox Models with Mandate Control
#   S17  Diagnostic Export
################################################################################


#### S1 Setup ####

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(here)
  library(ggplot2)
  library(broom)
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

POST_YEAR   <- 1999L
COL_TX      <- "#D55E00"
COL_CTRL    <- "#0072B2"
COL_YOUNG   <- "#009E73"
COL_OLD     <- "#CC79A7"


#### S2 Helper Functions ####

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

stars_surv <- function(p) {
  fcase(is.na(p), "",
        p < 0.01,  "$^{***}$",
        p < 0.05,  "$^{**}$",
        p < 0.10,  "$^{*}$",
        default =  "")
}

fmt_n   <- function(n) format(n, big.mark = ",", scientific = FALSE)
fmt_est <- function(x, p)  sprintf("%.4f%s", x, stars_surv(p))
fmt_se  <- function(x)     sprintf("(%.4f)", x)
fmt_hr  <- function(x, p)  sprintf("%.3f%s", x, stars_surv(p))

extract_cox_row <- function(m, term_name) {
  s   <- summary(m)$coefficients
  idx <- grep(term_name, rownames(s), fixed = TRUE)[1]
  list(hr   = s[idx, "exp(coef)"],
       coef = s[idx, "coef"],
       se   = s[idx, "se(coef)"],
       p    = s[idx, "Pr(>|z|)"],
       n    = m$n,
       ev   = m$nevent)
}

extract_cll_row <- function(m, term_name = "did_term") {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(term_name, rownames(ct), fixed = TRUE)[1]
  list(coef = ct[idx, "Estimate"],
       se   = ct[idx, "Std. Error"],
       p    = ct[idx, "Pr(>|t|)"],
       n    = nobs(m))
}

write_tex <- function(lines, filename) {
  writeLines(lines, file.path(OUTPUT_TABLES, filename))
}


#### S3 Load Analysis Samples ####

main_sample       <- readRDS(file.path(ANALYSIS_DIR, "main_sample.rds"))
youngest_sample   <- readRDS(file.path(ANALYSIS_DIR, "youngest_sample.rds"))
oldest_sample     <- readRDS(file.path(ANALYSIS_DIR, "oldest_sample.rds"))
sw_broader_sample <- readRDS(file.path(ANALYSIS_DIR, "sw_broader_sample.rds"))

wall_col <- intersect(
  c("pct_single_wall", "has_single_walled", "any_single_walled"),
  names(main_sample))[1]


#### S4 Counting-Process Time Variables ####

# tstart/tstop : calendar-year counting-process intervals
#                direct analog of panel_year fixed effects in OLS
# age_start/age_stop : tank-age intervals for structural age-time Cox
# tenure : years-in-panel, used only for Kaplan-Meier (not Cox)

setorder(main_sample,     panel_id, panel_year)
setorder(youngest_sample, panel_id, panel_year)
setorder(oldest_sample,   panel_id, panel_year)

main_sample[, `:=`(
  tstart    = panel_year - 1L,
  tstop     = panel_year,
  age_start = pmax(avg_tank_age - 1, 0),
  age_stop  = avg_tank_age,
  tenure    = seq_len(.N)),
  by = panel_id]

youngest_sample[, `:=`(
  tstart    = panel_year - 1L,
  tstop     = panel_year,
  age_start = pmax(avg_tank_age - 1, 0),
  age_stop  = avg_tank_age),
  by = panel_id]

oldest_sample[, `:=`(
  tstart    = panel_year - 1L,
  tstop     = panel_year,
  age_start = pmax(avg_tank_age - 1, 0),
  age_stop  = avg_tank_age),
  by = panel_id]

year_levels <- sort(unique(main_sample$panel_year))
main_sample[, year_fac := factor(panel_year, levels = year_levels)]


#### S5 Survivorship Diagnostic ####

surv_diag_dt <- main_sample[,
  .(exited_pre_reform = as.integer(
      any(closure_event == 1 & panel_year < POST_YEAR)),
    age_treat_bin     = first(age_treat_bin),
    texas             = first(texas_treated)),
  by = panel_id]

surv_summary <- surv_diag_dt[!is.na(age_treat_bin),
  .(n_facilities    = .N,
    n_exited_before = sum(exited_pre_reform,  na.rm = TRUE),
    pct_exited      = mean(exited_pre_reform, na.rm = TRUE)),
  by = .(age_treat_bin,
         state_group = fifelse(texas == 1, "Texas", "Control States"))
][order(state_group, age_treat_bin)]

fwrite(surv_summary,
       file.path(OUTPUT_TABLES, "Table_Survivorship_Diagnostic.csv"))


#### S6 Binned Scatter: Mean Closure Rate by Age Group and Period ####

scatter_dt <- main_sample[!is.na(age_treat_bin),
  .(closure_rate = mean(closure_event, na.rm = TRUE), n = .N),
  by = .(
    age_treat_bin,
    state_group = fifelse(texas_treated == 1, "Texas", "Control States"),
    period      = fifelse(panel_year < POST_YEAR,
                          "Pre-reform", "Post-reform"))
]
scatter_dt[, group_period := factor(
  paste(state_group, period),
  levels = c("Control States Pre-reform",  "Control States Post-reform",
             "Texas Pre-reform",            "Texas Post-reform"))]

p_scatter <- ggplot(
  scatter_dt[!is.na(age_treat_bin)],
  aes(x = age_treat_bin, y = closure_rate,
      color = group_period, linetype = group_period,
      group = group_period)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Control States Pre-reform"  = COL_CTRL,
    "Control States Post-reform" = "#56B4E9",
    "Texas Pre-reform"           = "#E69F00",
    "Texas Post-reform"          = COL_TX)) +
  scale_linetype_manual(values = c(
    "Control States Pre-reform"  = "dashed",
    "Control States Post-reform" = "solid",
    "Texas Pre-reform"           = "dashed",
    "Texas Post-reform"          = "solid")) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Mean Tank Age at Reform (years)",
       y = "Mean Annual Closure Rate") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_BinnedScatter_AgeClosure.png"),
       p_scatter, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_BinnedScatter_AgeClosure.pdf"),
       p_scatter, width = 9, height = 5.5, device = cairo_pdf)


#### S7 Cox Age Gradient and Forest Plot ####

cox_base_age <- coxph(
  Surv(tstart, tstop, closure_event) ~
    did_term + year_fac + strata(panel_id),
  data    = main_sample,
  cluster = main_sample$state,
  ties    = "efron")

cox_age_interact <- coxph(
  Surv(tstart, tstop, closure_event) ~
    did_term + age_bin + did_term:age_bin + year_fac + strata(panel_id),
  data    = main_sample,
  cluster = main_sample$state,
  ties    = "efron")

cox_base_s    <- summary(cox_base_age)$coefficients
cox_int_s     <- summary(cox_age_interact)$coefficients

base_hr_dt <- data.table(
  label = "Pooled",
  hr    = cox_base_s["did_term", "exp(coef)"],
  coef  = cox_base_s["did_term", "coef"],
  se    = cox_base_s["did_term", "se(coef)"])

interact_idx <- grep("did_term:age_bin", rownames(cox_int_s))
interact_dt  <- data.table(
  label = gsub("did_term:age_bin", "", rownames(cox_int_s)[interact_idx]),
  hr    = cox_int_s[interact_idx, "exp(coef)"],
  coef  = cox_int_s[interact_idx, "coef"],
  se    = cox_int_s[interact_idx, "se(coef)"])

forest_dt <- rbind(base_hr_dt, interact_dt)
forest_dt[, `:=`(
  ci_lo = exp(coef - 1.96 * se),
  ci_hi = exp(coef + 1.96 * se))]
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


#### S8 Headline Cloglog Fixed-Effects Model ####

# Prentice-Gloeckler complementary log-log.
# Identical sample, FE structure, and identifying variation as OLS TWFE.
# Coefficient on did_term = log-hazard ratio conditional on survival to t.

h3_dt <- main_sample[!is.na(avg_tank_age) & !is.na(did_term) & tstop > tstart]

h3_cloglog_fe <- feglm(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  family  = binomial(link = "cloglog"),
  data    = h3_dt,
  cluster = ~state)


#### S9 Cloglog HTE and Subsample Models ####

# Binary old/young interaction — mirrors m_did_interact in OLS script
h3_cloglog_hte <- feglm(
  closure_event ~ did_term + did_term:old_at_treat + age_bin |
    panel_id + panel_year,
  family  = binomial(link = "cloglog"),
  data    = h3_dt,
  cluster = ~state)

# Young subsample
h3_cloglog_young <- feglm(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  family  = binomial(link = "cloglog"),
  data    = youngest_sample[!is.na(avg_tank_age) & !is.na(did_term)],
  cluster = ~state)

# Old subsample
h3_cloglog_old <- feglm(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  family  = binomial(link = "cloglog"),
  data    = oldest_sample[!is.na(avg_tank_age) & !is.na(did_term)],
  cluster = ~state)

# OLS LPM on identical sample — direct reference comparison
h3_lpm <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  h3_dt, cluster = ~state)


#### S10 Discrete Hazard Profile Figure ####

# In-sample predicted hazard from headline cloglog, averaged by group × age bin.
# Groups: control (flat actuarial rate), Texas pre-reform, Texas post-reform.

h3_dt[, pred_hazard := predict(h3_cloglog_fe, type = "response")]

hazard_profile_dt <- h3_dt[!is.na(pred_hazard) & !is.na(age_bin),
  .(mean_hazard = mean(pred_hazard, na.rm = TRUE)),
  by = .(
    age_bin,
    group = fcase(
      texas_treated == 0,                            "Control (community-rated)",
      texas_treated == 1 & panel_year < POST_YEAR,   "Texas pre-reform",
      texas_treated == 1 & panel_year >= POST_YEAR,  "Texas post-reform (experience-rated)"
    ))
]
hazard_profile_dt <- hazard_profile_dt[!is.na(group)]
hazard_profile_dt[, age_bin := factor(age_bin,
  levels = levels(main_sample$age_bin))]
hazard_profile_dt[, group := factor(group, levels = c(
  "Control (community-rated)",
  "Texas pre-reform",
  "Texas post-reform (experience-rated)"))]

p_hazard_profile <- ggplot(hazard_profile_dt,
  aes(x = age_bin, y = mean_hazard, color = group, group = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "Control (community-rated)"             = COL_CTRL,
    "Texas pre-reform"                      = "#E69F00",
    "Texas post-reform (experience-rated)"  = COL_TX)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Tank Age (years)",
       y = "Predicted Annual Closure Hazard") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_DiscreteHazard_AgeProfile.png"),
       p_hazard_profile, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_DiscreteHazard_AgeProfile.pdf"),
       p_hazard_profile, width = 9, height = 5.5, device = cairo_pdf)


#### S11 Duration Model Comparison Table ####

h3_dt[, year_fac := factor(panel_year, levels = year_levels)]

h3_cox_cal <- coxph(
  Surv(tstart, tstop, closure_event) ~
    did_term + age_bin + year_fac + strata(panel_id),
  data    = h3_dt,
  cluster = h3_dt$state,
  ties    = "efron")

h3_cox_age <- coxph(
  Surv(age_start, age_stop, closure_event) ~
    did_term + year_fac + strata(panel_id),
  data    = h3_dt[age_stop > age_start],
  cluster = h3_dt[age_stop > age_start]$state,
  ties    = "efron")

# Extract all cells before writing
lpm_ct      <- coeftable(summary(h3_lpm, cluster = ~state))
lpm_est     <- sprintf("%.4f%s", lpm_ct["did_term","Estimate"],
                        stars_surv(lpm_ct["did_term","Pr(>|t|)"]))
lpm_se      <- fmt_se(lpm_ct["did_term","Std. Error"])
lpm_n       <- fmt_n(nobs(h3_lpm))

cll_ct      <- coeftable(summary(h3_cloglog_fe, cluster = ~state))
cll_est     <- sprintf("%.4f%s", cll_ct["did_term","Estimate"],
                        stars_surv(cll_ct["did_term","Pr(>|t|)"]))
cll_se      <- fmt_se(cll_ct["did_term","Std. Error"])
cll_n       <- fmt_n(nobs(h3_cloglog_fe))

n_events_dt <- fmt_n(sum(h3_dt$closure_event))

cox_c_s     <- summary(h3_cox_cal)$coefficients
cox_c_hr    <- fmt_hr(exp(cox_c_s["did_term","coef"]),
                       cox_c_s["did_term","Pr(>|z|)"])
cox_c_coef  <- sprintf("%.4f", cox_c_s["did_term","coef"])
cox_c_se    <- fmt_se(cox_c_s["did_term","se(coef)"])
cox_c_n     <- fmt_n(h3_cox_cal$n)
cox_c_ev    <- fmt_n(h3_cox_cal$nevent)

cox_a_s     <- summary(h3_cox_age)$coefficients
cox_a_hr    <- fmt_hr(exp(cox_a_s["did_term","coef"]),
                       cox_a_s["did_term","Pr(>|z|)"])
cox_a_coef  <- sprintf("%.4f", cox_a_s["did_term","coef"])
cox_a_se    <- fmt_se(cox_a_s["did_term","se(coef)"])
cox_a_n     <- fmt_n(h3_cox_age$n)
cox_a_ev    <- fmt_n(h3_cox_age$nevent)

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Effect of Reform on Annual Closure Hazard: Duration Model Estimates}",
  "\\label{tbl:duration_models}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) \\\\",
  " & OLS (reference) & Cloglog FE & Cox calendar & Cox age \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s & %s & --- & --- \\\\",
          lpm_est, cll_est),
  sprintf(" & %s & %s & & \\\\", lpm_se, cll_se),
  "\\addlinespace",
  sprintf("Hazard ratio & & & %s & %s \\\\", cox_c_hr, cox_a_hr),
  sprintf("Log coefficient & & & %s & %s \\\\", cox_c_coef, cox_a_coef),
  sprintf(" & & & %s & %s \\\\", cox_c_se, cox_a_se),
  "\\midrule",
  "Estimand & Probability & Log-hazard & Log-hazard & Log-hazard \\\\",
  "Conditioning & Unconditional & Survival to $t$ & Survival to $t$ & Survival to age \\\\",
  "Age controls & Yes & Yes & Yes & --- \\\\",
  "Facility FE / strata & FE & FE & Strata & Strata \\\\",
  "Year FE & Yes & Yes & Dummies & Covariate \\\\",
  "Time axis & Calendar & Calendar & Calendar & Tank age \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
          lpm_n, cll_n, cox_c_n, cox_a_n),
  sprintf("Events & %s & %s & %s & %s \\\\",
          n_events_dt, n_events_dt, cox_c_ev, cox_a_ev),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.98\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Make-model sample: single-product gasoline facilities",
  "installed 1990--1997, non-mandate cohorts, positive survival gap.",
  "Column~(1): OLS two-way FE, estimates the average treatment effect on",
  "unconditional annual closure probability.",
  "Column~(2): Prentice--Gloeckler complementary log-log; preferred",
  "specification. Estimates the treatment effect on the closure hazard",
  "conditional on survival to $t$. Identical sample, fixed effects,",
  "and identifying variation as column~(1).",
  "Columns~(3)--(4): counting-process Cox models.",
  "Column~(3) uses calendar year as the time axis with year dummies.",
  "Column~(4) uses tank age as the time axis.",
  "All models cluster standard errors at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_Duration_Models.tex")

fwrite(
  as.data.table(coeftable(h3_cloglog_fe), keep.rownames = "term"),
  file.path(OUTPUT_TABLES, "Table_Cloglog_FE_Coefficients.csv"))


#### S12 Cox Event Study ####

# Manual event-time dummies interacted with treatment, reference = -1
es_years <- sort(unique(main_sample$rel_year_bin))
es_years <- es_years[es_years != -1L]

for (yr in es_years) {
  vname <- paste0("ry_", fifelse(yr < 0, paste0("m", abs(yr)), as.character(yr)))
  main_sample[, (vname) := as.integer(rel_year_bin == yr) * texas_treated]
}
ry_vars <- grep("^ry_", names(main_sample), value = TRUE)

cox_es_fml <- as.formula(paste(
  "Surv(tstart, tstop, closure_event) ~",
  paste(ry_vars, collapse = " + "),
  "+ strata(panel_id)"))

cox_es_model <- coxph(cox_es_fml,
                      data    = main_sample,
                      cluster = main_sample$state,
                      ties    = "efron")

cox_es_coefs <- as.data.table(
  summary(cox_es_model)$coefficients, keep.rownames = "term")
setnames(cox_es_coefs, c("term","coef","exp_coef","se","z","p"))

cox_es_coefs[, rel_year := as.numeric(
  gsub("ry_m", "-", gsub("ry_", "", term)))]
cox_es_coefs <- cox_es_coefs[!is.na(rel_year)]
cox_es_coefs[, `:=`(
  hr     = exp_coef,
  ci_lo  = exp(coef - 1.96 * se),
  ci_hi  = exp(coef + 1.96 * se),
  period = fifelse(rel_year < 0, "Pre", "Post"))]

cox_es_coefs <- rbind(cox_es_coefs,
  data.table(term = "ref", coef = 0, exp_coef = 1, se = 0,
             z = 0, p = 1, rel_year = -1, hr = 1,
             ci_lo = 1, ci_hi = 1, period = "Ref"),
  fill = TRUE)
setorder(cox_es_coefs, rel_year)

p_cox_es <- ggplot(cox_es_coefs, aes(x = rel_year, y = hr)) +
  annotate("rect", xmin = -Inf, xmax = -0.5,
           ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.6) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              alpha = 0.12, fill = "grey40") +
  geom_line(color = "grey40", linewidth = 0.4, alpha = 0.6) +
  geom_point(aes(color = period), size = 2.5) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi, color = period),
                width = 0.25, linewidth = 0.5) +
  scale_color_manual(
    values = c(Pre = COL_CTRL, Post = COL_TX, Ref = "black"),
    guide  = "none") +
  labs(x = "Years Relative to Reform (1999)", y = "Hazard Ratio") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_EventStudy.png"),
       p_cox_es, width = 10, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_EventStudy.pdf"),
       p_cox_es, width = 10, height = 5.5, device = cairo_pdf)

main_sample[, (ry_vars) := NULL]


#### S13 Kaplan-Meier Survival Curves ####

# tenure = years-in-panel spell (set in S4 via seq_len(.N) by panel_id)
# Groups: Texas vs Control × pre- vs post-reform

main_sample[, km_group := factor(
  paste0(fifelse(texas_treated == 1, "Texas", "Control States"), ", ",
         fifelse(panel_year < POST_YEAR, "Pre-reform", "Post-reform")),
  levels = c("Control States, Pre-reform",  "Control States, Post-reform",
             "Texas, Pre-reform",            "Texas, Post-reform"))]

km_fit  <- survfit(Surv(tenure, closure_event) ~ km_group, data = main_sample)
km_tidy <- as.data.table(broom::tidy(km_fit))
km_tidy[, group := gsub("km_group=", "", strata)]
km_tidy[, group := factor(group, levels = c(
  "Control States, Pre-reform",  "Control States, Post-reform",
  "Texas, Pre-reform",            "Texas, Post-reform"))]

p_km <- ggplot(km_tidy, aes(x = time, y = estimate, color = group)) +
  geom_step(linewidth = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.08, color = NA, stat = "identity") +
  scale_color_manual(values = c(
    "Control States, Pre-reform"  = COL_CTRL,
    "Control States, Post-reform" = "#56B4E9",
    "Texas, Pre-reform"           = "#E69F00",
    "Texas, Post-reform"          = COL_TX)) +
  scale_fill_manual(values = c(
    "Control States, Pre-reform"  = COL_CTRL,
    "Control States, Post-reform" = "#56B4E9",
    "Texas, Pre-reform"           = "#E69F00",
    "Texas, Post-reform"          = COL_TX)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = "Years in Panel", y = "Survival Probability") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_KM_Survival.png"),
       p_km, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_KM_Survival.pdf"),
       p_km, width = 9, height = 5.5, device = cairo_pdf)


#### S14 Age-at-Closure Histogram ####

closed_dt <- main_sample[
  closure_event == 1 & !is.na(avg_tank_age),
  .(avg_tank_age,
    state_group = fifelse(texas_treated == 1, "Texas", "Control States"),
    period      = fifelse(panel_year < POST_YEAR, "Pre-reform", "Post-reform"))
]
closed_dt[, group := factor(
  paste(state_group, period),
  levels = c("Control States Pre-reform",  "Control States Post-reform",
             "Texas Pre-reform",            "Texas Post-reform"))]

median_by_group <- closed_dt[,
  .(n      = .N,
    median = median(avg_tank_age),
    mean   = mean(avg_tank_age)),
  by = group][order(group)]

fwrite(median_by_group,
       file.path(OUTPUT_TABLES, "Table_AgeAtClosure_ByGroup.csv"))

p_hist_acl <- ggplot(closed_dt, aes(x = avg_tank_age, fill = group)) +
  geom_histogram(binwidth = 1, alpha = 0.85) +
  facet_wrap(~group, ncol = 2, scales = "free_y") +
  geom_vline(data = median_by_group,
             aes(xintercept = median),
             linetype = "dashed", linewidth = 0.5) +
  scale_fill_manual(values = c(
    "Control States Pre-reform"  = COL_CTRL,
    "Control States Post-reform" = "#56B4E9",
    "Texas Pre-reform"           = "#E69F00",
    "Texas Post-reform"          = COL_TX),
    guide = "none") +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  labs(x = "Tank Age at Closure (years)", y = "Number of closures") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_Histogram_AgeAtClosure.png"),
       p_hist_acl, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Histogram_AgeAtClosure.pdf"),
       p_hist_acl, width = 10, height = 6, device = cairo_pdf)


#### S15 Wall Type Cox Models ####

setorder(sw_broader_sample, panel_id, panel_year)
sw_broader_sample[, `:=`(
  tstart   = panel_year - 1L,
  tstop    = panel_year,
  year_fac = factor(panel_year, levels = year_levels))]

cox_wall_base <- coxph(as.formula(sprintf(
  "Surv(tstart, tstop, closure_event) ~ did_term + %s + year_fac + strata(panel_id)",
  wall_col)),
  data    = sw_broader_sample,
  cluster = sw_broader_sample$state,
  ties    = "efron")

cox_wall_interact <- coxph(as.formula(sprintf(
  "Surv(tstart, tstop, closure_event) ~ did_term + %s + did_term:%s + year_fac + strata(panel_id)",
  wall_col, wall_col)),
  data    = sw_broader_sample,
  cluster = sw_broader_sample$state,
  ties    = "efron")

wall_base_s   <- summary(cox_wall_base)$coefficients
wall_int_s    <- summary(cox_wall_interact)$coefficients
int_row_idx   <- grep(paste0("did_term:", wall_col), rownames(wall_int_s))

w_base_hr     <- fmt_hr(exp(wall_base_s["did_term","coef"]),
                          wall_base_s["did_term","Pr(>|z|)"])
w_base_se     <- fmt_se(wall_base_s["did_term","se(coef)"])
w_int_hr      <- fmt_hr(exp(wall_int_s["did_term","coef"]),
                          wall_int_s["did_term","Pr(>|z|)"])
w_int_se      <- fmt_se(wall_int_s["did_term","se(coef)"])
w_sw_hr       <- fmt_hr(exp(wall_int_s[int_row_idx,"coef"]),
                          wall_int_s[int_row_idx,"Pr(>|z|)"])
w_sw_se       <- fmt_se(wall_int_s[int_row_idx,"se(coef)"])
n_wall_base   <- fmt_n(cox_wall_base$n)
n_wall_int    <- fmt_n(cox_wall_interact$n)
ev_wall_base  <- fmt_n(cox_wall_base$nevent)
ev_wall_int   <- fmt_n(cox_wall_interact$nevent)

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Closure Hazard: Heterogeneity by Tank Wall Construction}",
  "\\label{tbl:cox_wall}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & (1) & (2) \\\\",
  " & Main effect & Wall interaction \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post (HR) & %s & %s \\\\", w_base_hr, w_int_hr),
  sprintf(" & %s & %s \\\\", w_base_se, w_int_se),
  sprintf("\\quad $\\times$ Single-walled (HR) & & %s \\\\", w_sw_hr),
  sprintf(" & & %s \\\\", w_sw_se),
  "\\midrule",
  "Year fixed effects & Yes & Yes \\\\",
  "Facility strata    & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s \\\\", n_wall_base, n_wall_int),
  sprintf("Events       & %s & %s \\\\", ev_wall_base, ev_wall_int),
  "\\bottomrule",
  "\\multicolumn{3}{p{0.80\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Broader sample including single- and double-walled",
  "facilities installed 1990--1997.",
  "Counting-process Cox model with calendar-year dummies and facility strata.",
  "Hazard ratios reported; log-hazard standard errors in parentheses.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_Cox_WallType.tex")

fwrite(
  as.data.table(wall_int_s, keep.rownames = "term"),
  file.path(OUTPUT_TABLES, "Table_Cox_WallType_Coefficients.csv"))


#### S16 Additional Cox Models with Mandate Control ####

# These mirror the main cloglog spec but with explicit mandate control
# and offer both calendar-time and tank-age time axes for comparison.

cox_main_dt <- main_sample[tstop > tstart & !is.na(did_term)]

m_cox_cal <- coxph(
  Surv(tstart, tstop, closure_event) ~
    did_term + mandate_active + year_fac + strata(panel_id),
  data    = cox_main_dt,
  cluster = cox_main_dt$state,
  ties    = "efron")

cox_age_dt <- cox_main_dt[age_start >= 0 & age_stop > age_start]
m_cox_age <- coxph(
  Surv(age_start, age_stop, closure_event) ~
    did_term + mandate_active + year_fac + strata(panel_id),
  data    = cox_age_dt,
  cluster = cox_age_dt$state,
  ties    = "efron")

cal_s <- summary(m_cox_cal)$coefficients
age_s <- summary(m_cox_age)$coefficients

cal_did_hr    <- fmt_hr(exp(cal_s["did_term","coef"]),
                         cal_s["did_term","Pr(>|z|)"])
cal_did_coef  <- sprintf("%.4f", cal_s["did_term","coef"])
cal_did_se    <- fmt_se(cal_s["did_term","se(coef)"])
cal_man_hr    <- fmt_hr(exp(cal_s["mandate_active","coef"]),
                         cal_s["mandate_active","Pr(>|z|)"])
cal_man_se    <- fmt_se(cal_s["mandate_active","se(coef)"])

age_did_hr    <- fmt_hr(exp(age_s["did_term","coef"]),
                         age_s["did_term","Pr(>|z|)"])
age_did_coef  <- sprintf("%.4f", age_s["did_term","coef"])
age_did_se    <- fmt_se(age_s["did_term","se(coef)"])
age_man_hr    <- fmt_hr(exp(age_s["mandate_active","coef"]),
                         age_s["mandate_active","Pr(>|z|)"])
age_man_se    <- fmt_se(age_s["mandate_active","se(coef)"])

n_cox_cal     <- fmt_n(m_cox_cal$n)
n_cox_age     <- fmt_n(m_cox_age$n)
ev_cox_cal    <- fmt_n(m_cox_cal$nevent)
ev_cox_age    <- fmt_n(m_cox_age$nevent)

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Closure Hazard: Cox Models with Calendar and Age Time Axes}",
  "\\label{tbl:cox_did}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & (1) & (2) \\\\",
  " & Calendar time & Tank age \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post (HR) & %s & %s \\\\",
          cal_did_hr, age_did_hr),
  sprintf("Log coefficient & %s & %s \\\\", cal_did_coef, age_did_coef),
  sprintf(" & %s & %s \\\\", cal_did_se, age_did_se),
  "\\addlinespace",
  sprintf("Mandate active (HR) & %s & %s \\\\", cal_man_hr, age_man_hr),
  sprintf(" & %s & %s \\\\", cal_man_se, age_man_se),
  "\\midrule",
  "Year fixed effects & Dummies & Covariate \\\\",
  "Facility strata    & Yes     & Yes \\\\",
  "Time origin        & Calendar year & Tank age \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s \\\\", n_cox_cal, n_cox_age),
  sprintf("Events       & %s & %s \\\\", ev_cox_cal, ev_cox_age),
  "\\bottomrule",
  "\\multicolumn{3}{p{0.80\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Make-model sample.",
  "Counting-process Cox models with facility strata and year fixed effects.",
  "Hazard ratios reported; log-hazard standard errors in parentheses.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_Cox_CalAge_Comparison.tex")

fwrite(data.table(
  Model          = c("Calendar time", "Tank age"),
  HR_did_term    = round(c(exp(cal_s["did_term","coef"]),
                            exp(age_s["did_term","coef"])), 4),
  LogCoef        = round(c(cal_s["did_term","coef"],
                            age_s["did_term","coef"]), 4),
  SE_logcoef     = round(c(cal_s["did_term","se(coef)"],
                            age_s["did_term","se(coef)"]), 4),
  P_value        = round(c(cal_s["did_term","Pr(>|z|)"],
                            age_s["did_term","Pr(>|z|)"]), 4),
  N_obs          = c(m_cox_cal$n, m_cox_age$n),
  N_events       = c(m_cox_cal$nevent, m_cox_age$nevent)),
  file.path(OUTPUT_TABLES, "Table_Cox_DiD_Summary.csv"))


#### S17 Diagnostic Export ####

saveRDS(h3_cloglog_fe,    file.path(ANALYSIS_DIR, "surv_headline_cloglog.rds"))
saveRDS(h3_cloglog_hte,   file.path(ANALYSIS_DIR, "surv_cloglog_hte.rds"))
saveRDS(h3_cloglog_young, file.path(ANALYSIS_DIR, "surv_cloglog_young.rds"))
saveRDS(h3_cloglog_old,   file.path(ANALYSIS_DIR, "surv_cloglog_old.rds"))
saveRDS(m_cox_cal,        file.path(ANALYSIS_DIR, "surv_cox_calendar.rds"))
saveRDS(m_cox_age,        file.path(ANALYSIS_DIR, "surv_cox_age.rds"))

fwrite(
  data.table(
    Model    = c("OLS LPM", "Cloglog FE",
                 "Cloglog Young", "Cloglog Old"),
    Estimand = c("ATT on annual closure probability",
                 "Log-hazard ratio (conditional on survival)",
                 "Log-hazard ratio (young subsample)",
                 "Log-hazard ratio (old subsample)"),
    Estimate = round(c(
      lpm_ct["did_term","Estimate"],
      cll_ct["did_term","Estimate"],
      extract_cll_row(h3_cloglog_young)$coef,
      extract_cll_row(h3_cloglog_old)$coef), 5),
    SE       = round(c(
      lpm_ct["did_term","Std. Error"],
      cll_ct["did_term","Std. Error"],
      extract_cll_row(h3_cloglog_young)$se,
      extract_cll_row(h3_cloglog_old)$se), 5)),
  file.path(OUTPUT_TABLES, "Table_Duration_CrossSpec_Summary.csv"))
