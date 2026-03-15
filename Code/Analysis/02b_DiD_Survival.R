################################################################################
# 02b_DiD_Survival.R
# Texas UST Insurance Reform — Tank-Level Survival and Duration Analysis
#
# ANALYSIS LEVEL:  Individual tank (not facility)
#
# PRIMARY MODEL:   Cox proportional hazard
#   - Time axis   : calendar time (exact installation and closure dates in days)
#   - Stratification: strata(make_model_tank) — cell-specific baseline hazard,
#                     no parametric restriction on how hazard evolves within cell
#   - Treatment   : did_term = texas_treated × I(date >= 1998-12-22)
#                   implemented via survSplit at the statutory reform date
#   - Clustering  : state level (24 clusters: 1 TX + 23 controls)
#
# REFERENCE SPECS: OLS LPM and cloglog FE on annual tank-year panel
#   - Unit FE     : tank_panel_id
#   - Cell-year FE: make_model_tank^panel_year
#   - Estimand    : ATT on annual closure probability (OLS); log-hazard ratio
#                   conditional on survival to t (cloglog)
#   - Rationale   : OLS is the transparent prior-literature benchmark; cloglog
#                   addresses survivorship attenuation in long post-period.
#                   Neither replaces Cox as the primary spec.
#
# DATA INPUT:      Master_Harmonized_UST_Tanks.csv
#   MUST contain mm_wall, mm_fuel, mm_capacity, mm_install_cohort,
#   make_model_tank columns (written by Step 3 of the harmonization script).
#   No dependency on 02a_DiD_OLS.R or facility_leak_behavior_annual.csv.
#
# IDENTIFICATION NOTE:
#   Treatment is assigned at the state level. All tanks within a state share
#   the same treatment value, so within-facility variation does not identify
#   the treatment effect. Standard errors are clustered at the state level
#   throughout (24 clusters: 1 treated + 23 controls). With <30 clusters,
#   conventional CR1 SEs over-reject; wild cluster bootstrap (fwildclusterboot,
#   Webb 6-point weights) is used for inference in primary panel specs.
#
# SECTIONS:
#   S1   Setup and Constants
#   S2   Helper Functions
#   S3   Load Master Tanks and Build Annual Tank-Year Panel
#   S4   Define Primary Sample (mm_tank_primary)
#   S5   Build Exact-Date Cox Dataset (survSplit at reform date)
#   S6   Cell Coverage Diagnostics (numeric)
#   S6b  Cell Coverage Bar Charts (single-year and 3-year cohort grids)
#   S7   Survivorship Diagnostic
#   S8   Binned Scatter: Closure Rate by Cohort × Period × Group
#   S9   PRIMARY Cox Model + Slide Replication + Three-Sample Structure
#   S10  Cox Event Study (parallel trends test)
#   S11  Reference OLS LPM and Cloglog FE
#   S12  Duration Model Comparison Table
#   S13  HTE by Wall Type
#   S14  HTE by Install Cohort
#   S15  Age-at-Closure Analysis
#   S16  Kaplan-Meier Survival Curves
#   S17  Robustness Specifications
#   S18  Diagnostic Export
################################################################################


#### S1: Setup and Constants ####

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(here)
  library(ggplot2)
  library(broom)
  library(scales)
  library(car)
})

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

# ---- Directories ----
OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
ANALYSIS_DIR   <- here("Data", "Analysis")

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)
dir.create(ANALYSIS_DIR,   recursive = TRUE, showWarnings = FALSE)

# ---- Study Parameters ----
POST_YEAR       <- 1999L

# Exact statutory date of Texas HB 3169 (TCEQ implementation)
REFORM_DATE     <- as.IDate("1998-12-22")
REFORM_DAYS     <- as.numeric(as.Date("1998-12-22"))  # numeric for survSplit / Cox

STUDY_START     <- as.IDate("1985-01-01")
STUDY_END       <- as.IDate("2020-12-31")
STUDY_END_DAYS  <- as.numeric(as.Date("2020-12-31"))

# ---- Cohort definitions ----
# ALL_YEARS       : full sample from 1985 forward (used for Cox base + panel)
# POST_MANDATE    : tanks installed after federal upgrade mandate fully enacted;
#                   these cohorts have no compliance-driven closure confound
# PRE_MANDATE     : tanks installed before the mandate; require mandate_active
#                   control in regressions to absorb 1989-1993 differential closure
# PRIMARY_COHORTS : bin labels for coverage charts

ALL_YEARS       <- as.character(1985:1997)
POST_MANDATE    <- as.character(1990:1997)
PRE_MANDATE     <- as.character(1985:1989)

PRIMARY_COHORTS <- c("1985-1988", "1989-1991", "1992-1994", "1995-1997")
MANDATE_CUTOFF  <- as.IDate("1985-01-01")

# 3-year cohort mapping (used in S6b, S14, panel variables)
COHORT_3YR_MAP <- list(
  "1985-1988" = as.character(1985:1988),
  "1989-1991" = as.character(1989:1991),
  "1992-1994" = as.character(1992:1994),
  "1995-1997" = as.character(1995:1997)
)

# 3-year age bins (per research design document Section 6.4)
# Used as TIME-VARYING CONTROLS in reference specs — NOT for cell assignment
AGE_BIN_BREAKS  <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
AGE_BIN_LABELS  <- c("0-2",   "3-5",   "6-8",   "9-11",
                     "12-14", "15-17", "18-20", "21-23", "24+")


# ---- Study states (1 treated + 23 controls) ----
# Control states: union of 02b original list and Document 2 list, minus PA.
#
# EXCLUDED from controls (with rationale):
#   PA — PA's Act 16 amendments (signed June 26, 1995) coincide with a 29%
#        single-walled tank closure rate spike in 1995.  Until the regulatory
#        mechanism driving this spike is understood, PA is excluded.
#        A leave-one-out diagnostic confirms that PA single-handedly drives
#        the year -4 event-study outlier when included in the control group.
#
CONTROL_STATES <- c(
  "AR", "CO",  "ID",  "KS", "KY",
  "LA", "MA", "MD", "ME", "MN", "MO",  "NC",
   "OH", "OK", "SD", "TN", "VA"
)

#"IL","NM", "AL","MT", --- missin all wall type data here
# "GA", , "WV"--> no closure dates

STUDY_STATES   <- c("TX", CONTROL_STATES)

# States explicitly excluded from analysis (see rationale above)
EXCLUDED_STATES <- c("PA")


# ---- Colors ----
COL_TX      <- "#D55E00"   # Texas / treated
COL_CTRL    <- "#0072B2"   # Control states
COL_PRE     <- "#E69F00"   # Pre-reform
COL_POST    <- "#009E73"   # Post-reform
COL_YOUNG   <- "#56B4E9"   # Younger cohort
COL_OLD     <- "#CC79A7"   # Older cohort


#### S2: Helper Functions ####

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

stars_p <- function(p) {
  fcase(is.na(p), "",
        p < 0.01,  "$^{***}$",
        p < 0.05,  "$^{**}$",
        p < 0.10,  "$^{*}$",
        default =  "")
}

fmt_n   <- function(n) format(n, big.mark = ",", scientific = FALSE)
fmt_pct <- function(x) sprintf("%.1f%%", x * 100)
fmt_est <- function(x, p) sprintf("%.4f%s", x, stars_p(p))
fmt_se  <- function(x)    sprintf("(%.4f)", x)
fmt_hr  <- function(x, p) sprintf("%.3f%s", x, stars_p(p))
fmt_ci  <- function(lo, hi) sprintf("[%.3f, %.3f]", lo, hi)

extract_cox_row <- function(m, term_name = "did_term") {
  s   <- summary(m)$coefficients
  idx <- grep(term_name, rownames(s), fixed = TRUE)[1]
  if (is.na(idx))
    stop(sprintf("Term '%s' not found in Cox model coefficients.", term_name))
  # Determine SE column name (varies by coxph version / cluster option)
  se_col <- intersect(c("robust se", "se(coef)"), colnames(s))[1]
  list(
    hr   = s[idx, "exp(coef)"],
    coef = s[idx, "coef"],
    se   = s[idx, se_col],
    p    = s[idx, "Pr(>|z|)"],
    n    = m$n,
    ev   = m$nevent
  )
}

extract_panel_row <- function(m, term_name = "did_term") {
  ct  <- coeftable(m)
  idx <- grep(term_name, rownames(ct), fixed = TRUE)[1]
  if (is.na(idx))
    stop(sprintf("Term '%s' not found in panel model coefficients.", term_name))
  list(
    coef = ct[idx, "Estimate"],
    se   = ct[idx, "Std. Error"],
    p    = ct[idx, "Pr(>|t|)"],
    n    = nobs(m)
  )
}

write_tex <- function(lines, filename) {
  path <- file.path(OUTPUT_TABLES, filename)
  writeLines(lines, path)
  message(sprintf("  Written: %s", filename))
}

log_step <- function(msg, indent = 0L) {
  cat(strrep("  ", indent), msg, "\n", sep = "")
}

# ---- Coverage chart helpers (used in S6b) ----

build_coverage_dt <- function(dt, cohort_col, cohort_levels,
                               cap_levels = c("Under-1k", "1k-5k", "5k-12k",
                                              "12k-25k", "25k-Plus")) {
  counts <- dt[
    !is.na(get(cohort_col)),
    .(n = .N),
    by = .(
      mm_wall,
      mm_fuel,
      mm_capacity,
      cohort = factor(get(cohort_col), levels = cohort_levels),
      group  = fifelse(texas_treated == 1L, "Texas", "Control States")
    )
  ]
  group_tots <- counts[, .(group_total = sum(n)), by = group]
  counts     <- merge(counts, group_tots, by = "group")
  counts[, share := n / group_total]
  all_combos <- CJ(
    mm_wall     = sort(unique(dt$mm_wall)),
    mm_fuel     = sort(unique(dt$mm_fuel)),
    mm_capacity = factor(
      na.omit(sort(unique(as.character(dt$mm_capacity)))),
      levels = cap_levels
    ),
    cohort = factor(cohort_levels, levels = cohort_levels),
    group  = c("Texas", "Control States")
  )
  counts <- merge(all_combos, counts,
                  by = c("mm_wall", "mm_fuel", "mm_capacity", "cohort", "group"),
                  all.x = TRUE)
  counts[is.na(share), share := 0]
  counts[is.na(n),     n     := 0L]
  counts[, group := factor(group, levels = c("Texas", "Control States"))]
  counts
}

make_coverage_figure <- function(cov_dt, x_label, file_stem) {
  cov_dt <- copy(cov_dt)
  cov_dt[, facet_label := paste0(mm_wall, "\n", mm_fuel, "\n", mm_capacity)]
  n_cohorts    <- length(levels(cov_dt$cohort))
  n_facets     <- uniqueN(cov_dt$facet_label)
  n_cols_facet <- ceiling(sqrt(n_facets))
  n_rows_facet <- ceiling(n_facets / n_cols_facet)
  panel_w <- max(0.75 * n_cohorts, 2.5)
  fig_w   <- min(n_cols_facet * panel_w + 1.5, 60)
  fig_h   <- min(n_rows_facet * 3.0    + 1.5, 50)
  p <- ggplot(cov_dt, aes(x = cohort, y = share, fill = group)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.65, alpha = 0.88) +
    facet_wrap(~facet_label, ncol = n_cols_facet, scales = "fixed") +
    scale_fill_manual(values = c("Texas" = COL_TX, "Control States" = COL_CTRL)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1),
                       expand = expansion(mult = c(0, 0.06))) +
    scale_x_discrete(drop = FALSE) +
    labs(x = x_label, y = "Share of Group Total Sample", fill = NULL) +
    theme_minimal(base_size = 9) +
    theme(
      strip.text         = element_text(face = "bold", size = 7.5, lineheight = 1.1),
      strip.background   = element_rect(fill = "grey94", color = NA),
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y        = element_text(size = 7.5),
      axis.title         = element_text(size = 9),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "top",
      legend.text        = element_text(size = 9)
    )
  png_path <- file.path(OUTPUT_FIGURES, paste0(file_stem, ".png"))
  pdf_path <- file.path(OUTPUT_FIGURES, paste0(file_stem, ".pdf"))
  ggsave(png_path, p, width = fig_w, height = fig_h, dpi = 200, bg = "white", limitsize = FALSE)
  ggsave(pdf_path, p, width = fig_w, height = fig_h, device = cairo_pdf, limitsize = FALSE)
  log_step(sprintf("Saved: %s  (%.0f x %.0f in)", basename(png_path), fig_w, fig_h), 1)
  invisible(p)
}

# ---- OLS event-study helpers (used in S11) ----

run_ols_es <- function(dt, prefix = "es") {
  dt <- copy(dt)
  ry_all <- sort(unique(dt$rel_year))
  ry_indiv <- sort(ry_all[
    ry_all >  OLS_POOL_PRE  &
    ry_all <  OLS_POOL_POST &
    ry_all != OLS_REF_YEAR
  ])
  dt[, `:=`(
    ry_pool_pre  = as.integer(rel_year <= OLS_POOL_PRE)  * texas_treated,
    ry_pool_post = as.integer(rel_year >= OLS_POOL_POST) * texas_treated
  )]
  ry_vars <- c("ry_pool_pre", "ry_pool_post")
  for (yr in ry_indiv) {
    vname <- if (yr < 0L) paste0("ry_m", abs(yr)) else paste0("ry_", yr)
    dt[, (vname) := as.integer(rel_year == yr) * texas_treated]
    ry_vars <- c(ry_vars, vname)
  }
  ry_vars <- sort(ry_vars)
  fml <- as.formula(paste(
    "closure_event ~",
    paste(ry_vars, collapse = " + "),
    "| tank_panel_id + make_model_tank^panel_year"
  ))
  m <- feols(fml, data = dt, cluster = ~state)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "coef", "se", "t_stat", "p"))
  ct[, rel_year := fcase(
    term == "ry_pool_pre",        OLS_POOL_PRE,
    term == "ry_pool_post",       OLS_POOL_POST,
    grepl("^ry_m[0-9]+$", term), -as.integer(gsub("ry_m", "", term)),
    grepl("^ry_[0-9]+$",  term),  as.integer(gsub("ry_",  "", term)),
    default = NA_integer_
  )]
  ct <- ct[!is.na(rel_year)]
  ct[, `:=`(
    coef_pp  = coef * 100,
    ci_lo_pp = (coef - 1.96 * se) * 100,
    ci_hi_pp = (coef + 1.96 * se) * 100,
    period   = fcase(rel_year <  0L, "Pre", rel_year >= 0L, "Post"),
    pooled   = as.integer(term %in% c("ry_pool_pre", "ry_pool_post"))
  )]
  ct <- rbind(ct,
    data.table(term = "ref_year", coef = 0, se = 0, t_stat = 0, p = NA_real_,
               rel_year = OLS_REF_YEAR, coef_pp = 0, ci_lo_pp = 0, ci_hi_pp = 0,
               period = "Ref", pooled = 0L),
    fill = TRUE)
  setorder(ct, rel_year)
  list(model = m, coefs = ct, ry_vars = ry_vars)
}

ols_pre_tests <- function(es_obj) {
  m       <- es_obj$model
  ry_vars <- es_obj$ry_vars
  pre_vars_all <- ry_vars[ry_vars == "ry_pool_pre" | grepl("^ry_m[0-9]+$", ry_vars)]
  test_all <- linearHypothesis(m, paste0(pre_vars_all, " = 0"), test = "Chisq")
  chi_all  <- test_all[2, "Chisq"]
  dof_all  <- length(pre_vars_all)
  p_all    <- test_all[2, "Pr(>Chisq)"]
  pre_vars_dense <- ry_vars[ry_vars %in% paste0("ry_m", 2:6)]
  chi_dense <- NA_real_; dof_dense <- length(pre_vars_dense); p_dense <- NA_real_
  if (dof_dense >= 2L) {
    test_dense <- linearHypothesis(m, paste0(pre_vars_dense, " = 0"), test = "Chisq")
    chi_dense <- test_dense[2, "Chisq"]
    p_dense   <- test_dense[2, "Pr(>Chisq)"]
  }
  list(chi_all = chi_all, dof_all = dof_all, p_all = p_all,
       chi_dense = chi_dense, dof_dense = dof_dense, p_dense = p_dense)
}

plot_ols_es <- function(es_obj, pre_obj, subtitle = "") {
  coefs <- es_obj$coefs
  all_x  <- sort(unique(coefs$rel_year))
  x_labs <- as.character(all_x)
  x_labs[all_x == OLS_POOL_PRE]  <- paste0("\u2264", OLS_POOL_PRE)
  x_labs[all_x == OLS_POOL_POST] <- paste0("\u2265", OLS_POOL_POST)
  pre_label <- sprintf(
    "Pre-period parallel-trends test\nFull pre (%d dof): \u03c7\u00b2=%.2f, p=%.4f\nDense \u22126:\u22122 (%d dof): \u03c7\u00b2=%.2f, p=%.4f",
    pre_obj$dof_all, pre_obj$chi_all, pre_obj$p_all,
    pre_obj$dof_dense,
    ifelse(is.na(pre_obj$chi_dense), 0, pre_obj$chi_dense),
    ifelse(is.na(pre_obj$p_dense),   1, pre_obj$p_dense))
  ggplot(coefs, aes(x = rel_year, y = coef_pp)) +
    annotate("rect", xmin = -Inf, xmax = -0.5, ymin = -Inf, ymax = Inf,
             fill = "grey90", alpha = 0.50) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey35", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey20", linewidth = 0.65) +
    geom_line(aes(group = 1), color = "grey40", linewidth = 0.45, alpha = 0.65) +
    geom_errorbar(aes(ymin = ci_lo_pp, ymax = ci_hi_pp, color = period),
                  width = 0.30, linewidth = 0.45) +
    geom_point(data = coefs[pooled == 0], aes(color = period), size = 2.6, shape = 16) +
    geom_point(data = coefs[pooled == 1], aes(color = period), size = 3.2, shape = 18) +
    annotate("text", x = OLS_POOL_PRE + 0.3, y = OLS_Y_CAP * 0.97,
             label = pre_label, hjust = 0, vjust = 1, size = 2.8,
             color = "grey25", lineheight = 1.15) +
    scale_color_manual(values = c(Pre = COL_CTRL, Post = COL_TX, Ref = "black"), guide = "none") +
    scale_x_continuous(breaks = all_x, labels = x_labs) +
    scale_y_continuous(limits = c(OLS_Y_FLOOR, OLS_Y_CAP), oob = scales::squish,
                       labels = function(x) paste0(ifelse(x > 0, "+", ""), x, " pp")) +
    labs(x = "Years Relative to Reform  (0 = 1999)",
         y = "Change in Annual Closure Probability vs. Year \u22121  (pp)",
         subtitle = subtitle) +
    theme_pub() +
    theme(panel.grid.major.x = element_blank(),
          plot.margin   = margin(t = 8, r = 12, b = 8, l = 20, unit = "pt"),
          axis.text.x   = element_text(angle = 45, hjust = 1, vjust = 1),
          plot.subtitle = element_text(size = 9, color = "grey30"),
          plot.caption  = element_text(size = 8, color = "grey40", hjust = 0,
                                       margin = margin(t = 6))) +
    coord_cartesian(clip = "off")
}


#### S3: Load Master Tanks and Build Annual Tank-Year Panel ####

cat("\n========================================\n")
cat("S3: LOAD MASTER TANKS AND BUILD TANK-YEAR PANEL\n")
cat("========================================\n\n")

master_tanks <- fread(
  here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv"),
  na.strings = c("", "NA", "N/A")
)

log_step(sprintf("Loaded: %s rows, %s columns",
                 fmt_n(nrow(master_tanks)), ncol(master_tanks)))

# ---- Validate mm_* columns ----
required_mm_cols <- c("mm_wall", "mm_fuel", "mm_capacity",
                      "mm_install_cohort", "make_model_tank")
missing_cols <- setdiff(required_mm_cols, names(master_tanks))
if (length(missing_cols) > 0) {
  stop(paste0("Required columns not found: ", paste(missing_cols, collapse = ", "), "\n",
    "  Re-run 10_Master_Cleaning_and_Harmonization.r with Step 3 enabled."))
}

# ---- Coerce date columns ----
for (col in c("tank_installed_date", "tank_closed_date")) {
  if (col %in% names(master_tanks))
    master_tanks[[col]] <- as.IDate(master_tanks[[col]])
}

# ---- Treatment assignment ----
master_tanks[, texas_treated := fcase(
  state == "TX",                1L,
  state %in% CONTROL_STATES,   0L,
  default = NA_integer_
)]

log_step(sprintf("Treatment: TX=%s tanks, Control=%s tanks, Non-study (NA)=%s",
                 fmt_n(master_tanks[texas_treated == 1, .N]),
                 fmt_n(master_tanks[texas_treated == 0, .N]),
                 fmt_n(master_tanks[is.na(texas_treated), .N])))

if (any(master_tanks$state %in% EXCLUDED_STATES)) {
  for (es in EXCLUDED_STATES) {
    n_ex <- master_tanks[state == es, .N]
    if (n_ex > 0)
      log_step(sprintf("  Excluded state %s: %s tanks (see S1 rationale)",
                        es, fmt_n(n_ex)), 1)
  }
}

# ---- Unique identifiers ----
if ("tank_id" %in% names(master_tanks)) {
  master_tanks[, tank_panel_id := paste(facility_id, state, tank_id, sep = "_")]
} else {
  master_tanks[, tank_panel_id := paste(facility_id, state, .I, sep = "_")]
  warning("No tank_id column — using row index as tank_panel_id.")
}
master_tanks[, panel_id := paste(facility_id, state, sep = "_")]

# ---- Restrict to valid tanks ----
study_tanks <- master_tanks[!is.na(texas_treated) & !is.na(tank_installed_date)]
log_step(sprintf("Tanks with valid treatment + install date: %s", fmt_n(nrow(study_tanks))))

# ---- Expand to annual tank-year panel (1985-2020) ----
log_step("Building annual tank-year panel (1985-2020)...")

study_tanks[, `:=`(
  install_yr   = year(tank_installed_date),
  close_yr_raw = fifelse(!is.na(tank_closed_date), year(tank_closed_date), 2020L)
)]
study_tanks[, `:=`(
  expand_start = pmax(install_yr, 1985L),
  expand_end   = pmin(close_yr_raw, 2020L)
)]

panel_cols <- c("tank_panel_id", "panel_id", "facility_id", "state",
                "texas_treated", "mm_wall", "mm_fuel", "mm_capacity",
                "mm_install_cohort", "make_model_tank",
                "tank_installed_date", "tank_closed_date",
                "expand_start", "expand_end")

tank_year_panel <- study_tanks[
  expand_start <= expand_end, .SD, .SDcols = panel_cols
][, {
  yrs <- seq(expand_start, expand_end)
  .(panel_year    = yrs,
    closure_event = as.integer(!is.na(tank_closed_date) & year(tank_closed_date) == yrs))
}, by = panel_cols]

log_step(sprintf("Tank-year panel: %s rows, %s unique tanks, %s unique facilities",
                 fmt_n(nrow(tank_year_panel)),
                 fmt_n(uniqueN(tank_year_panel$tank_panel_id)),
                 fmt_n(uniqueN(tank_year_panel$panel_id))))

# ---- Add time variables ----
tank_year_panel[, `:=`(
  post_1999 = as.integer(panel_year >= POST_YEAR),
  did_term  = texas_treated * as.integer(panel_year >= POST_YEAR)
)]
tank_year_panel[, rel_year := panel_year - POST_YEAR]
tank_year_panel[, rel_year_bin := fcase(
  rel_year <= -5L, -5L, rel_year >= 5L, 5L, default = as.integer(rel_year)
)]

setorder(tank_year_panel, tank_panel_id, panel_year)
tank_year_panel[, `:=`(tstart = panel_year - 1L, tstop = panel_year)]

tank_year_panel[, `:=`(
  age_stop  = as.numeric(as.IDate(paste0(panel_year, "-12-31")) - tank_installed_date) / 365.25,
  age_start = pmax(as.numeric(as.IDate(paste0(panel_year - 1L, "-12-31")) - tank_installed_date) / 365.25, 0)
)]

tank_year_panel[, age_bin := cut(age_stop, breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS,
                                  right = FALSE, include.lowest = TRUE)]

# ---- Mandate and cohort sample flags ----
tank_year_panel[, `:=`(
  is_post_mandate = as.integer(mm_install_cohort %in% POST_MANDATE),
  is_pre_mandate  = as.integer(mm_install_cohort %in% PRE_MANDATE),
  mandate_active  = as.integer(
    panel_year %in% 1989:1993 &
    !is.na(mm_install_cohort) &
    mm_install_cohort %in% PRE_MANDATE
  )
)]

# ---- 3-year cohort bins (now includes 1985-1988) ----
tank_year_panel[, cohort_3yr := fcase(
  mm_install_cohort %in% as.character(1985:1988), "1985-1988",
  mm_install_cohort %in% as.character(1989:1991), "1989-1991",
  mm_install_cohort %in% as.character(1992:1994), "1992-1994",
  mm_install_cohort %in% as.character(1995:1997), "1995-1997",
  default = NA_character_
)]

# ---- Log cohort composition ----
cohort_counts <- tank_year_panel[
  !is.na(mm_install_cohort),
  .(n_tanks = uniqueN(tank_panel_id)),
  by = .(sample = fcase(
    is_post_mandate == 1L, "Post-mandate (1990-1997)",
    is_pre_mandate  == 1L, "Pre-mandate (1985-1989)",
    default = "Other"
  ))
]
log_step("Cohort composition (unique tanks):")
for (i in seq_len(nrow(cohort_counts)))
  log_step(sprintf("  %-30s %s", cohort_counts$sample[i], fmt_n(cohort_counts$n_tanks[i])), 1)

cohort_3yr_counts <- tank_year_panel[
  !is.na(cohort_3yr),
  .(n_tanks = uniqueN(tank_panel_id),
    n_tx    = uniqueN(tank_panel_id[texas_treated == 1L]),
    n_ctl   = uniqueN(tank_panel_id[texas_treated == 0L])),
  by = cohort_3yr][order(cohort_3yr)]
log_step("\n3-year cohort bins (unique tanks):")
log_step(sprintf("  %-15s %8s %8s %8s", "Cohort", "Total", "TX", "CTL"), 1)
for (i in seq_len(nrow(cohort_3yr_counts))) {
  r <- cohort_3yr_counts[i]
  log_step(sprintf("  %-15s %8s %8s %8s", r$cohort_3yr,
                   fmt_n(r$n_tanks), fmt_n(r$n_tx), fmt_n(r$n_ctl)), 1)
}

year_levels <- sort(unique(tank_year_panel$panel_year))
tank_year_panel[, year_fac := factor(panel_year, levels = year_levels)]

log_step("\nTank-year panel complete.\n")


#### S4: Define Primary Sample (mm_tank_primary) ####

cat("========================================\n")
cat("S4: DEFINE PRIMARY SAMPLE\n")
cat("========================================\n\n")

# ---- CHANGED: ALL_YEARS instead of old PRIMARY_YEARS ----
mm_tank_primary <- tank_year_panel[
  !is.na(make_model_tank)                    &
  mm_install_cohort %in% ALL_YEARS           &
  tank_installed_date >= MANDATE_CUTOFF      &
  tstop > tstart                             &
  !is.na(did_term)                           &
  state %in% STUDY_STATES
]

# ---- Verify no non-study states ----
leaked_states <- setdiff(unique(mm_tank_primary$state), STUDY_STATES)
if (length(leaked_states) > 0) {
  stop(sprintf("FATAL: Non-study states found: %s", paste(leaked_states, collapse = ", ")))
} else {
  log_step(sprintf("  State filter verified: %d states (%s)",
                   uniqueN(mm_tank_primary$state),
                   paste(sort(unique(mm_tank_primary$state)), collapse = ", ")), 1)
}

# ---- Sample size report ----
n_ty       <- nrow(mm_tank_primary)
n_tanks    <- uniqueN(mm_tank_primary$tank_panel_id)
n_tx_tanks <- mm_tank_primary[texas_treated == 1, uniqueN(tank_panel_id)]
n_ct_tanks <- mm_tank_primary[texas_treated == 0, uniqueN(tank_panel_id)]
n_tx_ty    <- mm_tank_primary[texas_treated == 1, .N]
n_ct_ty    <- mm_tank_primary[texas_treated == 0, .N]
n_cells    <- uniqueN(mm_tank_primary$make_model_tank)
n_cells_id <- mm_tank_primary[,
  .(identified = as.integer(any(texas_treated == 1) & any(texas_treated == 0))),
  by = .(make_model_tank, panel_year)][, sum(identified)]

log_step("Primary sample (mm_tank_primary):")
log_step(sprintf("  Tank-years  : %s  (TX: %s | CTL: %s)", fmt_n(n_ty), fmt_n(n_tx_ty), fmt_n(n_ct_ty)), 1)
log_step(sprintf("  Unique tanks: %s  (TX: %s | CTL: %s)", fmt_n(n_tanks), fmt_n(n_tx_tanks), fmt_n(n_ct_tanks)), 1)
log_step(sprintf("  Make-model cells: %s unique | %s identified cell-years", fmt_n(n_cells), fmt_n(n_cells_id)), 1)
log_step(sprintf("  Calendar years: %d - %d", min(mm_tank_primary$panel_year), max(mm_tank_primary$panel_year)), 1)

# ---- CHANGED: ALL_YEARS in exclusion report ----
prim_base <- tank_year_panel[mm_install_cohort %in% ALL_YEARS & tstop > tstart & !is.na(did_term)]
excl_uw <- prim_base[mm_wall == "Unknown-Wall", .N]
excl_uf <- prim_base[mm_fuel == "Unknown-Fuel", .N]
excl_na <- prim_base[is.na(make_model_tank), .N]
tot_base <- nrow(prim_base)
log_step(sprintf("\nExclusion from primary sample (cohort-eligible tank-years = %s):", fmt_n(tot_base)))
log_step(sprintf("  Unknown-Wall : %s (%s)", fmt_n(excl_uw), fmt_pct(excl_uw / tot_base)), 1)
log_step(sprintf("  Unknown-Fuel : %s (%s)", fmt_n(excl_uf), fmt_pct(excl_uf / tot_base)), 1)
log_step(sprintf("  NA cell      : %s (%s)", fmt_n(excl_na), fmt_pct(excl_na / tot_base)), 1)

fwrite(
  prim_base[, .(N_total = .N, N_unknown_wall = sum(mm_wall == "Unknown-Wall"),
    N_unknown_fuel = sum(mm_fuel == "Unknown-Fuel"), N_na_cell = sum(is.na(make_model_tank)),
    pre_closure_rate_uw = mean(closure_event[mm_wall == "Unknown-Wall"], na.rm = TRUE),
    pre_closure_rate_cls = mean(closure_event[mm_wall != "Unknown-Wall"], na.rm = TRUE)
  ), by = .(state_group = fifelse(texas_treated == 1, "Texas", "Control"))],
  file.path(OUTPUT_TABLES, "Diag_Exclusion_UnknownCells.csv"))

tx_share <- mm_tank_primary[, .(n = .N, share = .N / n_ty),
  by = .(group = fifelse(texas_treated == 1, "TX", "CTL"))]
log_step(sprintf("\nTX share of tank-years: %s", fmt_pct(tx_share[group == "TX", share])))
if (tx_share[group == "TX", share] > 0.40)
  warning(sprintf("Texas contributes %.1f%% of tank-years (>40%%).", 100 * tx_share[group == "TX", share]))
cat("\n")


#### S5: Build Exact-Date Cox Dataset ####

cat("========================================\n")
cat("S5: BUILD EXACT-DATE COX DATASET\n")
cat("========================================\n\n")

log_step("Filtering master tanks to primary sample attributes...")

# ---- CHANGED: ALL_YEARS instead of old PRIMARY_YEARS ----
exact_base <- master_tanks[
  !is.na(texas_treated)                &
  !is.na(make_model_tank)              &
  mm_install_cohort %in% ALL_YEARS     &
  !is.na(tank_installed_date)          &
  tank_installed_date >= MANDATE_CUTOFF &
  tank_installed_date < STUDY_END      &
  state %in% STUDY_STATES,
  .(tank_panel_id, panel_id, facility_id, state, texas_treated,
    mm_wall, mm_fuel, mm_capacity, mm_install_cohort, make_model_tank,
    tank_installed_date,
    t_enter = as.numeric(tank_installed_date),
    t_exit  = as.numeric(pmin(
      fifelse(!is.na(tank_closed_date), tank_closed_date, STUDY_END), STUDY_END)),
    failure = as.integer(
      !is.na(tank_closed_date) & tank_closed_date <= STUDY_END & tank_closed_date >= tank_installed_date)
  )
][t_exit > t_enter]

# ---- Verify samples align ----
panel_tanks <- sort(unique(mm_tank_primary$tank_panel_id))
cox_tanks   <- sort(unique(exact_base$tank_panel_id))
only_panel  <- setdiff(panel_tanks, cox_tanks)
only_cox    <- setdiff(cox_tanks, panel_tanks)
if (length(only_panel) > 0 || length(only_cox) > 0) {
  warning(sprintf("Sample mismatch: %d tanks in panel only, %d in Cox only.",
                  length(only_panel), length(only_cox)))
} else {
  log_step("  Tank sets match between panel and Cox datasets", 1)
}

leaked_cox <- setdiff(unique(exact_base$state), STUDY_STATES)
if (length(leaked_cox) > 0)
  stop(sprintf("Non-study states in exact_base: %s", paste(leaked_cox, collapse = ", ")))

# ---- survSplit at REFORM_DAYS ----
exact_split_df <- survSplit(
  formula = Surv(t_enter, t_exit, failure) ~ .,
  data    = as.data.frame(exact_base),
  cut     = REFORM_DAYS,
  episode = "reform_ep"
)
setDT(exact_split_df)
exact_split_df <- exact_split_df[t_exit > t_enter]
exact_split_df[, did_term := texas_treated * as.integer(reform_ep == 2L)]

log_step(sprintf("After survSplit: %s rows, %s tanks",
                 fmt_n(nrow(exact_split_df)), fmt_n(uniqueN(exact_split_df$tank_panel_id))))

# ---- Exact age intervals ----
exact_split_df[, `:=`(
  age_enter = pmax((t_enter - as.numeric(tank_installed_date)) / 365.25, 0),
  age_exit  = (t_exit - as.numeric(tank_installed_date)) / 365.25
)]
exact_split_df <- exact_split_df[age_exit > age_enter]

exact_split_df[, year_fac := factor(
  as.integer(format(as.Date(t_exit, origin = "1970-01-01"), "%Y")),
  levels = year_levels)]

# ---- Propagate mandate/sample flags to Cox dataset ----
exact_split_df[, `:=`(
  is_post_mandate = as.integer(mm_install_cohort %in% POST_MANDATE),
  is_pre_mandate  = as.integer(mm_install_cohort %in% PRE_MANDATE),
  mandate_active  = as.integer(
    mm_install_cohort %in% PRE_MANDATE &
    as.integer(format(as.Date((t_enter + t_exit) / 2, origin = "1970-01-01"), "%Y")) %in% 1989:1993
  )
)]

log_step(sprintf("Exact-date split final: %s rows, %s events",
                 fmt_n(nrow(exact_split_df)), fmt_n(sum(exact_split_df$failure))), 1)
cat("\n")


#### S6: Cell Coverage Diagnostics ####

cat("========================================\n")
cat("S6: CELL COVERAGE DIAGNOSTICS\n")
cat("========================================\n\n")

log_step("Computing cell coverage by (make_model_tank x panel_year)...")

cell_diag <- mm_tank_primary[, .(
  n_total = .N, n_tx = sum(texas_treated == 1L), n_ctl = sum(texas_treated == 0L),
  has_both = as.integer(sum(texas_treated == 1L) > 0L & sum(texas_treated == 0L) > 0L)
), by = .(make_model_tank, panel_year)]

coverage_summary <- cell_diag[, .(
  total_cell_years = .N, identified_cell_years = sum(has_both),
  pct_identified = round(mean(has_both) * 100, 1),
  tank_years_total = sum(n_total), tank_years_identified = sum(n_total * has_both),
  pct_tank_years_identified = round(sum(n_total * has_both) / sum(n_total) * 100, 1)
)]

log_step(sprintf("  Cell-years total        : %s", fmt_n(coverage_summary$total_cell_years)), 1)
log_step(sprintf("  Identified cell-years   : %s (%s%%)",
                 fmt_n(coverage_summary$identified_cell_years), coverage_summary$pct_identified), 1)
log_step(sprintf("  Tank-years identified   : %s / %s (%s%%)",
                 fmt_n(coverage_summary$tank_years_identified),
                 fmt_n(coverage_summary$tank_years_total),
                 coverage_summary$pct_tank_years_identified), 1)

if (coverage_summary$pct_tank_years_identified < 70)
  warning(sprintf("Only %s%% of tank-years are in identified cells (target: 70%%).",
                  coverage_summary$pct_tank_years_identified))

cell_sizes_98 <- mm_tank_primary[panel_year == 1998L, .(
  n_tx = sum(texas_treated == 1L), n_ctl = sum(texas_treated == 0L), n_total = .N,
  has_both = as.integer(any(texas_treated == 1L) & any(texas_treated == 0L))
), by = make_model_tank][order(-n_total)]

log_step(sprintf("\n  Cells at 1998: %s total | %s identified | %s well-populated",
                 nrow(cell_sizes_98), cell_sizes_98[has_both == 1L, .N],
                 cell_sizes_98[n_tx >= 5L & n_ctl >= 5L, .N]), 1)
fwrite(cell_sizes_98, file.path(OUTPUT_TABLES, "Diag_TankCell_Sizes.csv"))

pre_balance <- mm_tank_primary[panel_year < POST_YEAR, .(
  closure_rate = mean(closure_event, na.rm = TRUE), n = .N
), by = .(make_model_tank, group = fifelse(texas_treated == 1L, "Texas", "Control"))]
bal_wide <- dcast(pre_balance, make_model_tank ~ group, value.var = c("closure_rate", "n"))
bal_wide[, pre_gap := closure_rate_Texas - closure_rate_Control]
n_flagged <- bal_wide[abs(pre_gap) > 0.02 | n_Texas < 10L | n_Control < 10L, .N]
log_step(sprintf("  Cells flagged (|pre-gap|>2pp or n<10): %s", n_flagged), 1)
fwrite(bal_wide[order(-abs(pre_gap))], file.path(OUTPUT_TABLES, "Diag_PreBalance_ByCell.csv"))
cat("\n")


#### S6b: Cell Coverage Bar Charts ####

cat("========================================\n")
cat("S6b: CELL COVERAGE BAR CHARTS\n")
cat("========================================\n\n")

if (!"cohort_3yr" %in% names(mm_tank_primary)) {
  mm_tank_primary[, cohort_3yr := fcase(
    mm_install_cohort %in% as.character(1985:1988), "1985-1988",
    mm_install_cohort %in% as.character(1989:1991), "1989-1991",
    mm_install_cohort %in% as.character(1992:1994), "1992-1994",
    mm_install_cohort %in% as.character(1995:1997), "1995-1997",
    default = NA_character_)]
}

# ---- CHANGED: includes 1985-1988 bin ----
BIN_3YR_LEVELS <- c("1985-1988", "1989-1991", "1992-1994", "1995-1997")
CAP_LEVELS     <- c("Under-1k", "1k-5k", "5k-12k", "12k-25k", "25k-Plus")

# ---- Single-year cohort chart ----
log_step("Building single-year cohort coverage chart...")
# ---- CHANGED: ALL_YEARS instead of PRIMARY_YEARS ----
cov_1yr <- build_coverage_dt(dt = mm_tank_primary, cohort_col = "mm_install_cohort",
                              cohort_levels = ALL_YEARS, cap_levels = CAP_LEVELS)
make_coverage_figure(cov_dt = cov_1yr, x_label = "Install Year", file_stem = "Diag_CellCoverage_SingleYear")
fwrite(cov_1yr[order(mm_wall, mm_fuel, mm_capacity, cohort, group)],
       file.path(OUTPUT_TABLES, "Diag_CellCoverage_SingleYear.csv"))

# ---- 3-year cohort chart ----
log_step("Building 3-year cohort coverage chart...")
cov_3yr <- build_coverage_dt(dt = mm_tank_primary[cohort_3yr %in% BIN_3YR_LEVELS],
                              cohort_col = "cohort_3yr", cohort_levels = BIN_3YR_LEVELS,
                              cap_levels = CAP_LEVELS)
make_coverage_figure(cov_dt = cov_3yr, x_label = "Install Year Cohort (3-year bins)",
                     file_stem = "Diag_CellCoverage_3yr")
fwrite(cov_3yr[order(mm_wall, mm_fuel, mm_capacity, cohort, group)],
       file.path(OUTPUT_TABLES, "Diag_CellCoverage_3yr.csv"))

summarise_cov <- function(dt, label) {
  s <- dt[, .(n_combos_nonzero = sum(n > 0), n_combos_zero = sum(n == 0),
    mean_share_pct = round(mean(share[n > 0]) * 100, 2),
    max_share_pct  = round(max(share) * 100, 2)), by = group]
  cat(sprintf("\n%s:\n", label)); print(s)
}
summarise_cov(cov_1yr, "Single-year cohorts")
summarise_cov(cov_3yr, "3-year cohorts")
cat("\n")


#### S7: Survivorship Diagnostic ####

cat("========================================\n")
cat("S7: SURVIVORSHIP DIAGNOSTIC\n")
cat("========================================\n\n")

surv_diag <- mm_tank_primary[, .(
  n_tanks = uniqueN(tank_panel_id),
  exit_pre_reform  = uniqueN(tank_panel_id[closure_event == 1L & panel_year < POST_YEAR]),
  exit_post_reform = uniqueN(tank_panel_id[closure_event == 1L & panel_year >= POST_YEAR])
), by = .(mm_install_cohort,
          state_group = fifelse(texas_treated == 1L, "Texas", "Control States")
)][order(state_group, mm_install_cohort)]
surv_diag[, `:=`(pct_exit_pre = exit_pre_reform / n_tanks, pct_exit_post = exit_post_reform / n_tanks)]

log_step("Survivorship by cohort x group:")
print(surv_diag)
fwrite(surv_diag, file.path(OUTPUT_TABLES, "Diag_Survivorship_ByCohort.csv"))

tx_share_time <- mm_tank_primary[, .(
  n_tx = sum(texas_treated == 1L), n_ctl = sum(texas_treated == 0L),
  n = .N, tx_share = sum(texas_treated == 1L) / .N
), by = .(make_model_tank, panel_year)]
tx_share_wide <- dcast(tx_share_time[, .(make_model_tank, panel_year, tx_share)],
                        make_model_tank ~ panel_year, value.var = "tx_share")
fwrite(tx_share_wide, file.path(OUTPUT_TABLES, "Diag_TXShare_CellTime.csv"))
cat("\n")


#### S8: Binned Scatter — Closure Rate by Cohort × Period × Group ####

cat("========================================\n")
cat("S8: BINNED SCATTER\n")
cat("========================================\n\n")

# ---- CHANGED: ALL_YEARS instead of PRIMARY_YEARS ----
scatter_dt <- mm_tank_primary[, .(
  closure_rate = mean(closure_event, na.rm = TRUE), n = .N
), by = .(mm_install_cohort,
          state_group = fifelse(texas_treated == 1L, "Texas", "Control States"),
          period = fifelse(panel_year < POST_YEAR, "Pre-reform", "Post-reform"))]
scatter_dt[, group_period := factor(paste(state_group, period),
  levels = c("Control States Pre-reform", "Control States Post-reform",
             "Texas Pre-reform", "Texas Post-reform"))]
scatter_dt[, cohort_order := match(mm_install_cohort, ALL_YEARS)]
setorder(scatter_dt, cohort_order)
scatter_dt[, mm_install_cohort := factor(mm_install_cohort, levels = ALL_YEARS)]

p_scatter <- ggplot(scatter_dt[!is.na(mm_install_cohort)],
  aes(x = mm_install_cohort, y = closure_rate, color = group_period, group = group_period)) +
  geom_line(linewidth = 0.9) + geom_point(size = 3) +
  scale_color_manual(values = c(
    "Control States Pre-reform" = COL_CTRL, "Control States Post-reform" = "#56B4E9",
    "Texas Pre-reform" = COL_PRE, "Texas Post-reform" = COL_TX)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Install Year Cohort", y = "Mean Annual Closure Rate") +
  theme_pub() + theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_BinnedScatter_CohortClosure.png"),
       p_scatter, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_BinnedScatter_CohortClosure.pdf"),
       p_scatter, width = 9, height = 5.5, device = cairo_pdf)
cat("\n")


#### S9: PRIMARY Cox Model + Slide Replication + Three-Sample Structure ####
# Step 1: Identify facilities with at least one SW tank alive at reform
## tank cells too narro  lets just focus on  mm_wall x mm_install_cohort for now




################################################################################
# S1: SAMPLE CONSTRUCTION
################################################################################
 
cat("\n========================================\n")
cat("S1: SAMPLE CONSTRUCTION\n")
cat("========================================\n\n")
 
# ---- 1a. Identify SW-exposed facilities ----
sw_exposed_facs <- mm_tank_primary[
  mm_wall == "Single-Walled" &
  mm_install_cohort >= 1992L & mm_install_cohort <= 1998L,
  unique(panel_id)
]
 
# ---- 1b. All tanks at those facilities, 1990-1998 cohorts ----
analysis_tanks <- mm_tank_primary[
  panel_id %in% sw_exposed_facs &
  mm_install_cohort >= 1992L & mm_install_cohort <= 1998L
]
 
# ---- 1c. Drop first-year churn ----
# Tanks where panel_year == install_cohort are installation-year observations.
# These show high closure rates on the diagonal (heatmap diagnostic) driven by
# failed inspections, paperwork corrections, not economic closure decisions.
analysis_tanks <- analysis_tanks[panel_year > as.integer(mm_install_cohort)]
 
# ---- 1d. Treatment variables ----
analysis_tanks[, `:=`(
  did_term  = texas_treated * as.integer(panel_year >= 1999L),
  rel_year  = panel_year - 1999L,
  did_short = texas_treated * as.integer(panel_year >= 1999L & panel_year <= 2004L),
  did_long  = texas_treated * as.integer(panel_year >= 2005L)
)]
 
analysis_tanks[, rel_year_es := fcase(
  rel_year <= -5L, -5L,
  rel_year >= 10L, 10L,
  default = as.integer(rel_year)
)]
 
# ---- 1e. Age at treatment (frozen at 1999) ----
analysis_tanks[, age_at_treatment := 1999L - as.integer(mm_install_cohort)]
 
# Age bins
analysis_tanks[, age_treat_bin := fcase(
  age_at_treatment <= 4L,  "2-4yr",
  age_at_treatment <= 6L,  "5-6yr",
  age_at_treatment <= 9L,  "7-9yr",
  default = NA_character_
)]
 
# Quartile-based splits
age_q25 <- quantile(analysis_tanks[, unique(age_at_treatment)], 0.25)
age_q75 <- quantile(analysis_tanks[, unique(age_at_treatment)], 0.75)
analysis_tanks[, age_quartile := fcase(
  age_at_treatment <= age_q25, "Bottom 25%",
  age_at_treatment >= age_q75, "Top 25%",
  default = "Middle 50%"
)]
 
# Cell without cohort (for FE analysis that doesn't absorb age channel)
analysis_tanks[, make_model_noage := paste(mm_wall, mm_fuel, mm_capacity, sep = "_")]
 
# ---- 1f. Sample report ----
cat(sprintf("Analysis sample (post first-year-churn removal):\n"))
cat(sprintf("  Facilities: TX=%s  CTL=%s\n",
    fmt_n(analysis_tanks[texas_treated == 1, uniqueN(panel_id)]),
    fmt_n(analysis_tanks[texas_treated == 0, uniqueN(panel_id)])))
cat(sprintf("  Tanks: TX=%s  CTL=%s\n",
    fmt_n(analysis_tanks[texas_treated == 1, uniqueN(tank_panel_id)]),
    fmt_n(analysis_tanks[texas_treated == 0, uniqueN(tank_panel_id)])))
cat(sprintf("  Tank-years: %s\n", fmt_n(nrow(analysis_tanks))))
cat(sprintf("  Age at treatment: min=%d, Q25=%d, median=%d, Q75=%d, max=%d\n",
    min(analysis_tanks$age_at_treatment),
    as.integer(age_q25),
    median(analysis_tanks$age_at_treatment),
    as.integer(age_q75),
    max(analysis_tanks$age_at_treatment)))
 
cat("\n  Age quartile distribution:\n")
print(analysis_tanks[, .(n_tanks = uniqueN(tank_panel_id), n_ty = .N),
                     by = .(age_quartile, texas_treated)][order(age_quartile, texas_treated)])
 
cat("\n  Age bin distribution:\n")
print(analysis_tanks[, .(n_tanks = uniqueN(tank_panel_id), n_ty = .N),
                     by = .(age_treat_bin, texas_treated)][order(age_treat_bin, texas_treated)])
 
 
################################################################################
# S2: CEM MATCHING
################################################################################
 
cat("\n========================================\n")
cat("S2: CEM MATCHING\n")
cat("========================================\n\n")
 library(MatchIt)
# One row per tank
tank_chars <- unique(analysis_tanks[,
  .(tank_panel_id, panel_id, state, texas_treated,
    mm_wall, mm_fuel, mm_capacity, mm_install_cohort,
    age_at_treatment, age_treat_bin, age_quartile)])
 
# CEM on pricing dimensions
m_cem <- matchit(
  texas_treated ~ mm_wall + mm_fuel + mm_capacity + mm_install_cohort,
  data = tank_chars,
  method = "cem"
)
cat("CEM Summary:\n")
summary(m_cem)
 
# Extract weights and merge to panel
tank_chars[, cem_weight := m_cem$weights]
matched_ids <- tank_chars[cem_weight > 0, tank_panel_id]
 
matched_tanks <- merge(
  analysis_tanks[tank_panel_id %in% matched_ids],
  tank_chars[, .(tank_panel_id, cem_weight)],
  by = "tank_panel_id", all.x = TRUE
)
 
cat(sprintf("\nMatched sample:\n"))
cat(sprintf("  TX tanks: %s / %s (%.1f%%)\n",
    fmt_n(matched_tanks[texas_treated == 1, uniqueN(tank_panel_id)]),
    fmt_n(tank_chars[texas_treated == 1, .N]),
    100 * matched_tanks[texas_treated == 1, uniqueN(tank_panel_id)] /
      tank_chars[texas_treated == 1, .N]))
cat(sprintf("  CTL tanks: %s / %s (%.1f%%)\n",
    fmt_n(matched_tanks[texas_treated == 0, uniqueN(tank_panel_id)]),
    fmt_n(tank_chars[texas_treated == 0, .N]),
    100 * matched_tanks[texas_treated == 0, uniqueN(tank_panel_id)] /
      tank_chars[texas_treated == 0, .N]))
 

 ################################################################################
# S1: FACILITY-LEVEL SAMPLE CONSTRUCTION & AGGREGATION
################################################################################

cat("\n========================================\n")
cat("S1: FACILITY-LEVEL AGGREGATION\n")
cat("========================================\n\n")

# ---- 1a-1d. Base Tank Sample (Assuming analysis_tanks is generated as before) ----
# Ensure tank-level parameters are established prior to aggregation.

# ---- 1e. Aggregate to Facility Level (panel_id) ----
# To match at the facility level, tank-level characteristics must be collapsed 
# into facility-level summary statistics. Continuous/count variables are binned 
# to facilitate Coarsened Exact Matching (CEM).

fac_chars <- unique(analysis_tanks[, .(
  tank_panel_id, panel_id, texas_treated, 
  mm_wall, mm_fuel, mm_capacity, mm_install_cohort
)])

fac_chars <- fac_chars[, .(
  # Treatment status remains constant per facility
  texas_treated = max(texas_treated),
  
  # Total active tanks in the 1992-1998 cohort
  n_tanks = .N,
  
  # Proportion of Single-Walled tanks
  prop_sw = sum(mm_wall == "Single-Walled") / .N,
  
  # Age parameters (oldest tank dictates facility age risk)
  oldest_cohort = min(mm_install_cohort),
  
  # Modal fuel and capacity
  mode_fuel = mm_fuel[which.max(table(mm_fuel))],
  mode_capacity = mm_capacity[which.max(table(mm_capacity))]
), by = .(panel_id)]

# Coarsen variables for CEM compatibility
fac_chars[, `:=`(
  tank_count_bin = fcase(n_tanks == 1L, "1", n_tanks == 2L, "2", n_tanks >= 3L, "3+"),
  sw_dominance = fcase(prop_sw == 1, "All SW", prop_sw > 0.5, "Majority SW", default = "Minority/No SW"),
  cohort_bin = fcase(oldest_cohort <= 1994L, "1992-1994", default = "1995-1998")
)]

cat(sprintf("Facility-level pre-match sample:\n"))
cat(sprintf("  TX Facilities: %s\n", fmt_n(fac_chars[texas_treated == 1, .N])))
cat(sprintf("  CTL Facilities: %s\n", fmt_n(fac_chars[texas_treated == 0, .N])))

################################################################################
# S2: FACILITY-LEVEL CEM MATCHING
################################################################################

cat("\n========================================\n")
cat("S2: CEM MATCHING (FACILITY)\n")
cat("========================================\n\n")
# The MatchIt package requires engineered categorical variables to be explicitly typed as factors.
# Passing character vectors causes parsing failures in the underlying CEM evaluation environment.

# 1. Convert all categorical matching dimensions to factors
match_cols <- c("tank_count_bin", "sw_dominance", "cohort_bin", "mode_fuel", "mode_capacity")
fac_chars[, (match_cols) := lapply(.SD, as.factor), .SDcols = match_cols]

# 2. Execute CEM matching
m_cem_fac <- matchit(
  texas_treated ~ tank_count_bin + sw_dominance + cohort_bin + mode_fuel + mode_capacity,
  data = fac_chars,
  method = "cem"
)

# 3. Extract weights and merge
fac_chars[, cem_weight := m_cem_fac$weights]
matched_fac_ids <- fac_chars[cem_weight > 0, panel_id]

matched_facility_panel <- merge(
  analysis_tanks[panel_id %in% matched_fac_ids],
  fac_chars[, .(panel_id, cem_weight, tank_count_bin, sw_dominance, cohort_bin)],
  by = "panel_id", all.x = TRUE
)

cat(sprintf("\nMatched facility sample:\n"))
cat(sprintf("  TX Facilities: %s / %s (%.1f%%)\n", 
    fmt_n(fac_chars[panel_id %in% matched_fac_ids & texas_treated == 1, .N]),
    fmt_n(fac_chars[texas_treated == 1, .N]),
    100 * fac_chars[panel_id %in% matched_fac_ids & texas_treated == 1, .N] / 
      fac_chars[texas_treated == 1, .N]))
cat(sprintf("  CTL Facilities: %s / %s (%.1f%%)\n", 
    fmt_n(fac_chars[panel_id %in% matched_fac_ids & texas_treated == 0, .N]),
    fmt_n(fac_chars[texas_treated == 0, .N]),
    100 * fac_chars[panel_id %in% matched_fac_ids & texas_treated == 0, .N] / 
      fac_chars[texas_treated == 0, .N]))

 
################################################################################
# S3: CONTROL GROUP MEANS
################################################################################
 
cat("\n========================================\n")
cat("S3: CONTROL GROUP MEANS\n")
cat("========================================\n\n")
 
ctrl_means <- matched_tanks[texas_treated == 0, .(
  pre      = mean(closure_event[panel_year %between% c(1992L, 1997L)], na.rm = TRUE),
  post_all = mean(closure_event[panel_year >= 1999L], na.rm = TRUE),
  post_short = mean(closure_event[panel_year %between% c(1999L, 2004L)], na.rm = TRUE),
  post_long  = mean(closure_event[panel_year >= 2005L], na.rm = TRUE)
)]
 
cat(sprintf("Control means (per 1,000 tank-years):\n"))
cat(sprintf("  Pre-reform (1992-1997):  %.1f\n", ctrl_means$pre * 1000))
cat(sprintf("  Post-reform (all):       %.1f\n", ctrl_means$post_all * 1000))
cat(sprintf("  Post short (1999-2004):  %.1f\n", ctrl_means$post_short * 1000))
cat(sprintf("  Post long (2005-2020):   %.1f\n", ctrl_means$post_long * 1000))
 
ctrl_means_age <- matched_tanks[
  texas_treated == 0 & panel_year %between% c(1999L, 2004L),
  .(ctrl_post_short = mean(closure_event, na.rm = TRUE), N = .N),
  by = age_treat_bin
][order(age_treat_bin)]
 
cat("\nControl means by age bin (post-short, per 1,000):\n")
ctrl_means_age[, rate_per_1k := ctrl_post_short * 1000]
print(ctrl_means_age)
 
ctrl_means_quartile <- matched_tanks[
  texas_treated == 0 & panel_year %between% c(1999L, 2004L),
  .(ctrl_post_short = mean(closure_event, na.rm = TRUE), N = .N),
  by = age_quartile
][order(age_quartile)]
 
cat("\nControl means by age quartile (post-short, per 1,000):\n")
ctrl_means_quartile[, rate_per_1k := ctrl_post_short * 1000]
print(ctrl_means_quartile)



 ################################################################################
# OUTPUT SETUP
################################################################################

library(fixest)
library(survival)
library(modelsummary)

# Save base-R plots (iplot) as both PDF and PNG
save_fig <- function(plot_fn, filename, width = 7, height = 5.5) {
  pdf(file.path(OUTPUT_FIGURES, paste0(filename, ".pdf")),
      width = width, height = height, family = "Times")
  plot_fn()
  dev.off()
  png(file.path(OUTPUT_FIGURES, paste0(filename, ".png")),
      width = width, height = height, units = "in", res = 300)
  plot_fn()
  dev.off()
  invisible(NULL)
}

setFixest_etable(style.tex = style.tex("aer"), digits = 4, se.below = TRUE, depvar = FALSE)
setFixest_etable(fixef.print = FALSE)

################################################################################
# VARIABLE CONSTRUCTION
################################################################################

med_age <- median(matched_tanks$age_at_treatment, na.rm = TRUE)
cat(sprintf("Median age at treatment: %.0f years\n", med_age))

matched_tanks[, below_median_age := as.integer(age_at_treatment <  med_age)]
matched_tanks[, above_median_age := as.integer(age_at_treatment >= med_age)]
matched_tanks[, single_wall      := as.integer(mm_wall == "Single-walled")]
matched_tanks[, did_x_sw         := did_term * single_wall]
matched_tanks[, did_x_old        := did_term * above_median_age]
matched_tanks[, sw_x_old         := single_wall * above_median_age]
matched_tanks[, did_x_sw_old     := did_term * single_wall * above_median_age]

# Above-median label (used in both OLS and Cox tables)
age_label <- sprintf("Above Median Age ($\\geq$%.0f yr)", med_age)

################################################################################
# COX SAMPLE CONSTRUCTION
# exact_split_df is built in S5 — already survSplit at REFORM_DAYS with
# did_term, age_enter/exit, reform_ep. Filter to matched tank IDs, add
# make_model_noage and age_at_treatment to match matched_tanks conventions,
# then remove first-year churn (exit year == install year with failure).
################################################################################

matched_ids <- unique(matched_tanks$tank_panel_id)

cox_sample <- exact_split_df[tank_panel_id %in% matched_ids]

# age_at_treatment: years between install cohort and 1999 reform
cox_sample[, age_at_treatment := 1999L - as.integer(mm_install_cohort)]
cox_sample[, above_median_age := as.integer(age_at_treatment >= med_age)]

# make_model_noage: wall x fuel x capacity without cohort
cox_sample[, make_model_noage := paste(mm_wall, mm_fuel, mm_capacity, sep = "_")]

# Remove first-year churn: exit year == install year & failure == 1
cox_sample[, install_yr := as.integer(mm_install_cohort)]
cox_sample[, exit_yr    := as.integer(
  format(as.Date(t_exit, origin = "1970-01-01"), "%Y"))]
cox_sample <- cox_sample[!(exit_yr == install_yr & failure == 1L)]

cat(sprintf("Cox sample: %s rows, %s tanks, %s events\n",
    format(nrow(cox_sample),                  big.mark = ","),
    format(uniqueN(cox_sample$tank_panel_id), big.mark = ","),
    format(sum(cox_sample$failure),           big.mark = ",")))


################################################################################
# TABLE 1: OLS STEP-IN TABLE
# M1  Pooled matched + year FE
# M2  Pooled matched + cell x year FE
# M3  DiD x above_median_age interaction, year FE
# M4  DiD x above_median_age interaction, cell x year FE
# M5  Below-median age subsample, year FE
# M6  Above-median age subsample, year FE
# NOTE: above_median_age level effect is time-invariant and absorbed by tank
#       FE, so it cannot appear as a main effect. HTE is identified via the
#       interaction term (M3/M4) and confirmed by subsample split (M5/M6).
################################################################################

m1 <- feols(
  closure_event ~ did_term | tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

m2 <- feols(
  closure_event ~ did_term | tank_panel_id + panel_year^make_model_noage,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

m3 <- feols(
  closure_event ~ did_term + did_x_old | tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

m4 <- feols(
  closure_event ~ did_term + did_x_old |
    tank_panel_id + panel_year^make_model_noage,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

m5 <- feols(
  closure_event ~ did_term | tank_panel_id + panel_year,
  data = matched_tanks[above_median_age == 0L],
  weights = ~cem_weight, cluster = ~state)

m6 <- feols(
  closure_event ~ did_term | tank_panel_id + panel_year,
  data = matched_tanks[above_median_age == 1L],
  weights = ~cem_weight, cluster = ~state)

ols_dict <- c(
  "did_term"  = "DiD",
  "did_x_old" = paste0("DiD $\\times$ Old ($\\geq$", med_age, " yr)")
)

ctrl_mean_post <- sprintf("%.4f", ctrl_means$post_all)

etable(
  m1, m2, m3, m4, m5, m6,
  headers     = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
  dict        = ols_dict,
  fitstat     = ~ n + r2 + wr2,
  keep        = c("^DiD$", "DiD.*Old"),
  extralines  = list(
    "Tank FE"                    = rep("\\checkmark", 6),
    "Year FE"                    = rep("\\checkmark", 6),
    "Cell $\\times$ Year FE"     = c("","\\checkmark","","\\checkmark","",""),
    "Age subsample"              = c("All","All","All","All",
                                     paste0("$<$", med_age, " yr"),
                                     paste0("$\\geq$", med_age, " yr")),
    "Control Mean (post-reform)" = rep(ctrl_mean_post, 6)
  ),
  file    = file.path(OUTPUT_TABLES, "T1_ols_stepwise.tex"),
  replace = TRUE,
  notes   = paste0(
    "Cluster-robust SEs by state in parentheses. ",
    "Cell is make-model (excluding age). ",
    "Old $=$ above median age at treatment ($\\geq$", med_age, " yr). ",
    "Level effect of age absorbed by tank FE; HTE identified via interaction ",
    "(cols.\\ 3--4) and subsample split (cols.\\ 5--6). ",
    "Control mean is unweighted post-reform (1999+) closure rate ",
    "for matched control tanks. CEM weights applied throughout."
  )
)
cat("Saved: Output/Tables/T1_ols_stepwise.tex\n")


################################################################################
# TABLE 2: COX STEP-IN TABLE
# C1  Pooled, strata(make_model_noage)
# C2  + DiD x above_median_age interaction
# C3  Below-median subsample
# C4  Above-median subsample
#
# SURVPLIT at reform year (1999-01-01):
#   cox_sample spells span installation through exit/censor and cross the
#   reform boundary. survSplit at 1999 splits each crossing spell into a
#   pre- and post-reform row so did_term is a proper time-varying covariate
#   in the counting process formulation Surv(t_enter, t_exit, failure).
#
# STRATA: make_model_noage (not make_model_tank) so age variation is not
#   absorbed into the baseline hazard. make_model_tank includes age at
#   install which would partial out the HTE we are trying to identify.
#
# NOTE: above_median_age level effect is tank-invariant and collinear with
#   strata(make_model_noage) — not identified. Dropped; HTE via interaction
#   (C2) and subsample (C3/C4) only.
################################################################################

cox_reform_cut <- as.numeric(as.Date("1999-01-01"))

cox_split <- survSplit(
  formula  = Surv(t_enter, t_exit, failure) ~ .,
  data     = as.data.frame(cox_sample),
  cut      = cox_reform_cut,
  episode  = "reform_ep_split"
)
setDT(cox_split)
cox_split <- cox_split[t_exit > t_enter]

# Re-derive did_term on split data (texas_treated * post-reform row)
cox_split[, did_term := texas_treated * as.integer(reform_ep_split == 2L)]
# above_median_age and make_model_noage carry through from cox_sample via survSplit

cox1 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    strata(make_model_noage),
  data = cox_split, cluster = state)

cox2 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term + did_term:above_median_age +
    strata(make_model_noage),
  data = cox_split, cluster = state)

cox3 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    strata(make_model_noage),
  data = cox_split[above_median_age == 0L], cluster = state)

cox4 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    strata(make_model_noage),
  data = cox_split[above_median_age == 1L], cluster = state)

cox_coef_map <- c(
  "did_term"                  = "DiD",
  "did_term:above_median_age" = paste0("DiD $\\times$ Old ($\\geq$", med_age, " yr)")
)

cox_add_rows <- tibble::tribble(
  ~term,                      ~`(1)`,        ~`(2)`,        ~`(3)`,                         ~`(4)`,
  "Strata (make-model, no age)", "\\checkmark", "\\checkmark", "\\checkmark",                "\\checkmark",
  "Age subsample",            "All",         "All",         paste0("$<$", med_age, " yr"),  paste0("$\\geq$", med_age, " yr")
)

modelsummary::msummary(
  list("(1)" = cox1, "(2)" = cox2, "(3)" = cox3, "(4)" = cox4),
  coef_map     = cox_coef_map,
  exponentiate = TRUE,
  statistic    = "({std.error})",
  stars        = c("*" = .1, "**" = .05, "***" = .01),
  escape       = FALSE,
  gof_map      = list(
    list(raw = "nobs",   clean = "Observations", fmt = scales::comma),
    list(raw = "nevent", clean = "Events",        fmt = scales::comma)
  ),
  add_rows = cox_add_rows,
  output   = file.path(OUTPUT_TABLES, "T2_cox_stepwise.tex"),
  notes    = paste0(
    "Cluster-robust SEs by state in parentheses. ",
    "Hazard ratios reported (exp[$\\hat{\\beta}$]). ",
    "Spells split at January 1, 1999 so DiD is a proper time-varying covariate. ",
    "Strata defined on make-model excluding age (\\texttt{make\\_model\\_noage}) ",
    "so that age variation is not absorbed into the baseline hazard. ",
    "Old $=$ above median age at treatment ($\\geq$", med_age, " yr). ",
    "Level effect of age not identified (tank-invariant, collinear with strata); ",
    "HTE identified via interaction (col.\\ 2) and subsample split (cols.\\ 3--4)."
  )
)
cat("Saved: Output/Tables/T2_cox_stepwise.tex\n")


################################################################################
# TABLE 3: TRIPLE DIFF — DiD x Single-Walled x Old
#
# Identified terms (time-varying via did_term):
#   did_x_sw    = DiD x Single-Walled
#   did_x_old   = DiD x Old
#   did_x_sw_old = DiD x Single-Walled x Old
#
# Absorbed by tank FE (time-invariant):
#   single_wall, sw_x_old, above_median_age
#   These are dropped silently — correct behaviour, not a bug.
#
# Step-in structure:
#   TD1  DiD only (baseline)
#   TD2  + DiD x Single-Walled
#   TD3  + DiD x Old
#   TD4  Full triple (DiD + DiD×SW + DiD×Old + DiD×SW×Old)
#   TD5  TD4 + cell x year FE
################################################################################

matched_tanks[, single_wall := as.integer(mm_wall == "Single-Walled")]


td0 <- feols(
  closure_event ~ did_term |
    tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

td1 <- feols(
  closure_event ~ did_term*single_wall*above_median_age |
    tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)



etable(
  td0, td1,
  headers     = c("(1)", "(2)"),
  fitstat     = ~ n + r2,
  file    = file.path(OUTPUT_TABLES, "T3_triple_diff.tex"),
  replace = TRUE,
  notes   = paste0(
    "Cluster-robust SEs by state in parentheses. ",
    "Old $=$ above median age at treatment ($\\geq$", med_age, " yr). ",
    "Single-Walled $=$ \\texttt{mm\\_wall == ``Single-walled''}. ",
    "Level effects of Single-Walled and Old are time-invariant and ",
    "absorbed by tank FE. CEM weights applied throughout."
  )
)

cat("Saved: Output/Tables/T3_triple_diff.tex\n")


################################################################################
# EVENT STUDY MODELS
################################################################################
summary(matched_tanks$mm_install_cohort)
matched_tanks[,deadline_sw := as.integer(panel_year == 1998) * single_wall]
mean(matched_tanks$deadline_sw)
# --- Pooled event studies (matched sample) ---
# Alternative: reform date = 1998 (anticipation-inclusive)
matched_tanks[, did_term_early := texas_treated * as.integer(panel_year >= 1998L)]

m_es_matched <- feols(
  closure_event ~ i(rel_year_es, texas_treated, ref = -2) +deadline_sw|
    tank_panel_id + panel_year,
  data    = matched_tanks,
  weights = ~cem_weight,
  cluster = ~state
)

etable(m_es_matched)
m_es_cell <- feols(
  closure_event ~ i(rel_year_es, texas_treated, ref = -2) |
    tank_panel_id + panel_year^make_model_noage,
  data    = matched_tanks,
  weights = ~cem_weight,
  cluster = ~state
)

# --- Age-split event studies ---
m_es_below_med <- feols(
  closure_event ~ i(rel_year_es, texas_treated, ref = -2) |
    tank_panel_id + panel_year,
  data    = matched_tanks[above_median_age == 0L],
  weights = ~cem_weight,
  cluster = ~state
)

m_es_above_med <- feols(
  closure_event ~ i(rel_year_es, texas_treated, ref = -1) |
    panel_id + panel_year,
  data    = matched_tanks[above_median_age == 1L],
  weights = ~cem_weight,
  cluster = ~state
)


################################################################################
# FIGURES 1-4: EVENT STUDIES (ggplot2)
#
# Strategy: extract coefficient data from each fixest model using
# broom::tidy(), then plot with ggplot2 for full layout control.
# The reference year (-2) is not in the coef vector — we add a manual
# zero row so the gap is explicit on the x-axis.
################################################################################

library(ggplot2)
library(broom)

ES_YLIM <- c(-0.025, 0.025)
ES_REF  <- -2L

# Extract event-study coefficients from a fixest i() model into a tidy tibble.
# Adds the omitted reference year as an explicit zero row.
es_tidy <- function(model, ref = ES_REF) {
  df <- tidy(model, conf.int = TRUE)
  # fixest names i() terms as "rel_year_es::X:texas_treated"
  df <- df[grepl("rel_year_es::", df$term), ]
  df$year <- as.integer(gsub("rel_year_es::([-0-9]+):texas_treated",
                              "\\1", df$term))
  # Add the omitted reference row
  ref_row <- data.frame(
    term     = paste0("ref_", ref),
    estimate = 0, std.error = 0,
    conf.low = 0, conf.high = 0,
    year     = ref,
    stringsAsFactors = FALSE
  )
  df <- rbind(df[, c("term","estimate","std.error","conf.low","conf.high","year")],
              ref_row)
  df[order(df$year), ]
}

# Base ggplot theme for event studies — journal-clean
es_theme <- function() {
  theme_classic(base_size = 11, base_family = "Times") +
  theme(
    axis.line        = element_line(colour = "black", linewidth = 0.4),
    axis.ticks       = element_line(colour = "black", linewidth = 0.3),
    axis.text        = element_text(colour = "black"),
    panel.grid       = element_blank(),
    legend.position  = "none",
    plot.title       = element_blank(),
    plot.margin      = margin(8, 10, 8, 8)
  )
}

# Build one event-study ggplot.
# ref_label: text to annotate the omitted year bracket on the x-axis.
es_ggplot <- function(model, point_col, fill_col = NULL,
                      label = NULL, ylim = ES_YLIM, ref = ES_REF) {

  if (is.null(fill_col)) fill_col <- adjustcolor(point_col, alpha.f = 0.15)

  df      <- es_tidy(model, ref = ref)
  ref_row <- df[df$year == ref, ]
  est_row <- df[df$year != ref, ]

  p <- ggplot(df, aes(x = year, y = estimate)) +
    # CI ribbon (exclude the zero reference row from ribbon)
geom_errorbar(data = est_row,
              aes(ymin = conf.low, ymax = conf.high),
              colour = point_col, width = 0.25, linewidth = 0.45) +
                  # Zero reference line
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.5) +
    # Reform onset dashed line
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    # Connected point estimates
    # geom_line(data = est_row, colour = point_col, linewidth = 0.55) +
    geom_point(data = est_row, colour = point_col, size = 1.8) +
    # Omitted reference point (open circle at zero)
    geom_point(data = ref_row, shape = 1, size = 2.2,
               colour = "grey40") +
    # Bracket annotation for omitted year
    annotate("text", x = ref, y = ylim[1] * 0.82,
             label = sprintf("(%d)\nomitted", ref),
             size = 2.8, colour = "grey35", fontface = "italic",
             lineheight = 0.85) +
    # Optional sample label
    { if (!is.null(label))
        annotate("text", x = -Inf, y = Inf, label = label,
                 hjust = -0.08, vjust = 1.4, size = 3,
                 colour = "grey25", fontface = "plain")
      else list() } +
    scale_x_continuous(breaks = sort(unique(df$year[df$year != ref]))) +
    scale_y_continuous(limits = ylim) +
    labs(x = "Years Relative to 1999 (2 Years Before as Reference)",
         y = "Effect on Annual Closure Probability") +
    es_theme()

  p
}

# Save as both PDF and PNG
save_gg <- function(p, filename, width = 7, height = 5) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(filename, ".pdf")),
         plot = p, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(OUTPUT_FIGURES, paste0(filename, ".png")),
         plot = p, width = width, height = height, dpi = 300)
  invisible(p)
}

# --- Figure 1: Pooled matched ---
p1 <- es_ggplot(m_es_matched,   point_col = "grey25",
                label = "Pooled (matched sample)")
save_gg(p1, "F1_es_pooled_matched")

# --- Figure 2: Pooled cell x year FE ---
p2 <- es_ggplot(m_es_cell,      point_col = "grey25",
                label = "Pooled (matched + type \u00d7 year FE)")
save_gg(p2, "F2_es_pooled_cell_fe")

# --- Figure 3: Below median age ---
p3 <- es_ggplot(m_es_below_med, point_col = "#2166ac",
                label = sprintf("Below Median Age (< %.0f yr)", med_age))
save_gg(p3, "F3_es_below_median")

# --- Figure 4: Above median age ---
p4 <- es_ggplot(m_es_above_med, point_col = "#d6604d",
                label = sprintf("Above Median Age (\u2265 %.0f yr)", med_age))
save_gg(p4, "F4_es_above_median")

cat("Saved: Output/Figures/F1-F4 (PDF + PNG)\n")



################################################################################
# Raw Closure Rates: Estimation Sample
# Two separate figures, each plotting Texas and control states on the same axes.
#   Figure A: Matched tank sample (OLS estimation sample, CEM-weighted)
#   Figure B: Cox sample (exact-date spells collapsed to annual)
################################################################################

# ---- OLS matched sample rates ----
ols_rates <- matched_tanks[, .(
  closure_rate = weighted.mean(closure_event, w = cem_weight, na.rm = TRUE),
  n            = .N
), by = .(
  panel_year,
  group = fifelse(texas_treated == 1L, "Texas", "Control States")
)][panel_year >= 1990L]

# ---- Cox sample rates ----
cox_sample[, plot_year := as.integer(
  format(as.Date(t_exit, origin = "1970-01-01"), "%Y")
)]
cox_rates <- cox_sample[plot_year >= 1990L & plot_year <= 2020L, .(
  closure_rate = mean(failure, na.rm = TRUE),
  n            = .N
), by = .(
  plot_year,
  group = fifelse(texas_treated == 1L, "Texas", "Control States")
)]

# ---- Shared plot function ----
COL_GROUP <- c("Texas" = COL_TX, "Control States" = COL_CTRL)

plot_raw_closure <- function(dt, x_var, reform_x, reform_label,
                              show_mandate = FALSE) {
  p <- ggplot(dt, aes(x = .data[[x_var]], y = closure_rate,
                      colour = group, group = group)) +
    { if (show_mandate)
        annotate("rect",
                 xmin = 1989, xmax = 1993,
                 ymin = -Inf, ymax = Inf,
                 fill = COL_PRE, alpha = 0.12)
      else list() } +
    geom_vline(xintercept = reform_x, linetype = "dashed",
               colour = "grey35", linewidth = 0.55) +
    annotate("text",
             x = reform_x + 0.25,
             y = max(dt$closure_rate, na.rm = TRUE) * 0.97,
             label = reform_label,
             hjust = 0, size = 2.8, colour = "grey35") +
    geom_line(linewidth = 0.75) +
    geom_point(size = 1.8) +
    scale_colour_manual(values = COL_GROUP,
                        guide  = guide_legend(direction = "horizontal")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
    labs(x = "Calendar Year",
         y = "Annual Tank Closure Rate",
         colour = NULL) +
    theme_pub() +
    theme(
      legend.position    = c(0.85, 0.92),
      legend.background  = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank()
    )

  if (show_mandate) {
    p <- p + annotate("text",
                      x     = 1991,
                      y     = max(dt$closure_rate, na.rm = TRUE) * 0.75,
                      label = "Mandate\nWindow",
                      hjust = 0.5, size = 2.6,
                      colour = COL_PRE, fontface = "italic")
  }
  p
}

# ---- Figure A: OLS matched sample ----
p_ols <- plot_raw_closure(
  dt             = ols_rates,
  x_var          = "panel_year",
  reform_x       = 1999,
  reform_label   = "Reform\n(Jan 1999)",
  show_mandate   = TRUE
)

ggsave(file.path(OUTPUT_FIGURES, "Figure_OLS_Sample_ClosureRates.pdf"),
       p_ols, width = 7, height = 4.5, device = cairo_pdf)
ggsave(file.path(OUTPUT_FIGURES, "Figure_OLS_Sample_ClosureRates.png"),
       p_ols, width = 7, height = 4.5, dpi = 300)
cat("Saved: Figure_OLS_Sample_ClosureRates\n")

# ---- Figure B: Cox sample ----
p_cox <- plot_raw_closure(
  dt             = cox_rates,
  x_var          = "plot_year",
  reform_x       = 1999,
  reform_label   = "Reform\n(Dec 1998)",
  show_mandate   = FALSE
)

ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_Sample_ClosureRates.pdf"),
       p_cox, width = 7, height = 4.5, device = cairo_pdf)
ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_Sample_ClosureRates.png"),
       p_cox, width = 7, height = 4.5, dpi = 300)
cat("Saved: Figure_Cox_Sample_ClosureRates\n")



#### end or pretty &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

library(HonestDiD)

# ---- Extract only the event-study coefficients (drop deadline_sw) ----
b_full <- coef(m_es_matched)
V_full <- vcov(m_es_matched, type = "clustered")

es_idx <- grepl("rel_year_es", names(b_full))
b <- b_full[es_idx]
V <- V_full[es_idx, es_idx]

cat("ES coefficients:\n")
print(names(b))

# Pre: -5, -4, -3, -1  (ref = -2 is omitted; -1 is pre-reform)
# Post: 0 through 10
n_pre  <- 4L
n_post <- 11L
stopifnot(n_pre + n_post == length(b))

# ---- Max observed pre-trend slope ----
rel_years <- as.integer(regmatches(names(b), regexpr("-?[0-9]+", names(b))))
pre_coefs <- sort_by(b[rel_years < 0], rel_years[rel_years < 0])
max_pre_slope <- max(abs(diff(pre_coefs)))
cat("Max pre-trend slope:", round(max_pre_slope, 5), "\n")

# ---- Sensitivity ----
sensitivity <- createSensitivityResults(
  betahat        = b,
  sigma          = V,
  numPrePeriods  = n_pre,
  numPostPeriods = n_post,
  alpha          = 0.05
)
breakdown_M <- sensitivity$M[which(sign(sensitivity$lb) != sign(sensitivity$ub))[1]]
cat("Breakdown M:", round(breakdown_M, 5), "\n")

p_rr <- ggplot(sensitivity, aes(x = M)) +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = COL_TX, alpha = 0.20) +
  geom_line(aes(y = lb), color = COL_TX, linewidth = 0.7) +
  geom_line(aes(y = ub), color = COL_TX, linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  # Shade the breakdown region
  geom_vline(xintercept = breakdown_M, linetype = "dotted", color = "grey30", linewidth = 0.5) +
  geom_vline(xintercept = max_pre_slope, linetype = "dashed", color = "steelblue", linewidth = 0.5) +
  annotate("text", x = breakdown_M * 1.03, y = 0.002,
           label = sprintf("Breakdown\nM = %.4f", breakdown_M),
           hjust = 0, size = 2.8, color = "grey30") +
  annotate("text", x = max_pre_slope * 1.03, y = max(sensitivity$ub) * 0.92,
           label = sprintf("Max observed\npre-trend slope\n= %.4f", max_pre_slope),
           hjust = 0, size = 2.8, color = "steelblue") +
  scale_x_continuous(name = "M: Max Slope of Parallel Trends Violation (pp/year)",
                     labels = scales::number_format(accuracy = 0.001)) +
  scale_y_continuous(name = "Robust 95% CI for ATT",
                     labels = scales::number_format(accuracy = 0.001)) +
  theme_pub()


save_gg(p_rr, "Figure_RR_Sensitivity")

#### end or pretty &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


### whos respoinding to teh mandate 

# ============================================================
# DIAGNOSTIC: What drives the 1997-1998 closure spike?
# Is it vintage-specific? State-specific? State×vintage?
# ============================================================

# Use the full matched sample, pre-reform years only
pre_data <- matched_tanks[panel_year %between% c(1990L, 1999L)]

# ---- 1. Closure rate by vintage × year × treatment ----
vintage_yr <- pre_data[, .(
  closure_rate = mean(closure_event, na.rm = TRUE),
  n_closures = sum(closure_event),
  N = .N
), by = .(mm_install_cohort, panel_year, texas_treated)]

# Wide format for TX vs control comparison
vintage_wide <- dcast(vintage_yr, 
                      mm_install_cohort + panel_year ~ texas_treated,
                      value.var = c("closure_rate", "n_closures", "N"))
setnames(vintage_wide, 
         c("cohort", "year", "rate_CTL", "rate_TX", 
           "closures_CTL", "closures_TX", "N_CTL", "N_TX"))
vintage_wide[, gap := rate_TX - rate_CTL]

cat("Closure rates by vintage × year (sorted by largest gaps):\n")
print(vintage_wide[order(-abs(gap))][1:30])

# ---- 2. Heatmap: closure rate by vintage × year (control only) ----
ctrl_vintage <- pre_data[texas_treated == 0, .(
  closure_rate = mean(closure_event, na.rm = TRUE),
  n_closures = sum(closure_event),
  N = .N
), by = .(mm_install_cohort, panel_year)]

p_heat_ctrl <- ggplot(ctrl_vintage, 
       aes(x = panel_year, y = factor(mm_install_cohort), 
           fill = closure_rate)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n_closures > 0, n_closures, "")), 
            size = 2.5, color = "grey20") +
  scale_fill_gradient(low = "white", high = COL_TX, 
                      labels = scales::percent_format()) +
  labs(x = "Calendar Year", y = "Install Cohort",
       fill = "Closure\nRate",
       title = "Control states: closure rate by vintage × year") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_heat_ctrl)
ggsave(file.path(OUTPUT_FIGURES, "Diag_Vintage_Year_Heatmap_Control.png"),
       p_heat_ctrl, width = 10, height = 6, dpi = 300, bg = "white")

# ---- 3. Same for TX ----
tx_vintage <- pre_data[texas_treated == 1, .(
  closure_rate = mean(closure_event, na.rm = TRUE),
  n_closures = sum(closure_event),
  N = .N
), by = .(mm_install_cohort, panel_year)]

p_heat_tx <- ggplot(tx_vintage, 
       aes(x = panel_year, y = factor(mm_install_cohort), 
           fill = closure_rate)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n_closures > 0, n_closures, "")), 
            size = 2.5, color = "grey20") +
  scale_fill_gradient(low = "white", high = COL_CTRL,
                      labels = scales::percent_format()) +
  labs(x = "Calendar Year", y = "Install Cohort",
       fill = "Closure\nRate",
       title = "Texas: closure rate by vintage × year") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_heat_tx)
ggsave(file.path(OUTPUT_FIGURES, "Diag_Vintage_Year_Heatmap_TX.png"),
       p_heat_tx, width = 10, height = 6, dpi = 300, bg = "white")

# ---- 4. State-level: which control states spike in 1997-1998? ----
state_spike <- pre_data[texas_treated == 0 & panel_year %between% c(1995L, 1999L), .(
  closure_rate = mean(closure_event, na.rm = TRUE),
  n_closures = sum(closure_event),
  N = .N
), by = .(state, panel_year)]

state_spike_wide <- dcast(state_spike, state ~ panel_year, 
                          value.var = "closure_rate")
state_spike_wide[, spike_97 := `1997` - `1996`]
state_spike_wide[, spike_98 := `1998` - `1996`]

cat("\nControl state closure rates 1995-1999:\n")
print(state_spike_wide[order(-spike_98)])

# ---- 5. State × vintage: is the spike concentrated? ----
state_vintage_spike <- pre_data[
  texas_treated == 0 & panel_year %between% c(1996L, 1999L), .(
  closure_rate = mean(closure_event, na.rm = TRUE),
  n_closures = sum(closure_event),
  N = .N
), by = .(state, mm_install_cohort, panel_year)]

# Biggest spikes
state_vintage_98 <- state_vintage_spike[panel_year == 1998]
state_vintage_97 <- state_vintage_spike[panel_year == 1997]
state_vintage_96 <- state_vintage_spike[panel_year == 1996]

spike_compare <- merge(
  state_vintage_96[, .(state, mm_install_cohort, rate_96 = closure_rate)],
  state_vintage_98[, .(state, mm_install_cohort, rate_98 = closure_rate, 
                        N_98 = N, closures_98 = n_closures)],
  by = c("state", "mm_install_cohort"), all = TRUE
)
spike_compare[, spike := rate_98 - rate_96]

cat("\nLargest state × vintage spikes (1998 vs 1996):\n")
print(spike_compare[order(-spike)][1:20])

# ---- 6. Summary: which vintages drive the 1997-1998 spike? ----
cat("\nClosure rate by vintage, 1996 vs 1997 vs 1998 (control only):\n")
print(dcast(ctrl_vintage[panel_year %between% c(1996L, 1999L)],
            mm_install_cohort ~ panel_year, value.var = "closure_rate"))


#######################################################!!!!










# ---- Event study figures ----
# p_es_tank <- plot_es(m_es_tank,  # uses your plot_es helper if available
#                      ylab = "Effect on Annual Tank Closure Probability",
#                      filename = file.path(OUTPUT_FIGURES,
#                                           "Figure_ES_TankLevel_CellxYear.png"))
 
iplot(m_es_tank,
      main = "Tank-level: tank_id + cell×year FE, ref=-2",
      xlab = "Years Relative to Reform",
      ylab = "Treatment Effect on Closure Probability",
      col = COL_TX, pt.join = TRUE, ci.lwd = 1.5,
      ylim = c(-0.02, 0.02))
 
iplot(m_es_fac,
      main = "Facility-level (from tank data): fac_id + year FE, ref=-2",
      xlab = "Years Relative to Reform",
      ylab = "Treatment Effect on Closure Probability",
      col = COL_TX, pt.join = TRUE, ci.lwd = 1.5,
      ylim = c(-0.02, 0.06))
 
 
#### S6: Cox Proportional Hazard (Calendar Time) ####
 
cat("\n========================================\n")
cat("S6: COX MODEL (CALENDAR TIME)\n")
cat("========================================\n\n")
 
# ---- Build Cox dataset for analysis sample ----
cox_facs <- exact_split_df[
  panel_id %in% sw_exposed_facs &
  mm_install_cohort >= 1990L & mm_install_cohort <= 1998L
]
 
cat(sprintf("Cox sample: %s rows, %s tanks, %s events\n",
    fmt_n(nrow(cox_facs)),
    fmt_n(uniqueN(cox_facs$tank_panel_id)),
    fmt_n(sum(cox_facs$failure))))
 
# ---- Primary Cox: pooled post-reform effect ----
m_cox_pooled <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term + strata(make_model_tank),
  data = cox_facs,
  cluster = state
)
cat("\nPooled Cox (calendar time):\n")
summary(m_cox_pooled)
 
# ---- Cox: short-run / long-run split ----
# Second survSplit at 2005-01-01 to properly split person-time
cox_facs2 <- survSplit(
  formula = Surv(t_enter, t_exit, failure) ~ .,
  data = as.data.frame(cox_facs),
  cut = as.numeric(as.Date("2005-01-01")),
  episode = "period_ep"
)
setDT(cox_facs2)
cox_facs2 <- cox_facs2[t_exit > t_enter]
 
cox_facs2[, `:=`(
  did_short = texas_treated * as.integer(reform_ep == 2L & period_ep == 1L),
  did_long  = texas_treated * as.integer(reform_ep == 2L & period_ep == 2L)
)]
 
m_cox_split <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_short + did_long + strata(make_model_tank),
  data = cox_facs2,
  cluster = state
)
cat("\nSplit Cox (calendar time):\n")
summary(m_cox_split)
 
 
#### S7: Cox Proportional Hazard (Age/Duration Scale) ####
 
cat("\n========================================\n")
cat("S7: COX MODEL (AGE SCALE)\n")
cat("========================================\n\n")
 
# Age scale: time axis = years since tank installation
# The reform indicator still comes from survSplit at the reform date
# but the baseline hazard is now a function of tank age, not calendar time
 
m_cox_age_pooled <- coxph(
  Surv(age_enter, age_exit, failure) ~ did_term + strata(make_model_tank),
  data = cox_facs,
  cluster = state
)
cat("Pooled Cox (age scale):\n")
summary(m_cox_age_pooled)
 
# Split on age scale (using calendar-time split data)
m_cox_age_split <- coxph(
  Surv(age_enter, age_exit, failure) ~ did_short + did_long + strata(make_model_tank),
  data = cox_facs2,
  cluster = state
)
cat("\nSplit Cox (age scale):\n")
summary(m_cox_age_split)
 
 
#### S8: HTE by Install Cohort ####
 
cat("\n========================================\n")
cat("S8: HTE BY INSTALL COHORT\n")
cat("========================================\n\n")
 
# ---- OLS: Short/long × old/young ----
analysis_tanks[, `:=`(
  did_short_old   = did_short * old_cohort,
  did_short_young = did_short * (1L - old_cohort),
  did_long_old    = did_long  * old_cohort,
  did_long_young  = did_long  * (1L - old_cohort)
)]
 
m_hte_ols <- feols(
  closure_event ~ did_short_old + did_short_young +
                  did_long_old + did_long_young |
    tank_panel_id + panel_year^make_model_tank,
  data = analysis_tanks, cluster = ~state
)
 
cat("OLS HTE by cohort:\n")
etable(m_hte_ols, digits = 4, se.below = TRUE)
 
# ---- Cox: Short/long × old/young ----
cox_facs2[, old_cohort := as.integer(mm_install_cohort <= 1993L)]
cox_facs2[, `:=`(
  did_short_old   = did_short * old_cohort,
  did_short_young = did_short * (1L - old_cohort),
  did_long_old    = did_long  * old_cohort,
  did_long_young  = did_long  * (1L - old_cohort)
)]
 
m_hte_cox <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_short_old + did_short_young +
    did_long_old + did_long_young + strata(make_model_tank),
  data = cox_facs2,
  cluster = state
)
cat("\nCox HTE by cohort:\n")
summary(m_hte_cox)
 
 
#### S9: Placebo Tests ####
 
cat("\n========================================\n")
cat("S9: PLACEBO TESTS\n")
cat("========================================\n\n")
 
# Run event studies on control states as pseudo-treated
# to verify that the FE structure doesn't produce spurious effects
placebo_states <- c("OH", "CO", "NC", "MO")
 
for (st in placebo_states) {
  analysis_tanks[texas_treated == 0,
                 placebo_treat := as.integer(state == st)]
 
  m_plac <- feols(
    closure_event ~ i(rel_year_es, placebo_treat, ref = -2) |
      tank_panel_id + panel_year^make_model_tank,
    data = analysis_tanks[texas_treated == 0],
    cluster = ~state
  )
 
  iplot(m_plac,
        main = paste("Placebo:", st, "— tank_id + cell×year FE, ref=-2"),
        xlab = "Years Relative to Reform",
        ylab = "Placebo Treatment Effect",
        col = "black", pt.join = TRUE, ci.lwd = 1.5,
        ylim = c(-0.02, 0.02))
 
  cat(sprintf("\nPlacebo %s: pooled coef = %.4f (p = %.3f)\n",
      st,
      coef(feols(closure_event ~ placebo_treat * as.integer(panel_year >= 1999L) |
                   tank_panel_id + panel_year^make_model_tank,
                 data = analysis_tanks[texas_treated == 0], cluster = ~state))[1],
      NA))  # p-value extraction left for manual inspection
 
  analysis_tanks[, placebo_treat := NULL]
}
 
 
#### S10: Presentation Summary ####
 
cat("\n========================================\n")
cat("S10: PRESENTATION SUMMARY\n")
cat("========================================\n\n")
 
cat("============================================================\n")
cat("HEADLINE RESULTS: Tank-Level Closure\n")
cat("============================================================\n\n")
 
cat(sprintf("Sample: %s tank-years at %s SW-exposed facilities\n",
    fmt_n(nrow(analysis_tanks)),
    fmt_n(uniqueN(analysis_tanks$panel_id))))
cat(sprintf("        TX: %s tanks | CTL: %s tanks\n",
    fmt_n(analysis_tanks[texas_treated == 1, uniqueN(tank_panel_id)]),
    fmt_n(analysis_tanks[texas_treated == 0, uniqueN(tank_panel_id)])))
 
cat(sprintf("\nControl means:\n"))
cat(sprintf("  Pre-reform:    %.1f per 1,000 tank-years\n", pre_ctrl_mean * 1000))
cat(sprintf("  Post (short):  %.1f per 1,000 tank-years\n", post_ctrl_short * 1000))
cat(sprintf("  Post (long):   %.1f per 1,000 tank-years\n", post_ctrl_long * 1000))
 
cat(sprintf("\nOLS LPM (tank_id + cell×year FE, clustered by state):\n"))
cat(sprintf("  Pooled:    %.4f (SE=%.4f)\n", coef(m_t2), se(m_t2)))
cat(sprintf("  Short-run: %.4f (SE=%.4f) *\n", coef(m_t3)["did_short"], se(m_t3)["did_short"]))
cat(sprintf("  Long-run:  %.4f (SE=%.4f)\n", coef(m_t3)["did_long"], se(m_t3)["did_long"]))
 
cox_s <- summary(m_cox_split)$coefficients
cat(sprintf("\nCox PH (strata(make_model_tank), calendar time, clustered):\n"))
cat(sprintf("  Pooled:    HR=%.3f (robust SE=%.3f, p=%.3f)\n",
    summary(m_cox_pooled)$coefficients[1, "exp(coef)"],
    summary(m_cox_pooled)$coefficients[1, "robust se"],
    summary(m_cox_pooled)$coefficients[1, "Pr(>|z|)"]))
cat(sprintf("  Short-run: HR=%.3f (robust SE=%.3f, p=%.3f) *\n",
    cox_s["did_short", "exp(coef)"],
    cox_s["did_short", "robust se"],
    cox_s["did_short", "Pr(>|z|)"]))
cat(sprintf("  Long-run:  HR=%.3f (robust SE=%.3f, p=%.3f)\n",
    cox_s["did_long", "exp(coef)"],
    cox_s["did_long", "robust se"],
    cox_s["did_long", "Pr(>|z|)"]))
 
cat(sprintf("\nShort-run proportional effect: %.4f / %.4f = %.0f%% of control mean\n",
    coef(m_t3)["did_short"], post_ctrl_short,
    coef(m_t3)["did_short"] / post_ctrl_short * 100))


cat("========================================\n")
cat("S9: PRIMARY COX MODELS\n")
cat("========================================\n\n")
 
# ── panel_dt: full analysis panel ─────────────────────────────────────────────
panel_dt <- mm_tank_primary[!is.na(age_bin) & tstop > tstart & !is.na(did_term)]
 
log_step(sprintf("panel_dt (full): %s tank-years, %s unique tanks",
                 fmt_n(nrow(panel_dt)), fmt_n(uniqueN(panel_dt$tank_panel_id))))
log_step(sprintf("  Post-mandate (1990-1997): %s tank-years, %s tanks",
  fmt_n(panel_dt[is_post_mandate == 1L, .N]),
  fmt_n(panel_dt[is_post_mandate == 1L, uniqueN(tank_panel_id)])), 1)
log_step(sprintf("  Pre-mandate  (1985-1989): %s tank-years, %s tanks",
  fmt_n(panel_dt[is_pre_mandate == 1L, .N]),
  fmt_n(panel_dt[is_pre_mandate == 1L, uniqueN(tank_panel_id)])), 1)
 
 
 
# ══════════════════════════════════════════════════════════════════════════════
# S9-COARSEN: Progressive Cell Coarsening Diagnostic
# ══════════════════════════════════════════════════════════════════════════════
#
# PURPOSE: Test whether the SW pre-trend in the Cox event study is driven by
# overly granular cell stratification.  If the pre-trend shrinks as cells get
# coarser, the 4-dim cells are too fine for the data to support clean within-
# cell identification for single-walled tanks.
#
# The binned scatter (S8) shows clean parallel pre-trends at the aggregate
# level.  If the pre-trend only appears when conditioning on fine cells, it
# is likely noise amplified by thin TX-vs-control overlap in small cells,
# not a real differential trend in tank closure behavior.
#
# Strata tested (from finest to coarsest):
#   5-dim : wall × fuel × capacity × cohort   (= make_model_tank, the primary)
#   3-dim : wall × fuel × cohort              (drop capacity)
#   2-dim : wall × cohort                     (drop capacity + fuel)
#   1-dim : wall only                         (all SW tanks in one stratum)
#   none  : no stratification, year dummies only (≈ facility-level)
#
# ══════════════════════════════════════════════════════════════════════════════
 cat("\n── S9-COARSEN: Progressive Cell Coarsening & Fleet Diagnostic ──\n\n")

# ---- Build coarsened cell variables on exact_split_df ----
if (!"make_model_3dim" %in% names(exact_split_df))
  exact_split_df[, make_model_3dim := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "_")]

exact_split_df[, make_model_2dim := paste(mm_wall, mm_install_cohort, sep = "_")]
exact_split_df[, make_model_1dim := mm_wall]
exact_split_df[, strata_none := "all"]

log_step("Cell variables created on exact_split_df:")
log_step(sprintf("  make_model_tank  (4-dim): %d unique",
  exact_split_df[mm_wall == "Single-Walled", uniqueN(make_model_tank)]), 1)
log_step(sprintf("  make_model_3dim  (3-dim): %d unique",
  exact_split_df[mm_wall == "Single-Walled", uniqueN(make_model_3dim)]), 1)
log_step(sprintf("  make_model_2dim  (2-dim): %d unique",
  exact_split_df[mm_wall == "Single-Walled", uniqueN(make_model_2dim)]), 1)
log_step(sprintf("  make_model_1dim  (1-dim): %d unique",
  exact_split_df[mm_wall == "Single-Walled", uniqueN(make_model_1dim)]), 1)
log_step(sprintf("  strata_none      (none) : 1"), 1)


# ══════════════════════════════════════════════════════════════════════════════
# PART 1: Cell overlap diagnostic (SW post-mandate)
# ══════════════════════════════════════════════════════════════════════════════

sw_cox_dt <- exact_split_df[mm_wall == "Single-Walled" & is_post_mandate == 1L]
log_step(sprintf("\nSW post-mandate Cox sample: %s rows, %s tanks, %s events",
  fmt_n(nrow(sw_cox_dt)),
  fmt_n(uniqueN(sw_cox_dt$tank_panel_id)),
  fmt_n(sum(sw_cox_dt$failure))))

log_step("\nCell overlap (SW post-mandate, pre-reform years only):")
sw_pre <- sw_cox_dt[reform_ep == 1L]

for (cell_info in list(
  list(var = "make_model_tank",  label = "4-dim (wall×fuel×cap×cohort)"),
  list(var = "make_model_3dim",  label = "3-dim (wall×fuel×cohort)"),
  list(var = "make_model_2dim",  label = "2-dim (wall×cohort)"),
  list(var = "make_model_1dim",  label = "1-dim (wall only)")
)) {
  cell_ov <- sw_pre[, .(
    n_tx  = sum(texas_treated == 1L),
    n_ctl = sum(texas_treated == 0L)
  ), by = eval(cell_info$var)]
  n_cells    <- nrow(cell_ov)
  n_both     <- cell_ov[n_tx > 0 & n_ctl > 0, .N]
  n_tx_only  <- cell_ov[n_tx > 0 & n_ctl == 0, .N]
  n_ctl_only <- cell_ov[n_tx == 0 & n_ctl > 0, .N]
  pct_id     <- round(100 * n_both / n_cells, 1)
  med_n      <- cell_ov[n_tx > 0 & n_ctl > 0, median(n_tx + n_ctl)]

  log_step(sprintf("  %-35s %4d cells | %4d identified (%5.1f%%) | %4d TX-only | %4d CTL-only | median n = %s",
    cell_info$label, n_cells, n_both, pct_id, n_tx_only, n_ctl_only,
    ifelse(is.na(med_n), "—", fmt_n(med_n))), 1)
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 2: Facility fleet construction & closure correlation diagnostic
# ══════════════════════════════════════════════════════════════════════════════
#
# Instead of isolating SW tanks from their facilities, we identify facilities
# that have at least one SW tank (the "exposed" population) and take their
# ENTIRE fleet — SW + DW tanks together.  This preserves the facility as the
# decision unit and enables within-facility HTE (SW vs DW at the same facility).
#
# The closure correlation diagnostic checks whether owners close tanks
# selectively (SW only, DW only, or both) vs. closing everything at once.
# If closures are perfectly correlated, within-facility HTE has no power.

cat("\n── Facility Fleet Construction ──\n\n")

# Identify facilities with at least one post-mandate SW tank
sw_facility_ids <- exact_split_df[
  mm_wall == "Single-Walled" & is_post_mandate == 1L,
  unique(panel_id)
]
log_step(sprintf("Facilities with post-mandate SW tanks: %s", fmt_n(length(sw_facility_ids))))

# Take ALL post-mandate tanks at those facilities (SW + DW)
sw_facility_fleet <- exact_split_df[
  panel_id %in% sw_facility_ids &
  is_post_mandate == 1L
]

n_fleet_sw <- sw_facility_fleet[mm_wall == "Single-Walled", uniqueN(tank_panel_id)]
n_fleet_dw <- sw_facility_fleet[mm_wall == "Double-Walled", uniqueN(tank_panel_id)]
n_fleet_ot <- sw_facility_fleet[!mm_wall %in% c("Single-Walled", "Double-Walled"), uniqueN(tank_panel_id)]

log_step(sprintf("Fleet sample: %s rows, %s tanks (%s SW + %s DW + %s other), %s facilities",
  fmt_n(nrow(sw_facility_fleet)),
  fmt_n(uniqueN(sw_facility_fleet$tank_panel_id)),
  fmt_n(n_fleet_sw), fmt_n(n_fleet_dw), fmt_n(n_fleet_ot),
  fmt_n(uniqueN(sw_facility_fleet$panel_id))))
log_step(sprintf("  Events: %s total (%s SW, %s DW)",
  fmt_n(sum(sw_facility_fleet$failure)),
  fmt_n(sw_facility_fleet[mm_wall == "Single-Walled", sum(failure)]),
  fmt_n(sw_facility_fleet[mm_wall == "Double-Walled", sum(failure)])), 1)

# ---- Closure correlation diagnostic ----
cat("\n── Closure Correlation Diagnostic ──\n\n")
log_step("Among facilities with any post-reform closure, how are closures distributed?")

fac_closure_pattern <- sw_facility_fleet[, .(
  n_tanks      = uniqueN(tank_panel_id),
  n_sw         = uniqueN(tank_panel_id[mm_wall == "Single-Walled"]),
  n_dw         = uniqueN(tank_panel_id[mm_wall == "Double-Walled"]),
  n_closed     = uniqueN(tank_panel_id[failure == 1L]),
  n_closed_sw  = uniqueN(tank_panel_id[failure == 1L & mm_wall == "Single-Walled"]),
  n_closed_dw  = uniqueN(tank_panel_id[failure == 1L & mm_wall == "Double-Walled"]),
  any_closed   = as.integer(any(failure == 1L)),
  has_both_types = as.integer(
    any(mm_wall == "Single-Walled") & any(mm_wall == "Double-Walled"))
), by = panel_id]

# Overall closure patterns (all facilities with closures)
all_closers <- fac_closure_pattern[any_closed == 1L]
log_step(sprintf("\n  Facilities with any closure: %s / %s (%.1f%%)",
  fmt_n(nrow(all_closers)),
  fmt_n(nrow(fac_closure_pattern)),
  100 * nrow(all_closers) / nrow(fac_closure_pattern)))

if (nrow(all_closers) > 0) {
  log_step(sprintf("  Close ALL tanks:  %s (%.1f%%)",
    fmt_n(all_closers[n_closed == n_tanks, .N]),
    100 * all_closers[n_closed == n_tanks, .N] / nrow(all_closers)), 1)
  log_step(sprintf("  Close SOME tanks: %s (%.1f%%)",
    fmt_n(all_closers[n_closed < n_tanks, .N]),
    100 * all_closers[n_closed < n_tanks, .N] / nrow(all_closers)), 1)
}

# Among mixed-type facilities (have both SW + DW): the key HTE population
mixed_closers <- fac_closure_pattern[has_both_types == 1L & any_closed == 1L]
log_step(sprintf("\n  Mixed-type facilities (SW + DW) with closures: %s",
  fmt_n(nrow(mixed_closers))))

if (nrow(mixed_closers) > 0) {
  log_step(sprintf("  Close ALL tanks:     %s (%.1f%%)",
    fmt_n(mixed_closers[n_closed == n_tanks, .N]),
    100 * mixed_closers[n_closed == n_tanks, .N] / nrow(mixed_closers)), 1)
  log_step(sprintf("  Close SOME tanks:    %s (%.1f%%)",
    fmt_n(mixed_closers[n_closed < n_tanks, .N]),
    100 * mixed_closers[n_closed < n_tanks, .N] / nrow(mixed_closers)), 1)
  log_step(sprintf("  Close SW only:       %s (%.1f%%)",
    fmt_n(mixed_closers[n_closed_sw > 0 & n_closed_dw == 0, .N]),
    100 * mixed_closers[n_closed_sw > 0 & n_closed_dw == 0, .N] / nrow(mixed_closers)), 1)
  log_step(sprintf("  Close DW only:       %s (%.1f%%)",
    fmt_n(mixed_closers[n_closed_sw == 0 & n_closed_dw > 0, .N]),
    100 * mixed_closers[n_closed_sw == 0 & n_closed_dw > 0, .N] / nrow(mixed_closers)), 1)
  log_step(sprintf("  Close both types:    %s (%.1f%%)",
    fmt_n(mixed_closers[n_closed_sw > 0 & n_closed_dw > 0, .N]),
    100 * mixed_closers[n_closed_sw > 0 & n_closed_dw > 0, .N] / nrow(mixed_closers)), 1)
}

# Interpretation
cat("\n")
pct_all <- all_closers[n_closed == n_tanks, .N] / nrow(all_closers)
if (pct_all > 0.70) {
  log_step("DIAGNOSIS: >70% of closers shut down all tanks simultaneously.")
  log_step("  → Within-facility HTE (SW vs DW) has limited identifying variation.")
  log_step("  → Facility-level spec is the appropriate primary model.", 1)
} else if (nrow(mixed_closers) > 0 &&
           mixed_closers[n_closed_sw > 0 & n_closed_dw == 0, .N] / nrow(mixed_closers) > 0.30) {
  log_step("DIAGNOSIS: >30% of mixed facilities close SW only (not DW).")
  log_step("  → Selective within-facility closure decisions exist.")
  log_step("  → Within-facility HTE is well-identified.", 1)
} else {
  log_step("DIAGNOSIS: Mixed closure patterns. Within-facility HTE has some power.")
  log_step("  → Report both facility-level and fleet-level HTE results.", 1)
}

# Save diagnostic
fwrite(fac_closure_pattern,
       file.path(OUTPUT_TABLES, "Diag_FleetClosurePattern.csv"))


# ══════════════════════════════════════════════════════════════════════════════
# PART 3: Cox DiD at each granularity × sample
# ══════════════════════════════════════════════════════════════════════════════
#
# Three samples:
#   (a) SW post-mandate only (isolated SW tanks, clean but less power)
#   (b) SW all cohorts (includes mandate-contaminated, more power)
#   (c) Fleet: all tanks at SW-facilities, post-mandate (preserves facility unit)
#
# Five stratification levels each.

cat("\n── Cox DiD: Granularity × Sample ──\n\n")

sw_samples <- list(
  list(dt = exact_split_df[mm_wall == "Single-Walled" & is_post_mandate == 1L],
       label_prefix = "SW PostMandate",
       tag = "sw_postmandate"),
  list(dt = exact_split_df[mm_wall == "Single-Walled"],
       label_prefix = "SW All Cohorts",
       tag = "sw_allcohort"),
  list(dt = sw_facility_fleet,
       label_prefix = "Fleet (SW-facilities)",
       tag = "fleet")
)

coarsen_specs <- list(
  list(strata_var = "make_model_tank",  label = "4-dim"),
  list(strata_var = "make_model_3dim",  label = "3-dim"),
  list(strata_var = "make_model_2dim",  label = "2-dim"),
  list(strata_var = "make_model_1dim",  label = "1-dim"),
  list(strata_var = "strata_none",      label = "None")
)

coarsen_results <- list()

for (samp in sw_samples) {

  cat(sprintf("\n── %s: %s rows, %s tanks, %s events ──\n",
    samp$label_prefix,
    fmt_n(nrow(samp$dt)),
    fmt_n(uniqueN(samp$dt$tank_panel_id)),
    fmt_n(sum(samp$dt$failure))))

  for (spec in coarsen_specs) {

    result_key <- paste0(samp$tag, "_", spec$label)

    m <- tryCatch({
      if (spec$strata_var == "strata_none") {
        coxph(
          Surv(t_enter, t_exit, failure) ~ did_term + year_fac + strata(strata_none),
          data = samp$dt, cluster = samp$dt$state, ties = "efron")
      } else {
        coxph(
          as.formula(paste("Surv(t_enter, t_exit, failure) ~ did_term + strata(",
                            spec$strata_var, ")")),
          data = samp$dt, cluster = samp$dt$state, ties = "efron")
      }
    }, error = function(e) { log_step(sprintf("  %-25s FAILED: %s", spec$label, e$message), 1); NULL })

    if (!is.null(m)) {
      r <- extract_cox_row(m)
      coarsen_results[[result_key]] <- list(
        label = result_key, strata_var = spec$strata_var,
        sample = samp$label_prefix, model = m, row = r)
      log_step(sprintf("  %-12s HR = %.3f  log-HR = %+.4f  SE = %.4f  p = %.4f",
        spec$label, r$hr, r$coef, r$se, r$p), 1)
    }
  }
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 4: Fleet HTE — SW vs DW within SW-facilities
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Fleet HTE: SW vs DW at SW-Facilities (2-dim strata) ──\n\n")

sw_facility_fleet[, `:=`(
  is_sw       = as.integer(mm_wall == "Single-Walled"),
  did_term_sw = did_term * as.integer(mm_wall == "Single-Walled"),
  did_term_dw = did_term * as.integer(mm_wall == "Double-Walled")
)]

# Pooled: overall reform effect on tank closure at SW-facilities
m_fleet_pooled <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term + strata(make_model_2dim),
  data    = sw_facility_fleet,
  cluster = sw_facility_fleet$state,
  ties    = "efron"
)
r_fleet_pooled <- extract_cox_row(m_fleet_pooled)
log_step(sprintf("Fleet pooled (2-dim):  HR = %.3f  log-HR = %+.4f  SE = %.4f  p = %.4f  N = %s",
  r_fleet_pooled$hr, r_fleet_pooled$coef, r_fleet_pooled$se, r_fleet_pooled$p,
  fmt_n(r_fleet_pooled$n)), 1)

# HTE: separate SW vs DW treatment effects within the same facilities
m_fleet_hte <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term_sw + did_term_dw + strata(make_model_2dim),
  data    = sw_facility_fleet,
  cluster = sw_facility_fleet$state,
  ties    = "efron"
)

fleet_hte_s <- summary(m_fleet_hte)$coefficients
se_col <- intersect(c("robust se", "se(coef)"), colnames(fleet_hte_s))[1]

log_step(sprintf("Fleet HTE SW:  HR = %.3f  log-HR = %+.4f  SE = %.4f  p = %.4f",
  exp(fleet_hte_s["did_term_sw", "coef"]),
  fleet_hte_s["did_term_sw", "coef"],
  fleet_hte_s["did_term_sw", se_col],
  fleet_hte_s["did_term_sw", "Pr(>|z|)"]), 1)
log_step(sprintf("Fleet HTE DW:  HR = %.3f  log-HR = %+.4f  SE = %.4f  p = %.4f",
  exp(fleet_hte_s["did_term_dw", "coef"]),
  fleet_hte_s["did_term_dw", "coef"],
  fleet_hte_s["did_term_dw", se_col],
  fleet_hte_s["did_term_dw", "Pr(>|z|)"]), 1)

# Test: SW effect > DW effect?
sw_coef <- fleet_hte_s["did_term_sw", "coef"]
dw_coef <- fleet_hte_s["did_term_dw", "coef"]
cat("\n")
if (sw_coef > dw_coef) {
  log_step(sprintf("SW log-HR (%+.4f) > DW log-HR (%+.4f): pricing gradient confirmed",
    sw_coef, dw_coef))
  log_step("  → SW tanks close faster than DW tanks at the SAME facilities post-reform", 1)
} else {
  log_step(sprintf("SW log-HR (%+.4f) <= DW log-HR (%+.4f): no pricing gradient",
    sw_coef, dw_coef))
  log_step("  → Reform effect is facility-level exit, not selective SW closure", 1)
}

# Wald test: H0: beta_SW = beta_DW
hte_wald <- tryCatch({
  linearHypothesis(m_fleet_hte, "did_term_sw = did_term_dw", test = "Chisq")
}, error = function(e) NULL)

if (!is.null(hte_wald)) {
  wald_p <- hte_wald[2, "Pr(>Chisq)"]
  log_step(sprintf("Wald test (SW = DW): chi2 = %.2f, p = %.4f  %s",
    hte_wald[2, "Chisq"], wald_p,
    ifelse(wald_p < 0.10, "→ reject equality (differential effect)",
           "→ fail to reject (similar effect across wall types)")), 1)
}

# Clean up fleet HTE indicators
sw_facility_fleet[, c("is_sw", "did_term_sw", "did_term_dw") := NULL]


# ══════════════════════════════════════════════════════════════════════════════
# PART 5: Summary table
# ══════════════════════════════════════════════════════════════════════════════

coarsen_summary <- rbindlist(lapply(coarsen_results, function(x) {
  data.table(
    Sample         = x$sample,
    Stratification = gsub(".*_", "", x$label),
    HR      = round(x$row$hr, 4),
    log_HR  = round(x$row$coef, 5),
    SE      = round(x$row$se, 5),
    p       = round(x$row$p, 4),
    N       = x$row$n,
    Events  = x$row$ev
  )
}))

cat("\n── Cell Coarsening × Sample Summary ──\n\n")
print(coarsen_summary)

fwrite(coarsen_summary, file.path(OUTPUT_TABLES, "Table_CellCoarsening_Full.csv"))

# Fleet HTE summary
fleet_hte_summary <- data.table(
  Model = c("Fleet pooled (2-dim)", "Fleet HTE: SW", "Fleet HTE: DW"),
  HR = round(c(r_fleet_pooled$hr,
               exp(fleet_hte_s["did_term_sw", "coef"]),
               exp(fleet_hte_s["did_term_dw", "coef"])), 4),
  log_HR = round(c(r_fleet_pooled$coef,
                    fleet_hte_s["did_term_sw", "coef"],
                    fleet_hte_s["did_term_dw", "coef"]), 5),
  SE = round(c(r_fleet_pooled$se,
               fleet_hte_s["did_term_sw", se_col],
               fleet_hte_s["did_term_dw", se_col]), 5),
  p = round(c(r_fleet_pooled$p,
              fleet_hte_s["did_term_sw", "Pr(>|z|)"],
              fleet_hte_s["did_term_dw", "Pr(>|z|)"]), 4)
)

cat("\n── Fleet HTE Summary ──\n\n")
print(fleet_hte_summary)

fwrite(fleet_hte_summary, file.path(OUTPUT_TABLES, "Table_FleetHTE_Summary.csv"))
log_step("\n  Coarsening & fleet diagnostics saved.", 1)


# ---- Run Cox EVENT STUDIES at each granularity ----
log_step("\nCox event studies by cell granularity (SW post-mandate):")
log_step("  (Check whether pre-trend shrinks as cells coarsen)\n")
 
coarsen_es_results <- list()
 
for (spec in coarsen_specs) {
 
  label_clean <- gsub("[^A-Za-z0-9]", "", gsub(" ", "_", spec$label))
 
  if (spec$strata_var == "strata_none") {
    # For no-strata: can't use run_cox_event_study directly because it
    # builds strata(strata_none) which is fine, but we also want year_fac.
    # Workaround: temporarily set strata to strata_none and accept that
    # the baseline hazard is flat (year_fac would need to be added to the
    # modular function). For now, just run 1-dim as the coarsest usable spec.
    log_step(sprintf("  %-25s (skipping ES — use 1-dim as coarsest)", spec$label), 1)
    next
  }
 
  res_es <- tryCatch(
    run_cox_event_study(
      sw_cox_dt,
      strata_var = spec$strata_var,
      pool_pre   = -4L,
      pool_post  = 15L,
      label      = paste0("SW-Coarsen-", label_clean),
      save       = TRUE
    ),
    error = function(e) {
      log_step(sprintf("  %-25s FAILED: %s", spec$label, e$message), 1)
      NULL
    }
  )
 
  if (!is.null(res_es)) {
    coarsen_es_results[[spec$label]] <- res_es
 
    # Report pre-test
    pt <- res_es$pre_tests
    log_step(sprintf("  %-25s pre-test p = %.4f (full), %.4f (dense)  %s",
      spec$label,
      pt$p_all,
      ifelse(is.na(pt$p_dense), 1, pt$p_dense),
      ifelse(pt$p_all > 0.10, "trends OK", "PRE-TREND")), 1)
  }
}
 
# ---- Summary table ----
coarsen_summary <- rbindlist(lapply(coarsen_results, function(x) {
  data.table(
    Stratification = x$label,
    HR      = round(x$row$hr, 4),
    log_HR  = round(x$row$coef, 5),
    SE      = round(x$row$se, 5),
    p       = round(x$row$p, 4),
    N       = x$row$n,
    Events  = x$row$ev
  )
}))
 
# Add pre-test p-values from event studies where available
coarsen_summary[, pre_test_p := NA_real_]
for (i in seq_len(nrow(coarsen_summary))) {
  nm <- coarsen_summary$Stratification[i]
  if (nm %in% names(coarsen_es_results))
    coarsen_summary[i, pre_test_p := coarsen_es_results[[nm]]$pre_tests$p_all]
}
 
cat("\n── Cell Coarsening Summary (SW post-mandate) ──\n\n")
print(coarsen_summary)
 
# ---- Interpretation ----
finest_pretest  <- coarsen_summary[1, pre_test_p]
coarsest_pretest <- coarsen_summary[!is.na(pre_test_p), last(pre_test_p)]
 
cat("\n")
if (!is.na(finest_pretest) && !is.na(coarsest_pretest)) {
  if (finest_pretest < 0.10 && coarsest_pretest > 0.10) {
    log_step("DIAGNOSIS: Pre-trend present at fine stratification, absent at coarse.")
    log_step("  → The 4-dim cells are too granular for SW tanks.")
    log_step("  → Within-cell noise (thin overlap) generates a spurious pre-trend.")
    log_step("  → Use coarser stratification for SW-specific results.", 1)
  } else if (finest_pretest < 0.10 && coarsest_pretest < 0.10) {
    log_step("DIAGNOSIS: Pre-trend persists even at coarse stratification.")
    log_step("  → The pre-trend is real, not an artifact of cell granularity.")
    log_step("  → TX SW tanks were genuinely closing faster pre-reform.", 1)
  } else {
    log_step("DIAGNOSIS: No pre-trend at any granularity.")
    log_step("  → SW identification is clean across specifications.", 1)
  }
}
 
# ---- Save ----
fwrite(coarsen_summary, file.path(OUTPUT_TABLES, "Table_CellCoarsening_SW.csv"))
log_step("\n  Cell coarsening diagnostic saved.", 1)
 
 
# ══════════════════════════════════════════════════════════════════════════════
# S9-ABC: Three-Sample Cox + OLS Structure
# ══════════════════════════════════════════════════════════════════════════════
 
run_cox_ols_pair <- function(cox_dt, panel_dt, label,
                              cox_extra_vars = NULL, ols_extra_vars = NULL,
                              strata_var = "make_model_tank") {
  cat(sprintf("\n-- %s --\n", label))
  cox_rhs <- "did_term"
  if (!is.null(cox_extra_vars)) cox_rhs <- paste(c(cox_rhs, cox_extra_vars), collapse = " + ")
  cox_fml <- as.formula(paste("Surv(t_enter, t_exit, failure) ~", cox_rhs,
                               "+ strata(", strata_var, ")"))
  m_cox <- coxph(cox_fml, data = cox_dt, cluster = cox_dt$state, ties = "efron")
  r_cox <- extract_cox_row(m_cox)
  log_step(sprintf("  Cox:  HR = %.3f  log-HR = %+.4f  SE = %.4f  p = %.4f  N = %s  Events = %s",
    r_cox$hr, r_cox$coef, r_cox$se, r_cox$p, fmt_n(r_cox$n), fmt_n(r_cox$ev)), 1)
 
  cox_age_fml <- as.formula(paste("Surv(age_enter, age_exit, failure) ~", cox_rhs,
                                   "+ strata(", strata_var, ")"))
  m_cox_age <- coxph(cox_age_fml, data = cox_dt, cluster = cox_dt$state, ties = "efron")
  r_cox_age <- extract_cox_row(m_cox_age)
  log_step(sprintf("  Cox (age-axis):  HR = %.3f  log-HR = %+.4f  SE = %.4f  p = %.4f",
    r_cox_age$hr, r_cox_age$coef, r_cox_age$se, r_cox_age$p), 1)
 
  ols_rhs <- "did_term + age_bin"
  if (!is.null(ols_extra_vars)) ols_rhs <- paste(c("did_term", ols_extra_vars, "age_bin"), collapse = " + ")
  ols_fml <- as.formula(paste("closure_event ~", ols_rhs, "| tank_panel_id +", strata_var, "^panel_year"))
  m_ols <- feols(ols_fml, data = panel_dt, cluster = ~state)
  r_ols <- extract_panel_row(m_ols)
  log_step(sprintf("  OLS:  coef = %+.4f pp  SE = %.4f  p = %.4f  N = %s",
    r_ols$coef * 100, r_ols$se * 100, r_ols$p, fmt_n(r_ols$n)), 1)
 
  if (!is.null(ols_extra_vars) && "mandate_active" %in% ols_extra_vars) {
    ols_ct <- coeftable(m_ols)
    if ("mandate_active" %in% rownames(ols_ct)) {
      ma <- ols_ct["mandate_active", ]
      log_step(sprintf("  OLS mandate_active:  coef = %+.4f pp  SE = %.4f  p = %.4f",
        ma["Estimate"] * 100, ma["Std. Error"] * 100, ma["Pr(>|t|)"]), 1)
    }
    cox_s <- summary(m_cox)$coefficients
    if ("mandate_active" %in% rownames(cox_s))
      log_step(sprintf("  Cox mandate_active:  HR = %.3f  p = %.4f",
        exp(cox_s["mandate_active", "coef"]), cox_s["mandate_active", "Pr(>|z|)"]), 1)
  }
 
  list(label = label, m_cox = m_cox, m_cox_age = m_cox_age, m_ols = m_ols,
       r_cox = r_cox, r_cox_age = r_cox_age, r_ols = r_ols)
}
 
# (A) POST-MANDATE PRIMARY
log_step("(A) POST-MANDATE PRIMARY (1990-1997)")
res_A <- run_cox_ols_pair(
  cox_dt = exact_split_df[is_post_mandate == 1L],
  panel_dt = panel_dt[is_post_mandate == 1L],
  label = "Post-Mandate Primary (1990-1997)")
 
m_cox_primary <- res_A$m_cox; m_cox_age <- res_A$m_cox_age
m_ols <- res_A$m_ols; cox_prim <- res_A$r_cox; ols_row <- res_A$r_ols
 
# (B) EXPANDED FULL SAMPLE
log_step("\n(B) EXPANDED FULL SAMPLE (1985-1997) + mandate control")
res_B <- run_cox_ols_pair(
  cox_dt = exact_split_df, panel_dt = panel_dt,
  label = "Expanded Full Sample (1985-1997)",
  cox_extra_vars = "mandate_active", ols_extra_vars = "mandate_active")
 
# (C) PRE-MANDATE DIAGNOSTIC
log_step("\n(C) PRE-MANDATE DIAGNOSTIC (1985-1989)")
n_pre_cox <- exact_split_df[is_pre_mandate == 1L, .N]
n_pre_panel <- panel_dt[is_pre_mandate == 1L, .N]
res_C <- NULL
if (n_pre_cox > 100 && n_pre_panel > 100) {
  res_C <- run_cox_ols_pair(
    cox_dt = exact_split_df[is_pre_mandate == 1L],
    panel_dt = panel_dt[is_pre_mandate == 1L],
    label = "Pre-Mandate Diagnostic (1985-1989)",
    cox_extra_vars = "mandate_active", ols_extra_vars = "mandate_active")
} else {
  log_step(sprintf("  Pre-mandate sample too small. Skipping."), 1)
}
 
# ── Comparison table ──
build_comparison_row <- function(res, sample_label) {
  data.table(Sample = sample_label,
    Cox_logHR = round(res$r_cox$coef, 5), Cox_HR = round(res$r_cox$hr, 4),
    Cox_SE = round(res$r_cox$se, 5), Cox_p = round(res$r_cox$p, 4),
    Cox_N = res$r_cox$n, Cox_Events = res$r_cox$ev,
    OLS_coef_pp = round(res$r_ols$coef * 100, 4),
    OLS_SE_pp = round(res$r_ols$se * 100, 4), OLS_p = round(res$r_ols$p, 4),
    OLS_N = res$r_ols$n)
}
 
comparison_rows <- list(
  build_comparison_row(res_A, "A: Post-mandate (1990-1997)"),
  build_comparison_row(res_B, "B: Full sample (1985-1997) + mandate ctrl"))
if (!is.null(res_C))
  comparison_rows <- c(comparison_rows, list(build_comparison_row(res_C, "C: Pre-mandate (1985-1989)")))
 
s9_comparison <- rbindlist(comparison_rows)
log_step("\nThree-sample comparison:")
print(s9_comparison)
 
cox_drift <- abs(res_B$r_cox$coef - res_A$r_cox$coef) / max(abs(res_A$r_cox$coef), 1e-6)
ols_drift <- abs(res_B$r_ols$coef - res_A$r_ols$coef) / max(abs(res_A$r_ols$coef), 1e-6)
if (cox_drift > 0.20 || ols_drift > 0.20) {
  warning(sprintf("Sample expansion shifted estimates (Cox drift: %.1f%%, OLS drift: %.1f%%).",
                  cox_drift * 100, ols_drift * 100))
} else {
  log_step(sprintf("\n  Estimates stable (Cox drift: %.1f%%, OLS drift: %.1f%%)",
                   cox_drift * 100, ols_drift * 100), 1)
}
 
fwrite(s9_comparison, file.path(OUTPUT_TABLES, "Table_S9_ThreeSample_Comparison.csv"))
saveRDS(res_A, file.path(ANALYSIS_DIR, "s9_res_A_postmandate.rds"))
saveRDS(res_B, file.path(ANALYSIS_DIR, "s9_res_B_expanded.rds"))
if (!is.null(res_C)) saveRDS(res_C, file.path(ANALYSIS_DIR, "s9_res_C_premandate.rds"))
 
log_step("\nS9 Primary model summary (headline = Sample A):")
log_step(sprintf("  Cox (calendar)  HR = %.3f%s  [%.3f, %.3f]",
  cox_prim$hr, stars_p(cox_prim$p),
  exp(cox_prim$coef - 1.96 * cox_prim$se), exp(cox_prim$coef + 1.96 * cox_prim$se)), 1)
log_step(sprintf("  OLS LPM         coef = %+.4f pp%s  SE = %.4f pp",
  ols_row$coef * 100, stars_p(ols_row$p), ols_row$se * 100), 1)
cat("\n")
#### S10: Cox Event Study (Exact-Date Intervals, Pooled Tails) ####

cat("========================================\n")
cat("S10: COX EVENT STUDY (EXACT-DATE INTERVALS)\n")
cat("========================================\n\n")

source(here("Code", "Helpers", "S10_Cox_EventStudy_Modular.R"))

# Primary spec (full sample, post-mandate)
res_es_primary <- run_cox_event_study(
  exact_split_df[is_post_mandate == 1L], label = "Primary-PostMandate")

# Reference year sensitivity
res_es_ref2 <- run_cox_event_study(
  exact_split_df[is_post_mandate == 1L], ref_year = -2L, label = "RefYear-2")

# Tighter pooling
res_es_tight <- run_cox_event_study(
  exact_split_df[is_post_mandate == 1L], pool_pre = -4L, pool_post = 10L,
  label = "TightPool")

# Single-walled subsample
res_es_sw <- run_cox_event_study(
  exact_split_df[mm_wall == "Single-Walled" & is_post_mandate == 1L],
  label = "SingleWalled-PostMandate")

# SW late cohorts (cleanest SW spec)
res_es_sw_late <- run_cox_event_study(
  exact_split_df[mm_wall == "Single-Walled" & mm_install_cohort %in% as.character(1993:1997)],
  pool_pre = -4L, pool_post = 15L, label = "SW-Cohort-1993-1997")

# 3-dim cell robustness
exact_split_df[, make_model_3dim := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "_")]
res_es_3d <- run_cox_event_study(
  exact_split_df[is_post_mandate == 1L],
  strata_var = "make_model_3dim", label = "3dim-NoCapacity")

# Log-HR scale
res_es_loghr <- run_cox_event_study(
  exact_split_df[is_post_mandate == 1L], y_metric = "log_hr", label = "Primary-LogHR")

cat("\n")


#### S11: OLS LPM ####

cat("========================================\n")
cat("S11: OLS LPM\n")
cat("========================================\n\n")

OLS_REF_YEAR  <- -1L
OLS_POOL_PRE  <- -8L
OLS_POOL_POST <- 15L
OLS_Y_CAP     <-  5
OLS_Y_FLOOR   <- -5
OLS_PLOT_W    <- 13
OLS_PLOT_H    <-  6
WALL_COHORT_MIN <- 1990L

panel_full <- mm_tank_primary[tstop > tstart & !is.na(did_term)]
panel_sw <- mm_tank_primary[tstop > tstart & !is.na(did_term) &
  mm_wall == "Single-Walled" & as.integer(mm_install_cohort) >= WALL_COHORT_MIN]
panel_dw <- mm_tank_primary[tstop > tstart & !is.na(did_term) &
  mm_wall == "Double-Walled" & as.integer(mm_install_cohort) >= WALL_COHORT_MIN]

log_step("Sample sizes:")
log_step(sprintf("  Full      : %s tank-years, %s tanks", fmt_n(nrow(panel_full)), fmt_n(uniqueN(panel_full$tank_panel_id))), 1)
log_step(sprintf("  SW (>=%d) : %s tank-years, %s tanks", WALL_COHORT_MIN, fmt_n(nrow(panel_sw)), fmt_n(uniqueN(panel_sw$tank_panel_id))), 1)
log_step(sprintf("  DW (>=%d) : %s tank-years, %s tanks", WALL_COHORT_MIN, fmt_n(nrow(panel_dw)), fmt_n(uniqueN(panel_dw$tank_panel_id))), 1)

run_did <- function(dt, label) {
  m <- feols(closure_event ~ did_term | tank_panel_id + make_model_tank^panel_year,
             data = dt, cluster = ~state)
  r <- extract_panel_row(m)
  log_step(sprintf("  %-20s coef = %+.4f pp  SE = %.4f  p = %.4f  N = %s",
                   label, r$coef * 100, r$se * 100, r$p, fmt_n(r$n)), 1)
  list(model = m, row = r)
}

log_step("DiD estimates:")
did_full <- run_did(panel_full, "Full sample")
did_sw   <- run_did(panel_sw,   "Single-Walled")
did_dw   <- run_did(panel_dw,   "Double-Walled")

log_step("\nEvent studies:")
log_step("  Full sample...")
es_full  <- run_ols_es(panel_full); pre_full <- ols_pre_tests(es_full)
p_full   <- plot_ols_es(es_full, pre_full, subtitle = "Full primary sample")

log_step("  Single-walled...")
es_sw  <- run_ols_es(panel_sw); pre_sw <- ols_pre_tests(es_sw)
p_sw   <- plot_ols_es(es_sw, pre_sw, subtitle = sprintf("Single-walled, cohort >= %d", WALL_COHORT_MIN))

log_step("  Double-walled...")
es_dw  <- run_ols_es(panel_dw); pre_dw <- ols_pre_tests(es_dw)
p_dw   <- plot_ols_es(es_dw, pre_dw, subtitle = sprintf("Double-walled, cohort >= %d", WALL_COHORT_MIN))

es_specs <- list(
  list(plot = p_full, coefs = es_full$coefs, stem = "Full"),
  list(plot = p_sw,   coefs = es_sw$coefs,   stem = "SingleWall"),
  list(plot = p_dw,   coefs = es_dw$coefs,   stem = "DoubleWall"))
for (s in es_specs) {
  ggsave(file.path(OUTPUT_FIGURES, sprintf("Figure_OLS_ES_%s.png", s$stem)),
         s$plot, width = OLS_PLOT_W, height = OLS_PLOT_H, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, sprintf("Figure_OLS_ES_%s.pdf", s$stem)),
         s$plot, width = OLS_PLOT_W, height = OLS_PLOT_H, device = cairo_pdf)
  fwrite(s$coefs, file.path(OUTPUT_TABLES, sprintf("Table_OLS_ES_%s.csv", s$stem)))
  log_step(sprintf("  Saved: %s", s$stem), 1)
}

did_comparison <- rbindlist(list(
  data.table(spec = "Cox - primary (calendar)", metric = "HR",
    estimate = cox_prim$coef, se = cox_prim$se, p = cox_prim$p,
    hr_or_pp = cox_prim$hr, n_obs = cox_prim$n, n_events = cox_prim$ev),
  data.table(spec = "OLS - full sample", metric = "pp",
    estimate = did_full$row$coef * 100, se = did_full$row$se * 100,
    p = did_full$row$p, hr_or_pp = did_full$row$coef * 100,
    n_obs = did_full$row$n, n_events = NA_integer_),
  data.table(spec = sprintf("OLS - single-wall (>=%d)", WALL_COHORT_MIN), metric = "pp",
    estimate = did_sw$row$coef * 100, se = did_sw$row$se * 100,
    p = did_sw$row$p, hr_or_pp = did_sw$row$coef * 100,
    n_obs = did_sw$row$n, n_events = NA_integer_),
  data.table(spec = sprintf("OLS - double-wall (>=%d)", WALL_COHORT_MIN), metric = "pp",
    estimate = did_dw$row$coef * 100, se = did_dw$row$se * 100,
    p = did_dw$row$p, hr_or_pp = did_dw$row$coef * 100,
    n_obs = did_dw$row$n, n_events = NA_integer_)
), fill = TRUE)
did_comparison[, `:=`(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se,
  stars = fcase(p < 0.01, "***", p < 0.05, "**", p < 0.10, "*", default = ""))]
log_step("\nDiD comparison:")
print(did_comparison[, .(spec, metric, estimate = round(estimate, 4), se = round(se, 4), p = round(p, 3), stars, n_obs)])
fwrite(did_comparison, file.path(OUTPUT_TABLES, "Table_DiD_Comparison.csv"))
cat("\n")


#### S12: Duration Model Comparison Table ####

cat("========================================\n")
cat("S12: DURATION MODEL COMPARISON TABLE\n")
cat("========================================\n\n")

# ---- CHANGED: use cox_prim and ols_row directly (not stale r1/r4) ----
r1 <- cox_prim
r4 <- ols_row
n_events_panel <- fmt_n(sum(panel_dt$closure_event, na.rm = TRUE))

write_tex(c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{Effect of Experience Rating on Tank Closure}",
  "\\label{tbl:duration_models}", "\\begin{tabular}{lcc}", "\\toprule",
  " & (1) & (2) \\\\",
  " & \\textit{Primary: Cox} & \\textit{Reference: OLS LPM} \\\\",
  "\\midrule",
  sprintf("Hazard ratio (Texas $\\times$ Post) & %s & --- \\\\", fmt_hr(r1$hr, r1$p)),
  sprintf("95\\%% CI & %s & \\\\",
    fmt_ci(exp(r1$coef - 1.96 * r1$se), exp(r1$coef + 1.96 * r1$se))),
  "\\addlinespace",
  sprintf("Log-hazard / OLS coefficient & %s & %s \\\\", fmt_est(r1$coef, r1$p), fmt_est(r4$coef, r4$p)),
  sprintf("(SE) & %s & %s \\\\", fmt_se(r1$se), fmt_se(r4$se)),
  "\\midrule",
  "Estimand         & Log-hazard ratio    & Pr(closure) \\\\",
  "Time axis        & Calendar days       & Calendar year \\\\",
  "Unit FE          & Strata              & Tank FE \\\\",
  "\\midrule",
  sprintf("Observations      & %s & %s \\\\", fmt_n(r1$n), fmt_n(r4$n)),
  sprintf("Events (closures) & %s & %s \\\\", fmt_n(r1$ev), n_events_panel),
  "\\bottomrule",
  "\\end{tabular}", "\\end{table}"
), "Table_Duration_Models.tex")
cat("\n")


#### S13: HTE by Wall Type ####

cat("========================================\n")
cat("S13: HTE BY WALL TYPE\n")
cat("========================================\n\n")

log_step("HTE by wall type - OLS spec...")
m_hte_wall_ols <- feols(
  closure_event ~ did_term:mm_wall + age_bin | tank_panel_id + make_model_tank^panel_year,
  data = panel_dt, cluster = ~state)

exact_split_df[, `:=`(
  did_term_sw = did_term * as.integer(mm_wall == "Single-Walled"),
  did_term_dw = did_term * as.integer(mm_wall == "Double-Walled"))]

log_step("HTE by wall type - Cox spec...")
m_hte_wall_cox <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term_sw + did_term_dw + strata(make_model_tank),
  data = exact_split_df, cluster = exact_split_df$state, ties = "efron")

wall_cox_s <- summary(m_hte_wall_cox)$coefficients
log_step(sprintf("  SW HR = %.3f (p=%.4f)  DW HR = %.3f (p=%.4f)",
  exp(wall_cox_s["did_term_sw", "coef"]), wall_cox_s["did_term_sw", "Pr(>|z|)"],
  exp(wall_cox_s["did_term_dw", "coef"]), wall_cox_s["did_term_dw", "Pr(>|z|)"]), 1)

fwrite(as.data.table(coeftable(m_hte_wall_ols), keep.rownames = "term"),
       file.path(OUTPUT_TABLES, "Table_HTE_WallType_OLS.csv"))
fwrite(as.data.table(wall_cox_s, keep.rownames = "term"),
       file.path(OUTPUT_TABLES, "Table_HTE_WallType_Cox.csv"))
exact_split_df[, c("did_term_sw", "did_term_dw") := NULL]
cat("\n")


#### S14: HTE by Install Cohort ####

cat("========================================\n")
cat("S14: HTE BY INSTALL COHORT\n")
cat("========================================\n\n")

log_step("HTE by install cohort - OLS spec...")
m_hte_cohort_ols <- feols(
  closure_event ~ did_term:mm_install_cohort + age_bin | tank_panel_id + make_model_tank^panel_year,
  data = panel_dt, cluster = ~state)

# ---- CHANGED: ALL_YEARS instead of PRIMARY_YEARS ----
for (coh in ALL_YEARS) {
  safe_coh <- gsub("-", "_", coh)
  exact_split_df[, paste0("did_term_c", safe_coh) := did_term * as.integer(mm_install_cohort == coh)]
}
coh_did_vars <- grep("^did_term_c", names(exact_split_df), value = TRUE)

log_step("HTE by install cohort - Cox spec...")
cox_cohort_fml <- as.formula(paste("Surv(t_enter, t_exit, failure) ~",
  paste(coh_did_vars, collapse = " + "), "+ strata(make_model_tank)"))
m_hte_cohort_cox <- coxph(cox_cohort_fml, data = exact_split_df,
                            cluster = exact_split_df$state, ties = "efron")

coh_cox_s <- summary(m_hte_cohort_cox)$coefficients
cohort_forest <- data.table(
  cohort = ALL_YEARS, var = coh_did_vars,
  hr = exp(coh_cox_s[coh_did_vars, "coef"]),
  coef = coh_cox_s[coh_did_vars, "coef"],
  se = coh_cox_s[coh_did_vars, "se(coef)"],
  p = coh_cox_s[coh_did_vars, "Pr(>|z|)"])
cohort_forest[, `:=`(ci_lo = exp(coef - 1.96 * se), ci_hi = exp(coef + 1.96 * se))]
cohort_forest[, cohort := factor(cohort, levels = rev(ALL_YEARS))]

p_cohort_forest <- ggplot(cohort_forest,
  aes(x = hr, y = cohort, xmin = ci_lo, xmax = ci_hi)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(size = 0.6, linewidth = 0.7, color = COL_TX) +
  labs(x = "Hazard Ratio (Texas x Post)", y = "Install Cohort") +
  theme_pub() + theme(panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3))

ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_CohortForest.png"),
       p_cohort_forest, width = 7, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_CohortForest.pdf"),
       p_cohort_forest, width = 7, height = 5, device = cairo_pdf)
fwrite(cohort_forest, file.path(OUTPUT_TABLES, "Table_HTE_Cohort_Cox.csv"))
fwrite(as.data.table(coeftable(m_hte_cohort_ols), keep.rownames = "term"),
       file.path(OUTPUT_TABLES, "Table_HTE_Cohort_OLS.csv"))
exact_split_df[, (coh_did_vars) := NULL]
cat("\n")


#### S15: Age-at-Closure Analysis ####

cat("========================================\n")
cat("S15: AGE-AT-CLOSURE ANALYSIS\n")
cat("========================================\n\n")

closed_exact <- exact_base[failure == 1L, .(
  tank_panel_id, state, texas_treated, mm_install_cohort, mm_wall,
  age_at_closure = as.numeric(t_exit - t_enter) / 365.25,
  period = fifelse(t_exit >= REFORM_DAYS, "Post-reform", "Pre-reform"))]
closed_exact[, group := factor(
  paste(fifelse(texas_treated == 1L, "Texas", "Control States"), period),
  levels = c("Control States Pre-reform", "Control States Post-reform",
             "Texas Pre-reform", "Texas Post-reform"))]

summary_acl <- closed_exact[, .(n = .N, mean = round(mean(age_at_closure), 2),
  median = round(median(age_at_closure), 2), sd = round(sd(age_at_closure), 2)
), by = group][order(group)]
log_step("Age at closure summary:")
print(summary_acl)
fwrite(summary_acl, file.path(OUTPUT_TABLES, "Table_AgeAtClosure_Summary.csv"))

p_hist_acl <- ggplot(closed_exact, aes(x = age_at_closure, fill = group)) +
  geom_histogram(binwidth = 1, alpha = 0.85, color = "white", linewidth = 0.2) +
  facet_wrap(~group, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c(
    "Control States Pre-reform" = COL_CTRL, "Control States Post-reform" = "#56B4E9",
    "Texas Pre-reform" = COL_PRE, "Texas Post-reform" = COL_TX), guide = "none") +
  scale_x_continuous(breaks = seq(0, 35, 5)) +
  labs(x = "Age at Closure (years)", y = "Number of Closures") + theme_pub()
ggsave(file.path(OUTPUT_FIGURES, "Figure_AgeAtClosure_Histogram.png"),
       p_hist_acl, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_AgeAtClosure_Histogram.pdf"),
       p_hist_acl, width = 10, height = 6, device = cairo_pdf)
cat("\n")


#### S16: Kaplan-Meier Survival Curves ####

cat("========================================\n")
cat("S16: KAPLAN-MEIER SURVIVAL CURVES\n")
cat("========================================\n\n")

# ---- CHANGED: ALL_YEARS instead of PRIMARY_YEARS ----
km_base <- exact_base[, .(
  tank_panel_id, texas_treated, mm_install_cohort,
  time_to_event = as.numeric(t_exit - t_enter) / 365.25, event = failure)]
km_base[, state_group := fifelse(texas_treated == 1L, "Texas", "Control States")]
km_base[, km_group := factor(
  paste(state_group, mm_install_cohort),
  levels = c(paste("Control States", ALL_YEARS), paste("Texas", ALL_YEARS)))]

km_fit  <- survfit(Surv(time_to_event, event) ~ km_group, data = km_base)
km_tidy <- as.data.table(broom::tidy(km_fit))
km_tidy[, group := gsub("km_group=", "", strata)]
km_tidy[, group := factor(group, levels = levels(km_base$km_group))]
km_tidy[, state_part := fifelse(grepl("^Texas", group), "Texas", "Control States")]

p_km <- ggplot(km_tidy, aes(x = time, y = estimate, color = group)) +
  geom_step(linewidth = 0.7) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.07, color = NA) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 35, 5)) +
  facet_wrap(~state_part, ncol = 2) +
  labs(x = "Tank Age (years)", y = "Survival Probability") +
  theme_pub() + theme(legend.position = "right")

ggsave(file.path(OUTPUT_FIGURES, "Figure_KM_TankSurvival.png"),
       p_km, width = 10, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_KM_TankSurvival.pdf"),
       p_km, width = 10, height = 5.5, device = cairo_pdf)
cat("\n")


#### S17: Robustness Specifications ####

cat("========================================\n")
cat("S17: ROBUSTNESS SPECIFICATIONS\n")
cat("========================================\n\n")

# 17a: 3-dimension cell (drop capacity)
if (!"make_model_3dim" %in% names(exact_split_df))
  exact_split_df[, make_model_3dim := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "_")]
if (!"make_model_3dim" %in% names(panel_dt))
  panel_dt[, make_model_3dim := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "_")]

log_step("Robustness 17a: 3-dimension cell...")
m_cox_3dim <- coxph(Surv(t_enter, t_exit, failure) ~ did_term + strata(make_model_3dim),
  data = exact_split_df, cluster = exact_split_df$state, ties = "efron")
m_ols_3dim <- feols(closure_event ~ did_term + age_bin | tank_panel_id + make_model_3dim^panel_year,
  data = panel_dt, cluster = ~state)
r_3dim_cox <- extract_cox_row(m_cox_3dim)
r_3dim_ols <- extract_panel_row(m_ols_3dim)
log_step(sprintf("  Cox 3-dim HR = %.3f | Cox 4-dim HR = %.3f", r_3dim_cox$hr, cox_prim$hr), 1)

# 17b: Drop border states
BORDER_STATES <- c("OK", "AR", "LA", "NM")
exact_no_border <- exact_split_df[!state %in% BORDER_STATES]
panel_no_border <- panel_dt[!state %in% BORDER_STATES]

log_step("Robustness 17b: drop border states...")
m_cox_noborder <- coxph(Surv(t_enter, t_exit, failure) ~ did_term + strata(make_model_tank),
  data = exact_no_border, cluster = exact_no_border$state, ties = "efron")
m_ols_noborder <- feols(closure_event ~ did_term + age_bin | tank_panel_id + make_model_tank^panel_year,
  data = panel_no_border, cluster = ~state)
r_nb_cox <- extract_cox_row(m_cox_noborder)
r_nb_ols <- extract_panel_row(m_ols_noborder)
log_step(sprintf("  Cox no-border HR = %.3f (p=%.3f)", r_nb_cox$hr, r_nb_cox$p), 1)

# 17c: Expanded sample already covered by S9 Sample B.
# Report that result here for the robustness table.
r_exp_cox <- res_B$r_cox
r_exp_ols <- res_B$r_ols
log_step(sprintf("Robustness 17c: expanded sample (from S9-B) OLS coef = %.4f (vs primary %.4f)",
                 r_exp_ols$coef, ols_row$coef), 1)

rob_summary <- data.table(
  Specification = c(
    "Primary (Cox, 4-dim cell, post-mandate)",
    "Primary (OLS, 4-dim cell, post-mandate)",
    "Cox 3-dim cell (no capacity)",
    "OLS 3-dim cell (no capacity)",
    "Cox 4-dim, drop border states",
    "OLS 4-dim, drop border states",
    "Cox expanded 1985-1997 + mandate ctrl",
    "OLS expanded 1985-1997 + mandate ctrl"),
  Estimand = rep(c("Log-hazard ratio", "OLS coefficient"), 4),
  Estimate = round(c(cox_prim$coef, ols_row$coef,
    r_3dim_cox$coef, r_3dim_ols$coef,
    r_nb_cox$coef, r_nb_ols$coef,
    r_exp_cox$coef, r_exp_ols$coef), 5),
  HR = round(c(cox_prim$hr, NA, r_3dim_cox$hr, NA, r_nb_cox$hr, NA, r_exp_cox$hr, NA), 4),
  SE = round(c(cox_prim$se, ols_row$se,
    r_3dim_cox$se, r_3dim_ols$se,
    r_nb_cox$se, r_nb_ols$se,
    r_exp_cox$se, r_exp_ols$se), 5),
  p_value = round(c(cox_prim$p, ols_row$p,
    r_3dim_cox$p, r_3dim_ols$p,
    r_nb_cox$p, r_nb_ols$p,
    r_exp_cox$p, r_exp_ols$p), 4),
  N_obs = c(cox_prim$n, ols_row$n,
    r_3dim_cox$n, nobs(m_ols_3dim),
    r_nb_cox$n, nobs(m_ols_noborder),
    r_exp_cox$n, r_exp_ols$n))

fwrite(rob_summary, file.path(OUTPUT_TABLES, "Table_Robustness_Summary.csv"))
log_step(sprintf("\nRobustness table written: %d specifications", nrow(rob_summary)))
cat("\n")


#### S18: Diagnostic Export ####

cat("========================================\n")
cat("S18: DIAGNOSTIC EXPORT\n")
cat("========================================\n\n")

# Save primary model objects
saveRDS(m_cox_primary,    file.path(ANALYSIS_DIR, "tank_cox_primary.rds"))
saveRDS(m_cox_age,        file.path(ANALYSIS_DIR, "tank_cox_age_axis.rds"))
saveRDS(m_ols,            file.path(ANALYSIS_DIR, "tank_ols_reference.rds"))
saveRDS(m_hte_wall_cox,   file.path(ANALYSIS_DIR, "tank_cox_hte_wall.rds"))
saveRDS(m_hte_cohort_cox, file.path(ANALYSIS_DIR, "tank_cox_hte_cohort.rds"))

# ---- CHANGED: use correct variable names (no more r2/r3/m_cloglog) ----
cross_spec_summary <- data.table(
  Model = c("Cox primary (calendar)", "Cox primary (age axis)",
            "OLS LPM reference", "Cox 3-dim (robustness)",
            "Cox no-border (robustness)", "Cox expanded (robustness)"),
  Estimand = c("Log-hazard ratio (calendar time)", "Log-hazard ratio (tank age)",
    "ATT on annual closure probability", "Log-hazard ratio (no capacity dim)",
    "Log-hazard ratio (excl. border states)", "Log-hazard ratio (1985-1997 + mandate)"),
  Log_coef = round(c(cox_prim$coef, res_A$r_cox_age$coef, ols_row$coef,
                      r_3dim_cox$coef, r_nb_cox$coef, r_exp_cox$coef), 5),
  HR = round(c(cox_prim$hr, res_A$r_cox_age$hr, NA,
                r_3dim_cox$hr, r_nb_cox$hr, r_exp_cox$hr), 4),
  SE = round(c(cox_prim$se, res_A$r_cox_age$se, ols_row$se,
                r_3dim_cox$se, r_nb_cox$se, r_exp_cox$se), 5),
  p_value = round(c(cox_prim$p, res_A$r_cox_age$p, ols_row$p,
                     r_3dim_cox$p, r_nb_cox$p, r_exp_cox$p), 4),
  N_obs = c(cox_prim$n, res_A$r_cox_age$n, ols_row$n,
             r_3dim_cox$n, r_nb_cox$n, r_exp_cox$n),
  N_events = c(cox_prim$ev, res_A$r_cox_age$ev,
                sum(panel_dt$closure_event),
                r_3dim_cox$ev, r_nb_cox$ev, r_exp_cox$ev))

fwrite(cross_spec_summary, file.path(OUTPUT_TABLES, "Table_CrossSpec_Summary.csv"))

sample_summary <- data.table(
  Level = c("Master tanks loaded", "Study states (non-NA treatment)",
    "Tank-year panel (full)", "Primary sample tank-years", "Primary sample unique tanks",
    "  of which: Texas", "  of which: Control states",
    "Exact-date Cox (post-survSplit)", "  of which: events (closures)",
    "Make-model cells", "Identified cell-years"),
  N = c(nrow(master_tanks), nrow(study_tanks), nrow(tank_year_panel),
    n_ty, n_tanks, n_tx_tanks, n_ct_tanks,
    nrow(exact_split_df), sum(exact_split_df$failure), n_cells, n_cells_id))
fwrite(sample_summary, file.path(OUTPUT_TABLES, "Table_SampleSizes.csv"))

log_step("Saved model objects and summary tables.")
log_step(sprintf("Output tables: %s", OUTPUT_TABLES))
log_step(sprintf("Output figures: %s", OUTPUT_FIGURES))

cat("\n========================================\n")
cat("02b_DiD_Survival.R COMPLETE\n")
cat("========================================\n")
cat("\nCross-specification summary:\n")
print(cross_spec_summary[, .(Model, Log_coef, HR, SE, p_value, N_obs)])