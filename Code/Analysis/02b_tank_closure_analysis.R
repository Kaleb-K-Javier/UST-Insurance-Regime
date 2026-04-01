################################################################################
# 02b_DiD_Analysis.R
# Texas UST Insurance Reform — DiD Models, Event Studies, Sensitivity
#
# REQUIRES: 02b_Panel_Build.R must be run first.
#
# SECTIONS:
#   S1   Setup & Constants
#   S2   Load Datasets from CSV
#   S3   Plotting & Table Helpers
#   S4   Build Cox Split Dataset
#   S5   Table 1 — OLS Stepwise (matched sample)
#   S6   Table 2 — Cox Stepwise (matched sample)
#   S7   Table 3 — Triple Diff (DiD x wall x age)
#   S8   Event Study Models (pooled, cell FE, age splits, anticipation)
#   S9   Event Study Figures
#   S10  Raw Closure Rate Figures
#   S11  HonestDiD Sensitivity (Rambachan-Roth)
#   S12  Pre-Reform Mandate Spike Diagnostics
#   S13  Three-Sample Cox + OLS (post-mandate / full / pre-mandate)
#   S14  Duration Model Comparison Table
#   S15  HTE by Wall Type
#   S16  HTE by Install Cohort (vintage bin)
#   S17  Age-at-Closure Analysis
#   S18  Kaplan-Meier Survival Curves
#   S19  Robustness Specifications
#   S20  Diagnostic Export
################################################################################


#### S1 Setup & Constants ####

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(HonestDiD)
  library(ggplot2)
  library(broom)
  library(scales)
  library(modelsummary)
  library(tibble)
  library(car)
  library(here)
})

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
ANALYSIS_DIR   <- here("Data", "Analysis")

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# Study parameters — must match 02b_Panel_Build.R
POST_YEAR   <- 1999L
REFORM_DATE <- as.IDate("1998-12-22")
REFORM_DAYS <- as.numeric(as.Date("1998-12-22"))
STUDY_END   <- as.IDate("2020-12-31")

CONTROL_STATES <- c(
  "AR", "CO", "ID", "KS", "KY",
  "LA", "MA", "MD", "ME", "MN", "MO", "NC",
  "OH", "OK", "SD", "TN", "VA"
)
STUDY_STATES <- c("TX", CONTROL_STATES)

# Colors
COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"
COL_PRE  <- "#E69F00"
COL_BLUE <- "#2166ac"
COL_RED  <- "#d6604d"
COL_GROUP <- c("Texas" = COL_TX, "Control States" = COL_CTRL)

# fixest defaults
setFixest_etable(style.tex = style.tex("aer"), digits = 4,
                 se.below = TRUE, depvar = FALSE)
setFixest_etable(fixef.print = FALSE)


#### S2 Load Datasets from CSV ####

cat("\n========================================\n")
cat("S2: LOAD DATASETS\n")
cat("========================================\n\n")

panel_dt      <- fread(file.path(ANALYSIS_DIR, "panel_dt.csv"),
                       na.strings = c("", "NA"))
exact_base    <- fread(file.path(ANALYSIS_DIR, "exact_base.csv"),
                       na.strings = c("", "NA"))
matched_tanks <- fread(file.path(ANALYSIS_DIR, "matched_tanks.csv"),
                       na.strings = c("", "NA"))
matched_ids   <- fread(file.path(ANALYSIS_DIR, "matched_ids.csv"))$tank_panel_id

meta          <- fread(file.path(ANALYSIS_DIR, "panel_meta.csv"))
med_age       <- meta[key == "med_age",       value]
ctrl_mean_post <- meta[key == "ctrl_mean_post", value]
age_label     <- meta[key == "med_age",       label]
n_ty          <- meta[key == "n_ty",          value]
n_tanks       <- meta[key == "n_tanks",       value]
n_tx_tanks    <- meta[key == "n_tx_tanks",    value]
n_ct_tanks    <- meta[key == "n_ct_tanks",    value]
n_cells       <- meta[key == "n_cells",       value]

# mm_install_cohort is stored as character; ensure it stays that way after CSV load
panel_dt[,      mm_install_cohort := as.character(mm_install_cohort)]
exact_base[,    mm_install_cohort := as.character(mm_install_cohort)]
matched_tanks[, mm_install_cohort := as.character(mm_install_cohort)]

# Date columns
for (col in c("tank_installed_date", "tank_closed_date")) {
  panel_dt[,      (col) := as.IDate(get(col))]
  matched_tanks[, (col) := as.IDate(get(col))]
  exact_base[,    (col) := as.IDate(get(col))]
}

cat(sprintf("panel_dt:      %s rows | %s tanks\n",
  format(nrow(panel_dt),   big.mark = ","),
  format(uniqueN(panel_dt$tank_panel_id), big.mark = ",")))
cat(sprintf("exact_base:    %s rows | %s events\n",
  format(nrow(exact_base), big.mark = ","),
  format(sum(exact_base$failure), big.mark = ",")))
cat(sprintf("matched_tanks: %s rows | %s tanks\n",
  format(nrow(matched_tanks), big.mark = ","),
  format(uniqueN(matched_tanks$tank_panel_id), big.mark = ",")))
cat(sprintf("med_age: %.0f yr  |  ctrl_mean_post: %.4f\n\n", med_age, ctrl_mean_post))


#### S3 Plotting & Table Helpers ####

log_step <- function(msg, indent = 0L) cat(strrep("  ", indent), msg, "\n", sep = "")
fmt_n    <- function(n) format(n, big.mark = ",", scientific = FALSE)
fmt_est  <- function(x, p) sprintf("%.4f%s", x, stars_p(p))
fmt_se   <- function(x)    sprintf("(%.4f)", x)
fmt_hr   <- function(x, p) sprintf("%.3f%s", x, stars_p(p))
fmt_ci   <- function(lo, hi) sprintf("[%.3f, %.3f]", lo, hi)

stars_p <- function(p) {
  fcase(is.na(p), "", p < 0.01, "$^{***}$",
        p < 0.05, "$^{**}$", p < 0.10, "$^{*}$", default = "")
}

extract_cox_row <- function(m, term_name = "did_term") {
  s      <- summary(m)$coefficients
  idx    <- grep(term_name, rownames(s), fixed = TRUE)[1]
  stopifnot("Term not found in Cox model" = !is.na(idx))
  se_col <- intersect(c("robust se", "se(coef)"), colnames(s))[1]
  list(hr = s[idx, "exp(coef)"], coef = s[idx, "coef"],
       se  = s[idx, se_col],     p    = s[idx, "Pr(>|z|)"],
       n   = m$n,                ev   = m$nevent)
}

extract_panel_row <- function(m, term_name = "did_term") {
  ct  <- coeftable(m)
  idx <- grep(term_name, rownames(ct), fixed = TRUE)[1]
  stopifnot("Term not found in panel model" = !is.na(idx))
  list(coef = ct[idx, "Estimate"], se = ct[idx, "Std. Error"],
       p    = ct[idx, "Pr(>|t|)"], n  = nobs(m))
}

write_tex <- function(lines, filename) {
  writeLines(lines, file.path(OUTPUT_TABLES, filename))
  message(sprintf("  Written: %s", filename))
}

# load_tex_table — use this pattern inside QMD chunks (with tbl-cap) to load
# any .tex table saved by this script. Strips table wrappers and wraps in
# scalebox + minipage so Quarto owns the float and resizing works reliably.
# NEVER use \resizebox or \adjustbox — \textwidth = 0 inside Quarto floats.
# Usage in QMD:
#   ```{r}
#   #| results: asis
#   load_tex_table(here("Output", "Tables", "T1_ols_stepwise.tex"))
#   ```
load_tex_table <- function(path, scale = 0.85, minipage_w = 1.18) {
  lines <- readLines(path)
  lines <- lines[!grepl(
    paste0("^\\s*\\\\begin\\{table\\}|^\\s*\\\\end\\{table\\}",
           "|^\\s*\\\\centering|^\\s*\\\\begingroup",
           "|^\\s*\\\\endgroup|^\\s*\\\\par\\\\endgroup"),
    lines
  )]
  cat(sprintf("\\scalebox{%.2f}{\n", scale))
  cat(sprintf("\\begin{minipage}{%.2f\\linewidth}\n", minipage_w))
  cat("\\begingroup\n")
  cat(lines, sep = "\n")
  cat("\n\\endgroup\n")
  cat("\\end{minipage}\n")
  cat("}\n")
}

# Publication ggplot2 theme
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

save_gg <- function(p, stem, width = 7, height = 5) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(stem, ".pdf")),
         p, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(OUTPUT_FIGURES, paste0(stem, ".png")),
         p, width = width, height = height, dpi = 300, bg = "white")
  invisible(p)
}

# Event study coefficient extraction
# Works for fixest i() terms named "VAR_NAME::VALUE:texas_treated"
es_tidy <- function(model, ref = -2L, var_prefix = "rel_year_es") {
  df      <- as.data.table(tidy(model, conf.int = TRUE))
  pattern <- paste0(var_prefix, "::")
  df      <- df[grepl(pattern, term)]
  df[, year := as.integer(
    gsub(paste0(".*", var_prefix, "::([-0-9]+):texas_treated.*"), "\\1", term)
  )]
  ref_row <- data.table(
    term = paste0("ref_", ref), estimate = 0, std.error = 0,
    conf.low = 0, conf.high = 0, year = as.integer(ref)
  )
  df <- rbindlist(
    list(df[, .(term, estimate, std.error, conf.low, conf.high, year)], ref_row),
    fill = TRUE
  )
  setorder(df, year)
  df
}

# Event study plot — journal style, no title, no connecting line
es_ggplot <- function(model, point_col = "grey25", label = NULL,
                      ylim = c(-0.025, 0.025), ref = -2L,
                      var_prefix = "rel_year_es") {
  df      <- es_tidy(model, ref = ref, var_prefix = var_prefix)
  ref_row <- df[year == ref]
  est_row <- df[year != ref]

  ggplot(df, aes(x = year, y = estimate)) +
    geom_errorbar(data = est_row,
                  aes(ymin = conf.low, ymax = conf.high),
                  colour = point_col, width = 0.25, linewidth = 0.45) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    geom_point(data = est_row, colour = point_col, size = 1.8, shape = 16) +
    geom_point(data = ref_row, shape = 1, size = 2.2, colour = "grey40") +
    annotate("text", x = ref, y = ylim[1] * 0.82,
             label = sprintf("(%d)\nomitted", ref),
             size = 2.8, colour = "grey35", fontface = "italic",
             lineheight = 0.85) +
    { if (!is.null(label))
        annotate("text", x = -Inf, y = Inf, label = label,
                 hjust = -0.08, vjust = 1.4, size = 3, colour = "grey25")
      else list() } +
    scale_x_continuous(breaks = sort(unique(df$year))) +
    scale_y_continuous(limits = ylim) +
    labs(x = "Years Relative to 1999", y = "Effect on Annual Closure Probability") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(axis.line       = element_line(colour = "black", linewidth = 0.4),
          axis.ticks      = element_line(colour = "black", linewidth = 0.3),
          axis.text       = element_text(colour = "black"),
          panel.grid      = element_blank(),
          legend.position = "none",
          plot.title      = element_blank(),
          plot.margin     = margin(8, 10, 8, 8))
}


#### S4 Build Cox Split Dataset ####

cat("========================================\n")
cat("S4: BUILD COX SPLIT DATASET\n")
cat("========================================\n\n")

# Filter exact_base to matched IDs and survSplit at Jan 1, 1999.
# This gives a proper counting-process dataset where did_term is
# a time-varying covariate (0 before split, 1 after for TX tanks).
# Splitting at the OLS POST_YEAR (Jan 1, 1999) keeps Cox and OLS estimates
# on the same conceptual treatment onset for the stepwise tables.

cox_sample <- exact_base[tank_panel_id %in% matched_ids]
cox_sample[, above_median_age := as.integer(
  (1999L - install_yr_int) >= med_age
)]

cox_split_df <- survSplit(
  formula  = Surv(t_enter, t_exit, failure) ~ .,
  data     = as.data.frame(cox_sample),
  cut      = as.numeric(as.Date("1999-01-01")),
  episode  = "reform_ep"
)
setDT(cox_split_df)
cox_split_df <- cox_split_df[t_exit > t_enter]
cox_split_df[, did_term := texas_treated * as.integer(reform_ep == 2L)]

cat(sprintf("cox_split_df: %s rows | %s tanks | %s events\n\n",
  format(nrow(cox_split_df),                  big.mark = ","),
  format(uniqueN(cox_split_df$tank_panel_id), big.mark = ","),
  format(sum(cox_split_df$failure),           big.mark = ",")))


#### S5 Table 1 — OLS Stepwise ####

cat("========================================\n")
cat("S5: TABLE 1 — OLS STEPWISE\n")
cat("========================================\n\n")

# M1-M2: pooled, year FE vs cell x year FE
# M3-M4: DiD x above_median_age interaction
# M5-M6: age subsample split
# mandate_active included in all specs to absorb compliance-window pressure
# on pre-1989 cohorts (applies identically to TX and controls).
# Level effect of above_median_age is time-invariant -> absorbed by tank FE.

m1 <- feols(
  closure_event ~ did_term + mandate_active | tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

m2 <- feols(
  closure_event ~ did_term + mandate_active |
    tank_panel_id + panel_year^make_model_noage,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

m3 <- feols(
  closure_event ~ did_term + did_x_old + mandate_active |
    tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

m4 <- feols(
  closure_event ~ did_term + did_x_old + mandate_active |
    tank_panel_id + panel_year^make_model_noage,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

m5 <- feols(
  closure_event ~ did_term + mandate_active | tank_panel_id + panel_year,
  data    = matched_tanks[above_median_age == 0L],
  weights = ~cem_weight, cluster = ~state)

m6 <- feols(
  closure_event ~ did_term + mandate_active | tank_panel_id + panel_year,
  data    = matched_tanks[above_median_age == 1L],
  weights = ~cem_weight, cluster = ~state)

ols_dict <- c(
  "did_term"       = "DiD",
  "did_x_old"      = sprintf("DiD $\\times$ Old ($\\geq$%.0f yr)", med_age),
  "mandate_active" = "Mandate active"
)

etable(
  m1, m2, m3, m4, m5, m6,
  headers    = paste0("(", 1:6, ")"),
  dict       = ols_dict,
  fitstat    = ~ n + r2 + wr2,
  keep       = c("^DiD", "Mandate"),
  extralines = list(
    "Tank FE"                    = rep("\\checkmark", 6),
    "Year FE"                    = rep("\\checkmark", 6),
    "Cell $\\times$ Year FE"     = c("", "\\checkmark", "", "\\checkmark", "", ""),
    "Age subsample"              = c("All", "All", "All", "All",
                                     sprintf("$<$%.0f yr", med_age),
                                     sprintf("$\\geq$%.0f yr", med_age)),
    "Control Mean (post-reform)" = rep(sprintf("%.4f", ctrl_mean_post), 6)
  ),
  tex     = TRUE,
  file    = file.path(OUTPUT_TABLES, "T1_ols_stepwise.tex"),
  replace = TRUE,
  notes   = paste0(
    "Cluster-robust SEs by state. ",
    "mandate\\_active absorbs install-cohort-specific federal compliance windows. ",
    sprintf("Old $=$ above median age at reform ($\\geq$%.0f yr). ", med_age),
    "Level effect of age absorbed by tank FE. CEM weights applied throughout."
  )
)
cat("Saved: T1_ols_stepwise.tex\n\n")


#### S6 Table 2 — Cox Stepwise ####

cat("========================================\n")
cat("S6: TABLE 2 — COX STEPWISE\n")
cat("========================================\n\n")

# Strata on make_model_noage (not make_model_tank) so the age dimension
# is NOT absorbed into the baseline hazard — required to identify age HTE.
# mandate_active passed as a time-varying covariate via the survSplit data.

cox_split_df[, mandate_active := as.integer(
  !is.na(release_det_deadline_yr) & (
    (as.integer(format(as.Date(t_enter, origin = "1970-01-01"), "%Y")) >= 1989L &
     as.integer(format(as.Date(t_exit,  origin = "1970-01-01"), "%Y")) <= release_det_deadline_yr) |
    (as.integer(format(as.Date((t_enter + t_exit) / 2, origin = "1970-01-01"), "%Y")) %in% 1993L:1994L) |
    (as.integer(format(as.Date((t_enter + t_exit) / 2, origin = "1970-01-01"), "%Y")) %in% 1996L:1998L)
  )
)]

cox1 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term + mandate_active +
    strata(make_model_noage),
  data = cox_split_df, cluster = state)

cox2 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term + did_term:above_median_age +
    mandate_active + strata(make_model_noage),
  data = cox_split_df, cluster = state)

cox3 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term + mandate_active +
    strata(make_model_noage),
  data = cox_split_df[above_median_age == 0L], cluster = state)

cox4 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term + mandate_active +
    strata(make_model_noage),
  data = cox_split_df[above_median_age == 1L], cluster = state)

cox_coef_map <- c(
  "did_term"                  = "DiD",
  "did_term:above_median_age" = sprintf("DiD $\\times$ Old ($\\geq$%.0f yr)", med_age),
  "mandate_active"            = "Mandate active"
)

cox_add_rows <- tribble(
  ~term,                         ~`(1)`,        ~`(2)`,        ~`(3)`,                                   ~`(4)`,
  "Strata (make-model, no age)", "\\checkmark", "\\checkmark", "\\checkmark",                             "\\checkmark",
  "Age subsample",               "All",          "All",
     sprintf("$<$%.0f yr", med_age),  sprintf("$\\geq$%.0f yr", med_age)
)

msummary(
  list("(1)" = cox1, "(2)" = cox2, "(3)" = cox3, "(4)" = cox4),
  coef_map     = cox_coef_map,
  exponentiate = TRUE,
  statistic    = "({std.error})",
  stars        = c("*" = .1, "**" = .05, "***" = .01),
  escape       = FALSE,
  gof_map      = list(
    list(raw = "nobs",   clean = "Observations", fmt = scales::comma),
    list(raw = "nevent", clean = "Events",       fmt = scales::comma)
  ),
  add_rows = cox_add_rows,
  output   = file.path(OUTPUT_TABLES, "T2_cox_stepwise.tex"),
  notes    = paste0(
    "Cluster-robust SEs by state. Hazard ratios reported. ",
    "Spells split at January 1, 1999. ",
    "Strata on make-model excluding vintage so age variation is not absorbed. ",
    sprintf("Old $=$ above median age at reform ($\\geq$%.0f yr). ", med_age),
    "mandate\\_active absorbs vintage-cohort-specific compliance windows."
  )
)
cat("Saved: T2_cox_stepwise.tex\n\n")


#### S7 Table 3 — Triple Diff ####

cat("========================================\n")
cat("S7: TABLE 3 — TRIPLE DIFF\n")
cat("========================================\n\n")

# Level effects of single_wall and above_median_age are time-invariant
# and silently absorbed by tank FE — not a bug.

td0 <- feols(
  closure_event ~ did_term + mandate_active | tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

td1 <- feols(
  closure_event ~ did_term * single_wall * above_median_age + mandate_active |
    tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

etable(
  td0, td1,
  headers = c("(1)", "(2)"),
  fitstat = ~ n + r2,
  tex     = TRUE,
  file    = file.path(OUTPUT_TABLES, "T3_triple_diff.tex"),
  replace = TRUE,
  notes   = paste0(
    "Cluster-robust SEs by state. ",
    sprintf("Old $=$ above median age at reform ($\\geq$%.0f yr). ", med_age),
    "Level effects of Single-Walled and Old absorbed by tank FE. ",
    "CEM weights applied throughout."
  )
)
cat("Saved: T3_triple_diff.tex\n\n")


#### S8 Event Study Models ####

cat("========================================\n")
cat("S8: EVENT STUDY MODELS\n")
cat("========================================\n\n")

# All specs include mandate_active to absorb vintage-cohort compliance windows.
# rel_year_es is binned to [-8, 10] in the build script.

m_es_pooled <- feols(
  closure_event ~ i(rel_year_es, texas_treated, ref = -2L) + mandate_active |
    tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

m_es_cell <- feols(
  closure_event ~ i(rel_year_es, texas_treated, ref = -2L) + mandate_active |
    tank_panel_id + panel_year^make_model_noage,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

m_es_below_med <- feols(
  closure_event ~ i(rel_year_es, texas_treated, ref = -2L) + mandate_active |
    tank_panel_id + panel_year,
  data = matched_tanks[above_median_age == 0L],
  weights = ~cem_weight, cluster = ~state)

m_es_above_med <- feols(
  closure_event ~ i(rel_year_es, texas_treated, ref = -2L) + mandate_active |
    tank_panel_id + panel_year,
  data = matched_tanks[above_median_age == 1L],
  weights = ~cem_weight, cluster = ~state)

# Anticipation spec: rel_year_early bins the pre-reform period relative to 1998
# so that the 1998 coefficient captures anticipatory closure
m_es_antic <- feols(
  closure_event ~ i(rel_year_early, texas_treated, ref = -2L) + mandate_active |
    tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state)

cat("ES pooled (ref=-2):\n"); etable(m_es_pooled)
cat("\nES anticipation (ref=-2, treatment=1998):\n"); etable(m_es_antic)
cat("\n")


#### S9 Event Study Figures ####

cat("========================================\n")
cat("S9: EVENT STUDY FIGURES\n")
cat("========================================\n\n")

ES_YLIM <- c(-0.025, 0.025)

p_es1 <- es_ggplot(m_es_pooled,   point_col = "grey25",
  label = "Pooled (matched, mandate control)", ylim = ES_YLIM, ref = -2L)

p_es2 <- es_ggplot(m_es_cell,     point_col = "grey25",
  label = "Pooled + cell x year FE", ylim = ES_YLIM, ref = -2L)

p_es3 <- es_ggplot(m_es_below_med, point_col = COL_BLUE,
  label = sprintf("Below median age (< %.0f yr)", med_age),
  ylim = ES_YLIM, ref = -2L)

p_es4 <- es_ggplot(m_es_above_med, point_col = COL_RED,
  label = sprintf("Above median age (>= %.0f yr)", med_age),
  ylim = ES_YLIM, ref = -2L)

p_es5 <- es_ggplot(m_es_antic,    point_col = "grey25",
  label = "Anticipation spec (reform=1998)", ylim = ES_YLIM, ref = -2L,
  var_prefix = "rel_year_early")

save_gg(p_es1, "F1_es_pooled_matched")
save_gg(p_es2, "F2_es_pooled_cell_fe")
save_gg(p_es3, "F3_es_below_median")
save_gg(p_es4, "F4_es_above_median")
save_gg(p_es5, "F5_es_anticipation")
cat("Saved: F1-F5 (PDF + PNG)\n\n")


#### S10 Raw Closure Rate Figures ####

cat("========================================\n")
cat("S10: RAW CLOSURE RATE FIGURES\n")
cat("========================================\n\n")

ols_rates <- matched_tanks[, .(
  closure_rate = weighted.mean(closure_event, w = cem_weight, na.rm = TRUE)
), by = .(panel_year,
          group = fifelse(texas_treated == 1L, "Texas", "Control States"))]

cox_sample_rates <- exact_base[tank_panel_id %in% matched_ids]
cox_sample_rates[, plot_year := as.integer(
  format(as.Date(t_exit, origin = "1970-01-01"), "%Y"))]
cox_rates <- cox_sample_rates[plot_year %between% c(1985L, 2020L), .(
  closure_rate = mean(failure, na.rm = TRUE)
), by = .(plot_year,
          group = fifelse(texas_treated == 1L, "Texas", "Control States"))]

# Shared plot function
plot_raw_closure <- function(dt, x_var, reform_x, reform_label,
                              show_mandate_windows = FALSE) {
  ymax <- max(dt$closure_rate, na.rm = TRUE)
  p <- ggplot(dt, aes(x = .data[[x_var]], y = closure_rate,
                      colour = group, group = group)) +
    geom_vline(xintercept = reform_x, linetype = "dashed",
               colour = "grey35", linewidth = 0.55) +
    annotate("text", x = reform_x + 0.25, y = ymax * 0.97,
             label = reform_label, hjust = 0, size = 2.8, colour = "grey35") +
    geom_line(linewidth = 0.75) + geom_point(size = 1.8) +
    scale_colour_manual(values = COL_GROUP,
                        guide  = guide_legend(direction = "horizontal")) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_x_continuous(breaks = seq(1985, 2020, by = 5)) +
    labs(x = "Calendar Year", y = "Annual Tank Closure Rate", colour = NULL) +
    theme_pub() +
    theme(legend.position    = c(0.85, 0.92),
          legend.background  = element_blank(),
          panel.grid.major.x = element_blank())
  p
}

p_ols_rates <- plot_raw_closure(ols_rates, "panel_year", 1999, "Reform\n(Jan 1999)")
p_cox_rates <- plot_raw_closure(cox_rates, "plot_year",  1999, "Reform\n(Dec 1998)")

save_gg(p_ols_rates, "Figure_OLS_Sample_ClosureRates", width = 7, height = 4.5)
save_gg(p_cox_rates, "Figure_Cox_Sample_ClosureRates", width = 7, height = 4.5)
cat("Saved: Figure_OLS/Cox_Sample_ClosureRates\n\n")


#### S11 HonestDiD Sensitivity ####

cat("========================================\n")
cat("S11: HONESTDID SENSITIVITY\n")
cat("========================================\n\n")

b_full <- coef(m_es_pooled)
V_full <- vcov(m_es_pooled, type = "clustered")

es_idx <- grepl("rel_year_es", names(b_full))
b <- b_full[es_idx]
V <- V_full[es_idx, es_idx]

# Force positive-semidefinite (required with small cluster count + high-dim FE)
ev <- eigen(V, symmetric = TRUE)
ev$values <- pmax(ev$values, 0)
V <- ev$vectors %*% diag(ev$values) %*% t(ev$vectors)

cat("ES coefficients:\n"); print(names(b))

# Count pre/post periods from binned rel_year_es = [-8, -7, ..., -3, -1, 0, ..., 10]
# ref = -2 is omitted; pre = {-8,...,-3,-1}; post = {0,...,10}
rel_years_es <- as.integer(regmatches(names(b), regexpr("-?[0-9]+", names(b))))
n_pre  <- sum(rel_years_es < 0L)
n_post <- sum(rel_years_es >= 0L)
cat(sprintf("n_pre=%d  n_post=%d  total=%d  (expected %d)\n",
  n_pre, n_post, length(b), n_pre + n_post))
stopifnot(n_pre + n_post == length(b))

pre_coefs     <- b[order(rel_years_es[rel_years_es < 0L])]
max_pre_slope <- max(abs(diff(pre_coefs)))
cat(sprintf("Max pre-trend slope: %.5f\n", max_pre_slope))

sensitivity <- createSensitivityResults(
  betahat        = b,
  sigma          = V,
  numPrePeriods  = n_pre,
  numPostPeriods = n_post,
  alpha          = 0.05
)
sensitivity_dt <- as.data.table(sensitivity)

breakdown_M <- sensitivity_dt[sign(lb) != sign(ub), min(M, na.rm = TRUE)]
cat(sprintf("Breakdown M: %.5f\n", breakdown_M))

p_hd <- ggplot(sensitivity_dt, aes(x = M)) +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = COL_TX, alpha = 0.20) +
  geom_line(aes(y = lb), color = COL_TX, linewidth = 0.7) +
  geom_line(aes(y = ub), color = COL_TX, linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = breakdown_M, linetype = "dotted",
             color = "grey30", linewidth = 0.5) +
  geom_vline(xintercept = max_pre_slope, linetype = "dashed",
             color = "steelblue", linewidth = 0.5) +
  annotate("text", x = breakdown_M * 1.03, y = 0.002,
           label = sprintf("Breakdown\nM = %.4f", breakdown_M),
           hjust = 0, size = 2.8, color = "grey30") +
  annotate("text", x = max_pre_slope * 1.03, y = max(sensitivity_dt$ub) * 0.92,
           label = sprintf("Max pre-trend\nslope = %.4f", max_pre_slope),
           hjust = 0, size = 2.8, color = "steelblue") +
  scale_x_continuous(name = "M: Max Slope of Parallel Trends Violation",
                     labels = number_format(accuracy = 0.001)) +
  scale_y_continuous(name = "Robust 95% CI for ATT",
                     labels = number_format(accuracy = 0.001)) +
  theme_pub()

save_gg(p_hd, "Figure_HonestDiD_Sensitivity", width = 8, height = 5)
fwrite(sensitivity_dt, file.path(OUTPUT_TABLES, "Tab_HonestDiD_Sensitivity.csv"))
cat("Saved: Figure_HonestDiD_Sensitivity + Tab_HonestDiD_Sensitivity.csv\n\n")


#### S12 Pre-Reform Mandate Spike Diagnostics ####

cat("========================================\n")
cat("S12: MANDATE SPIKE DIAGNOSTICS\n")
cat("========================================\n\n")

# Check whether pre-reform closure spikes align with compliance deadlines,
# and whether the spike pattern differs between TX and controls.
# If identical across states, it's the federal mandate (expected).
# If TX spikes more, it indicates anticipation of the 1999 reform.

pre_data <- matched_tanks[panel_year %between% c(1985L, 1999L)]

closure_by_cohort_yr <- pre_data[, .(
  closure_rate = mean(closure_event, na.rm = TRUE),
  n_closures   = sum(closure_event),
  N = .N
), by = .(mm_install_cohort, panel_year,
          group = fifelse(texas_treated == 1L, "Texas", "Control States"))]

p_heat_ctrl <- ggplot(
  closure_by_cohort_yr[group == "Control States"],
  aes(x = panel_year, y = mm_install_cohort, fill = closure_rate)
) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n_closures > 5, n_closures, "")),
            size = 2.3, color = "grey20") +
  scale_fill_gradient(low = "white", high = COL_CTRL,
                      labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(1985, 1999, by = 2)) +
  labs(x = "Calendar Year", y = "Install Year", fill = "Closure Rate") +
  theme_pub() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_heat_tx <- ggplot(
  closure_by_cohort_yr[group == "Texas"],
  aes(x = panel_year, y = mm_install_cohort, fill = closure_rate)
) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n_closures > 5, n_closures, "")),
            size = 2.3, color = "grey20") +
  scale_fill_gradient(low = "white", high = COL_TX,
                      labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(1985, 1999, by = 2)) +
  labs(x = "Calendar Year", y = "Install Year", fill = "Closure Rate") +
  theme_pub() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_gg(p_heat_ctrl, "Diag_CohortYear_Heatmap_Control", width = 10, height = 6)
save_gg(p_heat_tx,   "Diag_CohortYear_Heatmap_TX",      width = 10, height = 6)
fwrite(closure_by_cohort_yr,
       file.path(OUTPUT_TABLES, "Diag_ClosureRate_CohortYear.csv"))
cat("Saved: Diag_CohortYear_Heatmap_* + Diag_ClosureRate_CohortYear.csv\n\n")


#### S13 Three-Sample Cox + OLS ####

cat("========================================\n")
cat("S13: THREE-SAMPLE COX + OLS\n")
cat("========================================\n\n")

# (A) Post-1988 only (no mandate pressure; cleanest identification)
# (B) Full sample with mandate_active control (primary spec)
# (C) Pre-1989 only (diagnostic: does the reform effect appear in cohorts
#     that faced mandate pressure? If so, it reflects pre-mandate cohort exit,
#     not the insurance reform.)

# Cox uses exact_base + survSplit at REFORM_DAYS for the full-sample specs.
# OLS uses panel_dt.

run_cox_ols <- function(cox_dt_arg, panel_dt_arg, label,
                         extra_cox = NULL, extra_ols = NULL,
                         strata_var = "make_model_tank") {
  cat(sprintf("\n--- %s ---\n", label))

  cox_rhs  <- paste(c("did_term", extra_cox), collapse = " + ")
  cox_fml  <- as.formula(paste(
    "Surv(t_enter, t_exit, failure) ~", cox_rhs, "+ strata(", strata_var, ")"
  ))
  m_cox    <- coxph(cox_fml, data = as.data.frame(cox_dt_arg),
                    cluster = cox_dt_arg$state, ties = "efron")
  r_cox    <- extract_cox_row(m_cox)
  cat(sprintf("  Cox  HR=%.3f  logHR=%+.4f  SE=%.4f  p=%.4f  N=%s  Ev=%s\n",
    r_cox$hr, r_cox$coef, r_cox$se, r_cox$p,
    format(r_cox$n, big.mark = ","), format(r_cox$ev, big.mark = ",")))

  ols_rhs  <- paste(c("did_term", extra_ols, "tank_age"), collapse = " + ")
  ols_fml  <- as.formula(paste(
    "closure_event ~", ols_rhs, "| tank_panel_id +", strata_var, "^panel_year"
  ))
  m_ols    <- feols(ols_fml, data = panel_dt_arg, cluster = ~state)
  r_ols    <- extract_panel_row(m_ols)
  cat(sprintf("  OLS  coef=%+.4f pp  SE=%.4f  p=%.4f  N=%s\n",
    r_ols$coef * 100, r_ols$se * 100, r_ols$p, format(r_ols$n, big.mark = ",")))

  list(label = label, m_cox = m_cox, m_ols = m_ols,
       r_cox = r_cox, r_ols = r_ols)
}

# Build survSplit at REFORM_DAYS for the three-sample Cox specs
exact_split_full <- as.data.table(survSplit(
  formula  = Surv(t_enter, t_exit, failure) ~ .,
  data     = as.data.frame(exact_base),
  cut      = REFORM_DAYS,
  episode  = "reform_ep"
))
exact_split_full <- exact_split_full[t_exit > t_enter]
exact_split_full[, did_term := texas_treated * as.integer(reform_ep == 2L)]

# mandate_active for the Cox split dataset
exact_split_full[, yr_mid := as.integer(
  format(as.Date((t_enter + t_exit) / 2, origin = "1970-01-01"), "%Y"))]
exact_split_full[, mandate_active := as.integer(
  (!is.na(release_det_deadline_yr) & yr_mid >= 1989L & yr_mid <= release_det_deadline_yr) |
  (!is.na(release_det_deadline_yr) & yr_mid %in% 1993L:1994L) |
  (yr_mid %in% 1996L:1998L)
)]
exact_split_full[, yr_mid := NULL]

res_A <- run_cox_ols(
  cox_dt_arg  = exact_split_full[install_yr_int >= 1989L],
  panel_dt_arg = panel_dt[install_yr_int >= 1989L],
  label       = "A: Post-1988 installs (no mandate phase-in)"
)

res_B <- run_cox_ols(
  cox_dt_arg   = exact_split_full,
  panel_dt_arg = panel_dt,
  label        = "B: Full sample + mandate_active control",
  extra_cox    = "mandate_active",
  extra_ols    = "mandate_active"
)

res_C <- run_cox_ols(
  cox_dt_arg   = exact_split_full[install_yr_int <= 1988L],
  panel_dt_arg = panel_dt[install_yr_int <= 1988L],
  label        = "C: Pre-1989 installs only (diagnostic)",
  extra_cox    = "mandate_active",
  extra_ols    = "mandate_active"
)

comparison <- rbindlist(lapply(list(res_A, res_B, res_C), function(r) {
  data.table(
    Sample      = r$label,
    Cox_logHR   = round(r$r_cox$coef, 5),
    Cox_HR      = round(r$r_cox$hr,   4),
    Cox_SE      = round(r$r_cox$se,   5),
    Cox_p       = round(r$r_cox$p,    4),
    Cox_N       = r$r_cox$n,
    OLS_coef_pp = round(r$r_ols$coef * 100, 4),
    OLS_SE_pp   = round(r$r_ols$se   * 100, 4),
    OLS_p       = round(r$r_ols$p,    4),
    OLS_N       = r$r_ols$n
  )
}))

cat("\nThree-sample comparison:\n")
print(comparison)
fwrite(comparison, file.path(OUTPUT_TABLES, "Table_ThreeSample_Comparison.csv"))

# Headline primary model objects for downstream sections
m_cox_primary <- res_B$m_cox
m_ols_primary <- res_B$m_ols
cox_prim      <- res_B$r_cox
ols_row       <- res_B$r_ols
cat("\n")


#### S14 Duration Model Comparison Table ####

cat("========================================\n")
cat("S14: DURATION MODEL COMPARISON TABLE\n")
cat("========================================\n\n")

write_tex(c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{Effect of Experience Rating on Tank Closure}",
  "\\label{tbl:duration_models}",
  "\\begin{tabular}{lcc}", "\\toprule",
  " & (1) & (2) \\\\",
  " & \\textit{Primary: Cox} & \\textit{Reference: OLS LPM} \\\\",
  "\\midrule",
  sprintf("Hazard ratio (Texas $\\times$ Post) & %s & --- \\\\",
    fmt_hr(cox_prim$hr, cox_prim$p)),
  sprintf("95\\%% CI & %s & \\\\",
    fmt_ci(exp(cox_prim$coef - 1.96 * cox_prim$se),
           exp(cox_prim$coef + 1.96 * cox_prim$se))),
  "\\addlinespace",
  sprintf("Log-hazard / OLS coefficient & %s & %s \\\\",
    fmt_est(cox_prim$coef, cox_prim$p),
    fmt_est(ols_row$coef, ols_row$p)),
  sprintf("(SE) & %s & %s \\\\",
    fmt_se(cox_prim$se), fmt_se(ols_row$se)),
  "\\midrule",
  "Estimand         & Log-hazard ratio & Pr(closure) \\\\",
  "Time axis        & Calendar days    & Calendar year \\\\",
  "Unit FE          & Strata           & Tank FE \\\\",
  "\\midrule",
  sprintf("Observations      & %s & %s \\\\",
    format(cox_prim$n, big.mark = ","), format(ols_row$n, big.mark = ",")),
  sprintf("Events (closures) & %s & %s \\\\",
    format(cox_prim$ev, big.mark = ","),
    format(sum(panel_dt$closure_event, na.rm = TRUE), big.mark = ",")),
  "\\bottomrule",
  "\\end{tabular}", "\\end{table}"
), "Table_Duration_Models.tex")
cat("Saved: Table_Duration_Models.tex\n\n")


#### S15 HTE by Wall Type ####

cat("========================================\n")
cat("S15: HTE BY WALL TYPE\n")
cat("========================================\n\n")

m_hte_wall_ols <- feols(
  closure_event ~ did_term:mm_wall + mandate_active + tank_age |
    tank_panel_id + make_model_tank^panel_year,
  data = panel_dt, cluster = ~state)

exact_split_full[, `:=`(
  did_term_sw = did_term * as.integer(mm_wall == "Single-Walled"),
  did_term_dw = did_term * as.integer(mm_wall == "Double-Walled")
)]

m_hte_wall_cox <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term_sw + did_term_dw +
    mandate_active + strata(make_model_tank),
  data = as.data.frame(exact_split_full),
  cluster = exact_split_full$state, ties = "efron")

wall_cox_s <- summary(m_hte_wall_cox)$coefficients
cat(sprintf("SW HR=%.3f (p=%.4f)  DW HR=%.3f (p=%.4f)\n",
  exp(wall_cox_s["did_term_sw", "coef"]), wall_cox_s["did_term_sw", "Pr(>|z|)"],
  exp(wall_cox_s["did_term_dw", "coef"]), wall_cox_s["did_term_dw", "Pr(>|z|)"]))

fwrite(as.data.table(coeftable(m_hte_wall_ols), keep.rownames = "term"),
       file.path(OUTPUT_TABLES, "Table_HTE_WallType_OLS.csv"))
fwrite(as.data.table(wall_cox_s, keep.rownames = "term"),
       file.path(OUTPUT_TABLES, "Table_HTE_WallType_Cox.csv"))
exact_split_full[, c("did_term_sw", "did_term_dw") := NULL]
cat("Saved: Table_HTE_WallType_*\n\n")


#### S16 HTE by Vintage Bin ####

cat("========================================\n")
cat("S16: HTE BY INSTALL YEAR\n")
cat("========================================\n\n")

# Interact DiD with mm_install_cohort to test whether the reform effect
# differs by install year — cells are already defined at this granularity
# so this is the natural HTE dimension. The cell x year FE specification
# absorbs the baseline hazard for each install-year/type combination,
# leaving the did_term:mm_install_cohort interaction to identify differential
# reform effects across cohorts.

m_hte_cohort_ols <- feols(
  closure_event ~ did_term:mm_install_cohort + mandate_active + tank_age |
    tank_panel_id + make_model_tank^panel_year,
  data = panel_dt, cluster = ~state)

# Get sorted install years present in the Cox split data
install_years <- sort(unique(exact_split_full$mm_install_cohort))

for (yr in install_years) {
  vname <- paste0("did_yr", yr)
  exact_split_full[, (vname) := did_term * as.integer(mm_install_cohort == yr)]
}
cohort_did_vars <- paste0("did_yr", install_years)

cox_cohort_fml <- as.formula(paste(
  "Surv(t_enter, t_exit, failure) ~",
  paste(cohort_did_vars, collapse = " + "),
  "+ mandate_active + strata(make_model_tank)"
))
m_hte_cohort_cox <- coxph(cox_cohort_fml,
  data    = as.data.frame(exact_split_full),
  cluster = exact_split_full$state, ties = "efron")

cohort_cox_s <- summary(m_hte_cohort_cox)$coefficients
cohort_forest <- data.table(
  install_year = install_years,
  var          = cohort_did_vars,
  hr           = exp(cohort_cox_s[cohort_did_vars, "coef"]),
  coef         = cohort_cox_s[cohort_did_vars, "coef"],
  se           = cohort_cox_s[cohort_did_vars, "se(coef)"],
  p            = cohort_cox_s[cohort_did_vars, "Pr(>|z|)"]
)
cohort_forest[, `:=`(
  ci_lo        = exp(coef - 1.96 * se),
  ci_hi        = exp(coef + 1.96 * se),
  install_yr_f = factor(install_year, levels = rev(install_years))
)]

p_cohort_forest <- ggplot(cohort_forest,
  aes(x = hr, y = install_yr_f, xmin = ci_lo, xmax = ci_hi)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(size = 0.5, linewidth = 0.6, color = COL_TX) +
  labs(x = "Hazard Ratio (Texas x Post)", y = "Install Year") +
  theme_pub() +
  theme(panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        axis.text.y = element_text(size = 8))

save_gg(p_cohort_forest, "Figure_Cox_InstallYearForest", width = 7, height = 7)
fwrite(cohort_forest[, !c("install_yr_f")],
       file.path(OUTPUT_TABLES, "Table_HTE_InstallYear_Cox.csv"))
fwrite(as.data.table(coeftable(m_hte_cohort_ols), keep.rownames = "term"),
       file.path(OUTPUT_TABLES, "Table_HTE_InstallYear_OLS.csv"))
exact_split_full[, (cohort_did_vars) := NULL]
cat("Saved: Figure_Cox_InstallYearForest + Table_HTE_InstallYear_*\n\n")


#### S17 Age-at-Closure Analysis ####

cat("========================================\n")
cat("S17: AGE-AT-CLOSURE ANALYSIS\n")
cat("========================================\n\n")

closed_tanks <- exact_base[failure == 1L, .(
  tank_panel_id, state, texas_treated, mm_install_cohort, mm_wall,
  age_at_closure = age_exit,
  period         = fifelse(t_exit >= REFORM_DAYS, "Post-reform", "Pre-reform")
)]
closed_tanks[, group := paste(
  fifelse(texas_treated == 1L, "Texas", "Control States"), period
)]

summary_acl <- closed_tanks[, .(
  n      = .N,
  mean   = round(mean(age_at_closure),   2),
  median = round(median(age_at_closure), 2),
  sd     = round(sd(age_at_closure),     2)
), by = group][order(group)]

cat("Age at closure summary:\n")
print(summary_acl)
fwrite(summary_acl, file.path(OUTPUT_TABLES, "Table_AgeAtClosure_Summary.csv"))

p_hist_acl <- ggplot(closed_tanks,
  aes(x = age_at_closure,
      fill = paste(fifelse(texas_treated == 1L, "Texas", "Control States"), period))) +
  geom_histogram(binwidth = 1, alpha = 0.85, color = "white", linewidth = 0.2) +
  facet_wrap(~group, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c(
    "Control States Pre-reform"  = COL_CTRL,
    "Control States Post-reform" = "#56B4E9",
    "Texas Pre-reform"           = COL_PRE,
    "Texas Post-reform"          = COL_TX), guide = "none") +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  labs(x = "Age at Closure (years)", y = "Number of Closures") +
  theme_pub()

save_gg(p_hist_acl, "Figure_AgeAtClosure_Histogram", width = 10, height = 6)
cat("Saved: Figure_AgeAtClosure_Histogram\n\n")


#### S18 Kaplan-Meier Survival Curves ####

cat("========================================\n")
cat("S18: KAPLAN-MEIER SURVIVAL CURVES\n")
cat("========================================\n\n")

km_base <- exact_base[, .(
  tank_panel_id, texas_treated, mm_install_cohort,
  time_to_event = age_exit,
  event         = failure
)]
km_base[, strata_group := paste(
  fifelse(texas_treated == 1L, "Texas", "Control States"),
  mm_install_cohort
)]

km_fit  <- survfit(Surv(time_to_event, event) ~ strata_group, data = as.data.frame(km_base))
km_tidy <- as.data.table(tidy(km_fit))
km_tidy[, state_part := fifelse(grepl("^Texas", strata), "Texas", "Control States")]

p_km <- ggplot(km_tidy, aes(x = time, y = estimate, color = strata)) +
  geom_step(linewidth = 0.6) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = strata),
              alpha = 0.06, color = NA) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 50, 10)) +
  facet_wrap(~state_part, ncol = 2) +
  labs(x = "Tank Age (years)", y = "Survival Probability") +
  theme_pub() + theme(legend.position = "right",
                      legend.text = element_text(size = 7))

save_gg(p_km, "Figure_KM_TankSurvival", width = 12, height = 5.5)
cat("Saved: Figure_KM_TankSurvival\n\n")


#### S19 Robustness Specifications ####

cat("========================================\n")
cat("S19: ROBUSTNESS SPECIFICATIONS\n")
cat("========================================\n\n")

# 3-dim cell (drop capacity) — already built in Panel_Build using install year,
# but rebuild here in case exact_split_full was constructed fresh in this session
panel_dt[,         make_model_3dim := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "|")]
exact_split_full[, make_model_3dim := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "|")]

m_cox_3dim <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term + mandate_active + strata(make_model_3dim),
  data = as.data.frame(exact_split_full), cluster = exact_split_full$state, ties = "efron")
m_ols_3dim <- feols(
  closure_event ~ did_term + mandate_active + tank_age |
    tank_panel_id + make_model_3dim^panel_year,
  data = panel_dt, cluster = ~state)
r_3dim_cox <- extract_cox_row(m_cox_3dim)
r_3dim_ols <- extract_panel_row(m_ols_3dim)
cat(sprintf("3-dim cell: Cox HR=%.3f vs primary HR=%.3f\n", r_3dim_cox$hr, cox_prim$hr))

# Drop TX border states (OK, AR, LA)
BORDER_STATES   <- c("OK", "AR", "LA")
exact_no_border <- exact_split_full[!state %in% BORDER_STATES]
panel_no_border <- panel_dt[!state %in% BORDER_STATES]

m_cox_noborder <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term + mandate_active + strata(make_model_tank),
  data = as.data.frame(exact_no_border), cluster = exact_no_border$state, ties = "efron")
m_ols_noborder <- feols(
  closure_event ~ did_term + mandate_active + tank_age |
    tank_panel_id + make_model_tank^panel_year,
  data = panel_no_border, cluster = ~state)
r_nb_cox <- extract_cox_row(m_cox_noborder)
r_nb_ols <- extract_panel_row(m_ols_noborder)
cat(sprintf("No-border: Cox HR=%.3f (p=%.4f)\n", r_nb_cox$hr, r_nb_cox$p))

rob_summary <- data.table(
  Specification = c(
    "Primary Cox (4-dim cell, full sample)",
    "Primary OLS (4-dim cell, full sample)",
    "Cox 3-dim (no capacity)",
    "OLS 3-dim (no capacity)",
    "Cox 4-dim, drop border states",
    "OLS 4-dim, drop border states",
    "Cox 4-dim, post-1988 only",
    "OLS 4-dim, post-1988 only"),
  Estimand = rep(c("Log-hazard ratio", "OLS coefficient"), 4),
  Estimate = round(c(
    cox_prim$coef,   ols_row$coef,
    r_3dim_cox$coef, r_3dim_ols$coef,
    r_nb_cox$coef,   r_nb_ols$coef,
    res_A$r_cox$coef, res_A$r_ols$coef), 5),
  HR = round(c(
    cox_prim$hr, NA, r_3dim_cox$hr, NA,
    r_nb_cox$hr, NA, res_A$r_cox$hr, NA), 4),
  SE = round(c(
    cox_prim$se,   ols_row$se,
    r_3dim_cox$se, r_3dim_ols$se,
    r_nb_cox$se,   r_nb_ols$se,
    res_A$r_cox$se, res_A$r_ols$se), 5),
  p_value = round(c(
    cox_prim$p,   ols_row$p,
    r_3dim_cox$p, r_3dim_ols$p,
    r_nb_cox$p,   r_nb_ols$p,
    res_A$r_cox$p, res_A$r_ols$p), 4),
  N = c(
    cox_prim$n,   ols_row$n,
    r_3dim_cox$n, nobs(m_ols_3dim),
    r_nb_cox$n,   nobs(m_ols_noborder),
    res_A$r_cox$n, res_A$r_ols$n)
)
fwrite(rob_summary, file.path(OUTPUT_TABLES, "Table_Robustness_Summary.csv"))
cat(sprintf("Saved: Table_Robustness_Summary.csv (%d specs)\n\n", nrow(rob_summary)))


#### S20 Diagnostic Export ####

cat("========================================\n")
cat("S20: DIAGNOSTIC EXPORT\n")
cat("========================================\n\n")

cross_spec <- data.table(
  Model = c(
    "Cox primary (full sample + mandate ctrl)",
    "Cox post-1988 only (no mandate pressure)",
    "OLS primary (full sample + mandate ctrl)",
    "Cox 3-dim robustness",
    "Cox no-border robustness"),
  Log_coef = round(c(cox_prim$coef,    res_A$r_cox$coef,
                      ols_row$coef,     r_3dim_cox$coef, r_nb_cox$coef), 5),
  HR       = round(c(cox_prim$hr,      res_A$r_cox$hr,
                      NA,               r_3dim_cox$hr,   r_nb_cox$hr), 4),
  SE       = round(c(cox_prim$se,      res_A$r_cox$se,
                      ols_row$se,       r_3dim_cox$se,   r_nb_cox$se), 5),
  p_value  = round(c(cox_prim$p,       res_A$r_cox$p,
                      ols_row$p,        r_3dim_cox$p,    r_nb_cox$p), 4),
  N_obs    = c(cox_prim$n,   res_A$r_cox$n, ols_row$n, r_3dim_cox$n, r_nb_cox$n),
  N_events = c(cox_prim$ev,  res_A$r_cox$ev, NA,       r_3dim_cox$ev, r_nb_cox$ev)
)
fwrite(cross_spec, file.path(OUTPUT_TABLES, "Table_CrossSpec_Summary.csv"))

sample_summ <- data.table(
  Level = c(
    "panel_dt tank-years", "panel_dt unique tanks",
    "  TX tanks", "  CTL tanks",
    "make-model cells",
    "exact_base tanks", "exact_base events (closures)",
    "matched tanks", "matched tank-years"),
  N = c(n_ty, n_tanks, n_tx_tanks, n_ct_tanks, n_cells,
        nrow(exact_base), sum(exact_base$failure),
        length(matched_ids), nrow(matched_tanks))
)
fwrite(sample_summ, file.path(OUTPUT_TABLES, "Table_SampleSizes.csv"))

cat("Cross-spec summary:\n")
print(cross_spec[, .(Model, Log_coef, HR, SE, p_value, N_obs)])

cat("\nAll outputs saved to:\n")
cat("  Tables: ", OUTPUT_TABLES,  "\n")
cat("  Figures:", OUTPUT_FIGURES, "\n")

cat("\n========================================\n")
cat("02b_DiD_Analysis.R COMPLETE\n")
cat("========================================\n\n")
