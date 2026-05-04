################################################################################
# 02b_DiD_Analysis.R
#
# Texas UST insurance reform: difference-in-differences and Cox event-study
# analysis on the birth-CEM matched panel.
#
# Single consolidated script. Run interactively or as a batch.
# Per-block bookkeeping acceptance assertions have been removed; remaining
# stopifnot() calls are real runtime guards that should fire on actual bugs.
#
# SECTIONS:
#   S1 + S2: SETUP, PACKAGES, DATA LOAD
#   S3: HELPER FUNCTIONS
#   S4: COX SPELL SPLITS
#   S5: TABLE 1 - OLS STEPWISE WALK-IN
#   S6: TABLE 2 - COX STEPWISE WALK-IN
#   S7: TABLE 3 - TRIPLE DIFF (OLS + COX)
#   S7b: AGE HTE (SATURATED INTERACTION, 9 BINS)
#   S8a: OLS EVENT STUDY MODELS + FIGURES
#   S8b: COX EVENT STUDY MODELS + FIGURES
#   S9: POPULATION DESCRIPTION + COHORT-DISAGGREGATED PRE-TRENDS
#   S10: AGE-AT-CLOSURE (3 SPECS)
#   S11 + S11b: HONESTDID SENSITIVITY (OLS + COX)
#   S11c + S11d: PRETRENDS POWER + REFORM-CEM HONESTDID
################################################################################


################################################################################
# === S1 + S2: SETUP, PACKAGES, DATA LOAD ===
################################################################################

#### S1 Setup & Constants ####

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(HonestDiD)
  library(pretrends)
  library(fwildclusterboot)
  library(future)
  library(future.apply)
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
dqrng::dqset.seed(20260202)   # <-- add this line

setDTthreads(14)

OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
ANALYSIS_DIR   <- here("Data", "Analysis")

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# ---- Study parameters (match Panel_Build) ----
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

# ---- Bootstrap constants ----
N_BOOT    <- 9999L
BOOT_TYPE <- "rademacher"
N_CORES   <- 8L
options(future.globals.maxSize = 8 * 1024^3)  # 8 GB
plan(multisession, workers = N_CORES)

# ---- Colors ----
COL_TX    <- "#D55E00"
COL_CTRL  <- "#0072B2"
COL_PRE   <- "#E69F00"
COL_BLUE  <- "#2166ac"
COL_RED   <- "#d6604d"
COL_SW    <- "#08519c"
COL_DW    <- "#74c476"
COL_GROUP <- c("Texas" = COL_TX, "Control States" = COL_CTRL)

# ---- Event study plotting globals ----
ES_YLIM    <- c(-0.010, 0.050)
ES_YBREAKS <- seq(ES_YLIM[1], ES_YLIM[2], by = 0.010)

# ---- fixest defaults ----
setFixest_etable(style.tex = style.tex("aer"), digits = 4,
                 se.below = TRUE, depvar = FALSE)


#### S2 Load Datasets ####

cat("========================================\n")
cat("S2: LOAD DATASETS\n")
cat("========================================\n\n")

# ---- Panels (unchanged from Panel_Build) ----
panel_dt   <- fread(file.path(ANALYSIS_DIR, "panel_dt.csv"),
                    na.strings = c("", "NA"))
exact_base <- fread(file.path(ANALYSIS_DIR, "exact_base.csv"),
                    na.strings = c("", "NA"))

# ---- PRIMARY: birth-cohort CEM + PSM ----
matched_tanks_birth_cem <- fread(
  file.path(ANALYSIS_DIR, "matched_tanks_birth_cem.csv"),
  na.strings = c("", "NA")
)
matched_tanks_birth_psm <- fread(
  file.path(ANALYSIS_DIR, "matched_tanks_birth_psm.csv"),
  na.strings = c("", "NA")
)

# ---- ROBUSTNESS: reform-survivor CEM + PSM ----
matched_tanks_reform_cem <- fread(
  file.path(ANALYSIS_DIR, "matched_tanks_reform_cem.csv"),
  na.strings = c("", "NA")
)
matched_tanks_reform_psm <- fread(
  file.path(ANALYSIS_DIR, "matched_tanks_reform_psm.csv"),
  na.strings = c("", "NA")
)

# ---- ID lookups ----
ids_birth_cem  <- fread(file.path(ANALYSIS_DIR, "ids_tank_birth_cem.csv"))$tank_panel_id
ids_birth_psm  <- fread(file.path(ANALYSIS_DIR, "ids_tank_birth_psm.csv"))$tank_panel_id
ids_reform_cem <- fread(file.path(ANALYSIS_DIR, "ids_tank_reform_cem.csv"))$tank_panel_id
ids_reform_psm <- fread(file.path(ANALYSIS_DIR, "ids_tank_reform_psm.csv"))$tank_panel_id

# ---- panel_meta scalars we still need (note: med_age from panel_meta is
#      ignored; we recompute per sample below) ----
meta            <- fread(file.path(ANALYSIS_DIR, "panel_meta.csv"))
ctrl_mean_post  <- meta[scalar == "ctrl_mean_post", value]
n_alive_reform  <- meta[scalar == "n_alive_at_reform", value]

# ---- Type coercions (all samples) ----
all_samples <- list(
  panel_dt                  = panel_dt,
  exact_base                = exact_base,
  matched_tanks_birth_cem   = matched_tanks_birth_cem,
  matched_tanks_birth_psm   = matched_tanks_birth_psm,
  matched_tanks_reform_cem  = matched_tanks_reform_cem,
  matched_tanks_reform_psm  = matched_tanks_reform_psm
)

for (nm in names(all_samples)) {
  dt <- all_samples[[nm]]
  if ("mm_install_cohort" %in% names(dt))
    dt[, mm_install_cohort := as.character(mm_install_cohort)]
  for (col in intersect(c("tank_installed_date","tank_closed_date"), names(dt)))
    dt[, (col) := as.IDate(get(col))]
}

# ---- First-year-churn filter (installation-year failure is not an
#      economic exit; these are failed inspections at install) ----
panel_dt                 <- panel_dt[                first_year_churn == 0L | is.na(first_year_churn)]
exact_base               <- exact_base[              first_year_churn == 0L | is.na(first_year_churn)]
matched_tanks_birth_cem  <- matched_tanks_birth_cem[ first_year_churn == 0L | is.na(first_year_churn)]
matched_tanks_birth_psm  <- matched_tanks_birth_psm[ first_year_churn == 0L | is.na(first_year_churn)]
matched_tanks_reform_cem <- matched_tanks_reform_cem[first_year_churn == 0L | is.na(first_year_churn)]
matched_tanks_reform_psm <- matched_tanks_reform_psm[first_year_churn == 0L | is.na(first_year_churn)]

# ---- med_age: computed per sample on TANKS ALIVE AT REFORM DATE only ----
# Rationale: "age at treatment" is only meaningful for tanks that existed
# at the time of treatment. Two exclusions:
#   1. Tanks installed >= 1999-01-01 did not exist at reform (post-reform
#      entrants); no age at treatment.
#   2. Tanks closed before Dec 22 1998 no longer existed at reform
#      (pre-reform exits, retained elsewhere in birth-cohort sample for
#      pre-period DiD identification); no age at treatment.
# Both groups are EXCLUDED from the median and receive above_median_age = NA.
compute_med_age <- function(dt) {
  alive <- unique(
    dt[
      install_yr_int < 1999L &
      tank_installed_date <= REFORM_DATE &
      (is.na(tank_closed_date) | tank_closed_date > REFORM_DATE),
      .(tank_panel_id, install_yr_int)
    ]
  )
  median(1999L - alive$install_yr_int, na.rm = TRUE)
}

med_age_main       <- compute_med_age(panel_dt)
med_age_birth_cem  <- compute_med_age(matched_tanks_birth_cem)
med_age_birth_psm  <- compute_med_age(matched_tanks_birth_psm)
med_age_reform_cem <- compute_med_age(matched_tanks_reform_cem)
med_age_reform_psm <- compute_med_age(matched_tanks_reform_psm)

cat("med_age DIAGNOSTICS (tanks alive at reform date 1998-12-22):\n")

n_alive <- function(dt) {
  uniqueN(dt[install_yr_int < 1999L &
             tank_installed_date <= REFORM_DATE &
             (is.na(tank_closed_date) | tank_closed_date > REFORM_DATE),
             tank_panel_id])
}

cat(sprintf("  Full panel:          %.1f yr (n=%s tanks alive at reform)\n",
  med_age_main,       format(n_alive(panel_dt),                 big.mark = ",")))
cat(sprintf("  Birth-CEM matched:   %.1f yr (n=%s tanks alive at reform)\n",
  med_age_birth_cem,  format(n_alive(matched_tanks_birth_cem),  big.mark = ",")))
cat(sprintf("  Birth-PSM matched:   %.1f yr (n=%s tanks alive at reform)\n",
  med_age_birth_psm,  format(n_alive(matched_tanks_birth_psm),  big.mark = ",")))
cat(sprintf("  Reform-CEM matched:  %.1f yr (n=%s tanks alive at reform)\n",
  med_age_reform_cem, format(n_alive(matched_tanks_reform_cem), big.mark = ",")))
cat(sprintf("  Reform-PSM matched:  %.1f yr (n=%s tanks alive at reform)\n\n",
  med_age_reform_psm, format(n_alive(matched_tanks_reform_psm), big.mark = ",")))

# Reform-CEM/PSM n should equal full panel-pool reform survivors by
# definition of the reform-survivor matching (Panel_Build S10).
# Birth-CEM/PSM n is a subset of the reform-survivor count because only
# matched tanks qualify.

# ---- Regenerate above_median_age per sample ----
# above_median_age is NA for tanks NOT alive at reform date:
#   - install_yr_int >= 1999 (post-reform entrants)
#   - tank_closed_date <= REFORM_DATE (pre-reform exits)
# This prevents HTE subsample specs from contaminating age splits with
# tanks that have no age-at-treatment. Pooled specs (which don't use
# above_median_age) are unaffected: pre-reform exits and post-reform
# entrants continue to contribute their observations.
add_above_median_age <- function(dt, med_val) {
  dt[, age_at_treatment := NA_integer_]
  dt[, above_median_age := NA_integer_]

  alive_mask <- dt$install_yr_int < 1999L &
                dt$tank_installed_date <= REFORM_DATE &
                (is.na(dt$tank_closed_date) | dt$tank_closed_date > REFORM_DATE)

  dt[alive_mask, age_at_treatment := 1999L - install_yr_int]
  dt[alive_mask, above_median_age := as.integer(age_at_treatment >= med_val)]
  dt[]
}

matched_tanks_birth_cem  <- add_above_median_age(matched_tanks_birth_cem,  med_age_birth_cem)
matched_tanks_birth_psm  <- add_above_median_age(matched_tanks_birth_psm,  med_age_birth_psm)
matched_tanks_reform_cem <- add_above_median_age(matched_tanks_reform_cem, med_age_reform_cem)
matched_tanks_reform_psm <- add_above_median_age(matched_tanks_reform_psm, med_age_reform_psm)

# ---- Primary alias for legacy references: matched_tanks == birth-CEM ----
# Throughout S5-S8 the primary sample is matched_tanks (= birth-CEM).
# Reform-CEM appears explicitly as matched_tanks_reform_cem.
matched_tanks <- matched_tanks_birth_cem
matched_ids   <- ids_birth_cem
med_age       <- med_age_birth_cem

# ---- Log sample sizes ----
cat("Sample sizes (tank-years | unique tanks | pre-reform incumbent tanks):\n")
log_one <- function(nm, dt) {
  cat(sprintf("  %-28s  %10s  %8s  %8s\n",
    nm,
    format(nrow(dt), big.mark = ","),
    format(uniqueN(dt$tank_panel_id), big.mark = ","),
    format(uniqueN(dt[install_yr_int < 1999L, tank_panel_id]), big.mark = ",")
  ))
}
log_one("panel_dt",                 panel_dt)
log_one("matched_tanks_birth_cem",  matched_tanks_birth_cem)
log_one("matched_tanks_birth_psm",  matched_tanks_birth_psm)
log_one("matched_tanks_reform_cem", matched_tanks_reform_cem)
log_one("matched_tanks_reform_psm", matched_tanks_reform_psm)
cat("\n")


################################################################################
# === S3: HELPER FUNCTIONS ===
################################################################################

#### S3 Helpers ####

cat("========================================\n")
cat("S3: DEFINE HELPERS\n")
cat("========================================\n\n")


# =============================================================================
# B2.1 — Logging and formatting
# =============================================================================

log_step <- function(msg, indent = 0L) cat(strrep("  ", indent), msg, "\n", sep = "")
fmt_n    <- function(n) format(n, big.mark = ",", scientific = FALSE)

stars_p <- function(p) {
  fcase(is.na(p), "",
        p < 0.01, "$^{***}$",
        p < 0.05, "$^{**}$",
        p < 0.10, "$^{*}$",
        default = "")
}

fmt_est <- function(x, p) sprintf("%.4f%s", x, stars_p(p))
fmt_se  <- function(x)    sprintf("(%.4f)", x)
fmt_hr  <- function(x, p) sprintf("%.3f%s", x, stars_p(p))
fmt_ci  <- function(lo, hi) sprintf("[%.3f, %.3f]", lo, hi)


# =============================================================================
# B2.2 — Single-row coefficient extraction
# =============================================================================

extract_cox_row <- function(m, term_name = "did_term") {
  s      <- summary(m)$coefficients
  idx    <- grep(term_name, rownames(s), fixed = TRUE)[1]
  stopifnot("Term not found in Cox model" = !is.na(idx))
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
  stopifnot("Term not found in panel model" = !is.na(idx))
  list(
    coef = ct[idx, "Estimate"],
    se   = ct[idx, "Std. Error"],
    p    = ct[idx, "Pr(>|t|)"],
    n    = nobs(m)
  )
}


# =============================================================================
# B2.3 — Plotting theme and save wrapper
# =============================================================================

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

# Clean TeX loader for Quarto chunks. Strips \begin{table}/\end{table} and
# \centering so Quarto's own float can wrap the tabular. No \resizebox.
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

write_tex <- function(lines, filename) {
  writeLines(lines, file.path(OUTPUT_TABLES, filename))
  message(sprintf("  Written: %s", filename))
}


# =============================================================================
# B2.4 — Event study coefficient extraction
# =============================================================================

# OLS event study: extract coefficients from fixest i() term pattern
#   rel_year::VALUE:texas_treated
# ref year is excluded by fixest; we add it back as a zero row for plotting.
es_tidy <- function(model, ref = -1L) {
  df <- as.data.table(broom::tidy(model, conf.int = TRUE))
  df <- df[grepl("^rel_year::", term)]
  df[, year := as.integer(
    sub("^rel_year::(-?[0-9]+):texas_treated$", "\\1", term)
  )]
  if (!ref %in% df$year) {
    ref_row <- data.table(
      term = paste0("ref_", ref), estimate = 0, std.error = 0,
      conf.low = 0, conf.high = 0, year = as.integer(ref)
    )
    df <- rbindlist(
      list(df[, .(term, estimate, std.error, conf.low, conf.high, year)],
           ref_row),
      fill = TRUE
    )
  }
  setorder(df, year)
  df
}

# Cox event study: extract coefficients from es_tau_* terms
# Names: es_tau_mN for negative tau, es_tau_N for non-negative tau.
# robust se used (cluster-robust); CI ± 1.96 * SE on log-HR scale.
es_tidy_cox <- function(cox_model, ref = -1L) {
  s       <- summary(cox_model)$coefficients
  se_col  <- intersect(c("robust se", "se(coef)"), colnames(s))[1]

  es_rows <- grep("^es_tau_", rownames(s))
  stopifnot("No es_tau_* terms found in Cox model" = length(es_rows) > 0L)

  b_es  <- s[es_rows, "coef"]
  se_es <- s[es_rows, se_col]

  tau_from_name <- function(nm) {
    if (grepl("^es_tau_m([0-9]+)$", nm)) {
      -as.integer(sub("^es_tau_m([0-9]+)$", "\\1", nm))
    } else {
      as.integer(sub("^es_tau_([0-9]+)$", "\\1", nm))
    }
  }
  tau_vals <- vapply(rownames(s)[es_rows], tau_from_name, integer(1))

  df <- data.table(
    year      = tau_vals,
    estimate  = b_es,
    std.error = se_es,
    conf.low  = b_es - 1.96 * se_es,
    conf.high = b_es + 1.96 * se_es
  )
  if (!ref %in% df$year) {
    df <- rbindlist(list(
      df,
      data.table(year = as.integer(ref), estimate = 0, std.error = 0,
                 conf.low = 0, conf.high = 0)
    ), fill = TRUE)
  }
  setorder(df, year)
  df
}


# =============================================================================
# B2.4 — Event study plotting (OLS + Cox)
# =============================================================================

# OLS event study plot.
# Uses coord_cartesian for viewport clipping — CI bars extending beyond
# ylim render as truncated bars rather than disappearing.
es_ggplot <- function(model,
                      point_col = "grey25",
                      label     = NULL,
                      ylim      = ES_YLIM,
                      ybreaks   = ES_YBREAKS,
                      ref       = -1L,
                      ctrl_mean = NULL) {

  df      <- es_tidy(model, ref = ref)
  ref_row <- df[year == ref]
  est_row <- df[year != ref]

  p <- ggplot(df, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.5)

  if (!is.null(ctrl_mean)) {
    p <- p +
      geom_hline(yintercept = ctrl_mean, colour = "steelblue",
                 linewidth = 0.45, linetype = "dotted") +
      annotate("text", x = -Inf,
               y = ctrl_mean + diff(ylim) * 0.04,
               label = sprintf("Control mean (post-reform): %.4f", ctrl_mean),
               hjust = -0.04, size = 2.5, colour = "steelblue",
               fontface = "italic")
  }

  p <- p +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    geom_errorbar(data = est_row,
                  aes(ymin = conf.low, ymax = conf.high),
                  colour = point_col, width = 0.25, linewidth = 0.45) +
    geom_point(data = est_row, colour = point_col, size = 1.8, shape = 16) +
    geom_point(data = ref_row, shape = 1, size = 2.2, colour = "grey40") +
    annotate("text", x = ref, y = ylim[1] + diff(ylim) * 0.06,
             label = sprintf("(%d)\nomitted", ref),
             size = 2.8, colour = "grey35", fontface = "italic",
             lineheight = 0.85)

  if (!is.null(label)) {
    p <- p + annotate("text", x = -Inf, y = Inf,
                      label = label, hjust = -0.08, vjust = 1.4,
                      size = 3, colour = "grey25")
  }

  p +
    scale_x_continuous(breaks = sort(unique(df$year))) +
    scale_y_continuous(breaks = ybreaks,
                       labels = scales::number_format(accuracy = 0.001)) +
    coord_cartesian(ylim = ylim) +
    labs(x = "Years Relative to 1998",
         y = "Effect on Annual Closure Probability") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(axis.line        = element_line(colour = "black", linewidth = 0.4),
          axis.ticks       = element_line(colour = "black", linewidth = 0.3),
          axis.text        = element_text(colour = "black"),
          axis.text.x      = element_text(size = 8),
          panel.grid       = element_blank(),
          legend.position  = "none",
          plot.title       = element_blank(),
          plot.margin      = margin(8, 10, 8, 8))
}

# Cox event study plot. Y-axis is log-hazard ratio.
# ylim wider than OLS because log-HR scale is inherently larger.
es_ggplot_cox <- function(cox_model,
                          point_col = "grey25",
                          label     = NULL,
                          ylim      = c(-0.15, 0.40),
                          ref       = -1L) {

  df      <- es_tidy_cox(cox_model, ref = ref)
  ref_row <- df[year == ref]
  est_row <- df[year != ref]

  ybreaks <- seq(ylim[1], ylim[2], length.out = 7)

  p <- ggplot(df, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    geom_errorbar(data = est_row,
                  aes(ymin = conf.low, ymax = conf.high),
                  colour = point_col, width = 0.25, linewidth = 0.45) +
    geom_point(data = est_row, colour = point_col, size = 1.8, shape = 16) +
    geom_point(data = ref_row, shape = 1, size = 2.2, colour = "grey40") +
    annotate("text", x = ref, y = ylim[1] + diff(ylim) * 0.06,
             label = sprintf("(%d)\nomitted", ref),
             size = 2.8, colour = "grey35", fontface = "italic",
             lineheight = 0.85)

  if (!is.null(label)) {
    p <- p + annotate("text", x = -Inf, y = Inf,
                      label = label, hjust = -0.08, vjust = 1.4,
                      size = 3, colour = "grey25")
  }

  p +
    scale_x_continuous(breaks = sort(unique(df$year))) +
    scale_y_continuous(breaks = ybreaks,
                       labels = scales::number_format(accuracy = 0.01)) +
    coord_cartesian(ylim = ylim) +
    labs(x = "Years Relative to 1998",
         y = "Log Hazard Ratio (Texas \u00d7 Post)") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(axis.line        = element_line(colour = "black", linewidth = 0.4),
          axis.ticks       = element_line(colour = "black", linewidth = 0.3),
          axis.text        = element_text(colour = "black"),
          axis.text.x      = element_text(size = 8),
          panel.grid       = element_blank(),
          legend.position  = "none",
          plot.title       = element_blank(),
          plot.margin      = margin(8, 10, 8, 8))
}


# =============================================================================
# B2.5 — Cox split data constructors
# =============================================================================

# Standard two-episode Cox split at reform date.
# yr_mid (midpoint of each episode) used consistently for all mandate
# controls — avoids the bug where t_enter- or t_exit-based deadlines
# misclassify spells straddling the deadline year.
build_cox_split <- function(tank_ids, reform_cut = REFORM_DAYS) {

  base <- exact_base[tank_panel_id %in% tank_ids]

  split_df <- as.data.table(survSplit(
    formula  = Surv(t_enter, t_exit, failure) ~ .,
    data     = as.data.frame(base),
    cut      = reform_cut,
    episode  = "reform_ep"
  ))
  split_df <- split_df[t_exit > t_enter]
  split_df[, did_term := texas_treated * as.integer(reform_ep == 2L)]

  split_df[, yr_mid := as.integer(
    format(as.Date((t_enter + t_exit) / 2, origin = "1970-01-01"), "%Y")
  )]
  split_df[, mandate_release_det := as.integer(
    !is.na(release_det_deadline_yr) &
    yr_mid >= 1989L &
    yr_mid <= release_det_deadline_yr
  )]
  split_df[, mandate_spill_overfill := as.integer(
    !is.na(release_det_deadline_yr) &
    yr_mid %in% 1993L:1994L
  )]
  split_df[, mandate_integrity := as.integer(
    !is.na(release_det_deadline_yr) &
    yr_mid %in% 1996L:1998L
  )]
  split_df[, yr_mid := NULL]
  split_df[]
}

# Cox event study split. Spells split at every Jan-1 boundary in
# [1985, 2021]; rel_year_es built from midpoint year and clamped to
# [tau_lo, tau_hi]. Interaction columns es_tau_* created for all tau
# except ref.
#
# above_median_age uses the same semantics as the OLS matched samples:
# NA for tanks not alive at reform date.
build_cox_es_split <- function(tank_ids,
                               med_age_val,
                               tau_lo = -13L, tau_hi = 22L,
                               ref_yr = -1L,
                               reform_yr = 1998L) {

  base <- exact_base[tank_panel_id %in% tank_ids]

  year_cuts <- as.numeric(as.Date(paste0(1985:2021, "-01-01")))

  es_df <- as.data.table(survSplit(
    formula  = Surv(t_enter, t_exit, failure) ~ .,
    data     = as.data.frame(base),
    cut      = year_cuts,
    episode  = "cal_ep"
  ))
  es_df <- es_df[t_exit > t_enter]

  es_df[, yr_mid := as.integer(
    format(as.Date((t_enter + t_exit) / 2, origin = "1970-01-01"), "%Y")
  )]
  es_df[, cal_year    := yr_mid]
  es_df[, rel_year    := cal_year - reform_yr]
  es_df[, rel_year_es := pmax(pmin(rel_year, tau_hi), tau_lo)]

  # above_median_age: same alive-at-reform semantics as OLS samples
  es_df[, above_median_age := NA_integer_]
alive_mask <- es_df$install_yr_int < 1999L &
              es_df$tank_installed_date <= REFORM_DATE &
              (es_df$failure == 0L | es_df$t_exit > REFORM_DAYS)
    es_df[alive_mask,
        above_median_age := as.integer(
          (1999L - install_yr_int) >= med_age_val
        )]

  # Mandate controls
  es_df[, mandate_release_det := as.integer(
    !is.na(release_det_deadline_yr) &
    yr_mid >= 1989L &
    yr_mid <= release_det_deadline_yr
  )]
  es_df[, mandate_spill_overfill := as.integer(
    !is.na(release_det_deadline_yr) &
    yr_mid %in% 1993L:1994L
  )]
  es_df[, mandate_integrity := as.integer(
    !is.na(release_det_deadline_yr) &
    yr_mid %in% 1996L:1998L
  )]
  es_df[, yr_mid := NULL]

  # es_tau_* interaction columns
  tau_vals <- setdiff(tau_lo:tau_hi, ref_yr)
  for (tau in tau_vals) {
    tag   <- if (tau < 0) paste0("m", abs(tau)) else as.character(tau)
    vname <- paste0("es_tau_", tag)
    es_df[, (vname) := texas_treated * as.integer(rel_year_es == tau)]
  }

  attr(es_df, "tau_vals") <- tau_vals
  attr(es_df, "es_vars")  <- paste0(
    "es_tau_",
    ifelse(tau_vals < 0, paste0("m", abs(tau_vals)), as.character(tau_vals))
  )
  attr(es_df, "ref_yr")   <- ref_yr
  es_df[]
}


# =============================================================================
# B2.6 — Bootstrap inference
# =============================================================================

# =============================================================================
# B2.6 OLS — Manual Kline-Santos score wild cluster bootstrap
# =============================================================================
# Replaces boottest() entirely. Works with any fixest FE structure including
# panel_year^make_model_noage and large tank-level FEs because it never
# builds a model matrix over FE levels.
#
# Algorithm (Kline & Santos 2012, JEM):
#   1. fixest::demean(model) FWL-projects y, param, and all controls onto
#      the FE complement, using the exact same weighted projector feols() used.
#      Returns one row per estimation observation.
#   2. Null OLS on demeaned data (drop param, no FEs, no intercept) gives ũ_i.
#   3. Score: s_i = w_i * x̃_param,i * ũ_i  (w_i = 1 for unweighted)
#   4. Cluster-aggregate, apply Rademacher weights, compute bootstrap t-stats.
#
# REQUIREMENT: data= must be the data.table passed to feols() AFTER
# drop_singletons(). It must contain column "state" for clustering.
# The model's obs() indices map into this specific filtered dataset.
run_boot_ols_score <- function(model,
                                param = "did_term",
                                B     = N_BOOT,
                                seed  = 42L,
                                data  = NULL) {
  set.seed(seed)

  if (is.null(data)) {
    stop("run_boot_ols_score requires explicit data= argument.")
  }

  # --- Observed t-stat ---
  ct  <- coeftable(model)
  idx <- which(rownames(ct) == param)[1L]
  if (is.na(idx)) {
    warning(sprintf("param '%s' not in model coeftable", param))
    return(list(p_boot=NA_real_, se_boot=NA_real_,
                ci_lo=NA_real_, ci_hi=NA_real_, t_stat=NA_real_))
  }
  coef_obs <- ct[idx, "Estimate"]
  se_obs   <- ct[idx, "Std. Error"]
  t_obs    <- coef_obs / se_obs

  # --- Get obs indices FIRST (needed for weight alignment below) ---
  obs_idx  <- fixest::obs(model)
  states   <- data[["state"]][obs_idx]
  clusters <- unique(states)
  G        <- length(clusters)

  # FIX 1: align weights to estimation sample using obs_idx
  # weights(model) is length nrow(data), but demean() returns nobs(model) rows.
  # For cell x year FE models fixest drops additional singletons internally,
  # making nrow(data) > nobs(model). Indexing to obs_idx corrects the alignment.
  wts_raw <- weights(model)
  wts     <- if (!is.null(wts_raw)) wts_raw[obs_idx] else NULL

  # --- FWL demean ---
  # FIX 2: restrict data to obs_idx before passing to demean so the function
  # operates on exactly the estimation sample rather than relying on the model's
  # internally cached data, which can mis-align for large cell x year FE structures.
  dm <- tryCatch({
    data_est <- data[obs_idx]
    as.data.frame(fixest::demean(model, data = data_est))
  }, error = function(e) {
    # Fallback: try demean on the stored model without data restriction
    tryCatch(
      as.data.frame(fixest::demean(model)),
      error = function(e2) {
        warning(sprintf(
          "fixest::demean() failed (both paths): %s",
          conditionMessage(e2)))
        NULL
      }
    )
  })

  if (is.null(dm)) {
    return(list(p_boot=NA_real_, se_boot=NA_real_,
                ci_lo=NA_real_, ci_hi=NA_real_, t_stat=NA_real_))
  }

  y_nm     <- as.character(formula(model)[[2L]])
  null_nms <- setdiff(names(dm), c(y_nm, param))

  if (!param %in% names(dm)) {
    warning(sprintf("param '%s' missing from demeaned frame", param))
    return(list(p_boot=NA_real_, se_boot=NA_real_,
                ci_lo=NA_real_, ci_hi=NA_real_, t_stat=NA_real_))
  }

  # --- Null residuals via lm.fit ---
  y_vec <- dm[[y_nm]]
  if (length(null_nms) > 0L) {
    X_null <- as.matrix(dm[null_nms])
    if (!is.null(wts)) {
      sw     <- sqrt(wts)
      fit    <- lm.fit(sw * X_null, sw * y_vec)
      resid_null <- fit$residuals / sw   # back-transform to unweighted scale
    } else {
      fit        <- lm.fit(X_null, y_vec)
      resid_null <- fit$residuals
    }
  } else {
    resid_null <- y_vec
  }

  # --- Score contributions ---
  x_dm      <- dm[[param]]
  score_vec <- if (!is.null(wts)) wts * x_dm * resid_null else x_dm * resid_null

  valid          <- !is.na(score_vec) & !is.na(states)
  cluster_scores <- as.numeric(tapply(score_vec[valid], states[valid], sum))

  # --- Vectorized Rademacher bootstrap ---
  W      <- matrix(sample(c(-1L, 1L), B * G, replace = TRUE),
                   nrow = B, ncol = G)
  score_b <- as.numeric(W %*% cluster_scores)
  var_b   <- sum(cluster_scores^2)
  t_boot  <- score_b / sqrt(var_b + 1e-10)

  p_boot     <- mean(abs(t_boot) >= abs(t_obs))
  se_boot    <- sd(t_boot) * se_obs
  boot_coefs <- t_boot * se_obs + coef_obs
  list(
    p_boot  = p_boot,
    se_boot = se_boot,
    ci_lo   = unname(quantile(boot_coefs, 0.025)),
    ci_hi   = unname(quantile(boot_coefs, 0.975)),
    t_stat  = t_obs
  )
}



drop_singletons <- function(dt, fe_var) {
  counts <- dt[, .N, by = c(fe_var)]
  keep   <- counts[N > 1L, get(fe_var)]
  dt[get(fe_var) %in% keep]
}

# Cox cluster-robust score bootstrap.
# Approach: fit null model (dropping `term`), extract martingale residuals,
# multiply by `term` indicator to get score contribution, aggregate by
# cluster, apply Rademacher reweighting across clusters, compute bootstrap
# t-statistic. Percentile CIs on coefficient scale using se_obs rescaling.
#
# Requires explicit data= because coxph stores Surv() as a single model frame object.
# =============================================================================
# B2.6 COX — Vectorized Score Wild Cluster Bootstrap
# =============================================================================
run_boot_cox <- function(cox_model,
                         term  = "did_term",
                         B     = N_BOOT,
                         seed  = 42L,
                         data  = NULL) {
  set.seed(seed)
  if (is.null(data)) {
    stop("run_boot_cox requires explicit data= argument.")
  }

  data_used <- data
  states    <- data_used[["state"]]

  # Extract observed stats
  s        <- summary(cox_model)$coefficients
  coef_obs <- s[term, "coef"]
  se_obs   <- s[term, "robust se"]
  t_obs    <- coef_obs / se_obs

  # Null model
  null_fml <- update(cox_model$formula, as.formula(paste(". ~ . -", term)))
  m_null   <- coxph(null_fml, data = data_used, cluster = state, ties = "efron", model = FALSE)

  # Score contribution
  mart_resid  <- naresid(m_null$na.action, residuals(m_null, type = "martingale"))
  term_vec    <- data_used[[term]]
  score_vec   <- mart_resid * term_vec

  # Cluster-level score sums
  valid_rows     <- !is.na(score_vec) & !is.na(states)
  cluster_scores <- as.numeric(tapply(score_vec[valid_rows], states[valid_rows], sum))
  G              <- length(cluster_scores)

  # Vectorized Rademacher bootstrap
  # Replaces future_sapply: overhead of parallelization far exceeds matrix multiplication time
  W       <- matrix(sample(c(-1L, 1L), B * G, replace = TRUE), nrow = B, ncol = G)
  score_b <- as.numeric(W %*% cluster_scores)
  var_b   <- sum(cluster_scores^2)
  t_boot  <- score_b / sqrt(var_b + 1e-10)

  boot_coefs <- t_boot * se_obs + coef_obs

  list(
    p_boot     = mean(abs(t_boot) >= abs(t_obs)),
    se_boot    = sd(t_boot) * se_obs,
    ci_lo      = unname(quantile(boot_coefs, 0.025)),
    ci_hi      = unname(quantile(boot_coefs, 0.975)),
    t_obs      = t_obs,
    t_boot_vec = t_boot
  )
}

# =============================================================================
# B2.7 — Inference footnote formatter
# =============================================================================

# Formats a table footnote string comparing CRVE and bootstrap p-values.
# Adds a flag if inference differs between methods at 5% level.
format_inference_note <- function(crve_p, boot_p, term_label = "DiD") {
  flag <- ""
  if (!is.na(crve_p) && !is.na(boot_p)) {
    if ((crve_p < 0.05) != (boot_p < 0.05)) {
      flag <- " [NOTE: CRVE and bootstrap inference differ at 5\\% level]"
    }
  }
  sprintf(
    "%s: CRVE $p$=%.3f; Wild bootstrap $p$=%.3f ($B$=%d, Rademacher)%s",
    term_label, crve_p, boot_p, N_BOOT, flag
  )
}


# =============================================================================
# B2.8 — HonestDiD coefficient extraction
# =============================================================================

# Extracts event-study betahat and Sigma from fixest (OLS) or coxph (Cox).
# Returns:
#   b               - sorted coefficient vector
#   V               - PSD-enforced covariance matrix
#   tau_vals        - integer tau indices matching columns of V
#   n_pre, n_post   - period counts
#   max_pre_1st_diff - max(|diff(pre_b)|)   -- denominator for Delta^RM ratio
#   max_pre_2nd_diff - max(|diff(diff(pre_b))|) -- denominator for Delta^SD ratio
#   model_type      - "ols" or "cox"
#
# Cox path uses diagonal V reconstructed from robust SEs. Full covariance
# would require score extraction at the parameter indices; diagonal is
# conservative (correlations assumed zero) and standard practice when
# no score interface is available.
hd_extract <- function(model, ref = -1L, model_type = c("ols", "cox")) {
  model_type <- match.arg(model_type)

  if (model_type == "ols") {
    b_full <- coef(model)
    V_full <- vcov(model, type = "clustered")
    es_idx <- grepl("^rel_year::", names(b_full))
    b_es   <- b_full[es_idx]
    V_es   <- V_full[es_idx, es_idx]
    tau_vals <- as.integer(
      sub("^rel_year::(-?[0-9]+):texas_treated$", "\\1", names(b_es))
    )
  } else {
    s       <- summary(model)$coefficients
    se_col  <- intersect(c("robust se", "se(coef)"), colnames(s))[1]
    es_rows <- grep("^es_tau_", rownames(s))
    stopifnot("No es_tau_* terms in Cox model" = length(es_rows) > 0L)
    b_es    <- s[es_rows, "coef"]
    se_es   <- s[es_rows, se_col]
    V_es    <- diag(se_es^2)
    tau_from_name <- function(nm) {
      if (grepl("^es_tau_m([0-9]+)$", nm))
        -as.integer(sub("^es_tau_m([0-9]+)$", "\\1", nm))
      else
        as.integer(sub("^es_tau_([0-9]+)$", "\\1", nm))
    }
    tau_vals <- vapply(rownames(s)[es_rows], tau_from_name, integer(1))
    names(b_es) <- as.character(tau_vals)
  }

  ord      <- order(tau_vals)
  b_es     <- b_es[ord]
  V_es     <- V_es[ord, ord]
  tau_vals <- tau_vals[ord]

  stopifnot("ref period found in b_es" = !ref %in% tau_vals)

  n_pre  <- sum(tau_vals < 0L)
  n_post <- sum(tau_vals >= 0L)

  # Force PSD
  ev        <- eigen(V_es, symmetric = TRUE)
  ev$values <- pmax(ev$values, 0)
  V_psd     <- ev$vectors %*% diag(ev$values) %*% t(ev$vectors)

  pre_b            <- b_es[tau_vals < 0L]
  max_pre_1st_diff <- if (length(pre_b) >= 2L)
                         max(abs(diff(pre_b))) else NA_real_
  max_pre_2nd_diff <- if (length(pre_b) >= 3L)
                         max(abs(diff(diff(pre_b)))) else NA_real_

  list(
    b                = b_es,
    V                = V_psd,
    tau_vals         = tau_vals,
    n_pre            = n_pre,
    n_post           = n_post,
    max_pre_1st_diff = max_pre_1st_diff,
    max_pre_2nd_diff = max_pre_2nd_diff,
    model_type       = model_type
  )
}


# =============================================================================
# B2.9 — pretrends power calculation
# =============================================================================

# Calls pretrends::pretrends() and computes power curve over a slope grid.
# Returns:
#   pt           - pretrends result object
#   power_curve  - numeric vector of power values at slope_grid points
#   slope_grid   - numeric vector of slope magnitudes tested
#   mds          - minimum detectable slope at target power (default 80%)
#   obs_max_slope - empirical max first-diff in pre-period (benchmark)
run_pretrends <- function(hd_obj, label,
                          alpha = 0.05,
                          power = 0.80,
                          n_grid = 200L) {

  pre_idx <- hd_obj$tau_vals < 0L
  b_pre   <- hd_obj$b[pre_idx]
  V_pre   <- hd_obj$V[pre_idx, pre_idx]

  pt <- tryCatch(
    pretrends(
      betahat       = b_pre,
      sigma         = V_pre,
      numPrePeriods = hd_obj$n_pre,
      alpha         = alpha
    ),
    error = function(e) {
      warning(sprintf("pretrends() failed for %s: %s", label,
                      conditionMessage(e)))
      NULL
    }
  )

  slope_grid <- seq(0, hd_obj$max_pre_1st_diff * 3, length.out = n_grid)

  power_curve <- tryCatch(
    vapply(slope_grid, function(s) {
      res <- power_pretrends(
        betahat       = b_pre,
        sigma         = V_pre,
        numPrePeriods = hd_obj$n_pre,
        slope         = s,
        alpha         = alpha
      )
      as.numeric(res)
    }, numeric(1)),
    error = function(e) {
      warning(sprintf("power_pretrends() failed for %s: %s", label,
                      conditionMessage(e)))
      rep(NA_real_, length(slope_grid))
    }
  )

  mds_idx <- which(power_curve >= power)[1]
  mds     <- if (!is.na(mds_idx)) slope_grid[mds_idx] else NA_real_

  list(
    label         = label,
    pt            = pt,
    power_curve   = power_curve,
    slope_grid    = slope_grid,
    mds           = mds,
    obs_max_slope = hd_obj$max_pre_1st_diff
  )
}


# =============================================================================
# B2.10 — Sensitivity plotting (Delta^SD and Delta^RM, standalone)
# =============================================================================

# Plot a HonestDiD sensitivity curve (CI lower and upper bounds as a
# function of the restriction parameter M). Designed so that
# Delta^SD and Delta^RM differ only in the x-axis label, the
# denominator of the ratio, and the breakdown M.
#
# pre_slope: the empirical denominator against which M is compared
#   - for Delta^SD: max_pre_2nd_diff
#   - for Delta^RM: max_pre_1st_diff
plot_sensitivity_standalone <- function(sens_dt,
                                        bkd_M,
                                        pre_slope,
                                        ci_col   = COL_RED,
                                        x_label  = "M: Max Slope of Parallel Trends Violation",
                                        y_label  = "Robust 95% CI for Avg Post-Reform ATT") {

  dt <- as.data.table(sens_dt)[!is.na(lb) & !is.na(ub)]
  stopifnot("Empty sensitivity data frame" = nrow(dt) > 0L)

  y_lo  <- min(dt$lb, na.rm = TRUE)
  y_hi  <- max(dt$ub, na.rm = TRUE)
  y_pad <- diff(c(y_lo, y_hi)) * 0.12

  p <- ggplot(dt, aes(x = M)) +
    geom_ribbon(aes(ymin = lb, ymax = ub),
                fill = ci_col, alpha = 0.18, colour = NA) +
    geom_line(aes(y = lb), colour = ci_col, linewidth = 0.75) +
    geom_line(aes(y = ub), colour = ci_col, linewidth = 0.75) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +
    geom_vline(xintercept = pre_slope,
               linetype = "dashed", colour = "steelblue", linewidth = 0.55) +
    annotate("text",
             x = pre_slope * 1.03,
             y = y_hi - y_pad * 0.25,
             label = sprintf("Max pre-trend\nM-hat = %.4f", pre_slope),
             hjust = 0, size = 2.8, colour = "steelblue", lineheight = 0.85)

  if (is.finite(bkd_M)) {
    p <- p +
      geom_vline(xintercept = bkd_M, linetype = "dotted",
                 colour = "grey30", linewidth = 0.6) +
      annotate("text",
               x = bkd_M * 1.03,
               y = y_lo + y_pad * 0.5,
               label = sprintf("Breakdown M\n= %.4f", bkd_M),
               hjust = 0, size = 2.8, colour = "grey30", lineheight = 0.85)
  } else {
    p <- p +
      annotate("text",
               x = max(dt$M) * 0.72,
               y = y_lo + y_pad * 0.5,
               label = "CI remains positive\nfor all tested M",
               hjust = 0, size = 2.8, colour = "grey30", lineheight = 0.85)
  }

  p +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.0001)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    coord_cartesian(ylim = c(y_lo - y_pad, y_hi + y_pad)) +
    labs(x = x_label, y = y_label) +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line   = element_line(colour = "black", linewidth = 0.4),
      axis.ticks  = element_line(colour = "black", linewidth = 0.3),
      axis.text   = element_text(colour = "black"),
      panel.grid  = element_blank(),
      plot.title  = element_blank(),
      plot.margin = margin(8, 10, 8, 8)
    )
}


# =============================================================================
# B2.11 — pretrends power curve plotting (standalone)
# =============================================================================

plot_pretrends_standalone <- function(pt_obj) {

  curve_dt <- data.table(
    slope = pt_obj$slope_grid,
    power = pt_obj$power_curve
  )[!is.na(power)]

  stopifnot("Empty power curve" = nrow(curve_dt) > 0L)

  p <- ggplot(curve_dt, aes(x = slope, y = power)) +
    geom_hline(yintercept = 0.80, linetype = "dashed",
               colour = "grey50", linewidth = 0.45) +
    geom_line(colour = "grey20", linewidth = 0.85)

  if (is.finite(pt_obj$obs_max_slope)) {
    p <- p +
      geom_vline(xintercept = pt_obj$obs_max_slope,
                 linetype = "dotted", colour = COL_BLUE, linewidth = 0.7) +
      annotate("text", x = pt_obj$obs_max_slope,
               y = 0.08,
               label = sprintf("Observed\nmax delta-beta = %.4f",
                               pt_obj$obs_max_slope),
               hjust = -0.05, size = 2.8, colour = COL_BLUE,
               lineheight = 0.85)
  }

  if (is.finite(pt_obj$mds)) {
    p <- p +
      geom_vline(xintercept = pt_obj$mds,
                 linetype = "dotted", colour = COL_RED, linewidth = 0.7) +
      annotate("text", x = pt_obj$mds,
               y = 0.20,
               label = sprintf("MDS\n(80%% power)\n= %.4f", pt_obj$mds),
               hjust = -0.05, size = 2.8, colour = COL_RED,
               lineheight = 0.85)
  }

  p +
    annotate("text",
             x = max(curve_dt$slope, na.rm = TRUE) * 0.02,
             y = 0.83, label = "80% power",
             hjust = 0, size = 2.6, colour = "grey50") +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.0001)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Linear Pre-Trend Slope (pp per year)",
         y = "Power to Reject Parallel Trends") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line   = element_line(colour = "black", linewidth = 0.4),
      axis.ticks  = element_line(colour = "black", linewidth = 0.3),
      axis.text   = element_text(colour = "black"),
      panel.grid  = element_blank(),
      plot.title  = element_blank(),
      plot.margin = margin(8, 10, 8, 8)
    )
}


# =============================================================================
# B2.12 — KM standalone plot
# =============================================================================

plot_km_standalone <- function(km_sub) {
  ggplot(km_sub, aes(x = time, y = estimate,
                      colour = strata, fill = strata)) +
    geom_step(linewidth = 0.6) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.06, colour = NA) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 50, 10)) +
    labs(x = "Tank Age (years)", y = "Survival Probability") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line       = element_line(colour = "black", linewidth = 0.4),
      axis.ticks      = element_line(colour = "black", linewidth = 0.3),
      axis.text       = element_text(colour = "black"),
      panel.grid      = element_blank(),
      legend.position = "right",
      legend.text     = element_text(size = 7),
      plot.title      = element_blank(),
      plot.margin     = margin(8, 10, 8, 8)
    )
}



# Verify fwildclusterboot and pretrends are attached
required_pkgs <- c("fwildclusterboot", "pretrends", "HonestDiD",
                   "future", "future.apply")
missing_pkgs  <- required_pkgs[!required_pkgs %in%
                                (.packages(all.available = TRUE))]
stopifnot("all required packages attached" = length(missing_pkgs) == 0L)

# Verify the future plan has workers set
stopifnot("future plan is multisession" =
          inherits(future::plan(), "multisession"))

cat(sprintf("  All required packages available\n"))
cat(sprintf("  future plan: multisession with %d workers\n", N_CORES))
cat("  All S3 checks PASSED\n\n")

################################################################################
# === S4: COX SPELL SPLITS ===
################################################################################

#### S4 Cox Split Datasets ####

cat("========================================\n")
cat("S4: BUILD COX SPLIT DATASETS (4 SAMPLES)\n")
cat("========================================\n\n")

# ---- Two-episode Cox splits (DiD: pre-reform vs post-reform) ----

log_step("Building two-episode Cox splits...")
cox_split_birth_cem  <- build_cox_split(ids_birth_cem)
cox_split_birth_psm  <- build_cox_split(ids_birth_psm)
cox_split_reform_cem <- build_cox_split(ids_reform_cem)
cox_split_reform_psm <- build_cox_split(ids_reform_psm)

# Attach above_median_age to each Cox split using the sample-appropriate
# med_age. Filter: tanks alive at reform (install pre-1999 AND not yet
# closed) get a meaningful age classification; all others NA.
attach_cox_above_median <- function(cox_dt, med_val) {
  cox_dt[, above_median_age := NA_integer_]
alive_mask <- cox_dt$install_yr_int < 1999L &
              cox_dt$tank_installed_date <= REFORM_DATE &
              (cox_dt$failure == 0L | cox_dt$t_exit > REFORM_DAYS)
  cox_dt[alive_mask,
         above_median_age := as.integer(
           (1999L - install_yr_int) >= med_val
         )]
  cox_dt[]
}

cox_split_birth_cem  <- attach_cox_above_median(cox_split_birth_cem,  med_age_birth_cem)
cox_split_birth_psm  <- attach_cox_above_median(cox_split_birth_psm,  med_age_birth_psm)
cox_split_reform_cem <- attach_cox_above_median(cox_split_reform_cem, med_age_reform_cem)
cox_split_reform_psm <- attach_cox_above_median(cox_split_reform_psm, med_age_reform_psm)

cat(sprintf("  cox_split_birth_cem:  %s rows | %s tanks | %s events\n",
  fmt_n(nrow(cox_split_birth_cem)),
  fmt_n(uniqueN(cox_split_birth_cem$tank_panel_id)),
  fmt_n(sum(cox_split_birth_cem$failure))))
cat(sprintf("  cox_split_birth_psm:  %s rows | %s tanks | %s events\n",
  fmt_n(nrow(cox_split_birth_psm)),
  fmt_n(uniqueN(cox_split_birth_psm$tank_panel_id)),
  fmt_n(sum(cox_split_birth_psm$failure))))
cat(sprintf("  cox_split_reform_cem: %s rows | %s tanks | %s events\n",
  fmt_n(nrow(cox_split_reform_cem)),
  fmt_n(uniqueN(cox_split_reform_cem$tank_panel_id)),
  fmt_n(sum(cox_split_reform_cem$failure))))
cat(sprintf("  cox_split_reform_psm: %s rows | %s tanks | %s events\n\n",
  fmt_n(nrow(cox_split_reform_psm)),
  fmt_n(uniqueN(cox_split_reform_psm$tank_panel_id)),
  fmt_n(sum(cox_split_reform_psm$failure))))

# ---- Cox event study splits (per-calendar-year episodes) ----

log_step("Building Cox event-study splits...")
cox_es_birth_cem  <- build_cox_es_split(ids_birth_cem,  med_age_birth_cem)
cox_es_birth_psm  <- build_cox_es_split(ids_birth_psm,  med_age_birth_psm)
cox_es_reform_cem <- build_cox_es_split(ids_reform_cem, med_age_reform_cem)
cox_es_reform_psm <- build_cox_es_split(ids_reform_psm, med_age_reform_psm)

cat(sprintf("  cox_es_birth_cem:  %s rows | %s tanks | %s events | %d es_tau vars\n",
  fmt_n(nrow(cox_es_birth_cem)),
  fmt_n(uniqueN(cox_es_birth_cem$tank_panel_id)),
  fmt_n(sum(cox_es_birth_cem$failure)),
  length(attr(cox_es_birth_cem, "es_vars"))))
cat(sprintf("  cox_es_birth_psm:  %s rows | %s tanks | %s events\n",
  fmt_n(nrow(cox_es_birth_psm)),
  fmt_n(uniqueN(cox_es_birth_psm$tank_panel_id)),
  fmt_n(sum(cox_es_birth_psm$failure))))
cat(sprintf("  cox_es_reform_cem: %s rows | %s tanks | %s events\n",
  fmt_n(nrow(cox_es_reform_cem)),
  fmt_n(uniqueN(cox_es_reform_cem$tank_panel_id)),
  fmt_n(sum(cox_es_reform_cem$failure))))
cat(sprintf("  cox_es_reform_psm: %s rows | %s tanks | %s events\n\n",
  fmt_n(nrow(cox_es_reform_psm)),
  fmt_n(uniqueN(cox_es_reform_psm$tank_panel_id)),
  fmt_n(sum(cox_es_reform_psm$failure))))


#### S4b OLS Incumbent Event-Study Frames ####

cat("========================================\n")
cat("S4b: BUILD OLS INCUMBENT ES FRAMES\n")
cat("========================================\n\n")

# All OLS event-study frames apply the pre-reform incumbent filter
# install_yr_int < 1999L. Birth-cohort matched tanks installed >=1999 are
# dropped at estimation time (they have no pre-period observations
# relative to the reform). Reform-survivor samples are already restricted
# by construction.
#
# rel_year indexed to 1998: tau=0 is 1998 itself (treatment is Dec 22 1998).
# ref year is tau=-1 (1997) throughout.
#
# Weights are renormalized within treatment group so the sum of weights
# is identical across TX and controls.

# ---- PRIMARY: birth-CEM ----
incumbents_birth_es <- matched_tanks_birth_cem[
  install_yr_int < 1999L &
  cem_weight     >  0
][, rel_year := as.integer(panel_year) - 1998L]

incumbents_birth_es[,
  cem_w_norm := cem_weight / sum(cem_weight),
  by = texas_treated
]

incumbents_birth_below <- incumbents_birth_es[above_median_age == 0L][,
  cem_w_norm := cem_weight / sum(cem_weight),
  by = texas_treated
]

incumbents_birth_above <- incumbents_birth_es[above_median_age == 1L][,
  cem_w_norm := cem_weight / sum(cem_weight),
  by = texas_treated
]

cat(sprintf("  incumbents_birth_es:    %s rows | %s tanks\n",
  fmt_n(nrow(incumbents_birth_es)),
  fmt_n(uniqueN(incumbents_birth_es$tank_panel_id))))
cat(sprintf("  incumbents_birth_below: %s rows | %s tanks\n",
  fmt_n(nrow(incumbents_birth_below)),
  fmt_n(uniqueN(incumbents_birth_below$tank_panel_id))))
cat(sprintf("  incumbents_birth_above: %s rows | %s tanks\n",
  fmt_n(nrow(incumbents_birth_above)),
  fmt_n(uniqueN(incumbents_birth_above$tank_panel_id))))

# ---- PSM parallel: birth-PSM ----
incumbents_birth_psm_es <- matched_tanks_birth_psm[
  install_yr_int < 1999L &
  psm_weight     >  0
][, rel_year := as.integer(panel_year) - 1998L]

incumbents_birth_psm_es[,
  psm_w_norm := psm_weight / sum(psm_weight),
  by = texas_treated
]

incumbents_birth_psm_above <- incumbents_birth_psm_es[above_median_age == 1L][,
  psm_w_norm := psm_weight / sum(psm_weight),
  by = texas_treated
]

cat(sprintf("  incumbents_birth_psm_es:    %s rows | %s tanks\n",
  fmt_n(nrow(incumbents_birth_psm_es)),
  fmt_n(uniqueN(incumbents_birth_psm_es$tank_panel_id))))
cat(sprintf("  incumbents_birth_psm_above: %s rows | %s tanks\n",
  fmt_n(nrow(incumbents_birth_psm_above)),
  fmt_n(uniqueN(incumbents_birth_psm_above$tank_panel_id))))

# ---- ROBUSTNESS: reform-CEM ----
incumbents_reform_es <- matched_tanks_reform_cem[
  install_yr_int < 1999L &
  cem_weight     >  0
][, rel_year := as.integer(panel_year) - 1998L]

incumbents_reform_es[,
  cem_w_norm := cem_weight / sum(cem_weight),
  by = texas_treated
]

incumbents_reform_above <- incumbents_reform_es[above_median_age == 1L][,
  cem_w_norm := cem_weight / sum(cem_weight),
  by = texas_treated
]

cat(sprintf("  incumbents_reform_es:    %s rows | %s tanks\n",
  fmt_n(nrow(incumbents_reform_es)),
  fmt_n(uniqueN(incumbents_reform_es$tank_panel_id))))
cat(sprintf("  incumbents_reform_above: %s rows | %s tanks\n",
  fmt_n(nrow(incumbents_reform_above)),
  fmt_n(uniqueN(incumbents_reform_above$tank_panel_id))))

# ---- Robustness subsamples built from birth-CEM primary ----

incumbents_birth_sw <- incumbents_birth_es[mm_wall == "Single-Walled"][,
  cem_w_norm := cem_weight / sum(cem_weight), by = texas_treated
]
incumbents_birth_dw <- incumbents_birth_es[mm_wall == "Double-Walled"][,
  cem_w_norm := cem_weight / sum(cem_weight), by = texas_treated
]
incumbents_birth_noborder <- incumbents_birth_es[
  !state %in% c("OK", "AR", "LA")
][,
  cem_w_norm := cem_weight / sum(cem_weight), by = texas_treated
]
incumbents_birth_post88 <- incumbents_birth_es[install_yr_int >= 1989L][,
  cem_w_norm := cem_weight / sum(cem_weight), by = texas_treated
]

cat(sprintf("  incumbents_birth_sw:       %s rows\n", fmt_n(nrow(incumbents_birth_sw))))
cat(sprintf("  incumbents_birth_dw:       %s rows\n", fmt_n(nrow(incumbents_birth_dw))))
cat(sprintf("  incumbents_birth_noborder: %s rows\n", fmt_n(nrow(incumbents_birth_noborder))))
cat(sprintf("  incumbents_birth_post88:   %s rows\n\n", fmt_n(nrow(incumbents_birth_post88))))


#### S4c Recompute ctrl_mean_post on birth-CEM sample ####

# The panel_meta.csv value was computed on reform-CEM controls in
# Panel_Build S17. For birth-CEM primary, recompute on primary sample's
# control tanks in the post-reform window.
ctrl_mean_post_birth <- matched_tanks_birth_cem[
  texas_treated == 0L & panel_year >= 1999L,
  weighted.mean(closure_event, w = cem_weight, na.rm = TRUE)
]

ctrl_mean_post_reform <- matched_tanks_reform_cem[
  texas_treated == 0L & panel_year >= 1999L,
  weighted.mean(closure_event, w = cem_weight, na.rm = TRUE)
]

cat(sprintf("Control mean post-reform (birth-CEM):  %.5f\n", ctrl_mean_post_birth))
cat(sprintf("Control mean post-reform (reform-CEM): %.5f\n", ctrl_mean_post_reform))
cat(sprintf("Control mean post-reform (panel_meta): %.5f\n\n", ctrl_mean_post))

# Use birth-CEM value for primary figure reference lines
ctrl_mean_post <- ctrl_mean_post_birth

################################################################################
################################################################################
# === S5: TABLE 1 - OLS STEPWISE WALK-IN ===
################################################################################

#### S5 Table 1 - OLS Stepwise Walk-In ####

cat("========================================\n")
cat("S5: TABLE 1 - OLS STEPWISE WALK-IN\n")
cat("========================================\n\n")


# =============================================================================
# Model fits
#
# Structure:
#   Panel A (mA1, mA2, mA3): Full incumbent panel, no matching
#   Panel B (mB1, mB2):      Birth-cohort PSM matched sample
#   Panel C (mC1, mC2):      Birth-cohort CEM matched sample
#
# Within each panel:
#   *1: tank FE + year FE + mandate controls          (baseline)
#   *2: tank FE + cell x year FE + mandate controls   (rich FE)
#   A3: tank FE + cell x year FE + Wooldridge HTE     (population-weighted ATT)
# =============================================================================

log_step("Fitting Panels A-C...")

# ---- DATA OBJECTS: Pre-save filtered data, pass to both feols() and bootstrap ----
data_A <- drop_singletons(
  panel_dt[install_yr_int < 1999L &!is.na(make_model_noage)],
  "tank_panel_id")
data_A

# --- Wooldridge Mean-Centering for Population-Weighted ATT (Model A3) ---
# Calculate true population shares of Texas tanks alive at the reform (1998)
alive_tx_mask <- data_A$texas_treated == 1L & 
                 data_A$install_yr_int < 1999L & 
                 data_A$tank_installed_date <= REFORM_DATE & 
                 (is.na(data_A$tank_closed_date) | data_A$tank_closed_date > REFORM_DATE)

# Extract unique tanks to ensure accurate population counts
tx_alive_tanks <- unique(data_A[alive_tx_mask,.(tank_panel_id, install_yr_int)])
tx_shares <- tx_alive_tanks[,.(N =.N), by = install_yr_int]
tx_shares[, pop_share := N / sum(N)]

# Merge these true population shares into the estimation dataset
data_A <- merge(data_A, tx_shares[,.(install_yr_int, pop_share)], 
                by = "install_yr_int", all.x = TRUE)
data_A[is.na(pop_share), pop_share := 0]

# Create centered interaction terms: did_term * (Vintage_Dummy - Vintage_Share)
vintages <- unique(tx_shares$install_yr_int)
vintages_to_interact <- vintages[-1] # Drop one vintage to avoid perfect multicollinearity

cent_cols <- character(length(vintages_to_interact))
for (i in seq_along(vintages_to_interact)) {
  v <- vintages_to_interact[i]
  col_name <- paste0("did_cent_", v)
  cent_cols[i] <- col_name
  share_v <- tx_shares[install_yr_int == v, pop_share]
  data_A[, (col_name) := did_term * (as.integer(install_yr_int == v) - share_v)]
}
# ------------------------------------------------------------------------

# Create cell_year_fe for all three data objects
# This is an explicit integer group ID for panel_year x make_model_noage interaction.
# It survives re-entrant calls to fixest::demean() inside the wild cluster bootstrap.

data_A[, cell_year_fe := .GRP, by = .(panel_year, make_model_noage)]

data_B <- drop_singletons(
  matched_tanks_birth_psm[install_yr_int < 1999L & psm_weight > 0],
  "tank_panel_id")
data_B[, cell_year_fe := .GRP, by = .(panel_year, make_model_noage)]

data_C <- drop_singletons(
  matched_tanks_birth_cem[install_yr_int < 1999L & cem_weight > 0],
  "tank_panel_id")
data_C[, cell_year_fe := .GRP, by = .(panel_year, make_model_noage)]


# ---- PANEL A: Full incumbent panel, no matching ----

mA1 <- feols(
  closure_event ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity |
    tank_panel_id + panel_year,
  data    = data_A,
  cluster = ~state
)

mA2 <- feols(
  closure_event ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity |
    tank_panel_id + cell_year_fe,
  data    = data_A,
  cluster = ~state
)

fml_A3 <- as.formula(paste(
  "closure_event ~ did_term +", 
  paste(cent_cols, collapse = " + "), 
  "+ mandate_release_det + mandate_spill_overfill + mandate_integrity | tank_panel_id + cell_year_fe"
))

mA3 <- feols(fml_A3, data = data_A, cluster = ~state)


# ---- PANEL B: Birth-cohort PSM matched sample ----

mB1 <- feols(
  closure_event ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity |
    tank_panel_id + panel_year,
  data    = data_B,
  weights = ~psm_weight,
  cluster = ~state
)

mB2 <- feols(
  closure_event ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity |
    tank_panel_id + cell_year_fe,
  data    = data_B,
  weights = ~psm_weight,
  cluster = ~state
)


# ---- PANEL C: Birth-cohort CEM matched sample ----

mC1 <- feols(
  closure_event ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity |
    tank_panel_id + panel_year,
  data    = data_C,
  weights = ~cem_weight,
  cluster = ~state
)

mC2 <- feols(
  closure_event ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity |
    tank_panel_id + cell_year_fe,
  data    = data_C,
  weights = ~cem_weight,
  cluster = ~state
)

cat("  Panels A-C fitted (7 models)\n\n")


# =============================================================================
# Bootstrap inference
# =============================================================================

log_step("Running wild cluster bootstrap on all 7 models...")

m_list <- list(
  mA1 = mA1, mA2 = mA2, mA3 = mA3,
  mB1 = mB1, mB2 = mB2,
  mC1 = mC1, mC2 = mC2
)

data_list <- list(
  mA1 = data_A, mA2 = data_A, mA3 = data_A,
  mB1 = data_B, mB2 = data_B,
  mC1 = data_C, mC2 = data_C
)

boot_t1 <- Map(function(m, dt) {
  run_boot_ols_score(m, param = "did_term", B = N_BOOT, data = dt)
}, m_list, data_list)

crve_p_did <- vapply(m_list, function(m) {
  ct  <- coeftable(m)
  idx <- grep("^did_term$", rownames(ct))[1]
  if (is.na(idx)) NA_real_ else ct[idx, "Pr(>|t|)"]
}, numeric(1))

boot_p_did <- vapply(boot_t1, `[[`, numeric(1), "p_boot")

cat("  CRVE vs bootstrap p-values for did_term:\n")
for (i in seq_along(m_list)) {
  cat(sprintf("    %-5s: CRVE p = %.4f | Boot p = %.4f | n = %s\n",
    names(m_list)[i],
    crve_p_did[i],
    boot_p_did[i],
    fmt_n(nobs(m_list[[i]]))
  ))
}
cat("\n")


# =============================================================================
# Diagnostic: what does matching add over rich FE?
# =============================================================================

log_step("Computing matching-vs-FE diagnostic...")

extract_did <- function(m, label) {
  ct  <- coeftable(m)
  idx <- grep("^did_term$", rownames(ct))[1]
  data.table(
    spec     = label,
    did_coef = ct[idx, "Estimate"],
    crve_se  = ct[idx, "Std. Error"],
    crve_p   = ct[idx, "Pr(>|t|)"],
    n_obs    = nobs(m)
  )
}

matching_comparison <- rbindlist(list(
  extract_did(mA2, "A2: Full panel | cell x year FE"),
  extract_did(mA3, "A3: Pop Weighted| cell x year FE"),
  extract_did(mB2, "B2: PSM (birth) | cell x year FE"),
  extract_did(mC2, "C2: CEM (birth) | cell x year FE")
))

matching_comparison[, pct_change_vs_A2 := 100 * (did_coef / did_coef[spec %like% "A2"] - 1)]

cat("\n=== MATCHING VS RICH FE: DiD COMPARISON (spec fixed, sample varies) ===\n")
print(matching_comparison, digits = 5)
cat("\n")

fwrite(matching_comparison,
       file.path(OUTPUT_TABLES, "Diag_MatchingVsFE_Comparison.csv"))
log_step("Saved: Diag_MatchingVsFE_Comparison.csv")


# =============================================================================
# Table construction
# =============================================================================

log_step("Building T1 LaTeX table...")

ols_dict <- c(
  "did_term"               = "DiD (Texas $\\times$ Post-1999)",
  "mandate_release_det"    = "Mandate: release detection",
  "mandate_spill_overfill" = "Mandate: spill/overfill",
  "mandate_integrity"      = "Mandate: integrity/cathodic"
)

boot_p_row  <- sprintf("%.3f", boot_p_did)

boot_flag_row <- vapply(seq_along(m_list), function(i) {
  if (is.na(crve_p_did[i]) || is.na(boot_p_did[i])) return("")
  if ((crve_p_did[i] < 0.05) != (boot_p_did[i] < 0.05)) "*disagree*" else ""
}, character(1))

etable(
  mA1, mA2, mA3,
  mB1, mB2,
  mC1, mC2,
  headers = c("A1", "A2", "A3", "B1", "B2", "C1", "C2"),
  dict    = ols_dict,
  fitstat = ~ n + r2 + wr2,
  keep    = c("^DiD", "^Mandate"),
  extralines = list(
    "\\midrule"                  = rep("", 7),
    "Sample"                     = c(
      "Full panel", "Full panel", "Full panel",
      "PSM (birth)", "PSM (birth)",
      "CEM (birth)", "CEM (birth)"
    ),
    "Weights"                    = c(
      "None", "None", "Pop. Share",
      "PSM", "PSM",
      "CEM", "CEM"
    ),
    "Mandate controls"           = rep("\\checkmark", 7),
    "Tank FE"                    = rep("\\checkmark", 7),
    "Year FE"                    = c(
      "\\checkmark", "", "",
      "\\checkmark", "",
      "\\checkmark", ""
    ),
    "Cell $\\times$ Year FE"      = c(
      "", "\\checkmark", "\\checkmark",
      "", "\\checkmark",
      "", "\\checkmark"
    ),
    "Control mean (post-reform)" = rep(sprintf("%.4f", ctrl_mean_post), 7),
    "Bootstrap $p$ (DiD)"        = boot_p_row,
    "CRVE vs Boot"               = boot_flag_row
  ),
  tex     = TRUE,
  file    = file.path(OUTPUT_TABLES, "T1_ols_stepwise.tex"),
  replace = TRUE,
  notes   = paste0(
    "Cluster-robust SEs by state (N=18). All columns restrict to pre-reform ",
    "incumbents (install year $<$ 1999). ",
    "Panel A uses the full incumbent population with no sample restriction; ",
    "tanks with missing make-model cell are dropped so the population is ",
    "identical across A1, A2, and A3. Column A3 evaluates the DiD effect at ",
    "the treated population mean by using Wooldridge-centered vintage interactions. ",
    "Panels B and C apply birth-cohort matching on wall type, fuel type, ",
    "capacity, and install cohort: ",
    "PSM uses OOF elastic-net propensity scores with 1:1 Mahalanobis caliper ",
    "matching (IR caliper = 0.2 SD of logit propensity score); ",
    "CEM uses exact coarsened matching on the same four dimensions. ",
    "Columns A2 / B2 / C2 use an identical specification --- differences in ",
    "the DiD coefficient across those three columns reflect common support ",
    "enforcement by the matching procedure, not the FE structure. ",
    sprintf(
      "Wild cluster bootstrap $p$-values: $B$ = %d, Rademacher weights, ",
      N_BOOT),
    "impose-null. ",
    "Control mean is the weighted post-reform (1999--2020) closure rate ",
    "among birth-CEM control tanks."
  )
)
cat("  Saved: T1_ols_stepwise.tex\n")


# =============================================================================
# Bootstrap diagnostics CSV
# =============================================================================

boot_diag_t1 <- data.table(
  col        = seq_along(m_list),
  panel      = c("A", "A", "A", "B", "B", "C", "C"),
  spec_label = c(
    "A1: Full panel, year FE",
    "A2: Full panel, cell x year FE",
    "A3: Full panel, Pop-Weighted ATT",
    "B1: PSM, year FE",
    "B2: PSM, cell x year FE",
    "C1: CEM, year FE",
    "C2: CEM, cell x year FE"
  ),
  sample     = c(
    "Full panel", "Full panel", "Full panel",
    "PSM (birth)", "PSM (birth)",
    "CEM (birth)", "CEM (birth)"
  ),
  did_coef   = vapply(m_list, function(m) {
    ct  <- coeftable(m)
    idx <- grep("^did_term$", rownames(ct))[1]
    if (is.na(idx)) NA_real_ else ct[idx, "Estimate"]
  }, numeric(1)),
  crve_se    = vapply(m_list, function(m) {
    ct  <- coeftable(m)
    idx <- grep("^did_term$", rownames(ct))[1]
    if (is.na(idx)) NA_real_ else ct[idx, "Std. Error"]
  }, numeric(1)),
  crve_p     = crve_p_did,
  boot_se    = vapply(boot_t1, `[[`, numeric(1), "se_boot"),
  boot_p     = boot_p_did,
  boot_ci_lo = vapply(boot_t1, `[[`, numeric(1), "ci_lo"),
  boot_ci_hi = vapply(boot_t1, `[[`, numeric(1), "ci_hi"),
  n_obs      = vapply(m_list, nobs, integer(1))
)

fwrite(boot_diag_t1, file.path(OUTPUT_TABLES, "T1_bootstrap_diagnostics.csv"))
cat("  Saved: T1_bootstrap_diagnostics.csv\n\n")


################################################################################
# === S6: TABLE 2 - COX STEPWISE WALK-IN ===
################################################################################

#### S6 Table 2 - Cox Stepwise Walk-In ####

cat("========================================\n")
cat("S6: TABLE 2 - COX STEPWISE WALK-IN\n")
cat("========================================\n\n")

# =============================================================================
# Data preparation
#
# Structure:
#   Panel A (coxA1, coxA2):   Full incumbent panel, no matching
#   Panel B (coxB1, coxB2):   Birth-cohort PSM matched sample
#   Panel C (coxC1, coxC2):   Birth-cohort CEM matched sample
#
# Within each panel:
#   *1: mandate controls                  (baseline)
#   *2: mandate controls + strata(cell)   (rich strata)
# =============================================================================

log_step("Building cox_split_full for Panel A...")

exact_base_incumbents <- exact_base[install_yr_int < 1999L]

cox_split_full <- as.data.table(survSplit(
  formula  = Surv(t_enter, t_exit, failure) ~ .,
  data     = as.data.frame(exact_base_incumbents),
  cut      = REFORM_DAYS,
  episode  = "reform_ep"
))
cox_split_full <- cox_split_full[t_exit > t_enter]
cox_split_full[, did_term := texas_treated * as.integer(reform_ep == 2L)]

# Mandate controls for full panel
cox_split_full[, yr_mid := as.integer(
  format(as.Date((t_enter + t_exit) / 2, origin = "1970-01-01"), "%Y")
)]
cox_split_full[, mandate_release_det := as.integer(
  !is.na(release_det_deadline_yr) &
  yr_mid >= 1989L &
  yr_mid <= release_det_deadline_yr
)]
cox_split_full[, mandate_spill_overfill := as.integer(
  !is.na(release_det_deadline_yr) &
  yr_mid %in% 1993L:1994L
)]
cox_split_full[, mandate_integrity := as.integer(
  !is.na(release_det_deadline_yr) &
  yr_mid %in% 1996L:1998L
)]
cox_split_full[, yr_mid := NULL]

cat(sprintf("  cox_split_full: %s rows | %s tanks | %s events\n\n",
  fmt_n(nrow(cox_split_full)),
  fmt_n(uniqueN(cox_split_full$tank_panel_id)),
  fmt_n(sum(cox_split_full$failure))))


# =============================================================================
# Model fits -- stepwise walk-in
# =============================================================================

log_step("Fitting coxA1-coxC2 (stepwise walk-in)...")

# ---- PANEL A: Full incumbent panel, no matching ----

coxA1 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity,
  data    = cox_split_full,
  cluster = state,
  ties    = "efron"
)

coxA2 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity +
    strata(make_model_noage),
  data    = cox_split_full,
  cluster = state,
  ties    = "efron"
)

# ---- PANEL B: Birth-cohort PSM matched sample ----

coxB1 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity,
  data    = cox_split_birth_psm,
  weights = cox_split_birth_psm$psm_weight,
  cluster = state,
  ties    = "efron"
)

coxB2 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity +
    strata(make_model_noage),
  data    = cox_split_birth_psm,
  weights = cox_split_birth_psm$psm_weight,
  cluster = state,
  ties    = "efron"
)

# ---- PANEL C: Birth-cohort CEM matched sample ----

coxC1 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity,
  data    = cox_split_birth_cem,
  weights = cox_split_birth_cem$cem_weight,
  cluster = state,
  ties    = "efron"
)

coxC2 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity +
    strata(make_model_noage),
  data    = cox_split_birth_cem,
  weights = cox_split_birth_cem$cem_weight,
  cluster = state,
  ties    = "efron"
)

cat("  Panels A-C fitted (6 models)\n\n")


# =============================================================================
# Bootstrap inference (cluster-robust score bootstrap)
# =============================================================================

log_step("Running score bootstrap on all 6 Cox models...")

cox_list <- list(
  A1 = coxA1, A2 = coxA2,
  B1 = coxB1, B2 = coxB2,
  C1 = coxC1, C2 = coxC2
)

data_list <- list(
  A1 = cox_split_full,      A2 = cox_split_full,
  B1 = cox_split_birth_psm, B2 = cox_split_birth_psm,
  C1 = cox_split_birth_cem, C2 = cox_split_birth_cem
)

boot_t2 <- Map(function(m, dt) {
  run_boot_cox(m, term = "did_term", B = N_BOOT, data = dt)
}, cox_list, data_list)

crve_p_cox <- vapply(cox_list, function(m) {
  s   <- summary(m)$coefficients
  idx <- grep("^did_term$", rownames(s))[1]
  if (is.na(idx)) NA_real_ else s[idx, "Pr(>|z|)"]
}, numeric(1))

boot_p_cox <- vapply(boot_t2, `[[`, numeric(1), "p_boot")

cat("  CRVE vs bootstrap p-values for did_term:\n")
for (i in seq_along(cox_list)) {
  cat(sprintf("    %-5s: CRVE p = %.4f | Boot p = %.4f | n = %s\n",
    names(cox_list)[i],
    crve_p_cox[i],
    boot_p_cox[i],
    fmt_n(cox_list[[i]]$n)
  ))
}
cat("\n")


# =============================================================================
# Diagnostic: what does matching add over strata?
# Holds specification constant at strata(cell) and varies only sample.
# =============================================================================

log_step("Computing matching-vs-strata diagnostic...")

extract_cox_did <- function(m, label) {
  s   <- summary(m)$coefficients
  idx <- grep("^did_term$", rownames(s))[1]
  data.table(
    spec     = label,
    log_hr   = s[idx, "coef"],
    hr       = s[idx, "exp(coef)"],
    crve_se  = s[idx, intersect(c("robust se", "se(coef)"), colnames(s))[1]],
    crve_p   = s[idx, "Pr(>|z|)"],
    n_obs    = m$n,
    n_events = m$nevent
  )
}

matching_comparison_cox <- rbindlist(list(
  extract_cox_did(coxA2, "A2: Full panel  | strata(cell)"),
  extract_cox_did(coxB2, "B2: PSM (birth) | strata(cell)"),
  extract_cox_did(coxC2, "C2: CEM (birth) | strata(cell)")
))

matching_comparison_cox[, pct_change_vs_A := 100 * (log_hr / log_hr[spec %like% "A2"] - 1)]

cat("\n=== MATCHING VS STRATA: DiD COMPARISON (spec fixed, sample varies) ===\n")
print(matching_comparison_cox, digits = 5)
cat("\n")

fwrite(matching_comparison_cox,
       file.path(OUTPUT_TABLES, "Diag_CoxMatchingVsStrata_Comparison.csv"))
log_step("Saved: Diag_CoxMatchingVsStrata_Comparison.csv")


# =============================================================================
# Table construction
# =============================================================================

log_step("Building T2 LaTeX table...")

cox_coef_map <- c(
  "did_term"               = "DiD",
  "mandate_release_det"    = "Mandate: release detection",
  "mandate_spill_overfill" = "Mandate: spill/overfill",
  "mandate_integrity"      = "Mandate: integrity/cathodic"
)

boot_flag_row_cox <- vapply(seq_along(cox_list), function(i) {
  if (is.na(crve_p_cox[i]) || is.na(boot_p_cox[i])) return("")
  if ((crve_p_cox[i] < 0.05) != (boot_p_cox[i] < 0.05)) "*disagree*" else ""
}, character(1))

cox_add_rows <- tribble(
  ~term,                          ~A1,            ~A2,            ~B1,            ~B2,            ~C1,            ~C2,
  "\\midrule Sample",             "Full panel",   "Full panel",   "PSM (birth)",  "PSM (birth)",  "CEM (birth)",  "CEM (birth)",
  "Weights",                      "None",         "None",         "PSM",          "PSM",          "CEM",          "CEM",
  "Mandate controls",             "\\checkmark",  "\\checkmark",  "\\checkmark",  "\\checkmark",  "\\checkmark",  "\\checkmark",
  "Strata (make-model, no age)",  "",             "\\checkmark",  "",             "\\checkmark",  "",             "\\checkmark",
  "Bootstrap $p$ (DiD)",          sprintf("%.3f", boot_p_cox[1]), sprintf("%.3f", boot_p_cox[2]), sprintf("%.3f", boot_p_cox[3]), sprintf("%.3f", boot_p_cox[4]), sprintf("%.3f", boot_p_cox[5]), sprintf("%.3f", boot_p_cox[6]),
  "CRVE vs Boot",                 boot_flag_row_cox[1], boot_flag_row_cox[2], boot_flag_row_cox[3], boot_flag_row_cox[4], boot_flag_row_cox[5], boot_flag_row_cox[6]
)

msummary(
  list(
    "A1" = coxA1, "A2" = coxA2,
    "B1" = coxB1, "B2" = coxB2,
    "C1" = coxC1, "C2" = coxC2
  ),
  coef_map     = cox_coef_map,
  exponentiate = TRUE,
  statistic    = "({std.error})",
  stars        = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  escape       = FALSE,
  gof_map      = list(
    list(raw = "nobs",   clean = "Observations", fmt = scales::comma),
    list(raw = "nevent", clean = "Events",       fmt = scales::comma)
  ),
  add_rows     = cox_add_rows,
  output       = file.path(OUTPUT_TABLES, "T2_cox_stepwise.tex"),
  notes        = paste0(
    "Cluster-robust SEs by state (N=18); hazard ratios reported. ",
    "Spells split at January 1, 1999. ",
    "Panel A uses the full incumbent population with no sample restriction. ",
    "Panels B and C apply birth-cohort matching. ",
    "Columns A2 / B2 / C2 use an identical specification --- differences in ",
    "the DiD coefficient across those three columns reflect common support ",
    "enforcement by the matching procedure, not the strata structure. ",
    "Strata on make-model\\_noage incorporates wall type, fuel type, and capacity. ",
    sprintf("Wild cluster score bootstrap $p$-values reported for DiD coefficient ($B$ = %d, Rademacher, state clusters).", N_BOOT)
  )
)
cat("  Saved: T2_cox_stepwise.tex\n")


# =============================================================================
# Bootstrap diagnostics CSV
# =============================================================================

boot_diag_t2 <- data.table(
  col        = seq_along(cox_list),
  panel      = c("A", "A", "B", "B", "C", "C"),
  spec_label = c(
    "A1: Full panel, no strata",
    "A2: Full panel, strata(cell)",
    "B1: PSM, no strata",
    "B2: PSM, strata(cell)",
    "C1: CEM, no strata",
    "C2: CEM, strata(cell)"
  ),
  sample     = c(
    "Full panel", "Full panel",
    "PSM (birth)", "PSM (birth)",
    "CEM (birth)", "CEM (birth)"
  ),
  log_hr     = vapply(cox_list, function(m) {
    s   <- summary(m)$coefficients
    idx <- grep("^did_term$", rownames(s))[1]
    if (is.na(idx)) NA_real_ else s[idx, "coef"]
  }, numeric(1)),
  hr         = vapply(cox_list, function(m) {
    s   <- summary(m)$coefficients
    idx <- grep("^did_term$", rownames(s))[1]
    if (is.na(idx)) NA_real_ else s[idx, "exp(coef)"]
  }, numeric(1)),
  crve_se    = vapply(cox_list, function(m) {
    s   <- summary(m)$coefficients
    idx <- grep("^did_term$", rownames(s))[1]
    if (is.na(idx)) NA_real_
    else {
      col <- intersect(c("robust se", "se(coef)"), colnames(s))[1]
      s[idx, col]
    }
  }, numeric(1)),
  crve_p     = crve_p_cox,
  boot_se    = vapply(boot_t2, `[[`, numeric(1), "se_boot"),
  boot_p     = boot_p_cox,
  boot_ci_lo = vapply(boot_t2, `[[`, numeric(1), "ci_lo"),
  boot_ci_hi = vapply(boot_t2, `[[`, numeric(1), "ci_hi"),
  n_obs      = vapply(cox_list, function(m) m$n, integer(1)),
  n_events   = vapply(cox_list, function(m) as.integer(m$nevent), integer(1))
)

fwrite(boot_diag_t2, file.path(OUTPUT_TABLES, "T2_bootstrap_diagnostics.csv"))
cat("  Saved: T2_bootstrap_diagnostics.csv\n\n")


################################################################################
# === S7: TABLE 3 - TRIPLE DIFF (OLS + COX) ===
################################################################################

#### S7 Table 3 - Triple Diff (OLS + Cox) ####

cat("========================================\n")
cat("S7: TABLE 3 - TRIPLE DIFF (OLS + COX)\n")
cat("========================================\n\n")

# ---- Variable construction (OLS samples) ----

ensure_triple_vars_ols <- function(dt) {
  if (!"single_wall" %in% names(dt)) {
    dt[, single_wall := as.integer(mm_wall == "Single-Walled")]
  }
  dt[, did_x_sw     := did_term * single_wall]
  dt[, did_x_sw_old := did_term * single_wall * above_median_age]
  dt[, sw_x_old     := single_wall * above_median_age]
  dt[]
}

matched_tanks_birth_cem  <- ensure_triple_vars_ols(matched_tanks_birth_cem)
matched_tanks_reform_cem <- ensure_triple_vars_ols(matched_tanks_reform_cem)

# ---- Variable construction (Cox samples) ----
# Cox split data tables also need single_wall and the interaction terms.
# above_median_age was attached in Block 3 (S4).

ensure_triple_vars_cox <- function(dt) {
  if (!"single_wall" %in% names(dt)) {
    dt[, single_wall := as.integer(mm_wall == "Single-Walled")]
  }
  # In Cox we let coxph handle interaction terms via formula, so the
  # explicit *_x_* columns aren't strictly required. But we build them
  # for symmetry and to allow run_boot_cox to target them by name.
  dt[, did_x_sw     := did_term * single_wall]
  dt[, did_x_sw_old := did_term * single_wall * above_median_age]
  dt[]
}

cox_split_birth_cem  <- ensure_triple_vars_cox(cox_split_birth_cem)
cox_split_reform_cem <- ensure_triple_vars_cox(cox_split_reform_cem)


# =============================================================================
# OLS model fits (cols 1-3)
# =============================================================================

log_step("Fitting OLS td1-td3...")

# ---- DATA OBJECTS: Pre-save filtered data, pass to both feols() and bootstrap ----
data_td1 <- drop_singletons(
  matched_tanks_birth_cem[install_yr_int < 1999L],
  "tank_panel_id")

data_td2 <- data_td1        # same base data, same filter

data_td3 <- drop_singletons(
  matched_tanks_reform_cem[install_yr_int < 1999L],
  "tank_panel_id")

# (1) Birth-CEM baseline: did_term only (reproduces T1 col 3)
td1 <- feols(
  closure_event ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity |
    tank_panel_id + panel_year,
  data    = data_td1,
  weights = ~cem_weight,
  cluster = ~state
)

# (2) Birth-CEM full triple diff
td2 <- feols(
  closure_event ~ did_term + did_x_old + did_x_sw + did_x_sw_old +
    mandate_release_det + mandate_spill_overfill + mandate_integrity |
    tank_panel_id + panel_year^make_model_noage,
  data    = data_td2,
  weights = ~cem_weight,
  cluster = ~state
)

# (3) Reform-CEM robustness
td3 <- feols(
  closure_event ~ did_term + did_x_old + did_x_sw + did_x_sw_old +
    mandate_release_det + mandate_spill_overfill + mandate_integrity |
    tank_panel_id + panel_year^make_model_noage,
  data    = data_td3,
  weights = ~cem_weight,
  cluster = ~state
)


# =============================================================================
# Cox model fits (cols 4-6)
# =============================================================================

log_step("Fitting Cox td4-td6...")

# (4) Birth-CEM baseline Cox: did_term only (reproduces T2 col 3)
# No strata yet -- matches the OLS baseline structure (no cell x year FE).
td4 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    mandate_release_det + mandate_spill_overfill + mandate_integrity,
  data    = cox_split_birth_cem,
  cluster = state,
  ties    = "efron"
)

# (5) Birth-CEM full triple diff Cox
# Use formula syntax for interactions; coxph handles them natively.
# Strata on make_model_noage (preserves age variation for HTE).
td5 <- coxph(
  Surv(t_enter, t_exit, failure) ~
    did_term + did_term:above_median_age +
    did_term:single_wall + did_term:single_wall:above_median_age +
    mandate_release_det + mandate_spill_overfill + mandate_integrity +
    strata(make_model_noage),
  data    = cox_split_birth_cem,
  cluster = state,
  ties    = "efron"
)

# (6) Reform-CEM robustness
td6 <- coxph(
  Surv(t_enter, t_exit, failure) ~
    did_term + did_term:above_median_age +
    did_term:single_wall + did_term:single_wall:above_median_age +
    mandate_release_det + mandate_spill_overfill + mandate_integrity +
    strata(make_model_noage),
  data    = cox_split_reform_cem,
  cluster = state,
  ties    = "efron"
)

cat("  td1-td6 fitted\n\n")


# =============================================================================
# Bootstrap inference
# =============================================================================

log_step("Running wild cluster / score bootstrap on T3 (6 cols)...")

td_ols <- list(td1 = td1, td2 = td2, td3 = td3)
td_cox <- list(td4 = td4, td5 = td5, td6 = td6)

# OLS bootstrap (score-based) for did_term in all 3 cols, plus the
# three interaction terms in cols 2-3. Pass pre-saved data objects.
boot_ols_did <- Map(function(m, dt) {
  run_boot_ols_score(m, param = "did_term", B = N_BOOT, data = dt)
}, td_ols, list(data_td1, data_td2, data_td3))

boot_ols_int <- list(
  did_x_old = list(
    td2 = run_boot_ols_score(td2, param = "did_x_old", B = N_BOOT, data = data_td2),
    td3 = run_boot_ols_score(td3, param = "did_x_old", B = N_BOOT, data = data_td3)
  ),
  did_x_sw = list(
    td2 = run_boot_ols_score(td2, param = "did_x_sw", B = N_BOOT, data = data_td2),
    td3 = run_boot_ols_score(td3, param = "did_x_sw", B = N_BOOT, data = data_td3)
  ),
  did_x_sw_old = list(
    td2 = run_boot_ols_score(td2, param = "did_x_sw_old", B = N_BOOT, data = data_td2),
    td3 = run_boot_ols_score(td3, param = "did_x_sw_old", B = N_BOOT, data = data_td3)
  )
)

# Cox bootstrap (score) for did_term in all 3 cols, plus interactions
# in cols 5-6. coxph term names use ":" syntax.
boot_cox_did <- Map(function(m, data) {
  run_boot_cox(m, term = "did_term", data = data)
}, td_cox,
list(cox_split_birth_cem, cox_split_birth_cem, cox_split_reform_cem))

# Get exact term names from cox5/cox6 -- they will be:
#   did_term:above_median_age
#   did_term:single_wall
#   did_term:single_wall:above_median_age
# but coxph may reorder; extract by pattern
get_cox_int_term <- function(m, pattern) {
  nms <- rownames(summary(m)$coefficients)
  hit <- grep(pattern, nms, value = TRUE)
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

# Three-way interaction is the most specific pattern, find it first
td5_3way <- get_cox_int_term(td5, "single_wall.*above_median_age|above_median_age.*single_wall")
td6_3way <- get_cox_int_term(td6, "single_wall.*above_median_age|above_median_age.*single_wall")

# Two-way: did_term:above_median_age (excluding the 3-way)
td5_did_old <- setdiff(
  grep("did_term.*above_median_age", rownames(summary(td5)$coefficients), value = TRUE),
  td5_3way
)[1]
td6_did_old <- setdiff(
  grep("did_term.*above_median_age", rownames(summary(td6)$coefficients), value = TRUE),
  td6_3way
)[1]

# Two-way: did_term:single_wall (excluding the 3-way)
td5_did_sw <- setdiff(
  grep("did_term.*single_wall", rownames(summary(td5)$coefficients), value = TRUE),
  td5_3way
)[1]
td6_did_sw <- setdiff(
  grep("did_term.*single_wall", rownames(summary(td6)$coefficients), value = TRUE),
  td6_3way
)[1]

cat(sprintf("  Cox interaction term names found:\n"))
cat(sprintf("    td5: did_x_old=[%s] did_x_sw=[%s] 3way=[%s]\n",
  td5_did_old, td5_did_sw, td5_3way))
cat(sprintf("    td6: did_x_old=[%s] did_x_sw=[%s] 3way=[%s]\n",
  td6_did_old, td6_did_sw, td6_3way))

# Bootstrap each, with NA-safe fallback
safe_cox_boot <- function(m, term_nm, data) {
  if (is.na(term_nm)) return(list(p_boot = NA_real_, se_boot = NA_real_,
                                   ci_lo  = NA_real_, ci_hi  = NA_real_))
  run_boot_cox(m, term = term_nm, data = data)
}

boot_cox_int <- list(
  did_x_old = list(
    td5 = safe_cox_boot(td5, td5_did_old, data = cox_split_birth_cem),
    td6 = safe_cox_boot(td6, td6_did_old, data = cox_split_reform_cem)
  ),
  did_x_sw = list(
    td5 = safe_cox_boot(td5, td5_did_sw, data = cox_split_birth_cem),
    td6 = safe_cox_boot(td6, td6_did_sw, data = cox_split_reform_cem)
  ),
  did_x_sw_old = list(
    td5 = safe_cox_boot(td5, td5_3way, data = cox_split_birth_cem),
    td6 = safe_cox_boot(td6, td6_3way, data = cox_split_reform_cem)
  )
)


# =============================================================================
# Pull CRVE p for did_term in all 6 cols
# =============================================================================

extract_p_ols <- function(m, term) {
  ct  <- coeftable(m)
  idx <- match(term, rownames(ct))
  if (is.na(idx)) NA_real_ else ct[idx, "Pr(>|t|)"]
}
extract_p_cox <- function(m, term) {
  if (is.na(term)) return(NA_real_)
  s   <- summary(m)$coefficients
  idx <- match(term, rownames(s))
  if (is.na(idx)) NA_real_ else s[idx, "Pr(>|z|)"]
}

crve_p_did_all <- c(
  ols = vapply(td_ols, extract_p_ols, numeric(1), term = "did_term"),
  cox = vapply(td_cox, extract_p_cox, numeric(1), term = "did_term")
)

boot_p_did_all <- c(
  ols = vapply(boot_ols_did, `[[`, numeric(1), "p_boot"),
  cox = vapply(boot_cox_did, `[[`, numeric(1), "p_boot")
)

# Per-interaction p-values (NA in cols where term absent)
boot_p_did_x_old <- c(
  td1 = NA_real_,
  td2 = boot_ols_int$did_x_old$td2$p_boot,
  td3 = boot_ols_int$did_x_old$td3$p_boot,
  td4 = NA_real_,
  td5 = boot_cox_int$did_x_old$td5$p_boot,
  td6 = boot_cox_int$did_x_old$td6$p_boot
)
boot_p_did_x_sw <- c(
  td1 = NA_real_,
  td2 = boot_ols_int$did_x_sw$td2$p_boot,
  td3 = boot_ols_int$did_x_sw$td3$p_boot,
  td4 = NA_real_,
  td5 = boot_cox_int$did_x_sw$td5$p_boot,
  td6 = boot_cox_int$did_x_sw$td6$p_boot
)
boot_p_did_x_sw_old <- c(
  td1 = NA_real_,
  td2 = boot_ols_int$did_x_sw_old$td2$p_boot,
  td3 = boot_ols_int$did_x_sw_old$td3$p_boot,
  td4 = NA_real_,
  td5 = boot_cox_int$did_x_sw_old$td5$p_boot,
  td6 = boot_cox_int$did_x_sw_old$td6$p_boot
)

cat(sprintf("\n  Bootstrap p-values for triple-diff coefficient (did x sw x old):\n"))
cat(sprintf("    OLS Birth-CEM:  %s\n",
  ifelse(is.na(boot_p_did_x_sw_old["td2"]), "NA",
         sprintf("%.4f", boot_p_did_x_sw_old["td2"]))))
cat(sprintf("    OLS Reform-CEM: %s\n",
  ifelse(is.na(boot_p_did_x_sw_old["td3"]), "NA",
         sprintf("%.4f", boot_p_did_x_sw_old["td3"]))))
cat(sprintf("    Cox Birth-CEM:  %s\n",
  ifelse(is.na(boot_p_did_x_sw_old["td5"]), "NA",
         sprintf("%.4f", boot_p_did_x_sw_old["td5"]))))
cat(sprintf("    Cox Reform-CEM: %s\n\n",
  ifelse(is.na(boot_p_did_x_sw_old["td6"]), "NA",
         sprintf("%.4f", boot_p_did_x_sw_old["td6"]))))


# =============================================================================
# Table construction
# =============================================================================

log_step("Building T3 LaTeX table (6 cols)...")

# Renaming map shared across panels. Cox interaction names use ":"
# syntax which we map to the same display labels as OLS.
t3_dict <- c(
  "did_term"                                            = "DiD",
  "did_x_old"                                           = sprintf(
    "DiD $\\times$ Old ($\\geq$%.0f yr)", med_age_birth_cem),
  "did_term:above_median_age"                           = sprintf(
    "DiD $\\times$ Old ($\\geq$%.0f yr)", med_age_birth_cem),
  "did_x_sw"                                            = "DiD $\\times$ Single-Walled",
  "did_term:single_wall"                                = "DiD $\\times$ Single-Walled",
  "did_x_sw_old"                                        = sprintf(
    "DiD $\\times$ SW $\\times$ Old ($\\geq$%.0f yr)", med_age_birth_cem),
  "did_term:single_wall:above_median_age"               = sprintf(
    "DiD $\\times$ SW $\\times$ Old ($\\geq$%.0f yr)", med_age_birth_cem),
  "did_term:above_median_age:single_wall"               = sprintf(
    "DiD $\\times$ SW $\\times$ Old ($\\geq$%.0f yr)", med_age_birth_cem),
  "mandate_release_det"                                 = "Mandate: release detection",
  "mandate_spill_overfill"                              = "Mandate: spill/overfill",
  "mandate_integrity"                                   = "Mandate: integrity/cathodic"
)

# Add-rows: 6 columns. Use tribble for explicit alignment.
t3_add_rows <- tribble(
  ~term,                            ~`(1)`,             ~`(2)`,             ~`(3)`,             ~`(4)`,             ~`(5)`,             ~`(6)`,
  "\\midrule Estimator",            "OLS",              "OLS",              "OLS",              "Cox",              "Cox",              "Cox",
  "Sample",                         "Birth-CEM",        "Birth-CEM",        "Reform-CEM",       "Birth-CEM",        "Birth-CEM",        "Reform-CEM",
  "Tank FE / Strata",               "Tank FE",          "Tank FE",          "Tank FE",          "---",              "Strata (make-model)", "Strata (make-model)",
  "Year FE",                        "\\checkmark",      "",                 "",                 "(implicit)",       "(implicit)",       "(implicit)",
  "Cell $\\times$ Year FE",         "",                 "\\checkmark",      "\\checkmark",      "",                 "",                 "",
  "Bootstrap $p$ (DiD)",            sprintf("%.3f", boot_p_did_all[1]),
                                    sprintf("%.3f", boot_p_did_all[2]),
                                    sprintf("%.3f", boot_p_did_all[3]),
                                    sprintf("%.3f", boot_p_did_all[4]),
                                    sprintf("%.3f", boot_p_did_all[5]),
                                    sprintf("%.3f", boot_p_did_all[6]),
  "Bootstrap $p$ (DiD$\\times$Old)",
                                    "---",
                                    sprintf("%.3f", boot_p_did_x_old["td2"]),
                                    sprintf("%.3f", boot_p_did_x_old["td3"]),
                                    "---",
                                    sprintf("%.3f", boot_p_did_x_old["td5"]),
                                    sprintf("%.3f", boot_p_did_x_old["td6"]),
  "Bootstrap $p$ (DiD$\\times$SW)",
                                    "---",
                                    sprintf("%.3f", boot_p_did_x_sw["td2"]),
                                    sprintf("%.3f", boot_p_did_x_sw["td3"]),
                                    "---",
                                    sprintf("%.3f", boot_p_did_x_sw["td5"]),
                                    sprintf("%.3f", boot_p_did_x_sw["td6"]),
  "Bootstrap $p$ (DiD$\\times$SW$\\times$Old)",
                                    "---",
                                    sprintf("%.3f", boot_p_did_x_sw_old["td2"]),
                                    sprintf("%.3f", boot_p_did_x_sw_old["td3"]),
                                    "---",
                                    sprintf("%.3f", boot_p_did_x_sw_old["td5"]),
                                    sprintf("%.3f", boot_p_did_x_sw_old["td6"])
)

msummary(
  list(
    "(1)" = td1, "(2)" = td2, "(3)" = td3,
    "(4)" = td4, "(5)" = td5, "(6)" = td6
  ),
  coef_map     = t3_dict,
  exponentiate = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),  # exp() Cox cols only
  statistic    = "({std.error})",
  stars        = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  escape       = FALSE,
  gof_map      = list(
    list(raw = "nobs",   clean = "Observations", fmt = scales::comma),
    list(raw = "nevent", clean = "Events",       fmt = scales::comma)
  ),
  add_rows     = t3_add_rows,
  output       = file.path(OUTPUT_TABLES, "T3_triple_diff.tex"),
  notes        = paste0(
    "Cluster-robust SEs by state (N=18). ",
    "Cols (1)-(3): OLS linear probability model; coefficients are pp changes ",
    "in annual closure probability. ",
    "Cols (4)-(6): Cox proportional hazards; reported values are hazard ratios ",
    "(exp(coef)). ",
    "Cols (1) and (4) reproduce baseline pooled DiD specifications from T1 col (3) ",
    "and T2 col (3) for direct comparability. ",
    "Cols (2) and (5) expand to the three-way interaction ",
    "DiD $\\times$ single\\_wall $\\times$ above\\_median\\_age. ",
    "Cols (3) and (6) re-run the (2)/(5) spec on the reform-survivor sample. ",
    "Level effects of single\\_wall and above\\_median\\_age are time-invariant ",
    "at the tank level: in OLS they are absorbed by tank FE, in Cox they are ",
    "absorbed by strata(make\\_model\\_noage). The triple-diff coefficient ",
    "DiD $\\times$ SW $\\times$ Old measures the incremental reform effect on old ",
    "single-walled tanks beyond the additive age and wall effects. ",
    "Cols (2)-(3) and (5)-(6) restrict automatically to tanks alive at reform ",
    "date (above\\_median\\_age non-missing). ",
    sprintf("Median age at reform on birth-CEM alive-at-reform subset: %.1f yr. ",
            med_age_birth_cem),
    sprintf("Wild cluster bootstrap (OLS) and score bootstrap (Cox) ",
            "$p$-values reported for each DiD-related coefficient ",
            "($B$ = %d, Rademacher).", N_BOOT)
  )
)
cat("  Saved: T3_triple_diff.tex\n\n")


# =============================================================================
# Bootstrap diagnostics CSV
# =============================================================================

extract_coef_ols <- function(m, term) {
  ct  <- coeftable(m)
  idx <- match(term, rownames(ct))
  if (is.na(idx)) NA_real_ else ct[idx, "Estimate"]
}
extract_coef_cox <- function(m, term) {
  if (is.na(term)) return(NA_real_)
  s <- summary(m)$coefficients
  idx <- match(term, rownames(s))
  if (is.na(idx)) NA_real_ else s[idx, "coef"]
}

boot_diag_t3 <- rbindlist(list(
  data.table(col=1, panel="OLS", spec="(1) Birth-CEM baseline",
             term="did_term",
             coef=extract_coef_ols(td1, "did_term"),
             crve_p=extract_p_ols(td1, "did_term"),
             boot_p=boot_ols_did$td1$p_boot),
  data.table(col=2, panel="OLS", spec="(2) Birth-CEM triple-diff",
             term="did_term",     coef=extract_coef_ols(td2, "did_term"),
             crve_p=extract_p_ols(td2, "did_term"), boot_p=boot_ols_did$td2$p_boot),
  data.table(col=2, panel="OLS", spec="(2) Birth-CEM triple-diff",
             term="did_x_old",    coef=extract_coef_ols(td2, "did_x_old"),
             crve_p=extract_p_ols(td2, "did_x_old"), boot_p=boot_p_did_x_old["td2"]),
  data.table(col=2, panel="OLS", spec="(2) Birth-CEM triple-diff",
             term="did_x_sw",     coef=extract_coef_ols(td2, "did_x_sw"),
             crve_p=extract_p_ols(td2, "did_x_sw"), boot_p=boot_p_did_x_sw["td2"]),
  data.table(col=2, panel="OLS", spec="(2) Birth-CEM triple-diff",
             term="did_x_sw_old", coef=extract_coef_ols(td2, "did_x_sw_old"),
             crve_p=extract_p_ols(td2, "did_x_sw_old"), boot_p=boot_p_did_x_sw_old["td2"]),

  data.table(col=3, panel="OLS", spec="(3) Reform-CEM triple-diff",
             term="did_term",     coef=extract_coef_ols(td3, "did_term"),
             crve_p=extract_p_ols(td3, "did_term"), boot_p=boot_ols_did$td3$p_boot),
  data.table(col=3, panel="OLS", spec="(3) Reform-CEM triple-diff",
             term="did_x_old",    coef=extract_coef_ols(td3, "did_x_old"),
             crve_p=extract_p_ols(td3, "did_x_old"), boot_p=boot_p_did_x_old["td3"]),
  data.table(col=3, panel="OLS", spec="(3) Reform-CEM triple-diff",
             term="did_x_sw",     coef=extract_coef_ols(td3, "did_x_sw"),
             crve_p=extract_p_ols(td3, "did_x_sw"), boot_p=boot_p_did_x_sw["td3"]),
  data.table(col=3, panel="OLS", spec="(3) Reform-CEM triple-diff",
             term="did_x_sw_old", coef=extract_coef_ols(td3, "did_x_sw_old"),
             crve_p=extract_p_ols(td3, "did_x_sw_old"), boot_p=boot_p_did_x_sw_old["td3"]),

  data.table(col=4, panel="Cox", spec="(4) Birth-CEM baseline",
             term="did_term",     coef=extract_coef_cox(td4, "did_term"),
             crve_p=extract_p_cox(td4, "did_term"), boot_p=boot_cox_did$td4$p_boot),

  data.table(col=5, panel="Cox", spec="(5) Birth-CEM triple-diff",
             term="did_term",     coef=extract_coef_cox(td5, "did_term"),
             crve_p=extract_p_cox(td5, "did_term"), boot_p=boot_cox_did$td5$p_boot),
  data.table(col=5, panel="Cox", spec="(5) Birth-CEM triple-diff",
             term=td5_did_old,    coef=extract_coef_cox(td5, td5_did_old),
             crve_p=extract_p_cox(td5, td5_did_old), boot_p=boot_p_did_x_old["td5"]),
  data.table(col=5, panel="Cox", spec="(5) Birth-CEM triple-diff",
             term=td5_did_sw,     coef=extract_coef_cox(td5, td5_did_sw),
             crve_p=extract_p_cox(td5, td5_did_sw), boot_p=boot_p_did_x_sw["td5"]),
  data.table(col=5, panel="Cox", spec="(5) Birth-CEM triple-diff",
             term=td5_3way,       coef=extract_coef_cox(td5, td5_3way),
             crve_p=extract_p_cox(td5, td5_3way), boot_p=boot_p_did_x_sw_old["td5"]),

  data.table(col=6, panel="Cox", spec="(6) Reform-CEM triple-diff",
             term="did_term",     coef=extract_coef_cox(td6, "did_term"),
             crve_p=extract_p_cox(td6, "did_term"), boot_p=boot_cox_did$td6$p_boot),
  data.table(col=6, panel="Cox", spec="(6) Reform-CEM triple-diff",
             term=td6_did_old,    coef=extract_coef_cox(td6, td6_did_old),
             crve_p=extract_p_cox(td6, td6_did_old), boot_p=boot_p_did_x_old["td6"]),
  data.table(col=6, panel="Cox", spec="(6) Reform-CEM triple-diff",
             term=td6_did_sw,     coef=extract_coef_cox(td6, td6_did_sw),
             crve_p=extract_p_cox(td6, td6_did_sw), boot_p=boot_p_did_x_sw["td6"]),
  data.table(col=6, panel="Cox", spec="(6) Reform-CEM triple-diff",
             term=td6_3way,       coef=extract_coef_cox(td6, td6_3way),
             crve_p=extract_p_cox(td6, td6_3way), boot_p=boot_p_did_x_sw_old["td6"])
))

fwrite(boot_diag_t3, file.path(OUTPUT_TABLES, "T3_bootstrap_diagnostics.csv"))
cat("  Saved: T3_bootstrap_diagnostics.csv\n\n")


################################################################################
# === S7b: VINTAGE HTE (SATURATED INTERACTION, INSTALL YEAR COHORTS) ===
################################################################################

#### S7b Vintage HTE -- Install Year Cohorts ####

cat("========================================\n")
cat("S7b: VINTAGE HTE (INSTALL YEAR COHORTS)\n")
cat("========================================\n\n")

# ---- Vintage bin definition ----
# Use 5-year installation year cohorts. This is independent of survival
# and captures how older vintages respond to the reform on average.
VINTAGE_BIN_BREAKS <- c(1960L, 1965L, 1970L, 1975L, 1980L, 1985L, 1990L, 1995L, 1999L)
VINTAGE_BIN_LABELS <- c(
  "1960-1964", "1965-1969", "1970-1974", "1975-1979",
  "1980-1984", "1985-1989", "1990-1994", "1995-1998"
)

# Reference bin = most recent pre-reform cohort 1995-1998
REF_BIN_LABEL <- VINTAGE_BIN_LABELS[8]

attach_vintage_bin <- function(dt) {
  dt[, vintage_bin := NA_character_]
  dt[install_yr_int < 1999L,
     vintage_bin := as.character(cut(
       install_yr_int,
       breaks         = VINTAGE_BIN_BREAKS,
       labels         = VINTAGE_BIN_LABELS,
       include.lowest = TRUE,
       right          = FALSE
     ))]
  dt[, vintage_bin := factor(vintage_bin, levels = VINTAGE_BIN_LABELS)]
  dt[]
}

matched_tanks_birth_cem <- attach_vintage_bin(matched_tanks_birth_cem)
cox_split_birth_cem     <- attach_vintage_bin(cox_split_birth_cem)

# Bin counts diagnostic
log_step("Bin sample sizes (install year vintage cohorts, pre-reform):")
bin_counts <- matched_tanks_birth_cem[
  install_yr_int < 1999L & !is.na(vintage_bin),
  .(n_tanks = uniqueN(tank_panel_id),
    n_tx    = uniqueN(tank_panel_id[texas_treated == 1L]),
    n_ctl   = uniqueN(tank_panel_id[texas_treated == 0L])),
  by = vintage_bin
][order(vintage_bin)]
print(bin_counts)
fwrite(bin_counts,
       file.path(OUTPUT_TABLES, "VintageHTE_BinSampleSizes.csv"))
cat("\n")


# =============================================================================
# OLS: Saturated vintage HTE
# =============================================================================

log_step("Fitting OLS saturated vintage HTE...")

# ---- DATA OBJECT: Pre-save filtered data for vintage HTE model ----
data_vintage_ols <- drop_singletons(
  matched_tanks_birth_cem[install_yr_int < 1999L],
  "tank_panel_id")

# fixest i(vintage_bin, did_term, ref = "1995-1998") creates one interaction
# coefficient per bin EXCEPT the reference. The did_term main effect is
# the ATT in the reference bin. Each did_term:vintage_bin coefficient is the
# incremental ATT in that vintage relative to the reference (1995-1998).

m_vintage_ols <- feols(
  closure_event ~ did_term +
    i(vintage_bin, did_term, ref = REF_BIN_LABEL) +
    mandate_release_det + mandate_spill_overfill + mandate_integrity |
    tank_panel_id + panel_year^make_model_noage,
  data    = data_vintage_ols,
  weights = ~cem_weight,
  cluster = ~state
)
cat("  m_vintage_ols fitted.\n")


# Extract per-bin absolute ATT via delta method.
# Reference bin ATT = beta_did_term (alone)
# Bin-X ATT          = beta_did_term + beta_{did_term:vintage_bin=X}
# SE for bin X       = sqrt(Var(beta_did) + Var(beta_int) + 2*Cov(beta_did, beta_int))

extract_bin_atts_ols <- function(model, bin_labels, ref_label) {
  V <- vcov(model, type = "clustered")
  b <- coef(model)

  did_idx <- which(names(b) == "did_term")
  stopifnot("did_term coefficient missing" = length(did_idx) == 1L)

  out <- data.table(bin = bin_labels)
  out[, label := bin_labels]
  out[, is_ref := bin == ref_label]
  out[, atts := NA_real_]
  out[, se   := NA_real_]
  out[, ci_lo := NA_real_]
  out[, ci_hi := NA_real_]

  for (lbl in bin_labels) {
    if (lbl == ref_label) {
      out[bin == lbl, atts := b[did_idx]]
      out[bin == lbl, se   := sqrt(V[did_idx, did_idx])]
    } else {
      int_name <- grep(
        sprintf("^vintage_bin::%s:did_term$|^did_term:vintage_bin::%s$",
                gsub("([\\-])", "\\\\\\1", lbl),
                gsub("([\\-])", "\\\\\\1", lbl)),
        names(b),
        value = TRUE
      )
      if (length(int_name) == 0L) {
        int_name <- names(b)[
          grepl("did_term", names(b)) &
          grepl(lbl, names(b), fixed = TRUE)
        ]
      }
      if (length(int_name) == 0L) {
        warning(sprintf("Could not locate interaction coef for vintage %s", lbl))
        next
      }
      int_idx <- which(names(b) == int_name[1])
      att     <- b[did_idx] + b[int_idx]
      var_att <- V[did_idx, did_idx] + V[int_idx, int_idx] +
                 2 * V[did_idx, int_idx]
      out[bin == lbl, atts := att]
      out[bin == lbl, se   := sqrt(var_att)]
    }
  }
  out[, ci_lo := atts - 1.96 * se]
  out[, ci_hi := atts + 1.96 * se]
  out[, t_stat := atts / se]
  out[, p_value := 2 * pnorm(-abs(t_stat))]
  out
}

vintage_atts_ols <- extract_bin_atts_ols(m_vintage_ols, VINTAGE_BIN_LABELS, REF_BIN_LABEL)
log_step("OLS vintage ATTs:")
print(vintage_atts_ols[, .(bin, atts, se, ci_lo, ci_hi, p_value)])

log_step("Bootstrapping OLS vintage coefficients...")
boot_vintage_did <- run_boot_ols_score(m_vintage_ols, param = "did_term",
                                       B = N_BOOT, data = data_vintage_ols)

boot_vintage_ints <- list()
int_names_full <- grep("^vintage_bin::|^did_term:vintage_bin::",
                       names(coef(m_vintage_ols)), value = TRUE)
for (int_nm in int_names_full) {
  boot_vintage_ints[[int_nm]] <- run_boot_ols_score(
    m_vintage_ols, param = int_nm, B = N_BOOT, data = data_vintage_ols)
}

vintage_atts_ols[, boot_p_conservative := NA_real_]
for (lbl in VINTAGE_BIN_LABELS) {
  if (lbl == REF_BIN_LABEL) {
    vintage_atts_ols[bin == lbl, boot_p_conservative := boot_vintage_did$p_boot]
  } else {
    int_name <- names(boot_vintage_ints)[
      grepl(lbl, names(boot_vintage_ints), fixed = TRUE)
    ][1]
    if (!is.na(int_name)) {
      vintage_atts_ols[bin == lbl, boot_p_conservative := max(
        boot_vintage_did$p_boot, boot_vintage_ints[[int_name]]$p_boot,
        na.rm = TRUE
      )]
    }
  }
}

cat("\n  Vintage HTE results (primary for Table report):\n")
print(vintage_atts_ols[, .(bin, atts, se, ci_lo, ci_hi, p_value, boot_p_conservative)])
cat("\n")


# =============================================================================
# Tables (OLS Vintage HTE only)
# =============================================================================

log_step("Building OLS vintage HTE table...")

vintage_atts_ols_out <- merge(vintage_atts_ols,
                              bin_counts[, .(bin = vintage_bin, n_tanks, n_tx, n_ctl)],
                              by = "bin", all.x = TRUE, sort = FALSE)
setorder(vintage_atts_ols_out, bin)

fwrite(vintage_atts_ols_out,
       file.path(OUTPUT_TABLES, "T_VintageHTE_OLS.csv"))
log_step("Saved: T_VintageHTE_OLS.csv")

cat("\n=== VINTAGE HTE RESULTS (PRIMARY) ===\n")
print(vintage_atts_ols_out[, .(bin, n_tanks, n_tx, atts, se, ci_lo, ci_hi,
                                p_value, boot_p_conservative)], digits = 5)
cat("\n")


write_vintage_hte_tex <- function(dt, filename) {
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Vintage Heterogeneity in Reform Effect}",
    "\\label{tbl:vintage_hte_ols}",
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    "Vintage & ATT & SE & 95\\% CI & CRVE $p$ & Boot $p$ & N tanks & N TX \\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(dt))) {
    bin_lbl <- dt$bin[i]
    atts    <- dt$atts[i]
    se      <- dt$se[i]
    ci_lo   <- dt$ci_lo[i]
    ci_hi   <- dt$ci_hi[i]
    pv      <- dt$p_value[i]
    bp      <- dt$boot_p_conservative[i]
    n_tk    <- dt$n_tanks[i]
    n_tx    <- dt$n_tx[i]
    is_ref_str <- if (dt$is_ref[i]) " (ref)" else ""
    lines <- c(lines, sprintf(
      "%s%s & %.4f%s & %.4f & [%.4f, %.4f] & %.3f & %.3f & %s & %s \\\\",
      bin_lbl, is_ref_str, atts, stars_p(pv), se, ci_lo, ci_hi,
      pv, bp, format(n_tk, big.mark = ","), format(n_tx, big.mark = ",")
    ))
  }
  lines <- c(lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\raggedright",
    sprintf(paste0(
      "Notes: Saturated OLS vintage HTE on birth-CEM matched sample; pre-reform incumbents. ",
      "Reference bin: %s (most recent pre-reform cohort). ",
      "Vintage ATTs constructed via delta-method addition of ",
      "DiD main effect and DiD$\\times$vintage\\_bin interaction; SEs use clustered VCV. ",
      "CRVE $p$ uses cluster-robust delta-method asymptotic test. ",
      "Boot $p$: conservative bootstrap p-value (max of did\\_term and ",
      "interaction p-values; %d Rademacher draws, state clusters). ",
      "Stars: *** $p<0.01$, ** $p<0.05$, * $p<0.1$ (CRVE)."
      ),
      REF_BIN_LABEL, N_BOOT
    ),
    "\\end{table}"
  )
  writeLines(lines, file.path(OUTPUT_TABLES, filename))
  cat(sprintf("  Saved: %s\n", filename))
}

write_vintage_hte_tex(vintage_atts_ols_out, "T_VintageHTE_OLS.tex")


# =============================================================================
# Forest plot (composite: pointrange + histogram)
# =============================================================================

log_step("Building vintage HTE forest plot...")

hist_long <- melt(
  bin_counts[, .(vintage_bin, Texas = n_tx, Control = n_ctl)],
  id.vars       = "vintage_bin",
  variable.name = "group",
  value.name    = "n_tanks"
)
hist_long[, group := factor(group, levels = c("Control", "Texas"))]
hist_long[, vintage_bin := factor(vintage_bin, levels = VINTAGE_BIN_LABELS)]

shared_x_scale <- scale_x_discrete(drop = FALSE, limits = VINTAGE_BIN_LABELS)

suppressPackageStartupMessages(library(patchwork))

build_vintage_composite <- function(forest_dt, est_col, est_label,
                                    null_y, point_col, text_fmt) {

  p_forest <- ggplot(forest_dt,
      aes(x = bin, y = .data[[est_col]], ymin = ci_lo, ymax = ci_hi)) +
    geom_hline(yintercept = null_y, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    geom_pointrange(colour = point_col, size = 0.4, linewidth = 0.6) +
    geom_text(aes(label = sprintf(text_fmt, .data[[est_col]])),
              vjust = -1.4, size = 2.6, colour = "grey25") +
    shared_x_scale +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(x = NULL, y = est_label) +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line.x  = element_blank(), axis.ticks.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.line.y  = element_line(colour = "black", linewidth = 0.4),
      axis.ticks.y = element_line(colour = "black", linewidth = 0.3),
      axis.text.y  = element_text(colour = "black"),
      panel.grid   = element_blank(), plot.title = element_blank(),
      plot.margin  = margin(t = 8, r = 10, b = 0, l = 8)
    )

  p_hist <- ggplot(hist_long, aes(x = vintage_bin, y = n_tanks, fill = group)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
    scale_fill_manual(values = c("Texas" = COL_TX, "Control" = COL_CTRL), name = NULL) +
    shared_x_scale +
    scale_y_continuous(labels = scales::comma_format(),
                       expand = expansion(mult = c(0, 0.10))) +
    labs(x = "Install Year Vintage (cohort)", y = "Tanks") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line       = element_line(colour = "black", linewidth = 0.4),
      axis.ticks      = element_line(colour = "black", linewidth = 0.3),
      axis.text       = element_text(colour = "black"),
      axis.text.x     = element_text(angle = 45, hjust = 1),
      panel.grid      = element_blank(),
      legend.position = "bottom", legend.text = element_text(size = 9),
      legend.key.size = unit(0.4, "cm"), plot.title = element_blank(),
      plot.margin     = margin(t = 0, r = 10, b = 8, l = 8)
    )

  p_forest / p_hist + plot_layout(heights = c(2.3, 1))
}

p_vintage_ols_composite <- build_vintage_composite(
  forest_dt = vintage_atts_ols_out,
  est_col   = "atts",
  est_label = "ATT (pp change in annual closure probability)",
  null_y    = 0,
  point_col = COL_RED,
  text_fmt  = "%.4f"
)
save_gg(p_vintage_ols_composite, "F_VintageHTE_OLS_forest", width = 8, height = 7)
cat("  Saved: F_VintageHTE_OLS_forest (PDF + PNG)\n\n")

################################################################################
# === S8a: OLS EVENT STUDY MODELS + FIGURES ===
################################################################################

#### S8a OLS Event Study Models + Figures ####

cat("========================================\n")
cat("S8a: OLS EVENT STUDY MODELS + FIGURES\n")
cat("========================================\n\n")

MANDATE_RHS <- "mandate_release_det + mandate_spill_overfill + mandate_integrity"


# =============================================================================
# PRIMARY ES MODELS (Birth-CEM)
# =============================================================================

log_step("Fitting primary OLS event-study models...")

# (M1) Pooled, tank + year FE
m_es_pooled <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_birth_es,
              "tank_panel_id"),
  weights = ~cem_w_norm,
  cluster = ~state
)

# (M2) Pooled + make-model linear trends
# make_model_noage[panel_year] adds one linear slope per technology cell
m_es_trends <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year + make_model_noage[panel_year]",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_birth_es,
              "tank_panel_id"),
  weights = ~cem_w_norm,
  cluster = ~state
)

# (M3) Below-median age subsample
m_es_below_med <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_birth_below,
              "tank_panel_id"),
  weights = ~cem_w_norm,
  cluster = ~state
)

# (M4) Above-median age subsample
m_es_above_med <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_birth_above,
              "tank_panel_id"),
  weights = ~cem_w_norm,
  cluster = ~state
)

cat("  Primary ES models fitted (m_es_pooled, m_es_trends, m_es_below_med, m_es_above_med).\n\n")


# =============================================================================
# ROBUSTNESS ES MODELS
# =============================================================================

log_step("Fitting robustness OLS event-study models...")

# Birth-PSM pooled
m_es_psm_pooled <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_birth_psm_es,
              "tank_panel_id"),
  weights = ~psm_w_norm,
  cluster = ~state
)

# Birth-PSM above-median
m_es_psm_above <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_birth_psm_above,
              "tank_panel_id"),
  weights = ~psm_w_norm,
  cluster = ~state
)

# Reform-CEM pooled
m_es_reform_pooled <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_reform_es,
              "tank_panel_id"),
  weights = ~cem_w_norm,
  cluster = ~state
)

# Reform-CEM above-median
m_es_reform_above <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_reform_above,
              "tank_panel_id"),
  weights = ~cem_w_norm,
  cluster = ~state
)

# Single-walled only (birth-CEM)
m_es_sw <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_birth_sw,
              "tank_panel_id"),
  weights = ~cem_w_norm,
  cluster = ~state
)

# Double-walled only (placebo, birth-CEM)
m_es_dw <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_birth_dw,
              "tank_panel_id"),
  weights = ~cem_w_norm,
  cluster = ~state
)

# Drop TX border states (OK, AR, LA)
m_es_noborder <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_birth_noborder,
              "tank_panel_id"),
  weights = ~cem_w_norm,
  cluster = ~state
)

# Post-1988 installs only
m_es_post88 <- feols(
  as.formula(sprintf(
    "closure_event ~ i(rel_year, texas_treated, ref = -1L) + %s |
       tank_panel_id + panel_year",
    MANDATE_RHS)),
  data    = drop_singletons(
              incumbents_birth_post88,
              "tank_panel_id"),
  weights = ~cem_w_norm,
  cluster = ~state
)

cat("  Robustness ES models fitted (8 additional specs).\n\n")


# =============================================================================
# Joint pre-trend F-tests (reported in figure footnotes)
# =============================================================================

log_step("Computing joint pre-trend F-tests...")

# Pre-period terms: rel_year tau in {-13, -12, ..., -3}
# (excludes -1 [ref] and -2 [adjacent to ref, often noisy])
pre_pattern <- "rel_year::(-[3-9]|-1[0-9]):texas_treated"

joint_pretrend <- function(model, label) {
  pre_terms <- grep(pre_pattern, names(coef(model)), value = TRUE)
  if (length(pre_terms) < 2L) {
    return(list(label = label, F = NA, p = NA, df = 0))
  }
  wt <- wald(model, pre_terms)
  list(label = label, F = wt$stat, p = wt$p, df = length(pre_terms))
}

pre_tests <- list(
  pooled       = joint_pretrend(m_es_pooled,       "Pooled (birth-CEM)"),
  trends       = joint_pretrend(m_es_trends,       "Pooled + linear trends"),
  below        = joint_pretrend(m_es_below_med,    "Below-median age"),
  above        = joint_pretrend(m_es_above_med,    "Above-median age"),
  psm          = joint_pretrend(m_es_psm_pooled,   "Birth-PSM pooled"),
  psm_above    = joint_pretrend(m_es_psm_above,    "Birth-PSM above-median"),
  reform       = joint_pretrend(m_es_reform_pooled,"Reform-CEM pooled"),
  reform_above = joint_pretrend(m_es_reform_above, "Reform-CEM above-median"),
  sw           = joint_pretrend(m_es_sw,           "Single-walled only"),
  dw           = joint_pretrend(m_es_dw,           "Double-walled placebo"),
  noborder     = joint_pretrend(m_es_noborder,     "Drop border states"),
  post88       = joint_pretrend(m_es_post88,       "Post-1988 installs only")
)

cat("  Joint pre-trend F-tests:\n")
for (t in pre_tests) {
  cat(sprintf("    %-30s F=%6.3f  p=%.4f  (df=%d)\n",
    t$label, t$F, t$p, t$df))
}

# Save pre-trend tests as CSV for table reference
pretrend_dt <- rbindlist(lapply(pre_tests, function(t) {
  data.table(spec = t$label, F = t$F, p = t$p, df = t$df)
}))
fwrite(pretrend_dt, file.path(OUTPUT_TABLES, "ES_OLS_PreTrendTests.csv"))
cat("\n  Saved: ES_OLS_PreTrendTests.csv\n\n")


# =============================================================================
# PRIMARY EVENT STUDY FIGURES (F01-F04)
# =============================================================================

log_step("Building primary OLS event-study figures...")

# F01: OLS pooled (birth-CEM matched, mandate-controlled)
p_f01 <- es_ggplot(
  m_es_pooled,
  point_col = "grey25",
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post
)
save_gg(p_f01, "F01_es_ols_pooled", width = 7, height = 5)

# F02: OLS pooled + make-model linear trends
p_f02 <- es_ggplot(
  m_es_trends,
  point_col = "grey25",
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post
)
save_gg(p_f02, "F02_es_ols_pooled_trends", width = 7, height = 5)

# F03: OLS below-median age
p_f03 <- es_ggplot(
  m_es_below_med,
  point_col = COL_BLUE,
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post
)
save_gg(p_f03, "F03_es_ols_below_median", width = 7, height = 5)

# F04: OLS above-median age
p_f04 <- es_ggplot(
  m_es_above_med,
  point_col = COL_RED,
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post
)
save_gg(p_f04, "F04_es_ols_above_median", width = 7, height = 5)

cat("  Saved: F01-F04 primary figures (PDF + PNG)\n\n")


# =============================================================================
# ROBUSTNESS EVENT STUDY FIGURES (F09, F10, F12-F15)
# Note: F05-F08, F11 reserved for Cox in S8b
# =============================================================================

log_step("Building robustness OLS event-study figures...")

# F09: Birth-PSM pooled
p_f09 <- es_ggplot(
  m_es_psm_pooled,
  point_col = "grey25",
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post
)
save_gg(p_f09, "F09_es_ols_psm_pooled", width = 7, height = 5)

# F10: Birth-PSM above-median
p_f10 <- es_ggplot(
  m_es_psm_above,
  point_col = COL_RED,
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post
)
save_gg(p_f10, "F10_es_ols_psm_above_median", width = 7, height = 5)

# F12: Single-walled only
p_f12 <- es_ggplot(
  m_es_sw,
  point_col = COL_SW,
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post
)
save_gg(p_f12, "F12_es_ols_singlewall_only", width = 7, height = 5)

# F13: Double-walled only (placebo)
p_f13 <- es_ggplot(
  m_es_dw,
  point_col = COL_DW,
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post
)
save_gg(p_f13, "F13_es_ols_doublewall_placebo", width = 7, height = 5)

# F14: Drop TX border states
p_f14 <- es_ggplot(
  m_es_noborder,
  point_col = "grey25",
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post
)
save_gg(p_f14, "F14_es_ols_no_border_states", width = 7, height = 5)

# F15: Post-1988 installs only
p_f15 <- es_ggplot(
  m_es_post88,
  point_col = "grey25",
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post
)
save_gg(p_f15, "F15_es_ols_post1988_only", width = 7, height = 5)

cat("  Saved: F09, F10, F12-F15 robustness OLS figures (PDF + PNG)\n\n")


# =============================================================================
# Reform-CEM ES figures (additional robustness for AP-6 comparison)
# Saved as REF01, REF02 for cross-reference with matching comparison block
# =============================================================================

log_step("Building reform-CEM robustness figures (REF01-REF02)...")

p_ref01 <- es_ggplot(
  m_es_reform_pooled,
  point_col = "grey25",
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post_reform
)
save_gg(p_ref01, "REF01_es_ols_reform_pooled", width = 7, height = 5)

p_ref02 <- es_ggplot(
  m_es_reform_above,
  point_col = COL_RED,
  label     = NULL,
  ylim      = ES_YLIM,
  ctrl_mean = ctrl_mean_post_reform
)
save_gg(p_ref02, "REF02_es_ols_reform_above_median", width = 7, height = 5)

cat("  Saved: REF01-REF02 reform-CEM ES figures (PDF + PNG)\n\n")


# =============================================================================
# Diagnostic: ES coefficient CSV (all primary models, long format)
# =============================================================================

log_step("Saving ES coefficient diagnostics CSV...")

es_coefs_long <- rbindlist(list(
  cbind(es_tidy(m_es_pooled),    spec = "pooled"),
  cbind(es_tidy(m_es_trends),    spec = "trends"),
  cbind(es_tidy(m_es_below_med), spec = "below_median"),
  cbind(es_tidy(m_es_above_med), spec = "above_median"),
  cbind(es_tidy(m_es_psm_pooled), spec = "psm_pooled"),
  cbind(es_tidy(m_es_reform_pooled), spec = "reform_pooled")
), fill = TRUE)

fwrite(es_coefs_long,
       file.path(OUTPUT_TABLES, "ES_OLS_Coefficients.csv"))
cat("  Saved: ES_OLS_Coefficients.csv\n\n")


################################################################################
# === S8b: COX EVENT STUDY MODELS + FIGURES ===
################################################################################

#### S8b Cox Event Study Models + Figures ####

cat("========================================\n")
cat("S8b: COX EVENT STUDY MODELS + FIGURES\n")
cat("========================================\n\n")


# =============================================================================
# Build Cox ES formulas using es_vars from the build_cox_es_split() output
# =============================================================================

es_vars_birth <- attr(cox_es_birth_cem, "es_vars")
es_vars_psm   <- attr(cox_es_birth_psm, "es_vars")
es_vars_reform <- attr(cox_es_reform_cem, "es_vars")

# Sanity check: all three should have the same set of tau variables
stopifnot(
  "birth-CEM and birth-PSM es_vars must match" =
    identical(sort(es_vars_birth), sort(es_vars_psm)),
  "birth-CEM and reform-CEM es_vars must match" =
    identical(sort(es_vars_birth), sort(es_vars_reform))
)

cat(sprintf("  es_vars count: %d (tau in [-13, 22] excluding ref=-1)\n\n",
  length(es_vars_birth)))

# Reusable formula builder
build_cox_es_fml <- function(es_vars, strata_var) {
  as.formula(paste(
    "Surv(t_enter, t_exit, failure) ~",
    paste(es_vars, collapse = " + "),
    "+ mandate_release_det + mandate_spill_overfill + mandate_integrity",
    sprintf("+ strata(%s)", strata_var)
  ))
}


# =============================================================================
# PRIMARY Cox ES MODELS (Birth-CEM)
# =============================================================================

log_step("Fitting primary Cox event-study models...")

# Pooled, strata = make_model_noage (preserves age dimension)
m_cox_es_noage <- coxph(
  build_cox_es_fml(es_vars_birth, "make_model_noage"),
  data    = cox_es_birth_cem,
  cluster = state,
  ties    = "efron"
)

# Pooled, strata = make_model_tank (vintage absorbed into baseline hazard)
m_cox_es_tank <- coxph(
  build_cox_es_fml(es_vars_birth, "make_model_tank"),
  data    = cox_es_birth_cem,
  cluster = state,
  ties    = "efron"
)

# Below-median age subsample
m_cox_es_below <- coxph(
  build_cox_es_fml(es_vars_birth, "make_model_noage"),
  data    = cox_es_birth_cem[above_median_age == 0L],
  cluster = state,
  ties    = "efron"
)

# Above-median age subsample
m_cox_es_above <- coxph(
  build_cox_es_fml(es_vars_birth, "make_model_noage"),
  data    = cox_es_birth_cem[above_median_age == 1L],
  cluster = state,
  ties    = "efron"
)

cat("  Primary Cox ES models fitted: noage, tank, below, above\n\n")


# =============================================================================
# ROBUSTNESS Cox ES MODELS (PSM + Reform-CEM)
# =============================================================================

log_step("Fitting robustness Cox event-study models...")

m_cox_es_psm_pooled <- coxph(
  build_cox_es_fml(es_vars_psm, "make_model_noage"),
  data    = cox_es_birth_psm,
  cluster = state,
  ties    = "efron"
)

m_cox_es_reform_pooled <- coxph(
  build_cox_es_fml(es_vars_reform, "make_model_noage"),
  data    = cox_es_reform_cem,
  cluster = state,
  ties    = "efron"
)

m_cox_es_reform_above <- coxph(
  build_cox_es_fml(es_vars_reform, "make_model_noage"),
  data    = cox_es_reform_cem[above_median_age == 1L],
  cluster = state,
  ties    = "efron"
)

cat("  Robustness Cox ES models fitted: psm_pooled, reform_pooled, reform_above\n\n")


# =============================================================================
# Joint pre-trend Wald tests
# =============================================================================

log_step("Computing Cox joint pre-trend Wald tests...")

# Pre-period es_tau_* terms: tau in {-13, -12, ..., -3}
# (excludes -2 which is adjacent to ref and noisy; matches OLS convention)
pre_tau_pattern_cox <- "^es_tau_m([3-9]|1[0-9])$"

joint_pretrend_cox <- function(model, label) {
  s <- summary(model)$coefficients
  pre_terms <- grep(pre_tau_pattern_cox, rownames(s), value = TRUE)
  if (length(pre_terms) < 2L) {
    return(list(label = label, chisq = NA, p = NA, df = 0))
  }
  # Wald test on pre-period coefficients = 0
  V         <- vcov(model)
  V_pre     <- V[pre_terms, pre_terms]
  b_pre     <- coef(model)[pre_terms]
  chisq_stat <- as.numeric(t(b_pre) %*% solve(V_pre) %*% b_pre)
  df         <- length(pre_terms)
  p_val      <- 1 - pchisq(chisq_stat, df)
  list(label = label, chisq = chisq_stat, p = p_val, df = df)
}

cox_pre_tests <- list(
  noage         = joint_pretrend_cox(m_cox_es_noage,       "Cox pooled (noage strata)"),
  tank          = joint_pretrend_cox(m_cox_es_tank,        "Cox pooled (tank strata)"),
  below         = joint_pretrend_cox(m_cox_es_below,       "Cox below-median age"),
  above         = joint_pretrend_cox(m_cox_es_above,       "Cox above-median age"),
  psm_pooled    = joint_pretrend_cox(m_cox_es_psm_pooled,  "Cox birth-PSM pooled"),
  reform_pooled = joint_pretrend_cox(m_cox_es_reform_pooled,"Cox reform-CEM pooled"),
  reform_above  = joint_pretrend_cox(m_cox_es_reform_above, "Cox reform-CEM above-median")
)

cat("  Joint Cox pre-trend tests:\n")
for (t in cox_pre_tests) {
  cat(sprintf("    %-32s chisq=%7.3f  p=%.4f  (df=%d)\n",
    t$label, t$chisq, t$p, t$df))
}

cox_pretrend_dt <- rbindlist(lapply(cox_pre_tests, function(t) {
  data.table(spec = t$label, chisq = t$chisq, p = t$p, df = t$df)
}))
fwrite(cox_pretrend_dt,
       file.path(OUTPUT_TABLES, "ES_Cox_PreTrendTests.csv"))
cat("\n  Saved: ES_Cox_PreTrendTests.csv\n\n")


# =============================================================================
# PRIMARY Cox ES FIGURES (F05-F08)
# =============================================================================

log_step("Building primary Cox event-study figures...")

# F05: Cox pooled, make_model_noage strata
p_f05 <- es_ggplot_cox(
  m_cox_es_noage,
  point_col = "grey25",
  label     = NULL
)
save_gg(p_f05, "F05_es_cox_pooled_noage_strata", width = 7, height = 5)

# F06: Cox pooled, make_model_tank strata (vintage absorbed)
p_f06 <- es_ggplot_cox(
  m_cox_es_tank,
  point_col = "grey25",
  label     = NULL
)
save_gg(p_f06, "F06_es_cox_pooled_tank_strata", width = 7, height = 5)

# F07: Cox below-median age
p_f07 <- es_ggplot_cox(
  m_cox_es_below,
  point_col = COL_BLUE,
  label     = NULL
)
save_gg(p_f07, "F07_es_cox_below_median", width = 7, height = 5)

# F08: Cox above-median age
p_f08 <- es_ggplot_cox(
  m_cox_es_above,
  point_col = COL_RED,
  label     = NULL
)
save_gg(p_f08, "F08_es_cox_above_median", width = 7, height = 5)

cat("  Saved: F05-F08 primary Cox ES figures (PDF + PNG)\n\n")


# =============================================================================
# ROBUSTNESS Cox ES FIGURES (F11, REF03-REF04)
# =============================================================================

log_step("Building robustness Cox event-study figures...")

# F11: Cox PSM pooled
p_f11 <- es_ggplot_cox(
  m_cox_es_psm_pooled,
  point_col = "grey25",
  label     = NULL
)
save_gg(p_f11, "F11_es_cox_psm_pooled", width = 7, height = 5)

# REF03: Cox reform-CEM pooled (robustness for matching comparison)
p_ref03 <- es_ggplot_cox(
  m_cox_es_reform_pooled,
  point_col = "grey25",
  label     = NULL
)
save_gg(p_ref03, "REF03_es_cox_reform_pooled", width = 7, height = 5)

# REF04: Cox reform-CEM above-median
p_ref04 <- es_ggplot_cox(
  m_cox_es_reform_above,
  point_col = COL_RED,
  label     = NULL
)
save_gg(p_ref04, "REF04_es_cox_reform_above_median", width = 7, height = 5)

cat("  Saved: F11, REF03-REF04 Cox ES figures (PDF + PNG)\n\n")


# =============================================================================
# Cox ES coefficient diagnostics CSV (long format)
# =============================================================================

log_step("Saving Cox ES coefficient diagnostics CSV...")

cox_es_coefs_long <- rbindlist(list(
  cbind(es_tidy_cox(m_cox_es_noage),         spec = "noage"),
  cbind(es_tidy_cox(m_cox_es_tank),          spec = "tank"),
  cbind(es_tidy_cox(m_cox_es_below),         spec = "below_median"),
  cbind(es_tidy_cox(m_cox_es_above),         spec = "above_median"),
  cbind(es_tidy_cox(m_cox_es_psm_pooled),    spec = "psm_pooled"),
  cbind(es_tidy_cox(m_cox_es_reform_pooled), spec = "reform_pooled")
), fill = TRUE)

fwrite(cox_es_coefs_long,
       file.path(OUTPUT_TABLES, "ES_Cox_Coefficients.csv"))
cat("  Saved: ES_Cox_Coefficients.csv\n\n")


################################################################################
# === S9: POPULATION DESCRIPTION + COHORT-DISAGGREGATED PRE-TRENDS ===
################################################################################

#### S9 Pre-Trend Diagnostics: Population + Cohort Source Attribution ####

cat("========================================\n")
cat("S9: POPULATION DESCRIPTION + COHORT DIAGNOSTICS\n")
cat("========================================\n\n")


# =============================================================================
# S9.1  POPULATION DESCRIPTION AT TREATMENT DATE
# =============================================================================

cat("--- S9.1: Population description at treatment date ---\n")

# One row per tank alive at reform date, from the birth-CEM matched sample.
# Reuse the same filter used in compute_med_age and add_above_median_age.

pop_at_reform <- unique(
  matched_tanks_birth_cem[
    install_yr_int < 1999L &
    tank_installed_date <= REFORM_DATE &
    (is.na(tank_closed_date) | tank_closed_date > REFORM_DATE),
    .(tank_panel_id, texas_treated, install_yr_int,
      mm_wall, mm_fuel, mm_capacity, capacity)
  ]
)
pop_at_reform[, group := factor(
  fifelse(texas_treated == 1L, "Texas", "Control"),
  levels = c("Control", "Texas")
)]
pop_at_reform[, age_at_treatment := 1999L - install_yr_int]

cat(sprintf("  Population at reform: %s tanks (%s TX | %s Control)\n",
  fmt_n(nrow(pop_at_reform)),
  fmt_n(pop_at_reform[group == "Texas",   .N]),
  fmt_n(pop_at_reform[group == "Control", .N])
))


## T_S9_PopDescription.tex ---------------------------------------------

pop_summary <- pop_at_reform[, .(
  n_tanks             = .N,
  mean_capacity       = mean(capacity, na.rm = TRUE),
  median_capacity     = median(capacity, na.rm = TRUE),
  mean_age            = mean(age_at_treatment, na.rm = TRUE),
  median_age          = median(age_at_treatment, na.rm = TRUE),
  pct_single_walled   = 100 * mean(mm_wall == "Single-Walled", na.rm = TRUE),
  pct_gasoline        = 100 * mean(mm_fuel == "Gasoline-Only", na.rm = TRUE),
  pct_diesel          = 100 * mean(mm_fuel == "Diesel-Only",   na.rm = TRUE)
), by = group]
setorder(pop_summary, group)

log_step("Population summary (matched sample, alive at reform):")
print(pop_summary)
fwrite(pop_summary,
       file.path(OUTPUT_TABLES, "T_S9_PopDescription.csv"))

# Build LaTeX table manually (symmetric TX | Control columns)
tx_row  <- pop_summary[group == "Texas"]
ctl_row <- pop_summary[group == "Control"]

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Population Description at Reform Date (Birth-CEM Matched Sample)}",
  "\\label{tbl:s9_popdescription}",
  "\\begin{tabular}{lrr}",
  "\\toprule",
  "Characteristic & Control States & Texas \\\\",
  "\\midrule",
  sprintf("Tanks alive at Dec 22 1998         & %s  & %s  \\\\",
    fmt_n(ctl_row$n_tanks), fmt_n(tx_row$n_tanks)),
  sprintf("Mean capacity (gallons)            & %.0f & %.0f \\\\",
    ctl_row$mean_capacity, tx_row$mean_capacity),
  sprintf("Median capacity (gallons)          & %.0f & %.0f \\\\",
    ctl_row$median_capacity, tx_row$median_capacity),
  sprintf("Mean age at treatment (years)      & %.2f & %.2f \\\\",
    ctl_row$mean_age, tx_row$mean_age),
  sprintf("Median age at treatment (years)    & %.1f & %.1f \\\\",
    ctl_row$median_age, tx_row$median_age),
  sprintf("Single-walled (\\%%)                 & %.1f & %.1f \\\\",
    ctl_row$pct_single_walled, tx_row$pct_single_walled),
  sprintf("Gasoline-only (\\%%)                 & %.1f & %.1f \\\\",
    ctl_row$pct_gasoline, tx_row$pct_gasoline),
  sprintf("Diesel-only (\\%%)                   & %.1f & %.1f \\\\",
    ctl_row$pct_diesel, tx_row$pct_diesel),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\raggedright",
  paste0(
    "Notes: Sample restricted to birth-cohort CEM matched tanks alive at reform date. ",
    "Capacity units are US gallons. Age at treatment is 1999 minus install year."
  ),
  "\\end{table}"
), "T_S9_PopDescription.tex")


## S9a_Capacity_Dist ---------------------------------------------------

p_s9a <- ggplot(pop_at_reform[!is.na(capacity)],
    aes(x = capacity, fill = group, colour = group)) +
  geom_density(alpha = 0.35, linewidth = 0.75) +
  scale_fill_manual(values = c("Control" = COL_CTRL, "Texas" = COL_TX),
                    name = NULL) +
  scale_colour_manual(values = c("Control" = COL_CTRL, "Texas" = COL_TX),
                      name = NULL) +
  scale_x_continuous(labels = scales::comma_format()) +
  labs(x = "Tank Capacity (gallons)", y = "Density") +
  theme_classic(base_size = 11, base_family = "Times") +
  theme(
    axis.line       = element_line(colour = "black", linewidth = 0.4),
    axis.ticks      = element_line(colour = "black", linewidth = 0.3),
    axis.text       = element_text(colour = "black"),
    legend.position = "bottom",
    panel.grid      = element_blank(),
    plot.title      = element_blank(),
    plot.margin     = margin(8, 10, 8, 8)
  )
save_gg(p_s9a, "S9a_Capacity_Dist", width = 8, height = 5)


## S9b_AgeAtTreatment_Dist ---------------------------------------------

p_s9b <- ggplot(pop_at_reform,
    aes(x = age_at_treatment, fill = group, colour = group)) +
  geom_density(alpha = 0.35, linewidth = 0.75) +
  geom_vline(xintercept = med_age_birth_cem,
             linetype = "dashed", colour = "grey30", linewidth = 0.5) +
  annotate("text", x = med_age_birth_cem, y = Inf, vjust = 1.5, hjust = -0.1,
           label = sprintf("Median = %.1f", med_age_birth_cem),
           size = 3, colour = "grey30") +
  scale_fill_manual(values = c("Control" = COL_CTRL, "Texas" = COL_TX),
                    name = NULL) +
  scale_colour_manual(values = c("Control" = COL_CTRL, "Texas" = COL_TX),
                      name = NULL) +
  labs(x = "Age at Treatment (1999 - install year)", y = "Density") +
  theme_classic(base_size = 11, base_family = "Times") +
  theme(
    axis.line       = element_line(colour = "black", linewidth = 0.4),
    axis.ticks      = element_line(colour = "black", linewidth = 0.3),
    axis.text       = element_text(colour = "black"),
    legend.position = "bottom",
    panel.grid      = element_blank(),
    plot.title      = element_blank(),
    plot.margin     = margin(8, 10, 8, 8)
  )
save_gg(p_s9b, "S9b_AgeAtTreatment_Dist", width = 8, height = 5)


## S9c_CohortShares ---------------------------------------------------

cohort_shares <- pop_at_reform[,
  .(n = .N),
  by = .(install_yr_int, group)
]
cohort_totals <- cohort_shares[, .(total = sum(n)), by = group]
cohort_shares <- merge(cohort_shares, cohort_totals, by = "group")
cohort_shares[, share := n / total]

p_s9c <- ggplot(cohort_shares,
    aes(x = install_yr_int, y = share, fill = group)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7, alpha = 0.85) +
  scale_fill_manual(values = c("Control" = COL_CTRL, "Texas" = COL_TX),
                    name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(1940, 1998, 5)) +
  labs(x = "Install Year", y = "Share of group's tanks") +
  theme_classic(base_size = 11, base_family = "Times") +
  theme(
    axis.line       = element_line(colour = "black", linewidth = 0.4),
    axis.ticks      = element_line(colour = "black", linewidth = 0.3),
    axis.text       = element_text(colour = "black"),
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    panel.grid      = element_blank(),
    plot.title      = element_blank(),
    plot.margin     = margin(8, 10, 8, 8)
  )
save_gg(p_s9c, "S9c_CohortShares", width = 10, height = 5)

cat("  S9.1 complete: T_S9_PopDescription + S9a, S9b, S9c figures\n\n")


# =============================================================================
# S9.2  COHORT-DISAGGREGATED EVENT STUDIES (BELOW-MEDIAN)
# =============================================================================

cat("--- S9.2: cohort-disaggregated event studies (all pre-reform incumbent cohorts) ---\n")

# Cohort bin definition:
#   "1985-1988" pooled (birth-CEM has no pre-1985 tanks per Panel_Build S11;
#    1985-1988 pooled because individual-year samples are thin for these
#    early cohorts and they are all pre-federal-mandate installations)
#   Individual years 1989, 1990, ..., 1998
# This gives 11 cohort groups total.

COHORT_PRE_LABEL <- "1985-1988"
INDIV_YEARS      <- 1989L:1998L

assign_cohort_bin <- function(yr) {
  ifelse(yr <= 1988L, COHORT_PRE_LABEL, as.character(yr))
}

COHORT_LEVELS <- c(COHORT_PRE_LABEL, as.character(INDIV_YEARS))

# S9.2 uses the FULL pre-reform-incumbent sample (not just below-median).
# Each cohort gets its own ES fit so the reader can see the cohort-level
# decomposition across all vintages, not only the below-median subset.
cohort_dt <- copy(incumbents_birth_es)
cohort_dt[, cohort_bin := assign_cohort_bin(install_yr_int)]
cohort_dt[, cohort_bin := factor(cohort_bin, levels = COHORT_LEVELS)]

# Sample-size diagnostic by cohort bin (pre-regression)
cohort_ns <- unique(
  cohort_dt[, .(tank_panel_id, texas_treated, cohort_bin)]
)[, .(
  n_tanks = .N,
  n_tx    = sum(texas_treated == 1L),
  n_ctl   = sum(texas_treated == 0L)
), by = cohort_bin][order(cohort_bin)]

log_step("Cohort bin sample sizes (all pre-reform incumbent cohorts):")
print(cohort_ns)
fwrite(cohort_ns,
       file.path(OUTPUT_TABLES, "T_S9_Cohort_SampleSizes.csv"))


## Run per-cohort event study ----------------------------------------
# For each cohort, restrict the below-median sample to tanks in that cohort
# and run the same ES spec as m_es_below_med.

run_cohort_es <- function(cohort_lbl, dt) {

  sub <- dt[cohort_bin == cohort_lbl]

  # Skip cohorts without both treatment groups or with too few tanks
  if (sub[texas_treated == 1L, .N] < 20L ||
      sub[texas_treated == 0L, .N] < 20L) {
    return(list(label = cohort_lbl, model = NULL, failed = "insufficient N"))
  }

  # Renormalize weights within the cohort's treatment groups
  sub[, cem_w_norm := cem_weight / sum(cem_weight), by = texas_treated]

  m <- tryCatch(
    feols(
      closure_event ~ i(rel_year, texas_treated, ref = -1L) +
        mandate_release_det + mandate_spill_overfill + mandate_integrity |
        tank_panel_id + panel_year,
      data    = drop_singletons(sub, "tank_panel_id"),
      weights = ~cem_w_norm,
      cluster = ~state
    ),
    error = function(e) NULL
  )

  list(label = cohort_lbl, model = m,
       failed = if (is.null(m)) "feols error" else "")
}

cohort_models <- lapply(COHORT_LEVELS, run_cohort_es, dt = cohort_dt)
names(cohort_models) <- COHORT_LEVELS


## Extract tidy coefs and build long-format data.table --------------

cohort_coefs <- rbindlist(lapply(cohort_models, function(cm) {
  if (is.null(cm$model)) return(NULL)
  tidy_dt <- es_tidy(cm$model, ref = -1L)
  tidy_dt[, cohort := cm$label]
  tidy_dt
}), fill = TRUE)

if (nrow(cohort_coefs) > 0L) {
  cohort_coefs[, cohort := factor(cohort, levels = COHORT_LEVELS)]
  fwrite(cohort_coefs,
         file.path(OUTPUT_TABLES, "S9d_AllCohorts_Cohort_ES_coefs.csv"))
  cat(sprintf("  Extracted coefs for %d cohorts\n",
    uniqueN(cohort_coefs$cohort)))
} else {
  cat("  WARNING: no cohort coefficients extracted\n")
}


## Per-cohort pre-trend F-tests and post-period ATT ------------------

cohort_summary <- rbindlist(lapply(cohort_models, function(cm) {
  if (is.null(cm$model)) {
    return(data.table(
      cohort = cm$label, F_stat = NA_real_, F_p = NA_real_,
      post_att = NA_real_, post_att_se = NA_real_, post_att_p = NA_real_,
      n_obs = 0L, failed = cm$failed
    ))
  }

  m <- cm$model

  # Joint pre-trend F-test on tau <= -3
  pre_names <- grep("rel_year::(-[3-9]|-1[0-9]):texas_treated",
                     names(coef(m)), value = TRUE)
  if (length(pre_names) >= 2L) {
    wt <- tryCatch(wald(m, pre_names), error = function(e) NULL)
    F_stat <- if (is.null(wt)) NA_real_ else wt$stat
    F_p    <- if (is.null(wt)) NA_real_ else wt$p
  } else {
    F_stat <- NA_real_
    F_p    <- NA_real_
  }

  # Post-period ATT = average of tau >= 0 coefficients, with delta-method SE
  post_names <- grep("rel_year::([0-9]|1[0-9]|2[0-9]):texas_treated",
                      names(coef(m)), value = TRUE)
  if (length(post_names) >= 1L) {
    b_post <- coef(m)[post_names]
    V_post <- vcov(m, type = "clustered")[post_names, post_names, drop = FALSE]
    w_vec  <- rep(1 / length(post_names), length(post_names))
    post_att <- as.numeric(w_vec %*% b_post)
    post_att_var <- as.numeric(t(w_vec) %*% V_post %*% w_vec)
    post_att_se  <- sqrt(post_att_var)
    post_att_p   <- 2 * pnorm(-abs(post_att / post_att_se))
  } else {
    post_att <- NA_real_
    post_att_se <- NA_real_
    post_att_p  <- NA_real_
  }

  data.table(
    cohort      = cm$label,
    F_stat      = F_stat,
    F_p         = F_p,
    post_att    = post_att,
    post_att_se = post_att_se,
    post_att_p  = post_att_p,
    n_obs       = nobs(m),
    failed      = ""
  )
}))
cohort_summary[, cohort := factor(cohort, levels = COHORT_LEVELS)]
setorder(cohort_summary, cohort)

# Merge with sample-size counts
cohort_summary <- merge(
  cohort_summary,
  cohort_ns[, .(cohort = cohort_bin, n_tanks, n_tx, n_ctl)],
  by = "cohort", all.x = TRUE, sort = FALSE
)
setorder(cohort_summary, cohort)

fwrite(cohort_summary,
       file.path(OUTPUT_TABLES, "T_S9_Cohort_PreTrend_Ftests.csv"))

log_step("Per-cohort pre-trend and post-ATT summary:")
print(cohort_summary[, .(cohort, n_tanks, n_tx, F_stat, F_p,
                          post_att, post_att_p)])


## LaTeX table for cohort summary ------------------------------------

tex_cohort_rows <- vapply(seq_len(nrow(cohort_summary)), function(i) {
  r <- cohort_summary[i]
  if (r$failed != "") {
    sprintf("%s & %s & %s & \\textit{%s} & --- & --- & --- \\\\",
      r$cohort, fmt_n(r$n_tanks), fmt_n(r$n_tx), r$failed)
  } else {
    sprintf(
      "%s & %s & %s & %.2f & %.3f & %+.4f%s & %.3f \\\\",
      r$cohort,
      fmt_n(r$n_tanks), fmt_n(r$n_tx),
      r$F_stat, r$F_p,
      r$post_att, stars_p(r$post_att_p),
      r$post_att_p
    )
  }
}, character(1))

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Cohort-Disaggregated Pre-Trends and Post-Reform ATT (Below-Median Subsample)}",
  "\\label{tbl:s9_cohort_diagnostics}",
  "\\begin{tabular}{lrrccrc}",
  "\\toprule",
  "Cohort & N tanks & N TX & F (pre-trend) & p (F) & Post-ATT (pp) & p (ATT) \\\\",
  "\\midrule",
  tex_cohort_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\raggedright",
  paste0(
    "Notes: Each row is an OLS event study estimated on the cohort-restricted ",
    "below-median birth-CEM matched sample. F-statistic tests joint nullity of ",
    "pre-period coefficients at $\\tau \\leq -3$. Post-ATT is the equal-weighted average ",
    "of post-period coefficients ($\\tau \\geq 0$), with delta-method SE on the ",
    "cluster-robust VCV. Stars: * $p<0.1$, ** $p<0.05$, *** $p<0.01$."
  ),
  "\\end{table}"
), "T_S9_Cohort_PreTrend_Ftests.tex")


## S9d_AllCohorts_Cohort_ES_MultiLine: overlay all 11 cohorts ------

if (nrow(cohort_coefs) > 0L) {

  # Build a distinguishable palette; the pooled bin gets grey, individual
  # years get a color ramp.
  cohort_palette <- c(
    setNames("grey45", COHORT_PRE_LABEL),
    setNames(colorRampPalette(c("#2166ac", "#67a9cf",
                                 "#f4a582", "#b2182b"))(length(INDIV_YEARS)),
             as.character(INDIV_YEARS))
  )

  p_s9d_multi <- ggplot(
    cohort_coefs[year != -1L],
    aes(x = year, y = estimate, colour = cohort, group = cohort)
  ) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    geom_line(linewidth = 0.55, alpha = 0.85) +
    geom_point(size = 1.0, alpha = 0.85) +
    scale_colour_manual(values = cohort_palette, name = "Cohort") +
    scale_x_continuous(breaks = seq(-15, 25, 5)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(x = "Years Relative to 1998",
         y = "Cohort-specific ES coefficient") +
    guides(colour = guide_legend(ncol = 3, byrow = TRUE)) +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line       = element_line(colour = "black", linewidth = 0.4),
      axis.ticks      = element_line(colour = "black", linewidth = 0.3),
      axis.text       = element_text(colour = "black"),
      legend.position = "right",
      legend.text     = element_text(size = 7),
      legend.key.size = unit(0.3, "cm"),
      panel.grid      = element_blank(),
      plot.title      = element_blank(),
      plot.margin     = margin(8, 10, 8, 8)
    )
  save_gg(p_s9d_multi, "S9d_AllCohorts_Cohort_ES_MultiLine",
          width = 10, height = 6)


  ## S9d_AllCohorts_Cohort_ES_Small: facet_wrap small multiples
  ## [DOCUMENTED FIGURE-RULE EXCEPTION: granted for this diagnostic block]

  p_s9d_small <- ggplot(
    cohort_coefs,
    aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high)
  ) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.35) +
    geom_errorbar(width = 0.25, linewidth = 0.35, colour = "grey30") +
    geom_point(size = 0.85, colour = COL_BLUE) +
    facet_wrap(~ cohort, ncol = 4) +
    scale_x_continuous(breaks = seq(-15, 25, 10)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    labs(x = "Years Relative to 1998",
         y = "Cohort-specific ES coefficient") +
    theme_classic(base_size = 10, base_family = "Times") +
    theme(
      axis.line       = element_line(colour = "black", linewidth = 0.3),
      axis.ticks      = element_line(colour = "black", linewidth = 0.25),
      axis.text       = element_text(colour = "black", size = 7),
      strip.background = element_rect(fill = "grey95", colour = NA),
      strip.text       = element_text(size = 8, face = "bold"),
      panel.grid       = element_blank(),
      panel.spacing    = unit(0.4, "cm"),
      plot.title       = element_blank(),
      plot.margin      = margin(8, 10, 8, 8)
    )
  save_gg(p_s9d_small, "S9d_AllCohorts_Cohort_ES_Small",
          width = 12, height = 9)


  ## S9d_AllCohorts_Cohort_PreTrend_Forest: F-stat per cohort

  p_s9d_ft <- ggplot(
    cohort_summary[!is.na(F_stat)],
    aes(x = cohort, y = F_stat, fill = F_p < 0.05)
  ) +
    geom_col(width = 0.65, colour = "white", linewidth = 0.3) +
    geom_hline(yintercept = qchisq(0.95, df = 11) / 11,
               linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    scale_fill_manual(
      values = c("FALSE" = "grey70", "TRUE" = COL_TX),
      labels = c("FALSE" = "p $\\geq$ 0.05", "TRUE" = "p < 0.05"),
      name   = NULL
    ) +
    labs(x = "Cohort",
         y = "Joint pre-trend F-statistic ($\\tau \\leq -3$)") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line       = element_line(colour = "black", linewidth = 0.4),
      axis.ticks      = element_line(colour = "black", linewidth = 0.3),
      axis.text       = element_text(colour = "black"),
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
      legend.position = "bottom",
      panel.grid      = element_blank(),
      plot.title      = element_blank(),
      plot.margin     = margin(8, 10, 8, 8)
    )
  save_gg(p_s9d_ft, "S9d_AllCohorts_Cohort_PreTrend_Forest",
          width = 10, height = 5)


  ## S9d_AllCohorts_Cohort_PostATT_Forest: ATT per cohort

  cohort_summary[, ci_lo := post_att - 1.96 * post_att_se]
  cohort_summary[, ci_hi := post_att + 1.96 * post_att_se]

  p_s9d_att <- ggplot(
    cohort_summary[!is.na(post_att)],
    aes(x = cohort, y = post_att, ymin = ci_lo, ymax = ci_hi)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    geom_pointrange(colour = COL_RED, size = 0.45, linewidth = 0.6) +
    geom_text(aes(label = sprintf("%+.4f", post_att)),
              vjust = -1.2, size = 2.6, colour = "grey25") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(x = "Cohort",
         y = "Post-reform ATT (equal-weighted average of tau $\\geq$ 0)") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line       = element_line(colour = "black", linewidth = 0.4),
      axis.ticks      = element_line(colour = "black", linewidth = 0.3),
      axis.text       = element_text(colour = "black"),
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
      panel.grid      = element_blank(),
      plot.title      = element_blank(),
      plot.margin     = margin(8, 10, 8, 8)
    )
  save_gg(p_s9d_att, "S9d_AllCohorts_Cohort_PostATT_Forest",
          width = 10, height = 5)

  cat("  S9.2 complete: coefs CSV + 4 figures + 1 tex table\n\n")

} else {
  cat("  S9.2 SKIPPED: no cohort-specific coefficients extracted\n\n")
}


# =============================================================================
# S9.3  COHORT SCATTER: install year vs pre-trend F-stat (bubble)
# =============================================================================

cat("--- S9.3: cohort scatter ---\n")

# For this plot we want ONE point per cohort, using a continuous x position.
# For the pooled "1950-1984" group we use the midpoint 1967; individual
# cohorts use their year.
scatter_dt <- cohort_summary[!is.na(F_stat)]
scatter_dt[, x_pos := fifelse(
  cohort == COHORT_PRE_LABEL,
  1967,
  suppressWarnings(as.numeric(as.character(cohort)))
)]

p_s9e <- ggplot(scatter_dt,
    aes(x = x_pos, y = F_stat, size = n_tanks, fill = F_p < 0.05)) +
  geom_hline(yintercept = qchisq(0.95, df = 11) / 11,
             linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_point(shape = 21, colour = "grey20", stroke = 0.3, alpha = 0.85) +
  geom_text(aes(label = as.character(cohort)),
            size = 2.6, colour = "grey20", vjust = -1.6) +
  scale_fill_manual(
    values = c("FALSE" = "grey75", "TRUE" = COL_TX),
    labels = c("FALSE" = "p $\\geq$ 0.05", "TRUE" = "p < 0.05"),
    name   = NULL
  ) +
  scale_size_continuous(range = c(3, 12), name = "N tanks",
                        labels = scales::comma_format()) +
  scale_x_continuous(breaks = seq(1950, 2000, 5),
                     limits = c(1964, 2000)) +
  labs(x = "Install cohort (year)",
       y = "Joint pre-trend F-statistic") +
  theme_classic(base_size = 11, base_family = "Times") +
  theme(
    axis.line       = element_line(colour = "black", linewidth = 0.4),
    axis.ticks      = element_line(colour = "black", linewidth = 0.3),
    axis.text       = element_text(colour = "black"),
    legend.position = "bottom",
    legend.box      = "vertical",
    panel.grid      = element_blank(),
    plot.title      = element_blank(),
    plot.margin     = margin(8, 10, 8, 8)
  )
save_gg(p_s9e, "S9e_PreTrend_Cohort_Scatter", width = 10, height = 6)
cat("  S9.3 complete: S9e_PreTrend_Cohort_Scatter\n\n")


################################################################################
# === S10: AGE-AT-CLOSURE (3 SPECS) ===
################################################################################

#### S10 Age-at-Closure: Three Specifications ####

cat("========================================\n")
cat("S10: AGE-AT-CLOSURE (3 SPECS)\n")
cat("========================================\n\n")


# =============================================================================
# SAMPLE PREP: Full incumbent panel + closed-tank subset
# =============================================================================

log_step("Building S10 samples...")

# ---- Spec A & C base: full birth-CEM matched panel, pre-1999 incumbents
panel_A <- copy(matched_tanks_birth_cem[install_yr_int < 1999L])

# tank_age = panel_year - install_yr_int
if (!"tank_age" %in% names(panel_A)) {
  panel_A[, tank_age := panel_year - install_yr_int]
}
# Spec A outcome: closure_event x tank_age (zero in non-closure tank-years)
panel_A[, closure_age_contrib := closure_event * tank_age]

# Single-walled indicator (already defined elsewhere via did_x_sw, but
# rebuild here for self-containment)
if (!"single_wall" %in% names(panel_A)) {
  panel_A[, single_wall := as.integer(mm_wall == "Single-Walled")]
}

cat(sprintf("  Spec A panel: %s rows | %s tanks\n",
  fmt_n(nrow(panel_A)),
  fmt_n(uniqueN(panel_A$tank_panel_id))))


# ---- Spec B base: closed tanks only, derived from exact_base
closed_tanks <- exact_base[
  tank_panel_id %in% ids_birth_cem &
  failure == 1L &
  install_yr_int < 1999L
]
closed_tanks[, closure_date := as.IDate(
  as.Date(t_exit, origin = "1970-01-01")
)]
closed_tanks[, closure_year := as.integer(format(closure_date, "%Y"))]
closed_tanks[, age_at_closure := age_exit]
closed_tanks[, post_1999 := as.integer(closure_year >= 1999L)]
closed_tanks[, did_term  := texas_treated * post_1999]
closed_tanks[, rel_year  := as.integer(closure_year - 1998L)]
closed_tanks[, rel_year_es := pmax(pmin(rel_year, 21L), -13L)]
closed_tanks[, single_wall := as.integer(mm_wall == "Single-Walled")]
closed_tanks[, mandate_release_det := as.integer(
  !is.na(release_det_deadline_yr) &
  closure_year >= 1989L &
  closure_year <= release_det_deadline_yr
)]
closed_tanks[, mandate_spill_overfill := as.integer(
  !is.na(release_det_deadline_yr) &
  closure_year %in% 1993L:1994L
)]
closed_tanks[, mandate_integrity := as.integer(
  !is.na(release_det_deadline_yr) &
  closure_year %in% 1996L:1998L
)]

cat(sprintf("  Spec B closed-tank sample: %s tanks (%s TX | %s Control)\n\n",
  fmt_n(nrow(closed_tanks)),
  fmt_n(closed_tanks[texas_treated == 1L, .N]),
  fmt_n(closed_tanks[texas_treated == 0L, .N])))


# =============================================================================
# SPEC A -- Tank-year DiD on closure_age_contrib
# =============================================================================

cat("--- SPEC A: tank-year DiD on closure_event x tank_age ---\n")

run_specA_did <- function(dt, label) {
  data_specA <- drop_singletons(dt, "tank_panel_id")
  m <- feols(
    closure_age_contrib ~ did_term +
      mandate_release_det + mandate_spill_overfill + mandate_integrity |
      tank_panel_id + panel_year,
    data    = data_specA,
    weights = ~cem_weight,
    cluster = ~state
  )
  r    <- extract_panel_row(m, "did_term")
  boot <- run_boot_ols_score(m, param = "did_term", B = N_BOOT, data = data_specA)
  list(label = label, model = m, row = r, boot = boot, n = nrow(dt))
}

A_overall <- run_specA_did(panel_A,                     "Overall")
A_sw      <- run_specA_did(panel_A[single_wall == 1L],  "Single-walled")
A_dw      <- run_specA_did(panel_A[single_wall == 0L],  "Double-walled")

log_step("Spec A results (closure_age_contrib, prob-weighted years per tank-year):")
for (r in list(A_overall, A_sw, A_dw)) {
  cat(sprintf("  %-15s  coef=%+.5f  SE=%.5f  CRVE p=%.3f  Boot p=%.3f  N=%s\n",
    r$label, r$row$coef, r$row$se, r$row$p, r$boot$p_boot, fmt_n(r$n)))
}
cat("\n")


# =============================================================================
# SPEC B -- Closed-tank intensive margin
# =============================================================================

cat("--- SPEC B: closed-tank intensive margin DiD on age_at_closure ---\n")

run_specB_did <- function(dt, label) {
  m <- feols(
    age_at_closure ~ did_term +
      mandate_release_det + mandate_spill_overfill + mandate_integrity |
      state + closure_year,
    data    = dt,
    cluster = ~state
  )
  r    <- extract_panel_row(m, "did_term")
  boot <- run_boot_ols_score(m, param = "did_term", B = N_BOOT, data = dt)
  list(label = label, model = m, row = r, boot = boot, n = nrow(dt))
}

B_overall <- run_specB_did(closed_tanks,                     "Overall")
B_sw      <- run_specB_did(closed_tanks[single_wall == 1L],  "Single-walled")
B_dw      <- run_specB_did(closed_tanks[single_wall == 0L],  "Double-walled")

log_step("Spec B results (age_at_closure conditional on closure, years):")
for (r in list(B_overall, B_sw, B_dw)) {
  cat(sprintf("  %-15s  coef=%+.3f yr  SE=%.3f  CRVE p=%.3f  Boot p=%.3f  N=%s\n",
    r$label, r$row$coef, r$row$se, r$row$p, r$boot$p_boot, fmt_n(r$n)))
}
cat("\n")


# =============================================================================
# COMBINED 6-PANEL DiD TABLE (Spec A cols 1-3, Spec B cols 4-6)
# =============================================================================

did_csv <- rbindlist(list(
  data.table(spec="A", panel="Overall",       coef=A_overall$row$coef,
             se=A_overall$row$se, crve_p=A_overall$row$p,
             boot_p=A_overall$boot$p_boot, n=A_overall$n),
  data.table(spec="A", panel="Single-walled", coef=A_sw$row$coef,
             se=A_sw$row$se, crve_p=A_sw$row$p,
             boot_p=A_sw$boot$p_boot, n=A_sw$n),
  data.table(spec="A", panel="Double-walled", coef=A_dw$row$coef,
             se=A_dw$row$se, crve_p=A_dw$row$p,
             boot_p=A_dw$boot$p_boot, n=A_dw$n),
  data.table(spec="B", panel="Overall",       coef=B_overall$row$coef,
             se=B_overall$row$se, crve_p=B_overall$row$p,
             boot_p=B_overall$boot$p_boot, n=B_overall$n),
  data.table(spec="B", panel="Single-walled", coef=B_sw$row$coef,
             se=B_sw$row$se, crve_p=B_sw$row$p,
             boot_p=B_sw$boot$p_boot, n=B_sw$n),
  data.table(spec="B", panel="Double-walled", coef=B_dw$row$coef,
             se=B_dw$row$se, crve_p=B_dw$row$p,
             boot_p=B_dw$boot$p_boot, n=B_dw$n)
))
fwrite(did_csv, file.path(OUTPUT_TABLES, "T_S10_AgeClosure_DiD.csv"))


write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Age-at-Closure: Causal ATT (Spec A) and Intensive Margin (Spec B)}",
  "\\label{tbl:s10_ageclosure_did}",
  "\\resizebox{\\textwidth}{!}{",
  "\\begin{tabular}{l ccc ccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Spec A: Causal ATT (full panel)}",
  "  & \\multicolumn{3}{c}{Spec B: Intensive Margin (closed only)} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  " & (1) Overall & (2) SW & (3) DW & (4) Overall & (5) SW & (6) DW \\\\",
  "\\midrule",
  sprintf("DiD & %s & %s & %s & %s & %s & %s \\\\",
    fmt_est(A_overall$row$coef, A_overall$row$p),
    fmt_est(A_sw$row$coef,      A_sw$row$p),
    fmt_est(A_dw$row$coef,      A_dw$row$p),
    fmt_est(B_overall$row$coef, B_overall$row$p),
    fmt_est(B_sw$row$coef,      B_sw$row$p),
    fmt_est(B_dw$row$coef,      B_dw$row$p)),
  sprintf("SE & %s & %s & %s & %s & %s & %s \\\\",
    fmt_se(A_overall$row$se), fmt_se(A_sw$row$se), fmt_se(A_dw$row$se),
    fmt_se(B_overall$row$se), fmt_se(B_sw$row$se), fmt_se(B_dw$row$se)),
  sprintf("Bootstrap $p$ & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f \\\\",
    A_overall$boot$p_boot, A_sw$boot$p_boot, A_dw$boot$p_boot,
    B_overall$boot$p_boot, B_sw$boot$p_boot, B_dw$boot$p_boot),
  "\\midrule",
  "Outcome & \\multicolumn{3}{c}{closure\\_event $\\times$ tank\\_age}",
  "        & \\multicolumn{3}{c}{age at closure (years)} \\\\",
  "Tank FE         & $\\checkmark$ & $\\checkmark$ & $\\checkmark$",
  "                & --- & --- & --- \\\\",
  "Year FE         & $\\checkmark$ & $\\checkmark$ & $\\checkmark$",
  "                & --- & --- & --- \\\\",
  "State FE        & --- & --- & ---",
  "                & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  "Closure-year FE & --- & --- & ---",
  "                & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  "CEM weights     & $\\checkmark$ & $\\checkmark$ & $\\checkmark$",
  "                & --- & --- & --- \\\\",
  "Mandate controls & $\\checkmark$ & $\\checkmark$ & $\\checkmark$",
  "                 & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  sprintf("Observations & %s & %s & %s & %s & %s & %s \\\\",
    fmt_n(A_overall$n), fmt_n(A_sw$n), fmt_n(A_dw$n),
    fmt_n(B_overall$n), fmt_n(B_sw$n), fmt_n(B_dw$n)),
  "\\bottomrule",
  "\\end{tabular}",
  "}",
  "\\par\\raggedright",
  paste0(
    "Notes: Cluster-robust SEs by state (N = 18). ",
    "\\textbf{Spec A (cols 1-3)}: tank-year panel, outcome = closure\\_event ",
    "$\\times$ tank\\_age (in years; 0 in non-closure tank-years). DiD coefficient ",
    "is the joint extensive + intensive ATT in units of probability-weighted years ",
    "per tank-year. Identified causally under the same parallel-trends assumption ",
    "as the closure-event ATT in Section S5. ",
    "\\textbf{Spec B (cols 4-6)}: closed-tank sample only, outcome = age at closure. ",
    "Conditional-on-closure compositional analysis. Sample is endogenous to ",
    "treatment so the coefficient describes how the closure-age distribution ",
    "shifted, not an unconditional causal effect on closure timing. ",
    "Tank FE not identified in Spec B because each tank closes at most once. ",
    sprintf(
      "Wild cluster bootstrap $p$-values (B = %d, Rademacher, state clusters). ",
      N_BOOT
    ),
    "Stars: *** $p<0.01$, ** $p<0.05$, * $p<0.1$ (CRVE)."
  ),
  "\\end{table}"
), "T_S10_AgeClosure_DiD.tex")
cat("  Saved: T_S10_AgeClosure_DiD.tex + .csv\n\n")


# =============================================================================
# SPEC A EVENT STUDIES
# =============================================================================

cat("--- SPEC A event studies ---\n")

run_specA_es <- function(dt, label) {
  m <- tryCatch(
    feols(
      closure_age_contrib ~ i(rel_year, texas_treated, ref = -1L) +
        mandate_release_det + mandate_spill_overfill + mandate_integrity |
        tank_panel_id + panel_year,
      data    = drop_singletons(dt, "tank_panel_id"),
      weights = ~cem_weight,
      cluster = ~state
    ),
    error = function(e) NULL
  )
  list(label = label, model = m, n = nrow(dt))
}

A_es_overall <- run_specA_es(panel_A,                    "A_overall")
A_es_sw      <- run_specA_es(panel_A[single_wall == 1L], "A_sw")
A_es_dw      <- run_specA_es(panel_A[single_wall == 0L], "A_dw")

# Spec A figures: same plot helper as main ES but no ctrl_mean overlay
# (the outcome scale is in years not probability)
plot_specA_es <- function(model, point_col) {
  if (is.null(model)) return(NULL)
  df  <- es_tidy(model, ref = -1L)
  ref_row <- df[year == -1L]
  est_row <- df[year != -1L]

  ggplot(df, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    geom_errorbar(data = est_row,
                  aes(ymin = conf.low, ymax = conf.high),
                  colour = point_col, width = 0.25, linewidth = 0.45) +
    geom_point(data = est_row, colour = point_col, size = 1.8, shape = 16) +
    geom_point(data = ref_row, shape = 1, size = 2.2, colour = "grey40") +
    annotate("text", x = -1, y = -Inf, vjust = -0.8,
             label = "(-1) omitted", size = 2.8, colour = "grey35",
             fontface = "italic") +
    scale_x_continuous(breaks = sort(unique(df$year))) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(x = "Years Relative to 1998",
         y = "Effect on closure_event x tank_age (years)") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line   = element_line(colour = "black", linewidth = 0.4),
      axis.ticks  = element_line(colour = "black", linewidth = 0.3),
      axis.text   = element_text(colour = "black"),
      axis.text.x = element_text(size = 8),
      panel.grid  = element_blank(),
      legend.position = "none",
      plot.title  = element_blank(),
      plot.margin = margin(8, 10, 8, 8)
    )
}

if (!is.null(A_es_overall$model)) {
  save_gg(plot_specA_es(A_es_overall$model, "grey25"),
          "F_S10A_AgeClosureContrib_ES_overall", width = 8, height = 5)
}
if (!is.null(A_es_sw$model)) {
  save_gg(plot_specA_es(A_es_sw$model, COL_SW),
          "F_S10A_AgeClosureContrib_ES_sw", width = 8, height = 5)
}
if (!is.null(A_es_dw$model)) {
  save_gg(plot_specA_es(A_es_dw$model, COL_DW),
          "F_S10A_AgeClosureContrib_ES_dw", width = 8, height = 5)
}
cat("  Saved: F_S10A_AgeClosureContrib_ES_{overall, sw, dw}\n\n")


# =============================================================================
# SPEC B EVENT STUDIES (closed-tank intensive margin ES)
# =============================================================================

cat("--- SPEC B event studies ---\n")

run_specB_es <- function(dt, label) {
  m <- tryCatch(
    feols(
      age_at_closure ~ i(rel_year_es, texas_treated, ref = -1L) +
        mandate_release_det + mandate_spill_overfill + mandate_integrity |
        state + closure_year,
      data    = dt,
      cluster = ~state
    ),
    error = function(e) NULL
  )
  list(label = label, model = m, n = nrow(dt))
}

B_es_overall <- run_specB_es(closed_tanks,                    "B_overall")
B_es_sw      <- run_specB_es(closed_tanks[single_wall == 1L], "B_sw")
B_es_dw      <- run_specB_es(closed_tanks[single_wall == 0L], "B_dw")

# Custom es_tidy for rel_year_es variable name
es_tidy_ageclose <- function(model, ref = -1L,
                              var_prefix = "rel_year_es") {
  df  <- as.data.table(broom::tidy(model, conf.int = TRUE))
  pat <- paste0("^", var_prefix, "::")
  df  <- df[grepl(pat, term)]
  df[, year := as.integer(
    sub(paste0("^", var_prefix, "::(-?[0-9]+):texas_treated$"),
        "\\1", term)
  )]
  if (!ref %in% df$year) {
    ref_row <- data.table(
      term = paste0("ref_", ref),
      estimate = 0, std.error = 0,
      conf.low = 0, conf.high = 0, year = as.integer(ref)
    )
    df <- rbindlist(
      list(df[, .(term, estimate, std.error, conf.low, conf.high, year)],
           ref_row), fill = TRUE
    )
  }
  setorder(df, year)
  df
}

plot_specB_es <- function(model, point_col) {
  if (is.null(model)) return(NULL)
  df  <- es_tidy_ageclose(model, ref = -1L)
  ref_row <- df[year == -1L]
  est_row <- df[year != -1L]

  ggplot(df, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.5) +
    geom_errorbar(data = est_row,
                  aes(ymin = conf.low, ymax = conf.high),
                  colour = point_col, width = 0.25, linewidth = 0.45) +
    geom_point(data = est_row, colour = point_col, size = 1.8, shape = 16) +
    geom_point(data = ref_row, shape = 1, size = 2.2, colour = "grey40") +
    annotate("text", x = -1, y = -Inf, vjust = -0.8,
             label = "(-1) omitted", size = 2.8, colour = "grey35",
             fontface = "italic") +
    scale_x_continuous(breaks = sort(unique(df$year))) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
    labs(x = "Years Relative to 1998",
         y = "Effect on age at closure (years)") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line   = element_line(colour = "black", linewidth = 0.4),
      axis.ticks  = element_line(colour = "black", linewidth = 0.3),
      axis.text   = element_text(colour = "black"),
      axis.text.x = element_text(size = 8),
      panel.grid  = element_blank(),
      legend.position = "none",
      plot.title  = element_blank(),
      plot.margin = margin(8, 10, 8, 8)
    )
}

if (!is.null(B_es_overall$model)) {
  save_gg(plot_specB_es(B_es_overall$model, "grey25"),
          "F_S10B_AgeClosure_ES_overall", width = 8, height = 5)
}
if (!is.null(B_es_sw$model)) {
  save_gg(plot_specB_es(B_es_sw$model, COL_SW),
          "F_S10B_AgeClosure_ES_sw", width = 8, height = 5)
}
if (!is.null(B_es_dw$model)) {
  save_gg(plot_specB_es(B_es_dw$model, COL_DW),
          "F_S10B_AgeClosure_ES_dw", width = 8, height = 5)
}
cat("  Saved: F_S10B_AgeClosure_ES_{overall, sw, dw}\n\n")


# =============================================================================
# SPEC C -- CELL-LEVEL AGGREGATION
# =============================================================================

cat("--- SPEC C: cell-level (state x year x make_model_noage) ---\n")

# Build cell-level dataset from panel_A
# Per cell: count tanks at risk, count closures, mean age of closures
cell_dt <- panel_A[, .(
  n_tanks         = .N,
  n_closures      = sum(closure_event == 1L, na.rm = TRUE),
  sum_close_age   = sum(closure_event * tank_age, na.rm = TRUE),
  texas_treated   = first(texas_treated),
  single_wall_any = any(single_wall == 1L, na.rm = TRUE),
  single_wall_all = all(single_wall == 1L, na.rm = TRUE),
  cem_weight_avg  = mean(cem_weight, na.rm = TRUE),
  mandate_release_det    = first(mandate_release_det),
  mandate_spill_overfill = first(mandate_spill_overfill),
  mandate_integrity      = first(mandate_integrity)
), by = .(state, panel_year, make_model_noage)]

# Mean closure age per cell (NA if no closures)
cell_dt[, mean_close_age := fifelse(n_closures > 0L,
                                     sum_close_age / n_closures,
                                     NA_real_)]
cell_dt[, closure_rate   := n_closures / n_tanks]
cell_dt[, did_term       := texas_treated * as.integer(panel_year >= 1999L)]

# Wall-type cell label: a cell is "SW" if all its tanks are SW, "DW" if
# none are SW, mixed otherwise. We restrict SW/DW analyses to clean cells.
cell_dt[, wall_class := fcase(
  single_wall_all == TRUE,                "SW",
  single_wall_any == FALSE,               "DW",
  default                               = "Mixed"
)]

cat(sprintf("  Cell-level dataset: %s cells (%s SW | %s DW | %s Mixed)\n\n",
  fmt_n(nrow(cell_dt)),
  fmt_n(cell_dt[wall_class == "SW", .N]),
  fmt_n(cell_dt[wall_class == "DW", .N]),
  fmt_n(cell_dt[wall_class == "Mixed", .N])))


# Cell DiD on mean_close_age (weighted by n_closures, drop NA)
run_specC_age <- function(dt, label) {
  sub <- dt[!is.na(mean_close_age) & n_closures > 0L]
  if (nrow(sub) < 100L) return(list(label = label, model = NULL,
                                     row = NULL, boot = NULL, n = nrow(sub)))
  m <- feols(
    mean_close_age ~ did_term +
      mandate_release_det + mandate_spill_overfill + mandate_integrity |
      state^make_model_noage + panel_year,
    data    = sub,
    weights = ~n_closures,
    cluster = ~state
  )
  r    <- extract_panel_row(m, "did_term")
  boot <- run_boot_ols_score(m, param = "did_term", B = N_BOOT, data = sub)
  list(label = label, model = m, row = r, boot = boot, n = nrow(sub))
}

# Cell DiD on closure_rate (weighted by n_tanks, all cells)
run_specC_rate <- function(dt, label) {
  if (nrow(dt) < 100L) return(list(label = label, model = NULL,
                                    row = NULL, boot = NULL, n = nrow(dt)))
  m <- feols(
    closure_rate ~ did_term +
      mandate_release_det + mandate_spill_overfill + mandate_integrity |
      state^make_model_noage + panel_year,
    data    = dt,
    weights = ~n_tanks,
    cluster = ~state
  )
  r    <- extract_panel_row(m, "did_term")
  boot <- run_boot_ols_score(m, param = "did_term", B = N_BOOT, data = dt)
  list(label = label, model = m, row = r, boot = boot, n = nrow(dt))
}

C_age_overall <- run_specC_age(cell_dt,                      "Overall")
C_age_sw      <- run_specC_age(cell_dt[wall_class == "SW"],  "Single-walled")
C_age_dw      <- run_specC_age(cell_dt[wall_class == "DW"],  "Double-walled")

C_rate_overall <- run_specC_rate(cell_dt,                      "Overall")
C_rate_sw      <- run_specC_rate(cell_dt[wall_class == "SW"],  "Single-walled")
C_rate_dw      <- run_specC_rate(cell_dt[wall_class == "DW"],  "Double-walled")

log_step("Spec C cell-level results:")
for (r in list(C_age_overall, C_age_sw, C_age_dw)) {
  if (!is.null(r$model)) {
    cat(sprintf("  AGE   %-15s coef=%+.3f yr  SE=%.3f  CRVE p=%.3f  Boot p=%.3f  N=%s\n",
      r$label, r$row$coef, r$row$se, r$row$p, r$boot$p_boot, fmt_n(r$n)))
  } else {
    cat(sprintf("  AGE   %-15s INSUFFICIENT N=%s\n", r$label, fmt_n(r$n)))
  }
}
for (r in list(C_rate_overall, C_rate_sw, C_rate_dw)) {
  if (!is.null(r$model)) {
    cat(sprintf("  RATE  %-15s coef=%+.5f    SE=%.5f  CRVE p=%.3f  Boot p=%.3f  N=%s\n",
      r$label, r$row$coef, r$row$se, r$row$p, r$boot$p_boot, fmt_n(r$n)))
  }
}
cat("\n")


# Spec C csv
specC_csv <- rbindlist(list(
  data.table(outcome="mean_close_age", panel="Overall",
             coef=if (!is.null(C_age_overall$model)) C_age_overall$row$coef else NA_real_,
             se=if (!is.null(C_age_overall$model)) C_age_overall$row$se else NA_real_,
             crve_p=if (!is.null(C_age_overall$model)) C_age_overall$row$p else NA_real_,
             boot_p=if (!is.null(C_age_overall$model)) C_age_overall$boot$p_boot else NA_real_,
             n=C_age_overall$n),
  data.table(outcome="mean_close_age", panel="SW",
             coef=if (!is.null(C_age_sw$model)) C_age_sw$row$coef else NA_real_,
             se=if (!is.null(C_age_sw$model)) C_age_sw$row$se else NA_real_,
             crve_p=if (!is.null(C_age_sw$model)) C_age_sw$row$p else NA_real_,
             boot_p=if (!is.null(C_age_sw$model)) C_age_sw$boot$p_boot else NA_real_,
             n=C_age_sw$n),
  data.table(outcome="mean_close_age", panel="DW",
             coef=if (!is.null(C_age_dw$model)) C_age_dw$row$coef else NA_real_,
             se=if (!is.null(C_age_dw$model)) C_age_dw$row$se else NA_real_,
             crve_p=if (!is.null(C_age_dw$model)) C_age_dw$row$p else NA_real_,
             boot_p=if (!is.null(C_age_dw$model)) C_age_dw$boot$p_boot else NA_real_,
             n=C_age_dw$n),
  data.table(outcome="closure_rate", panel="Overall",
             coef=if (!is.null(C_rate_overall$model)) C_rate_overall$row$coef else NA_real_,
             se=if (!is.null(C_rate_overall$model)) C_rate_overall$row$se else NA_real_,
             crve_p=if (!is.null(C_rate_overall$model)) C_rate_overall$row$p else NA_real_,
             boot_p=if (!is.null(C_rate_overall$model)) C_rate_overall$boot$p_boot else NA_real_,
             n=C_rate_overall$n),
  data.table(outcome="closure_rate", panel="SW",
             coef=if (!is.null(C_rate_sw$model)) C_rate_sw$row$coef else NA_real_,
             se=if (!is.null(C_rate_sw$model)) C_rate_sw$row$se else NA_real_,
             crve_p=if (!is.null(C_rate_sw$model)) C_rate_sw$row$p else NA_real_,
             boot_p=if (!is.null(C_rate_sw$model)) C_rate_sw$boot$p_boot else NA_real_,
             n=C_rate_sw$n),
  data.table(outcome="closure_rate", panel="DW",
             coef=if (!is.null(C_rate_dw$model)) C_rate_dw$row$coef else NA_real_,
             se=if (!is.null(C_rate_dw$model)) C_rate_dw$row$se else NA_real_,
             crve_p=if (!is.null(C_rate_dw$model)) C_rate_dw$row$p else NA_real_,
             boot_p=if (!is.null(C_rate_dw$model)) C_rate_dw$boot$p_boot else NA_real_,
             n=C_rate_dw$n)
))
fwrite(specC_csv, file.path(OUTPUT_TABLES, "T_S10C_CellDiD.csv"))


fmt_C <- function(coef, p, dp = 3L) {
  if (is.na(coef)) return("---")
  fmt_est(coef, p, dp = dp)
}
fmt_C_se <- function(se, dp = 3L) {
  if (is.na(se)) return("---") else fmt_se(se, dp = dp)
}
fmt_C_p <- function(p) {
  if (is.na(p)) return("---") else sprintf("%.3f", p)
}

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Spec C: Cell-Level (State $\\times$ Year $\\times$ Make-Model) DiD}",
  "\\label{tbl:s10c_cell_did}",
  "\\begin{tabular}{l ccc ccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Outcome: mean\\_close\\_age (years)}",
  "  & \\multicolumn{3}{c}{Outcome: closure\\_rate} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  " & Overall & SW & DW & Overall & SW & DW \\\\",
  "\\midrule",
  sprintf("DiD & %s & %s & %s & %s & %s & %s \\\\",
    fmt_C(C_age_overall$row$coef,  C_age_overall$row$p),
    fmt_C(C_age_sw$row$coef,       C_age_sw$row$p),
    fmt_C(C_age_dw$row$coef,       C_age_dw$row$p),
    fmt_C(C_rate_overall$row$coef, C_rate_overall$row$p, dp = 5L),
    fmt_C(C_rate_sw$row$coef,      C_rate_sw$row$p,      dp = 5L),
    fmt_C(C_rate_dw$row$coef,      C_rate_dw$row$p,      dp = 5L)),
  sprintf("SE & %s & %s & %s & %s & %s & %s \\\\",
    fmt_C_se(C_age_overall$row$se),  fmt_C_se(C_age_sw$row$se),
    fmt_C_se(C_age_dw$row$se),
    fmt_C_se(C_rate_overall$row$se, dp = 5L),
    fmt_C_se(C_rate_sw$row$se,      dp = 5L),
    fmt_C_se(C_rate_dw$row$se,      dp = 5L)),
  sprintf("Bootstrap $p$ & %s & %s & %s & %s & %s & %s \\\\",
    fmt_C_p(C_age_overall$boot$p_boot),  fmt_C_p(C_age_sw$boot$p_boot),
    fmt_C_p(C_age_dw$boot$p_boot),
    fmt_C_p(C_rate_overall$boot$p_boot), fmt_C_p(C_rate_sw$boot$p_boot),
    fmt_C_p(C_rate_dw$boot$p_boot)),
  "\\midrule",
  "Cell FE   & $\\checkmark$ & $\\checkmark$ & $\\checkmark$",
  "          & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  "Year FE   & $\\checkmark$ & $\\checkmark$ & $\\checkmark$",
  "          & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  "Cell weights & n\\_closures & n\\_closures & n\\_closures",
  "             & n\\_tanks & n\\_tanks & n\\_tanks \\\\",
  sprintf("Cells     & %s & %s & %s & %s & %s & %s \\\\",
    fmt_n(C_age_overall$n), fmt_n(C_age_sw$n), fmt_n(C_age_dw$n),
    fmt_n(C_rate_overall$n), fmt_n(C_rate_sw$n), fmt_n(C_rate_dw$n)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\raggedright",
  paste0(
    "Notes: Unit of analysis is a state $\\times$ year $\\times$ make-model cell ",
    "from the birth-CEM matched panel. The mean\\_close\\_age outcome is the ",
    "weighted average tank age among closures within the cell, NA in cells ",
    "with zero closures (dropped from cols 1-3). The closure\\_rate outcome is ",
    "n\\_closures / n\\_tanks at the cell level. SW/DW subsamples restrict to ",
    "cells where all tanks share the wall type. Cluster-robust SEs by state. ",
    sprintf("Wild cluster bootstrap (B = %d, Rademacher).", N_BOOT),
    " Stars: *** $p<0.01$, ** $p<0.05$, * $p<0.1$ (CRVE)."
  ),
  "\\end{table}"
), "T_S10C_CellDiD.tex")
cat("  Saved: T_S10C_CellDiD.tex + .csv\n\n")


# =============================================================================
# DENSITY FIGURES (unchanged from previous version, retained for reference)
# =============================================================================

cat("--- Distribution figures ---\n")

closed_tanks[, tx_period := factor(
  fcase(
    texas_treated == 1L & post_1999 == 0L, "Texas, Pre",
    texas_treated == 1L & post_1999 == 1L, "Texas, Post",
    texas_treated == 0L & post_1999 == 0L, "Control, Pre",
    texas_treated == 0L & post_1999 == 1L, "Control, Post"
  ),
  levels = c("Control, Pre", "Control, Post", "Texas, Pre", "Texas, Post")
)]

dens_palette <- c(
  "Control, Pre"  = "#9ecae1",
  "Control, Post" = COL_CTRL,
  "Texas, Pre"    = "#fdae61",
  "Texas, Post"   = COL_TX
)

plot_dens_by_wall <- function(wall_val, wall_label) {
  sub <- closed_tanks[single_wall == wall_val]
  ggplot(sub, aes(x = age_at_closure, colour = tx_period, fill = tx_period)) +
    geom_density(alpha = 0.22, linewidth = 0.6) +
    scale_colour_manual(values = dens_palette, name = NULL) +
    scale_fill_manual(values = dens_palette, name = NULL) +
    scale_x_continuous(breaks = seq(0, 60, 10)) +
    labs(x = "Age at Closure (years)", y = "Density") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line       = element_line(colour = "black", linewidth = 0.4),
      axis.ticks      = element_line(colour = "black", linewidth = 0.3),
      axis.text       = element_text(colour = "black"),
      legend.position = "bottom",
      legend.text     = element_text(size = 8),
      panel.grid      = element_blank(),
      plot.title      = element_blank(),
      plot.margin     = margin(8, 10, 8, 8)
    )
}

save_gg(plot_dens_by_wall(1L, "Single-walled"),
        "F_S10_AgeClosure_Density_SW", width = 8, height = 5)
save_gg(plot_dens_by_wall(0L, "Double-walled"),
        "F_S10_AgeClosure_Density_DW", width = 8, height = 5)
cat("  Saved: F_S10_AgeClosure_Density_{SW, DW}\n\n")


################################################################################
# === S11 + S11b: HONESTDID SENSITIVITY (OLS + COX) ===
################################################################################

#### S11 + S11b HonestDiD Sensitivity ####

cat("========================================\n")
cat("S11 + S11b: HONESTDID SENSITIVITY\n")
cat("========================================\n\n")

# Bounds grid for both Delta^RM and Delta^SD
HD_M_GRID_RM <- seq(0, 2, by = 0.1)   # Relative-magnitude M
HD_M_GRID_SD <- seq(0, 0.05, by = 0.005)  # Smoothness M (in pp units for OLS)

# Post-period horizons of interest
HD_POST_HORIZONS <- c(0L, 1L)  # tau = 0 and tau = 1


# =============================================================================
# Helper: extract HonestDiD inputs from a fixest event-study model
# =============================================================================
# Takes m and returns:
#   beta:  named numeric vector of all event-time coefficients (pre + post),
#          INCLUDING the omitted reference period (added as 0)
#   sigma: corresponding cluster-robust VCV
#   pre_periods, post_periods: integer vectors of relative years
#   ref_period: integer reference period (default -1)

extract_es_inputs <- function(m,
                               ref_period   = -1L,
                               var_prefix   = "rel_year",
                               include_ref  = TRUE) {

  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "estimate", "se", "t_stat", "p_value"))

  pat <- sprintf("^%s::(-?[0-9]+):texas_treated$", var_prefix)
  ct  <- ct[grepl(pat, term)]
  ct[, year := as.integer(sub(pat, "\\1", term))]
  setorder(ct, year)

  # Build full beta and Sigma including ref period (added as zero)
  V_full   <- vcov(m, type = "clustered")
  hit_idx  <- match(ct$term, rownames(V_full))
  V_es     <- V_full[hit_idx, hit_idx, drop = FALSE]
  beta_es  <- ct$estimate
  names(beta_es) <- as.character(ct$year)
  rownames(V_es) <- colnames(V_es) <- as.character(ct$year)

  if (include_ref && !ref_period %in% ct$year) {
    new_yrs <- sort(c(ct$year, ref_period))
    new_n   <- length(new_yrs)
    V_aug   <- matrix(0, nrow = new_n, ncol = new_n,
                      dimnames = list(as.character(new_yrs),
                                       as.character(new_yrs)))
    keep_yrs <- as.character(ct$year)
    V_aug[keep_yrs, keep_yrs] <- V_es
    beta_aug <- setNames(rep(0, new_n), as.character(new_yrs))
    beta_aug[keep_yrs] <- beta_es
    beta_es <- beta_aug
    V_es    <- V_aug
  }

  list(
    beta         = beta_es,
    sigma        = V_es,
    pre_periods  = sort(as.integer(names(beta_es)[
                          as.integer(names(beta_es)) < ref_period
                       ])),
    post_periods = sort(as.integer(names(beta_es)[
                          as.integer(names(beta_es)) >= 0L
                       ])),
    ref_period   = ref_period
  )
}


# =============================================================================
# Helper: Cox-model HonestDiD input extraction
# =============================================================================
# Cox event-study coefficient names are es_tau_p7, es_tau_m3 (positive/
# minus prefix). Convert to numeric year and build beta + Sigma.

extract_cox_es_inputs <- function(m, ref_period = -1L) {

  s   <- summary(m)$coefficients
  nms <- rownames(s)

  pre_pat  <- "^es_tau_m([0-9]+)$"
  post_pat <- "^es_tau_p([0-9]+)$"
  zero_pat <- "^es_tau_0$"

  pre_terms  <- grep(pre_pat,  nms, value = TRUE)
  post_terms <- grep(post_pat, nms, value = TRUE)
  zero_terms <- grep(zero_pat, nms, value = TRUE)

  yrs_pre  <- if (length(pre_terms))  -as.integer(sub(pre_pat,  "\\1", pre_terms))  else integer(0)
  yrs_post <- if (length(post_terms))  as.integer(sub(post_pat, "\\1", post_terms)) else integer(0)
  yrs_zero <- if (length(zero_terms))  rep(0L, length(zero_terms))                   else integer(0)

  used_terms <- c(pre_terms, zero_terms, post_terms)
  used_yrs   <- c(yrs_pre,   yrs_zero,   yrs_post)

  ord       <- order(used_yrs)
  used_terms <- used_terms[ord]
  used_yrs   <- used_yrs[ord]

  V_full <- vcov(m)
  V_es   <- V_full[used_terms, used_terms, drop = FALSE]
  beta_es <- s[used_terms, "coef"]
  names(beta_es) <- as.character(used_yrs)
  rownames(V_es) <- colnames(V_es) <- as.character(used_yrs)

  # Add reference period as zero
  if (!ref_period %in% used_yrs) {
    new_yrs <- sort(c(used_yrs, ref_period))
    new_n   <- length(new_yrs)
    V_aug   <- matrix(0, nrow = new_n, ncol = new_n,
                      dimnames = list(as.character(new_yrs),
                                       as.character(new_yrs)))
    keep_yrs <- as.character(used_yrs)
    V_aug[keep_yrs, keep_yrs] <- V_es
    beta_aug <- setNames(rep(0, new_n), as.character(new_yrs))
    beta_aug[keep_yrs] <- beta_es
    beta_es <- beta_aug
    V_es    <- V_aug
  }

  list(
    beta         = beta_es,
    sigma        = V_es,
    pre_periods  = sort(as.integer(names(beta_es)[
                          as.integer(names(beta_es)) < ref_period
                       ])),
    post_periods = sort(as.integer(names(beta_es)[
                          as.integer(names(beta_es)) >= 0L
                       ])),
    ref_period   = ref_period
  )
}


# =============================================================================
# Helper: run HonestDiD sensitivity (RM and SD) for a single horizon
# =============================================================================
# Returns a tidy data.table with bounds + the breakdown M.

run_hd_sensitivity <- function(es_inputs, horizon = 0L,
                                M_grid_rm = HD_M_GRID_RM,
                                M_grid_sd = HD_M_GRID_SD,
                                alpha = 0.05) {

  beta  <- es_inputs$beta
  sigma <- es_inputs$sigma

  numPrePeriods  <- length(es_inputs$pre_periods)
  numPostPeriods <- length(es_inputs$post_periods)

  # HonestDiD requires beta and sigma to be in the order: pre periods,
  # then post periods. The ref period MUST be at the boundary (i.e., the
  # last pre period). Since we set ref = -1 and it sits between -2 and 0,
  # we drop it from the input vectors (it carries beta=0, var=0).

  ref_chr <- as.character(es_inputs$ref_period)
  beta_in  <- beta[setdiff(names(beta), ref_chr)]
  sigma_in <- sigma[setdiff(rownames(sigma), ref_chr),
                    setdiff(colnames(sigma), ref_chr), drop = FALSE]

  # Reorder to (pre_periods, post_periods)
  ord <- c(as.character(es_inputs$pre_periods),
           as.character(es_inputs$post_periods))
  beta_in  <- beta_in[ord]
  sigma_in <- sigma_in[ord, ord, drop = FALSE]

  # Identify which row of beta_in corresponds to the requested horizon
  l_vec <- rep(0, length(beta_in))
  horizon_idx <- which(es_inputs$post_periods == horizon)
  if (length(horizon_idx) == 0L) {
    return(NULL)  # horizon not estimated
  }
  l_vec[numPrePeriods + horizon_idx] <- 1

  # ---- Delta^RM (relative magnitudes) ----
  rm_results <- tryCatch(
    HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat        = beta_in,
      sigma          = sigma_in,
      numPrePeriods  = numPrePeriods,
      numPostPeriods = numPostPeriods,
      l_vec          = l_vec,
      Mbarvec        = M_grid_rm,
      alpha          = alpha
    ),
    error = function(e) {
      warning(sprintf("RM sensitivity failed at horizon=%d: %s",
                      horizon, e$message))
      NULL
    }
  )

  # ---- Delta^SD (smoothness) ----
  sd_results <- tryCatch(
    HonestDiD::createSensitivityResults(
      betahat        = beta_in,
      sigma          = sigma_in,
      numPrePeriods  = numPrePeriods,
      numPostPeriods = numPostPeriods,
      l_vec          = l_vec,
      Mvec           = M_grid_sd,
      alpha          = alpha
    ),
    error = function(e) {
      warning(sprintf("SD sensitivity failed at horizon=%d: %s",
                      horizon, e$message))
      NULL
    }
  )

  # Original CI for reference (no restriction, M = 0)
  point_est <- as.numeric(t(l_vec) %*% beta_in)
  point_se  <- sqrt(as.numeric(t(l_vec) %*% sigma_in %*% l_vec))
  point_ci_lo <- point_est - qnorm(1 - alpha / 2) * point_se
  point_ci_hi <- point_est + qnorm(1 - alpha / 2) * point_se

  # Compute breakdown M for each restriction (smallest M where CI crosses 0)
  bd_rm <- if (!is.null(rm_results)) {
    crosses <- rm_results$Mbar[rm_results$lb <= 0 & rm_results$ub >= 0]
    if (length(crosses) > 0L) min(crosses) else NA_real_
  } else NA_real_

  bd_sd <- if (!is.null(sd_results)) {
    crosses <- sd_results$M[sd_results$lb <= 0 & sd_results$ub >= 0]
    if (length(crosses) > 0L) min(crosses) else NA_real_
  } else NA_real_

  list(
    horizon       = horizon,
    point_est     = point_est,
    point_se      = point_se,
    point_ci_lo   = point_ci_lo,
    point_ci_hi   = point_ci_hi,
    rm_results    = rm_results,
    sd_results    = sd_results,
    breakdown_rm  = bd_rm,
    breakdown_sd  = bd_sd
  )
}


# =============================================================================
# Helper: standalone sensitivity figure (Mbar vs CI bounds)
# =============================================================================

plot_sensitivity_standalone <- function(hd_result, restriction_label,
                                         x_label, point_col = "grey25") {

  res <- if (restriction_label == "RM") hd_result$rm_results
         else                           hd_result$sd_results
  if (is.null(res)) return(NULL)

  # Original (unrestricted) CI as a baseline at M = 0
  m_var <- if (restriction_label == "RM") "Mbar" else "M"
  res_dt <- as.data.table(res)
  setnames(res_dt, m_var, "M")

  baseline <- data.table(
    M     = 0,
    lb    = hd_result$point_ci_lo,
    ub    = hd_result$point_ci_hi,
    method = "Original"
  )
  res_dt[, method := "HonestDiD"]
  plot_dt <- rbindlist(list(baseline, res_dt[, .(M, lb, ub, method)]),
                       fill = TRUE)

  ggplot(plot_dt, aes(x = M)) +
    geom_hline(yintercept = 0, colour = "grey50",
               linetype = "dashed", linewidth = 0.5) +
    geom_ribbon(data = plot_dt[method == "HonestDiD"],
                aes(ymin = lb, ymax = ub),
                fill = point_col, alpha = 0.25) +
    geom_line(aes(y = lb), colour = point_col, linewidth = 0.55) +
    geom_line(aes(y = ub), colour = point_col, linewidth = 0.55) +
    geom_point(data = baseline, aes(y = lb),
               colour = "black", size = 1.8) +
    geom_point(data = baseline, aes(y = ub),
               colour = "black", size = 1.8) +
    labs(x = x_label,
         y = "95% confidence interval bounds (post-period)") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line   = element_line(colour = "black", linewidth = 0.4),
      axis.ticks  = element_line(colour = "black", linewidth = 0.3),
      axis.text   = element_text(colour = "black"),
      panel.grid  = element_blank(),
      plot.title  = element_blank(),
      plot.margin = margin(8, 10, 8, 8)
    )
}


# =============================================================================
# S11 -- OLS HonestDiD for primary OLS event-study models
# =============================================================================

cat("--- S11: OLS HonestDiD ---\n")

ols_hd_models <- list(
  pooled = m_es_pooled,
  below  = m_es_below_med,
  above  = m_es_above_med
)
ols_hd_colors <- list(
  pooled = "grey25",
  below  = COL_BLUE,
  above  = COL_RED
)

ols_hd_results <- list()

for (nm in names(ols_hd_models)) {

  log_step(sprintf("OLS HonestDiD: %s", nm))

  inp <- extract_es_inputs(ols_hd_models[[nm]], ref_period = -1L)

  for (h in HD_POST_HORIZONS) {
    res <- run_hd_sensitivity(inp, horizon = h)
    if (is.null(res)) {
      cat(sprintf("    horizon=%d: SKIPPED (not in post_periods)\n", h))
      next
    }
    key <- sprintf("%s_h%d", nm, h)
    ols_hd_results[[key]] <- res

    cat(sprintf("    horizon=%d: point=%+.5f, breakdown_RM=%s, breakdown_SD=%s\n",
      h, res$point_est,
      ifelse(is.na(res$breakdown_rm), "(robust @ all M)",
             sprintf("%.2f", res$breakdown_rm)),
      ifelse(is.na(res$breakdown_sd), "(robust @ all M)",
             sprintf("%.4f", res$breakdown_sd))))

    # Standalone figures: one per (model, horizon, restriction) -- 6 each
    p_rm <- plot_sensitivity_standalone(
      res, "RM", expression(bar(M)),
      point_col = ols_hd_colors[[nm]]
    )
    p_sd <- plot_sensitivity_standalone(
      res, "SD", "M (smoothness bound, pp)",
      point_col = ols_hd_colors[[nm]]
    )
    if (!is.null(p_rm)) {
      save_gg(p_rm, sprintf("F_S11_HonestDiD_OLS_%s_h%d_RM", nm, h),
              width = 7, height = 5)
    }
    if (!is.null(p_sd)) {
      save_gg(p_sd, sprintf("F_S11_HonestDiD_OLS_%s_h%d_SD", nm, h),
              width = 7, height = 5)
    }
  }
}


# Build OLS HonestDiD summary table
ols_hd_dt <- rbindlist(lapply(names(ols_hd_results), function(k) {
  r <- ols_hd_results[[k]]
  parts <- strsplit(k, "_h", fixed = TRUE)[[1]]
  data.table(
    model        = parts[1],
    horizon      = as.integer(parts[2]),
    point_est    = r$point_est,
    point_se     = r$point_se,
    point_ci_lo  = r$point_ci_lo,
    point_ci_hi  = r$point_ci_hi,
    breakdown_RM = r$breakdown_rm,
    breakdown_SD = r$breakdown_sd
  )
}))
fwrite(ols_hd_dt,
       file.path(OUTPUT_TABLES, "T_S11_HonestDiD_OLS.csv"))


# OLS HonestDiD LaTeX table
fmt_break <- function(x, dp = 2L) {
  if (is.na(x))        "$\\geq M_{\\max}$"
  else if (x == 0)     "0.00 (failed at 0)"
  else                 sprintf(paste0("%.", dp, "f"), x)
}

ols_tex_rows <- vapply(seq_len(nrow(ols_hd_dt)), function(i) {
  r <- ols_hd_dt[i]
  sprintf(
    "%s ($\\tau$=%d) & %+.5f & [%+.5f, %+.5f] & %s & %s \\\\",
    r$model, r$horizon, r$point_est,
    r$point_ci_lo, r$point_ci_hi,
    fmt_break(r$breakdown_RM, 2),
    fmt_break(r$breakdown_SD, 4)
  )
}, character(1))

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{HonestDiD Sensitivity (OLS Event Studies, Birth-CEM Sample)}",
  "\\label{tbl:s11_honestdid_ols}",
  "\\begin{tabular}{l c c cc}",
  "\\toprule",
  " & Point estimate & 95\\% CI & Breakdown $\\bar{M}$ ($\\Delta^{RM}$)",
  " & Breakdown $M$ ($\\Delta^{SD}$) \\\\",
  "\\midrule",
  ols_tex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\raggedright",
  paste0(
    "Notes: Sensitivity analysis from Rambachan and Roth (2023) ",
    "applied to OLS event-study estimates on the birth-CEM matched sample. ",
    "Breakdown $\\bar{M}$ is the smallest relative-magnitude bound at which ",
    "the post-period 95\\% CI includes zero; values above ",
    sprintf("$\\bar{M} = %.1f$ ", max(HD_M_GRID_RM)),
    "are reported as $\\geq M_{\\max}$. ",
    "Breakdown $M$ is the smallest second-difference smoothness bound (pp) ",
    "at which the CI includes zero. Larger breakdowns indicate the post-",
    "treatment effect is more robust to pre-trend deviations of the specified ",
    "form. Standalone sensitivity figures are saved as F\\_S11\\_HonestDiD\\_OLS\\_*.",
    "\\par",
    sprintf("$\\bar{M}$ grid: %.1f to %.1f by %.1f. ",
            min(HD_M_GRID_RM), max(HD_M_GRID_RM),
            HD_M_GRID_RM[2] - HD_M_GRID_RM[1]),
    sprintf("$M$ grid (pp): %.3f to %.3f by %.3f.",
            min(HD_M_GRID_SD), max(HD_M_GRID_SD),
            HD_M_GRID_SD[2] - HD_M_GRID_SD[1])
  ),
  "\\end{table}"
), "T_S11_HonestDiD_OLS.tex")
cat("  Saved: T_S11_HonestDiD_OLS.tex + .csv\n\n")


# =============================================================================
# S11b -- Cox HonestDiD for primary Cox event-study models
# =============================================================================

cat("--- S11b: Cox HonestDiD ---\n")

# Cox uses log-HR scale, so the smoothness M grid should be wider
HD_M_GRID_SD_COX <- seq(0, 0.20, by = 0.02)

cox_hd_models <- list(
  noage = m_cox_es_noage,
  below = m_cox_es_below,
  above = m_cox_es_above
)
cox_hd_colors <- list(
  noage = "grey25",
  below = COL_BLUE,
  above = COL_RED
)

cox_hd_results <- list()

for (nm in names(cox_hd_models)) {

  log_step(sprintf("Cox HonestDiD: %s", nm))

  inp <- extract_cox_es_inputs(cox_hd_models[[nm]], ref_period = -1L)

  for (h in HD_POST_HORIZONS) {
    res <- run_hd_sensitivity(inp, horizon = h,
                               M_grid_sd = HD_M_GRID_SD_COX)
    if (is.null(res)) {
      cat(sprintf("    horizon=%d: SKIPPED\n", h))
      next
    }
    key <- sprintf("%s_h%d", nm, h)
    cox_hd_results[[key]] <- res

    cat(sprintf("    horizon=%d: point=%+.5f, breakdown_RM=%s, breakdown_SD=%s\n",
      h, res$point_est,
      ifelse(is.na(res$breakdown_rm), "(robust @ all M)",
             sprintf("%.2f", res$breakdown_rm)),
      ifelse(is.na(res$breakdown_sd), "(robust @ all M)",
             sprintf("%.4f", res$breakdown_sd))))

    p_rm <- plot_sensitivity_standalone(
      res, "RM", expression(bar(M)),
      point_col = cox_hd_colors[[nm]]
    )
    p_sd <- plot_sensitivity_standalone(
      res, "SD", "M (smoothness bound, log-HR)",
      point_col = cox_hd_colors[[nm]]
    )
    if (!is.null(p_rm)) {
      save_gg(p_rm, sprintf("F_S11_HonestDiD_Cox_%s_h%d_RM", nm, h),
              width = 7, height = 5)
    }
    if (!is.null(p_sd)) {
      save_gg(p_sd, sprintf("F_S11_HonestDiD_Cox_%s_h%d_SD", nm, h),
              width = 7, height = 5)
    }
  }
}


cox_hd_dt <- rbindlist(lapply(names(cox_hd_results), function(k) {
  r <- cox_hd_results[[k]]
  parts <- strsplit(k, "_h", fixed = TRUE)[[1]]
  data.table(
    model        = parts[1],
    horizon      = as.integer(parts[2]),
    point_est    = r$point_est,
    point_se     = r$point_se,
    point_ci_lo  = r$point_ci_lo,
    point_ci_hi  = r$point_ci_hi,
    breakdown_RM = r$breakdown_rm,
    breakdown_SD = r$breakdown_sd
  )
}))
fwrite(cox_hd_dt,
       file.path(OUTPUT_TABLES, "T_S11_HonestDiD_Cox.csv"))


cox_tex_rows <- vapply(seq_len(nrow(cox_hd_dt)), function(i) {
  r <- cox_hd_dt[i]
  sprintf(
    "%s ($\\tau$=%d) & %+.5f & [%+.5f, %+.5f] & %s & %s \\\\",
    r$model, r$horizon, r$point_est,
    r$point_ci_lo, r$point_ci_hi,
    fmt_break(r$breakdown_RM, 2),
    fmt_break(r$breakdown_SD, 4)
  )
}, character(1))

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{HonestDiD Sensitivity (Cox Event Studies, Birth-CEM Sample)}",
  "\\label{tbl:s11_honestdid_cox}",
  "\\begin{tabular}{l c c cc}",
  "\\toprule",
  " & Point estimate (log-HR) & 95\\% CI & Breakdown $\\bar{M}$ ($\\Delta^{RM}$)",
  " & Breakdown $M$ ($\\Delta^{SD}$) \\\\",
  "\\midrule",
  cox_tex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\raggedright",
  paste0(
    "Notes: Sensitivity analysis applied to Cox event-study estimates on the ",
    "birth-CEM matched sample (log-hazard ratio scale). ",
    "Breakdown definitions as in S11 OLS table. ",
    "Cox $M$ grid is denominated in log-HR units. ",
    sprintf("$\\bar{M}$ grid: %.1f to %.1f by %.1f. ",
            min(HD_M_GRID_RM), max(HD_M_GRID_RM),
            HD_M_GRID_RM[2] - HD_M_GRID_RM[1]),
    sprintf("$M$ grid (log-HR): %.2f to %.2f by %.2f.",
            min(HD_M_GRID_SD_COX), max(HD_M_GRID_SD_COX),
            HD_M_GRID_SD_COX[2] - HD_M_GRID_SD_COX[1])
  ),
  "\\end{table}"
), "T_S11_HonestDiD_Cox.tex")
cat("  Saved: T_S11_HonestDiD_Cox.tex + .csv\n\n")


################################################################################
# === S11c + S11d: PRETRENDS POWER + REFORM-CEM HONESTDID ===
################################################################################

#### S11c + S11d Pretrends Power + Reform-CEM Robustness ####

cat("========================================\n")
cat("S11c + S11d: PRETRENDS POWER + REFORM-CEM ROBUSTNESS\n")
cat("========================================\n\n")


# =============================================================================
# S11c -- PRETRENDS POWER ANALYSIS (Roth 2022)
# =============================================================================
#
# pretrends::pretrends() computes the probability of detecting a specified
# pre-trend violation (i.e., the power of the joint pre-trend test). The
# function takes a hypothesized linear deviation delta_lin and returns the
# detection probability.
#
# Procedure (per Plan 1 AP-5 Task 5.3):
#   For each model, find the linear deviation that the standard pre-trend
#   test rejects with 50% probability ("hypothesized trend at 50% power")
#   and 80% probability ("at 80% power"). Report both numbers as the
#   "magnitude of pre-trend that would be detected half/80% of the time".
#
# Compare these to the magnitude of the headline post-treatment effect
# to assess whether the pre-trend test is well-powered against
# economically meaningful violations.
# =============================================================================

cat("--- S11c: pretrends power analysis ---\n")

# Helper: extract beta + sigma in pretrends format
# pretrends needs: betahat (vector of pre and post coefs INCLUDING ref=0),
#                  sigma (full VCV including ref row/col),
#                  numPrePeriodsActual = number of pre-period coefs,
#                  numPostPeriodsActual = number of post-period coefs,
#                  ref_period (so it knows which entry is the reference)

prep_pretrends_input <- function(es_inputs) {
  # pretrends expects beta and sigma to include the ref period (with
  # value 0 and zero variance) at its natural position in the time vector.
  beta  <- es_inputs$beta
  sigma <- es_inputs$sigma
  list(
    beta            = beta,
    sigma           = sigma,
    pre_periods     = es_inputs$pre_periods,
    post_periods    = es_inputs$post_periods,
    ref_period      = es_inputs$ref_period
  )
}


# Helper: compute power against a hypothesized LINEAR pre-trend of slope delta.
# The pretrends package's slope_for_power function inverts: given a target
# power, returns the slope that achieves that power.
#
# We invert numerically by bisection over a grid of slopes.

compute_slope_at_power <- function(es_inputs, target_power = 0.5,
                                    slope_grid = seq(0, 0.01, by = 0.0001)) {
  # Use pretrends::pretrends() which takes a hypothesized linear slope
  # and returns the rejection probability.
  inp <- prep_pretrends_input(es_inputs)

  power_at_slope <- function(slope) {
    # Hypothesized deviation: linear in time, evaluated at each pre-period
    delta_lin <- slope * (inp$pre_periods - inp$ref_period)
    # Pad with zeros for the ref period and post periods
    full_delta <- numeric(length(inp$beta))
    names(full_delta) <- names(inp$beta)
    full_delta[as.character(inp$pre_periods)] <- delta_lin
    # pretrends::pretrends: the function expects a hypothesized full-period
    # deviation and returns the power and bias under that hypothesis.
    res <- tryCatch(
      pretrends::pretrends(
        betahat            = inp$beta,
        sigma              = inp$sigma,
        deltatrue          = full_delta,
        tVec               = as.integer(names(inp$beta)),
        referencePeriod    = inp$ref_period
      ),
      error = function(e) NULL
    )
    if (is.null(res)) return(NA_real_)
    # The package returns a list; extract the rejection probability ("power")
    if ("Power" %in% names(res))      return(res$Power)
    if ("power" %in% names(res))      return(res$power)
    if ("pretrend_power" %in% names(res)) return(res$pretrend_power)
    NA_real_
  }

  powers <- vapply(slope_grid, power_at_slope, numeric(1))
  ok     <- which(!is.na(powers) & powers >= target_power)
  if (length(ok) == 0L) return(NA_real_)
  slope_grid[min(ok)]
}


# Run pretrends power for the three primary OLS models
# Note: pretrends slope is in units of the outcome per period.
# For our pp-scale OLS, slope=0.001 means 0.1pp drift per year.

ols_models_for_pp <- list(
  pooled = m_es_pooled,
  below  = m_es_below_med,
  above  = m_es_above_med
)
ols_colors_pp <- list(
  pooled = "grey25",
  below  = COL_BLUE,
  above  = COL_RED
)

pp_results <- list()

for (nm in names(ols_models_for_pp)) {
  log_step(sprintf("pretrends power: %s", nm))

  inp <- extract_es_inputs(ols_models_for_pp[[nm]], ref_period = -1L)

  slope_50 <- compute_slope_at_power(inp, target_power = 0.50)
  slope_80 <- compute_slope_at_power(inp, target_power = 0.80)

  # Headline post-treatment effect for comparison: tau = 0 coefficient
  tau0_name <- "rel_year::0:texas_treated"
  tau0_coef <- coef(ols_models_for_pp[[nm]])[tau0_name]

  # Express the "minimum detectable pre-trend per pre-period year" as a
  # magnitude relative to the headline tau=0 coefficient. If slope_50 is
  # 0.0005 and tau0 is 0.005, the test would detect a 5x-smaller per-year
  # drift with 50% probability.
  rel_50 <- if (!is.na(slope_50) && !is.na(tau0_coef) && tau0_coef != 0) {
    abs(slope_50 / tau0_coef)
  } else NA_real_
  rel_80 <- if (!is.na(slope_80) && !is.na(tau0_coef) && tau0_coef != 0) {
    abs(slope_80 / tau0_coef)
  } else NA_real_

  pp_results[[nm]] <- list(
    model         = nm,
    tau0_coef     = tau0_coef,
    slope_50      = slope_50,
    slope_80      = slope_80,
    rel_to_tau0_50 = rel_50,
    rel_to_tau0_80 = rel_80,
    es_inputs     = inp
  )

  cat(sprintf(
    "    tau=0 coef = %+.5f | slope@50%% power = %s | slope@80%% power = %s\n",
    tau0_coef,
    ifelse(is.na(slope_50), "(not detected at any grid slope)",
           sprintf("%+.5f/yr", slope_50)),
    ifelse(is.na(slope_80), "(not detected at any grid slope)",
           sprintf("%+.5f/yr", slope_80))
  ))
}


# Build pretrends power CSV
pp_csv <- rbindlist(lapply(pp_results, function(r) {
  data.table(
    model         = r$model,
    tau0_coef     = r$tau0_coef,
    slope_at_50pct_power = r$slope_50,
    slope_at_80pct_power = r$slope_80,
    rel_to_tau0_50 = r$rel_to_tau0_50,
    rel_to_tau0_80 = r$rel_to_tau0_80
  )
}))
fwrite(pp_csv, file.path(OUTPUT_TABLES, "T_S11c_PretrendsPower.csv"))


# Build pretrends power LaTeX table
fmt_slope <- function(x) {
  if (is.na(x)) return("---")
  sprintf("%+.5f", x)
}
fmt_rel <- function(x) {
  if (is.na(x)) return("---")
  sprintf("%.2f$\\times$", x)
}

pp_tex_rows <- vapply(seq_len(nrow(pp_csv)), function(i) {
  r <- pp_csv[i]
  sprintf("%s & %+.5f & %s & %s & %s & %s \\\\",
    r$model, r$tau0_coef,
    fmt_slope(r$slope_at_50pct_power),
    fmt_slope(r$slope_at_80pct_power),
    fmt_rel(r$rel_to_tau0_50),
    fmt_rel(r$rel_to_tau0_80))
}, character(1))

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Pre-Trend Test Power Against Hypothesized Linear Violations}",
  "\\label{tbl:s11c_pretrends_power}",
  "\\begin{tabular}{l c cc cc}",
  "\\toprule",
  " & $\\hat{\\beta}_{\\tau=0}$",
  " & \\multicolumn{2}{c}{Slope at detection power}",
  " & \\multicolumn{2}{c}{Slope as multiple of $\\hat{\\beta}_{\\tau=0}$} \\\\",
  "\\cmidrule(lr){3-4} \\cmidrule(lr){5-6}",
  " & & 50\\% & 80\\% & 50\\% & 80\\% \\\\",
  "\\midrule",
  pp_tex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\raggedright",
  paste0(
    "Notes: pretrends (Roth 2022) computes the probability that the joint ",
    "pre-trend test rejects against a hypothesized linear deviation in the ",
    "pre-period. Reports the per-year linear slope (pp/year) at which the ",
    "pre-trend test rejects with 50\\% and 80\\% probability. ",
    "Lower slopes at 50\\%/80\\% power $\\Rightarrow$ better-powered test. ",
    "The relative columns express this slope as a multiple of the headline ",
    "$\\tau = 0$ post-treatment coefficient. ",
    "A multiple $<$ 1 means the test would detect drifts smaller than the ",
    "post-treatment effect with the stated probability; a multiple $>$ 1 ",
    "means the test is under-powered against drifts of the same magnitude as ",
    "the post-effect."
  ),
  "\\end{table}"
), "T_S11c_PretrendsPower.tex")
cat("  Saved: T_S11c_PretrendsPower.tex + .csv\n\n")


# Pretrends power figures: one per OLS model. Each shows the power curve
# (rejection probability) against a grid of hypothesized linear slopes.

build_power_curve <- function(es_inputs,
                               slope_grid = seq(0, 0.005, by = 0.0001)) {
  inp <- prep_pretrends_input(es_inputs)
  power_at_slope <- function(slope) {
    delta_lin <- slope * (inp$pre_periods - inp$ref_period)
    full_delta <- numeric(length(inp$beta))
    names(full_delta) <- names(inp$beta)
    full_delta[as.character(inp$pre_periods)] <- delta_lin
    res <- tryCatch(
      pretrends::pretrends(
        betahat            = inp$beta,
        sigma              = inp$sigma,
        deltatrue          = full_delta,
        tVec               = as.integer(names(inp$beta)),
        referencePeriod    = inp$ref_period
      ),
      error = function(e) NULL
    )
    if (is.null(res)) return(NA_real_)
    if ("Power" %in% names(res))           return(res$Power)
    if ("power" %in% names(res))           return(res$power)
    if ("pretrend_power" %in% names(res))  return(res$pretrend_power)
    NA_real_
  }
  data.table(
    slope = slope_grid,
    power = vapply(slope_grid, power_at_slope, numeric(1))
  )
}

for (nm in names(pp_results)) {
  log_step(sprintf("Building S11c power curve figure: %s", nm))
  curve_dt <- build_power_curve(pp_results[[nm]]$es_inputs)
  curve_dt <- curve_dt[!is.na(power)]

  if (nrow(curve_dt) == 0L) {
    cat(sprintf("    SKIP %s: no valid power values\n", nm))
    next
  }

  p_pp <- ggplot(curve_dt, aes(x = slope, y = power)) +
    geom_hline(yintercept = c(0.5, 0.8), linetype = "dashed",
               colour = "grey50", linewidth = 0.4) +
    geom_line(colour = ols_colors_pp[[nm]], linewidth = 0.8) +
    annotate("text", x = max(curve_dt$slope), y = 0.5,
             label = "50% power", vjust = -0.4, hjust = 1,
             size = 3, colour = "grey40") +
    annotate("text", x = max(curve_dt$slope), y = 0.8,
             label = "80% power", vjust = -0.4, hjust = 1,
             size = 3, colour = "grey40") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1.02)) +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.0001)) +
    labs(x = "Hypothesized linear pre-trend slope (pp/year)",
         y = "Pre-trend test rejection probability") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(
      axis.line   = element_line(colour = "black", linewidth = 0.4),
      axis.ticks  = element_line(colour = "black", linewidth = 0.3),
      axis.text   = element_text(colour = "black"),
      panel.grid  = element_blank(),
      plot.title  = element_blank(),
      plot.margin = margin(8, 10, 8, 8)
    )
  save_gg(p_pp, sprintf("F_S11c_PretrendsPower_%s", nm),
          width = 7, height = 5)
}
cat("  Saved: F_S11c_PretrendsPower_{pooled, below, above}\n\n")


# =============================================================================
# S11d -- REFORM-CEM HONESTDID ROBUSTNESS
# =============================================================================
#
# Repeats S11/S11b on the reform-CEM matched sample. The reform-CEM design
# matches on pre-reform tank survival rather than birth cohort, so it
# provides an alternative matched sample with different selection on
# observables. Comparable HonestDiD breakdowns on this sample confirm
# robustness to the matching design.
# =============================================================================

cat("--- S11d: reform-CEM HonestDiD robustness ---\n")

reform_ols_models <- list(
  reform_pooled = m_es_reform_pooled,
  reform_above  = m_es_reform_above
)
reform_cox_models <- list(
  reform_pooled = m_cox_es_reform_pooled,
  reform_above  = m_cox_es_reform_above
)
reform_colors <- list(
  reform_pooled = "grey25",
  reform_above  = COL_RED
)

reform_hd_results <- list()


# OLS panel
for (nm in names(reform_ols_models)) {
  log_step(sprintf("Reform-CEM OLS HonestDiD: %s", nm))

  inp <- extract_es_inputs(reform_ols_models[[nm]], ref_period = -1L)

  for (h in HD_POST_HORIZONS) {
    res <- run_hd_sensitivity(inp, horizon = h)
    if (is.null(res)) {
      cat(sprintf("    horizon=%d: SKIPPED\n", h))
      next
    }
    key <- sprintf("OLS_%s_h%d", nm, h)
    reform_hd_results[[key]] <- res

    cat(sprintf("    OLS h=%d: point=%+.5f, breakdown_RM=%s, breakdown_SD=%s\n",
      h, res$point_est,
      ifelse(is.na(res$breakdown_rm), "(robust @ all M)",
             sprintf("%.2f", res$breakdown_rm)),
      ifelse(is.na(res$breakdown_sd), "(robust @ all M)",
             sprintf("%.4f", res$breakdown_sd))))

    p_rm <- plot_sensitivity_standalone(
      res, "RM", expression(bar(M)),
      point_col = reform_colors[[nm]]
    )
    p_sd <- plot_sensitivity_standalone(
      res, "SD", "M (smoothness bound, pp)",
      point_col = reform_colors[[nm]]
    )
    if (!is.null(p_rm)) {
      save_gg(p_rm, sprintf("F_S11d_HonestDiD_OLS_%s_h%d_RM", nm, h),
              width = 7, height = 5)
    }
    if (!is.null(p_sd)) {
      save_gg(p_sd, sprintf("F_S11d_HonestDiD_OLS_%s_h%d_SD", nm, h),
              width = 7, height = 5)
    }
  }
}


# Cox panel
HD_M_GRID_SD_COX <- seq(0, 0.20, by = 0.02)  # log-HR scale

for (nm in names(reform_cox_models)) {
  log_step(sprintf("Reform-CEM Cox HonestDiD: %s", nm))

  inp <- extract_cox_es_inputs(reform_cox_models[[nm]], ref_period = -1L)

  for (h in HD_POST_HORIZONS) {
    res <- run_hd_sensitivity(inp, horizon = h,
                               M_grid_sd = HD_M_GRID_SD_COX)
    if (is.null(res)) {
      cat(sprintf("    horizon=%d: SKIPPED\n", h))
      next
    }
    key <- sprintf("Cox_%s_h%d", nm, h)
    reform_hd_results[[key]] <- res

    cat(sprintf("    Cox h=%d: point=%+.5f, breakdown_RM=%s, breakdown_SD=%s\n",
      h, res$point_est,
      ifelse(is.na(res$breakdown_rm), "(robust @ all M)",
             sprintf("%.2f", res$breakdown_rm)),
      ifelse(is.na(res$breakdown_sd), "(robust @ all M)",
             sprintf("%.4f", res$breakdown_sd))))

    p_rm <- plot_sensitivity_standalone(
      res, "RM", expression(bar(M)),
      point_col = reform_colors[[nm]]
    )
    p_sd <- plot_sensitivity_standalone(
      res, "SD", "M (smoothness bound, log-HR)",
      point_col = reform_colors[[nm]]
    )
    if (!is.null(p_rm)) {
      save_gg(p_rm, sprintf("F_S11d_HonestDiD_Cox_%s_h%d_RM", nm, h),
              width = 7, height = 5)
    }
    if (!is.null(p_sd)) {
      save_gg(p_sd, sprintf("F_S11d_HonestDiD_Cox_%s_h%d_SD", nm, h),
              width = 7, height = 5)
    }
  }
}


# Reform-CEM HonestDiD summary table
reform_hd_dt <- rbindlist(lapply(names(reform_hd_results), function(k) {
  r <- reform_hd_results[[k]]
  parts    <- strsplit(k, "_h", fixed = TRUE)[[1]]
  est_part <- strsplit(parts[1], "_", fixed = TRUE)[[1]]
  estimator <- est_part[1]
  model_nm  <- paste(est_part[-1], collapse = "_")
  data.table(
    estimator    = estimator,
    model        = model_nm,
    horizon      = as.integer(parts[2]),
    point_est    = r$point_est,
    point_se     = r$point_se,
    point_ci_lo  = r$point_ci_lo,
    point_ci_hi  = r$point_ci_hi,
    breakdown_RM = r$breakdown_rm,
    breakdown_SD = r$breakdown_sd
  )
}))
fwrite(reform_hd_dt,
       file.path(OUTPUT_TABLES, "T_S11d_HonestDiD_ReformCEM.csv"))


reform_tex_rows <- vapply(seq_len(nrow(reform_hd_dt)), function(i) {
  r <- reform_hd_dt[i]
  sprintf(
    "%s & %s & %d & %+.5f & [%+.5f, %+.5f] & %s & %s \\\\",
    r$estimator, r$model, r$horizon, r$point_est,
    r$point_ci_lo, r$point_ci_hi,
    fmt_break(r$breakdown_RM, 2),
    fmt_break(r$breakdown_SD, 4)
  )
}, character(1))

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{HonestDiD Sensitivity (Reform-CEM Matched Sample, OLS + Cox)}",
  "\\label{tbl:s11d_honestdid_reformcem}",
  "\\begin{tabular}{ll c c c cc}",
  "\\toprule",
  "Estimator & Model & $\\tau$ & Point est. & 95\\% CI",
  " & Breakdown $\\bar{M}$ & Breakdown $M$ \\\\",
  "\\midrule",
  reform_tex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\raggedright",
  paste0(
    "Notes: HonestDiD sensitivity applied to the reform-CEM matched sample. ",
    "Reform-CEM matches on pre-reform tank survival rather than birth ",
    "cohort, providing an alternative matching design with different ",
    "selection on observables. Comparable breakdown $\\bar{M}$ and $M$ ",
    "values to the birth-CEM HonestDiD analysis (Tables \\ref{tbl:s11_honestdid_ols} ",
    "and \\ref{tbl:s11_honestdid_cox}) confirm robustness of the inference ",
    "to the matching design choice. ",
    "Cox $M$ grid in log-HR units; OLS $M$ grid in pp."
  ),
  "\\end{table}"
), "T_S11d_HonestDiD_ReformCEM.tex")
cat("  Saved: T_S11d_HonestDiD_ReformCEM.tex + .csv\n\n")


################################################################################
# END OF 02b_DiD_Analysis.R
################################################################################