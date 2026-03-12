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
#   - Clustering  : state level (20 clusters: 1 TX + 19 controls)
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
#   throughout (20 clusters: 1 treated + 19 controls). With <30 clusters,
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
#   S9   PRIMARY Cox Model (calendar time, make_model_tank strata)
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

# Non-mandate install cohorts: tanks built after the TX upgrade mandate was
# fully enacted (1989-1993) and before the reform date.
# These cohorts have no compliance-driven closure in the pre-period.
#
# PRIMARY_YEARS   : individual year strings as written by the harmonization
#                   script (mm_install_cohort = "1989", "1990", ..., "1997")
# PRIMARY_COHORTS : 3-year bin labels per the design-memo; used only for
#                   the cohort_3yr variable and S6b 3-year coverage chart
PRIMARY_YEARS   <- as.character(1989:1997)
PRIMARY_COHORTS <- c("1989-1991", "1992-1994", "1995-1997")

# 3-year age bins (per research design document Section 6.4)
# Used as TIME-VARYING CONTROLS in reference specs — NOT for cell assignment
AGE_BIN_BREAKS  <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
AGE_BIN_LABELS  <- c("0-2",   "3-5",   "6-8",   "9-11",
                     "12-14", "15-17", "18-20", "21-23", "24+")

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
  list(
    hr   = s[idx, "exp(coef)"],
    coef = s[idx, "coef"],
    se   = s[idx, "se(coef)"],
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
#
# build_coverage_dt:
#   Collapses the primary sample to (wall x fuel x capacity x cohort x group)
#   counts and computes each bar's height as share of that group's total across
#   ALL cells -- so TX bars sum to 100% and CTL bars sum to 100% over the grid.
#   All combos are present (zeros included) so ggplot does not silently drop
#   empty x-axis positions.
#
# Arguments:
#   dt            : data.table with primary sample rows
#   cohort_col    : column name to place on the x-axis
#   cohort_levels : ordered character vector of cohort labels
#   cap_levels    : ordered capacity bin labels (sets facet ordering)

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

  # Group totals across ALL cells (TX sums to 1, CTL sums to 1 over full grid)
  group_tots <- counts[, .(group_total = sum(n)), by = group]
  counts     <- merge(counts, group_tots, by = "group")
  counts[, share := n / group_total]

  # Complete grid: every (wall x fuel x capacity x cohort x group) combo
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

# make_coverage_figure:
#   Renders the coverage grid and saves PNG + PDF.
#   Facets = mm_wall x mm_fuel x mm_capacity with FIXED scales.
#   Bars are dodged by group (TX orange / CTL blue).
#
# Arguments:
#   cov_dt    : output of build_coverage_dt
#   x_label   : x-axis title string
#   file_stem : output filename without extension

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

  p <- ggplot(cov_dt,
              aes(x = cohort, y = share, fill = group)) +
    geom_col(
      position = position_dodge(width = 0.72),
      width    = 0.65,
      alpha    = 0.88
    ) +
    facet_wrap(
      ~facet_label,
      ncol   = n_cols_facet,
      scales = "fixed"        # identical axes across ALL panels
    ) +
    scale_fill_manual(
      values = c("Texas" = COL_TX, "Control States" = COL_CTRL)
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 0.1),
      expand = expansion(mult = c(0, 0.06))
    ) +
    scale_x_discrete(drop = FALSE) +   # keep empty cohort positions
    labs(
      x       = x_label,
      y       = "Share of Group Total Sample",
      fill    = NULL,
      caption = paste0(
        "Bar height = n(cell-cohort) / group total across all cells.  ",
        "TX bars and CTL bars each independently sum to 100% over the full grid."
      )
    ) +
    theme_minimal(base_size = 9) +
    theme(
      strip.text         = element_text(face = "bold", size = 7.5,
                                        lineheight = 1.1),
      strip.background   = element_rect(fill = "grey94", color = NA),
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y        = element_text(size = 7.5),
      axis.title         = element_text(size = 9),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "top",
      legend.text        = element_text(size = 9),
      plot.caption       = element_text(size = 7, color = "grey50", hjust = 0)
    )

  png_path <- file.path(OUTPUT_FIGURES, paste0(file_stem, ".png"))
  pdf_path <- file.path(OUTPUT_FIGURES, paste0(file_stem, ".pdf"))
  ggsave(png_path, p, width = fig_w, height = fig_h,
         dpi = 200, bg = "white", limitsize = FALSE)
  ggsave(pdf_path, p, width = fig_w, height = fig_h,
         device = cairo_pdf, limitsize = FALSE)
  log_step(sprintf("Saved: %s  (%.0f x %.0f in)", basename(png_path), fig_w, fig_h), 1)
  invisible(p)
}


#### S3: Load Master Tanks and Build Annual Tank-Year Panel ####

cat("\n========================================\n")
cat("S3: LOAD MASTER TANKS AND BUILD TANK-YEAR PANEL\n")
cat("========================================\n\n")

master_tanks <- fread(
  here("Data", "Master_Harmonized_UST_Tanks.csv"),
  na.strings = c("", "NA", "N/A")
)
log_step(sprintf("Loaded: %s rows, %s columns",
                 fmt_n(nrow(master_tanks)), ncol(master_tanks)))

# ---- Validate mm_* columns (written by harmonization Step 3) ----
required_mm_cols <- c("mm_wall", "mm_fuel", "mm_capacity",
                      "mm_install_cohort", "make_model_tank")
missing_cols <- setdiff(required_mm_cols, names(master_tanks))
if (length(missing_cols) > 0) {
  stop(paste0(
    "Required columns not found: ", paste(missing_cols, collapse = ", "), "\n",
    "  Re-run 10_Master_Cleaning_and_Harmonization.r with Step 3 enabled."))
}

# ---- Coerce date columns ----
for (col in c("tank_installed_date", "tank_closed_date")) {
  if (col %in% names(master_tanks))
    master_tanks[[col]] <- as.IDate(master_tanks[[col]])
}

# ---- Treatment assignment ----
# Texas = 1; all other study states = 0.
# Tanks in non-study states (not in the 19-state control group) should have
# been removed by the harmonization script. If any remain, they receive NA
# and are excluded downstream via !is.na(did_term).
master_tanks[, texas_treated := fcase(
  state == "TX", 1L,
  state != "TX" & !is.na(state), 0L,
  default = NA_integer_
)]

log_step(sprintf("Treatment: TX=%s tanks, Control=%s tanks, NA=%s",
                 fmt_n(master_tanks[texas_treated == 1, .N]),
                 fmt_n(master_tanks[texas_treated == 0, .N]),
                 fmt_n(master_tanks[is.na(texas_treated), .N])))

# ---- Unique identifiers ----
# tank_panel_id: unique per physical tank, used as unit FE in panel regressions
# panel_id:      facility-level ID, matches convention in 02a_DiD_OLS.R
if ("tank_id" %in% names(master_tanks)) {
  master_tanks[, tank_panel_id := paste(facility_id, state, tank_id, sep = "_")]
} else {
  # Fallback: use row index; flag for verification
  master_tanks[, tank_panel_id := paste(facility_id, state, .I, sep = "_")]
  warning("No tank_id column found in master_tanks — using row index as tank_panel_id. Verify uniqueness.")
}
master_tanks[, panel_id := paste(facility_id, state, sep = "_")]

# ---- Restrict to tanks with valid install date and treatment status ----
study_tanks <- master_tanks[
  !is.na(texas_treated) & !is.na(tank_installed_date)
]
log_step(sprintf("Tanks with valid treatment + install date: %s",
                 fmt_n(nrow(study_tanks))))

# ---- Expand to annual tank-year panel ----
# A tank is active in calendar year t if:
#   year(install_date) <= t  AND  (is.na(close_date) OR year(close_date) >= t)
# closure_event = 1 in the year the tank was closed.
# Panel window: 1985-2020.

log_step("Building annual tank-year panel (1985-2020)...")

study_tanks[, `:=`(
  install_yr   = year(tank_installed_date),
  close_yr_raw = fifelse(!is.na(tank_closed_date),
                         year(tank_closed_date), 2020L)
)]
study_tanks[, `:=`(
  expand_start = pmax(install_yr, 1985L),
  expand_end   = pmin(close_yr_raw, 2020L)
)]

# Keep columns needed for the panel to minimise memory during expansion
panel_cols <- c("tank_panel_id", "panel_id", "facility_id", "state",
                "texas_treated", "mm_wall", "mm_fuel", "mm_capacity",
                "mm_install_cohort", "make_model_tank",
                "tank_installed_date", "tank_closed_date",
                "expand_start", "expand_end")

tank_year_panel <- study_tanks[
  expand_start <= expand_end,
  .SD,
  .SDcols = panel_cols
][, {
  yrs <- seq(expand_start, expand_end)
  .(panel_year    = yrs,
    closure_event = as.integer(
      !is.na(tank_closed_date) & year(tank_closed_date) == yrs))
}, by = c(setdiff(panel_cols, c("expand_start", "expand_end")))]

log_step(sprintf("Tank-year panel: %s rows, %s unique tanks, %s unique facilities",
                 fmt_n(nrow(tank_year_panel)),
                 fmt_n(uniqueN(tank_year_panel$tank_panel_id)),
                 fmt_n(uniqueN(tank_year_panel$panel_id))))

# ---- Add time variables to panel ----

# Treatment indicator
tank_year_panel[, `:=`(
  post_1999 = as.integer(panel_year >= POST_YEAR),
  did_term  = texas_treated * as.integer(panel_year >= POST_YEAR)
)]

# Relative year to reform and event-study bin (capped at ±5)
tank_year_panel[, rel_year := panel_year - POST_YEAR]
tank_year_panel[, rel_year_bin := fcase(
  rel_year <= -5L, -5L,
  rel_year >=  5L,  5L,
  default = as.integer(rel_year)
)]

# Counting-process: calendar-year intervals for panel-based specs
setorder(tank_year_panel, tank_panel_id, panel_year)
tank_year_panel[, `:=`(tstart = panel_year - 1L, tstop = panel_year)]

# Tank age at each interval endpoint from exact install date (years)
# age_stop  : age at end of calendar year t   (Dec 31 of year t   - install_date)
# age_start : age at start of calendar year t (Dec 31 of year t-1 - install_date)
# pmax(..., 0) handles tanks installed partway through their first panel year
tank_year_panel[, `:=`(
  age_stop  = as.numeric(
    as.IDate(paste0(panel_year,       "-12-31")) - tank_installed_date) / 365.25,
  age_start = pmax(
    as.numeric(
      as.IDate(paste0(panel_year - 1L, "-12-31")) - tank_installed_date) / 365.25,
    0)
)]

# Time-varying age bin: used as CONTROL in reference specs only, not for
# cell assignment (per design doc Section 6.4)
tank_year_panel[, age_bin := cut(
  age_stop,
  breaks         = AGE_BIN_BREAKS,
  labels         = AGE_BIN_LABELS,
  right          = FALSE,
  include.lowest = TRUE
)]

# mandate_active: TX upgrade mandate period (1989-1993) for pre-1989 cohorts
# Used only in S17 robustness expansion; always 0 in the primary sample
tank_year_panel[, mandate_active := as.integer(
  panel_year %in% 1989:1993 &
  !is.na(mm_install_cohort) &
  mm_install_cohort %in% c("Pre-1980", "1980-1982", "1983-1985", "1986-1988")
)]

# 3-year cohort bin: collapsed version of mm_install_cohort for S6b charts
# and for the 3-year-bin robustness coverage check.
# mm_install_cohort is a single year string ("1989", "1990", ...) as written
# by the harmonization script; cohort_3yr aggregates to 3-year design-memo bins.
tank_year_panel[, cohort_3yr := fcase(
  mm_install_cohort %in% as.character(1989:1991), "1989-1991",
  mm_install_cohort %in% as.character(1992:1994), "1992-1994",
  mm_install_cohort %in% as.character(1995:1997), "1995-1997",
  default = NA_character_
)]

year_levels <- sort(unique(tank_year_panel$panel_year))
tank_year_panel[, year_fac := factor(panel_year, levels = year_levels)]

log_step("Tank-year panel complete.\n")


#### S4: Define Primary Sample (mm_tank_primary) ####

cat("========================================\n")
cat("S4: DEFINE PRIMARY SAMPLE\n")
cat("========================================\n\n")

# Exclusion criteria (per research design Part 3.5):
#   1. make_model_tank is NA           → install date or capacity missing
#   2. mm_wall == "Unknown-Wall"       → insurer cannot price; reform mechanism unclear
#   3. mm_fuel == "Unknown-Fuel"       → fuel type unclassifiable
#   4. mm_capacity is NA               → capacity bin unassigned
#   5. mm_install_cohort outside PRIMARY_YEARS  → pre-mandate or post-reform cohorts
#   6. tstop <= tstart                 → invalid counting-process interval (zero-duration)
#   7. is.na(did_term)                 → treatment status unknown

mm_tank_primary <- tank_year_panel[
  !is.na(make_model_tank)                &
  mm_wall    != "Unknown-Wall"           &
  mm_fuel    != "Unknown-Fuel"           &
  !is.na(mm_capacity)                    &
  mm_install_cohort %in% PRIMARY_YEARS   &
  tstop > tstart                         &
  !is.na(did_term)
]

# ---- Sample size report ----
n_ty       <- nrow(mm_tank_primary)
n_tanks    <- uniqueN(mm_tank_primary$tank_panel_id)
n_tx_tanks <- mm_tank_primary[texas_treated == 1, uniqueN(tank_panel_id)]
n_ct_tanks <- mm_tank_primary[texas_treated == 0, uniqueN(tank_panel_id)]
n_tx_ty    <- mm_tank_primary[texas_treated == 1, .N]
n_ct_ty    <- mm_tank_primary[texas_treated == 0, .N]
n_cells    <- uniqueN(mm_tank_primary$make_model_tank)
n_cells_id <- mm_tank_primary[,
  .(identified = as.integer(
    any(texas_treated == 1) & any(texas_treated == 0))),
  by = .(make_model_tank, panel_year)
][, sum(identified)]

log_step(sprintf("Primary sample (mm_tank_primary):"))
log_step(sprintf("  Tank-years  : %s  (TX: %s | CTL: %s)",
                 fmt_n(n_ty), fmt_n(n_tx_ty), fmt_n(n_ct_ty)), 1)
log_step(sprintf("  Unique tanks: %s  (TX: %s | CTL: %s)",
                 fmt_n(n_tanks), fmt_n(n_tx_tanks), fmt_n(n_ct_tanks)), 1)
log_step(sprintf("  Make-model cells: %s unique | %s identified cell-years",
                 fmt_n(n_cells), fmt_n(n_cells_id)), 1)
log_step(sprintf("  Calendar years: %d – %d",
                 min(mm_tank_primary$panel_year),
                 max(mm_tank_primary$panel_year)), 1)

# ---- Unknown-cell exclusion report ----
prim_base <- tank_year_panel[mm_install_cohort %in% PRIMARY_YEARS &
                               tstop > tstart & !is.na(did_term)]
excl_uw <- prim_base[mm_wall == "Unknown-Wall",   .N]
excl_uf <- prim_base[mm_fuel == "Unknown-Fuel",   .N]
excl_na <- prim_base[is.na(make_model_tank),      .N]
tot_base <- nrow(prim_base)
log_step(sprintf("\nExclusion from primary sample (cohort-eligible tank-years = %s):",
                 fmt_n(tot_base)))
log_step(sprintf("  Unknown-Wall : %s (%s)", fmt_n(excl_uw), fmt_pct(excl_uw / tot_base)), 1)
log_step(sprintf("  Unknown-Fuel : %s (%s)", fmt_n(excl_uf), fmt_pct(excl_uf / tot_base)), 1)
log_step(sprintf("  NA cell      : %s (%s)", fmt_n(excl_na), fmt_pct(excl_na / tot_base)), 1)

fwrite(
  prim_base[, .(
    N_total        = .N,
    N_unknown_wall = sum(mm_wall == "Unknown-Wall"),
    N_unknown_fuel = sum(mm_fuel == "Unknown-Fuel"),
    N_na_cell      = sum(is.na(make_model_tank)),
    pre_closure_rate_uw = mean(closure_event[mm_wall == "Unknown-Wall"], na.rm = TRUE),
    pre_closure_rate_cls = mean(closure_event[mm_wall != "Unknown-Wall"], na.rm = TRUE)
  ), by = .(state_group = fifelse(texas_treated == 1, "Texas", "Control"))],
  file.path(OUTPUT_TABLES, "Diag_Exclusion_UnknownCells.csv")
)

# ---- Texas leverage diagnostic ----
tx_share <- mm_tank_primary[, .(n = .N, share = .N / n_ty),
  by = .(group = fifelse(texas_treated == 1, "TX", "CTL"))]
log_step(sprintf("\nTX share of tank-years: %s",
                 fmt_pct(tx_share[group == "TX", share])))
if (tx_share[group == "TX", share] > 0.40) {
  warning(sprintf(
    "Texas contributes %.1f%% of tank-years (>40%%).\n  Consider CR3 bias-reduced SEs and subcluster jackknife (MacKinnon-Nielsen-Webb 2023).",
    100 * tx_share[group == "TX", share]))
}
cat("\n")


#### S5: Build Exact-Date Cox Dataset ####

cat("========================================\n")
cat("S5: BUILD EXACT-DATE COX DATASET\n")
cat("========================================\n\n")

# Build a counting-process dataset where time is measured in exact calendar days
# (numeric: days since 1970-01-01, R's default Date origin).
#
# Each tank contributes at most TWO rows after splitting at the reform date:
#   Row 1 (reform_ep = 1): [install_date, min(close_date, REFORM_DATE))
#                          did_term = 0 for all tanks
#   Row 2 (reform_ep = 2): [REFORM_DATE, close_date_or_study_end)
#                          did_term = 1 for Texas tanks, 0 for controls
#
# Tanks that close before or are installed after the reform date contribute
# only one row. survSplit handles all edge cases automatically.

log_step("Filtering master tanks to primary sample attributes...")

exact_base <- master_tanks[
  !is.na(texas_treated)                  &
  !is.na(make_model_tank)                &
  mm_wall    != "Unknown-Wall"           &
  mm_fuel    != "Unknown-Fuel"           &
  !is.na(mm_capacity)                    &
  mm_install_cohort %in% PRIMARY_YEARS   &
  !is.na(tank_installed_date)            &
  tank_installed_date < STUDY_END,
  .(tank_panel_id, panel_id, facility_id, state, texas_treated,
    mm_wall, mm_fuel, mm_capacity, mm_install_cohort, make_model_tank,
    tank_installed_date,                         # kept to compute exact age later
    t_enter = as.numeric(tank_installed_date),   # days since 1970-01-01
    t_exit  = as.numeric(
      pmin(
        fifelse(!is.na(tank_closed_date),
                tank_closed_date, STUDY_END),
        STUDY_END)),
    failure = as.integer(
      !is.na(tank_closed_date)          &
      tank_closed_date <= STUDY_END     &
      tank_closed_date >= tank_installed_date)
  )
][t_exit > t_enter]

log_step(sprintf("Exact-date base: %s tanks", fmt_n(nrow(exact_base))))

# ---- survSplit at REFORM_DAYS ----
# Produces a data.frame with reform_ep = 1 (pre) or 2 (post).
# All non-Surv columns (including tank_installed_date) are preserved.
exact_split_df <- survSplit(
  formula = Surv(t_enter, t_exit, failure) ~ .,
  data    = as.data.frame(exact_base),
  cut     = REFORM_DAYS,
  episode = "reform_ep"
)
setDT(exact_split_df)
exact_split_df <- exact_split_df[t_exit > t_enter]

# Treatment: Texas AND in the post-reform interval
exact_split_df[, did_term := texas_treated * as.integer(reform_ep == 2L)]

log_step(sprintf("After survSplit: %s rows, %s tanks",
                 fmt_n(nrow(exact_split_df)),
                 fmt_n(uniqueN(exact_split_df$tank_panel_id))))

# ---- Exact age intervals ----
# age_enter / age_exit: years since exact install date at each interval endpoint
# These are used for the age-axis Cox robustness model (S17).
exact_split_df[, `:=`(
  age_enter = pmax(
    (t_enter - as.numeric(tank_installed_date)) / 365.25, 0),
  age_exit  =
    (t_exit - as.numeric(tank_installed_date)) / 365.25
)]
exact_split_df <- exact_split_df[age_exit > age_enter]

# Year factor: calendar year of interval end, for models using year dummies
exact_split_df[, year_fac := factor(
  as.integer(format(as.Date(t_exit, origin = "1970-01-01"), "%Y")),
  levels = year_levels
)]

log_step(sprintf("Exact-date split final: %s rows, %s events",
                 fmt_n(nrow(exact_split_df)),
                 fmt_n(sum(exact_split_df$failure))), 1)
cat("\n")


#### S6: Cell Coverage Diagnostics ####

cat("========================================\n")
cat("S6: CELL COVERAGE DIAGNOSTICS\n")
cat("========================================\n\n")

# Per research design Part 5.1: measure what share of tank-years fall in cells
# with BOTH Texas and control-state observations (identified cells).
# Target: pct_tank_years_identified >= 70%.
# If below, consider coarsening capacity bins or widening cohort intervals.

log_step("Computing cell coverage by (make_model_tank × panel_year)...")

cell_diag <- mm_tank_primary[, .(
  n_total  = .N,
  n_tx     = sum(texas_treated == 1L),
  n_ctl    = sum(texas_treated == 0L),
  has_both = as.integer(
    sum(texas_treated == 1L) > 0L & sum(texas_treated == 0L) > 0L)
), by = .(make_model_tank, panel_year)]

coverage_summary <- cell_diag[, .(
  total_cell_years          = .N,
  identified_cell_years     = sum(has_both),
  pct_identified            = round(mean(has_both) * 100, 1),
  tank_years_total          = sum(n_total),
  tank_years_identified     = sum(n_total * has_both),
  pct_tank_years_identified = round(
    sum(n_total * has_both) / sum(n_total) * 100, 1)
)]

log_step(sprintf("  Cell-years total        : %s",
                 fmt_n(coverage_summary$total_cell_years)), 1)
log_step(sprintf("  Identified cell-years   : %s (%s%%)",
                 fmt_n(coverage_summary$identified_cell_years),
                 coverage_summary$pct_identified), 1)
log_step(sprintf("  Tank-years identified   : %s / %s (%s%%)",
                 fmt_n(coverage_summary$tank_years_identified),
                 fmt_n(coverage_summary$tank_years_total),
                 coverage_summary$pct_tank_years_identified), 1)

if (coverage_summary$pct_tank_years_identified < 70) {
  warning(sprintf(
    "Only %s%% of tank-years are in identified cells (target: 70%%).\n  Consider: (a) collapsing Under-1k and 1k-5k capacity bins, or\n             (b) widening install-year cohorts from 3-year to 4-year bins.",
    coverage_summary$pct_tank_years_identified))
}

# Cell size table
cell_sizes_98 <- mm_tank_primary[panel_year == 1998L, .(
  n_tx     = sum(texas_treated == 1L),
  n_ctl    = sum(texas_treated == 0L),
  n_total  = .N,
  has_both = as.integer(
    any(texas_treated == 1L) & any(texas_treated == 0L))
), by = make_model_tank][order(-n_total)]

log_step(sprintf("\n  Cells at 1998: %s total | %s identified | %s well-populated (≥5 each side)",
                 nrow(cell_sizes_98),
                 cell_sizes_98[has_both == 1L, .N],
                 cell_sizes_98[n_tx >= 5L & n_ctl >= 5L, .N]), 1)

fwrite(cell_sizes_98, file.path(OUTPUT_TABLES, "Diag_TankCell_Sizes.csv"))

# Pre-reform balance within cells
pre_balance <- mm_tank_primary[panel_year < POST_YEAR, .(
  closure_rate = mean(closure_event, na.rm = TRUE),
  n            = .N
), by = .(make_model_tank,
          group = fifelse(texas_treated == 1L, "Texas", "Control"))
]
bal_wide <- dcast(pre_balance, make_model_tank ~ group,
                  value.var = c("closure_rate", "n"))
bal_wide[, pre_gap := closure_rate_Texas - closure_rate_Control]

n_flagged <- bal_wide[abs(pre_gap) > 0.02 |
                        n_Texas < 10L | n_Control < 10L, .N]
log_step(sprintf("  Cells flagged (|pre-gap|>2pp or n<10): %s", n_flagged), 1)
fwrite(bal_wide[order(-abs(pre_gap))],
       file.path(OUTPUT_TABLES, "Diag_PreBalance_ByCell.csv"))
cat("\n")


#### S6b: Cell Coverage Bar Charts ####

cat("========================================\n")
cat("S6b: CELL COVERAGE BAR CHARTS\n")
cat("========================================\n\n")

# Two grid charts -- one per cohort granularity -- showing the share of the
# primary sample that falls in each (wall x fuel x capacity x cohort) cell,
# separately for Texas (orange) and Control States (blue).
#
# Reading the charts:
#   - A cell where TX and CTL bars are roughly equal height is BALANCED.
#   - A cell where TX bars are tall and CTL bars are short (or absent) indicates
#     that Texas tanks are over-represented in that cell -- the identifying
#     assumption bears extra scrutiny there.
#   - A cell with no bars on either side is unidentified and excluded from
#     the primary regressions.
#   - Because scales are FIXED across all facets, you can directly compare
#     the height of bars across different wall/fuel/capacity combinations.

# ---- Forward cohort_3yr to mm_tank_primary ----
# mm_tank_primary is defined in S4 as a filter of tank_year_panel, so
# cohort_3yr is already present. Add it explicitly if somehow absent.
if (!"cohort_3yr" %in% names(mm_tank_primary)) {
  mm_tank_primary[, cohort_3yr := fcase(
    mm_install_cohort %in% as.character(1989:1991), "1989-1991",
    mm_install_cohort %in% as.character(1992:1994), "1992-1994",
    mm_install_cohort %in% as.character(1995:1997), "1995-1997",
    default = NA_character_
  )]
}

BIN_3YR_LEVELS <- c("1989-1991", "1992-1994", "1995-1997")
CAP_LEVELS     <- c("Under-1k", "1k-5k", "5k-12k", "12k-25k", "25k-Plus")

# ---- Single-year cohort chart ----
log_step("Building single-year cohort coverage chart...")

cov_1yr <- build_coverage_dt(
  dt            = mm_tank_primary,
  cohort_col    = "mm_install_cohort",
  cohort_levels = PRIMARY_YEARS,
  cap_levels    = CAP_LEVELS
)
make_coverage_figure(
  cov_dt    = cov_1yr,
  x_label   = "Install Year",
  file_stem = "Diag_CellCoverage_SingleYear"
)
fwrite(cov_1yr[order(mm_wall, mm_fuel, mm_capacity, cohort, group)],
       file.path(OUTPUT_TABLES, "Diag_CellCoverage_SingleYear.csv"))

# ---- 3-year cohort chart ----
log_step("Building 3-year cohort coverage chart...")

cov_3yr <- build_coverage_dt(
  dt            = mm_tank_primary[cohort_3yr %in% BIN_3YR_LEVELS],
  cohort_col    = "cohort_3yr",
  cohort_levels = BIN_3YR_LEVELS,
  cap_levels    = CAP_LEVELS
)
make_coverage_figure(
  cov_dt    = cov_3yr,
  x_label   = "Install Year Cohort (3-year bins)",
  file_stem = "Diag_CellCoverage_3yr"
)
fwrite(cov_3yr[order(mm_wall, mm_fuel, mm_capacity, cohort, group)],
       file.path(OUTPUT_TABLES, "Diag_CellCoverage_3yr.csv"))

# ---- Console comparison summary ----
summarise_cov <- function(dt, label) {
  s <- dt[, .(
    n_combos_nonzero = sum(n > 0),
    n_combos_zero    = sum(n == 0),
    mean_share_pct   = round(mean(share[n > 0]) * 100, 2),
    max_share_pct    = round(max(share)          * 100, 2)
  ), by = group]
  cat(sprintf("\n%s:\n", label))
  print(s)
}
summarise_cov(cov_1yr, "Single-year cohorts")
summarise_cov(cov_3yr, "3-year cohorts")
cat("\n")


#### S7: Survivorship Diagnostic ####

cat("========================================\n")
cat("S7: SURVIVORSHIP DIAGNOSTIC\n")
cat("========================================\n\n")

# Track the share of tanks that exit (close) by cohort × treatment group.
# If the reform causes rapid exit of high-risk TX tanks in early post-reform
# years, surviving TX tanks become a selected lower-risk subsample.
# The OLS ATT will attenuate toward zero in later years; Cox conditions on
# survival so this is not a bias problem for the primary spec, but it IS
# a finding (the selection effect IS the treatment effect for this margin).

surv_diag <- mm_tank_primary[, .(
  n_tanks         = uniqueN(tank_panel_id),
  exit_pre_reform = uniqueN(tank_panel_id[
    closure_event == 1L & panel_year < POST_YEAR]),
  exit_post_reform = uniqueN(tank_panel_id[
    closure_event == 1L & panel_year >= POST_YEAR])
), by = .(
  mm_install_cohort,
  state_group = fifelse(texas_treated == 1L, "Texas", "Control States")
)][order(state_group, mm_install_cohort)]

surv_diag[, `:=`(
  pct_exit_pre  = exit_pre_reform  / n_tanks,
  pct_exit_post = exit_post_reform / n_tanks
)]

log_step("Survivorship by cohort × group:")
print(surv_diag)
fwrite(surv_diag,
       file.path(OUTPUT_TABLES, "Diag_Survivorship_ByCohort.csv"))

# TX share within each cell over time (attrition diagnostic)
tx_share_time <- mm_tank_primary[, .(
  n_tx  = sum(texas_treated == 1L),
  n_ctl = sum(texas_treated == 0L),
  n     = .N,
  tx_share = sum(texas_treated == 1L) / .N
), by = .(make_model_tank, panel_year)]

tx_share_wide <- dcast(
  tx_share_time[, .(make_model_tank, panel_year, tx_share)],
  make_model_tank ~ panel_year,
  value.var = "tx_share"
)
fwrite(tx_share_wide,
       file.path(OUTPUT_TABLES, "Diag_TXShare_CellTime.csv"))
cat("\n")


#### S8: Binned Scatter — Closure Rate by Cohort × Period × Group ####

cat("========================================\n")
cat("S8: BINNED SCATTER\n")
cat("========================================\n\n")

scatter_dt <- mm_tank_primary[, .(
  closure_rate = mean(closure_event, na.rm = TRUE),
  n            = .N
), by = .(
  mm_install_cohort,
  state_group = fifelse(texas_treated == 1L, "Texas", "Control States"),
  period      = fifelse(panel_year < POST_YEAR, "Pre-reform", "Post-reform")
)]

scatter_dt[, group_period := factor(
  paste(state_group, period),
  levels = c("Control States Pre-reform",  "Control States Post-reform",
             "Texas Pre-reform",            "Texas Post-reform"))]

scatter_dt[, cohort_order := match(mm_install_cohort, PRIMARY_YEARS)]
setorder(scatter_dt, cohort_order)
scatter_dt[, mm_install_cohort := factor(mm_install_cohort,
                                         levels = PRIMARY_YEARS)]

p_scatter <- ggplot(
  scatter_dt[!is.na(mm_install_cohort)],
  aes(x     = mm_install_cohort,
      y     = closure_rate,
      color = group_period,
      group = group_period)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Control States Pre-reform"  = COL_CTRL,
    "Control States Post-reform" = "#56B4E9",
    "Texas Pre-reform"           = COL_PRE,
    "Texas Post-reform"          = COL_TX)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Install Year Cohort",
       y = "Mean Annual Closure Rate") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_BinnedScatter_CohortClosure.png"),
       p_scatter, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_BinnedScatter_CohortClosure.pdf"),
       p_scatter, width = 9, height = 5.5, device = cairo_pdf)
cat("\n")


#### S9: PRIMARY Cox Model ####

cat("========================================\n")
cat("S9: PRIMARY COX MODEL\n")
cat("========================================\n\n")

# Primary specification (eq-cox in the empirical framework):
#   h(t|i) = h_0m(t) · exp(β · did_term)
#
# where:
#   t             = calendar time (days since 1970-01-01)
#   h_0m(t)       = cell-specific baseline hazard (strata(make_model_tank))
#   did_term      = texas_treated × I(t >= REFORM_DATE)
#   β             = log hazard ratio; exp(β) = HR
#
# Identification: within each make_model_tank cell, β is identified by the
# change in the Texas closure hazard at the reform date relative to the pre-
# reform trajectory shared by TX and control tanks in the same cell.
#
# Standard errors clustered at the state level.
# Time axis: exact calendar days from exact install dates (no interval ties).

log_step("Estimating primary Cox model (calendar time, make_model_tank strata)...")

m_cox_primary <- coxph(
  Surv(t_enter, t_exit, failure) ~
    did_term + strata(make_model_tank),
  data    = exact_split_df,
  cluster = exact_split_df$state,
  ties    = "efron"
)

cox_prim <- extract_cox_row(m_cox_primary)

log_step(sprintf("  HR  = %.3f (95%% CI: %.3f – %.3f)",
                 cox_prim$hr,
                 exp(cox_prim$coef - 1.96 * cox_prim$se),
                 exp(cox_prim$coef + 1.96 * cox_prim$se)), 1)
log_step(sprintf("  log-coef = %.4f  SE = %.4f  p = %.4f",
                 cox_prim$coef, cox_prim$se, cox_prim$p), 1)
log_step(sprintf("  N = %s  Events = %s",
                 fmt_n(cox_prim$n), fmt_n(cox_prim$ev)), 1)

# Summary for quick inspection
print(summary(m_cox_primary)$coefficients)

# Also estimate age-axis Cox (t = years since installation) for comparison
log_step("\nEstimating age-axis Cox model (robustness / comparison)...")

m_cox_age <- coxph(
  Surv(age_enter, age_exit, failure) ~
    did_term + strata(make_model_tank),
  data    = exact_split_df[age_exit > age_enter],
  cluster = exact_split_df[age_exit > age_enter]$state,
  ties    = "efron"
)

cox_age <- extract_cox_row(m_cox_age)
log_step(sprintf("  Age-axis HR = %.3f  log-coef = %.4f  SE = %.4f  p = %.4f",
                 cox_age$hr, cox_age$coef, cox_age$se, cox_age$p), 1)
cat("\n")


#### S10: Cox Event Study ####

cat("========================================\n")
cat("S10: COX EVENT STUDY\n")
cat("========================================\n\n")

# Parallel trends test in a Cox framework using the annual tank-year panel
# (not exact-date data, so year bins are exact calendar year integers).
#
# Model: Surv(tstart, tstop, closure_event) ~
#          ry_m5 + ... + ry_m2 + ry_0 + ... + ry_5 + strata(make_model_tank)
# where ry_k = I(rel_year_bin == k) × texas_treated  for k ≠ -1 (reference)
#
# Within each cell-year stratum, the estimator identifies whether Texas tanks
# diverge from control tanks in each relative year, net of the cell baseline.

es_years     <- sort(unique(mm_tank_primary$rel_year_bin))
es_years_nref <- es_years[es_years != -1L]

for (yr in es_years_nref) {
  vname <- ifelse(yr < 0,
                  paste0("ry_m", abs(yr)),
                  paste0("ry_",  yr))
  mm_tank_primary[, (vname) := as.integer(rel_year_bin == yr) * texas_treated]
}
ry_vars <- sort(grep("^ry_", names(mm_tank_primary), value = TRUE))

cox_es_fml <- as.formula(paste(
  "Surv(tstart, tstop, closure_event) ~",
  paste(ry_vars, collapse = " + "),
  "+ strata(make_model_tank)"
))

log_step("Estimating Cox event study...")
m_cox_es <- coxph(
  cox_es_fml,
  data    = mm_tank_primary,
  cluster = mm_tank_primary$state,
  ties    = "efron"
)

# Extract and tidy
cox_es_coefs <- as.data.table(
  summary(m_cox_es)$coefficients,
  keep.rownames = "term"
)
setnames(cox_es_coefs, c("term", "coef", "exp_coef", "se", "z", "p"))

cox_es_coefs[, rel_year := as.integer(
  ifelse(grepl("^ry_m", term),
         -as.integer(gsub("ry_m", "", term)),
          as.integer(gsub("ry_",  "", term)))
)]
cox_es_coefs <- cox_es_coefs[!is.na(rel_year)]
cox_es_coefs[, `:=`(
  hr    = exp_coef,
  ci_lo = exp(coef - 1.96 * se),
  ci_hi = exp(coef + 1.96 * se),
  period = fcase(rel_year < 0, "Pre", rel_year == 0, "Post", default = "Post")
)]

# Add reference row at -1
cox_es_coefs <- rbind(
  cox_es_coefs,
  data.table(term = "ref", coef = 0, exp_coef = 1, se = 0,
             z = 0, p = 1, rel_year = -1L, hr = 1,
             ci_lo = 1, ci_hi = 1, period = "Ref"),
  fill = TRUE
)
setorder(cox_es_coefs, rel_year)

# Pre-period joint test
pre_vars <- ry_vars[grepl("^ry_m[2-5]", ry_vars)]
if (length(pre_vars) >= 2) {
  pre_test <- linearHypothesis(m_cox_es, paste0(pre_vars, " = 0"),
                               test = "Chisq")
  log_step(sprintf("Pre-period joint χ² test (%d dof): p = %.3f",
                   length(pre_vars),
                   pre_test[2, "Pr(>Chisq)"]))
}

# Plot
p_cox_es <- ggplot(cox_es_coefs, aes(x = rel_year, y = hr)) +
  annotate("rect",
           xmin = -Inf, xmax = -0.5,
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.45) +
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
  scale_x_continuous(breaks = es_years) +
  labs(x = "Years Relative to Reform (1999 = 0)",
       y = "Hazard Ratio (ref = year −1)") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_EventStudy.png"),
       p_cox_es, width = 10, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_EventStudy.pdf"),
       p_cox_es, width = 10, height = 5.5, device = cairo_pdf)

fwrite(cox_es_coefs,
       file.path(OUTPUT_TABLES, "Table_CoxES_Coefficients.csv"))

# Clean up event-study variables from the panel
mm_tank_primary[, (ry_vars) := NULL]
cat("\n")


#### S11: Reference OLS LPM and Cloglog FE ####

cat("========================================\n")
cat("S11: REFERENCE OLS LPM AND CLOGLOG FE\n")
cat("========================================\n\n")

# These reference specifications serve two purposes:
#   (a) OLS provides a transparent, prior-literature-comparable benchmark
#   (b) Cloglog addresses survivorship attenuation relative to OLS; the
#       Prentice-Gloeckler cloglog estimates log-hazard conditional on
#       survival to t using identical data and FE structure
#
# Both use:
#   - unit FE: tank_panel_id (individual tank intercept)
#   - cell-year FE: make_model_tank^panel_year (within-cell identification)
#   - time-varying age control: age_bin (NOT used for cell assignment)
#   - clustering: state level
#
# NOTE: with 20 state clusters, conventional CR1 SEs over-reject.
# Wild cluster bootstrap inference (fwildclusterboot) is preferred for
# hypothesis tests; CR1 SEs are reported alongside for transparency.

panel_dt <- mm_tank_primary[!is.na(age_bin) & tstop > tstart & !is.na(did_term)]

log_step("Estimating OLS LPM (reference spec)...")
m_ols <- feols(
  closure_event ~ did_term + age_bin | tank_panel_id + make_model_tank^panel_year,
  data    = panel_dt,
  cluster = ~state
)

log_step("Estimating Prentice-Gloeckler cloglog FE (reference spec)...")
m_cloglog <- feglm(
  closure_event ~ did_term + age_bin | tank_panel_id + make_model_tank^panel_year,
  family  = binomial(link = "cloglog"),
  data    = panel_dt,
  cluster = ~state
)

ols_row <- extract_panel_row(m_ols)
cll_row <- extract_panel_row(m_cloglog)

log_step(sprintf("  OLS    : coef = %.4f  SE = %.4f  p = %.4f  N = %s",
                 ols_row$coef, ols_row$se, ols_row$p, fmt_n(ols_row$n)), 1)
log_step(sprintf("  Cloglog: coef = %.4f  SE = %.4f  p = %.4f  N = %s",
                 cll_row$coef, cll_row$se, cll_row$p, fmt_n(cll_row$n)), 1)

# Discrete hazard profile from cloglog predictions
panel_dt[, pred_hazard := predict(m_cloglog, type = "response")]

haz_profile <- panel_dt[!is.na(pred_hazard) & !is.na(age_bin), .(
  mean_hazard = mean(pred_hazard, na.rm = TRUE)
), by = .(
  age_bin,
  group = fcase(
    texas_treated == 0L,                           "Control (community-rated)",
    texas_treated == 1L & panel_year < POST_YEAR,  "Texas pre-reform",
    texas_treated == 1L & panel_year >= POST_YEAR, "Texas post-reform"
  )
)][!is.na(group)]

haz_profile[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
haz_profile[, group := factor(group, levels = c(
  "Control (community-rated)", "Texas pre-reform", "Texas post-reform"))]

p_haz_profile <- ggplot(haz_profile,
  aes(x = age_bin, y = mean_hazard, color = group, group = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "Control (community-rated)" = COL_CTRL,
    "Texas pre-reform"          = COL_PRE,
    "Texas post-reform"         = COL_TX)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Tank Age (years, 3-year bins)",
       y = "Predicted Annual Closure Hazard") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_DiscreteHazard_AgeProfile.png"),
       p_haz_profile, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_DiscreteHazard_AgeProfile.pdf"),
       p_haz_profile, width = 9, height = 5.5, device = cairo_pdf)

panel_dt[, pred_hazard := NULL]
cat("\n")


#### S12: Duration Model Comparison Table ####

cat("========================================\n")
cat("S12: DURATION MODEL COMPARISON TABLE\n")
cat("========================================\n\n")

# Side-by-side comparison of all primary and reference duration specifications:
#   (1) Cox primary      — exact calendar dates, strata(make_model_tank)
#   (2) Cox age-axis     — exact age intervals,  strata(make_model_tank)
#   (3) Cloglog FE       — annual panel, tank FE + make_model_tank^panel_year
#   (4) OLS LPM          — annual panel, tank FE + make_model_tank^panel_year

# Cox primary
r1 <- extract_cox_row(m_cox_primary)
# Cox age-axis
r2 <- extract_cox_row(m_cox_age)
# Cloglog
r3 <- extract_panel_row(m_cloglog)
# OLS
r4 <- extract_panel_row(m_ols)

n_events_panel <- fmt_n(sum(panel_dt$closure_event, na.rm = TRUE))

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Effect of Experience Rating on Tank Closure: Duration Model Estimates}",
  "\\label{tbl:duration_models}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) \\\\",
  " & \\multicolumn{2}{c}{\\textit{Primary: Cox}} & \\multicolumn{2}{c}{\\textit{Reference}} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  " & Calendar time & Age time & Cloglog FE & OLS LPM \\\\",
  "\\midrule",
  sprintf("Hazard ratio (Texas $\\times$ Post) & %s & %s & --- & --- \\\\",
          fmt_hr(r1$hr, r1$p), fmt_hr(r2$hr, r2$p)),
  sprintf("95\\%% CI & %s & %s & & \\\\",
          fmt_ci(exp(r1$coef - 1.96*r1$se), exp(r1$coef + 1.96*r1$se)),
          fmt_ci(exp(r2$coef - 1.96*r2$se), exp(r2$coef + 1.96*r2$se))),
  "\\addlinespace",
  sprintf("Log-hazard / OLS coefficient & %s & %s & %s & %s \\\\",
          fmt_est(r1$coef, r1$p), fmt_est(r2$coef, r2$p),
          fmt_est(r3$coef, r3$p), fmt_est(r4$coef, r4$p)),
  sprintf("(SE) & %s & %s & %s & %s \\\\",
          fmt_se(r1$se), fmt_se(r2$se), fmt_se(r3$se), fmt_se(r4$se)),
  "\\midrule",
  "Estimand & Log-hazard & Log-hazard & Log-hazard & Pr(closure) \\\\",
  "Conditioning & Survival to $t$ & Survival to age & Survival to $t$ & Unconditional \\\\",
  "Time axis & Calendar days & Tank age (yrs) & Calendar year & Calendar year \\\\",
  "Baseline hazard & By cell & By cell & Absorbed by FE & Absorbed by FE \\\\",
  "Unit FE & Strata & Strata & Tank FE & Tank FE \\\\",
  "Cell $\\times$ year FE & Strata only & Strata only & Yes & Yes \\\\",
  "Age control & --- & --- & $\\checkmark$ & $\\checkmark$ \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
          fmt_n(r1$n), fmt_n(r2$n), fmt_n(r3$n), fmt_n(r4$n)),
  sprintf("Events (closures) & %s & %s & %s & %s \\\\",
          fmt_n(r1$ev), fmt_n(r2$ev),
          n_events_panel, n_events_panel),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.98\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Tank-level analysis.",
  "Primary sample: non-mandate install cohorts (1989--1997), classified wall and fuel type,",
  "non-missing capacity. Columns~(1)--(2): counting-process Cox models with exact",
  "installation and closure dates; strata absorb the cell-specific baseline hazard",
  "with no parametric restriction.",
  "Column~(1) uses calendar time as the time axis.",
  "Column~(2) uses exact tank age (years since installation) as the time axis.",
  "Columns~(3)--(4): annual tank-year panel with tank fixed effects and",
  "make-model-cell $\\times$ year fixed effects.",
  "Column~(3): Prentice--Gl\\\"ockler complementary log-log; estimates",
  "log-hazard ratio conditional on survival to $t$.",
  "Column~(4): OLS linear probability model; estimates the ATT on the",
  "unconditional annual closure probability (transparent prior-literature benchmark).",
  "All specifications cluster standard errors at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_Duration_Models.tex")
cat("\n")


#### S13: HTE by Wall Type ####

cat("========================================\n")
cat("S13: HTE BY WALL TYPE\n")
cat("========================================\n\n")

# Test whether the reform effect is larger for single-walled tanks,
# consistent with the premium-gradient mechanism (SW tanks carry higher
# actuarial risk → larger per-tank premium under experience rating).
#
# PANEL SPEC (OLS/cloglog):
#   closure_event ~ did_term:mm_wall | tank_panel_id + make_model_tank^panel_year
#   Per research design memo Section 4.3: did_term:mm_wall interaction.
#   Since mm_wall is part of make_model_tank, this effectively compares the
#   treatment effect between SW and DW cells.
#
# COX SPEC:
#   Two separate cell-specific treatment indicators:
#   did_term_sw = did_term × I(mm_wall == "Single-Walled")
#   did_term_dw = did_term × I(mm_wall == "Double-Walled")
#   Within each stratum all tanks share the same mm_wall, so these are
#   perfectly identified by the contrast across cell types.

log_step("HTE by wall type — OLS spec...")
m_hte_wall_ols <- feols(
  closure_event ~ did_term:mm_wall + age_bin |
    tank_panel_id + make_model_tank^panel_year,
  data    = panel_dt,
  cluster = ~state
)

log_step("HTE by wall type — Cloglog spec...")
m_hte_wall_cll <- feglm(
  closure_event ~ did_term:mm_wall + age_bin |
    tank_panel_id + make_model_tank^panel_year,
  family  = binomial(link = "cloglog"),
  data    = panel_dt,
  cluster = ~state
)

# Cox HTE by wall type (separate treatment indicators)
exact_split_df[, `:=`(
  did_term_sw = did_term * as.integer(mm_wall == "Single-Walled"),
  did_term_dw = did_term * as.integer(mm_wall == "Double-Walled")
)]

log_step("HTE by wall type — Cox spec...")
m_hte_wall_cox <- coxph(
  Surv(t_enter, t_exit, failure) ~
    did_term_sw + did_term_dw + strata(make_model_tank),
  data    = exact_split_df,
  cluster = exact_split_df$state,
  ties    = "efron"
)

# Tidy Cox wall HTE results
wall_cox_s <- summary(m_hte_wall_cox)$coefficients
sw_hr  <- fmt_hr(exp(wall_cox_s["did_term_sw", "coef"]),
                      wall_cox_s["did_term_sw", "Pr(>|z|)"])
dw_hr  <- fmt_hr(exp(wall_cox_s["did_term_dw", "coef"]),
                      wall_cox_s["did_term_dw", "Pr(>|z|)"])
sw_se  <- fmt_se(wall_cox_s["did_term_sw", "se(coef)"])
dw_se  <- fmt_se(wall_cox_s["did_term_dw", "se(coef)"])

# OLS wall HTE
ols_wall_ct <- coeftable(m_hte_wall_ols)
sw_ols <- ols_wall_ct[grep("did_term:mm_wallSingle-Walled", rownames(ols_wall_ct)), ]
dw_ols <- ols_wall_ct[grep("did_term:mm_wallDouble-Walled", rownames(ols_wall_ct)), ]

sw_ols_est <- fmt_est(sw_ols[, "Estimate"], sw_ols[, "Pr(>|t|)"])
dw_ols_est <- fmt_est(dw_ols[, "Estimate"], dw_ols[, "Pr(>|t|)"])
sw_ols_se  <- fmt_se(sw_ols[, "Std. Error"])
dw_ols_se  <- fmt_se(dw_ols[, "Std. Error"])

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Reform Effect on Closure Hazard: Heterogeneity by Tank Wall Construction}",
  "\\label{tbl:hte_wall}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & (1) & (2) \\\\",
  " & Cox (primary) & OLS LPM (reference) \\\\",
  "\\midrule",
  "\\textit{Texas $\\times$ Post $\\times$:} & & \\\\",
  sprintf("\\quad Single-walled & %s & %s \\\\", sw_hr,      sw_ols_est),
  sprintf("                    & %s & %s \\\\", sw_se,      sw_ols_se),
  sprintf("\\quad Double-walled & %s & %s \\\\", dw_hr,      dw_ols_est),
  sprintf("                    & %s & %s \\\\", dw_se,      dw_ols_se),
  "\\midrule",
  "Metric & Hazard ratio & OLS coefficient \\\\",
  "Cell $\\times$ year & Strata & FE \\\\",
  "Unit FE & Strata & Tank FE \\\\",
  sprintf("Observations & %s & %s \\\\",
          fmt_n(m_hte_wall_cox$n), fmt_n(nobs(m_hte_wall_ols))),
  sprintf("Events / closures & %s & %s \\\\",
          fmt_n(m_hte_wall_cox$nevent), fmt_n(sum(panel_dt$closure_event))),
  "\\bottomrule",
  "\\multicolumn{3}{p{0.80\\linewidth}}{\\footnotesize",
  "\\textit{Notes:} Tank-level primary sample.",
  "Column~(1): Cox model with separate treatment indicators for single- and",
  "double-walled tanks; strata(make\\_model\\_tank) absorbs cell baselines.",
  "Column~(2): OLS with \\texttt{did\\_term:mm\\_wall} interaction,",
  "tank FE, and cell$\\times$year FE.",
  "Standard errors clustered at the state level.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
), "Table_HTE_WallType.tex")

fwrite(as.data.table(coeftable(m_hte_wall_ols), keep.rownames = "term"),
       file.path(OUTPUT_TABLES, "Table_HTE_WallType_OLS.csv"))
fwrite(as.data.table(wall_cox_s, keep.rownames = "term"),
       file.path(OUTPUT_TABLES, "Table_HTE_WallType_Cox.csv"))

# Clean up wall HTE indicators
exact_split_df[, c("did_term_sw", "did_term_dw") := NULL]
cat("\n")


#### S14: HTE by Install Cohort ####

cat("========================================\n")
cat("S14: HTE BY INSTALL COHORT\n")
cat("========================================\n\n")

# Test whether the reform effect is larger for older cohorts.
# Older tanks at reform date → higher per-tank premium under experience rating
# → stronger closure incentive.
# Three cohorts in primary sample: 1989-1991 (7-9yr at reform),
# 1992-1994 (4-6yr), 1995-1997 (1-3yr).

log_step("HTE by install cohort — OLS spec...")
m_hte_cohort_ols <- feols(
  closure_event ~ did_term:mm_install_cohort + age_bin |
    tank_panel_id + make_model_tank^panel_year,
  data    = panel_dt,
  cluster = ~state
)

log_step("HTE by install cohort — Cloglog spec...")
m_hte_cohort_cll <- feglm(
  closure_event ~ did_term:mm_install_cohort + age_bin |
    tank_panel_id + make_model_tank^panel_year,
  family  = binomial(link = "cloglog"),
  data    = panel_dt,
  cluster = ~state
)

# Cox: separate treatment indicators by cohort
for (coh in PRIMARY_YEARS) {
  safe_coh <- gsub("-", "_", coh)
  exact_split_df[, paste0("did_term_c", safe_coh) :=
    did_term * as.integer(mm_install_cohort == coh)]
}
coh_did_vars <- grep("^did_term_c", names(exact_split_df), value = TRUE)

log_step("HTE by install cohort — Cox spec...")
cox_cohort_fml <- as.formula(paste(
  "Surv(t_enter, t_exit, failure) ~",
  paste(coh_did_vars, collapse = " + "),
  "+ strata(make_model_tank)"
))
m_hte_cohort_cox <- coxph(
  cox_cohort_fml,
  data    = exact_split_df,
  cluster = exact_split_df$state,
  ties    = "efron"
)

# Extract and plot
coh_cox_s <- summary(m_hte_cohort_cox)$coefficients

cohort_forest <- data.table(
  cohort = PRIMARY_YEARS,
  var    = coh_did_vars,
  hr     = exp(coh_cox_s[coh_did_vars, "coef"]),
  coef   = coh_cox_s[coh_did_vars, "coef"],
  se     = coh_cox_s[coh_did_vars, "se(coef)"],
  p      = coh_cox_s[coh_did_vars, "Pr(>|z|)"]
)
cohort_forest[, `:=`(
  ci_lo = exp(coef - 1.96 * se),
  ci_hi = exp(coef + 1.96 * se)
)]
cohort_forest[, cohort := factor(cohort,
                                  levels = rev(PRIMARY_YEARS))]

p_cohort_forest <- ggplot(cohort_forest,
  aes(x = hr, y = cohort, xmin = ci_lo, xmax = ci_hi)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(size = 0.6, linewidth = 0.7, color = COL_TX) +
  labs(x = "Hazard Ratio (Texas × Post)", y = "Install Cohort") +
  theme_pub() +
  theme(panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3))

ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_CohortForest.png"),
       p_cohort_forest, width = 7, height = 4, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Cox_CohortForest.pdf"),
       p_cohort_forest, width = 7, height = 4, device = cairo_pdf)

fwrite(cohort_forest,
       file.path(OUTPUT_TABLES, "Table_HTE_Cohort_Cox.csv"))
fwrite(as.data.table(coeftable(m_hte_cohort_ols), keep.rownames = "term"),
       file.path(OUTPUT_TABLES, "Table_HTE_Cohort_OLS.csv"))

# Clean up cohort HTE indicators
exact_split_df[, (coh_did_vars) := NULL]
cat("\n")


#### S15: Age-at-Closure Analysis ####

cat("========================================\n")
cat("S15: AGE-AT-CLOSURE ANALYSIS\n")
cat("========================================\n\n")

# Age at closure is measured from exact install date to exact closure date
# (years). This is a direct output of the tank-level analysis; the facility-
# level equivalent (avg_tank_age) was a lossy average across multiple tanks.

closed_exact <- exact_base[failure == 1L, .(
  tank_panel_id,
  state,
  texas_treated,
  mm_install_cohort,
  mm_wall,
  age_at_closure = as.numeric(t_exit - t_enter) / 365.25,
  period = fifelse(t_exit >= REFORM_DAYS, "Post-reform", "Pre-reform")
)]
closed_exact[, group := factor(
  paste(fifelse(texas_treated == 1L, "Texas", "Control States"), period),
  levels = c("Control States Pre-reform",  "Control States Post-reform",
             "Texas Pre-reform",            "Texas Post-reform"))]

summary_acl <- closed_exact[, .(
  n      = .N,
  mean   = round(mean(age_at_closure),   2),
  median = round(median(age_at_closure), 2),
  sd     = round(sd(age_at_closure),     2)
), by = group][order(group)]

log_step("Age at closure summary:")
print(summary_acl)
fwrite(summary_acl,
       file.path(OUTPUT_TABLES, "Table_AgeAtClosure_Summary.csv"))

p_hist_acl <- ggplot(closed_exact,
  aes(x = age_at_closure, fill = group)) +
  geom_histogram(binwidth = 1, alpha = 0.85, color = "white", linewidth = 0.2) +
  geom_vline(data = summary_acl,
             aes(xintercept = median),
             linetype = "dashed", linewidth = 0.6) +
  facet_wrap(~group, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c(
    "Control States Pre-reform"  = COL_CTRL,
    "Control States Post-reform" = "#56B4E9",
    "Texas Pre-reform"           = COL_PRE,
    "Texas Post-reform"          = COL_TX),
    guide = "none") +
  scale_x_continuous(breaks = seq(0, 30, 3)) +
  labs(x = "Age at Closure — Exact Install to Close Date (years)",
       y = "Number of Closures") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_AgeAtClosure_Histogram.png"),
       p_hist_acl, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_AgeAtClosure_Histogram.pdf"),
       p_hist_acl, width = 10, height = 6, device = cairo_pdf)
cat("\n")


#### S16: Kaplan-Meier Survival Curves ####

cat("========================================\n")
cat("S16: KAPLAN-MEIER SURVIVAL CURVES\n")
cat("========================================\n\n")

# KM curves use tank age (exact years from install date) as the time axis.
# Groups: Texas vs Control × cohort.
# Note: KM is purely descriptive; it does not condition on cell membership
# and does not identify the causal effect. It illustrates the raw survival
# patterns before and after the reform for each group.

km_base <- exact_base[, .(
  tank_panel_id,
  texas_treated,
  mm_install_cohort,
  time_to_event = as.numeric(t_exit - t_enter) / 365.25,
  event         = failure
)]
km_base[, state_group := fifelse(texas_treated == 1L, "Texas", "Control States")]
km_base[, km_group := factor(
  paste(state_group, mm_install_cohort),
  levels = c(paste("Control States", PRIMARY_YEARS),
             paste("Texas",          PRIMARY_YEARS))
)]

km_fit  <- survfit(Surv(time_to_event, event) ~ km_group, data = km_base)
km_tidy <- as.data.table(broom::tidy(km_fit))
km_tidy[, group := gsub("km_group=", "", strata)]
km_tidy[, group := factor(group, levels = levels(km_base$km_group))]
km_tidy[, state_part := fifelse(grepl("^Texas", group), "Texas", "Control States")]

p_km <- ggplot(km_tidy, aes(x = time, y = estimate, color = group)) +
  geom_step(linewidth = 0.7) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.07, color = NA) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  facet_wrap(~state_part, ncol = 2) +
  labs(x = "Tank Age (years since installation)",
       y = "Survival Probability (not yet closed)") +
  theme_pub() +
  theme(legend.position = "right")

ggsave(file.path(OUTPUT_FIGURES, "Figure_KM_TankSurvival.png"),
       p_km, width = 10, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_KM_TankSurvival.pdf"),
       p_km, width = 10, height = 5.5, device = cairo_pdf)
cat("\n")


#### S17: Robustness Specifications ####

cat("========================================\n")
cat("S17: ROBUSTNESS SPECIFICATIONS\n")
cat("========================================\n\n")

# ---- 17a: 3-dimension cell (drop capacity) ----
# Per research design Threat 7 / Section 5.
# If capacity measurement error drives the primary estimate, the 3-dim cell
# (wall, fuel, cohort) and 4-dim cell (wall, fuel, capacity, cohort) estimates
# should diverge. Report both; divergence flags measurement error.

exact_split_df[, make_model_3dim := paste(mm_wall, mm_fuel, mm_install_cohort,
                                           sep = "_")]
panel_dt[, make_model_3dim := paste(mm_wall, mm_fuel, mm_install_cohort,
                                     sep = "_")]

log_step("Robustness 17a: 3-dimension cell...")
m_cox_3dim <- coxph(
  Surv(t_enter, t_exit, failure) ~
    did_term + strata(make_model_3dim),
  data    = exact_split_df,
  cluster = exact_split_df$state,
  ties    = "efron"
)
m_ols_3dim <- feols(
  closure_event ~ did_term + age_bin |
    tank_panel_id + make_model_3dim^panel_year,
  data    = panel_dt,
  cluster = ~state
)
r_3dim_cox <- extract_cox_row(m_cox_3dim)
r_3dim_ols <- extract_panel_row(m_ols_3dim)
log_step(sprintf("  Cox 3-dim HR = %.3f (p=%.3f) | Cox 4-dim HR = %.3f (p=%.3f)",
                 r_3dim_cox$hr, r_3dim_cox$p,
                 cox_prim$hr,   cox_prim$p), 1)
log_step(sprintf("  OLS 3-dim coef = %.4f | OLS 4-dim coef = %.4f",
                 r_3dim_ols$coef, ols_row$coef), 1)

# ---- 17b: Drop border states ----
# Per research design Threat 6.
# Border states (TX neighbors: OK, AR, LA, NM) could be contaminated via
# insurer entry, contractor capacity, or informal spillovers.

BORDER_STATES <- c("OK", "AR", "LA", "NM")
exact_no_border <- exact_split_df[!state %in% BORDER_STATES]
panel_no_border <- panel_dt[!state %in% BORDER_STATES]

log_step("Robustness 17b: drop border states...")
m_cox_noborder <- coxph(
  Surv(t_enter, t_exit, failure) ~
    did_term + strata(make_model_tank),
  data    = exact_no_border,
  cluster = exact_no_border$state,
  ties    = "efron"
)
m_ols_noborder <- feols(
  closure_event ~ did_term + age_bin |
    tank_panel_id + make_model_tank^panel_year,
  data    = panel_no_border,
  cluster = ~state
)
r_nb_cox <- extract_cox_row(m_cox_noborder)
r_nb_ols <- extract_panel_row(m_ols_noborder)
log_step(sprintf("  Cox no-border HR = %.3f (p=%.3f)", r_nb_cox$hr, r_nb_cox$p), 1)
log_step(sprintf("  OLS no-border coef = %.4f", r_nb_ols$coef), 1)

# ---- 17c: Pre-1989 cohort expansion with mandate control ----
# Per research design Threat 2.
# The primary sample restricts to post-mandate cohorts (1989+).
# Including pre-1989 cohorts requires a mandate-period control.
# The reform estimate should be stable; a large shift would indicate
# mandate contamination in the main sample is a concern.

pre89_cohorts <- c("Pre-1980", "1980-1982", "1983-1985", "1986-1988")
panel_expanded <- tank_year_panel[
  !is.na(make_model_tank)                          &
  mm_wall    != "Unknown-Wall"                     &
  mm_fuel    != "Unknown-Fuel"                     &
  !is.na(mm_capacity)                              &
  mm_install_cohort %in% c(PRIMARY_YEARS, pre89_cohorts) &
  tstop > tstart                                   &
  !is.na(did_term)                                 &
  !is.na(age_bin)
]

log_step("Robustness 17c: pre-1989 cohort expansion + mandate control...")
m_ols_expanded <- feols(
  closure_event ~ did_term + mandate_active + age_bin |
    tank_panel_id + make_model_tank^panel_year,
  data    = panel_expanded,
  cluster = ~state
)
r_exp_ols <- extract_panel_row(m_ols_expanded)
log_step(sprintf("  OLS expanded coef = %.4f (vs primary %.4f)",
                 r_exp_ols$coef, ols_row$coef), 1)

# ---- Robustness summary table ----
rob_summary <- data.table(
  Specification = c(
    "Primary (Cox calendar, 4-dim cell)",
    "Primary (OLS, 4-dim cell)",
    "Cox 3-dim cell (no capacity)",
    "OLS 3-dim cell (no capacity)",
    "Cox 4-dim, drop border states",
    "OLS 4-dim, drop border states",
    "OLS expanded to pre-1989 + mandate control"
  ),
  Estimand = c(
    "Log-hazard ratio", "OLS coefficient",
    "Log-hazard ratio", "OLS coefficient",
    "Log-hazard ratio", "OLS coefficient",
    "OLS coefficient"
  ),
  Estimate = round(c(
    r1$coef,       ols_row$coef,
    r_3dim_cox$coef, r_3dim_ols$coef,
    r_nb_cox$coef,   r_nb_ols$coef,
    r_exp_ols$coef
  ), 5),
  HR_or_ExpCoef = round(c(
    r1$hr,          NA,
    r_3dim_cox$hr,  NA,
    r_nb_cox$hr,    NA,
    NA
  ), 4),
  SE = round(c(
    r1$se,          ols_row$se,
    r_3dim_cox$se,  r_3dim_ols$se,
    r_nb_cox$se,    r_nb_ols$se,
    r_exp_ols$se
  ), 5),
  p_value = round(c(
    r1$p,           ols_row$p,
    r_3dim_cox$p,   r_3dim_ols$p,
    r_nb_cox$p,     r_nb_ols$p,
    r_exp_ols$p
  ), 4),
  N_obs = c(
    r1$n,            r4$n,
    r_3dim_cox$n,    nobs(m_ols_3dim),
    r_nb_cox$n,      nobs(m_ols_noborder),
    nobs(m_ols_expanded)
  )
)

fwrite(rob_summary,
       file.path(OUTPUT_TABLES, "Table_Robustness_Summary.csv"))
log_step(sprintf("\nRobustness table written: %d specifications",
                 nrow(rob_summary)))
cat("\n")


#### S18: Diagnostic Export ####

cat("========================================\n")
cat("S18: DIAGNOSTIC EXPORT\n")
cat("========================================\n\n")

# Save primary model objects
saveRDS(m_cox_primary,      file.path(ANALYSIS_DIR, "tank_cox_primary.rds"))
saveRDS(m_cox_age,          file.path(ANALYSIS_DIR, "tank_cox_age_axis.rds"))
saveRDS(m_ols,              file.path(ANALYSIS_DIR, "tank_ols_reference.rds"))
saveRDS(m_cloglog,          file.path(ANALYSIS_DIR, "tank_cloglog_reference.rds"))
saveRDS(m_hte_wall_cox,     file.path(ANALYSIS_DIR, "tank_cox_hte_wall.rds"))
saveRDS(m_hte_cohort_cox,   file.path(ANALYSIS_DIR, "tank_cox_hte_cohort.rds"))

# Cross-spec coefficient summary (used in paper result text and tables)
cross_spec_summary <- data.table(
  Model     = c("Cox primary (calendar)",
                "Cox primary (age axis)",
                "Cloglog FE reference",
                "OLS LPM reference",
                "Cox 3-dim (robustness)",
                "Cox no-border (robustness)"),
  Estimand  = c(
    "Log-hazard ratio (calendar time)",
    "Log-hazard ratio (tank age)",
    "Log-hazard ratio (conditional on survival to t)",
    "ATT on annual closure probability",
    "Log-hazard ratio (no capacity dim)",
    "Log-hazard ratio (excl. border states)"),
  Log_coef  = round(c(r1$coef, r2$coef, r3$coef, r4$coef,
                       r_3dim_cox$coef, r_nb_cox$coef), 5),
  HR        = round(c(r1$hr,   r2$hr,   NA, NA,
                       r_3dim_cox$hr, r_nb_cox$hr), 4),
  SE        = round(c(r1$se,   r2$se,   r3$se, r4$se,
                       r_3dim_cox$se, r_nb_cox$se), 5),
  p_value   = round(c(r1$p,    r2$p,    r3$p,  r4$p,
                       r_3dim_cox$p, r_nb_cox$p),   4),
  N_obs     = c(r1$n, r2$n, r3$n, r4$n,
                r_3dim_cox$n, r_nb_cox$n),
  N_events  = c(r1$ev, r2$ev,
                sum(panel_dt$closure_event),
                sum(panel_dt$closure_event),
                r_3dim_cox$ev, r_nb_cox$ev)
)

fwrite(cross_spec_summary,
       file.path(OUTPUT_TABLES, "Table_CrossSpec_Summary.csv"))

# Sample size summary for paper footnotes
sample_summary <- data.table(
  Level    = c("Master tanks loaded",
               "Study states (non-NA treatment)",
               "Tank-year panel (full)",
               "Primary sample (mm_tank_primary) tank-years",
               "Primary sample unique tanks",
               "  of which: Texas",
               "  of which: Control states",
               "Exact-date Cox (post-survSplit)",
               "  of which: events (closures)",
               "Make-model cells (primary sample)",
               "Identified cell-years (TX+CTL both present)"),
  N = c(
    nrow(master_tanks),
    nrow(study_tanks),
    nrow(tank_year_panel),
    n_ty,
    n_tanks,
    n_tx_tanks,
    n_ct_tanks,
    nrow(exact_split_df),
    sum(exact_split_df$failure),
    n_cells,
    n_cells_id
  )
)
fwrite(sample_summary,
       file.path(OUTPUT_TABLES, "Table_SampleSizes.csv"))

log_step("Saved model objects and summary tables.")
log_step(sprintf("Output tables: %s", OUTPUT_TABLES))
log_step(sprintf("Output figures: %s", OUTPUT_FIGURES))
log_step(sprintf("Model RDS objects: %s", ANALYSIS_DIR))

cat("\n")
cat("========================================\n")
cat("02b_DiD_Survival.R COMPLETE\n")
cat("========================================\n")

# Print final cross-spec summary to console
cat("\nCross-specification summary:\n")
print(cross_spec_summary[, .(Model, Log_coef, HR, SE, p_value, N_obs)])