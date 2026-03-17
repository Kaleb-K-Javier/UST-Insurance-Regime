################################################################################
# UST_DiD_Analysis.R
# Texas UST Insurance Reform — Streamlined Difference-in-Differences Analysis
#
# CONTENTS
#   S1   Setup & Constants
#   S2   Helper Functions (theme, save, LaTeX utilities)
#   S3   Load Data & Build Annual Tank-Year Panel
#   S4   Primary Analysis Sample
#   S5   CEM Matching (tank-level)
#   S6   Cox Exact-Date Dataset
#   S7   Control Group Descriptives
#   S8   OLS Models (pooled + HTE)
#   S9   Cox Models (pooled + HTE)
#   S10  Event Studies — 5 specifications
#   S11  Raw Closure-Rate Figures
#   S12  HonestDiD Sensitivity (anticipation specification)
#   S13  Anticipation OLS Robustness Table
################################################################################


################################################################################
# S1: SETUP & CONSTANTS
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(MatchIt)
  library(HonestDiD)
  library(ggplot2)
  library(broom)
  library(scales)
  library(modelsummary)
  library(tibble)
  library(here)
})

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

# ── Directories ──
OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# ── Study parameters ──
POST_YEAR       <- 1999L
REFORM_DATE     <- as.IDate("1998-12-22")
REFORM_DAYS     <- as.numeric(as.Date("1998-12-22"))
COX_SPLIT_DATE  <- as.numeric(as.Date("1999-01-01"))
STUDY_END       <- as.IDate("2020-12-31")
MANDATE_CUTOFF  <- as.IDate("1985-01-01")

ALL_YEARS    <- (1985:1997)
POST_MANDATE <- 1989:1997

# Event-study window (relative to 1999)
ES_LOWER <- -8L
ES_UPPER <- 15L

# ── Study states ──
# PA excluded: Act 16 (June 1995) drives anomalous 1995 closure spike.
CONTROL_STATES <- c("AR","CO","ID","KS","KY","LA","MA","MD","ME",
                    "MN","MO","NC","OH","OK","SD","TN","VA")
STUDY_STATES   <- c("TX", CONTROL_STATES)

# ── Color palette ──
COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"
COL_PRE  <- "#E69F00"
COL_BLUE <- "#2166ac"
COL_RED  <- "#d6604d"

GROUP_COLS <- c("Texas" = COL_TX, "Control States" = COL_CTRL)

# ── fixest table defaults ──
setFixest_etable(
  style.tex  = style.tex("aer"),
  digits     = 4,
  se.below   = TRUE,
  depvar     = FALSE
)
setFixest_etable(fixef.print = FALSE)


################################################################################
# S2: HELPER FUNCTIONS
################################################################################

# ── Utilities ──
fmt_n <- function(n) format(n, big.mark = ",", scientific = FALSE)

# ── Publication ggplot2 theme ──
theme_pub <- function(base_size = 11) {
  theme_classic(base_size = base_size, base_family = "Times") +
    theme(
      plot.title        = element_blank(),
      plot.subtitle     = element_blank(),
      plot.caption      = element_blank(),
      axis.line         = element_line(colour = "black", linewidth = 0.4),
      axis.ticks        = element_line(colour = "black", linewidth = 0.3),
      axis.text         = element_text(colour = "black"),
      panel.grid        = element_blank(),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.background = element_blank(),
      legend.text       = element_text(size = base_size - 1),
      plot.margin       = margin(8, 10, 8, 8)
    )
}
theme_set(theme_pub())

# ── Save ggplot as PDF + PNG ──
save_gg <- function(p, stem, width = 7, height = 5) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(stem, ".pdf")),
         plot = p, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(OUTPUT_FIGURES, paste0(stem, ".png")),
         plot = p, width = width, height = height, dpi = 300, bg = "white")
  message("  Saved: ", stem)
  invisible(p)
}

# ── LaTeX table helper for QMD loading ──────────────────────────────────────
# Usage in QMD chunk WITH tbl-cap: (Quarto owns the float wrapper)
#
#   ```{r}
#   #| results: asis
#   load_tex_table(here("Output","Tables","Tab_01_OLS_Stepwise.tex"))
#   ```
#
# This strips \begin{table}/\end{table}/\centering/\begingroup/\endgroup
# and wraps in \scalebox{0.75}{\begin{minipage}{1.39\linewidth}...}
# which is the only pattern that reliably works inside Quarto floats.
# NEVER use \resizebox or \adjustbox — they fail (\textwidth = 0 in floats).
# NEVER use latex_options = "hold_position" in kable_styling() here.
# ────────────────────────────────────────────────────────────────────────────
load_tex_table <- function(path, scale = 0.75, minipage_w = 1.39) {
  lines <- readLines(path)
  # Strip wrappers — note: NO ^ anchor on \endgroup because kableExtra
  # often emits \par\endgroup on one line
  lines <- lines[!grepl(
    paste0("^\\s*\\\\begin\\{table\\}|^\\s*\\\\end\\{table\\}",
           "|^\\s*\\\\centering|^\\s*\\\\begingroup|\\\\endgroup"),
    lines
  )]
  # No % in cat() strings — Quarto escapes them to \% causing arithmetic errors
  cat(sprintf("\\scalebox{%.2f}{\n", scale))
  cat(sprintf("\\begin{minipage}{%.2f\\linewidth}\n", minipage_w))
  cat("\\begingroup\n")
  cat(lines, sep = "\n")
  cat("\n\\endgroup\n")
  cat("\\end{minipage}\n")
  cat("}\n")
}


# ── Event study: tidy coefficients from fixest i() model ──
# Parses "rel_year_bin::VALUE:texas_treated" coefficient names.
# Returns data.table with columns: year, estimate, std.error, conf.low, conf.high
# Injects the omitted reference year as an explicit zero row.
es_tidy <- function(model, ref = -1L) {
  df <- as.data.table(tidy(model, conf.int = TRUE))
  df <- df[grepl("rel_year_bin::", term)]
  df[, year := as.integer(
    gsub(".*rel_year_bin::([-0-9]+):texas_treated.*", "\\1", term)
  )]
  ref_row <- data.table(
    term = "ref", estimate = 0, std.error = 0,
    conf.low = 0, conf.high = 0, year = as.integer(ref)
  )
  df <- rbindlist(
    list(df[, .(term, estimate, std.error, conf.low, conf.high, year)], ref_row),
    fill = TRUE
  )
  setorder(df, year)
  df
}

# ── Event study plot ──
# Deliberately NO geom_line through point estimates.
# No figure title.
es_plot <- function(model,
                    ref        = -1L,
                    point_col  = "grey25",
                    ylim       = c(-0.025, 0.025)) {

  df      <- es_tidy(model, ref = ref)
  est_row <- df[year != ref]
  ref_row <- df[year == ref]

  y_range <- diff(ylim)

  ggplot(df, aes(x = year, y = estimate)) +
    # Shaded pre-reform region
    annotate("rect",
             xmin = -Inf, xmax = -0.5, ymin = -Inf, ymax = Inf,
             fill = "grey93", alpha = 0.7) +
    # Reform onset line
    geom_vline(xintercept = -0.5, colour = "grey25",
               linewidth = 0.55, linetype = "solid") +
    # Zero line
    geom_hline(yintercept = 0, colour = "grey55",
               linewidth = 0.40, linetype = "dashed") +
    # Confidence intervals — estimated periods only
    geom_errorbar(
      data    = est_row,
      aes(ymin = conf.low, ymax = conf.high),
      colour  = point_col,
      width   = 0.28,
      linewidth = 0.45
    ) +
    # Point estimates
    geom_point(data = est_row, colour = point_col, size = 2.2, shape = 16) +
    # Reference point (open circle at zero)
    geom_point(data = ref_row, shape = 1, size = 2.5, colour = "grey45") +
    # Label the omitted year
    annotate("text",
             x         = ref,
             y         = ylim[1] + y_range * 0.05,
             label     = sprintf("(%d)\nomit", ref),
             size      = 2.6,
             colour    = "grey40",
             fontface  = "italic",
             lineheight = 0.90) +
    scale_x_continuous(breaks = sort(unique(df$year))) +
    scale_y_continuous(
      limits = ylim,
      labels = function(x) sprintf("%+.3f", x)
    ) +
    labs(
      x = "Years Relative to 1999",
      y = "Effect on Annual Closure Probability"
    ) +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
}


################################################################################
# S3: LOAD DATA & BUILD ANNUAL TANK-YEAR PANEL
################################################################################

cat("\n=== S3: LOAD DATA & BUILD PANEL ===\n\n")

master_tanks <- fread(
  here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv"),
  na.strings = c("", "NA", "N/A")
)

# Validate required columns
stopifnot(all(c("mm_wall","mm_fuel","mm_capacity",
                "mm_install_cohort","make_model_tank") %in% names(master_tanks)))

# Coerce dates
for (col in c("tank_installed_date", "tank_closed_date"))
  if (col %in% names(master_tanks))
    master_tanks[[col]] <- as.IDate(master_tanks[[col]])

# Treatment assignment
master_tanks[, texas_treated := fcase(
  state == "TX",              1L,
  state %in% CONTROL_STATES, 0L,
  default = NA_integer_
)]

# Unique IDs
if ("tank_id" %in% names(master_tanks)) {
  master_tanks[, tank_panel_id := paste(facility_id, state, tank_id, sep = "_")]
} else {
  master_tanks[, tank_panel_id := paste(facility_id, state, .I, sep = "_")]
  warning("No tank_id column — using row index for tank_panel_id.")
}
master_tanks[, panel_id := paste(facility_id, state, sep = "_")]

# Filter to valid tanks (treatment + install date)
study_tanks <- master_tanks[!is.na(texas_treated) & !is.na(tank_installed_date)]

# ── Expand to annual tank-year panel (1985–2020) ──
study_tanks[, `:=`(
  install_yr   = year(tank_installed_date),
  close_yr_raw = fifelse(!is.na(tank_closed_date), year(tank_closed_date), 2020L)
)]
study_tanks[, `:=`(
  expand_start = pmax(install_yr, 1985L),
  expand_end   = pmin(close_yr_raw, 2020L)
)]

keep_cols <- c(
  "tank_panel_id","panel_id","facility_id","state","texas_treated",
  "mm_wall","mm_fuel","mm_capacity","mm_install_cohort","make_model_tank",
  "tank_installed_date","tank_closed_date","expand_start","expand_end"
)

tank_year_panel <- study_tanks[
  expand_start <= expand_end, .SD, .SDcols = keep_cols
][, {
  yrs <- seq(expand_start, expand_end)
  .(panel_year    = yrs,
    closure_event = as.integer(
      !is.na(tank_closed_date) & year(tank_closed_date) == yrs
    ))
}, by = keep_cols]

# Time variables
tank_year_panel[, `:=`(
  did_term  = texas_treated * as.integer(panel_year >= POST_YEAR),
  rel_year  = panel_year - POST_YEAR,
  tstart    = panel_year - 1L,
  tstop     = panel_year
)]

# Cohort flags
tank_year_panel[, `:=`(
  is_post_mandate = as.integer(mm_install_cohort %in% POST_MANDATE),
  mandate_active  = as.integer(
    panel_year %in% 1989:1993 & mm_install_cohort %in% as.character(1985:1989)
  )
)]

cat(sprintf("Panel: %s rows | %s tanks | %s facilities\n",
    fmt_n(nrow(tank_year_panel)),
    fmt_n(uniqueN(tank_year_panel$tank_panel_id)),
    fmt_n(uniqueN(tank_year_panel$panel_id))))


################################################################################
# S4: PRIMARY ANALYSIS SAMPLE
################################################################################

cat("\n=== S4: PRIMARY ANALYSIS SAMPLE ===\n\n")

# ── Full primary sample (mm cells, all cohorts 1985–1997) ──
mm_tank_primary <- tank_year_panel[
  !is.na(make_model_tank)               &
  mm_install_cohort %in% ALL_YEARS      &
  tank_installed_date >= MANDATE_CUTOFF &
  tstop > tstart                        &
  !is.na(did_term)                      &
  state %in% STUDY_STATES
]

# ── Analysis sample: SW-exposed facilities, post-mandate cohorts ──
# Focus on 1992–1998 cohorts at facilities with at least one SW tank
sw_exposed_facs <- mm_tank_primary[
  mm_wall == "Single-Walled" & mm_install_cohort %in% as.character(1992:1998),
  unique(panel_id)
]

analysis_tanks <- mm_tank_primary[
  panel_id %in% sw_exposed_facs                &
  mm_install_cohort %in% 1989:1997 # cap at 1997: 1998-cohort tanks
                                                   # would be post-only after first-year
                                                   # churn removal and cannot contribute
                                                   # to the pre-reform baseline
]

# Drop first-year churn (installation-year observations)
# These show high closure rates driven by failed inspections / paperwork corrections,
# not genuine economic closure decisions.
analysis_tanks <- analysis_tanks[panel_year > as.integer(mm_install_cohort)]

# Require at least one pre-reform observation per tank.
# Ensures every tank's FE is identified from pre-period variation so the
# within-tank DiD estimator has a genuine pre-treatment counterfactual.
# This also catches any edge cases where a late-installed 1997-cohort tank
# ends up with no 1998 observation for other reasons.
pre_obs        <- analysis_tanks[panel_year < 1999L, unique(tank_panel_id)]
n_dropped      <- uniqueN(analysis_tanks$tank_panel_id) - length(pre_obs)
analysis_tanks <- analysis_tanks[tank_panel_id %in% pre_obs]
if (n_dropped > 0)
  warning(sprintf("%d tank(s) dropped: no pre-reform observations.", n_dropped))

# Treatment and age variables
analysis_tanks[, `:=`(
  did_term         = texas_treated * as.integer(panel_year >= 1999L),
  rel_year         = panel_year - 1999L,
  age_at_treatment = 1999L - as.integer(mm_install_cohort),
  make_model_noage = paste(mm_wall, mm_fuel, mm_capacity, sep = "_")
)]

# Bin rel_year to event-study window
analysis_tanks[, rel_year_bin := pmax(pmin(rel_year, ES_UPPER), ES_LOWER)]

cat(sprintf("Analysis sample: %s tank-years | TX=%s CTL=%s facilities\n",
    fmt_n(nrow(analysis_tanks)),
    fmt_n(analysis_tanks[texas_treated == 1, uniqueN(panel_id)]),
    fmt_n(analysis_tanks[texas_treated == 0, uniqueN(panel_id)])))


################################################################################
# S5: CEM MATCHING (TANK LEVEL)
################################################################################

cat("\n=== S5: CEM MATCHING ===\n\n")

# One row per tank for matching
tank_chars <- unique(analysis_tanks[, .(
  tank_panel_id, panel_id, state, texas_treated,
  mm_wall, mm_fuel, mm_capacity, mm_install_cohort, age_at_treatment
)])

# Coarsen exact matching on pricing/market dimensions
# Match on: wall type, fuel type, capacity category, install-year cohort
m_cem <- matchit(
  texas_treated ~ mm_wall + mm_fuel + mm_capacity + mm_install_cohort,
  data   = as.data.frame(tank_chars),
  method = "cem"
)
cat("CEM balance summary:\n")
print(summary(m_cem, un = FALSE))

# Merge weights back
tank_chars[, cem_weight := m_cem$weights]
matched_ids <- tank_chars[cem_weight > 0, tank_panel_id]

matched_tanks <- merge(
  analysis_tanks[tank_panel_id %in% matched_ids],
  tank_chars[, .(tank_panel_id, cem_weight)],
  by = "tank_panel_id"
)

# ── Age variables (frozen at reform date) ──
med_age <- median(matched_tanks$age_at_treatment, na.rm = TRUE)

matched_tanks[, `:=`(
  above_median_age = as.integer(age_at_treatment >= med_age),
  single_wall      = as.integer(mm_wall == "Single-Walled")
)]
matched_tanks[, `:=`(
  did_x_old = did_term * above_median_age,
  did_x_sw  = did_term * single_wall
)]

cat(sprintf("\nMatched sample: TX=%s  CTL=%s tanks | Median age at reform = %.0f yr\n",
    fmt_n(matched_tanks[texas_treated == 1, uniqueN(tank_panel_id)]),
    fmt_n(matched_tanks[texas_treated == 0, uniqueN(tank_panel_id)]),
    med_age))


################################################################################
# S6: COX EXACT-DATE DATASET
################################################################################

cat("\n=== S6: COX DATASET ===\n\n")

exact_base <- master_tanks[
  !is.na(texas_treated) & !is.na(make_model_tank)   &
  mm_install_cohort %in% ALL_YEARS                   &
  !is.na(tank_installed_date)                        &
  tank_installed_date >= MANDATE_CUTOFF              &
  tank_installed_date < STUDY_END                    &
  state %in% STUDY_STATES ,
  .(tank_panel_id, panel_id, state, texas_treated,
    mm_wall, mm_fuel, mm_capacity, mm_install_cohort, make_model_tank,
    tank_installed_date,
    t_enter = as.numeric(tank_installed_date),
    t_exit  = as.numeric(pmin(
      fifelse(!is.na(tank_closed_date), tank_closed_date, STUDY_END),
      STUDY_END
    )),
    failure = as.integer(
      !is.na(tank_closed_date)                   &
      tank_closed_date <= STUDY_END              &
      tank_closed_date >= tank_installed_date
    ))
][t_exit > t_enter]

# Restrict to matched tank IDs
cox_sample <- exact_base[tank_panel_id %in% matched_ids]

cox_sample[, `:=`(
  age_at_treatment = 1999L - as.integer(mm_install_cohort),
  make_model_noage = paste(mm_wall, mm_fuel, mm_capacity, sep = "_")
)]
cox_sample[, above_median_age := as.integer(age_at_treatment >= med_age)]
nrow(cox_sample)
# Drop first-year churn
cox_sample[, `:=`(
  install_yr = as.integer(mm_install_cohort),
  exit_yr    = as.integer(format(as.Date(t_exit, origin = "1970-01-01"), "%Y"))
)]
cox_sample <- cox_sample[!(exit_yr == install_yr & failure == 1L)]
nrow(cox_sample) 
# survSplit at Jan 1, 1999 (treatment onset)
cox_split_df <- survSplit(
  Surv(t_enter, t_exit, failure) ~ .,
  data    = as.data.frame(cox_sample),
  cut     = COX_SPLIT_DATE,
  episode = "reform_ep"
)
setDT(cox_split_df)
cox_split_df <- cox_split_df[t_exit > t_enter]
cox_split_df[, did_term := texas_treated * as.integer(reform_ep == 2L)]

cat(sprintf("Cox dataset: %s rows | %s tanks | %s events\n",
    fmt_n(nrow(cox_split_df)),
    fmt_n(uniqueN(cox_split_df$tank_panel_id)),
    fmt_n(sum(cox_split_df$failure))))


################################################################################
# S7: CONTROL GROUP DESCRIPTIVES
################################################################################

cat("\n=== S7: CONTROL GROUP DESCRIPTIVES ===\n\n")

ctrl_means <- matched_tanks[texas_treated == 0, .(
  pre_1992_1997  = mean(closure_event[panel_year %between% c(1992L, 1997L)], na.rm = TRUE),
  post_all       = mean(closure_event[panel_year >= 1999L], na.rm = TRUE),
  post_short     = mean(closure_event[panel_year %between% c(1999L, 2004L)], na.rm = TRUE),
  post_long      = mean(closure_event[panel_year >= 2005L], na.rm = TRUE)
)]

cat("Control closure rates (per 1,000 tank-years):\n")
cat(sprintf("  Pre-reform (1992-1997) : %.1f\n", ctrl_means$pre_1992_1997 * 1e3))
cat(sprintf("  Post-reform (all)      : %.1f\n", ctrl_means$post_all * 1e3))
cat(sprintf("  Post short (1999-2004) : %.1f\n", ctrl_means$post_short * 1e3))
cat(sprintf("  Post long  (2005-2020) : %.1f\n", ctrl_means$post_long * 1e3))

ctrl_mean_post <- sprintf("%.4f", ctrl_means$post_all)


################################################################################
# S8: OLS MODELS — POOLED AND HTE
################################################################################

cat("\n=== S8: OLS MODELS ===\n\n")

# M1  Pooled + year FE
# M2  Pooled + cell x year FE
# M3  DiD x Old interaction + year FE
# M4  DiD x Old interaction + cell x year FE
# M5  Below-median age subsample
# M6  Above-median age subsample
# NOTE: level effect of age_at_treatment is time-invariant → absorbed by
#       tank FE; cannot appear as a main effect. HTE via interaction (M3/M4)
#       and subsample split (M5/M6).

nrow(matched_tanks)
nrow(matched_tanks[state != 'MO'])
nrow(matched_tanks[panel_year > mm_install_cohort & state != 'MO'])
nrow(matched_tanks[panel_year > mm_install_cohort])
ols_m1 <- feols(
  closure_event ~ did_term | tank_panel_id + panel_year,
  data = matched_tanks[panel_year > mm_install_cohort & state != 'MO'], weights = ~cem_weight, cluster = ~state
)
ols_m2 <- feols(
  closure_event ~ did_term | tank_panel_id + panel_year^make_model_noage,
  data = matched_tanks[panel_year > mm_install_cohort & state != 'MO'], weights = ~cem_weight, cluster = ~state
)
ols_m3 <- feols(
  closure_event ~ did_term + did_x_old | tank_panel_id + panel_year,
  data = matched_tanks[panel_year > mm_install_cohort & state != 'MO'], weights = ~cem_weight, cluster = ~state
)
ols_m4 <- feols(
  closure_event ~ did_term + did_x_old | tank_panel_id + panel_year^make_model_noage,
  data = matched_tanks[panel_year > mm_install_cohort & state != 'MO'], weights = ~cem_weight, cluster = ~state
)
ols_m5 <- feols(
  closure_event ~ did_term | tank_panel_id + panel_year,
  data    = matched_tanks[panel_year > mm_install_cohort & state != 'MO' & above_median_age == 0L],
  weights = ~cem_weight, cluster = ~state
)
ols_m6 <- feols(
  closure_event ~ did_term | tank_panel_id + panel_year,
  data    = matched_tanks[panel_year > mm_install_cohort & state != 'MO' & above_median_age == 1L],
  weights = ~cem_weight, cluster = ~state
)

ols_dict <- c(
  "did_term"  = "DiD",
  "did_x_old" = paste0("DiD $\\times$ Old ($\\geq$", med_age, " yr)")
)

etable(
  ols_m1, ols_m2, ols_m3, ols_m4, ols_m5, ols_m6,
  headers    = paste0("(", 1:6, ")"),
  dict       = ols_dict,
  fitstat    = ~ n + r2,
  keep       = c("^DiD$", "DiD.*Old"),
  extralines = list(
    "Tank FE"                    = rep("\\checkmark", 6),
    "Year FE"                    = rep("\\checkmark", 6),
    "Cell $\\times$ Year FE"     = c("", "\\checkmark", "", "\\checkmark", "", ""),
    "Age Subsample"              = c("All", "All", "All", "All",
                                     paste0("$<$", med_age, " yr"),
                                     paste0("$\\geq$", med_age, " yr")),
    "Control Mean (post-reform)" = rep(ctrl_mean_post, 6)
  ),
  file    = file.path(OUTPUT_TABLES, "Tab_01_OLS_Stepwise.tex"),
  replace = TRUE,
  notes   = paste0(
    "Cluster-robust standard errors by state in parentheses. ",
    "Old $=$ above median age at reform ($\\geq$", med_age, " yr). ",
    "Level effect of tank age absorbed by tank fixed effect. ",
    "CEM weights applied throughout."
  )
)
cat("  Saved: Tab_01_OLS_Stepwise.tex\n")


################################################################################
# S9: COX MODELS — POOLED AND HTE
################################################################################

cat("\n=== S9: COX MODELS ===\n\n")

# C1  Pooled, strata(make_model_noage)
# C2  + DiD x above_median_age interaction
# C3  Below-median age subsample
# C4  Above-median age subsample
# Strata on make_model_noage (not make_model_tank) so age variation is NOT
# absorbed into the baseline hazard — allows identification of age HTE.

cox_m1 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    strata(make_model_noage),
  data = cox_split_df[ state != 'MO'], cluster = state
)
cox_m2 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term + did_term:above_median_age +
    strata(make_model_noage),
  data = cox_split_df[state != 'MO'], cluster = state
)
cox_m3 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    strata(make_model_noage),
  data = cox_split_df[ state != 'MO' & above_median_age == 0L], cluster = state
)
cox_m4 <- coxph(
  Surv(t_enter, t_exit, failure) ~ did_term +
    strata(make_model_noage),
  data = cox_split_df[state != 'MO' & above_median_age == 1L], cluster = state
)

cox_coef_map <- c(
  "did_term"                  = "DiD",
  "did_term:above_median_age" = paste0("DiD $\\times$ Old ($\\geq$", med_age, " yr)")
)

cox_add_rows <- tribble(
  ~term,                          ~`(1)`,         ~`(2)`,        ~`(3)`,                        ~`(4)`,
  "Strata (make-model, no age)", "\\checkmark", "\\checkmark", "\\checkmark",                "\\checkmark",
  "Age Subsample",               "All",          "All",
     paste0("$<$", med_age, " yr"),  paste0("$\\geq$", med_age, " yr")
)

msummary(
  list("(1)" = cox_m1, "(2)" = cox_m2, "(3)" = cox_m3, "(4)" = cox_m4),
  coef_map     = cox_coef_map,
  exponentiate = TRUE,
  statistic    = "({std.error})",
  stars        = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  escape       = FALSE,
  gof_map      = list(
    list(raw = "nobs",   clean = "Observations", fmt = scales::comma),
    list(raw = "nevent", clean = "Events",       fmt = scales::comma)
  ),
  add_rows = cox_add_rows,
  output   = file.path(OUTPUT_TABLES, "Tab_02_Cox_Stepwise.tex"),
  notes    = paste0(
    "Cluster-robust standard errors by state in parentheses. ",
    "Hazard ratios (exp[$\\hat{\\beta}$]) reported. ",
    "Spells split at January 1, 1999 so DiD is a proper time-varying covariate. ",
    "Strata defined on make-model excluding age cohort (\\texttt{make\\_model\\_noage}) ",
    "so age variation is not absorbed into the baseline hazard. ",
    "Old $=$ above median age at reform ($\\geq$", med_age, " yr)."
  )
)
cat("  Saved: Tab_02_Cox_Stepwise.tex\n")


################################################################################
# S10: EVENT STUDIES — 5 SPECIFICATIONS
################################################################################

cat("\n=== S10: EVENT STUDIES ===\n\n")
ES_LOWER <- -5
# ── Shared event-study window ──
# rel_year already on matched_tanks; clip to [ES_LOWER, ES_UPPER]
matched_tanks[, rel_year_bin := pmax(pmin(rel_year, ES_UPPER), ES_LOWER)]
matched_tanks[, rel_year_bin := pmax(pmin(rel_year, ES_UPPER), ES_LOWER)]

# ────────────────────────────────────────────────────────────────────────────
# ES-1  Pooled matched sample (ref = -1, i.e. year 1998 omitted)
# ES-2  Pooled + cell × year FE  (ref = -1)
# ES-3  Young: below-median age  (ref = -1)
# ES-4  Old: above-median age    (ref = -1)
# ES-5  Anticipation spec         (ref = -2, so year 1998 τ = -1 is estimated)
#        Reference: 1997 (two years pre-reform)
#        Anticipation period: 1998 explicitly estimated as β_{-1}
#        Reform onset: 1999 onward
# ────────────────────────────────────────────────────────────────────────────

es_m1 <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1L) |
    tank_panel_id + panel_year,
  data = matched_tanks[!state %in% c("ME", "MN", "MO")], weights = ~cem_weight, cluster = ~state
)

es_m2 <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1L) |
    tank_panel_id + panel_year^make_model_tank,
  data =matched_tanks[!state %in% c("ME", "MN", "MO")], weights = ~cem_weight, cluster = ~state
)

es_m3 <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1L) |
    tank_panel_id + panel_year,
  data    = matched_tanks[above_median_age == 0L],
  weights = ~cem_weight, cluster = ~state
)

es_m4 <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1L) |
    tank_panel_id + panel_year,
  data    = matched_tanks[above_median_age == 1L],
  weights = ~cem_weight, cluster = ~state
)

# Anticipation spec: ref = -2, so year 1997 is the omitted baseline and
# 1998 (τ = -1) is explicitly estimated as the anticipation coefficient.

es_m5_antic <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -2L)  |
    tank_panel_id +make_model_tank^panel_year,
  data = matched_tanks[state != "MO"], weights = ~cem_weight, cluster = ~state
)


# ── Print coefficient tables ──
cat("ES-1 (Pooled, ref = -1):\n"); etable(es_m1)
cat("\nES-5 (Anticipation, ref = -2):\n"); etable(es_m5_antic)

# ── y-axis limits ──
ES_YLIM       <- c(-0.025, 0.025)
ES_YLIM_TIGHT <- c(-0.015, 0.015)

# ── Build and save figures ──
p_es1 <- es_plot(es_m1,       ref = -1L, point_col = "grey25",  ylim = ES_YLIM)
p_es2 <- es_plot(es_m2,       ref = -1L, point_col = "grey25",  ylim = ES_YLIM)
p_es3 <- es_plot(es_m3,       ref = -1L, point_col = COL_BLUE,  ylim = ES_YLIM)
p_es4 <- es_plot(es_m4,       ref = -1L, point_col = COL_RED,   ylim = ES_YLIM)
p_es5 <- es_plot(es_m5_antic, ref = -2L, point_col = "grey25",  ylim = ES_YLIM)

save_gg(p_es1, "Fig_01_EventStudy_Pooled_Matched",         width = 7, height = 5)
save_gg(p_es2, "Fig_02_EventStudy_Pooled_CellYearFE",      width = 7, height = 5)
save_gg(p_es3, "Fig_03_EventStudy_Young_BelowMedianAge",   width = 7, height = 5)
save_gg(p_es4, "Fig_04_EventStudy_Old_AboveMedianAge",     width = 7, height = 5)
save_gg(p_es5, "Fig_05_EventStudy_Anticipation_Spec",      width = 7, height = 5)

# Anticipation spec: ref = -2, so year 1997 is the omitted baseline and
# 1998 (τ = -1) is explicitly estimated as the anticipation coefficient.
es_m5_antic_old <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -2L) |
    tank_panel_id + make_model_tank^panel_year,
  data =  matched_tanks[above_median_age == 1L & state != "MO"], weights = ~cem_weight, cluster = ~state
)

p_es5_old <- es_plot(es_m5_antic_old, ref = -2L, point_col = "grey25",  ylim = ES_YLIM)
save_gg(p_es5_old, "Fig_05_EventStudy_Anticipation_Spec_AboveMedianAge",      width = 7, height = 5)
etable(es_m5_antic_old)
################################################################################
# S11: RAW CLOSURE-RATE FIGURES
################################################################################

cat("\n=== S11: CLOSURE-RATE FIGURES ===\n\n")

# ── Figure A: OLS matched sample (CEM-weighted) ──
ols_rates <- matched_tanks[, .(
  closure_rate = weighted.mean(closure_event, w = cem_weight, na.rm = TRUE)
), by = .(
  panel_year,
  group = fifelse(texas_treated == 1L, "Texas", "Control States")
)]

p_rates_ols <- ggplot(
  ols_rates, aes(x = panel_year, y = closure_rate, colour = group, group = group)
) +
  # Federal mandate window shading
  annotate("rect",
           xmin = 1989, xmax = 1993, ymin = -Inf, ymax = Inf,
           fill = COL_PRE, alpha = 0.09) +
  annotate("text",
           x = 1991,
           y = max(ols_rates$closure_rate) * 0.72,
           label = "Mandate\nWindow",
           hjust = 0.5, size = 2.6, colour = COL_PRE, fontface = "italic") +
  # Reform onset
  geom_vline(xintercept = 1999, linetype = "dashed",
             colour = "grey35", linewidth = 0.50) +
  annotate("text",
           x = 1999.3,
           y = max(ols_rates$closure_rate) * 0.97,
           label = "Reform\n(Jan 1999)",
           hjust = 0, size = 2.7, colour = "grey35") +
  geom_line(linewidth = 0.80) +
  geom_point(size = 1.9) +
  scale_colour_manual(values = GROUP_COLS,
                      guide  = guide_legend(direction = "horizontal")) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  labs(x = "Calendar Year", y = "Annual Tank Closure Rate", colour = NULL) +
  theme_pub() +
  theme(legend.position = c(0.85, 0.92))

# ── Figure B: Cox sample ──
cox_sample[, plot_year := as.integer(
  format(as.Date(t_exit, origin = "1970-01-01"), "%Y")
)]
cox_rates <- cox_sample[plot_year >= 1990L, .(
  closure_rate = mean(failure, na.rm = TRUE)
), by = .(
  plot_year,
  group = fifelse(texas_treated == 1L, "Texas", "Control States")
)]

p_rates_cox <- ggplot(
  cox_rates, aes(x = plot_year, y = closure_rate, colour = group, group = group)
) +
  geom_vline(xintercept = 1999, linetype = "dashed",
             colour = "grey35", linewidth = 0.50) +
  annotate("text",
           x = 1999.3,
           y = max(cox_rates$closure_rate) * 0.97,
           label = "Reform\n(Dec 1998)",
           hjust = 0, size = 2.7, colour = "grey35") +
  geom_line(linewidth = 0.80) +
  geom_point(size = 1.9) +
  scale_colour_manual(values = GROUP_COLS,
                      guide  = guide_legend(direction = "horizontal")) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  labs(x = "Calendar Year", y = "Annual Tank Closure Rate", colour = NULL) +
  theme_pub() +
  theme(legend.position = c(0.85, 0.92))

save_gg(p_rates_ols, "Fig_06_ClosureRates_OLS_Matched",  width = 7, height = 4.5)
save_gg(p_rates_cox, "Fig_07_ClosureRates_Cox_Sample",   width = 7, height = 4.5)



################################################################################
# S12: HONEST DiD SENSITIVITY ANALYSIS
#
# TWO SPECIFICATIONS:
#   A. Pooled matched sample  (es_m1, ref = -1)
#      Standard parallel trends pre-period: τ ∈ {-5, -4, -3}
#      Post-period estimand: average ATT over τ ∈ {0, ..., ES_UPPER-1}
#
#   B. Above-median age subsample  (es_m4, ref = -1)
#      The HTE tables show the treatment effect is concentrated in older tanks.
#      This is the primary economic result — test its robustness separately.
#      Same pre/post partition as spec A.
#
# RESTRICTION: Relative Magnitudes (DeltaRM, Rambachan & Roth 2023).
#   Post-period violation bounded by Mbar × max pre-period deviation.
#   Both binned endpoints (ES_LOWER, ES_UPPER) excluded so that diff(pre_coefs)
#   computes genuine one-period slopes and l_vec weights are well-scaled.
#
# NOTE: The anticipation event study (es_m5_antic) uses ref = -2 so its
#   pre-period is τ ∈ {-5, -4, -3} and post includes τ = -1. For specs A and B
#   (ref = -1), the pre-period is τ ∈ {-5, -4, -3} and post starts at τ = 0.
################################################################################
 
cat("\n=== S12: HONEST DiD ===\n\n")
 
# ── Shared helper: extract betahat/sigma and run DeltaRM sensitivity ──
run_honestdid <- function(model, label,
                           ref        = -1L,
                           Mbarvec    = seq(0, 2, by = 0.1)) {
 
  b <- coef(model)
  V <- vcov(model, type = "clustered")
   # Force PSD — eigen-decomposition, zero negative eigenvalues, reconstruct
  # Necessary when VCOV was flagged as non-PSD by fixest (common with <20 clusters
  # and high-dimensional FE). Without this HonestDiD's QP solver will fail.
  ev  <- eigen(V, symmetric = TRUE)
  ev$values <- pmax(ev$values, 0)
  V   <- ev$vectors %*% diag(ev$values) %*% t(ev$vectors)
  # Parse τ from coefficient names (works for both rel_year_bin and rel_year_early)
  nms   <- names(b)
  years <- as.integer(gsub(".*::([-0-9]+):texas_treated.*", "\\1", nms))
 
  # Exclude binned endpoints — not genuine single-year periods
  interior <- years != ES_LOWER & years != ES_UPPER
 
  # Pre  = τ < ref  ∩ interior   (strict pre-treatment window)
  # Post = τ > ref  ∩ interior   (post-treatment, exclude ref which is omitted)
  pre_mask  <- years <  ref & interior
  post_mask <- years >  ref & interior
 
  pre_ord  <- which(pre_mask)[order(years[pre_mask])]
  post_ord <- which(post_mask)[order(years[post_mask])]
  ord_all  <- c(pre_ord, post_ord)
 
  betahat <- b[ord_all]
  sigma   <- V[ord_all, ord_all]
  n_pre   <- length(pre_ord)
  n_post  <- length(post_ord)
 
  cat(sprintf("\n%s\n", label))
  cat(sprintf("  Pre-period  (n=%d): τ ∈ {%s}\n", n_pre,
      paste(sort(years[pre_mask]),  collapse = ", ")))
  cat(sprintf("  Post-period (n=%d): τ ∈ {%s}\n", n_post,
      paste(sort(years[post_mask]), collapse = ", ")))
  stopifnot(n_pre >= 2L, n_post >= 1L,
            length(betahat) == n_pre + n_post)
 
  # Equal weight across all interior post-period years
  l_vec <- rep(1 / n_post, n_post)
 
  sens <- createSensitivityResults_relativeMagnitudes(
    betahat        = betahat,
    sigma          = sigma,
    numPrePeriods  = n_pre,
    numPostPeriods = n_post,
    alpha          = 0.05,
    l_vec          = l_vec,
    Mbarvec        = Mbarvec
  )
 
  sens_dt <- as.data.table(sens)
  stopifnot("Mbar" %in% names(sens_dt))
 
  # Pre-trend diagnostics
  pre_coefs     <- betahat[seq_len(n_pre)]
  max_pre_slope <- max(abs(diff(pre_coefs)))
 
  breakpt <- sens_dt[ub >= 0 & lb <= 0, min(Mbar, na.rm = TRUE)]
  if (!is.finite(breakpt)) breakpt <- NA_real_
 
  cat(sprintf("  Max pre-trend slope : %.5f\n", max_pre_slope))
  cat(sprintf("  Breakdown Mbar      : %.5f\n", breakpt))
  if (!is.na(breakpt))
    cat(sprintf("  Breakdown / slope   : %.2fx\n", breakpt / max_pre_slope))
 
  list(sens_dt = sens_dt, max_pre_slope = max_pre_slope, breakpt = breakpt)
}
 
# ── Build HonestDiD figure ──
plot_honestdid <- function(res, point_col = COL_TX) {
  sens_dt       <- res$sens_dt
  max_pre_slope <- res$max_pre_slope
  breakpt       <- res$breakpt
 
  ggplot(sens_dt, aes(x = Mbar)) +
    geom_ribbon(aes(ymin = lb, ymax = ub),
                fill = point_col, alpha = 0.18) +
    geom_line(aes(y = lb), colour = point_col, linewidth = 0.85) +
    geom_line(aes(y = ub), colour = point_col, linewidth = 0.85) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey40", linewidth = 0.45) +
    # Max pre-trend slope benchmark
    geom_vline(xintercept = max_pre_slope, linetype = "longdash",
               colour = COL_BLUE, linewidth = 0.65) +
    annotate("text",
             x         = max_pre_slope * 1.05,
             y         = max(sens_dt$ub, na.rm = TRUE) * 0.93,
             label     = sprintf("Max observed\npre-trend slope\n= %.4f", max_pre_slope),
             hjust     = 0, vjust = 1,
             size      = 2.9, colour = COL_BLUE, lineheight = 1.1) +
    # Breakdown point
    { if (!is.na(breakpt))
        list(
          geom_vline(xintercept = breakpt, linetype = "dotted",
                     colour = "grey30", linewidth = 0.60),
          annotate("text",
                   x         = breakpt * 1.05,
                   y         = min(sens_dt$lb, na.rm = TRUE) * 0.55,
                   label     = sprintf("Breakdown\nMbar = %.4f", breakpt),
                   hjust     = 0, vjust = 0,
                   size      = 2.9, colour = "grey30", lineheight = 1.1)
        )
      else list()
    } +
    scale_x_continuous(
      name = expression(bar(M)*": Relative Magnitude of Parallel Trends Violation")
    ) +
    scale_y_continuous(
      name   = "Robust 95% CI — Average Post-Reform ATT (pp)",
      labels = function(x) sprintf("%.3f", x)
    ) +
    theme_pub()
}
 
# ────────────────────────────────────────────────────────────────────────────
# SPEC A: Pooled matched sample (es_m1, ref = -1)
# Post-period = τ ∈ {0, ..., ES_UPPER-1} — standard post-reform ATT
# ────────────────────────────────────────────────────────────────────────────
res_pooled <- run_honestdid(
  model  = es_m5_antic,
  label  = "SPEC A: Anticipation Spec (Pooled, ref = -2)",
  ref    = -2L,
  Mbarvec = seq(0, 2, by = 0.1)
)
 
p_hd_pooled <- plot_honestdid(res_pooled, point_col = "grey30")
save_gg(p_hd_pooled, "Fig_08a_HonestDiD_Pooled",
        width = 8, height = 5)
fwrite(res_pooled$sens_dt,
       file.path(OUTPUT_TABLES, "Tab_Appendix_HonestDiD_Pooled.csv"))
cat("  Saved: Fig_08a + Tab_Appendix_HonestDiD_Pooled.csv\n")
 
# ────────────────────────────────────────────────────────────────────────────
# SPEC B: Above-median age subsample (es_m4, ref = -1)
# This is the primary economic result — the OLS and Cox HTE tables show
# the treatment effect is concentrated in older tanks. Robustness of this
# subsample result is the main claim that needs defending.
# ────────────────────────────────────────────────────────────────────────────
res_old <- run_honestdid(
  model   = es_m4,
  label   = sprintf("SPEC B: Above-median age (>= %.0f yr)", med_age),
  ref     = -1L,
  Mbarvec = seq(0, 2, by = 0.1)
)
 
p_hd_old <- plot_honestdid(res_old, point_col = COL_RED)
save_gg(p_hd_old, "Fig_08b_HonestDiD_AboveMedianAge",
        width = 8, height = 5)
fwrite(res_old$sens_dt,
       file.path(OUTPUT_TABLES, "Tab_Appendix_HonestDiD_AboveMedianAge.csv"))
cat("  Saved: Fig_08b + Tab_Appendix_HonestDiD_AboveMedianAge.csv\n")

################################################################################
# S13: ANTICIPATION OLS ROBUSTNESS TABLE
#
# Compares two DiD definitions side-by-side:
#   did_term      : Post = 1999 onward  (standard)
#   did_term_1998 : Post = 1998 onward  (anticipation-inclusive)
# Each crossed with the age interaction to quantify HTE in both framings.
################################################################################

cat("\n=== S13: ANTICIPATION ROBUSTNESS TABLE ===\n\n")

matched_tanks[, did_term_1998 := texas_treated * as.integer(panel_year >= 1998L)]
matched_tanks[, did_1998_x_old := did_term_1998 * above_median_age]

# Cols 1-2: pooled, reform = 1999 vs 1998
ols_a1 <- feols(
  closure_event ~ did_term | tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state
)
ols_a2 <- feols(
  closure_event ~ did_term_1998 | tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state
)
# Cols 3-4: add age interaction
ols_a3 <- feols(
  closure_event ~ did_term + did_x_old | tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state
)
ols_a4 <- feols(
  closure_event ~ did_term_1998 + did_1998_x_old | tank_panel_id + panel_year,
  data = matched_tanks, weights = ~cem_weight, cluster = ~state
)

antic_dict <- c(
  "did_term"        = "DiD (Post $\\geq$ 1999)",
  "did_term_1998"   = "DiD (Post $\\geq$ 1998)",
  "did_x_old"       = paste0("DiD (1999) $\\times$ Old"),
  "did_1998_x_old"  = paste0("DiD (1998) $\\times$ Old")
)

etable(
  ols_a1, ols_a2, ols_a3, ols_a4,
  headers    = paste0("(", 1:4, ")"),
  dict       = antic_dict,
  depvar     = FALSE,
  fitstat    = ~ n + r2,
  extralines = list(
    "Reform Definition"          = c("1999", "1998", "1999", "1998"),
    "Age Interaction"            = c("No",   "No",   "Yes",  "Yes"),
    "Control Mean (post-reform)" = rep(ctrl_mean_post, 4)
  ),
  file    = file.path(OUTPUT_TABLES, "Tab_03_Anticipation_Robustness.tex"),
  replace = TRUE,
  notes   = paste0(
    "Cluster-robust standard errors by state in parentheses. ",
    "Tank and year fixed effects throughout. CEM weights applied. ",
    "Old $=$ above median age at reform ($\\geq$", med_age, " yr). ",
    "Columns (3) and (4) test whether anticipation effects differ by age."
  )
)
cat("  Saved: Tab_03_Anticipation_Robustness.tex\n")

################################################################################
# DIAGNOSTIC: Monthly Closure Rates — TX vs Control, Exact Dates
# Uses master_tanks directly for maximum granularity
################################################################################

# ── Pull matched tanks with exact closure dates ──
# Restrict to matched sample cohorts (1992-1997) in study states
diag_tanks <- master_tanks[
  !is.na(texas_treated)                          &
  !is.na(tank_closed_date)                       &
  mm_install_cohort %in% 1989:1997&
  state %in% STUDY_STATES                        &
  tank_closed_date >= as.IDate("1996-01-01")     &
  tank_closed_date <= as.IDate("2002-12-31")
]

# ── Denominator: tanks at risk each month ──
# For each month, count tanks installed before month start and not yet closed
months_seq <- seq(as.IDate("1996-01-01"), as.IDate("2002-12-31"), by = "month")

at_risk <- rbindlist(lapply(months_seq, function(m) {
  m_end <- as.IDate(format(m + 31, "%Y-%m-01")) - 1  # last day of month
  master_tanks[
    !is.na(texas_treated)                          &
    mm_install_cohort %in% as.character(1992:1997) &
    state %in% STUDY_STATES                        &
    tank_installed_date <= m                       &
    (is.na(tank_closed_date) | tank_closed_date >= m),
    .(month      = m,
      n_at_risk  = .N),
    by = .(group = fifelse(texas_treated == 1L, "Texas", "Control States"))
  ]
}))

# ── Closures per month ──
diag_tanks[, month := as.IDate(format(tank_closed_date, "%Y-%m-01"))]

monthly_closures <- diag_tanks[, .(
  n_closed = .N
), by = .(
  month,
  group = fifelse(texas_treated == 1L, "Texas", "Control States")
)]

# ── Merge and compute rate ──
monthly_rates <- merge(monthly_closures, at_risk, by = c("month", "group"), all.y = TRUE)
monthly_rates[is.na(n_closed), n_closed := 0L]
monthly_rates[, closure_rate := n_closed / n_at_risk]
monthly_rates[, month_num := as.numeric(month)]

# ── Key dates ──
fed_deadline  <- as.IDate("1998-12-22")   # federal mandate + HB 3169 effective
reform_jan99  <- as.IDate("1999-01-01")   # POST_YEAR start in panel

# ── Figure: monthly closure rate ──
p_monthly <- ggplot(
  monthly_rates,
  aes(x = as.Date(month), y = closure_rate * 1000,
      colour = group, group = group)
) +
  # Shade the implementation month
  annotate("rect",
           xmin = as.Date("1998-12-01"), xmax = as.Date("1999-01-31"),
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.5) +
  annotate("text",
           x = as.Date("1999-01-10"),
           y = -Inf,
           label = "Dec 22, 1998\n(HB 3169 + Fed. deadline)",
           vjust = -0.3, hjust = 0.5,
           size = 2.6, colour = "grey30", lineheight = 1.0) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = GROUP_COLS,
                      guide  = guide_legend(direction = "horizontal")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(name = "Monthly Closures per 1,000 At-Risk Tanks") +
  labs(x = NULL, colour = NULL) +
  theme_pub() +
  theme(legend.position = c(0.15, 0.92))

# ── Figure: raw closure counts (easier to see spike magnitude) ──
p_monthly_counts <- ggplot(
  monthly_rates,
  aes(x = as.Date(month), y = n_closed,
      fill = group)
) +
  annotate("rect",
           xmin = as.Date("1998-12-01"), xmax = as.Date("1999-01-31"),
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.5) +
  geom_col(position = position_dodge(width = 25), width = 22, alpha = 0.85) +
  scale_fill_manual(values = GROUP_COLS,
                    guide  = guide_legend(direction = "horizontal")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(name = "Raw Monthly Closure Count") +
  labs(x = NULL, fill = NULL) +
  theme_pub() +
  theme(legend.position = c(0.15, 0.92))

# ── Cohort x month heatmap — TX only ──
# Shows whether Dec 1998 spike is uniform across cohorts or cohort-specific
tx_cohort_monthly <- diag_tanks[texas_treated == 1L, .(
  n_closed = .N
), by = .(month, mm_install_cohort)]

p_heatmap_tx <- ggplot(
  tx_cohort_monthly,
  aes(x = as.Date(month), y = factor(mm_install_cohort), fill = n_closed)
) +
  annotate("rect",
           xmin = as.Date("1998-12-01"), xmax = as.Date("1999-01-31"),
           ymin = -Inf, ymax = Inf,
           fill = "grey60", alpha = 0.25) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(n_closed > 0, n_closed, "")),
            size = 2.2, colour = "grey20") +
  scale_fill_gradient(low = "white", high = COL_TX,
                      name = "Closures", na.value = "grey95") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(x = NULL, y = "Install Cohort") +
  theme_pub() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

save_gg(p_monthly,        "Diag_Monthly_ClosureRate_TXvCTL",    width = 9, height = 4.5)
save_gg(p_monthly_counts, "Diag_Monthly_ClosureCount_TXvCTL",   width = 9, height = 4.5)
save_gg(p_heatmap_tx,     "Diag_Monthly_CohortHeatmap_TX",      width = 9, height = 5)

# ── Print the Dec 1998 spike in numbers ──
cat("\nDecember 1998 closures vs. surrounding months:\n")
print(monthly_rates[
  month %between% c(as.IDate("1998-06-01"), as.IDate("1999-06-01"))
][order(month, group)])

# Monthly closure rate by cohort group x TX/CTL
# Split into: early cohorts (1990-1991) vs analysis cohorts (1992-1997)
diag_tanks[, cohort_group := fcase(
  mm_install_cohort %in% as.character(1990:1991), "1990-1991 (pre-mandate)",
  mm_install_cohort %in% as.character(1992:1997), "1992-1997 (analysis)",
  default = "Other"
)]

cohort_monthly_rates <- merge(
  diag_tanks[cohort_group != "Other", .(
    n_closed = .N
  ), by = .(month, cohort_group,
            group = fifelse(texas_treated == 1L, "Texas", "Control States"))],
  at_risk[, .(month, group, n_at_risk)],
  by = c("month", "group")
)[, closure_rate := n_closed / n_at_risk]

ggplot(cohort_monthly_rates,
  aes(x = as.Date(month), y = closure_rate * 1000,
      colour = group, group = group)) +
  geom_line() + geom_point(size = 1.2) +
  facet_wrap(~cohort_group, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = as.Date("1998-12-22"),
             linetype = "dashed", colour = "grey35") +
  scale_colour_manual(values = GROUP_COLS) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(x = NULL, y = "Monthly Closures per 1,000 At-Risk Tanks") +
  theme_pub() +
  theme(legend.position = "bottom")


################################################################################
# Diag_ClosureRates_CohortState.R
# Granular closure rate diagnostics to identify source of pre-period
# level difference between TX and control states in analysis cohorts.
#
# FIGURE 1: Annual closure rate by individual install cohort × TX/Control
# FIGURE 2: Annual closure rate by individual state (one panel per state)
# TABLE 1:  Pre-period mean closure rate by cohort × group
# TABLE 2:  Pre-period mean closure rate by state × period
################################################################################

# ── Assumes these objects exist from main script ──
# master_tanks, STUDY_STATES, CONTROL_STATES, GROUP_COLS,
# OUTPUT_FIGURES, OUTPUT_TABLES, theme_pub, save_gg, COL_TX, COL_CTRL

ANALYSIS_COHORTS <- 1989:1997
PLOT_START       <- as.IDate("1990-01-01")
PLOT_END         <- as.IDate("2006-12-31")
REFORM_YR        <- 1999L

# ── Base sample: analysis cohorts in study states with exact dates ──
diag_base <- master_tanks[
  state %in% STUDY_STATES                        &
  mm_install_cohort %in% ANALYSIS_COHORTS        &
  !is.na(tank_installed_date)
]

diag_base[, group := fifelse(texas_treated == 1L, "Texas", "Control States")]

################################################################################
# FIGURE 1: Annual closure rate by cohort × group
# One panel per install cohort, TX vs Control on same axes
################################################################################

cat("Building Figure 1: Annual closure rates by cohort...\n")

# Annual closures
diag_base[, close_yr := fifelse(
  !is.na(tank_closed_date) & tank_closed_date <= PLOT_END,
  year(tank_closed_date), NA_integer_
)]

# Tanks at risk each year — installed before year start, not yet closed
years_seq <- 1990:2006

annual_at_risk <- rbindlist(lapply(years_seq, function(yr) {
  diag_base[
    year(tank_installed_date) <= yr &
    (is.na(tank_closed_date) | year(tank_closed_date) >= yr),
    .(n_at_risk = .N),
    by = .(mm_install_cohort, group)
  ][, panel_year := yr]
}))

annual_closures <- diag_base[
  !is.na(close_yr) & close_yr %in% years_seq,
  .(n_closed = .N),
  by = .(mm_install_cohort, group, panel_year = close_yr)
]

cohort_annual <- merge(
  annual_at_risk, annual_closures,
  by = c("mm_install_cohort", "group", "panel_year"),
  all.x = TRUE
)
cohort_annual[is.na(n_closed), n_closed := 0L]
cohort_annual[, closure_rate := n_closed / n_at_risk]
cohort_annual[, cohort_label := paste0("Cohort: ", mm_install_cohort)]
cohort_annual[, period := fifelse(panel_year < REFORM_YR, "Pre", "Post")]

p_cohort <- ggplot(
  cohort_annual,
  aes(x = panel_year, y = closure_rate * 1000,
      colour = group, group = group)
) +
  geom_vline(xintercept = REFORM_YR - 0.5, linetype = "dashed",
             colour = "grey35", linewidth = 0.5) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.6) +
  facet_wrap(~cohort_label, ncol = 4, scales = "free_y") +
  scale_colour_manual(values = GROUP_COLS,
                      guide  = guide_legend(direction = "horizontal")) +
  scale_x_continuous(breaks = c(1995, 1999, 2003)) +
  scale_y_continuous(name = "Annual Closures per 1,000 At-Risk Tanks") +
  labs(x = "Calendar Year", colour = NULL) +
  theme_pub(base_size = 9) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 8),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.major.x = element_blank()
  )

save_gg(p_cohort, "Diag_ClosureRate_ByCohort", width = 11, height = 7)

# ── Pre-period summary table by cohort ──
pre_tbl <- cohort_annual[panel_year < REFORM_YR, .(
  pre_rate_per1k = round(mean(closure_rate, na.rm = TRUE) * 1000, 2),
  n_tank_years   = sum(n_at_risk)
), by = .(mm_install_cohort, group)]

pre_wide <- dcast(pre_tbl,
  mm_install_cohort ~ group,
  value.var = c("pre_rate_per1k", "n_tank_years")
)
pre_wide[, gap_per1k := `pre_rate_per1k_Texas` - `pre_rate_per1k_Control States`]
setorder(pre_wide, mm_install_cohort)

cat("\nPre-period closure rates by cohort (per 1,000 tank-years):\n")
print(pre_wide)
fwrite(pre_wide, file.path(OUTPUT_TABLES, "Diag_PreRate_ByCohort.csv"))


################################################################################
# FIGURE 2: Annual closure rate by individual state
# One panel per state — shows whether the control pre-period spike
# is concentrated in specific states or diffuse across all controls
################################################################################

cat("\nBuilding Figure 2: Annual closure rates by state...\n")

annual_at_risk_state <- rbindlist(lapply(years_seq, function(yr) {
  diag_base[
    year(tank_installed_date) <= yr &
    (is.na(tank_closed_date) | year(tank_closed_date) >= yr),
    .(n_at_risk = .N),
    by = .(state, group)
  ][, panel_year := yr]
}))

annual_closures_state <- diag_base[
  !is.na(close_yr) & close_yr %in% years_seq,
  .(n_closed = .N),
  by = .(state, group, panel_year = close_yr)
]

state_annual <- merge(
  annual_at_risk_state, annual_closures_state,
  by = c("state", "group", "panel_year"),
  all.x = TRUE
)
state_annual[is.na(n_closed), n_closed := 0L]
state_annual[, closure_rate := n_closed / n_at_risk]

# Order states: TX first, then controls alphabetically
state_order <- c("TX", sort(CONTROL_STATES))
state_annual[, state := factor(state, levels = state_order)]

# Color: TX in orange, all controls in blue
state_cols <- c("TX" = COL_TX,
                setNames(rep(COL_CTRL, length(CONTROL_STATES)), CONTROL_STATES))

p_state <- ggplot(
  state_annual,
  aes(x = panel_year, y = closure_rate * 1000,
      colour = state, group = state)
) +
  geom_vline(xintercept = REFORM_YR - 0.5, linetype = "dashed",
             colour = "grey35", linewidth = 0.5) +
  geom_line(linewidth = 0.65) +
  geom_point(size = 1.4) +
  facet_wrap(~state, ncol = 6, scales = "free_y") +
  scale_colour_manual(values = state_cols, guide = "none") +
  scale_x_continuous(breaks = c(1995, 1999, 2003)) +
  scale_y_continuous(name = "Annual Closures per 1,000 At-Risk Tanks") +
  labs(x = "Calendar Year") +
  theme_pub(base_size = 8) +
  theme(
    strip.text         = element_text(face = "bold", size = 8),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y        = element_text(size = 6),
    panel.grid.major.x = element_blank()
  )

save_gg(p_state, "Diag_ClosureRate_ByState", width = 14, height = 10)

# ── Pre vs post summary by state ──
state_period_tbl <- state_annual[, .(
  pre_rate_per1k  = round(mean(closure_rate[panel_year < REFORM_YR],  na.rm = TRUE) * 1000, 2),
  post_rate_per1k = round(mean(closure_rate[panel_year >= REFORM_YR], na.rm = TRUE) * 1000, 2),
  n_pre_ty        = sum(n_at_risk[panel_year < REFORM_YR]),
  n_post_ty       = sum(n_at_risk[panel_year >= REFORM_YR])
), by = .(state, group)]
state_period_tbl[, change := post_rate_per1k - pre_rate_per1k]
setorder(state_period_tbl, group, -pre_rate_per1k)

cat("\nPre/post closure rates by state (per 1,000 tank-years):\n")
print(state_period_tbl)
fwrite(state_period_tbl,
       file.path(OUTPUT_TABLES, "Diag_PrePostRate_ByState.csv"))

################################################################################
# FIGURE 3: Control states only — pre-period heat map
# Shows which state × cohort combinations drive the level difference
# Rows = states, Cols = cohorts, Fill = pre-period closure rate
################################################################################

cat("\nBuilding Figure 3: State x cohort pre-period heatmap...\n")

state_cohort_pre <- cohort_annual[
  panel_year < REFORM_YR,
  .(pre_rate = mean(closure_rate, na.rm = TRUE) * 1000,
    n_ty      = sum(n_at_risk)),
  by = .(state = fifelse(group == "Texas", "TX",
           # need to recover state from diag_base
           group),
         mm_install_cohort, group)
]

# Recover state-level data directly
state_cohort_base <- diag_base[
  !is.na(close_yr) & close_yr %in% years_seq,
  .(n_closed = .N),
  by = .(state, mm_install_cohort, panel_year = close_yr)
]

state_cohort_risk <- rbindlist(lapply(years_seq, function(yr) {
  diag_base[
    year(tank_installed_date) <= yr &
    (is.na(tank_closed_date) | year(tank_closed_date) >= yr),
    .(n_at_risk = .N),
    by = .(state, mm_install_cohort)
  ][, panel_year := yr]
}))

state_cohort_annual <- merge(
  state_cohort_risk, state_cohort_base,
  by = c("state", "mm_install_cohort", "panel_year"),
  all.x = TRUE
)
state_cohort_annual[is.na(n_closed), n_closed := 0L]
state_cohort_annual[, rate := n_closed / n_at_risk]
fwrite(state_cohort_annual,
       file.path(OUTPUT_TABLES, "Diag_StateCohort_AnnualRates.csv"))

pre_heatmap <- state_cohort_annual[panel_year < REFORM_YR, .(
  pre_rate_per1k = round(mean(rate, na.rm = TRUE) * 1000, 2),
  n_ty           = sum(n_at_risk)
), by = .(state, mm_install_cohort)]

pre_heatmap[, state := factor(state, levels = rev(state_order))]
pre_heatmap[, mm_install_cohort := factor(mm_install_cohort,
                                           levels = ANALYSIS_COHORTS)]
# Flag TX
pre_heatmap[, is_tx := state == "TX"]

p_heatmap <- ggplot(
  pre_heatmap,
  aes(x = mm_install_cohort, y = state, fill = pre_rate_per1k)
) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.1f", pre_rate_per1k)),
            size = 2.4, colour = "grey20") +
  # Highlight TX row
  geom_tile(data = pre_heatmap[is_tx == TRUE],
            colour = COL_TX, linewidth = 1.0, fill = NA) +
  scale_fill_gradient(
    low  = "white",
    high = COL_CTRL,
    name = "Pre-reform rate\n(per 1,000)",
    na.value = "grey95"
  ) +
  labs(x = "Install Cohort", y = NULL) +
  theme_pub(base_size = 9) +
  theme(
    panel.grid    = element_blank(),
    legend.position = "right",
    axis.text.y   = element_text(size = 8),
    axis.text.x   = element_text(size = 8)
  )

save_gg(p_heatmap, "Diag_PreRate_Heatmap_StateCohort", width = 10, height = 7)

cat("\nAll diagnostics saved.\n")
cat("  Diag_ClosureRate_ByCohort.pdf/.png\n")
cat("  Diag_ClosureRate_ByState.pdf/.png\n")
cat("  Diag_PreRate_Heatmap_StateCohort.pdf/.png\n")
cat("  Diag_PreRate_ByCohort.csv\n")
cat("  Diag_PrePostRate_ByState.csv\n")

################################################################################
# DONE
################################################################################

cat("\n", strrep("=", 60), "\n", sep = "")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 60), "\n\n", sep = "")
cat("Tables  →  ", OUTPUT_TABLES,  "\n", sep = "")
cat("Figures →  ", OUTPUT_FIGURES, "\n\n", sep = "")

cat("Figures saved:\n")
cat("  Fig_01_EventStudy_Pooled_Matched\n")
cat("  Fig_02_EventStudy_Pooled_CellYearFE\n")
cat("  Fig_03_EventStudy_Young_BelowMedianAge\n")
cat("  Fig_04_EventStudy_Old_AboveMedianAge\n")
cat("  Fig_05_EventStudy_Anticipation_Spec\n")
cat("  Fig_06_ClosureRates_OLS_Matched\n")
cat("  Fig_07_ClosureRates_Cox_Sample\n")
cat("  Fig_08_HonestDiD_Sensitivity_Anticipation\n")
cat("\nTables saved:\n")
cat("  Tab_01_OLS_Stepwise.tex\n")
cat("  Tab_02_Cox_Stepwise.tex\n")
cat("  Tab_03_Anticipation_Robustness.tex\n")
cat("  Tab_Appendix_HonestDiD_Sensitivity.csv\n")

cat("\n── QMD LOADING PATTERN ─────────────────────────────────────\n")
cat("In a QMD chunk WITH tbl-cap (Quarto owns the float):\n\n")
cat("  ```{r}\n")
cat("  #| results: asis\n")
cat("  source(here('UST_DiD_Analysis.R'))  # or just copy load_tex_table()\n")
cat("  load_tex_table(here('Output','Tables','Tab_01_OLS_Stepwise.tex'))\n")
cat("  ```\n\n")
cat("Never use latex_options = 'hold_position' inside a tbl-cap chunk.\n")
cat("Never use \\resizebox or \\adjustbox (\\textwidth = 0 in Quarto floats).\n")
cat("Never use % in cat() strings — Quarto escapes them to \\%.\n")
cat(strrep("─", 60), "\n\n", sep = "")