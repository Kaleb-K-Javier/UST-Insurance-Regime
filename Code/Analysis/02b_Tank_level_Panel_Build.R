################################################################################
# 02b_Panel_Build.R
# Texas UST Insurance Reform — Panel and Sample Construction
#
# LEGAL HISTORY NOTE (UST_Legal_History.md):
#   The upgrade mandate phase-in schedule is federal (40 CFR §280.21) and
#   IDENTICAL across all states. Texas 30 TAC §§334.44/334.47 mirror it exactly
#   as a condition of State Program Approval. Key implications:
#
#   (1) Pre-1989 install cohorts are NOT excluded — the mandate applied
#       identically to TX and all control states; they are not confounded.
#   (2) make_model_tank cells use the federal legal vintage bins since
#       those are the exact risk dimensions private insurers priced in 1999.
#   (3) Three separate mandate controls are built cohort-specifically from
#       the legal schedule, not as a single pooled flag.
#
#   Release detection compliance deadlines (§280.21 / §334.47(b)(4)):
#     install year <= 1964:   deadline Dec 22, 1989
#     1965-1969:              deadline Dec 22, 1990
#     1970-1974:              deadline Dec 22, 1991
#     1975-1979:              deadline Dec 22, 1992
#     1980-1988:              deadline Dec 22, 1993
#   Spill/overfill (all pre-1989 installs):              Dec 22, 1994
#   Tank integrity + cathodic (existing/pre-1989 installs): Dec 22, 1998
#     Post-1988 installs complied at installation under §334.44(a);
#     NOT subject to the §280.21(b) upgrade deadline.
#
# OUTPUTS (Data/Analysis/ — CSV):
#   panel_dt.csv          annual tank-year panel (primary analysis sample)
#   exact_base.csv        exact-date Cox base (one row per tank)
#   matched_tanks.csv     CEM-matched annual panel
#   matched_ids.csv       tank_panel_id of matched tanks
#   panel_meta.csv        scalar constants
#
# SECTIONS:
#   S1   Setup & Constants
#   S2   Utility Functions
#   S3   Load Master Tanks
#   S4   Build Annual Tank-Year Panel
#   S5   Add Mandate Controls (three separate flags, vintage-cohort specific)
#   S6   Define Primary Sample (panel_dt)
#   S7   Cell Coverage Diagnostics
#   S8   Build Exact-Date Cox Base
#   S9   Analysis Sample Construction (SW-exposed facilities)
#   S10  CEM Matching -> matched_tanks
#   S11  Post-Match Variable Construction
#   S12  Save Outputs as CSV
################################################################################


#### S1 Setup & Constants ####

suppressPackageStartupMessages({
  library(data.table)
  library(MatchIt)
  library(ggplot2)
  library(scales)
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
dir.create(ANALYSIS_DIR,   recursive = TRUE, showWarnings = FALSE)

# ---- Study parameters ----
POST_YEAR   <- 1999L
REFORM_DATE <- as.IDate("1998-12-22")
REFORM_DAYS <- as.numeric(as.Date("1998-12-22"))
STUDY_END   <- as.IDate("2020-12-31")

# ---- Federal/Texas release detection compliance deadlines ----
# 40 CFR §280.21 / 30 TAC §334.47(b)(4) — identical in both programs
RELEASE_DET_DEADLINE_YR <- list(
  pre_1965 = 1989L,   # install year <= 1964 (or unknown)
  y1965_69 = 1990L,   # 1965-1969
  y1970_74 = 1991L,   # 1970-1974
  y1975_79 = 1992L,   # 1975-1979
  y1980_88 = 1993L    # 1980-Dec 22, 1988
)
# post-1988 installs: comply from installation — no phase-in deadline
SPILL_OVERFILL_DEADLINE_YR <- 1994L   # §280.20(c) / §334.44(b)(1)(B); all pre-1989 installs
INTEGRITY_DEADLINE_YR      <- 1998L   # §280.21(b) / §334.44(b)(1)(A); existing/pre-1989 installs only

# ---- Study states ----
# PA excluded: PA Act 16 (June 1995) drives a spurious 1995 SW closure spike
# unrelated to the Texas reform — confirmed by leave-one-out event study.

#"IL","NM", "AL","MT", --- missing all wall type data here
# "GA", "WV" --> no closure dates

CONTROL_STATES <- c(
  "AR", "CO", "ID", "KS", "KY",
  "LA", "MA", "MD", "ME", "MN", "MO", "NC",
  "OH", "OK", "SD", "TN", "VA"
)
STUDY_STATES <- c("TX", CONTROL_STATES)

# ---- Colors ----
COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"
COL_PRE  <- "#E69F00"


#### S2 Utility Functions ####

log_step <- function(msg, indent = 0L) cat(strrep("  ", indent), msg, "\n", sep = "")
fmt_n    <- function(n) format(n, big.mark = ",", scientific = FALSE)
fmt_pct  <- function(x) sprintf("%.1f%%", x * 100)


#### S3 Load Master Tanks ####

cat("\n========================================\n")
cat("S3: LOAD MASTER TANKS\n")
cat("========================================\n\n")

master_tanks <- fread(
  here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv"),
  na.strings = c("", "NA", "N/A")
)
log_step(sprintf("Loaded: %s rows, %d columns", fmt_n(nrow(master_tanks)), ncol(master_tanks)))

required_cols <- c("mm_wall", "mm_fuel", "mm_capacity", "mm_install_cohort",
                   "facility_id", "state", "tank_id",
                   "tank_installed_date", "tank_closed_date")
missing_cols <- setdiff(required_cols, names(master_tanks))
stopifnot("Required columns missing — re-run harmonization script." = length(missing_cols) == 0)

master_tanks[, tank_installed_date := as.IDate(tank_installed_date)]
master_tanks[, tank_closed_date    := as.IDate(tank_closed_date)]

# Treatment assignment — NA for non-study states
master_tanks[, texas_treated := fcase(
  state == "TX",              1L,
  state %in% CONTROL_STATES, 0L,
  default = NA_integer_
)]

log_step(sprintf("TX=%s  CTL=%s  non-study=%s",
  fmt_n(master_tanks[texas_treated == 1L, .N]),
  fmt_n(master_tanks[texas_treated == 0L, .N]),
  fmt_n(master_tanks[is.na(texas_treated), .N])))

# Composite identifiers — facility_id alone is NOT unique across states
master_tanks[, panel_id      := paste(facility_id, state, sep = "_")]
master_tanks[, tank_panel_id := paste(facility_id, state, tank_id, sep = "_")]

study_tanks <- master_tanks[
  !is.na(texas_treated) &
  !is.na(tank_installed_date) &
  state %in% STUDY_STATES
]
log_step(sprintf("Valid study tanks: %s", fmt_n(nrow(study_tanks))))
cat("\n")


#### S4 Build Annual Tank-Year Panel ####

cat("========================================\n")
cat("S4: BUILD ANNUAL TANK-YEAR PANEL\n")
cat("========================================\n\n")

# ---- Install year as integer ----
study_tanks[, install_yr_int := as.integer(mm_install_cohort)]

# ---- Release detection deadline year per tank ----
# NA = post-1988 install; no phase-in; exempt from all mandate window controls.
# NOTE: The legal cutoff is Dec 22, 1988, not Jan 1, 1989. Annual install year
# data cannot distinguish within-1988; late-1988 installs are minor
# misclassified to the 1993 deadline — documented data limitation.
study_tanks[, release_det_deadline_yr := fcase(
  install_yr_int <= 1964L,           RELEASE_DET_DEADLINE_YR$pre_1965,
  install_yr_int %in% 1965L:1969L,   RELEASE_DET_DEADLINE_YR$y1965_69,
  install_yr_int %in% 1970L:1974L,   RELEASE_DET_DEADLINE_YR$y1970_74,
  install_yr_int %in% 1975L:1979L,   RELEASE_DET_DEADLINE_YR$y1975_79,
  install_yr_int %in% 1980L:1988L,   RELEASE_DET_DEADLINE_YR$y1980_88,
  default = NA_integer_
)]

# ---- Make-model cell variables ----
# make_model_tank  : wall x fuel x capacity x install_year (primary cell)
#   Using actual install year (not bins) so that cell x year FE specifications
#   identify vintage-year effects through within-cell variation over time.
#   Private insurers priced at the individual cohort year level post-reform.
# make_model_noage : wall x fuel x capacity (for Cox age-HTE specs where
#   the age dimension must not be absorbed into the baseline hazard stratum)
# make_model_3dim  : wall x fuel x install_year (robustness — drops capacity)
study_tanks[, make_model_tank  := paste(mm_wall, mm_fuel, mm_capacity, mm_install_cohort, sep = "|")]
study_tanks[, make_model_noage := paste(mm_wall, mm_fuel, mm_capacity, sep = "|")]
study_tanks[, make_model_3dim  := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "|")]

# Null out cells for tanks with unknown wall, fuel, or missing install cohort
# — these cannot be matched and would create spurious cells
study_tanks[
  mm_wall == "Unknown-Wall" | mm_fuel == "Unknown-Fuel" | is.na(mm_install_cohort),
  `:=`(make_model_tank  = NA_character_,
       make_model_noage = NA_character_,
       make_model_3dim  = NA_character_)
]

# ---- Expand to annual tank-year panel (1985-2020) ----
log_step("Expanding to annual tank-year panel (1985-2020)...")

study_tanks[, `:=`(
  install_yr   = year(tank_installed_date),
  close_yr_raw = fifelse(!is.na(tank_closed_date), year(tank_closed_date), 2020L)
)]
study_tanks[, `:=`(
  expand_start = pmax(install_yr, 1985L),
  expand_end   = pmin(close_yr_raw, 2020L)
)]

panel_cols <- c(
  "tank_panel_id", "panel_id", "facility_id", "state", "texas_treated",
  "mm_wall", "mm_fuel", "mm_capacity",
  "mm_install_cohort", "install_yr_int",
  "make_model_tank", "make_model_noage", "make_model_3dim",
  "release_det_deadline_yr",
  "tank_installed_date", "tank_closed_date",
  "expand_start", "expand_end"
)

tank_year_panel <- study_tanks[
  expand_start <= expand_end, .SD, .SDcols = panel_cols
][, {
  yrs <- seq(expand_start, expand_end)
  .(panel_year    = yrs,
    closure_event = as.integer(!is.na(tank_closed_date) & year(tank_closed_date) == yrs))
}, by = panel_cols]

setorder(tank_year_panel, tank_panel_id, panel_year)

log_step(sprintf("Panel: %s rows | %s tanks | %s facilities",
  fmt_n(nrow(tank_year_panel)),
  fmt_n(uniqueN(tank_year_panel$tank_panel_id)),
  fmt_n(uniqueN(tank_year_panel$panel_id))))
cat("\n")


#### S5 Add Mandate Controls ####

cat("========================================\n")
cat("S5: ADD MANDATE CONTROLS\n")
cat("========================================\n\n")

# Core DiD time variables
tank_year_panel[, `:=`(
  post_1999 = as.integer(panel_year >= POST_YEAR),
  did_term  = texas_treated * as.integer(panel_year >= POST_YEAR),
  rel_year  = panel_year - POST_YEAR,
  tstart    = panel_year - 1L,
  tstop     = panel_year,
  tank_age  = panel_year - install_yr_int
)]

# -----------------------------------------------------------------------
# MANDATE 1: Release Detection
# Applies to: pre-1989 installs only (release_det_deadline_yr is NA for
#   post-1988 installs, so the !is.na() gate excludes them automatically).
# Logic: tank-year is inside its cohort's federal compliance window.
#   Window opens:  1989 — when 30 TAC Ch. 334 took effect and the first
#                  deadline (pre-1965 cohort, Dec 22 1989) passed.
#   Window closes: the calendar year of the cohort's specific deadline.
#     pre-1965 cohort → window is 1989 only  (deadline 1989)
#     1965-69 cohort  → window is 1989-1990  (deadline 1990)
#     1970-74 cohort  → window is 1989-1991  (deadline 1991)
#     1975-79 cohort  → window is 1989-1992  (deadline 1992)
#     1980-88 cohort  → window is 1989-1993  (deadline 1993)
# Variation is two-dimensional: install-year cohort × calendar year.
# Both TX and all control states faced this schedule identically
# (40 CFR §280.21 / 30 TAC §334.47(b)(4)).
# -----------------------------------------------------------------------
tank_year_panel[, mandate_release_det := as.integer(
  !is.na(release_det_deadline_yr) &
  panel_year >= 1989L &
  panel_year <= release_det_deadline_yr
)]

# -----------------------------------------------------------------------
# MANDATE 2: Spill and Overfill Prevention
# Applies to: all pre-1989 installs (§280.20(c) / §334.44(b)(1)(B)).
# Deadline: Dec 22, 1994 — uniform across all pre-1989 cohorts.
# No install-year variation within the pre-1989 universe: a 1965 tank
# and a 1988 tank faced the identical deadline. Variation is only
# pre-1989 (affected) vs post-1988 (already compliant at installation).
# Pressure window: 1993-1994 (two years leading into the deadline).
# -----------------------------------------------------------------------
tank_year_panel[, mandate_spill_overfill := as.integer(
  !is.na(release_det_deadline_yr) &
  panel_year %in% 1993L:1994L
)]

# -----------------------------------------------------------------------
# MANDATE 3: Tank Integrity Assessment + Cathodic Protection
# Applies to: existing/pre-1989 installs only (§280.21(b) / §334.44(b)(1)(A)).
# Deadline: Dec 22, 1998 — uniform across all pre-1989 cohorts.
# Post-1988 installs complied at installation under §334.44(a)/§§334.45-46;
# NOT subject to this deadline. The !is.na() gate correctly excludes them.
# This is the highest-stakes mandate: §334.47(a)(2) requires permanent
# removal from service within 60 days of the deadline for non-complying
# systems — a mandatory closure order, not just a penalty. Cathodic
# protection retrofits on old steel tanks are expensive; this mandate
# forced large-scale exit of marginal single-walled tanks.
# Pressure window: 1996-1998 (three years leading into the deadline).
# -----------------------------------------------------------------------
tank_year_panel[, mandate_integrity := as.integer(
  !is.na(release_det_deadline_yr) &
  panel_year %in% 1996L:1998L
)]

# -----------------------------------------------------------------------
# Verification: all three mandate windows must equal zero for every
# post-1998 panel year. Non-overlap with did_term is a precondition for
# clean identification — mandate controls cannot contaminate the DiD
# estimate if they are mechanically zero wherever did_term is nonzero.
# -----------------------------------------------------------------------
mandate_post_check <- tank_year_panel[
  panel_year >= 1999L,
  .(
    n_release_det    = sum(mandate_release_det),
    n_spill_overfill = sum(mandate_spill_overfill),
    n_integrity      = sum(mandate_integrity)
  )
]
stopifnot(
  "mandate_release_det nonzero post-1998"    = mandate_post_check$n_release_det    == 0L,
  "mandate_spill_overfill nonzero post-1998" = mandate_post_check$n_spill_overfill == 0L,
  "mandate_integrity nonzero post-1998"      = mandate_post_check$n_integrity      == 0L
)
log_step("Mandate overlap check passed: all three windows are zero for panel_year >= 1999")

# -----------------------------------------------------------------------
# Binned relative year for event studies [-14, 16]
# Floor at -14 (1984) to expose pre-1989 mandate compliance years;
# cap at +16 (2017) for post-reform horizon.
# -----------------------------------------------------------------------
tank_year_panel[, rel_year_bin := fcase(
  rel_year <= -14L, -14L,
  rel_year >= 16L,   16L,
  default = as.integer(rel_year)
)]

# -----------------------------------------------------------------------
# Mandate coverage diagnostic — per-mandate tank-year counts by cohort.
# mandate_integrity expected to dominate for old single-walled steel tanks.
# -----------------------------------------------------------------------
mand_summ <- tank_year_panel[install_yr_int>= 1940L & (
  mandate_release_det == 1L | mandate_spill_overfill == 1L | mandate_integrity == 1L),
  .(
    n_ty_release_det    = sum(mandate_release_det),
    n_ty_spill_overfill = sum(mandate_spill_overfill),
    n_ty_integrity      = sum(mandate_integrity),
    n_tanks             = uniqueN(tank_panel_id)
  ),
  by = .(mm_install_cohort, release_det_deadline_yr)
][order(mm_install_cohort)]

log_step("Mandate coverage by install cohort (tank-years per mandate window):")
print(mand_summ)
fwrite(mand_summ, file.path(OUTPUT_TABLES, "Diag_MandateCoverage.csv"))

# ---- Visualization: Mandate coverage by vintage & state ----
# Count unique tanks per mandate type per cohort per state
mand_summ_by_state <- rbind(
  tank_year_panel[mandate_release_det == 1L, 
    .(mandate_type = "Release Detection", n_tanks = uniqueN(tank_panel_id)),
    by = .(mm_install_cohort, state)],
  tank_year_panel[mandate_spill_overfill == 1L, 
    .(mandate_type = "Spill/Overfill", n_tanks = uniqueN(tank_panel_id)),
    by = .(mm_install_cohort, state)],
  tank_year_panel[mandate_integrity == 1L, 
    .(mandate_type = "Tank Integrity", n_tanks = uniqueN(tank_panel_id)),
    by = .(mm_install_cohort, state)]
)

# Aggregate: sum tanks by cohort-mandate, grouping TX vs Control
mand_long <- mand_summ_by_state[, 
  .(n_tanks = sum(n_tanks)), 
  by = .(mm_install_cohort, mandate_type, state_group = fifelse(state == "TX", "Texas", "Control"))
]

# Bin years 1940-1960 together to shorten graph
mand_long[, x_label := fcase(
  mm_install_cohort <= 1960, "1940-1960 (pooled)",
  default = as.character(mm_install_cohort)
)]

# Aggregate by x_label to consolidate pooled cohorts into single bars
mand_long <- mand_long[, .(n_tanks = sum(n_tanks)), 
  by = .(x_label, mandate_type, state_group)]

# Create ordered factor for x-axis to preserve order
all_labels <- c("1940-1960 (pooled)", 
                as.character(sort(setdiff(as.integer(mand_long[x_label != "1940-1960 (pooled)", x_label]), NA))))
mand_long[, x_label := factor(x_label, levels = all_labels)]

# Add readable mandate labels with compliance deadlines
mand_long[, mandate_label := fcase(
  mandate_type == "Release Detection",    "Release Detection\n(Deadline: 1989-1993)",
  mandate_type == "Spill/Overfill",       "Spill/Overfill\n(Deadline: Dec 22, 1994)",
  mandate_type == "Tank Integrity",       "Tank Integrity\n(Deadline: Dec 22, 1998)",
  default = mandate_type
)]

# Create decade background rectangles for alternating shading
decade_rects <- rbind(
  data.table(xmin = 0.5, xmax = 1.5, is_gray = TRUE),   # 1940-1960 pooled - gray
  data.table(xmin = 1.5, xmax = 11.5, is_gray = FALSE), # 1961-1970 - white
  data.table(xmin = 11.5, xmax = 21.5, is_gray = TRUE), # 1971-1980 - gray
  data.table(xmin = 21.5, xmax = 31.5, is_gray = FALSE)# 1981-1990 - white
)

mand_plot <- ggplot(mand_long, aes(x = x_label, y = n_tanks, fill = state_group)) +
  # Add gray decade background rectangles
  geom_rect(data = decade_rects[is_gray == TRUE], 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "gray85", alpha = 0.2, inherit.aes = FALSE, color = NA) +
  # Add bar columns
  geom_col(width = 0.6, color = "black", linewidth = 0.3, position = "dodge") +
  facet_wrap(~ mandate_label, ncol = 1, scales = "fixed") +
  labs(
    x = "Install Year Cohort",
    y = "Number of Tanks Affected",
    fill = "State Group"
  ) +
  scale_fill_manual(
    values = c("Texas" = "#D55E00",
               "Control" = "#0072B2")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 10, face = "bold", hjust = 0),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.y = unit(10, "pt"),
    legend.position = "bottom"
  )

ggsave(
  file.path(OUTPUT_FIGURES, "Mandate_Coverage_by_Vintage.png"),
  plot = mand_plot,
  width = 8,
  height = 9,
  dpi = 300
)



log_step("Saved: Mandate_Coverage_by_Vintage.png")
cat("\n")

# ---- Visualization: Mandate response rates by vintage & state ----
# For each vintage and state, calculate:
#   - Number of tanks exposed to each mandate
#   - Number that closed in the deadline year
#   - Closure rate (%)

# Release Detection deadlines vary by cohort; create mapping
deadline_map <- data.table(
  mm_install_cohort = c(1940:1964, 1965:1969, 1970:1974, 1975:1979, 1980:1988),
  deadline_yr = c(rep(1989L, 25), rep(1990L, 5), rep(1991L, 5), 
                  rep(1992L, 5), rep(1993L, 9))
)

# For Release Detection: closure rate in deadline year per cohort
rd_response <- tank_year_panel[
  !is.na(release_det_deadline_yr),
  .(
    n_exposed = uniqueN(tank_panel_id),
    n_closed_deadline = sum((closure_event == 1L) & (panel_year == release_det_deadline_yr))
  ),
  by = .(mm_install_cohort, release_det_deadline_yr, state)
]
rd_response[, closure_rate := 100 * n_closed_deadline / n_exposed]
rd_response[, mandate_type := "Release Detection"]

# For Spill/Overfill: closure rate in 1994 (uniform deadline)
so_response <- tank_year_panel[
  !is.na(release_det_deadline_yr) & panel_year == 1994L,
  .(
    n_exposed = uniqueN(tank_panel_id),
    n_closed_deadline = sum(closure_event == 1L)
  ),
  by = .(mm_install_cohort, state)
]
so_response[, closure_rate := 100 * n_closed_deadline / n_exposed]
so_response[, mandate_type := "Spill/Overfill"]
so_response[, deadline_yr := 1994L]

# For Tank Integrity: closure rate in 1998 (uniform deadline)
ti_response <- tank_year_panel[
  !is.na(release_det_deadline_yr) & panel_year == 1998L,
  .(
    n_exposed = uniqueN(tank_panel_id),
    n_closed_deadline = sum(closure_event == 1L)
  ),
  by = .(mm_install_cohort, state)
]
ti_response[, closure_rate := 100 * n_closed_deadline / n_exposed]
ti_response[, mandate_type := "Tank Integrity"]
ti_response[, deadline_yr := 1998L]

# Combine all response data
response_long <- rbind(
  rd_response[, .(mm_install_cohort, state, mandate_type, n_exposed, closure_rate)],
  so_response[, .(mm_install_cohort, state, mandate_type, n_exposed, closure_rate)],
  ti_response[, .(mm_install_cohort, state, mandate_type, n_exposed, closure_rate)]
)

# Add state group and vintage label
response_long[, state_group := fifelse(state == "TX", "Texas", "Control")]
response_long[, vintage_label := fcase(
  mm_install_cohort <= 1960, "1940-1960",
  default = as.character(mm_install_cohort)
)]

# Add mandate labels
response_long[, mandate_label := fcase(
  mandate_type == "Release Detection",    "Release Detection",
  mandate_type == "Spill/Overfill",       "Spill/Overfill",
  mandate_type == "Tank Integrity",       "Tank Integrity",
  default = mandate_type
)]



# ---- Aggregate response_long across states within each group ----
# Back-calculate closures from rate x exposure, then re-derive weighted rate
response_agg <- response_long[, .(
  n_exposed    = sum(n_exposed, na.rm = TRUE),
  n_closed     = sum(n_exposed * closure_rate / 100, na.rm = TRUE)
), by = .(vintage_label, state_group, mandate_type)]

response_agg[, closure_rate := 100 * n_closed / n_exposed]
response_agg[n_exposed == 0, closure_rate := 0]

# Add mandate labels to match plot_data_corrected expectations
response_agg[, mandate_label := fcase(
  mandate_type == "Release Detection", "Release Detection\n(Deadline: 1989-1993)",
  mandate_type == "Spill/Overfill",    "Spill/Overfill\n(Deadline: Dec 22, 1994)",
  mandate_type == "Tank Integrity",    "Tank Integrity\n(Deadline: Dec 22, 1998)",
  default = mandate_type
)]


response_plot <- ggplot(response_long[mm_install_cohort >= 1940], 
                        aes(x = n_exposed, y = closure_rate, color = state_group, size = n_exposed)) +
  geom_point(alpha = 0.6, stroke = 0.8) +
  facet_wrap(~ mandate_label, ncol = 1) +
  labs(
    x = "Number of Tanks Exposed (log scale)",
    y = "Closure Rate in Deadline Year (%)",
    color = "State Group",
    size = "Tank Count"
  ) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_manual(
    values = c("Texas" = "#D55E00",
               "Control" = "#0072B2")
  ) +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 10, face = "bold", hjust = 0),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(10, "pt"),
    legend.position = "bottom"
  )

ggsave(
  file.path(OUTPUT_FIGURES, "Mandate_Response_by_Vintage.png"),
  plot = response_plot,
  width = 8,
  height = 9,
  dpi = 300
)

log_step("Saved: Mandate_Response_by_Vintage.png")
cat("\n")
# 1. Isolate the true, static baseline population for each cohort
# Extracting the maximum observed tanks across all mandates captures the inventory prior to mandate-induced attrition
static_inventory <- response_agg[, .(baseline_tanks = max(n_exposed)), by = .(vintage_label, state_group)]

# 2. Calculate the total pre-1989 baseline population per state group
state_totals <- static_inventory[, .(total_state_tanks = sum(baseline_tanks)), by = state_group]

# 3. Compute the fixed structural share per cohort
static_shares <- merge(static_inventory, state_totals, by = "state_group")
static_shares[, static_cohort_share := (baseline_tanks / total_state_tanks) * 100]

# 4. Merge the static metric back to the dynamic mandate response data
plot_data_corrected <- merge(
  response_agg, 
  static_shares[, .(vintage_label, state_group, static_cohort_share)], 
  by = c("vintage_label", "state_group")
)

# 5. Generate abbreviated labels for point geometries
plot_data_corrected[, point_label := fcase(
  vintage_label == "1940-1960", "40-60",
  default = substr(as.character(vintage_label), 3, 4)
)]

# Add readable mandate labels with compliance deadlines to match other figures
plot_data_corrected[, mandate_label := fcase(
  mandate_type == "Release Detection",    "Release Detection\n(Deadline: 1989-1993)",
  mandate_type == "Spill/Overfill",       "Spill/Overfill\n(Deadline: Dec 22, 1994)",
  mandate_type == "Tank Integrity",       "Tank Integrity\n(Deadline: Dec 22, 1998)",
  default = mandate_type
)]

# 6. Generate the Corrected Static Share vs. Response Scatter Plot
scatter_text_plot_corrected <- ggplot(plot_data_corrected, aes(x = static_cohort_share, y = closure_rate, color = state_group)) +
  geom_point(shape = 21, fill = "white", size = 11, stroke = 1.2, alpha = 0.8) +
  geom_text(aes(label = point_label), size = 3, color = "black", fontface = "bold") +
  facet_wrap(~ mandate_label, ncol = 1) +
  labs(
    x = "Fixed Cohort Share of Pre-1989 Inventory (%)",
    y = "Closure Rate in Deadline Year (%)",
    color = "State Group"
  ) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  scale_color_manual(values = c("Texas" = "#D55E00", "Control" = "#0072B2")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 10, face = "bold", hjust = 0),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(12, "pt"),
    legend.position = "bottom"
  )

ggsave(
  file.path(OUTPUT_FIGURES, "Mandate_Static_Share_vs_Response.png"),
  plot = scatter_text_plot_corrected,
  width = 9,
  height = 10,
  dpi = 300
)

log_step("Saved: Mandate_Static_Share_vs_Response.png")
cat("\n")

# ---- Visualization: Tank population composition by decade over time ----
# For each year 1990-2018 and state group, calculate the share of active tanks
# from each install decade to motivate the "existing source pollutants" narrative

# Create close_yr_int from expand_end (closure year if closed, else study end year)
tank_year_panel[, close_yr_int := as.integer(expand_end)]

# Extract unique tank characteristics with closure year
tank_chars <- unique(tank_year_panel[, .(tank_panel_id, state, install_yr_int, close_yr_int)])

# Expand each tank to all years 1990-2018 in which it was active
# A tank is active in year YYYY if: install_yr_int <= YYYY <= close_yr_int
composition_data <- tank_chars[, .(
  panel_year = seq(max(1990L, install_yr_int), min(2018L, close_yr_int))
), by = .(tank_panel_id, state, install_yr_int)]

# Assign install decade: 1940-1979 pooled (old pre-reform), then 1980s, 1990s, 2000s, 2010+
composition_data[, install_decade := fcase(
  install_yr_int <= 1979, "1940-1979",
  install_yr_int %in% 1980:1989, "1980-1989",
  install_yr_int %in% 1990:1999, "1990-1999",
  install_yr_int %in% 2000:2009, "2000-2009",
  default = "2010+"
)]

# Count unique tanks by year, state, and decade
decade_comp <- composition_data[, 
  .(n_tanks = uniqueN(tank_panel_id)),
  by = .(panel_year, state, install_decade)
]

# Calculate total active tanks per year and state (across all cohorts)
decade_totals <- decade_comp[, .(total_tanks = sum(n_tanks)), by = .(panel_year, state)]

# Merge and compute share: (tanks in this cohort for this year) / (total tanks in this year+state) * 100
decade_comp <- merge(decade_comp, decade_totals, by = c("panel_year", "state"))
decade_comp[, share := 100 * n_tanks / total_tanks]

# Add state group
decade_comp[, state_group := fifelse(state == "TX", "Texas", "Control")]

# Ordered factor for decades
decade_order <- c("1940-1979", "1980-1989", "1990-1999", "2000-2009", "2010+")
decade_comp[, install_decade := factor(install_decade, levels = decade_order)]

# B&W-friendly grayscale palette (5 colors for print/B&W compatibility)
bw_palette <- c(
  "1940-1979" = "#000000",    # black (oldest, pre-reform)
  "1980-1989" = "#404040",    # dark gray
  "1990-1999" = "#808080",    # medium gray
  "2000-2009" = "#c0c0c0",    # light gray
  "2010+"     = "#f0f0f0"     # near white (newest)
)

# ---- VERSION 1: Aggregate across all states (whole sample) ----
# For each year and decade, sum tanks across all states, then calculate share
# Share = (total tanks in decade G for year YYYY) / (total tanks all decades for year YYYY)
comp_timeline_all <- decade_comp[panel_year %between% c(1990L, 2018L), 
  .(n_tanks_cohort = sum(n_tanks)),
  by = .(panel_year, install_decade)
]

# Ensure all decade-year combinations exist (fill missing with 0)
all_years <- unique(comp_timeline_all$panel_year)
all_decades <- unique(comp_timeline_all$install_decade)
complete_grid <- CJ(panel_year = all_years, install_decade = all_decades)
comp_timeline_all <- merge(comp_timeline_all, complete_grid, 
                            by = c("panel_year", "install_decade"), all = TRUE)
comp_timeline_all[is.na(n_tanks_cohort), n_tanks_cohort := 0L]

# Calculate total tanks across all cohorts per year
total_by_year <- comp_timeline_all[, .(n_tanks_total = sum(n_tanks_cohort)), by = panel_year]

# Merge and compute share
comp_timeline_all <- merge(comp_timeline_all, total_by_year, by = "panel_year")
comp_timeline_all[, share := 100 * n_tanks_cohort / n_tanks_total]
comp_timeline_all[, `:=`(n_tanks_cohort = NULL, n_tanks_total = NULL)]

# Ensure proper sorting and factor ordering for stacked area
setorder(comp_timeline_all, panel_year, install_decade)
comp_timeline_all[, install_decade := factor(install_decade, levels = decade_order)]

composition_plot_all <- ggplot(comp_timeline_all, aes(x = panel_year, y = share, fill = install_decade, group = install_decade)) +
  geom_area(alpha = 0.85, color = "black", linewidth = 0.15, position = "stack") +
  labs(
    x = "Year",
    y = "Share of Active Tank Inventory (%)",
    fill = "Install Period"
  ) +
  scale_x_continuous(limits = c(1990, 2018), breaks = seq(1990, 2018, 5)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = bw_palette) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

ggsave(
  file.path(OUTPUT_FIGURES, "Tank_Population_Composition_Overall.png"),
  plot = composition_plot_all,
  width = 10,
  height = 6,
  dpi = 300
)

log_step("Saved: Tank_Population_Composition_Overall.png")

# Compute 5-year period averages of cohort shares from comp_timeline_all
comp_timeline_all[, period := fcase(
  panel_year %between% c(1990L, 1994L), "1990-1994",
  panel_year %between% c(1995L, 1999L), "1995-1999",
  panel_year %between% c(2000L, 2004L), "2000-2004",
  panel_year %between% c(2005L, 2009L), "2005-2009",
  panel_year %between% c(2010L, 2014L), "2010-2014",
  panel_year %between% c(2015L, 2018L), "2015-2018"
)]

bar_comp <- comp_timeline_all[!is.na(period), .(
  mean_share = mean(share, na.rm = TRUE)
), by = .(period, install_decade)]

# Renormalize within each period so bars sum exactly to 100
bar_comp[, mean_share := 100 * mean_share / sum(mean_share), by = period]

bar_comp[, period := factor(period, levels = c(
  "1990-1994", "1995-1999", "2000-2004",
  "2005-2009", "2010-2014", "2015-2018"
))]
bar_comp[, install_decade := factor(install_decade, levels = decade_order)]

# Add label text: only show if share is large enough to read
bar_comp[, label := ifelse(mean_share >= 5, sprintf("%.0f%%", mean_share), "")]

fig_bar_comp <- ggplot(bar_comp,
    aes(x = period, y = mean_share, fill = install_decade)) +
  geom_col(
    width = 0.7,
    color = "white",
    linewidth = 0.3
  ) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = c("white", "white", "black", "black", "black")[ # light text on dark, dark on light
      as.integer(bar_comp$install_decade)
    ],
    fontface = "bold"
  ) +
  scale_fill_manual(values = bw_palette) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = NULL,
    y = "Share of Active Tank Inventory (%)",
    fill = "Install Period"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 9, face = "bold"),
    legend.text        = element_text(size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave(
  file.path(OUTPUT_FIGURES, "Tank_Population_Composition_Bar.png"),
  plot = fig_bar_comp,
  width = 8, height = 5, dpi = 300
)

# ---- VERSION 2: Separated by Texas vs Control ----
# For each year and state group, sum tanks within that group, then calculate share
# Share = (tanks in cohort G and state_group S for year YYYY) / (total tanks in state_group S for year YYYY)
comp_timeline_by_state <- decade_comp[panel_year %between% c(1990L, 2018L), 
  .(n_tanks_cohort = sum(n_tanks)),
  by = .(panel_year, state_group, install_decade)
]

# Ensure all state_group-decade-year combinations exist (fill missing with 0)
all_years_st <- unique(comp_timeline_by_state$panel_year)
all_state_groups <- unique(comp_timeline_by_state$state_group)
all_decades_st <- unique(comp_timeline_by_state$install_decade)
complete_grid_st <- CJ(panel_year = all_years_st, state_group = all_state_groups, 
                       install_decade = all_decades_st)
comp_timeline_by_state <- merge(comp_timeline_by_state, complete_grid_st, 
                                 by = c("panel_year", "state_group", "install_decade"), all = TRUE)
comp_timeline_by_state[is.na(n_tanks_cohort), n_tanks_cohort := 0L]

# Calculate total tanks per state group per year
total_by_state_year <- comp_timeline_by_state[, 
  .(n_tanks_total = sum(n_tanks_cohort)), 
  by = .(panel_year, state_group)
]

# Merge and compute share
comp_timeline_by_state <- merge(
  comp_timeline_by_state, 
  total_by_state_year, 
  by = c("panel_year", "state_group")
)
comp_timeline_by_state[, share := 100 * n_tanks_cohort / n_tanks_total]
comp_timeline_by_state[, `:=`(n_tanks_cohort = NULL, n_tanks_total = NULL)]

# Ensure proper sorting and factor ordering for stacked area
setorder(comp_timeline_by_state, state_group, panel_year, install_decade)
comp_timeline_by_state[, install_decade := factor(install_decade, levels = decade_order)]

composition_plot_by_state <- ggplot(comp_timeline_by_state, aes(x = panel_year, y = share, fill = install_decade, group = install_decade)) +
  geom_area(alpha = 0.85, color = "black", linewidth = 0.15, position = "stack") +
  facet_wrap(~ state_group, ncol = 1) +
  labs(
    x = "Year",
    y = "Share of Active Tank Inventory (%)",
    fill = "Install Period"
  ) +
  scale_x_continuous(limits = c(1990, 2018), breaks = seq(1990, 2018, 5)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = bw_palette) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 11, face = "bold", hjust = 0),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.y = unit(12, "pt"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

ggsave(
  file.path(OUTPUT_FIGURES, "Tank_Population_Composition_by_State.png"),
  plot = composition_plot_by_state,
  width = 10,
  height = 8,
  dpi = 300
)

log_step("Saved: Tank_Population_Composition_by_State.png")
cat("\n")


# ---- Figure 1: Composition stacked bar by TX vs Control ----

comp_by_state_period <- comp_timeline_by_state[, period := fcase(
  panel_year %between% c(1990L, 1994L), "1990-1994",
  panel_year %between% c(1995L, 1999L), "1995-1999",
  panel_year %between% c(2000L, 2004L), "2000-2004",
  panel_year %between% c(2005L, 2009L), "2005-2009",
  panel_year %between% c(2010L, 2014L), "2010-2014",
  panel_year %between% c(2015L, 2018L), "2015-2018"
)]

bar_comp_state <- comp_timeline_by_state[!is.na(period), .(
  mean_share = mean(share, na.rm = TRUE)
), by = .(period, state_group, install_decade)]

# Renormalize within each period x state_group so bars sum to 100
bar_comp_state[, mean_share := 100 * mean_share / sum(mean_share),
  by = .(period, state_group)]

bar_comp_state[, period := factor(period, levels = c(
  "1990-1994", "1995-1999", "2000-2004",
  "2005-2009", "2010-2014", "2015-2018"
))]
bar_comp_state[, install_decade := factor(install_decade, levels = decade_order)]
bar_comp_state[, state_group := factor(state_group, levels = c("Control", "Texas"))]
bar_comp_state[, label := ifelse(mean_share >= 6, sprintf("%.0f%%", mean_share), "")]

fig_bar_comp_state <- ggplot(bar_comp_state,
    aes(x = period, y = mean_share, fill = install_decade)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 2.8, color = "white", fontface = "bold"
  ) +
  facet_wrap(~ state_group, ncol = 2) +
  scale_fill_manual(values = bw_palette) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = NULL,
    y = "Share of Active Tank Inventory (%)",
    fill = "Install Period"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 9, face = "bold"),
    legend.text        = element_text(size = 9),
    strip.text         = element_text(face = "bold", size = 11),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.x    = unit(16, "pt")
  )

ggsave(
  file.path(OUTPUT_FIGURES, "Tank_Population_Composition_Bar_byState.png"),
  plot = fig_bar_comp_state,
  width = 10, height = 6, dpi = 300
)

# ---- Build cohort_wide (needed for exit rate diff figures) ----
tank_year_panel[, cohort_group := fcase(
  install_yr_int < 1980L,                   "Pre-1980",
  install_yr_int %between% c(1980L, 1988L), "1980-1988",
  install_yr_int %between% c(1989L, 1998L), "1989-1998",
  default = NA_character_
)]

cohort_exits <- tank_year_panel[
  !is.na(cohort_group) &
  panel_year %between% c(1980L, 2018L),
  .(
    n_active = .N,
    n_closed = sum(closure_event)
  ),
  by = .(panel_year, state, cohort_group)
]

cohort_exits[, exit_rate   := 100 * n_closed / n_active]
cohort_exits[, state_group := fifelse(state == "TX", "Texas", "Control")]

cohort_exits_agg <- cohort_exits[, .(
  exit_rate = mean(exit_rate)
), by = .(panel_year, state_group, cohort_group)]

cohort_wide <- dcast(
  cohort_exits_agg,
  panel_year + cohort_group ~ state_group,
  value.var = "exit_rate"
)
cohort_wide[, diff := Texas - Control]

cohort_palette <- c(
  "Pre-1980"  = "#000000",
  "1980-1988" = "#808080",
  "1989-1998" = "#c0c0c0"
)

# ---- Figure 2: Exit rate diff grouped bar, 5-year periods, by cohort ----

cohort_wide[, period5 := fcase(
  panel_year %between% c(1980L, 1984L), "1980-1984",
  panel_year %between% c(1985L, 1989L), "1985-1989",
  panel_year %between% c(1990L, 1994L), "1990-1994",
  panel_year %between% c(1995L, 1999L), "1995-1999",
  panel_year %between% c(2000L, 2004L), "2000-2004",
  panel_year %between% c(2005L, 2009L), "2005-2009",
  panel_year %between% c(2010L, 2014L), "2010-2014",
  panel_year %between% c(2015L, 2018L), "2015-2018"
)]

bar_diff <- cohort_wide[!is.na(period5), .(
  mean_diff = mean(diff, na.rm = TRUE)
), by = .(period5, cohort_group)]

bar_diff[, period5 := factor(period5, levels = c(
  "1980-1984", "1985-1989", "1990-1994", "1995-1999",
  "2000-2004", "2005-2009", "2010-2014", "2015-2018"
))]
bar_diff[, cohort_group := factor(cohort_group,
  levels = c("Pre-1980", "1980-1988", "1989-1998")
)]

fig_bar_diff_periods <- ggplot(bar_diff,
    aes(x = period5, y = mean_diff, fill = cohort_group)) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.65,
    color = "white",
    linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
  geom_vline(xintercept = 4.5, linetype = "dashed",
    color = "gray40", linewidth = 0.5) +
  scale_fill_manual(
    values = cohort_palette,
    labels = c(
      "Pre-1980"  = "Pre-1980 (highest risk)",
      "1980-1988" = "1980\u20131988",
      "1989-1998" = "1989\u20131998 (placebo)"
    )
  ) +
  scale_y_continuous(
    labels = function(x) ifelse(x == 0, "0", paste0(x, " pp"))
  ) +
  labs(
    x = NULL,
    y = "Texas minus Control States (percentage points)",
    fill = "Install Cohort",
    caption = paste0(
      "Bars show mean annual difference in cohort-specific exit rates (TX \u2212 control state average) ",
      "within each five-year period.\n",
      "Exit rate = closures as share of active tanks in that cohort \u00d7 year. ",
      "Dashed line marks onset of Texas insurance reform (1999)."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 9, face = "bold"),
    legend.text        = element_text(size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption       = element_text(size = 8, color = "gray50", hjust = 0)
  )

ggsave(
  file.path(OUTPUT_FIGURES, "Fleet_ExitRate_Diff_5yr_Bar.png"),
  plot = fig_bar_diff_periods,
  width = 9, height = 5, dpi = 300
)

# ---- Figure 3: Exit rate diff grouped bar, four broad periods, by cohort ----

cohort_wide[, period := fcase(
  panel_year %between% c(1980L, 1988L), "Pre-Mandate\n(1980\u20131988)",
  panel_year %between% c(1989L, 1998L), "Federal Mandates\n(1989\u20131998)",
  panel_year %between% c(1999L, 2008L), "Early Reform\n(1999\u20132008)",
  panel_year %between% c(2009L, 2018L), "Late Reform\n(2009\u20132018)"
)]

bar_data <- cohort_wide[!is.na(period), .(
  mean_diff = mean(diff, na.rm = TRUE)
), by = .(period, cohort_group)]

bar_data[, period := factor(period, levels = c(
  "Pre-Mandate\n(1980\u20131988)",
  "Federal Mandates\n(1989\u20131998)",
  "Early Reform\n(1999\u20132008)",
  "Late Reform\n(2009\u20132018)"
))]

bar_data[, cohort_group := factor(cohort_group,
  levels = c("Pre-1980", "1980-1988", "1989-1998")
)]

fig_bar_periods <- ggplot(bar_data,
    aes(x = period, y = mean_diff, fill = cohort_group)) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.65,
    color = "white",
    linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
  geom_vline(xintercept = 2.5, linetype = "dashed",
    color = "gray40", linewidth = 0.5) +
  scale_fill_manual(
    values = cohort_palette,
    labels = c(
      "Pre-1980"  = "Pre-1980 (highest risk)",
      "1980-1988" = "1980\u20131988",
      "1989-1998" = "1989\u20131998 (placebo)"
    )
  ) +
  scale_y_continuous(
    labels = function(x) ifelse(x == 0, "0", paste0(x, " pp"))
  ) +
  labs(
    x = NULL,
    y = "Texas minus Control States (percentage points)",
    fill = "Install Cohort",
    caption = paste0(
      "Bars show mean annual difference in cohort-specific exit rates (TX \u2212 control state average).\n",
      "Exit rate = closures as share of active tanks in that cohort \u00d7 year. ",
      "Dashed line marks onset of Texas insurance reform."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 9, face = "bold"),
    legend.text        = element_text(size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption       = element_text(size = 8, color = "gray50", hjust = 0)
  )

ggsave(
  file.path(OUTPUT_FIGURES, "Fleet_ExitRate_Diff_Periods_Bar.png"),
  plot = fig_bar_periods,
  width = 8, height = 5, dpi = 300
)

#### S6 Define Primary Sample (panel_dt) ####

cat("========================================\n")
cat("S6: DEFINE PRIMARY SAMPLE\n")
cat("========================================\n\n")

# Include all vintage bins — pre-1989 cohorts retained.
# Drop installation-year rows (panel_year == install year): high closure
# rates there reflect failed inspections, not economic exit decisions.
# Cells with unknown wall/fuel (make_model_tank = NA) are excluded —
# they cannot be matched and break cell x year FE identification.

panel_dt <- tank_year_panel[
  !is.na(make_model_tank) &
  !is.na(did_term)        &
  state %in% STUDY_STATES &
  tstop > tstart          &
  panel_year > install_yr_int
]

stopifnot("Non-study states in panel_dt" =
  length(setdiff(unique(panel_dt$state), STUDY_STATES)) == 0)

n_ty       <- nrow(panel_dt)
n_tanks    <- uniqueN(panel_dt$tank_panel_id)
n_tx_tanks <- panel_dt[texas_treated == 1L, uniqueN(tank_panel_id)]
n_ct_tanks <- panel_dt[texas_treated == 0L, uniqueN(tank_panel_id)]
n_cells    <- uniqueN(panel_dt$make_model_tank)

log_step(sprintf("panel_dt: %s tank-years | %s tanks (TX=%s | CTL=%s) | %s cells",
  fmt_n(n_ty), fmt_n(n_tanks), fmt_n(n_tx_tanks), fmt_n(n_ct_tanks), fmt_n(n_cells)))

cohort_comp <- panel_dt[, .(
  n_tanks = uniqueN(tank_panel_id),
  n_tx    = uniqueN(tank_panel_id[texas_treated == 1L]),
  n_ctl   = uniqueN(tank_panel_id[texas_treated == 0L])
), by = mm_install_cohort][order(mm_install_cohort)]

log_step("\nInstall-year cohort composition:")
print(cohort_comp,130)
fwrite(cohort_comp, file.path(OUTPUT_TABLES, "Diag_InstallCohortComposition.csv"))

# Exclusion accounting
full_eligible <- tank_year_panel[
  !is.na(did_term) & state %in% STUDY_STATES &
  tstop > tstart & panel_year > install_yr_int
]
excl_na_cell <- full_eligible[is.na(make_model_tank), .N]
log_step(sprintf("\nExcluded for unknown cell: %s / %s (%s)",
  fmt_n(excl_na_cell), fmt_n(nrow(full_eligible)),
  fmt_pct(excl_na_cell / nrow(full_eligible))))
cat("\n")


#### S7 Cell Coverage Diagnostics ####

cat("========================================\n")
cat("S7: CELL COVERAGE DIAGNOSTICS\n")
cat("========================================\n\n")

cell_diag <- panel_dt[, .(
  n_total  = .N,
  n_tx     = sum(texas_treated == 1L),
  n_ctl    = sum(texas_treated == 0L),
  has_both = as.integer(sum(texas_treated == 1L) > 0L & sum(texas_treated == 0L) > 0L)
), by = .(make_model_tank, panel_year)]

cov_summ <- cell_diag[, .(
  total_cell_years      = .N,
  identified_cell_years = sum(has_both),
  pct_identified        = round(mean(has_both) * 100, 1),
  tank_years_total      = sum(n_total),
  tank_years_identified = sum(n_total * has_both),
  pct_ty_identified     = round(sum(n_total * has_both) / sum(n_total) * 100, 1)
)]

log_step(sprintf("Cell-years: %s total | %s identified (%s%%)",
  fmt_n(cov_summ$total_cell_years),
  fmt_n(cov_summ$identified_cell_years),
  cov_summ$pct_identified))
log_step(sprintf("Tank-years identified: %s / %s (%s%%)",
  fmt_n(cov_summ$tank_years_identified),
  fmt_n(cov_summ$tank_years_total),
  cov_summ$pct_ty_identified))

cov_by_cohort <- panel_dt[, .(
  n_tx     = sum(texas_treated == 1L),
  n_ctl    = sum(texas_treated == 0L),
  n_cells  = uniqueN(make_model_tank)
), by = mm_install_cohort][order(mm_install_cohort)]

log_step("\nTank-years by install-year cohort:")
print(cov_by_cohort)

fwrite(cell_diag,     file.path(OUTPUT_TABLES, "Diag_CellCoverage_Detail.csv"))
fwrite(cov_summ,      file.path(OUTPUT_TABLES, "Diag_CellCoverage_Summary.csv"))
fwrite(cov_by_cohort, file.path(OUTPUT_TABLES, "Diag_CellCoverage_ByCohort.csv"))
cat("\n")


# ---- Colors and theme ----
COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"
 
COL_SW <- "#08519c"   # single-walled
COL_DW <- "#74c476"   # double-walled
 
WALL_COLORS <- c("Single-Walled" = COL_SW, "Double-Walled" = COL_DW)
 
theme_ust <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      axis.title        = element_text(size = base_size, face = "bold"),
      axis.text         = element_text(size = base_size - 1L),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "gray90", linewidth = 0.3),
      legend.position   = "bottom",
      legend.title      = element_text(size = base_size - 1L, face = "bold"),
      legend.text       = element_text(size = base_size - 1L),
      plot.margin       = margin(t = 6, r = 10, b = 6, l = 6)
    )
}
 
 
#### Figure 1: Identification Heatmap ####
# install-cohort bin (5yr) x panel year, weighted share of tank-years
# in identified cells (has both TX and control).
 
cat("\n--- Building FigureApp_Cell_Heatmap.png ---\n")
 
# ---- Build cell-year identification flag ----
# A cell-year is identified if it has >= 1 TX tank-year AND >= 1 CTL tank-year.
cell_year <- panel_dt[
  !is.na(make_model_tank),
  .(
    n_tx    = sum(texas_treated == 1L),
    n_ctl   = sum(texas_treated == 0L),
    n_total = .N
  ),
  by = .(make_model_tank, panel_year, mm_install_cohort)
]
cell_year[, has_both := as.integer(n_tx > 0L & n_ctl > 0L)]
 
# Parse install year from cohort (numeric)
cell_year[, install_yr := as.integer(mm_install_cohort)]
 
# Drop junk cohorts; bin to 5-year intervals for display
cell_year_clean <- cell_year[
  !is.na(install_yr) & install_yr >= 1940L & install_yr <= 2019L
]
cell_year_clean[, cohort_bin := (install_yr %/% 5L) * 5L]
 
# Aggregate to cohort_bin x panel_year: weighted share identified
heat_dt <- cell_year_clean[, .(
  share_id = sum(n_total * has_both) / sum(n_total)
), by = .(cohort_bin, panel_year)]
 
heat_dt <- heat_dt[panel_year %between% c(1985L, 2020L)]
 
# Full grid: fill missing with NA so tiles are absent rather than wrong
heat_grid <- CJ(
  cohort_bin = sort(unique(heat_dt$cohort_bin)),
  panel_year = 1985L:2020L
)
heat_dt <- merge(heat_grid, heat_dt, by = c("cohort_bin", "panel_year"), all.x = TRUE)
 
# X-axis: show every 10 years for readability
cohort_breaks <- seq(1940L, 2015L, 10L)
 
fig_heatmap <- ggplot(heat_dt,
  aes(x = factor(cohort_bin), y = panel_year, fill = share_id)) +
  geom_tile(color = "white", linewidth = 0.1) +
  geom_hline(yintercept = 1998.5, color = "#222222",
             linewidth = 0.9, linetype = "dashed") +
  scale_fill_gradientn(
    colours  = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#084594"),
    limits   = c(0, 1),
    labels   = label_percent(accuracy = 1),
    name     = "Share of Tank-Years\nIdentified",
    na.value = "gray92"
  ) +
  scale_x_discrete(
    breaks = as.character(cohort_breaks),
    labels = as.character(cohort_breaks)
  ) +
  scale_y_continuous(breaks = seq(1985L, 2020L, 5L)) +
  labs(
    x = "Installation Year Cohort (5-Year Bins)",
    y = "Panel Year"
  ) +
  theme_ust() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8.5),
    panel.grid.major = element_blank(),
    legend.key.width  = unit(1.6, "cm"),
    legend.key.height = unit(0.4, "cm")
  )
 
ggsave(
  file.path(OUTPUT_FIGURES, "FigureApp_Cell_Heatmap.png"),
  plot   = fig_heatmap,
  width  = 11,
  height = 6.5,
  dpi    = 300
)
cat("  Saved: FigureApp_Cell_Heatmap.png\n")
 
 
#### Figure 2: Cell Size ECDF ####
# TX vs control distributions of tank-years per cell.
# Each observation = one cell in which the respective group appears.
 
cat("\n--- Building FigureApp_Cell_ECDF.png ---\n")
 
# Collapse to cell level: total tank-years per group
cell_level <- panel_dt[
  !is.na(make_model_tank),
  .(
    n_tx  = sum(texas_treated == 1L),
    n_ctl = sum(texas_treated == 0L)
  ),
  by = make_model_tank
]
 
# Long format: one row per (cell, group) where group has >= 1 tank-year
ecdf_tx  <- cell_level[n_tx  > 0L, .(tank_years = n_tx,  group = "Texas")]
ecdf_ctl <- cell_level[n_ctl > 0L, .(tank_years = n_ctl, group = "Control States")]
ecdf_long <- rbind(ecdf_tx, ecdf_ctl)
ecdf_long[, group := factor(group, levels = c("Texas", "Control States"))]
 
# Medians for reference lines
med_tx  <- median(ecdf_tx$tank_years)
med_ctl <- median(ecdf_ctl$tank_years)
 
fig_ecdf <- ggplot(ecdf_long, aes(x = tank_years, color = group, linetype = group)) +
  stat_ecdf(linewidth = 1.0, pad = FALSE) +
  geom_vline(xintercept = med_tx,  color = COL_TX,   linewidth = 0.55,
             linetype = "dotted", alpha = 0.8) +
  geom_vline(xintercept = med_ctl, color = COL_CTRL, linewidth = 0.55,
             linetype = "dotted", alpha = 0.8) +
  scale_x_log10(
    breaks = c(1L, 5L, 10L, 50L, 100L, 500L, 1000L, 5000L, 50000L),
    labels = label_comma(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    breaks = seq(0, 1, 0.1)
  ) +
  scale_color_manual(
    values = c("Texas" = COL_TX, "Control States" = COL_CTRL),
    name   = NULL
  ) +
  scale_linetype_manual(
    values = c("Texas" = "solid", "Control States" = "dashed"),
    name   = NULL
  ) +
  labs(
    x = "Tank-Years per Cell (log scale)",
    y = "Cumulative Share of Cells"
  ) +
  theme_ust() +
  theme(panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3))
 
ggsave(
  file.path(OUTPUT_FIGURES, "FigureApp_Cell_ECDF.png"),
  plot   = fig_ecdf,
  width  = 7.5,
  height = 5,
  dpi    = 300
)
cat("  Saved: FigureApp_Cell_ECDF.png\n")
 
 
#### Figure 3: TX/Control Balance Scatter ####
# Log-log scatter: TX tank-years vs CTL tank-years per identified cell.
# Color = wall type (most policy-relevant dimension).
 
cat("\n--- Building FigureApp_Cell_Balance.png ---\n")
 
# Cell-level collapse with wall type
cell_bal <- panel_dt[
  !is.na(make_model_tank),
  .(
    n_tx   = sum(texas_treated == 1L),
    n_ctl  = sum(texas_treated == 0L),
    mm_wall = mm_wall[1L]   # wall is a defining cell dimension — constant within cell
  ),
  by = make_model_tank
]
 
# Identified cells only
cell_bal_id <- cell_bal[n_tx > 0L & n_ctl > 0L]
 
# Axis limits: shared for interpretable 45-degree line
ax_max <- max(cell_bal_id[, c(n_tx, n_ctl)]) * 1.5
ax_min <- 0.8
 
fig_balance <- ggplot(cell_bal_id,
  aes(x = n_ctl, y = n_tx, color = mm_wall)) +
  # Reference lines
  geom_abline(slope = 1,   intercept = 0, color = "#444444",
              linewidth = 0.75, linetype = "dashed") +
  geom_abline(slope = 2,   intercept = 0, color = "#bbbbbb",
              linewidth = 0.4,  linetype = "dotted") +
  geom_abline(slope = 0.5, intercept = 0, color = "#bbbbbb",
              linewidth = 0.4,  linetype = "dotted") +
  # Points
  geom_point(alpha = 0.45, size = 1.6, stroke = 0) +
  # Marginal rugs
  geom_rug(aes(x = n_ctl), sides = "b", alpha = 0.12,
           linewidth = 0.3, color = COL_CTRL, inherit.aes = FALSE) +
  geom_rug(aes(y = n_tx),  sides = "l", alpha = 0.12,
           linewidth = 0.3, color = COL_TX,   inherit.aes = FALSE) +
  scale_x_log10(
    limits = c(ax_min, ax_max),
    breaks = c(1L, 10L, 100L, 1000L, 10000L, 100000L),
    labels = label_comma(accuracy = 1)
  ) +
  scale_y_log10(
    limits = c(ax_min, ax_max),
    breaks = c(1L, 10L, 100L, 1000L, 10000L, 100000L),
    labels = label_comma(accuracy = 1)
  ) +
  scale_color_manual(
    values = WALL_COLORS,
    name   = "Wall Type"
  ) +
  coord_fixed() +
  labs(
    x = "Control States: Tank-Years per Cell (log scale)",
    y = "Texas: Tank-Years per Cell (log scale)"
  ) +
  theme_ust() +
  theme(
    legend.key.size  = unit(0.5, "cm"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3)
  )
 
ggsave(
  file.path(OUTPUT_FIGURES, "FigureApp_Cell_Balance.png"),
  plot   = fig_balance,
  width  = 7,
  height = 7,
  dpi    = 300
)
cat("  Saved: FigureApp_Cell_Balance.png\n")
 
 
#### Figure 4: Identified vs. Unidentified Cell Sizes ####
# Two-panel figure:
#   Top:    ECDF of tank-years per cell, identified vs unidentified
#   Bottom: Bar chart — unidentified cells by type (TX-only vs CTL-only)
#           showing tank-year count and number of cells
 
cat("\n--- Building FigureApp_Cell_Unidentified.png ---\n")
 
# Classification: identified / TX-only / CTL-only
cell_level[, cell_type := fcase(
  n_tx > 0L & n_ctl > 0L, "Identified",
  n_tx > 0L & n_ctl == 0L, "Texas-Only\n(no control match)",
  n_tx == 0L & n_ctl > 0L, "Control-Only\n(no Texas match)",
  default = "Other"
)]
cell_level[, n_total := n_tx + n_ctl]
 
# ---- Top panel: ECDF ----
ecdf_top <- rbind(
  cell_level[cell_type == "Identified",
    .(n_total, status = "Identified")],
  cell_level[cell_type %in% c("Texas-Only\n(no control match)",
                              "Control-Only\n(no Texas match)"),
    .(n_total, status = "Unidentified")]
)
ecdf_top[, status := factor(status, levels = c("Identified", "Unidentified"))]
 
med_id  <- median(cell_level[cell_type == "Identified",  n_total])
med_uid <- median(cell_level[cell_type != "Identified" & cell_type != "Other", n_total])
 
fig_top <- ggplot(ecdf_top, aes(x = n_total, color = status, linetype = status)) +
  stat_ecdf(linewidth = 1.0, pad = FALSE) +
  geom_vline(xintercept = med_id,  color = "#333333",
             linewidth = 0.55, linetype = "dotted", alpha = 0.8) +
  geom_vline(xintercept = med_uid, color = "#888888",
             linewidth = 0.55, linetype = "dotted", alpha = 0.8) +
  scale_x_log10(
    breaks = c(1L, 10L, 100L, 1000L, 10000L, 100000L),
    labels = label_comma(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_color_manual(
    values = c("Identified" = "#222222", "Unidentified" = "#aaaaaa"),
    name   = NULL
  ) +
  scale_linetype_manual(
    values = c("Identified" = "solid", "Unidentified" = "dashed"),
    name   = NULL
  ) +
  labs(
    x = "Tank-Years per Cell (log scale)",
    y = "Cumulative Share of Cells"
  ) +
  theme_ust(base_size = 10) +
  theme(panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3))
 
# ---- Bottom panel: bar chart of unidentified cells by type ----
unid_summ <- cell_level[
  cell_type %in% c("Texas-Only\n(no control match)", "Control-Only\n(no Texas match)"),
  .(
    n_cells    = .N,
    n_tank_yrs = sum(n_total)
  ),
  by = cell_type
]
 
# Two bars side by side: n_cells and n_tank_yrs
# Normalize to a common axis using share of total
unid_long <- rbind(
  unid_summ[, .(cell_type, metric = "Cells",       value = n_cells)],
  unid_summ[, .(cell_type, metric = "Tank-Years",  value = n_tank_yrs)]
)
unid_long[, metric := factor(metric, levels = c("Cells", "Tank-Years"))]
 
# Clean up labels for display
unid_long[, cell_type_label := fifelse(
  cell_type == "Texas-Only\n(no control match)",
  "Texas-Only",
  "Control-Only"
)]
unid_long[, cell_type_label := factor(
  cell_type_label, levels = c("Texas-Only", "Control-Only")
)]
 
fig_bot <- ggplot(unid_long,
  aes(x = cell_type_label, y = value, fill = cell_type_label)) +
  geom_col(width = 0.55, color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = label_comma(accuracy = 1)(value)),
    vjust = -0.35, size = 3, fontface = "bold", color = "gray30"
  ) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_fill_manual(
    values = c("Texas-Only" = COL_TX, "Control-Only" = COL_CTRL),
    guide  = "none"
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    x = "Unidentified Cell Type",
    y = NULL
  ) +
  theme_ust(base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(size = 10, face = "bold")
  )
 
 library(patchwork)
 # ---- Combine with patchwork ----
fig_unid <- fig_top / fig_bot +
  plot_layout(heights = c(1.4, 1))
 
ggsave(
  file.path(OUTPUT_FIGURES, "FigureApp_Cell_Unidentified.png"),
  plot   = fig_unid,
  width  = 8,
  height = 8,
  dpi    = 300
)
cat("  Saved: FigureApp_Cell_Unidentified.png\n")
 
 
cat("\n========================================\n")
cat("02d_Cell_Overlap_AppendixFigures.R COMPLETE\n")
cat("Output/Figures:\n")
cat("  FigureApp_Cell_Heatmap.png\n")
cat("  FigureApp_Cell_ECDF.png\n")
cat("  FigureApp_Cell_Balance.png\n")
cat("  FigureApp_Cell_Unidentified.png\n")
cat("========================================\n\n")

#### S8 Build Exact-Date Cox Base ####

cat("========================================\n")
cat("S8: BUILD EXACT-DATE COX BASE\n")
cat("========================================\n\n")

# One row per tank. Time axis in days since 1970-01-01 (numeric).
# survSplit at REFORM_DAYS is done in the analysis script.
# All vintage bins included — no cohort cutoff.

exact_base <- study_tanks[
  !is.na(texas_treated)   &
  !is.na(make_model_tank) &
  state %in% STUDY_STATES &
  !is.na(tank_installed_date) &
  tank_installed_date < STUDY_END,
  .(tank_panel_id, panel_id, facility_id, state, texas_treated,
    mm_wall, mm_fuel, mm_capacity,
    mm_install_cohort, install_yr_int,
    release_det_deadline_yr,
    make_model_tank, make_model_noage, make_model_3dim,
    tank_installed_date,
    t_enter  = as.numeric(tank_installed_date),
    t_exit   = as.numeric(pmin(
      fifelse(!is.na(tank_closed_date), tank_closed_date, STUDY_END),
      STUDY_END)),
    failure  = as.integer(
      !is.na(tank_closed_date) &
      tank_closed_date <= STUDY_END &
      tank_closed_date >= tank_installed_date)
  )
][t_exit > t_enter]

# Drop installation-year exits (same churn exclusion as panel)
exact_base[, exit_yr_tmp := as.integer(format(as.Date(t_exit, origin = "1970-01-01"), "%Y"))]
exact_base <- exact_base[!(exit_yr_tmp == install_yr_int & failure == 1L)]
exact_base[, exit_yr_tmp := NULL]

exact_base[, `:=`(
  age_enter = 0,
  age_exit  = as.numeric(t_exit - t_enter) / 365.25
)]

log_step(sprintf("exact_base: %s tanks | %s events | %s install-year cohorts",
  fmt_n(nrow(exact_base)),
  fmt_n(sum(exact_base$failure)),
  uniqueN(exact_base$mm_install_cohort)))
cat("\n")


#### S9 Analysis Sample Construction ####

cat("========================================\n")
cat("S9: ANALYSIS SAMPLE CONSTRUCTION\n")
cat("========================================\n\n")

# The full make-model sample is the analysis population.
# Wall construction type enters identification only through the cell fixed
# effects and matching weights; it is not a pre-sample gate.
# Single-walled vs. double-walled heterogeneity is recovered via the
# did_x_sw interaction constructed in S11.

REFORM_DATE <- as.Date("1998-12-22")

analysis_tanks <- copy(panel_dt[!is.na(make_model_tank)])

# Treatment and event-study variables
analysis_tanks[, `:=`(
  did_term  = texas_treated * as.integer(panel_year >= 1999L),
  did_short = texas_treated * as.integer(panel_year >= 1999L & panel_year <= 2004L),
  did_long  = texas_treated * as.integer(panel_year >= 2005L),
  rel_year  = panel_year - 1999L
)]

# rel_year_es: binned [-12, 15]
analysis_tanks[, rel_year_es := fcase(
  rel_year <= -12L, -12L,
  rel_year >= 15L,   15L,
  default = as.integer(rel_year)
)]

# Anticipation spec variables (reform date = 1998)
analysis_tanks[, rel_year_early := panel_year - 1998L]
analysis_tanks[, rel_year_early := pmax(pmin(rel_year_early, 15L), -12L)]
analysis_tanks[, did_term_early := texas_treated * as.integer(panel_year >= 1998L)]
analysis_tanks[, deadline_sw    := as.integer(panel_year == 1998L) *
                                    as.integer(mm_wall == "Single-Walled")]

# Age at treatment date (years from install to 1999)
analysis_tanks[, age_at_treatment := 1999L - install_yr_int]

log_step(sprintf("Analysis sample: TX=%s fac / %s tanks | CTL=%s fac / %s tanks | %s tank-years",
  fmt_n(analysis_tanks[texas_treated == 1L, uniqueN(panel_id)]),
  fmt_n(analysis_tanks[texas_treated == 1L, uniqueN(tank_panel_id)]),
  fmt_n(analysis_tanks[texas_treated == 0L, uniqueN(panel_id)]),
  fmt_n(analysis_tanks[texas_treated == 0L, uniqueN(tank_panel_id)]),
  fmt_n(nrow(analysis_tanks))))

age_by_cohort <- analysis_tanks[, .(
  n_tanks    = uniqueN(tank_panel_id),
  median_age = as.integer(median(age_at_treatment))
), by = mm_install_cohort][order(mm_install_cohort)]
log_step("\nAge at treatment by install cohort:")
print(age_by_cohort)
cat("\n")


#### S10 Reform-Date Cross-Section and CEM Matching ####

cat("========================================\n")
cat("S10: CEM MATCHING\n")
cat("========================================\n\n")

# Matching is performed on the reform-date cross-section: tanks alive on
# December 22, 1998 (HB 2587 sunset date). A tank is alive on the reform
# date if it was installed before that date and either has no recorded
# closure or closed strictly after that date. This ensures matched pairs
# are contemporaneously at risk at reform onset and observed over the same
# post-reform window under the same pre-reform insurance regime.
#
# Pre-reform tank-years for matched tanks are retained in the panel for
# parallel trends estimation. The 1998-12-22 restriction governs matching
# eligibility only, not which years enter the regression.

# ---- Identify tanks alive on the reform date using exact dates ----
alive_at_reform <- study_tanks[
  tank_installed_date <= REFORM_DATE &
  (is.na(tank_closed_date) | tank_closed_date > REFORM_DATE),
  .(tank_panel_id)
]

cat(sprintf("Tanks alive on %s: %s\n",
  format(REFORM_DATE), fmt_n(nrow(alive_at_reform))))

# ---- Reform-date matching pool ----
# One row per tank. Characteristics are time-invariant within the cell
# definition so pulling from any panel year is equivalent; we use the
# reform-year record for clarity.
tank_chars <- unique(analysis_tanks[
  tank_panel_id %in% alive_at_reform$tank_panel_id,
  .(
    tank_panel_id, panel_id, state, texas_treated,
    mm_wall, mm_fuel, mm_capacity, mm_install_cohort, age_at_treatment
  )
])

cat(sprintf("Reform-date matching pool: %s tanks (%s TX, %s CTL)\n",
  fmt_n(nrow(tank_chars)),
  fmt_n(tank_chars[texas_treated == 1L, .N]),
  fmt_n(tank_chars[texas_treated == 0L, .N])))

# ---- Drop tanks with missing matching dimensions ----
tank_chars_complete <- tank_chars[
  !is.na(mm_wall) & !is.na(mm_fuel) &
  !is.na(mm_capacity) & !is.na(mm_install_cohort)
]

n_dropped <- nrow(tank_chars) - nrow(tank_chars_complete)
log_step(sprintf("Dropped %s tanks with missing matching covariates (%.2f%%)",
  fmt_n(n_dropped),
  100 * n_dropped / nrow(tank_chars)))

# MatchIt requires factor inputs
tank_chars_complete[, `:=`(
  mm_wall           = as.factor(mm_wall),
  mm_fuel           = as.factor(mm_fuel),
  mm_capacity       = as.factor(mm_capacity),
  mm_install_cohort = as.factor(mm_install_cohort)
)]

# ---- Exact (CEM) matching on four insurer-pricing dimensions ----
m_cem <- matchit(
  texas_treated ~ mm_wall + mm_fuel + mm_capacity + mm_install_cohort,
  data   = as.data.frame(tank_chars_complete),
  method = "exact"
)

cat(sprintf("CEM Matched TX:    %s tanks\n",
  fmt_n(sum(m_cem$weights[tank_chars_complete$texas_treated == 1] > 0))))
cat(sprintf("CEM Matched CTL:   %s tanks\n",
  fmt_n(sum(m_cem$weights[tank_chars_complete$texas_treated == 0] > 0))))
cat(sprintf("CEM Unmatched TX:  %s tanks\n",
  fmt_n(sum(m_cem$weights[tank_chars_complete$texas_treated == 1] == 0))))
cat(sprintf("CEM Unmatched CTL: %s tanks\n",
  fmt_n(sum(m_cem$weights[tank_chars_complete$texas_treated == 0] == 0))))

tank_chars_complete[, cem_weight := m_cem$weights]
cem_matched_ids <- tank_chars_complete[cem_weight > 0, tank_panel_id]

# ---- Build panel for CEM-matched tanks (all years, not just 1998) ----
matched_tanks <- merge(
  analysis_tanks[tank_panel_id %in% cem_matched_ids],
  tank_chars_complete[, .(tank_panel_id, cem_weight)],
  by    = "tank_panel_id",
  all.x = TRUE
)

log_step(sprintf("\nCEM matched TX:  %s / %s tanks (%.1f%%)",
  fmt_n(matched_tanks[texas_treated == 1L, uniqueN(tank_panel_id)]),
  fmt_n(tank_chars_complete[texas_treated == 1L, .N]),
  100 * matched_tanks[texas_treated == 1L, uniqueN(tank_panel_id)] /
        tank_chars_complete[texas_treated == 1L, .N]))

log_step(sprintf("CEM matched CTL: %s / %s tanks (%.1f%%)",
  fmt_n(matched_tanks[texas_treated == 0L, uniqueN(tank_panel_id)]),
  fmt_n(tank_chars_complete[texas_treated == 0L, .N]),
  100 * matched_tanks[texas_treated == 0L, uniqueN(tank_panel_id)] /
        tank_chars_complete[texas_treated == 0L, .N]))

log_step(sprintf("CEM matched tank-years: TX=%s | CTL=%s",
  fmt_n(matched_tanks[texas_treated == 1L, .N]),
  fmt_n(matched_tanks[texas_treated == 0L, .N])))

fwrite(
  tank_chars_complete[, .(tank_panel_id, cem_weight)],
  file.path(OUTPUT_TABLES, "Diag_CEM_Balance.csv")
)

# Save capacity-missing TX 1987 SW tanks for upstream registry investigation
missing_1987_sw <- tank_chars[
  texas_treated == 1L & mm_wall == "Single-Walled" &
  mm_install_cohort == 1987L & is.na(mm_capacity),
  .(tank_panel_id, panel_id)
]
fwrite(missing_1987_sw,
  file.path(OUTPUT_TABLES, "Diag_Missing_Capacity_1987_SW_TX.csv"))
cat(sprintf("Saved %s capacity-missing 1987 SW TX tank IDs for upstream review.\n",
  fmt_n(nrow(missing_1987_sw))))
cat("\n")


#### S10b Mahalanobis-PSM with Cross-Validated Propensity Scores (Robustness) ####

cat("========================================\n")
cat("S10b: MAHALANOBIS-PSM MATCHING\n")
cat("========================================\n\n")

# Imbens and Rubin (2015, Ch. 18): 1:1 nearest-neighbor without replacement.
# Propensity score caliper screens gross mismatches; Mahalanobis distance
# ranks candidates within the caliper on the full covariate vector.
#
# Matching pool: same reform-date cross-section as CEM (tanks alive on
# December 22, 1998). PSM uses continuous capacity and binary wall/fuel
# indicators in place of categorical bins, exploiting within-bin variation.
# Propensity scores estimated via 10-fold CV elastic net (glmnet, alpha=0.5);
# out-of-fold predictions used so every score comes from a model that never
# saw that observation. Caliper = 0.2 * SD(logit(p_hat)) on OOF scores
# (Austin 2011).
# County FE excluded: perfectly nested within treatment status.

library(glmnet)

# ---- Merge continuous capacity and binary indicators from study_tanks ----
# Use exact-date alive_at_reform pool to stay consistent with CEM sample.
cap_indicator_lookup <- unique(study_tanks[
  tank_panel_id %in% tank_chars$tank_panel_id,
  .(
    tank_panel_id,
    capacity,
    single_walled, double_walled,
    is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other
  )
])

tank_chars_complete <- merge(
  tank_chars_complete,
  cap_indicator_lookup,
  by    = "tank_panel_id",
  all.x = TRUE
)

indicator_cols <- c("single_walled", "double_walled",
                    "is_gasoline", "is_diesel",
                    "is_oil_kerosene", "is_jet_fuel", "is_other")

cat("Missing indicator check after merge:\n")
print(tank_chars_complete[, lapply(.SD, function(x) sum(is.na(x))),
      .SDcols = c("capacity", indicator_cols)])

# ---- Build PSM estimation sample ----
psm_data <- tank_chars_complete[
  !is.na(capacity) &
  !is.na(single_walled) &
  !is.na(is_gasoline)
]

cat(sprintf("\nPSM estimation sample: %s tanks (%s TX, %s CTL)\n",
  fmt_n(nrow(psm_data)),
  fmt_n(psm_data[texas_treated == 1L, .N]),
  fmt_n(psm_data[texas_treated == 0L, .N])))

cat(sprintf("Excluded for missing capacity: %s tanks\n",
  fmt_n(nrow(tank_chars_complete) - nrow(psm_data))))

# ---- Build sparse model matrix ----
psm_data[, mm_install_cohort_f := as.factor(mm_install_cohort)]

X <- sparse.model.matrix(
  ~ single_walled + double_walled +
    is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel + is_other +
    capacity + mm_install_cohort_f - 1,
  data = as.data.frame(psm_data)
)

y <- psm_data$texas_treated

cat(sprintf("Model matrix: %s rows x %s columns\n",
  fmt_n(nrow(X)), fmt_n(ncol(X))))

# ---- Step 1: Tune lambda via 10-fold CV elastic net ----
set.seed(42L)
cat("\nStep 1: Tuning lambda via 10-fold CV elastic net...\n")

cv_fit <- cv.glmnet(
  x            = X,
  y            = y,
  family       = "binomial",
  alpha        = 0.5,
  nfolds       = 10L,
  type.measure = "deviance",
  parallel     = FALSE
)

lambda_opt <- cv_fit$lambda.min
cat(sprintf("  Optimal lambda (min deviance): %.6f\n", lambda_opt))
cat(sprintf("  Lambda 1SE:                    %.6f\n", cv_fit$lambda.1se))

# ---- Step 2: Out-of-fold propensity scores ----
# Same seed replicates fold assignments from cv.glmnet. Each fold is fit
# at lambda_opt on the training data and predicts the held-out fold.
# Every observation's score comes from a model that never saw it.
cat("\nStep 2: Generating out-of-fold propensity scores...\n")

set.seed(42L)
n_obs   <- nrow(X)
n_fold  <- 10L
fold_id <- sample(rep(seq_len(n_fold), length.out = n_obs))

pscore_oof <- numeric(n_obs)

for (k in seq_len(n_fold)) {
  train_idx <- which(fold_id != k)
  test_idx  <- which(fold_id == k)

  fit_k <- glmnet(
    x      = X[train_idx, ],
    y      = y[train_idx],
    family = "binomial",
    alpha  = 0.5,
    lambda = lambda_opt
  )

  pscore_oof[test_idx] <- as.numeric(
    predict(fit_k, newx = X[test_idx, ], s = lambda_opt, type = "response")
  )

  cat(sprintf("  Fold %2d / %2d complete\n", k, n_fold))
}

# Winsorize away from 0/1 to keep logit finite
psm_data[, pscore       := pmax(pmin(pscore_oof, 0.999), 0.001)]
psm_data[, logit_pscore := log(pscore / (1 - pscore))]

cat("\nOut-of-fold propensity score distribution:\n")
print(psm_data[, .(
  min    = round(min(pscore),            4),
  p25    = round(quantile(pscore, 0.25), 4),
  median = round(median(pscore),         4),
  p75    = round(quantile(pscore, 0.75), 4),
  max    = round(max(pscore),            4)
), by = texas_treated])

# ---- Step 3: Imbens-Rubin optimal caliper ----
ir_caliper <- 0.2 * sd(psm_data$logit_pscore)
cat(sprintf("\nImbens-Rubin caliper (0.2 x SD logit OOF pscore): %.4f\n",
  ir_caliper))

# ---- Step 4: Propensity score overlap histogram ----
cat("\nStep 4: Building propensity score overlap histogram...\n")

psm_plot_dt <- psm_data[, .(
  pscore        = pscore,
  treatment_grp = fifelse(texas_treated == 1L, "Texas", "Control States")
)]
psm_plot_dt[, treatment_grp := factor(treatment_grp,
  levels = c("Texas", "Control States"))]

fig_pscore <- ggplot(psm_plot_dt, aes(x = pscore, fill = treatment_grp)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth  = 0.1,
    boundary  = 0,
    alpha     = 0.55,
    color     = "white",
    linewidth = 0.2,
    position  = "identity"
  ) +
  geom_vline(
    xintercept = 0.5,
    color      = "#222222",
    linewidth  = 0.9,
    linetype   = "dashed"
  ) +
  scale_fill_manual(
    values = c("Texas" = COL_TX, "Control States" = COL_CTRL),
    name   = NULL
  ) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Estimated Propensity Score (Pr[Texas])",
    y = "Density"
  ) +
  theme_ust() +
  theme(legend.position = "bottom")

ggsave(
  file.path(OUTPUT_FIGURES, "FigureApp_PSM_Overlap.png"),
  plot   = fig_pscore,
  width  = 8,
  height = 5,
  dpi    = 300
)
cat("  Saved: FigureApp_PSM_Overlap.png\n")

# ---- Step 5: Imbens-Rubin 1:1 Mahalanobis-PSM ----
# 1:1 without replacement per Imbens and Rubin (2015, Ch. 18).
# Each control tank can be matched to at most one treated tank.
# Unmatched treated units (those outside the caliper) are discarded --
# this is the common support enforcement step.
cat("\nStep 5: Running Imbens-Rubin 1:1 Mahalanobis-PSM...\n")

m_psm <- matchit(
  texas_treated ~ single_walled + double_walled +
                  is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel + is_other +
                  capacity + mm_install_cohort_f,
  data        = as.data.frame(psm_data),
  method      = "nearest",
  distance    = psm_data$logit_pscore,
  mahvars     = ~ single_walled + double_walled +
                  is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel + is_other +
                  capacity + mm_install_cohort_f,
  ratio       = 1L,
  replace     = FALSE,
  caliper     = ir_caliper
)

psm_tx_idx  <- psm_data$texas_treated == 1L
psm_ctl_idx <- psm_data$texas_treated == 0L

n_psm_tx_matched  <- sum(m_psm$weights[psm_tx_idx]  > 0)
n_psm_ctl_matched <- sum(m_psm$weights[psm_ctl_idx] > 0)
tx_unmatch_rate   <- mean(m_psm$weights[psm_tx_idx] == 0)

cat(sprintf("PSM Matched TX:    %s / %s tanks (%.1f%%)\n",
  fmt_n(n_psm_tx_matched), fmt_n(sum(psm_tx_idx)),
  100 * n_psm_tx_matched / sum(psm_tx_idx)))
cat(sprintf("PSM Matched CTL:   %s / %s tanks (%.1f%%)\n",
  fmt_n(n_psm_ctl_matched), fmt_n(sum(psm_ctl_idx)),
  100 * n_psm_ctl_matched / sum(psm_ctl_idx)))
cat(sprintf("PSM Unmatched TX:  %s tanks (%.1f%%) -- outside common support\n",
  fmt_n(sum(m_psm$weights[psm_tx_idx] == 0)), 100 * tx_unmatch_rate))
cat(sprintf("PSM Unmatched CTL: %s tanks -- not selected as nearest neighbor\n",
  fmt_n(sum(m_psm$weights[psm_ctl_idx] == 0))))

# Confirm 1:1 at tank level
stopifnot(n_psm_tx_matched == n_psm_ctl_matched)
cat(sprintf("\n1:1 confirmed: %s matched pairs.\n", fmt_n(n_psm_tx_matched)))

if (tx_unmatch_rate > 0.03) {
  warning(sprintf(
    "PSM unmatched TX rate = %.1f%%. Caliper (%.4f) may be too tight -- consider 0.25 x SD.",
    100 * tx_unmatch_rate, ir_caliper))
}

# ---- Step 6: Assign PSM weights and scores; build PSM panel ----
# psm_weight is defined only for tanks in the reform-date PSM sample.
# Tanks excluded for missing capacity receive NA (data limitation).
# Tanks in PSM sample but unmatched by caliper receive 0.
psm_weight_lookup <- data.table(
  tank_panel_id = psm_data$tank_panel_id,
  psm_weight    = m_psm$weights,
  pscore        = psm_data$pscore,
  logit_pscore  = psm_data$logit_pscore
)

# Append onto matched_tanks (CEM panel)
matched_tanks <- merge(
  matched_tanks,
  psm_weight_lookup,
  by    = "tank_panel_id",
  all.x = TRUE
)

# Tanks in CEM panel but outside PSM sample (missing capacity): leave as NA
# Tanks in PSM sample but unmatched: already 0 in m_psm$weights
matched_tanks[
  is.na(psm_weight) & tank_panel_id %in% psm_data$tank_panel_id,
  psm_weight := 0
]

# Verification
cat(sprintf("\nmatched_tanks carries cem_weight, psm_weight, pscore, logit_pscore.\n"))
cat(sprintf("Reform-date alive tanks used for matching: %s\n",
  fmt_n(nrow(alive_at_reform))))
cat(sprintf("Rows with cem_weight > 0:  %s tank-years\n",
  fmt_n(matched_tanks[cem_weight > 0, .N])))
cat(sprintf("Rows with psm_weight > 0:  %s tank-years\n",
  fmt_n(matched_tanks[psm_weight > 0, .N])))
cat(sprintf("Rows with psm_weight = NA: %s tank-years (missing capacity)\n",
  fmt_n(matched_tanks[is.na(psm_weight), .N])))

# Confirm 1:1 preserved in tank counts
cat(sprintf("\nUnique PSM-matched TX tanks in panel: %s\n",
  fmt_n(matched_tanks[texas_treated == 1L & psm_weight > 0,
    uniqueN(tank_panel_id)])))
cat(sprintf("Unique PSM-matched CTL tanks in panel: %s\n",
  fmt_n(matched_tanks[texas_treated == 0L & psm_weight > 0,
    uniqueN(tank_panel_id)])))

fwrite(psm_weight_lookup, file.path(OUTPUT_TABLES, "Diag_PSM_Pscores.csv"))
cat("\n")

#### S8b Kaplan-Meier Survival Figures ####

cat("========================================\n")
cat("S8b: KAPLAN-MEIER SURVIVAL FIGURES\n")
cat("========================================\n\n")

library(survival)
library(broom)

# ---- Helper: build KM data.table from a survfit object ----
tidy_km <- function(fit) {
  dt <- as.data.table(broom::tidy(fit))
  dt[, group := fifelse(grepl("Texas", strata), "Texas", "Control States")]
  dt[, group := factor(group, levels = c("Texas", "Control States"))]
  dt
}

# ---- Helper: run and label log-rank test ----
lr_label <- function(survdiff_obj, df = 1) {
  pval <- 1 - pchisq(survdiff_obj$chisq, df = df)
  sprintf("Log-rank p %s", ifelse(pval < 0.001, "< 0.001", sprintf("= %.3f", pval)))
}

# ---- Helper: standard KM ggplot ----
km_plot <- function(km_dt, label_text, free_y = FALSE) {
  ggplot(km_dt, aes(x = time, y = estimate, color = group, fill = group)) +
    geom_step(linewidth = 0.9) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.12, color = NA) +
    annotate("text", x = max(km_dt$time) * 0.05,
             y = 0.08, label = label_text,
             hjust = 0, size = 3.5, color = "gray30") +
    scale_color_manual(
      values = c("Texas" = COL_TX, "Control States" = COL_CTRL),
      name   = NULL
    ) +
    scale_fill_manual(
      values = c("Texas" = COL_TX, "Control States" = COL_CTRL),
      name   = NULL
    ) +
    scale_x_continuous(
      breaks = seq(0, 22, 2),
      labels = function(x) as.integer(1999 + x)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.1),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(x = "Year", y = "Survival Probability (Fraction Still Active)") +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3)
    )
}

# ---- Helper: vintage facet KM ggplot ----
km_vintage_plot <- function(km_dt_v, label_text) {
  ggplot(km_dt_v, aes(x = time, y = estimate, color = group, fill = group)) +
    geom_step(linewidth = 0.85) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.12, color = NA) +
    facet_wrap(~ vintage, ncol = 2, scales = "free_y") +
    annotate("text", x = -Inf, y = -Inf, label = label_text,
             hjust = -0.05, vjust = -0.5, size = 3, color = "gray40") +
    scale_color_manual(
      values = c("Texas" = COL_TX, "Control States" = COL_CTRL),
      name   = NULL
    ) +
    scale_fill_manual(
      values = c("Texas" = COL_TX, "Control States" = COL_CTRL),
      name   = NULL
    ) +
    scale_x_continuous(
      breaks = seq(0, 22, 4),
      labels = function(x) as.integer(1999 + x)
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(x = "Year", y = "Survival Probability (Fraction Still Active)") +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.spacing    = unit(14, "pt")
    )
}

# -----------------------------------------------------------------------
# Build base KM datasets
# -----------------------------------------------------------------------

# -- Full sample: alive at reform date --
km_base <- exact_base[
  t_enter <= REFORM_DAYS &
  (is.na(tank_closed_date) | as.numeric(tank_closed_date) > REFORM_DAYS)
]
km_base[, `:=`(
  t_stop_km = pmax((t_exit - REFORM_DAYS) / 365.25, 0),
  group     = fifelse(texas_treated == 1L, "Texas", "Control States")
)]
km_base <- km_base[t_stop_km > 0]

# Vintage bins (shared across all three samples)
assign_vintage <- function(dt) {
  dt[, vintage := fcase(
    install_yr_int <  1974L,                  "Pre-1974",
    install_yr_int %between% c(1974L, 1984L), "1974-1984",
    install_yr_int %between% c(1985L, 1988L), "1985-1988",
    install_yr_int %between% c(1989L, 1998L), "1989-1998 (placebo)",
    default = NA_character_
  )]
  dt[, vintage := factor(vintage, levels = c(
    "Pre-1974", "1974-1984", "1985-1988", "1989-1998 (placebo)"
  ))]
}
assign_vintage(km_base)

# -- CEM-matched: pull tank-level weights from tank_chars_complete --
cem_weights_tank <- unique(
  tank_chars_complete[cem_weight > 0, .(tank_panel_id, cem_weight)]
)
km_base_cem <- merge(
  km_base[tank_panel_id %in% cem_matched_ids],
  cem_weights_tank,
  by = "tank_panel_id",
  all.x = TRUE
)
km_base_cem[is.na(cem_weight), cem_weight := 1]

# -- PSM-matched: 1:1, unit weights, filter to matched IDs --
km_base_psm <- km_base[tank_panel_id %in% psm_matched_ids]


# -----------------------------------------------------------------------
# Figure 1a: 1998 Stock KM — Full Sample
# -----------------------------------------------------------------------
surv1   <- Surv(km_base$t_stop_km, km_base$failure)
fit1    <- survfit(surv1 ~ group, data = km_base)
lr1     <- lr_label(survdiff(surv1 ~ km_base$group))
km_dt1  <- tidy_km(fit1)

ggsave(
  file.path(OUTPUT_FIGURES, "KM_1998Stock_Full.png"),
  plot  = km_plot(km_dt1, lr1),
  width = 9, height = 6, dpi = 300
)
log_step("Saved: KM_1998Stock_Full.png")


# -----------------------------------------------------------------------
# Figure 1b: 1998 Stock KM — CEM Matched (weighted)
# -----------------------------------------------------------------------
surv1c  <- Surv(km_base_cem$t_stop_km, km_base_cem$failure)
fit1c   <- survfit(surv1c ~ group, data = km_base_cem,
                   weights = km_base_cem$cem_weight)
lr1c    <- lr_label(survdiff(surv1c ~ km_base_cem$group,
                             weights = km_base_cem$cem_weight))
km_dt1c <- tidy_km(fit1c)

ggsave(
  file.path(OUTPUT_FIGURES, "KM_1998Stock_CEM.png"),
  plot  = km_plot(km_dt1c, lr1c),
  width = 9, height = 6, dpi = 300
)
log_step("Saved: KM_1998Stock_CEM.png")


# -----------------------------------------------------------------------
# Figure 1c: 1998 Stock KM — PSM Matched (1:1, unweighted)
# -----------------------------------------------------------------------
surv1p  <- Surv(km_base_psm$t_stop_km, km_base_psm$failure)
fit1p   <- survfit(surv1p ~ group, data = km_base_psm)
lr1p    <- lr_label(survdiff(surv1p ~ km_base_psm$group))
km_dt1p <- tidy_km(fit1p)

ggsave(
  file.path(OUTPUT_FIGURES, "KM_1998Stock_PSM.png"),
  plot  = km_plot(km_dt1p, lr1p),
  width = 9, height = 6, dpi = 300
)
log_step("Saved: KM_1998Stock_PSM.png")


# -----------------------------------------------------------------------
# Figure 2a: Vintage Cohort KM — Full Sample
# -----------------------------------------------------------------------
km_v    <- km_base[!is.na(vintage)]
surv2   <- Surv(km_v$t_stop_km, km_v$failure)
fit2    <- survfit(surv2 ~ group + vintage, data = km_v)
lr2     <- lr_label(survdiff(surv2 ~ km_v$group + strata(km_v$vintage)))
km_dt2  <- tidy_km(fit2)
km_dt2[, vintage := factor(
  sub(".*vintage=(.*)", "\\1", strata),
  levels = levels(km_v$vintage)
)]

ggsave(
  file.path(OUTPUT_FIGURES, "KM_Vintage_Full.png"),
  plot  = km_vintage_plot(km_dt2, lr2),
  width = 10, height = 8, dpi = 300
)
log_step("Saved: KM_Vintage_Full.png")


# -----------------------------------------------------------------------
# Figure 2b: Vintage Cohort KM — CEM Matched (weighted)
# -----------------------------------------------------------------------
km_vc   <- km_base_cem[!is.na(vintage)]
surv2c  <- Surv(km_vc$t_stop_km, km_vc$failure)
fit2c   <- survfit(surv2c ~ group + vintage, data = km_vc,
                   weights = km_vc$cem_weight)
lr2c    <- lr_label(survdiff(surv2c ~ km_vc$group + strata(km_vc$vintage),
                             weights = km_vc$cem_weight))
km_dt2c <- tidy_km(fit2c)
km_dt2c[, vintage := factor(
  sub(".*vintage=(.*)", "\\1", strata),
  levels = levels(km_vc$vintage)
)]

ggsave(
  file.path(OUTPUT_FIGURES, "KM_Vintage_CEM.png"),
  plot  = km_vintage_plot(km_dt2c, lr2c),
  width = 10, height = 8, dpi = 300
)
log_step("Saved: KM_Vintage_CEM.png")


# -----------------------------------------------------------------------
# Figure 2c: Vintage Cohort KM — PSM Matched (1:1, unweighted)
# -----------------------------------------------------------------------
km_vp   <- km_base_psm[!is.na(vintage)]
surv2p  <- Surv(km_vp$t_stop_km, km_vp$failure)
fit2p   <- survfit(surv2p ~ group + vintage, data = km_vp)
lr2p    <- lr_label(survdiff(surv2p ~ km_vp$group + strata(km_vp$vintage)))
km_dt2p <- tidy_km(fit2p)
km_dt2p[, vintage := factor(
  sub(".*vintage=(.*)", "\\1", strata),
  levels = levels(km_vp$vintage)
)]

ggsave(
  file.path(OUTPUT_FIGURES, "KM_Vintage_PSM.png"),
  plot  = km_vintage_plot(km_dt2p, lr2p),
  width = 10, height = 8, dpi = 300
)
log_step("Saved: KM_Vintage_PSM.png")

cat("\n")

#### S11 Post-Match Variable Construction ####

cat("========================================\n")
cat("S11: POST-MATCH VARIABLES\n")
cat("========================================\n\n")

# age_at_treatment = 1999 - install_yr_int: age at the first post-reform
# year, consistent with did_term activating at panel_year >= 1999.
# Median computed on the reform-date-alive CEM matched sample only.
med_age <- median(matched_tanks$age_at_treatment, na.rm = TRUE)
cat(sprintf("Median age at treatment (matched sample): %.0f years\n\n", med_age))

matched_tanks[, `:=`(
  above_median_age = as.integer(age_at_treatment >= med_age),
  below_median_age = as.integer(age_at_treatment <  med_age),
  single_wall      = as.integer(mm_wall == "Single-Walled")
)]

matched_tanks[, `:=`(
  did_x_old    = did_term * above_median_age,
  did_x_sw     = did_term * single_wall,
  did_x_sw_old = did_term * single_wall * above_median_age,
  sw_x_old     = single_wall * above_median_age
)]

age_label <- sprintf("Above Median Age (>= %.0f yr)", med_age)

# Control group means for regression table footnotes
ctrl_means <- matched_tanks[texas_treated == 0L, .(
  pre_reform = mean(closure_event[panel_year %between% c(1992L, 1998L)], na.rm = TRUE),
  post_all   = mean(closure_event[panel_year >= 1999L],                  na.rm = TRUE),
  post_short = mean(closure_event[panel_year %between% c(1999L, 2004L)], na.rm = TRUE),
  post_long  = mean(closure_event[panel_year >= 2005L],                  na.rm = TRUE)
)]

cat("Control closure rates (per 1,000 tank-years):\n")
cat(sprintf("  Pre-reform (1992-1998):  %.1f\n", ctrl_means$pre_reform * 1000))
cat(sprintf("  Post-reform (all):       %.1f\n", ctrl_means$post_all   * 1000))
cat(sprintf("  Post short (1999-2004):  %.1f\n", ctrl_means$post_short * 1000))
cat(sprintf("  Post long  (2005-2020):  %.1f\n", ctrl_means$post_long  * 1000))

ctrl_mean_post <- ctrl_means$post_all

ctrl_means_dt <- data.table(
  period = c("pre_1992_1998", "post_all", "post_1999_2004", "post_2005_2020"),
  rate   = unlist(ctrl_means)
)
fwrite(ctrl_means_dt, file.path(OUTPUT_TABLES, "Diag_ControlMeans.csv"))

# PSM weight diagnostic
# NA  = excluded for missing capacity (data limitation -- not a matching failure)
# 0   = in PSM sample but outside caliper (common support enforcement)
# > 0 = matched; always use !is.na(psm_weight) & psm_weight > 0 to subset
cat(sprintf("\nPSM weight summary in matched_tanks:\n"))
cat(sprintf("  psm_weight > 0  (matched):          %s tank-years\n",
  fmt_n(matched_tanks[!is.na(psm_weight) & psm_weight > 0,  .N])))
cat(sprintf("  psm_weight == 0 (outside support):  %s tank-years\n",
  fmt_n(matched_tanks[!is.na(psm_weight) & psm_weight == 0, .N])))
cat(sprintf("  psm_weight NA   (missing capacity): %s tank-years\n",
  fmt_n(matched_tanks[is.na(psm_weight), .N])))
cat("\n")


#### S12 Save Outputs as CSV ####

cat("========================================\n")
cat("S12: SAVE OUTPUTS\n")
cat("========================================\n\n")

fwrite(panel_dt,      file.path(ANALYSIS_DIR, "panel_dt.csv"))
fwrite(exact_base,    file.path(ANALYSIS_DIR, "exact_base.csv"))
fwrite(matched_tanks, file.path(ANALYSIS_DIR, "matched_tanks.csv"))

# CEM matched IDs: reform-date cross-section, exact-matched on four
# insurer-pricing dimensions. Primary identification sample.
fwrite(
  data.table(tank_panel_id = cem_matched_ids),
  file.path(ANALYSIS_DIR, "matched_ids.csv")
)

# PSM matched IDs: reform-date cross-section, Imbens-Rubin 1:1
# Mahalanobis-PSM. Robustness sample.
psm_matched_ids <- matched_tanks[
  !is.na(psm_weight) & psm_weight > 0,
  unique(tank_panel_id)
]
fwrite(
  data.table(tank_panel_id = psm_matched_ids),
  file.path(ANALYSIS_DIR, "psm_matched_ids.csv")
)

fwrite(
  data.table(
    scalar      = c("med_age", "ctrl_mean_post", "n_ty", "n_tanks",
              "n_tx_tanks", "n_ct_tanks", "n_cells",
              "reform_date", "n_alive_at_reform",
              "n_cem_matched", "n_psm_matched"),
    value = c(med_age, ctrl_mean_post, n_ty, n_tanks,
              n_tx_tanks, n_ct_tanks, n_cells,
              as.numeric(REFORM_DATE),
              nrow(alive_at_reform),
              length(cem_matched_ids),
              length(psm_matched_ids)),
    label = c(age_label, rep("", 10))
  ),
  file.path(ANALYSIS_DIR, "panel_meta.csv")
)

cat(sprintf("Saved to %s:\n", ANALYSIS_DIR))
cat("  panel_dt.csv        —", fmt_n(nrow(panel_dt)),          "rows\n")
cat("  exact_base.csv      —", fmt_n(nrow(exact_base)),        "rows\n")
cat("  matched_tanks.csv   —", fmt_n(nrow(matched_tanks)),     "rows\n")
cat("  matched_ids.csv     —", fmt_n(length(cem_matched_ids)), "CEM-matched tank IDs\n")
cat("  psm_matched_ids.csv —", fmt_n(length(psm_matched_ids)), "PSM-matched tank IDs\n")
cat("  panel_meta.csv      — scalar constants\n")

cat("\n========================================\n")
cat("02b_Panel_Build.R COMPLETE\n")
cat("========================================\n\n")