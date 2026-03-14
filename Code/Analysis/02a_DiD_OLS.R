################################################################################
# 02a_DiD_FacBehavior.R
# Texas UST Insurance Reform — Facility-Level Behavioral Response
#
# ANALYSIS LEVEL: Facility-year panel
#
# RESEARCH QUESTIONS:
#   RQ1 — Conditional Closure Response
#         Given a facility closes a tank, does actuarial pricing shift the
#         response toward retrofit (replacement) or toward exit?
#         Outcomes: replacement_closure_year, permanent_closure_year, exit_flag
#         Sample:   conditional-on-closure (closure_year == 1) + unconditional
#
#   RQ2 — Portfolio Investment Response
#         When facilities retrofit, do they expand or contract their portfolio?
#         Do they upgrade wall quality?
#         Outcomes: net_tank_change, capacity_change_year, single_to_double_year,
#                   n_installs, double_walled_installed_year
#         Sample:   conditional-on-retrofit (retrofit_year == 1) + unconditional
#
#   RQ3 — LUST Incidence
#         Does actuarial pricing change the rate of reported leaks?
#         Outcomes: leak_year (total)
#
#   RQ3.1 — LUST Decomposition: Mechanical vs. Standalone
#         Are changes in LUSTs driven by more tank closures (mechanical —
#         closure inspections reveal latent leaks) or by genuine changes in
#         leak incidence independent of closures?
#         Outcomes: tank_closure_revealed  (LUST within 0-60d of closure)
#                   lust_standalone         (LUST with no associated closure)
#                   tank_closure_known_leak (pre-existing leak, closure follows)
#
# NOTE on conditional samples (RQ1, RQ2):
#   Subsetting to closure_year == 1 or retrofit_year == 1 conditions on a
#   variable that may itself be affected by treatment.  The subsample DiD
#   estimates the ATT among closers/retrofitters — an interesting economic
#   quantity, but NOT the same as the unconditional DiD.  Both are reported.
#   See S4 and S5 comments for discussion.
#
# IDENTIFICATION:
#   Treatment:    did_term = texas_treated × I(panel_year >= 1999)
#   Unit FE:      panel_id (facility intercept)
#   Cell-year FE: make_model_fac^panel_year
#   Clustering:   state level (1 TX + 16 controls = 17 clusters)
#
# DATA INPUT:
#   Data/Processed/facility_leak_behavior_annual.csv
#
# SECTIONS:
#   S1   Setup and Constants
#   S2   Load Data and Construct Variables
#   S3   Sample Construction and Diagnostics
#   S4   Conditional Closure Response (RQ1)
#   S5   Portfolio Investment Response (RQ2)
#   S6   LUST Decomposition (RQ3 / RQ3.1)
#   S7   Event Studies
#   S8   HTE by Wall Type
#   S9   HTE by Oldest-Tank Age
#   S10  Robustness
#   S11  Export
################################################################################


#### S1: Setup and Constants ####

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
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

# ---- Study parameters ----
POST_YEAR   <- 1999L
PANEL_START <- 1990L
PANEL_END   <- 2018L

# ---- Mandate parameters ----
MANDATE_ANNOUNCE_YR <- 1987L

# ---- Study states (MUST MATCH 02b_DiD_Survival.R) ----
CONTROL_STATES <- c(
  "AL", "AR", "CO", "GA", "ID", "KS", "KY",
  "MD", "MN", "MO", "NC", "OH",
  "OK", "TN", "VA", "WV"
)
STUDY_STATES <- c("TX", CONTROL_STATES)

# ---- Factor levels ----
AGE_BIN_LEVELS <- c("0-2yr", "3-5yr", "6-8yr", "9-11yr",
                     "12-14yr", "15-19yr", "20yr-Plus")
WALL_LEVELS    <- c("All-SW", "Mixed-Wall", "All-DW")

AGE_BIN_BREAKS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
AGE_BIN_LABELS <- c("0-2", "3-5", "6-8", "9-11",
                     "12-14", "15-17", "18-20", "21-23", "24+")

# ---- Colors ----
COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"

# ---- Helpers ----
log_step <- function(msg, indent = 0L) cat(strrep("  ", indent), msg, "\n", sep = "")
fmt_n    <- function(n) format(n, big.mark = ",", scientific = FALSE)

extract_did <- function(m, tvar = "did_term") {
  ct  <- coeftable(m)
  idx <- grep(tvar, rownames(ct), fixed = TRUE)[1]
  list(beta = ct[idx, "Estimate"], se = ct[idx, "Std. Error"],
       p = ct[idx, "Pr(>|t|)"], n = nobs(m))
}

theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(plot.title = element_blank(), plot.subtitle = element_blank(),
          plot.caption = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(), legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = base_size),
          axis.text  = element_text(size = base_size - 1),
          strip.text = element_text(face = "bold", size = base_size))
}
theme_set(theme_pub())

plot_es <- function(model, ylab = "Effect on Annual Probability",
                    xlim_lo = -8L, xlim_hi = NULL, filename = NULL) {
  ct <- as.data.table(tidy(model, conf.int = TRUE))
  ct <- ct[grepl("rel_year", term)]
  ct[, rel_year := as.numeric(gsub(".*::(-?[0-9]+).*", "\\1", term))]
  ct <- rbind(ct,
    data.table(term = "ref", estimate = 0, std.error = 0,
               conf.low = 0, conf.high = 0, rel_year = -1L),
    fill = TRUE)
  setorder(ct, rel_year)
  ct[, period := fcase(rel_year < -1, "Pre", rel_year == -1, "Ref",
                       default = "Post")]
  xh <- if (is.null(xlim_hi)) max(ct$rel_year) else xlim_hi

  p <- ggplot(ct[rel_year %between% c(xlim_lo, xh)],
              aes(x = rel_year, y = estimate)) +
    annotate("rect", xmin = -Inf, xmax = -0.5, ymin = -Inf, ymax = Inf,
             fill = "grey90", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50",
               linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.6) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.12, fill = "grey40") +
    geom_line(color = "grey40", linewidth = 0.4, alpha = 0.6) +
    geom_point(aes(color = period), size = 2.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = period),
                  width = 0.25, linewidth = 0.5) +
    scale_color_manual(values = c(Pre = COL_CTRL, Post = COL_TX,
                                  Ref = "black"), guide = "none") +
    labs(x = "Years Relative to Reform (1999)", y = ylab) +
    theme_pub()

  if (!is.null(filename)) {
    ggsave(filename, p, width = 10, height = 5.5, dpi = 300, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p,
           width = 10, height = 5.5, device = cairo_pdf)
  }
  invisible(list(plot = p, data = ct))
}


#### S2: Load Data and Construct Variables ####

cat("\n========================================\n")
cat("S2: LOAD DATA AND CONSTRUCT VARIABLES\n")
cat("========================================\n\n")

annual_data <- fread(
  here("Data", "Processed", "facility_leak_behavior_annual.csv"),
  na.strings = c("", "NA", "N/A")
)
log_step(sprintf("Loaded: %s rows, %s facilities, %s states",
                 fmt_n(nrow(annual_data)),
                 fmt_n(uniqueN(annual_data$panel_id)),
                 uniqueN(annual_data$state)))

# ---- Override treatment assignment to match study design ----
annual_data[, texas_treated := fcase(
  state == "TX",              1L,
  state %in% CONTROL_STATES, 0L,
  default = NA_integer_
)]

# ---- Core DiD variables ----
annual_data[, did_term       := texas_treated * as.integer(panel_year >= POST_YEAR)]
annual_data[, rel_year_1999  := panel_year - POST_YEAR]
rel_max <- PANEL_END - POST_YEAR
annual_data[, rel_year_bin   := pmax(pmin(rel_year_1999, rel_max), -8L)]
annual_data[, closure_event  := closure_year]   # alias for 02b consistency

# ---- Time-varying age bin ----
annual_data[, age_bin := factor(
  cut(avg_tank_age,
      breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS,
      right = FALSE, include.lowest = TRUE),
  levels = AGE_BIN_LABELS)]
annual_data[, age_bin := relevel(age_bin, ref = "0-2")]

# ---- LUST decomposition variables ----
# lust_standalone: a LUST was reported this year but NOT revealed by a tank
#   closure (i.e., not within the 0-60d post-closure window).  This is the
#   "non-mechanical" component — changes here cannot be attributed to the
#   reform increasing the number of closure inspections.
annual_data[, lust_standalone := as.integer(
  leak_year == 1 & tank_closure_revealed == 0
)]

# lust_closure_induced: alias for tank_closure_revealed (LUST within 0-60d of
#   a closure).  Changes here are *consistent with* a mechanical story: more
#   closures → more inspections → more revealed latent leaks.
annual_data[, lust_closure_induced := tank_closure_revealed]

# ---- Portfolio quality change ----
# single_to_double_year is already in the panel builder output (binary).
# Confirm the wide-format name used downstream.
if (!"single_to_double_year" %in% names(annual_data) &&
     "n_single_to_double" %in% names(annual_data))
  annual_data[, single_to_double_year := as.integer(n_single_to_double > 0)]

# ---- Verify required columns ----
required_cols <- c(
  # Identifiers
  "panel_id", "state", "panel_year", "texas_treated",
  # RQ1 — Conditional closure response
  "closure_year", "replacement_closure_year", "permanent_closure_year",
  "exit_flag", "exit_no_leak", "exit_with_leak",
  "retrofit_no_exit", "exit_no_retrofit", "both_exit_retrofit",
  # RQ2 — Portfolio investment
  "net_tank_change", "capacity_change_year",
  "n_installs", "retrofit_year", "single_to_double_year",
  "double_walled_installed_year",
  # RQ3 — LUST
  "leak_year", "tank_closure_revealed", "tank_closure_known_leak",
  "tank_closure_clean",
  # Cell and covariates
  "make_model_fac", "fac_wall", "fac_fuel", "fac_oldest_age_bin",
  "facility_first_install_yr", "had_pre1989_tanks"
)
missing <- setdiff(required_cols, names(annual_data))
if (length(missing) > 0)
  stop(sprintf("Missing columns: %s\n  Re-run 10_Build_Annual_Panel_Optimized.R",
               paste(missing, collapse = ", ")))
log_step(sprintf("  ✓ All %d required columns present", length(required_cols)), 1)
cat("\n")


#### S3: Sample Construction and Diagnostics ####

cat("========================================\n")
cat("S3: SAMPLE CONSTRUCTION\n")
cat("========================================\n\n")

mm_fac_primary <- annual_data[
  !is.na(make_model_fac)     &
  !is.na(fac_oldest_age_bin) &
  !is.na(texas_treated)      &
  !is.na(did_term)           &
  panel_year %between% c(PANEL_START, PANEL_END)
]

leaked <- setdiff(unique(mm_fac_primary$state), STUDY_STATES)
if (length(leaked) > 0)
  stop(sprintf("Non-study states in sample: %s", paste(leaked, collapse = ", ")))

n_fy     <- nrow(mm_fac_primary)
n_fac    <- uniqueN(mm_fac_primary$panel_id)
n_tx     <- mm_fac_primary[texas_treated == 1, uniqueN(panel_id)]
n_ctl    <- mm_fac_primary[texas_treated == 0, uniqueN(panel_id)]
n_cells  <- uniqueN(mm_fac_primary$make_model_fac)
n_states <- uniqueN(mm_fac_primary$state)

log_step("Primary sample (mm_fac_primary):")
log_step(sprintf("  Facility-years : %s  (TX: %s | CTL: %s)",
                 fmt_n(n_fy),
                 fmt_n(mm_fac_primary[texas_treated == 1, .N]),
                 fmt_n(mm_fac_primary[texas_treated == 0, .N])), 1)
log_step(sprintf("  Facilities     : %s  (TX: %s | CTL: %s)",
                 fmt_n(n_fac), fmt_n(n_tx), fmt_n(n_ctl)), 1)
log_step(sprintf("  States         : %d  (1 TX + %d control)",
                 n_states, n_states - 1L), 1)
log_step(sprintf("  Cells          : %s", fmt_n(n_cells)), 1)
log_step(sprintf("  Panel window   : %d – %d", PANEL_START, PANEL_END), 1)

# ---- Conditional subsamples ----
# Used in S4 and S5.  Size reported here for diagnostics.
mm_closure_sample <- mm_fac_primary[closure_year == 1]
mm_retrofit_sample <- mm_fac_primary[retrofit_year == 1]

log_step(sprintf("  Closure-year subsample  : %s facility-years (%s TX, %s CTL)",
  fmt_n(nrow(mm_closure_sample)),
  fmt_n(mm_closure_sample[texas_treated == 1, .N]),
  fmt_n(mm_closure_sample[texas_treated == 0, .N])), 1)
log_step(sprintf("  Retrofit-year subsample : %s facility-years (%s TX, %s CTL)",
  fmt_n(nrow(mm_retrofit_sample)),
  fmt_n(mm_retrofit_sample[texas_treated == 1, .N]),
  fmt_n(mm_retrofit_sample[texas_treated == 0, .N])), 1)

# ---- Cell coverage ----
fac_cell_diag <- mm_fac_primary[, .(
  n_total  = .N,
  has_both = as.integer(
    sum(texas_treated == 1L) > 0 & sum(texas_treated == 0L) > 0)
), by = .(make_model_fac, panel_year)]
cell_coverage <- fac_cell_diag[, .(
  pct_fac_years_identified = round(
    sum(n_total * has_both) / sum(n_total) * 100, 1)
)]
log_step(sprintf("  Cell coverage  : %.1f%% of facility-years in identified cells",
                 cell_coverage$pct_fac_years_identified), 1)
if (cell_coverage$pct_fac_years_identified < 70)
  warning(sprintf("Only %.1f%% in identified cells (target: 70%%).",
                  cell_coverage$pct_fac_years_identified))
rm(fac_cell_diag)
cat("\n")


#### S4: Conditional Closure Response (RQ1) ####

cat("========================================\n")
cat("S4: CONDITIONAL CLOSURE RESPONSE (RQ1)\n")
cat("========================================\n\n")

# ---- 4a: Unconditional DiD ----
# Estimates the average treatment effect on the probability of each closure
# type in any given year — the extensive margin question.  These coefficients
# are the cleanest causal estimates and should be the headline results.

m_rq1_replacement <- feols(
  replacement_closure_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_rq1_permanent <- feols(
  permanent_closure_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_rq1_exit <- feols(
  exit_flag ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# Exit decompositions (unconditional; no closure conditioning here)
m_rq1_exit_noleak <- feols(
  exit_no_leak ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_rq1_exit_withleak <- feols(
  exit_with_leak ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_rq1_retrofit_noexit <- feols(
  retrofit_no_exit ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# ---- 4b: Conditional-on-closure subsample DiD ----
# Conditions on closure_year == 1.  Identifies: given a tank was closed, did
# actuarial pricing change whether that closure was a replacement or a
# permanent exit?
#
# SELECTION CAVEAT: treatment itself may affect who closes (see 02b).  The
# subsample is endogenously selected; interpret as ATT among closers, not as
# an unconditional causal effect.  Prefer 4a for causal inference; use 4b
# for economic characterization of the closure margin.

m_rq1_cond_replacement <- feols(
  replacement_closure_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_closure_sample, cluster = ~state)

m_rq1_cond_permanent <- feols(
  permanent_closure_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_closure_sample, cluster = ~state)

m_rq1_cond_exit <- feols(
  exit_flag ~ did_term | panel_id + make_model_fac^panel_year,
  mm_closure_sample, cluster = ~state)

# ---- Report ----
rq1_outcomes_uncond <- list(
  list(m = m_rq1_replacement,    label = "Replacement closure (uncond)"),
  list(m = m_rq1_permanent,      label = "Permanent closure (uncond)"),
  list(m = m_rq1_exit,           label = "Exit (uncond)"),
  list(m = m_rq1_exit_noleak,    label = "Exit, no leak (uncond)"),
  list(m = m_rq1_exit_withleak,  label = "Exit, with leak (uncond)"),
  list(m = m_rq1_retrofit_noexit,label = "Retrofit, no exit (uncond)")
)
rq1_outcomes_cond <- list(
  list(m = m_rq1_cond_replacement, label = "Replacement closure | closed"),
  list(m = m_rq1_cond_permanent,   label = "Permanent closure   | closed"),
  list(m = m_rq1_cond_exit,        label = "Exit                | closed")
)

rq1_dt <- rbindlist(lapply(c(rq1_outcomes_uncond, rq1_outcomes_cond), function(o) {
  d <- extract_did(o$m)
  data.table(Outcome = o$label, Est = d$beta, SE = d$se, P = d$p, N = d$n)
}))

log_step("RQ1 — Closure response DiD:")
for (i in seq_len(nrow(rq1_dt))) {
  r <- rq1_dt[i]
  log_step(sprintf("  %-38s: %+.4f  (%.4f)  p=%.3f",
                   r$Outcome, r$Est, r$SE, r$P), 1)
}
fwrite(rq1_dt, file.path(OUTPUT_TABLES, "Table_RQ1_ClosureResponse.csv"))
cat("\n")


#### S5: Portfolio Investment Response (RQ2) ####

cat("========================================\n")
cat("S5: PORTFOLIO INVESTMENT RESPONSE (RQ2)\n")
cat("========================================\n\n")

# ---- 5a: Unconditional DiD on portfolio outcomes ----
# These are the correct causal estimates for the full population of incumbent
# facilities.  They capture BOTH the decision to invest AND the scale of
# investment, averaging over non-investing years.

# Tank count
m_rq2_nettank <- feols(
  net_tank_change ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# Capacity
m_rq2_capacity <- feols(
  capacity_change_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# New installs (extensive and count)
m_rq2_retrofit <- feols(
  retrofit_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_rq2_ninstalls <- feols(
  n_installs ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# Wall quality upgrade (single-to-double conversion)
m_rq2_std <- feols(
  single_to_double_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_rq2_dw_installed <- feols(
  double_walled_installed_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# ---- 5b: Conditional-on-retrofit subsample DiD ----
# Among facility-years where a replacement/retrofit occurred, did treatment
# change the scale or quality of the portfolio response?
# E.g., did TX retrofitters expand their tank count more than control retrofitters?
#
# SELECTION CAVEAT: same as S4b.  Retrofit incidence itself is treatment-affected.
# Interpret as a characterization of what retrofitters did, not as an
# unconditional causal effect.

m_rq2_cond_nettank <- feols(
  net_tank_change ~ did_term | panel_id + make_model_fac^panel_year,
  mm_retrofit_sample, cluster = ~state)

m_rq2_cond_capacity <- feols(
  capacity_change_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_retrofit_sample, cluster = ~state)

m_rq2_cond_std <- feols(
  single_to_double_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_retrofit_sample, cluster = ~state)

# ---- Report ----
rq2_outcomes_uncond <- list(
  list(m = m_rq2_nettank,      label = "Net tank change (uncond)"),
  list(m = m_rq2_capacity,     label = "Capacity change (uncond)"),
  list(m = m_rq2_retrofit,     label = "Retrofit year (uncond)"),
  list(m = m_rq2_ninstalls,    label = "N installs (uncond)"),
  list(m = m_rq2_std,          label = "SW→DW upgrade (uncond)"),
  list(m = m_rq2_dw_installed, label = "DW installed (uncond)")
)
rq2_outcomes_cond <- list(
  list(m = m_rq2_cond_nettank,  label = "Net tank change | retrofit"),
  list(m = m_rq2_cond_capacity, label = "Capacity change | retrofit"),
  list(m = m_rq2_cond_std,      label = "SW→DW upgrade   | retrofit")
)

rq2_dt <- rbindlist(lapply(c(rq2_outcomes_uncond, rq2_outcomes_cond), function(o) {
  d <- extract_did(o$m)
  data.table(Outcome = o$label, Est = d$beta, SE = d$se, P = d$p, N = d$n)
}))

log_step("RQ2 — Portfolio investment DiD:")
for (i in seq_len(nrow(rq2_dt))) {
  r <- rq2_dt[i]
  log_step(sprintf("  %-36s: %+.4f  (%.4f)  p=%.3f",
                   r$Outcome, r$Est, r$SE, r$P), 1)
}
fwrite(rq2_dt, file.path(OUTPUT_TABLES, "Table_RQ2_PortfolioInvestment.csv"))
cat("\n")


#### S6: LUST Decomposition (RQ3 / RQ3.1) ####

cat("========================================\n")
cat("S6: LUST DECOMPOSITION (RQ3 / RQ3.1)\n")
cat("========================================\n\n")

# ---- Total LUST incidence (RQ3) ----
m_rq3_leak <- feols(
  leak_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_rq3_leak_age <- feols(
  leak_year ~ did_term + age_bin | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# ---- LUST decomposition (RQ3.1) ----
# The question: are more LUSTs mechanical (closure-inspection-driven) or
# genuine changes in leak incidence?
#
# lust_closure_induced (= tank_closure_revealed):
#   LUST reported 0–60 days AFTER a tank closure.  The most natural explanation
#   is that the closure inspection revealed a latent leak.  If treatment
#   increases closures, we mechanically expect more of these even with no
#   underlying change in leak rates.  An increase here is CONSISTENT with a
#   pure mechanical story.
#
# lust_standalone (= leak_year & !tank_closure_revealed):
#   LUST reported with no associated closure in the 0–60d window.  This is
#   harder to attribute to mechanics.  A treatment effect here is more likely
#   to reflect a genuine change in leak rates or reporting behavior.
#
# tank_closure_known_leak:
#   A tank closure where a LUST was already on record >180 days before
#   closure.  These tanks were leaking BEFORE the closure decision.  A
#   treatment effect here would suggest the reform induced closures of
#   already-leaking tanks — a positive environmental externality.

m_rq31_induced <- feols(
  lust_closure_induced ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_rq31_standalone <- feols(
  lust_standalone ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_rq31_known <- feols(
  tank_closure_known_leak ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_rq31_clean <- feols(
  tank_closure_clean ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# ---- Wide spec robustness (0-90d window) ----
m_rq31_induced_wide <- feols(
  lust_within_90d_of_closure ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

# ---- Report ----
rq3_outcomes <- list(
  list(m = m_rq3_leak,          label = "Total LUST"),
  list(m = m_rq3_leak_age,      label = "Total LUST + age ctrl"),
  list(m = m_rq31_induced,      label = "Closure-induced LUST (0-60d)"),
  list(m = m_rq31_induced_wide, label = "Closure-induced LUST (0-90d)"),
  list(m = m_rq31_standalone,   label = "Standalone LUST (not closure-induced)"),
  list(m = m_rq31_known,        label = "Known pre-existing leak at closure"),
  list(m = m_rq31_clean,        label = "Clean closure (no assoc. LUST)")
)

rq3_dt <- rbindlist(lapply(rq3_outcomes, function(o) {
  d <- extract_did(o$m)
  pre_mean <- mean(mm_fac_primary[panel_year < POST_YEAR,
                                   get(gsub("\\s.*", "", o$label))],
                   na.rm = TRUE)
  data.table(Outcome = o$label, Est = d$beta, SE = d$se, P = d$p, N = d$n)
}))

log_step("RQ3 — LUST decomposition DiD:")
for (i in seq_len(nrow(rq3_dt))) {
  r <- rq3_dt[i]
  log_step(sprintf("  %-44s: %+.4f  (%.4f)  p=%.3f",
                   r$Outcome, r$Est, r$SE, r$P), 1)
}
fwrite(rq3_dt, file.path(OUTPUT_TABLES, "Table_RQ3_LUSTDecomposition.csv"))

# ---- Descriptive: pre-post-period LUST composition ----
# Summarizes the LUST mix (standalone vs. closure-induced) by treatment/control
# and pre/post period.  Not a causal estimate; useful for contextualizing
# the regression decomposition.
lust_composition <- mm_fac_primary[, .(
  n_fy                 = .N,
  rate_total_lust      = mean(leak_year,             na.rm = TRUE),
  rate_induced_lust    = mean(lust_closure_induced,   na.rm = TRUE),
  rate_standalone_lust = mean(lust_standalone,        na.rm = TRUE),
  rate_known_lust      = mean(tank_closure_known_leak, na.rm = TRUE),
  rate_clean_closure   = mean(tank_closure_clean,      na.rm = TRUE)
), by = .(treat_group = fifelse(texas_treated == 1, "TX", "CTL"),
          period       = fifelse(panel_year < POST_YEAR, "Pre", "Post"))]

lust_composition[, pct_induced    := round(rate_induced_lust    / rate_total_lust * 100, 1)]
lust_composition[, pct_standalone := round(rate_standalone_lust / rate_total_lust * 100, 1)]
setorder(lust_composition, treat_group, period)

log_step("  LUST composition (rates per facility-year):", 1)
print(lust_composition[, .(treat_group, period, rate_total_lust,
                            rate_induced_lust, rate_standalone_lust,
                            pct_induced, pct_standalone)])
fwrite(lust_composition, file.path(OUTPUT_TABLES, "Table_RQ3_LUSTComposition.csv"))
cat("\n")


#### S7: Event Studies ####

cat("========================================\n")
cat("S7: EVENT STUDIES\n")
cat("========================================\n\n")

es_specs <- list(
  # RQ1
  list(depvar = "replacement_closure_year",
       ylab   = "Effect on Replacement Closure Probability",
       stem   = "ReplacementClosure"),
  list(depvar = "permanent_closure_year",
       ylab   = "Effect on Permanent Closure Probability",
       stem   = "PermanentClosure"),
  list(depvar = "exit_flag",
       ylab   = "Effect on Exit Probability",
       stem   = "Exit"),
  # RQ2
  list(depvar = "net_tank_change",
       ylab   = "Effect on Net Tank Change",
       stem   = "NetTankChange"),
  list(depvar = "single_to_double_year",
       ylab   = "Effect on SW→DW Upgrade Probability",
       stem   = "SingleToDouble"),
  list(depvar = "capacity_change_year",
       ylab   = "Effect on Capacity Change (gallons)",
       stem   = "CapacityChange"),
  # RQ3
  list(depvar = "leak_year",
       ylab   = "Effect on Total LUST Probability",
       stem   = "LUSTTotal"),
  list(depvar = "lust_standalone",
       ylab   = "Effect on Standalone LUST Probability",
       stem   = "LUSTStandalone"),
  list(depvar = "lust_closure_induced",
       ylab   = "Effect on Closure-Induced LUST Probability",
       stem   = "LUSTInduced")
)

es_models <- list()
for (spec in es_specs) {
  log_step(sprintf("  Event study: %s", spec$stem))
  fml <- as.formula(sprintf(
    "%s ~ i(rel_year_bin, texas_treated, ref = -1) | panel_id + make_model_fac^panel_year",
    spec$depvar
  ))
  m <- feols(fml, mm_fac_primary, cluster = ~state)
  es_models[[spec$stem]] <- m
  plot_es(m, ylab = spec$ylab, xlim_hi = rel_max,
          filename = file.path(OUTPUT_FIGURES,
                               sprintf("Figure_ES_%s_FacMM.png", spec$stem)))
}

# Pre-trend joint tests
for (stem in c("ReplacementClosure", "NetTankChange", "LUSTTotal")) {
  pt_nms <- names(coef(es_models[[stem]]))
  pt_pre <- pt_nms[grepl("::-[2-9]|::-1[0-9]", pt_nms)]
  if (length(pt_pre) >= 2) {
    pt_p <- suppressWarnings(wald(es_models[[stem]], keep = pt_pre)$p)
    log_step(sprintf("  Pre-trend joint test (%s): p = %.3f", stem, pt_p))
  }
}
cat("\n")


#### S8: HTE by Wall Type ####

cat("========================================\n")
cat("S8: HTE BY WALL TYPE\n")
cat("========================================\n\n")

mm_wall_sample <- mm_fac_primary[fac_wall %in% WALL_LEVELS]
mm_wall_sample[, fac_wall := factor(fac_wall, levels = WALL_LEVELS)]

# Wall HTE for each research question
m_hte_wall_replace <- feols(
  replacement_closure_year ~ did_term:fac_wall |
    panel_id + make_model_fac^panel_year,
  mm_wall_sample, cluster = ~state)

m_hte_wall_exit <- feols(
  exit_flag ~ did_term:fac_wall | panel_id + make_model_fac^panel_year,
  mm_wall_sample, cluster = ~state)

m_hte_wall_std <- feols(
  single_to_double_year ~ did_term:fac_wall |
    panel_id + make_model_fac^panel_year,
  mm_wall_sample, cluster = ~state)

m_hte_wall_lust <- feols(
  leak_year ~ did_term:fac_wall | panel_id + make_model_fac^panel_year,
  mm_wall_sample, cluster = ~state)

m_hte_wall_lust_sa <- feols(
  lust_standalone ~ did_term:fac_wall | panel_id + make_model_fac^panel_year,
  mm_wall_sample, cluster = ~state)

extract_wall_hte <- function(m, outcome_label) {
  ct <- coeftable(m)
  rbindlist(lapply(WALL_LEVELS, function(w) {
    idx <- grep(paste0("fac_wall", w), rownames(ct), fixed = TRUE)
    if (length(idx) == 0) return(NULL)
    data.table(wall = w, outcome = outcome_label,
               est = ct[idx[1], "Estimate"],
               se  = ct[idx[1], "Std. Error"],
               p   = ct[idx[1], "Pr(>|t|)"])
  }))
}

wall_hte_dt <- rbind(
  extract_wall_hte(m_hte_wall_replace, "Replacement"),
  extract_wall_hte(m_hte_wall_exit,    "Exit"),
  extract_wall_hte(m_hte_wall_std,     "SW→DW upgrade"),
  extract_wall_hte(m_hte_wall_lust,    "Total LUST"),
  extract_wall_hte(m_hte_wall_lust_sa, "Standalone LUST")
)
wall_hte_dt[, `:=`(
  ci_lo   = est - 1.96 * se,
  ci_hi   = est + 1.96 * se,
  wall    = factor(wall, levels = rev(WALL_LEVELS)),
  outcome = factor(outcome, levels = c("Replacement", "Exit",
                                        "SW→DW upgrade",
                                        "Total LUST", "Standalone LUST"))
)]

log_step("Wall-type HTE:")
for (i in seq_len(nrow(wall_hte_dt))) {
  r <- wall_hte_dt[i]
  log_step(sprintf("  %-12s %-18s: %+.4f  (%.4f)  p=%.3f",
                   r$wall, r$outcome, r$est, r$se, r$p), 1)
}

p_wall_hte <- ggplot(wall_hte_dt[!is.na(est)],
  aes(x = est, y = wall, xmin = ci_lo, xmax = ci_hi, color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.6),
                  size = 0.4, linewidth = 0.5) +
  scale_color_manual(values = c(
    "Replacement"    = COL_TX,
    "Exit"           = COL_CTRL,
    "SW→DW upgrade"  = "#009E73",
    "Total LUST"     = "#CC79A7",
    "Standalone LUST"= "#E69F00")) +
  labs(x = "Effect (Texas × Post)", y = "Portfolio Wall Composition") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_HTE_WallType.png"),
       p_wall_hte, width = 9, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_HTE_WallType.pdf"),
       p_wall_hte, width = 9, height = 5, device = cairo_pdf)
fwrite(wall_hte_dt, file.path(OUTPUT_TABLES, "Table_HTE_WallType.csv"))
cat("\n")


#### S9: HTE by Oldest-Tank Age ####

cat("========================================\n")
cat("S9: HTE BY OLDEST-TANK AGE\n")
cat("========================================\n\n")

mm_fac_primary[, fac_oldest_age_bin := factor(fac_oldest_age_bin,
                                               levels = AGE_BIN_LEVELS)]

m_hte_age_replace <- feols(
  replacement_closure_year ~ did_term:fac_oldest_age_bin |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_hte_age_exit <- feols(
  exit_flag ~ did_term:fac_oldest_age_bin |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_hte_age_lust <- feols(
  leak_year ~ did_term:fac_oldest_age_bin |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

extract_age_hte <- function(m, outcome_label) {
  ct <- coeftable(m)
  rbindlist(lapply(AGE_BIN_LEVELS, function(b) {
    idx <- grep(b, rownames(ct), fixed = TRUE)
    if (length(idx) == 0) return(NULL)
    data.table(age_bin = b, outcome = outcome_label,
               est = ct[idx[1], "Estimate"],
               se  = ct[idx[1], "Std. Error"],
               p   = ct[idx[1], "Pr(>|t|)"])
  }))
}

age_hte_dt <- rbind(
  extract_age_hte(m_hte_age_replace, "Replacement"),
  extract_age_hte(m_hte_age_exit,    "Exit"),
  extract_age_hte(m_hte_age_lust,    "Total LUST")
)
age_hte_dt[, `:=`(
  ci_lo   = est - 1.96 * se,
  ci_hi   = est + 1.96 * se,
  age_bin = factor(age_bin, levels = AGE_BIN_LEVELS),
  outcome = factor(outcome, levels = c("Replacement", "Exit", "Total LUST"))
)]

p_age_hte <- ggplot(age_hte_dt[!is.na(est)],
  aes(x = age_bin, y = est, ymin = ci_lo, ymax = ci_hi,
      color = outcome, group = outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 2.5) +
  geom_errorbar(width = 0.25, linewidth = 0.5) +
  scale_color_manual(values = c("Replacement" = COL_TX,
                                 "Exit"        = COL_CTRL,
                                 "Total LUST"  = "#CC79A7")) +
  labs(x = "Age of Oldest Tank at Reform", y = "Effect (Texas × Post)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_HTE_OldestTankAge.png"),
       p_age_hte, width = 9, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_HTE_OldestTankAge.pdf"),
       p_age_hte, width = 9, height = 5, device = cairo_pdf)
fwrite(age_hte_dt, file.path(OUTPUT_TABLES, "Table_HTE_OldestTankAge.csv"))
cat("\n")


#### S10: Robustness ####

cat("========================================\n")
cat("S10: ROBUSTNESS\n")
cat("========================================\n\n")

# Run robustness checks on two headline outcomes: replacement closure (RQ1)
# and standalone LUST (RQ3.1 — the non-mechanical component).

BORDER_STATES <- c("OK", "AR", "LA", "NM")

# ════════════════════════════════════════════════════════════
# Mandate variable construction
# ════════════════════════════════════════════════════════════
# TX phased mandate (30 TAC Ch. 334):
#   Pre-1965 tanks  → 12/22/1989
#   1965-1974 tanks → 12/22/1990
#   1975-1979 tanks → 12/22/1991
#   1980-1984 tanks → 12/22/1992
#   1985-1988 tanks → 12/22/1993
# Federal mandate (40 CFR 280): ALL pre-1989 tanks → 12/22/1998
#
# mandate_active   = 1 within the federal compliance window (all states).
# tx_early_mandate = 1 only for TX facility-years within the cohort-specific
#                    TX deadline — the differential early pressure that controls
#                    did not face.  This is the variable entered in 10c.
#                    The common federal mandate is absorbed by cell × year FE.

mm_fac_primary[, tx_mandate_deadline_yr := NA_integer_]
mm_fac_primary[state == "TX" & had_pre1989_tanks == 1L,
  tx_mandate_deadline_yr := fcase(
    facility_first_install_yr <= 1964L,                    1989L,
    facility_first_install_yr %between% c(1965L, 1974L),   1990L,
    facility_first_install_yr %between% c(1975L, 1979L),   1991L,
    facility_first_install_yr %between% c(1980L, 1984L),   1992L,
    facility_first_install_yr %between% c(1985L, 1988L),   1993L,
    default = NA_integer_
  )]

mm_fac_primary[, federal_mandate_deadline_yr := fifelse(
  had_pre1989_tanks == 1L, 1998L, NA_integer_
)]

mm_fac_primary[, mandate_active := 0L]
mm_fac_primary[had_pre1989_tanks == 1L,
  mandate_active := as.integer(
    panel_year >= MANDATE_ANNOUNCE_YR &
    panel_year <= federal_mandate_deadline_yr
  )]

mm_fac_primary[, tx_early_mandate := 0L]
mm_fac_primary[
  state == "TX" & had_pre1989_tanks == 1L & !is.na(tx_mandate_deadline_yr),
  tx_early_mandate := as.integer(
    panel_year >= MANDATE_ANNOUNCE_YR &
    panel_year <= tx_mandate_deadline_yr
  )]

mm_fac_primary[, c("tx_mandate_deadline_yr", "federal_mandate_deadline_yr") := NULL]

log_step("Mandate variables constructed:")
log_step(sprintf("  mandate_active    : %s fy (TX: %s, CTL: %s)",
  fmt_n(mm_fac_primary[mandate_active == 1, .N]),
  fmt_n(mm_fac_primary[mandate_active == 1 & state == "TX", .N]),
  fmt_n(mm_fac_primary[mandate_active == 1 & state != "TX", .N])), 1)
log_step(sprintf("  tx_early_mandate  : %s fy",
  fmt_n(mm_fac_primary[tx_early_mandate == 1, .N])), 1)

# ════════════════════════════════════════════════════════════
# Robustness models — replacement closure
# ════════════════════════════════════════════════════════════
m_rob_rc_base      <- m_rq1_replacement   # already estimated

m_rob_rc_noborder  <- feols(
  replacement_closure_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary[!state %in% BORDER_STATES], cluster = ~state)

m_rob_rc_mandate   <- feols(
  replacement_closure_year ~ did_term + tx_early_mandate |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

mm_fac_primary[, make_model_3dim := paste(fac_wall, fac_fuel, sep = "_")]
m_rob_rc_3dim      <- feols(
  replacement_closure_year ~ did_term | panel_id + make_model_3dim^panel_year,
  mm_fac_primary, cluster = ~state)

m_rob_rc_sw        <- feols(
  replacement_closure_year ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary[fac_wall == "All-SW"], cluster = ~state)

# ════════════════════════════════════════════════════════════
# Robustness models — standalone LUST
# ════════════════════════════════════════════════════════════
m_rob_ls_base      <- m_rq31_standalone

m_rob_ls_noborder  <- feols(
  lust_standalone ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary[!state %in% BORDER_STATES], cluster = ~state)

m_rob_ls_mandate   <- feols(
  lust_standalone ~ did_term + tx_early_mandate |
    panel_id + make_model_fac^panel_year,
  mm_fac_primary, cluster = ~state)

m_rob_ls_3dim      <- feols(
  lust_standalone ~ did_term | panel_id + make_model_3dim^panel_year,
  mm_fac_primary, cluster = ~state)

m_rob_ls_sw        <- feols(
  lust_standalone ~ did_term | panel_id + make_model_fac^panel_year,
  mm_fac_primary[fac_wall == "All-SW"], cluster = ~state)

spec_labels <- c("Baseline", "No border", "TX early mandate",
                 "3-dim cell", "All-SW only")

rob_rc_dt <- rbindlist(Map(function(m, nm) {
  d <- extract_did(m)
  data.table(Outcome = "Replacement closure", Spec = nm,
             Est = d$beta, SE = d$se, P = d$p, N = d$n)
}, list(m_rob_rc_base, m_rob_rc_noborder, m_rob_rc_mandate,
        m_rob_rc_3dim, m_rob_rc_sw), spec_labels))

rob_ls_dt <- rbindlist(Map(function(m, nm) {
  d <- extract_did(m)
  data.table(Outcome = "Standalone LUST", Spec = nm,
             Est = d$beta, SE = d$se, P = d$p, N = d$n)
}, list(m_rob_ls_base, m_rob_ls_noborder, m_rob_ls_mandate,
        m_rob_ls_3dim, m_rob_ls_sw), spec_labels))

rob_dt <- rbind(rob_rc_dt, rob_ls_dt)

log_step("Robustness summary (coefficients in pp):")
print(rob_dt[, .(Outcome, Spec,
                 Est_pp = round(Est * 100, 3),
                 SE_pp  = round(SE  * 100, 3),
                 P      = round(P, 4), N)])
fwrite(rob_dt, file.path(OUTPUT_TABLES, "Table_Robustness_FacBehavior.csv"))
cat("\n")


#### S11: Export ####

cat("========================================\n")
cat("S11: EXPORT\n")
cat("========================================\n\n")

# Primary sample
saveRDS(mm_fac_primary, file.path(ANALYSIS_DIR, "mm_fac_primary.rds"))

# RQ1 — Closure response
saveRDS(m_rq1_replacement,      file.path(ANALYSIS_DIR, "fac_rq1_replacement.rds"))
saveRDS(m_rq1_permanent,        file.path(ANALYSIS_DIR, "fac_rq1_permanent.rds"))
saveRDS(m_rq1_exit,             file.path(ANALYSIS_DIR, "fac_rq1_exit.rds"))
saveRDS(m_rq1_retrofit_noexit,  file.path(ANALYSIS_DIR, "fac_rq1_retrofit_noexit.rds"))

# RQ2 — Portfolio investment
saveRDS(m_rq2_nettank,          file.path(ANALYSIS_DIR, "fac_rq2_nettank.rds"))
saveRDS(m_rq2_capacity,         file.path(ANALYSIS_DIR, "fac_rq2_capacity.rds"))
saveRDS(m_rq2_std,              file.path(ANALYSIS_DIR, "fac_rq2_std.rds"))
saveRDS(m_rq2_retrofit,         file.path(ANALYSIS_DIR, "fac_rq2_retrofit.rds"))

# RQ3 — LUST
saveRDS(m_rq3_leak,             file.path(ANALYSIS_DIR, "fac_rq3_leak.rds"))
saveRDS(m_rq31_induced,         file.path(ANALYSIS_DIR, "fac_rq31_induced.rds"))
saveRDS(m_rq31_standalone,      file.path(ANALYSIS_DIR, "fac_rq31_standalone.rds"))
saveRDS(m_rq31_known,           file.path(ANALYSIS_DIR, "fac_rq31_known.rds"))

# HTE models
saveRDS(m_hte_wall_replace,     file.path(ANALYSIS_DIR, "fac_hte_wall_replace.rds"))
saveRDS(m_hte_wall_std,         file.path(ANALYSIS_DIR, "fac_hte_wall_std.rds"))
saveRDS(m_hte_age_replace,      file.path(ANALYSIS_DIR, "fac_hte_age_replace.rds"))

# Event study models
for (nm in names(es_models))
  saveRDS(es_models[[nm]],
          file.path(ANALYSIS_DIR, sprintf("fac_es_%s.rds", tolower(nm))))

log_step(sprintf("Primary sample: %s facility-years, %s facilities, %s cells",
                 fmt_n(n_fy), fmt_n(n_fac), fmt_n(n_cells)))

cat("\n========================================\n")
cat("02a_DiD_FacBehavior.R COMPLETE\n")
cat("========================================\n")