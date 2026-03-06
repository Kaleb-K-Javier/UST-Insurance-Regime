#==============================================================================
# 02_DiD_Main_MakeModel.R
# Texas UST Insurance Reform — Causal DiD Estimates
#
# PREREQUISITES: Run 00_RunAll.R (or 01m_MakeModelSample.R) first.
# This script loads ONLY from Data/Analysis/ — it does NOT source any 01x
# descriptive module and does NOT re-filter the raw data.
#
# Loads from Data/Analysis/:
#   analysis_annual_data.rds     — facility-year panel (all analysis vars set)
#   analysis_tank_inventory.rds  — incumbent tanks
#   analysis_tanks_1998.rds      — 1998 cross-section (w/ is_make_model flag)
#   analysis_closed_tanks.rds    — closed tanks (w/ is_make_model flag)
#   analysis_metadata.rds        — study params, sample sizes, pt_results
#   analysis_cv_data.rds         — CV risk scores (optional; if absent, H2b skipped)
#
# Also loads from Data/Interim/ (saved by 01j):
#   km_1990_1997.rds             — Kaplan-Meier context figure data
#   mm_closed_tanks_for_H3.rds   — age-at-closure data (make-model only)
#
# Table/Figure index:
#   Tables 3-10     — Primary DiD and event-study results
#   Figures 6-9     — Event study plots
#   H1/H2           — Age heterogeneity (youngest vs oldest subgroup)
#   H3              — Age at closure
#   H4              — Wall-type heterogeneity (sw_broader_sample)
#
# Panel figure convention (from 01a_Setup.R save_panels()):
#   Each multi-panel figure produces:
#   - Individual PNG + PDF per panel  (e.g., Figure6_EventStudy_PanelA.png)
#   - Combined patchwork              (e.g., Figure6_EventStudy_Combined.png)
#==============================================================================

# ── Shared setup: constants, theme, save_panels() helper ─────────────────────
library(here)
source(here("Code", "01a_Setup.R"))   # defines all constants + save_panels()

cat("=== 02_DiD_Main_MakeModel.R ===\n")
cat(sprintf("  Started: %s\n", Sys.time()))

# ── Load analysis objects ─────────────────────────────────────────────────────
ANALYSIS_DIR <- here("Data", "Analysis")
INTERIM_DIR  <- here("Data", "Interim")

load_analysis <- function(name) {
  path <- file.path(ANALYSIS_DIR, paste0(name, ".rds"))
  if (!file.exists(path))
    stop(sprintf("Analysis file not found: %s\n  → Run 00_RunAll.R first.", path))
  readRDS(path)
}

annual_data    <- load_analysis("analysis_annual_data")
tank_inventory <- load_analysis("analysis_tank_inventory")
tanks_1998     <- load_analysis("analysis_tanks_1998")
closed_tanks   <- load_analysis("analysis_closed_tanks")
meta           <- load_analysis("analysis_metadata")

# Optional CV scores (needed for H2b risk-stratified HTE)
cv_path <- file.path(ANALYSIS_DIR, "analysis_cv_data.rds")
cv_data <- if (file.exists(cv_path)) {
  cat("  CV data: LOADED\n")
  readRDS(cv_path)
} else {
  cat("  CV data: NOT FOUND (H2b skipped) — run 01n_CVValidation.R\n")
  NULL
}

# KM and age-at-closure context (from 01j — graceful skip if absent)
km_data        <- tryCatch(readRDS(file.path(INTERIM_DIR, "km_1990_1997.rds")),
                            error = function(e) NULL)
mm_closed_H3   <- tryCatch(readRDS(file.path(INTERIM_DIR, "mm_closed_tanks_for_H3.rds")),
                            error = function(e) NULL)

cat(sprintf("  Full sample:    %s facilities (%s TX, %s CTL)\n",
    format(meta$n_facilities_full,  big.mark=","),
    format(meta$n_facilities_tx,    big.mark=","),
    format(meta$n_facilities_ctl,   big.mark=",")))
cat(sprintf("  Make-Model:     %s facilities (%s TX, %s CTL)\n",
    format(meta$n_facilities_mm,    big.mark=","),
    format(meta$n_facilities_mm_tx, big.mark=","),
    format(meta$n_facilities_mm_ctl,big.mark=",")))

# ── Convenience: extract make-model sub-samples ───────────────────────────────
mm_annual      <- annual_data[is_make_model == 1]
mm_tanks_1998  <- tanks_1998[is_make_model == 1]
mm_closed      <- closed_tanks[is_make_model == 1]
sw_broader     <- annual_data[is_sw_broader == 1]   # for H4

# Merge CV risk scores into make-model data (for H2b)
if (!is.null(cv_data)) {
  mm_annual <- merge(mm_annual, cv_data[, .(panel_id, fac_emp_risk_quartile,
                                              fac_all_sw, pred_emp)],
                     by = "panel_id", all.x = TRUE)
}

# ── Age subgroups (H1/H2) ─────────────────────────────────────────────────────
# Median threshold set in 01m, stored in metadata
median_age_mm <- meta$median_age_mm_1998
mm_annual[, age_subgroup := fcase(
  !is.na(mean_age_1998) & mean_age_1998 <= median_age_mm, "Youngest",
  !is.na(mean_age_1998) & mean_age_1998 >  median_age_mm, "Oldest",
  default = NA_character_
)]

cat(sprintf("  Median age (make-model, Dec 1998): %.2f years\n", median_age_mm))
cat(sprintf("  Youngest sub-sample: %s facilities\n",
    format(uniqueN(mm_annual[age_subgroup=="Youngest", panel_id]), big.mark=",")))
cat(sprintf("  Oldest  sub-sample: %s facilities\n",
    format(uniqueN(mm_annual[age_subgroup=="Oldest",   panel_id]), big.mark=",")))

#==============================================================================
# → PASTE YOUR EXISTING 02_DiD SECTIONS BELOW THIS LINE
#    (event studies, Tables 3-10, H1-H4, matching diagnostics)
#
#    All constants (TREATMENT_YEAR, COL_TX, theme_pub, save_panels, etc.)
#    are available via 01a_Setup.R, which was sourced above.
#
#    All sample objects are ready:
#      mm_annual     — make-model facility-year panel
#      mm_tanks_1998 — make-model 1998 cross-section
#      mm_closed     — make-model closed tanks
#      sw_broader    — broader SW sample (H4 wall-type test)
#      cv_data       — empirical risk scores (or NULL)
#      km_data       — KM survival context (or NULL)
#      mm_closed_H3  — age-at-closure data for H3 (or NULL)
#==============================================================================

cat("\n=== 02 DATA LOADED — ready for causal estimation ===\n")
