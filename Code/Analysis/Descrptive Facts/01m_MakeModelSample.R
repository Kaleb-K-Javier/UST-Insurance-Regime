#==============================================================================
# 01m_MakeModelSample.R
# Make-Model Sample Construction + Export for 02_DiD
#
# This is the FINAL 01x module. It assembles the make-model analysis objects
# and writes them to ANALYSIS_DIR (Data/Analysis/) so that 02_DiD_Main.R
# can load them directly without needing to source any 01x file.
#
# Outputs (ANALYSIS_DIR):
#   analysis_annual_data.rds     — full facility-year panel (for event studies)
#   analysis_tank_inventory.rds  — incumbent tanks
#   analysis_tanks_1998.rds      — active tanks Dec 22 1998 cross-section
#   analysis_closed_tanks.rds    — completed closures in study window
#   analysis_metadata.rds        — study params, sample sizes, Wald p-values
#
# Also writes to INTERIM_DIR (for CV validation module 01n):
#   mm_annual_data.rds           — make-model subset of annual_data
#   mm_tanks_1998.rds            — make-model subset of tanks_1998
#==============================================================================

source(here::here("Code",'Analysis','Descrptive Facts', "01a_Setup.R"))
cat("=== 01m: MAKE-MODEL SAMPLE CONSTRUCTION + EXPORT ===\n")

annual_data    <- load_interim("annual_data")
tanks_1998     <- load_interim("tanks_1998")
tanks          <- load_interim("tanks")
closed_tanks   <- load_interim("closed_tanks")
pt_results     <- tryCatch(load_interim("pt_results"), error = function(e) NULL)

# ─────────────────────────────────────────────────────────────────────────────
# Make-model sample flags (ensure consistent with 01h/01l)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Applying make-model restriction ---\n")

# Facility-year flag
annual_data[, is_make_model := as.integer(
  has_single_walled == 1 &
  has_gasoline_year == 1 &
  single_tanks == active_tanks &
  !is.na(install_year) &
  install_year > MM_INSTALL_START - 1 &
  install_year <= MM_INSTALL_END
)]

# Age subgroups at treatment (used for H1/H2 in 02_DiD)
# Threshold: median age within MM sample at 1998
mm_t98  <- tanks_1998[
  single_walled == 1 &
  !is.na(install_year) &
  install_year > MM_INSTALL_START - 1 &
  install_year <= MM_INSTALL_END
]
median_age_mm <- mm_t98[, median(tank_age_1998, na.rm=TRUE)]
cat(sprintf("  Median age (MM cohort) at Dec 1998: %.2f years\n", median_age_mm))

# Facility-level mean age at treatment
fac_mean_age <- mm_t98[, .(mean_age_1998 = mean(tank_age_1998, na.rm=TRUE)),
                        by = panel_id]
if ("mean_age_1998" %in% names(annual_data)) annual_data[, mean_age_1998 := NULL]
annual_data <- merge(annual_data, fac_mean_age, by="panel_id", all.x=TRUE)

annual_data[, age_subgroup := fcase(
  is_make_model == 1 & !is.na(mean_age_1998) & mean_age_1998 <= median_age_mm,
  "Youngest",
  is_make_model == 1 & !is.na(mean_age_1998) & mean_age_1998 > median_age_mm,
  "Oldest",
  default = NA_character_
)]

mm_summary <- annual_data[is_make_model == 1, .(
  n_fac       = uniqueN(panel_id),
  n_fac_tx    = uniqueN(panel_id[texas_treated == 1]),
  n_fac_ctl   = uniqueN(panel_id[texas_treated == 0]),
  n_fac_years = .N,
  yr_start    = min(panel_year),
  yr_end      = max(panel_year)
)]
cat("\nMake-Model sample summary:\n")
print(mm_summary)

# ─────────────────────────────────────────────────────────────────────────────
# Broader single-wall sample (for H4 wall-type heterogeneity in 02_DiD)
# Only adds multi-tank / non-gasoline; still restricts to post-1989 vintage
# ─────────────────────────────────────────────────────────────────────────────
annual_data[, is_sw_broader := as.integer(
  has_single_walled == 1 &
  !is.na(install_year) &
  install_year > MM_INSTALL_START - 1 &
  install_year <= MM_INSTALL_END
)]

# ─────────────────────────────────────────────────────────────────────────────
# Relative event-time variable (for event-study plots in 02_DiD)
# ─────────────────────────────────────────────────────────────────────────────
annual_data[, rel_year := panel_year - TREATMENT_YEAR]   # 0 = reform year
# Binned: collapse endpoints for clean event-study display
annual_data[, rel_year_binned := fcase(
  rel_year < -8,  -9L,   # bin pre-1990 years
  rel_year > 12,  13L,   # bin far post years
  default = as.integer(rel_year)
)]

# ─────────────────────────────────────────────────────────────────────────────
# Stacked DiD cohort identifier (for Callaway-Sant'Anna / stacked)
# Panel builder assigns each facility to the TREATMENT cohort = 1999
# All treated facilities have the same treatment date here,
# so "stacked" collapses to standard 2x2; kept for API compatibility.
# ─────────────────────────────────────────────────────────────────────────────
annual_data[, treatment_cohort := fifelse(texas_treated == 1, 1999L, NA_integer_)]

# ─────────────────────────────────────────────────────────────────────────────
# Tanks 1998 — make-model flag
# ─────────────────────────────────────────────────────────────────────────────
tanks_1998[, is_make_model := as.integer(
  single_walled == 1 &
  !is.na(install_year) &
  install_year > MM_INSTALL_START - 1 &
  install_year <= MM_INSTALL_END
)]
mm_tanks_1998 <- tanks_1998[is_make_model == 1]

# ─────────────────────────────────────────────────────────────────────────────
# Closed tanks — make-model flag
# ─────────────────────────────────────────────────────────────────────────────
closed_tanks[, is_make_model := as.integer(
  single_walled == 1 &
  !is.na(install_year) &
  install_year > MM_INSTALL_START - 1 &
  install_year <= MM_INSTALL_END
)]

# ─────────────────────────────────────────────────────────────────────────────
# Metadata object (loaded by 02_DiD for display in log / tables)
# ─────────────────────────────────────────────────────────────────────────────
analysis_metadata <- list(
  # Study parameters
  TREATMENT_YEAR     = TREATMENT_YEAR,
  POST_YEAR          = POST_YEAR,
  PANEL_START        = PANEL_START,
  PANEL_END          = PANEL_END,
  CONTROL_STATES     = CONTROL_STATES,
  MM_INSTALL_START   = MM_INSTALL_START,
  MM_INSTALL_END     = MM_INSTALL_END,
  median_age_mm_1998 = median_age_mm,

  # Sample sizes
  n_facilities_full   = uniqueN(annual_data$panel_id),
  n_facilities_tx     = uniqueN(annual_data[texas_treated==1, panel_id]),
  n_facilities_ctl    = uniqueN(annual_data[texas_treated==0, panel_id]),
  n_facilities_mm     = mm_summary$n_fac,
  n_facilities_mm_tx  = mm_summary$n_fac_tx,
  n_facilities_mm_ctl = mm_summary$n_fac_ctl,

  # Parallel trends (from 01g)
  pt_results          = pt_results,

  # Age bins (for DiD heterogeneity plots)
  AGE_BIN_BREAKS      = AGE_BIN_BREAKS,
  AGE_BIN_LABELS      = AGE_BIN_LABELS,
  AGE_BIN_REF         = AGE_BIN_REF
)

# ─────────────────────────────────────────────────────────────────────────────
# Save to ANALYSIS_DIR (→ 02_DiD reads from here)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Saving to ANALYSIS_DIR (for 02_DiD) ---\n")

saveRDS(annual_data,       file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))
saveRDS(tanks,             file.path(ANALYSIS_DIR, "analysis_tank_inventory.rds"))
saveRDS(tanks_1998,        file.path(ANALYSIS_DIR, "analysis_tanks_1998.rds"))
saveRDS(closed_tanks,      file.path(ANALYSIS_DIR, "analysis_closed_tanks.rds"))
saveRDS(analysis_metadata, file.path(ANALYSIS_DIR, "analysis_metadata.rds"))

cat("  analysis_annual_data.rds\n")
cat("  analysis_tank_inventory.rds\n")
cat("  analysis_tanks_1998.rds\n")
cat("  analysis_closed_tanks.rds\n")
cat("  analysis_metadata.rds\n")

# ─────────────────────────────────────────────────────────────────────────────
# Save to INTERIM_DIR (→ 01n CV validation reads from here)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Saving make-model subsets to INTERIM_DIR (for 01n) ---\n")
save_interim(annual_data[is_make_model == 1], "mm_annual_data")
save_interim(mm_tanks_1998,                   "mm_tanks_1998")

# ─────────────────────────────────────────────────────────────────────────────
# Print final sample summary for paper
# ─────────────────────────────────────────────────────────────────────────────
cat(sprintf("\n=== FINAL SAMPLE SUMMARY ===\n"))
cat(sprintf("  Full sample:      %s facilities (%s TX, %s CTL)\n",
    format(analysis_metadata$n_facilities_full,  big.mark=","),
    format(analysis_metadata$n_facilities_tx,    big.mark=","),
    format(analysis_metadata$n_facilities_ctl,   big.mark=",")))
cat(sprintf("  Make-Model:       %s facilities (%s TX, %s CTL)\n",
    format(analysis_metadata$n_facilities_mm,     big.mark=","),
    format(analysis_metadata$n_facilities_mm_tx,  big.mark=","),
    format(analysis_metadata$n_facilities_mm_ctl, big.mark=",")))
cat(sprintf("  Median age 1998:  %.2f years (make-model cohort)\n",
    analysis_metadata$median_age_mm_1998))

cat("=== 01m COMPLETE ===\n")
