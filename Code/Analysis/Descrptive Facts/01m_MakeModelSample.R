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

source(here::here("Code", "Analysis", "Descrptive Facts", "01a_Setup.R"))
open_log("01m_MakeModelSample")
log_cat("=== 01m: MAKE-MODEL SAMPLE CONSTRUCTION + EXPORT ===\n")

annual_data  <- load_interim("annual_data")
tanks_1998   <- load_interim("tanks_1998")
tanks        <- load_interim("tanks")
closed_tanks <- load_interim("closed_tanks")
pt_results   <- load_interim("pt_results")

# ─────────────────────────────────────────────────────────────────────────────
# Validate that all required mm_* columns are present
# ─────────────────────────────────────────────────────────────────────────────
required_annual_mm <- c("make_model_fac", "fac_wall", "fac_fuel",
                        "fac_oldest_age_bin", "n_tanks_at_reform")
missing_annual <- setdiff(required_annual_mm, names(annual_data))
if (length(missing_annual) > 0)
  stop("annual_data missing required mm columns: ",
       paste(missing_annual, collapse = ", "),
       "\n  → Rerun 01c to merge facility_make_model.rds.")

required_tank_mm <- c("mm_wall", "mm_fuel", "mm_capacity", "mm_install_cohort")
missing_tank <- setdiff(required_tank_mm, names(tanks_1998))
if (length(missing_tank) > 0)
  stop("tanks_1998 missing required mm columns: ",
       paste(missing_tank, collapse = ", "),
       "\n  → Rerun 10_Master_Cleaning_and_Harmonization.r.")

# ─────────────────────────────────────────────────────────────────────────────
# Make-model primary sample flag on annual_data (mm_fac_primary)
# Matches the filter applied in 02a S4 exactly.
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Applying make-model restriction (facility-year) ---\n")

annual_data[, is_make_model := as.integer(
  !is.na(make_model_fac)     &
  fac_wall != "Unknown-Wall" &
  fac_fuel != "Unknown-Fuel" &
  !is.na(fac_oldest_age_bin)
)]

# Convenience alias used by 02a
annual_data[, mm_fac_primary_flag := is_make_model]

mm_summary <- annual_data[is_make_model == 1, .(
  n_fac       = uniqueN(panel_id),
  n_fac_tx    = uniqueN(panel_id[texas_treated == 1]),
  n_fac_ctl   = uniqueN(panel_id[texas_treated == 0]),
  n_fac_years = .N,
  yr_start    = min(panel_year),
  yr_end      = max(panel_year)
)]
log_cat("\nMake-Model primary sample summary:\n")
print(mm_summary)

# ─────────────────────────────────────────────────────────────────────────────
# Relative event-time variable (for event-study plots in 02_DiD)
# ─────────────────────────────────────────────────────────────────────────────
annual_data[, rel_year := panel_year - TREATMENT_YEAR]
annual_data[, rel_year_binned := fcase(
  rel_year < -8, -9L,
  rel_year > 12,  13L,
  default = as.integer(rel_year)
)]

# ─────────────────────────────────────────────────────────────────────────────
# Stacked DiD cohort identifier (API compatibility)
# ─────────────────────────────────────────────────────────────────────────────
annual_data[, treatment_cohort := fifelse(texas_treated == 1, 1999L, NA_integer_)]

# ─────────────────────────────────────────────────────────────────────────────
# tanks_1998 — make-model flag using harmonised mm_* columns
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Applying make-model restriction (tanks_1998) ---\n")

tanks_1998[, is_make_model := as.integer(
  mm_wall == "Single-Walled"          &
  mm_fuel != "Unknown-Fuel"           &
  !is.na(mm_capacity)                 &
  mm_install_cohort %in% MM_COHORT_YEARS
)]
mm_tanks_1998 <- tanks_1998[is_make_model == 1]
log_cat(sprintf("  mm_tanks_1998: %s tanks\n",
                format(nrow(mm_tanks_1998), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# closed_tanks — make-model flag using harmonised mm_* columns
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Applying make-model restriction (closed_tanks) ---\n")

missing_closed <- setdiff(required_tank_mm, names(closed_tanks))
if (length(missing_closed) > 0)
  stop("closed_tanks missing required mm columns: ",
       paste(missing_closed, collapse = ", "),
       "\n  → Rerun 10_Master_Cleaning_and_Harmonization.r.")

closed_tanks[, is_make_model := as.integer(
  mm_wall == "Single-Walled"          &
  mm_fuel != "Unknown-Fuel"           &
  !is.na(mm_capacity)                 &
  mm_install_cohort %in% MM_COHORT_YEARS
)]
log_cat(sprintf("  mm_closed_tanks: %s tanks\n",
                format(sum(closed_tanks$is_make_model, na.rm = TRUE), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# Make-model cell coverage diagnostic
# ─────────────────────────────────────────────────────────────────────────────
n_cells_fac      <- uniqueN(annual_data[is_make_model == 1, make_model_fac])
# Cells that contain at least one TX and one CTL facility-year
cells_both <- annual_data[is_make_model == 1,
  .(has_tx  = any(texas_treated == 1),
    has_ctl = any(texas_treated == 0)),
  by = make_model_fac]
n_cells_fac_both <- cells_both[has_tx == TRUE & has_ctl == TRUE, .N]

log_cat(sprintf("  Make-model cells: %d total | %d with both TX and CTL\n",
                n_cells_fac, n_cells_fac_both))

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
  MM_COHORT_YEARS    = MM_COHORT_YEARS,
  MM_WALL_EXCLUDE    = MM_WALL_EXCLUDE,
  MM_FUEL_EXCLUDE    = MM_FUEL_EXCLUDE,

  # Sample sizes — full
  n_facilities_full   = uniqueN(annual_data$panel_id),
  n_facilities_tx     = uniqueN(annual_data[texas_treated == 1, panel_id]),
  n_facilities_ctl    = uniqueN(annual_data[texas_treated == 0, panel_id]),

  # Sample sizes — make-model primary
  n_facilities_mm     = mm_summary$n_fac,
  n_facilities_mm_tx  = mm_summary$n_fac_tx,
  n_facilities_mm_ctl = mm_summary$n_fac_ctl,

  # Make-model cell coverage
  n_cells_fac         = n_cells_fac,
  n_cells_fac_both    = n_cells_fac_both,

  # Parallel trends (from 01g)
  pt_results          = pt_results,

  # Age bins (for DiD heterogeneity plots — full bin interaction in 02a S9)
  AGE_BIN_BREAKS      = AGE_BIN_BREAKS,
  AGE_BIN_LABELS      = AGE_BIN_LABELS,
  AGE_BIN_REF         = AGE_BIN_REF,

  # Retained for backward compatibility; median no longer used as a split threshold
  median_age_mm_1998  = annual_data[is_make_model == 1 & panel_year == 1998,
                           median(avg_tank_age, na.rm = TRUE)]
)

# ─────────────────────────────────────────────────────────────────────────────
# Save to ANALYSIS_DIR (→ 02_DiD reads from here)
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Saving to ANALYSIS_DIR (for 02_DiD) ---\n")

saveRDS(annual_data,       file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))
saveRDS(tanks,             file.path(ANALYSIS_DIR, "analysis_tank_inventory.rds"))
saveRDS(tanks_1998,        file.path(ANALYSIS_DIR, "analysis_tanks_1998.rds"))
saveRDS(closed_tanks,      file.path(ANALYSIS_DIR, "analysis_closed_tanks.rds"))
saveRDS(analysis_metadata, file.path(ANALYSIS_DIR, "analysis_metadata.rds"))

log_cat("  analysis_annual_data.rds\n")
log_cat("  analysis_tank_inventory.rds\n")
log_cat("  analysis_tanks_1998.rds\n")
log_cat("  analysis_closed_tanks.rds\n")
log_cat("  analysis_metadata.rds\n")

# ─────────────────────────────────────────────────────────────────────────────
# Save to INTERIM_DIR (→ 01n CV validation reads from here)
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Saving make-model subsets to INTERIM_DIR (for 01n) ---\n")
save_interim(annual_data[is_make_model == 1], "mm_annual_data")
save_interim(mm_tanks_1998,                   "mm_tanks_1998")

# ─────────────────────────────────────────────────────────────────────────────
# Final sample summary
# ─────────────────────────────────────────────────────────────────────────────
log_cat(sprintf("\n=== FINAL SAMPLE SUMMARY ===\n"))
log_cat(sprintf("  Full sample:      %s facilities (%s TX, %s CTL)\n",
    format(analysis_metadata$n_facilities_full,  big.mark = ","),
    format(analysis_metadata$n_facilities_tx,    big.mark = ","),
    format(analysis_metadata$n_facilities_ctl,   big.mark = ",")))
log_cat(sprintf("  Make-Model:       %s facilities (%s TX, %s CTL)\n",
    format(analysis_metadata$n_facilities_mm,     big.mark = ","),
    format(analysis_metadata$n_facilities_mm_tx,  big.mark = ","),
    format(analysis_metadata$n_facilities_mm_ctl, big.mark = ",")))
log_cat(sprintf("  MM cells (total): %d | with TX+CTL: %d\n",
    analysis_metadata$n_cells_fac,
    analysis_metadata$n_cells_fac_both))

log_cat("=== 01m COMPLETE ===\n")
close_log("01m_MakeModelSample")