#==============================================================================
# 01c_DataPrep.R
# Filtering, variable construction, tank cross-sections, mandate assignment.
# Reads raw interim files from 01b; saves processed intermediates.
#
# Outputs (INTERIM_DIR):
#   annual_data.rds         — filtered facility-year panel, all analysis vars
#   tanks.rds               — tank inventory filtered to analysis sample
#   tanks_1998.rds          — active tanks on Dec 22 1998 cross-section
#   closed_tanks.rds        — completed closures with age_at_closure
#   pre_period_closures.rds — 1987-1997 closures
#   data_quality_report.rds — state-level data quality metrics (→ Table B1)
#   balance_glm.rds         — missing-date balance logistic (→ Table B3)
#   attrition_log.rds       — sequential filter counts (→ Table B2)
#   incumbent_ids.rds       — vector of panel_ids in final sample
#==============================================================================

source(here::here("Code", "01a_Setup.R"))
cat("=== 01c: DATA PREPARATION ===\n")

annual_data    <- load_interim("annual_data_raw")
tank_inventory <- load_interim("tank_inventory_raw")
master_lust    <- load_interim("master_lust_raw")

attrition_log <- list()

# ─────────────────────────────────────────────────────────────────────────────
# S1: State data quality report (from raw tank inventory, pre-filter)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- S1: State data quality report ---\n")

tank_inventory[, state_std := toupper(trimws(as.character(state)))]

# Parse key date columns once
for (col in c("tank_installed_date","tank_closed_date")) {
  if (col %in% names(tank_inventory) && !inherits(tank_inventory[[col]], "IDate"))
    tank_inventory[, (col) := as.IDate(get(col))]
}

data_quality_report <- tank_inventory[, {
  closed_rows <- .SD[tolower(tank_status) == "closed"]
  n_closed    <- nrow(closed_rows)
  list(
    n_tanks               = .N,
    pct_miss_install_date = round(100 * mean(is.na(tank_installed_date)), 1),
    pct_closed_miss_date  = if (n_closed > 0)
      round(100 * mean(is.na(closed_rows$tank_closed_date)), 1) else NA_real_,
    pct_miss_tank_type    = round(
      100 * mean(is.na(single_walled) |
                 (!is.na(single_walled) & single_walled == 0 &
                  !is.na(double_walled) & double_walled == 0)), 1)
  )
}, by = .(state = state_std)]

tank_inventory[, state_std := NULL]
cat(sprintf("  data_quality_report: %d states\n", nrow(data_quality_report)))

attrition_log[["raw"]] <- list(
  stage = "Raw harmonized panel", filter = "All states, all years",
  facilities = uniqueN(annual_data$panel_id), fac_years = nrow(annual_data))

# ─────────────────────────────────────────────────────────────────────────────
# S2: Filter facility-year panel
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- S2: Filter panel ---\n")

annual_data <- annual_data[panel_year >= PANEL_START & panel_year <= PANEL_END]
attrition_log[["time_window"]] <- list(
  stage = "Study window", filter = sprintf("[%d, %d]", PANEL_START, PANEL_END),
  facilities = uniqueN(annual_data$panel_id), fac_years = nrow(annual_data))

annual_data <- annual_data[state == "TX" | state %in% CONTROL_STATES]
attrition_log[["states"]] <- list(
  stage = "Analysis states",
  filter = sprintf("TX + %d controls (NJ excl.)", length(CONTROL_STATES)),
  facilities = uniqueN(annual_data$panel_id), fac_years = nrow(annual_data))

annual_data[, is_incumbent := as.integer(first_observed < TREATMENT_YEAR)]
annual_data <- annual_data[is_incumbent == 1]
attrition_log[["incumbent"]] <- list(
  stage = "Incumbent sample",
  filter = sprintf("first_observed < %d", TREATMENT_YEAR),
  facilities = uniqueN(annual_data$panel_id), fac_years = nrow(annual_data))

# TN / MD institutional flags
annual_data[, `:=`(
  tn_pre_fund = as.integer(state == "TN" & panel_year < TN_FUND_START_YEAR),
  md_no_fund  = as.integer(state == "MD")
)]

# ─────────────────────────────────────────────────────────────────────────────
# S3: Missing-date exclusion + balance test
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- S3: Missing-date exclusion ---\n")

incumbent_ids <- unique(annual_data$panel_id)
tank_inventory[, panel_id := paste(toupper(trimws(facility_id)),
                                    toupper(trimws(state)), sep = "_")]
tanks_inc <- tank_inventory[panel_id %in% incumbent_ids]

for (col in c("tank_installed_date","tank_closed_date")) {
  if (!inherits(tanks_inc[[col]], "IDate"))
    tanks_inc[, (col) := as.IDate(get(col))]
}
tanks_inc[, is_closed_status := (tolower(tank_status) == "closed")]

miss_install <- unique(tanks_inc[is.na(tank_installed_date), panel_id])
miss_closure <- unique(tanks_inc[is_closed_status == TRUE &
                                   is.na(tank_closed_date), panel_id])
facilities_to_exclude <- union(miss_install, miss_closure)

tank_fac_flag <- tanks_inc[, .(
  has_miss_install = any(is.na(tank_installed_date)),
  has_miss_closure = any(is_closed_status == TRUE & is.na(tank_closed_date)),
  texas            = as.integer(first(state) == "TX")
), by = panel_id]
tank_fac_flag[, has_any_missing := (has_miss_install | has_miss_closure)]

balance_glm <- tryCatch(
  glm(has_any_missing ~ texas, data = tank_fac_flag, family = binomial),
  error = function(e) NULL)

if (!is.null(balance_glm)) {
  p_bal <- summary(balance_glm)$coefficients["texas","Pr(>|z|)"]
  cat(sprintf("  Missing-date balance p = %.4f %s\n", p_bal,
              if (p_bal < 0.10) "WARNING: imbalanced" else "OK"))
}

annual_data <- annual_data[!panel_id %in% facilities_to_exclude]
incumbent_ids <- unique(annual_data$panel_id)
attrition_log[["missing_data"]] <- list(
  stage = "Complete records",
  filter = sprintf("Facility-level date exclusion (%s excl.)",
                   format(length(facilities_to_exclude), big.mark=",")),
  facilities = uniqueN(annual_data$panel_id), fac_years = nrow(annual_data))

cat(sprintf("  Excluded: %s | Remaining: %s facilities | %s rows\n",
    format(length(facilities_to_exclude), big.mark=","),
    format(uniqueN(annual_data$panel_id), big.mark=","),
    format(nrow(annual_data), big.mark=",")))

rm(tanks_inc, tank_fac_flag, miss_install, miss_closure)
gc()

# ─────────────────────────────────────────────────────────────────────────────
# S4: Core analysis variables
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- S4: Analysis variables ---\n")

annual_data[, `:=`(
  treated       = texas_treated,
  post          = post_1999,
  did_term      = texas_treated * post_1999,
  closure_event = as.integer(n_closures > 0),
  rel_year_1999 = panel_year - POST_YEAR,
  county_fips_fac = as.factor(county_fips)
)]

# age_bin: use canonical bins, handle panel builder naming variance
if ("age_bins" %in% names(annual_data)) {
  existing_labels <- if (is.factor(annual_data$age_bins))
    levels(annual_data$age_bins) else sort(unique(annual_data$age_bins[!is.na(annual_data$age_bins)]))
  if (identical(as.character(existing_labels), AGE_BIN_LABELS)) {
    annual_data[, age_bin := age_bins]
  } else {
    setnames(annual_data, "age_bins", "age_bins_raw")
    annual_data[, age_bin := make_age_bin(avg_tank_age)]
  }
} else {
  annual_data[, age_bin := make_age_bin(avg_tank_age)]
}

cat(sprintf("  TX: %s fac | Control: %s fac | %d-%d\n",
    format(uniqueN(annual_data[texas_treated==1, panel_id]), big.mark=","),
    format(uniqueN(annual_data[texas_treated==0, panel_id]), big.mark=","),
    min(annual_data$panel_year), max(annual_data$panel_year)))

# ─────────────────────────────────────────────────────────────────────────────
# S5: Tank-level datasets
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- S5: Tank-level datasets ---\n")

tanks <- tank_inventory[panel_id %in% incumbent_ids]
for (col in c("tank_installed_date","tank_closed_date"))
  if (col %in% names(tanks) && !inherits(tanks[[col]], "IDate"))
    tanks[, (col) := as.IDate(get(col))]

# 1998 cross-section
tanks_1998 <- tanks[
  !is.na(tank_installed_date) &
  tank_installed_date <= TREATMENT_DATE &
  (is.na(tank_closed_date) | tank_closed_date > TREATMENT_DATE)
]
tanks_1998[, `:=`(
  tank_age_1998 = as.numeric(TREATMENT_DATE - tank_installed_date) / 365.25,
  texas_treated = as.integer(state == "TX"),
  install_year  = year(tank_installed_date),
  Group         = fifelse(state == "TX", "Texas", "Control")
)]
tanks_1998[, is_pre_1988 := as.integer(tank_installed_date <= MANDATE_CUTOFF_DATE)]
tanks_1998[, age_bin     := make_age_bin(tank_age_1998)]
if (!"capacity" %in% names(tanks_1998)) tanks_1998[, capacity := NA_real_]

cat(sprintf("  Active tanks Dec 22 1998: %s (TX: %s | CTL: %s)\n",
    format(nrow(tanks_1998), big.mark=","),
    format(nrow(tanks_1998[texas_treated==1]), big.mark=","),
    format(nrow(tanks_1998[texas_treated==0]), big.mark=",")))

# Closed tanks
closed_tanks <- tanks[
  !is.na(tank_installed_date) & !is.na(tank_closed_date) &
  tank_closed_date >= as.IDate(paste0(PANEL_START, "-01-01")) &
  tank_closed_date <= STUDY_END_DATE
]
closed_tanks[, `:=`(
  age_at_closure       = as.numeric(tank_closed_date - tank_installed_date) / 365.25,
  closure_year         = year(tank_closed_date),
  texas_treated        = as.integer(state == "TX"),
  install_year         = year(tank_installed_date)
)]
closed_tanks <- closed_tanks[age_at_closure >= 0]
closed_tanks[, age_bin_at_closure := make_age_bin(age_at_closure)]
cat(sprintf("  Closed tanks in study window: %s\n",
    format(nrow(closed_tanks), big.mark=",")))

# Pre-period closures
pre_period_closures <- closed_tanks[closure_year >= 1987 & closure_year <= 1997]

# ─────────────────────────────────────────────────────────────────────────────
# S6: Mandate cohort assignment (Spec A / Spec B, phased deadlines)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- S6: Mandate cohort construction ---\n")

# Facility-level vintage aggregates from 1998 cross-section
fac_cohort <- tanks_1998[, .(
  n_tanks      = .N,
  n_pre_1988   = sum(is_pre_1988),
  n_post_1988  = sum(1L - is_pre_1988),
  mean_age_1998 = mean(tank_age_1998, na.rm = TRUE),
  pct_pre_1988 = mean(is_pre_1988)
), by = panel_id]

annual_data <- merge(annual_data,
  fac_cohort[, .(panel_id, n_tanks, n_pre_1988, n_post_1988,
                 mean_age_1998, pct_pre_1988)],
  by = "panel_id", all.x = TRUE)

annual_data[, `:=`(
  spec_A_eligible = as.integer(n_pre_1988 == 0),
  spec_B_eligible = as.integer(n_post_1988 == 0)
)]

# Phased TX mandate deadlines by vintage cohort
tanks_1998[state == "TX" & is_pre_1988 == 1,
           tx_mandate_deadline := fcase(
  install_year < 1965,                         as.IDate("1989-12-22"),
  install_year %between% c(1965L, 1974L),      as.IDate("1990-12-22"),
  install_year %between% c(1975L, 1979L),      as.IDate("1991-12-22"),
  install_year %between% c(1980L, 1984L),      as.IDate("1992-12-22"),
  install_year %between% c(1985L, 1988L),      as.IDate("1993-12-22"),
  default = NA_integer_)]
tanks_1998[state != "TX" & is_pre_1988 == 1,
           tx_mandate_deadline := FEDERAL_MANDATE_DATE]
tanks_1998[is_pre_1988 == 1,
           years_past_deadline :=
             as.numeric(TREATMENT_DATE - tx_mandate_deadline) / 365.25]

# Facility-level years past deadline
fac_deadline <- tanks_1998[is_pre_1988 == 1,
  .(mean_yrs_past_deadline = mean(years_past_deadline, na.rm = TRUE)),
  by = panel_id]
annual_data <- merge(annual_data, fac_deadline, by = "panel_id", all.x = TRUE)

# Mandate contamination control vars
annual_data[, mandate_active := as.integer(
  state == "TX" & spec_B_eligible == 1 &
  panel_year >= TX_MANDATE_START & panel_year <= TX_MANDATE_END)]
annual_data[, mandate_window_3yr := as.integer(
  state == "TX" & spec_B_eligible == 1 &
  panel_year >= TX_MANDATE_WINDOW_BROAD_START &
  panel_year <= TX_MANDATE_WINDOW_BROAD_END)]

cat(sprintf("  Spec A: %s fac | Spec B: %s fac\n",
    format(uniqueN(annual_data[spec_A_eligible==1, panel_id]), big.mark=","),
    format(uniqueN(annual_data[spec_B_eligible==1, panel_id]), big.mark=",")))

# ─────────────────────────────────────────────────────────────────────────────
# S7: Pre-panel leak flag (exclude pre-1990 leakers from risk set)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- S7: Pre-panel leak flag ---\n")

if (!"panel_id" %in% names(master_lust)) {
  master_lust[, panel_id := paste(toupper(trimws(as.character(facility_id))),
                                   toupper(trimws(as.character(state))),
                                   sep = "_")]
}
if ("report_date" %in% names(master_lust) &&
    !inherits(master_lust$report_date, "IDate"))
  master_lust[, report_date := as.IDate(report_date)]

pre_panel_leakers <- master_lust[
  !is.na(report_date) & report_date < as.Date("1990-01-01"),
  .(pre_panel_leak = 1L), by = panel_id]

if ("pre_panel_leak" %in% names(annual_data)) annual_data[, pre_panel_leak := NULL]
annual_data <- merge(annual_data, pre_panel_leakers, by="panel_id", all.x=TRUE)
annual_data[is.na(pre_panel_leak), pre_panel_leak := 0L]

cat(sprintf("  Pre-1990 leakers flagged: %s facilities (%.1f%%)\n",
    format(uniqueN(pre_panel_leakers$panel_id), big.mark=","),
    100 * uniqueN(pre_panel_leakers$panel_id) / uniqueN(annual_data$panel_id)))

# Validate provenance variables (built by panel builder)
stopifnot("event_first_leak"   %in% names(annual_data))
stopifnot("ever_leaked"        %in% names(annual_data))
stopifnot("has_previous_leak"  %in% names(annual_data))

# ─────────────────────────────────────────────────────────────────────────────
# S8: Vintage cohort variable (for H4 broader sample in 02_DiD)
# ─────────────────────────────────────────────────────────────────────────────
vc_levels <- c("Pre-1965","1965-1974","1975-1979","1980-1984","1985-1988","Post-1988")
if (!"vintage_cohort" %in% names(annual_data)) {
  tanks_1998[, vc := fcase(
    install_year < 1965,                          "Pre-1965",
    install_year %between% c(1965L,1974L),        "1965-1974",
    install_year %between% c(1975L,1979L),        "1975-1979",
    install_year %between% c(1980L,1984L),        "1980-1984",
    install_year %between% c(1985L,1988L),        "1985-1988",
    default =                                     "Post-1988")]
  fac_vc <- tanks_1998[, .(vintage_cohort = names(which.max(table(vc)))),
                        by = panel_id]
  annual_data <- merge(annual_data, fac_vc, by="panel_id", all.x=TRUE)
  annual_data[is.na(vintage_cohort), vintage_cohort := "Post-1988"]
}
annual_data[, vintage_cohort := factor(vintage_cohort, levels = vc_levels)]

# ─────────────────────────────────────────────────────────────────────────────
# S9: Install year on annual_data (for make-model sample restriction in 02)
# ─────────────────────────────────────────────────────────────────────────────
if (!"install_year" %in% names(annual_data)) {
  fac_install <- tanks_1998[, .(install_year = as.integer(min(install_year, na.rm=TRUE))),
                              by = panel_id]
  annual_data <- merge(annual_data, fac_install, by="panel_id", all.x=TRUE)
}

# Attrition final entry
attrition_log[["final"]] <- list(
  stage = "Final analysis sample", filter = "",
  facilities = uniqueN(annual_data$panel_id), fac_years = nrow(annual_data))

# ─────────────────────────────────────────────────────────────────────────────
# Save all intermediates
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Saving intermediates ---\n")
save_interim(annual_data,         "annual_data")
save_interim(tanks,               "tanks")
save_interim(tanks_1998,          "tanks_1998")
save_interim(closed_tanks,        "closed_tanks")
save_interim(pre_period_closures, "pre_period_closures")
save_interim(data_quality_report, "data_quality_report")
save_interim(balance_glm,         "balance_glm")
save_interim(attrition_log,       "attrition_log")
save_interim(incumbent_ids,       "incumbent_ids")

cat(sprintf("\n=== 01c COMPLETE | %s facilities | %s fac-years ===\n",
    format(uniqueN(annual_data$panel_id), big.mark=","),
    format(nrow(annual_data), big.mark=",")))
