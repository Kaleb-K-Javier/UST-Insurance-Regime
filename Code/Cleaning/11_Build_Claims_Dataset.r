###############################################################################
# UST Claims x LUST x Facility Crosswalk – Cleaned Multi-State Claims Table
# Author: <you>   •   Last edit: 2026-02-26
#
# DESCRIPTION:
# This script is split by a toggle: 'RUN_LOCAL'.
#
# 1. SET RUN_LOCAL = TRUE  (local machine):
#    - Cleans and combines all state claims data from raw sources.
#    - Downloads CPI data and computes 2023-real costs.
#    - SAVES: 'Outputs/all_cleaned_claims.csv'           (one row per claim)
#    - SAVES: 'Outputs/facility_month_claims_summary.csv' (monthly summary)
#
# 2. SET RUN_LOCAL = FALSE  (server):
#    - LOADS: 'Data/Processed/all_cleaned_claims.csv' (transfer from local)
#    - LOADS: 'Data/Processed/facility_leak_behavior_annual.csv' (from script 10)
#    - Aggregates claims to ANNUAL resolution, then merges to the annual panel.
#    - SAVES: 'Data/Processed/claims_panel_annual_merged.csv'
#
# ─── PANEL / HARMONIZATION ALIGNMENT NOTES ────────────────────────────────────
# The master panel (facility_leak_behavior_annual.csv, built by
# 10_Build_Annual_Panel_Optimized.R) uses the following key conventions that
# this script must respect:
#
#   panel_id   = paste(facility_id, state, sep = "_")  e.g. "12345_TN"
#   facility_id = RAW state-assigned ID with NO state-abbreviation prefix
#   state       = 2-letter abbreviation, NEVER the full state name
#   time key    = panel_year  (annual; there is NO panel_month in the final output)
#
# State-specific merge key logic (from 10_Master_Cleaning_and_Harmonization.r):
#   TX, CO, LA, ME, NJ, NM, TN, AR : paste0(state, facility_id) for EPA geo only;
#                                     raw facility_id stored in panel
#   MI                              : strip leading zeros from facility_id
#   AL                              : numeric chars only in facility_id
#   OK                              : OK[facility_id] in EPA; raw stored in panel
#   PA                              : permit_number (XX-XXXXX) format
#
# KEY FIX — Tennessee (TN):
#   The raw TN facility ID is the numeric divisionsiteid.  The harmonization
#   script stores facility_id = "12345" (raw) with state = "TN" in the panel.
#   Previous versions of this script stored facility_id = "TN12345" which
#   NEVER matched the panel. Prefix is now stripped at finalization.
#
# KEY FIX — Server-side merge:
#   Previous versions loaded 'facility_leak_behavior_monthly.csv' (does not
#   exist as a final output) and merged on (facility_id, state, year, month).
#   Corrected to load 'facility_leak_behavior_annual.csv' and aggregate claims
#   to annual before merging on (panel_id, panel_year).
#
# KEY FIX — State name map:
#   Removed from the server-side merge path. The panel stores 'state' as a
#   2-letter abbreviation; applying the old map produced zero matches.
###############################################################################


# ===================================================================
# CONTROL SCRIPT:
# SET 'TRUE' to run data cleaning locally and save intermediate files.
# SET 'FALSE' to run the panel merge on a server (loads saved files).
# ===================================================================
RUN_LOCAL <- FALSE
# ===================================================================


## ── libraries ────────────────────────────────────────────────────────────────
library(data.table)
library(readxl)
library(janitor)
library(lubridate)
library(stringr)
library(here)

if (!requireNamespace("fredr", quietly = TRUE)) install.packages("fredr")
library(fredr)
fredr_set_key("2d9a1f281713753ddce9c4250540c2aa")


## ── global helpers ───────────────────────────────────────────────────────────

# Normalise an ID column: upper-case, trim whitespace, optionally add prefix
norm_id <- function(x, prefix = NULL) {
  x <- toupper(trimws(as.character(x)))
  if (!is.null(prefix)) {
    need_prefix <- !is.na(x) & !startsWith(x, prefix)
    x[need_prefix] <- paste0(prefix, x[need_prefix])
  }
  x
}

# Safely parse dates across multiple format strings
safe_parse <- function(x) {
  as.Date(lubridate::parse_date_time(
    x,
    orders = c("ymd", "mdy", "dmy", "Y-m-d", "m/d/Y"),
    quiet  = TRUE
  ))
}

# Finalise a state claims table:
#   • facility_id = raw panel ID (NO state-abbreviation prefix)
#   • state       = 2-letter abbreviation
#   • All IDs coerced to character
finalize_claims <- function(dt, state_abbrev) {
  dt[, claim_start_year  := year(claims_start_date)]
  dt[, claim_end_year    := year(claims_end_date)]
  dt[, state             := state_abbrev]
  dt[, facility_id       := as.character(facility_id)]
  dt[, lust_id           := as.character(lust_id)]
  # Build panel_id to match the panel's composite key convention
  dt[, panel_id          := paste(facility_id, state, sep = "_")]
  setcolorder(
    dt,
    c("panel_id", "facility_id", "lust_id",
      "claims_start_date", "claims_end_date",
      "total_cost", "claim_start_year", "claim_end_year", "state")
  )
  dt[]
}

# State-to-full-name map — kept for the monthly summary display only;
# NOT applied before any panel merge (panel uses abbreviations).
state_name_map <- c(
  LA = "Louisiana",
  TN = "Tennessee",
  NM = "New Mexico",
  UT = "Utah",
  CO = "Colorado",
  PA = "Pennsylvania"
)


###############################################################################
# ═══════════════════════════════════════════════════════════════════════════════
# RUN_LOCAL = TRUE  — clean raw state files and save intermediate CSVs
# ═══════════════════════════════════════════════════════════════════════════════
###############################################################################

if (RUN_LOCAL) {

  cat("=================================================================\n")
  cat("RUN_LOCAL = TRUE: Local data cleaning and prep\n")
  cat("=================================================================\n\n")

  ## ── CPI (annual CPI-U, base = 2023 average) ──────────────────────────────
  cat("Downloading CPI data from FRED...\n")
  cpi_dt <- as.data.table(
    fredr(
      series_id         = "CPIAUCSL",
      observation_start = as.Date("1980-01-01"),
      observation_end   = as.Date("2025-04-01"),
      frequency         = "m"
    )
  )[, .(YEAR = year(date), MONTH = month(date), CPI = value)]

  cpi_2023_avg         <- mean(cpi_dt[YEAR == 2023, CPI])
  cpi_dt[, cpi_factor := cpi_2023_avg / CPI]
  cat(sprintf("   CPI 2023 average base: %.4f\n", cpi_2023_avg))


  ##############################################################################
  # SECTION A — Louisiana (LA)
  ##############################################################################
  cat("Processing Louisiana (LA)...\n")

  la_claims_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/Claims/LA Claims.xlsx"
  la_claims <- read_excel(la_claims_path, sheet = 1) |> data.table() |> clean_names()

  la_claims[, claims_start_date := ymd(x1st_app_received_date)]
  la_claims[, claims_end_date   := ymd(last_app_processed_date)]
  la_claims[, total_cost        := as.numeric(total_requested_amt)]
  # facility_id = raw numeric ai_num — matches panel's raw LA ID
  la_claims[, facility_id       := norm_id(ai_num, prefix = NULL)]
  la_claims[, lust_id           := as.character(ai_num)]

  la_clean <- finalize_claims(
    la_claims[, .(facility_id, lust_id, claims_start_date, claims_end_date, total_cost)],
    "LA"
  )


  ##############################################################################
  # SECTION B — Tennessee (TN)
  #
  # PANEL ALIGNMENT FIX:
  #   The master panel stores facility_id = divisionsiteid (raw numeric, e.g.
  #   "12345") with state = "TN".  Previous versions stored "TN12345" which
  #   never matched.  This section now stores the raw divisionsiteid as
  #   facility_id, consistent with the harmonization script convention:
  #     merge_id = paste0("TN", facility_id)  ← used only for EPA geo lookup,
  #                                              NOT stored as facility_id itself.
  ##############################################################################
  cat("Processing Tennessee (TN)...\n")

  tn_lust_path      <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Tennessee/Tennessee_UST_LUST.csv"
  tn_claims_path    <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Tennessee/ust_all-tn-reimbursement-sites.xlsx"
  tn_fac_extra_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Tennessee/ust_all-tn-compartments.xlsx"

  tn_lust   <- fread(tn_lust_path)
  tn_claims <- read_excel(tn_claims_path) |> data.table() |> clean_names()
  tn_extra  <- read_excel(tn_fac_extra_path) |> data.table() |> clean_names()

  # Standardise LUST file — keep raw divisionsiteid for joining
  tn_lust[, divisionsiteid := trimws(as.character(sub("^TN", "", facility_id)))]
  tn_lust[, casenumber     := sub(".*-(\\d+)$", "\\1", LUST_id)]
  tn_lust[, lust_facility_id := divisionsiteid]          # raw, no prefix
  tn_lust[, report_date    := ymd_hms(report_date)]

  # Claims: keep raw divisionsiteid as facility_id (no "TN" prefix)
  tn_claims[, divisionsiteid := trimws(as.character(divisionsiteid))]
  tn_claims[, casenumber     := trimws(as.character(casenumber))]
  tn_claims[, facility_id    := divisionsiteid]           # raw — matches panel
  tn_claims[, LUST_id        := paste0("TN", divisionsiteid, "-", casenumber)]
  tn_claims[, total_cost     := as.numeric(paidrequested)]
  tn_claims <- tn_claims[status == "Closed"]

  setkey(tn_claims, divisionsiteid, casenumber)
  setkey(tn_lust,   divisionsiteid, casenumber)

  clm2lust <- merge(
    tn_claims,
    tn_lust[, .(divisionsiteid, casenumber, LUST_id,
                lust_facility_id, report_date)],
    by = c("divisionsiteid", "casenumber")
  )

  # Filter to regulated gas-station facilities using raw divisionsiteid
  tn_extra[, raw_facility_id := trimws(as.character(facility_id_ust))]  # raw
  tn_extra[, gas_station     := facility_type == "Gas Station or Truck Stop"]
  tn_extra[, regulated_ok    := regulated_status == "Regulated"]
  allowed_ids <- tn_extra[gas_station & regulated_ok, unique(raw_facility_id)]
  clm2lust    <- clm2lust[facility_id %in% allowed_ids]

  tn_clean <- finalize_claims(
    clm2lust[, .(
      facility_id,
      lust_id           = LUST_id,
      claims_start_date = as.Date(report_date),
      claims_end_date   = as.Date(NA),
      total_cost
    )],
    "TN"
  )


  ##############################################################################
  # SECTION C — New Mexico (NM)
  ##############################################################################
  cat("Processing New Mexico (NM)...\n")

  nm_claim_paths <- c(
    "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/New Mexico/rp_nfa_amounts.xlsx",
    "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/New Mexico/sl_nfa_amounts.xlsx"
  )

  nm_claims_rp <- read_excel(nm_claim_paths[1]) |> data.table() |> clean_names()
  nm_claims_sl <- read_excel(nm_claim_paths[2]) |> data.table() |> clean_names()
  nm_claims_rp <- nm_claims_rp[total_wp > 0]
  nm_claims    <- rbind(nm_claims_rp, nm_claims_sl, fill = TRUE)

  rid_col <- grep("^r\\s*id$", names(nm_claims), ignore.case = TRUE, value = TRUE)[1]
  fid_col <- grep("^f\\s*id$", names(nm_claims), ignore.case = TRUE, value = TRUE)[1]
  setnames(nm_claims, rid_col, "lust_id")
  setnames(nm_claims, fid_col, "facility_id")

  nm_claims[, lust_id           := norm_id(lust_id,     prefix = NULL)]
  nm_claims[, facility_id       := norm_id(facility_id, prefix = NULL)]
  nm_claims[, claims_start_date := as.Date(ymd_hms(release_date))]
  nm_claims[, claims_end_date   := as.Date(ymd_hms(nfa_date))]
  nm_claims[, total_cost        := as.numeric(total_wp)]

  nm_clean <- finalize_claims(
    nm_claims[, .(facility_id, lust_id, claims_start_date, claims_end_date, total_cost)],
    "NM"
  )


  ##############################################################################
  # SECTION D — Utah (UT)
  ##############################################################################
  cat("Processing Utah (UT)...\n")

  ut_leaks_path  <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Utah/utah_state_LUSTs.xls"
  ut_claims_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Utah/Utah PST Fund Cleanup Amounts.xlsx"

  ut_leaks  <- read_excel(ut_leaks_path)  |> data.table() |> clean_names()
  ut_claims <- read_excel(ut_claims_path) |> data.table() |> clean_names()

  setnames(ut_claims, "rel_id",          "leak_id")
  setnames(ut_claims, "pst_amount_paid", "total_cost")

  ut_leaks [, leak_id     := norm_id(leak_id,     prefix = NULL)]
  ut_leaks [, facility_id := norm_id(facility_id, prefix = NULL)]
  ut_claims[, leak_id     := norm_id(leak_id,     prefix = NULL)]
  ut_claims[, facility_id := norm_id(facility_id, prefix = NULL)]
  ut_claims[, total_cost  := as.numeric(total_cost)]

  ut_leaks[, claims_start_date := as.Date(ymd(notif_date))]
  ut_leaks[, claims_end_date   := as.Date(ymd(date_closed))]

  ut_join <- merge(
    ut_leaks,
    ut_claims[, .(leak_id, facility_id, total_cost)],
    by = c("leak_id", "facility_id")
  )

  ut_clean <- finalize_claims(
    ut_join[, .(facility_id, lust_id = leak_id,
                claims_start_date, claims_end_date, total_cost)],
    "UT"
  )


  ##############################################################################
  # SECTION E — Colorado (CO)
  ##############################################################################
  cat("Processing Colorado (CO)...\n")

  co_releases_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Colorado/Petroleum_Releases__Colorado_Oil___Public_Safety_20250728.csv"
  co_claims_path   <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Colorado/CO Claims.xlsx"

  co_releases <- fread(co_releases_path) |> clean_names()
  co_claims   <- read_excel(co_claims_path) |> data.table() |> clean_names()

  legacy_col <- grep("legacy.*event.*id",  names(co_releases), value = TRUE)[1]
  relno_col  <- grep("^release.*number$",  names(co_releases), value = TRUE)[1]
  fid_col    <- grep("^facility.*id$",     names(co_releases), value = TRUE)[1]
  setnames(co_releases, legacy_col, "lust_id")
  setnames(co_releases, relno_col,  "release_number")
  setnames(co_releases, fid_col,    "facility_id")

  co_releases[, lust_id        := norm_id(lust_id,        prefix = NULL)]
  co_releases[, facility_id    := norm_id(facility_id,    prefix = NULL)]
  co_releases[, release_number := toupper(trimws(as.character(release_number)))]
  co_releases[, release_date   := as.character(release_date)]

  setnames(co_claims, "total_amount_paid_for_reimbursement", "total_cost")
  co_claims[, facility_id    := norm_id(facility_id, prefix = NULL)]
  co_claims[, release_number := toupper(trimws(as.character(release_number)))]
  co_claims[, total_cost     := as.numeric(total_cost)]
  co_claims[, release_date   := as.character(release_date)]
  co_claims[, claims_end_date   := as.Date(mdy(nfa_letter_sent_date))]
  co_claims[, claims_start_date := as.Date(mdy(release_date))]

  co_claims <- unique(
    co_claims[, .(facility_id, release_number, release_date,
                  claims_start_date, claims_end_date, total_cost)]
  )

  co_join <- merge(
    co_releases[, .(facility_id, release_number, release_date, lust_id)],
    co_claims  [, .(facility_id, release_number, release_date,
                    claims_start_date, claims_end_date, total_cost)],
    by = c("facility_id", "release_number", "release_date")
  )

  co_clean <- finalize_claims(
    co_join[, .(facility_id, lust_id, claims_start_date, claims_end_date, total_cost)],
    "CO"
  )


  ##############################################################################
  # SECTION F — Wisconsin (WI)  [PLACEHOLDER — crosswalk pending]
  ##############################################################################
  # Blocked on Activity_detail_no <-> Facility Id crosswalk.


  ##############################################################################
  # SECTION G — North Carolina (NC)  [PLACEHOLDER — crosswalk pending]
  ##############################################################################
  # Blocked on Incident Number <-> Facility Id crosswalk.


  ##############################################################################
  # SECTION PA — Pennsylvania (PA)
  #
  # Source: PA_UST_Auction_Analysis repo (separate from UST-Insurance project).
  # Join key: claims$department (permit_number "XX-XXXXX")
  #           -> facility_linkage_table$permit_number -> facility_id (PA DEP ID)
  # The PA DEP facility_id is stored verbatim in the master panel for PA.
  #
  # NOTE: In the MAIN panel (facility_leak_behavior_annual.csv), PA sits in
  # study_group = "2. Control Tier 1 (Custom)" and treatment_group = "Control".
  # (treated_states in the panel script does NOT include PA.)
  # PA claims are therefore control-group cost data for L(x_t) calibration
  # under THIS project's framing.
  ##############################################################################
  cat("Processing Pennsylvania (PA)...\n")

  pa_repo_base  <- "C:/Users/kalebkja/PA_UST_Auction_Analysis"
  pa_claims_csv <- file.path(pa_repo_base, "data", "processed", "claims_clean.csv")
  pa_linkage    <- file.path(pa_repo_base, "data", "external", "padep",
                             "facility_linkage_table.csv")

  if (!file.exists(pa_claims_csv))
    stop(paste("PA claims not found:", pa_claims_csv,
               "\nRun R/etl/01_load_ustif_data.R in the PA repo first."))
  if (!file.exists(pa_linkage))
    stop(paste("PA linkage table not found:", pa_linkage,
               "\nRun R/etl/02a_padep_download.R in the PA repo first."))

  pa_claims_raw <- fread(pa_claims_csv, colClasses = "character") |> clean_names()
  pa_link_dt    <- fread(pa_linkage,    colClasses = "character") |> clean_names()

  cat(sprintf("   PA claims loaded: %d rows | columns: %s\n",
              nrow(pa_claims_raw), paste(names(pa_claims_raw), collapse = ", ")))

  # Normalise the join key on both sides
  pa_link_key <- pa_link_dt[, .(
    permit_key  = toupper(trimws(permit_number)),
    facility_id = as.character(trimws(facility_id))
  )] |> unique()

  pa_claims_raw[, dept_key := toupper(trimws(department))]

  # Resolve cost column
  cost_col <- intersect(
    c("total_cost", "amount_paid", "amount_paid_to_date",
      "total_amount_paid", "paid_amount", "reimbursement_amount"),
    names(pa_claims_raw)
  )[1]
  if (is.na(cost_col))
    cost_col <- grep("amount|cost|paid", names(pa_claims_raw), value = TRUE)[1]
  if (is.na(cost_col))
    stop("Cannot identify cost column in PA claims. Inspect column names above.")
  cat(sprintf("   Using '%s' as cost column\n", cost_col))
  pa_claims_raw[, total_cost := as.numeric(get(cost_col))]

  # Resolve date columns
  open_col  <- intersect(
    c("claim_open_date", "open_date", "date_opened",
      "first_payment_date", "loss_date", "incident_date"),
    names(pa_claims_raw)
  )[1]
  close_col <- intersect(
    c("claim_close_date", "close_date", "date_closed",
      "last_payment_date", "nfa_date"),
    names(pa_claims_raw)
  )[1]
  lust_col  <- intersect(
    c("lust_id", "claim_number", "incident_number", "release_id"),
    names(pa_claims_raw)
  )[1]

  if (is.na(open_col)) stop("Cannot identify open-date column in PA claims.")
  if (is.na(lust_col)) stop("Cannot identify LUST/claim ID column in PA claims.")

  pa_claims_raw[, claims_start_date := safe_parse(get(open_col))]
  pa_claims_raw[, claims_end_date   :=
    if (!is.na(close_col)) safe_parse(get(close_col)) else as.Date(NA)]
  pa_claims_raw[, lust_id           := as.character(get(lust_col))]

  # Inner join: claims -> linkage table -> facility_id
  pa_merged <- merge(
    pa_claims_raw[, .(dept_key, lust_id, claims_start_date, claims_end_date, total_cost)],
    pa_link_key,
    by.x  = "dept_key",
    by.y  = "permit_key",
    all.x = FALSE
  )

  cat(sprintf("   Matched %d of %d PA claims (%.1f%%)\n",
              nrow(pa_merged), nrow(pa_claims_raw),
              100 * nrow(pa_merged) / max(nrow(pa_claims_raw), 1)))

  pa_merged <- pa_merged[!is.na(total_cost) & total_cost > 0 &
                           !is.na(claims_start_date)]

  pa_clean <- finalize_claims(
    pa_merged[, .(facility_id, lust_id, claims_start_date, claims_end_date, total_cost)],
    "PA"
  )
  cat(sprintf("   PA clean rows: %d\n", nrow(pa_clean)))


  ##############################################################################
  # SECTION H — Bind all states
  ##############################################################################
  all_cleaned_claims <- rbindlist(
    list(la_clean, tn_clean, nm_clean, ut_clean, co_clean, pa_clean),
    use.names = TRUE, fill = TRUE
  )

  ## Attach CPI and compute real costs
  cat("Attaching CPI and calculating real costs...\n")
  all_cleaned_claims[, claim_start_year  := year(claims_start_date)]
  all_cleaned_claims[, claim_start_month := month(claims_start_date)]

  all_cleaned_claims <- merge(
    all_cleaned_claims,
    cpi_dt[, .(claim_start_year = YEAR, claim_start_month = MONTH, cpi_factor)],
    by    = c("claim_start_year", "claim_start_month"),
    all.x = TRUE
  )

  all_cleaned_claims[, total_cost      := as.numeric(total_cost)]
  all_cleaned_claims[, total_cost_2023 := total_cost * cpi_factor]

  # Type-safety check
  all_cleaned_claims[, facility_id := as.character(facility_id)]
  all_cleaned_claims[, lust_id     := as.character(lust_id)]
  cat(sprintf("   facility_id type : %s\n", class(all_cleaned_claims$facility_id)))
  cat(sprintf("   lust_id type     : %s\n", class(all_cleaned_claims$lust_id)))

  ## OUTPUT 1 — full claims table (one row per claim)
  dir.create(here("Outputs"), showWarnings = FALSE, recursive = TRUE)
  output_claims_path <- here("Outputs", "all_cleaned_claims.csv")
  fwrite(all_cleaned_claims, output_claims_path)
  cat(sprintf("   Saved: %s\n", output_claims_path))

  ## Monthly summary (for descriptive analysis / figures)
  claims_monthly <- all_cleaned_claims[
    !is.na(facility_id) & !is.na(claim_start_year) & !is.na(claim_start_month),
    .(
      claims_n             = .N,
      claims_total_nominal = sum(total_cost,      na.rm = TRUE),
      claims_total_2023    = sum(total_cost_2023, na.rm = TRUE),
      claim_earliest_date  = min(claims_start_date, na.rm = TRUE),
      claim_latest_date    = max(claims_start_date, na.rm = TRUE)
    ),
    by = .(
      panel_id,
      facility_id,
      state,
      panel_year  = claim_start_year,
      panel_month = claim_start_month
    )
  ][panel_year >= 1990 & panel_year <= 2023]

  claims_monthly[, month_key := sprintf("%d-%02d", panel_year, panel_month)]

  ## OUTPUT 2 — monthly summary
  output_monthly_path <- here("Outputs", "facility_month_claims_summary.csv")
  fwrite(claims_monthly, output_monthly_path)
  cat(sprintf("   Saved: %s\n", output_monthly_path))

  cat("\n✓ LOCAL RUN COMPLETE\n")
  cat("  Transfer 'Outputs/all_cleaned_claims.csv' to the server.\n\n")


} else {

  ###############################################################################
  # ═══════════════════════════════════════════════════════════════════════════════
  # RUN_LOCAL = FALSE  — server-side merge to annual panel
  # ═══════════════════════════════════════════════════════════════════════════════
  #
  # DESIGN RATIONALE:
  #   The final panel is ANNUAL (facility_leak_behavior_annual.csv).  There is
  #   no final monthly panel file.  Claims are therefore aggregated to the
  #   facility-year level before merging, preserving:
  #     claims_n_year        — number of distinct claims in that year
  #     claims_total_2023    — total inflation-adjusted payout
  #     claims_first_date    — earliest claim date in that year
  #   The merged file can then be used directly in regression specifications.
  ###############################################################################

  cat("=================================================================\n")
  cat("RUN_LOCAL = FALSE: Server-side annual panel merge\n")
  cat("=================================================================\n\n")

  ## ── 1. Load claims ────────────────────────────────────────────────────────
  cat("Loading all_cleaned_claims.csv...\n")
  claims_path <- here("Data", "Processed", "all_cleaned_claims.csv")
  if (!file.exists(claims_path))
    stop(sprintf("File not found: %s\nTransfer from local run first.", claims_path))

  claims_full <- fread(claims_path)
  claims_full[, facility_id := as.character(facility_id)]
  claims_full[, lust_id     := as.character(lust_id)]
  # Reconstruct panel_id in case it was dropped during CSV round-trip
  claims_full[, panel_id := paste(facility_id, state, sep = "_")]
  cat(sprintf("   Loaded %d claims across %d states\n",
              nrow(claims_full), uniqueN(claims_full$state)))

  ## ── 2. Load annual panel ─────────────────────────────────────────────────
  # CORRECTED: final panel output is facility_leak_behavior_annual.csv
  # (produced by 10_Build_Annual_Panel_Optimized.R Section 11).
  # The 'monthly_intermediate' file is an internal checkpoint, NOT the merge target.
  cat("Loading facility_leak_behavior_annual.csv...\n")
  panel_path <- here("Data", "Processed", "facility_leak_behavior_annual.csv")
  if (!file.exists(panel_path))
    stop(sprintf("Panel file not found: %s", panel_path))

  panel <- fread(panel_path)
  panel[, facility_id := as.character(facility_id)]
  panel[, state       := toupper(trimws(state))]  # ensure abbreviation
  panel[, panel_id    := paste(facility_id, state, sep = "_")]
  cat(sprintf("   Loaded %d facility-years | %d states | years %d–%d\n",
              nrow(panel),
              uniqueN(panel$state),
              min(panel$panel_year, na.rm = TRUE),
              max(panel$panel_year, na.rm = TRUE)))

  ## ── 3. Aggregate claims to ANNUAL resolution ─────────────────────────────
  # The annual panel has one row per (panel_id, panel_year).
  # Aggregate claims accordingly before merging to avoid duplication.
  cat("Aggregating claims to annual resolution...\n")

  claims_annual <- claims_full[
    !is.na(total_cost_2023) & total_cost_2023 > 0 & !is.na(claim_start_year),
    .(
      claims_n_year        = .N,
      claims_total_nominal = sum(total_cost,      na.rm = TRUE),
      claims_total_2023    = sum(total_cost_2023, na.rm = TRUE),
      claims_first_date    = min(claims_start_date, na.rm = TRUE),
      claims_last_date     = max(claims_start_date, na.rm = TRUE)
    ),
    by = .(panel_id, facility_id, state, panel_year = claim_start_year)
  ]

  cat(sprintf("   Annual claims records: %d\n", nrow(claims_annual)))

  ## ── 4. Pre-merge verification ─────────────────────────────────────────────
  cat("\n=== PRE-MERGE VERIFICATION ===\n")
  cat(sprintf("Claims panel_id type  : %s  | example: %s\n",
              class(claims_annual$panel_id),
              head(claims_annual$panel_id, 1)))
  cat(sprintf("Panel  panel_id type  : %s  | example: %s\n",
              class(panel$panel_id),
              head(panel$panel_id, 1)))
  cat(sprintf("Claims state values   : %s\n",
              paste(sort(unique(claims_annual$state)), collapse = ", ")))
  cat(sprintf("Panel states with claims data:\n"))
  print(panel[state %in% unique(claims_annual$state),
              .(n_fac_years = .N, sample_id = panel_id[1]),
              by = state][order(state)])

  cat("\nClaims per state in annual aggregation:\n")
  print(claims_annual[, .(
    n_facility_years = .N,
    n_facilities     = uniqueN(panel_id),
    year_range       = paste(min(panel_year), max(panel_year), sep = "-")
  ), by = state][order(state)])

  ## ── 5. Inner join: annual claims -> annual panel ──────────────────────────
  # CORRECTED merge keys:
  #   panel_id   (= "12345_TN") — composite key, matches exactly
  #   panel_year (integer)      — annual; no panel_month in final output
  #
  # NOTE: State is embedded in panel_id so is not needed as a separate key,
  # but included for clarity and to catch any cross-state panel_id collisions.
  cat("\nMerging annual claims to annual panel...\n")

  claims_panel_merged <- merge(
    claims_annual,
    panel,
    by        = c("panel_id", "state", "panel_year"),
    all       = FALSE   # inner join — keep only matched rows
  )

  cat(sprintf("   Merged rows   : %s\n", format(nrow(claims_panel_merged), big.mark = ",")))
  cat(sprintf("   Merged facilities: %s\n", format(uniqueN(claims_panel_merged$panel_id), big.mark = ",")))

  ## ── 6. Save ───────────────────────────────────────────────────────────────
  output_path <- here("Data", "Processed", "claims_panel_annual_merged.csv")
  fwrite(claims_panel_merged, output_path)
  cat(sprintf("\n✓ Saved: %s\n", output_path))

  ## ── 7. Match summary ─────────────────────────────────────────────────────
  cat("\n=== MATCH SUMMARY BY STATE ===\n")
  match_summary <- claims_panel_merged[, .(
    matched_facility_years = .N,
    matched_facilities     = uniqueN(panel_id),
    median_cost_2023       = median(claims_total_2023, na.rm = TRUE)
  ), by = state][order(-matched_facility_years)]
  print(match_summary)

  # Unmatched claims diagnosis
  cat("\n=== UNMATCHED CLAIMS DIAGNOSIS ===\n")
  unmatched <- claims_annual[!panel_id %in% claims_panel_merged$panel_id]
  if (nrow(unmatched) > 0) {
    cat(sprintf("   %d claim-years did not match the panel\n", nrow(unmatched)))
    print(unmatched[, .(
      n_unmatched   = .N,
      sample_ids    = paste(head(unique(panel_id), 3), collapse = ", ")
    ), by = state][order(-n_unmatched)])
  } else {
    cat("   All claim-years matched the panel.\n")
  }

  cat("\n✓ SERVER RUN COMPLETE\n\n")

} # End of script