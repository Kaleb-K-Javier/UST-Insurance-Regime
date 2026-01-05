###############################################################################
# UST Claims x LUST x Facility Crosswalk – Cleaned Multi-State Claims Table
# Author: <you>   •   Last edit: 2025-11-17
#
#
#
# DESCRIPTION:
# This script is split by a toggle: 'RUN_LOCAL'.
#
# 1. SET RUN_LOCAL = TRUE:
#    - Runs on your local machine.
#    - Cleans and combines all state claims data from raw sources.
#    - Downloads CPI data and computes 2023-real costs.
#    - SAVES: 'Outputs/all_cleaned_claims.csv'
#    - SAVES: 'Outputs/facility_month_claims_summary.csv'
#
# 2. SET RUN_LOCAL = FALSE:
#    - Runs on your server.
#    - SKIPS all local cleaning.
#    - LOADS: 'Outputs/all_cleaned_claims.csv' (which you must transfer)
#    - LOADS: 'Data/Processed/facility_leak_behavior_monthly.csv'
#    - Merges them and SAVES: 'Data/Processed/claims_panel_merged_matches_SAMPLE.csv'
#
# FIX APPLIED (2025-11-17):
#    - Added explicit character type conversion for facility_id to prevent
#      data type mismatch errors during merge (integer vs character)
#    - Added verification checks to confirm data types are correct
###############################################################################



#
# UTAH AND COLORADO ARE NOT IN PANEL!!! SO CANT MATCH THEM?
#

# ===================================================================
# CONTROL SCRIPT: 
# SET 'TRUE' to run data cleaning locally and save intermediate files.
# SET 'FALSE' to run the panel merge on a server (loads saved files).
# ===================================================================
RUN_LOCAL <- FALSE 
# ===================================================================


## ── libraries (needed for both runs) ─────────────────────────────────────────
library(data.table)
library(readxl)
library(janitor)
library(lubridate)
library(stringr)
library(here)         

# Install fredr if not already installed
if (!requireNamespace("fredr", quietly = TRUE)) {
    install.packages("fredr")
}
library(fredr)          
fredr_set_key("2d9a1f281713753ddce9c4250540c2aa") 

## ── global helper: functions ─────────────────────────────────────────────────
norm_id <- function(x, prefix = NULL) {
  x <- as.character(x)
  x <- trimws(toupper(x))

  if (!is.null(prefix)) {
    # work only on non-missing rows
    need_prefix <- !is.na(x) & !startsWith(x, prefix)
    x[need_prefix] <- paste0(prefix, x[need_prefix])
  }
  x
}
finalize_claims <- function(dt, state_abbrev) {
  dt[, claim_start_year := year(claims_start_date)]
  dt[, claim_end_year   := year(claims_end_date)]
  dt[, state := state_abbrev]
  # FIX: Ensure facility_id is character type
  dt[, facility_id := as.character(facility_id)]
  dt[, lust_id := as.character(lust_id)]
  setcolorder(
    dt,
    c("facility_id","lust_id","claims_start_date","claims_end_date",
      "total_cost","claim_start_year","claim_end_year","state")
  )
  dt[]
}

## ── global helper: state map (needed for both runs) ──────────────────────────
state_name_map <- c(
  LA = "Louisiana",
  TN = "Tennessee",
  NM = "New Mexico",
  UT = "Utah",
  CO = "Colorado",
  WI = "Wisconsin",
  NC = "North Carolina"
)

###############################################################################
# START SCRIPT LOGIC
###############################################################################

if (RUN_LOCAL) {
  
  cat("=================================================================\n")
  cat("RUN_LOCAL = TRUE: Running local data cleaning and prep...\n")
  cat("=================================================================\n\n")

  ## ── CPI SERIES (annual CPI-U, base = 2023) ───────────────────────────────────
  cat("Downloading CPI data from FRED...\n")
  cpi_dt <- as.data.table(
    fredr(
      series_id         = "CPIAUCSL",
      observation_start = as.Date("1980-01-01"),
      observation_end   = as.Date("2025-04-01"),
      frequency         = "m"
    )
  )[, .(YEAR = year(date), MONTH = month(date), CPI = value)]

  # --- CPI FIX: Use a single annual average for the 2023 base ---
  cpi_2023_monthly <- cpi_dt[YEAR == 2023, CPI]
  cpi_2023_avg <- mean(cpi_2023_monthly)
  cpi_dt[, cpi_factor := cpi_2023_avg / CPI]
  cat(sprintf("   - CPI 2023 Average Base: %f\n", cpi_2023_avg))


  ###############################################################################
  # SECTION A — Louisiana (LA)
  ###############################################################################
  cat("Processing Louisiana (LA)...\n")
  la_claims_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/Claims/LA Claims.xlsx"
  la_claims <- read_excel(la_claims_path, sheet = 1) |> data.table() |> clean_names()
  
  la_claims[, claims_start_date := ymd(x1st_app_received_date)]
  la_claims[, claims_end_date   := ymd(last_app_processed_date)]
  la_claims[, total_cost        := as.numeric(total_requested_amt)]
  # --- FIX: Removed "LA" prefix to match panel data ---
  la_claims[, facility_id       := norm_id(ai_num, prefix = NULL)] 
  la_claims[, lust_id           := ai_num]   # provisional
  
  la_clean <- finalize_claims(
    la_claims[, .(facility_id, lust_id, claims_start_date, claims_end_date, total_cost)],
    "LA"
  )
  
  
  ###############################################################################
  # SECTION B — Tennessee (TN)
  ###############################################################################
  cat("Processing Tennessee (TN)...\n")
  # NOTE: TN logic is correct and left as-is, as its panel IDs include the prefix.
  tn_lust_path      <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Tennessee/Tennessee_UST_LUST.csv"
  tn_claims_path    <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Tennessee/ust_all-tn-reimbursement-sites.xlsx"
  tn_fac_extra_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Tennessee/ust_all-tn-compartments.xlsx"
  
  tn_lust   <- fread(tn_lust_path)
  tn_claims <- read_excel(tn_claims_path) |> data.table() |> clean_names()
  tn_extra  <- read_excel(tn_fac_extra_path) |> data.table() |> clean_names()
  
  tn_lust[, divisionsiteid := sub("^TN", "", facility_id)]
  tn_lust[, casenumber     := sub(".*-(\\d+)$", "\\1", LUST_id)]
  tn_lust[, facility_id    := norm_id(facility_id, "TN")]
  tn_lust[, report_date    := ymd_hms(report_date)]
  
  tn_claims[, facility_id    := paste0("TN", trimws(as.character(divisionsiteid)))]
  tn_claims[, divisionsiteid := trimws(as.character(divisionsiteid))]
  tn_claims[, casenumber     := trimws(as.character(casenumber))]
  tn_claims[, LUST_id       := paste0("TN", divisionsiteid, "-", casenumber)]
  tn_claims[, total_cost     := as.numeric(paidrequested)]
  tn_claims <- tn_claims[status == "Closed"]
  
  setkey(tn_claims, divisionsiteid, casenumber)
  setkey(tn_lust,   divisionsiteid, casenumber)
  
  clm2lust <- merge(
    tn_claims,
    tn_lust[, .(divisionsiteid, casenumber, LUST_id , facility_id, report_date)],
    by = c("divisionsiteid","casenumber", "facility_id", "LUST_id"))
  
  tn_extra[, facility_id := paste0("TN", trimws(as.character(facility_id_ust)))]
  tn_extra[, gas_station  := facility_type == "Gas Station or Truck Stop"]
  tn_extra[, regulated_ok := regulated_status == "Regulated"]
  allowed_ids <- tn_extra[gas_station & regulated_ok, unique(facility_id)]
  clm2lust    <- clm2lust[facility_id %in% allowed_ids]
  
  tn_clean <- finalize_claims(
    clm2lust[, .(
      facility_id,
      lust_id           = LUST_id,
      claims_start_date = report_date,
      claims_end_date   = NA,
      total_cost
    )],
    "TN"
  )
  
  tn_clean[, claims_start_date := as.Date(claims_start_date)]
  tn_clean[, claims_end_date   := as.Date(claims_end_date)]
  
  
  ###############################################################################
  # SECTION C — New Mexico (NM)
  ###############################################################################
  cat("Processing New Mexico (NM)...\n")
  nm_claim_paths <- c(
      "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/New Mexico/rp_nfa_amounts.xlsx",
      "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/New Mexico/sl_nfa_amounts.xlsx"
  )
  
  nm_claims_rp <- read_excel(nm_claim_paths[1]) |> data.table() |> clean_names()
  nm_claims_sl <- read_excel(nm_claim_paths[2]) |> data.table() |> clean_names()
  
  nm_claims_rp <- nm_claims_rp[total_wp > 0]
  nm_claims <- rbind(nm_claims_rp, nm_claims_sl, fill = TRUE)
  
  rid_col <- grep("^r\\s*id$", names(nm_claims), ignore.case = TRUE, value = TRUE)[1]
  fid_col <- grep("^f\\s*id$", names(nm_claims), ignore.case = TRUE, value = TRUE)[1]
  setnames(nm_claims, rid_col, "lust_id")
  setnames(nm_claims, fid_col, "facility_id")
  
  # --- FIX: Removed "NM" prefix to match panel data ---
  nm_claims[, lust_id     := norm_id(lust_id, prefix = NULL)]
  nm_claims[, facility_id := norm_id(facility_id, prefix = NULL)]
  
  nm_claims[, claims_start_date := ymd_hms(release_date)]
  nm_claims[, claims_end_date   := ymd_hms(nfa_date)]
  nm_claims[, total_cost        := as.numeric(total_wp)]
  
  nm_clean <- finalize_claims(
    nm_claims[, .(facility_id, lust_id, claims_start_date, claims_end_date, total_cost)],
    "NM"
  )
  
  nm_clean[, claims_start_date := as.Date(claims_start_date)]
  nm_clean[, claims_end_date   := as.Date(claims_end_date)]
  
  
  ###############################################################################
  # SECTION D — Utah (UT)
  ###############################################################################
  cat("Processing Utah (UT)...\n")
  ut_leaks_path  <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Utah/utah_state_LUSTs.xls"
  ut_claims_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Utah/Utah PST Fund Cleanup Amounts.xlsx"
  
  ut_leaks  <- read_excel(ut_leaks_path)  |> data.table() |> clean_names()
  ut_claims <- read_excel(ut_claims_path) |> data.table() |> clean_names()
  
  setnames(ut_claims, "rel_id",          "leak_id")
  setnames(ut_claims, "pst_amount_paid", "total_cost")
  
  # --- FIX: Removed "UT" prefix to match panel data ---
  ut_leaks [, leak_id     := norm_id(leak_id, prefix = NULL)]
  ut_leaks [, facility_id := norm_id(facility_id, prefix = NULL)]
  ut_claims[, leak_id     := norm_id(leak_id, prefix = NULL)]
  ut_claims[, facility_id := norm_id(facility_id, prefix = NULL)]
  ut_claims[, total_cost  := as.numeric(total_cost)]
  
  ut_leaks[, claims_start_date := ymd(notif_date)]
  ut_leaks[, claims_end_date   := ymd(date_closed)]
  
  ut_join <- merge(
    ut_leaks,
    ut_claims[, .(leak_id, facility_id, total_cost)],
    by = c("leak_id","facility_id")
  )

  ut_clean <- finalize_claims(
    ut_join[, .(facility_id, lust_id = leak_id, claims_start_date, claims_end_date, total_cost)],
    "UT"
  )
  
  ut_clean[, claims_start_date := as.Date(claims_start_date)]
  ut_clean[, claims_end_date   := as.Date(claims_end_date)]
  
  
  ###############################################################################
  # SECTION E — Colorado (CO)
  ###############################################################################
  cat("Processing Colorado (CO)...\n")
  co_releases_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Colorado/Petroleum_Releases__Colorado_Oil___Public_Safety_20250728.csv"
  co_claims_path   <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Colorado/CO Claims.xlsx"
  
  co_releases <- fread(co_releases_path) |> clean_names()
  co_claims   <- read_excel(co_claims_path) |> data.table() |> clean_names()
  
  legacy_col <- grep("legacy.*event.*id", names(co_releases), value = TRUE)[1]
  relno_col  <- grep("^release.*number$", names(co_releases), value = TRUE)[1]
  fid_col    <- grep("^facility.*id$", names(co_releases), value = TRUE)[1]
  setnames(co_releases, legacy_col, "lust_id")
  setnames(co_releases, relno_col,  "release_number")
  setnames(co_releases, fid_col,    "facility_id")
  
  # --- FIX: Removed "CO" prefix to match panel data ---
  co_releases[, lust_id        := norm_id(lust_id, prefix = NULL)]
  co_releases[, facility_id    := norm_id(facility_id, prefix = NULL)]
  co_releases[, release_number := as.character(toupper(trimws(release_number)))]
  co_releases[, release_date   := as.character(release_date)]
  
  setnames(co_claims, "total_amount_paid_for_reimbursement", "total_cost")
  # --- FIX: Removed "CO" prefix to match panel data ---
  co_claims[, facility_id    := norm_id(facility_id, prefix = NULL)]
  co_claims[, release_number := as.character(toupper(trimws(release_number)))]
  co_claims[, total_cost     := as.numeric(total_cost)]
  co_claims[, release_date   := as.character(release_date)]
  co_claims[, claims_end_date   := mdy(nfa_letter_sent_date)]
  co_claims[, claims_start_date := mdy(release_date)]
  
  co_claims <- co_claims[, .(
    facility_id, release_number, release_date,
    claims_start_date, claims_end_date, total_cost
  )][!duplicated(co_claims[, .(facility_id, release_number, release_date, total_cost)])]
  
  
  co_join <- merge(
    co_releases[, .(facility_id, release_number, release_date, lust_id)],
    co_claims  [, .(facility_id, release_number, release_date,
                    claims_start_date, claims_end_date, total_cost)],
    by = c("facility_id","release_number","release_date"))
  
  co_clean <- finalize_claims(
    co_join[, .(facility_id, lust_id, claims_start_date, claims_end_date, total_cost)],
    "CO"
  )
  
  co_clean[, claims_start_date := as.Date(claims_start_date)]
  co_clean[, claims_end_date   := as.Date(claims_end_date)]


###############################################################################
# SECTION F — Wisconsin (WI)
###############################################################################
#  WI ISSUES:
#    • Need to map Activity_detail_no ↔ Facility Id; claims file uses Activity.
#  Placeholder pipeline below is commented until crosswalk is ready.

# ## ---- WI file paths ----
# wi_lust_path   <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Wisconsin/WI_LUST.csv"
# wi_tanks_path  <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Wisconsin/WI_UST_tanks.csv"
# wi_claims_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Wisconsin/WI_Claims.xlsx"
#
# wi_lust   <- fread(wi_lust_path)  |> clean_names()
# wi_tanks  <- fread(wi_tanks_path) |> clean_names()
# wi_claims <- read_excel(wi_claims_path) |> data.table() |> clean_names()
#
# ## --- build mapping from LUST to Activity_detail_no to License_no ---
# lust_to_activity <- wi_lust[, .(ACTIVITY_DETAIL_NO, LICENSE_NO)]
# license_to_fac   <- wi_tanks[, .(LICENSE_NO = as.character(license_no), facility_id = as.character(facilityid))]
#
# claims_with_license <- merge(
#   lust_to_activity,
#   license_to_fac,
#   by = "LICENSE_NO",
#   all.x = TRUE
# )
#
# wi_claims[, claims_start_date := ymd(first_payment_date)]
# wi_claims[, claims_end_date   := ymd(last_payment_date)]
# wi_claims[, total_cost        := as.numeric(total_amount_submitted)]
#
# claims_with_license <- merge(
#   claims_with_license,
#   wi_claims[, .(activity_detail_no, claims_start_date, claims_end_date, total_cost)],
#   by.x = "ACTIVITY_DETAIL_NO",
#   by.y = "activity_detail_no", all.x = TRUE
# )
#
# wi_clean <- finalize_claims(
#   claims_with_license[, .(
#     facility_id, lust_id, claims_start_date, claims_end_date, total_cost
#   )],
#   "WI"
# )

###############################################################################
# SECTION G — North Carolina (NC)
###############################################################################
#  NC ISSUES:
#    • Need to map Incident Number ↔ Facility Id; claims file uses Incident+Claim.
#    • My UST panel prefixes Facility Id with "NC".
#  Placeholder pipeline below is commented until crosswalk is ready.

# ## ---- NC file paths ----
# nc_lust_path   <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/North Carolina/NC_LUST.csv"
# nc_tanks_path  <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/North Carolina/NC_UST_tanks.csv"
# nc_claims_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/North Carolina/NC_Claims.xlsx"
#
# nc_lust   <- fread(nc_lust_path)  |> clean_names()
# nc_tanks  <- fread(nc_tanks_path) |> clean_names()
# nc_claims <- read_excel(nc_claims_path) |> data.table() |> clean_names()
#
# ## --- build when mapping is complete ---
# nc_clean <- finalize_claims(
#   nc_claims[, .(
#     facility_id = NA_character_,
#     lust_id     = NA_character_,
#     claims_start_date = NA,
#     claims_end_date   = NA,
#     total_cost  = NA_real_
#   )][0],   # empty placeholder
#   "NC"
# )

###############################################################################
# SECTION H — Bind all finished states
###############################################################################


  all_cleaned_claims <- rbindlist(
    list(
      la_clean,
      tn_clean,
      nm_clean,
      ut_clean,
      co_clean
      # wi_clean,
      # nc_clean
    ))
  
  
  cat("Attaching CPI factor and calculating real costs...\n")
  all_cleaned_claims[, claim_start_year := year(claims_start_date)]
  all_cleaned_claims[, claim_start_month := month(claims_start_date)]
  
  all_cleaned_claims <- merge(
    all_cleaned_claims,
    cpi_dt[, .(claim_start_year = YEAR, claim_start_month = MONTH, cpi_factor)],
    by = c("claim_start_year", "claim_start_month"),
    all.x = TRUE
  )
  
  all_cleaned_claims[, total_cost       := as.numeric(total_cost)]
  all_cleaned_claims[, total_cost_2023  := total_cost * cpi_factor]
  
  # FIX: Ensure facility_id and lust_id are character before saving
  cat("Ensuring ID columns are character type...\n")
  all_cleaned_claims[, facility_id := as.character(facility_id)]
  all_cleaned_claims[, lust_id := as.character(lust_id)]
  cat(sprintf("   - facility_id type: %s\n", class(all_cleaned_claims$facility_id)))
  cat(sprintf("   - lust_id type: %s\n", class(all_cleaned_claims$lust_id)))
  
  # --- OUTPUT 1: FULL CLEANED CLAIMS (for server) ---
  cat("Saving full cleaned claims file...\n")
  output_claims_path <- here("Outputs", "all_cleaned_claims.csv")
  write.csv(all_cleaned_claims, output_claims_path, row.names = FALSE)
  cat(sprintf("   - Saved to: %s\n", output_claims_path))

  
  # --- Create monthly summary ---
  all_cleaned_claims[is.na(claim_start_year), claim_start_year := year(claims_start_date)]
  all_cleaned_claims[is.na(claim_start_month), claim_start_month := month(claims_start_date)]
  
  claims_monthly <- all_cleaned_claims[
    !is.na(facility_id) & !is.na(claim_start_year) & !is.na(claim_start_month),
    .(
      claims_n               = .N,
      claims_total_nominal   = sum(total_cost, na.rm = TRUE),
      claims_total_2023      = sum(total_cost_2023, na.rm = TRUE),
      claim_earliest_date    = min(claims_start_date, na.rm = TRUE),
      claim_latest_date      = max(claims_start_date, na.rm = TRUE)
    ),
    by = .(
      facility_id,
      state = state_name_map[state], # Uses global state_name_map
      panel_year  = claim_start_year,
      panel_month = claim_start_month
    )
  ][panel_year >= 1990 & panel_year <= 2023]
  
  claims_monthly[, month_key := sprintf("%d-%02d", panel_year, panel_month)]
  
  # --- OUTPUT 2: MONTHLY SUMMARY (for analysis) ---
  cat("Saving monthly claims summary...\n")
  output_monthly_path <- here("Outputs", "facility_month_claims_summary.csv")
  fwrite(claims_monthly, output_monthly_path)
  cat(sprintf("   - Saved to: %s\n", output_monthly_path))
  
  cat("\n✓ LOCAL RUN COMPLETE.\n")
  cat("   - You can now transfer 'Outputs/all_cleaned_claims.csv' to the server.\n")

  
} else {
  
  # ===================================================================
  # RUN_LOCAL = FALSE: SERVER-SIDE MERGE
  # ===================================================================
  
  cat("=================================================================\n")
  cat("RUN_LOCAL = FALSE: Running server-side panel merge...\n")
  cat("=================================================================\n\n")

  ###############################################################################
  # SECTION I — MERGE CLAIMS TO PANEL DATA (FROM SCRIPT 02)
  ###############################################################################
  
  # 1. Load FULL claims data (from local run)
  cat("Loading 'all_cleaned_claims.csv' from 'Outputs' folder...\n")
  claims_full_path <- here("Data\\Processed", "all_cleaned_claims.csv")
  if (!file.exists(claims_full_path)) {
    stop(sprintf("ERROR: File not found: %s\nDid you forget to transfer it to the server?", claims_full_path))
  }
  claims_full <- fread(claims_full_path)
  
  # FIX: Force facility_id and lust_id to character type to prevent merge errors
  cat("Converting ID columns to character type...\n")
  claims_full[, facility_id := as.character(facility_id)]
  claims_full[, lust_id := as.character(lust_id)]
  
  cat(sprintf("   - Loaded %d total claims\n", nrow(claims_full)))
  cat(sprintf("   - facility_id type: %s\n", class(claims_full$facility_id)))
  
  # 2. Load FULL panel data (from server's Data/Processed)
  cat("Loading FULL panel data (this may take a moment)...\n")
  panel_full_path <- here("Data", "Processed", "facility_leak_behavior_monthly.csv")
  if (!file.exists(panel_full_path)) {
    stop(sprintf("ERROR: Panel file not found: %s", panel_full_path))
  }
  panel_full <- fread(panel_full_path)
  
  # FIX: Force facility_id to character type to prevent merge errors
  cat("Converting panel facility_id to character type...\n")
  panel_full[, facility_id := as.character(facility_id)]
  
  cat(sprintf("   - Loaded %d facility-months in full panel\n", nrow(panel_full)))
  cat(sprintf("   - panel facility_id type: %s\n", class(panel_full$facility_id)))
  
  # 3. Prepare claims data for merging
  cat("Preparing claims data for merge...\n")
  claims_prepped <- claims_full[total_cost_2023 > 0 & !is.na(total_cost_2023)]
  claims_prepped[, `:=`(
    claim_year = as.integer(claim_start_year),
    claim_month = as.integer(claim_start_month)
  )]
  cat(sprintf("   - Prepared %d claims with valid costs for merging\n", nrow(claims_prepped)))
  
  
  # 4. Map state abbreviations to full names
  cat("   - Mapping state abbreviations to full names for merge...\n")
  state_map_dt <- data.table(
    state_abbrev = names(state_name_map), # Uses global state_name_map
    state_full = state_name_map
  )
  
  claims_prepped[state_map_dt, on = .(state = state_abbrev), state := i.state_full]
  
  cat("   - State name mapping complete.\n")
  
  # 5. Prepare panel data for merging
  cat("Preparing panel data for merge...\n")
  panel_prepped <- panel_full[, `:=`(
    panel_year = as.integer(panel_year),
    panel_month = as.integer(panel_month)
  )]
  
  # VERIFICATION: Check data types and sample IDs before merge
  cat("\n=== PRE-MERGE VERIFICATION ===\n")
  cat(sprintf("Claims facility_id type: %s\n", class(claims_prepped$facility_id)))
  cat(sprintf("Panel facility_id type: %s\n", class(panel_prepped$facility_id)))
  
  cat("\nSample claims facility IDs by state:\n")
  print(claims_prepped[, .(n_claims = .N, sample_ids = paste(head(unique(facility_id), 3), collapse=", ")), by=state])
  
  cat("\nSample panel facility IDs (for states with claims):\n")
  print(panel_prepped[state %in% unique(claims_prepped$state), 
                      .(n_facilities = uniqueN(facility_id), 
                        sample_ids = paste(head(unique(facility_id), 3), collapse=", ")), 
                      by=state])
  
  # 6. Perform INNER JOIN (all.x = FALSE) to keep ONLY matches
  cat("\nPerforming inner join on FULL datasets to find all matches...\n")
  cleanup_data_matches <- merge(
    claims_prepped,
    panel_prepped,
    by.x = c("facility_id", "state", "claim_year", "claim_month"),
    by.y = c("facility_id", "state", "panel_year", "panel_month"),
    all = FALSE # This performs an INNER JOIN
  )
  
  # 7. Save the new pre-merged sample file (OUTPUT 2)
  output_merged_path <- here("Data", "Processed", "claims_panel_merged_matches_SAMPLE.csv")
  fwrite(cleanup_data_matches, output_merged_path)
  
  cat(sprintf("\n✓ Pre-merged sample (all matches from full data) saved to: %s\n", output_merged_path))
  cat(sprintf("   - Total matched claim rows: %s\n", format(nrow(cleanup_data_matches), big.mark=",")))
  
  # SUMMARY: Show matches by state
  cat("\n=== MATCH SUMMARY BY STATE ===\n")
  match_summary <- cleanup_data_matches[, .(
    matched_claims = .N,
    matched_facilities = uniqueN(facility_id)
  ), by = state][order(-matched_claims)]
  print(match_summary)
  
  cat("\n✓ SERVER RUN COMPLETE.\n\n")
  
} # End of script
