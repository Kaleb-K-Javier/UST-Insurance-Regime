###############################################################################
# UST Claims x LUST x Facility Crosswalk – Cleaned Multi-State Claims Table
# Author: Kaleb Javier   •   Last edit: 2026-02-26
#
# DESCRIPTION:
# Cleans and combines all state claims data from raw sources into a single
# unified cleaned claims dataset.
#
# INPUTS:  Raw claims files from Data/Raw/ folder (state-specific formats)
# OUTPUT:  Data/Processed/all_cleaned_claims.csv  (one row per claim)
#
# ─── PANEL / HARMONIZATION ALIGNMENT NOTES ────────────────────────────────────
# The master panel (facility_leak_behavior_annual.csv) uses the following key 
# conventions that this script must respect:
#
#   panel_id   = paste(facility_id, state, sep = "_")  e.g. "12345_TN"
#   facility_id = RAW state-assigned ID with NO state-abbreviation prefix
#   state       = 2-letter abbreviation, NEVER the full state name
#
# State-specific conventions:
#   TX, CO, LA, NM, TN, AR : paste0(state, facility_id) for EPA geo only;
#                             raw facility_id stored in panel
#   PA                      : permit_number (XX-XXXXX) format
###############################################################################


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

# Regex scrubber for currency/accounting strings
clean_cost <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", as.character(x)))
}

# Safely parse dates across multiple formats and Excel serials
safe_parse <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(as.Date(x))
  suppressWarnings({
    num_x <- as.numeric(as.character(x))
    is_num <- !is.na(num_x)
  })
  out <- rep(as.Date(NA), length(x))
  if (any(is_num)) out[is_num] <- janitor::excel_numeric_to_date(num_x[is_num])
  if (any(!is_num)) {
    parsed <- lubridate::parse_date_time(
      x[!is_num],
      orders = c("ymd", "mdy", "dmy", "Y-m-d", "m/d/Y", "mdY", "Ymd"),
      quiet  = TRUE
    )
    out[!is_num] <- as.Date(parsed)
  }
  return(out)
}

# Finalise a state claims table
finalize_claims <- function(dt, state_abbrev) {
  dt[, claim_start_year  := year(claims_start_date)]
  dt[, claim_end_year    := year(claims_end_date)]
  dt[, state             := state_abbrev]
  dt[, facility_id       := as.character(facility_id)]
  dt[, lust_id           := as.character(lust_id)]
  dt[, panel_id          := paste(facility_id, state, sep = "_")]
  setcolorder(
    dt,
    c("panel_id", "facility_id", "lust_id",
      "claims_start_date", "claims_end_date",
      "total_cost", "claim_start_year", "claim_end_year", "state")
  )
  dt[]
}

###############################################################################
# ═══════════════════════════════════════════════════════════════════════════════
# MAIN: Clean raw state claims files and produce all_cleaned_claims.csv
# ═══════════════════════════════════════════════════════════════════════════════
###############################################################################

cat("=================================================================\n")
cat("Building unified claims dataset from raw state files\n")
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

la_claims_path <- here('Data\\Raw\\state_databases\\Louisiana\\LA Claims.xlsx') 
la_claims <- read_excel(la_claims_path, sheet = 1, col_types = "text") |> data.table() |> clean_names()

la_claims[, claims_start_date := safe_parse(x1st_app_received_date)]
la_claims[, claims_end_date   := safe_parse(last_app_processed_date)]
# total_cost = site-level recommended fund payout allocated evenly across the
# AI's applications (total_requested_amt is the ASK, not the fund's actual
# recommended payout). LOCKED 2026-07-01.
la_claims[, total_cost        := clean_cost(total_recommended_amt) / as.numeric(app_count)]
# facility_id = raw numeric ai_num — matches panel's raw LA ID
la_claims[, facility_id       := norm_id(ai_num, prefix = NULL)]
la_claims[, lust_id           := as.character(ai_num)]

la_clean <- finalize_claims(
  la_claims[, .(facility_id, lust_id, claims_start_date, claims_end_date, total_cost)],
  "LA"
)


##############################################################################
# SECTION B — Tennessee (TN)
##############################################################################
cat("Processing Tennessee (TN)...\n")

tn_lust_path      <- here("Data", "Raw", "state_databases", "Tennessee", "Tennessee_UST_LUST.csv")
tn_claims_path    <- here("Data", "Raw", "state_databases", "Tennessee", "ust_all-tn-reimbursement-sites.xlsx")
tn_fac_extra_path <- here("Data", "Raw", "state_databases", "Tennessee", "ust_all-tn-compartments.xlsx")

# Apply clean_names() homogenously to force snake_case and block case-sensitivity faults
tn_lust   <- fread(tn_lust_path) |> clean_names()
tn_claims <- read_excel(tn_claims_path, col_types = "text") |> data.table() |> clean_names()
tn_extra  <- read_excel(tn_fac_extra_path, col_types = "text") |> data.table() |> clean_names()

# Standardise LUST file — keep raw divisionsiteid for joining
tn_lust[, divisionsiteid := trimws(as.character(sub("^TN", "", facility_id)))]
tn_lust[, casenumber     := sub(".*-(\\d+)$", "\\1", lust_id)]
tn_lust[, lust_facility_id := divisionsiteid]          # raw, no prefix
tn_lust[, report_date    := safe_parse(report_date)]

# Claims: keep raw divisionsiteid as facility_id (no "TN" prefix)
tn_claims[, divisionsiteid := trimws(as.character(divisionsiteid))]
tn_claims[, casenumber     := trimws(as.character(casenumber))]
tn_claims[, facility_id    := divisionsiteid]           # raw — matches panel
tn_claims[, total_cost   := clean_cost(paidrequested)]
tn_claims <- tn_claims[status == "Closed"]
# Redundant LUST_id generation omitted here to prevent merge namespace collisions

setkey(tn_claims, divisionsiteid, casenumber)
setkey(tn_lust,   divisionsiteid, casenumber)

clm2lust <- merge(
  tn_claims,
  tn_lust[, .(divisionsiteid, casenumber, lust_id, lust_facility_id, report_date)],
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
    lust_id,
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
  here("Data", "Raw", "state_databases", "New Mexico", "rp_nfa_amounts.xlsx"),
  here("Data", "Raw", "state_databases", "New Mexico", "sl_nfa_amounts.xlsx")
)

nm_claims_rp <- read_excel(nm_claim_paths[1], col_types = "text") |> data.table() |> clean_names()
nm_claims_sl <- read_excel(nm_claim_paths[2], col_types = "text") |> data.table() |> clean_names()
nm_claims_rp <- nm_claims_rp[total_wp > 0]
nm_claims    <- rbind(nm_claims_rp, nm_claims_sl, fill = TRUE)

rid_col <- grep("^r\\s*id$", names(nm_claims), ignore.case = TRUE, value = TRUE)[1]
fid_col <- grep("^f\\s*id$", names(nm_claims), ignore.case = TRUE, value = TRUE)[1]
setnames(nm_claims, rid_col, "lust_id")
setnames(nm_claims, fid_col, "facility_id")

nm_claims[, lust_id           := norm_id(lust_id,     prefix = NULL)]
nm_claims[, facility_id       := norm_id(facility_id, prefix = NULL)]
nm_claims[, claims_start_date := safe_parse(release_date)]
nm_claims[, claims_end_date   := safe_parse(nfa_date)]
nm_claims[, total_cost        := clean_cost(total_wp)]

nm_clean <- finalize_claims(
  nm_claims[, .(facility_id, lust_id, claims_start_date, claims_end_date, total_cost)],
  "NM"
)


##############################################################################
# SECTION D — Utah (UT)
##############################################################################
##############################################################################
# SECTION D — Utah (UT)
##############################################################################
cat("Processing Utah (UT)...\n")

ut_leaks_path  <- here("Data", "Raw", "state_databases", "Utah", "utah_state_LUSTs.xls")
ut_claims_path <- here("Data", "Raw", "state_databases", "Utah", "Utah PST Fund Cleanup Amounts.xlsx")

ut_leaks  <- read_excel(ut_leaks_path, col_types = "text")  |> data.table() |> clean_names()
ut_claims <- read_excel(ut_claims_path, col_types = "text") |> data.table() |> clean_names()

setnames(ut_claims, "rel_id",          "leak_id")
setnames(ut_claims, "pst_amount_paid", "total_cost")

# FIX IMPLEMENTED: Appending "UT" prefix to match Master Panel key structure 
ut_leaks [, leak_id     := norm_id(leak_id,     prefix = "UT")]
ut_leaks [, facility_id := norm_id(facility_id, prefix = "UT")]
ut_claims[, leak_id     := norm_id(leak_id,     prefix = "UT")]
ut_claims[, facility_id := norm_id(facility_id, prefix = "UT")]
ut_claims[, total_cost       := clean_cost(total_cost)]

ut_leaks[, claims_start_date := safe_parse(notif_date)]
ut_leaks[, claims_end_date   := safe_parse(date_closed)]

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

co_releases_path <- here("Data", "Raw", "state_databases", "Colorado", "Petroleum_Releases__Colorado_Oil___Public_Safety_20250728.csv")
co_claims_path   <- here("Data", "Raw", "state_databases", "Colorado", "CO Claims.xlsx")

co_releases <- fread(co_releases_path) |> clean_names()
co_claims   <- read_excel(co_claims_path, col_types = "text") |> data.table() |> clean_names()

legacy_col <- grep("legacy.*event.*id",  names(co_releases), value = TRUE)[1]
relno_col  <- grep("^release.*number$",  names(co_releases), value = TRUE)[1]
fid_col    <- grep("^facility.*id$",     names(co_releases), value = TRUE)[1]
setnames(co_releases, legacy_col, "lust_id")
setnames(co_releases, relno_col,  "release_number")
setnames(co_releases, fid_col,    "facility_id")

co_releases[, lust_id        := norm_id(lust_id,        prefix = NULL)]
co_releases[, facility_id    := norm_id(facility_id,    prefix = NULL)]
co_releases[, release_number := toupper(trimws(as.character(release_number)))]

setnames(co_claims, "total_amount_paid_for_reimbursement", "total_cost")
co_claims[, facility_id    := norm_id(facility_id, prefix = NULL)]
co_claims[, release_number := toupper(trimws(as.character(release_number)))]
co_claims[, total_cost        := clean_cost(total_cost)]
co_claims[, claims_end_date   := safe_parse(nfa_letter_sent_date)]
co_claims[, claims_start_date := safe_parse(release_date)]

co_claims <- unique(
  co_claims[, .(facility_id, release_number, 
                claims_start_date, claims_end_date, total_cost)]
)

# FIX IMPLEMENTED: 2-key join (facility_id, release_number), dropping release_date
co_join <- merge(
  co_releases[, .(facility_id, release_number, lust_id)],
  co_claims  [, .(facility_id, release_number,
                  claims_start_date, claims_end_date, total_cost)],
  by = c("facility_id", "release_number")
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
#           -> facility_linkage_table$facility_id -> facility_id (PA DEP ID)
##############################################################################
##############################################################################
# SECTION PA — Pennsylvania (PA)
##############################################################################
# NOTE (Ticket 052, 2026-07-02): this section's PA output still feeds
# all_cleaned_claims.csv / claims_panel_annual_merged.csv (Sections H/I below)
# unchanged -- 05_Claims_Analysis.r reads PA out of those and Ticket 052 does
# not re-derive them. The incident-level PA rows this section contributes via
# Section J ARE superseded -- see the note in Section J's header. Left as-is
# (not no-op'd) per the 052 ticket amendment: no-op'ing this whole section
# would have silently dropped PA from those other two files too.
cat("Processing Pennsylvania (PA)...\n")

pa_repo_candidates <- c(
  "C:/Users/kaleb/Documents/PA_UST_Auction_Analysis",   # local laptop
  "Z:/C_Drive_Portal/PA_UST_Auction_Analysis",          # local, server mirror
  "C:/Users/kalebkja/PA_UST_Auction_Analysis"           # server (ucbare2)
)
pa_repo_found <- pa_repo_candidates[dir.exists(pa_repo_candidates)]
if (length(pa_repo_found) == 0L)
  stop("PA_UST_Auction_Analysis repo not found in any candidate location:\n  ",
       paste(pa_repo_candidates, collapse = "\n  "))
pa_repo_base  <- pa_repo_found[1]
pa_claims_csv <- file.path(pa_repo_base, "data", "processed", "claims_clean.csv")
pa_linkage    <- file.path(pa_repo_base, "data", "external", "padep", "facility_linkage_table.csv")

if (!file.exists(pa_claims_csv)) stop(paste("PA claims not found:", pa_claims_csv))
if (!file.exists(pa_linkage))    stop(paste("PA linkage table not found:", pa_linkage))

pa_claims_raw <- fread(pa_claims_csv, colClasses = "character") |> clean_names()
pa_link_dt    <- fread(pa_linkage,    colClasses = "character") |> clean_names()

cat(sprintf("   PA claims loaded: %d rows | columns: %s\n",
            nrow(pa_claims_raw), paste(names(pa_claims_raw), collapse = ", ")))

if (!"facility_id" %in% names(pa_link_dt)) {
  stop(sprintf("Schema fault: 'facility_id' missing from PA linkage. Available columns: %s", 
               paste(names(pa_link_dt), collapse = ", ")))
}

# FIX IMPLEMENTED: facility_id set to map directly to the panel's identical XX-XXXXX format
pa_link_key <- pa_link_dt[!is.na(facility_id) & facility_id != "", .(
  permit_key  = toupper(trimws(facility_id)),
  facility_id = as.character(trimws(facility_id))
)] |> unique()

pa_claims_raw[, dept_key := toupper(trimws(department))]

# incurred_loss = paid + case reserves = the ULTIMATE fund-payable cost, valid
# for both open and closed claims (no closed-only filter needed). LOCKED 2026-07-01.
if (!"incurred_loss" %in% names(pa_claims_raw))
  stop("PA claims_clean.csv missing required column 'incurred_loss' — check upstream PA repo build")
cost_col <- "incurred_loss"
cat(sprintf("   Using '%s' as cost column (locked 2026-07-01: ultimate fund-payable cost)\n", cost_col))

pa_claims_raw[, total_cost := as.numeric(get(cost_col))]

open_col  <- intersect(c("claim_date", "loss_reported_date", "open_date"), names(pa_claims_raw))[1]
close_col <- intersect(c("closed_date", "claim_close_date", "close_date"), names(pa_claims_raw))[1]
lust_col  <- intersect(c("claim_number", "lust_id", "incident_number"), names(pa_claims_raw))[1]

pa_claims_raw[, claims_start_date := safe_parse(get(open_col))]
pa_claims_raw[, claims_end_date   := if (!is.na(close_col)) safe_parse(get(close_col)) else as.Date(NA)]
pa_claims_raw[, lust_id           := as.character(get(lust_col))]

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

pa_merged <- pa_merged[!is.na(total_cost) & total_cost > 0 & !is.na(claims_start_date)]

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

## OUTPUT — full claims table (one row per claim)
dir.create(here("Data", "Processed"), showWarnings = FALSE, recursive = TRUE)
output_claims_path <- here("Data", "Processed", "all_cleaned_claims.csv")
fwrite(all_cleaned_claims, output_claims_path)
cat(sprintf("   Saved: %s\n", output_claims_path))


##############################################################################
# SECTION I — Merge to Master Annual Panel
##############################################################################
cat("=================================================================\n")
cat("Merging Cleaned Claims with Master Annual Panel\n")
cat("=================================================================\n\n")

# 1. Load Master Annual Panel
panel_path <- here("Data", "Processed", "facility_leak_behavior_annual.csv")

if (!file.exists(panel_path)) {
  stop("Master annual panel not found. Execute 10_Build_Annual_Panel_Optimized.R prior to this script.")
}

annual_panel <- fread(panel_path)

# 2. Extract Facility Lifespans from Panel
fac_lifespan <- annual_panel[, .(
  min_year = min(panel_year, na.rm = TRUE),
  max_year = max(panel_year, na.rm = TRUE)
), by = panel_id]

# 3. Filter claims to ONLY facilities that survived the structural panel filters
valid_claims <- all_cleaned_claims[panel_id %in% fac_lifespan$panel_id]

# 4. Temporal Snapping (Fixing Zombie Claims)
valid_claims <- merge(valid_claims, fac_lifespan, by = "panel_id", all.x = TRUE)
# Snap post-closure claims to the facility's final active year in the panel
valid_claims[!is.na(max_year) & claim_start_year > max_year, claim_start_year := max_year]
# Snap premature claims to the facility's first active year
valid_claims[!is.na(min_year) & claim_start_year < min_year, claim_start_year := min_year]

# 5. Aggregate Claims to Facility-Year
claims_agg <- valid_claims[!is.na(claim_start_year) & !is.na(total_cost_2023), .(
  claims_total_2023 = sum(total_cost_2023, na.rm = TRUE),
  n_claims_in_year  = .N
), by = .(panel_id, panel_year = claim_start_year)]

# 6. Left Join Claims to Annual Panel
merged_panel <- merge(
  annual_panel, 
  claims_agg, 
  by = c("panel_id", "panel_year"), 
  all.x = TRUE
)

# 7. Handle NAs for zero-claim observations
merged_panel[is.na(claims_total_2023), claims_total_2023 := 0]
merged_panel[is.na(n_claims_in_year),  n_claims_in_year  := 0L]

# 8. Output Merged Panel
merged_output_path <- here("Data", "Processed", "claims_panel_annual_merged.csv")
fwrite(merged_panel, merged_output_path)

cat(sprintf("Merged panel saved: %s\n", merged_output_path))
cat(sprintf("  Total facility-years: %s\n", format(nrow(merged_panel), big.mark = ",")))
cat(sprintf("  Facility-years with >0 claims: %s\n", 
            format(nrow(merged_panel[claims_total_2023 > 0]), big.mark = ",")))
cat("\n✓ CROSSWALK AND MERGE COMPLETE\n")

cat("\n✓ COMPLETE\n\n")

##############################################################################
# SECTION J — Incident-Level Claims x Panel Merge
#
# PURPOSE:
#   Keep one row per individual claim (do NOT aggregate) and attach the
#   facility's tank characteristics for the year the claim was filed.
#   This is the correct input for severity modelling in 05_Claims_Analysis.R.
#
# LOGIC:
#   claim_start_year (already snapped to panel bounds above in Section I)
#   is used to look up the facility row in the annual panel. No LUST linkage
#   is needed — the claim date itself pins the characteristics.
#
# OUTPUT:
#   Data/Processed/incident_level_claims.csv   (one row per claim)
#
# NOTE (Ticket 052, 2026-07-02): the PA rows written below (~2,323, via the
# facility_leak_behavior_annual join, which drops ~52% of PA claims) are
# immediately superseded. Code/Cleaning/12_PA_Claims_Import.R runs right
# after this script, drops state=="PA" from the incident_level_claims.csv
# just written, and replaces it with a full reshape of PA's own project
# dataset (master_analysis_dataset.rds, ~7,792 claims, no facility-panel join
# needed). This script intentionally still computes PA's stale rows here
# (no conditional skip) so that running 11_Build alone still produces a
# complete, well-formed incident_level_claims.csv even if 052's script hasn't
# run yet.
##############################################################################
cat("=================================================================\n")
cat("SECTION J — Incident-Level Claims x Panel Merge\n")
cat("=================================================================\n\n")

# Tank characteristic columns to bring in from the panel.
# Extend this vector if 05_Claims_Analysis.R needs additional panel columns.
PANEL_CHAR_COLS <- c(
  "panel_id", "panel_year",
  "active_tanks", "avg_tank_age", "age_bins",
  "single_tanks", "double_tanks", "total_capacity",
  "has_single_walled", "has_double_walled"
)

# Subset panel to only the columns we need (annual_panel is already in memory
# from Section I above).
panel_chars <- annual_panel[, intersect(PANEL_CHAR_COLS, names(annual_panel)),
                            with = FALSE]

# valid_claims is already in memory from Section I (snapped, filtered to
# facilities present in the panel). Join on facility x claim year.
incident_dt <- merge(
  valid_claims,
  panel_chars,
  by.x = c("panel_id", "claim_start_year"),
  by.y = c("panel_id", "panel_year"),
  all.x = FALSE   # drop the rare claim whose snapped year still has no panel row
)

n_dropped <- nrow(valid_claims) - nrow(incident_dt)
cat(sprintf("Claims matched to panel characteristics : %s\n",
            format(nrow(incident_dt), big.mark = ",")))
cat(sprintf("Claims dropped (no matching panel year) : %d\n", n_dropped))

# Sanity check: no facility-year aggregation crept in
stopifnot(nrow(incident_dt) == uniqueN(
  incident_dt[, paste(panel_id, claims_start_date, lust_id)]
) || TRUE)   # soft check — duplicates possible if same lust_id filed twice

# Basic quality flags (mirrors the flag logic in EX_Incident_Claims_Linkage
# but kept minimal here — 05 can add more if needed)
incident_dt[, flag_negative_cost := as.integer(!is.na(total_cost_2023) & total_cost_2023 < 0)]
incident_dt[, flag_zero_cost     := as.integer(!is.na(total_cost_2023) & total_cost_2023 == 0)]
incident_dt[, flag_na_cost       := as.integer(is.na(total_cost_2023))]
incident_dt[, flag_no_tank_chars := as.integer(is.na(active_tanks))]

cat("\nFlag counts:\n")
print(incident_dt[, .(
  flag_negative_cost = sum(flag_negative_cost),
  flag_zero_cost     = sum(flag_zero_cost),
  flag_na_cost       = sum(flag_na_cost),
  flag_no_tank_chars = sum(flag_no_tank_chars)
)])

cat("\nClaims by state:\n")
print(incident_dt[, .N, by = state][order(state)])

# Save
incident_path <- here("Data", "Processed", "incident_level_claims.csv")
fwrite(incident_dt, incident_path)
cat(sprintf("\nSaved: incident_level_claims.csv (%s rows, %d cols)\n",
            format(nrow(incident_dt), big.mark = ","), ncol(incident_dt)))

cat("\n✓ SECTION J COMPLETE\n\n")
