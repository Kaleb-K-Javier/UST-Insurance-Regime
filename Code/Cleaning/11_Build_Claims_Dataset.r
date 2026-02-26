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
##############################################################################
cat("Processing Tennessee (TN)...\n")

tn_lust_path      <- here("Data", "Raw", "state_databases", "Tennessee", "Tennessee_UST_LUST.csv")
tn_claims_path    <- here("Data", "Raw", "state_databases", "Tennessee", "ust_all-tn-reimbursement-sites.xlsx")
tn_fac_extra_path <- here("Data", "Raw", "state_databases", "Tennessee", "ust_all-tn-compartments.xlsx")

# Apply clean_names() homogenously to force snake_case and block case-sensitivity faults
tn_lust   <- fread(tn_lust_path) |> clean_names()
tn_claims <- read_excel(tn_claims_path) |> data.table() |> clean_names()
tn_extra  <- read_excel(tn_fac_extra_path) |> data.table() |> clean_names()

# Standardise LUST file — keep raw divisionsiteid for joining
tn_lust[, divisionsiteid := trimws(as.character(sub("^TN", "", facility_id)))]
tn_lust[, casenumber     := sub(".*-(\\d+)$", "\\1", lust_id)]
tn_lust[, lust_facility_id := divisionsiteid]          # raw, no prefix
tn_lust[, report_date    := ymd_hms(report_date)]

# Claims: keep raw divisionsiteid as facility_id (no "TN" prefix)
tn_claims[, divisionsiteid := trimws(as.character(divisionsiteid))]
tn_claims[, casenumber     := trimws(as.character(casenumber))]
tn_claims[, facility_id    := divisionsiteid]           # raw — matches panel
tn_claims[, total_cost     := as.numeric(paidrequested)]
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

ut_leaks_path  <- here("Data", "Raw", "state_databases", "Utah", "utah_state_LUSTs.xls")
ut_claims_path <- here("Data", "Raw", "state_databases", "Utah", "Utah PST Fund Cleanup Amounts.xlsx")

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

co_releases_path <- here("Data", "Raw", "state_databases", "Colorado", "Petroleum_Releases__Colorado_Oil___Public_Safety_20250728.csv")
co_claims_path   <- here("Data", "Raw", "state_databases", "Colorado", "CO Claims.xlsx")

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
#           -> facility_linkage_table$facility_id -> facility_id (PA DEP ID)
##############################################################################
cat("Processing Pennsylvania (PA)...\n")

# Explicit absolute paths bypassing here() relative referencing
pa_repo_base  <- "C:/Users/kalebkja/PA_UST_Auction_Analysis"
pa_claims_csv <- file.path(pa_repo_base, "data", "processed", "claims_clean.csv")
pa_linkage    <- file.path(pa_repo_base, "data", "external", "padep", "facility_linkage_table.csv")

if (!file.exists(pa_claims_csv)) stop(paste("PA claims not found:", pa_claims_csv))
if (!file.exists(pa_linkage))    stop(paste("PA linkage table not found:", pa_linkage))

pa_claims_raw <- fread(pa_claims_csv, colClasses = "character") |> clean_names()
pa_link_dt    <- fread(pa_linkage,    colClasses = "character") |> clean_names()

cat(sprintf("   PA claims loaded: %d rows | columns: %s\n",
            nrow(pa_claims_raw), paste(names(pa_claims_raw), collapse = ", ")))

# In the current PA linkage schema, 'facility_id' contains the PA DEP 
# permit sequence (e.g., '02-12345') which maps directly to 'department' in the claims.
if (!"facility_id" %in% names(pa_link_dt)) {
  stop(sprintf("Schema fault: 'facility_id' missing from PA linkage. Available columns: %s", 
               paste(names(pa_link_dt), collapse = ", ")))
}

# Normalise the linkage table side
pa_link_key <- pa_link_dt[!is.na(facility_id) & facility_id != "", .(
  permit_key  = toupper(trimws(facility_id)),
  facility_id = as.character(trimws(facility_id))
)] |> unique()

# Claims data 'department' operates as the permit number key
pa_claims_raw[, dept_key := toupper(trimws(department))]

# Resolve cost column
cost_col <- intersect(
  c("total_paid", "total_cost", "amount_paid", "paid_amount"),
  names(pa_claims_raw)
)[1]
if (is.na(cost_col)) cost_col <- grep("paid|cost|amount", names(pa_claims_raw), value = TRUE)[1]
cat(sprintf("   Using '%s' as cost column\n", cost_col))

pa_claims_raw[, total_cost := as.numeric(get(cost_col))]

# Resolve date and ID columns
open_col  <- intersect(c("claim_date", "loss_reported_date", "open_date"), names(pa_claims_raw))[1]
close_col <- intersect(c("closed_date", "claim_close_date", "close_date"), names(pa_claims_raw))[1]
lust_col  <- intersect(c("claim_number", "lust_id", "incident_number"), names(pa_claims_raw))[1]

pa_claims_raw[, claims_start_date := safe_parse(get(open_col))]
pa_claims_raw[, claims_end_date   := if (!is.na(close_col)) safe_parse(get(close_col)) else as.Date(NA)]
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

# 2. Aggregate Claims to Facility-Year
# Uses the in-memory all_cleaned_claims data.table from Section H
claims_agg <- all_cleaned_claims[!is.na(claim_start_year) & !is.na(total_cost_2023), .(
  claims_total_2023 = sum(total_cost_2023, na.rm = TRUE),
  n_claims_in_year  = .N
), by = .(panel_id, panel_year = claim_start_year)]

# 3. Left Join Claims to Annual Panel
merged_panel <- merge(
  annual_panel, 
  claims_agg, 
  by = c("panel_id", "panel_year"), 
  all.x = TRUE
)

# 4. Handle NAs for zero-claim observations
merged_panel[is.na(claims_total_2023), claims_total_2023 := 0]
merged_panel[is.na(n_claims_in_year),  n_claims_in_year  := 0L]

# 5. Output Merged Panel
merged_output_path <- here("Data", "Processed", "claims_panel_annual_merged.csv")
fwrite(merged_panel, merged_output_path)

cat(sprintf("Merged panel saved: %s\n", merged_output_path))
cat(sprintf("  Total facility-years: %s\n", format(nrow(merged_panel), big.mark = ",")))
cat(sprintf("  Facility-years with >0 claims: %s\n", 
            format(nrow(merged_panel[claims_total_2023 > 0]), big.mark = ",")))
cat("\n✓ CROSSWALK AND MERGE COMPLETE\n")

cat("\n✓ COMPLETE\n\n")