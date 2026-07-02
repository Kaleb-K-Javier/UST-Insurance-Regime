################################################################################
# 12_PA_Claims_Import.R
# Ticket 052 -- Import PA severity claims from the PA project (supersede
# 11_Build_Claims_Dataset.r's PA facility-filter path).
#
# WHAT THIS DOES: drops the PA rows (state=="PA") that 11_Build_Claims_Dataset.r
# Section J wrote into incident_level_claims.csv -- those came from a join
# through facility_leak_behavior_annual that drops ~52% of PA claims -- and
# replaces them with a full reshape of the PA project's finished claim-level
# dataset (master_analysis_dataset.rds, ALL claims, covariates embedded per
# claim, no facility-panel join needed). Idempotent: always drops state=="PA"
# before appending, so re-running (or running after a fresh 11_Build) is safe.
#
# RUN ORDER: Code/Cleaning/11_Build_Claims_Dataset.r THEN this script. 11_Build
# still computes its own (stale, superseded) PA rows -- see the note in its
# Section J header -- so that running 11_Build alone still yields a complete,
# well-formed incident_level_claims.csv even if this script hasn't run yet.
#
# NOT IN SCOPE: PA PANEL (panel_dt.csv / facility_panel.csv). That's owned by
# 02b_PA_Append.R and is untouched here. all_cleaned_claims.csv and
# claims_panel_annual_merged.csv (11_Build Sections H/I) are also untouched --
# 05_Claims_Analysis.r reads PA out of those and this ticket doesn't re-derive
# them.
#
# INPUT:
#   master_analysis_dataset.rds  (PA_UST_Auction_Analysis project; resolved
#                                 local -> server(ucbare2) -> Z mirror -> error,
#                                 same order as 11_Build's PA section)
#   Data/Processed/incident_level_claims.csv   (from 11_Build; must exist)
#   Data/Processed/all_cleaned_claims.csv      (from 11_Build; CPI lookup source)
# OUTPUT:
#   Data/Processed/incident_level_claims.csv   (overwritten; PA rows replaced)
################################################################################

suppressPackageStartupMessages({ library(data.table); library(here) })

SCRIPT_NAME <- "12_PA_Claims_Import"

# === LOGGING ===
.log_path <- here::here("logs", paste0(SCRIPT_NAME, "_",
                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output")
sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
            .log_path, SCRIPT_NAME, R.version.string, getwd()))

cat("=================================================================\n")
cat("TICKET 052 -- PA claims import from PA_UST_Auction_Analysis project\n")
cat("=================================================================\n\n")

# Canonical UST age-bin contract (identical to 01a_Setup.R / 01n / 01p / 01q).
# 052 must land as these exact 9 label strings in a column literally named
# age_bins -- 01q_Severity_Model.R:58-67 reads incident_level_claims.csv's
# age_bins column as pre-binned strings and factors it directly.
AGE_BIN_BREAKS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
AGE_BIN_LABELS <- c("0-2", "3-5", "6-8", "9-11", "12-14",
                     "15-17", "18-20", "21-23", "24+")

# =============================================================================
cat("=== SECTION A: RESOLVE + LOAD PA SOURCE ===\n")
# =============================================================================
# Same resolver order as 11_Build_Claims_Dataset.r's SECTION PA (local laptop ->
# server mirror on Z -> direct server path). NOTE (ticket 052, resolved
# 2026-07-02): the ticket text as written listed only local + Z, which would
# hard-error on ucbare2 (Z: not mounted there, profile is kalebkja not kaleb).
# Added the third candidate to match 11_Build exactly.
pa_repo_candidates <- c(
  "C:/Users/kaleb/Documents/PA_UST_Auction_Analysis",   # local laptop
  "Z:/C_Drive_Portal/PA_UST_Auction_Analysis",          # local, server mirror
  "C:/Users/kalebkja/PA_UST_Auction_Analysis"           # server (ucbare2)
)
pa_repo_found <- pa_repo_candidates[dir.exists(pa_repo_candidates)]
if (length(pa_repo_found) == 0L)
  stop("PA_UST_Auction_Analysis repo not found in any candidate location:\n  ",
       paste(pa_repo_candidates, collapse = "\n  "))
pa_repo_base    <- pa_repo_found[1]
pa_master_path  <- file.path(pa_repo_base, "data", "processed", "master_analysis_dataset.rds")
if (!file.exists(pa_master_path))
  stop(paste("PA master dataset not found:", pa_master_path))

pa_raw <- as.data.table(readRDS(pa_master_path))
cat(sprintf("Loaded master_analysis_dataset.rds: %d rows, %d cols (from %s)\n",
            nrow(pa_raw), ncol(pa_raw), pa_repo_base))

stopifnot(
  "claim_number is not unique in master_analysis_dataset.rds" =
    uniqueN(pa_raw$claim_number) == nrow(pa_raw),
  "department has NA values" = !anyNA(pa_raw$department),
  "incurred_loss has NA values" = !anyNA(pa_raw$incurred_loss),
  "incurred_loss has negative values" = all(pa_raw$incurred_loss >= 0),
  "Year and claim_year disagree" = all(pa_raw$Year == pa_raw$claim_year)
)
cat("Source sanity checks passed: claim_number unique, department/incurred_loss non-NA, ",
    "incurred_loss >= 0, Year==claim_year.\n", sep = "")

# =============================================================================
cat("\n=== SECTION B: CPI FACTOR LOOKUP (reuse 11_Build's, do not re-pull FRED) ===\n")
# =============================================================================
# Resolved 2026-07-02: re-hitting FRED for CPIAUCSL a second time in the same
# pipeline run risks a different vintage (BLS seasonal-adjustment revisions)
# than what 11_Build baked into total_cost_2023 for the other five states.
# Instead we read the (year, month) -> cpi_factor lookup straight out of
# all_cleaned_claims.csv, which 11_Build just wrote in this same run --
# identical by construction for every covered cell. A cell PA needs that no
# other state's claim falls in will show up as NA below and IS a hard stop
# (per instruction: stop and report the missing cells, do not silently patch).
all_claims_path <- here("Data", "Processed", "all_cleaned_claims.csv")
if (!file.exists(all_claims_path))
  stop(paste("all_cleaned_claims.csv not found -- run 11_Build_Claims_Dataset.r first:",
             all_claims_path))

cpi_lookup <- unique(fread(all_claims_path,
                           select = c("claim_start_year", "claim_start_month", "cpi_factor")))
stopifnot(
  "duplicate (year, month) cells with different cpi_factor in all_cleaned_claims.csv" =
    uniqueN(cpi_lookup, by = c("claim_start_year", "claim_start_month")) == nrow(cpi_lookup)
)
cat(sprintf("CPI lookup: %d distinct (year, month) cells, years %d-%d\n",
            nrow(cpi_lookup), min(cpi_lookup$claim_start_year), max(cpi_lookup$claim_start_year)))

# =============================================================================
cat("\n=== SECTION C: RESHAPE TO THE SHARED UST SEVERITY DESIGN ===\n")
# =============================================================================
pa_dt <- copy(pa_raw)

# D4: panel_id = department-based key. facility_id namespaced the same way
# (raw department could otherwise collide with another state's facility_id).
pa_dt[, panel_id    := paste0(toupper(trimws(department)), "_PA")]
pa_dt[, facility_id := paste0(toupper(trimws(department)), "_PA")]
pa_dt[, lust_id     := as.character(claim_number)]
pa_dt[, state       := "PA"]

pa_dt[, panel_year        := claim_year]
pa_dt[, claim_start_year  := claim_year]
pa_dt[, claim_start_month := Month]
pa_dt[, claim_end_year    := closed_year]
pa_dt[, claims_start_date := as.Date(loss_reported_date)]
pa_dt[, claims_end_date   := as.Date(closed_date)]

# D1: total_cost = incurred_loss (ultimate fund-payable cost), keep zero/positive.
pa_dt[, total_cost := as.numeric(incurred_loss)]

# CPI join (Section B). all.x=TRUE so a missing cell surfaces as NA and is
# caught by the stopifnot below rather than silently propagating.
pa_dt <- merge(pa_dt, cpi_lookup,
               by.x = c("claim_start_year", "claim_start_month"),
               by.y = c("claim_start_year", "claim_start_month"),
               all.x = TRUE, sort = FALSE)

missing_cpi_cells <- unique(pa_dt[is.na(cpi_factor), .(claim_start_year, claim_start_month)])
if (nrow(missing_cpi_cells) > 0L) {
  cat("\n!! MISSING CPI CELLS -- (year, month) combinations with no covering claim\n")
  cat("!! in any other state's all_cleaned_claims.csv rows:\n")
  print(missing_cpi_cells[order(claim_start_year, claim_start_month)])
  stop(sprintf(
    "cpi_factor lookup is missing %d (year, month) cell(s) needed by PA claims. ",
    nrow(missing_cpi_cells)),
    "Per the 052 resolution: STOP here, do not silently patch. Report the cells ",
    "printed above; they'll be filled with a targeted CPIAUCSL pull for just ",
    "those months.")
}
pa_dt[, total_cost_2023 := total_cost * cpi_factor]
cat("CPI join complete: 0 missing cells.\n")

# D2: reshape to the shared design. age_bins MUST be the 9 canonical label
# strings (not a numeric index), right=FALSE + include.lowest=TRUE to match
# every other call site in the repo (01a_Setup.R, 01n, 01p, 01q).
pa_dt[, age_bins := as.character(cut(avg_tank_age, breaks = AGE_BIN_BREAKS,
                                      labels = AGE_BIN_LABELS,
                                      right = FALSE, include.lowest = TRUE))]

pa_dt[, has_single_walled := as.integer(has_single_walled)]
pa_dt[, has_unknown_wall  := as.integer(has_tank_construction_unknown)]
pa_dt[, total_capacity    := as.numeric(total_capacity_gal)]
pa_dt[, active_tanks      := as.integer(n_tanks_active)]
# Q3 (resolved 2026-07-02): use the PA project's own precomputed has_gasoline/
# has_diesel directly -- do NOT re-derive from the free-text products/
# product_other fields (would invent a mapping no other state's coding shares).
pa_dt[, has_gasoline := as.integer(has_gasoline)]
pa_dt[, has_diesel   := as.integer(has_diesel)]
pa_dt[, is_closed    := as.integer(is_closed)]

# Flags, computed the same way Section J computes them for the other states.
pa_dt[, flag_negative_cost := as.integer(!is.na(total_cost_2023) & total_cost_2023 < 0)]
pa_dt[, flag_zero_cost     := as.integer(!is.na(total_cost_2023) & total_cost_2023 == 0)]
pa_dt[, flag_na_cost       := as.integer(is.na(total_cost_2023))]
pa_dt[, flag_no_tank_chars := as.integer(is.na(active_tanks))]

# Columns the existing schema has that PA's embedded data can't populate
# (facility-panel snap bounds, per-wall tank counts, double-walled flag --
# none used downstream for PA today; left NA per "fill as the schema
# requires", NOT reconstructed from an unverified assumption).
pa_dt[, `:=`(min_year = NA_real_, max_year = NA_real_,
             single_tanks = NA_integer_, double_tanks = NA_integer_,
             has_double_walled = NA_integer_)]

pa_out_cols <- c(
  "panel_id", "facility_id", "lust_id", "state",
  "panel_year", "claim_start_year", "claim_end_year", "claim_start_month",
  "claims_start_date", "claims_end_date",
  "total_cost", "total_cost_2023", "cpi_factor",
  "age_bins", "has_single_walled", "has_unknown_wall", "has_double_walled",
  "total_capacity", "active_tanks", "single_tanks", "double_tanks",
  "has_gasoline", "has_diesel", "is_closed",
  "min_year", "max_year",
  "flag_negative_cost", "flag_zero_cost", "flag_na_cost", "flag_no_tank_chars"
)
stopifnot("reshape is missing an expected output column" = all(pa_out_cols %in% names(pa_dt)))
pa_reshaped <- pa_dt[, ..pa_out_cols]

stopifnot(
  "panel_id has NA" = !anyNA(pa_reshaped$panel_id),
  "state is not all PA" = all(pa_reshaped$state == "PA"),
  "total_cost has NA" = !anyNA(pa_reshaped$total_cost),
  "total_cost has negative values" = all(pa_reshaped$total_cost >= 0),
  "age_bins has a value outside the canonical 9 labels" =
    all(pa_reshaped$age_bins[!is.na(pa_reshaped$age_bins)] %in% AGE_BIN_LABELS)
)
cat(sprintf("Reshaped %d PA claim rows to the shared severity design.\n", nrow(pa_reshaped)))

# =============================================================================
cat("\n=== SECTION D: DROP-THEN-APPEND ONTO incident_level_claims.csv ===\n")
# =============================================================================
incident_path <- here("Data", "Processed", "incident_level_claims.csv")
if (!file.exists(incident_path))
  stop(paste("incident_level_claims.csv not found -- run 11_Build_Claims_Dataset.r first:",
             incident_path))

incident_dt <- fread(incident_path)
stopifnot("incident_level_claims.csv has no 'state' column" = "state" %in% names(incident_dt))

n_pa_old <- sum(incident_dt$state == "PA")
non_pa_dt <- incident_dt[state != "PA"]
cat(sprintf("Existing incident_level_claims.csv: %d rows (%d PA, dropped).\n",
            nrow(incident_dt), n_pa_old))

new_cols <- setdiff(names(pa_reshaped), names(non_pa_dt))
if (length(new_cols) > 0L)
  cat(sprintf("New columns introduced by this reshape (NA for CO/NM/TN/LA/UT): %s\n",
              paste(new_cols, collapse = ", ")))

combined_dt <- rbind(non_pa_dt, pa_reshaped, fill = TRUE)
setcolorder(combined_dt, c(names(non_pa_dt), new_cols))

fwrite(combined_dt, incident_path)
cat(sprintf("Saved: %s (%d rows total, %d PA)\n",
            incident_path, nrow(combined_dt), nrow(pa_reshaped)))

# =============================================================================
cat("\n=== SECTION E: REPORT ===\n")
# =============================================================================
cat(sprintf("PA claim count: %d (was %d under the superseded facility-filter path)\n",
            nrow(pa_reshaped), n_pa_old))
cat(sprintf("total_cost_2023 ($, 2023): median = %s, mean = %s\n",
            format(round(median(pa_reshaped$total_cost_2023)), big.mark = ","),
            format(round(mean(pa_reshaped$total_cost_2023)), big.mark = ",")))

cat("\nAge-bin distribution:\n")
print(pa_reshaped[, .N, by = age_bins][order(match(age_bins, AGE_BIN_LABELS))])

cat("\nWall distribution:\n")
print(pa_reshaped[, .(has_single_walled = sum(has_single_walled == 1L, na.rm = TRUE),
                       has_unknown_wall  = sum(has_unknown_wall  == 1L, na.rm = TRUE),
                       na_wall           = sum(is.na(has_single_walled)))])

cat("\nFuel distribution:\n")
print(pa_reshaped[, .(has_gasoline = sum(has_gasoline == 1L, na.rm = TRUE),
                       has_diesel   = sum(has_diesel   == 1L, na.rm = TRUE),
                       na_fuel      = sum(is.na(has_gasoline)))])

cat("\nNA counts (key severity-design columns):\n")
print(pa_reshaped[, lapply(.SD, function(x) sum(is.na(x))),
                   .SDcols = c("total_cost", "total_cost_2023", "age_bins",
                               "has_single_walled", "has_unknown_wall",
                               "total_capacity", "active_tanks",
                               "has_gasoline", "has_diesel")])

cat("\n✓ TICKET 052 COMPLETE\n\n")
