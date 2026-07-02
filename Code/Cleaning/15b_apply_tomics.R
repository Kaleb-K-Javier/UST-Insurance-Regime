###############################################################################
# 15b_apply_tomics.R — TOMICS premium: load raw data, price, write output
# Filing:  SERFF TEXS-131241913  (eff. 2018-01-01)
# Carrier: Tank Owners Members Insurance Company
# Window:  TOMICS in TX FR data 1999-2023 (long contract terms; FR data is truth)
#
# Sources 15a_engine_tomics.R for all rating functions and module constants.
# Builds tank attributes from raw_pst_ust.csv (DO NOT READ texas_static_tank_details.csv
# — that file is empty/absent on the server; building from raw is the fix).
#
# OUTPUT (4 files, Data/Analysis/rate_engines/ — RATE_ENGINE_BUILD_TARGET.md):
#   TOMICS_facility_year_premium.csv (File 1 — panel_id, panel_year, min_prem,
#     standard_prem, max_prem, carrier, n_tanks_rated — no source_era.)
#   TOMICS_cell_era_card.csv         (File 2 — keyed on calendar panel_year, not era.)
#   TOMICS_multitank_credit.csv      (File 3 — trivial: TOMICS filing has no
#     facility tank-count discount; one 1..9999 = 1.0 band per active year.)
#   TOMICS_min_premium.csv           (File 4 — $350 filing minimum, p.2, every
#     active year.)
###############################################################################

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  BEHAVIORAL ASSUMPTIONS — TOMICS filing TEXS-131241913                     ║
# ╠══════════════════════════════════════════════════════════════════════════════╣
# ║ A1  Policy limits: COVER_OCC / COVER_AGG from FR contract panel used       ║
# ║     directly. NA or 0 → 1M occ / 1M agg reference (ILF load = 0.000).    ║
# ║ A2  Schedule rating: ±40% band (p.3). No credit or debit guessed.          ║
# ║     standard_prem = filed schedule-neutral premium.                        ║
# ║     min_prem = standard × 0.60  ;  max_prem = standard × 1.40.           ║
# ║     All three floored at $350 filing minimum policy premium (p.2).         ║
# ║ A3  Deductible: $5k reference tier (p.1) → 0.000 load.                    ║
# ║     Field absent from contract panel [V2]; held at reference throughout.   ║
# ║ A4  Defense sub-limit: $1M reference (p.1) → 0.000 load. Not in data.     ║
# ║ A5  Retro date: 0-1yr reference (p.1) → 0.000 load. Not in data.         ║
# ║ A6  Site-capacity table: "Above Ground Tanks Only" header (p.2) →         ║
# ║     0.000 load for all underground storage tanks.                          ║
# ║ A7  Prior contamination: LUST join deferred to v2 → 0.000 load.           ║
# ║ A8  TOMICS 2nd program (filing 131214138) is OUT OF SCOPE.                 ║
# ║     Single filed card (131241913, eff. 2018-01-01) applied to all TOMICS   ║
# ║     contract-years in the FR data (1999-2023; long contract terms explain   ║
# ║     the full range). TX FR data taken as truth — no year filter applied.    ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(data.table)
library(lubridate)
library(here)

source(here("Code", "Cleaning", "15a_engine_tomics.R"))
# inherits: SCHED_CAP, POLICY_MIN_PREMIUM, ENERGY_ACT_DATE
# inherits: base_rate_tomics, ilf_load_tomics, age_load_tomics,
#           leak_load_tomics, construction_load_tomics, pipe_load_tomics

CARRIER_KEY  <- "TOMICS"
ISSUER_EXACT <- "TANK OWNERS MEMBERS INS CO"   # confirmed in contract panel
out_dir      <- here("Data", "Analysis", "rate_engines")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


###############################################################################
## STEP 1 — Build tank attributes from raw_pst_ust.csv                       ##
##          (12_Rate_MidCont Section A recipe; mirrors 04a Step 1)            ##
###############################################################################

cat("=== STEP 1: load + derive tank attributes from raw ===\n")

raw_path <- here("Data", "Raw", "state_databases", "Texas", "raw_pst_ust.csv")
if (!file.exists(raw_path))
  raw_path <- here("Data", "Raw_do_not_write", "panel_merge_staging", "raw_pst_ust.csv")
if (!file.exists(raw_path))
  stop("raw_pst_ust.csv not found at canonical or staging path — check Data/ on server")

# Needed raw columns — check names before reading
actual_names <- names(fread(raw_path, nrows = 0L))

needed_raw <- c("UST_ID", "TANK_ID", "INSTALL_DATE",
                "TANK_MAT_FRP", "TANK_MAT_COMPOSITE",
                "PIP_DOUBLE", "PIP_MAT_FLEX",
                "DET_C_INTERSTITIAL", "DET_P_INTERSTITIAL",
                "end_date")

# FACILITY_ID: prefer FACILITY_ID_PAD (padded) if available, else FACILITY_ID
id_col <- if ("FACILITY_ID_PAD" %in% actual_names) "FACILITY_ID_PAD" else "FACILITY_ID"
if (!id_col %in% actual_names)
  stop("Neither FACILITY_ID_PAD nor FACILITY_ID found in raw_pst_ust.csv")

missing_flds <- setdiff(needed_raw, actual_names)
if (length(missing_flds) > 0L) {
  cat("  STOP: fields not found in raw_pst_ust.csv:\n")
  cat(paste(" ", missing_flds, collapse = "\n"), "\n")
  stop("Fix the column names above and re-run.")
}

raw_ust <- fread(raw_path, select = c(id_col, needed_raw))

# Merge key: 6-char trimmed FACILITY_ID
raw_ust[, FACILITY_ID := trimws(substr(get(id_col), 1L, 6L))]
if (id_col != "FACILITY_ID") raw_ust[, (id_col) := NULL]

raw_ust[, UST_ID       := toupper(trimws(UST_ID))]
raw_ust[, INSTALL_DATE := ymd(INSTALL_DATE)]
raw_ust[, CLOSED_DATE  := ymd(end_date)]
raw_ust[, end_date     := NULL]

# ── mm_wall JOIN (panel_dt.csv, TX) — SINGLE source of truth for wall ───────
# SUPERSEDES the old "use the pre-built double_walled dummy" fix (that dummy
# is retired fleet-wide, researcher 2026-07-01): every engine sources wall from
# the estimation panel's mm_wall column (Data/Analysis/panel_dt.csv) so the
# card cell matches the structural model's state cell.
panel_wall <- fread(here("Data", "Analysis", "panel_dt.csv"),
                     select = c("facility_id", "tank_id", "state", "mm_wall"))
panel_wall <- panel_wall[state == "TX"]
panel_wall[, state := NULL]
panel_wall <- unique(panel_wall, by = c("facility_id", "tank_id"))
# PM-state wall mapping — mirrors 04al_BOY_Composition_Build.R:67 (the BOY-
# composition input PM01/PM03 build the portfolio state space from): SW iff
# mm_wall contains "single" (case-insensitive), else DW. Mixed-Wall/Unknown-Wall
# fall through to DW here exactly as the panel does — not special-cased.
panel_wall[, wall := fifelse(grepl("single", mm_wall, ignore.case = TRUE), "SW", "DW")]
panel_wall[, mm_wall := NULL]
setnames(panel_wall, c("facility_id", "tank_id"), c("facility_id_key", "tank_id_key"))

# Join keys mirror 08_Clean_TX.R's own harmonization of panel_dt.csv's IDs:
# facility_id is standardize_numeric_id()'d (integer round-trip, leading zeros
# stripped; 08_Clean_TX.R:566) and tank_id is a straight as.character(TANK_ID),
# no zero-strip (08_Clean_TX.R:824). Straight join, no fallback / match-rate
# heuristics.
raw_ust[, facility_id_key := {
  v <- suppressWarnings(as.integer(as.character(FACILITY_ID)))
  fifelse(is.na(v), trimws(as.character(FACILITY_ID)), as.character(v))
}]
raw_ust[, tank_id_key := trimws(as.character(TANK_ID))]

raw_ust <- panel_wall[raw_ust, on = c("facility_id_key", "tank_id_key")]
cat(sprintf("  mm_wall panel join: %d / %d tanks matched (%.1f%%)\n",
            sum(!is.na(raw_ust$wall)), nrow(raw_ust), 100 * mean(!is.na(raw_ust$wall))))
raw_ust[, c("facility_id_key", "tank_id_key") := NULL]

# ── Derive binary tank attributes (all 0/1, no NA) ───────────────────────────
raw_ust[, det_interstitial  := fifelse((!is.na(DET_C_INTERSTITIAL) & DET_C_INTERSTITIAL  == "Y") |
                                       (!is.na(DET_P_INTERSTITIAL) & DET_P_INTERSTITIAL  == "Y"), 1L, 0L)]
raw_ust[, is_composite      := fifelse((!is.na(TANK_MAT_COMPOSITE) & TANK_MAT_COMPOSITE  == "Y") |
                                       (!is.na(TANK_MAT_FRP)       & TANK_MAT_FRP        == "Y"), 1L, 0L)]
raw_ust[, pip_double_walled := fifelse(!is.na(PIP_DOUBLE)          & PIP_DOUBLE          == "Y", 1L, 0L)]
raw_ust[, pip_flex          := fifelse(!is.na(PIP_MAT_FLEX)        & PIP_MAT_FLEX        == "Y", 1L, 0L)]
raw_ust[, pip_dw_rigid      := fifelse(pip_double_walled == 1L & pip_flex == 0L, 1L, 0L)]

stopifnot(!anyNA(raw_ust$det_interstitial))
stopifnot(!anyNA(raw_ust$is_composite))
stopifnot(!anyNA(raw_ust$pip_dw_rigid))

# Drop source columns no longer needed
raw_ust[, c("TANK_MAT_FRP", "TANK_MAT_COMPOSITE",
            "PIP_DOUBLE", "PIP_MAT_FLEX",
            "DET_C_INTERSTITIAL", "DET_P_INTERSTITIAL",
            "pip_double_walled", "pip_flex") := NULL]

# ── base_category: New / Replace / Upgrade (2006-01-01 cutoff) ───────────────
# min install per facility across ALL tanks (incl. removed — captures pit history)
fac_min <- raw_ust[!is.na(INSTALL_DATE),
                   .(min_fac_install = min(INSTALL_DATE, na.rm = TRUE)),
                   by = FACILITY_ID]
raw_ust <- fac_min[raw_ust, on = "FACILITY_ID"]

raw_ust[, base_category := fcase(
  INSTALL_DATE <  ENERGY_ACT_DATE,                                    "Upgrade",
  INSTALL_DATE >= ENERGY_ACT_DATE & INSTALL_DATE > min_fac_install,  "Replace",
  INSTALL_DATE >= ENERGY_ACT_DATE & INSTALL_DATE <= min_fac_install, "New",
  default = "Upgrade"   # NA INSTALL_DATE -> Upgrade
)]
raw_ust[, min_fac_install := NULL]

cat(sprintf("  Raw tanks loaded: %d | facilities: %d\n",
            nrow(raw_ust), uniqueN(raw_ust$FACILITY_ID)))
cat("  base_category distribution:\n")
print(raw_ust[, .N, by = base_category][order(-N)])


###############################################################################
## STEP 2 — TOMICS contracts: filter + cartesian tank x contract-month       ##
###############################################################################

cat("=== STEP 2: TOMICS contracts ===\n")

contract_path <- here("Data", "Processed", "texas_fr_contract_month_panel.csv")
if (!file.exists(contract_path))
  stop("texas_fr_contract_month_panel.csv not found at: ", contract_path)

fa_contracts <- fread(contract_path)[
  ISSUER_NAME == ISSUER_EXACT,
  .(FACILITY_ID, YEAR, MONTH, EFF_DATE, COVER_OCC, COVER_AGG)
]
fa_contracts[, EFF_DATE := ymd(EFF_DATE)]
fa_contracts[, FACILITY_ID := trimws(FACILITY_ID)]

stopifnot(nrow(fa_contracts) > 0L)
cat(sprintf("  TOMICS contracts: %d rows | %d facilities | years %d-%d\n",
            nrow(fa_contracts), uniqueN(fa_contracts$FACILITY_ID),
            min(fa_contracts$YEAR), max(fa_contracts$YEAR)))

# Subset tank table to TOMICS facilities only
tomics_facs <- unique(fa_contracts$FACILITY_ID)
tank_sub    <- raw_ust[FACILITY_ID %in% tomics_facs]
cat(sprintf("  Tanks at TOMICS facilities: %d\n", nrow(tank_sub)))

# Cartesian join: tank x contract-month at same facility
ust_tm <- fa_contracts[tank_sub, on = "FACILITY_ID",
                        allow.cartesian = TRUE, nomatch = 0L]

# Keep tanks in-service at contract inception
ust_tm <- ust_tm[
  !is.na(INSTALL_DATE) &
  INSTALL_DATE <= EFF_DATE &
  (is.na(CLOSED_DATE) | CLOSED_DATE > EFF_DATE)
]

# Age at contract inception (whole years, floored at 0)
ust_tm[, age_years := pmax(0L, floor(as.numeric(EFF_DATE - INSTALL_DATE) / 365.25))]

cat(sprintf("  Tank-months (in-service): %d\n", nrow(ust_tm)))
stopifnot(nrow(ust_tm) > 0L)


###############################################################################
## STEP 3 — Price each tank-month; roll up tank -> month(sum) -> year(mean)  ##
###############################################################################

cat("=== STEP 3: price + roll up ===\n")

# Per tank-month: apply all rating factors
ust_tm[, `:=`(
  base_rate         = base_rate_tomics(base_category),
  ilf_load          = ilf_load_tomics(COVER_OCC, COVER_AGG),
  age_load          = age_load_tomics(age_years),
  leak_load         = leak_load_tomics(det_interstitial),
  construction_load = construction_load_tomics(wall == "DW", is_composite),
  pipe_load         = pipe_load_tomics(pip_dw_rigid)
)]
# Held-reference loads: ded, defense, retro, contam, capacity all 0.000 (see A3-A7)
ust_tm[, sec_load     := ilf_load + age_load + leak_load + construction_load + pipe_load]
ust_tm[, tank_premium := base_rate * (1 + sec_load)]

stopifnot(all(is.finite(ust_tm$tank_premium)))
stopifnot(all(ust_tm$tank_premium >= 0))

cat(sprintf("  tank_premium: mean=$%.0f  min=$%.0f  max=$%.0f\n",
            mean(ust_tm$tank_premium), min(ust_tm$tank_premium), max(ust_tm$tank_premium)))

###############################################################################
## STEP 3b — Aggregate to cell-year card (File 2 — keyed on panel_year)      ##
###############################################################################
cat("=== STEP 3b: build cell-year card ===\n")

# wall carried through from raw_ust's mm_wall panel join (STEP 1) — no
# re-derivation here.
ust_tm[, age_bin    := as.integer(cut(age_years,
                        c(0, 5, 10, 15, 20, 25, 30, 35, Inf),
                        labels = 1:8, right = FALSE, include.lowest = TRUE))]
ust_tm[is.na(age_bin), age_bin := 8L]
ust_tm[, panel_year := as.integer(YEAR)]

cell_card <- ust_tm[!is.na(wall), .(premium_usd_per_tank_yr = mean(tank_premium, na.rm = TRUE)),
                     by = .(wall, age_bin, panel_year)]
cell_card[, carrier := CARRIER_KEY]
setcolorder(cell_card, c("carrier", "wall", "age_bin", "panel_year", "premium_usd_per_tank_yr"))

stopifnot(all(is.finite(cell_card$premium_usd_per_tank_yr)))
stopifnot(all(cell_card$premium_usd_per_tank_yr > 0))
stopifnot(all(cell_card$age_bin %in% 1:8))
stopifnot(all(cell_card$wall %in% c("SW", "DW")))

card_path <- file.path(out_dir, paste0(CARRIER_KEY, "_cell_era_card.csv"))
fwrite(cell_card, card_path)
cat(sprintf("  cell-year card: %s (%d rows)\n", card_path, nrow(cell_card)))
print(cell_card[order(wall, age_bin, panel_year)][seq_len(min(.N, 10))])

# Facility-month: sum across tanks
fac_month <- ust_tm[, .(
  fac_std_fm = sum(tank_premium, na.rm = TRUE),
  n_tanks_fm = uniqueN(UST_ID)
), by = .(FACILITY_ID, YEAR, MONTH)]

# Facility-year: mean over months ("mean last", then apply band + floor)
fac_year <- fac_month[, .(
  standard_prem_pre = mean(fac_std_fm,  na.rm = TRUE),
  n_tanks_rated     = as.integer(round(mean(n_tanks_fm)))
), by = .(FACILITY_ID, YEAR)]

# Schedule ±40% band + $350 minimum policy premium floor (all at facility level)
fac_year[, `:=`(
  min_prem      = pmax(standard_prem_pre * (1 - SCHED_CAP), POLICY_MIN_PREMIUM),
  standard_prem = pmax(standard_prem_pre,                    POLICY_MIN_PREMIUM),
  max_prem      = pmax(standard_prem_pre * (1 + SCHED_CAP), POLICY_MIN_PREMIUM)
)]
fac_year[, standard_prem_pre := NULL]

stopifnot(all(fac_year$min_prem      >= POLICY_MIN_PREMIUM))
stopifnot(all(fac_year$standard_prem >= POLICY_MIN_PREMIUM))
stopifnot(all(fac_year$max_prem      >= POLICY_MIN_PREMIUM))
stopifnot(all(fac_year$min_prem      <= fac_year$standard_prem))
stopifnot(all(fac_year$standard_prem <= fac_year$max_prem))


###############################################################################
## STEP 4 — Assemble canonical schema + write (File 1 — no source_era,      ##
##          per RATE_ENGINE_BUILD_TARGET.md)                                ##
###############################################################################

cat("=== STEP 4: assemble + write ===\n")

# Canonical panel_id: "<FACILITY_ID>_TX" (matches 02b / 04a convention)
fac_year[, panel_id  := paste(toupper(trimws(FACILITY_ID)), "TX", sep = "_")]
fac_year[, panel_year := as.integer(YEAR)]
fac_year[, carrier    := CARRIER_KEY]

setcolorder(fac_year, c("panel_id", "panel_year",
                        "min_prem", "standard_prem", "max_prem",
                        "carrier", "n_tanks_rated"))

# Type enforcement
fac_year[, panel_year    := as.integer(panel_year)]
fac_year[, min_prem      := as.double(min_prem)]
fac_year[, standard_prem := as.double(standard_prem)]
fac_year[, max_prem      := as.double(max_prem)]
fac_year[, carrier       := as.character(carrier)]
fac_year[, n_tanks_rated := as.integer(n_tanks_rated)]

# Drop the working FACILITY_ID / YEAR columns (not in the canonical schema)
fac_year[, c("FACILITY_ID", "YEAR") := NULL]

# Write
out_path <- file.path(out_dir, "TOMICS_facility_year_premium.csv")
fwrite(fac_year, out_path)

# Print summary only (no full table)
cat(sprintf("\n  Output: %s\n", out_path))
cat(sprintf("  Rows: %d | Facilities: %d | Years: %d-%d\n",
            nrow(fac_year),
            uniqueN(fac_year$panel_id),
            min(fac_year$panel_year),
            max(fac_year$panel_year)))

cat("\n  Mean standard_prem by panel_year:\n")
print(fac_year[, .(
  N             = .N,
  mean_std_prem = round(mean(standard_prem), 2),
  median_std    = round(median(standard_prem), 2)
), by = panel_year][order(panel_year)])


###############################################################################
## STEP 5 — File 3: multi-tank credit (trivial — TOMICS filing               ##
##          TEXS-131241913 has NO facility tank-count discount; 15a defines  ##
##          no such function, and SS3 sums tank_premium with no facility-    ##
##          level multiplier)                                                ##
###############################################################################
cat("\n=== STEP 5: multi-tank credit (File 3) ===\n")

active_years <- sort(unique(fac_year$panel_year))
credit_tbl <- data.table(
  carrier     = CARRIER_KEY,
  panel_year  = active_years,
  n_min       = 1L,
  n_max       = 9999L,
  credit_mult = 1.0
)
setcolorder(credit_tbl, c("carrier", "panel_year", "n_min", "n_max", "credit_mult"))

stopifnot(all(credit_tbl$n_min == 1L), all(credit_tbl$n_max == 9999L))
for (yr in active_years) {
  yb <- credit_tbl[panel_year == yr][order(n_min)]
  stopifnot(yb$n_min[1L] == 1L, yb$n_max[nrow(yb)] == 9999L)
}

credit_path <- file.path(out_dir, paste0(CARRIER_KEY, "_multitank_credit.csv"))
fwrite(credit_tbl, credit_path)
cat(sprintf("  multi-tank credit: %s (%d rows, %d active years)\n",
            credit_path, nrow(credit_tbl), length(active_years)))


###############################################################################
## STEP 6 — File 4: minimum premium (POLICY_MIN_PREMIUM = $350, p.2 of the  ##
##          filing per 15a — one filed card applied across all active years)##
###############################################################################
cat("=== STEP 6: minimum premium (File 4) ===\n")

min_prem_tbl <- data.table(
  carrier         = CARRIER_KEY,
  panel_year      = active_years,
  min_premium_usd = POLICY_MIN_PREMIUM
)
setcolorder(min_prem_tbl, c("carrier", "panel_year", "min_premium_usd"))

stopifnot(all(min_prem_tbl$min_premium_usd > 0))
stopifnot(uniqueN(min_prem_tbl$panel_year) == nrow(min_prem_tbl))

min_prem_path <- file.path(out_dir, paste0(CARRIER_KEY, "_min_premium.csv"))
fwrite(min_prem_tbl, min_prem_path)
cat(sprintf("  min premium: %s (%d rows)\n", min_prem_path, nrow(min_prem_tbl)))

cat("\n=== 15b DONE ===\n")
