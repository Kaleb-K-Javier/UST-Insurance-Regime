###############################################################################
# 18b_apply_ace.R — Apply ACE (ACE American Insurance Company, later Chubb)
#                    rate engine to Texas FR contracts; write the canonical
#                    4-file output (RATE_ENGINE_BUILD_TARGET.md).
# Filing: ACE 2011 "TankSafe Rating Manual 2-11.pdf", Section B (UST); see
# 18a_engine_ace.R header for page cites / STIP3 2012 correction / TX +/-40%
# schedule cap source.
# Sources 18a_engine_ace.R for ALL rating functions, the multi-tank credit
# ladder, and the minimum premium — nothing is re-derived here.
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# BEHAVIORAL ASSUMPTIONS BANNER
# ══════════════════════════════════════════════════════════════════════════════
# CARRIER:        ACE American Insurance Company (Chubb acquired ACE in 2016;
#                 the filing/manual is unchanged post-acquisition per SESSION
#                 PROMPT — one unified manual throughout, no era switch).
# CARRIER_KEY:    "ACE"
# ISSUER_NAME:    PROPOSED, PENDING RESEARCHER CONFIRMATION (see return
#                 summary for the 036b snippet). Candidates carried below as
#                 ISSUER_MATCH: "ACE AMERICAN INS CO", "ACE", "CHUBB",
#                 "ILLINOIS UNION INS CO" (the last per the fam() pattern in
#                 Code/Macro/fr_carrier_diagnostics.R, an ACE/Chubb-group
#                 subsidiary that appears in TX FR data). "CHUBB" and "ACE"
#                 bare-word candidates are BROAD and risk over-matching other
#                 Chubb-group entities that are NOT this manual's book — DO
#                 NOT run this script for real until the researcher has run
#                 the snippet and edited ISSUER_MATCH to the confirmed exact
#                 string(s).
#
# COVERAGE LIMIT / DEDUCTIBLE: $1,000,000/$1,000,000 occ/agg + $5,000
#                 deductible held at the filing's only confirmed reference
#                 point (18a has NO ILF/deductible ladder — only a single
#                 reference point was transcribed). Per 036-locked invariant 1
#                 and this task's instruction, actual FR contract
#                 COVER_OCC/COVER_AGG are NOT wired into pricing — printed as
#                 a QA diagnostic only (STEP 2). Because 18a's tank-level
#                 formula (base * (1 - construction_credit)) never multiplies
#                 by an ILF/deductible factor at all, this is a true no-op —
#                 there is no LIMIT_DED_FACTOR to assert (unlike 17b/AIG,
#                 which has ilf_aig()/deductible_credit_aig() to check).
#
# SCHEDULE:       +/-40% (SCHED_CAP, TX Appendix A) -> min_prem / max_prem
#                 envelope around standard_prem, at facility-year level (File
#                 1 only). Marina/airport, prior-contamination, >30k gal,
#                 non-regulated, loading/unloading, ethanol, >50yr,
#                 leak-detection, risk-management discretionary schedule
#                 items are ALL held at 0 for standard_prem (18a's own
#                 behavioral assumption, not re-litigated here) — File 2 is
#                 schedule-neutral by construction (no band applied).
#
# MULTI-TANK CREDIT: Applied at FACILITY-MONTH level off the actual observed
#                 tank count that month (18a's ladder, multi_tank_credit_ace()).
#
# CONTENTS:       ACE's Section B has no separate contents/product factor —
#                 not a term in 18a's tank_standard formula at all (same
#                 convention as AIG).
#
# NO ERA SWITCH:  ACE is one unified manual across all active TX years; STIP3
#                 = 50% throughout (18a header; supersedes any pre-2012
#                 STIP3 value per the 2012 correction). MIN_PREMIUM = $350
#                 (TRIA-inclusive) for every active year — File 4 is a flat
#                 $350 row per year, not a lookup.
#
# ══════════════════════════════════════════════════════════════════════════════
# WALL SOURCE (researcher rule, 2026-07-01 — READ THIS BEFORE TRUSTING FILE 1/2
# NUMBERS): The card cell wall (SW/DW) and the wall component of ACE's
# construction bucket come from the DCM-state `mm_wall` column in
# `Data/Analysis/panel_dt.csv` (built by Code/Analysis/02b_Tank_level_Panel_
# Build.R), JOINED onto the raw tanks on (facility_id, tank_id) — NOT
# re-derived from raw_pst_ust.csv's own double_walled flag, no dummy anywhere.
# This guarantees the card cell matches the structural model's state cell
# exactly (PM03 looks the card up by the state cell).
#
# panel_dt.csv is a tank-YEAR panel (facility_id, tank_id, state, mm_wall,
# panel_year, ...), 1985-2020, TX + 7 control states. TX rows are what we
# join to (deduped to one row per (facility_id, tank_id) — mm_wall is a
# time-invariant physical tank attribute).
#
# JOIN KEY (SETTLED — SUPERSEDES the old two-candidate auto-pick/match-rate/
# warn/hard-stop logic; straight join, no fallback / match-rate heuristics):
#   - facility_id: standardize_numeric_id()'d in 08_Clean_TX.R:566 (integer
#     round-trip -> leading zeros STRIPPED, e.g. "001234" -> "1234"). Our own
#     raw-side FACILITY_ID (6-char substr of FACILITY_ID_PAD) keeps leading
#     zeros, so we build a separate, leading-zero-stripped `facility_id_key`
#     for the join only (the un-stripped `FACILITY_ID` is still used for the
#     output `panel_id`, per README §7).
#   - tank_id: `as.character(TANK_ID)` straight out of 08_Clean_TX.R:824 —
#     trimws only, NO leading-zero strip. TANK_ID (byte-position 4 in the
#     PST_UST.TXT spec, a facility-local tank number, e.g. "Tank #1") is a
#     DIFFERENT raw field from UST_ID (byte-position 1, the TCEQ system-wide
#     unique tank key, used elsewhere in this script for uniqueN() tank
#     counts) — TANK_ID is required for the mm_wall join, UST_ID is not a
#     substitute.
#   - Mixed-Wall tanks are KEPT (not dropped): the PM-state wall mapping
#     (04al_BOY_Composition_Build.R:67, SW iff mm_wall contains "single"
#     else DW) is applied uniformly, so Mixed-Wall/Unknown-Wall fall through
#     to DW exactly as the panel does — no carrier-specific exclusion rule.
#   - Tanks that fail to match ANY panel_dt.csv row are NA on `wall`; they are
#     excluded from the cell-year card (File 2, `!is.na(wall)`) but still
#     priced into File 1 via the default BARE_STEEL construction branch (a
#     tank's construction_code doesn't strictly require a wall match — only
#     the STIP3 branch does; unmatched tanks fall to BARE_STEEL/FIBERGLASS by
#     their own composite/fiberglass flags). Match-rate is printed, not gated.
#
# ACE CONSTRUCTION MAPPING (researcher-directed, 2026-07-01 fleet-wide wall
# task — supersedes the earlier PROPOSED mapping):
#   STIP3 (50%)      <- mm_wall == "Double-Walled" OR is_composite_tank == 1
#                        [STIP3 = "Double-Walled / Clad-Steel" per the filing
#                        name; composite tanks (steel core clad in
#                        fiberglass) are the clad-steel construction the
#                        filing means, so they route here, NOT to FIBERGLASS.]
#   FIBERGLASS (20%) <- is_fiberglass_tank == 1 (pure FRP only)
#   BARE_STEEL (0%)  <- default catch-all: bare/coated steel, mm_wall in
#                        {"Single-Walled","Unknown-Wall","Mixed-Wall"} with
#                        no composite/fiberglass flag, or unmatched-wall rows.
#
# OUTPUT (4 files, Data/Analysis/rate_engines/):
#   ACE_facility_year_premium.csv (File 1 — panel_id, panel_year, min_prem,
#     standard_prem, max_prem, carrier, n_tanks_rated — matches
#     RATE_ENGINE_BUILD_TARGET.md exactly.)
#   ACE_cell_era_card.csv         (File 2 — carrier, wall, age_bin, panel_year,
#     premium_usd_per_tank_yr — keyed on calendar year; wall from mm_wall;
#     unmatched-wall tanks EXCLUDED from the card, but still priced into
#     File 1 via the default BARE_STEEL branch.)
#   ACE_multitank_credit.csv      (File 3 — carrier, panel_year, n_min, n_max,
#     credit_mult; bands tile 1..9999 exactly, repeated for every active year.)
#   ACE_min_premium.csv           (File 4 — carrier, panel_year, min_premium_usd
#     = $350 flat, one row per active year.)
# ══════════════════════════════════════════════════════════════════════════════

library(data.table)
library(lubridate)
library(here)

source(here("Code", "Cleaning", "18a_engine_ace.R"))
# inherits: SCHED_CAP, MIN_PREMIUM, REF_COVER_OCC, REF_COVER_AGG, REF_DEDUCTIBLE
# inherits: base_premium_ace, construction_credit_ace, multi_tank_credit_ace,
#           ace_facility_premium

# ── Logging ──────────────────────────────────────────────────────────────────
.log_path <- here::here("logs", paste0(
  "18b_apply_ace_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 18b_apply_ace.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== 18b_apply_ace: ACE premium apply ===\n")
cat(sprintf("Run started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

CARRIER_KEY <- "ACE"

# PROPOSED, PENDING CONFIRMATION (see banner + return summary snippet).
# DO NOT run for real until the researcher has confirmed / edited this list.
ISSUER_MATCH <- c("ACE AMERICAN INS CO", "ACE", "CHUBB", "ILLINOIS UNION INS CO")

out_dir <- here("Data", "Analysis", "rate_engines")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Filing-minimum-premium lookup — ACE has NO era switch, flat $350 every year
# (shared by File 1's facility-year floor and File 4).
min_premium_of_year <- function(panel_year) rep(MIN_PREMIUM, length(panel_year))


###############################################################################
## STEP 1 — Build tank attributes from raw_pst_ust.csv                       ##
##          (12_Rate_MidCont Section A recipe; mirrors 17b Step 1 — ACE      ##
##          only needs age + construction, so this is a SMALLER field list   ##
##          than AIG's: no leak-detection / piping factors in the ACE       ##
##          filing.)                                                        ##
###############################################################################

cat("=== STEP 1: load + derive tank attributes from raw ===\n")

raw_path <- here("Data", "Raw", "state_databases", "Texas", "raw_pst_ust.csv")
if (!file.exists(raw_path))
  stop("raw_pst_ust.csv not found at: ", raw_path,
       " — do NOT fall back to texas_static_tank_details.csv, it is empty on the server")

actual_names <- names(fread(raw_path, nrows = 0L))

# FACILITY_ID: prefer FACILITY_ID_PAD (padded) if available, else FACILITY_ID
id_col <- if ("FACILITY_ID_PAD" %in% actual_names) "FACILITY_ID_PAD" else "FACILITY_ID"
if (!id_col %in% actual_names)
  stop("Neither FACILITY_ID_PAD nor FACILITY_ID found in raw_pst_ust.csv")

# TANK_ID: REQUIRED for the mm_wall panel join (see banner "WALL SOURCE").
# NOT the same field as UST_ID — hard-stop if missing rather than silently
# fall back to UST_ID, which would silently break the join (see banner).
needed_raw <- c(
  "UST_ID", "TANK_ID", "INSTALL_DATE", "end_date",
  "is_fiberglass_tank", "is_composite_tank"
)
missing_flds <- setdiff(needed_raw, actual_names)
if (length(missing_flds) > 0L) {
  cat("  STOP: fields not found in raw_pst_ust.csv:\n")
  cat(paste(" ", missing_flds, collapse = "\n"), "\n")
  if ("TANK_ID" %in% missing_flds)
    cat("  TANK_ID missing -> the mm_wall panel join (banner 'WALL SOURCE') has no",
        " raw-side key. STOP and confirm the real column name with the researcher\n")
  stop("Fix the column names above (server column names may differ) and re-run.")
}

raw_ust <- fread(raw_path, select = c(id_col, needed_raw))

# Output-facing FACILITY_ID: README §7 convention (6-char substr, leading
# zeros KEPT) — used for panel_id in File 1. NOT the join key (see below).
raw_ust[, FACILITY_ID := trimws(substr(get(id_col), 1L, 6L))]
if (id_col != "FACILITY_ID") raw_ust[, (id_col) := NULL]

raw_ust[, UST_ID       := toupper(trimws(UST_ID))]
raw_ust[, TANK_ID      := toupper(trimws(TANK_ID))]
raw_ust[, INSTALL_DATE := ymd(INSTALL_DATE)]
raw_ust[, CLOSED_DATE  := ymd(end_date)]
raw_ust[, end_date     := NULL]

raw_ust[, is_fiberglass_tank := fifelse(is.na(is_fiberglass_tank), 0L, as.integer(as.logical(is_fiberglass_tank)))]
raw_ust[, is_composite_tank  := fifelse(is.na(is_composite_tank), 0L, as.integer(as.logical(is_composite_tank)))]

cat(sprintf("  Raw tanks loaded: %d | facilities: %d\n",
            nrow(raw_ust), uniqueN(raw_ust$FACILITY_ID)))

# ── mm_wall JOIN (panel_dt.csv, TX) — SINGLE source of truth for wall ───────
# SETTLED (supersedes the old two-candidate auto-pick/match-rate/warn/hard-stop
# logic — see banner "WALL SOURCE"). Straight join, no fallback / match-rate
# heuristics.
panel_dt_path <- here("Data", "Analysis", "panel_dt.csv")
if (!file.exists(panel_dt_path))
  stop("panel_dt.csv not found at: ", panel_dt_path,
       " — this is the 02b_Tank_level_Panel_Build.R output that carries mm_wall")

panel_wall <- fread(panel_dt_path, select = c("facility_id", "tank_id", "state", "mm_wall"))
panel_wall <- panel_wall[state == "TX"]
panel_wall[, state := NULL]
panel_wall <- unique(panel_wall, by = c("facility_id", "tank_id"))
cat(sprintf("  panel_dt.csv TX tanks loaded: %d unique (facility_id, tank_id)\n",
            nrow(panel_wall)))
cat("  mm_wall distribution (panel_dt.csv, TX):\n")
print(panel_wall[, .N, by = mm_wall][order(-N)])

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
# no zero-strip (08_Clean_TX.R:824).
raw_ust[, facility_id_key := {
  v <- suppressWarnings(as.integer(as.character(FACILITY_ID)))
  fifelse(is.na(v), trimws(as.character(FACILITY_ID)), as.character(v))
}]
raw_ust[, tank_id_key := TANK_ID]   # already toupper(trimws())'d above

raw_ust <- panel_wall[raw_ust, on = c("facility_id_key", "tank_id_key")]
cat(sprintf("  mm_wall panel join: %d / %d tanks matched (%.1f%%)\n",
            sum(!is.na(raw_ust$wall)), nrow(raw_ust), 100 * mean(!is.na(raw_ust$wall))))
raw_ust[, c("facility_id_key", "tank_id_key") := NULL]


###############################################################################
## STEP 2 — ACE contracts: filter + cartesian tank x contract-month          ##
###############################################################################

cat("=== STEP 2: ACE contracts ===\n")

contract_path <- here("Data", "Processed", "texas_fr_contract_month_panel.csv")
if (!file.exists(contract_path))
  stop("texas_fr_contract_month_panel.csv not found at: ", contract_path)

fa_contracts <- fread(contract_path)[
  ISSUER_NAME %chin% ISSUER_MATCH,
  .(FACILITY_ID, YEAR, MONTH, EFF_DATE, COVER_OCC, COVER_AGG)
]
fa_contracts[, EFF_DATE := ymd(EFF_DATE)]
fa_contracts[, FACILITY_ID := trimws(FACILITY_ID)]

stopifnot(nrow(fa_contracts) > 0L)
cat(sprintf("  ACE contracts: %d rows | %d facilities | years %d-%d\n",
            nrow(fa_contracts), uniqueN(fa_contracts$FACILITY_ID),
            min(fa_contracts$YEAR), max(fa_contracts$YEAR)))

# QA: how often does the actual FR contract limit deviate from the $1M/$1M
# reference we hold coverage fixed at (task instruction, banner)? Diagnostic
# only — not enforced (18a has no ILF/deductible ladder to check against).
cat(sprintf("  COVER_OCC == $1M in %.1f%% of contract-months (reference held fixed regardless)\n",
            100 * mean(fa_contracts$COVER_OCC == 1e6, na.rm = TRUE)))

# Subset tank table to ACE facilities only
ace_facs <- unique(fa_contracts$FACILITY_ID)
tank_sub <- raw_ust[FACILITY_ID %in% ace_facs]
cat(sprintf("  Tanks at ACE facilities: %d\n", nrow(tank_sub)))

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
ust_tm[, panel_year := as.integer(YEAR)]

cat(sprintf("  Tank-months (in-service): %d\n", nrow(ust_tm)))
stopifnot(nrow(ust_tm) > 0L)


###############################################################################
## STEP 3 — ACE construction bucket (wall carried through from raw_ust's     ##
##          mm_wall panel join at STEP 1 — no re-derivation here)           ##
###############################################################################

cat("=== STEP 3: construction bucket ===\n")

# mm_wall == "Double-Walled" OR is_composite_tank == 1 -> STIP3 (clad-steel;
# see banner "ACE CONSTRUCTION MAPPING"). Mixed-Wall/Unknown-Wall/unmatched
# tanks (wall NA) fall to BARE_STEEL by default, same as before.
ust_tm[, construction_code := fcase(
  wall == "DW" | is_composite_tank == 1L,   "STIP3",
  is_fiberglass_tank == 1L,                  "FIBERGLASS",
  default = "BARE_STEEL"
)]
cat("  construction_code distribution:\n")
print(ust_tm[, .N, by = construction_code][order(-N)])
cat(sprintf("  Card-eligible tank-months (wall SW/DW): %d / %d (%.1f%%)\n",
            sum(!is.na(ust_tm$wall)), nrow(ust_tm),
            100 * mean(!is.na(ust_tm$wall))))


###############################################################################
## STEP 4 — Price tank-months (Sec. B: base(age) * (1 - construction_credit))##
###############################################################################

cat("=== STEP 4: price tank-months ===\n")

ust_tm[, base_rate         := base_premium_ace(age_years)]
ust_tm[, construction_load := construction_credit_ace(construction_code)]
ust_tm[, tank_standard     := base_rate * (1 - construction_load)]

stopifnot(all(is.finite(ust_tm$tank_standard)))
stopifnot(all(ust_tm$tank_standard >= 0))

cat(sprintf("  tank_standard: mean=$%.0f  min=$%.0f  max=$%.0f\n",
            mean(ust_tm$tank_standard), min(ust_tm$tank_standard), max(ust_tm$tank_standard)))


###############################################################################
## STEP 4b — File 2: cell-year card (structural kernel — no credit/floor)   ##
###############################################################################
cat("=== STEP 4b: build cell-year card ===\n")

ust_tm[, age_bin := as.integer(cut(age_years,
                     c(0, 5, 10, 15, 20, 25, 30, 35, Inf),
                     labels = 1:8, right = FALSE, include.lowest = TRUE))]
ust_tm[is.na(age_bin), age_bin := 8L]

cell_card <- ust_tm[!is.na(wall), .(premium_usd_per_tank_yr = mean(tank_standard, na.rm = TRUE)),
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
print(cell_card[order(wall, age_bin, panel_year)][1:min(.N, 10)])


###############################################################################
## STEP 5 — Facility-month rollup: sum tanks, apply multi-tank credit        ##
###############################################################################
cat("=== STEP 5: facility-month rollup ===\n")

fac_month <- ust_tm[, .(
  fac_unmodified = sum(tank_standard, na.rm = TRUE),
  n_tanks_fm     = uniqueN(UST_ID)
), by = .(FACILITY_ID, YEAR, MONTH)]

fac_month[, iv_credit := multi_tank_credit_ace(n_tanks_fm)]
fac_month[, fac_credited := fac_unmodified * (1 - iv_credit)]

stopifnot(all(is.finite(fac_month$fac_credited)))
cat(sprintf("  Facility-month rows: %d\n", nrow(fac_month)))


###############################################################################
## STEP 6 — Facility-year rollup (mean months) + band + floor -> File 1      ##
###############################################################################
cat("=== STEP 6: facility-year rollup, band, floor ===\n")

fac_year <- fac_month[, .(
  standard_prem_pre = mean(fac_credited, na.rm = TRUE),
  n_tanks_rated      = as.integer(round(mean(n_tanks_fm)))
), by = .(FACILITY_ID, YEAR)]

fac_year[, panel_year := as.integer(YEAR)]
fac_year[, floor_y     := min_premium_of_year(panel_year)]

fac_year[, `:=`(
  min_prem      = pmax(standard_prem_pre * (1 - SCHED_CAP), floor_y),
  standard_prem = pmax(standard_prem_pre,                    floor_y),
  max_prem      = pmax(standard_prem_pre * (1 + SCHED_CAP), floor_y)
)]

stopifnot(all(fac_year$min_prem      >= fac_year$floor_y))
stopifnot(all(fac_year$standard_prem >= fac_year$floor_y))
stopifnot(all(fac_year$max_prem      >= fac_year$floor_y))
stopifnot(all(fac_year$min_prem      <= fac_year$standard_prem))
stopifnot(all(fac_year$standard_prem <= fac_year$max_prem))
stopifnot(all(fac_year$n_tanks_rated >= 1L))

fac_year[, panel_id := paste(toupper(trimws(FACILITY_ID)), "TX", sep = "_")]
fac_year[, carrier   := CARRIER_KEY]

out1 <- fac_year[, .(panel_id, panel_year, min_prem, standard_prem, max_prem,
                      carrier, n_tanks_rated)]
setorder(out1, panel_id, panel_year)

stopifnot(identical(names(out1), c("panel_id", "panel_year", "min_prem", "standard_prem",
                "max_prem", "carrier", "n_tanks_rated")))

out1_path <- file.path(out_dir, paste0(CARRIER_KEY, "_facility_year_premium.csv"))
fwrite(out1, out1_path)
cat(sprintf("  Facility-year rows: %d | facilities: %d | years: %d-%d\n",
            nrow(out1), uniqueN(out1$panel_id), min(out1$panel_year), max(out1$panel_year)))
cat(sprintf("  Saved: %s\n", out1_path))
cat("  Mean standard_prem by panel_year:\n")
print(out1[, .(N = .N, mean_std = round(mean(standard_prem), 2)), by = panel_year][order(panel_year)])


###############################################################################
## STEP 7 — File 3: multi-tank credit bands (from 18a's ladder, computed    ##
##          not transcribed, to guarantee agreement with the engine)        ##
###############################################################################
cat("=== STEP 7: multi-tank credit bands ===\n")

band_edges <- data.table(
  n_min = c(1L, 10L, 20L, 50L, 75L, 101L, 201L, 301L),
  n_max = c(9L, 19L, 49L, 74L, 100L, 200L, 300L, 9999L)
)
band_edges[, credit_mult := 1 - multi_tank_credit_ace(n_min)]

active_years <- sort(unique(out1$panel_year))
credit_tbl <- CJ(panel_year = active_years, n_min = band_edges$n_min)[
  band_edges, on = "n_min"
][, carrier := CARRIER_KEY]
setcolorder(credit_tbl, c("carrier", "panel_year", "n_min", "n_max", "credit_mult"))
setorder(credit_tbl, panel_year, n_min)

# Tiling check: every year's bands cover 1..9999 with no gap/overlap
for (yr in active_years) {
  yb <- credit_tbl[panel_year == yr][order(n_min)]
  n_bands <- nrow(yb)
  stopifnot(yb$n_min[1L] == 1L, yb$n_max[n_bands] == 9999L)
  stopifnot(all(yb$n_min[-1L] == yb$n_max[-n_bands] + 1L))
}

out3_path <- file.path(out_dir, paste0(CARRIER_KEY, "_multitank_credit.csv"))
fwrite(credit_tbl, out3_path)
cat(sprintf("  multi-tank credit: %s (%d rows, %d active years x %d bands)\n",
            out3_path, nrow(credit_tbl), length(active_years), nrow(band_edges)))


###############################################################################
## STEP 8 — File 4: minimum premium per active year (flat $350, no era)     ##
###############################################################################
cat("=== STEP 8: minimum premium per year ===\n")

min_prem_tbl <- data.table(panel_year = active_years)
min_prem_tbl[, min_premium_usd := min_premium_of_year(panel_year)]
min_prem_tbl[, carrier := CARRIER_KEY]
setcolorder(min_prem_tbl, c("carrier", "panel_year", "min_premium_usd"))

stopifnot(all(min_prem_tbl$min_premium_usd > 0))
stopifnot(uniqueN(min_prem_tbl$panel_year) == nrow(min_prem_tbl))

out4_path <- file.path(out_dir, paste0(CARRIER_KEY, "_min_premium.csv"))
fwrite(min_prem_tbl, out4_path)
cat(sprintf("  min premium: %s (%d rows)\n", out4_path, nrow(min_prem_tbl)))
print(min_prem_tbl)

cat(sprintf("\nRun completed: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=== 18b DONE ===\n")
