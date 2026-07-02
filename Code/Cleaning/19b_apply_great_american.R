###############################################################################
# 19b_apply_great_american.R — Apply Great American (STP Program) rate engine
#                                to Texas FR contracts; write the canonical
#                                4-file output (RATE_ENGINE_BUILD_TARGET.md).
# Filing: SERFF GACX-132517275 "STP Program Rates - Texas.pdf" (+ deductible
# correction GACX-132842896 p.3). See 19a_engine_great_american.R header for
# page cites and the MULTIPLICATIVE-composition resolution (2026-07-01).
# Sources 19a_engine_great_american.R for ALL rating functions, the multiple
# tank factor ladder, and the $225 minimum premium — nothing is re-derived here.
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# BEHAVIORAL ASSUMPTIONS BANNER
# ══════════════════════════════════════════════════════════════════════════════
# CARRIER:        Great American Insurance Company (STP Program)
# CARRIER_KEY:    "GREAT_AMERICAN"
# ISSUER_NAME:    PROPOSED, PENDING RESEARCHER CONFIRMATION (see return summary
#                 for the 036b DuckDB snippet). Candidate: "GREAT AMERICAN INS
#                 CO", NAIC 16691 (the entity that filed GACX-132517275). DO
#                 NOT run this script for real until the researcher has run the
#                 snippet and confirmed/edited ISSUER_MATCH.
#                 *** NAIC CAUTION: only NAIC 16691 filed this manual. Sibling
#                 Great American pool NAICs (26832, 22136, 26344, and any other
#                 "GREAT AMERICAN"-family issuer string that turns up in the FR
#                 panel) must NOT be folded in unless the researcher confirms
#                 they write under this same 16691 program — do not assume a
#                 pool-wide crosswalk the way AIG's C&I pooling was confirmed
#                 (project_aig_pooled_issuer_crosswalk memory). Flag any such
#                 sibling-NAIC string the snippet turns up and STOP. ***
#
# ACTIVE YEARS:   GA's STP program is filed 2021+ ONLY (GA_MIN_FILING_DATE,
#                 inherited from 19a). FR contract-months with EFF_DATE before
#                 2021-01-01 are dropped in STEP 2 (19a's own hard-stop lives in
#                 the ga_facility_premium() wrapper, which this apply script
#                 does NOT call directly — see STEP 5 note — so the date floor
#                 is re-implemented here explicitly).
#
# COVERAGE LIMIT / DEDUCTIBLE / COVERAGE C: $1,000,000/$1,000,000 occ/agg +
#                 $5,000 deductible + Coverage C OFF, held at 19a's reference
#                 contract for BOTH File 1 (the real bill) and File 2 (the
#                 card) — per 036-locked invariant 1 and this task's explicit
#                 instruction. Actual FR contract COVER_OCC/COVER_AGG are NOT
#                 wired into pricing (19a's ilf_factor_ga() hard-stops on any
#                 non-reference limit — no ILF ladder beyond the single
#                 reference point was filed for GA). REF_DED_ILF_FACTOR =
#                 deductible_factor_ga(REF_DEDUCTIBLE) * ilf_factor_ga(REF_
#                 COVER_OCC, REF_COVER_AGG) is asserted == 1.00 below.
#
# SCHEDULE:       NONE FILED for this program (19a, confirmed) -> min_prem ==
#                 standard_prem == max_prem identically, at facility-month
#                 grain, before the mean-to-year rollup. No +/-40% envelope
#                 (unlike AIG/TOMICS/ACE/Zurich).
#
# MULTIPLE TANK FACTOR: Applied at FACILITY-MONTH level off the actual
#                 observed tank count that month (multiple_tank_factor_ga(),
#                 19a §E9), 1-10=1.000 .. 101-200=0.700 (filed ceiling 200
#                 tanks). File 3's top band (101-9999) EXTENDS the filed
#                 101-200=0.700 factor flat to 9999 — ASSUMPTION, flagged,
#                 same convention as 19a's own age>35 extension.
#
# MINIMUM PREMIUM: Flat $225 (MIN_PREMIUM_GA), no era switch, applied at
#                 facility-MONTH grain to facility_standard before the mean-
#                 to-year rollup (19a's own facility_premium builder applies
#                 the floor to facility_standard, not to a pre-credit sum).
#
# CONTENTS MAPPING (raw is_gasoline/is_diesel/is_oil_kerosene/is_jet_fuel/
#                 is_other — the ONLY fuel granularity in raw_pst_ust.csv, per
#                 08_Clean_TX.R; there is NO raw signal finer than these 5
#                 buckets) -> GA's 10 contents codes:
#   GASOLINE        <- is_gasoline == 1
#   DIESEL           <- is_diesel == 1
#   AVIATION_FUEL    <- is_jet_fuel == 1
#   KEROSENE         <- is_oil_kerosene == 1
#     [HITL FLAG: is_oil_kerosene is ONE raw bucket covering what GA prices as
#      FOUR separate codes — FuelOil/HeatingOil/WasteOil/Kerosene — but all
#      four share the IDENTICAL 0.95 factor, so the KEROSENE label is
#      numerically inconsequential among those four. It is NOT interchangeable
#      with Lube Oil (0.90).]
#   default (is_other == 1, or no fuel flag set) -> GASOLINE (1.00, no-op)
#     [HITL FLAG: Lube Oil (0.90), Propane (0.70), and LNG (0.80) have NO raw
#      source at all in raw_pst_ust.csv — UNREACHABLE from this data. The
#      is_other catch-all is defaulted to GASOLINE (the neutral 1.00 factor)
#      rather than fabricating a discount/surcharge for an unclassified
#      substance. Confirm this default, or supply a field that can positively
#      identify Lube Oil / Propane / LNG tanks.]
#
# LEAK DETECTION MAPPING (raw DET_C_* compartment-level flags) -> GA's 7 codes:
#   CONTINUOUS_IN_TANK <- DET_C_SEC_CONT == 1
#     [HITL FLAG: GA's best-factor code (0.80) has no obviously-named raw
#      counterpart. DET_C_SEC_CONT ("secondary containment" continuous
#      monitoring) is PROPOSED as the closest proxy but is NOT a confirmed 1:1
#      match to "Continuous In-Tank Leak Detection" — confirm, or leave this
#      code permanently unassigned (falls through to INTERSTITIAL etc.).
#      *** DET_C_SEC_CONT's PRESENCE in raw_pst_ust.csv is UNCONFIRMED — it is
#      in the PST_UST_COMPRT.TXT spec (08_Clean_TX.R COMPRT_NAMES) but no
#      existing engine (17b/18b/16b) has needed it, so it has never been
#      proven present in the "parsed" raw_pst_ust.csv. STOP: hard-stops below
#      if missing. ***]
#   INTERSTITIAL       <- DET_C_INTERSTITIAL == 1
#   ATG                <- DET_C_ATG == 1
#   SIR                <- DET_C_SIR == 1
#   VAPOR              <- DET_C_VAPOR == 1
#   GROUNDWATER        <- DET_C_GW == 1
#   default (no other flag set) -> MANUAL (1.15, the worst/most conservative
#     factor)
#     [HITL FLAG: unlike AIG's leak table, GA's filed leak-detection table has
#      NO "None" option — every tank must land in one of the 7 codes. Tanks
#      with no positive detection flag default to MANUAL as the conservative
#      catch-all (highest factor), mirroring AIG/ACE's own catch-all
#      philosophy. This default requires no raw "Manual" flag at all — it
#      fires unconditionally for any tank that matched none of the other 6
#      codes, so DET_C_MANUAL_WEEK/DET_C_MANUAL_MONTH are deliberately NOT
#      fetched here (they would add hard-stop risk for zero functional
#      benefit, since they can only ever route tanks to a code they'd already
#      land on by default).]
#
# LINE LEAK DETECTION MAPPING: YES <- DET_P_LLD == 1, else NO. Clean 1:1 raw
#                 source (piping Line Leak Detector flag), high confidence.
#
# CONSTRUCTION MAPPING (GA's 6 codes) — CONFIRMED FROM THE FILING TEXT
#   (pdftotext -layout on "rate manuel 2020.pdf" p.6, cross-checked against
#   19a's already-confirmed factor values 1.15/1.00/1.15/1.50/0.90/0.80):
#     SWF  = "Single-walled Fiberglass"                              1.15
#     DWF  = "Double-walled Fiberglass"                               1.00
#     SWSC = "Single-walled Steel with cathodic protection"           1.15
#     SWS  = "Single-walled steel no cathodic protection"             1.50
#     FRP  = "Steel with Fiberglass Reinforced Plastic (Containment)" 0.90
#     DWS  = "Double-walled Steel"                                    0.80
#   *** CORRECTION vs the task's own illustrative example ("composite/clad-
#   steel -> the clad code, e.g. SWSC/DWS"): the filing text shows FRP itself
#   IS the composite/clad-steel code (steel core with an FRP containment
#   layer), NOT SWSC/DWS (which are cathodic-protection / plain double-wall
#   steel, no cladding mentioned). Routing composite tanks to FRP per the
#   actual filing language, not the task's example. ***
#   Raw material flags (is_fiberglass_tank vs is_composite_tank — DISTINCT
#   0/1 columns in raw_pst_ust.csv, confirmed via 17b_apply_aig.R Step 1):
#     FRP  <- is_composite_tank == 1
#       [HITL FLAG: FRP has NO wall-specific (SW vs DW) split in the filing —
#        only ONE FRP code exists for composite/clad-steel construction,
#        unlike Fiberglass (SWF/DWF) and Steel (SWSC/SWS/DWS). PROPOSED:
#        composite tanks route to FRP regardless of mm_wall (SW or DW).
#        Confirm, or state whether composite+DW tanks should instead route to
#        DWS as ordinary double-wall steel.]
#     SWF  <- is_fiberglass_tank == 1 & wall == "SW"
#     DWF  <- is_fiberglass_tank == 1 & wall == "DW"
#     DWS  <- (steel/unclassified material, i.e. not fiberglass, not
#              composite) & wall == "DW"
#              [no DW+CP-specific code is filed — DWS covers all DW steel]
#     SWSC <- (steel/unclassified material) & wall == "SW" & has_cp
#     SWS  <- (steel/unclassified material) & wall == "SW" & !has_cp
#       [HITL FLAG: concrete/unknown-material tanks (is_fiberglass_tank == 0 &
#        is_composite_tank == 0) fall through to the steel branch by default —
#        same conservative-default convention as AIG (17b) and ACE (18b).
#        Confirm this is acceptable.]
#   CORR_TANK_CP coding scheme is AUTO-DETECTED at runtime (Y/N vs
#   categorical C/E/B/N/U — precedent scripts disagree, see 17b) and hard-
#   stops on any unrecognized value (detect_cp_scheme(), copied verbatim from
#   17b_apply_aig.R — this is a raw-coding-ambiguity safeguard, unrelated to
#   the mm_wall-join "no auto-pick" rule below).
#
# TANK CAPACITY: raw CAPACITY (gallons) fed directly to capacity_factor_ga(),
#                 which hard-stops outside the filed 1-30,000 range (19a §E1
#                 — by design, does not fabricate a factor). Tank-months with
#                 missing or out-of-[1,30000] CAPACITY are DROPPED before
#                 pricing (STEP 2), with the drop count/pct printed as a QA
#                 diagnostic, rather than letting one bad value hard-stop the
#                 entire run.
#
# ══════════════════════════════════════════════════════════════════════════════
# WALL SOURCE (UNIVERSAL RULE — task instruction, no exceptions):
# The tank wall (SW/DW) — for BOTH the card cell AND the wall component of
# GA's construction code — comes from the DCM-state `mm_wall` column in
# `Data/Analysis/panel_dt.csv` (built by Code/Analysis/02b_Tank_level_Panel_
# Build.R, ultimately sourced from Code/Cleaning/10_Master_Cleaning_and_
# Harmonization.r Step 3.5.1), joined onto tanks on (facility_id, tank_id) —
# a STRAIGHT left join, standard normalization ONLY (toupper/trim
# facility_id; tank_id used exactly AS PARSED, no further normalization —
# per the task's explicit instruction). NO raw double_walled/single_walled
# dummy anywhere in this script. NO candidate-key auto-pick / match-rate
# gating / fallback heuristics (contrast with 18b_apply_ace.R's TRIM-vs-INT
# candidate-key dance, which this task explicitly says NOT to repeat) — ONE
# key, built one way, joined directly. A single informational match-rate
# cat() is printed (not a branch, not a gate — pure diagnostic, matching the
# project's general QA-print convention).
#
# panel_dt.csv has NO standalone tank_id column — only `tank_panel_id =
# paste(facility_id, state, tank_id, sep="_")` (confirmed,
# 02b_Tank_level_Panel_Build.R line 287). tank_id is recovered by stripping
# the known `facility_id_state_` prefix (vectorized substring, exact — safe
# because facility_id is numeric-only and state is a 2-letter code, neither
# containing "_").
#
# mm_wall categories (confirmed, 10_Master_Cleaning_and_Harmonization.r Step
# 3.5.1): "Single-Walled", "Double-Walled", "Mixed-Wall", "Unknown-Wall".
# Card wall := "SW"/"DW" for the first two, NA (card-excluded) for the other
# two. "Mixed-Wall" tanks are KEPT in ust_tm (not row-dropped outright) but,
# per the task's explicit instruction ("do not force it into SW or DW"), are
# NOT assigned a wall for pricing purposes either — since GA's construction
# code needs a wall for 4 of its 6 codes (SWF/DWF/SWSC/SWS; only FRP is
# wall-free), a tank with no confirmed SW/DW cannot be construction-coded
# without fabricating a wall pick. Wall-NA tank-months (Mixed-Wall,
# Unknown-Wall, AND any tank that fails the mm_wall join) are therefore
# DROPPED before pricing in BOTH File 1 and File 2 — same "cannot price
# without the join" logic as 18b_apply_ace.R, just without the candidate-key
# machinery. Drop count/pct printed (STEP 3).
#
# *** THREE OPEN RISKS ON THIS JOIN — read before trusting File 1/2 numbers: ***
#   (1) LEADING ZEROS: panel_dt.csv's TX `facility_id` is passed through
#       `standardize_numeric_id()` in 08_Clean_TX.R (integer round-trip ->
#       leading zeros STRIPPED, e.g. "001234" -> "1234"; confirmed by reading
#       08_Clean_TX.R lines 562-566, 822). This script's raw-side FACILITY_ID
#       (6-char substr of FACILITY_ID_PAD, the 12_Rate_MidCont/17b/18b
#       convention, ALSO used for the output `panel_id` per README §7) KEEPS
#       leading zeros. A literal toupper/trim-only join (as instructed) may
#       therefore under-match on facility IDs with leading zeros. This is
#       NOT worked around here (no auto zero-stripping — that would be
#       exactly the "fallback heuristic" the task says not to add); it is
#       flagged for the researcher to confirm via real match-rate numbers
#       (the STEP 3 diagnostic print) before trusting the output.
#   (2) YEAR RANGE: a local scan of panel_dt.csv found `panel_year` tops out
#       at 2020 (max observed: 2020; TX cannot exceed the global max). GA
#       prices 2021+ ONLY. The join is therefore correctly built WITHOUT
#       panel_year (matching the task's literal "(facility_id, tank_id)" key,
#       not ACE's "(facility_id, tank_id, panel_year)") — mm_wall is treated
#       as time-invariant per tank (consistent with its own documented
#       provenance as "a pure function of immutable tank physical
#       characteristics", 10_Master_Cleaning_and_Harmonization.r comment). A
#       stopifnot below asserts mm_wall really is constant within
#       tank_panel_id in the observed data (if it isn't, the assumption is
#       wrong and the script hard-stops rather than silently picking one).
#   (3) SAMPLE COVERAGE: panel_dt.csv is built by 02b_Tank_level_Panel_
#       Build.R as a restricted analysis/study-tank sample (e.g. tanks with
#       unknown install cohort or unknown fuel get NA'd out of several derived
#       columns upstream), NOT necessarily a full census of raw_pst_ust.csv's
#       TX tanks. A local scan found ONLY "Single-Walled"/"Double-Walled" in
#       panel_dt.csv's mm_wall column (no "Mixed-Wall"/"Unknown-Wall" rows
#       observed) — consistent with upstream sample restriction, not with
#       the full raw population. Unmatched-join tanks in this script may
#       therefore reflect genuine sample exclusion, not just an ID-key
#       mismatch. The match-rate diagnostic (STEP 3) cannot distinguish these
#       two causes; flagging both for the researcher.
# ══════════════════════════════════════════════════════════════════════════════
#
# OUTPUT (4 files, Data/Analysis/rate_engines/):
#   GREAT_AMERICAN_facility_year_premium.csv (File 1 — panel_id, panel_year,
#     min_prem, standard_prem, max_prem, carrier, n_tanks_rated — the 7 cols
#     match RATE_ENGINE_BUILD_TARGET.md exactly. min==standard==max, no
#     schedule band filed.)
#   GREAT_AMERICAN_cell_era_card.csv         (File 2 — carrier, wall, age_bin,
#     panel_year, premium_usd_per_tank_yr — keyed on calendar year; wall from
#     mm_wall; Mixed-Wall/Unknown-Wall/unmatched tanks EXCLUDED.)
#   GREAT_AMERICAN_multitank_credit.csv      (File 3 — carrier, panel_year,
#     n_min, n_max, credit_mult; bands tile 1..9999 exactly; top band
#     101-9999 EXTENDS the filed 101-200 factor flat, flagged above.)
#   GREAT_AMERICAN_min_premium.csv           (File 4 — carrier, panel_year,
#     min_premium_usd = $225 flat, one row per active year.)
# ══════════════════════════════════════════════════════════════════════════════

library(data.table)
library(lubridate)
library(here)

source(here("Code", "Cleaning", "19a_engine_great_american.R"))
# inherits: BASE_RATE_GA, MIN_PREMIUM_GA, REF_COVER_OCC, REF_COVER_AGG,
#           REF_DEDUCTIBLE, GA_MIN_FILING_DATE
# inherits: capacity_factor_ga, age_factor_ga, construction_factor_ga,
#           contents_factor_ga, leak_detection_factor_ga,
#           line_leak_detection_factor_ga, deductible_factor_ga,
#           ilf_factor_ga, multiple_tank_factor_ga, apply_floor_ga,
#           ga_facility_premium

# ── Logging ──────────────────────────────────────────────────────────────────
.log_path <- here::here("logs", paste0(
  "19b_apply_great_american_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 19b_apply_great_american.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== 19b_apply_great_american: Great American premium apply ===\n")
cat(sprintf("Run started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

CARRIER_KEY <- "GREAT_AMERICAN"

# PROPOSED, PENDING CONFIRMATION (see banner + return summary snippet).
# DO NOT run for real until the researcher has confirmed / edited this list.
ISSUER_MATCH <- c("GREAT AMERICAN INS CO")

out_dir <- here("Data", "Analysis", "rate_engines")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Reference-contract self-check: deductible/ILF factor at the 036-locked
# reference ($5,000 ded, $1M/$1M) collapses to a no-op multiplier.
REF_DED_ILF_FACTOR <- deductible_factor_ga(REF_DEDUCTIBLE) * ilf_factor_ga(REF_COVER_OCC, REF_COVER_AGG)
stopifnot(isTRUE(all.equal(REF_DED_ILF_FACTOR, 1.00, tolerance = 1e-8)))
cat(sprintf("  Reference deductible/ILF factor = %.4f (OK)\n", REF_DED_ILF_FACTOR))

# Filing-minimum-premium lookup — GA has NO era switch, flat $225 every year
# (shared by File 1's facility-month floor and File 4).
min_premium_of_year <- function(panel_year) rep(MIN_PREMIUM_GA, length(panel_year))


###############################################################################
## STEP 1 — Build tank attributes from raw_pst_ust.csv                       ##
##          (12_Rate_MidCont Section A recipe; mirrors 17b/18b Step 1)       ##
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

# TANK_ID: REQUIRED for the mm_wall join (banner "WALL SOURCE"). NOT the same
# field as UST_ID (see 18b's note) — hard-stop if missing rather than
# silently fall back to UST_ID, which would silently break the join.
needed_raw <- c(
  "UST_ID", "TANK_ID", "INSTALL_DATE", "end_date",
  "CAPACITY",
  "is_fiberglass_tank", "is_composite_tank", "is_steel_tank",
  "CORR_TANK_CP",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel",
  "DET_C_INTERSTITIAL", "DET_C_ATG", "DET_C_GW", "DET_C_VAPOR", "DET_C_SIR",
  "DET_C_SEC_CONT",
  "DET_P_LLD"
)
missing_flds <- setdiff(needed_raw, actual_names)
if (length(missing_flds) > 0L) {
  cat("  STOP: fields not found in raw_pst_ust.csv:\n")
  cat(paste(" ", missing_flds, collapse = "\n"), "\n")
  if ("TANK_ID" %in% missing_flds)
    cat("  TANK_ID missing -> the mm_wall panel join (banner 'WALL SOURCE') has no",
        " raw-side key. STOP and confirm the real column name with the researcher\n")
  if ("DET_C_SEC_CONT" %in% missing_flds)
    cat("  DET_C_SEC_CONT presence was UNCONFIRMED (banner LEAK DETECTION MAPPING) —",
        " if genuinely absent, drop it from needed_raw, remove the CONTINUOUS_IN_TANK",
        " branch (MANUAL already covers every tank that would have fallen there as the",
        " unconditional default), and confirm with the researcher before re-running.\n")
  stop("Fix the column names above (server column names may differ) and re-run.")
}

raw_ust <- fread(raw_path, select = c(id_col, needed_raw))

# Output-facing FACILITY_ID: README §7 convention (6-char substr, leading
# zeros KEPT) — used for panel_id (File 1) AND as the mm_wall-join facility_id
# after toupper/trim (see banner risk (1) — leading zeros are NOT stripped
# here per the task's literal "toupper/trim" instruction).
raw_ust[, FACILITY_ID := trimws(substr(get(id_col), 1L, 6L))]
if (id_col != "FACILITY_ID") raw_ust[, (id_col) := NULL]

raw_ust[, UST_ID       := toupper(trimws(UST_ID))]
raw_ust[, TANK_ID_join := as.character(TANK_ID)]     # tank_id AS-IS — no trim, no case change (task instruction)
raw_ust[, INSTALL_DATE := ymd(INSTALL_DATE)]
raw_ust[, CLOSED_DATE  := ymd(end_date)]
raw_ust[, end_date     := NULL]

# ── CORR_TANK_CP coding auto-detect (raw-coding ambiguity, unrelated to the
#    mm_wall-join "no heuristics" rule — copied verbatim from 17b) ──────────
detect_cp_scheme <- function(x, field_name) {
  vals <- unique(x[!is.na(x) & x != ""])
  if (length(vals) == 0L) {
    cat(sprintf("  %s: no non-missing values observed — defaulting has_cp=FALSE for all\n", field_name))
    return("NONE")
  }
  if (all(vals %chin% c("Y", "N"))) return("YN")
  if (all(vals %chin% c("C", "E", "B", "N", "U"))) return("CEBNU")
  stop(sprintf(
    "%s: unrecognized value set {%s} — matches neither the Y/N scheme (12_Rate_MidCont) nor the C/E/B/N/U scheme (16a Zurich notes). STOP and confirm the real coding with the researcher before proceeding.",
    field_name, paste(vals, collapse = ", ")))
}
cp_scheme_tank <- detect_cp_scheme(raw_ust$CORR_TANK_CP, "CORR_TANK_CP")
cat(sprintf("  CORR_TANK_CP coding detected: %s\n", cp_scheme_tank))

cp_flag <- function(x, scheme) {
  if (scheme == "YN")    return(!is.na(x) & x == "Y")
  if (scheme == "CEBNU") return(!is.na(x) & x %chin% c("C", "E", "B"))
  rep(FALSE, length(x))
}
raw_ust[, has_cp := cp_flag(CORR_TANK_CP, cp_scheme_tank)]

# ── Normalize boolean/Y-N inputs to 0/1 (no NA) ──────────────────────────────
raw_ust[, is_fiberglass_tank := fifelse(is.na(is_fiberglass_tank), 0L, as.integer(as.logical(is_fiberglass_tank)))]
raw_ust[, is_composite_tank  := fifelse(is.na(is_composite_tank), 0L, as.integer(as.logical(is_composite_tank)))]
raw_ust[, is_steel_tank      := fifelse(is.na(is_steel_tank), 0L, as.integer(as.logical(is_steel_tank)))]
raw_ust[, is_gasoline        := fifelse(is.na(is_gasoline), 0L, as.integer(as.logical(is_gasoline)))]
raw_ust[, is_diesel          := fifelse(is.na(is_diesel), 0L, as.integer(as.logical(is_diesel)))]
raw_ust[, is_oil_kerosene    := fifelse(is.na(is_oil_kerosene), 0L, as.integer(as.logical(is_oil_kerosene)))]
raw_ust[, is_jet_fuel        := fifelse(is.na(is_jet_fuel), 0L, as.integer(as.logical(is_jet_fuel)))]
raw_ust[, det_c_interstitial := fifelse(!is.na(DET_C_INTERSTITIAL) & DET_C_INTERSTITIAL == "Y", 1L, 0L)]
raw_ust[, det_c_atg          := fifelse(!is.na(DET_C_ATG)          & DET_C_ATG          == "Y", 1L, 0L)]
raw_ust[, det_c_gw           := fifelse(!is.na(DET_C_GW)           & DET_C_GW           == "Y", 1L, 0L)]
raw_ust[, det_c_vapor        := fifelse(!is.na(DET_C_VAPOR)        & DET_C_VAPOR        == "Y", 1L, 0L)]
raw_ust[, det_c_sir          := fifelse(!is.na(DET_C_SIR)          & DET_C_SIR          == "Y", 1L, 0L)]
raw_ust[, det_c_sec_cont     := fifelse(!is.na(DET_C_SEC_CONT) & DET_C_SEC_CONT == "Y", 1L, 0L)]
raw_ust[, det_p_lld          := fifelse(!is.na(DET_P_LLD) & DET_P_LLD == "Y", 1L, 0L)]

cat(sprintf("  Raw tanks loaded: %d | facilities: %d\n",
            nrow(raw_ust), uniqueN(raw_ust$FACILITY_ID)))


###############################################################################
## STEP 2 — Great American contracts: filter + cartesian tank x contract-mo. ##
###############################################################################

cat("=== STEP 2: Great American contracts ===\n")

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
cat(sprintf("  Great American contracts (pre 2021+ filter): %d rows | %d facilities | years %d-%d\n",
            nrow(fa_contracts), uniqueN(fa_contracts$FACILITY_ID),
            min(fa_contracts$YEAR), max(fa_contracts$YEAR)))

# GA's STP program is filed 2021+ only (GA_MIN_FILING_DATE) — re-implemented
# here explicitly since this apply script calls the individual rating
# functions directly, not the ga_facility_premium() wrapper that carries the
# hard-stop (see STEP 5 note).
n_pre_2021 <- fa_contracts[EFF_DATE < GA_MIN_FILING_DATE, .N]
fa_contracts <- fa_contracts[EFF_DATE >= GA_MIN_FILING_DATE]
cat(sprintf("  Dropped %d pre-2021 contract-months (GA's STP program is 2021+ only)\n", n_pre_2021))
stopifnot(nrow(fa_contracts) > 0L)
cat(sprintf("  Great American contracts (2021+): %d rows | %d facilities | years %d-%d\n",
            nrow(fa_contracts), uniqueN(fa_contracts$FACILITY_ID),
            min(fa_contracts$YEAR), max(fa_contracts$YEAR)))

# QA: how often does the actual FR contract limit deviate from the $1M/$1M
# reference we hold coverage fixed at (invariant 1 / task instruction)?
cat(sprintf("  COVER_OCC == $1M in %.1f%% of contract-months (reference held fixed regardless)\n",
            100 * mean(fa_contracts$COVER_OCC == 1e6, na.rm = TRUE)))

# Subset tank table to Great American facilities only
ga_facs <- unique(fa_contracts$FACILITY_ID)
tank_sub <- raw_ust[FACILITY_ID %in% ga_facs]
cat(sprintf("  Tanks at Great American facilities: %d\n", nrow(tank_sub)))

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
ust_tm[, age_years  := pmax(0L, floor(as.numeric(EFF_DATE - INSTALL_DATE) / 365.25))]
ust_tm[, panel_year := as.integer(YEAR)]

cat(sprintf("  Tank-months (in-service): %d\n", nrow(ust_tm)))
stopifnot(nrow(ust_tm) > 0L)

# Capacity range QA/drop (capacity_factor_ga hard-stops outside [1,30000] —
# drop out-of-range/missing tank-months here with a printed diagnostic rather
# than let one bad value crash the whole run; see banner "TANK CAPACITY").
n_pre_cap <- nrow(ust_tm)
n_bad_cap <- ust_tm[is.na(CAPACITY) | CAPACITY < 1 | CAPACITY > 30000, .N]
ust_tm <- ust_tm[!is.na(CAPACITY) & CAPACITY >= 1 & CAPACITY <= 30000]
cat(sprintf("  Dropped %d / %d tank-months (%.2f%%) with missing/out-of-[1,30000] CAPACITY\n",
            n_bad_cap, n_pre_cap, 100 * n_bad_cap / n_pre_cap))
stopifnot(nrow(ust_tm) > 0L)


###############################################################################
## STEP 3 — mm_wall JOIN (panel_dt.csv, TX only, straight left join)         ##
##          See banner "WALL SOURCE" — NO candidate-key auto-pick, NO        ##
##          match-rate gating; one key, joined once, diagnostic print only.  ##
###############################################################################

cat("=== STEP 3: mm_wall panel join ===\n")

panel_dt_path <- here("Data", "Analysis", "panel_dt.csv")
if (!file.exists(panel_dt_path))
  stop("panel_dt.csv not found at: ", panel_dt_path,
       " — this is the 02b_Tank_level_Panel_Build.R output that carries mm_wall")

panel_wall <- fread(panel_dt_path,
                     select = c("tank_panel_id", "facility_id", "state", "mm_wall"))
panel_wall <- panel_wall[state == "TX"]
panel_wall[, state := NULL]

# tank_panel_id = paste(facility_id, state, tank_id, sep = "_") — recover
# tank_id by stripping the known "<facility_id>_TX_" prefix (vectorized
# substring; exact because facility_id is numeric-only and state is 2 chars,
# neither containing "_").
prefix_len <- nchar(panel_wall$facility_id) + nchar("TX") + 2L
panel_wall[, tank_id_join := substring(tank_panel_id, prefix_len + 1L)]
panel_wall[, facility_id_join := toupper(trimws(facility_id))]

# mm_wall is documented as a pure function of immutable tank physical
# characteristics (10_Master_Cleaning_and_Harmonization.r Step 3.5.1 comment)
# — collapse to ONE row per tank_panel_id (no panel_year in the join key; see
# banner risk (2) — panel_dt.csv's years top out at 2020, entirely before
# GA's 2021+ active window). Assert constancy rather than silently picking
# one if it turns out NOT to be constant.
n_multi_wall <- panel_wall[, uniqueN(mm_wall), by = tank_panel_id][V1 > 1L, .N]
if (n_multi_wall > 0L)
  stop(sprintf(
    "mm_wall is NOT constant within tank_panel_id for %d tanks — the 'pure function of immutable characteristics' assumption (banner risk 2) is wrong. STOP and confirm with the researcher before collapsing across panel_year.",
    n_multi_wall))
panel_wall <- unique(panel_wall, by = "tank_panel_id")
cat(sprintf("  panel_dt.csv TX tanks (deduped, mm_wall constant per tank): %d\n", nrow(panel_wall)))
cat("  mm_wall distribution (panel_dt.csv, TX):\n")
print(panel_wall[, .N, by = mm_wall][order(-N)])

# ONE join key, joined once (no candidate-key auto-pick).
n_total <- nrow(ust_tm)
match_n <- ust_tm[panel_wall, on = c(FACILITY_ID = "facility_id_join", TANK_ID_join = "tank_id_join"),
                   nomatch = NULL, .N]
cat(sprintf("  mm_wall join match: %d / %d tank-months (%.1f%%) — INFORMATIONAL ONLY, not a gate;",
            match_n, n_total, 100 * match_n / n_total))
cat(" see banner risks (1)-(3) before trusting this number.\n")

ust_tm <- panel_wall[ust_tm, on = c(facility_id_join = "FACILITY_ID", tank_id_join = "TANK_ID_join")]

# Card wall dimension: SW/DW only. "Mixed-Wall"/"Unknown-Wall"/unmatched -> NA.
ust_tm[, wall := fcase(
  mm_wall == "Single-Walled", "SW",
  mm_wall == "Double-Walled", "DW",
  default = NA_character_
)]
cat(sprintf("  Wall-eligible tank-months (SW/DW from mm_wall): %d / %d (%.1f%%)\n",
            sum(!is.na(ust_tm$wall)), nrow(ust_tm), 100 * mean(!is.na(ust_tm$wall))))
cat("  Wall-NA breakdown (join-unmatched vs Mixed-Wall vs Unknown-Wall):\n")
print(ust_tm[is.na(wall), .N, by = .(mm_wall_raw = fifelse(is.na(mm_wall), "UNMATCHED", mm_wall))][order(-N)])

# Wall-NA tank-months cannot be construction-coded (4 of GA's 6 codes require
# a wall) without forcing a wall pick — DROPPED before pricing, both File 1
# and File 2 (banner "WALL SOURCE").
n_pre_wall <- nrow(ust_tm)
ust_tm <- ust_tm[!is.na(wall)]
cat(sprintf("  Dropped %d / %d tank-months (%.1f%%) with no confirmed SW/DW wall\n",
            n_pre_wall - nrow(ust_tm), n_pre_wall, 100 * (n_pre_wall - nrow(ust_tm)) / n_pre_wall))
stopifnot(nrow(ust_tm) > 0L)


###############################################################################
## STEP 4 — Assign construction / contents / leak / line-leak codes          ##
###############################################################################

cat("=== STEP 4: assign rating codes ===\n")

# ── A) Construction code (see banner CONSTRUCTION MAPPING) ─────────────────
map_ga_construction_code <- function(wall, is_fiberglass, is_composite, has_cp) {
  fcase(
    is_composite == 1L,                              "FRP",
    is_fiberglass == 1L & wall == "SW",               "SWF",
    is_fiberglass == 1L & wall == "DW",               "DWF",
    wall == "DW",                                      "DWS",
    wall == "SW" & has_cp,                              "SWSC",
    wall == "SW" & !has_cp,                              "SWS",
    default = NA_character_
  )
}
ust_tm[, construction_code := map_ga_construction_code(
  wall, is_fiberglass_tank, is_composite_tank, has_cp)]
stopifnot(!anyNA(ust_tm$construction_code))
cat("  construction_code distribution:\n"); print(ust_tm[, .N, by = construction_code][order(-N)])

# ── B) Contents code (see banner CONTENTS MAPPING) ──────────────────────────
map_ga_contents_code <- function(is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel) {
  fcase(
    is_gasoline == 1L,     "GASOLINE",
    is_diesel == 1L,       "DIESEL",
    is_jet_fuel == 1L,     "AVIATION_FUEL",
    is_oil_kerosene == 1L, "KEROSENE",
    default = "GASOLINE"
  )
}
ust_tm[, contents_code := map_ga_contents_code(is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel)]
cat("  contents_code distribution:\n"); print(ust_tm[, .N, by = contents_code][order(-N)])

# ── C) Leak detection code (see banner LEAK DETECTION MAPPING) ─────────────
map_ga_leak_code <- function(sec_cont, interstitial, atg, sir, vapor, gw) {
  fcase(
    sec_cont == 1L,      "CONTINUOUS_IN_TANK",
    interstitial == 1L,  "INTERSTITIAL",
    atg == 1L,            "ATG",
    sir == 1L,             "SIR",
    vapor == 1L,            "VAPOR",
    gw == 1L,                "GROUNDWATER",
    default = "MANUAL"
  )
}
ust_tm[, leak_code := map_ga_leak_code(
  det_c_sec_cont, det_c_interstitial, det_c_atg, det_c_sir, det_c_vapor, det_c_gw)]
cat("  leak_code distribution:\n"); print(ust_tm[, .N, by = leak_code][order(-N)])

# ── D) Line leak detection code ─────────────────────────────────────────────
ust_tm[, line_leak_code := fifelse(det_p_lld == 1L, "YES", "NO")]
cat("  line_leak_code distribution:\n"); print(ust_tm[, .N, by = line_leak_code][order(-N)])


###############################################################################
## STEP 5 — Price each tank-month (multiplicative chain, §III-VII of 19a)   ##
##          NOTE: calls the individual pure functions directly, NOT the     ##
##          ga_facility_premium() wrapper (that wrapper is facility-scalar, ##
##          single eff_date; this apply script needs vectorized tank-month  ##
##          pricing — same convention as 17b/18b, which also bypass their   ##
##          engine's facility-level wrapper for the same reason).           ##
###############################################################################

cat("=== STEP 5: price tank-months ===\n")

ust_tm[, `:=`(
  cap_f    = capacity_factor_ga(CAPACITY),
  age_f    = age_factor_ga(age_years),
  constr_f = construction_factor_ga(construction_code),
  cont_f   = contents_factor_ga(contents_code),
  leak_f   = leak_detection_factor_ga(leak_code),
  llk_f    = line_leak_detection_factor_ga(line_leak_code)
)]
ust_tm[, rating_adjustments := cap_f * age_f * constr_f * cont_f * leak_f * llk_f]
ust_tm[, tank_premium := BASE_RATE_GA * rating_adjustments * REF_DED_ILF_FACTOR]

stopifnot(all(is.finite(ust_tm$tank_premium)))
stopifnot(all(ust_tm$tank_premium >= 0))

cat(sprintf("  tank_premium: mean=$%.0f  min=$%.0f  max=$%.0f\n",
            mean(ust_tm$tank_premium), min(ust_tm$tank_premium), max(ust_tm$tank_premium)))


###############################################################################
## STEP 5b — File 2: cell-year card (structural kernel — no credit/floor)   ##
###############################################################################
cat("=== STEP 5b: build cell-year card ===\n")

ust_tm[, age_bin := as.integer(cut(age_years,
                     c(0, 5, 10, 15, 20, 25, 30, 35, Inf),
                     labels = 1:8, right = FALSE, include.lowest = TRUE))]
ust_tm[is.na(age_bin), age_bin := 8L]

cell_card <- ust_tm[, .(premium_usd_per_tank_yr = mean(tank_premium, na.rm = TRUE)),
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
## STEP 6 — Facility-month rollup: sum tanks, Multiple Tank Factor, floor    ##
##          (NO schedule band -> min = standard = max, per 19a)             ##
###############################################################################
cat("=== STEP 6: facility-month rollup ===\n")

fac_month <- ust_tm[, .(
  fac_unmodified = sum(tank_premium, na.rm = TRUE),
  n_tanks_fm     = uniqueN(UST_ID)
), by = .(FACILITY_ID = facility_id_join, YEAR, MONTH)]

fac_month[, mtf := multiple_tank_factor_ga(n_tanks_fm)]
fac_month[, fac_standard := fac_unmodified * mtf]
fac_month[, panel_year := as.integer(YEAR)]
fac_month[, floor_y := min_premium_of_year(panel_year)]

# NO schedule-rating band exists for this filing -> min = standard = max
fac_month[, `:=`(
  min_prem      = pmax(fac_standard, floor_y),
  standard_prem = pmax(fac_standard, floor_y),
  max_prem      = pmax(fac_standard, floor_y)
)]

stopifnot(all(is.finite(fac_month$standard_prem)))
stopifnot(all(fac_month$standard_prem >= fac_month$floor_y))
stopifnot(all(fac_month$min_prem == fac_month$standard_prem))
stopifnot(all(fac_month$standard_prem == fac_month$max_prem))
cat(sprintf("  Facility-month rows: %d\n", nrow(fac_month)))


###############################################################################
## STEP 7 — Facility-year rollup (mean months) -> File 1                    ##
###############################################################################
cat("=== STEP 7: facility-year rollup ===\n")

fac_year <- fac_month[, .(
  min_prem      = mean(min_prem,      na.rm = TRUE),
  standard_prem = mean(standard_prem, na.rm = TRUE),
  max_prem      = mean(max_prem,      na.rm = TRUE),
  n_tanks_rated = as.integer(round(mean(n_tanks_fm)))
), by = .(FACILITY_ID, panel_year)]

stopifnot(all(fac_year$min_prem      <= fac_year$standard_prem + 1e-6))
stopifnot(all(fac_year$standard_prem <= fac_year$max_prem      + 1e-6))
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
## STEP 8 — File 3: Multiple Tank Factor bands (computed from                ##
##          multiple_tank_factor_ga(), not transcribed, to guarantee        ##
##          agreement with the engine). Top band extends 101-200's filed     ##
##          0.700 flat to 9999 — ASSUMPTION, flagged in banner.             ##
###############################################################################
cat("=== STEP 8: Multiple Tank Factor bands ===\n")

band_edges <- data.table(
  n_min = c(1L, 11L, 26L, 51L, 101L),
  n_max = c(10L, 25L, 50L, 100L, 9999L)
)
# credit_mult IS the multiplier directly for GA (multiple_tank_factor_ga()
# already returns the multiplier, unlike AIG/ACE/Zurich's "1 - credit_fraction"
# convention) — no transformation needed.
band_edges[, credit_mult := multiple_tank_factor_ga(n_min)]

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
cat(sprintf("  Multiple Tank Factor bands: %s (%d rows, %d active years x %d bands)\n",
            out3_path, nrow(credit_tbl), length(active_years), nrow(band_edges)))


###############################################################################
## STEP 9 — File 4: minimum premium per active year (flat $225, no era)     ##
###############################################################################
cat("=== STEP 9: minimum premium per year ===\n")

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
cat("=== 19b DONE ===\n")
