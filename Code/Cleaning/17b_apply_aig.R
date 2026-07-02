###############################################################################
# 17b_apply_aig.R — Apply AIG (Commerce & Industry Insurance Company) rate
#                    engine to Texas FR contracts; write the canonical 4-file
#                    output (RATE_ENGINE_BUILD_TARGET.md).
# Filing: SERFF AGNY-130002263 (see 17a_engine_aig.R header for page cites).
# Sources 17a_engine_aig.R for ALL rating functions, the multi-tank credit
# ladder, and the minimum premiums — nothing is re-derived here.
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# BEHAVIORAL ASSUMPTIONS BANNER
# ══════════════════════════════════════════════════════════════════════════════
# CARRIER:        Commerce & Industry Insurance Company (AIG)
# CARRIER_KEY:    "AIG"
# ISSUER_NAME:    SETTLED (Decision A, 2026-07-01): all three AIG-group issuer
#                 strings observed in texas_fr_contract_month_panel.csv are
#                 pooled and priced with this one Commerce & Industry engine —
#                 "CHARTIS SPECIALTY INS CO", "AIG INS CO",
#                 "COMMERCE & INDUSTRY INS CO". 32.4% within-group carrier-
#                 switching means splitting them would fabricate carrier-
#                 switches and poison gamma_p (project_aig_issuer_crosswalk_
#                 pooled memory). File 1 flags every AIG row with
#                 pooled_manual_flag/manual_note (see OUTPUT below) so a
#                 downstream PM08_REAL_ENGINE_ONLY switch can re-fit excluding
#                 pooled-manual carriers.
#
# COVERAGE LIMIT / DEDUCTIBLE: $1,000,000/$1,000,000 occ/agg + $5,000 deductible
#                 held at the filing's reference contract for BOTH File 1 (the
#                 real bill) and File 2 (the card) — per 036-locked invariant 1
#                 and per this task's explicit instruction (actual FR contract
#                 COVER_OCC/COVER_AGG are NOT wired into pricing). LIMIT_DED_FACTOR
#                 = ilf_aig(1e6,1e6) - deductible_credit_aig(5000) is asserted to
#                 equal 1.00 at source time (invariant-1 self-check, see below).
#
# SCHEDULE:       ±40% (SCHED_CAP, filing §V) → min_prem / max_prem envelope
#                 around standard_prem, at facility-year level (File 1 only —
#                 File 2 is schedule-neutral by construction, no band applied).
#
# MULTI-TANK CREDIT: Applied at FACILITY-MONTH level off the actual observed
#                 tank count that month (17a §IV.C ladder, multi_tank_credit_aig()).
#                 Compliance-management (§IV.A, 5%) and remote-monitoring
#                 (§IV.B, 10%) credits are DISCRETIONARY / unobservable →
#                 held at 0, per 17a's own behavioral assumptions.
#
# CONTENTS:       AIG's filing has NO contents/product factor — 0 by construction
#                 (not included in the tank_premium formula at all).
#
# PRIOR ACTS (§VIII): Held at reference (continuous coverage / retro = inception)
#                 → no surcharge, matching 17a.
#
# 2015 FILING SWITCH (NEW eff. 2015-08-01, OLD before):
#   - Per-tank BASE TABLE: uses era_of_date_aig(EFF_DATE) at the TANK-MONTH grain
#     (each contract-month row's own EFF_DATE), so a facility whose contract
#     straddles 2015-08-01 correctly gets OLD-table months before and NEW-table
#     months after. This is exact, not a simplification.
#   - MINIMUM PREMIUM FLOOR (File 1's facility-year floor AND File 4's per-year
#     floor): keyed on calendar `panel_year` only (one value per year, per the
#     File 4 schema — "one positive row per active year"). PROPOSED RULE:
#     panel_year >= 2015 -> $525 (NEW), panel_year < 2015 -> $500 (OLD). This is
#     a SIMPLIFICATION for calendar-year 2015 itself (Jan-Jul 2015 was actually
#     still under the $500 OLD minimum; only Aug-Dec 2015 is truly $525) — the
#     schema has no sub-year granularity to represent the true split. FLAGGED
#     FOR RESEARCHER CONFIRMATION — see return summary. File 1's fac_year floor
#     uses the IDENTICAL rule (same helper function) so Files 1 and 4 never
#     disagree on which floor applied to a given year.
#
# WALL SOURCE FOR CONSTRUCTION BUCKET (FIXED 2026-07-01; is_jacketed OR-term
#   REMOVED 2026-07-01): map_aig_construction_code()'s DW/SW determination
#   (A-bucket "DW" vs everything else) keys OFF THE JOINED mm_wall-DERIVED
#   `wall` (wall == "DW") ALONE — same as every other engine's construction
#   bucket. The raw `double_walled` field is no longer read (nothing else
#   used it).
#   The `is_jacketed_tank==1` OR-term that previously rode alongside
#   wall=="DW" has been REMOVED. It was both wrong and redundant:
#     - REDUNDANT for the normal case: is_jacketed already flows into mm_wall.
#       08_Clean_TX.R:313 (wall_class fcase, Rule 4) maps is_jacketed==TRUE ->
#       "Double" -> double_walled=1 (08_Clean_TX.R:356); 10_Master_Cleaning_
#       and_Harmonization.r:293 then maps double_walled==1 & single_walled==0
#       -> mm_wall="Double-Walled". So a normal jacketed tank already comes
#       back DW in mm_wall; the OR term added nothing.
#     - WRONG at the edge case: a design-single-walled jacketed tank (an
#       earlier 08 rule, e.g. Rule 3 design_sgl, overriding to single ->
#       mm_wall "Single-Walled") is priced by wall=="DW" alone as SW, matching
#       the STATE. With the old OR term it would have been binned into the
#       mm_wall SW cell (STEP 3b card) but priced with the DW construction
#       factor — the exact cell-vs-factor contamination removed from every
#       other engine.
#   is_jacketed_tank is no longer read from raw_pst_ust.csv or referenced in
#   the tank construction-code determination at all (piping's separate
#   pip_jacketed / PIP_MAT_JACKETED flag, used only in map_aig_pipe_
#   construction_code() for the piping DW_PIPING bucket, is unaffected).
#
# RELINED CREDIT (SETTLED 2026-07-01): AIG's A) RELINED -45% credit is NEVER
#   assigned — removed entirely from the mapping. The only candidate raw field
#   (TANK_INT_PROT_DATE / "coated") is semantically wrong for AIG's "relined"
#   category: 0% of SW tanks and only 28% of steel tanks show it, so it is not
#   a valid interior-relining signal (reference_tx_tank_int_prot_date_not_
#   relining memory). Steel tanks that would have hit this branch now route by
#   CP status instead: cathodic-protected -> CATHODIC (-.18); otherwise ->
#   BARE_STEEL (+.10).
#
# ANNUAL_TIGHTNESS (SETTLED 2026-07-01): confirmed by reading the base filing
#   directly (AGNY-130002263, "Rate-Rule Attachments/TX Rate - 2015.pdf", p.1).
#   §II.B item 5 reads "Annual Tank Tightness Test" and is filed under
#   "B.) TANK LEAK DETECTION" — i.e. TANK-level, not piping. §II.D (PIPING LEAK
#   DETECTION) has no tightness-test line at all. The raw file has no distinct
#   tank-level periodic-tightness-test flag, so this bucket stays unmapped and
#   NEVER assigned (falls to NONE, +.05, by default) — the pre-existing
#   default is kept as-is; DET_P_TT_ANNUAL is NOT used here (it is piping-only).
#
# A-D RAW-FIELD MAPPING: See the `map_aig_*` functions in STEP 1 below. EVERY
#   mapping is a PROPOSAL pending researcher confirmation — several AIG filing
#   categories have NO clean 1:1 raw-field source and are folded into a
#   neighboring bucket (flagged inline with "# HITL FLAG:"). Full list also in
#   the return summary. CORR_TANK_CP / CORR_PIPE_CP coding is auto-detected at
#   runtime (Y/N vs categorical C/E/B/N/U — precedent scripts disagree on this;
#   see detect_cp_scheme()) and hard-stops on any unrecognized value so the
#   researcher sees the real server coding on first run.
#
# OUTPUT (4 files, Data/Analysis/rate_engines/):
#   AIG_facility_year_premium.csv  (File 1 — panel_id, panel_year, min_prem,
#     standard_prem, max_prem, carrier, n_tanks_rated — the core 7 columns
#     match RATE_ENGINE_BUILD_TARGET.md exactly, which supersedes the older
#     README §6 schema that TOMICS/Zurich still use — PLUS two AIG-specific
#     machine-readable columns, pooled_manual_flag (1L on every row) and
#     manual_note ("AIG group, C&I manual"), per Decision A above. PM02/PM03/
#     PM08 are untouched — this script only emits the columns.)
#   AIG_cell_era_card.csv          (File 2 — carrier, wall, age_bin, panel_year,
#     premium_usd_per_tank_yr — keyed on calendar year, NOT era.)
#   AIG_multitank_credit.csv       (File 3 — carrier, panel_year, n_min, n_max,
#     credit_mult; bands tile 1..9999 exactly, repeated for every active year.)
#   AIG_min_premium.csv            (File 4 — carrier, panel_year, min_premium_usd.)
# ══════════════════════════════════════════════════════════════════════════════

library(data.table)
library(lubridate)
library(here)

source(here("Code", "Cleaning", "17a_engine_aig.R"))
# inherits: SCHED_CAP, NEW_ERA_CUTOFF, MIN_PREMIUM_NEW, MIN_PREMIUM_OLD,
#           REF_COVER_OCC, REF_COVER_AGG, REF_DEDUCTIBLE
# inherits: era_of_date_aig, base_premium_aig, construction_load_aig,
#           leak_detection_load_aig, pipe_construction_load_aig,
#           pipe_leak_load_aig, multi_tank_credit_aig, deductible_credit_aig,
#           ilf_aig, aig_facility_premium

# ── Logging ──────────────────────────────────────────────────────────────────
.log_path <- here::here("logs", paste0(
  "17b_apply_aig_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 17b_apply_aig.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== 17b_apply_aig: AIG premium apply ===\n")
cat(sprintf("Run started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

CARRIER_KEY <- "AIG"

# SETTLED (Decision A, 2026-07-01): all three AIG-group issuer strings are
# pooled and priced with the Commerce & Industry engine.
ISSUER_MATCH <- c("CHARTIS SPECIALTY INS CO", "AIG INS CO", "COMMERCE & INDUSTRY INS CO")

out_dir <- here("Data", "Analysis", "rate_engines")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── Invariant-1 self-check: reference contract collapses the limit/deductible
#    factor to a no-op multiplier (036-locked $1M/$1M, $5k deductible) ────────
LIMIT_DED_FACTOR <- ilf_aig(REF_COVER_OCC, REF_COVER_AGG) - deductible_credit_aig(REF_DEDUCTIBLE)
stopifnot(isTRUE(all.equal(LIMIT_DED_FACTOR, 1.00, tolerance = 1e-8)))
cat(sprintf("  Invariant-1 check: LIMIT_DED_FACTOR at reference contract = %.4f (OK)\n",
            LIMIT_DED_FACTOR))

# Filing-minimum-premium lookup keyed on calendar panel_year (shared by File 1's
# facility-year floor and File 4) — see "2015 FILING SWITCH" banner note above.
min_premium_of_year <- function(panel_year) {
  fifelse(panel_year >= 2015L, MIN_PREMIUM_NEW, MIN_PREMIUM_OLD)
}


###############################################################################
## STEP 1 — Build tank attributes from raw_pst_ust.csv                       ##
##          (12_Rate_MidCont Section A recipe; mirrors 15b/16b Step 1)        ##
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

# double_walled: NOT read here (2026-07-01 fix) — the DW/SW determination for
# map_aig_construction_code() now comes from the joined mm_wall-derived `wall`
# (see mm_wall JOIN below), not this raw field, and nothing else in this script
# needs it. (Docs/Texas_Data_Characteristics.md's "MIXED-CODING TRAP" — raw
# TANK_SINGLE/TANK_DOUBLE are TRUE/FALSE in this file, NOT "Y"/"N" — still
# appears to be live in 15b_apply_tomics.R's Step 1 via `TANK_DOUBLE == "Y"`;
# out of scope to fix here, flagged in the return summary for awareness.)
needed_raw <- c(
  "UST_ID", "TANK_ID", "INSTALL_DATE", "end_date",
  "is_fiberglass_tank", "is_composite_tank", "is_steel_tank",
  "TANK_MAT_COATED", "CORR_TANK_CP",
  "PIP_DOUBLE", "PIP_MAT_FRP", "PIP_MAT_STEEL", "PIP_MAT_JACKETED", "PIP_MAT_FLEX",
  "CORR_PIPE_CP", "PIPE_TYPE",
  "DET_C_INTERSTITIAL", "DET_C_ATG", "DET_C_GW", "DET_C_VAPOR", "DET_C_SIR",
  "DET_P_INTERSTITIAL", "DET_P_LLD", "DET_P_GW", "DET_P_VAPOR", "DET_P_SUCTION_EXEMPT"
)
missing_flds <- setdiff(needed_raw, actual_names)
if (length(missing_flds) > 0L) {
  cat("  STOP: fields not found in raw_pst_ust.csv:\n")
  cat(paste(" ", missing_flds, collapse = "\n"), "\n")
  stop("Fix the column names above (server column names may differ) and re-run.")
}

raw_ust <- fread(raw_path, select = c(id_col, needed_raw))

raw_ust[, FACILITY_ID := trimws(substr(get(id_col), 1L, 6L))]
if (id_col != "FACILITY_ID") raw_ust[, (id_col) := NULL]

raw_ust[, UST_ID       := toupper(trimws(UST_ID))]
raw_ust[, INSTALL_DATE := ymd(INSTALL_DATE)]
raw_ust[, CLOSED_DATE  := ymd(end_date)]
raw_ust[, end_date     := NULL]

# ── mm_wall JOIN (panel_dt.csv, TX) — SINGLE source of truth for the CARD's
#    wall dimension (researcher 2026-07-01). ALSO the SOLE double/single input
#    to AIG's own construction_code bucket below (map_aig_construction_code,
#    fixed 2026-07-01 — see "WALL SOURCE FOR CONSTRUCTION BUCKET" banner note),
#    so a tank's DW/SW rating bucket always agrees with its card cell. The
#    is_jacketed OR-term formerly ORed alongside wall=="DW" was removed
#    2026-07-01 (see banner) — jacketing is already folded into mm_wall
#    upstream. ──────────────────────────────────────────────────────────────
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

# ── CORR_TANK_CP / CORR_PIPE_CP coding auto-detect ───────────────────────────
# HITL FLAG: 12_Rate_MidCont_2006_2011.R §A treats CORR_TANK_CP as "Y"/"N";
# 16a_engine_zurich.R's build notes treat it as categorical {"C","E","B"}=
# protected / {"N","U",""}=unprotected. The two precedent scripts DISAGREE on
# the coding. Auto-detect from the actual values and hard-stop if neither
# scheme matches, so the researcher sees the real coding on first run rather
# than silently mis-classifying every tank's cathodic-protection status.
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
cp_scheme_pipe <- detect_cp_scheme(raw_ust$CORR_PIPE_CP, "CORR_PIPE_CP")
cat(sprintf("  CORR_TANK_CP coding detected: %s | CORR_PIPE_CP coding detected: %s\n",
            cp_scheme_tank, cp_scheme_pipe))

cp_flag <- function(x, scheme) {
  # scheme is a scalar (one detected coding per column) branching to a
  # vectorized (length(x)) computation — fcase() requires condition and value
  # vectors of matching length, so a scalar-condition/vector-value mix errors;
  # plain if/else is correct here.
  if (scheme == "YN")    return(!is.na(x) & x == "Y")
  if (scheme == "CEBNU") return(!is.na(x) & x %chin% c("C", "E", "B"))
  rep(FALSE, length(x))
}
raw_ust[, has_cp     := cp_flag(CORR_TANK_CP, cp_scheme_tank)]
raw_ust[, pip_has_cp := cp_flag(CORR_PIPE_CP, cp_scheme_pipe)]

# ── Normalize boolean/Y-N inputs to 0/1 (no NA) ──────────────────────────────
raw_ust[, is_fiberglass_tank := fifelse(is.na(is_fiberglass_tank), 0L, as.integer(as.logical(is_fiberglass_tank)))]
raw_ust[, is_composite_tank  := fifelse(is.na(is_composite_tank), 0L, as.integer(as.logical(is_composite_tank)))]
raw_ust[, is_steel_tank      := fifelse(is.na(is_steel_tank), 0L, as.integer(as.logical(is_steel_tank)))]
raw_ust[, is_coated_tank     := fifelse(!is.na(TANK_MAT_COATED) & TANK_MAT_COATED == "Y", 1L, 0L)]
raw_ust[, pip_double         := fifelse(!is.na(PIP_DOUBLE)       & PIP_DOUBLE       == "Y", 1L, 0L)]
raw_ust[, pip_frp            := fifelse(!is.na(PIP_MAT_FRP)      & PIP_MAT_FRP      == "Y", 1L, 0L)]
raw_ust[, pip_steel          := fifelse(!is.na(PIP_MAT_STEEL)    & PIP_MAT_STEEL    == "Y", 1L, 0L)]
raw_ust[, pip_jacketed       := fifelse(!is.na(PIP_MAT_JACKETED) & PIP_MAT_JACKETED == "Y", 1L, 0L)]
raw_ust[, pip_flex           := fifelse(!is.na(PIP_MAT_FLEX)     & PIP_MAT_FLEX     == "Y", 1L, 0L)]
raw_ust[, det_c_interstitial := fifelse(!is.na(DET_C_INTERSTITIAL) & DET_C_INTERSTITIAL == "Y", 1L, 0L)]
raw_ust[, det_c_atg          := fifelse(!is.na(DET_C_ATG)          & DET_C_ATG          == "Y", 1L, 0L)]
raw_ust[, det_c_gw           := fifelse(!is.na(DET_C_GW)           & DET_C_GW           == "Y", 1L, 0L)]
raw_ust[, det_c_vapor        := fifelse(!is.na(DET_C_VAPOR)        & DET_C_VAPOR        == "Y", 1L, 0L)]
raw_ust[, det_c_sir          := fifelse(!is.na(DET_C_SIR)          & DET_C_SIR          == "Y", 1L, 0L)]
raw_ust[, det_p_interstitial := fifelse(!is.na(DET_P_INTERSTITIAL) & DET_P_INTERSTITIAL == "Y", 1L, 0L)]
raw_ust[, det_p_lld          := fifelse(!is.na(DET_P_LLD)          & DET_P_LLD          == "Y", 1L, 0L)]
raw_ust[, det_p_gw           := fifelse(!is.na(DET_P_GW)           & DET_P_GW           == "Y", 1L, 0L)]
raw_ust[, det_p_vapor        := fifelse(!is.na(DET_P_VAPOR)        & DET_P_VAPOR        == "Y", 1L, 0L)]
raw_ust[, det_p_suction_exempt := fifelse(!is.na(DET_P_SUCTION_EXEMPT) & DET_P_SUCTION_EXEMPT == "Y", 1L, 0L)]
raw_ust[, PIPE_TYPE := trimws(PIPE_TYPE)]

# ── A) Tank construction — PROPOSED mapping, HITL FLAGS inline ──────────────
# AIG select-one categories: DW(-.50) STP3(-.30) FIBERGLASS_CLAD(-.20)
# CATHODIC(-.18) BARE_STEEL(+.10) RELINED(-.45, NEVER ASSIGNED — see banner
# "RELINED CREDIT" note above; dropped entirely, SETTLED 2026-07-01). Priority
# = most-protective-flag-wins (raw tanks often have >1 material flag true
# simultaneously).
#   DW              <- wall=="DW" ALONE (mm_wall-derived, joined from
#                       panel_dt.csv, fixed 2026-07-01 — see "WALL SOURCE FOR
#                       CONSTRUCTION BUCKET" banner note). The is_jacketed_
#                       tank==1 OR-term was REMOVED 2026-07-01 (see banner):
#                       jacketing is already folded into mm_wall upstream via
#                       08_Clean_TX.R's wall_class Rule 4 -> double_walled ->
#                       10_Master_Cleaning_and_Harmonization.r's mm_wall, so
#                       the OR term was redundant for normal jacketed tanks
#                       and a cell-vs-factor mismatch source for design-single
#                       jacketed tanks (mm_wall SW, would have priced DW).
#   STP3             NO CLEAN RAW SOURCE — raw data cannot distinguish a
#                       STI-P3-certified steel tank from a generically
#                       cathodically-protected steel tank (both present as
#                       steel + has_cp). STP3's larger -.30 credit is NEVER
#                       assigned; all steel+CP tanks fall to CATHODIC (-.18)
#                       below. [HITL FLAG — confirm this conservative default]
#   FIBERGLASS_CLAD <- is_composite_tank==1 OR is_fiberglass_tank==1
#                       [HITL FLAG: bucket-merges pure FRP tanks (fiberglass
#                        throughout) with composite tanks (steel core clad in
#                        fiberglass) into AIG's one "Fiberglass/Clad" line —
#                        cannot separately verify against the filing PDF text
#                        without the researcher reading the actual category
#                        definition]
#   CATHODIC        <- is_steel_tank==1 & has_cp==TRUE   [see STP3 flag above]
#   BARE_STEEL      <- default catch-all (steel w/ no protection whether or
#                       not coated, OR TANK_MAT_CONCRETE / genuinely unknown
#                       material) — steel tanks that would have hit the now-
#                       dropped RELINED branch (steel + coated + !has_cp) fall
#                       through to this bucket.
#                       [HITL FLAG: concrete/unknown-material tanks have no
#                        AIG line at all; defaulted to BARE_STEEL (no credit,
#                        the actuarially conservative choice) rather than
#                        erroring — confirm this is acceptable]
map_aig_construction_code <- function(wall, composite, frp, steel, has_cp) {
  fcase(
    wall == "DW",                  "DW",
    composite == 1L | frp == 1L,   "FIBERGLASS_CLAD",
    steel == 1L & has_cp,          "CATHODIC",
    default = "BARE_STEEL"
  )
}
raw_ust[, construction_code := map_aig_construction_code(
  wall, is_composite_tank, is_fiberglass_tank,
  is_steel_tank, has_cp)]

# ── B) Tank leak detection — PROPOSED mapping, HITL FLAGS inline ────────────
# AIG select-one categories: INTERSTITIAL(-.05) ATG(-.05) MONITORING_WELLS(-.02)
# SIR(+.00) ANNUAL_TIGHTNESS(+.00) NONE(+.05). Uses DET_C_* (compartment/tank-
# level) fields only — DET_P_* are reserved for piping leak detection (D).
#   MONITORING_WELLS <- det_c_gw==1 OR det_c_vapor==1
#                        [HITL FLAG: groundwater monitoring wells and soil-vapor
#                         monitoring are folded into one bucket; both are
#                         external well-based methods but the filing may
#                         intend "Monitoring Wells" as groundwater-only]
#   ANNUAL_TIGHTNESS  SETTLED (2026-07-01), confirmed by reading the filing
#                        directly: §II.B item 5 "Annual Tank Tightness Test" is
#                        filed under "B.) TANK LEAK DETECTION" — TANK-level,
#                        not piping (see banner note above). DET_P_TT_ANNUAL is
#                        piping-only and NOT used here. Raw file has no distinct
#                        tank-level periodic-tightness-test flag, so this
#                        bucket stays unmapped and NEVER assigned (falls to
#                        NONE, +.05, by default — unchanged from before).
map_aig_leak_code <- function(interstitial, atg, gw, vapor, sir) {
  fcase(
    interstitial == 1L,       "INTERSTITIAL",
    atg == 1L,                 "ATG",
    gw == 1L | vapor == 1L,    "MONITORING_WELLS",
    sir == 1L,                 "SIR",
    default = "NONE"
  )
}
raw_ust[, leak_code := map_aig_leak_code(det_c_interstitial, det_c_atg, det_c_gw, det_c_vapor, det_c_sir)]

# ── C) Piping construction — PROPOSED mapping, HITL FLAGS inline ───────────
# AIG select-one categories: DW_PIPING(-.10) FIBERGLASS_PIPING(-.05)
# EXTERNAL_COATING(-.02) CATHODIC_PIPING(-.02) BARE_STEEL_PIPING(+.10).
#   DW_PIPING         <- pip_double==1 OR pip_jacketed==1
#   FIBERGLASS_PIPING <- pip_frp==1 OR pip_flex==1
#                        [HITL FLAG: flex piping (PIP_MAT_FLEX) is typically a
#                         flexible composite/fiberglass product; folded here
#                         as a proxy]
#   EXTERNAL_COATING  NOT SEPARATELY IDENTIFIABLE from CATHODIC_PIPING — both
#                        credit an identical -.02, so raw CORR_PIPE_CP cannot
#                        (and does not need to) distinguish them; all
#                        pipe+CP cases are labeled CATHODIC_PIPING below.
#                        [HITL FLAG — numerically inconsequential, flagged for
#                         completeness only]
map_aig_pipe_construction_code <- function(pip_double, pip_jacketed, pip_frp, pip_flex, pip_steel, pip_has_cp) {
  fcase(
    pip_double == 1L | pip_jacketed == 1L, "DW_PIPING",
    pip_frp == 1L | pip_flex == 1L,        "FIBERGLASS_PIPING",
    pip_steel == 1L & pip_has_cp,          "CATHODIC_PIPING",
    default = "BARE_STEEL_PIPING"
  )
}
raw_ust[, pipe_construction_code := map_aig_pipe_construction_code(
  pip_double, pip_jacketed, pip_frp, pip_flex, pip_steel, pip_has_cp)]

# ── D) Piping leak detection — PROPOSED mapping, HITL FLAGS inline ─────────
# AIG select-one categories: INTERSTITIAL(-.05) ELECTRONIC_SHUTOFF(-.05)
# EXTERNAL_WELLS(-.02) MECHANICAL_LLD(-.02) SUCTION_CHECK_VALVE(-.02) NONE(+.05).
#   ELECTRONIC_SHUTOFF vs MECHANICAL_LLD: raw DET_P_LLD ("line leak detector")
#     cannot distinguish an electronic-shutoff LLD from a purely mechanical
#     one — these credit DIFFERENTLY (-.05 vs -.02), so this ambiguity IS
#     numerically material. PROPOSED: default DET_P_LLD==1 to the smaller,
#     conservative MECHANICAL_LLD credit. [HITL FLAG — confirm, or supply a
#     field that distinguishes electronic from mechanical LLDs]
#   SUCTION_CHECK_VALVE <- PIPE_TYPE=="Suction" OR det_p_suction_exempt==1
#                        [HITL FLAG: assumes every suction-type line has a
#                         functioning check valve meeting AIG's "safe suction"
#                         definition — raw data has no separate certification
#                         flag for that]
map_aig_pipe_leak_code <- function(interstitial, lld, gw, vapor, pipe_type, suction_exempt) {
  fcase(
    interstitial == 1L,                          "INTERSTITIAL",
    lld == 1L,                                    "MECHANICAL_LLD",
    gw == 1L | vapor == 1L,                       "EXTERNAL_WELLS",
    pipe_type == "Suction" | suction_exempt == 1L, "SUCTION_CHECK_VALVE",
    default = "NONE"
  )
}
raw_ust[, pipe_leak_code := map_aig_pipe_leak_code(
  det_p_interstitial, det_p_lld, det_p_gw, det_p_vapor, PIPE_TYPE, det_p_suction_exempt)]

cat(sprintf("  Raw tanks loaded: %d | facilities: %d\n",
            nrow(raw_ust), uniqueN(raw_ust$FACILITY_ID)))
cat("  construction_code distribution (A):\n");        print(raw_ust[, .N, by = construction_code][order(-N)])
cat("  leak_code distribution (B):\n");                 print(raw_ust[, .N, by = leak_code][order(-N)])
cat("  pipe_construction_code distribution (C):\n");     print(raw_ust[, .N, by = pipe_construction_code][order(-N)])
cat("  pipe_leak_code distribution (D):\n");             print(raw_ust[, .N, by = pipe_leak_code][order(-N)])


###############################################################################
## STEP 2 — AIG contracts: filter + cartesian tank x contract-month          ##
###############################################################################

cat("=== STEP 2: AIG contracts ===\n")

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
cat(sprintf("  AIG contracts: %d rows | %d facilities | years %d-%d\n",
            nrow(fa_contracts), uniqueN(fa_contracts$FACILITY_ID),
            min(fa_contracts$YEAR), max(fa_contracts$YEAR)))

# QA: how often does the actual FR contract limit deviate from the $1M/$1M
# reference we hold coverage fixed at (invariant 1 / task instruction)?
cat(sprintf("  COVER_OCC == $1M in %.1f%% of contract-months (reference held fixed regardless)\n",
            100 * mean(fa_contracts$COVER_OCC == 1e6, na.rm = TRUE)))

# Subset tank table to AIG facilities only
aig_facs <- unique(fa_contracts$FACILITY_ID)
tank_sub <- raw_ust[FACILITY_ID %in% aig_facs]
cat(sprintf("  Tanks at AIG facilities: %d\n", nrow(tank_sub)))

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

# Filing era from THIS row's own EFF_DATE (exact — handles the 2015-08-01
# mid-year base-table switch at the tank-month grain; see banner)
ust_tm[, filing_era := era_of_date_aig(EFF_DATE)]

cat(sprintf("  Tank-months (in-service): %d\n", nrow(ust_tm)))
stopifnot(nrow(ust_tm) > 0L)
cat("  filing_era distribution (tank-months):\n"); print(ust_tm[, .N, by = filing_era])


###############################################################################
## STEP 3 — Price each tank-month (§II tank_unmodified — NO credit/band/floor)##
###############################################################################

cat("=== STEP 3: price tank-months ===\n")

ust_tm[, `:=`(
  base_rate          = base_premium_aig(age_years, filing_era),
  construction_load   = construction_load_aig(construction_code),
  leak_load           = leak_detection_load_aig(leak_code),
  pipe_construction_load = pipe_construction_load_aig(pipe_construction_code),
  pipe_leak_load       = pipe_leak_load_aig(pipe_leak_code)
)]
# Contents: AIG has NO contents factor (0 by construction — not a term here).
ust_tm[, tank_char_load := construction_load + leak_load + pipe_construction_load + pipe_leak_load]
ust_tm[, tank_premium   := base_rate * (1 + tank_char_load)]

stopifnot(all(is.finite(ust_tm$tank_premium)))
stopifnot(all(ust_tm$tank_premium >= 0))

cat(sprintf("  tank_premium (tank_unmodified): mean=$%.0f  min=$%.0f  max=$%.0f\n",
            mean(ust_tm$tank_premium), min(ust_tm$tank_premium), max(ust_tm$tank_premium)))


###############################################################################
## STEP 3b — File 2: cell-year card (structural kernel — no credit/floor)    ##
###############################################################################
cat("=== STEP 3b: build cell-year card ===\n")

# Canonical cross-carrier wall dimension — carried through from raw_ust's
# mm_wall panel join (STEP 1). Same `wall` now also SOLELY drives the DW/SW
# term of the AIG-specific construction_code bucket above (fixed 2026-07-01;
# is_jacketed OR-term removed 2026-07-01), so a tank's card cell and its AIG
# rating bucket never disagree on wall.
ust_tm[, age_bin := as.integer(cut(age_years,
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
print(cell_card[order(wall, age_bin, panel_year)][1:min(.N, 10)])


###############################################################################
## STEP 4 — Facility-month rollup: sum tanks, apply multi-tank credit        ##
###############################################################################
cat("=== STEP 4: facility-month rollup ===\n")

fac_month <- ust_tm[, .(
  fac_unmodified = sum(tank_premium, na.rm = TRUE),
  n_tanks_fm     = uniqueN(UST_ID)
), by = .(FACILITY_ID, YEAR, MONTH)]

# §IV: compliance-mgmt + remote-monitoring held at 0 (unobservable, per 17a);
# multi-tank credit keyed on the facility-month's ACTUAL observed tank count.
fac_month[, iv_credit := multi_tank_credit_aig(n_tanks_fm)]
fac_month[, fac_credited := fac_unmodified * (1 - iv_credit) * LIMIT_DED_FACTOR]

stopifnot(all(is.finite(fac_month$fac_credited)))
cat(sprintf("  Facility-month rows: %d\n", nrow(fac_month)))


###############################################################################
## STEP 5 — Facility-year rollup (mean months) + band + floor -> File 1      ##
###############################################################################
cat("=== STEP 5: facility-year rollup, band, floor ===\n")

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

# Pooled-manual-rate flag (Decision A): every AIG row is priced off the ONE
# pooled Commerce & Industry manual, not a carrier-specific filing for each of
# the three AIG-group issuer strings — machine-readable so a downstream
# PM08_REAL_ENGINE_ONLY switch can re-fit excluding pooled-manual carriers.
# PM02/PM03/PM08 are not touched here; this script only emits the columns.
fac_year[, pooled_manual_flag := 1L]
fac_year[, manual_note        := "AIG group, C&I manual"]

out1 <- fac_year[, .(panel_id, panel_year, min_prem, standard_prem, max_prem,
                      carrier, n_tanks_rated, pooled_manual_flag, manual_note)]
setorder(out1, panel_id, panel_year)

stopifnot(identical(names(out1), c("panel_id", "panel_year", "min_prem", "standard_prem",
                "max_prem", "carrier", "n_tanks_rated", "pooled_manual_flag", "manual_note")))
stopifnot(all(out1$pooled_manual_flag == 1L))
stopifnot(all(out1$manual_note == "AIG group, C&I manual"))

out1_path <- file.path(out_dir, paste0(CARRIER_KEY, "_facility_year_premium.csv"))
fwrite(out1, out1_path)
cat(sprintf("  Facility-year rows: %d | facilities: %d | years: %d-%d\n",
            nrow(out1), uniqueN(out1$panel_id), min(out1$panel_year), max(out1$panel_year)))
cat(sprintf("  Saved: %s\n", out1_path))
cat("  Mean standard_prem by panel_year:\n")
print(out1[, .(N = .N, mean_std = round(mean(standard_prem), 2)), by = panel_year][order(panel_year)])


###############################################################################
## STEP 6 — File 3: multi-tank credit bands (from 17a §IV.C, computed not    ##
##          transcribed, to guarantee agreement with the engine)             ##
###############################################################################
cat("=== STEP 6: multi-tank credit bands ===\n")

band_edges <- data.table(
  n_min = c(1L, 6L, 15L, 30L, 50L, 75L, 100L, 150L),
  n_max = c(5L, 14L, 29L, 49L, 74L, 99L, 149L, 9999L)
)
band_edges[, credit_mult := 1 - multi_tank_credit_aig(n_min)]

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
## STEP 7 — File 4: minimum premium per active year                         ##
###############################################################################
cat("=== STEP 7: minimum premium per year ===\n")

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
cat("=== 17b DONE ===\n")
