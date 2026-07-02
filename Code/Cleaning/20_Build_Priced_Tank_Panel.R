###############################################################################
# 20_Build_Priced_Tank_Panel.R — ONE shared priced-tank panel (Ticket 053)
#
# LAYER 1 of the engine refactor: assembles the canonical per-(tank, year,
# carrier) input panel ONCE, joins debugged once, so every carrier's b-apply
# script becomes "read this file -> filter carrier -> price -> roll up ->
# write 4 files" with NO raw/panel/compartment/FR plumbing of its own.
#
# Output: Data/Analysis/rate_engines/_priced_tank_panel.csv
# Grain:  one row per (tank, panel_year, carrier) that a priceable carrier
#         actually insured that year (FR coverage joined onto panel_dt's own
#         active tank-year rows — panel_dt's presence/absence of a
#         (tank_panel_id, panel_year) row already IS the install/close
#         window, so this is a key-matched merge, never a tank x all-months
#         cartesian product).
#
# Does NOT touch PM02/PM03/PM08. Does NOT implement the
# pmax(credit(N) x card, floor) billed-premium reconstruction — that is
# PM03's job (RATE_ENGINE_BUILD_TARGET.md "What the builder does NOT do").
###############################################################################

library(data.table)
library(lubridate)
library(here)

source(here("Code", "Cleaning", "issuer_crosswalk.R"))

# ── Logging ──────────────────────────────────────────────────────────────────
.log_path <- here::here("logs", paste0(
  "20_Build_Priced_Tank_Panel_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 20_Build_Priced_Tank_Panel.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== 20_Build_Priced_Tank_Panel: shared priced-tank panel builder ===\n")
cat(sprintf("Run started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

out_dir  <- here("Data", "Analysis", "rate_engines")
out_path <- file.path(out_dir, "_priced_tank_panel.csv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

MIN_INSURED_MATCH_RATE <- 0.70   # guard threshold — see check_insured_match_rate()

###############################################################################
## Helper functions (defined + self-tested BEFORE any data load, so both    ##
## guards are exercised on synthetic fixtures, not only the real-data path) ##
###############################################################################

# ── CORR_TANK_CP / CORR_PIPE_CP coding auto-detect ───────────────────────────
# Precedent scripts disagreed on this coding (12_Rate_MidCont: Y/N;
# 16a Zurich build notes: categorical C/E/B/N/U). Auto-detect from the actual
# values and hard-stop if neither scheme matches, so a coding change on the
# server surfaces immediately instead of silently mis-classifying every
# tank's cathodic-protection status. Centralized here (was duplicated
# verbatim in 17b and 19b) — debugged once, every carrier inherits it.
detect_cp_scheme <- function(x, field_name) {
  vals <- unique(x[!is.na(x) & x != ""])
  if (length(vals) == 0L) {
    cat(sprintf("  %s: no non-missing values observed — defaulting has_cp=FALSE for all\n", field_name))
    return("NONE")
  }
  if (all(vals %chin% c("Y", "N"))) return("YN")
  if (all(vals %chin% c("C", "E", "B", "N", "U"))) return("CEBNU")
  stop(sprintf(
    "detect_cp_scheme: %s unrecognized value set {%s} — matches neither the Y/N scheme nor the C/E/B/N/U scheme. STOP and confirm the real coding with the researcher before proceeding.",
    field_name, paste(vals, collapse = ", ")))
}

cp_flag <- function(x, scheme) {
  if (scheme == "YN")    return(!is.na(x) & x == "Y")
  if (scheme == "CEBNU") return(!is.na(x) & x %chin% c("C", "E", "B"))
  rep(FALSE, length(x))
}

# ── Insured-tank match-rate guard ────────────────────────────────────────────
# Gate on the fraction of (facility, year, carrier) FR-insured combinations
# that found at least one matching panel_dt tank-year row. A LOW rate means
# the raw<->panel_dt join key is broken (the exact bug class this ticket
# exists to prevent) and the panel should not be trusted; a moderately-below-
# 100% rate is expected (panel_dt is a restricted analysis sample, not a full
# census — see 19b's documented "SAMPLE COVERAGE" risk).
check_insured_match_rate <- function(n_matched, n_total, min_rate = MIN_INSURED_MATCH_RATE) {
  rate <- if (n_total == 0L) NA_real_ else n_matched / n_total
  if (is.na(rate) || rate <= min_rate) {
    stop(sprintf(
      "check_insured_match_rate: insured-tank match rate %.1f%% (%d / %d) is at or below the %.0f%% floor — the raw<->panel_dt join key may be broken. STOP and investigate before trusting the shared panel.",
      100 * ifelse(is.na(rate), 0, rate), n_matched, n_total, 100 * min_rate))
  }
  rate
}

cat("=== Guard self-test (synthetic fixtures, no real data) ===\n")

# Guard 1 (issuer crosswalk hard-stop) is self-tested in issuer_crosswalk.R,
# sourced above — its self-test already ran and printed PASS.

# Guard 2 (match-rate) — good case passes and returns the rate
r_good <- check_insured_match_rate(90L, 100L)
stopifnot(isTRUE(all.equal(r_good, 0.90)))

# Guard 2 — deliberately-broken-key case (10% match) hard-stops
err_mr <- tryCatch(check_insured_match_rate(10L, 100L), error = function(e) e)
stopifnot(inherits(err_mr, "error"))
stopifnot(grepl("match rate", conditionMessage(err_mr)))

# Guard 2 — exactly-at-the-floor case (70%) also hard-stops (strict >, not >=)
err_mr2 <- tryCatch(check_insured_match_rate(70L, 100L), error = function(e) e)
stopifnot(inherits(err_mr2, "error"))

cat("  check_insured_match_rate: good-case pass + broken-key hard-stop + at-floor hard-stop OK\n")
cat("Guard self-test PASS\n\n")


###############################################################################
## STEP 1 — Load raw tank census, derive the UNION of finer attrs every     ##
##          engine's rate tables need. ONE load, ONE derivation, verified   ##
##          against the real server schema (NOT the stale column names     ##
##          several existing per-engine scripts assumed).                  ##
###############################################################################
cat("=== STEP 1: load raw_pst_ust.csv, derive finer attrs ===\n")

raw_path <- here("Data", "Raw", "state_databases", "Texas", "raw_pst_ust.csv")
if (!file.exists(raw_path))
  stop("raw_pst_ust.csv not found at: ", raw_path,
       " — do NOT fall back to texas_static_tank_details.csv, it is empty on the server")

actual_names <- names(fread(raw_path, nrows = 0L))

needed_raw <- c(
  "FACILITY_ID", "TANK_ID", "UST_ID",
  "install_date", "end_date", "STATUS", "CAPACITY",
  "is_fiberglass_tank", "is_composite_tank", "is_steel_tank", "is_jacketed_tank",
  "CORR_TANK_CP", "CORR_PIPE_CP",
  "PIP_DOUBLE", "PIP_MAT_FRP", "PIP_MAT_STEEL", "PIP_MAT_JACKETED", "PIP_MAT_FLEX",
  "PIPE_TYPE",
  "DET_C_INTERSTITIAL", "DET_C_ATG", "DET_C_GW", "DET_C_VAPOR", "DET_C_SIR", "DET_C_SEC_CONT",
  "DET_P_INTERSTITIAL", "DET_P_LLD", "DET_P_GW", "DET_P_VAPOR", "DET_P_SUCTION_EXEMPT",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel"
)
missing_flds <- setdiff(needed_raw, actual_names)
if (length(missing_flds) > 0L) {
  cat("  STOP: fields not found in raw_pst_ust.csv:\n")
  cat(paste(" ", missing_flds, collapse = "\n"), "\n")
  stop("Fix the column names above (server column names may differ from this list) and re-run.")
}

raw_ust <- fread(raw_path, select = needed_raw)

raw_ust[, FACILITY_ID := trimws(as.character(FACILITY_ID))]
raw_ust[, TANK_ID      := trimws(as.character(TANK_ID))]
raw_ust[, UST_ID       := toupper(trimws(as.character(UST_ID)))]
raw_ust[, install_date := ymd(install_date)]
raw_ust[, end_date     := ymd(end_date)]
raw_ust[, STATUS       := toupper(trimws(STATUS))]

cat(sprintf("  raw_pst_ust: %s tanks | %s facilities\n",
            format(nrow(raw_ust), big.mark = ","),
            format(uniqueN(raw_ust$FACILITY_ID), big.mark = ",")))

# tank_status — canonical labels (04a's derivation, mirrored here so
# 21b/MidCont and any future consumer share one status vocabulary)
raw_ust[, tank_status := fcase(
  STATUS == "IN USE",                                             "In Use",
  STATUS == "TEMP OUT OF SERVICE",                                "Temporarily Out of Use",
  STATUS %chin% c("REMOVED FROM GROUND", "PERM FILLED IN PLACE"), "Permanently Out of Use",
  default = "Unknown")]

# Facility-wide earliest install (incl. removed tanks — "captures pit
# history"), needed by TOMICS's New/Replace/Upgrade categorization. Computed
# from the FULL raw census, not the panel_dt-restricted sample, matching the
# original 15b design intent.
fac_min <- raw_ust[!is.na(install_date), .(facility_min_install = min(install_date, na.rm = TRUE)),
                    by = FACILITY_ID]
raw_ust <- fac_min[raw_ust, on = "FACILITY_ID"]

# CORR_TANK_CP / CORR_PIPE_CP — auto-detected coding scheme, applied ONCE
cp_scheme_tank <- detect_cp_scheme(raw_ust$CORR_TANK_CP, "CORR_TANK_CP")
cp_scheme_pipe <- detect_cp_scheme(raw_ust$CORR_PIPE_CP, "CORR_PIPE_CP")
cat(sprintf("  CORR_TANK_CP coding: %s | CORR_PIPE_CP coding: %s\n", cp_scheme_tank, cp_scheme_pipe))
raw_ust[, has_cp     := cp_flag(CORR_TANK_CP, cp_scheme_tank)]
raw_ust[, pip_has_cp := cp_flag(CORR_PIPE_CP, cp_scheme_pipe)]

# Normalize boolean/Y-N inputs to clean 0/1 (no NA)
yn01 <- function(x) fifelse(is.na(x), 0L, as.integer(as.logical(x)))
raw_ust[, `:=`(
  is_fiberglass_tank = yn01(is_fiberglass_tank),
  is_composite_tank  = yn01(is_composite_tank),
  is_steel_tank      = yn01(is_steel_tank),
  is_jacketed_tank   = yn01(is_jacketed_tank),
  is_gasoline        = yn01(is_gasoline),
  is_diesel          = yn01(is_diesel),
  is_oil_kerosene    = yn01(is_oil_kerosene),
  is_jet_fuel        = yn01(is_jet_fuel)
)]
y01 <- function(x) fifelse(!is.na(x) & x == "Y", 1L, 0L)
raw_ust[, `:=`(
  pip_double          = y01(PIP_DOUBLE),
  pip_frp             = y01(PIP_MAT_FRP),
  pip_steel           = y01(PIP_MAT_STEEL),
  pip_jacketed        = y01(PIP_MAT_JACKETED),
  pip_flex            = y01(PIP_MAT_FLEX),
  det_c_interstitial  = y01(DET_C_INTERSTITIAL),
  det_c_atg           = y01(DET_C_ATG),
  det_c_gw            = y01(DET_C_GW),
  det_c_vapor         = y01(DET_C_VAPOR),
  det_c_sir           = y01(DET_C_SIR),
  det_c_sec_cont      = y01(DET_C_SEC_CONT),
  det_p_interstitial  = y01(DET_P_INTERSTITIAL),
  det_p_lld           = y01(DET_P_LLD),
  det_p_gw            = y01(DET_P_GW),
  det_p_vapor         = y01(DET_P_VAPOR),
  det_p_suction_exempt = y01(DET_P_SUCTION_EXEMPT)
)]
raw_ust[, pipe_type := trimws(PIPE_TYPE)]
raw_ust[, has_cp     := as.integer(has_cp)]
raw_ust[, pip_has_cp := as.integer(pip_has_cp)]

# tank_panel_id — the CONFIRMED join key (verified 2026-07-02 against real
# server data: raw TANK_ID values match panel_dt's tank_panel_id suffix
# exactly, e.g. facility 43650's TANK_ID "56A"/"1037D" <-> tank_panel_id
# "43650_TX_56A"/"43650_TX_1037D". NOT UST_ID — the ticket's own fallback
# guess was wrong.
raw_ust[, tank_panel_id := paste(FACILITY_ID, "TX", TANK_ID, sep = "_")]

finer_attr_cols <- c(
  "tank_panel_id", "TANK_ID", "UST_ID", "CAPACITY",
  "is_fiberglass_tank", "is_composite_tank", "is_steel_tank", "is_jacketed_tank",
  "has_cp", "pip_has_cp",
  "pip_double", "pip_frp", "pip_steel", "pip_jacketed", "pip_flex", "pipe_type",
  "det_c_interstitial", "det_c_atg", "det_c_gw", "det_c_vapor", "det_c_sir", "det_c_sec_cont",
  "det_p_interstitial", "det_p_lld", "det_p_gw", "det_p_vapor", "det_p_suction_exempt",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel",
  "tank_status", "facility_min_install"
)
tank_attrs <- unique(raw_ust[, ..finer_attr_cols], by = "tank_panel_id")
cat(sprintf("  Distinct tank_panel_id in raw census: %s\n",
            format(nrow(tank_attrs), big.mark = ",")))
rm(raw_ust); invisible(gc())


###############################################################################
## STEP 1b — spill_comply (Zurich's overfill factor) — join texas_          ##
##          compartment.csv on ust_id (NOT the facility+tank guess — that   ##
##          key matched only 21.7% in the pre-flight diagnostic; ust_id is  ##
##          the universal TCEQ tank key common to both files).              ##
###############################################################################
cat("=== STEP 1b: spill_comply (texas_compartment.csv, joined on ust_id) ===\n")

comp_path <- here("Data", "Raw", "state_databases", "Texas", "texas_compartment.csv")
if (!file.exists(comp_path)) {
  cat("  texas_compartment.csv not found — spill_comply_y defaults FALSE for all tanks\n")
  tank_attrs[, spill_comply_y := FALSE]
} else {
  comp_raw <- fread(comp_path, select = c("ust_id", "spill_overfill_prevention_compliance_flag"))
  comp_raw[, ust_id := toupper(trimws(as.character(ust_id)))]
  comp_raw <- comp_raw[!is.na(spill_overfill_prevention_compliance_flag) &
                        spill_overfill_prevention_compliance_flag != ""]
  comp_raw <- comp_raw[, .(spill_flag = spill_overfill_prevention_compliance_flag[1L]), by = ust_id]

  n_before <- nrow(tank_attrs)
  tank_attrs <- comp_raw[tank_attrs, on = c(ust_id = "UST_ID")]
  setnames(tank_attrs, "ust_id", "UST_ID")
  match_rate_spill <- mean(!is.na(tank_attrs$spill_flag))
  cat(sprintf("  spill_comply ust_id join: %d / %d tanks matched (%.1f%%)\n",
              sum(!is.na(tank_attrs$spill_flag)), n_before, 100 * match_rate_spill))
  # No-data/unknown default -> FALSE (+10% load, Zurich's own convention;
  # matches the ORIGINAL per-engine behavior, applied uniformly here instead
  # of per-carrier — see ticket clarifying-Q answer on this join).
  tank_attrs[, spill_comply_y := (!is.na(spill_flag) & spill_flag == "Y")]
  tank_attrs[, spill_flag := NULL]
}


###############################################################################
## STEP 2 — panel_dt.csv join: the SINGLE source of truth for wall,         ##
##          age_years/age_bin, capacity, fuel, panel_year grain, and        ##
##          panel_id. TX only.                                              ##
###############################################################################
cat("=== STEP 2: panel_dt.csv join (wall, age, capacity, fuel, grain) ===\n")

panel_dt_path <- here("Data", "Analysis", "panel_dt.csv")
if (!file.exists(panel_dt_path))
  stop("panel_dt.csv not found at: ", panel_dt_path)

panel_tx <- fread(panel_dt_path, select = c(
  "tank_panel_id", "panel_id", "facility_id", "state", "mm_wall", "mm_fuel",
  "capacity", "tank_age", "panel_year", "tank_installed_date"))
panel_tx <- panel_tx[state == "TX"]
panel_tx[, state := NULL]

# PM-state wall mapping — mirrors 04al_BOY_Composition_Build.R:67: SW iff
# mm_wall contains "single" (case-insensitive), else DW. Mixed-Wall/
# Unknown-Wall fall through to DW exactly as the structural panel does.
panel_tx[, wall := fifelse(grepl("single", mm_wall, ignore.case = TRUE), "SW", "DW")]

panel_tx[, age_bin := as.integer(cut(tank_age,
            c(0, 5, 10, 15, 20, 25, 30, 35, Inf),
            labels = 1:8, right = FALSE, include.lowest = TRUE))]
panel_tx[is.na(age_bin), age_bin := 8L]

cat(sprintf("  panel_dt.csv TX tank-year rows: %s | distinct tanks: %s | years %d-%d\n",
            format(nrow(panel_tx), big.mark = ","),
            format(uniqueN(panel_tx$tank_panel_id), big.mark = ","),
            min(panel_tx$panel_year), max(panel_tx$panel_year)))

panel_tx <- tank_attrs[panel_tx, on = "tank_panel_id"]
cat(sprintf("  raw finer-attr match onto panel_dt tank-years: %.1f%%\n",
            100 * mean(!is.na(panel_tx$facility_min_install))))


###############################################################################
## STEP 3 — FR coverage: crosswalk ISSUER_NAME -> carrier_key, collapse to  ##
##          (panel_id, panel_year, carrier_key), filter to priceable tier. ##
##          Representative eff_date = earliest contract-month EFF_DATE     ##
##          observed that year (for era-dependent carriers).               ##
###############################################################################
cat("=== STEP 3: FR coverage + issuer crosswalk ===\n")

fr_path <- here("Data", "Processed", "texas_fr_contract_month_panel.csv")
if (!file.exists(fr_path))
  stop("texas_fr_contract_month_panel.csv not found at: ", fr_path)

fr <- fread(fr_path, select = c("FACILITY_ID", "YEAR", "MONTH", "EFF_DATE",
                                 "ISSUER_NAME", "COVER_OCC", "COVER_AGG"))
fr[, FACILITY_ID := toupper(trimws(as.character(FACILITY_ID)))]
fr[, panel_id     := paste(FACILITY_ID, "TX", sep = "_")]
fr[, EFF_DATE     := ymd(EFF_DATE)]

resolved <- write_issuer_crosswalk_qc(
  fr$ISSUER_NAME, file.path(out_dir, "_issuer_crosswalk_qc.csv"))
fr[, `:=`(carrier_key = resolved$carrier_key, tier = resolved$tier)]

fr_priced <- fr[tier == "priceable"]
cat(sprintf("  FR contract-months: %s total | %s priceable (%.1f%%)\n",
            format(nrow(fr), big.mark = ","), format(nrow(fr_priced), big.mark = ","),
            100 * nrow(fr_priced) / nrow(fr)))

fr_year <- fr_priced[, .(
  n_months  = .N,
  eff_date  = min(EFF_DATE, na.rm = TRUE),
  cover_occ = COVER_OCC[1L],
  cover_agg = COVER_AGG[1L]
), by = .(panel_id, panel_year = YEAR, carrier = carrier_key)]

cat(sprintf("  Insured (facility, year, carrier) combinations: %s\n",
            format(nrow(fr_year), big.mark = ",")))
cat("  By carrier:\n")
print(fr_year[, .N, by = carrier][order(-N)])


###############################################################################
## STEP 4 — Final join: FR coverage x panel_dt active tank-years. panel_dt's ##
##          own (tank_panel_id, panel_year) rows already ARE the install/   ##
##          close window (upstream 02b logic) — a key-matched merge on      ##
##          (panel_id, panel_year), never a tank x all-months cartesian.    ##
###############################################################################
cat("=== STEP 4: join FR coverage onto active tank-years ===\n")

priced <- fr_year[panel_tx, on = c("panel_id", "panel_year"), nomatch = NULL, allow.cartesian = TRUE]

cat(sprintf("  Priced tank-year-carrier rows: %s\n", format(nrow(priced), big.mark = ",")))

# Insured-match-rate guard: what share of insured (facility, year, carrier)
# combinations found at least one matching panel_dt tank? (See guard
# self-test above for the synthetic hard-stop cases.)
n_insured_combos <- nrow(fr_year)
n_matched_combos <- uniqueN(priced[, .(panel_id, panel_year, carrier)])
match_rate <- check_insured_match_rate(n_matched_combos, n_insured_combos)
cat(sprintf("  Insured-combo match rate: %.1f%% (%d / %d) — guard OK (floor %.0f%%)\n",
            100 * match_rate, n_matched_combos, n_insured_combos, 100 * MIN_INSURED_MATCH_RATE))


###############################################################################
## STEP 5 — Assemble + validate output schema                               ##
###############################################################################
cat("=== STEP 5: assemble output ===\n")

setnames(priced, c("TANK_ID", "UST_ID", "CAPACITY"),
         c("tank_id", "ust_id", "capacity_raw"))

out_cols <- c(
  "panel_id", "tank_panel_id", "tank_id", "ust_id", "panel_year", "carrier",
  "wall", "age_bin", "age_years", "capacity", "mm_fuel",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel",
  "is_fiberglass_tank", "is_composite_tank", "is_steel_tank", "is_jacketed_tank",
  "has_cp", "pip_has_cp",
  "pip_double", "pip_frp", "pip_steel", "pip_jacketed", "pip_flex", "pipe_type",
  "det_c_interstitial", "det_c_atg", "det_c_gw", "det_c_vapor", "det_c_sir", "det_c_sec_cont",
  "det_p_interstitial", "det_p_lld", "det_p_gw", "det_p_vapor", "det_p_suction_exempt",
  "spill_comply_y", "tank_status",
  "tank_installed_date", "facility_min_install",
  "eff_date", "cover_occ", "cover_agg"
)
setnames(priced, c("tank_age"), c("age_years"))
priced[, capacity := capacity_raw]

missing_out <- setdiff(out_cols, names(priced))
stopifnot(length(missing_out) == 0L)

out <- priced[, ..out_cols]
setorder(out, panel_id, panel_year, carrier, tank_id)

###############################################################################
## STEP 6 — Validate (ticket ACCEPTANCE criteria) + write                   ##
###############################################################################
cat("=== STEP 6: validate + write ===\n")

stopifnot(all(!is.na(out$wall)))
stopifnot(all(out$wall %in% c("SW", "DW")))
stopifnot(all(out$age_bin %in% 1:8))
stopifnot(all(out$age_years >= 0, na.rm = TRUE))
stopifnot(nrow(out) == uniqueN(out[, .(panel_id, tank_id, panel_year, carrier)]))

cat(sprintf("  Rows: %s | distinct tanks: %s | distinct facilities: %s | years %d-%d\n",
            format(nrow(out), big.mark = ","),
            format(uniqueN(out$tank_panel_id), big.mark = ","),
            format(uniqueN(out$panel_id), big.mark = ","),
            min(out$panel_year), max(out$panel_year)))
cat("  Row counts by carrier:\n")
print(out[, .N, by = carrier][order(-N)])
cat("  SW/DW share:\n")
print(out[, .N, by = wall][, pct := round(100 * N / sum(N), 1)][])
cat(sprintf("  age_years range: %.0f - %.0f\n", min(out$age_years), max(out$age_years)))

fwrite(out, out_path)
cat(sprintf("\n  Saved: %s (%s rows)\n", out_path, format(nrow(out), big.mark = ",")))

cat(sprintf("\nRun completed: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=== 20_Build_Priced_Tank_Panel DONE ===\n")
