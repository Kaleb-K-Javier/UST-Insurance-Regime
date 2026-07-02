###############################################################################
# 19a_engine_great_american.R — Great American (STP Program) rate engine:
#                                pure functions + self-test
# Filing:  SERFF GACX-132517275, "STP Program Rates- Texas.pdf" — base rate,
#          capacity/age/construction/contents/leak-detection/line-leak-
#          detection factor tables, ILF, multiple-tank factor, min premium.
#          Deductible table CORRECTED per SERFF GACX-132842896, page 3
#          (use the corrected table for all 2021+ business; supersedes any
#          earlier-printed deductible row in GACX-132517275).
# Sources: library(data.table) only — no data, no I/O.
#
# *** COMPOSITION IS MULTIPLICATIVE — RESEARCHER-RESOLVED 2026-07-01 ***
#   Composition is MULTIPLICATIVE. The filing prints "[Base + (Rating
#   Adjustments)]" on p.8, but that "+" is a transcription typo for "x": GA
#   states its rates derive from ACE American's program, and ACE TankSafe is
#   dispositively multiplicative (ACE's own computed worksheet: every row =
#   Base x (1-credit)); the additive reading makes GA's Age/Construction
#   factors inert. Researcher-resolved 2026-07-01.
#
#   Proof chain: GA's filing states its rates "are a function of previously
#   approved rates for storage tank programs submitted by ACE American,
#   Zurich American, and Mid-Continent Casualty" — i.e. GA copied ACE
#   TankSafe's structure. ACE TankSafe is fully multiplicative, verified
#   against ACE's own internal computed worksheet (TankSafe Rating Worksheets
#   2-11.pdf): every row = Base x (1 - credit%) exactly (age-0 base $300 ->
#   ".50 credit" column = $150 = 300x0.50; ".20 credit" = $240 = 300x0.80).
#   ACE Section A states verbatim "The credit/debits are multiplicative."
#   The additive reading made GA's Age/Construction/etc. factors inert
#   (~$750 flat), inconsistent with that lineage.
#
#   Individual Tank Premium = Base Rate x Rating Adjustments x
#                              Deductible Factor x Increased Limit Factor
#   Rating Adjustments = Capacity Factor x Age Factor x Construction Factor x
#                         Contents Factor x Leak Detection Factor x
#                         Line Leak Detection Factor
#
# FULL ORDERED FORMULA (facility/policy level):
#   1. rating_adjustments = Capacity(capacity_gal) x Age(age_years) x
#        Construction(code) x Contents(code) x LeakDetection(code) x
#        LineLeakDetection(code)                 [GACX-132517275, rate pages]
#   2. per_tank_standard = BASE_RATE[$750] x rating_adjustments x
#        DeductibleFactor(deductible) x ILF(cover_occ, cover_agg) x
#        CoverageC(off = 1.00, not separately transcribed)
#      At the reference contract (deductible=$5,000, cover=$1M/$1M, default
#      args) DeductibleFactor = ILF = CoverageC = 1.0, collapsing to:
#        per_tank_standard = 750 x Capacity x Age x Construction x Contents x
#          LeakDetection x LineLeakDetection
#   3. facility_unmodified = sum(per_tank_standard) across the facility's
#      rated UST tanks
#   4. facility_standard = facility_unmodified x MultipleTankFactor(n_tanks)
#        [facility tank-count ladder, GACX-132517275]
#   5. NO schedule-rating band is filed for this program -> min = standard =
#        max = facility_standard (no +/-40% envelope, unlike AIG/TOMICS/ACE)
#   6. facility_premium = pmax(facility_standard, MIN_PREMIUM[$225])
#        [policy minimum, GACX-132517275]
#
# BEHAVIORAL ASSUMPTIONS (held at the filing's default/reference contract —
# firm-choice items we do not observe/guess, per RATE_ENGINES_README §1 /
# SESSION_PROMPT_040 §1):
#   - Base Rate ($750) covers Coverage A+B combined; Coverage C (optional, not
#     separately transcribed) is held OFF -> contributes a no-op factor of
#     1.00, not separately coded (nothing to look up at this reference
#     contract).
#   - Deductible held at the filing's $5,000 base -> factor 1.000 by default;
#     the full CORRECTED ladder (GACX-132842896 p.3) is encoded for all 2021+
#     business and spot-checked in the self-test.
#   - Coverage limit held FIXED at $1,000,000/$1,000,000 (036 locked) -> ILF =
#     1.000. No ILF ladder beyond this single reference point was filed for
#     GA -> the builder hard-stops (does not fabricate a factor) if called
#     with a non-reference cover_occ/cover_agg, same convention as the ACE
#     engine (18a).
#   - Age table stops at the 31-35 band (factor 4.00). No band is filed for
#     age > 35. ASSUMPTION (FLAGGED): extend the top band (4.00) to all
#     age > 35 tanks rather than error them out of the sample. Spot-checked
#     in the self-test at age=35 and age=50.
#   - No schedule-rating band exists in this filing (none filed, confirmed) ->
#     min = standard = max identically; there is no discretionary +/-40%
#     envelope to encode here (unlike AIG/TOMICS/ACE).
#   - MIN_PREMIUM = $225 (policy/facility level) is applied to the single
#     facility_standard value (min=standard=max share it). Under the
#     multiplicative chain, the worst-discount single tank at the reference
#     deductible/ILF ($5,000 / $1M/$1M) prices at $750 x 0.90 x 1.00 x 0.80 x
#     0.70 x 0.80 x 0.90 = $272.16 — still above the $225 floor, so the floor
#     is unreachable at the reference contract for a single tank under the
#     filed tables. It becomes reachable at deeper filed deductible levels
#     (e.g. $100,000 -> factor 0.574 would drop that same tank to $156.20).
#     The floor mechanism is encoded generally and unit-tested directly below.
#   - GA prices 2021+ only (new STP program; no earlier GA filing located for
#     TX, per _BUILD_PROMPT_GREAT_AMERICAN.txt). The builder takes eff_date
#     and hard-stops for eff_date < 2021-01-01 rather than silently apply a
#     2021+ table to a pre-2021 contract-year.
#   - Wall (SW/DW) derivation from the construction code is an APPLY-stage
#     (Script 2) concern — this engine only takes a construction code and
#     returns its factor.
###############################################################################

library(data.table)

# ── Module constants ────────────────────────────────────────────────────────
BASE_RATE_GA       <- 750                       # Coverage A+B base; Coverage C off = 1.00
MIN_PREMIUM_GA     <- 225                        # policy/facility minimum premium
REF_COVER_OCC      <- 1e6                        # 036-locked reference limit (occ)
REF_COVER_AGG      <- 1e6                        # 036-locked reference limit (agg)
REF_DEDUCTIBLE     <- 5000                       # filing base deductible
GA_MIN_FILING_DATE <- as.Date("2021-01-01")      # GA's STP program is 2021+ only

###############################################################################
## E1. Tank capacity factor  (GACX-132517275) — select-one, gallon bands    ##
##   1-1000=0.90 | 1001-5000=1.00 | 5001-10000=1.25 | 10001-20000=1.50      ##
##   20001-30000=2.00                                                       ##
###############################################################################
capacity_factor_ga <- function(capacity_gal) {
  v <- fcase(
    capacity_gal >=     1 & capacity_gal <=  1000, 0.90,
    capacity_gal >=  1001 & capacity_gal <=  5000, 1.00,
    capacity_gal >=  5001 & capacity_gal <= 10000, 1.25,
    capacity_gal >= 10001 & capacity_gal <= 20000, 1.50,
    capacity_gal >= 20001 & capacity_gal <= 30000, 2.00,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("capacity_factor_ga: capacity_gal outside filed 1-30000 range: %s",
                  paste(unique(capacity_gal[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E2. Tank age factor  (GACX-132517275) — select-one, year bands          ##
##   0-10=1.00 | 11-15=1.25 | 16-20=1.50 | 21-25=1.75 | 26-30=2.50          ##
##   31-35=4.00 | >35=4.00 EXTENDED (ASSUMPTION, flagged in header)         ##
###############################################################################
age_factor_ga <- function(age_years) {
  v <- fcase(
    age_years >=  0 & age_years <= 10, 1.00,
    age_years >= 11 & age_years <= 15, 1.25,
    age_years >= 16 & age_years <= 20, 1.50,
    age_years >= 21 & age_years <= 25, 1.75,
    age_years >= 26 & age_years <= 30, 2.50,
    age_years >= 31,                    4.00,   # 31-35 filed; >35 extended (top band held flat)
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("age_factor_ga: invalid age_years (must be >= 0): %s",
                  paste(unique(age_years[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E3. Tank construction factor  (GACX-132517275) — select-one             ##
##   SWF=1.15 | DWF=1.00 | SWSC=1.15 | SWS=1.50 | FRP=0.90 | DWS=0.80       ##
###############################################################################
construction_factor_ga <- function(code) {
  v <- fcase(
    code == "SWF",  1.15,
    code == "DWF",  1.00,
    code == "SWSC", 1.15,
    code == "SWS",  1.50,
    code == "FRP",  0.90,
    code == "DWS",  0.80,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("construction_factor_ga: unrecognized code(s): %s",
                  paste(unique(code[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E4. Tank contents factor  (GACX-132517275) — select-one                 ##
##   Gasoline=1.00 | Diesel=0.95 | FuelOil=0.95 | HeatingOil=0.95           ##
##   WasteOil=0.95 | LubeOil=0.90 | AviationFuel=1.00 | Kerosene=0.95       ##
##   Propane=0.70 | LNG=0.80                                                ##
###############################################################################
contents_factor_ga <- function(code) {
  v <- fcase(
    code == "GASOLINE",      1.00,
    code == "DIESEL",        0.95,
    code == "FUEL_OIL",      0.95,
    code == "HEATING_OIL",   0.95,
    code == "WASTE_OIL",     0.95,
    code == "LUBE_OIL",      0.90,
    code == "AVIATION_FUEL", 1.00,
    code == "KEROSENE",      0.95,
    code == "PROPANE",       0.70,
    code == "LNG",           0.80,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("contents_factor_ga: unrecognized code(s): %s",
                  paste(unique(code[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E5. Leak detection factor  (GACX-132517275) — select-one                ##
##   ContinuousInTank=0.80 | Interstitial=0.85 | ATG=0.90 | SIR=1.00        ##
##   Manual=1.15 | Vapor=1.05 | Groundwater=1.05                            ##
###############################################################################
leak_detection_factor_ga <- function(code) {
  v <- fcase(
    code == "CONTINUOUS_IN_TANK", 0.80,
    code == "INTERSTITIAL",       0.85,
    code == "ATG",                0.90,
    code == "SIR",                1.00,
    code == "MANUAL",             1.15,
    code == "VAPOR",              1.05,
    code == "GROUNDWATER",        1.05,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("leak_detection_factor_ga: unrecognized code(s): %s",
                  paste(unique(code[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E6. Line leak detection factor  (GACX-132517275) — binary               ##
##   Yes=0.90 | No=1.00                                                     ##
###############################################################################
line_leak_detection_factor_ga <- function(code) {
  v <- fcase(
    code == "YES", 0.90,
    code == "NO",  1.00,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("line_leak_detection_factor_ga: unrecognized code(s): %s",
                  paste(unique(code[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E7. Deductible factor  (CORRECTED, GACX-132842896 p.3) — base = $5,000  ##
##   2500=1.075 | 5000=1.000(base) | 10000=0.949 | 15000=0.911              ##
##   20000=0.881 | 25000=0.851 | 50000=0.698 | 75000=0.613 | 100000=0.574   ##
###############################################################################
deductible_factor_ga <- function(deductible) {
  v <- fcase(
    deductible ==   2500, 1.075,
    deductible ==   5000, 1.000,
    deductible ==  10000, 0.949,
    deductible ==  15000, 0.911,
    deductible ==  20000, 0.881,
    deductible ==  25000, 0.851,
    deductible ==  50000, 0.698,
    deductible ==  75000, 0.613,
    deductible == 100000, 0.574,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("deductible_factor_ga: unrecognized deductible(s): %s",
                  paste(unique(deductible[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E8. Increased Limit Factor  (GACX-132517275) — single reference point;  ##
##   base = $1,000,000/$1,000,000 = 1.000. No further ILF ladder was filed ##
##   for GA -> hard-stop on any non-reference limit (do not fabricate).    ##
###############################################################################
ilf_factor_ga <- function(cover_occ, cover_agg) {
  v <- fcase(
    cover_occ == 1e6 & cover_agg == 1e6, 1.000,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf(
      "ilf_factor_ga: only the $1,000,000/$1,000,000 reference limit is filed for GA; got occ=%s agg=%s",
      paste(unique(cover_occ[is.na(v)]), collapse = ", "),
      paste(unique(cover_agg[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E9. Multiple tank factor  (facility tank count, GACX-132517275)         ##
##   1-10=1.000 | 11-25=0.900 | 26-50=0.800 | 51-100=0.750 | 101-200=0.700  ##
###############################################################################
multiple_tank_factor_ga <- function(n_tanks) {
  v <- fcase(
    n_tanks >=   1 & n_tanks <=  10, 1.000,
    n_tanks >=  11 & n_tanks <=  25, 0.900,
    n_tanks >=  26 & n_tanks <=  50, 0.800,
    n_tanks >=  51 & n_tanks <= 100, 0.750,
    n_tanks >= 101 & n_tanks <= 200, 0.700,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("multiple_tank_factor_ga: n_tanks outside filed 1-200 range: %s",
                  paste(unique(n_tanks[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E10. Policy minimum premium floor  (GACX-132517275) — $225              ##
###############################################################################
apply_floor_ga <- function(x) pmax(x, MIN_PREMIUM_GA)

###############################################################################
## SS1 — Facility-level premium builder                                     ##
##                                                                          ##
## tanks_dt: one row per in-service UST tank; required cols:                ##
##   capacity_gal        (numeric, 1-30000)                                 ##
##   age_years            (numeric, >= 0)                                   ##
##   construction_code    {SWF, DWF, SWSC, SWS, FRP, DWS}                   ##
##   contents_code        {GASOLINE, DIESEL, FUEL_OIL, HEATING_OIL,         ##
##                         WASTE_OIL, LUBE_OIL, AVIATION_FUEL, KEROSENE,    ##
##                         PROPANE, LNG}                                    ##
##   leak_code            {CONTINUOUS_IN_TANK, INTERSTITIAL, ATG, SIR,      ##
##                         MANUAL, VAPOR, GROUNDWATER}                      ##
##   line_leak_code       {YES, NO}                                         ##
##                                                                          ##
## eff_date: length-1 Date/character — hard-stops if < 2021-01-01 (GA's    ##
##   STP program is 2021+ only).                                           ##
## cover_occ/cover_agg/deductible: facility-level scalars, default to the  ##
##   036-locked reference contract ($1M/$1M, $5,000 deductible).           ##
##                                                                          ##
## Returns: list(min_prem, standard_prem, max_prem, n_tanks_rated)         ##
## min_prem == standard_prem == max_prem: no schedule-rating band exists   ##
## for this filing. Floor applied at the facility level.                  ##
###############################################################################
ga_facility_premium <- function(tanks_dt, eff_date,
                                 cover_occ  = REF_COVER_OCC,
                                 cover_agg  = REF_COVER_AGG,
                                 deductible = REF_DEDUCTIBLE) {
  stopifnot(length(eff_date) == 1)
  eff_date <- as.Date(eff_date)
  if (eff_date < GA_MIN_FILING_DATE) {
    stop(sprintf(
      "ga_facility_premium: Great American's STP program is filed 2021+ only; got eff_date=%s",
      eff_date))
  }

  req_cols <- c("capacity_gal", "age_years", "construction_code",
                "contents_code", "leak_code", "line_leak_code")
  stopifnot(all(req_cols %in% names(tanks_dt)))

  dt <- copy(tanks_dt)
  dt[, rating_adjustments := (
    capacity_factor_ga(capacity_gal) *
    age_factor_ga(age_years) *
    construction_factor_ga(construction_code) *
    contents_factor_ga(contents_code) *
    leak_detection_factor_ga(leak_code) *
    line_leak_detection_factor_ga(line_leak_code)
  )]

  ded_ilf_factor <- deductible_factor_ga(deductible) * ilf_factor_ga(cover_occ, cover_agg)

  # Fully multiplicative chain (see header) — CoverageC held off (no-op 1.00)
  dt[, tank_premium := BASE_RATE_GA * rating_adjustments * ded_ilf_factor]

  n_tanks <- nrow(dt)
  facility_unmodified <- sum(dt$tank_premium)
  facility_standard <- facility_unmodified * multiple_tank_factor_ga(n_tanks)

  # No schedule-rating band exists in this filing -> min = standard = max
  premium <- apply_floor_ga(facility_standard)

  stopifnot(is.finite(premium), premium >= MIN_PREMIUM_GA)

  list(min_prem = premium, standard_prem = premium, max_prem = premium,
       n_tanks_rated = n_tanks)
}

###############################################################################
## SS2 — Self-test (runs at source time; stopifnot on hand-calc values)    ##
###############################################################################

cat("=== 19a self-test ===\n")

# ── Ladder spot-checks (full factor tables, incl. the age>35 extension) ────
stopifnot(isTRUE(all.equal(capacity_factor_ga(1000),   0.90)))
stopifnot(isTRUE(all.equal(capacity_factor_ga(1001),   1.00)))
stopifnot(isTRUE(all.equal(capacity_factor_ga(10000),  1.25)))
stopifnot(isTRUE(all.equal(capacity_factor_ga(30000),  2.00)))
stopifnot(isTRUE(all.equal(age_factor_ga(10), 1.00)))
stopifnot(isTRUE(all.equal(age_factor_ga(11), 1.25)))
stopifnot(isTRUE(all.equal(age_factor_ga(30), 2.50)))
stopifnot(isTRUE(all.equal(age_factor_ga(31), 4.00)))
stopifnot(isTRUE(all.equal(age_factor_ga(35), 4.00)))
stopifnot(isTRUE(all.equal(age_factor_ga(50), 4.00)))   # extended top band (flagged assumption)
stopifnot(isTRUE(all.equal(construction_factor_ga("SWF"),  1.15)))
stopifnot(isTRUE(all.equal(construction_factor_ga("DWF"),  1.00)))
stopifnot(isTRUE(all.equal(construction_factor_ga("SWSC"), 1.15)))
stopifnot(isTRUE(all.equal(construction_factor_ga("SWS"),  1.50)))
stopifnot(isTRUE(all.equal(construction_factor_ga("FRP"),  0.90)))
stopifnot(isTRUE(all.equal(construction_factor_ga("DWS"),  0.80)))
stopifnot(isTRUE(all.equal(contents_factor_ga("GASOLINE"),      1.00)))
stopifnot(isTRUE(all.equal(contents_factor_ga("AVIATION_FUEL"), 1.00)))
stopifnot(isTRUE(all.equal(contents_factor_ga("PROPANE"),       0.70)))
stopifnot(isTRUE(all.equal(contents_factor_ga("LNG"),           0.80)))
stopifnot(isTRUE(all.equal(leak_detection_factor_ga("CONTINUOUS_IN_TANK"), 0.80)))
stopifnot(isTRUE(all.equal(leak_detection_factor_ga("MANUAL"),            1.15)))
stopifnot(isTRUE(all.equal(line_leak_detection_factor_ga("YES"), 0.90)))
stopifnot(isTRUE(all.equal(line_leak_detection_factor_ga("NO"),  1.00)))
stopifnot(isTRUE(all.equal(deductible_factor_ga(5000),   1.000)))
stopifnot(isTRUE(all.equal(deductible_factor_ga(100000), 0.574)))
stopifnot(isTRUE(all.equal(ilf_factor_ga(1e6, 1e6), 1.000)))
stopifnot(isTRUE(all.equal(multiple_tank_factor_ga(10),  1.000)))
stopifnot(isTRUE(all.equal(multiple_tank_factor_ga(25),  0.900)))
stopifnot(isTRUE(all.equal(multiple_tank_factor_ga(200), 0.700)))
cat("  ladder spot-checks OK\n")

# ── Case 1: best-in-class new DWS tank — demonstrates the multiplicative
#     chain lands in the hundreds, not near-flat at ~$750+ ─────────────────
# capacity=3000 (1001-5000=1.00), age=5 (0-10=1.00), DWS(0.80), Gasoline(1.00),
#   ContinuousInTank(0.80), LineLeak Yes(0.90)
#   rating_adjustments = 1.00*1.00*0.80*1.00*0.80*0.90 = 0.576
#   tank_premium = 750 * 0.576 * (1.000*1.000) = 432.00
#   n_tanks=1 -> 1-10 band -> multi-tank factor 1.000
#   facility_standard = 432.00*1.000 = 432.00
#   min = standard = max = 432.00

fake1 <- data.table(
  capacity_gal      = 3000,
  age_years         = 5L,
  construction_code = "DWS",
  contents_code     = "GASOLINE",
  leak_code         = "CONTINUOUS_IN_TANK",
  line_leak_code    = "YES"
)
r1 <- ga_facility_premium(fake1, eff_date = as.Date("2022-01-01"))
stopifnot(isTRUE(all.equal(r1$standard_prem, 432.00, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r1$min_prem,      432.00, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r1$max_prem,      432.00, tolerance = 1e-6)))
stopifnot(r1$n_tanks_rated == 1L)

# ── Case 2: worst-case old single-wall-steel tank — several $thousand,     ─
#     confirms a real (not near-flat) spread vs Case 1 ────────────────────
# capacity=25000 (20001-30000=2.00), age=50 (>35 extended=4.00), SWS(1.50),
#   Gasoline(1.00), Manual(1.15), LineLeak No(1.00)
#   rating_adjustments = 2.00*4.00*1.50*1.00*1.15*1.00 = 13.80
#   tank_premium = 750 * 13.80 * (1.000*1.000) = 10350.00
#   n_tanks=1 -> multi-tank factor 1.000
#   min = standard = max = 10350.00
# Case1 vs Case2: $432.00 -> $10,350.00 -> a ~24x, thousands-vs-hundreds
#   spread -> confirms the multiplicative chain is NOT near-flat.

fake2 <- data.table(
  capacity_gal      = 25000,
  age_years         = 50L,
  construction_code = "SWS",
  contents_code     = "GASOLINE",
  leak_code         = "MANUAL",
  line_leak_code    = "NO"
)
r2 <- ga_facility_premium(fake2, eff_date = as.Date("2023-06-01"))
stopifnot(isTRUE(all.equal(r2$standard_prem, 10350.00, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r2$min_prem,      10350.00, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r2$max_prem,      10350.00, tolerance = 1e-6)))
stopifnot((r2$standard_prem - r1$standard_prem) > 5000)
cat("  real hundreds-to-thousands best-vs-worst spread confirmed (NOT near-flat)\n")

# ── Case 3: 15-tank facility — exercises the 11-25 (0.900) multi-tank band ─
# Each tank: capacity=3000 (1.00), age=8 (1.00), DWF(1.00), Diesel(0.95),
#   SIR(1.00), LineLeak No(1.00)
#   rating_adjustments = 1.00*1.00*1.00*0.95*1.00*1.00 = 0.95
#   tank_premium = 750 * 0.95 * (1.000*1.000) = 712.50
#   facility_unmodified = 15*712.50 = 10687.50
#   n_tanks=15 -> 11-25 band -> multi-tank factor 0.900
#   facility_standard = 10687.50*0.900 = 9618.75
#   min = standard = max = 9618.75

fake3 <- data.table(
  capacity_gal      = rep(3000, 15),
  age_years         = rep(8L, 15),
  construction_code = rep("DWF", 15),
  contents_code     = rep("DIESEL", 15),
  leak_code         = rep("SIR", 15),
  line_leak_code    = rep("NO", 15)
)
r3 <- ga_facility_premium(fake3, eff_date = as.Date("2021-01-01"))
stopifnot(r3$n_tanks_rated == 15L)
stopifnot(isTRUE(all.equal(r3$standard_prem, 9618.75, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r3$min_prem,      9618.75, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r3$max_prem,      9618.75, tolerance = 1e-6)))

# ── Case 4: floor mechanism — unit-tested directly (unreachable via the
#     builder at the reference contract for a single tank — worst-discount
#     single tank prices at $272.16, see header — but encoded generally and
#     tested here directly) ─────────────────────────────────────────────────
stopifnot(isTRUE(all.equal(apply_floor_ga(100), 225)))
stopifnot(isTRUE(all.equal(apply_floor_ga(500), 500)))
cat("  floor mechanism unit-test OK\n")

# ── Case 5: pre-2021 eff_date hard-stops (GA's program is 2021+ only) ──────
stopifnot(inherits(
  tryCatch(ga_facility_premium(fake1, eff_date = as.Date("2019-01-01")), error = function(e) e),
  "error"
))
cat("  pre-2021 eff_date hard-stop OK\n")

# ── Case 6: non-reference coverage limit hard-stops (no ILF ladder filed) ──
stopifnot(inherits(
  tryCatch(ga_facility_premium(fake1, eff_date = as.Date("2022-01-01"), cover_occ = 2e6, cover_agg = 2e6),
           error = function(e) e),
  "error"
))
cat("  non-reference-ILF hard-stop OK\n")

cat("19a self-test PASS\n")

###############################################################################
## price_ga_tank — pure per-tank premium (Ticket 053, LAYER 2)              ##
##   Vectorized over rows of the shared priced-tank panel. Extracts the     ##
##   "tank_premium" step already computed inline inside                     ##
##   ga_facility_premium() (SS1 above) into its own named function — same   ##
##   math, unchanged (fully multiplicative chain, see header). deductible/  ##
##   ILF factor is computed at the 036-locked reference contract internally ##
##   ($5,000 ded, $1M/$1M -> factor 1.000; ilf_factor_ga() would hard-stop  ##
##   on any other limit, so this function is reference-contract-only by     ##
##   construction, matching every other carrier's card invariant). Returns  ##
##   per-tank premium (no multiple-tank factor, no floor — facility-level,  ##
##   applied in 19b). NOTE: unlike the other 4 carriers, GA has NO schedule ##
##   band -> this per-tank number IS the standalone standard (min=std=max   ##
##   at facility level too, per 19a's own convention).                     ##
###############################################################################
price_ga_tank <- function(capacity_gal, age_years, construction_code,
                           contents_code, leak_code, line_leak_code) {
  rating_adjustments <- (
    capacity_factor_ga(capacity_gal) *
    age_factor_ga(age_years) *
    construction_factor_ga(construction_code) *
    contents_factor_ga(contents_code) *
    leak_detection_factor_ga(leak_code) *
    line_leak_detection_factor_ga(line_leak_code)
  )
  ded_ilf_factor <- deductible_factor_ga(REF_DEDUCTIBLE) *
                     ilf_factor_ga(REF_COVER_OCC, REF_COVER_AGG)
  BASE_RATE_GA * rating_adjustments * ded_ilf_factor
}

cat("=== 19a price_ga_tank self-test ===\n")

# Re-use fake1 (SS2 Case 1) — best-in-class single tank, 432.00, equals
# r1$standard_prem exactly (n_tanks=1 -> multiple tank factor 1.000, no band).
pt1 <- price_ga_tank(
  capacity_gal = 3000, age_years = 5L, construction_code = "DWS",
  contents_code = "GASOLINE", leak_code = "CONTINUOUS_IN_TANK",
  line_leak_code = "YES"
)
stopifnot(isTRUE(all.equal(pt1, 432.00, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(pt1, r1$standard_prem, tolerance = 1e-6)))

# Re-use fake2 (SS2 Case 2) — worst-case single tank, 10350.00.
pt2 <- price_ga_tank(
  capacity_gal = 25000, age_years = 50L, construction_code = "SWS",
  contents_code = "GASOLINE", leak_code = "MANUAL", line_leak_code = "NO"
)
stopifnot(isTRUE(all.equal(pt2, 10350.00, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(pt2, r2$standard_prem, tolerance = 1e-6)))

# Re-use fake3 (SS2 Case 3) — 15 identical tanks, each tank_premium = 712.50
# (hand-verified in the Case 3 comment; facility_unmodified = 10687.50 before
# the 0.900 multiple-tank factor that produces r3$standard_prem = 9618.75).
pt3 <- price_ga_tank(
  capacity_gal = rep(3000, 15), age_years = rep(8L, 15),
  construction_code = rep("DWF", 15), contents_code = rep("DIESEL", 15),
  leak_code = rep("SIR", 15), line_leak_code = rep("NO", 15)
)
stopifnot(isTRUE(all.equal(pt3, rep(712.50, 15), tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(sum(pt3), 10687.50, tolerance = 1e-6)))

cat("19a price_ga_tank self-test PASS\n")
