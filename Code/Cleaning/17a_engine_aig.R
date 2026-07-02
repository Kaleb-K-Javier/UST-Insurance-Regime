###############################################################################
# 17a_engine_aig.R — AIG (Commerce & Industry Insurance Company) rate engine:
#                    pure functions + self-test
# Filing:  SERFF AGNY-130002263, "Rate-Rule Attachments/TX Rate - 2015.pdf"
#          NEW table eff. 08/01/2015.
#   OLD  = same filing's "Supporting Document Attachments/TX Rate - 2015.pdf"
#          redline, in effect 04/14/2000 - 07/31/2015.
#   Page cites below are the printed "Page N of 6" footers in the base PDF.
# Sources: library(data.table) only — no data, no I/O.
#
# COMPOSITION RULE (filing §III / p.1, VERBATIM):
#   "Multiply the base premium in II above by the factor of the sum of the
#    total credits and debits from A, B, C, and D below, Tank Characteristics.
#    [Base Premium * (1 + tank characteristics)] = unmodified premium for UST"
#   => tank_unmodified = Base(age_cat, era) * (1 + A + B + C + D)  -- SUM the
#      four characteristic loads, THEN one multiplier. NOT a chained product
#      of (1+each) — confirmed against the filing text, do not "improve" this.
#
# FULL ORDERED FORMULA (facility/policy level; mirrors filing §II -> §XI):
#   1. tank_unmodified = Base(age_cat, era) * (1 + A_construction + B_leak +
#        C_pipe_construction + D_pipe_leak)                          [p.1-2 §II]
#   2. facility_unmodified = sum(tank_unmodified) across the facility's rated
#      UST tanks. (Filing's §IV also folds in AST premia from §III; we have
#      no AST tanks in raw_pst_ust.csv, so that term is 0 here.)
#   3. modified_tank_premium_IV = facility_unmodified *
#        (1 - (compliance_credit[held 0] + remote_credit[held 0] +
#               multi_tank_credit(n_tanks_actual)))                   [p.3-4 §IV]
#   4. modified_tank_premium_V = modified_tank_premium_IV *
#        (1 -/+ SCHED_CAP) for min/max ; * 1 for standard              [p.4 §V]
#   5. tank_premium_at_limits = modified_tank_premium_V *
#        (ILF(cover_occ, cover_agg) - deductible_credit(deductible))  [p.4 §VI/§VII]
#   6. facility_premium = max(tank_premium_at_limits, MIN_PREMIUM[era])[p.6 §XI]
#
# BEHAVIORAL ASSUMPTIONS (held at the filing's default/reference contract —
# firm-choice items we do not observe/guess, per RATE_ENGINES_README §1 /
# SESSION_PROMPT_040 §1):
#   - §IV.A Compliance management credit (5%) and §IV.B Remote monitoring
#     credit (10%) are DISCRETIONARY / unobservable in raw_pst_ust.csv ->
#     held at 0. §IV.C Multi-tank credit IS applied, keyed on the facility's
#     ACTUAL observed tank count (1-5 tanks = 0%, researcher-confirmed; not
#     an explicit printed row, the ladder table itself starts at 6-14 tanks).
#   - §VI Deductible held at the filing's $5,000 base -> credit 0.00.
#   - §VII Coverage limit held FIXED at $1,000,000/$1,000,000 (036 locked)
#     -> ILF = 1.00.
#   - Combined (ILF - deductible_credit) factor = 1.00 at these defaults, so
#     step 5 is a no-op multiplier under the locked assumptions. The full
#     §VI/§VII ladders are still encoded below (task requirement) and spot-
#     checked in the self-test, in case a future session needs a non-default
#     contract.
#   - §VIII Prior acts (retro date) held at reference: continuous coverage /
#     retro date = policy inception -> no surcharge. Not separately coded
#     (factor 1, same as TOMICS's held-reference retro date).
#   - §IX Coverage options (liability-only, dedicated limits per location)
#     and §X Extended reporting period: not modeled — standard full-coverage
#     annual policy, no ERP purchase.
#   - STANDARD premium = §V schedule held at 0 (schedule-neutral).
#   - MIN / MAX = standard * (1 -/+ SCHED_CAP); SCHED_CAP = 0.40 is §V's
#     stated cap on the TOTAL modification. We apply the overall +/-40% cap
#     directly (min/max envelope) rather than summing three separate +/-40%
#     sub-components (i. tank management, ii. loss history, iii. individual
#     risk characteristics) — matches the TOMICS/Mid-Continent convention.
#   - ALL THREE (min, standard, max) are floored at the filing's minimum
#     policy premium (facility/policy level, fully earned, not subject to
#     short-rate/prorata): NEW $525 (eff. 08/01/2015) / OLD $500 (pre-2015,
#     back to 04/14/2000).                                             [p.6 §XI]
#   - Era switch is a function of the contract's EFF_DATE (>= 2015-08-01 =>
#     NEW; else OLD) so Script 2 can pass each contract-year's own date.
#     NOTE: this NEW/OLD filing-vintage era is DISTINCT from PM02's
#     2006/2014/2019 `source_era` bucketing — Script 2 computes that
#     separately from panel_year; this engine's `era` field is base-table
#     bookkeeping only.
###############################################################################

library(data.table)

# ── Module constants ────────────────────────────────────────────────────────
SCHED_CAP        <- 0.40                       # p.4 §V: schedule +/-40% total cap
NEW_ERA_CUTOFF   <- as.Date("2015-08-01")      # NEW table effective date
MIN_PREMIUM_NEW  <- 525                        # p.6 §XI: NEW policy minimum
MIN_PREMIUM_OLD  <- 500                        # p.6 §XI: OLD policy minimum (redline)
REF_COVER_OCC    <- 1e6                        # 036-locked reference limit (occ)
REF_COVER_AGG    <- 1e6                        # 036-locked reference limit (agg)
REF_DEDUCTIBLE   <- 5000                       # filing base deductible

###############################################################################
## E1. Era from contract EFF_DATE                                           ##
###############################################################################
era_of_date_aig <- function(eff_date) {
  eff_date <- as.Date(eff_date)
  fifelse(eff_date >= NEW_ERA_CUTOFF, "NEW", "OLD")
}

###############################################################################
## E2. Base premium by age category x era  (p.1, §II)                      ##
##   A 0-5 468/446   B 6-10 625/595   C 11-12 798/760   D 13-14 977/930     ##
##   E 15-16 1155/1100   F 17-19 1523/1450   G 20-23 1943/1850              ##
##   H 24-26 2468/2350   I 27-30 3098/2950   J 31+ 3833/3650   (NEW/OLD)    ##
###############################################################################
base_premium_aig <- function(age_years, era) {
  era <- rep(era, length.out = length(age_years))
  cat_code <- fcase(
    age_years <=  5, "A",
    age_years <= 10, "B",
    age_years <= 12, "C",
    age_years <= 14, "D",
    age_years <= 16, "E",
    age_years <= 19, "F",
    age_years <= 23, "G",
    age_years <= 26, "H",
    age_years <= 30, "I",
    age_years >  30, "J",
    default = NA_character_
  )
  if (anyNA(cat_code)) {
    stop(sprintf("base_premium_aig: invalid age_years: %s",
                  paste(unique(age_years[is.na(cat_code)]), collapse = ", ")))
  }
  new_tbl <- c(A = 468, B = 625, C = 798,  D = 977,  E = 1155,
               F = 1523, G = 1943, H = 2468, I = 3098, J = 3833)
  old_tbl <- c(A = 446, B = 595, C = 760,  D = 930,  E = 1100,
               F = 1450, G = 1850, H = 2350, I = 2950, J = 3650)
  out <- fifelse(era == "NEW", new_tbl[cat_code],
         fifelse(era == "OLD", old_tbl[cat_code], NA_real_))
  if (anyNA(out)) {
    stop(sprintf("base_premium_aig: invalid era value(s): %s",
                  paste(unique(era[is.na(out)]), collapse = ", ")))
  }
  unname(out)
}

###############################################################################
## E3. A) Tank construction  (p.1, §II.A) — select-one, era-invariant       ##
##   DW -.50 | STP3 -.30 | Fiberglass/Clad -.20 | Cathodic -.18             ##
##   Bare Steel +.10 | Relined -.45                                         ##
###############################################################################
construction_load_aig <- function(code) {
  v <- fcase(
    code == "DW",              -0.50,
    code == "STP3",            -0.30,
    code == "FIBERGLASS_CLAD",  -0.20,
    code == "CATHODIC",         -0.18,
    code == "BARE_STEEL",       0.10,
    code == "RELINED",         -0.45,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("construction_load_aig: unrecognized code(s): %s",
                  paste(unique(code[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E4. B) Tank leak detection  (p.1-2, §II.B) — select-one                 ##
##   Interstitial -.05 | ATG -.05 | Monitoring Wells -.02 | SIR +.00        ##
##   Annual Tightness Test +.00 | None +.05                                 ##
###############################################################################
leak_detection_load_aig <- function(code) {
  v <- fcase(
    code == "INTERSTITIAL",     -0.05,
    code == "ATG",               -0.05,
    code == "MONITORING_WELLS",  -0.02,
    code == "SIR",                0.00,
    code == "ANNUAL_TIGHTNESS",   0.00,
    code == "NONE",                0.05,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("leak_detection_load_aig: unrecognized code(s): %s",
                  paste(unique(code[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E5. C) Piping construction  (p.2, §II.C) — select-one                   ##
##   DW piping -.10 | Fiberglass -.05 | External Coating -.02              ##
##   Cathodic -.02 | Bare Steel +.10                                        ##
###############################################################################
pipe_construction_load_aig <- function(code) {
  v <- fcase(
    code == "DW_PIPING",         -0.10,
    code == "FIBERGLASS_PIPING", -0.05,
    code == "EXTERNAL_COATING",  -0.02,
    code == "CATHODIC_PIPING",   -0.02,
    code == "BARE_STEEL_PIPING",  0.10,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("pipe_construction_load_aig: unrecognized code(s): %s",
                  paste(unique(code[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E6. D) Piping leak detection  (p.2, §II.D) — select-one                 ##
##   Interstitial -.05 | Electronic w/ shutoff -.05 | External/wells -.02  ##
##   Mechanical LLD -.02 | Suction check valve -.02 | None +.05             ##
###############################################################################
pipe_leak_load_aig <- function(code) {
  v <- fcase(
    code == "INTERSTITIAL",        -0.05,
    code == "ELECTRONIC_SHUTOFF",  -0.05,
    code == "EXTERNAL_WELLS",      -0.02,
    code == "MECHANICAL_LLD",      -0.02,
    code == "SUCTION_CHECK_VALVE", -0.02,
    code == "NONE",                 0.05,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("pipe_leak_load_aig: unrecognized code(s): %s",
                  paste(unique(code[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E7. §IV.C Multi-tank credit ladder  (p.3-4)                              ##
##   1-5 tanks 0% (confirmed, not a printed row) | 6-14 5% | 15-29 10%      ##
##   30-49 15% | 50-74 20% | 75-99 25% | 100-149 30% | 150+ 40%            ##
##   Era-invariant (identical in NEW and OLD redline text).                 ##
###############################################################################
multi_tank_credit_aig <- function(n_tanks) {
  v <- fcase(
    n_tanks >=   1 & n_tanks <=   5, 0.00,
    n_tanks >=   6 & n_tanks <=  14, 0.05,
    n_tanks >=  15 & n_tanks <=  29, 0.10,
    n_tanks >=  30 & n_tanks <=  49, 0.15,
    n_tanks >=  50 & n_tanks <=  74, 0.20,
    n_tanks >=  75 & n_tanks <=  99, 0.25,
    n_tanks >= 100 & n_tanks <= 149, 0.30,
    n_tanks >= 150,                  0.40,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("multi_tank_credit_aig: invalid n_tanks: %s",
                  paste(unique(n_tanks[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E8. §VI Deductible credit ladder  (p.4) — FULL ladder, base = $5,000.   ##
##   Era-invariant (identical in NEW and OLD redline text).                 ##
###############################################################################
deductible_credit_aig <- function(deductible) {
  v <- fcase(
    deductible ==       5000, 0.00,
    deductible ==      10000, 0.13,
    deductible ==      25000, 0.26,
    deductible ==      50000, 0.41,
    deductible ==     100000, 0.56,
    deductible ==     200000, 0.64,
    deductible ==     250000, 0.69,
    deductible ==     300000, 0.72,
    deductible ==     400000, 0.74,
    deductible ==     500000, 0.76,
    deductible ==    1000000, 0.87,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("deductible_credit_aig: unrecognized deductible(s): %s",
                  paste(unique(deductible[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E9. §VII Increase Limit Factor ladder  (p.4) — FULL ladder,             ##
##   base = $1,000,000/$1,000,000. Era-invariant (identical NEW/OLD text). ##
###############################################################################
ilf_aig <- function(cover_occ, cover_agg) {
  v <- fcase(
    cover_occ ==  5e5 & cover_agg ==  1e6, 0.80,
    cover_occ ==  1e6 & cover_agg ==  1e6, 1.00,
    cover_occ ==  1e6 & cover_agg ==  2e6, 1.15,
    cover_occ ==  1e6 & cover_agg ==  3e6, 1.32,
    cover_occ ==  1e6 & cover_agg ==  4e6, 1.45,
    cover_occ ==  1e6 & cover_agg ==  5e6, 1.60,
    cover_occ ==  2e6 & cover_agg ==  2e6, 1.40,
    cover_occ ==  2e6 & cover_agg ==  3e6, 1.50,
    cover_occ ==  2e6 & cover_agg ==  4e6, 1.60,
    cover_occ ==  2e6 & cover_agg ==  5e6, 1.70,
    cover_occ ==  3e6 & cover_agg ==  3e6, 1.70,
    cover_occ ==  3e6 & cover_agg ==  4e6, 1.80,
    cover_occ ==  3e6 & cover_agg ==  5e6, 1.95,
    cover_occ ==  3e6 & cover_agg ==  6e6, 2.10,
    cover_occ ==  4e6 & cover_agg ==  4e6, 1.95,
    cover_occ ==  4e6 & cover_agg ==  5e6, 2.05,
    cover_occ ==  5e6 & cover_agg ==  5e6, 2.15,
    cover_occ ==  5e6 & cover_agg == 10e6, 2.47,
    cover_occ == 10e6 & cover_agg == 10e6, 3.23,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("ilf_aig: unrecognized limit combination(s): occ=%s agg=%s",
                  paste(unique(cover_occ[is.na(v)]), collapse = ", "),
                  paste(unique(cover_agg[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## SS1 — Facility-level premium builder                                     ##
##                                                                          ##
## tanks_dt: one row per in-service UST tank; required cols:                ##
##   age_years                (integer or numeric)                          ##
##   construction_code         {DW, STP3, FIBERGLASS_CLAD, CATHODIC,        ##
##                              BARE_STEEL, RELINED}                        ##
##   leak_code                 {INTERSTITIAL, ATG, MONITORING_WELLS, SIR,   ##
##                              ANNUAL_TIGHTNESS, NONE}                     ##
##   pipe_construction_code    {DW_PIPING, FIBERGLASS_PIPING,               ##
##                              EXTERNAL_COATING, CATHODIC_PIPING,          ##
##                              BARE_STEEL_PIPING}                          ##
##   pipe_leak_code            {INTERSTITIAL, ELECTRONIC_SHUTOFF,           ##
##                              EXTERNAL_WELLS, MECHANICAL_LLD,             ##
##                              SUCTION_CHECK_VALVE, NONE}                  ##
##                                                                          ##
## eff_date: length-1 Date/character — the contract's effective date;      ##
##   determines NEW vs OLD base table + minimum premium.                   ##
## cover_occ/cover_agg/deductible: facility-level scalars, default to the  ##
##   036-locked reference contract ($1M/$1M, $5,000 deductible).           ##
##                                                                          ##
## Returns: list(min_prem, standard_prem, max_prem, n_tanks_rated, era)    ##
## Band (§V) and floor (§XI) are applied at the facility level.            ##
###############################################################################
aig_facility_premium <- function(tanks_dt, eff_date,
                                  cover_occ  = REF_COVER_OCC,
                                  cover_agg  = REF_COVER_AGG,
                                  deductible = REF_DEDUCTIBLE) {
  stopifnot(length(eff_date) == 1)
  req_cols <- c("age_years", "construction_code", "leak_code",
                "pipe_construction_code", "pipe_leak_code")
  stopifnot(all(req_cols %in% names(tanks_dt)))

  era <- era_of_date_aig(eff_date)

  dt <- copy(tanks_dt)
  dt[, tank_char_load := (
    construction_load_aig(construction_code) +
    leak_detection_load_aig(leak_code) +
    pipe_construction_load_aig(pipe_construction_code) +
    pipe_leak_load_aig(pipe_leak_code)
  )]
  dt[, tank_unmodified := base_premium_aig(age_years, era) * (1 + tank_char_load)]

  n_tanks <- nrow(dt)
  facility_unmodified <- sum(dt$tank_unmodified)

  # §IV: compliance mgmt + remote monitoring held at 0 (unobservable); multi-tank observed
  iv_credit_total <- 0.00 + 0.00 + multi_tank_credit_aig(n_tanks)
  modified_iv <- facility_unmodified * (1 - iv_credit_total)

  # §VI/§VII: combined limit/deductible factor (= 1.00 at the locked reference contract)
  limit_ded_factor <- ilf_aig(cover_occ, cover_agg) - deductible_credit_aig(deductible)

  at_limits_min <- modified_iv * (1 - SCHED_CAP) * limit_ded_factor
  at_limits_std <- modified_iv * 1               * limit_ded_factor
  at_limits_max <- modified_iv * (1 + SCHED_CAP) * limit_ded_factor

  min_floor <- fifelse(era == "NEW", MIN_PREMIUM_NEW, MIN_PREMIUM_OLD)

  mn  <- pmax(at_limits_min, min_floor)
  std <- pmax(at_limits_std, min_floor)
  mx  <- pmax(at_limits_max, min_floor)

  stopifnot(all(is.finite(c(mn, std, mx))))
  stopifnot(mn >= min_floor, std >= min_floor, mx >= min_floor)
  stopifnot(mn <= std, std <= mx)

  list(min_prem = mn, standard_prem = std, max_prem = mx,
       n_tanks_rated = n_tanks, era = era)
}

###############################################################################
## SS2 — Self-test (runs at source time; stopifnot on hand-calc values)    ##
###############################################################################

cat("=== 17a self-test ===\n")

# ── Ladder spot-checks (full §IV/§VI/§VII tables, beyond the base rows) ────
stopifnot(isTRUE(all.equal(multi_tank_credit_aig(30),  0.15)))
stopifnot(isTRUE(all.equal(multi_tank_credit_aig(150), 0.40)))
stopifnot(isTRUE(all.equal(deductible_credit_aig(100000), 0.56)))
stopifnot(isTRUE(all.equal(ilf_aig(2e6, 3e6), 1.50)))
stopifnot(isTRUE(all.equal(ilf_aig(10e6, 10e6), 3.23)))
cat("  ladder spot-checks OK\n")

# ── Case 1: young DW single tank, NEW era, floors at minimum ───────────────
# age=2 -> Cat A, NEW base=468. DW(-.50) + None-leak(+.05) + bare-steel-pipe(+.10)
#   + None-pipe-leak(+.05) = -0.30
#   tank_unmodified = 468*(1-0.30) = 327.60
#   n_tanks=1 -> multi-tank credit 0; IV factor 1; limit/ded factor 1 (defaults)
#   standard = 327.60 -> floor 525 = 525.00
#   min = 327.60*0.60 = 196.56 -> floor 525 = 525.00
#   max = 327.60*1.40 = 458.64 -> floor 525 = 525.00

fake1 <- data.table(
  age_years              = 2L,
  construction_code      = "DW",
  leak_code              = "NONE",
  pipe_construction_code = "BARE_STEEL_PIPING",
  pipe_leak_code         = "NONE"
)
r1 <- aig_facility_premium(fake1, eff_date = as.Date("2018-01-01"))
stopifnot(r1$era == "NEW")
stopifnot(isTRUE(all.equal(r1$min_prem,      525, tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(r1$standard_prem, 525, tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(r1$max_prem,      525, tolerance = 1e-8)))
stopifnot(r1$n_tanks_rated == 1L)

# ── Case 2: mid-age SW single tank, NEW era, above the floor ────────────────
# age=18 -> Cat F, NEW base=1523. Cathodic(-.18) + Interstitial-leak(-.05)
#   + External-coating-pipe(-.02) + Mechanical-LLD-pipe(-.02) = -0.27
#   tank_unmodified = 1523*(1-0.27) = 1111.79
#   standard = 1111.79 ; min = 667.074 ; max = 1556.506 (none floor-bound)

fake2 <- data.table(
  age_years              = 18L,
  construction_code      = "CATHODIC",
  leak_code              = "INTERSTITIAL",
  pipe_construction_code = "EXTERNAL_COATING",
  pipe_leak_code         = "MECHANICAL_LLD"
)
r2 <- aig_facility_premium(fake2, eff_date = as.Date("2020-06-01"))
stopifnot(r2$era == "NEW")
stopifnot(isTRUE(all.equal(r2$standard_prem, 1111.79,  tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r2$min_prem,       667.074, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r2$max_prem,      1556.506, tolerance = 1e-6)))

# ── Case 3: 10-tank facility, NEW era, exercises the multi-tank credit ─────
# Each tank: age=8 -> Cat B, NEW base=625. Bare-steel(+.10) + SIR-leak(+.00)
#   + Cathodic-pipe(-.02) + Suction-check-valve-pipe(-.02) = +0.06
#   tank_unmodified = 625*1.06 = 662.50 ; facility_unmodified = 10*662.50 = 6625.00
#   n_tanks=10 -> 6-14 band -> multi-tank credit 5% -> IV factor 0.95
#   modified_IV = 6625.00*0.95 = 6293.75
#   standard = 6293.75 ; min = 3776.25 ; max = 8811.25

fake3 <- data.table(
  age_years              = rep(8L, 10),
  construction_code      = rep("BARE_STEEL", 10),
  leak_code              = rep("SIR", 10),
  pipe_construction_code = rep("CATHODIC_PIPING", 10),
  pipe_leak_code         = rep("SUCTION_CHECK_VALVE", 10)
)
r3 <- aig_facility_premium(fake3, eff_date = as.Date("2019-03-01"))
stopifnot(r3$era == "NEW")
stopifnot(r3$n_tanks_rated == 10L)
stopifnot(isTRUE(all.equal(r3$standard_prem, 6293.75, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r3$min_prem,      3776.25, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r3$max_prem,      8811.25, tolerance = 1e-6)))

# ── Case 4: OLD era single tank — min floors, standard/max do not ──────────
# age=25 -> Cat H, OLD base=2350. Relined(-.45) + ATG-leak(-.05)
#   + DW-piping(-.10) + Electronic-shutoff-pipe(-.05) = -0.65
#   tank_unmodified = 2350*(1-0.65) = 822.50
#   standard = 822.50 (above floor 500)
#   min = 822.50*0.60 = 493.50 -> floors to 500 (below floor)
#   max = 822.50*1.40 = 1151.50 (above floor)

fake4 <- data.table(
  age_years              = 25L,
  construction_code      = "RELINED",
  leak_code              = "ATG",
  pipe_construction_code = "DW_PIPING",
  pipe_leak_code         = "ELECTRONIC_SHUTOFF"
)
r4 <- aig_facility_premium(fake4, eff_date = as.Date("2010-01-01"))
stopifnot(r4$era == "OLD")
stopifnot(isTRUE(all.equal(r4$standard_prem, 822.5,  tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(r4$min_prem,      500,    tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(r4$max_prem,      1151.5, tolerance = 1e-8)))

cat("17a self-test PASS\n")
