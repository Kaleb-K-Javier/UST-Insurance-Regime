###############################################################################
# 18a_engine_ace.R — ACE rate engine: pure functions + self-test
# Filing:  ACE 2011 "TankSafe Rating Manual 2-11.pdf", Section B (UST).
#          STIP3 (Double-Walled / Clad-Steel) credit = 50% per the 2012
#          correction (supersedes any earlier printed STIP3 value).
#          TX schedule +/-40% cap: "Schedule Rating State Max Appendix A.pdf".
# Sources: library(data.table) only — no data, no I/O.
#
# COMPOSITION RULE (researcher-confirmed 2026-07-01, one unified manual):
#   tank_standard      = Base(age_years) * (1 - construction_credit)   [Sec. B]
#   facility_unmodified= sum(tank_standard) across the facility's rated tanks
#   facility_standard  = facility_unmodified * (1 - multi_tank_credit(n_tanks))
#   TX schedule (Appendix A discretionary ranges) held at 0 for STANDARD; the
#   filed +/-40% aggregate cap defines MIN/MAX directly (not summed schedule
#   items) — same convention as the AIG/TOMICS engines.
#
# FULL ORDERED FORMULA (facility/policy level):
#   1. tank_standard   = Base(age_years) * (1 - construction_credit(code))
#   2. facility_unmodified = sum(tank_standard) over the facility's rated tanks
#   3. facility_standard   = facility_unmodified * (1 - multi_tank_credit(n_tanks))
#   4. facility_min/std/max = facility_standard * (1 -/1/+ SCHED_CAP)   [Appendix A +/-40%]
#   5. facility_premium = pmax(facility_min/std/max, MIN_PREMIUM)  [$350 policy min, incl TRIA]
#
# BEHAVIORAL ASSUMPTIONS (held at the filing's default/reference contract —
# firm-choice items we do not observe/guess, per RATE_ENGINES_README §1 /
# SESSION_PROMPT_040 §1):
#   - Deductible $5,000 = the filing base -> factor 1.000. Coverage limit FIXED
#     at $1,000,000/$1,000,000 (036-locked) -> ILF = 1.000. Only this single
#     reference point is confirmed for ACE — no deductible/ILF ladder was
#     transcribed (none given). The builder below hard-stops if called with a
#     non-reference cover/deductible rather than fabricate a ladder value.
#   - TRIA load = 0: embedded in the base rates per the 2015 correction, not a
#     separate line item.
#   - Retro date = policy inception -> no prior-acts surcharge. Policy term =
#     1 yr (1.00). Both are no-op multipliers of 1 at these defaults and are
#     not separately coded (nothing to look up).
#   - TX schedule rating (Appendix A): Marina/Airport, prior contamination,
#     >30k gal, non-regulated, loading/unloading, ethanol, >50yr,
#     leak-detection (-10%..+10%), risk-management (-25%..+25%) are
#     DISCRETIONARY ranges, not fixed filed values -> held at 0 for STANDARD,
#     including the ones that look "observable" (researcher instruction: do
#     not apply even those). The filed +/-40% aggregate cap defines MIN/MAX.
#   - No era/backcast switch: ACE is one unified manual across all active TX
#     years; STIP3 = 50% throughout (unlike the AIG/TOMICS NEW/OLD splits).
#   - ALL THREE (min, standard, max) are floored at the filed minimum policy
#     premium: $350 (facility/policy level, TRIA-inclusive).
###############################################################################

library(data.table)

# ── Module constants ────────────────────────────────────────────────────────
SCHED_CAP      <- 0.40   # Appendix A: TX schedule +/-40% aggregate cap
MIN_PREMIUM    <- 350    # filed minimum policy premium, incl. TRIA
REF_COVER_OCC  <- 1e6    # 036-locked reference limit (occ)
REF_COVER_AGG  <- 1e6    # 036-locked reference limit (agg)
REF_DEDUCTIBLE <- 5000   # filing base deductible

###############################################################################
## E1. Base premium by tank age  (Sec. B; per tank, $1M/$1M, $5k ded)       ##
##   0-4=300 | 5=316 | 6=392 | 7=468 | 8=544 | 9=620 | 10=696                ##
##   11+ : 696 + 76*(age-10)                                                 ##
###############################################################################
base_premium_ace <- function(age_years) {
  if (anyNA(age_years) || any(age_years < 0)) {
    stop(sprintf("base_premium_ace: invalid age_years: %s",
                  paste(unique(age_years[is.na(age_years) | age_years < 0]),
                        collapse = ", ")))
  }
  fcase(
    age_years <=  4, 300,
    age_years ==  5, 316,
    age_years ==  6, 392,
    age_years ==  7, 468,
    age_years ==  8, 544,
    age_years ==  9, 620,
    age_years == 10, 696,
    age_years >= 11, 696 + 76 * (age_years - 10)
  )
}

###############################################################################
## E2. Tank construction credit  (Sec. B) — select-one, mutually exclusive  ##
##   Double-Walled / Clad-Steel (STIP3, 2012 correction) = 50%              ##
##   Fiberglass = 20% | Bare Steel = 0%                                     ##
###############################################################################
construction_credit_ace <- function(code) {
  v <- fcase(
    code == "STIP3",       0.50,
    code == "FIBERGLASS",  0.20,
    code == "BARE_STEEL",  0.00,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("construction_credit_ace: unrecognized code(s): %s",
                  paste(unique(code[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## E3. Multiple-tank credit ladder  (facility tank count)                   ##
##   0-9=0% | 10-19=5% | 20-49=15% | 50-74=20% | 75-100=30% | 101-200=40%   ##
##   201-300=50% | 301+=60%                                                 ##
###############################################################################
multi_tank_credit_ace <- function(n_tanks) {
  v <- fcase(
    n_tanks >=   0 & n_tanks <=   9, 0.00,
    n_tanks >=  10 & n_tanks <=  19, 0.05,
    n_tanks >=  20 & n_tanks <=  49, 0.15,
    n_tanks >=  50 & n_tanks <=  74, 0.20,
    n_tanks >=  75 & n_tanks <= 100, 0.30,
    n_tanks >= 101 & n_tanks <= 200, 0.40,
    n_tanks >= 201 & n_tanks <= 300, 0.50,
    n_tanks >= 301,                  0.60,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("multi_tank_credit_ace: invalid n_tanks: %s",
                  paste(unique(n_tanks[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## SS1 — Facility-level premium builder                                     ##
##                                                                          ##
## tanks_dt: one row per in-service UST tank; required cols:                ##
##   age_years          (integer or numeric, >= 0)                          ##
##   construction_code   {STIP3, FIBERGLASS, BARE_STEEL}                    ##
##                                                                          ##
## cover_occ/cover_agg/deductible: facility-level scalars. ACE has only a   ##
##   single confirmed reference point (no ladder transcribed) -> passing    ##
##   anything other than the 036-locked reference hard-stops rather than    ##
##   fabricate a factor.                                                    ##
##                                                                          ##
## Returns: list(min_prem, standard_prem, max_prem, n_tanks_rated)          ##
## Band (Appendix A) and floor (filed minimum) applied at facility level.  ##
###############################################################################
ace_facility_premium <- function(tanks_dt,
                                  cover_occ  = REF_COVER_OCC,
                                  cover_agg  = REF_COVER_AGG,
                                  deductible = REF_DEDUCTIBLE) {
  req_cols <- c("age_years", "construction_code")
  stopifnot(all(req_cols %in% names(tanks_dt)))

  if (!isTRUE(all.equal(cover_occ, REF_COVER_OCC)) ||
      !isTRUE(all.equal(cover_agg, REF_COVER_AGG)) ||
      !isTRUE(all.equal(deductible, REF_DEDUCTIBLE))) {
    stop("ace_facility_premium: only the 036-locked reference contract ($1M/$1M, ",
         "$5,000 deductible) is confirmed for ACE — no ILF/deductible ladder was ",
         "transcribed. Refusing to fabricate a factor for a non-reference contract.")
  }

  dt <- copy(tanks_dt)
  dt[, tank_standard := base_premium_ace(age_years) *
                         (1 - construction_credit_ace(construction_code))]

  n_tanks <- nrow(dt)
  facility_unmodified <- sum(dt$tank_standard)

  facility_standard <- facility_unmodified * (1 - multi_tank_credit_ace(n_tanks))

  at_min <- facility_standard * (1 - SCHED_CAP)
  at_std <- facility_standard
  at_max <- facility_standard * (1 + SCHED_CAP)

  mn  <- pmax(at_min, MIN_PREMIUM)
  std <- pmax(at_std, MIN_PREMIUM)
  mx  <- pmax(at_max, MIN_PREMIUM)

  stopifnot(all(is.finite(c(mn, std, mx))))
  stopifnot(mn >= MIN_PREMIUM, std >= MIN_PREMIUM, mx >= MIN_PREMIUM)
  stopifnot(mn <= std, std <= mx)

  list(min_prem = mn, standard_prem = std, max_prem = mx, n_tanks_rated = n_tanks)
}

###############################################################################
## SS2 — Self-test (runs at source time; stopifnot on hand-calc values)    ##
###############################################################################

cat("=== 18a self-test ===\n")

# ── Ladder spot-checks (base table + multi-tank band tiling) ───────────────
stopifnot(isTRUE(all.equal(base_premium_ace(4),   300)))
stopifnot(isTRUE(all.equal(base_premium_ace(5),   316)))
stopifnot(isTRUE(all.equal(base_premium_ace(10),  696)))
stopifnot(isTRUE(all.equal(base_premium_ace(11),  772)))
stopifnot(isTRUE(all.equal(base_premium_ace(20), 1456)))
stopifnot(isTRUE(all.equal(construction_credit_ace("STIP3"),      0.50)))
stopifnot(isTRUE(all.equal(construction_credit_ace("FIBERGLASS"), 0.20)))
stopifnot(isTRUE(all.equal(construction_credit_ace("BARE_STEEL"), 0.00)))
stopifnot(isTRUE(all.equal(multi_tank_credit_ace(9),    0.00)))
stopifnot(isTRUE(all.equal(multi_tank_credit_ace(10),   0.05)))
stopifnot(isTRUE(all.equal(multi_tank_credit_ace(49),   0.15)))
stopifnot(isTRUE(all.equal(multi_tank_credit_ace(50),   0.20)))
stopifnot(isTRUE(all.equal(multi_tank_credit_ace(100),  0.30)))
stopifnot(isTRUE(all.equal(multi_tank_credit_ace(101),  0.40)))
stopifnot(isTRUE(all.equal(multi_tank_credit_ace(300),  0.50)))
stopifnot(isTRUE(all.equal(multi_tank_credit_ace(301),  0.60)))
cat("  ladder spot-checks OK\n")

# ── Case 1: young DW/STIP3 single tank — floors at $350 on all three ──────
# age=2 -> 0-4 band -> base=300. STIP3 credit=0.50.
#   tank_standard = 300*(1-0.50) = 150.00
#   n_tanks=1 -> 0-9 band -> multi-tank credit 0% -> facility_standard = 150.00
#   std = 150.00 -> floor 350 = 350.00
#   min = 150.00*0.60 =  90.00 -> floor 350 = 350.00
#   max = 150.00*1.40 = 210.00 -> floor 350 = 350.00

fake1 <- data.table(age_years = 2L, construction_code = "STIP3")
r1 <- ace_facility_premium(fake1)
stopifnot(isTRUE(all.equal(r1$min_prem,      350, tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(r1$standard_prem, 350, tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(r1$max_prem,      350, tolerance = 1e-8)))
stopifnot(r1$n_tanks_rated == 1L)

# ── Case 2: mid-age bare-steel single tank — above the floor ───────────────
# age=15 -> >=11 -> base = 696 + 76*(15-10) = 696+380 = 1076. Bare Steel credit=0.
#   tank_standard = 1076*(1-0) = 1076.00
#   n_tanks=1 -> multi-tank credit 0% -> facility_standard = 1076.00
#   std = 1076.00 ; min = 1076.00*0.60 = 645.60 ; max = 1076.00*1.40 = 1506.40
#   (none floor-bound)

fake2 <- data.table(age_years = 15L, construction_code = "BARE_STEEL")
r2 <- ace_facility_premium(fake2)
stopifnot(isTRUE(all.equal(r2$standard_prem, 1076.00, tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(r2$min_prem,       645.60, tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(r2$max_prem,      1506.40, tolerance = 1e-8)))

# ── Case 3: 15-tank fiberglass facility — exercises the 10-19 (5%) band ────
# Each tank: age=7 -> base=468. Fiberglass credit=0.20.
#   tank_standard = 468*(1-0.20) = 374.40 ; facility_unmodified = 15*374.40 = 5616.00
#   n_tanks=15 -> 10-19 band -> multi-tank credit 5% -> facility_standard = 5616.00*0.95 = 5335.20
#   std = 5335.20 ; min = 5335.20*0.60 = 3201.12 ; max = 5335.20*1.40 = 7469.28

fake3 <- data.table(age_years = rep(7L, 15), construction_code = rep("FIBERGLASS", 15))
r3 <- ace_facility_premium(fake3)
stopifnot(r3$n_tanks_rated == 15L)
stopifnot(isTRUE(all.equal(r3$standard_prem, 5335.20, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r3$min_prem,      3201.12, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r3$max_prem,      7469.28, tolerance = 1e-6)))

# ── Case 4: 350-tank STIP3 facility — worst-case, exercises the 301+ (60%) ─
#     band and confirms the +40% MAX is engaged (not floor-bound) at scale.
# Each tank: age=30 -> base = 696 + 76*(30-10) = 696+1520 = 2216. STIP3 credit=0.50.
#   tank_standard = 2216*(1-0.50) = 1108.00 ; facility_unmodified = 350*1108.00 = 387800.00
#   n_tanks=350 -> 301+ band -> multi-tank credit 60% -> facility_standard = 387800.00*0.40 = 155120.00
#   std = 155120.00 ; min = 155120.00*0.60 = 93072.00 ; max = 155120.00*1.40 = 217168.00

fake4 <- data.table(age_years = rep(30L, 350), construction_code = rep("STIP3", 350))
r4 <- ace_facility_premium(fake4)
stopifnot(r4$n_tanks_rated == 350L)
stopifnot(isTRUE(all.equal(r4$standard_prem, 155120.00, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r4$min_prem,       93072.00, tolerance = 1e-6)))
stopifnot(isTRUE(all.equal(r4$max_prem,      217168.00, tolerance = 1e-6)))

# ── Case 5: non-reference contract hard-stops rather than fabricate a ladder
stopifnot(inherits(
  tryCatch(ace_facility_premium(fake1, deductible = 10000), error = function(e) e),
  "error"
))
cat("  non-reference-contract hard-stop OK\n")

cat("18a self-test PASS\n")
