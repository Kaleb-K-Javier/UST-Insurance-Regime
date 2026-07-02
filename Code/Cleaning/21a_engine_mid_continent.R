###############################################################################
# 21a_engine_mid_continent.R — Mid-Continent Casualty rate engine: pure
#                               functions + self-test (Ticket 053)
#
# PROVENANCE: extracted from Code/Dynamic_Model/04a_TX_Premium_All_1999_
# onwards.R, which itself duplicated the era-specific rating tables from
# Code/Cleaning/12_Rate_MidCont_2006_2011.R, 12_Rate_MidCont_2014_2019.R,
# 13_Rate_MidCont_2019_2021.R, 14_Rate_MidCont_2021_onwards.R. This file is
# the FIRST time those four era tables are consolidated into one pure-
# function module in the two-script standard (15a-19a) — 04a itself was a
# monolithic "apply Mid-Continent rates to every TX facility" script, not a
# pricing-function library.
#
# RATE MATH: byte-identical to 04a's Section 2/3 (constants, per-era factor
# tables, apply_engine() dispatch) — same breakpoints, same factor values.
#
# NA HANDLING — ONE DELIBERATE DEVIATION FROM 04a: 04a summed the six load
# components via `rowSums(cbind(...), na.rm = TRUE)`, which silently treats
# an unrecognized tank_status (04a's own derivation defaults unmapped STATUS
# values to "Unknown", which status_factor() maps to NA) as a 0 contribution
# rather than erroring. That was tolerable for 04a's "impute for every TX
# facility regardless of actual carrier" universe, where many facilities
# never had a real observed status. This engine prices ACTUAL Mid-Continent
# contracts only (shared panel rows a carrier priceably insured), and
# CLAUDE.md's hard-error-propagation rule + every sibling engine's own
# convention (15a-19a all `stop()` on an unrecognized categorical code
# rather than silently zeroing it) both argue for the same here:
# status_factor_midcont() now `stop()`s on any tank_status value outside the
# three recognized labels, instead of contributing a silent 0.
#
# Sources: library(data.table) only — no data, no I/O.
###############################################################################

library(data.table)

# ── Module constants (constant across all four eras) ───────────────────────
BASE_RATE_MC       <- 300L    # 04a: BASE_RATE
SCHED_CAP          <- 0.40    # same +/-40% convention as every other engine
                               # (04a's SCHED_MIN_FACTOR/SCHED_MAX_FACTOR = 0.60/1.40)
POLICY_MIN_PREMIUM <- 500L    # HITL FLAG (unresolved, carried from 04a verbatim):
                               # 04a's own Section 8 comment flags this as
                               # "PROPOSED $500/facility-month floor, per
                               # POLICY_MIN_PREMIUM <- 500 in 12_/13_/14_
                               # Rate_MidCont_*.R (identical across all four
                               # era scripts). HITL FLAG: that constant is
                               # never actually applied as a pmax() floor
                               # anywhere in 04a's own fac_year rollup —
                               # confirm $500 against the underlying filing
                               # before treating this as authoritative."
                               # Carried forward unresolved — flag to the
                               # researcher, do not treat as verified.
REF_COVER_OCC      <- 1e6     # 036-locked reference limit (occ)
REF_COVER_AGG      <- 1e6     # 036-locked reference limit (agg)
REF_DEDUCTIBLE     <- 5000    # filing base deductible ($5k, 2019/2021 tables)
REF_COVER_FORM     <- "A"     # filing base coverage form (2019/2021 tables)

ERA_R2008_EFF <- as.Date("2014-05-01")   # era_2006 -> era_2014
ERA_R2010_EFF <- as.Date("2019-02-01")   # era_2014 -> era_2019
ERA_R2011_EFF <- as.Date("2021-05-01")   # era_2019 -> era_2021

###############################################################################
## era_of_eff_date_midcont — assign rate era from contract EFF_DATE          ##
##   era_2006 covers everything before 2014-05-01, INCLUDING any pre-2006   ##
##   contract (04a's "legal-fixed-rate" backward-extrapolation assumption:  ##
##   TX rate filings do not move with inflation, so the earliest filed      ##
##   schedule is the defensible choice for any earlier date). This engine   ##
##   prices REAL contracts only, so any pre-2006 case would be an actual    ##
##   observed FR contract, not a 04a-style universal imputation.            ##
###############################################################################
era_of_eff_date_midcont <- function(eff_date) {
  eff_date <- as.Date(eff_date)
  fcase(
    eff_date <  ERA_R2008_EFF, "era_2006",
    eff_date <  ERA_R2010_EFF, "era_2014",
    eff_date <  ERA_R2011_EFF, "era_2019",
    default                   = "era_2021"
  )
}

to_bool <- function(x) !is.na(x) & as.logical(x)

###############################################################################
## Status factor — 04a Section 2 (STATUS -> canonical label, mapped here)   ##
##   In Use = 0.00 | Temporarily Out of Use = -0.50 |                       ##
##   Permanently Out of Use = -0.75                                         ##
##   DEVIATION FROM 04a: stop()s on any other value (see header) instead of ##
##   contributing silent 0 via NA + rowSums(na.rm=TRUE).                    ##
###############################################################################
status_factor_midcont <- function(tank_status) {
  v <- fcase(
    tank_status == "In Use",                  0.00,
    tank_status == "Temporarily Out of Use", -0.50,
    tank_status == "Permanently Out of Use", -0.75,
    default = NA_real_
  )
  if (anyNA(v)) {
    stop(sprintf("status_factor_midcont: unrecognized tank_status value(s): %s",
                  paste(unique(tank_status[is.na(v)]), collapse = ", ")))
  }
  v
}

###############################################################################
## Construction / leak / piping factors — 04a Section 2, byte-identical.    ##
## Boolean-flag inputs (to_bool() coerces NA/non-logical to FALSE) are      ##
## exhaustively covered by their fcase() -> no unrecognized-value case is   ##
## possible here (unlike status, which takes an open-ended string).        ##
###############################################################################
construction_factor_midcont <- function(is_steel_cathodic, is_reinforced_fiberglass,
                                         is_double_walled_steel) {
  dw  <- to_bool(is_double_walled_steel)
  sc  <- to_bool(is_steel_cathodic)
  frp <- to_bool(is_reinforced_fiberglass)
  fcase(dw, -0.20, sc | frp, 0.00, default = 0.00)
}

leak_factor_midcont <- function(det_interstitial, det_ATG, det_vapor, det_groundwater) {
  di <- to_bool(det_interstitial); da <- to_bool(det_ATG)
  dv <- to_bool(det_vapor);        dg <- to_bool(det_groundwater)
  fcase(di, -0.20, da, -0.10, dv | dg, -0.10, default = 0.00)
}

pipe_const_factor_midcont <- function(pip_steel_cathodic, pip_fiberglass, pip_double_walled) {
  st <- to_bool(pip_steel_cathodic); fb <- to_bool(pip_fiberglass)
  dw <- to_bool(pip_double_walled)
  fcase(st, +0.10, dw, -0.10, fb, 0.00, default = 0.00)
}

pipe_design_factor_midcont <- function(pip_suction, pip_pressure_ll) 0  # neutral, all eras (04a)

###############################################################################
## Era-specific ILF + age tables — 04a Section 2, byte-identical.           ##
## NOTE: unlike the other five carriers, Mid-Continent's OWN filed ILF      ##
## table does not evaluate to 1.00 at the $1M/$1M reference (it is 1.18) —  ##
## this is the filed number (04a ilf_factor_2006/2014), not an error. The   ##
## 036-locked "reference contract" invariant holds coverage TERMS fixed     ##
## across carriers; it does not require every carrier's own factor table   ##
## to normalize to 1.00 at that point.                                     ##
###############################################################################
ilf_factor_2006_midcont <- function(occ, agg) {
  occ <- fifelse(is.na(occ), 1e6, occ)
  agg <- fifelse(is.na(agg), 1e6, agg)
  fcase(occ == 1e6 & agg == 1e6, 1.18,
        occ == 1e6 & agg == 2e6, 1.20,
        default = 1.00)
}

age_factor_2006_midcont <- function(a) {
  v <- fcase(
    a <=  5, -0.10, a <= 10,  0.00, a <= 15,  0.05,
    a <= 20,  0.10, a <= 25,  0.20, a >  25,  0.25,
    default = NA_real_
  )
  if (anyNA(v)) stop(sprintf("age_factor_2006_midcont: invalid age_years: %s",
                              paste(unique(a[is.na(v)]), collapse = ", ")))
  v
}

ilf_factor_2014_midcont <- function(occ, agg) {
  occ <- fifelse(is.na(occ), 1e6, occ)
  agg <- fifelse(is.na(agg), 1e6, agg)
  fcase(occ == 1e6 & agg == 1e6, 1.18,
        occ == 1e6 & agg == 2e6, 1.20,
        default = 1.18)
}

age_factor_2014_midcont <- function(a) {
  v <- fcase(
    a <=  5, -0.10, a <= 10,  0.00, a <= 15,  0.05,
    a <= 20,  0.10, a <= 25,  0.20, a <= 30,  0.25,
    a <= 35,  0.35, a <= 40,  0.45, a <= 45,  0.55,
    a <= 50,  0.65, a >  50,  0.75,
    default = NA_real_
  )
  if (anyNA(v)) stop(sprintf("age_factor_2014_midcont: invalid age_years: %s",
                              paste(unique(a[is.na(v)]), collapse = ", ")))
  v
}

# 2019/2021 share the same ILF table as 2014 (04a: `ilf_factor_2019 <- ilf_factor_2014`)
ilf_factor_2019_midcont <- ilf_factor_2014_midcont

age_factor_2019_midcont <- function(a) {
  v <- fcase(
    a <=  2, -0.20, a <=  4, -0.16, a <=  6, -0.12, a <=  8, -0.08,
    a <= 10, -0.04, a <= 15,  0.00, a <= 20,  0.10, a <= 25,  0.20,
    a <= 30,  0.25, a <= 35,  0.35, a <= 40,  0.45, a <= 45,  0.55,
    a <= 50,  0.65, a >  50,  0.75,
    default = NA_real_
  )
  if (anyNA(v)) stop(sprintf("age_factor_2019_midcont: invalid age_years: %s",
                              paste(unique(a[is.na(v)]), collapse = ", ")))
  v
}

deduct_factor_2019_midcont <- function(ded) {
  fcase(ded <=  5e3 | is.na(ded), 1.00,
        ded <= 10e3,              0.92,
        ded <= 25e3,              0.80,
        ded <= 50e3,              0.60,
        ded >  50e3,              0.40)
}

coverage_factor_2019_midcont <- function(form) {
  fcase(toupper(form) == "B", 1.50,
        toupper(form) == "C", 1.25,
        default = 1.00)
}

# 2021 is byte-identical to 2019 in the rating tables (04a comment: "2021 is
# byte-identical to 2019 in the rating tables — share definitions.")
age_factor_2021_midcont      <- age_factor_2019_midcont
ilf_factor_2021_midcont      <- ilf_factor_2019_midcont
deduct_factor_2021_midcont   <- deduct_factor_2019_midcont
coverage_factor_2021_midcont <- coverage_factor_2019_midcont

###############################################################################
## price_mid_continent_tank — pure per-tank premium (Ticket 053, LAYER 2)   ##
##   Vectorized over rows of the shared priced-tank panel. era must be      ##
##   pre-computed by the caller (era_of_eff_date_midcont(eff_date)) since   ##
##   it depends on contract EFF_DATE, same convention as price_aig_tank().  ##
##   cover_occ/cover_agg/deductible/cover_form default to the 036-locked    ##
##   reference contract, at which the 2019/2021 deductible+coverage factor  ##
##   is a no-op (1.00 x 1.00), matching every sibling engine's card         ##
##   invariant (self-tested below).                                        ##
###############################################################################
price_mid_continent_tank <- function(age_years, tank_status, era,
                                      is_steel_cathodic, is_reinforced_fiberglass,
                                      is_double_walled_steel,
                                      det_interstitial, det_ATG, det_vapor, det_groundwater,
                                      pip_steel_cathodic, pip_fiberglass, pip_double_walled,
                                      cover_occ  = REF_COVER_OCC,
                                      cover_agg  = REF_COVER_AGG,
                                      deductible = REF_DEDUCTIBLE,
                                      cover_form = REF_COVER_FORM) {
  if (!all(era %in% c("era_2006", "era_2014", "era_2019", "era_2021"))) {
    stop(sprintf("price_mid_continent_tank: invalid era value(s): %s",
                  paste(unique(era[!era %in% c("era_2006", "era_2014", "era_2019", "era_2021")]),
                        collapse = ", ")))
  }

  n <- length(age_years)
  era <- rep(era, length.out = n)

  status_load <- status_factor_midcont(tank_status)
  age_load <- rep(NA_real_, n)
  ilf      <- rep(NA_real_, n)
  for (e in unique(era)) {
    i <- era == e
    age_load[i] <- switch(e,
      era_2006 = age_factor_2006_midcont(age_years[i]),
      era_2014 = age_factor_2014_midcont(age_years[i]),
      era_2019 = age_factor_2019_midcont(age_years[i]),
      era_2021 = age_factor_2021_midcont(age_years[i]))
    ilf[i] <- switch(e,
      era_2006 = ilf_factor_2006_midcont(cover_occ[i], cover_agg[i]),
      era_2014 = ilf_factor_2014_midcont(cover_occ[i], cover_agg[i]),
      era_2019 = ilf_factor_2019_midcont(cover_occ[i], cover_agg[i]),
      era_2021 = ilf_factor_2021_midcont(cover_occ[i], cover_agg[i]))
  }

  cons_load <- construction_factor_midcont(is_steel_cathodic, is_reinforced_fiberglass,
                                            is_double_walled_steel)
  leak_load <- leak_factor_midcont(det_interstitial, det_ATG, det_vapor, det_groundwater)
  pc_load   <- pipe_const_factor_midcont(pip_steel_cathodic, pip_fiberglass, pip_double_walled)
  pd_load   <- pipe_design_factor_midcont(NA, NA)   # always 0 (04a: neutral, all eras)

  sec_load <- status_load + age_load + cons_load + leak_load + pc_load + pd_load
  tank_premium <- BASE_RATE_MC * ilf * (1 + sec_load)

  # 2019/2021 additionally multiply by deductible + coverage-form factors
  # (04a Section 3). At the 036-locked reference ($5,000 / "A") both = 1.00.
  is_2019_2021 <- era %in% c("era_2019", "era_2021")
  if (any(is_2019_2021)) {
    df <- rep(1.0, n); cf <- rep(1.0, n)
    df[is_2019_2021] <- ifelse(era[is_2019_2021] == "era_2019",
                                deduct_factor_2019_midcont(deductible[is_2019_2021]),
                                deduct_factor_2021_midcont(deductible[is_2019_2021]))
    cf[is_2019_2021] <- ifelse(era[is_2019_2021] == "era_2019",
                                coverage_factor_2019_midcont(cover_form[is_2019_2021]),
                                coverage_factor_2021_midcont(cover_form[is_2019_2021]))
    tank_premium <- tank_premium * df * cf
  }

  tank_premium
}

###############################################################################
## Self-test (runs at source time; stopifnot on hand-calc values)           ##
###############################################################################
cat("=== 21a self-test ===\n")

# ── Ladder / table spot-checks ──────────────────────────────────────────────
stopifnot(isTRUE(all.equal(status_factor_midcont("In Use"), 0.00)))
stopifnot(isTRUE(all.equal(status_factor_midcont("Temporarily Out of Use"), -0.50)))
stopifnot(isTRUE(all.equal(status_factor_midcont("Permanently Out of Use"), -0.75)))
stopifnot(inherits(tryCatch(status_factor_midcont("Unknown"), error = function(e) e), "error"))
stopifnot(isTRUE(all.equal(ilf_factor_2006_midcont(1e6, 1e6), 1.18)))
stopifnot(isTRUE(all.equal(ilf_factor_2014_midcont(1e6, 2e6), 1.20)))
stopifnot(isTRUE(all.equal(age_factor_2019_midcont(3),  -0.16)))
stopifnot(isTRUE(all.equal(deduct_factor_2019_midcont(10000), 0.92)))
stopifnot(isTRUE(all.equal(coverage_factor_2019_midcont("B"), 1.50)))
cat("  ladder spot-checks OK\n")

# ── Case 1: era_2006, In Use, age=8, no construction/leak/pipe loads,       ─
#     reference $1M/$1M -> ILF 1.18 (MidCont's own filed reference point,    ─
#     NOT 1.00 — see header note) ──────────────────────────────────────────
# sec_load = 0(status) + 0(age, 8<=10) + 0(cons) + 0(leak) + 0(pipe_const) + 0 = 0
# tank_premium = 300 * 1.18 * (1+0) = 354.00
pt1 <- price_mid_continent_tank(
  age_years = 8, tank_status = "In Use", era = "era_2006",
  is_steel_cathodic = FALSE, is_reinforced_fiberglass = FALSE, is_double_walled_steel = FALSE,
  det_interstitial = FALSE, det_ATG = FALSE, det_vapor = FALSE, det_groundwater = FALSE,
  pip_steel_cathodic = FALSE, pip_fiberglass = FALSE, pip_double_walled = FALSE
)
stopifnot(isTRUE(all.equal(pt1, 354.00, tolerance = 1e-8)))

# ── Case 2: era_2006, In Use, age=8, DW steel(-0.20) + interstitial(-0.20)  ─
#     + steel-cathodic pipe(+0.10) ─────────────────────────────────────────
# sec_load = 0 + 0 - 0.20 - 0.20 + 0.10 + 0 = -0.30
# tank_premium = 300 * 1.18 * (1-0.30) = 354 * 0.70 = 247.80
pt2 <- price_mid_continent_tank(
  age_years = 8, tank_status = "In Use", era = "era_2006",
  is_steel_cathodic = FALSE, is_reinforced_fiberglass = FALSE, is_double_walled_steel = TRUE,
  det_interstitial = TRUE, det_ATG = FALSE, det_vapor = FALSE, det_groundwater = FALSE,
  pip_steel_cathodic = TRUE, pip_fiberglass = FALSE, pip_double_walled = FALSE
)
stopifnot(isTRUE(all.equal(pt2, 247.80, tolerance = 1e-6)))

# ── Case 3: era_2014, Temporarily Out of Use(-0.50), age=30(0.25),          ─
#     $1M/$2M -> ILF 1.20 ──────────────────────────────────────────────────
# sec_load = -0.50 + 0.25 + 0 + 0 + 0 + 0 = -0.25
# tank_premium = 300 * 1.20 * (1-0.25) = 360 * 0.75 = 270.00
pt3 <- price_mid_continent_tank(
  age_years = 30, tank_status = "Temporarily Out of Use", era = "era_2014",
  is_steel_cathodic = FALSE, is_reinforced_fiberglass = FALSE, is_double_walled_steel = FALSE,
  det_interstitial = FALSE, det_ATG = FALSE, det_vapor = FALSE, det_groundwater = FALSE,
  pip_steel_cathodic = FALSE, pip_fiberglass = FALSE, pip_double_walled = FALSE,
  cover_occ = 1e6, cover_agg = 2e6
)
stopifnot(isTRUE(all.equal(pt3, 270.00, tolerance = 1e-6)))

# ── Case 4: era_2019, Permanently Out of Use(-0.75), age=3(-0.16),          ─
#     $1M/$1M -> ILF 1.18 (2019 shares the 2014 table), deductible=$10k     ─
#     (0.92), coverage form "B" (1.50) ─────────────────────────────────────
# sec_load = -0.75 - 0.16 + 0 + 0 + 0 + 0 = -0.91
# tank_premium = 300*1.18*(1-0.91) = 354*0.09 = 31.86
#   31.86 * deduct(0.92) * coverage(1.50) = 31.86*0.92=29.3112; *1.50=43.9668
pt4 <- price_mid_continent_tank(
  age_years = 3, tank_status = "Permanently Out of Use", era = "era_2019",
  is_steel_cathodic = FALSE, is_reinforced_fiberglass = FALSE, is_double_walled_steel = FALSE,
  det_interstitial = FALSE, det_ATG = FALSE, det_vapor = FALSE, det_groundwater = FALSE,
  pip_steel_cathodic = FALSE, pip_fiberglass = FALSE, pip_double_walled = FALSE,
  cover_occ = 1e6, cover_agg = 1e6, deductible = 10000, cover_form = "B"
)
stopifnot(isTRUE(all.equal(pt4, 43.9668, tolerance = 1e-4)))

# ── Case 5: reference-contract self-check (invariant-1 analog) — 2019/2021 ─
#     deductible/coverage factor collapses to 1.00 x 1.00 at the 036-locked ─
#     reference ($5,000 deductible, coverage form "A") ─────────────────────
stopifnot(isTRUE(all.equal(deduct_factor_2019_midcont(REF_DEDUCTIBLE), 1.00)))
stopifnot(isTRUE(all.equal(coverage_factor_2019_midcont(REF_COVER_FORM), 1.00)))
cat("  reference-contract deductible/coverage no-op check OK\n")

# ── Case 6: vectorized multi-era call reproduces the same per-row values as ─
#     Cases 1 and 3 called individually (era dispatch correctness) ─────────
pt_vec <- price_mid_continent_tank(
  age_years = c(8, 30), tank_status = c("In Use", "Temporarily Out of Use"),
  era = c("era_2006", "era_2014"),
  is_steel_cathodic = c(FALSE, FALSE), is_reinforced_fiberglass = c(FALSE, FALSE),
  is_double_walled_steel = c(FALSE, FALSE),
  det_interstitial = c(FALSE, FALSE), det_ATG = c(FALSE, FALSE),
  det_vapor = c(FALSE, FALSE), det_groundwater = c(FALSE, FALSE),
  pip_steel_cathodic = c(FALSE, FALSE), pip_fiberglass = c(FALSE, FALSE),
  pip_double_walled = c(FALSE, FALSE),
  cover_occ = c(1e6, 1e6), cover_agg = c(1e6, 2e6)
)
stopifnot(isTRUE(all.equal(pt_vec, c(354.00, 270.00), tolerance = 1e-6)))
cat("  vectorized multi-era dispatch OK\n")

cat("21a self-test PASS\n")
