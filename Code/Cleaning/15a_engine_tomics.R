###############################################################################
# 15a_engine_tomics.R — TOMICS rate engine: pure functions + self-test
# Filing:  SERFF TEXS-131241913 ("TOMIC 2017 10_rate_ TEXS_131241913.pdf")
# Eff:     2018-01-01 (re-file; applied across all TOMICS contract-years)
# Sources: library(data.table) only — no data, no I/O
###############################################################################

library(data.table)

# ── Module constants ───────────────────────────────────────────────────────────
SCHED_CAP          <- 0.40          # p.3: schedule ±40%
POLICY_MIN_PREMIUM <- 350L          # p.2: minimum policy premium
ENERGY_ACT_DATE    <- as.Date("2006-01-01")   # New vs Replace/Upgrade cutoff

to_bool <- function(x) !is.na(x) & as.logical(x)

###############################################################################
## C1. Base rate: New / Replace / Upgrade  (p.2 base definitions)            ##
##   New     $250 — new pit, eff 2005 EPA Energy Act, facility had no prior tank
##   Replace $340 — same pit as prior tanks (facility had earlier install)
##   Upgrade $420 — pre-Energy Act (1998-retrofit era and older)
###############################################################################
base_rate_tomics <- function(cat) {
  fcase(cat == "New", 250L, cat == "Replace", 340L, default = 420L)
}

###############################################################################
## C2. Policy-limits ILF  (p.1)                                              ##
##   Additive load. Reference = 1MM/1MM (0.000).                             ##
##   NA or 0 defaults to 1M/1M reference.                                    ##
###############################################################################
ilf_load_tomics <- function(occ, agg) {
  occ <- fifelse(is.na(occ) | occ == 0, 1e6, as.numeric(occ))
  agg <- fifelse(is.na(agg) | agg == 0, 1e6, as.numeric(agg))
  fcase(
    occ == 5e5 & agg == 1e6, -0.150,
    occ == 1e6 & agg == 1e6,  0.000,   # reference
    occ == 1e6 & agg == 2e6,  0.080,
    occ == 1e6 & agg == 3e6,  0.150,
    occ == 1e6 & agg == 5e6,  0.200,
    default                =  0.000
  )
}

###############################################################################
## C6. Age of tanks  (p.1)                                                   ##
##   Additive load. Reference = 1-2 yrs (0.000). NA -> NA (unrated row).    ##
###############################################################################
age_load_tomics <- function(a) {
  fcase(
    is.na(a),  NA_real_,
    a <=  2,    0.000,   # reference
    a <=  5,    0.050,
    a <=  8,    0.100,
    a <= 11,    0.150,
    a <= 15,    0.200,
    a <= 20,    0.300,
    a <= 25,    0.400,
    a <= 34,    0.500,
    a <= 44,    0.600,
    a  > 44,    0.700
  )
}

###############################################################################
## C7. Leak detection  (p.2)                                                 ##
##   interstitial monitoring OR sump monitors = -0.150; else ref = 0.000.   ##
##   det_interstitial covers both (DET_C_INTERSTITIAL | DET_P_INTERSTITIAL). ##
###############################################################################
leak_load_tomics <- function(det_interstitial) {
  fifelse(to_bool(det_interstitial), -0.150, 0.000)
}

###############################################################################
## C9. Tank construction  (p.2)                                              ##
##   DW tank = -0.300 | composite (FRP or FRP-clad steel) = -0.150          ##
##   DW evaluated first — no stacking. ref = single (0.000).                ##
###############################################################################
construction_load_tomics <- function(double_walled, is_composite) {
  fcase(
    to_bool(double_walled), -0.300,
    to_bool(is_composite),  -0.150,
    default =                0.000
  )
}

###############################################################################
## C10. Pipe construction  (p.2)                                             ##
##   dbl/wall rigid pipe = -0.150 | dble wall flex pipe = 0.000 | else 0.000 ##
###############################################################################
pipe_load_tomics <- function(pip_dw_rigid) {
  fifelse(to_bool(pip_dw_rigid), -0.150, 0.000)
}

## Held at filing reference — firm-choice, default contract (all 0.000):    ##
##   deductible ($5k, p.1), defense limit ($1M, p.1), retro date (0-1yr, p.1)
##   prior contamination (none, p.1), site capacity (AGT header->N/A USTs, p.2)

###############################################################################
## SS3 — Facility-level premium builder                                      ##
##                                                                           ##
## tanks_dt: one row per in-service tank; required cols:                     ##
##   base_category {New,Replace,Upgrade}                                     ##
##   COVER_OCC, COVER_AGG   (policy limits; NA -> 1M/1M reference)          ##
##   age_years              (integer or numeric)                              ##
##   det_interstitial       (0/1)                                            ##
##   double_walled          (0/1)                                            ##
##   is_composite           (0/1)                                            ##
##   pip_dw_rigid           (0/1)                                            ##
##                                                                           ##
## Returns: list(min_prem, standard_prem, max_prem, n_tanks_rated)          ##
## Band and floor are applied at facility level, not per tank.               ##
###############################################################################
tomics_facility_premium <- function(tanks_dt) {
  dt <- copy(tanks_dt)
  dt[, sec_load := (
    ilf_load_tomics(COVER_OCC, COVER_AGG) +
    age_load_tomics(age_years) +
    leak_load_tomics(det_interstitial) +
    construction_load_tomics(double_walled, is_composite) +
    pipe_load_tomics(pip_dw_rigid)
  )]
  dt[, tank_premium := base_rate_tomics(base_category) * (1 + sec_load)]

  standard <- sum(dt$tank_premium, na.rm = TRUE)
  mn  <- pmax(standard * (1 - SCHED_CAP), POLICY_MIN_PREMIUM)
  std <- pmax(standard,                   POLICY_MIN_PREMIUM)
  mx  <- pmax(standard * (1 + SCHED_CAP), POLICY_MIN_PREMIUM)

  stopifnot(all(is.finite(c(mn, std, mx))))
  stopifnot(mn >= POLICY_MIN_PREMIUM, std >= POLICY_MIN_PREMIUM, mx >= POLICY_MIN_PREMIUM)
  stopifnot(mn <= std, std <= mx)

  list(min_prem = mn, standard_prem = std, max_prem = mx, n_tanks_rated = nrow(dt))
}


###############################################################################
## SS4 — Self-test (runs at source time; stopifnot on hand-calc values)     ##
###############################################################################

cat("=== 15a self-test ===\n")

# ── Case 1: 2-tank facility ────────────────────────────────────────────────
# Tank 1: Upgrade 1M/1M age=12 single-wall not-composite no-interstitial non-DW-pipe
#   sec = 0.000(ilf) + 0.200(age) + 0.000(leak) + 0.000(constr) + 0.000(pipe) = 0.200
#   tank_premium = 420 * 1.200 = 504.00
# Tank 2: New 1M/1M age=1 double-wall composite interstitial DW-rigid-pipe
#   sec = 0.000(ilf) + 0.000(age) - 0.150(leak) - 0.300(DW,no stack) - 0.150(pipe) = -0.600
#   tank_premium = 250 * 0.400 = 100.00
# facility standard = 604.00  min = 0.6*604 = 362.40  max = 1.4*604 = 845.60  floor not binding

fake2 <- data.table(
  base_category    = c("Upgrade", "New"),
  COVER_OCC        = c(1e6, 1e6),
  COVER_AGG        = c(1e6, 1e6),
  age_years        = c(12L, 1L),
  det_interstitial = c(0L,  1L),
  double_walled    = c(0L,  1L),
  is_composite     = c(0L,  1L),
  pip_dw_rigid     = c(0L,  1L)
)

r2 <- tomics_facility_premium(fake2)
stopifnot(isTRUE(all.equal(r2$standard_prem, 604.00, tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(r2$min_prem,      362.40, tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(r2$max_prem,      845.60, tolerance = 1e-8)))
stopifnot(r2$n_tanks_rated == 2L)

# ── Case 2: floor-binding ─────────────────────────────────────────────────
# Single Upgrade, DW, interstitial, DW-rigid-pipe, age=1
#   sec = -0.300(DW) - 0.150(leak) - 0.150(pipe) = -0.600
#   standard = 420 * 0.400 = 168.00
#   min = 0.6*168 = 100.80 -> floor 350
#   std = 168 -> floor 350
#   max = 1.4*168 = 235.20 -> floor 350

fake_floor <- data.table(
  base_category    = "Upgrade",
  COVER_OCC        = 1e6,
  COVER_AGG        = 1e6,
  age_years        = 1L,
  det_interstitial = 1L,
  double_walled    = 1L,
  is_composite     = 1L,
  pip_dw_rigid     = 1L
)

rfl <- tomics_facility_premium(fake_floor)
stopifnot(isTRUE(all.equal(rfl$min_prem,      350, tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(rfl$standard_prem, 350, tolerance = 1e-8)))
stopifnot(isTRUE(all.equal(rfl$max_prem,      350, tolerance = 1e-8)))

cat("15a self-test PASS\n")
