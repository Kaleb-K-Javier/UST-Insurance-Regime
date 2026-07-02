###############################################################################
# 16a_engine_zurich.R — Zurich American Insurance Co. rate engine (Iowa proxy)
#
# Filings (Iowa rate pages; applied to TX tanks as Iowa-proxy — see 16b banner):
#   USPH-67FSNS710  r2005  eff 06/01/2005  Edition 4/05
#   ZURC-125821722  r2008  eff 11/01/2008  Edition 11/08
#   ZURC-126418617  r2010  eff 03/01/2010  Edition 2/10
#   ZURC-127338727  r2011  eff 10/01/2011  (base rates = 2010 Exh 4 "Proposed",
#                                            cross-verified against 2011 filing)
#
# Rule XIII of the General Rating Manual: factors applied CONSECUTIVELY
# (multiplicative chain), NEVER added together.
#
# Two different sections labeled "X" in the filing package:
#   General Rules §X     = Schedule Rating (±25% per Iowa state rate page;
#                           TX-written policies use ±40% instead — see
#                           SCHED_CAP below)
#   Storage Tank Prog §X = Location Specific (mandatory; not schedule rating)
#
# ILF held at $1M/$1M reference (= 1.000) per 036 locked decision.
# Iowa territory factor = 1.00 throughout.
#
# Sources: library(data.table) only — no data, no I/O
###############################################################################

library(data.table)

# ── Module constants ───────────────────────────────────────────────────────────
# SCHED_CAP: p.Z-IA-2 §IV filed schedule cap is ±25%, but that is the Iowa
# rate-page cap; TX-written policies use ±40% (ticket 048 Part C — the filed
# ±25% figure was the wrong cap to apply to TX tanks).
SCHED_CAP          <- 0.40   # TX schedule credit/debit cap (was 0.25, Iowa cap)
POLICY_MIN_PREMIUM <- 425L   # p.Z-IA-2 §VI: minimum premium

ERA_R2008_EFF <- as.Date("2008-11-01")   # ZURC-125821722
ERA_R2010_EFF <- as.Date("2010-03-01")   # ZURC-126418617
ERA_R2011_EFF <- as.Date("2011-10-01")   # ZURC-127338727

###############################################################################
## Base rate tables — Iowa underground storage tanks (p.Z-IA-1)             ##
## Index 1..36 = age 0..35 yr; index 37 = age > 35 yr                      ##
###############################################################################
BASE_RATES <- list(
  r2005 = list(
    SW = c(  284,  295,  306,  317,  328,  339,  350,  380,  410,  440,
             470,  500,  550,  600,  650,  700,  750,  820,  890,  960,
            1030, 1100, 1170, 1240, 1310, 1380, 1450, 1510, 1570, 1630,
            1690, 1750, 1750, 1750, 1750, 1750, 1850),  # Edition 4/05
    DW = c(  185,  192,  199,  207,  214,  221,  228,  246,  265,  283,
             302,  320,  329,  338,  347,  356,  365,  380,  395,  411,
             426,  441,  458,  475,  492,  509,  526,  540,  554,  568,
             582,  596,  596,  596,  596,  596,  620)
  ),
  r2008 = list(
    SW = c(  291,  302,  314,  325,  336,  347,  359,  390,  420,  451,
             482,  525,  578,  630,  683,  735,  788,  861,  935, 1008,
            1082, 1183, 1258, 1333, 1408, 1484, 1559, 1623, 1688, 1752,
            1817, 1881, 1881, 1881, 1881, 1881, 1989),  # Edition 11/08
    DW = c(  190,  197,  204,  212,  219,  227,  234,  252,  272,  290,
             310,  336,  345,  355,  364,  374,  383,  399,  415,  432,
             447,  474,  492,  511,  529,  547,  565,  581,  596,  611,
             626,  641,  641,  641,  641,  641,  667)
  ),
  r2010 = list(
    SW = c(  378,  393,  408,  422,  437,  452,  466,  506,  546,  586,
             626,  683,  751,  819,  887,  956, 1024, 1119, 1215, 1310,
            1406, 1537, 1635, 1733, 1831, 1929, 2026, 2110, 2194, 2278,
            2362, 2446, 2446, 2446, 2446, 2446, 2585),  # Edition 2/10
    DW = c(  247,  256,  265,  276,  285,  294,  304,  328,  353,  377,
             402,  437,  449,  461,  474,  486,  498,  519,  539,  561,
             581,  616,  640,  664,  688,  711,  735,  755,  774,  794,
             813,  833,  833,  833,  833,  833,  866)
  ),
  r2011 = list(
    SW = c(  454,  472,  490,  506,  524,  542,  559,  607,  655,  703,
             751,  820,  901,  983, 1064, 1147, 1229, 1343, 1458, 1572,
            1687, 1844, 1962, 2080, 2197, 2315, 2431, 2532, 2633, 2734,
            2834, 2935, 2935, 2935, 2935, 2935, 3102),  # 2010 Exh 4 "Proposed"
    DW = c(  340,  354,  367,  380,  393,  407,  419,  455,  491,  527,
             563,  615,  676,  737,  798,  860,  922, 1007, 1094, 1179,
            1265, 1383, 1472, 1560, 1648, 1736, 1823, 1899, 1975, 2050,
            2126, 2201, 2201, 2201, 2201, 2201, 2327)
  )
)
# Integrity: every vector must be length 37 (ages 0–35 + >35)
stopifnot(all(lengths(lapply(BASE_RATES, `[[`, "SW")) == 37L))
stopifnot(all(lengths(lapply(BASE_RATES, `[[`, "DW")) == 37L))

###############################################################################
## era_from_eff_date — assign rate era from policy effective date            ##
## Pre-2005-06-01 contracts (if any) fall back to r2005 (earliest table).   ##
###############################################################################
era_from_eff_date <- function(eff_date) {
  fcase(
    eff_date < ERA_R2008_EFF, "r2005",
    eff_date < ERA_R2010_EFF, "r2008",
    eff_date < ERA_R2011_EFF, "r2010",
    default                  = "r2011"
  )
}

###############################################################################
## zurich_base_rate — per-tank base rate lookup (p.Z-IA-1)                  ##
## Vectorized: age_years (int), double_walled (0/1), rate_era (chr)          ##
###############################################################################
zurich_base_rate <- function(age_years, double_walled, rate_era) {
  idx <- pmin(as.integer(age_years), 36L) + 1L  # age 0→1 … age≥36→37
  dw  <- as.logical(as.integer(double_walled))
  fcase(
    rate_era == "r2005" & !dw, BASE_RATES[["r2005"]][["SW"]][idx],
    rate_era == "r2005" &  dw, BASE_RATES[["r2005"]][["DW"]][idx],
    rate_era == "r2008" & !dw, BASE_RATES[["r2008"]][["SW"]][idx],
    rate_era == "r2008" &  dw, BASE_RATES[["r2008"]][["DW"]][idx],
    rate_era == "r2010" & !dw, BASE_RATES[["r2010"]][["SW"]][idx],
    rate_era == "r2010" &  dw, BASE_RATES[["r2010"]][["DW"]][idx],
    rate_era == "r2011" & !dw, BASE_RATES[["r2011"]][["SW"]][idx],
    rate_era == "r2011" &  dw, BASE_RATES[["r2011"]][["DW"]][idx]
  )
}

###############################################################################
## Per-tank modification factors — Storage Tank Coverage Program §VIII       ##
## (p.CW-4 to p.CW-5)                                                       ##
## Each returns a fractional load; apply as base × (1 + load)               ##
## All factors multiplicative per Rule XIII                                  ##
###############################################################################

# §VIII.1 Size of Tank (p.CW-4)
zurich_cap_load <- function(capacity_gal) {
  cap <- as.numeric(capacity_gal)
  fcase(
    is.na(cap),         0.00,   # unknown → reference (behavioral assumption)
    cap <=  1999L,     -0.08,
    cap <=  3999L,     -0.05,
    cap <=  8000L,      0.00,   # reference
    cap <= 10000L,      0.02,
    cap <= 12000L,      0.04,
    cap <= 15000L,      0.06,
    cap <= 30000L,      0.08,
    cap <= 50000L,      0.10,
    default           = 0.15
  )
}

# §VIII.2 Leak Detection (p.CW-4)
# has_leak_det: TRUE = interstitial monitoring present (Yes → 0%)
zurich_leak_load <- function(has_leak_det) {
  fifelse(as.logical(has_leak_det), 0.00, 0.10)
}

# §VIII.3 Overfill Protection (p.CW-4)
# spill_comply_y: TRUE = SPILL_COMPLY == "Y" (Yes → 0%)
zurich_overfill_load <- function(spill_comply_y) {
  fifelse(as.logical(spill_comply_y), 0.00, 0.10)
}

# §VIII.4 Corrosion Protection (p.CW-5)
# corr_present: TRUE = FRP/composite/jacketed OR CORR_TANK_CP ∈ {C,E,B}
zurich_corr_load <- function(corr_present) {
  fifelse(as.logical(corr_present), 0.00, 0.10)
}

###############################################################################
## Contents load — Storage Tank Coverage Program §XI.2 (p.CW-6)            ##
## Priority order: gasoline > diesel > oil_kero > jet_fuel > reference       ##
## is_oil_kerosene rated at Fuel Oil (-8%) — modal product in that category  ##
###############################################################################
zurich_contents_load <- function(is_gasoline, is_diesel, is_oil_kero, is_jet_fuel) {
  fcase(
    as.logical(is_gasoline),  0.05,   # Gasoline (all grades)
    as.logical(is_diesel),   -0.08,   # Diesel
    as.logical(is_oil_kero), -0.08,   # Oil/Kerosene → rated as Fuel Oil
    as.logical(is_jet_fuel),  0.00,   # Aviation/Jet Fuel (reference)
    default                =  0.00    # Unknown/empty → reference
  )
}

###############################################################################
## Facility-level factors                                                     ##
###############################################################################

# §XI.1 Size of Risk Credit (p.CW-6)
# n_total_tanks: TOTAL active tanks at the facility for the coverage month
zurich_size_credit <- function(n_total_tanks) {
  n <- as.integer(n_total_tanks)
  fcase(
    n <=  3L,  0.00,
    n <=  6L, -0.10,
    n <= 10L, -0.20,
    n <= 15L, -0.25,
    n <= 25L, -0.30,
    n <= 45L, -0.35,
    default  = -0.40
  )
}

# §X.3 Location Type (p.CW-6) — mandatory factor, NOT schedule rating
# Wholesale tier depends on n_tanks (number of tanks at facility).
# BEHAVIORAL ASSUMPTION: "All Other / unrecognized" held at 0%
#   (filing says "Refer to Company"; we treat as reference).
# NOTE: Verify FACILITY_TYPE codes against texas_facility.csv before running.
zurich_loc_type_load <- function(fac_type, n_tanks) {
  ft <- toupper(trimws(as.character(fac_type)))
  nt <- as.integer(n_tanks)
  fcase(
    ft %in% c("GAS", "GS", "G", "CONV", "C", "CONVENIENCE"),       0.00,
    ft %in% c("MUN", "M", "MUNI", "MUNICIPAL", "MUNISVCS"),         0.00,
    ft %in% c("AIR", "A", "AIRPORT"),                                0.00,
    ft %in% c("HEA", "H", "HEALTH", "HEALTHCARE", "HLTH"),          0.00,
    ft %in% c("VEH", "V", "AUTO", "VEHICLE", "DEALER"),             0.00,
    ft %in% c("AGR", "F", "FARM", "AG", "AGRICULTURAL"),            0.10,
    ft %in% c("MAR", "MARINA"),                                      0.25,
    ft %in% c("WHO", "W", "WHOL", "WHOLESALE") & nt <=  3L,         0.00,
    ft %in% c("WHO", "W", "WHOL", "WHOLESALE") & nt <= 10L,         0.10,
    ft %in% c("WHO", "W", "WHOL", "WHOLESALE") & nt >  10L,         0.25,
    default                                                        =  0.00
  )
}

# §X.1 Location Prior Release (p.CW-5) — mandatory factor, NOT schedule rating
# had_prior_lust: any LUST report_date < policy EFF_DATE
# lust_open:      any such LUST with nfa_date NA or nfa_date >= EFF_DATE
zurich_prior_release_load <- function(had_prior_lust, lust_open) {
  fcase(
    !as.logical(had_prior_lust),                                      0.00,
    as.logical(had_prior_lust) & !as.logical(lust_open),             0.10,
    as.logical(had_prior_lust) &  as.logical(lust_open),             0.20
  )
}

###############################################################################
## zurich_facility_premium — full premium for one facility-month             ##
##                                                                           ##
## tanks_dt: data.table, one row per in-service tank                         ##
##   Required cols: age_years, double_walled, rate_era, CAPACITY,           ##
##                  has_leak_det, spill_comply_y, corr_present,              ##
##                  is_gasoline, is_diesel, is_oil_kero, is_jet_fuel         ##
##                                                                           ##
## Scalar facility inputs: fac_type, had_prior_lust, lust_open              ##
##                                                                           ##
## Returns list(min_prem, standard_prem, max_prem, n_tanks_rated)           ##
## ILF = 1.000 (held at $1M/$1M reference). Schedule ±25%. Floor $425.      ##
###############################################################################
zurich_facility_premium <- function(tanks_dt, fac_type, had_prior_lust, lust_open) {
  dt <- copy(tanks_dt)
  n  <- nrow(dt)
  stopifnot(n >= 1L)

  # Per-tank premiums (Rule XIII: consecutive multiplication)
  dt[, base_r := zurich_base_rate(age_years, double_walled, rate_era)]
  dt[, tank_premium :=
       base_r
     * (1 + zurich_cap_load(CAPACITY))
     * (1 + zurich_leak_load(has_leak_det))
     * (1 + zurich_overfill_load(spill_comply_y))
     * (1 + zurich_corr_load(corr_present))
     * (1 + zurich_contents_load(is_gasoline, is_diesel, is_oil_kero, is_jet_fuel))
  ]
  stopifnot(all(is.finite(dt$tank_premium)), all(dt$tank_premium > 0))

  tank_sum <- sum(dt$tank_premium)

  # Facility-level factors (Rule XIII: consecutive on the summed base)
  fac_raw <- tank_sum *
    (1 + zurich_size_credit(n)) *
    (1 + zurich_loc_type_load(fac_type, n)) *
    (1 + zurich_prior_release_load(had_prior_lust, lust_open))

  std <- pmax(fac_raw,                   POLICY_MIN_PREMIUM)
  mn  <- pmax(fac_raw * (1 - SCHED_CAP), POLICY_MIN_PREMIUM)
  mx  <- pmax(fac_raw * (1 + SCHED_CAP), POLICY_MIN_PREMIUM)

  stopifnot(all(is.finite(c(mn, std, mx))))
  stopifnot(mn <= std, std <= mx)
  stopifnot(mn >= POLICY_MIN_PREMIUM, std >= POLICY_MIN_PREMIUM, mx >= POLICY_MIN_PREMIUM)

  list(min_prem = mn, standard_prem = std, max_prem = mx, n_tanks_rated = n)
}

###############################################################################
## Self-test (runs at source time)                                            ##
###############################################################################
cat("=== 16a self-test ===\n")

# ── Case 1: 1-tank, r2010, diesel, all other factors at reference ─────────
# base = BASE_RATES$r2010$SW[age=20, idx=21] = 1406
# tank  = 1406 × (1+0.00)[cap] × (1+0.00)[leak] × (1+0.00)[overfill]
#              × (1+0.00)[corr] × (1-0.08)[diesel]  = 1406 × 0.92 = 1293.52
# fac   = 1293.52 × (1+0%size) × (1+0%loc) × (1+0%release)  = 1293.52
# std   = max(1293.52, 425) = 1293.52
# min   = max(1293.52 × 0.60, 425) = 776.112   (SCHED_CAP = 0.40, TX not Iowa)
# max   = max(1293.52 × 1.40, 425) = 1810.928

t1 <- data.table(
  age_years      = 20L,    double_walled  = 0L,  rate_era = "r2010",
  CAPACITY       = 5000,   has_leak_det   = TRUE, spill_comply_y = TRUE,
  corr_present   = TRUE,   is_gasoline    = FALSE, is_diesel = TRUE,
  is_oil_kero    = FALSE,  is_jet_fuel    = FALSE
)
r1 <- zurich_facility_premium(t1, fac_type = "GS", had_prior_lust = FALSE, lust_open = FALSE)
stopifnot(isTRUE(all.equal(r1$standard_prem, 1293.520, tolerance = 1e-4)))
stopifnot(isTRUE(all.equal(r1$min_prem,       776.112, tolerance = 1e-4)))
stopifnot(isTRUE(all.equal(r1$max_prem,      1810.928, tolerance = 1e-4)))
stopifnot(r1$n_tanks_rated == 1L)

# ── Case 2: 2-tank, r2008, agricultural (+10%) + prior_release_open (+20%) ─
# Tank 1: age=10 SW, cap=5000(ref), no_leak(+10%), spill_Y(0%), corr_T(0%), gasoline(+5%)
#   base=482;  482 × 1.10 × 1.05 = 556.71
# Tank 2: age=0  DW, cap=5000(ref), leak_T(0%),  spill_N(+10%), corr_T(0%), diesel(-8%)
#   base=190;  190 × 1.10 × 0.92 = 192.28
# tank_sum = 748.99
# fac = 748.99 × (1+0%size) × (1+10%agric) × (1+20%open_lust) = 988.6668
# std = 988.6668; min = 988.6668 × 0.60 = 593.2001; max = 988.6668 × 1.40 = 1384.1335

t2 <- data.table(
  age_years      = c(10L,    0L),   double_walled  = c(0L,  1L),
  rate_era       = c("r2008","r2008"),
  CAPACITY       = c(5000,   5000), has_leak_det   = c(FALSE, TRUE),
  spill_comply_y = c(TRUE,  FALSE), corr_present   = c(TRUE,  TRUE),
  is_gasoline    = c(TRUE,  FALSE), is_diesel      = c(FALSE, TRUE),
  is_oil_kero    = c(FALSE, FALSE), is_jet_fuel    = c(FALSE, FALSE)
)
r2 <- zurich_facility_premium(t2, fac_type = "F", had_prior_lust = TRUE, lust_open = TRUE)
stopifnot(isTRUE(all.equal(r2$standard_prem,  988.6668, tolerance = 1e-4)))
stopifnot(isTRUE(all.equal(r2$min_prem,        593.2001, tolerance = 1e-4)))
stopifnot(isTRUE(all.equal(r2$max_prem,       1384.1335, tolerance = 1e-4)))
stopifnot(r2$n_tanks_rated == 2L)

# ── Case 3: floor-binding ─────────────────────────────────────────────────
# 1-tank r2005, age=0 SW, all-reference except gasoline (+5%)
# tank = 284 × 1.05 = 298.20  →  fac_raw = 298.20  →  all three floor at 425

t3 <- data.table(
  age_years      = 0L,    double_walled  = 0L,    rate_era = "r2005",
  CAPACITY       = 5000,  has_leak_det   = TRUE,  spill_comply_y = TRUE,
  corr_present   = TRUE,  is_gasoline    = TRUE,  is_diesel = FALSE,
  is_oil_kero    = FALSE, is_jet_fuel    = FALSE
)
r3 <- zurich_facility_premium(t3, fac_type = "GS", had_prior_lust = FALSE, lust_open = FALSE)
stopifnot(isTRUE(all.equal(r3$min_prem,      425, tolerance = 1e-4)))
stopifnot(isTRUE(all.equal(r3$standard_prem, 425, tolerance = 1e-4)))
stopifnot(isTRUE(all.equal(r3$max_prem,      425, tolerance = 1e-4)))

cat("16a self-test PASS\n")
