

#########################################################################

library(data.table)
library(lubridate)
library(tidyverse)
library(here)

# ── helper to resolve project paths (already used in your ETL) ───────────────
get_data_path <- function(...){
  here::here("Data", "Raw_do_not_write", "panel_merge_staging", ...)
}

out_dir <- get_data_path()
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

###############################################################################
##  SECTION A  –  Static UST‑tank table (revised)                            ##
##             • loads raw_pst_ust.csv                                       ##
##             • normalises STATUS to Mid‑Continent’s three labels           ##
##             • re‑computes closure flag & feature dummies                  ##
###############################################################################


# 0) load raw tank snapshot ---------------------------------------------------
ust_raw <- fread(get_data_path( "raw_pst_ust.csv"))

# 1) merge keys ---------------------------------------------------------------
ust_raw[, FACILITY_ID := trimws(substr(FACILITY_ID_PAD, 1, 6))]
ust_raw[, UST_ID      := toupper(UST_ID)]

# 2) basic parsing ------------------------------------------------------------
ust_raw[, INSTALL_DATE := ymd(INSTALL_DATE)]
ust_raw[, STATUS       := toupper(trimws(STATUS))]

# 3) map raw STATUS → Mid‑Continent canonical labels -------------------------
ust_raw[, tank_status :=
          fcase(STATUS == "IN USE",                   "In Use",
                STATUS == "TEMP OUT OF SERVICE",      "Temporarily Out of Use",
                STATUS %chin% c("REMOVED FROM GROUND",
                                "PERM FILLED IN PLACE"), "Permanently Out of Use",
                default = "Unknown")]

# 4) updated closure flag (for later exclusions, not for rating) -------------
ust_raw[, is_closed_removed := as.integer(
  STATUS %chin% c("REMOVED FROM GROUND", "PERM FILLED IN PLACE")
)]

ust_raw[,CLOSED_DATE := ymd(end_date)]

# 5) wall‑type flags ----------------------------------------------------------
ust_raw[, `:=`(
  single_walled = as.integer(TANK_SINGLE == "Y"),
  double_walled = as.integer(TANK_DOUBLE == "Y")
)]

# 6) tank‑construction dummies (Mid‑Continent language) ----------------------
ust_raw[, `:=`(
  is_steel_cathodic       = single_walled & (CORR_TANK_CP == "Y"),
  is_double_walled_steel  = double_walled & (TANK_MAT_STEEL == "Y"),
  is_reinforced_fiberglass= (TANK_MAT_FRP   == "Y"))]



ust_raw[, `:=`(
  is_other_tank_material  = as.integer(
                              !(is_steel_cathodic | is_double_walled_steel |
                                is_reinforced_fiberglass))
)]





# 7) piping dummies -----------------------------------------------------------

# groupby and  count unique facilyte ids by pipe_type
pipe_type_df <- ust_raw[, .(
  pipe_type = uniqueN(FACILITY_ID)
), by = PIPE_TYPE ]

#      PIPE_TYPE pipe_type
# 1:                 34590
# 2: Pressurized     22116
# 3:     Suction      6968
# 4:     Gravity      1548


ust_raw[, `:=`(
  pip_steel_cathodic  = as.integer(PIP_MAT_STEEL == "Y" & CORR_PIPE_CP == "Y"),
  pip_fiberglass      = as.integer(PIP_MAT_FRP   == "Y"),
  pip_double_walled   = as.integer(PIP_DOUBLE    == "Y"),
  pip_suction         = as.integer(PIPE_TYPE == "Suction"),
  pip_pressure_ll     = as.integer(PIPE_TYPE == "Pressurized" | DET_P_LLD == "Y"))]

ust_raw[, `:=`(
  pip_other           = as.integer(!(pip_suction | pip_pressure_ll))
)]


# 8) leak‑detection dummies ---------------------------------------------------
ust_raw[, `:=`(
  det_interstitial = as.integer(DET_C_INTERSTITIAL == "Y" | DET_P_INTERSTITIAL == "Y"),
  det_ATG          = as.integer(DET_C_ATG == "Y"          | DET_P_LLD == "Y"),
  det_vapor        = as.integer(DET_C_VAPOR == "Y"        | DET_P_VAPOR == "Y"),
  det_groundwater  = as.integer(DET_C_GW == "Y"           | DET_P_GW == "Y"),
  det_SIR          = as.integer(DET_C_SIR == "Y"          | DET_P_SIR == "Y"))
]
ust_raw[, `:=`(
  det_other           = as.integer(!(det_interstitial | det_ATG | det_vapor |
                                      det_groundwater | det_SIR))
)]




# 9) assemble static‐detail table --------------------------------------------
static_tank_details <- ust_raw[, .(
  FACILITY_ID, UST_ID, INSTALL_DATE, tank_status, is_closed_removed,CLOSED_DATE,
  # construction
  is_steel_cathodic, is_double_walled_steel,
  is_reinforced_fiberglass, is_other_tank_material,
  # piping
  pip_steel_cathodic, pip_fiberglass, pip_double_walled,
  pip_suction, pip_pressure_ll, pip_other,
  # detection
  det_interstitial, det_ATG, det_vapor,
  det_groundwater, det_SIR, det_other,
  #tank capacity
  CAPACITY
)]

# 10) optional write‑out ------------------------------------------------------
fwrite(static_tank_details,
       get_data_path( "texas_static_tank_details_2019.csv"))



###############################################################################
##  ⬇︎ SECTION SET‑UP – parameters that change with the new manual ⬇︎
###############################################################################

## ── rating window for the “TOP Program” manual (on or after 01‑Jan‑2019)
RATING_START <- 2019L                    # inclusive
RATING_END   <- 2021L                    # leave roomy for future years

BASE_RATE          <- 300                # still the base rate per tank
SCHED_MIN_FACTOR   <- 0.60               # –40 %
SCHED_MAX_FACTOR   <- 1.40               # +40 %
POLICY_MIN_PREMIUM <- 500                # policy‑level floor for UST policies

###############################################################################
##  ⬇︎ 2019‑manual helpers (replaces the older ones where names collide) ⬇︎
###############################################################################

## ---------- 1) granular Tank‑age factor (TOP Program 01‑2019) --------------
age_factor <- function(a){
  fcase( is.na(a),                NA_real_,   # keep NA so we can drop unrated
         a <=  2,   -0.20,
         a <=  4,   -0.16,
         a <=  6,   -0.12,
         a <=  8,   -0.08,
         a <= 10,   -0.04,
         a <= 15,    0.00,
         a <= 20,   +0.10,
         a <= 25,   +0.20,
         a <= 30,   +0.25,
         a <= 35,   +0.35,
         a <= 40,   +0.45,
         a <= 45,   +0.55,
         a <= 50,   +0.65,
         a  > 50,   +0.75 )
}

## ---------- 2) site‑capacity factor (new in 2019 manual) -------------------
## Expecting a *facility‑month* column `site_capacity_gal` = total UST capacity
capacity_factor <- function(cap){
  fcase( is.na(cap),             0.00,               # missing ⇒ assume ≤100 k
         cap <= 100000,          0.00,
         cap <= 150000,         +0.10,
         cap <= 200000,         +0.15,
         cap <= 250000,         +0.20,
         cap  > 250000,         +0.25 )
}


###############################################################################
##  Mid‑Continent TOP filing (eff. 01‑01‑2019) – helper functions
##  – All functions are fully vectorised
##  – Inputs may be NA / 0‑1 / TRUE‑FALSE; NA is treated as “flag not present”
###############################################################################

library(data.table)

## ── tiny helper --------------------------------------------------------------
to_bool <- function(x) !is.na(x) & as.logical(x)

## ── 1) Tank‑status factor (unchanged) ---------------------------------------
status_factor <- function(status_chr) {
  fcase(
    status_chr == "In Use",                   0.00,
    status_chr == "Temporarily Out of Use",  -0.50,
    status_chr == "Permanently Out of Use",  -0.75,
    default = NA_real_
  )
}

## ── 2) Increased‑Limit Factor  (Table II) -----------------------------------
## Base manual rate = 500 k / 1 m with $5 k deductible
ilf_factor <- function(occ_limit, agg_limit) {
  occ <- fifelse(is.na(occ_limit), 1e6, occ_limit)
  agg <- fifelse(is.na(agg_limit), 1e6, agg_limit)

  fcase(
    occ == 1e6 & agg == 1e6, 1.18,   # 1 m / 1 m
    occ == 1e6 & agg == 2e6, 1.20,   # 1 m / 2 m
    default = 1.00                   # 500 k / 1 m (or anything else)
  )
}

## ── 3) Deductible factor  (Table III) ---------------------------------------
deduct_factor <- function(ded) {
  fcase(
    ded <=  5e3 | is.na(ded), 1.00,
    ded <= 10e3,              0.92,
    ded <= 25e3,              0.80,
    ded <= 50e3,              0.60,
    ded >  50e3,              0.40
  )
}

## ── 4) Coverage‑form multiplier (Tables IV & V) -----------------------------
coverage_factor <- function(form) {
  fcase(
    toupper(form) == "B", 1.50,   # Coverage B
    toupper(form) == "C", 1.25,   # Coverage C
    default = 1.00                # Coverage A (base)
  )
}

## ── 5) NEW granular age factor  (Secondary I) -------------------------------
age_factor <- function(age_yrs) {
  a <- fifelse(is.na(age_yrs), NA_real_, age_yrs)

  fcase(
    a <=  2, -0.20,
    a <=  4, -0.16,
    a <=  6, -0.12,
    a <=  8, -0.08,
    a <= 10, -0.04,
    a <= 15,  0.00,
    a <= 20,  0.10,
    a <= 25,  0.20,
    a <= 30,  0.25,
    a <= 35,  0.35,
    a <= 40,  0.45,
    a <= 45,  0.55,
    a <= 50,  0.65,
    a  > 50,  0.75
  )
}

## ── 6) Tank‑construction factor  (Secondary II) -----------------------------
construction_factor <- function(is_steel_cathodic,
                                is_reinforced_fiberglass,
                                is_double_walled_steel) {

  dw  <- to_bool(is_double_walled_steel)
  sc  <- to_bool(is_steel_cathodic)
  frp <- to_bool(is_reinforced_fiberglass)

  fcase(
    dw,          -0.20,
    sc | frp,     0.00,
    default =     0.00             # “other” materials
  )
}

## ── 7) Leak‑detection factor  (Secondary III) -------------------------------
leak_factor <- function(det_interstitial,
                        det_ATG,
                        det_vapor,
                        det_groundwater) {

  di <- to_bool(det_interstitial)
  da <- to_bool(det_ATG)
  dv <- to_bool(det_vapor)
  dg <- to_bool(det_groundwater)

  fcase(
    di,          -0.20,        # IM
    da,          -0.10,        # ATG / LLD
    dv | dg,     -0.10,        # VM or GM
    default =     0.00
  )
}

## ── 8) Piping‑construction factor (Secondary IV) ----------------------------
pipe_const_factor <- function(pip_steel_cathodic,
                              pip_fiberglass,
                              pip_double_walled) {

  st <- to_bool(pip_steel_cathodic)
  fb <- to_bool(pip_fiberglass)
  dw <- to_bool(pip_double_walled)

  fcase(
    st,  +0.10,
    dw,  -0.10,
    fb,   0.00,
    default = 0.00
  )
}

## ── 9) Piping‑design factor  (unchanged) ------------------------------------
pipe_design_factor <- function(pip_suction,
                               pip_pressure_ll) 0   # still neutral

## ── 10) Flood‑plain, marina, aquifer surcharges (Secondary V‑VII) -----------
flood_factor   <- function(flood_zone) fifelse(flood_zone %chin% c("A","B"), 0.25, 0)
marina_factor  <- function(is_marina)  fifelse(to_bool(is_marina),            0.50, 0)
aquifer_factor <- function(over_aquif)  fifelse(to_bool(over_aquif),          0.50, 0)

## ── 11) Retro‑date factor  (Secondary VIII) ---------------------------------
retro_factor <- function(years_prior) {
  yrs <- fifelse(is.na(years_prior), Inf, years_prior)

  fcase(
    yrs <=  1, 0.00,
    yrs <=  5, 0.10,
    yrs <= 10, 0.20,
    yrs <= 15, 0.30,
    yrs <= 20, 0.40,
    yrs  > 20, 0.50
  )
}

## ── 12) Site‑capacity factor  (Secondary IX) --------------------------------
site_capacity_factor <- function(gallons) {
  cap <- fifelse(is.na(gallons), 0, gallons)

  fcase(
    cap <= 100000, 0.00,
    cap <= 150000, 0.10,
    cap <= 200000, 0.15,
    cap <= 250000, 0.20,
    cap  > 250000, 0.25
  )
}

## ── 13) Update / upgrade credits (Secondary X & XI) -------------------------
update_credit_factor <- function(warranty_years) {
  yrs <- fifelse(is.na(warranty_years), 0, warranty_years)

  fcase(
    yrs >= 30, -0.15,
    yrs >= 20, -0.10,
    yrs >= 10, -0.07,
    yrs >=  5, -0.05,
    default =  0.00
  )
}

upgrade_credit_factor <- function(warranty_years) {
  yrs <- fifelse(is.na(warranty_years), 0, warranty_years)

  fcase(
    yrs >= 30, -0.40,
    yrs >= 20, -0.30,
    yrs >= 10, -0.20,
    yrs >=  5, -0.10,
    default =  0.00
  )
}




###############################################################################
##  SECTION B  –  Mid‑Continent OK‑SERFF premium engine (2014‑05 … 2019‑01) ##
###############################################################################

WINDOW_START <- ymd("2019-01-31")
WINDOW_END   <- ymd("2021-04-30")

BASE_RATE          <- 300      # $ / tank – 500 k / 1 m, $5 k deductible
SCHED_MIN_FACTOR   <- 0.60
SCHED_MAX_FACTOR   <- 1.40
POLICY_MIN_PREMIUM <- 500      # facility‑month minimum

## ────────────────────────────────────────────────────────────────────────────
## 1 ▸ Mid‑Continent contracts (year‑month panel)                            ##
## ────────────────────────────────────────────────────────────────────────────
fa_contracts <- fread( get_data_path("fa_monthly_contract.csv") )[
  , `:=`(EFF_DATE = ymd(EFF_DATE),
         EXP_DATE = ymd(EXP_DATE))
][
  tolower(ISSUER_NAME) %like% "mid-" &
  EFF_DATE <= WINDOW_END &
  EFF_DATE >= WINDOW_START
]


fa_contracts = fa_contracts[
  , .(FACILITY_ID, YEAR, MONTH, EFF_DATE, COVER_OCC, COVER_AGG,ISSUER_NAME)
]

midc_fac_ids <- unique(fa_contracts$FACILITY_ID)

## ────────────────────────────────────────────────────────────────────────────
## 2 ▸ Static tank attributes  – *no* pre‑filter on is_closed_removed        ##
## ────────────────────────────────────────────────────────────────────────────
tank_static <- fread( get_data_path("texas_static_tank_details_2019.csv") )[
  FACILITY_ID %in% midc_fac_ids              # facilities that actually matter
  ## ↓  NO test on is_closed_removed here – the EFF_DATE filter in step 4
  ##    will decide if a tank is in‑service on a given contract month
]

## ────────────────────────────────────────────────────────────────────────────
## 3 ▸ Build tank‑CONTRACT‑MONTH panel (cartesian join)                      ##
## ────────────────────────────────────────────────────────────────────────────
tank_month <- fa_contracts[
  tank_static,
  on = "FACILITY_ID",
  allow.cartesian = TRUE,
  nomatch = 0
]

## 4 ▸ Keep only tanks that are in‑service when the contract starts
tank_month <- tank_month[
  INSTALL_DATE <= EFF_DATE &                          # installed already
  (is.na(CLOSED_DATE) | CLOSED_DATE > EFF_DATE)       # not yet removed
]

## 5 ▸ Age at contract inception
tank_month[ , `:=`(
  EFF_DATE      = as.IDate(EFF_DATE),      # was Date → IDate
  INSTALL_DATE  = as.IDate(INSTALL_DATE)   # ensure IDate as well
)]

## age in completed years at contract inception
tank_month[ ,
  age_years := pmax(
    0,
    floor( as.numeric(EFF_DATE - INSTALL_DATE) / 365.25 )
  )
]

ust_tm = tank_month
# create site capacity column, its the sum of all tanks capacity at a facility
ust_tm[, site_capacity_gal := sum(CAPACITY, na.rm = TRUE), by = .(FACILITY_ID, YEAR, MONTH)]

## ── Secondary‑factor columns ------------------------------------------------
ust_tm[ , `:=`(
   status_load        = 0, #status_factor(tank_status), not used in 2019 manual
  age_load           = age_factor(age_years),          # <‑‑ updated
  construction_load  = construction_factor(
                         is_steel_cathodic,
                         is_reinforced_fiberglass,
                         is_double_walled_steel),
  leak_load          = leak_factor(
                         det_interstitial,
                         det_ATG,
                         det_vapor,
                         det_groundwater),
  pipe_const_load    = pipe_const_factor(
                         pip_steel_cathodic,
                         pip_fiberglass,
                         pip_double_walled),
  pipe_design_load   = pipe_design_factor(
                         pip_suction,
                         pip_pressure_ll),
  capacity_load      = capacity_factor(site_capacity_gal),  # <‑‑ NEW
  flood_load         = 0, # don't have this info at this time
  marina_load        = 0, # don't have this info at this time
  aquifer_load       = 0 # don't have this info at this time
  )]

sec_cols <- c("status_load","age_load","construction_load",
              "leak_load","pipe_const_load","pipe_design_load",
              "capacity_load","flood_load","marina_load","aquifer_load")

ust_tm[ , sec_load := rowSums(.SD, na.rm = TRUE), .SDcols = sec_cols ]

## ── Tank‑month premium (still no tank‑level floor) --------------------------
ust_tm[ , tank_premium := BASE_RATE * ilf_factor(COVER_OCC, COVER_AGG) *
                           (1 + sec_load) ]

ust_tm[ , `:=`(
  tank_premium_sched_min = tank_premium * SCHED_MIN_FACTOR,
  tank_premium_sched_max = tank_premium * SCHED_MAX_FACTOR)
]



## ── annual roll‑up (if you still need facility‑YEAR) ------------------------
fac_year <- ust_tm[ , .(
  base_premium   = sum(tank_premium),
  sched_min        = sum(tank_premium_sched_min),
  sched_max        = sum(tank_premium_sched_max),
  n_tanks             = .N),
  by = .(FACILITY_ID, MONTH, YEAR)
]



## ── write outputs (paths unchanged) ----------------------------------------
rate_fillings_dir <- here::here("Rate FIllings", "Mid-Continent Casualty Company – 23418")

if (!dir.exists(rate_fillings_dir)) dir.create(rate_fillings_dir, recursive = TRUE)

fwrite(ust_tm,
   file.path("Rate FIllings", "Mid-Continent Casualty Company ­– 23418",
           "texas_midcontinent_tank_month_premium_2019_2021.csv"))
fwrite(fac_year,
  file.path("Rate FIllings", "Mid-Continent Casualty Company ­– 23418",
           "texas_midcontinent_facility_year_premium_2019_2021.csv"))

cat("✔ Finished rating Mid‑Continent facilities for contracts effective 2019‑01‑01 to 2021-12-31\n")
