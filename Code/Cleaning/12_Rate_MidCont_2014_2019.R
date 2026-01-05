###############################################################################
# Mid-Continent Casualty – Texas UST premium engine (05-2014 ·· 01-2019)
###############################################################################
#  1.  Loads panel & static-tank data that you already produced
#  2.  Filters to facilities whose FA-issuer is Mid-Continent during 2014-05…2019-01
#  3.  Re-uses all rating factors from the 2006–11 engine EXCEPT the age-factor,
#      which is replaced by the granular table in the 06/14 filing
#  4.  Outputs tank-month & facility-year premia
###############################################################################


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

ust_raw[,CLOSED_DATE:=ymd(end_date)]

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
  FACILITY_ID, UST_ID, INSTALL_DATE, tank_status, is_closed_removed,
  CLOSED_DATE,
  # construction
  is_steel_cathodic, is_double_walled_steel,
  is_reinforced_fiberglass, is_other_tank_material,
  # piping
  pip_steel_cathodic, pip_fiberglass, pip_double_walled,
  pip_suction, pip_pressure_ll, pip_other,
  # detection
  det_interstitial, det_ATG, det_vapor,
  det_groundwater, det_SIR, det_other
)]

# 10) optional write‑out ------------------------------------------------------
fwrite(static_tank_details,
       get_data_path( "texas_static_tank_details.csv"))



###############################################################################
##  SECTION B  –  Mid‑Continent OK‑SERFF premium engine (2014‑05 … 2019‑01) ##
###############################################################################

WINDOW_START <- as.Date("2014-05-01")
WINDOW_END   <- as.Date("2019-01-31")

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
][
  , .(FACILITY_ID, YEAR, MONTH, EFF_DATE, COVER_OCC, COVER_AGG,ISSUER_NAME)
]

midc_fac_ids <- unique(fa_contracts$FACILITY_ID)

## ────────────────────────────────────────────────────────────────────────────
## 2 ▸ Static tank attributes  – *no* pre‑filter on is_closed_removed        ##
## ────────────────────────────────────────────────────────────────────────────
tank_static <- fread( get_data_path("texas_static_tank_details.csv") )[
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
tank_month[ ,
  age_years := pmax(0, floor(as.numeric(EFF_DATE - INSTALL_DATE) / 365.25))
]


ust_tm = tank_month

###############################################################################
##  SECTION C  –  Mid‑Continent secondary‑rating factors (refactored)       ##
###############################################################################
##
##  * All helpers are 100 % vectorised.
##  * They accept 1 / 0 / TRUE / FALSE / NA – whatever is in the columns.
##  * Missing values are treated as “flag not present”.
###############################################################################

## ---- tiny helper: coerce 0–1 / NA → logical -------------------------------
to_bool <- function(x) !is.na(x) & as.logical(x)

## ---- 1) Tank–status factor -------------------------------------------------
status_factor <- function(status_chr) {
  fcase(
    status_chr == "In Use",                  0.00,
    status_chr == "Temporarily Out of Use", -0.50,
    status_chr == "Permanently Out of Use", -0.75,
    default = NA_real_                      # drop from rating if unknown
  )
}

## ---- Increased‑Limit Factor (ILF) ------------------------------------------
## Base manual rate is for 500 k / 1 m.  Anything missing defaults to the
## statutory 1 m / 1 m limits.
ilf_factor <- function(occ, agg) {
  occ <- fifelse(is.na(occ), 1e6, occ)
  agg <- fifelse(is.na(agg), 1e6, agg)

  fcase(
    occ == 1e6 & agg == 1e6, 1.18,            # 1 m / 1 m
    occ == 1e6 & agg == 2e6, 1.20,            # 1 m / 2 m
    default = 1.18                            # by law they must have 1 m / 1 m so it will be 1.18 by assumption
  )
}


## NEW granular age factor (06/14 filing) ------------------------------------
age_factor <- function(a){
  fcase(is.na(a),       NA_real_,
        a <=  5,  -0.10,
        a <= 10,   0.00,
        a <= 15,   0.05,
        a <= 20,   0.10,
        a <= 25,   0.20,
        a <= 30,   0.25,
        a <= 35,   0.35,
        a <= 40,   0.45,
        a <= 45,   0.55,
        a <= 50,   0.65,
        a  > 50,   0.75)
}

## ---- 3) Tank‑construction factor ------------------------------------------
construction_factor <- function(is_steel_cathodic,
                                 is_reinforced_fiberglass,
                                 is_double_walled_steel) {

  dw  <- to_bool(is_double_walled_steel)
  sc  <- to_bool(is_steel_cathodic)
  frp <- to_bool(is_reinforced_fiberglass)

  fcase(
    dw,          -0.20,           # biggest credit first
    sc | frp,     0.00,           # neutral
    default =     0.00            # “other”
  )
}

## ---- 4) Leak‑detection factor ---------------------------------------------
leak_factor <- function(det_interstitial,
                        det_ATG,
                        det_vapor,
                        det_groundwater) {

  di <- to_bool(det_interstitial)
  da <- to_bool(det_ATG)
  dv <- to_bool(det_vapor)
  dg <- to_bool(det_groundwater)

  fcase(
    di,          -0.20,                  # Interstitial Monitoring
    da,          -0.10,                  # ATG / LLD
    dv | dg,     -0.10,                  # Vapor or Ground‑water monitoring
    default =     0.00                   # SIR / none / other
  )
}

## ---- 5) Piping‑construction factor ----------------------------------------
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

## ---- 6) Piping‑design factor ----------------------------------------------
pipe_design_factor <- function(pip_suction,
                               pip_pressure_ll) {

  su <- to_bool(pip_suction)
  pr <- to_bool(pip_pressure_ll)

  ## Mid‑Continent filing gives 0 charge/credit for either design;
  ## keep the function for completeness / future tweaks.
  fcase(
    pr,  0.00,
    su,  0.00,
    default = 0.00
  )
}

## ── 7.  Derive every sub‑load as its *own* column ---------------------------
ust_tm[ , status_load       := status_factor(tank_status)                     ]
ust_tm[ , ilf := ilf_factor(COVER_OCC, COVER_AGG) ]
ust_tm[ , age_load          := age_factor(age_years)                          ]
ust_tm[ , construction_load := construction_factor(
                                 is_steel_cathodic,
                                 is_reinforced_fiberglass,
                                 is_double_walled_steel)                      ]

ust_tm[ , leak_load         := leak_factor(
                                 det_interstitial,
                                 det_ATG,
                                 det_vapor,
                                 det_groundwater)                             ]

ust_tm[ , pipe_const_load   := pipe_const_factor(
                                 pip_steel_cathodic,
                                 pip_fiberglass,
                                 pip_double_walled)                           ]

ust_tm[ , pipe_design_load  := pipe_design_factor(
                                 pip_suction,
                                 pip_pressure_ll)                             ]

ust_tm[ , flood_load        := 0 ] # Don't have this data at the moment
ust_tm[ , marina_load       := 0 ] # Don't have this data at the moment
ust_tm[ , aquifer_load      := 0 ] # Don't have this data at the moment

## ---- 8) Combine into one secondary‑factor column ---------------------------
sec_cols <- c("status_load","age_load","construction_load",
              "leak_load","pipe_const_load","pipe_design_load",
              "flood_load","marina_load","aquifer_load")

ust_tm[ , sec_load := rowSums(.SD, na.rm = TRUE), .SDcols = sec_cols ]

## quick sanity check
summary(ust_tm$sec_load)



## ── Tank‑month premium (NO tank‑level $500 minimum) ──────────
ust_tm[ , tank_premium := BASE_RATE * ilf * (1 + sec_load) ]

## schedule‑rating envelope (±40 %) – no hard floor
ust_tm[ , `:=`(
  tank_premium_sched_min = tank_premium * SCHED_MIN_FACTOR,
  tank_premium_sched_max = tank_premium * SCHED_MAX_FACTOR
)]

summary(ust_tm$tank_premium)

## histogram of tank premia

ggplot(ust_tm, aes(x = tank_premium)) +
  geom_histogram(binwidth = 25, center = 200, fill = "lightblue", color = "black") +
  labs(
    title = "Mid‑Continent Tank Premia (2006–11)",
    x = "Tank Premium ($)",
    y = "Count"
  ) +
  theme_minimal()

fac_year <- ust_tm[, .(
  number_of_tanks = uniqueN(UST_ID),
  mean_tank_premium = mean(tank_premium, na.rm = TRUE),
  mean_tank_age = mean(age_years, na.rm = TRUE),
  base_premium = sum(tank_premium),
  sched_min    = sum(tank_premium_sched_min),
  sched_max    = sum(tank_premium_sched_max)
), by = .(FACILITY_ID, MONTH, YEAR)]
## histogram of facility premia

# Count how many facility-months have base_premium < 500 and their share of total premium
below_500 <- fac_year[base_premium < 500]
n_below_500 <- nrow(below_500)
total_fac_months <- nrow(fac_year)
share_fac_months <- n_below_500 / total_fac_months

total_premium <- sum(fac_year$base_premium, na.rm = TRUE)
premium_below_500 <- sum(below_500$base_premium, na.rm = TRUE)
share_premium <- premium_below_500 / total_premium

cat(sprintf(
  "\nFacility-months with base_premium < $500: %d of %d (%.1f%%)\nShare of total premium: $%.0f of $%.0f (%.1f%%)\n",
  n_below_500, total_fac_months, 100 * share_fac_months,
  premium_below_500, total_premium, 100 * share_premium
))

ggplot(fac_year %>% filter(## filter to the bottom 99% of premia
               base_premium < quantile(base_premium, 0.99)),
     aes(x = base_premium)) +
  geom_histogram(binwidth = 50, fill = "lightgreen", color = "black") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "red") +
  labs(
  title = "Mid‑Continent Facility Premia (2006–14)",
  x = "Facility Premium ($)",
  y = "Count"
  ) +
  theme_minimal()

#create two  time series plosts where we plot the mean and the mean bounds for each year
## the two time sereis is one where I just have base premium  with the mean min and max boudns we computed
## the second time series is the same, but we force all premia that is under $500 to be 500
# Collapse to one row per YEAR with mean values
series_1 <- fac_year[, .(
  mean_base_premium = mean(base_premium, na.rm = TRUE),
  mean_sched_min    = mean(sched_min, na.rm = TRUE),
  mean_sched_max    = mean(sched_max, na.rm = TRUE)
), by = YEAR]

series_2 <- fac_year[, .(
  mean_base_premium = mean(pmax(base_premium, 500), na.rm = TRUE),
  mean_sched_min    = mean(pmax(sched_min, 500), na.rm = TRUE),
  mean_sched_max    = mean(pmax(sched_max, 500), na.rm = TRUE)
), by = YEAR]



ggplot(series_1, aes(x = YEAR)) +
  geom_line(aes(y = mean_base_premium, color = "Base Premium"), size = 1) +
  geom_line(aes(y = mean_sched_min, color = "Min Premium"), size = 1, linetype = "dashed") +
  geom_line(aes(y = mean_sched_max, color = "Max Premium"), size = 1, linetype = "dashed") +
  labs(
    title = "Mid‑Continent Facility Premia (2006–14)",
    x = "Year",
    y = "Mean Premium ($)"
  ) +
  scale_color_manual(values = c("Base Premium" = "blue", "Min Premium" = "green", "Max Premium" = "red")) +
  theme_minimal()


ggplot(series_2, aes(x = YEAR)) +
  geom_line(aes(y = mean_base_premium, color = "Base Premium"), size = 1) +
  geom_line(aes(y = mean_sched_min, color = "Min Premium"), size = 1, linetype = "dashed") +
  geom_line(aes(y = mean_sched_max, color = "Max Premium"), size = 1, linetype = "dashed") +
  labs(
    title = "Mid‑Continent Facility Premia (2006–14)",
    x = "Year",
    y = "Mean Premium ($)"
  ) +
  scale_color_manual(values = c("Base Premium" = "blue", "Min Premium" = "green", "Max Premium" = "red")) +
  theme_minimal()

## plot the mean_basepream from both series to see how they compare
ggplot() +
  geom_line(data = series_1, aes(x = YEAR, y = mean_base_premium, color = "Original Base Premium"), size = 1) +
  geom_line(data = series_2, aes(x = YEAR, y = mean_base_premium, color = "Adjusted Base Premium"), size = 1) +
  labs(
    title = "Comparison of Mean Base Premiums (2006–14)",
    x = "Year",
    y = "Mean Base Premium ($)"
  ) +
  scale_color_manual(values = c("Original Base Premium" = "blue", "Adjusted Base Premium" = "orange")) +
  theme_minimal()

## write out the tank and facility id level premia data for issuer and dates 
# Change output directory to "Rate FIllings/Mid-Continent Casualty Company ­– 23418"
out_dir <- here::here("Rate FIllings", "Mid-Continent Casualty Company ­– 23418")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ──────────────────────────────── write outputs ──────────────────────────────
fwrite(ust_tm,
  file.path("Rate FIllings", "Mid-Continent Casualty Company ­– 23418",
       "texas_midcontinent_tank_month_premium_2014_05_to_2019_01.csv"))
fwrite(fac_year,
  file.path("Rate FIllings", "Mid-Continent Casualty Company ­– 23418",
       "texas_midcontinent_facility_year_premium_2014_05_to_2019_01.csv"))

cat("✔ Finished rating Mid-Continent facilities for 05-2014 – 01-2019\n")
