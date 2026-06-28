###############################################################################
# Tank Owners Members Insurance Company (TOMICS) – Texas UST premium engine
# Filing:    SERFF TEXS-131241913
#            "TOMIC 2017 10_rate_ TEXS_131241913.pdf"  (re-file, eff. 2018-01-01)
# Window:    TOMICS presence in FR data (~2007-2018); era tagged by contract YEAR
# Built:     2026-06-27
# Mirrors:   Code/Cleaning/12_Rate_MidCont_2006_2011.R (same A-E structure)
#
# OPEN ITEMS (documented, not blocking)
#   [ded]    Deductible absent from contract panel [V2] — held at $5k ref (0.000)
#   [retro]  Retro date not in FR/tank data        — held at 0-1yr ref (0.000)
#   [contam] Prior contamination via LUST join     — deferred to v2
#
# FIELD-NAME FLAGS (update if Snippet A shows different names)
#   TOMICS_FLD_COMPOSITE  <- "TANK_MAT_COMPOSITE"
#   TOMICS_FLD_FLEX_PIPE  <- "PIP_MAT_FLEX"
#   Section A will hard-stop and print actual names if either is absent.
###############################################################################

library(data.table)
library(lubridate)
library(here)

# ── Constants ─────────────────────────────────────────────────────────────────
POLICY_MIN_PREMIUM <- 350L           # p.2: minimum policy premium
SCHED_MIN_FACTOR   <- 0.60           # p.3: schedule ±40% cap
SCHED_MAX_FACTOR   <- 1.40
ENERGY_ACT_DATE    <- as.Date("2006-01-01")   # New vs Replace/Upgrade cutoff

ISSUER_EXACT       <- "TANK OWNERS MEMBERS INS CO"   # [V1] confirmed 388,658 rows

# Raw field names for TOMICS extras not in the Mid-Continent static table.
# Confirm these against Snippet A (names(fread(raw_path, nrows=0))).
TOMICS_FLD_COMPOSITE <- "TANK_MAT_COMPOSITE"   # Tank Material: steel w/ FRP cladding
TOMICS_FLD_FLEX_PIPE <- "PIP_MAT_FLEX"         # Piping Material: nonmetallic flexible

out_dir <- here("Data", "Analysis", "rate_engines")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)


###############################################################################
## SECTION A – Static tank table + TOMICS extensions                         ##
##                                                                            ##
## Loads texas_static_tank_details.csv (pre-built by 12_Rate_MidCont Section A)
## then joins two extra fields from raw_pst_ust.csv:                         ##
##   (a) is_composite     — FRP or FRP-clad steel tank (filing: "composit tanks")
##   (b) pip_dw_rigid     — double-wall rigid pipe (NOT flex; filing: "dbl/wall pipes")
## and derives:                                                               ##
##   (c) base_category    — New / Replace / Upgrade (2006-01-01 cutoff +     ##
##                          prior-tank-at-facility check)                     ##
###############################################################################

cat("=== SECTION A: static tank data ===\n")

static_path <- here("Data","Raw_do_not_write","panel_merge_staging",
                    "texas_static_tank_details.csv")
if (!file.exists(static_path))
  stop("texas_static_tank_details.csv not found — run 12_Rate_MidCont_2006_2011.R first")

static_tank <- fread(static_path)
cat(sprintf("  Loaded static table: %d tanks\n", nrow(static_tank)))

# ── Load raw UST file for TOMICS extras ─────────────────────────────────────
raw_path <- here("Data","Raw_do_not_write","panel_merge_staging","raw_pst_ust.csv")
if (!file.exists(raw_path))
  raw_path <- here("Data","Raw","state_databases","Texas","raw_pst_ust.csv")
if (!file.exists(raw_path))
  stop("raw_pst_ust.csv not found at staging path or Data/Raw/state_databases/Texas/")

# Read only the columns we need (fast even for large file)
needed_raw <- c("FACILITY_ID","UST_ID","INSTALL_DATE",
                "TANK_MAT_FRP", TOMICS_FLD_COMPOSITE,
                "PIP_DOUBLE",   TOMICS_FLD_FLEX_PIPE)

actual_names <- names(fread(raw_path, nrows = 0))
missing_flds <- setdiff(needed_raw, actual_names)
if (length(missing_flds) > 0) {
  cat("  STOP: these field names were not found in raw_pst_ust.csv:\n")
  cat(paste(" ", missing_flds, collapse = "\n"), "\n")
  cat("  Actual names in file:\n")
  print(actual_names)
  stop("Update TOMICS_FLD_COMPOSITE / TOMICS_FLD_FLEX_PIPE at top of script and re-run.")
}

raw_ust <- fread(raw_path, select = needed_raw)
raw_ust[, FACILITY_ID  := trimws(substr(FACILITY_ID, 1, 6))]
raw_ust[, UST_ID       := toupper(trimws(UST_ID))]
raw_ust[, INSTALL_DATE := ymd(INSTALL_DATE)]

# (a) Composite tank: pure FRP OR steel w/ FRP cladding → -0.150 credit
raw_ust[, is_composite := as.integer(
  get(TOMICS_FLD_COMPOSITE) == "Y" | TANK_MAT_FRP == "Y"
)]

# (b) Double-wall rigid pipe — flex pipe gets 0 credit per filing
#     pip_double_walled from static table captures PIP_DOUBLE == "Y"
#     Here we flag the flex exception so Section C can zero it out
raw_ust[, pip_flex := as.integer(
  !is.na(get(TOMICS_FLD_FLEX_PIPE)) & get(TOMICS_FLD_FLEX_PIPE) == "Y"
)]

# (c) New / Replace / Upgrade
#   min install date per facility across ALL tanks (incl. removed — captures pit history)
fac_min <- raw_ust[!is.na(INSTALL_DATE),
                   .(min_fac_install = min(INSTALL_DATE, na.rm = TRUE)),
                   by = FACILITY_ID]
raw_ust  <- fac_min[raw_ust, on = "FACILITY_ID"]

raw_ust[, base_category := fcase(
  INSTALL_DATE <  ENERGY_ACT_DATE,                                    "Upgrade",
  INSTALL_DATE >= ENERGY_ACT_DATE & INSTALL_DATE > min_fac_install,  "Replace",
  INSTALL_DATE >= ENERGY_ACT_DATE & INSTALL_DATE <= min_fac_install, "New",
  default = "Upgrade"
)]

cat("  base_category distribution (all TX tanks):\n")
print(raw_ust[, .N, by = base_category][order(-N)])

# ── Merge TOMICS extras onto static table ───────────────────────────────────
extra <- raw_ust[, .(FACILITY_ID, UST_ID, is_composite, pip_flex, base_category)]
static_tank <- extra[static_tank, on = c("FACILITY_ID","UST_ID")]

# pip_double_walled_rigid = double-wall AND NOT flex
static_tank[, pip_dw_rigid := as.integer(
  pip_double_walled == 1 & (is.na(pip_flex) | pip_flex == 0)
)]

cat(sprintf("  Extended static table: %d tanks\n", nrow(static_tank)))


###############################################################################
## SECTION B – TOMICS contracts                                               ##
###############################################################################

cat("=== SECTION B: TOMICS contracts ===\n")

contract_path <- here("Data","Processed","texas_fr_contract_month_panel.csv")
if (!file.exists(contract_path))
  stop("texas_fr_contract_month_panel.csv not found")

fa_contracts <- fread(contract_path)[
  ISSUER_NAME == ISSUER_EXACT,
  .(FACILITY_ID, YEAR, MONTH, EFF_DATE, COVER_OCC, COVER_AGG, ISSUER_NAME)
][, EFF_DATE := ymd(EFF_DATE)]

stopifnot(nrow(fa_contracts) > 0)
cat(sprintf("  TOMICS contracts: %d rows | %d facilities | years %d-%d\n",
            nrow(fa_contracts), uniqueN(fa_contracts$FACILITY_ID),
            min(fa_contracts$YEAR), max(fa_contracts$YEAR)))

tomics_facs <- unique(fa_contracts$FACILITY_ID)
tank_static <- static_tank[FACILITY_ID %in% tomics_facs]

# Cartesian join: tank × contract-month at same facility
tank_month <- fa_contracts[tank_static, on = "FACILITY_ID",
                            allow.cartesian = TRUE, nomatch = 0]

# Keep only tanks in-service at contract inception
tank_month <- tank_month[
  INSTALL_DATE <= EFF_DATE &
  (is.na(CLOSED_DATE) | CLOSED_DATE > EFF_DATE)
]

# Age at contract inception
tank_month[, age_years := pmax(0L, floor(as.numeric(EFF_DATE - INSTALL_DATE) / 365.25))]

cat(sprintf("  Tank-month rows (in-service): %d\n", nrow(tank_month)))
ust_tm <- tank_month


###############################################################################
## SECTION C – TOMICS rating factor functions  (TEXS-131241913)              ##
##                                                                            ##
## Formula: tank_premium = base_rate × (1 + Σ loads)                        ##
## All factors are additive %-loads from the filing's "percent DR or CR" col ##
###############################################################################

cat("=== SECTION C: rating factor functions ===\n")

to_bool <- function(x) !is.na(x) & as.logical(x)

## C1. Base rate: New / Replace / Upgrade  (filing p.2 base definitions) ------
##   New     $250 — post-2005 Energy Act, new pit, no prior tank at facility
##   Replace $340 — post-2005 Energy Act, facility had a prior tank
##   Upgrade $420 — pre-Energy Act (both 1998-retrofit era and older)
base_rate_tomics <- function(cat) {
  fcase(cat == "New", 250L, cat == "Replace", 340L, default = 420L)
}

## C2. Policy limits ILF (filing p.1) ----------------------------------------
##   Reference = 1MM/1MM (0.000).  TOMICS already prices at 1M/1M base.
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

## C3-C5. Deductible / defense limit / retro / prior contamination ------------
##   All held at filing reference (0.000); see open-item log at top of script.
##   Deductible: $5k reference — field absent from contract panel [V2].
##   Defense limit: $1M reference — not in data.
##   Retro date: 0-1yr reference — not in data.
##   Prior contamination: LUST join deferred to v2.

## C6. Age of tanks (filing p.1) ---------------------------------------------
age_load_tomics <- function(a) {
  fcase(
    is.na(a),  NA_real_,
    a <=  2,    0.000,
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

## C7. Leak detection (filing p.2) -------------------------------------------
##   interstitial monitoring = -0.150 | sump monitors = -0.150
##   det_interstitial already covers both (DET_C_INTERSTITIAL | DET_P_INTERSTITIAL)
##   All other detection types = 0.000 ("required leak detection" reference)
leak_load_tomics <- function(det_interstitial) {
  fifelse(to_bool(det_interstitial), -0.150, 0.000)
}

## C8. Site capacity (filing p.2) --------------------------------------------
##   Header: "Above Ground Tanks Only" — all our tanks are USTs → 0.000

## C9. Tank construction (filing p.2) ----------------------------------------
##   single walled = 0.000 (ref) | dbl/wall tanks = -0.300 | composit = -0.150
##   DW evaluated first; composite only if not DW (can't stack both)
construction_load_tomics <- function(double_walled, is_composite) {
  fcase(
    to_bool(double_walled),  -0.300,
    to_bool(is_composite),   -0.150,
    default =                 0.000
  )
}

## C10. Pipe construction (filing p.2) ----------------------------------------
##   dbl/wall pipes (rigid) = -0.150 | dble wall flex pipe = 0.000 | other = 0.000
pipe_load_tomics <- function(pip_dw_rigid) {
  fifelse(to_bool(pip_dw_rigid), -0.150, 0.000)
}


###############################################################################
## SECTION D – Apply factors, compute tank-month premiums                    ##
###############################################################################

cat("=== SECTION D: apply rating factors ===\n")

ust_tm[, base_rate         := base_rate_tomics(base_category)]
ust_tm[, ilf_load          := ilf_load_tomics(COVER_OCC, COVER_AGG)]
ust_tm[, age_load          := age_load_tomics(age_years)]
ust_tm[, leak_load         := leak_load_tomics(det_interstitial)]
ust_tm[, construction_load := construction_load_tomics(double_walled, is_composite)]
ust_tm[, pipe_load         := pipe_load_tomics(pip_dw_rigid)]

# Reference-held loads (all 0.000; documented above)
ust_tm[, c("ded_load","defense_load","retro_load","contam_load","capacity_load") := 0.000]

sec_cols <- c("ilf_load","age_load","leak_load","construction_load","pipe_load",
              "ded_load","defense_load","retro_load","contam_load","capacity_load")
ust_tm[, sec_load := rowSums(.SD, na.rm = TRUE), .SDcols = sec_cols]

ust_tm[, tank_premium := base_rate * (1 + sec_load)]

stopifnot(all(is.finite(ust_tm$tank_premium)))
stopifnot(all(ust_tm$tank_premium >= 0))

# Schedule ±40% bounds (page 3) — minimum applied at facility level in Section E
ust_tm[, `:=`(
  tank_premium_sched_min = tank_premium * SCHED_MIN_FACTOR,
  tank_premium_sched_max = tank_premium * SCHED_MAX_FACTOR
)]

cat(sprintf("  tank_premium: mean=$%.0f  min=$%.0f  max=$%.0f\n",
            mean(ust_tm$tank_premium, na.rm = TRUE),
            min(ust_tm$tank_premium,  na.rm = TRUE),
            max(ust_tm$tank_premium,  na.rm = TRUE)))


###############################################################################
## SECTION E – Facility rollup + $350 minimum policy premium                 ##
##                                                                            ##
## The $350 minimum is applied AFTER schedule modification so that even the  ##
## most heavily credited small facility can't go below $350.                 ##
###############################################################################

cat("=== SECTION E: facility rollup ===\n")

fac_year <- ust_tm[, .(
  n_tanks       = uniqueN(UST_ID),
  mean_age      = mean(age_years,    na.rm = TRUE),
  base_premium  = sum(tank_premium,           na.rm = TRUE),
  sched_min     = sum(tank_premium_sched_min, na.rm = TRUE),
  sched_max     = sum(tank_premium_sched_max, na.rm = TRUE)
), by = .(FACILITY_ID, YEAR, MONTH)]

# $350 floor applied to all three premium columns
fac_year[, base_premium_w_min := pmax(base_premium, POLICY_MIN_PREMIUM)]
fac_year[, sched_min_w_min    := pmax(sched_min,    POLICY_MIN_PREMIUM)]
fac_year[, sched_max_w_min    := pmax(sched_max,    POLICY_MIN_PREMIUM)]

below_min <- fac_year[base_premium < POLICY_MIN_PREMIUM, .N]
cat(sprintf("  Facility-months below $%d minimum: %d of %d (%.1f%%)\n",
            POLICY_MIN_PREMIUM, below_min, nrow(fac_year),
            100 * below_min / nrow(fac_year)))
cat(sprintf("  Facility-year premium (with min): mean=$%.0f  min=$%.0f  max=$%.0f\n",
            mean(fac_year$base_premium_w_min),
            min(fac_year$base_premium_w_min),
            max(fac_year$base_premium_w_min)))


###############################################################################
## SECTION F – Write outputs                                                  ##
###############################################################################

cat("=== SECTION F: write outputs ===\n")

fwrite(ust_tm,
  file.path(out_dir, "tomics_131241913_tank_month_premium.csv"))

fwrite(fac_year,
  file.path(out_dir, "tomics_131241913_facility_year_premium.csv"))

cat(sprintf("  Written to %s/\n    tomics_131241913_tank_month_premium.csv\n    tomics_131241913_facility_year_premium.csv\n",
            out_dir))
cat("\n=== DONE ===\n")
