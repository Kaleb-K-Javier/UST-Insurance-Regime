#==============================================================================
# 01b_DataLoad.R
# Load raw source files. No filtering, no variable construction.
# Saves raw objects to INTERIM_DIR for downstream scripts.
#
# Outputs (INTERIM_DIR):
#   annual_data_raw.rds
#   tank_inventory_raw.rds
#   master_lust_raw.rds
#   fr_year_raw.rds          (NULL if file absent)
#   rate_data_raw.rds        (NULL if files absent)
#==============================================================================

source(here::here("Code",'Analysis','Descrptive Facts', "01a_Setup.R"))
cat("=== 01b: DATA LOADING ===\n")

# ── Facility-year panel ───────────────────────────────────────────────────────
PANEL_PATH <- here("Data","Processed","facility_leak_behavior_annual.csv")
if (!file.exists(PANEL_PATH)) stop("Panel not found: ", PANEL_PATH)
annual_data_raw <- fread(PANEL_PATH)
cat(sprintf("  Panel: %s rows | %s facilities | %d-%d\n",
    format(nrow(annual_data_raw),             big.mark = ","),
    format(uniqueN(annual_data_raw$panel_id), big.mark = ","),
    min(annual_data_raw$panel_year), max(annual_data_raw$panel_year)))

# ── Tank inventory ────────────────────────────────────────────────────────────
TANK_PATH <- here("Data","Processed","Master_Harmonized_UST_Tanks.csv")
if (!file.exists(TANK_PATH)) stop("Tank inventory not found: ", TANK_PATH)
tank_inventory_raw <- fread(TANK_PATH)
cat(sprintf("  Tanks: %s rows\n", format(nrow(tank_inventory_raw), big.mark = ",")))

# ── LUST ──────────────────────────────────────────────────────────────────────
LUST_PATH <- here("Data","Processed","Master_Harmonized_LUST.csv")
if (!file.exists(LUST_PATH)) stop("LUST not found: ", LUST_PATH)
master_lust_raw <- fread(LUST_PATH)
cat(sprintf("  LUST: %s rows\n", format(nrow(master_lust_raw), big.mark = ",")))

# ── Texas FR panel (institutional figures) ────────────────────────────────────
FR_PATH <- here("Data","Processed","texas_fr_facility_year_panel.csv")
fr_year_raw <- if (file.exists(FR_PATH)) {
  dt <- fread(FR_PATH)
  cat(sprintf("  TX FR panel: %s rows\n", format(nrow(dt), big.mark = ",")))
  dt
} else {
  cat("  TX FR panel: NOT FOUND — institutional figures will be skipped\n")
  NULL
}

# ── Mid-Continent rate filings ────────────────────────────────────────────────
RATE_DIR <- here(
  "Data", "Rate FIllings",
  "Mid-Continent Casualty Company ­– 23418"
)
rate_files <-c('Data\\Rate FIllings\\Mid-Continent Casualty Company ­– 23418\\texas_midcontinent_facility_year_premium_2006_to_04_2014.csv',
'Data\\Rate FIllings\\Mid-Continent Casualty Company ­– 23418\\texas_midcontinent_facility_year_premium_2014_05_to_2019_01.csv',
'Data\\Rate FIllings\\Mid-Continent Casualty Company ­– 23418\\texas_midcontinent_facility_year_premium_2019_2021.csv',
'Data\\Rate FIllings\\Mid-Continent Casualty Company ­– 23418\\texas_midcontinent_facility_year_premium_2021_onwards.csv')


rate_data_raw <- if (length(rate_files) > 0) {
  dt <- rbindlist(lapply(rate_files, fread), fill = TRUE)
  dt[, YEAR := as.numeric(YEAR)]
  dt <- dt[!is.na(YEAR)]
  cat(sprintf(
    "  Rate filings: %s rows | %d-%d\n",
    format(nrow(dt), big.mark = ","),
    min(dt$YEAR),
    max(dt$YEAR)
  ))
  dt
} else {
  cat("  Rate filings: NOT FOUND\n")
  NULL
}

# ── FR contract month panel ───────────────────────────────────────────────────
CONTRACT_PATH <- here("Data","Processed","texas_fr_contract_month_panel.csv")
contract_month_raw <- if (file.exists(CONTRACT_PATH)) {
  dt <- fread(CONTRACT_PATH)
  cat(sprintf("  FR contract panel: %s rows\n", format(nrow(dt), big.mark=",")))
  dt
} else {
  cat("  FR contract panel: NOT FOUND\n")
  NULL
}

# ── Save to interim ───────────────────────────────────────────────────────────
save_interim(annual_data_raw,    "annual_data_raw")
save_interim(tank_inventory_raw, "tank_inventory_raw")
save_interim(master_lust_raw,    "master_lust_raw")
save_interim(fr_year_raw,        "fr_year_raw")
save_interim(rate_data_raw,      "rate_data_raw")
save_interim(contract_month_raw, "contract_month_raw")

cat("=== 01b COMPLETE ===\n")
