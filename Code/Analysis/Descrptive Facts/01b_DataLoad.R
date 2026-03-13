#==============================================================================
# 01b_DataLoad.R
# Load raw source files. No filtering, no variable construction.
# Saves raw objects to INTERIM_DIR for downstream scripts.
#
# Outputs (INTERIM_DIR):
#   annual_data_raw.rds
#   tank_inventory_raw.rds
#   master_lust_raw.rds
#   fr_year_raw.rds
#   rate_data_raw.rds
#   contract_month_raw.rds
#==============================================================================
 
source(here::here("Code", "Analysis", "Descrptive Facts", "01a_Setup.R"))
open_log("01b_DataLoad")
log_cat("=== 01b: DATA LOADING ===\n")
 
# ── Facility-year panel ───────────────────────────────────────────────────────
PANEL_PATH <- here("Data", "Processed", "facility_leak_behavior_annual.csv")
if (!file.exists(PANEL_PATH))

  stop("Panel not found: ", PANEL_PATH)
annual_data_raw <- fread(PANEL_PATH)
log_cat(sprintf("  Panel: %s rows | %s facilities | %d-%d\n",
    format(nrow(annual_data_raw),             big.mark = ","),
    format(uniqueN(annual_data_raw$panel_id), big.mark = ","),
    min(annual_data_raw$panel_year), max(annual_data_raw$panel_year)))
 
# ── Tank inventory ────────────────────────────────────────────────────────────
TANK_PATH <- here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv")
if (!file.exists(TANK_PATH))
  stop("Tank inventory not found: ", TANK_PATH)
tank_inventory_raw <- fread(TANK_PATH)
 
 
log_cat(sprintf("  Tanks: %s rows\n", format(nrow(tank_inventory_raw), big.mark = ",")))
 
# ── LUST ──────────────────────────────────────────────────────────────────────
LUST_PATH <- here("Data", "Processed", "Master_Harmonized_LUST.csv")
if (!file.exists(LUST_PATH))
  stop("LUST not found: ", LUST_PATH)
master_lust_raw <- fread(LUST_PATH)
log_cat(sprintf("  LUST: %s rows\n", format(nrow(master_lust_raw), big.mark = ",")))
 
# ── Texas FR panel (institutional figures) ────────────────────────────────────
FR_PATH <- here("Data", "Processed", "texas_fr_facility_year_panel.csv")
if (!file.exists(FR_PATH))
  stop("TX FR panel not found: ", FR_PATH,
       "\n  → Required for institutional figures. Provide the file or remove 01k from the run.")
fr_year_raw <- fread(FR_PATH)
log_cat(sprintf("  TX FR panel: %s rows\n", format(nrow(fr_year_raw), big.mark = ",")))
 

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
CONTRACT_PATH <- here("Data", "Processed", "texas_fr_contract_month_panel.csv")
if (!file.exists(CONTRACT_PATH))
  stop("FR contract panel not found: ", CONTRACT_PATH)
contract_month_raw <- fread(CONTRACT_PATH)
log_cat(sprintf("  FR contract panel: %s rows\n",
                format(nrow(contract_month_raw), big.mark = ",")))
 
# ── Save to interim ───────────────────────────────────────────────────────────
save_interim(annual_data_raw,    "annual_data_raw")
save_interim(tank_inventory_raw, "tank_inventory_raw")
save_interim(master_lust_raw,    "master_lust_raw")
save_interim(fr_year_raw,        "fr_year_raw")
save_interim(rate_data_raw,      "rate_data_raw")
save_interim(contract_month_raw, "contract_month_raw")
 
log_cat("=== 01b COMPLETE ===\n")
close_log("01b_DataLoad")