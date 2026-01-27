library(here)
library(data.table)
library(readxl)
library(DBI)
library(odbc)

BASE_PATH <- here('Data/Raw/state_databases')

# ==============================================================================
# ARKANSAS (Access DB)
# ==============================================================================
cat("\n=== ARKANSAS ===\n")
ar_db <- file.path(BASE_PATH, "Arkansas", "TankStats_web.mdb")
con <- dbConnect(odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", ar_db, ";"))

cat("\n-- TempRFACS_CountryNO (Facilities) --\n")
print(head(dbReadTable(con, "TempRFACS_CountryNO"), 3))

cat("\n-- TempTankStats_UG (Tanks) --\n")
print(head(dbReadTable(con, "TempTankStats_UG"), 3))

cat("\n-- TempRLUSTLOG1 (LUST) --\n")
print(head(dbReadTable(con, "TempRLUSTLOG1"), 3))

dbDisconnect(con)

# ==============================================================================
# LOUISIANA (Excel)
# ==============================================================================
cat("\n=== LOUISIANA ===\n")
la_file <- file.path(BASE_PATH, "Louisiana", "LA_record_request.xlsx")

cat("\n-- Tank Info by compartment --\n")
print(head(read_excel(la_file, sheet = "Tank Info by compartment"), 3))

cat("\n-- LUST Sites by incident ID --\n")
print(head(read_excel(la_file, sheet = "LUST Sites by incident ID"), 3))

# ==============================================================================
# MAINE (Text files from web)
# ==============================================================================
cat("\n=== MAINE ===\n")
me_tanks_url <- "https://www.maine.gov/dep/ftp/tanks/tanks_owner_all_registered_tanks.txt"
me_lust_url <- "https://www.maine.gov/dep/ftp/tanks/tanks_hazmat_sites.txt"

cat("\n-- tanks_owner_all_registered_tanks.txt --\n")
print(head(fread(me_tanks_url, sep = "*"), 3))

cat("\n-- tanks_hazmat_sites.txt (LUST) --\n")
print(head(fread(me_lust_url, sep = "*"), 3))

# ==============================================================================
# MICHIGAN (Multi-sheet Excel)
# ==============================================================================
cat("\n=== MICHIGAN ===\n")
mi_file <- file.path(BASE_PATH, "Michigan", "UTK Master List 3-3-25.xlsx")

cat("\n-- Sheet 1 (skip=3) --\n")
print(head(read_excel(mi_file, sheet = 1, skip = 3), 3))

# ==============================================================================
# NEW JERSEY (Two CSVs)
# ==============================================================================
cat("\n=== NEW JERSEY ===\n")

cat("\n-- New Jersey Tanks Data.csv --\n")
print(head(fread(file.path(BASE_PATH, "New Jersey", "New Jersey Tanks Data.csv")), 3))

cat("\n-- New Jersey Facility Data.csv --\n")
print(head(fread(file.path(BASE_PATH, "New Jersey", "New Jersey Facility Data.csv")), 3))

# ==============================================================================
# NEW MEXICO (EPA + State LUST)
# ==============================================================================
cat("\n=== NEW MEXICO ===\n")

cat("\n-- EPA USTs_4.csv --\n")
print(head(fread(here("Data", "Raw", "epa_ust_data_1_8_2024", "USTs_4.csv")), 3))

cat("\n-- EPA Facilities_edited_by_kj.csv --\n")
print(head(fread(here("Data", "Raw", "epa_ust_data_1_8_2024", "Facilities_edited_by_kj.csv")), 3))

nm_lust_files <- list.files(file.path(BASE_PATH, "New Mexico"), pattern = "nfa", full.names = TRUE)
cat("\n-- NM LUST file:", basename(nm_lust_files[1]), "--\n")
print(head(read_excel(nm_lust_files[1]), 3))

# ==============================================================================
# OKLAHOMA (CSVs)
# ==============================================================================
cat("\n=== OKLAHOMA ===\n")
ok_files <- list.files(file.path(BASE_PATH, "Oklahoma"), pattern = "\\.csv$", full.names = TRUE)

for (f in ok_files) {
  cat("\n--", basename(f), "--\n")
  print(head(fread(f), 3))
}