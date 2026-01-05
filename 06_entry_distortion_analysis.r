library(data.table)
library(here)
library(lubridate)
library(tidyverse)
library(fixest)

# 1. Load Data
tank_data <- fread(here("Data", "Processed", "all_tank_data.csv"))
lust_data <- fread(here("Data", "Processed", "all_lust_data.csv"))

# 2. Prepare Tank Data
tank_data[, tank_installed_date := suppressWarnings(as_date(tank_installed_date))]
tank_data[, install_year := year(tank_installed_date)]
tank_data <- tank_data[!is.na(install_year)]

# glimpse(tank_data)
# Rows: 1,165,522
# Columns: 26
# $ facility_id                    <chr> "26", "26", "32", "32", "32", "32", "32…
# $ tank_id                        <chr> "TANK 1", "TANK 2", "TANK 1", "TANK 2",…
# $ tank_installed_date            <date> 1941-01-01, 1941-01-01, 1978-01-01, 19…
# $ tank_closed_date               <IDate> 1990-02-26, 1990-02-26, 1995-04-04, 1…
# $ leak_after_closure             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ leak_before_NFA_before_closure <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ leak_before_NFA_after_closure  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ no_leak                        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
# $ capacity                       <dbl> 10000, 10000, 10000, 10000, 10000, 1000…
# $ single_walled                  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
# $ double_walled                  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
# $ missing_walled                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
# $ unknown_walled                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
# $ is_gasoline                    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
# $ is_diesel                      <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
# $ is_oil_kerosene                <lgl> FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,…
# $ is_jet_fuel                    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
# $ is_other                       <lgl> TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, F…
# $ county_name                    <chr> "", "", "", "", "", "", "", "", "", "",…
# $ county_geoid                   <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ state                          <chr> "New Mexico", "New Mexico", "New Mexico…
# $ file_source                    <chr> "C:/Users/kaleb/Box/UST-Insurance/Data/…
# $ panel_id                       <chr> "26_TANK 1", "26_TANK 2", "32_TANK 1", …
# $ facility_tank_id               <chr> "26_TANK 1_NM", "26_TANK 2_NM", "32_TAN…
# $ state_abb                      <chr> "NM", "NM", "NM", "NM", "NM", "NM", "NM…
# $ install_year                   <dbl> 1941, 1941, 1978, 1978, 1978, 1978, 197…

# count the number of observations by state and the number of observations without a county name and county geoid

missing_county <- tank_data[install_year >= 1980, .(missing_county = sum(is.na(county_name)|county_name == ""),
                                missing_geoid = sum(is.na(county_geoid)),
                                obs = .N), by = .(state_abb,state)]
# order the data by state and the number of observations without a county name and county geoid
missing_county <- missing_county[order(state_abb, -missing_county)]
view(missing_county)
print(missing_county)
 
#    state_abb          state missing_county missing_geoid    obs
#  1:        AL        Alabama            448           448  21840
#  2:        AR       Arkansas              0             0  15188
#  3:        CT    Connecticut              0             0  15642
#  4:        ID          Idaho              0             0   8006
#  5:        IL       Illinois              0          2250  26574
#  6:        KS         Kansas              6             6  10678
#  7:        LA      Louisiana          26072           327  26072
#  8:        MA  Massachusetts             18            18  17340
#  9:        ME          Maine           4740          4740  14150
# 10:        MI       Michigan              0             0  32556
# 11:        MN      Minnesota             85            85  21570
# 12:        MT        Montana              9            16   6461
# 13:        NC North Carolina             67            67  36197
# 14:        NJ     New Jersey            198           198  40938
# 15:        NM     New Mexico             53           427   6303
# 16:        OH           Ohio             55            55  35489
# 17:        OK       Oklahoma            719           719  17585
# 18:        PA   Pennsylvania              1             1  50010
# 19:        SD   South Dakota              4           103   5090
# 20:        TN      Tennessee             12            12  25454
# 21:        TX          Texas              1             1 111443
# 22:        VA       Virginia             27            27  37394
## need to do a new county_id that is created by groupping the two county ids together for now.
## Note I have no county data for in a big portion of the data.
# i guess I just toss those obs who are missing the geoid and use that for now.

# Ensure logical columns and replace NA with FALSE for counting
bool_cols <- c("single_walled", "double_walled", "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other")
for (col in bool_cols) {
  tank_data[[col]] <- as.logical(tank_data[[col]])
  tank_data[[col]][is.na(tank_data[[col]])] <- FALSE
}
glimpse(tank_data)
# Filter for tanks with gasoline, diesel, or both (allowing for other oil/kerosene but excluding jet fuel)
tank_data_filtered <- tank_data[
  (is_gasoline | is_diesel) &
    !is_jet_fuel
]



tank_data_with_geoid <- tank_data_filtered[!is.na(county_geoid) & county_geoid != ""]

nrow(tank_data) 
nrow(tank_data_with_geoid)
nrow(tank_data_with_geoid)/nrow(tank_data) # 0.9998
#[1] 0.6741795
agg_cols <- c("state_abb", "county_geoid", "county_name", "install_year")

tank_counts <- tank_data_with_geoid[
  ,
  .(
    total_new_tanks = .N,
    new_single_wall = sum(single_walled),
    new_double_wall = sum(double_walled)
  ),
  by = agg_cols
]
setnames(tank_counts, "install_year", "year")

# 4. Create State-County-Year Panel
panel_years <- 1980:2022
  unique_pairs <- unique(tank_data[, .(state_abb, county_geoid,county_name)])
  panel_data <- CJ(state_abb = unique_pairs$state_abb, county_geoid = unique_pairs$county_geoid,county_name = unique_pairs$county_name, year = panel_years, unique = TRUE)

# 5. Merge Data
panel_data <- merge(panel_data, tank_counts, by = agg_cols, all.x = TRUE)
panel_data[is.na(total_new_tanks), total_new_tanks := 0]
panel_data[is.na(new_single_wall), new_single_wall := 0]
panel_data[is.na(new_double_wall), new_double_wall := 0]

# 6. Output
fwrite(panel_data, here("Data", "Processed", "state_county_year_tank_counts.csv"))