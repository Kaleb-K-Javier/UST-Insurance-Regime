# Latitude/Longitude Data Loading Analysis

## Scripts that LOAD lat/long from raw data:

1. **01_Clean_AR.R** (Arkansas)
   - Source: `LOC_LATITUDE`, `LOC_LONGITUDE` from facilities table
   - Lines: 76-77 (load), 248-249 (aggregate)

2. **02_Clean_LA.R** (Louisiana)
   - Source: `x_coord_standard_value`, `y_coord_standard_value` from tank data
   - Lines: 210-211 (load), 224-225 (rename), 231-232 (convert), 310-311 (aggregate)

3. **03_Clean_ME.R** (Maine)
   - Source: `latitude`, `longitude` columns from raw tank data
   - Lines: 124-125 (select), 129-133 (placeholder logic + convert), 260-261 (aggregate)
   - Note: Has placeholder logic if columns missing, but loads from raw when present

4. **05_Clean_NJ.R** (New Jersey)
   - Source: `x`, `y` coordinates from facility data (State Plane EPSG:3424)
   - Conversion: Converts State Plane to Lat/Long using `sf` package
   - Lines: 110-111 (load), 125-133 (convert), 136-140 (merge/placeholder), 226-227 (aggregate)

5. **06_Clean_NM.R** (New Mexico)
   - Source: EPA `Facilities.csv` file (`latitude`, `longitude`)
   - Lines: 86 (join cols), 122-123 (select), 146-147 (placeholder + convert), 243-244 (aggregate)

6. **07_Clean_OK.R** (Oklahoma)
   - Source: `latitude`, `longitude` from raw tank data
   - Lines: 106-107 (select), 129-130 (convert), 187-188 (aggregate)

7. **10_Clean_AL.R** (Alabama)
   - Source: `latitude`, `longitude` from sites data
   - Lines: 158-159 (load), 194 (include in output)

8. **11_Clean_TN.R** (Tennessee)
   - Source: EPA `Facilities.csv` file (`latitude`, `longitude`)
   - Lines: 142-163 (load from EPA), 278 (merge), 293 (include in output)

9. **13_Clean_CO.R** (Colorado)
   - Source: `latitude`, `longitude` from Release file (not Tank file)
   - Method: Creates facility-level geo lookup from Release file, merges to tanks
   - Lines: 145-146 (initial placeholders), 186-188 (load from Release), 196-199 (geo lookup), 202-203 (merge), 245-246 (aggregate)

## Scripts that CREATE PLACEHOLDERS (no raw data):

1. **04_Clean_MI.R** (Michigan)
   - Creates: `latitude := NA_real_`, `longitude := NA_real_` if missing
   - Lines: 215-217 (create placeholders), 236-237 (aggregate)
   - Note: No raw lat/long data available in Michigan files

2. **08_Clean_TX.R** (Texas)
   - Status: **CONFIRMED - NOT INCLUDED** - No lat/long columns loaded or created
   - Verification: Searched entire script - no latitude/longitude references found
   - Note: Texas script focuses on panel construction and does not include lat/long in harmonized output

## Summary:

- **Load from Raw Data**: AR, LA, ME, NJ, NM, OK, AL, TN, CO (9 states)
- **Placeholders Only**: MI (1 state)
- **Not Included**: TX (1 state)
