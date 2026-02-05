# Incentivizing Risk Reduction: The Role of Risk-Based Liability Insurance in Managing Underground Storage Tank Pollution

**Theory, Estimation, and Welfare Analysis**

**Author:** Kaleb Javier

**Status:** Job Market Paper (2025-2026)

## 📌 Project Overview

This repository contains the replication code, data construction scripts, and manuscript source files for my Job Market Paper.

**Research Question:** How do alternative insurance regimes—specifically flat-fee state funds versus risk-based private insurance—affect the management of environmental risks?

**Methodology:** [TEXT]

**Key Findings:**
[TEXT]

## 📂 Repository Structure

The codebase is organized into modular pipelines for data processing, reduced-form analysis, and structural estimation.

```text
.
├── Code/
│   ├── Cleaning/          # Raw data processing (EPA, State administrative data)
│   │   ├── 01_Clean_AR.R
│   │   ├── ...
│   │   └── 10_Build_Master_Panel.R
│   ├── Analysis/          # Reduced-form DiD and descriptive evidence
│   │   ├── 02_DiD_Results.R
│   │   └── 06_entry_distortion_analysis.r
│   ├── Dynamic_Model/     # Structural model estimation logic
│   │   ├── 01_Estimate_Hazard.R
│   │   └── 03_Welfare.R
│   ├── Simulation/        # Monte Carlo validation and robustness checks
│   │   └── 01_Monte_Carlo_Master.R
│   └── Helpers/           # Core estimator functions
│       ├── cpp_engine.cpp          # Rcpp acceleration for NPL algorithm
│       └── estimator_functions.R   # Likelihood and Bellman operators
├── Reports/
│   ├── Paper/             # Quarto source for the JMP manuscript
│   └── Slides/            # Presentation slides (Camp Resources, AERE)
└── Output/                # Generated tables, figures, and estimation checkpoints

```

## 💻 Installation & Requirements

The project relies on **R** for data manipulation and estimation, with **C++ (Rcpp)** acceleration for the computationally intensive NPL algorithm.

### Dependencies

Ensure the following R packages are installed:

```r
install.packages(c(
  "data.table",   # High-performance data manipulation
  "Matrix",       # Sparse matrix operations for state transitions
  "Rcpp",         # C++ integration
  "RcppArmadillo",# Linear algebra acceleration
  "fixest",       # High-dimensional fixed effects (DiD)
  "parallel",     # Multi-core estimation
  "here"          # Relative path management
))

```

### C++ Compilation

The structural estimator uses `Code/Helpers/cpp_engine.cpp` for speed. This typically compiles automatically when sourcing the master scripts, provided a C++ compiler (Rtools on Windows, Xcode on Mac, or gcc on Linux) is available.

## 🚀 Usage Guide

### 1. Data Construction

The scripts in `Code/Cleaning/` process raw facility data from state registries and the EPA.

* *Note:* Raw data files are not included in this repository due to privacy/licensing restrictions.
* Run `10_Build_Master_Panel.R` to generate the analysis-ready panel.

### 2. Reduced-Form Analysis

Run `Code/Analysis/02_DiD_Results.R` to replicate the difference-in-differences evidence regarding the Texas 1999 policy change.

### 3. Structural Estimation (Model A & B)

The estimation framework is modular. To verify identification or run the estimator:

```r
# Example: Run Monte Carlo Validation
source("Code/Simulation/01_Monte_Carlo_Master.R")

```

This script:

1. Generates synthetic facility panels.
2. Runs the NPL estimator (checking Models A and B).
3. Outputs Hessian eigenvalue analysis to verify parameter identification.

### 4. Paper Generation

The manuscript is written in Quarto. Render [TEXT]

## 📊 Identification Strategy

[TEXT]

## 📄 Citation

Please cite this draft as:

```bibtex
[TEXT]

```


## Panel Data Structure

**Dataset**: `facility_leak_behavior_annual.csv` / `facility_leak_behavior_annual.rds`  
**Unit of Observation**: Facility-Year  
**Time Period**: 1970-2025  
**Geographic Coverage**: 50 US States + DC (EPA Region 6 + Select States)

### Variable Summary by Category

| Category | Count | Key Variables | Description |
|----------|-------|---------------|-------------|
| **Identifiers** | 6 | `panel_id`, `facility_id`, `state`, `panel_year` | Uniquely identify facility-year observations |
| **Stock Variables** | 12 | `active_tanks_dec`, `total_capacity_dec`, `avg_tank_age_dec` | End-of-year (December 31) snapshots of facility composition |
| **Flow Variables** | 12 | `n_leaks`, `n_closures`, `n_installs`, `n_retrofits` | Annual sums of events/transactions |
| **YoY Changes** | 4 | `capacity_change_year`, `net_tank_change` | Year-over-year differences in December stocks |
| **Leak Classification** | 13 | `tank_closure_revealed`, `tank_closure_known_leak` | 4 robustness specs (Primary/Narrow/Wide/Regulatory) |
| **Duration & Survival** | 17 | `exit_flag`, `ever_leaked`, `cumulative_leaks` | Panel tenure, exit timing, event history |
| **Exit Decomposition** | 5 | `exit_no_leak`, `exit_with_leak` | Mutually exclusive exit types |
| **Financial Responsibility** | 11 | `fr_coverage_share`, `dropped_by_zurich` | Texas FR coverage and treatment variables |
| **Policy & Treatment** | 7 | `post_1999`, `texas_treated`, `rel_year` | DiD treatment indicators |
| **Cohort & Vintage** | 7 | `cohort`, `reg_vintage`, `wall_type` | Facility entry timing and tank characteristics |
| **Fuel Type** | 3 | `has_gasoline_year`, `is_motor_fuel` | Product storage indicators |
| **Aliases** | 7 | `active_tanks`, `total_capacity` | Backward-compatible duplicates of EOY stocks |

**Total Variables**: 104 (excluding 637 wide issuer columns)

### Leak Classification Specifications

Our primary outcome measures use four robustness specifications to classify tank closures:

| Specification | Revealed Leak Window | Known Leak Threshold | Purpose |
|---------------|---------------------|---------------------|---------|
| **Primary** | 0-60 days after closure | 180+ days before closure | Main analysis (balances accuracy & coverage) |
| **Narrow** | 0-30 days after closure | 365+ days before closure | Conservative (minimizes false positives) |
| **Wide** | 0-90 days after closure | 90+ days before closure | Inclusive (maximizes detection) |
| **Regulatory** | 0-45 days after closure | 180+ days before closure | Aligns with reporting requirements |

**Clean Closure**: Closure with NO leak reported in any window (primary spec).

<!-- ### Panel Balance

- **Facilities**: 45,234 unique facilities (varies by specification)
- **States**: 18 states (Texas + EPA Region 6 + select treated states)
- **Observations**: ~850,000 facility-year observations (unbalanced panel)
- **Time Coverage**: 1970-2025 (left-censored at 1970, right-censored at 2025)

### Data Sources

1. **Texas Tank Data**: TCEQ Petroleum Storage Tank database (monthly panels)
2. **EPA Tank Data**: EPA Region 6 + select states tank inventory
3. **LUST Data**: State & EPA Leaking Underground Storage Tank databases
4. **Financial Responsibility**: Texas TCEQ FR compliance data (Texas-only)

### Key Design Features

- **Stock-Flow Consistency**: December snapshots for stocks; annual sums for flows
- **YoY Changes**: Calculated as differences in December stocks (not flow sums)
- **Dual-Date System**: Raw dates preserved for age calculation; panel-bounded dates for backbone
- **Treatment Variables**: Multiple DiD specifications (Texas 1999, 8-state staggered adoption)
- **Exit Definition**: Facility last observed before 2025 (right-censoring at panel end) -->

----------------------------------------------------------------


================================================================================
          MASTER DATA RECOVERY DIRECTORY (DIRECT LINKS & INSTRUCTIONS)
================================================================================

# State Data Recovery Priority Matrix

## 🎯 Special Status States

| State | Study Group | Tanks | LUSTs | Status | Action |
|-------|-------------|------:|------:|--------|--------|
| **PA** | Control Tier 1 | 99,788 | 17,489 | 🎯 Data Available in Repo | Integrate facility data from PA_UST_Auction_Analysis |
| **MO** | Remainder | 41,774 | 6,899 | 📦 Database Acquired | Parse Access database tables |
| **IN** | Remainder | 57,525 | 9,987 | ⏸️ Records Request Pending | Wait for state response |
| **CA** | Remainder | 37,400 | 41,661 | ❌ On Hold | Troubleshoot GeoTracker linkage |
| **CO** | Control Tier 1 | 32,910 | 16,879 | ✅ Completed | None - data integrated |

## 🟢 High Priority (Documented Solutions)

| State | Study Group | Tanks | LUSTs | Critical Issues | Action |
|-------|-------------|------:|------:|-----------------|--------|
| **NY** | Remainder | 122,648 | 25,839 | 100% Closure | Download data.ny.gov datasets |
| **IL** | Remainder | 84,307 | 25,336 | Wall type + LUST NFA | Download SFMO + IEPA databases |
| **GA** | Remainder | 79,273 | 17,924 | 100% Closure | Download GA EPD corrective actions |
| **SC** | Remainder | 45,996 | 10,128 | 100% Closure, Install | Download DES registry |
| **MS** | Remainder | 32,132 | 4,092 | 100% Closure, Capacity, Install | Download MDEQ MUSTERWEB |
| **UT** | Remainder | 18,100 | 4,782 | 100% Closure | Download DERR or GRAMA request |
| **NH** | Remainder | 15,350 | 2,420 | 100% Closure, LUST | Download or email EMD@des.nh.gov |
| **NV** | Remainder | 12,023 | 2,459 | 100% Closure, 86% Capacity | Download + email ust@ndep.nv.gov |
| **VT** | Remainder | 11,001 | 2,182 | 100% Install | Download ANR data dump |
| **WY** | Remainder | 10,172 | 1,630 | 100% Closure, LUST | Download DEQ storage tanks |
| **SD** | Remainder | 10,178 | 3,963 | 80% Closure | Download DANR tanks/spills |
| **NE** | Remainder | 6,188 | 8,717 | 100% Closure | Download SFM regulated tanks |

## 🟡 Medium Priority (Documented Solutions, Minor Issues)

| State | Study Group | Tanks | LUSTs | Issues | Action |
|-------|-------------|------:|------:|--------|--------|
| **NC** | Control Tier 2 | 91,664 | 23,594 | 14% LUST dates | Good quality overall |
| **OH** | Control Tier 2 | 73,200 | 34,249 | 39% LUST dates | Minor recovery needed |
| **AR** | Control Tier 1 | 34,083 | 4,868 | 63% LUST dates | Moderate issue |

## 🔴 Needs Investigation

| State | Study Group | Tanks | LUSTs | Issues |
|-------|-------------|------:|------:|--------|
| **FL** | Excluded (Treated) | 127,314 | 34,940 | 100% Closure |
| **NJ** | Excluded (Treated) | 108,992 | 16,816 | 100% LUST |
| **MI** | Excluded (Treated) | 98,847 | 23,369 | 100% LUST |
| **WI** | Excluded (Treated) | 83,600 | 20,596 | 100% Closure, 80% Install |
| **WA** | Excluded (Other) | 52,245 | 2,864 | 94% Closure, 100% LUST |
| **KY** | Control Tier 2 | 51,771 | 13,115 | 100% LUST |
| **AL** | Control Tier 1 | 53,361 | 5,112 | 100% Capacity |
| **IA** | Excluded (Treated) | 37,200 | 6,334 | 100% Closure |
| **WV** | Remainder | 25,796 | 3,699 | 100% Closure |
| **NM** | Control Tier 1 | 16,322 | 1,880 | 97% Closure, 98% Install |
| **ND** | Control Tier 2 | 10,372 | 1,015 | 100% LUST |
| **RI** | Remainder | 10,834 | 2,083 | 100% Closure |
| **DE** | Remainder | 9,388 | 4,979 | 100% LUST |
| **AK** | Remainder | 6,853 | 2,187 | 100% Closure, LUST |
| **HI** | Remainder | 6,763 | 1,395 | 100% Closure, LUST |

## ✅ Excellent Quality (No Action Needed)

| State | Study Group | Tanks | LUSTs | Notes |
|-------|-------------|------:|------:|-------|
| **TX** | Target | 217,725 | 29,196 | Target state - excellent data |
| **LA** | Control Tier 1 | 46,520 | 4,116 | Minimal missingness |
| **OK** | Control Tier 1 | 40,004 | 6,845 | Good quality |
| **VA** | Control Tier 2 | 78,612 | 12,137 | Minor issues only |
| **MA** | Control Tier 2 | 36,448 | 5,335 | Excellent quality |
| **TN** | Control Tier 1 | 60,277 | 0 | Good quality |
| **ME** | Control Tier 1 | 16,902 | 80,962 | Good quality |
| **KS** | Control Tier 2 | 27,477 | 11,369 | Good quality |
| **ID** | Control Tier 2 | 14,880 | 1,513 | Excellent quality |
| **MT** | Remainder | 16,060 | 4,651 | Good quality |
| **MD** | Control Tier 2 | 54,296 | 12,341 | Good quality |
| **MN** | Control Tier 2 | 45,575 | 11,477 | Good quality |
| **CT** | Excluded (Treated) | 36,076 | 2,270 | Minor issues |
| **AZ** | Excluded (Treated) | 28,544 | 9,100 | Good quality |
| **OR** | Excluded (Other) | 30,900 | 8,565 | Good quality |

---

## Summary Statistics

### By Priority Category
- **Immediate Action (PA + MO)**: 2 states, 141,562 tanks
- **High Priority (Documented)**: 12 states, 454,639 tanks  
- **Needs Investigation**: 16 states, 645,267 tanks
- **Excellent Quality**: 19 states, 1,084,457 tanks

### Control Group Health
- **Control Tier 1**: 7/9 excellent/good (78%) → 8/9 after PA integration (89%)
- **Control Tier 2**: 8/10 excellent/good (80%)

### Expected Impact
- **After PA + MO**: ~140K tanks with complete data
- **After High Priority states**: ~600K additional tanks with complete data
- **Total potential**: 85-90% data completeness across all states


[ STATE: CALIFORNIA (CA) ] ---> askined state for data you cant link LUSTs to sites??
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Closure Dates (All tanks listed as "Open").
DATA TYPE | UST & LUST (GeoTracker)
URL       | https://geotracker.waterboards.ca.gov/data_download/
ACTION    | 1. Download "GeoTracker Cleanup Sites" (LUST) for closure dates.
            2. Download "Permitted UST" file for inventory.
            * Note: GeoTracker is the "gold standard" for CA data.

[ STATE: COLORADO (CO) ] --> big portion of very early tanks that are closed have no closure date in raw.
[Done: Yes]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Closure Dates.
DATA TYPE | UST & LUST (OPS Dashboard)
URL       | https://ops.colorado.gov/Petroleum/DataDocuments
ACTION    | 1. Download "Regulated Storage Tanks in Colorado" (Inventory).
            2. Download "Active and Closed OPS Petroleum Release Events" (LUST).
            * These files are often hidden under "Data & Documents" -> "Spreadsheets".

[ STATE: UTAH (UT) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Closure Dates.
DATA TYPE | UST & LUST (DERR)
URL       | https://deq.utah.gov/environmental-response-and-remediation/ust-compliance
ACTION    | Look for "UST and LUST Lists" or "Interactive Map Data Download".
            * UT DEQ often requires a "GRAMA" request if the direct CSV is down,
            but the "Easy Records Search" usually allows exports.

[ STATE: SOUTH DAKOTA (SD) ]
--------------------------------------------------------------------------------
REASON    | Fixes 86% Missing Dates.
DATA TYPE | UST & LUST (DANR)
URL       | https://danr.sd.gov/Agriculture/Inspection/StorageTanks/default.aspx
ACTION    | 1. Use "Tanks and Spills Map" -> Export Data.
            2. Download "Database of Reported Releases" for event dates.

[ STATE: ILLINOIS (IL) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Wall Type & 100% Missing LUST NFA Dates
DATA TYPE | UST (Inventory/Wall Type)
URL       | https://webapps.sfm.illinois.gov/ustsearch/
ACTION    | Do NOT use the search fields. Look for "Download Data" or "Export" 
            link in the footer/sidebar to get the full facility export.
--------------------------------------------------------------------------------
DATA TYPE | LUST (Leaks/Dates)
URL       | https://epa.illinois.gov/topics/cleanup-programs/lust.html
ACTION    | Download "Leaking UST Database" (Access/Excel) or "LUST Incident 
            Report". Note: This is a different agency (IEPA) than the UST list.

[ STATE: GEORGIA (GA) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Closure Dates & 100% Missing LUST NFA Dates
DATA TYPE | LUST & UST (Combined Utility)
URL       | https://epd.georgia.gov/ust-data-and-reporting
ACTION    | Download "Corrective Action Projects and Cleanups" (Excel).
            * This file is CRITICAL for closure dates.
            * Also grab "UST Public Record Report" for inventory cross-check.

[ STATE: INDIANA (IN) ] --- Have to do a record request their database has it but cant get it publicly.
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Capacity, Wall Type, and Closure Dates
DATA TYPE | UST (Inventory)
URL       | https://www.in.gov/idem/tanks/resources/data-and-reports/
FILE      | "Underground Storage Tanks Public Record Summary" (XLSX)
--------------------------------------------------------------------------------
DATA TYPE | LUST (Leaks)
URL       | https://www.in.gov/idem/tanks/resources/data-and-reports/
FILE      | "LUST Report [XLSX]" (Contains full priority & date history)

[ STATE: MISSOURI (MO) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Closure Dates & 100% Missing LUST NFA Dates
DATA TYPE | UST & LUST (Full Database)
URL       | https://dnr.mo.gov/waste-recycling/sites-regulated-facilities/underground-storage-tanks-database
FILE      | "Underground Storage Tank Summary Database" (Zipped .accdb)
NOTE      | This is a Microsoft Access dump. It contains everything.

[ STATE: NEW YORK (NY) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Wall Type, Closure Dates, and LUST NFA Dates
DATA TYPE | UST (Inventory/Wall Type)
URL       | https://data.ny.gov/Energy-Environment/Bulk-Storage-Facilities-in-New-York-State/pteg-c78n
ACTION    | Click "Export" -> "CSV". Key Column: "Tank Type"
--------------------------------------------------------------------------------
DATA TYPE | LUST (Leaks/Dates)
URL       | https://data.ny.gov/Energy-Environment/Spill-Incidents/u44d-k5fk
ACTION    | Click "Export" -> "CSV". Key Column: "Spill Date"

[ STATE: MISSISSIPPI (MS) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing LUST NFA (No Further Action) Dates
DATA TYPE | UST & LUST
URL       | https://www.mdeq.ms.gov/water/groundwater-assessment-and-remediation/underground-storage-tanks/musterweb/
FILE      | "ZipFile with CSV files for UST Info and Leaking UST Info"

[ STATE: SOUTH CAROLINA (SC) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing LUST NFA (No Further Action) Dates
DATA TYPE | UST (Registry)
URL       | https://apps.des.sc.gov/USTRegistry/
ACTION    | Look for small "Download Registry" link (often Excel/CSV).
--------------------------------------------------------------------------------
DATA TYPE | LUST (Releases)
URL       | https://des.sc.gov/about-des/data-reports
ACTION    | Look for "Leaking UST List" or "Registry of Releases".

[ STATE: WYOMING (WY) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Wall Type & 100% Missing Closure Dates
DATA TYPE | UST (Inventory)
URL       | https://deq.wyoming.gov/shwd/storage-tank/
FILE      | "STP Regulated Storage Tank List" (Excel)
--------------------------------------------------------------------------------
DATA TYPE | LUST (Remediation)
URL       | https://deq.wyoming.gov/shwd/storage-tank/remediation/
FILE      | "Contaminated Site List"

[ STATE: VERMONT (VT) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Wall Type & 100% Missing LUST NFA Dates
DATA TYPE | UST & LUST (Data Dump)
URL       | https://anrweb.vt.gov/dec/ert/DataDump.aspx
ACTION    | 1. Select "UST Tanks" -> Download
            2. Select "Hazardous Sites" -> Download (for LUST dates)

[ STATE: NEBRASKA (NE) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Wall Type, Closure Dates, & LUST NFA Dates
DATA TYPE | UST & LUST
URL       | https://sfm.nebraska.gov/fuels-safety/lists-publications
FILES     | 1. "UST Regulated Tanks" (Excel)
            2. "Public Record Underground Storage Tank Summary"

[ STATE: NEVADA (NV) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Closure Dates & High Missing Capacity
DATA TYPE | LUST (Cleanup Cases)
URL       | https://nevadaenvironmentalactivities.ndep.nv.gov/
ACTION    | Click "Download a List of Active and Closed Cleanup Cases"
--------------------------------------------------------------------------------
DATA TYPE | UST (Inventory)
URL       | https://ndep.nv.gov/land/underground-storage-tanks/ust-system-testing
ACTION    | Email NDEP (ust@ndep.nv.gov) if "Facility List" is not visible.

[ STATE: NEW HAMPSHIRE (NH) ]
--------------------------------------------------------------------------------
REASON    | Fixes 100% Missing Wall Type & 100% Missing Closure Dates
DATA TYPE | UST & LUST
URL       | https://www.des.nh.gov/onestop-navigation
ACTION    | Use "OneStop Data Mapper" to export layers.
BACKUP    | Email EMD@des.nh.gov requesting "Master UST/LUST Excel List".
            (They are responsive and this is often faster than scraping).
-----------------------------------------------------------------------------

================================================================================
                        DEWEY DATA INVENTORY & UST STRATEGY
                            Target: Berkeley PhD Access
================================================================================

[1] DATASET INVENTORY
    Verified available via Berkeley "Academic Full" Tier

--------------------------------------------------------------------------------
DATASET NAME          PROVIDER       DATE RANGE          KEY UTILITY
--------------------------------------------------------------------------------
Places (Core)         SafeGraph      07/2019 - Present   Facility Classification
                                                         (Brand, NAICS, Open/Close)

Monthly Patterns      SafeGraph      01/2018 - Present* Demand Proxy (Vol)
(or Advan Patterns)                                      (Visits, Dwell Time)

Spend Patterns        SafeGraph      01/2020 - Present   Revenue Proxy
                                                         (Trans. Size, Raw Spend)

Nationwide Parcels    Regrid         Current Snapshot    Ownership Classification
                                     (Updated Monthly)   (Public vs. Private)
--------------------------------------------------------------------------------
*Note: "Advan Weekly Patterns Plus" is the active successor to SafeGraph Patterns 
 on Dewey, with backfilled data to 2019. It is preferred for longitudinal work.


[2] UST CLASSIFICATION MATRIX
    How to map your 5 Target Groups using these datasets

--------------------------------------------------------------------------------
GROUP 1: RETAIL BRANDED GAS
--------------------------------------------------------------------------------
Dataset:    SafeGraph Places
Filter:     NAICS_CODE starts with '447' (Gas Stations)
Logic:      BRANDS is NOT NULL (e.g., "Chevron", "Shell")
Linking:    Spatial Join (UST Lat/Long -> SafeGraph Geometry)

--------------------------------------------------------------------------------
GROUP 2: RETAIL NON-BRANDED GAS
--------------------------------------------------------------------------------
Dataset:    SafeGraph Places
Filter:     NAICS_CODE starts with '447'
Logic:      BRANDS is NULL
Warning:    Verify 'location_name' doesn't contain brand strings to avoid
            false negatives.

--------------------------------------------------------------------------------
GROUP 3: PRIVATE COMPANY (NON-RETAIL)
--------------------------------------------------------------------------------
Dataset:    SafeGraph Places
Filter:     NAICS_CODE is NOT '447'
            Look for: '48-49' (Transportation/Warehousing)
                      '23'    (Construction)
                      '42'    (Wholesale Trade)
Logic:      These are fleet yards, rental centers, or logistics hubs.
Note:       Spend Data (Consumer Cards) will be NULL here. Do not use it.

--------------------------------------------------------------------------------
GROUP 4: PUBLICLY OWNED
--------------------------------------------------------------------------------
Dataset:    Regrid Parcels (California Partition Only)
Filter:     OWNER column
Logic:      Text Search for Keywords:
            "CITY OF", "COUNTY", "STATE OF", "SCHOOL DIST", "USA", "FED"
Linking:    Spatial Point-in-Polygon (UST Lat/Long -> Regrid Parcel Shape)

--------------------------------------------------------------------------------
GROUP 5: TRIBAL
--------------------------------------------------------------------------------
Dataset:    ** EXTERNAL SOURCE ** (Not in Dewey)
Source:     Bureau of Indian Affairs (BIA) LAR Shapefile
URL:        https://www.bia.gov/bia/gis
Linking:    Spatial Point-in-Polygon (UST Lat/Long -> Tribal Boundary)
Reasoning:  Tribal sovereignty is a legal status, not a business attribute.
            POI data is unreliable for this legal distinction.


[3] DEMAND & REVENUE ESTIMATION STRATEGY
    Proxies for "Sales Totals" (which do not exist as absolute values)

--------------------------------------------------------------------------------
METRIC A: PHYSICAL DEMAND (Gallons Proxy)
--------------------------------------------------------------------------------
Source:     SafeGraph/Advan Monthly Patterns
Variable:   `raw_visit_counts`
Constraint: Filter for `dwell_time` < 20 mins (Refueling behavior)
Math:       Station_Visits / Market_Total_Visits = Market_Share_Index

--------------------------------------------------------------------------------
METRIC B: REVENUE INTENSITY (Dollars Proxy)
--------------------------------------------------------------------------------
Source:     SafeGraph Spend
Variable:   `raw_total_spend`
Variable:   `median_spend_per_transaction`
Warning:    This is PANEL data (sample of credit cards), not a ledger.
            It captures Convenience Store (C-Store) spending mixed with Fuel.
            Use it to compare Station A vs. Station B, not to calculate tax.

================================================================================
END OF SPEC
================================================================================