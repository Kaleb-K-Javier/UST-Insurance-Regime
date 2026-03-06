# Texas UST Reform — Modular Analysis Pipeline

## File Map

```
Code/
├── 00_RunAll.R                       ← Master runner (run this first)
│
├── 01a_Setup.R                       ← Constants, theme, helpers (sourced by all modules)
├── 01b_DataLoad.R                    ← Reads raw CSVs → Data/Interim/*_raw.rds
├── 01c_DataPrep.R                    ← Filters, variable construction → Data/Interim/*.rds
│
├── 01e_InstallTrends.R               ← New tanks/facilities by year; SW share over time
├── 01f_CapitalStock.R                ← Age density, vintage composition, wall-by-vintage
├── 01g_PreTrends.R                   ← Pre-period trends, mandate spike, Wald tests (TableB4)
├── 01h_SampleRestrictionFunnel.R     ← Attrition waterfall + balance at each step (TableB2)
├── 01i_CompositionBalance.R          ← Full vs. make-model composition (Table 2)
├── 01j_AgeHeterogeneity.R            ← Within-cohort age imbalance, KM survival, age-at-closure
├── 01k_InstitutionalContext.R        ← FR coverage, pricing, switching (graceful skip if absent)
├── 01l_DataQuality.R                 ← TableB1/B3 + KEY FIGURE: restriction earns parallel trends
├── 01m_MakeModelSample.R             ← *** Writes Data/Analysis/ for 02_DiD ***
├── 01n_CVValidation.R                ← k-fold CV risk scores (guarded by RUN_FULL flag)
│
└── 02_DiD_Main_MakeModel_HEADER.R   ← Paste this at the top of your 02_DiD script
```

## Data Flow

```
Data/Processed/*.csv      (raw source)
        │
        ▼
    01b_DataLoad
        │ *_raw.rds
        ▼
    01c_DataPrep
        │ annual_data.rds, tanks_1998.rds,
        │ closed_tanks.rds, attrition_log.rds, ...
        ▼
  01e → 01n (figures)
        │ km_1990_1997.rds, mm_closed_tanks_for_H3.rds,
        │ cv_data.rds, pt_results.rds, ...
        ▼
    01m_MakeModelSample
        │
        ├── Data/Analysis/analysis_annual_data.rds
        ├── Data/Analysis/analysis_tank_inventory.rds
        ├── Data/Analysis/analysis_tanks_1998.rds
        ├── Data/Analysis/analysis_closed_tanks.rds
        └── Data/Analysis/analysis_metadata.rds
                │
                ▼
    02_DiD_Main_MakeModel.R   (reads only from Data/Analysis/)
```

## Panel Figure Convention

Every multi-panel figure produces **both** individual panels and a patchwork via
`save_panels()` in `01a_Setup.R`:

| File | Contents |
|------|----------|
| `Figure_X_PanelA.png` | Individual panel A (ready to drop into paper) |
| `Figure_X_PanelB.png` | Individual panel B |
| `Figure_X_Combined.png` | Patchwork of all panels |
| `Figure_X_PanelA.pdf` | PDF version (for LaTeX `\includegraphics`) |

Faceted figures (same measure split by subgroup) are saved as a single figure,
not split into individual panels.

## Key Figure: `Figure_RestrictionEarnsParallelTrends_Combined`

2×2 grid (produced by `01l_DataQuality.R`):

|           | Closure Rate Trend      | TX − Control Difference |
|-----------|------------------------|------------------------|
| **Full sample** | Mandate spike visible  | Pre-trend rejected     |
| **Make-model**  | Clean parallel trends  | Pre-trend NOT rejected |

This is the paper's central identification argument in a single figure.

## Running

```r
# Full pipeline (all figures + export for DiD):
source(here::here("Code", "00_RunAll.R"))

# Quick run (skip slow CV):
# Set SKIP_CV_VALIDATION = TRUE in 00_RunAll.R, or RUN_FULL = FALSE in 01a_Setup.R

# Individual module (after 01b and 01c have run once):
source(here::here("Code", "01f_CapitalStock.R"))

# DiD estimation:
source(here::here("Code", "02_DiD_Main_MakeModel.R"))
```

## Inter-module Dependencies

| Module | Reads from Interim | Writes to Interim/Analysis |
|--------|--------------------|---------------------------|
| 01b    | (raw CSVs)         | `*_raw.rds`               |
| 01c    | `*_raw.rds`        | `annual_data`, `tanks_1998`, `closed_tanks`, `attrition_log`, ... |
| 01e    | `tanks`, `annual_data` | `Data_InstallTanks_ByYear.csv` |
| 01f    | `tanks_1998`       | `Data_VintageWall_Summary.csv` |
| 01g    | `annual_data`      | **`pt_results.rds`** (used by 01l, 02) |
| 01h    | `annual_data`, `tanks_1998`, `attrition_log` | `TableB2_Balance_By_Step.csv` |
| 01i    | `annual_data`, `tanks_1998` | `Table2_CompositionBalance.csv` |
| 01j    | `tanks_1998`, `closed_tanks`, `annual_data` | **`km_1990_1997.rds`**, **`mm_closed_tanks_for_H3.rds`** |
| 01k    | `fr_year_raw`, `rate_data_raw`, `contract_month_raw` | (figures only) |
| 01l    | `annual_data`, `data_quality_report`, `attrition_log`, `balance_glm`, `pt_results` | (figures + tables only) |
| 01m    | `annual_data`, `tanks_1998`, `tanks`, `closed_tanks`, `pt_results` | **`Data/Analysis/*.rds`** (→ 02_DiD), `mm_annual_data`, `mm_tanks_1998` |
| 01n    | `annual_data`      | **`Data/Analysis/analysis_cv_data.rds`** (→ 02_DiD H2b) |
| 02_DiD | `Data/Analysis/*.rds`, `km_1990_1997`, `mm_closed_tanks_for_H3` | Tables 3–10, Figures 6–9 |
