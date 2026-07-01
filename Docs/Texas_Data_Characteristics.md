# Texas UST Data — Characteristics Catalog (for rate-engine sessions)

Single reference for **what Texas data we have, where it lives, every rating-relevant field, and how
it is coded** — so rate-filing sessions don't re-discover the schema (and don't guess codings). Pair
with `.claude/SESSION_PROMPT_036b_fr_mapping.md`. The live column/value/fill profile is auto-generated
by `Code/Cleaning/profile_texas_data.R` into `Docs/Texas_Data_Characteristics_profile.md` — regenerate
it on the server after any data refresh.

Server repo root: `C:\Users\kalebkja\ust_ins_move_to_github\` (junctioned to `D:\shares\...`; same files).
All of `Data/` is gitignored and lives on the SERVER — the laptop is code-only.

## Data sources & locations

| File | What it is | Key fields | Notes |
|---|---|---|---|
| `Data/Raw/state_databases/Texas/pst_fin_assur.txt` | RAW financial-responsibility records | issuer, coverage, mechanism, dates | source for the FR panels (locate exact path via profiler) |
| **`Data/Raw/state_databases/Texas/raw_pst_ust.csv`** ✓verified | RAW tank registry (parsed) — the **rich construction** file | `TANK_*`, `PIP_*`, `DET_*`, `CORR_*`, `MAT_*`, `PIPE_TYPE`, `STATUS`, `INSTALL_DATE` | the ONLY source of construction/detection detail. Use the `.csv`; the sibling `pst_ust.txt` is the raw FIXED-WIDTH source (unparsed) |
| `Data/Processed/texas_fr_facility_month_panel.csv` | FR facility×month panel | `ISSUER_NAME`, `max_COVER_OCC/AGG`, `CATEGORY`, `DETAIL_TYPE`, flags | **~22 GB — DuckDB only**, never `fread` |
| `Data/Processed/texas_fr_contract_month_panel.csv` | FR contract×month panel (the engine's `fa_monthly_contract`) | `ISSUER_NAME`, `COVER_OCC`, `COVER_AGG`, `EFF/EXP_DATE` | smaller; the rate engines read this |
| `texas_static_tank_details.csv` | per-tank attribute dummies | construction / piping / detection / status / age | NOT on the server — **regenerate via Mid-Continent Section A from `raw_pst_ust.csv` above** (the hardcoded `Raw_do_not_write/panel_merge_staging/` path is wrong) |
| `Data/Analysis/panel_dt.csv`, `pm_panel.csv` | model tank/facility panels | wall, age, capacity, G | carry wall/age/capacity but the **finer construction dummies were dropped** — use the raw tank file for those |
| `tx_midcont_premium_all_1999_onwards.csv` | the BUILT Mid-Continent premium card | per-tank premium by age×wall×era | consumed by `PM02_Lookups.R`; proof the inputs above existed at build time |
| `Docs/petroleum-storage-tank-data-specifications (10).docx` | the official data dictionary | — | `.docx` — convert with pandoc to read |

## Build lineage
`00_Download_TX_UST_Database.R` (raw download) → `08_Clean_TX_FR.R` (FR month/year/contract panels +
`ISSUER_NAME`, coverage, mechanism, switch/gap flags) → `10_Build_Annual_Panel_Optimized.R` / `02b`
(model panels). Rate engines: `Code/Cleaning/12_Rate_MidCont_2006_2011.R` (canonical) → `12_..2014_2019`,
`13_..2019_2021`, `14_..2021_onwards`. Each engine: **Section A** builds the static tank table from the
raw tank file; **Section B** filters `fa_monthly_contract` to the carrier's `ISSUER_NAME` and joins;
**Section C** applies the filing's factors → tank-month + facility-year premia.

## Rating field map (VERIFIED from the running Mid-Continent engine, `12_Rate_MidCont_2006_2011.R` §A)
The construction/detection detail is in the **raw tank file**, engineered into `texas_static_tank_details.csv`.
Do NOT flag these as unobservable — they are explicit `"Y"/"N"` columns.

| Rating dimension | Raw field(s) | Pre-built dummy |
|---|---|---|
| wall SW/DW | `TANK_SINGLE`, `TANK_DOUBLE` | `single_walled`, `double_walled` |
| material / **FRP** / composite | `TANK_MAT_STEEL`, `TANK_MAT_FRP`, `CORR_TANK_CP` | `is_double_walled_steel`, `is_reinforced_fiberglass`, `is_steel_cathodic`, `is_other_tank_material` |
| leak detection (incl. **"sump"/interstitial**) | `DET_C_INTERSTITIAL`/`DET_P_INTERSTITIAL`, `DET_C_ATG`/`DET_P_LLD`, `DET_C_VAPOR`/`DET_P_VAPOR`, `DET_C_GW`/`DET_P_GW`, `DET_C_SIR`/`DET_P_SIR` | `det_interstitial`, `det_ATG`, `det_vapor`, `det_groundwater`, `det_SIR`, `det_other` |
| piping | `PIPE_TYPE`, `PIP_MAT_STEEL`, `PIP_MAT_FRP`, `PIP_DOUBLE`, `CORR_PIPE_CP`, `DET_P_LLD` | `pip_steel_cathodic`, `pip_fiberglass`, `pip_double_walled`, `pip_suction`, `pip_pressure_ll` |
| tank status | `STATUS` | `tank_status` (In Use / Temp / Perm Out of Use) |
| age | `INSTALL_DATE` (+ `end_date`) | `age_years = floor((EFF_DATE − INSTALL_DATE)/365.25)` |
| coverage limit (ILF) | `COVER_OCC`, `COVER_AGG` (in `fa_monthly_contract`) | `ilf` |
| carrier | `ISSUER_NAME` (in `fa_monthly_contract`) | the FR↔engine crosswalk |

## Coding gotchas (verified — don't re-guess)
- **`tp_fa_met` / `fp_corr_met`**: coded `TRUE` / `FALSE` / `NA` — **NOT `Y`/`N`**. And **degenerate**:
  ~100% `TRUE` among insured, `NA` for the ~78% non-insured. So they're just an "is-insured" indicator,
  **NOT a usable third-party / corrective-action credit**. Don't wire them as rating factors.
- **`max_COVER_OCC` / `max_COVER_AGG`**: scientific-notation **text** (`1e+06`=$1M, `2e+06`=$2M). ~91% on
  $1M, ~8% on $2M, modal $1M every year, no drift → hold the limit fixed at $1M.
- **`ISSUER_NAME` is the FR *mechanism***, not carrier-among-insured: ~78% of facility-years are
  `NO COVERAGE` / `SELF(FIN TEST OR LOCAL GOV FT)` / `PARENT (GUARANTEE)`; only ~22% are real carriers.
  TOMICS = `TANK OWNERS MEMBERS INS CO`; some rows are pipe-joined combos (`A | B`).
- **Construction/detection live ONLY in `raw_pst_ust.csv`** (217,725 tanks, 107 cols) — the model
  panels (`panel_dt`) carry only `mm_wall`/capacity/age. The raw file has BOTH raw flags AND pre-built
  derived dummies — **USE THE DERIVED DUMMIES** (all TRUE/FALSE, consistent):
  `single_walled`/`double_walled`; `is_fiberglass_tank` (=FRP, 53k), `is_composite_tank` (41k),
  `is_steel_tank`; `has_interstitial_det`/`has_electronic_det`/`has_manual_stat_det`/`has_any_det`;
  fuel `is_gasoline`/`is_diesel`/`is_oil_kerosene`/`is_jet_fuel`/`is_other`. Finer raw flags exist too:
  `TANK_MAT_*`, `PIP_MAT_FLEX` (flex pipe, 21k), `DET_C_INTERSTITIAL`/`DET_P_INTERSTITIAL`, `CORR_TANK_CP`.
- **MIXED-CODING TRAP in the raw tank file**: `TANK_SINGLE`/`TANK_DOUBLE` are **`TRUE`/`FALSE`**, but
  `TANK_MAT_*`/`PIP_*`/`DET_*`/`CORR_*` are **`Y`/`N`**. So Mid-Continent's `TANK_DOUBLE == "Y"`
  (`12_Rate_MidCont` §A) silently yields **all-FALSE** wall dummies against this file — use the pre-built
  `single_walled`/`double_walled` instead. (Material/detection `== "Y"` are fine; only the wall flags are the trap.)
- **Deductible**: CONFIRMED absent from BOTH the FR panel and the contract panel
  (`texas_fr_contract_month_panel.csv` cols are `…COVER_OCC, COVER_AGG, PREMIUM_PREPAID, PROOF_OF_FA,
  FP_CORR_MET, TP_FA_MET, USE_PRIVATE, USE_SELF` — no deductible). Mid-Continent assumes the standard
  **$5k** (base rate "500k/1m, $5k deductible"); do the same.
- **Data quality ramps over time**: FR insurance coverage is near-empty pre-2008, then ~80–95% filled
  2008–2022. Profile time-varying fields BY YEAR, not pooled.

## Live data profile (auto-generated)
Regenerate on the server (writes `Docs/Texas_Data_Characteristics_profile.md`):
```powershell
& "C:\Program Files\R\R-4.4.3\bin\x64\Rscript.exe" Code/Cleaning/profile_texas_data.R
```
That script locates each file (robust to path drift), dumps the full column inventory, and tabulates
the value distribution (with `NA`) of every construction / detection / mechanism / coverage field — so
the exact codings are always one run away.
