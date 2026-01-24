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