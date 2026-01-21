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