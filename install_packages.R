# install_packages.R - COMPLETE PACKAGE LIST FOR UST INSURANCE PROJECT
# Run this script to install all required packages for the entire project
# Usage: source('install_packages.R')

cat("=" , strrep("=", 70), "=\n", sep = "")
cat("  UST Insurance Regime Project - Complete Package Installation\n")
cat("=" , strrep("=", 70), "=\n\n", sep = "")
cat("This will install ~80 packages. Estimated time: 15-20 minutes\n\n")

# CRAN packages - comprehensive list from entire repo
cran_packages <- c(
  # Core data manipulation
  "data.table", "dplyr", "tidyr", "purrr", "tibble", "tidyverse",
  "readr", "readxl", "haven", "janitor", "magrittr",
  
  # Econometrics & causal inference
  "fixest", "lfe", "did", "panelView", "marginaleffects",
  "fastDummies", "plm",
  
  # Survival & competing risks
  "survival", "cmprsk", "survminer", "coxme",
  
  # Statistical inference
  "fwildclusterboot", "sandwich", "lmtest", "multcomp", "car",
  
  # Machine learning
  "pROC", "randomForest", "ranger", "gbm", "glmnet", "caret",
  
  # Visualization
  "ggplot2", "gridExtra", "grid", "patchwork", "cowplot",
  "viridis", "RColorBrewer", "ggthemes", "ggridges", "ggalluvial", "ggpubr",
  "lattice",
  
  # Tables & reporting  
  "kableExtra", "stargazer", "modelsummary", "gt", "flextable",
  "officer", "rmarkdown", "knitr", "tinytex",
  
  # Spatial analysis
  "sf", "sp", "tigris", "maps", "spdep", "spatialreg",
  "tidygeocoder", "stringdist",
  
  # Utilities
  "here", "broom", "scales", "stringr", "lubridate", "glue",
  "fs", "tools",
  
  # Parallel computing
  "parallel", "foreach", "doParallel", "furrr", "progressr",
  
  # Matrix operations
  "Matrix", "Rcpp", "RcppArmadillo",
  
  # APIs & data sources
  "fredr", "tidycensus", "httr", "jsonlite",
  
  # Database connectivity
  "DBI", "odbc",
  
  # Time series
  "zoo", "tseries", "urca", "dynlm",
  
  # Numerical methods
  "numDeriv", "digest",
  
  # Testing & diagnostics
  "testthat", "microbenchmark",
  
  # Fast storage
  "fst",
  
  # Clustering
  "cluster", "factoextra",
  
  # Additional specialized packages
  "Hmisc", "rms", "vcd", "colorspace", "xtable"
)

# GitHub packages (installed separately)
github_packages <- list(
  list(repo = "asheshrambachan/HonestDiD", name = "HonestDiD"),
  list(repo = "jonathandroth/pretrends", name = "pretrends"),
  list(repo = "evanjflack/bacondecomp", name = "bacondecomp")
)

cat("STEP 1: Installing CRAN packages (%d total)\n", length(cran_packages))
cat(strrep("-", 72), "\n\n")

# Install CRAN packages with progress tracking
installed_count <- 0
failed_cran <- character(0)

for (i in seq_along(cran_packages)) {
  pkg <- cran_packages[i]
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("[%d/%d] Installing %s...", i, length(cran_packages), pkg))
    
    tryCatch({
      install.packages(pkg, dependencies = TRUE, quiet = TRUE)
      installed_count <- installed_count + 1
      cat(" ✓\n")
    }, error = function(e) {
      cat(" ✗ FAILED\n")
      failed_cran <- c(failed_cran, pkg)
    })
  } else {
    cat(sprintf("[%d/%d] ✓ %s already installed\n", i, length(cran_packages), pkg))
  }
}

cat("\n", strrep("=", 72), "\n")
cat("STEP 2: Installing GitHub packages\n")
cat(strrep("-", 72), "\n\n")

# Install remotes if needed
if (!requireNamespace("remotes", quietly = TRUE)) {
  cat("Installing remotes package...\n")
  install.packages("remotes", quiet = TRUE)
}

library(remotes)

failed_github <- character(0)

for (pkg_info in github_packages) {
  pkg_name <- pkg_info[["name"]]
  pkg_repo <- pkg_info[["repo"]]
  
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    cat(sprintf("Installing %s from GitHub (%s)...", pkg_name, pkg_repo))
    
    tryCatch({
      remotes::install_github(pkg_repo, quiet = TRUE, upgrade = "never")
      cat(" ✓\n")
    }, error = function(e) {
      cat(" ✗ FAILED:", conditionMessage(e), "\n")
      failed_github <- c(failed_github, pkg_name)
    })
  } else {
    cat(sprintf("✓ %s already installed\n", pkg_name))
  }
}

# Final summary
cat("\n", strrep("=", 72), "\n")
cat("INSTALLATION COMPLETE!\n")
cat(strrep("=", 72), "\n\n")
cat(sprintf("CRAN packages installed: %d/%d\n", 
            installed_count, length(cran_packages)))
cat(sprintf("GitHub packages installed: %d/%d\n",
            length(github_packages) - length(failed_github), 
            length(github_packages)))

if (length(failed_cran) > 0) {
  cat("\n⚠ Failed CRAN packages:\n")
  cat("  ", paste(failed_cran, collapse = ", "), "\n")
}

if (length(failed_github) > 0) {
  cat("\n⚠ Failed GitHub packages:\n")
  cat("  ", paste(failed_github, collapse = ", "), "\n")
  cat("\nTo install manually:\n")
  for (pkg_info in github_packages) {
    if (pkg_info[["name"]] %in% failed_github) {
      cat(sprintf("  remotes::install_github('%s')\n", pkg_info[["repo"]]))
    }
  }
}

cat("\n✓ Ready to run analysis scripts!\n")
cat(strrep("=", 72), "\n")
