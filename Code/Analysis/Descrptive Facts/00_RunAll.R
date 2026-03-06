#==============================================================================
# 00_RunAll.R
# Master runner: sources all 01x descriptive modules, then signals readiness
# for 02_DiD_Main_MakeModel.R.
#
# Execution order:
#   01a  Setup & constants       (no data, no output)
#   01b  Data loading            (→ raw interim RDS)
#   01c  Data preparation        (→ processed interim RDS)
#   01e  Install trends          (→ Figures: InstallTanks, InstallFacilities, SWShare)
#   01f  Capital stock           (→ Figures 1-3: AgeDensity, Vintage, WallByVintage)
#   01g  Pre-trends              (→ Figure 4, TableB4, saves pt_results)
#   01h  Restriction funnel      (→ Figure_Funnel, TableB2)
#   01i  Composition balance     (→ Figure_CompositionBalance, Table2)
#   01j  Age heterogeneity       (→ Figure 5, Figure_Survival, Figure_AgeAtClosure)
#   01k  Institutional context   (→ FR Coverage/Pricing/Switching figures)
#   01l  Data quality + key fig  (→ Figure_RestrictionEarnsParallelTrends, TableB1/B3)
#   01m  Make-model export       (→ ANALYSIS_DIR: all RDS files that 02_DiD reads)
#   01n  CV validation           (→ analysis_cv_data.rds; guarded by RUN_FULL)
#
# After this script, run:
#   source(here::here("Code", "02_DiD_Main_MakeModel.R"))
#
# Usage:
#   Rscript Code/00_RunAll.R
#   # or interactively:
#   source(here::here("Code", "00_RunAll.R"))
#
# Module-level flags (set below):
#   SKIP_INSTALL_TRENDS    = FALSE
#   SKIP_CAPITAL_STOCK     = FALSE
#   SKIP_PRE_TRENDS        = FALSE
#   SKIP_FUNNEL            = FALSE
#   SKIP_COMPOSITION       = FALSE
#   SKIP_AGE_HTE           = FALSE
#   SKIP_INSTITUTIONAL     = FALSE
#   SKIP_DATA_QUALITY      = FALSE
#   SKIP_CV_VALIDATION     = FALSE   (also requires RUN_FULL=TRUE in 01a)
#==============================================================================

SKIP_INSTALL_TRENDS <- FALSE
SKIP_CAPITAL_STOCK  <- FALSE
SKIP_PRE_TRENDS     <- FALSE
SKIP_FUNNEL         <- FALSE
SKIP_COMPOSITION    <- FALSE
SKIP_AGE_HTE        <- FALSE
SKIP_INSTITUTIONAL  <- FALSE
SKIP_DATA_QUALITY   <- FALSE
SKIP_CV_VALIDATION  <- FALSE

library(here)

# ── Utility ──────────────────────────────────────────────────────────────────
run_module <- function(script, skip = FALSE) {
  name <- basename(script)
  if (skip) {
    cat(sprintf("\n[SKIP] %s\n", name)); return(invisible(NULL))
  }
  cat(sprintf("\n%s\n%s\n", strrep("=",60), name))
  t0 <- proc.time()["elapsed"]
  tryCatch(
    source(here("Code", script), local = FALSE),
    error = function(e) {
      cat(sprintf("  *** ERROR in %s: %s\n", name, conditionMessage(e)))
      cat("  *** Continuing with next module...\n")
    }
  )
  elapsed <- proc.time()["elapsed"] - t0
  cat(sprintf("  [done] %s in %.1f sec\n", name, elapsed))
  invisible(NULL)
}

wall_start <- proc.time()["elapsed"]
cat(sprintf("=== 00_RunAll.R started: %s ===\n", Sys.time()))

# ── Core (always run) ─────────────────────────────────────────────────────────
run_module("01b_DataLoad.R")
run_module("01c_DataPrep.R")

# ── Descriptive figures ───────────────────────────────────────────────────────
run_module("01e_InstallTrends.R",        skip = SKIP_INSTALL_TRENDS)
run_module("01f_CapitalStock.R",         skip = SKIP_CAPITAL_STOCK)
run_module("01g_PreTrends.R",            skip = SKIP_PRE_TRENDS)
run_module("01h_SampleRestrictionFunnel.R", skip = SKIP_FUNNEL)
run_module("01i_CompositionBalance.R",   skip = SKIP_COMPOSITION)
run_module("01j_AgeHeterogeneity.R",     skip = SKIP_AGE_HTE)
run_module("01k_InstitutionalContext.R", skip = SKIP_INSTITUTIONAL)
run_module("01l_DataQuality.R",          skip = SKIP_DATA_QUALITY)

# ── Make-model export (MUST run; 02_DiD depends on this) ─────────────────────
run_module("01m_MakeModelSample.R")

# ── CV validation (optional, slow) ───────────────────────────────────────────
run_module("01n_CVValidation.R",         skip = SKIP_CV_VALIDATION)

# ── Summary ───────────────────────────────────────────────────────────────────
total_elapsed <- proc.time()["elapsed"] - wall_start
cat(sprintf("\n%s\n", strrep("=",60)))
cat(sprintf("=== 00_RunAll.R COMPLETE: %s ===\n", Sys.time()))
cat(sprintf("    Total wall time: %.1f minutes\n", total_elapsed / 60))
cat("\nOutputs ready:\n")
cat("  Figures  → Output/Figures/\n")
cat("  Tables   → Output/Tables/\n")
cat("  Analysis → Data/Analysis/   (read by 02_DiD_Main_MakeModel.R)\n")
cat("\nNext step: source(here::here('Code', '02_DiD_Main_MakeModel.R'))\n")
