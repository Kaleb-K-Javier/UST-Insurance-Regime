################################################################################
# Code/Analysis/T007_Phase0_Regime_Diagnostic.R
# TICKET 007 Phase 0 — Pre-fix churn-filtered within-TX regime test
# Quantifies how much of the 4+ tank partial-shrinkage regime effect
# (TX FF pre-1999 vs TX RB 2006+) survives once churn installs are filtered.
################################################################################

#### LOGGING ####
.log_path <- here::here("logs", paste0(
  "T007_Phase0_Regime_Diagnostic_",
  format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: T007_Phase0_Regime_Diagnostic.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

#### PACKAGES ####
suppressPackageStartupMessages({
  library(data.table)
  library(here)
})
setDTthreads(0L)

source(here::here("Code", "Helpers", "data_paths.R"))

ANALYSIS_DIR <- here::here("Data",    "Analysis")
AUDIT_DIR    <- here::here("Reports", "Audits")
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)

fmt_n <- function(x) format(as.integer(x), big.mark = ",")

cat("=== SECTION 1: LOAD FACILITY PANEL ===\n")

fp_path <- data_in("Data", "Analysis", "facility_panel.csv")
cat(sprintf("  Reading: %s\n", fp_path))

KEEP_FP <- c("panel_id", "panel_year", "texas_treated",
             "n_tanks_active", "n_closures", "n_installs",
             "n_tanks_eoy", "any_closure", "facility_complete_closure")

fp <- fread(fp_path, select = KEEP_FP, na.strings = c("", "NA", "N/A"))
fp[, panel_year := as.integer(panel_year)]

cat(sprintf("  facility_panel: %s rows | %s facilities\n",
    fmt_n(nrow(fp)), fmt_n(uniqueN(fp$panel_id))))

cat("=== SECTION 2: FILTER TX AND ASSIGN BINS ===\n")

# TX-only, two regime windows
fp_tx <- fp[texas_treated == 1L & (panel_year < 1999L | panel_year >= 2006L)]
fp_tx[, regime := fifelse(panel_year < 1999L, "TX_FF_pre1999", "TX_RB_2006plus")]

# 4-bin size classification (per Q1 answer: 1, 2, 3, 4+)
fp_tx[, size_bin := fcase(
  n_tanks_active == 1L, "1",
  n_tanks_active == 2L, "2",
  n_tanks_active == 3L, "3",
  n_tanks_active >= 4L, "4+",
  default = NA_character_)]

fp_tx <- fp_tx[!is.na(size_bin)]
cat(sprintf("  TX rows (pre-1999 + 2006+): %s | facilities: %s\n",
    fmt_n(nrow(fp_tx)), fmt_n(uniqueN(fp_tx$panel_id))))
cat("  Regime × size_bin distribution:\n")
print(fp_tx[, .N, by = .(regime, size_bin)][order(regime, size_bin)])

cat("=== SECTION 3: COMPUTE ORIGINAL PARTIAL-SHRINKAGE RATE ===\n")
# T006 definition:
#   is_partial = any_closure == 1 & is.na(years_to_next_install) &
#                facility_complete_closure == 0
# years_to_next_install: strictly future (iy > y) within facility

setorder(fp_tx, panel_id, panel_year)

fp_tx[, install_year_of_orig := fifelse(n_installs > 0L, panel_year, NA_integer_)]
fp_tx[, years_to_next_install_orig := {
  iy <- install_year_of_orig[!is.na(install_year_of_orig)]
  vapply(panel_year, function(y) {
    fut <- iy[iy > y]
    if (length(fut) == 0L) NA_integer_ else as.integer(min(fut) - y)
  }, integer(1L))
}, by = panel_id]

fp_tx[, is_partial_orig := any_closure == 1L &
                            is.na(years_to_next_install_orig) &
                            facility_complete_closure == 0L]

n_orig <- fp_tx[, .(
  n_closures_total  = sum(any_closure == 1L),
  n_partial_orig    = sum(is_partial_orig == TRUE, na.rm = TRUE)
), by = .(size_bin, regime)]
n_orig[, pct_orig := round(100 * n_partial_orig / pmax(n_closures_total, 1L), 2)]
cat("  Original partial-shrinkage rates:\n")
print(n_orig[order(regime, size_bin)])

cat("=== SECTION 4: LOAD PANEL_DT AND COUNT CHURN INSTALLS ===\n")

pdt_path <- file.path(ANALYSIS_DIR, "panel_dt.csv")
stopifnot(file.exists(pdt_path))
cat(sprintf("  Reading: %s\n", pdt_path))

panel_dt <- fread(pdt_path,
                  select = c("tank_panel_id", "panel_id", "panel_year",
                             "tank_installed_date", "tank_closed_date"),
                  na.strings = c("", "NA", "N/A"))
panel_dt[, panel_year          := as.integer(panel_year)]
panel_dt[, tank_installed_date := as.IDate(tank_installed_date)]
panel_dt[, tank_closed_date    := as.IDate(tank_closed_date)]

# Churn: install AND close in the same calendar year (same definition as 02b S4)
panel_dt[, first_year_churn := as.integer(
  !is.na(tank_installed_date) & !is.na(tank_closed_date) &
  year(tank_installed_date) == year(tank_closed_date)
)]

n_churn_total <- panel_dt[first_year_churn == 1L, .N]
n_total       <- nrow(panel_dt)
cat(sprintf("  panel_dt: %s rows | first_year_churn==1: %s (%.2f%%)\n",
    fmt_n(n_total), fmt_n(n_churn_total), 100 * n_churn_total / n_total))

# Count churn installs per (panel_id, panel_year) — each churn row is one install
churn_installs <- panel_dt[first_year_churn == 1L,
                            .(n_churn_installs = .N),
                            by = .(panel_id, panel_year)]
cat(sprintf("  Facility-years with ≥1 churn install: %s\n", fmt_n(nrow(churn_installs))))

cat("=== SECTION 5: COMPUTE CLEAN PARTIAL-SHRINKAGE RATE ===\n")

fp_tx <- merge(fp_tx, churn_installs, by = c("panel_id", "panel_year"), all.x = TRUE)
fp_tx[is.na(n_churn_installs), n_churn_installs := 0L]

# Remove churn installs from n_installs
fp_tx[, n_installs_clean := pmax(n_installs - n_churn_installs, 0L)]

n_affected <- fp_tx[n_churn_installs > 0L, .N]
cat(sprintf("  TX facility-years where n_installs changes: %s (%.1f%% of %s)\n",
    fmt_n(n_affected), 100 * n_affected / nrow(fp_tx), fmt_n(nrow(fp_tx))))

# Recompute years_to_next_install using cleaned n_installs
fp_tx[, install_year_of_clean := fifelse(n_installs_clean > 0L, panel_year, NA_integer_)]
fp_tx[, years_to_next_install_clean := {
  iy <- install_year_of_clean[!is.na(install_year_of_clean)]
  vapply(panel_year, function(y) {
    fut <- iy[iy > y]
    if (length(fut) == 0L) NA_integer_ else as.integer(min(fut) - y)
  }, integer(1L))
}, by = panel_id]

fp_tx[, is_partial_clean := any_closure == 1L &
                             is.na(years_to_next_install_clean) &
                             facility_complete_closure == 0L]

n_clean <- fp_tx[, .(
  n_partial_clean = sum(is_partial_clean == TRUE, na.rm = TRUE)
), by = .(size_bin, regime)]

cat("=== SECTION 6: ASSEMBLE OUTPUT AND WRITE CSV ===\n")

out <- merge(n_orig, n_clean, by = c("size_bin", "regime"))
out[, pct_partial_clean := round(100 * n_partial_clean / pmax(n_closures_total, 1L), 2)]
out[, delta := round(pct_partial_clean - pct_orig, 2)]
setnames(out, c("n_closures_total", "n_partial_orig", "pct_orig"),
              c("n_closure_events", "n_partial_shrinkage", "pct_partial_original"))

out_final <- out[, .(size_bin, regime, n_closure_events, n_partial_shrinkage,
                     pct_partial_original, pct_partial_clean, delta)]
setorder(out_final, regime, size_bin)

cat("  Final output table:\n")
print(out_final)

out_path <- file.path(AUDIT_DIR, "Phase0_Churn_Filtered_Regime_Test.csv")
fwrite(out_final, out_path)
cat(sprintf("  Saved: %s\n", out_path))

cat("=== SECTION 7: VERDICT ===\n")

ff_4plus <- out_final[size_bin == "4+" & regime == "TX_FF_pre1999"]
rb_4plus <- out_final[size_bin == "4+" & regime == "TX_RB_2006plus"]

if (nrow(ff_4plus) == 0L || nrow(rb_4plus) == 0L) {
  cat("  WARNING: 4+ size_bin not found for one or both regimes. Cannot compute regime effect.\n")
} else {
  effect_orig  <- rb_4plus$pct_partial_original - ff_4plus$pct_partial_original
  effect_clean <- rb_4plus$pct_partial_clean    - ff_4plus$pct_partial_clean

  verdict <- if (abs(effect_clean) >= 5) {
    "SURVIVES — regime effect >= 5pp after churn filter; real policy response margin"
  } else if (abs(effect_clean) < 2) {
    "COLLAPSES — regime effect < 2pp; was mostly churn artifact"
  } else {
    "INTERMEDIATE — regime effect 2-5pp; partial signal"
  }

  cat(sprintf("  4+ tank regime effect (RB − FF):\n"))
  cat(sprintf("    Original:  %.1fpp (FF=%.1f%%, RB=%.1f%%)\n",
      effect_orig,  ff_4plus$pct_partial_original, rb_4plus$pct_partial_original))
  cat(sprintf("    Clean:     %.1fpp (FF=%.1f%%, RB=%.1f%%)\n",
      effect_clean, ff_4plus$pct_partial_clean,    rb_4plus$pct_partial_clean))
  cat(sprintf("\n  VERDICT: %s\n", verdict))
}

cat("\n=== Phase 0 COMPLETE ===\n")
cat(sprintf("  Output: %s\n", out_path))
