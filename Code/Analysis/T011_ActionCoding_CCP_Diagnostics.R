# ==============================================================================
# T011_ActionCoding_CCP_Diagnostics.R
# Action-coding audit + empirical CCP diagnostics (READ-ONLY)
# Spec: .claude/TICKETS/011_spec.md  (revised 2026-06-01, attempt 2)
# ==============================================================================
# No estimation, no model changes. Produces 10 tables + 8 figures.
# Size measure = beginning-of-year (BOY) stock per Section 1.5 (corrections #9-#11).
# The qmd append (deliverable 20) is handled in a separate step after this runs.
# ==============================================================================

SCRIPT_NAME <- "T011_ActionCoding_CCP_Diagnostics"

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(here)
  library(scales)
  library(patchwork)
})

# ---- Logging block (Rscript: no rstudioapi; SCRIPT_NAME hardcoded per spec #8)
.log_path <- here::here("logs", paste0(
  SCRIPT_NAME, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output", split = TRUE)   # split => console AND log
sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
            .log_path, SCRIPT_NAME, R.version.string, getwd()))

dir.create(here("Output", "Tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(here("Output", "Figures"), recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
cat("=== SECTION 1 — LOAD DATA ===\n")
# ==============================================================================

# 1.1 path helpers
source(here("Code", "Helpers", "data_paths.R"))

# 1.2 DCM observation panel (estimation sample)
obs_path <- here("Data", "Analysis", "dcm_obs_panel_observed.csv")
if (!file.exists(obs_path)) stop("Run 04b first")
obs <- fread(obs_path)
cat(sprintf("obs: %s  (%d rows, %d cols)\n", obs_path, nrow(obs), ncol(obs)))

# 1.3 primitives -> state_lut (32 rows)
prim <- readRDS(here("Output", "Estimation_Results",
                     "DCM_Primitives_Replacement_observed.rds"))
state_lut <- as.data.table(prim$state_lut)
cat(sprintf("state_lut: %d rows, cols = %s\n",
            nrow(state_lut), paste(names(state_lut), collapse = ",")))

# 1.4 facility_panel (column subset only; ~3.2 GB file)
KEEP_FP <- c("panel_id", "panel_year", "texas_treated", "state",
             "active_tanks", "n_installs", "n_closures", "n_sw_closures",
             "any_closure", "facility_complete_closure",
             "facility_exit", "replacement_closure_year",
             "permanent_closure_year", "single_to_double_year",
             "n_tanks_eoy", "n_tanks_active", "total_capacity", "total_capacity_dec",
             "has_single_walled_dec", "has_double_walled_dec",
             "all_sw", "all_dw", "mixed_wall", "wall_type")
fp_path <- data_in("Data", "Analysis", "facility_panel.csv")
fp_is_local <- startsWith(normalizePath(fp_path, mustWork = FALSE),
                          normalizePath(here(), mustWork = FALSE))
fp <- fread(fp_path, select = KEEP_FP)
cat(sprintf("fp: %s  (source = %s, %d rows, %d cols)\n",
            fp_path, if (fp_is_local) "LOCAL" else "Z", nrow(fp), ncol(fp)))

# EOY capacity column presence (spec #4): use total_capacity_dec; fall back loud
HAS_CAP_DEC <- "total_capacity_dec" %in% names(fp)
if (HAS_CAP_DEC) {
  cap_col <- "total_capacity_dec"
  cat("Capacity column: total_capacity_dec (EOY stock) — OK\n")
} else {
  cap_col <- "total_capacity"
  cat("CAVEAT: total_capacity_dec absent; falling back to total_capacity ",
      "(02b line 915 counts tanks present at ANY point in year => same-year ",
      "close+install double-counts). Capacity deltas may be biased.\n", sep = "")
}

# 1.5 CANONICAL FACILITY SIZE = beginning-of-year (BOY) stock = prior-year EOY
#     (corrections #9): EOY stock is 0 at full closures (drops 174k events);
#     n_tanks_active over-sizes same-year replacements. BOY avoids both.
stopifnot(!anyDuplicated(fp[, .(panel_id, panel_year)]))  # 1:1 forward-key join
setorder(fp, panel_id, panel_year)
prior <- fp[, .(panel_id, panel_year = panel_year + 1L, boy_stock = n_tanks_eoy)]
fp <- merge(fp, prior, by = c("panel_id", "panel_year"), all.x = TRUE)
n_boy_fallback <- fp[is.na(boy_stock) & n_tanks_active > 0L, .N]
fp[is.na(boy_stock), boy_stock := n_tanks_active]   # fallback: left-censored 1st yr
fp[, size_bin := fcase(
  boy_stock == 1L, "1", boy_stock == 2L, "2", boy_stock == 3L, "3",
  boy_stock >= 4L, "4+", default = "unknown")]
cat(sprintf("BOY size: n_tanks_active fallback used on %d rows (%.3f%% of fp); size_bin unknown on fp: %.3f%%\n",
            n_boy_fallback, 100 * n_boy_fallback / nrow(fp),
            100 * fp[size_bin == "unknown", .N] / nrow(fp)))

# ==============================================================================
cat("\n=== SECTION 2 — ACTION CODING AUDIT (Task A) ===\n")
# ==============================================================================

# 2.1 merge obs (estimation sample) with 02b flags + BOY size_bin from fp.
#     single_to_double_year carried for A4 (step 2.5).
merged <- merge(
  obs[, .(panel_id, panel_year, s_idx, A_bin, w_state, rho_state, y_it, I_replace)],
  fp[, .(panel_id, panel_year, active_tanks, any_closure,
         facility_complete_closure, replacement_closure_year,
         permanent_closure_year, n_installs, n_tanks_eoy, size_bin,
         single_to_double_year)],
  by = c("panel_id", "panel_year"), all.x = FALSE)
cat(sprintf("merged (inner join, estimation sample): %d rows\n", nrow(merged)))

n_merged   <- nrow(merged)
n_closures <- merged[y_it == 1L, .N]

# ---- 2.2 TABLE A1 — action distribution ----
n_maintain      <- merged[y_it == 0, .N]
n_exit          <- merged[y_it == 1 & I_replace == 0, .N]
n_replace       <- merged[y_it == 1 & I_replace == 1, .N]
n_partial       <- merged[any_closure == 1 & facility_complete_closure == 0, .N]
n_partial_maint <- merged[any_closure == 1 & facility_complete_closure == 0 &
                            y_it == 0, .N]
n_cexit_nofut   <- merged[facility_complete_closure == 1 &
                            replacement_closure_year == 0, .N]
n_cexit_withfut <- merged[facility_complete_closure == 1 &
                            replacement_closure_year == 1, .N]

cat(sprintf("Partial closures in the estimation sample: %d rows, of which %d (%.1f%%) are correctly Maintain (y_it=0)\n",
            n_partial, n_partial_maint, 100 * n_partial_maint / max(n_partial, 1L)))

a1 <- data.table(
  category = c("Maintain", "Exit", "Replace", "PartialClosure_inMaintain",
               "CompleteExit_noFutureInstall", "CompleteExit_withFutureInstall"),
  N = c(n_maintain, n_exit, n_replace, n_partial_maint,
        n_cexit_nofut, n_cexit_withfut))
a1[, pct := 100 * N / n_merged]   # common denominator = nrow(merged) (spec #6)
fwrite(a1, here("Output", "Tables", "T011_A1_ActionDistribution.csv"))
cat("A1 saved.\n"); print(a1)

# ---- 2.3 TABLE A2 — closure-type overlap ----
a2 <- merged[y_it == 1, .N, by = .(facility_complete_closure, replacement_closure_year)]
a2[, pct_of_closures := 100 * N / n_closures]
setorder(a2, -facility_complete_closure, -replacement_closure_year)
fwrite(a2, here("Output", "Tables", "T011_A2_ClosureTypeOverlap.csv"))
n_double_trig <- merged[y_it == 1 & facility_complete_closure == 1 &
                          replacement_closure_year == 1, .N]
cat(sprintf("Double-triggered (full closure + future install): %d (%.1f%% of %d closures)\n",
            n_double_trig, 100 * n_double_trig / max(n_closures, 1L), n_closures))
cat("A2 saved.\n"); print(a2)

# ---- 2.4 TABLE A3 — partial closure by facility size (BOY size_bin) ----
n_a3_unknown <- merged[any_closure == 1 & facility_complete_closure == 0 &
                         size_bin == "unknown", .N]
a3 <- merged[size_bin != "unknown", .(
  n_obs_in_size      = .N,
  n_closures_in_size = sum(y_it == 1L),
  n_partial          = sum(any_closure == 1 & facility_complete_closure == 0)
), by = size_bin]
a3[, `:=`(pct_of_obs_that_are_partial      = 100 * n_partial / pmax(n_obs_in_size, 1L),
          pct_of_closures_that_are_partial = 100 * n_partial / pmax(n_closures_in_size, 1L))]
setcolorder(a3, c("size_bin", "n_partial", "n_obs_in_size", "n_closures_in_size",
                  "pct_of_obs_that_are_partial", "pct_of_closures_that_are_partial"))
setorder(a3, size_bin)
fwrite(a3, here("Output", "Tables", "T011_A3_PartialClosure_by_Size.csv"))
cat(sprintf("A3 saved (%d partial-closure rows had size_bin==unknown, excluded).\n",
            n_a3_unknown))
print(a3)

# ---- 2.5 TABLE A4 — replace-definition concordance ----
n_dcm_r            <- merged[I_replace == 1L, .N]
n_is_sw2dw         <- merged[I_replace == 1L & single_to_double_year == 1L, .N]
n_broad_not_sw2dw  <- merged[I_replace == 1L & replacement_closure_year == 1L &
                               single_to_double_year == 0L, .N]
n_broad_not_dcm    <- merged[replacement_closure_year == 1L & I_replace != 1L, .N]
n_sw2dw_not_dcm    <- merged[single_to_double_year == 1L & I_replace != 1L, .N]
disc_mask          <- merged$y_it == 1L &
  (merged$I_replace != merged$replacement_closure_year)
disc_mask[is.na(disc_mask)] <- FALSE
n_identity_disc    <- sum(disc_mask)

a4 <- data.table(
  category = c("DCM_Replace_total", "DCM_Replace_is_SW2DW",
               "DCM_Replace_broad_not_SW2DW", "broad_replace_NOT_in_DCM_replace",
               "SW2DW_NOT_in_DCM_replace", "Identity_check_discrepancies"),
  N = c(n_dcm_r, n_is_sw2dw, n_broad_not_sw2dw, n_broad_not_dcm,
        n_sw2dw_not_dcm, n_identity_disc),
  note = c("all DCM Replace events (I_replace==1)",
           "DCM Replace that are same-year SW->DW (single_to_double_year==1)",
           "DCM Replace that are broad future-install but NOT same-year SW->DW",
           "broad replacement flag but NOT coded DCM Replace (should be 0)",
           "SW->DW flag but NOT coded DCM Replace (should be 0; sw2dw subset of broad)",
           "rows where I_replace != replacement_closure_year among closures (should be 0)"))
a4[, `:=`(pct_of_dcm_replacements = 100 * N / max(n_dcm_r, 1L),
          pct_of_all_closures     = 100 * N / max(n_closures, 1L))]
setcolorder(a4, c("category", "N", "pct_of_dcm_replacements",
                  "pct_of_all_closures", "note"))
fwrite(a4, here("Output", "Tables", "T011_A4_ReplaceDefinition_Concordance.csv"))
cat("A4 saved.\n"); print(a4)

pct_sw2dw <- 100 * n_is_sw2dw / max(n_dcm_r, 1L)
pct_broad <- 100 * n_broad_not_sw2dw / max(n_dcm_r, 1L)
cat(sprintf(paste0(
  "Replace in DCM = broad definition (any future install after closure).\n",
  "  %.1f%% of DCM Replace events are specifically SW->DW upgrades.\n",
  "  %.1f%% are broad-only (future install, but NOT same-year SW->DW).\n",
  "  Identity check: 04b is_retrofit == 02b replacement_closure_year for all ",
  "%d closure rows — [%s].\n"),
  pct_sw2dw, pct_broad, n_closures,
  if (n_identity_disc == 0) "PASS" else sprintf("FAIL: %d discrepancies", n_identity_disc)))

if (n_identity_disc > 0L) {
  warning(sprintf("A4 identity check FAILED: %d rows where I_replace != replacement_closure_year",
                  n_identity_disc))
  cat("Disagreeing rows (first 20):\n")
  print(merged[disc_mask, .(panel_id, panel_year, y_it, I_replace,
                            replacement_closure_year)][1:min(20, n_identity_disc)])
}
stopifnot(n_identity_disc == 0L)
stopifnot(n_sw2dw_not_dcm == 0L)

# ---- 2.6 TABLE A5 — replacement margin validity ----
# Step A: years-to-next-install for each replace event (FULL facility_panel)
fp_inst <- fp[n_installs > 0L, .(panel_id, install_yr = panel_year)]
replace_events <- fp[replacement_closure_year == 1L, .(panel_id, panel_year)]
n_re <- nrow(replace_events)
gaps <- merge(replace_events, fp_inst, by = "panel_id", allow.cartesian = TRUE)
gaps <- gaps[install_yr >= panel_year]
gaps[, gap := install_yr - panel_year]
timing <- gaps[, .(years_to_next = min(gap)), by = .(panel_id, panel_year)]

re <- merge(replace_events, timing, by = c("panel_id", "panel_year"), all.x = TRUE)
n_timing_na <- re[is.na(years_to_next), .N]
cat(sprintf("Replace events: %d; years_to_next NA (no future install in panel): %d\n",
            n_re, n_timing_na))

# Step B/C: calendar-year-keyed neighbor lookups (NO row-offset shift; spec #3)
fp_lk <- fp[, .(panel_id, panel_year, n_tanks_eoy, cap = get(cap_col))]
re[, t_before := panel_year - 1L]
re[, t_after  := panel_year + years_to_next]   # NA if years_to_next NA
re <- merge(re,
            fp_lk[, .(panel_id, t_before = panel_year,
                      n_before = n_tanks_eoy, cap_before = cap)],
            by = c("panel_id", "t_before"), all.x = TRUE)
re <- merge(re,
            fp_lk[, .(panel_id, t_after = panel_year,
                      n_after = n_tanks_eoy, cap_after = cap)],
            by = c("panel_id", "t_after"), all.x = TRUE)
re[, delta_tanks   := n_after - n_before]
re[, delta_cap     := cap_after - cap_before]
re[, delta_cap_pct := 100 * delta_cap / pmax(cap_before, 1)]

n_tank_missing <- re[is.na(delta_tanks), .N]
n_cap_missing  <- re[is.na(delta_cap_pct), .N]
cat(sprintf("Tank-count delta missing-neighbor events: %d\n", n_tank_missing))
cat(sprintf("Capacity delta missing-neighbor events: %d\n", n_cap_missing))

# Bucketers
bucket_timing <- function(x) fcase(
  is.na(x), "NA", x == 0, "0", x == 1, "1",
  x >= 2 & x <= 3, "2-3", x >= 4 & x <= 5, "4-5",
  x >= 6 & x <= 10, "6-10", x >= 11, "11+")
bucket_dtanks <- function(d) fcase(
  is.na(d), "missing", d >= 2, "+2 or more", d == 1, "+1",
  d == 0, "0", d == -1, "-1", d <= -2, "-2 or more")
bucket_cap <- function(p) fcase(
  is.na(p), "missing capacity data", p > 10, "expanded >10pct",
  p >= -10 & p <= 10, "flat -10 to +10pct",
  p >= -25 & p < -10, "modest shrink -25 to -10pct",
  p < -25, "substantial shrink below -25pct")

re[, tim_bucket  := bucket_timing(years_to_next)]
re[, tank_bucket := bucket_dtanks(delta_tanks)]
re[, cap_bucket  := bucket_cap(delta_cap_pct)]

mk_block <- function(dt, col, levels, section, base_n, note) {
  tab <- dt[, .(N = .N), by = c(col)]
  setnames(tab, col, "category")
  tab <- merge(data.table(category = levels), tab, by = "category", all.x = TRUE)
  tab[is.na(N), N := 0L]
  tab[, category := factor(category, levels = levels)]
  setorder(tab, category)
  tab[, `:=`(section = section,
             pct_of_replace_events = 100 * N / base_n,
             note = note,
             category = as.character(category))]
  tab[, .(section, category, N, pct_of_replace_events, note)]
}

tim_levels  <- c("0", "1", "2-3", "4-5", "6-10", "11+", "NA")
tank_levels <- c("+2 or more", "+1", "0", "-1", "-2 or more")   # missing excluded
cap_levels  <- c("expanded >10pct", "flat -10 to +10pct",
                 "modest shrink -25 to -10pct",
                 "substantial shrink below -25pct", "missing capacity data")

blk_tim  <- mk_block(re, "tim_bucket", tim_levels,
                     "Timing (years to next install)", n_re,
                     "base = all replace events")
blk_tank <- mk_block(re[tank_bucket != "missing"], "tank_bucket", tank_levels,
                     "Net tank count change (t-1 to t+1)", n_re,
                     sprintf("base = all replace events; %d missing-neighbor excluded",
                             n_tank_missing))
blk_cap  <- mk_block(re, "cap_bucket", cap_levels,
                     "Net capacity change pct (t-1 to t+1)", n_re,
                     "base = all replace events")

# Step D: SW-replacement install timing (P_SWR; spec #5)
swr <- fp[n_sw_closures > 0L & replacement_closure_year == 1L, .(panel_id, panel_year)]
swr <- merge(swr, timing, by = c("panel_id", "panel_year"), all.x = TRUE)
n_swr  <- nrow(swr)
n_s2d  <- fp[single_to_double_year == 1L, .N]
n_swr_na <- swr[is.na(years_to_next), .N]
swr[, swr_bucket := fcase(
  years_to_next == 0, "same year",
  years_to_next == 1, "next year",
  years_to_next >= 2, "2 or more years later",
  default = NA_character_)]
swr_levels <- c("same year", "next year", "2 or more years later")
blk_swr <- mk_block(swr[!is.na(swr_bucket)], "swr_bucket", swr_levels,
                    "SW-replacement install timing (SW closure + future install)",
                    n_swr,
                    sprintf("base = P_SWR (%d); single_to_double_year==1 count = %d",
                            n_swr, n_s2d))
cat(sprintf("P_SWR events: %d (single_to_double_year==1 cross-check: %d; years_to_next NA: %d)\n",
            n_swr, n_s2d, n_swr_na))

a5 <- rbindlist(list(blk_tim, blk_tank, blk_cap, blk_swr))
fwrite(a5, here("Output", "Tables", "T011_A5_ReplacementMarginValidity.csv"))
cat("A5 saved.\n"); print(a5)

# A5 plain-English validity summary (7 lines)
med_yrs   <- median(re$years_to_next, na.rm = TRUE)
sh_same   <- 100 * re[years_to_next == 0, .N] / n_re
sh_6plus  <- 100 * re[!is.na(years_to_next) & years_to_next >= 6, .N] / n_re
tank_nm   <- re[!is.na(delta_tanks)]
n_tank_nm <- nrow(tank_nm)
sh_tank_flatexp <- 100 * tank_nm[delta_tanks >= 0, .N] / max(n_tank_nm, 1L)
sh_tank_shrink  <- 100 * tank_nm[delta_tanks <= -1, .N] / max(n_tank_nm, 1L)
cap_nm    <- re[!is.na(delta_cap_pct)]
n_cap_nm  <- nrow(cap_nm)
sh_cap_flatexp <- 100 * cap_nm[delta_cap_pct >= -10, .N] / max(n_cap_nm, 1L)
sh_cap_shrink  <- 100 * cap_nm[delta_cap_pct < -10, .N] / max(n_cap_nm, 1L)
both_nm   <- re[!is.na(delta_tanks) & !is.na(delta_cap_pct)]
n_both_nm <- nrow(both_nm)
sh_capneutral <- 100 * both_nm[delta_tanks < 0 & abs(delta_cap_pct) <= 10, .N] /
  max(n_both_nm, 1L)
sh_trueshrink <- 100 * both_nm[delta_tanks < 0 & delta_cap_pct < -10, .N] /
  max(n_both_nm, 1L)
cat("Replacement margin validity:\n")
cat(sprintf("  - Median years to next install after Replace event: %g\n", med_yrs))
cat(sprintf("  - Share with same-year install (most credible replacements): %.1f%%\n", sh_same))
cat(sprintf("  - Share with 6+ year gap (likely unrelated to closure): %.1f%%\n", sh_6plus))
cat(sprintf("  - Net tank count: %.1f%% flat/expanded, %.1f%% shrunk by 1+\n",
            sh_tank_flatexp, sh_tank_shrink))
cat(sprintf("  - Net capacity: %.1f%% flat/expanded, %.1f%% shrunk >10%%\n",
            sh_cap_flatexp, sh_cap_shrink))
cat(sprintf("  - Capacity-neutral but tank-count-negative (2 SW -> 1 DW type): %.1f%%\n",
            sh_capneutral))
cat(sprintf("  - True facility shrinkage (both count AND capacity down): %.1f%%\n",
            sh_trueshrink))

# ---- 2.6b TABLE A6 + FIGURE A6 — distribution of size change at Replace events ----
# per-event set: non-missing before AND after stock (same rows feeding A5 B/C blocks)
re_both <- re[!is.na(delta_tanks) & !is.na(delta_cap_pct)]
n_a6 <- nrow(re_both)
cat(sprintf("A6/A7 per-event set (non-missing before AND after): %d events\n", n_a6))

# delta_tanks integer bin labels clipped [-5,+5]
dt_lvls <- c("<=-5", "-4", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4", ">=+5")
dt_lbl <- function(d) fcase(
  d <= -5L, "<=-5", d >= 5L, ">=+5", d == 0L, "0",
  d > 0L, sprintf("+%d", d), d < 0L, sprintf("%d", d))
# delta_cap in GALLONS — wide bins, explicit no-change, out to +/-10,000 (~p10-p90)
gal_lvls <- c("<=-10000", "-10000 to -5000", "-5000 to -2000", "-2000 to -500",
              "-500 to <0", "0 (no change)", ">0 to 500", "500 to 2000",
              "2000 to 5000", "5000 to 10000", ">10000")
gal_lbl <- function(d) fcase(
  d <= -10000, "<=-10000",
  d <= -5000,  "-10000 to -5000",
  d <= -2000,  "-5000 to -2000",
  d <= -500,   "-2000 to -500",
  d < 0,       "-500 to <0",
  d == 0,      "0 (no change)",
  d <= 500,    ">0 to 500",
  d <= 2000,   "500 to 2000",
  d <= 5000,   "2000 to 5000",
  d <= 10000,  "5000 to 10000",
  d > 10000,   ">10000")

a6_tanks <- re_both[, .(n_events = .N), by = .(bin = dt_lbl(delta_tanks))]
a6_tanks <- merge(data.table(bin = dt_lvls), a6_tanks, by = "bin", all.x = TRUE)
a6_tanks[is.na(n_events), n_events := 0L]
a6_tanks[, bin := factor(bin, levels = dt_lvls)]; setorder(a6_tanks, bin)
a6_tanks[, `:=`(metric = "delta_tanks", pct_of_events = 100 * n_events / n_a6,
                bin = as.character(bin))]

a6_cap <- re_both[, .(n_events = .N), by = .(bin = gal_lbl(delta_cap))]
a6_cap <- merge(data.table(bin = gal_lvls), a6_cap, by = "bin", all.x = TRUE)
a6_cap[is.na(n_events), n_events := 0L]
a6_cap[, bin := factor(bin, levels = gal_lvls)]; setorder(a6_cap, bin)
a6_cap[, `:=`(metric = "delta_cap_gal", pct_of_events = 100 * n_events / n_a6,
              bin = as.character(bin))]

a6 <- rbindlist(list(a6_tanks[, .(metric, bin, n_events, pct_of_events)],
                     a6_cap[,   .(metric, bin, n_events, pct_of_events)]))
fwrite(a6, here("Output", "Tables", "T011_A6_CapacityChange_Distribution.csv"))
cat(sprintf("A6 saved (delta_tanks block sum=%.2f%%, delta_cap_gal block sum=%.2f%%).\n",
            a6_tanks[, sum(pct_of_events)], a6_cap[, sum(pct_of_events)]))

# A6 figure: 2 stacked histogram panels via patchwork (capacity in GALLONS)
re_both[, dtanks_clip   := pmax(pmin(delta_tanks, 5L), -5L)]
re_both[, dcap_gal_clip := pmax(pmin(delta_cap, 20000), -20000)]
n_cap_clipped <- re_both[abs(delta_cap) > 20000, .N]
p_top <- ggplot(re_both, aes(x = dtanks_clip)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey30") +
  scale_x_continuous(breaks = -5:5) +
  labs(subtitle = "Tank count change (clipped to [-5,+5])",
       x = "delta tanks", y = "Replace events") +
  theme_bw()
p_bot <- ggplot(re_both, aes(x = dcap_gal_clip)) +
  geom_histogram(binwidth = 2000, fill = "darkorange", color = "white") +
  geom_vline(xintercept = 0, color = "grey30") +
  scale_x_continuous(breaks = seq(-20000, 20000, 5000), labels = scales::comma) +
  labs(subtitle = sprintf("Total capacity change in gallons (clipped to +/-20,000; %d events beyond clip)",
                          n_cap_clipped),
       x = "delta capacity (gallons)", y = "Replace events") +
  theme_bw()
g_a6 <- (p_top / p_bot) +
  plot_annotation(
    title = "Distribution of facility size change at Replace events",
    subtitle = sprintf(paste0("Tank count (top) and total capacity in gallons (bottom); n = %d ",
                              "replace events with non-missing before/after stock. ",
                              "Mass at/above 0 on capacity = consolidation/upgrade, not shrinkage."),
                       n_a6))
ggsave(here("Output", "Figures", "T011_A6_CapacityChange_Distribution.png"),
       g_a6, width = 10, height = 6, dpi = 150)
cat("Figure A6 saved.\n")

# ---- 2.6c TABLE A7 + FIGURE A7 — JOINT tank-count vs capacity change ----
re_both[, dtanks_bin := factor(dt_lbl(delta_tanks), levels = dt_lvls)]
a7 <- re_both[, .(
  n_events             = .N,
  mean_cap_pct         = mean(delta_cap_pct),
  median_cap_pct       = median(delta_cap_pct),
  p25_cap_pct          = quantile(delta_cap_pct, 0.25),
  p75_cap_pct          = quantile(delta_cap_pct, 0.75),
  share_cap_preserving = mean(delta_cap_pct >= -10) * 100
), by = dtanks_bin]
a7 <- merge(data.table(dtanks_bin = factor(dt_lvls, levels = dt_lvls)),
            a7, by = "dtanks_bin", all.x = TRUE)
a7[is.na(n_events), n_events := 0L]
setorder(a7, dtanks_bin)
a7[, pct_of_events := 100 * n_events / n_a6]
setnames(a7, "dtanks_bin", "delta_tanks_bin")
setcolorder(a7, c("delta_tanks_bin", "n_events", "pct_of_events", "mean_cap_pct",
                  "median_cap_pct", "p25_cap_pct", "p75_cap_pct", "share_cap_preserving"))
a7[, delta_tanks_bin := as.character(delta_tanks_bin)]
fwrite(a7, here("Output", "Tables", "T011_A7_TankVsCapacity_Joint.csv"))
r_tc <- cor(re_both$delta_tanks, re_both$delta_cap_pct)
cat(sprintf("A7 saved. cor(delta_tanks, delta_cap_pct) = %.4f (n=%d)\n", r_tc, n_a6))
print(a7)

# A7 figure: bin scatter (mean point + p25-p75 band, point size = n_events)
a7_plot <- copy(a7)
a7_plot[, delta_tanks_bin := factor(delta_tanks_bin, levels = dt_lvls)]
a7_plot <- a7_plot[n_events > 0]
zero_pos <- which(dt_lvls == "0")
g_a7 <- ggplot(a7_plot, aes(x = delta_tanks_bin, y = median_cap_pct)) +
  geom_hline(yintercept = 0, color = "grey30") +
  geom_hline(yintercept = -10, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = zero_pos, linetype = "dotted", color = "grey50") +
  geom_linerange(aes(ymin = p25_cap_pct, ymax = p75_cap_pct), color = "grey60") +
  geom_point(aes(size = n_events), color = "darkred") +
  scale_size_continuous(name = "Events per bin") +
  scale_x_discrete(limits = dt_lvls) +
  coord_cartesian(ylim = c(-100, 200)) +
  labs(title = "Tank-count change vs. capacity change at Replace events",
       subtitle = sprintf(paste0("Bin scatter: median (point) and p25-p75 (band) capacity %% ",
                                 "within each tank-count-change bin; point size = events. ",
                                 "y clipped to [-100,200]. cor(dtanks, dcap%%) = %.3f."), r_tc),
       x = "delta tanks (bin)", y = "capacity % change (median, p25-p75)") +
  theme_bw()
ggsave(here("Output", "Figures", "T011_A7_TankVsCapacity_BinScatter.png"),
       g_a7, width = 9, height = 6, dpi = 150)
cat("Figure A7 saved.\n")

# ==============================================================================
cat("\n=== SECTION 3 — TRUE DATA CCPs BY STATE CELL (Task B) ===\n")
# ==============================================================================

# nominal-label maps (used in CCP tables + figures below)
age_labels <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35+")
wall_lab   <- c("1" = "SW", "2" = "DW")
regime_lab <- c("1" = "FF", "2" = "RB")

# 3.1 cell-level counts (drop NA premium)
cell_dt <- obs[!is.na(premium), .(
  n_obs     = .N,
  n_maint   = sum(y_it == 0L),
  n_close   = sum(y_it == 1L),
  n_exit    = sum(y_it == 1L & !is.na(I_replace) & I_replace == 0L),
  n_replace = sum(y_it == 1L & !is.na(I_replace) & I_replace == 1L)
), by = s_idx]
cell_dt <- merge(state_lut[, .(s_idx, A_bin, w_state, rho_state)],
                 cell_dt, by = "s_idx", all.x = TRUE)
cell_dt[is.na(n_obs), `:=`(n_obs = 0, n_maint = 0, n_close = 0,
                           n_exit = 0, n_replace = 0)]

# 3.2 raw CCPs
cell_dt[, `:=`(
  P_M     = n_maint   / pmax(n_obs, 1L),
  P_E     = n_exit    / pmax(n_obs, 1L),
  P_R     = n_replace / pmax(n_obs, 1L),
  P_close = n_close   / pmax(n_obs, 1L))]
stopifnot(all(abs(cell_dt[n_obs > 0, P_M + P_E + P_R] - 1) < 1e-8))

# 3.3 TABLE B1 (nominal labels + conditional CCPs added)
cell_dt[, cell_pct := 100 * n_obs / sum(n_obs)]
cell_dt[, `:=`(
  wall     = wall_lab[as.character(w_state)],
  regime   = regime_lab[as.character(rho_state)],
  age      = age_labels[A_bin],
  P_E_cond = n_exit    / pmax(n_close, 1L),
  P_R_cond = n_replace / pmax(n_close, 1L))]
setorder(cell_dt, s_idx)
b1 <- cell_dt[, .(s_idx, age, wall, regime, A_bin, w_state, rho_state,
                  n_obs, n_maint, n_exit, n_replace, n_close,
                  P_M, P_E, P_R, P_close, P_E_cond, P_R_cond, cell_pct)]
fwrite(b1, here("Output", "Tables", "T011_B1_EmpiricalCCPs_by_Cell.csv"))
ovr_P_M <- cell_dt[, sum(n_maint)   / sum(n_obs)]
ovr_P_E <- cell_dt[, sum(n_exit)    / sum(n_obs)]
ovr_P_R <- cell_dt[, sum(n_replace) / sum(n_obs)]
cat(sprintf("B1 saved. Overall (n_obs-weighted) P_M=%.4f P_E=%.5f P_R=%.5f (sum=%.6f)\n",
            ovr_P_M, ovr_P_E, ovr_P_R, ovr_P_M + ovr_P_E + ovr_P_R))

# Figure helpers (age_labels / wall_lab / regime_lab defined at top of Section 3)
action_labs <- c(P_M = "P(Maintain)", P_E = "P(Exit)", P_R = "P(Replace)")

# 3.4 / 3.5 FIGURES B1 (SW) and B2 (DW)
ccp_long <- melt(cell_dt, id.vars = c("s_idx", "A_bin", "w_state", "rho_state",
                                      "cell_pct"),
                 measure.vars = c("P_M", "P_E", "P_R"),
                 variable.name = "action", value.name = "P")
ccp_long[, action := factor(action, levels = c("P_M", "P_E", "P_R"),
                            labels = action_labs)]

make_ccp_fig <- function(wall_val, wall_lbl, out_png) {
  d <- ccp_long[w_state == wall_val & cell_pct > 0]
  g <- ggplot(d, aes(x = A_bin, y = P, color = factor(rho_state),
                     group = factor(rho_state))) +
    geom_line() +
    geom_point(aes(size = cell_pct)) +
    facet_wrap(~action, scales = "free_y") +
    scale_x_continuous(breaks = 1:8, labels = age_labels) +
    scale_size_continuous(name = "Cell share (% of est. obs)",
                          breaks = c(0.5, 2, 5, 10, 20)) +
    scale_color_discrete(name = "Regime", labels = c("1" = "FF", "2" = "RB")) +
    labs(title = sprintf("Empirical CCPs — %s, by Regime and Age Bin", wall_lbl),
         subtitle = "Raw data shares; no smoothing. Dot area = cell share of estimation sample.",
         x = "Age bin", y = "Choice probability") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(out_png, g, width = 10, height = 6, dpi = 150)
  cat(sprintf("Figure saved: %s\n", out_png))
}
make_ccp_fig(1, "Single-Walled",
             here("Output", "Figures", "T011_B1_EmpiricalCCPs_SW.png"))
make_ccp_fig(2, "Double-Walled",
             here("Output", "Figures", "T011_B2_EmpiricalCCPs_DW.png"))

# 3.6 FIGURE B3 — replace share among closures (faceted by regime so RB is visible;
#     P_R_cond computed in Section 3.3). DW = 1.0 by construction; SW carries variation.
g_b3 <- ggplot(cell_dt[n_close > 0],
               aes(x = A_bin, y = P_R_cond, color = wall, group = wall)) +
  geom_line() + geom_point(aes(size = n_close)) +
  facet_wrap(~regime) +
  scale_x_continuous(breaks = 1:8, labels = age_labels) +
  scale_color_discrete(name = "Wall") +
  scale_y_continuous(limits = c(0, 1)) +
  guides(size = "none") +
  labs(title = "P(Replace | Closure) by Age, Wall, and Regime",
       subtitle = "DW closures are coded Replace by construction (P=1.0); SW carries the variation.",
       x = "Age bin", y = "P(Replace | Closure)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(here("Output", "Figures", "T011_B3_ReplaceShareAmongClosures.png"),
       g_b3, width = 9, height = 5, dpi = 150)
cat("Figure B3 saved.\n")

# ==============================================================================
cat("\n=== SECTION 4 — CCP VARIATION BY FACILITY SIZE (Task C) ===\n")
# ==============================================================================

# 4.1 add BOY size_bin to obs panel (carried from fp; do NOT recompute)
obs_s <- merge(obs[!is.na(premium)],
               fp[, .(panel_id, panel_year, size_bin, any_closure,
                      facility_complete_closure)],
               by = c("panel_id", "panel_year"), all.x = TRUE)
obs_s[is.na(size_bin), size_bin := "unknown"]   # obs rows not in fp
pct_unknown <- 100 * obs_s[size_bin == "unknown", .N] / nrow(obs_s)
cat(sprintf("obs_s rows: %d; size_bin == 'unknown' (BOY): %.3f%%\n",
            nrow(obs_s), pct_unknown))

# 4.2 TABLE C1 (nominal labels + conditional CCPs added)
size_cell_dt <- obs_s[, .(
  n_obs     = .N,
  n_close   = sum(y_it == 1L),
  n_exit    = sum(y_it == 1L & (is.na(I_replace) | I_replace == 0L)),
  n_replace = sum(y_it == 1L & !is.na(I_replace) & I_replace == 1L),
  P_M       = mean(y_it == 0L),
  P_E       = mean(y_it == 1L & (is.na(I_replace) | I_replace == 0L)),
  P_R       = mean(y_it == 1L & !is.na(I_replace) & I_replace == 1L),
  P_close   = mean(y_it == 1L)
), by = .(size_bin, s_idx, A_bin, w_state, rho_state)]
size_cell_dt[, `:=`(
  wall     = wall_lab[as.character(w_state)],
  regime   = regime_lab[as.character(rho_state)],
  age      = age_labels[A_bin],
  P_E_cond = n_exit    / pmax(n_close, 1L),
  P_R_cond = n_replace / pmax(n_close, 1L))]
setorder(size_cell_dt, size_bin, s_idx)
c1 <- size_cell_dt[, .(size_bin, s_idx, age, wall, regime, A_bin, w_state, rho_state,
                       n_obs, n_close, P_M, P_E, P_R, P_close, P_E_cond, P_R_cond)]
fwrite(c1, here("Output", "Tables", "T011_C1_CCPs_by_SizeBin_Cell.csv"))
cat(sprintf("C1 saved (%d rows).\n", nrow(c1)))

# 4.3 / 4.4 FIGURES C1 (Exit) and C2 (Replace), SW only, n_obs >= 20, 4 real bins
make_size_fig <- function(yvar, ylab, title, out_png) {
  d <- size_cell_dt[w_state == 1 & n_obs >= 20 &
                      size_bin %in% c("1", "2", "3", "4+")]
  g <- ggplot(d, aes(x = A_bin, y = get(yvar), color = size_bin,
                     group = size_bin)) +
    geom_line() + geom_point(aes(size = n_obs)) +
    facet_wrap(~factor(rho_state),
               labeller = labeller(`factor(rho_state)` = c("1" = "FF", "2" = "RB"))) +
    scale_x_continuous(breaks = 1:8, labels = age_labels) +
    scale_color_discrete(name = "Size bin") +
    guides(size = "none") +
    labs(title = title,
         subtitle = "If lines coincide by size, size does not belong in DCM state space",
         x = "Age bin", y = ylab) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(out_png, g, width = 10, height = 5, dpi = 150)
  cat(sprintf("Figure saved: %s\n", out_png))
}
make_size_fig("P_E", "P(Exit)", "P(Exit) by Facility Size, Age, and Regime — SW",
              here("Output", "Figures", "T011_C1_ExitRate_by_Size_SW.png"))
make_size_fig("P_R", "P(Replace)", "P(Replace) by Facility Size, Age, and Regime — SW",
              here("Output", "Figures", "T011_C2_ReplaceRate_by_Size_SW.png"))

# 4.5 TABLE C2 — partial closure rate by (size_bin, regime), FULL facility_panel
fp[, regime := fcase(
  texas_treated == 1L & panel_year >= 1999L, "TX_RB",
  texas_treated == 1L & panel_year <  1999L, "TX_FF",
  texas_treated == 0L,                       "Control_FF",
  default = NA_character_)]
n_c2_unknown <- fp[size_bin == "unknown" & !is.na(regime) & n_closures > 0, .N]
partial_rate <- fp[size_bin != "unknown" & !is.na(regime) & n_closures > 0, .(
  n_closure_events = .N,
  n_partial        = sum(any_closure == 1 & facility_complete_closure == 0),
  n_full_exit      = sum(facility_complete_closure == 1 & replacement_closure_year == 0),
  n_full_replace   = sum(facility_complete_closure == 1 & replacement_closure_year == 1),
  pct_partial      = mean(any_closure == 1 & facility_complete_closure == 0) * 100,
  pct_full_exit    = mean(facility_complete_closure == 1 & replacement_closure_year == 0) * 100,
  pct_full_replace = mean(facility_complete_closure == 1 & replacement_closure_year == 1) * 100
), by = .(size_bin, regime)]
setorder(partial_rate, size_bin, regime)
fwrite(partial_rate, here("Output", "Tables", "T011_C2_PartialClosure_by_Size_Regime.csv"))
cat(sprintf("C2 saved (%d rows; %d closure-event rows had size_bin==unknown, excluded).\n",
            nrow(partial_rate), n_c2_unknown))
print(partial_rate)

# 4.6 FIGURE C3 — stacked closure composition, n_closure_events >= 30
c3_long <- melt(partial_rate[n_closure_events >= 30],
                id.vars = c("size_bin", "regime"),
                measure.vars = c("pct_partial", "pct_full_exit", "pct_full_replace"),
                variable.name = "action_type", value.name = "pct")
c3_long[, action_type := factor(action_type,
  levels = c("pct_partial", "pct_full_exit", "pct_full_replace"),
  labels = c("Partial", "Full exit", "Full replace"))]
g_c3 <- ggplot(c3_long, aes(x = size_bin, y = pct, fill = action_type)) +
  geom_col() +
  facet_wrap(~regime) +
  scale_fill_brewer(name = "Action", palette = "Set2") +
  labs(title = "Closure composition by facility size and regime",
       subtitle = "Among facility-years with >=1 closure event",
       x = "Size bin", y = "Percent of closure events") +
  theme_bw()
ggsave(here("Output", "Figures", "T011_C3_ClosureComposition_by_Size.png"),
       g_c3, width = 10, height = 6, dpi = 150)
cat("Figure C3 saved.\n")

# ==============================================================================
cat("\n=== RESEARCHER SUMMARY ===\n")
# ==============================================================================
cat("TASK A — Action coding:\n")
cat(sprintf("  Partial closures: %d (%.2f%% of est. sample), %d Maintain (%.1f%%)\n",
            n_partial, 100 * n_partial / n_merged, n_partial_maint,
            100 * n_partial_maint / max(n_partial, 1L)))
cat(sprintf("  Double-triggered (retrofit wins over full exit): %d (%.2f%% of closures)\n",
            n_double_trig, 100 * n_double_trig / max(n_closures, 1L)))
cat(sprintf("  Replace definition: %.1f%% SW->DW, %.1f%% broad-only\n",
            pct_sw2dw, pct_broad))
cat(sprintf("  Identity check: %s\n",
            if (n_identity_disc == 0) "PASS" else sprintf("FAIL (%d)", n_identity_disc)))

cat("TASK B — True data CCPs:\n")
cat(sprintf("  Overall (weighted) P_M=%.4f P_E=%.5f P_R=%.5f\n",
            ovr_P_M, ovr_P_E, ovr_P_R))
top_e <- cell_dt[n_obs > 0][which.max(P_E)]
top_r <- cell_dt[n_obs > 0][which.max(P_R)]
cat(sprintf("  Highest P_E cell: s_idx=%d (A=%d,wall=%d,regime=%d) P_E=%.4f\n",
            top_e$s_idx, top_e$A_bin, top_e$w_state, top_e$rho_state, top_e$P_E))
cat(sprintf("  Highest P_R cell: s_idx=%d (A=%d,wall=%d,regime=%d) P_R=%.4f\n",
            top_r$s_idx, top_r$A_bin, top_r$w_state, top_r$rho_state, top_r$P_R))

cat("TASK C — Size heterogeneity:\n")
sz <- size_cell_dt[n_obs >= 20 & size_bin %in% c("1", "2", "3", "4+")]
spread <- sz[, .(rng_PE = max(P_E) - min(P_E), rng_PR = max(P_R) - min(P_R),
                 n_sizes = uniqueN(size_bin)), by = s_idx][n_sizes >= 2]
cat(sprintf("  Max P_E spread across size bins within a cell: %.4f\n",
            if (nrow(spread)) max(spread$rng_PE) else NA_real_))
cat(sprintf("  Max P_R spread across size bins within a cell: %.4f\n",
            if (nrow(spread)) max(spread$rng_PR) else NA_real_))
pp_spread <- partial_rate[n_closure_events >= 30,
  .(rng = max(pct_partial) - min(pct_partial)), by = regime]
cat(sprintf("  Partial-closure-rate spread across size bins (max over regimes): %.1f pp -> differs >5pp: %s\n",
            if (nrow(pp_spread)) max(pp_spread$rng) else NA_real_,
            if (nrow(pp_spread) && max(pp_spread$rng) > 5) "YES" else "NO"))
cat("=========================\n")

cat(sprintf("\nLOG END %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
