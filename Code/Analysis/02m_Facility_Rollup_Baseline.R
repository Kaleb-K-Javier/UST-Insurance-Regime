# 02m_Facility_Rollup_Baseline.R
# TWO-PANEL summary / balance exhibit for the TICKET-038 FACILITY ROLL-UP (02l) sample.
# Section 3 "tbl-summary" of the paper. Two-panel design (canvas para 8).
#
#   Panel A  TX-vs-control facility BALANCE, before vs after birth-CEM, with SMDs.
#            before = the FULL eligible pre-match population: all birth-cohort (install_yr<1999)
#                     facilities active at the 1998 reform in the 18 analysis states (TX + 17 donor
#                     controls), read from panel_dt.csv (the raw panel). This is the population CEM
#                     matched FROM.
#            after  = the birth-CEM matched subset (matched_tanks_birth_cem.csv, cem_weight>0, active
#                     at 1998) = the 02l estimation sample (data_C_active).
#            The SMD change before->after shows what CEM matching/pruning did. BOTH unweighted (the
#            038 DiD is unweighted; a facility-level cem_weight is not well defined; residual
#            composition is absorbed by the cell x year FE in the regressions). Single-walled share
#            uses mm_wall in BOTH so the definition is identical across columns.
#   Panel B  Pre-reform (panel_year<1999) base rates of the 7 outcome margins, by TX/control x
#            pre/post, plus the raw DiD. control_pre is the exact denominator the Section-4 ATTs are
#            quoted against ("+1.66pp on a X% control mean").
#
# NOTE (2026-07-01): matched_tanks_birth_cem.csv contains ONLY matched tanks (no cem_weight<=0 rows),
#   so the before/after contrast CANNOT be built by toggling cem_weight>0 within it -- the pre-match
#   pool must come from panel_dt.csv. That is why 'before' reads a different (larger) file.
#
# Sample def mirrors 02b:3965-3988 / 02l Section 1 (first_year_churn filter; install_yr<1999;
#   active-at-1998 union). UNWEIGHTED throughout (no cem_weight in any mean). any_closure = matched
#   roll-up; the 6 portfolio margins = facility_panel.csv (Ticket-031 defs, verbatim from 02l Sec 4).
#
# Run from repo root ON THE SERVER, AFTER 02l finishes. Reads three large panels sequentially
#   (panel_dt ~4.1GB, matched_tanks_birth_cem ~4.6GB, facility_panel ~3.9GB); each big table is freed
#   before the next read.
#   & "C:/Program Files/R/R-4.4.3/bin/x64/Rscript.exe" Code/Analysis/02m_Facility_Rollup_Baseline.R
# Prints both panels to console (the table IS the deliverable). Outputs:
#   Output/Tables/T_Facility_Rollup_Balance_PanelA.csv    (machine-readable Panel A)
#   Output/Tables/T_Facility_Rollup_BaseRates_PanelB.csv  (machine-readable Panel B)
#   Output/Tables/T_Facility_Rollup_Summary.tex           (booktabs two-panel tabular; editor wraps caption/notes)

suppressPackageStartupMessages({ library(data.table); library(here) })
cat("=== 02m: FACILITY ROLL-UP TWO-PANEL SUMMARY (038 sample, unweighted) ===\n")

ANALYSIS_DIR <- here("Data", "Analysis")
OUT_TAB      <- here("Output", "Tables")
GAS_COMP     <- here("Output", "GIS", "gis_gas_competitors.csv")   # motor-fuel competitor counts
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
REL_THRESH   <- 0.05          # Ticket-031 capacity band (verbatim)
CONTROL_STATES <- c("AR","CO","ID","KS","KY","LA","MA","MD","ME","MN","MO","NC","OH","OK","SD","TN","VA")
ANALYSIS_STATES <- c("TX", CONTROL_STATES)   # 18 states = the CEM treated + donor pool
fmt_n <- function(x) formatC(as.numeric(x), format = "d", big.mark = ",")

MARGINS <- c("any_closure", "facility_exit", "downsize", "consolidate",
             "reconfigure_up", "any_replace", "cap_decrease")
MARGIN_LAB <- c(any_closure = "Any closure", facility_exit = "Facility exit",
                downsize = "Downsize", consolidate = "Consolidate",
                reconfigure_up = "Reconfigure-up", any_replace = "Any replacement",
                cap_decrease = "Capacity cut")

# active-at-1998 union (>=1 tank open in 1998 OR newly installed in 1998), within a given pool
active_ids <- function(d)
  union(d[panel_year == 1998L & closure_event == 0L, unique(panel_id)],
        d[install_yr_int == 1998L, unique(panel_id)])

# facility-level characteristics (one row per facility; stations equal-weighted). mm_wall used for sw.
build_fac <- function(dca) {
  tk <- unique(dca[, .(panel_id, tank_panel_id, texas_treated, install_yr_int, capacity, mm_wall)],
               by = "tank_panel_id")
  tk[, pre89      := as.integer(install_yr_int < 1989L)]
  tk[, age_reform := 1999L - install_yr_int]
  tk[, sw         := as.integer(mm_wall == "Single-Walled")]
  tk[, .(texas_treated = texas_treated[1L],
         n_tanks       = .N,
         avg_age       = mean(age_reform, na.rm = TRUE),
         pre89_share   = mean(pre89, na.rm = TRUE),
         total_cap_k   = sum(capacity, na.rm = TRUE) / 1000,
         sw_share      = mean(sw, na.rm = TRUE)),
     by = panel_id]
}

# ---------------------------------------------------------------------------
# 1a. BEFORE-CEM pool = full eligible pre-match population (panel_dt.csv)
# ---------------------------------------------------------------------------
pdt_cols <- c("panel_id", "tank_panel_id", "state", "texas_treated", "mm_wall",
              "install_yr_int", "capacity", "panel_year", "closure_event", "first_year_churn")
pdt <- fread(file.path(ANALYSIS_DIR, "panel_dt.csv"), select = pdt_cols, na.strings = c("", "NA"))
pdt <- pdt[(first_year_churn == 0L | is.na(first_year_churn)) & state %in% ANALYSIS_STATES]
dC_before  <- pdt[install_yr_int < 1999L]
dca_before <- dC_before[panel_id %in% active_ids(dC_before)]
fac_before <- build_fac(dca_before)
cat(sprintf("  BEFORE-CEM pool (panel_dt, 18 states, birth-cohort, active-1998): facilities=%s (TX=%s Ctrl=%s)\n",
            fmt_n(uniqueN(dca_before$panel_id)),
            fmt_n(fac_before[texas_treated == 1L, .N]), fmt_n(fac_before[texas_treated == 0L, .N])))
rm(pdt, dC_before, dca_before); invisible(gc())

# ---------------------------------------------------------------------------
# 1b. AFTER-CEM pool = birth-CEM matched subset (= 02l data_C_active)
# ---------------------------------------------------------------------------
mt_cols <- c("panel_id", "tank_panel_id", "texas_treated", "mm_wall", "install_yr_int",
             "capacity", "cem_weight", "panel_year", "closure_event", "first_year_churn")
mt <- fread(file.path(ANALYSIS_DIR, "matched_tanks_birth_cem.csv"), select = mt_cols, na.strings = c("", "NA"))
mt <- mt[first_year_churn == 0L | is.na(first_year_churn)]
dC_after  <- mt[install_yr_int < 1999L & cem_weight > 0]
dca_after <- dC_after[panel_id %in% active_ids(dC_after)]
fac_after <- build_fac(dca_after)
cat(sprintf("  AFTER-CEM (matched, active-1998): facilities=%s (TX=%s Ctrl=%s)\n",
            fmt_n(uniqueN(dca_after$panel_id)),
            fmt_n(fac_after[texas_treated == 1L, .N]), fmt_n(fac_after[texas_treated == 0L, .N])))
cat(sprintf("  CEM pruned %s facilities (%.1f%% of the before pool)\n",
            fmt_n(uniqueN(fac_before$panel_id) - uniqueN(fac_after$panel_id)),
            100 * (uniqueN(fac_before$panel_id) - uniqueN(fac_after$panel_id)) / uniqueN(fac_before$panel_id)))
# keep a slim facility-year skeleton for Panel B; drop the big matched tank table
fy <- dca_after[, .(any_closure   = as.integer(sum(closure_event, na.rm = TRUE) > 0),
                    texas_treated = texas_treated[1L]),
                by = .(panel_id, panel_year)]
fy[, post := as.integer(panel_year >= 1999L)]
rm(mt, dC_after, dca_after); invisible(gc())

# motor-fuel share (is_gas) + gasoline-competitor count within 1 mi (n_gas_1609m), joined by panel_id.
# Source: Output/GIS/gis_gas_competitors.csv (Code/GIS/03_gas_competitors.R). Fixed pre-reform geography.
HAS_GAS <- file.exists(GAS_COMP)
if (HAS_GAS) {
  gcmp <- fread(GAS_COMP, select = c("panel_id", "is_gas", "n_gas_1609m"), na.strings = c("", "NA"))
  fac_before <- merge(fac_before, gcmp, by = "panel_id", all.x = TRUE)
  fac_after  <- merge(fac_after,  gcmp, by = "panel_id", all.x = TRUE)
  cat(sprintf("  motor-fuel share: before TX=%.3f Ctrl=%.3f | after TX=%.3f Ctrl=%.3f\n",
      fac_before[texas_treated == 1L, mean(is_gas, na.rm = TRUE)], fac_before[texas_treated == 0L, mean(is_gas, na.rm = TRUE)],
      fac_after[texas_treated == 1L,  mean(is_gas, na.rm = TRUE)], fac_after[texas_treated == 0L,  mean(is_gas, na.rm = TRUE)]))
  cat(sprintf("  gas competitors <=1mi (MEDIAN): before TX=%.0f Ctrl=%.0f | after TX=%.0f Ctrl=%.0f\n",
      median(fac_before[texas_treated == 1L]$n_gas_1609m, na.rm = TRUE), median(fac_before[texas_treated == 0L]$n_gas_1609m, na.rm = TRUE),
      median(fac_after[texas_treated == 1L]$n_gas_1609m,  na.rm = TRUE), median(fac_after[texas_treated == 0L]$n_gas_1609m,  na.rm = TRUE)))
} else cat("  gas-competitors lookup absent -- motor-fuel/competition rows skipped\n")

# ---------------------------------------------------------------------------
# 2. PANEL A — TX vs control balance, before/after CEM, with SMDs (unweighted)
# ---------------------------------------------------------------------------
smd <- function(x, g) {
  a <- x[g == 1L]; b <- x[g == 0L]
  d <- sqrt((var(a, na.rm = TRUE) + var(b, na.rm = TRUE)) / 2)
  if (!is.finite(d) || d == 0) return(NA_real_)
  (mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE)) / d
}
bal_one <- function(fac, v) list(
  tx   = mean(fac[texas_treated == 1L][[v]], na.rm = TRUE),
  ctrl = mean(fac[texas_treated == 0L][[v]], na.rm = TRUE),
  smd  = smd(fac[[v]], fac$texas_treated))

VARS <- c(n_tanks     = "Tanks per station",
          total_cap_k = "Total capacity (000 gal)",
          avg_age     = "Mean tank age at reform (yrs)",
          sw_share    = "Single-walled share",
          pre89_share = "Pre-1989 vintage share")
if (HAS_GAS) VARS <- c(VARS, is_gas      = "Motor-fuel share",
                             n_gas_1609m = "Gas competitors within 1 mi (mean)")

panelA <- rbindlist(lapply(names(VARS), function(v) {
  b <- bal_one(fac_before, v); a <- bal_one(fac_after, v)
  data.table(characteristic = VARS[[v]],
             tx_before = b$tx, ctrl_before = b$ctrl, smd_before = b$smd,
             tx_after  = a$tx, ctrl_after  = a$ctrl, smd_after  = a$smd)
}))
panelA <- rbind(
  data.table(characteristic = "N stations",
             tx_before = fac_before[texas_treated == 1L, .N], ctrl_before = fac_before[texas_treated == 0L, .N], smd_before = NA_real_,
             tx_after  = fac_after[texas_treated == 1L, .N],  ctrl_after  = fac_after[texas_treated == 0L, .N],  smd_after  = NA_real_),
  panelA)
fwrite(panelA, file.path(OUT_TAB, "T_Facility_Rollup_Balance_PanelA.csv"))
cat("\n  --- PANEL A: balance (TX vs control), before(full pool)/after(matched) CEM, unweighted ---\n"); print(panelA)

# ---------------------------------------------------------------------------
# 3. PANEL B — pre-reform outcome base rates (control denominator for Section 4)
# ---------------------------------------------------------------------------
# the 6 portfolio margins from facility_panel.csv (Ticket-031 defs; verbatim port of 02l Section 4).
fp_cols <- c("panel_id", "panel_year", "facility_exit", "net_tank_change",
             "capacity_change", "total_capacity_dec", "n_closures_replacement", "capacity_decreased")
fp <- fread(file.path(ANALYSIS_DIR, "facility_panel.csv"), select = fp_cols, na.strings = c("", "NA"))
fp[, dC   := capacity_change]
fp[, Clag := total_capacity_dec - dC]
fp[, rel_cap     := fifelse(Clag > 0, dC / Clag, NA_real_)]
fp[, contraction := as.integer(facility_exit == 0L & net_tank_change < 0L & Clag > 0 & !is.na(dC))]
fp[, consolidate    := as.integer(contraction == 1L & abs(rel_cap) <= REL_THRESH)]
fp[, downsize       := as.integer(contraction == 1L & rel_cap <  -REL_THRESH)]
fp[, reconfigure_up := as.integer(contraction == 1L & rel_cap >   REL_THRESH)]
fp[is.na(consolidate),    consolidate    := 0L]
fp[is.na(downsize),       downsize       := 0L]
fp[is.na(reconfigure_up), reconfigure_up := 0L]
fp[, any_replace  := as.integer(n_closures_replacement > 0L)]; fp[is.na(any_replace),  any_replace  := 0L]
fp[, cap_decrease := as.integer(capacity_decreased)];          fp[is.na(cap_decrease), cap_decrease := 0L]
stopifnot(fp[, max(consolidate + downsize + reconfigure_up)] <= 1L)

fy <- merge(fy, fp[, .(panel_id, panel_year, facility_exit, downsize, consolidate,
                       reconfigure_up, any_replace, cap_decrease)],
            by = c("panel_id", "panel_year"), all.x = TRUE)

cell_mean <- function(m, tx, po) mean(fy[texas_treated == tx & post == po][[m]], na.rm = TRUE)
panelB <- rbindlist(lapply(MARGINS, function(m) {
  cp <- cell_mean(m, 0L, 0L); cq <- cell_mean(m, 0L, 1L)
  tp <- cell_mean(m, 1L, 0L); tq <- cell_mean(m, 1L, 1L)
  data.table(margin = m, label = MARGIN_LAB[[m]],
             ctrl_pre = cp, ctrl_post = cq, tx_pre = tp, tx_post = tq,
             raw_did = (tq - tp) - (cq - cp),
             n_ctrl_pre = fy[texas_treated == 0L & post == 0L & !is.na(get(m)), .N])
}))
fwrite(panelB, file.path(OUT_TAB, "T_Facility_Rollup_BaseRates_PanelB.csv"))
cat("\n  --- PANEL B: pre-reform outcome base rates (control_pre is the Section-4 denominator) ---\n")
print(panelB)

# ---------------------------------------------------------------------------
# 4. Combined two-panel booktabs tabular (.tex); editor adds caption + \textit{Notes:}
# ---------------------------------------------------------------------------
f2  <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
f4  <- function(x) formatC(x, format = "f", digits = 4)
fS  <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 2))   # SMD
rowsA <- vapply(seq_len(nrow(panelA)), function(i) {
  r <- panelA[i]
  if (r$characteristic == "N stations")
    sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", r$characteristic,
            fmt_n(r$tx_before), fmt_n(r$ctrl_before), "",
            fmt_n(r$tx_after),  fmt_n(r$ctrl_after),  "")
  else
    sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", r$characteristic,
            f2(r$tx_before), f2(r$ctrl_before), fS(r$smd_before),
            f2(r$tx_after),  f2(r$ctrl_after),  fS(r$smd_after))
}, character(1))
rowsB <- sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
                 panelB$label, f4(panelB$ctrl_pre), f4(panelB$ctrl_post),
                 f4(panelB$tx_pre), f4(panelB$tx_post), fS(panelB$raw_did), fmt_n(panelB$n_ctrl_pre))

writeLines(c(
  "\\begin{tabular}{lrrrrrr}", "\\toprule",
  "\\multicolumn{7}{l}{\\textit{Panel A. Facility balance: Texas vs.\\ control, before and after birth-CEM}} \\\\",
  "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
  " & \\multicolumn{3}{c}{Before CEM (full pool)} & \\multicolumn{3}{c}{After CEM (matched)} \\\\",
  " & Texas & Control & SMD & Texas & Control & SMD \\\\", "\\midrule",
  rowsA, "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Panel B. Pre-reform outcome base rates (facility-year indicators)}} \\\\",
  " & Control & Control & Texas & Texas & Raw & $N$ \\\\",
  " & pre & post & pre & post & DiD & (ctrl pre) \\\\", "\\midrule",
  rowsB, "\\bottomrule", "\\end{tabular}"),
  file.path(OUT_TAB, "T_Facility_Rollup_Summary.tex"))

cat(sprintf("\n  Written: T_Facility_Rollup_Balance_PanelA.csv (%d rows), T_Facility_Rollup_BaseRates_PanelB.csv (%d rows),\n           T_Facility_Rollup_Summary.tex (two-panel)\n",
            nrow(panelA), nrow(panelB)))
cat("=== 02m DONE ===\n")
