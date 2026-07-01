# 02m_Facility_Rollup_Baseline.R
# TWO-PANEL summary / balance exhibit for the TICKET-038 FACILITY ROLL-UP (02l) sample.
# Section 3 "tbl-summary" of the paper. Restores the two-panel design (canvas para 8).
#
#   Panel A  TX-vs-control facility BALANCE, before vs after birth-CEM, with SMDs.
#            before = all birth-cohort (install_yr<1999) facilities active at the 1998 reform;
#            after  = the same, restricted to CEM-matched facilities (cem_weight>0) = the 02l
#                     estimation sample (data_C_active). The only toggle between the two columns
#                     is the cem_weight>0 filter, so the SMD change isolates CEM pruning to common
#                     support. BOTH unweighted (matches the unweighted 038 DiD; a facility-level
#                     cem_weight is not well defined; residual composition is absorbed by the
#                     cell x year FE in the regressions).
#   Panel B  Pre-reform (panel_year<1999) base rates of the 7 outcome margins, by TX/control x
#            pre/post, plus the raw DiD. The control-pre column is the exact denominator the
#            Section-4 ATTs are quoted against ("+1.66pp on a X% control mean").
#
# Sample reconstructed independently here (mirror 02b:3965-3988 / 02l Section 1) so the table and
#   the regressions describe the same stations. UNWEIGHTED throughout (no cem_weight in any mean
#   except where noted; here: nowhere). Characteristics are FACILITY-level (stations equal-weighted).
#   any_closure = matched roll-up; the 6 portfolio margins = facility_panel.csv (Ticket-031 defs,
#   ported verbatim from 02j build_margins / 02l Section 4).
#
# Run from repo root ON THE SERVER, AFTER 02l finishes (avoids two >4 GB reads at once):
#   & "C:/Program Files/R/R-4.4.3/bin/x64/Rscript.exe" Code/Analysis/02m_Facility_Rollup_Baseline.R
# Prints both panels to console (the table IS the deliverable). Outputs:
#   Output/Tables/T_Facility_Rollup_Balance_PanelA.csv    (machine-readable Panel A)
#   Output/Tables/T_Facility_Rollup_BaseRates_PanelB.csv  (machine-readable Panel B)
#   Output/Tables/T_Facility_Rollup_Summary.tex           (booktabs two-panel tabular; editor wraps caption/notes)

suppressPackageStartupMessages({ library(data.table); library(here) })
cat("=== 02m: FACILITY ROLL-UP TWO-PANEL SUMMARY (038 sample, unweighted) ===\n")

ANALYSIS_DIR <- here("Data", "Analysis")
OUT_TAB      <- here("Output", "Tables")
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
REL_THRESH   <- 0.05          # Ticket-031 capacity band (verbatim)
fmt_n <- function(x) formatC(as.numeric(x), format = "d", big.mark = ",")

MARGINS <- c("any_closure", "facility_exit", "downsize", "consolidate",
             "reconfigure_up", "any_replace", "cap_decrease")
MARGIN_LAB <- c(any_closure = "Any closure", facility_exit = "Facility exit",
                downsize = "Downsize", consolidate = "Consolidate",
                reconfigure_up = "Reconfigure-up", any_replace = "Any replacement",
                cap_decrease = "Capacity cut")

# ---------------------------------------------------------------------------
# 1. Read the birth-CEM tank file; build BEFORE (pre-CEM) and AFTER (matched) samples
# ---------------------------------------------------------------------------
mt_path <- file.path(ANALYSIS_DIR, "matched_tanks_birth_cem.csv")
hdr      <- names(fread(mt_path, nrows = 0))
wall_col <- if ("single_wall" %in% hdr) "single_wall" else if ("mm_wall" %in% hdr) "mm_wall" else NA_character_
base_need <- c("panel_id", "tank_panel_id", "texas_treated", "install_yr_int", "cem_weight",
               "panel_year", "closure_event", "first_year_churn")
opt_need  <- intersect(c("capacity", wall_col), hdr)
need      <- intersect(c(base_need, opt_need), hdr)
stopifnot(all(base_need %in% need))   # the sample-defining columns must be present

mt <- fread(mt_path, na.strings = c("", "NA"), select = need)
mt <- mt[first_year_churn == 0L | is.na(first_year_churn)]

# active-at-1998 union (>=1 tank open in 1998 OR newly installed in 1998), computed within a pool
active_ids <- function(d)
  union(d[panel_year == 1998L & closure_event == 0L, unique(panel_id)],
        d[install_yr_int == 1998L, unique(panel_id)])

# BEFORE CEM: full birth cohort, no cem_weight filter
dC_before  <- mt[install_yr_int < 1999L]
dca_before <- dC_before[panel_id %in% active_ids(dC_before)]

# AFTER CEM: matched only (= 02l data_C_active)
dC_after   <- mt[install_yr_int < 1999L & cem_weight > 0]
dca_after  <- dC_after[panel_id %in% active_ids(dC_after)]

n_pruned_rows <- mt[install_yr_int < 1999L & (is.na(cem_weight) | cem_weight <= 0), .N]
cat(sprintf("  BEFORE-CEM facilities (active-1998) = %s | AFTER-CEM (matched) = %s | pruned rows = %s\n",
            fmt_n(uniqueN(dca_before$panel_id)), fmt_n(uniqueN(dca_after$panel_id)), fmt_n(n_pruned_rows)))
if (uniqueN(dca_before$panel_id) == uniqueN(dca_after$panel_id))
  cat("  WARNING: before == after -- this file has no pruned (cem_weight<=0) rows; the\n",
      "           before/after contrast is empty. Source the pre-match pool from matched_tanks.csv\n",
      "           / panel_dt.csv instead if a genuine before-CEM column is required.\n", sep = "")

# ---------------------------------------------------------------------------
# 2. Facility-level characteristics (one row per facility; stations equal-weighted)
# ---------------------------------------------------------------------------
build_fac <- function(dca) {
  keep_tk <- intersect(c("panel_id", "tank_panel_id", "texas_treated",
                         "install_yr_int", "capacity", wall_col), names(dca))
  tk <- unique(dca[, ..keep_tk], by = "tank_panel_id")
  tk[, pre89      := as.integer(install_yr_int < 1989L)]
  tk[, age_reform := 1999L - install_yr_int]
  if (!is.na(wall_col)) {
    if (wall_col == "single_wall") tk[, sw := as.integer(single_wall)]
    else                           tk[, sw := as.integer(mm_wall == "Single-Walled")]
  }
  agg <- list(
    texas_treated = quote(texas_treated[1L]),
    n_tanks       = quote(.N),
    avg_age       = quote(mean(age_reform, na.rm = TRUE)),
    pre89_share   = quote(mean(pre89, na.rm = TRUE))
  )
  if ("capacity" %in% names(tk)) agg$total_cap_k <- quote(sum(capacity, na.rm = TRUE) / 1000)
  if ("sw" %in% names(tk))       agg$sw_share    <- quote(mean(sw, na.rm = TRUE))
  tk[, eval(as.call(c(quote(list), agg))), by = panel_id]
}
fac_before <- build_fac(dca_before)
fac_after  <- build_fac(dca_after)
cat(sprintf("  Facilities: BEFORE TX=%s Ctrl=%s | AFTER TX=%s Ctrl=%s\n",
            fmt_n(fac_before[texas_treated == 1L, .N]), fmt_n(fac_before[texas_treated == 0L, .N]),
            fmt_n(fac_after[texas_treated == 1L, .N]),  fmt_n(fac_after[texas_treated == 0L, .N])))

# ---------------------------------------------------------------------------
# 3. PANEL A — TX vs control balance, before/after CEM, with SMDs (unweighted)
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
VARS <- VARS[names(VARS) %in% intersect(names(fac_before), names(fac_after))]

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
cat("\n  --- PANEL A: balance (TX vs control), before/after CEM, unweighted ---\n"); print(panelA)

# ---------------------------------------------------------------------------
# 4. PANEL B — pre-reform outcome base rates (control denominator for Section 4)
# ---------------------------------------------------------------------------
# 4a. facility-year skeleton from the matched sample: any_closure + texas_treated + post
fy <- dca_after[, .(any_closure   = as.integer(sum(closure_event, na.rm = TRUE) > 0),
                    texas_treated = texas_treated[1L]),
                by = .(panel_id, panel_year)]
fy[, post := as.integer(panel_year >= 1999L)]

# 4b. the 6 portfolio margins from facility_panel.csv (Ticket-031 defs; verbatim port of
#     02j build_margins / 02l Section 4). REL_THRESH = 0.05.
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
# 5. Combined two-panel booktabs tabular (.tex); editor adds caption + \textit{Notes:}
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
  " & \\multicolumn{3}{c}{Before CEM} & \\multicolumn{3}{c}{After CEM (matched)} \\\\",
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
