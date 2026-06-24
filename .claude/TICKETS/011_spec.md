# TICKET 011 — Action-coding audit + empirical CCP diagnostics
# Created: 2026-06-01
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0
# Assignee: Sonnet (read-only analytics — no estimation, no model changes)

═══════════════════════════════════════════════════
MOTIVATION
═══════════════════════════════════════════════════
Before extending the DCM state space (add size dimension) or adding a
4th model action (partial closure), the researcher needs:

  1. A clear audit of how Exit / Replace / Maintain are coded in the
     raw data pipeline (02b → 04b) — including edge cases and the
     treatment of partial closures.

  2. The true data CCPs (empirical choice probabilities) for all three
     actions by state cell, so the researcher can see how common each
     action is and how it varies with age / wall / regime.

  3. Documentation of how CCPs and partial-closure rates vary across
     facility size bins, to inform both DiD heterogeneity analysis and
     the DCM model-extension decision (Option A: 4th action vs
     Option B: size as state dimension, per §4.8.5 of
     Identifying_Variation_Size_Capacity.qmd).

  4. All of the above embedded in the existing
     Reports/Paper/Identifying_Variation_Size_Capacity.qmd as new
     top-level section §6 "T011 Diagnostics", with ZERO prose — only
     section headers, tables, and figures. Every chunk is guarded with
     file.exists() so the qmd still compiles before T011 runs.

This is a READ-ONLY diagnostics script. No estimation, no model changes,
no edits to any existing script. All output comes from:
  - Data/Analysis/dcm_obs_panel_observed.csv       (T007 cleaned, ~2.28M rows)
  - Data/Analysis/facility_panel.csv               (use data_in() fallback)
  - Output/Estimation_Results/DCM_Primitives_Replacement_observed.rds

═══════════════════════════════════════════════════
ENVIRONMENT
═══════════════════════════════════════════════════
Rscript: C:\Program Files\R\R-4.5.2\bin\Rscript.exe
Working dir: repo root (here::here())
Packages: data.table, ggplot2, here, scales

Prereqs — verify these exist before running:
  Data/Analysis/dcm_obs_panel_observed.csv         (T007 Phase 2 output)
  Output/Estimation_Results/DCM_Primitives_Replacement_observed.rds
  Data/Analysis/facility_panel.csv                 (any version; use data_in())

If dcm_obs_panel_observed.csv is missing: stop with message "Run 04b first".
If facility_panel.csv only exists on Z:  use z_path() and note in output log.

═══════════════════════════════════════════════════
SCRIPT STRUCTURE
═══════════════════════════════════════════════════
New file: Code/Analysis/T011_ActionCoding_CCP_Diagnostics.R

Use standard CLAUDE.md logging block and section headers.

─────────────────────────────────────────────────
SECTION 1 — LOAD DATA
─────────────────────────────────────────────────

1.1  Source Code/Helpers/data_paths.R

1.2  Load dcm_obs_panel_observed.csv:
       obs <- fread(here("Data","Analysis","dcm_obs_panel_observed.csv"))
     Columns expected: panel_id, panel_year, state, texas_treated,
       s_idx, A_bin, w_state, rho_state, premium, y_it, I_replace,
       reset_state_index, P_close_init

1.3  Load DCM_Primitives_Replacement_observed.rds:
       prim <- readRDS(here("Output","Estimation_Results",
                            "DCM_Primitives_Replacement_observed.rds"))
     Extract state_lut (32-row data.table with s_idx, A_bin, w_state, rho_state).

1.4  Load facility_panel.csv (subset of columns only — 3.2 GB file):
       KEEP_FP <- c("panel_id","panel_year","texas_treated","state",
                    "active_tanks","n_installs","n_closures","n_sw_closures",
                    "any_closure","facility_complete_closure",
                    "facility_exit","replacement_closure_year",
                    "permanent_closure_year","single_to_double_year",
                    "n_tanks_eoy","n_tanks_active","total_capacity","total_capacity_dec",
                    "has_single_walled_dec","has_double_walled_dec",
                    "all_sw","all_dw","mixed_wall","wall_type")
       fp_path <- data_in("Data","Analysis","facility_panel.csv")
       fp <- fread(fp_path, select = KEEP_FP)
     Log the source path (local vs Z) and nrow.

1.5  CANONICAL FACILITY SIZE = beginning-of-year (BOY) stock
     RATIONALE: facility "size" for a choice must be the portfolio the
     facility holds GOING INTO the year's decision, i.e. start-of-year stock.
       - n_tanks_eoy (end-of-year) is WRONG for closure events: it is 0 at a
         full closure, so EOY binning silently drops EVERY full-exit and
         full-replace event (verified: 174,119 such events bin to NA).
       - n_tanks_active (within-year, any-point count) is also WRONG for a
         same-year replacement: it counts the closing tanks PLUS the new
         install, over-sizing a "2 SW -> 1 DW" facility as size 3.
     BOY stock avoids both. It is the prior-year EOY stock, looked up
     GAP-SAFELY (the panel has no row in zero-tank years, so DO NOT row-shift):
       setorder(fp, panel_id, panel_year)
       prior <- fp[, .(panel_id, panel_year = panel_year + 1L,
                       boy_stock = n_tanks_eoy)]   # shift key forward 1 yr
       fp <- merge(fp, prior, by = c("panel_id","panel_year"), all.x = TRUE)
       # fallback when no prior-year row (left-censored first appearance):
       n_boy_fallback <- fp[is.na(boy_stock) & n_tanks_active > 0L, .N]
       fp[is.na(boy_stock), boy_stock := n_tanks_active]
       fp[, size_bin := fcase(
         boy_stock == 1L, "1",
         boy_stock == 2L, "2",
         boy_stock == 3L, "3",
         boy_stock >= 4L, "4+",
         default = "unknown")]
     Log n_boy_fallback (count + pct of fp rows using the n_tanks_active
     fallback). This single size_bin column on fp is the ONLY size measure
     used in 2.4, 4.1, and 4.5 below — carry it through every merge from fp.
     Do NOT re-derive size from active_tanks anywhere.

─────────────────────────────────────────────────
SECTION 2 — ACTION CODING AUDIT (Task A)
─────────────────────────────────────────────────
Goal: audit the 02b → 04b chain, quantify edge cases.

2.1  MERGE obs panel with facility_panel on (panel_id, panel_year) to
     recover the 02b-derived flags. Use all.x = FALSE (inner join on
     the estimation sample).

       merged <- merge(obs[, .(panel_id, panel_year, s_idx, A_bin, w_state,
                               rho_state, y_it, I_replace)],
                       fp[, .(panel_id, panel_year, active_tanks,
                              any_closure, facility_complete_closure,
                              replacement_closure_year, permanent_closure_year,
                              n_installs, n_tanks_eoy, size_bin)],
                       by = c("panel_id","panel_year"), all.x = FALSE)

2.2  TABLE A1 — Action distribution: 02b raw flags vs 04b final actions
     Show counts and pct for each combination of
     (facility_complete_closure, replacement_closure_year, y_it, I_replace).
     This reveals how the 02b flags map to the three DCM actions.

     Explicitly compute and print:
       n_maintain      <- merged[y_it == 0, .N]
       n_exit          <- merged[y_it == 1 & I_replace == 0, .N]
       n_replace       <- merged[y_it == 1 & I_replace == 1, .N]
       n_partial       <- merged[any_closure == 1 & facility_complete_closure == 0, .N]
       n_partial_maint <- merged[any_closure == 1 & facility_complete_closure == 0
                                 & y_it == 0, .N]

     Print: "Partial closures in the estimation sample: N_partial rows,
             of which N_partial_maint (pct%) are correctly Maintain (y_it=0)"

     Save: Output/Tables/T011_A1_ActionDistribution.csv
     Cols: category, N, pct
     Rows: Maintain / Exit / Replace / PartialClosure_inMaintain /
           CompleteExit_noFutureInstall / CompleteExit_withFutureInstall
           (last = is_retrofit==1 AND facility_complete_closure==1 — the
            "double-triggered" case where retrofit wins)

2.3  TABLE A2 — Overlap: double-triggered cases
     How many rows have BOTH facility_complete_closure == 1 AND
     replacement_closure_year == 1 (i.e., full closure + future install)?
     These map to Replace (retrofit wins). Print count and pct of closures.

     Cross-tab:
       merged[y_it==1, .N, by=.(facility_complete_closure, replacement_closure_year)]
     Save: Output/Tables/T011_A2_ClosureTypeOverlap.csv
     Cols: facility_complete_closure, replacement_closure_year, N, pct_of_closures

2.4  TABLE A3 — Partial closure breakdown by facility size
     partial_dt <- merged[any_closure == 1 & facility_complete_closure == 0]
     Use the size_bin already carried from fp (Section 1.5, BOY stock) — do
     NOT recompute from active_tanks. Drop rows with size_bin == "unknown"
     from the stratified table and log their count.
     Tabulate: N, pct_of_all_obs, pct_of_closures by size_bin.
     Save: Output/Tables/T011_A3_PartialClosure_by_Size.csv
     Cols: size_bin, n_partial, n_obs_in_size, n_closures_in_size,
           pct_of_obs_that_are_partial, pct_of_closures_that_are_partial

2.5  TABLE A4 — Replace definition concordance: broad vs specific vs DCM
     This is the key audit table. Three versions of "replace" exist in the
     pipeline. Show all three side-by-side and their overlap.

     Definitions:
       broad_replace   = replacement_closure_year == 1L in facility_panel
                         (02b S12.4: any future install after this closure)
       sw2dw_replace   = single_to_double_year == 1L in facility_panel
                         (02b S12.4: SW closed AND DW installed same year)
       dcm_replace     = I_replace == 1L in dcm_obs_panel (04b: inherits broad)

     Step A: In the MERGED estimation-sample rows (from Step 2.1), compute:
       n_broad   = merged[replacement_closure_year == 1L, .N]
       n_sw2dw   = merged[single_to_double_year == 1L, .N]   # need col from fp
       n_dcm_r   = merged[I_replace == 1L, .N]

     NOTE: single_to_double_year must be added to the KEEP_FP column list
     in Section 1.4 (add "single_to_double_year" to the fread select vector).

     Step B: Among rows where I_replace == 1L (DCM Replace events), break down:
       n_is_sw2dw         = rows where single_to_double_year == 1L
       n_broad_not_sw2dw  = rows where replacement_closure_year==1 & single_to_double_year==0
       n_dcm_ne_broad     = rows where I_replace==1 but replacement_closure_year!=1
                            (should be ZERO — this is a coding error if nonzero)

     Step C: Identity check — 04b's is_retrofit should equal 02b's replacement_closure_year
     for every row in the merged table. Hard-assert:
       stopifnot(all(merged$I_replace[merged$y_it==1] ==
                     merged$replacement_closure_year[merged$y_it==1], na.rm=TRUE))
     If this fails: print which rows disagree and their counts — this is a bug.

     Save: Output/Tables/T011_A4_ReplaceDefinition_Concordance.csv
     Rows: 6 (enumerated exactly):
       {1: DCM_Replace_total,
        2: DCM_Replace_is_SW2DW,
        3: DCM_Replace_broad_not_SW2DW,
        4: broad_replace_NOT_in_DCM_replace   (should be 0 if identity holds),
        5: SW2DW_NOT_in_DCM_replace            (should be 0 if single_to_double ⊆ broad),
        6: Identity_check_discrepancies        (should be 0 — flag if nonzero)}
     Cols: category, N, pct_of_dcm_replacements, pct_of_all_closures, note

     Print to log in plain English:
       "Replace in DCM = broad definition (any future install after closure).
        X% of DCM Replace events are specifically SW→DW upgrades.
        Y% are broad-only (future install, but NOT same-year SW→DW).
        Identity check: 04b is_retrofit == 02b replacement_closure_year for
        all N rows — [PASS / FAIL: N discrepancies]."

2.6  TABLE A5 — Replacement margin validity: timing, tank-count change,
     and capacity change

     This is the core test of whether "Replace" is a real behavioral margin.
     Restrict to facility-years where replacement_closure_year == 1 in the
     facility_panel (NOT restricted to the estimation sample — use all years
     with n_closures > 0 and replacement_closure_year == 1).

     NOTE: two dimensions of size change must both be reported — tank COUNT
     and total CAPACITY (gallons). Capacity is the true operating footprint:
     closing 2 small SW tanks and installing 1 large DW tank may be net-neutral
     on throughput even though tank count dropped by 1.

     ── Step A: Compute years-to-next-install for each replace event ──
     For each (panel_id, panel_year) where replacement_closure_year == 1:
       years_to_next_install = min(future_panel_year where n_installs > 0)
                               - panel_year
     If no future install exists in the panel: years_to_next_install = NA
     (These NAs should be rare because replacement_closure_year = 1 requires
     a future install to exist. If NAs > 0, flag them: the install year is
     beyond the panel end-date, or data quality issue.)

     Compute via self-join on panel_id, panel_year < future panel_year,
     n_installs > 0:
       fp_inst <- fp[n_installs > 0L, .(panel_id, panel_year)]
       setnames(fp_inst, "panel_year", "install_yr")
       replace_events <- fp[replacement_closure_year == 1L,
                            .(panel_id, panel_year)]
       gaps <- merge(replace_events, fp_inst, by = "panel_id", allow.cartesian = TRUE)
       gaps <- gaps[install_yr >= panel_year]
       gaps[, gap := install_yr - panel_year]
       timing <- gaps[, .(years_to_next = min(gap)), by = .(panel_id, panel_year)]

     Bucket years_to_next into:
       {0, 1, 2-3, 4-5, 6-10, 11+, NA}
     Save: timing distribution as part of Table A5.

     ── Step B: Net tank-count change at replace events ──
     CRITICAL (data structure): facility_panel has a row for a panel_id ONLY
     in years with >=1 active tank (fac_stock is aggregated by panel_id,
     panel_year over active tanks; 02b S12.1). Rows are NOT a complete year
     grid — gaps occur EXACTLY in the lagged-install replacement case (full
     closure -> zero-tank years have no row -> the row reappears at the next
     install year). Therefore DO NOT use a row-offset shift(); use calendar-
     year-keyed lookups (self-join on (panel_id, year)).

     For each replace event (panel_id, event_year = panel_year):
       t_before = event_year - 1
       t_after  = event_year + years_to_next   (next-install year from Step A;
                  for a same-year install years_to_next = 0 -> t_after = event_year)
       n_before = n_tanks_eoy at (panel_id, t_before)  via join; NA if no row
       n_after  = n_tanks_eoy at (panel_id, t_after)   via join; NA if no row
       delta_tanks = n_after - n_before
     n_tanks_eoy is an end-of-year stock (02b line 985), so at a same-year
     close+install t_after row it already nets the closure and the new install.
     If years_to_next is NA (no future install) OR either neighbor row is
     missing (left/right censored): delta_tanks = NA -> bucket "missing".
     Report the count of missing-neighbor events to the log.

     Bucket delta_tanks:
       {>=+2 (expanded 2+), +1 (expanded 1), 0 (flat), -1 (shrunk 1), <=-2 (shrunk 2+)}

     ── Step C: Net capacity change at replace events ──
     Same calendar-year-keyed anchors (t_before, t_after) as Step B, but on
     END-OF-YEAR capacity. Use total_capacity_dec, NOT total_capacity:
     total_capacity (02b line 915) counts tanks present at ANY point in the
     year, so at a same-year close+install year it double-counts the closed-
     and-replaced tanks. total_capacity_dec = total_capacity - capacity_closed
     (02b line 995) is the consistent EOY stock. If total_capacity_dec is
     absent from the CSV header, fall back to total_capacity and LOG the
     same-year double-count caveat (do not silently substitute).
       cap_before = total_capacity_dec at (panel_id, t_before)  via join
       cap_after  = total_capacity_dec at (panel_id, t_after)   via join
       delta_cap     = cap_after - cap_before
       delta_cap_pct = 100 * delta_cap / pmax(cap_before, 1)
     NA handling identical to Step B.

     Bucket delta_cap_pct:
       {>+10% (expanded), -10% to +10% (roughly flat), -25% to -10% (modest shrink),
        <-25% (substantial shrink)}

     Key insight: cases where delta_tanks < 0 but delta_cap_pct ≈ 0 are the
     "2 small SW → 1 large DW" capacity-neutral upgrades. These ARE economically
     meaningful replacements even though tank count dropped. Cases where BOTH
     delta_tanks < 0 AND delta_cap_pct < -10% are true facility shrinkage events
     that got miscoded as Replace.

     ── Step D: SW-replacement install timing (simultaneity of SW upgrades) ──
     NOTE (spec correction): single_to_double_year == 1 already REQUIRES a
     same-year DW install (02b line 1109: n_sw_replacement>0 & n_dw_installs>0
     in the SAME panel_year), so restricting to it makes every event "same
     year" by construction and the lagged buckets are vacuous. To get a non-
     degenerate timing distribution, use the broader SW-replacement set built
     from the kept columns:
       P_SWR = facility-years with n_sw_closures > 0 AND replacement_closure_year == 1
               (an SW tank closed AND some future install exists at the facility).
     For each row in P_SWR compute years_to_next (Step A logic) and bucket:
       same year      = years_to_next == 0   (simultaneous close+install; this
                        subset ~ single_to_double_year==1 — report BOTH the
                        P_SWR same-year count and the single_to_double_year==1
                        count to the log for cross-check)
       next year      = years_to_next == 1
       2+ years later = years_to_next >= 2
     This shows what fraction of SW-closure replacements are a simultaneous
     SW->DW decision vs. a lagged install.

     Save: Output/Tables/T011_A5_ReplacementMarginValidity.csv
     Structure: ONE table with FOUR sub-blocks, separated by a "section" column.
     Cols: section, category, N, pct_of_replace_events, note

     Rows — SECTION = "Timing (years to next install)":
       {0, 1, 2-3, 4-5, 6-10, 11+, NA}

     Rows — SECTION = "Net tank count change (t-1 to t+1)":
       {+2 or more, +1, 0, -1, -2 or more}

     Rows — SECTION = "Net capacity change pct (t-1 to t+1)":
       {expanded >10pct, flat -10 to +10pct, modest shrink -25 to -10pct,
        substantial shrink below -25pct, missing capacity data}

     Rows — SECTION = "SW-replacement install timing (SW closure + future install)":
       {same year, next year, 2 or more years later}

     Total: approximately 20 rows.

     Print to log:
       "Replacement margin validity:
        - Median years to next install after Replace event: X
        - Share with same-year install (most credible replacements): X%
        - Share with 6+ year gap (likely unrelated to closure): X%
        - Net tank count: X% flat/expanded, X% shrunk by 1+
        - Net capacity: X% flat/expanded, X% shrunk >10%
        - Capacity-neutral but tank-count-negative (2 SW → 1 DW type): X%
        - True facility shrinkage (both count AND capacity down): X%"

2.6b TABLE A6 + FIGURE A6 — Distribution of size change at Replace events
     (researcher request: see the full distribution of facility capacity
     change, not just the bucket counts of A5).
     Inputs: the per-event table from Steps B/C above — one row per replace
     event carrying delta_tanks and delta_cap_pct, restricted to events with
     non-missing before AND after stock (the same rows that populate the A5
     tank-count and capacity sub-blocks).

     (i) TABLE Output/Tables/T011_A6_CapacityChange_Distribution.csv
         Cols: metric, bin, n_events, pct_of_events
         metric in {"delta_tanks","delta_cap_pct"} (two metrics stacked).
         delta_tanks bins: integer values clipped to [-5,+5] with "<=-5" and
           ">=+5" end bins (so 11 labelled bins per metric block).
         delta_cap_pct bins: 10-pp-wide bins over [-100,+100] with "<=-100" and
           ">=+100" end bins. pct_of_events within each metric block sums to 100.

     (ii) FIGURE Output/Figures/T011_A6_CapacityChange_Distribution.png
          width=10 height=6 dpi=150. TWO vertically stacked panels:
            TOP    = histogram of delta_tanks (integer bins, clipped [-5,5])
            BOTTOM = histogram of delta_cap_pct (clipped to [-100,100] for
                     readability; report the clipped-tail count in the subtitle)
          Reference vline at 0 on both panels; additional vlines at -10 and +10
          on the capacity panel (the A5 "roughly flat" band).
          Title:    "Distribution of facility size change at Replace events"
          Subtitle: "Tank count (top) and total capacity %% (bottom); n = <N>
                     replace events with non-missing before/after stock.
                     Mass at/above 0 on capacity = consolidation/upgrade,
                     not facility shrinkage."

2.6c TABLE A7 + FIGURE A7 — JOINT tank-count vs capacity change (bin scatter)
     (researcher request: the A6 marginals do not show the COVARIANCE between
     tank-count change and capacity change, which is the economically important
     object. A negative delta_tanks paired with delta_cap_pct ~ 0 is
     consolidation (fewer, larger tanks); a negative delta_tanks paired with a
     strongly negative delta_cap_pct is true facility shrinkage.)
     Inputs: same per-event table from Steps B/C (delta_tanks, delta_cap_pct,
     non-missing before AND after stock).

     (i) TABLE Output/Tables/T011_A7_TankVsCapacity_Joint.csv
         Bin by delta_tanks clipped to [-5,+5]: labelled bins
           {"<=-5","-4","-3","-2","-1","0","+1","+2","+3","+4",">=+5"} (11 bins).
         Per bin compute over its events:
           n_events, pct_of_events,
           mean_cap_pct, median_cap_pct, p25_cap_pct, p75_cap_pct,
           share_cap_preserving = mean(delta_cap_pct >= -10) * 100.
         Cols: delta_tanks_bin, n_events, pct_of_events, mean_cap_pct,
               median_cap_pct, p25_cap_pct, p75_cap_pct, share_cap_preserving
         ALSO compute and LOG the Pearson correlation
           r_tc = cor(delta_tanks, delta_cap_pct) over all events
         as a scalar covariance summary (report to the log, not the CSV).

     (ii) FIGURE Output/Figures/T011_A7_TankVsCapacity_BinScatter.png
          width=9 height=6 dpi=150. Bin scatter:
            x = delta_tanks_bin (ordered factor, -5..+5 left-to-right)
            y = mean_cap_pct (point), point size proportional to n_events
                (legend "Events per bin")
            vertical p25..p75 band per bin (geom_linerange or geom_errorbar)
          Reference: hline at y=0 (capacity-flat) and y=-10 (A5 shrink
          threshold, dashed); vline at the x="0" bin (tank-count-flat).
          Title:    "Tank-count change vs. capacity change at Replace events"
          Subtitle: "Bin scatter: mean (point) and p25-p75 (band) capacity %%
                     within each tank-count-change bin; point size = events.
                     Near y=0 at negative x = consolidation (fewer, larger
                     tanks); well below 0 at negative x = true facility
                     shrinkage. cor(dtanks, dcap%%) = <r_tc>."

2.7  Print summary statements to log for each table.

─────────────────────────────────────────────────
SECTION 3 — TRUE DATA CCPs BY STATE CELL (Task B)
─────────────────────────────────────────────────
Goal: raw empirical P(action | state cell) for all 32 cells, suitable
for comparing to model-implied CCPs and for identification documentation.

3.1  Compute cell-level counts from obs panel (drop rows with NA premium):
       cell_dt <- obs[!is.na(premium), .(
         n_obs     = .N,
         n_maint   = sum(y_it == 0L),
         n_close   = sum(y_it == 1L),
         n_exit    = sum(y_it == 1L & !is.na(I_replace) & I_replace == 0L),
         n_replace = sum(y_it == 1L & !is.na(I_replace) & I_replace == 1L)
       ), by = s_idx]
       cell_dt <- merge(state_lut[, .(s_idx, A_bin, w_state, rho_state)],
                        cell_dt, by="s_idx", all.x=TRUE)
       cell_dt[is.na(n_obs), `:=`(n_obs=0, n_maint=0, n_close=0, n_exit=0, n_replace=0)]

3.2  Compute raw CCPs with sanity-floor (do NOT smooth — show raw data):
       cell_dt[, `:=`(
         P_M = n_maint   / pmax(n_obs, 1L),
         P_E = n_exit    / pmax(n_obs, 1L),
         P_R = n_replace / pmax(n_obs, 1L),
         P_close = n_close / pmax(n_obs, 1L)
       )]
     Assert: all(abs(cell_dt$P_M + cell_dt$P_E + cell_dt$P_R - 1) < 1e-8)
             (for cells with n_obs > 0; skip n_obs==0 cells)

3.3  Save TABLE B1: Output/Tables/T011_B1_EmpiricalCCPs_by_Cell.csv
     32 rows × cols: s_idx, A_bin, w_state, rho_state, n_obs, n_maint,
                     n_exit, n_replace, P_M, P_E, P_R, P_close, cell_pct
     where cell_pct = 100 * n_obs / sum(n_obs) (share of estimation sample).
     Print to log: overall P_E, P_R, P_M across all cells (weighted by n_obs).

3.4  FIGURE B1 — Empirical CCPs: SW panel (rho_state = 1 FF and 2 RB, w_state = 1)
     Long-format melt on (P_M, P_E, P_R), x = A_bin, y = P,
     facet_wrap(~action, scales="free_y"), color = factor(rho_state),
     geom_line + geom_point(aes(size=cell_pct)), size legend showing
     "Cell share (% of est. obs)", breaks c(0.5,2,5,10,20).
     scale_x_continuous(breaks=1:8, labels=c("0-5","5-10","10-15","15-20",
                                              "20-25","25-30","30-35","35+"))
     Title: "Empirical CCPs — Single-Walled, by Regime and Age Bin"
     Subtitle: "Raw data shares; no smoothing. Dot area = cell share of estimation sample."
     Save: Output/Figures/T011_B1_EmpiricalCCPs_SW.png  width=10 height=6 dpi=150

3.5  FIGURE B2 — Same for DW panel (w_state = 2):
     Save: Output/Figures/T011_B2_EmpiricalCCPs_DW.png

3.6  FIGURE B3 — Replace-share-among-closures:
     cell_dt[, P_R_cond := n_replace / pmax(n_close, 1L)]
     x = A_bin, y = P_R_cond, color = factor(w_state), linetype = factor(rho_state),
     title = "P(Replace | Closure) by Age, Wall, and Regime"
     subtitle = "Identification of K: higher conditional replace share = larger K relative to kappa"
     Save: Output/Figures/T011_B3_ReplaceShareAmongClosures.png  width=8 height=5 dpi=150

─────────────────────────────────────────────────
SECTION 4 — CCP VARIATION BY FACILITY SIZE (Task C)
─────────────────────────────────────────────────
Goal: show whether choice probabilities vary across facility portfolio
size WITHIN the same state cell. If yes, size belongs in the state space.
If no, the 32-cell spec is adequate.

4.1  Add size_bin to obs panel via left-join from fp (use the BOY-stock
     size_bin from Section 1.5 — do NOT recompute from active_tanks):
       obs_s <- merge(obs[!is.na(premium)],
                      fp[, .(panel_id, panel_year, size_bin,
                             any_closure, facility_complete_closure)],
                      by = c("panel_id","panel_year"), all.x = TRUE)
       obs_s[is.na(size_bin), size_bin := "unknown"]   # obs rows not in fp
     Log: share of rows with size_bin == "unknown". NOTE: under BOY stock this
     may be modestly higher than the old <1% (EOY) figure because left-censored
     first-appearance years have no prior-year stock; report the actual share.

4.2  TABLE C1 — CCP by (size_bin, A_bin, w_state, rho_state):
       size_cell_dt <- obs_s[, .(
         n_obs     = .N,
         P_M       = mean(y_it == 0L),
         P_E       = mean(y_it == 1L & (is.na(I_replace) | I_replace == 0L)),
         P_R       = mean(y_it == 1L & !is.na(I_replace) & I_replace == 1L),
         P_close   = mean(y_it == 1L)
       ), by = .(size_bin, s_idx, A_bin, w_state, rho_state)]
     Merge state_lut labels to ensure all s_idx values are labelled.
     Save: Output/Tables/T011_C1_CCPs_by_SizeBin_Cell.csv
     Cols: size_bin, s_idx, A_bin, w_state, rho_state, n_obs, P_M, P_E, P_R, P_close

4.3  FIGURE C1 — Exit rate by size bin × age × regime (SW only for readability):
     From size_cell_dt filtered to w_state==1:
     x = A_bin, y = P_E, color = size_bin, facet = factor(rho_state),
     geom_line + geom_point(aes(size=n_obs)), size legend hidden.
     Title: "P(Exit) by Facility Size, Age, and Regime — SW"
     subtitle = "If lines coincide by size, size does not belong in DCM state space"
     Save: Output/Figures/T011_C1_ExitRate_by_Size_SW.png  width=10 height=5 dpi=150

4.4  FIGURE C2 — Same for P(Replace):
     Save: Output/Figures/T011_C2_ReplaceRate_by_Size_SW.png

4.5  Partial closure rate by (size_bin, regime, TX vs ctrl) from facility_panel.
     This is independent of the DCM estimation sample — uses all facility_panel rows.
     size_bin is already on fp from Section 1.5 (BOY stock) — do NOT recompute.
     Exclude size_bin == "unknown" rows from the stratified table (log count).
     fp[, regime := fcase(
       texas_treated == 1L & panel_year >= 1999L, "TX_RB",
       texas_treated == 1L & panel_year <  1999L, "TX_FF",
       texas_treated == 0L,                       "Control_FF",
       default = NA_character_)]
     partial_rate <- fp[!is.na(size_bin) & !is.na(regime) & n_closures > 0,
       .(
         n_closure_events   = .N,
         n_partial          = sum(any_closure==1 & facility_complete_closure==0),
         n_full_exit        = sum(facility_complete_closure==1 & replacement_closure_year==0),
         n_full_replace     = sum(facility_complete_closure==1 & replacement_closure_year==1),
         pct_partial        = mean(any_closure==1 & facility_complete_closure==0) * 100,
         pct_full_exit      = mean(facility_complete_closure==1 & replacement_closure_year==0) * 100,
         pct_full_replace   = mean(facility_complete_closure==1 & replacement_closure_year==1) * 100
       ), by = .(size_bin, regime)]
     Save: Output/Tables/T011_C2_PartialClosure_by_Size_Regime.csv
     Cols: size_bin, regime, n_closure_events, n_partial, n_full_exit, n_full_replace,
           pct_partial, pct_full_exit, pct_full_replace
     Rows: 4 size_bins × 4 regimes = up to 16 rows (some combinations sparse)

4.6  FIGURE C3 — Stacked bar: partial + full-exit + full-replace shares,
     by size_bin × regime (only rows with n_closure_events >= 30):
     Long-format melt on (pct_partial, pct_full_exit, pct_full_replace),
     x = size_bin, y = pct, fill = action_type, facet = regime
     title = "Closure composition by facility size and regime"
     subtitle = "Among facility-years with ≥1 closure event"
     Save: Output/Figures/T011_C3_ClosureComposition_by_Size.png  width=10 height=6 dpi=150

─────────────────────────────────────────────────
SECTION 5 — PRINT RESEARCHER SUMMARY
─────────────────────────────────────────────────
Print to log (and cat to console) a plain-English summary:

  cat("\n=== RESEARCHER SUMMARY ===\n")
  cat("TASK A — Action coding:\n")
  # Partial closure count and pct
  # Double-triggered count (retrofit wins over full exit)
  # Replace definition: X% of DCM Replace = SW→DW; Y% = broad-only
  # Identity check result (PASS/FAIL)
  cat("TASK B — True data CCPs:\n")
  # Overall P_M, P_E, P_R (weighted)
  # Most common cell for Exit (highest P_E)
  # Most common cell for Replace (highest P_R)
  cat("TASK C — Size heterogeneity:\n")
  # Max difference in P_E across size bins within any single s_idx
  # Max difference in P_R across size bins within any single s_idx
  # Whether partial closure rate differs by >5pp across size bins
  cat("=========================\n")

─────────────────────────────────────────────────
SECTION 6 — APPEND TO Identifying_Variation_Size_Capacity.qmd
─────────────────────────────────────────────────
Goal: embed all T011 outputs into the existing qmd as a new top-level
section with ZERO prose. Section headers only, then the chunk. No
explanatory sentences inside any subsection body.

File to EDIT (append only — do NOT modify any existing content above
line 1182): Reports/Paper/Identifying_Variation_Size_Capacity.qmd

Append the following content verbatim at the end of the file (after
the last line of "## For paper exposition"):

─── BEGIN APPEND ───────────────────────────────────────────────────────

CAPTION/NOTE CONVENTION (corrected post-attempt-2; the chunk bodies below
show the OLD kable(caption=) form and are SUPERSEDED — match the rendered qmd,
not this block, for caption mechanics). Econ-journal (AER/QJE/JPE/Econometrica)
style, as used by tbl-size-action and 01_LUST_Insurance_Draft.qmd:
  - SHORT descriptive title in the chunk option (#| tbl-cap: / #| fig-cap:),
    with a tbl-/fig- prefixed #| label: for cross-referencing.
  - NO caption= argument inside knitr::kable().
  - One DETAILED \begin{flushleft}\footnotesize \textit{Notes:} ... \end{flushleft}
    block after the chunk (the title is a label; the note carries the detail).
Do NOT put the descriptive paragraph inside kable(caption=) — that produces a
paragraph-as-title plus a duplicate note.

# T011 Diagnostics: action coding, empirical CCPs, and size variation {#sec-t011}

## Action coding audit {#sec-t011-action}

### Action distribution: 02b flags vs 04b y\_it {#sec-t011-a1}

```{r t011-a1, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Tables","T011_A1_ActionDistribution.csv")
if (file.exists(.f)) {
  dt <- data.table::fread(.f)
  knitr::kable(dt, format="latex", booktabs=TRUE,
    caption="Action distribution: 02b raw flags mapped to 04b DCM actions. Partial closures (any\\_closure=1, facility\\_complete\\_closure=0) are correctly classified as Maintain (y\\_it=0).",
    col.names=c("Category","N","Pct (pct)")) |>
    kableExtra::kable_styling(font_size=9, latex_options="hold_position")
} else { cat("*[T011 not yet run — Output/Tables/T011\\_A1\\_ActionDistribution.csv missing]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Six categories computed on the merged estimation sample; the common denominator is all sample rows, so the categories overlap by design (a partial closure is also a Maintain). Partial closures (any\_closure $=1$, facility\_complete\_closure $=0$) are classified Maintain in the DCM. CompleteExit\_withFutureInstall is the double-triggered case in which a full closure coincides with a future install, which the model codes as Replace (retrofit wins).
\end{flushleft}

### Closure type overlap: full-exit x replacement flag {#sec-t011-a2}

```{r t011-a2, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Tables","T011_A2_ClosureTypeOverlap.csv")
if (file.exists(.f)) {
  dt <- data.table::fread(.f)
  knitr::kable(dt, format="latex", booktabs=TRUE,
    caption="Cross-tab of facility\\_complete\\_closure x replacement\\_closure\\_year among y\\_it=1 rows. Double-triggered rows (both=1) map to Replace (retrofit wins).",
    col.names=c("Complete closure","Replacement flag","N","Pct of closures")) |>
    kableExtra::kable_styling(font_size=9, latex_options="hold_position")
} else { cat("*[T011 not yet run — T011\\_A2 missing]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Cross-tabulation among closure rows (y\_it $=1$). Pct of closures uses total estimation-sample closures as the denominator. Rows with facility\_complete\_closure $=1$ and replacement\_closure\_year $=1$ are the double-triggered events mapped to Replace.
\end{flushleft}

### Replacement margin validity: timing, tank count, and capacity change {#sec-t011-a5}

```{r t011-a5, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Tables","T011_A5_ReplacementMarginValidity.csv")
if (file.exists(.f)) {
  dt <- data.table::fread(.f)
  knitr::kable(dt, format="latex", booktabs=TRUE, longtable=TRUE,
    caption="Replacement margin validity. Four sub-blocks: (1) years between closure and next install; (2) net change in tank COUNT from year before to year after the replace event; (3) net change in total CAPACITY (gallons); (4) SW-replacement install timing. Both count and capacity matter: a firm closing 2 small SW tanks and installing 1 large DW tank shrinks on tank count but may be flat or expanding on capacity.",
    col.names=c("Section","Category","N","Pct of Replace events","Note")) |>
    kableExtra::kable_styling(font_size=9, latex_options=c("hold_position","repeat_header"))
} else { cat("*[T011 not yet run — T011\\_A5 missing]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Replace events are facility-years with replacement\_closure\_year $=1$ in the full facility panel. Before/after stock is anchored by calendar year (t$-1$ and the next-install year) via a gap-safe join. The tank-count and capacity sub-blocks exclude events with a missing before/after neighbor row. SW-replacement timing uses the broader set of facility-years with an SW closure and a future install; the same-year SW$\rightarrow$DW count (single\_to\_double\_year $=1$) is reported in the note column for cross-check.
\end{flushleft}

### Replace definition: broad vs SW-to-DW vs DCM coding {#sec-t011-a4}

```{r t011-a4, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Tables","T011_A4_ReplaceDefinition_Concordance.csv")
if (file.exists(.f)) {
  dt <- data.table::fread(.f)
  knitr::kable(dt, format="latex", booktabs=TRUE,
    caption="Replace action definition concordance. Three definitions: (1) broad = any future install after closure (02b replacement\\_closure\\_year = 04b is\\_retrofit); (2) specific = SW closed and DW installed same year (02b single\\_to\\_double\\_year); (3) DCM = broad. Row 6 = identity check discrepancies between 02b and 04b — should be zero.",
    col.names=c("Category","N","Pct of DCM Replace","Pct of all closures","Note")) |>
    kableExtra::kable_styling(font_size=9, latex_options="hold_position")
} else { cat("*[T011 not yet run — T011\\_A4 missing]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Three replace definitions exist in the pipeline: broad (replacement\_closure\_year, any future install; equals 04b is\_retrofit), SW$\rightarrow$DW (single\_to\_double\_year, an SW tank closed and a DW tank installed the same year), and DCM (equals broad). Rows 4--6 are validity checks that should all equal zero: SW$\rightarrow$DW is a subset of broad, and 04b is\_retrofit reproduces 02b replacement\_closure\_year exactly.
\end{flushleft}

### Partial closure breakdown by facility size {#sec-t011-a3}

```{r t011-a3, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Tables","T011_A3_PartialClosure_by_Size.csv")
if (file.exists(.f)) {
  dt <- data.table::fread(.f)
  knitr::kable(dt, format="latex", booktabs=TRUE,
    caption="Partial-closure events (any\\_closure=1, facility\\_complete\\_closure=0) in the estimation sample by facility size bin (beginning-of-year stock). Partial closures are always Maintain in the DCM.",
    col.names=c("Size bin","N partial","N obs (size)","N closures (size)",
                "Pct obs partial","Pct closures partial")) |>
    kableExtra::kable_styling(font_size=9, latex_options="hold_position")
} else { cat("*[T011 not yet run — T011\\_A3 missing]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Partial-closure events (any\_closure $=1$, facility\_complete\_closure $=0$) in the estimation sample, binned by beginning-of-year facility size (prior-year end-of-year tank stock; see Section 1.5). Rows with size bin ``unknown'' are excluded. Partial closures are always Maintain in the DCM.
\end{flushleft}

### Distribution of size change at Replace events {#sec-t011-a6}

```{r t011-a6, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Tables","T011_A6_CapacityChange_Distribution.csv")
if (file.exists(.f)) {
  dt <- data.table::fread(.f)
  knitr::kable(dt, format="latex", booktabs=TRUE, longtable=TRUE,
    caption="Binned distribution of facility size change at Replace events, by tank count (delta\\_tanks, integer bins) and total capacity in gallons (delta\\_cap\\_gal, wide bins with an explicit no-change bin). pct\\_of\\_events sums to 100 within each metric block. Mass at or above zero on capacity indicates consolidation or upgrade rather than facility shrinkage.",
    col.names=c("Metric","Bin","N events","Pct of events")) |>
    kableExtra::kable_styling(font_size=8, latex_options=c("hold_position","repeat_header"))
} else { cat("*[T011 not yet run — T011\\_A6 missing]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Distribution of facility size change at Replace events (replace-event population with non-missing before/after stock). Tank count is in integer bins clipped to $[-5,+5]$; capacity is in gallon bins with an explicit no-change bin, extending to $\pm 10{,}000$ gallons (about the p10--p90 range). Pct of events sums to 100 within each metric block. Capacity is reported in gallons rather than percent because tank sizes are standardized in the thousands of gallons, which makes percent change heavy-tailed.
\end{flushleft}

### Capacity-change distribution figure {#sec-t011-a6-fig}

```{r t011-a6-fig, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Figures","T011_A6_CapacityChange_Distribution.png")
if (file.exists(.f)) {
  knitr::include_graphics(.f)
} else { cat("*[T011\\_A6\\_CapacityChange\\_Distribution.png not yet generated]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Two-panel histogram of facility size change at Replace events: tank count (top, clipped to $[-5,+5]$) and total capacity in gallons (bottom, clipped to $\pm 20{,}000$; events beyond the clip are noted in the panel subtitle). Vertical line marks no change. The capacity distribution is roughly symmetric around zero, consistent with consolidation rather than systematic shrinkage.
\end{flushleft}

### Tank-count vs capacity change (joint) {#sec-t011-a7}

```{r t011-a7, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Tables","T011_A7_TankVsCapacity_Joint.csv")
if (file.exists(.f)) {
  dt <- data.table::fread(.f)
  knitr::kable(dt, format="latex", booktabs=TRUE,
    caption="Joint relationship of tank-count change and capacity change at Replace events. For each tank-count-change bin: mean/median/IQR of capacity percent change and the share that is capacity-preserving (delta\\_cap\\_pct >= -10).",
    col.names=c("Tank chg bin","N","Pct","Mean cap pct","Median cap pct",
                "P25 cap pct","P75 cap pct","Pct cap-preserving")) |>
    kableExtra::kable_styling(font_size=8, latex_options=c("hold_position","scale_down"))
} else { cat("*[T011 not yet run — T011\\_A7 missing]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Joint distribution of tank-count and capacity change at Replace events. For each tank-count-change bin: median and interquartile range of capacity percent change, and the share that is capacity-preserving (delta\_cap\_pct $\geq -10$). A negative tank-count bin with median capacity near zero is consolidation (fewer, larger tanks); a negative bin with strongly negative capacity is true shrinkage. The mean column is dominated by small-denominator outliers and is reported only for completeness; the figure plots the median.
\end{flushleft}

### Tank-count vs capacity change bin scatter {#sec-t011-a7-fig}

```{r t011-a7-fig, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Figures","T011_A7_TankVsCapacity_BinScatter.png")
if (file.exists(.f)) {
  knitr::include_graphics(.f)
} else { cat("*[T011\\_A7\\_TankVsCapacity\\_BinScatter.png not yet generated]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Bin scatter of capacity percent change against tank-count change at Replace events: median (point) and p25--p75 (band) within each tank-count-change bin; point area is the number of events. The y-axis is clipped to $[-100,+200]$ so the bands and reference lines are visible. Solid line at zero (capacity flat); dashed line at $-10$ (shrink threshold); dotted vertical line at zero tank change. Near zero capacity at negative tank change is consolidation; well below zero is true facility shrinkage.
\end{flushleft}

## Empirical CCPs by state cell {#sec-t011-ccp}

### Raw empirical CCP table — all 32 cells {#sec-t011-b1}

```{r t011-b1, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Tables","T011_B1_EmpiricalCCPs_by_Cell.csv")
if (file.exists(.f)) {
  dt <- data.table::fread(.f)
  dt[, `:=`(P_M=round(P_M,4), P_E=round(P_E,5), P_R=round(P_R,5),
             P_E_cond=round(P_E_cond,3), P_R_cond=round(P_R_cond,3),
             cell_pct=round(cell_pct,2))]
  knitr::kable(dt[, .(s_idx,age,wall,regime,n_obs,P_M,P_E,P_R,P_E_cond,P_R_cond,cell_pct)],
    format="latex", booktabs=TRUE,
    caption="Raw empirical choice probabilities by state cell (32 cells), with nominal age/wall/regime labels. P(M)+P(E)+P(R)=1 by construction; P(E$\\mid$cl) and P(R$\\mid$cl) are conditional on a closure and sum to 1. Cell pct = share of estimation sample.",
    col.names=c("s","Age","Wall","Regime","N obs","P(M)","P(E)","P(R)",
                "P(E|cl)","P(R|cl)","Cell pct")) |>
    kableExtra::kable_styling(font_size=8, latex_options=c("hold_position","scale_down"))
} else { cat("*[T011 not yet run — T011\\_B1 missing]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Raw empirical choice probabilities by state cell (age $\times$ wall $\times$ regime; 32 cells). Wall: SW = single-walled, DW = double-walled. Regime: FF = flat-fee, RB = risk-based. P(M)$+$P(E)$+$P(R)$=1$ by construction; P(E$\mid$cl) and P(R$\mid$cl) are conditional on a closure and sum to 1. Every DW closure is coded Replace, so P(R$\mid$cl)$=1$ for all DW cells. Cell pct is the cell's share of the estimation sample; sparse cells should be read with caution.
\end{flushleft}

### Empirical CCPs — single-walled panel {#sec-t011-b1-sw}

```{r t011-b1-sw, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Figures","T011_B1_EmpiricalCCPs_SW.png")
if (file.exists(.f)) {
  knitr::include_graphics(.f)
} else { cat("*[T011\\_B1\\_EmpiricalCCPs\\_SW.png not yet generated]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Single-walled facilities. Raw data shares by age bin, faceted by action (Maintain, Exit, Replace); no smoothing. Dot area is the cell's share of the estimation sample. FF = flat-fee, RB = risk-based.
\end{flushleft}

### Empirical CCPs — double-walled panel {#sec-t011-b2-dw}

```{r t011-b2-dw, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Figures","T011_B2_EmpiricalCCPs_DW.png")
if (file.exists(.f)) {
  knitr::include_graphics(.f)
} else { cat("*[T011\\_B2\\_EmpiricalCCPs\\_DW.png not yet generated]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Double-walled facilities. Raw data shares by age bin, faceted by action; no smoothing. Dot area is the cell's share of the estimation sample. Double-walled closures are coded Replace by construction, so the Exit panel is empty. FF = flat-fee, RB = risk-based.
\end{flushleft}

### P(Replace | Closure) by age, wall, and regime {#sec-t011-b3}

```{r t011-b3, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Figures","T011_B3_ReplaceShareAmongClosures.png")
if (file.exists(.f)) {
  knitr::include_graphics(.f)
} else { cat("*[T011\\_B3\\_ReplaceShareAmongClosures.png not yet generated]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Conditional replace share among closures, faceted by pricing regime (FF, RB) and colored by wall type. Double-walled closures are coded Replace by construction (P $=1.0$ in every cell), so the single-walled series carries all the identifying variation. Point area is the number of closures in the cell.
\end{flushleft}

## CCP variation by facility size {#sec-t011-size}

### CCPs by (size bin x state cell) {#sec-t011-c1}

```{r t011-c1, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Tables","T011_C1_CCPs_by_SizeBin_Cell.csv")
if (file.exists(.f)) {
  dt <- data.table::fread(.f)
  dt[, `:=`(P_M=round(P_M,4), P_E=round(P_E,5), P_R=round(P_R,5),
             P_E_cond=round(P_E_cond,3), P_R_cond=round(P_R_cond,3))]
  knitr::kable(dt[n_obs>=20, .(size_bin,age,wall,regime,n_obs,P_M,P_E,P_R,P_E_cond,P_R_cond)],
    format="latex", booktabs=TRUE, longtable=TRUE,
    caption="Empirical CCPs by (size bin x state cell), with nominal labels. Cells with fewer than 20 observations excluded. If P(E$\\mid$cl) and P(R$\\mid$cl) are roughly constant across size bins within the same (age, wall, regime) cell, size does not belong in the DCM state space.",
    col.names=c("Size","Age","Wall","Regime","N obs","P(M)","P(E)","P(R)",
                "P(E|cl)","P(R|cl)")) |>
    kableExtra::kable_styling(font_size=8, latex_options=c("hold_position","repeat_header"))
} else { cat("*[T011 not yet run — T011\\_C1 missing]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Empirical CCPs by (size bin $\times$ state cell); cells with fewer than 20 observations excluded. Facility size is the beginning-of-year tank count (Section 1.5). P(E$\mid$cl) and P(R$\mid$cl) are conditional on a closure. If the conditional shares are roughly constant across size bins within the same (age, wall, regime) cell, facility size does not belong in the DCM state space.
\end{flushleft}

### Exit rate by size x age (SW) {#sec-t011-c1-fig}

```{r t011-c1-fig, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Figures","T011_C1_ExitRate_by_Size_SW.png")
if (file.exists(.f)) {
  knitr::include_graphics(.f)
} else { cat("*[T011\\_C1\\_ExitRate\\_by\\_Size\\_SW.png not yet generated]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Exit rate by facility size, age, and regime, single-walled facilities only; cells with fewer than 20 observations dropped. Four lines correspond to the four beginning-of-year size bins. Coincident lines imply facility size does not belong in the DCM state space.
\end{flushleft}

### Replace rate by size x age (SW) {#sec-t011-c2-fig}

```{r t011-c2-fig, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Figures","T011_C2_ReplaceRate_by_Size_SW.png")
if (file.exists(.f)) {
  knitr::include_graphics(.f)
} else { cat("*[T011\\_C2\\_ReplaceRate\\_by\\_Size\\_SW.png not yet generated]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Replace rate by facility size, age, and regime, single-walled facilities only; cells with fewer than 20 observations dropped. Four lines correspond to the four beginning-of-year size bins. Coincident lines imply facility size does not belong in the DCM state space.
\end{flushleft}

### Partial closure composition by size and regime {#sec-t011-c2}

```{r t011-c2, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Tables","T011_C2_PartialClosure_by_Size_Regime.csv")
if (file.exists(.f)) {
  dt <- data.table::fread(.f)
  dt[, `:=`(pct_partial=round(pct_partial,1),
             pct_full_exit=round(pct_full_exit,1),
             pct_full_replace=round(pct_full_replace,1))]
  knitr::kable(dt,
    format="latex", booktabs=TRUE,
    caption="Closure composition among facility-years with any closure, by beginning-of-year size bin and pricing regime. Pct partial = portfolio shrinkage; pct full exit = true walk-away; pct full replace = full-facility replacement.",
    col.names=c("Size","Regime","N events","N partial","N full exit","N full replace",
                "Pct partial","Pct exit","Pct replace")) |>
    kableExtra::kable_styling(font_size=9, latex_options=c("hold_position","scale_down"))
} else { cat("*[T011 not yet run — T011\\_C2 missing]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Closure composition among facility-years with at least one closure, by beginning-of-year size bin and pricing regime, from the full facility panel (not restricted to the DCM estimation sample). Pct partial = portfolio shrinkage (some tanks closed, facility survives); pct full exit = complete walk-away with no future install; pct full replace = complete closure with a future install. The three shares sum to 100 within each row.
\end{flushleft}

### Closure composition stacked bar {#sec-t011-c3-fig}

```{r t011-c3-fig, echo=FALSE, warning=FALSE, message=FALSE}
.f <- here::here("Output","Figures","T011_C3_ClosureComposition_by_Size.png")
if (file.exists(.f)) {
  knitr::include_graphics(.f)
} else { cat("*[T011\\_C3\\_ClosureComposition\\_by\\_Size.png not yet generated]*") }
```

\vspace{0.1cm}
\begin{flushleft}
\footnotesize
\textit{Notes:} Closure composition (partial, full exit, full replace) by beginning-of-year facility size and regime, among facility-years with at least one closure. Bars are shown only for size $\times$ regime cells with at least 30 closure events. Partial closures rise steeply with facility size.
\end{flushleft}

─── END APPEND ─────────────────────────────────────────────────────────

IMPORTANT qmd editing rules:
  - Append ONLY. Do not touch any content before the final line of
    "## For paper exposition" (currently line ~1182 of the file).
  - Preserve the existing section numbering (Quarto auto-numbers based
    on #/## heading level — the appended # heading will become §6 in
    the rendered PDF automatically).
  - Every chunk label (t011-a1, t011-a2, etc.) must be unique within
    the file. Check that none of these labels already exist in the qmd
    before appending.
  - Use `here::here()` for all paths (the qmd is rendered from repo root).
  - The `data.table::` and `knitr::` and `kableExtra::` prefixes are
    required because the setup chunk loads these but sub-chunks need
    explicit namespace in case of lazy evaluation.

═══════════════════════════════════════════════════
DELIVERABLES — ENUMERATED
═══════════════════════════════════════════════════

(1) Code/Analysis/T011_ActionCoding_CCP_Diagnostics.R  (NEW)

(2) Output/Tables/T011_A1_ActionDistribution.csv
    Rows: 6 (Maintain / Exit / Replace / PartialClosure_inMaintain /
              CompleteExit_noFutureInstall / CompleteExit_withFutureInstall)
    Cols: category, N, pct

(3) Output/Tables/T011_A2_ClosureTypeOverlap.csv
    Rows: up to 4 (2x2 cross-tab of facility_complete_closure × replacement_closure_year)
    Cols: facility_complete_closure, replacement_closure_year, N, pct_of_closures

(4) Output/Tables/T011_A3_PartialClosure_by_Size.csv
    Rows: 4 (size_bins: 1, 2, 3, 4+)
    Cols: size_bin, n_partial, n_obs_in_size, n_closures_in_size,
          pct_of_obs_that_are_partial, pct_of_closures_that_are_partial

(5) Output/Tables/T011_A4_ReplaceDefinition_Concordance.csv
    Rows: 6 (enumerated in Step 2.5)
    Cols: category, N, pct_of_dcm_replacements, pct_of_all_closures, note

(6) Output/Tables/T011_A5_ReplacementMarginValidity.csv
    Rows: ~20 (4 sub-blocks: timing 7 rows, tank-count 5 rows, capacity 5 rows,
               SW-replacement timing 3 rows)
    Cols: section, category, N, pct_of_replace_events, note

(7) Output/Tables/T011_A6_CapacityChange_Distribution.csv
    Rows: 2 metric blocks (delta_tanks ~11 bins + delta_cap_pct ~22 bins)
    Cols: metric, bin, n_events, pct_of_events

(8) Output/Figures/T011_A6_CapacityChange_Distribution.png
    2-panel histogram (delta_tanks top, delta_cap_pct bottom)

(9) Output/Tables/T011_A7_TankVsCapacity_Joint.csv
    Rows: 11 (delta_tanks bins <=-5 .. >=+5)
    Cols: delta_tanks_bin, n_events, pct_of_events, mean_cap_pct, median_cap_pct,
          p25_cap_pct, p75_cap_pct, share_cap_preserving

(10) Output/Figures/T011_A7_TankVsCapacity_BinScatter.png
     Bin scatter: mean+IQR capacity pct change per tank-count-change bin

(11) Output/Tables/T011_B1_EmpiricalCCPs_by_Cell.csv
     Rows: 32 (one per state cell)
     Cols: s_idx, A_bin, w_state, rho_state, n_obs, n_maint, n_exit, n_replace,
           P_M, P_E, P_R, P_close, cell_pct

(12) Output/Figures/T011_B1_EmpiricalCCPs_SW.png
(13) Output/Figures/T011_B2_EmpiricalCCPs_DW.png
(14) Output/Figures/T011_B3_ReplaceShareAmongClosures.png

(15) Output/Tables/T011_C1_CCPs_by_SizeBin_Cell.csv
     Rows: up to 4 size_bins × 32 cells = up to 128 rows (sparse cells omitted)
     Cols: size_bin, s_idx, A_bin, w_state, rho_state, n_obs, P_M, P_E, P_R, P_close

(16) Output/Tables/T011_C2_PartialClosure_by_Size_Regime.csv
     Rows: up to 16 (4 size_bins × 4 regimes {TX_FF, TX_RB, Control_FF})
     Cols: size_bin, regime, n_closure_events, n_partial, n_full_exit,
           n_full_replace, pct_partial, pct_full_exit, pct_full_replace

(17) Output/Figures/T011_C1_ExitRate_by_Size_SW.png
(18) Output/Figures/T011_C2_ReplaceRate_by_Size_SW.png
(19) Output/Figures/T011_C3_ClosureComposition_by_Size.png

(20) Reports/Paper/Identifying_Variation_Size_Capacity.qmd  (APPENDED)
     New content: # T011 Diagnostics section with 18 subsections, each
     containing exactly one table chunk or one figure chunk, zero prose.
     All existing content (lines 1–1182) unchanged.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════

DATA INTEGRITY:
- [ ] All 18 data outputs exist at the specified paths
      (10 tables: A1, A2, A3, A4, A5, A6, A7, B1, C1, C2;
       8 figures: A6, A7, B1, B2, B3, C1, C2, C3)
- [ ] T011_B1: 32 rows; P_M + P_E + P_R sums to 1 ± 1e-8 for every row
      with n_obs > 0
- [ ] T011_B1: cell_pct sums to 100 ± 0.1 across all 32 cells
- [ ] T011_C1: no NA in P_M, P_E, P_R for rows with n_obs > 0
- [ ] T011_A2: row counts sum to total closures in the estimation sample
- [ ] T011_A4: exactly 6 rows with the enumerated category values
- [ ] T011_A4: row "Identity_check_discrepancies" has N = 0 (PASS); if nonzero
      the script must print a warning and the coder must NOT suppress it
- [ ] T011_A4: row "SW2DW_NOT_in_DCM_replace" has N = 0 (single_to_double ⊆ broad)
- [ ] T011_A5: exactly 4 sections present in the section column
- [ ] T011_A5: timing section sums to total replace events ± NA rows
- [ ] T011_A5: tank-count and capacity sections sum to replace events with
      non-missing before/after (joined) stock
- [ ] T011_A5: log prints the 7-line plain-English validity summary
- [ ] T011_A6: pct_of_events sums to 100 ± 0.1 within EACH metric block
      (delta_tanks block and delta_cap_pct block separately)
- [ ] T011_A7: 11 delta_tanks bins; n_events sums to the same non-missing
      replace-event count as the A5 tank-count section; log reports
      cor(delta_tanks, delta_cap_pct)
- [ ] Size bins (A3, C1, C2) derived from BOY stock per Section 1.5, NOT
      active_tanks; log reports the n_tanks_active-fallback count and the
      size_bin == "unknown" share. Full-closure events MUST appear in size
      bins 1/2/3/4+ (not collapsed to unknown) — this is the bug from
      attempt 1 that the BOY fix resolves
- [ ] T011_C2: pct_partial + pct_full_exit + pct_full_replace sum to 100 ± 0.5
      for every row with n_closure_events >= 10

FIGURES:
- [ ] B1, B2: x-axis labels are the 8 age bin strings (not integers)
- [ ] B1, B2: three facets (one per action P_M / P_E / P_R), free y-scale
- [ ] B1, B2: dot size legend visible with label "Cell share (% of est. obs)"
- [ ] C1, C2: four lines (one per size_bin), faceted by rho_state
- [ ] C3: stacked bars summing to ~100 per group, filtered to n>=30

QMD APPEND (deliverable 20):
- [ ] Appended content begins immediately after last line of "## For paper exposition"
- [ ] No existing content modified (diff shows only additions)
- [ ] 18 subsections present under # T011 Diagnostics, one per output file
      (Task A: A1,A2,A5,A4,A3,A6-table,A6-fig,A7-table,A7-fig = 9 chunks;
       Task B: B1-table,B1,B2,B3 = 4; Task C: C1-table,C1,C2,C3 figs+C2-table
       = 5 = 18 chunks — verify all 18 are in the appended section)
- [ ] Every chunk has a unique label not present elsewhere in the file
- [ ] Every chunk body wrapped in `if (file.exists(.f)) { ... } else { cat("*[...]") }`
- [ ] No prose sentences inside any subsection body (only the kable/include_graphics call)
- [ ] qmd compiles without error via
      `quarto render Reports/Paper/Identifying_Variation_Size_Capacity.qmd`
      BOTH before T011 runs (all else-branches fire, PDF has placeholder text)
      AND after T011 runs (all if-branches fire, PDF has real tables and figures)

CODE HYGIENE:
- [ ] No tryCatch returning NULL silently
- [ ] No try(silent=TRUE)
- [ ] All paths use here::here() or data_in() / z_path()
- [ ] Logging block per CLAUDE.md present at top of script
- [ ] Script runs end-to-end in < 10 minutes (no estimation, pure data table ops)

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- facility_panel.csv is ~3.2 GB. Use fread(..., select = KEEP_FP) — do NOT
  read all columns. The KEEP_FP vector is defined in Section 1.4 above.
- data_in() is defined in Code/Helpers/data_paths.R. It returns the local
  path if the file exists locally, Z path as fallback.
- The state_lut from the primitives RDS has all 32 (s_idx, A_bin, w_state,
  rho_state) combinations — use it to ensure full 32-row output in Table B1
  even if some cells have no observations.
- For the partial closure computation in Step 4.5, use the FULL facility_panel
  (not just the estimation sample), so size bins have adequate cell counts.
  The DCM estimation sample excludes TX pre-2006 and controls pre-1999 —
  the full panel has more size-bin variation, especially in the FF period.
- If `active_tanks` is NA in some rows of facility_panel, assign those to
  "unknown" and exclude from stratified tables (log the count).
- For Figure C1/C2: filter size_cell_dt to n_obs >= 20 per (size_bin, s_idx)
  before plotting to avoid showing lines based on 1-2 observations.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════

--- Architect pre-implementation corrections (2026-06-01, before attempt 1) ---
Triggered by R1's pre-coding clarification questions. All verified against
Code/Analysis/02b_Tank_level_Panel_Build.R. These are architect-directed
spec changes, NOT coder drift:

  (1) Deliverable/chunk counts fixed: 14 data outputs (8 tables + 6 figures),
      14 qmd chunks; deliverables renumbered (1)-(16). Was inconsistently
      stated as "13 outputs / 12 chunks / 11 subsections".

  (2) §1.4 KEEP_FP: added total_capacity_dec (EOY capacity) for Step C.

  (3) §2.6 Step B (tank-count delta): replaced row-offset shift() with a
      calendar-year-keyed join. Reason: 02b S12.1 aggregates fac_stock
      by (panel_id, panel_year) over active tanks only -> no row in zero-tank
      years -> panel is NOT a complete grid, and gaps fall exactly in the
      lagged-install replacement population A5 studies. Anchors:
      t_before = event_year-1, t_after = event_year + years_to_next.

  (4) §2.6 Step C (capacity delta): use total_capacity_dec (EOY) not
      total_capacity. total_capacity (02b line 915) counts tanks present at
      ANY point in the year and double-counts closed+replaced tanks in a
      same-year close+install. Fall back to total_capacity with a logged
      caveat only if the EOY column is absent.

  (5) §2.6 Step D (SW-replacement timing): population corrected. Restricting
      to single_to_double_year==1 is degenerate — that flag (02b line 1109:
      n_sw_replacement>0 & n_dw_installs>0 same year) ALREADY requires a
      same-year DW install, so the lagged buckets would be empty. Now uses
      P_SWR = (n_sw_closures>0 & replacement_closure_year==1) and buckets its
      years_to_next {0 / 1 / >=2}; reports single_to_double_year==1 count for
      cross-check.

  (6) A1 (§2.2): common denominator = nrow(merged) for all 6 rows (categories
      overlap by design). Double-triggered row uses
      facility_complete_closure==1 & replacement_closure_year==1 (consistent
      with A2), not I_replace==1.

  (7) [SUPERSEDED by correction #9 below — was: active_tanks = EOY stock.
      That is wrong for closure events; see #9.]

  (8) Logging: skip rstudioapi under Rscript; hardcode SCRIPT_NAME.

--- Architect correction after attempt 1 run (2026-06-01) ---
R1 ran attempt 1 end-to-end (Tasks A & B valid; A4 identity check PASS,
0 discrepancies; A5 margin-validity numbers produced). Task C was degenerate
and R1 correctly stopped before the qmd append to bounce a measurement call.

  (9) SIZE MEASURE FIX (supersedes #7). active_tanks = n_tanks_eoy (EOY stock)
      is 0 at every full closure, so it collapsed all 174,119 full-exit/
      full-replace events to size_bin=NA — C2/C3 showed 100% partial, C1 exit-
      by-size was flat zero. n_tanks_active is also wrong (counts closing tanks
      + same-year install, over-sizing replacements). FIX: new Section 1.5 —
      size_bin from BEGINNING-OF-YEAR stock = prior-year EOY (n_tanks_eoy at
      panel_year-1), gap-safe join, n_tanks_active fallback only when no prior
      row. Applied to §2.4 (A3), §4.1 (C1), §4.5 (C2). KEEP_FP gains
      n_tanks_active. This is the portfolio the facility holds entering the
      decision — matches DCM state semantics.

  (10) NEW DELIVERABLE A6 (researcher request): full distribution of size
       change at Replace events — Output/Tables/T011_A6_CapacityChange_
       Distribution.csv + Output/Figures/T011_A6_CapacityChange_Distribution.png
       (2-panel histogram: delta_tanks, delta_cap_pct). Built from the per-
       event deltas already computed in §2.6 Steps B/C.

  (11) NEW DELIVERABLE A7 (researcher request): JOINT tank-count vs capacity
       change — the A6 marginals miss the covariance, which is the point.
       Output/Tables/T011_A7_TankVsCapacity_Joint.csv (binscatter table by
       delta_tanks bin: mean/median/IQR cap_pct + share cap-preserving) +
       Output/Figures/T011_A7_TankVsCapacity_BinScatter.png (mean+IQR per
       tank-count-change bin, point size = events). Log reports
       cor(delta_tanks, delta_cap_pct). See §2.6c. Final counts:
       18 data outputs (10 tables + 8 figures), 18 qmd chunks, deliverables (1)-(20).

--- Attempt 2 run + researcher-directed presentation refinements (2026-06-01) ---
Attempt 2 ran clean end-to-end (BOY size fix resolved Task C: full closures now
bin to 1/2/3/4+; A4 identity PASS, 0 disc; all 18 outputs + qmd + PDF OK).
Researcher then requested five presentation changes (architect-approved live);
spec append block (BEGIN/END APPEND) updated to match the qmd, and the script
edited accordingly:

  (12) A6 capacity metric switched from delta_cap_pct (10-pp bins, mass piled in
       the >=+100 end bin) to ABSOLUTE delta_cap in GALLONS — wide bins with an
       explicit "0 (no change)" bin out to +/-10,000 gal (~p10-p90). Tank sizes
       are standardized in the thousands of gallons, so percent change is heavy-
       tailed; gallons is bounded and interpretable. A6 table metric label is now
       "delta_cap_gal"; figure bottom panel is a gallon histogram (clipped +/-20k).

  (13) A7 figure: plotted central tendency switched mean -> MEDIAN with p25-p75
       band and coord_cartesian(ylim=c(-100,200)). The mean was outlier-wrecked
       (small-denominator delta_cap_pct -> bin means to +60,000%), collapsing the
       bands. Table A7 still carries mean+median+IQR+share_cap_preserving; cor
       unchanged (0.0205).

  (14) B1 + C1 CCP tables: added nominal labels (age "0-5".."35+", wall SW/DW,
       regime FF/RB) and conditional columns P(E|Close), P(R|Close) = n_exit or
       n_replace over n_close. These are the identification objects. s_idx/A_bin/
       w_state/rho_state retained in the CSV for reference.

  (15) B3 figure: re-faceted by regime (FF | RB), colored by wall. Prior single-
       panel design hid RB because DW P(R|Closure)=1.000 in EVERY cell for BOTH
       regimes (every DW closure is coded Replace; zero DW exits anywhere), so
       DW-FF and DW-RB lines coincided at y=1.0 and SW lines were squished. NOTE
       for identification: K is NOT identified off the DW conditional-replace
       share (pinned at 1.0); SW cells carry all P(R|Closure) variation.

  (16) Table/figure NOTES: each of the 18 qmd subsections now carries a paper-
       style \footnotesize \textit{Notes:} block (raw LaTeX after the chunk,
       matching 01_LUST_Insurance_Draft.qmd). This SUPERSEDES the original
       "zero prose in subsection bodies" rule for the T011 section.

  Note: enumerated DELIVERABLES/ACCEPTANCE column lists above still describe the
  attempt-1 schema (pre-#14 columns, pre-#12 metric name). The authoritative
  output schema is the attempt-2 script + the updated APPEND block. cor and the
  A4 identity check are unchanged. PDF re-rendered clean (18/18 chunks).

[Reviewer fills below after each coder attempt.]

### Attempt 2 — 2026-06-01 (Reviewer: Opus)
Transcript: skipped (W6 — host transcript only; reviewed script + run log + qmd directly).
Run log: logs/T011_ActionCoding_CCP_Diagnostics_20260601_114909.log

Result: PASS (analytics + all 18 data outputs) / PSEUDOCODE_FAIL (qmd caption convention only)

Analytics checklist — all PASS (verified against the run log):
- ✓ A4 identity: I_replace == replacement_closure_year for all 44,602 closures (0 disc).
- ✓ A4: SW2DW_NOT_in_DCM = 0; broad_NOT_in_DCM = 0.
- ✓ B1 simplex sums to 1; weighted P_M=0.9805 P_E=0.0168 P_R=0.0027.
- ✓ A6 both metric blocks sum to 100.00%.
- ✓ A7 n=25,983 = A5 tank-count non-missing (26,737 − 754).
- ✓ BOY size fix (corr #9) resolves the attempt-1 collapse: full closures now bin
  to 1/2/3/4+ (A3, C2 populated); fallback 5.4%, unknown 0.04–0.05%.
- ✓ Size signal: P_E spread 0.057, P_R spread 0.082, partial-rate spread 52pp.
- ✓ All 18 data outputs present.
Accepted deviations (defensible; researcher blessed the attempt-2 schema in #12–#15):
A6 capacity in gallons not pct; A7 figure plots median not mean; B3 wall-color/regime-facet;
extra label cols in B1/C1. Minor caveat: cor(dtanks,dcap%)=0.0205 is on heavy-tailed pct,
near-uninterpretable — the A7 median trend (−1→0%, −3→−27%) is the real signal.

qmd presentation — FAIL (root cause = spec, not coder drift):
- The APPEND block authored in this spec used knitr::kable(caption="<long paragraph>").
  R1 faithfully reproduced that, then added a LUST-style \textit{Notes:} block (#16),
  so every one of the 18 subsections now carries a paragraph-as-title PLUS a near-
  duplicate note. House convention (01_LUST_Insurance_Draft.qmd line 895; this qmd's
  own tbl-size-action line 261) is: SHORT caption via #| tbl-cap: / #| fig-cap:, no
  caption= in kable(), and a single concise \textit{Notes:} block.

Resolution: PRESENTATION-POLISH (no R1 attempt 3). The fix is pure caption/label
mechanics on a downstream qmd consumer — eligible for the CLAUDE.md polish exception.
Opus to (a) rewrite the 18 T011 chunks to #| tbl-cap:/#| fig-cap: short captions +
drop kable(caption=) + trim each Notes block to remove caption duplication, and
(b) update this spec's APPEND block to the same convention so it does not recur.
Pending researcher waiver of the architect role for that turn.
