# TICKET 031 — Repartition facility downsize into {downsize, consolidate, reconfigure-up} + machine-readable size/vintage CSVs
# Created: 2026-06-26  (rescoped from "add CSVs" after researcher redefinition)
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0
# Type: REDUCED-FORM OUTCOME REDEFINITION (measurement change, not estimation core).
#       DATA CODER seat (run_coder_pro_api.ps1), NOT the structural builder.
#       02j-LOCAL edit. Runs on SERVER (ucbare2, renv, no Z paths). Edit locally,
#       commit+push; researcher git-pulls on server and runs the loop.

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
The DCM scoping shows the dominant facility actions are CONSOLIDATE (drop tanks while
holding total capacity ~constant — a viability / modernization move) and DOWNSIZE (drop
tanks AND lose capacity — genuine contraction). True 1-for-1 replacement is rare. The
current `downsize` flag is tank-count-only (`n_closures>0 & not exit`) and silently lumps
these together (and lumps in replacements). This ticket repartitions the contraction event
by NET CAPACITY so the portfolio sorting story separates "viable, capacity-preserving"
from "contracting," matching the structural action set.

═══════════════════════════════════════════════════
MATH / CONSTRUCT (locked with researcher 2026-06-26)
═══════════════════════════════════════════════════
Per facility-year (panel_id, panel_year), among NON-EXITED facilities that lost tank count:
  Let  dN  = net_tank_change           (tanks_eoy - tanks_eoy_lag; <0 means fewer tanks)
       dC  = capacity_change           (gallons; capacity_eoy - capacity_eoy_lag)
       Clag = last year's total capacity (gallons)
       rel = dC / Clag                 (relative capacity change; guard Clag>0)
       REL_THRESH = 0.05               (5% band; first pass — a single named constant)
Gate (all three require): facility_exit==0  &  dN < 0  &  Clag > 0  &  !is.na(dC)
  consolidate   := |rel| <= REL_THRESH         (fewer tanks, ~same gallons)
  downsize       := rel  < -REL_THRESH          (fewer tanks AND fewer gallons) [REPLACES old def]
  reconfigure_up := rel  >  +REL_THRESH         (fewer tanks but MORE gallons — rare upgrade flag)
These three are MUTUALLY EXCLUSIVE and partition {exit==0 & dN<0 & Clag>0 & dC observed}.
Facility-years with dN>=0 (replacement / expansion / no change) are in NONE of the three
(0/0/0) — replacement stays measured by the existing `repl_share`; do not add a new
replacement flag in this ticket.

IDENTIFICATION is unchanged: each new flag is an LPM outcome in the SAME static DiD and
event study as the existing margins — FE = panel_id + cell_fac_year (portfolio mix x year),
control = Yhat0 + mandates, cluster by state. Do NOT change FE/RHS/cluster.

Clag reconstruction: 02b defines capacity_change = capacity_eoy - capacity_eoy_lag, so
  Clag = capacity_eoy_now - dC,
where capacity_eoy_now is the facility's END-OF-YEAR total capacity for the row. Use a
time-varying total-capacity column from the CSV (NOT total_capacity_reform, which is fixed).

═══════════════════════════════════════════════════
KEY INVARIANTS (do not deviate)
═══════════════════════════════════════════════════
I1 SOURCE COLUMNS: add to 02j's `keep` (Step 1 intersect, ~L70-74): "capacity_change" AND a
   time-varying end-of-year total-capacity column. Probe names IN THIS PRIORITY and use the
   first present: capacity_change  -> fallbacks {"capacity_change_year"}; and for the level:
   {"total_capacity_dec","total_capacity"}. ASSERT at least one capacity-change name AND one
   capacity-level name resolved. If neither resolves, STOP (hard error) with this message:
   "capacity_change / total_capacity_dec absent from matched_facs_birth_cem.csv — carry them
    through facility_panel in 02b (built at 02b:1135 / 02b:995) and rebuild the matched panel."
   (net_tank_change is already read at L72 — keep it.)
I2 REPLACE, DON'T DUAL-DEFINE: delete the old `downsize` line (02j:86) and define the new
   `downsize` per the construct above. `cap_decrease` (any-drop boolean, 02j:87) STAYS as-is
   for continuity.
I3 FE UNCHANGED: new margins use the existing FE ("panel_id + cell_fac_year") and RHS
   ("+ Yhat0 + <mandates>") via the existing FE/RHS variables. Do NOT introduce panel_year-only
   FE for these (Route B imputation is the only panel_year spec and is not in scope here).
I4 REUSE FITTED MODELS for the CSVs: the size/vintage CSVs serialize the EXISTING mS/mV lists
   (Steps 5/5b) via the SAME coeftable->data.table->grepl idiom already in Step 6 (the ES
   block). Do NOT refit for the CSV.
I5 MUTUAL EXCLUSIVITY ASSERT: stopifnot consolidate+downsize+reconfigure_up <= 1 per row
   (no double-counting), and all three are 0 wherever facility_exit==1 or dN>=0 or is.na(dC).
I6 NA/ENTRANT HANDLING: where Clag<=0 or dC is NA (entrants, first obs year, missing), all
   three flags = 0L (not NA) — they are "not a contraction event."
I7 ADD-ONLY ELSEWHERE: do not touch the tank scripts, 02k, Yhat0, the mandate set, the
   cluster var, or the .tex emission beyond adding the new margins to the existing label/loop
   vectors.

═══════════════════════════════════════════════════
PSEUDOCODE (edits to Code/Analysis/02j_Facility_Portfolio_DiD.R)
═══════════════════════════════════════════════════
Step 1 — Read the new source columns (Step 1 LOAD, ~L70):
  Add the resolved capacity_change + total-capacity-level names to `keep`. Resolve actual
  names from the header (.hdr) per I1; assert; else hard-stop per I1 message.

Step 2 — Build the partition (in the margin-construction block, ~L83-87):
  REL_THRESH <- 0.05    # named constant near CAP_BREAKS (L64) for easy flip to 0.10
  setorder(fy, panel_id, panel_year)
  dC   := <resolved capacity_change col>
  Ceoy := <resolved total-capacity level col>
  fy[, Clag := Ceoy - dC]
  fy[, rel_cap := fifelse(Clag > 0, dC / Clag, NA_real_)]
  contraction := (facility_exit == 0L & net_tank_change < 0L & Clag > 0 & !is.na(dC))
  fy[, consolidate    := as.integer(contraction & abs(rel_cap) <= REL_THRESH)]
  fy[, downsize       := as.integer(contraction & rel_cap <  -REL_THRESH)]   # REPLACES old L86
  fy[, reconfigure_up := as.integer(contraction & rel_cap >   REL_THRESH)]
  fy[is.na(consolidate),    consolidate := 0L]
  fy[is.na(downsize),       downsize := 0L]
  fy[is.na(reconfigure_up), reconfigure_up := 0L]
  assert (I5): max(consolidate+downsize+reconfigure_up) <= 1; all three == 0 where exit==1 | dN>=0 | is.na(dC).
  cat one line: share of facility-years in each of the three + n eligible (contraction).

Step 3 — Labels: add to ATT_HEAD (02j:171-172):
  downsize="Downsize (fewer tanks & gallons)", consolidate="Consolidate (fewer tanks, ~same gal)",
  reconfigure_up="Reconfigure-up (fewer tanks, +gal)".

Step 4 — Add new margins to every margin vector (existing loops, do not restructure):
  - outs (ATT, 02j:152): add "consolidate","reconfigure_up" (downsize already listed).
  - MARG (fuel/spatial HTE, 02j:184): add "consolidate" (keep downsize).
  - SZM (size+vintage HTE, 02j:215): add "consolidate","reconfigure_up" (downsize already there).
  The existing loops then fit ATT + HTE + size + vintage for the new margins automatically,
  on the unchanged FE/RHS.

Step 5 — Event studies (Step 6, 02j:270-274): add ES for the two MAIN contraction margins:
  es_one("consolidate", "Effect on facility consolidation", "Fig_ES_Facility_Consolidate")
  (downsize ES already present -> Fig_ES_Facility_Downsize). reconfigure_up: ES ONLY if it has
  >= 200 treated post events (coder checks n and prints; skip ES otherwise — too sparse — but
  still include it in static + CSVs).

Step 6 — Machine-readable size/vintage CSVs (the original 031 deliverable; AFTER mS / inside
  the mV guard). Define cat_hte_coefs(model_list, dimension_label) using the Step-6 coeftable
  idiom; emit:
    T_Facility_SizeHTE_byMargin.csv     <- cat_hte_coefs(mS, "cap_G")
    T_Facility_VintageHTE_byMargin.csv  <- cat_hte_coefs(mV, "vintage")  # inside vintage guard
  Long format, columns EXACTLY in order:
    margin (chr), dimension (chr), level (chr), is_reference (lgl),
    estimate (num), std_error (num), p_value (num)
  Per margin model m: one is_reference=TRUE row = the `did_term` coefficient (level =
  CAP_LABS[1] for size / VREF for vintage); one row per i(fvar,did_term)::<lvl>:did_term
  interaction (parse <lvl> like the Step-6 "rel_year::" parsing; is_reference=FALSE).
  Since SZM now includes the new margins, these CSVs cover consolidate/downsize/reconfigure_up
  by construction. cat the row count of each.

Step 7 — Re-run 02j on the server (loop below). Confirm: both CSVs present; new ES figure(s)
  present; the three-flag shares printed and summing sanely; existing outputs intact. Report
  the printed shares + a 3-line head of each new CSV.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- data.table only; snake_case; here()/OUTPUT_TABLES paths; fwrite. No tryCatch->NULL, no
  try(silent) — let a missing-column mismatch surface (I1 hard-stop is the intended error).
- i() term string is fixest's "<fvar>::<level>:did_term"; print head(ct$term) for one model
  and parse defensively (do not hardcode levels). Mirror Step 6's tstrsplit/sub.
- REL_THRESH is the ONLY tuning knob; leave it a named 0.05 constant (researcher may flip to 0.10).
- Do NOT modify mS/mV/.tex/FE/RHS/cluster or the tank scripts. Logging per CLAUDE.md (>1 min).

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════
- [ ] capacity_change + a time-varying total-capacity column resolved from the CSV header and
      added to `keep`; hard-stop (not silent) if absent, with the I1 message.
- [ ] Old tank-count-only `downsize` (02j:86) removed; new gallons-based `downsize` defined.
      `cap_decrease` unchanged.
- [ ] consolidate / downsize / reconfigure_up are 0/1 ints, mutually exclusive (max sum <=1),
      all 0 where facility_exit==1 | net_tank_change>=0 | is.na(capacity_change) | Clag<=0.
- [ ] REL_THRESH == 0.05 named constant; relative test is dC/Clag, Clag reconstructed as
      Ceoy - dC from a time-varying capacity level (NOT total_capacity_reform).
- [ ] All three new margins appear in ATT (outs); consolidate+downsize in fuel/spatial HTE;
      consolidate/downsize/reconfigure_up in size + vintage HTE — all on FE=panel_id+cell_fac_year.
- [ ] ES produced for downsize (existing) + consolidate (new Fig_ES_Facility_Consolidate);
      reconfigure_up ES only if >=200 post events (else printed-skip, still in static+CSV).
- [ ] Two CSVs written with columns EXACTLY: margin,dimension,level,is_reference,estimate,
      std_error,p_value; dimension cap_G / vintage; one is_reference row per margin; finite values.
- [ ] 02j re-runs end-to-end on server; three-flag shares + CSV heads reported; existing
      .tex/figure outputs intact; no tolerance/constant other than REL_THRESH introduced.
- [ ] No tryCatch->NULL; log written.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════

### Attempt 1 — 2026-06-26
Transcript: skipped (local build; translation review only)
Result: PASS

Criteria:
- [✓] C1: capacity_change + total_capacity_dec resolved from header; hard-stop with exact I1 message.
- [✓] C2: Old L86 tank-count-only downsize removed; new gallons-based downsize defined; cap_decrease unchanged.
- [✓] C3: 0/1 ints; stopifnot(max sum ≤1) + exit/dN/isNA(dC) asserts. Clag≤0 exclusion covered implicitly by contraction gate (benign).
- [✓] C4: REL_THRESH=0.05 named constant; rel = dC/Clag; Clag = Ceoy - dC from time-varying col, not total_capacity_reform.
- [✓] C5: consolidate+reconfigure_up in ATT; consolidate in MARG (downsize already there); all three in SZM and mV.
- [✓] C6: Downsize ES present; consolidate ES added (Fig_ES_Facility_Consolidate); reconfigure_up gated ≥200. Minor: no explicit skip-cat branch when count<200; count is printed before the gate so it is log-visible. Not a TRANSLATION_FAIL.
- [✓] C7: cat_hte_coefs emits columns exactly margin,dimension,level,is_reference,estimate,std_error,p_value; one ref row per margin (asserted); finite-values stopifnot.
- [ ] C8: RUNTIME — server run pending.
- [✓] C9: No tryCatch→NULL in diff. Log: not verifiable from diff (runtime).

Boundary-partition spot-check: at rel_cap=±0.05, abs(0.05)<=REL_THRESH → consolidate=1, strict inequalities keep downsize/reconfigure_up=0. No gap, no overlap. PASS.
Clag-sign spot-check: Ceoy=total_capacity_dec (time-varying, current year); dC=capacity_change; Clag=Ceoy-dC recovers last year's level per 02b definition. PASS.
cat_hte_coefs ref-row: term=="did_term" matches the +did_term main effect; double sub() strips ^cap_G:: and :did_term$. stopifnot(nrow==1) guards at runtime. PASS.
