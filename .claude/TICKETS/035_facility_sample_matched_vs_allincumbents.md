# TICKET 035 — Facility portfolio DiD: matched birth-CEM vs ALL incumbents (sample robustness)
# Created: 2026-06-26
# Status: IMPLEMENTED_AWAITING_RUN
# Attempt: 1
# Type: REDUCED-FORM ROBUSTNESS (add a second sample + a comparison block to 02j; no change to
#       the existing matched headline outputs). DATA CODER seat (run_coder_pro_api.ps1).
#       Runs on SERVER (needs matched_facs_birth_cem.csv, which is server-only). Edit locally,
#       commit+push; researcher pulls on server and runs the loop.

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
The facility portfolio DiD (02j) currently runs on the birth-CEM MATCHED facility sample
(matched_facs_birth_cem.csv, ~3.44M facility-years). Question: are the ATTs and the size/vintage
sorting robust to instead using ALL active-at-reform incumbents (~4.82M facility-years), letting
the portfolio-mix x year fixed effect (cell_fac_year) carry comparability instead of CEM matching?
Run BOTH and compare side by side, so the researcher can decide which sample is the headline.

═══════════════════════════════════════════════════
DESIGN (locked) — three configs, to separate SAMPLE from the Yhat0 control
═══════════════════════════════════════════════════
02k builds Yhat0 over the MATCHED tank panel only, so the all-incumbents sample cannot carry the
same Yhat0 control without rebuilding the crosswalk. To keep the comparison apples-to-apples, run:

  C1  matched_full     matched_facs_birth_cem | FE=panel_id+cell_fac_year | RHS=Yhat0+mandates
                       == the CURRENT headline. Outputs UNCHANGED (reference).
  C2  matched_feonly   matched_facs_birth_cem | FE=panel_id+cell_fac_year | RHS=mandates only (NO Yhat0)
  C3  allinc_feonly    ALL incumbents          | FE=panel_id+cell_fac_year | RHS=mandates only (NO Yhat0)

Comparison logic:
  C2 vs C3  = the SAMPLE effect (identical FE-only spec; matched vs all incumbents). ** the answer **
  C1 vs C2  = the Yhat0-control effect (same matched sample, with/without Yhat0).
All three use the SAME cell_fac_year FE, SAME margin definitions, SAME cluster (~state).

ALL-INCUMBENTS sample (C3) definition: from Data/Analysis/facility_panel.csv (LOCAL/on server),
  keep fac_is_incumbent==1L | n_tanks_at_reform>0L  (already restricted to the 18 STUDY_STATES;
  texas_treated present). This is exactly the alive-at-reform incumbent frame used by
  T_Baseline_Characteristics_Slide.R (252,670 facilities / ~4.82M facility-years).

═══════════════════════════════════════════════════
KEY INVARIANTS (do not deviate)
═══════════════════════════════════════════════════
I1 MARGINS IDENTICAL across configs: build the SAME outcomes on every sample using the Ticket-031
   construction — closure_share, facility_exit, downsize, consolidate, reconfigure_up (rel-5% via
   capacity_change/Clag; Clag=total_capacity_dec - capacity_change; REL_THRESH=0.05), repl_share,
   cap_decrease. facility_panel.csv HAS all needed cols (capacity_change, total_capacity_dec,
   net_tank_change, n_closures, n_tanks_active, n_closures_permanent/_replacement, facility_exit,
   make_model_fac, fac_vintage, has_gasoline, n_tanks_at_reform, mandates). REFACTOR the existing
   Step-2 margin block into a function build_margins(fy) and CALL it on both samples — do not
   re-derive by hand.
I2 FE IDENTICAL: cell_fac_year := .GRP by (make_model_fac, panel_year), built PER sample (so the
   all-incumbents FE spans all incumbents). FE = panel_id + cell_fac_year for ALL three configs.
   Cluster ~state. did_term = texas_treated*post, post=1{panel_year>=1999}.
I3 C1 UNCHANGED: the existing matched run (FE+Yhat0) and its current outputs (Pub.tex, byMargin
   CSVs, ES figs) stay exactly as they are. The comparison is ADD-ON; do not alter C1 outputs.
I4 NO Yhat0 IN C2/C3: drop Yhat0 from RHS for the FE-only configs (do NOT left-join it and then let
   feols drop NA rows — that would silently re-restrict C3 toward matched coverage). RHS = mandates
   only for C2/C3.
I5 SAME HTE DIMS where available: size (cap_G on total_capacity_reform), vintage (fac_vintage),
   fuel (has_gasoline), GIS (gis_hte_vars.csv by panel_id) — run on each sample; GIS covers ~93%
   of panel_ids so it works for all incumbents too.
I6 NO tolerance/definition/FE changes beyond adding the sample+spec switch. No tryCatch->NULL.

═══════════════════════════════════════════════════
PSEUDOCODE (edits to Code/Analysis/02j_Facility_Portfolio_DiD.R)
═══════════════════════════════════════════════════
Step 1 — Refactor (no behavior change for C1):
  - Wrap the Step-2 margin construction (closure_share ... cap_decrease, incl. the Ticket-031
    partition) into build_margins(fy) returning fy with all outcome columns + cell_fac_year + did_term.
  - Wrap the ATT fit into att_table(fy, rhs) returning a data.table over a fixed margin vector
    MARG_ALL = c("closure_share","facility_exit","downsize","consolidate","reconfigure_up",
    "repl_share","cap_decrease) with cols (margin, beta, se, p, n), FE = panel_id+cell_fac_year,
    cluster ~state, rhs = either "+ Yhat0 + <mandates>" or "+ <mandates>".
  - Re-express the existing C1 run through these functions so current outputs are byte-identical.

Step 2 — Build the three config data sets:
  - fy_matched : current matched panel (already loaded) -> build_margins(); join Yhat0 (as today).
  - fy_allinc  : fread facility_panel.csv; filter incumbents (I1); select the needed cols;
                 build_margins(); build cell_fac_year on THIS sample. (No Yhat0.)
  Print n facility-years + n facilities for each, and the contraction-partition shares for each.

Step 3 — Run + stack ATT comparison:
  cmp <- rbind(
    att_table(fy_matched, rhs_full   )[, `:=`(config="matched_full",   sample="matched", spec="FE+Yhat0")],
    att_table(fy_matched, rhs_feonly )[, `:=`(config="matched_feonly", sample="matched", spec="FE-only")],
    att_table(fy_allinc,  rhs_feonly )[, `:=`(config="allinc_feonly",  sample="allinc",  spec="FE-only")])
  fwrite(cmp, Output/Tables/T_Facility_SampleCompare_ATT.csv)
  Columns EXACTLY: margin, config, sample, spec, beta, se, p, n.

Step 4 — Size/vintage HTE comparison (matched_feonly vs allinc_feonly only — the sample test):
  For cap_G and fac_vintage, emit long-format coefs (same schema as T_Facility_SizeHTE_byMargin.csv:
  margin, dimension, level, is_reference, estimate, std_error, p_value) with an added `config`
  column, for C2 and C3. Files: T_Facility_SampleCompare_SizeHTE.csv,
  T_Facility_SampleCompare_VintageHTE.csv.

Step 5 — Event studies on the all-incumbents sample (FE-only) for the key margins, to compare with
  the existing matched ES figures: closure_share, downsize, consolidate ->
  Fig_ES_Facility_{Portfolio,Downsize,Consolidate}_allinc.{pdf,png}. Same es_one() styling; same FE.

Step 6 — Print a compact console summary: for each margin, the three betas (matched_full /
  matched_feonly / allinc_feonly) + p, so the run log shows the comparison at a glance.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- facility_panel.csv is ~5M rows; fread the needed columns only. It is on the server clone.
- build cell_fac_year per sample (do NOT reuse the matched one for allinc).
- gis_hte_vars.csv / edges join by panel_id exactly as the current code does.
- data.table; feols(fixest); cluster=~state; here()/OUTPUT_TABLES paths; snake_case.
- Logging per CLAUDE.md (>1 min). Hard errors surface.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════
- [ ] C1 (matched_full) outputs byte-identical to the current 02j (refactor is behavior-preserving).
- [ ] T_Facility_SampleCompare_ATT.csv exists; columns EXACTLY margin,config,sample,spec,beta,se,p,n;
      3 configs x 7 margins = 21 rows; all finite.
- [ ] allinc sample = facility_panel.csv filtered to incumbents (fac_is_incumbent==1 | n_tanks_at_reform>0);
      printed n facilities ~252,670 and n facility-years ~4.82M (Full); matches the baseline table frame.
- [ ] C2/C3 RHS = mandates only (NO Yhat0); C1 RHS = Yhat0+mandates. FE = panel_id+cell_fac_year for all.
- [ ] Margins identical across samples (built by the SAME build_margins()); contraction shares printed per sample.
- [ ] Size/vintage compare CSVs written with a `config` column (matched_feonly + allinc_feonly).
- [ ] Fig_ES_Facility_{Portfolio,Downsize,Consolidate}_allinc.* produced.
- [ ] No tolerance/definition changes; no tryCatch->NULL; log written. 02j re-runs end-to-end on server.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
Attempt 1 (2026-06-26, Sonnet 4.6):
  - Clarifications resolved (Q1-Q6) before writing.
  - build_margins(fy): detects cap cols internally; builds post/did_term/rel_year +
    all partition outcomes + cell_fac_year in-place.
  - att_table(fy_in, rhs): loops MARG_ALL, returns (margin,beta,se,p,n).
  - es_one(): refactored with data/rhs/fe defaults so C1 calls unchanged.
  - cat_hte_param(): parameterized HTE for Step 9 compare; cat_hte() left intact for C1.
  - Steps 7-11 appended; C1 pipeline (Steps 1-6) structurally unchanged.
  - Parse: OK. Awaiting server run.
