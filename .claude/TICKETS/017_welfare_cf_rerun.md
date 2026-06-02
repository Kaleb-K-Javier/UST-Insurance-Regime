# TICKET 017 — Welfare CF re-run on the canonical BOY + gammafree fit
# Created: 2026-06-01
# Status: CLOSED-PASS (reviewer PASS attempt 1, 2026-06-02)
# Attempt: 1
# Assignee: R1
# Gate: re-solves CF equilibria + welfare (estimation logic) — goes through R1.
# Depends on: Ticket 013 (BOY panel/primitives) AND Ticket 016 (gammafree fit) both landed.

# ═══════════════════════════════════════════════════
# ATTEMPT LOG
# ═══════════════════════════════════════════════════
# Attempt 1 (2026-06-02) — PASS.
#   Implementation: Code/Dynamic_Model/04o_T017_Welfare_Rerun.R (new driver,
#     STEP 0/1/3/4/5 + Rscript-subprocess re-run of the engine) + one in-place
#     edit to 04o_CF_TX_FlatFee.R:82-83 (fit_path -> ...clean_observed_gammafree.rds).
#   Reviewer (Opus, acting reviewer) verdict: PASS. Independently confirmed:
#     STEP 0 guard — gammas read by NAME, box from fit$config$*_bounds (not
#       hardcoded), interior tol=1e-3*(hi-lo)=0.04; gp=-2.16323 / gr=0.11369
#       interior; converged=TRUE LL=-227239.784; guard runs BEFORE any file move.
#     STEP 1 — 5 pre-BOY files MOVEd to _pre_BOY/; hard stop() on missing source.
#     STEP 2 — engine exit 0; welfare identity re-derived from BOY CSV holds
#       EXACTLY (297002-3412-0=293590; 306215-10156-0=296058).
#     STEP 3 — T017_Welfare_PreBOY_vs_BOY.csv: 8 rows / 6 cols / correct types;
#       GovtOutlay pct_change = NA (preBOY 0); E_external_USD correctly ignored;
#       arithmetic re-checked (SW $50k +4711 = 107.5%; PS +4755 = 106.7%).
#     STEP 4 — C5 full 3-col fit$P_hat[17:32,] vs re-solve P_baseline[17:32,]:
#       max disc 2.558e-06 < 1e-4.
#     STEP 5 — caveat note written (F_replace / Ticket-014).
#     No forbidden changes (E_GRID/beta/premium/Semantic-2/tolerance/F_replace/
#       Bellman/CCP untouched; no re-estimation).
#   Benign deviation (reviewer-blessed): engine run as Rscript subprocess (system2)
#     instead of source() — better sink/log isolation; STEP 4 reads P_baseline from
#     CF_TX_FlatFee_results.rds$P_baseline (persisted fallback). Faithful.
#   Result: headline $50k SocialWelfare CF delta $4,381 -> $9,092 (+108%), driven by
#     ProducerSurplus/premium->maintain channel. Replace-margin caveat carried.
#   Non-blocking note: 04o_CF_TX_FlatFee.R was untracked in git -> committed this
#     session alongside the driver + T017 tables so the in-place edit is auditable.

═══════════════════════════════════════════════════
MOTIVATION
═══════════════════════════════════════════════════
The BOY fix (013) cleaned the wall mis-celling and moved the structural params
(K_DW +34%, gamma_price -1.11 -> -2.19); 016 frees the gamma boxes. gamma_price
feeds the premium/Pigouvian channel directly, so the FF<->RB welfare numbers WILL
move. Re-run the TX-FlatFee counterfactual on the canonical fit and quantify how
much the headline welfare changed.

CAVEAT SCOPE (state in outputs, do not silently bury):
 - The HEADLINE FF<->RB welfare runs through the premium -> maintain channel
   (gamma_price). That channel is CLEAN post-BOY.
 - The REPLACE-margin welfare (retrofit response / govt-replacement channel)
   carries the F_replace single-tank-reset caveat (T013 D1 replace off-support
   98%) -> deferred to Ticket 014. Footnote it; do not present replace-margin
   welfare as fully validated.

═══════════════════════════════════════════════════
ENVIRONMENT
═══════════════════════════════════════════════════
Rscript: C:\Program Files\R\R-4.5.2\bin\Rscript.exe
Script: Code/Dynamic_Model/04o_CF_TX_FlatFee.R (re-point the fit input; otherwise
unchanged). Everything else UNCHANGED: E_GRID (50k headline, 17k sensitivity),
beta, the FF-premium construction from control 2006+, Semantic-2 (alphas dropped
in the re-solve), mu = TX empirical state distribution.

═══════════════════════════════════════════════════
STEP 0 — SELECT THE CANONICAL FIT (do not guess)
═══════════════════════════════════════════════════
Canonical fit = the 016 gammafree fit IF 016 promoted it (both gammas interior,
converged):
  Output/Estimation_Results/Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds
If 016 did NOT promote (a gamma pinned / box-shaped, flagged for architect review):
  STOP. Do not run the welfare CF on an unresolved fit — report and wait.

═══════════════════════════════════════════════════
STEP 1 — ARCHIVE THE PRE-BOY CF OUTPUTS (preserve the old welfare numbers)
═══════════════════════════════════════════════════
Before re-running, MOVE (not copy) the existing CF outputs to _pre_BOY/:
  Output/Estimation_Results/CF_TX_FlatFee_results.rds
  Output/Tables/04o_CF_Welfare_Summary.csv  (+ .tex)
  Output/Tables/04o_CF_ActionShares_byAge.csv
  Output/Tables/04o_CF_RemovalAge_Distribution.csv
into Output/Estimation_Results/_pre_BOY/ and Output/Tables/_pre_BOY/ respectively.
The archived 04o_CF_Welfare_Summary.csv is the pre-BOY source for Step 3.

═══════════════════════════════════════════════════
STEP 2 — RE-POINT THE FIT AND RE-RUN
═══════════════════════════════════════════════════
In 04o_CF_TX_FlatFee.R, change fit_path (Section 4) to the canonical gammafree
fit from Step 0. Re-run the script end-to-end. It regenerates (BOY + gammafree):
  CF_TX_FlatFee_results.rds, 04o_CF_Welfare_Summary.csv/.tex,
  04o_CF_ActionShares_byAge.csv/.png, 04o_CF_RemovalAge_Distribution.csv/.png,
  the Section-13 fit figures.
Acceptance: baseline AND cf equilibria converged; all P rows sum to 1; the welfare
identity (Social = Producer - External - Govt) holds within $1 (the script already
asserts this).

═══════════════════════════════════════════════════
STEP 3 — DELIVERABLE: pre-BOY vs BOY welfare comparison
═══════════════════════════════════════════════════
Output/Tables/T017_Welfare_PreBOY_vs_BOY.csv
  Read the archived _pre_BOY/04o_CF_Welfare_Summary.csv (pre-BOY) and the new
  04o_CF_Welfare_Summary.csv (BOY-gammafree). Both have component, E_label,
  baseline_USD, cf_USD, delta_USD.
  Rows: 8 — every (component x E_label):
    component in {ProducerSurplus_USD, ExternalDamage_USD, GovtOutlay_USD,
                  SocialWelfare_USD}
    E_label   in {HEALTH_ONLY, HEALTH_PLUS_UNMEASURED}
  Cols (exact, types):
    component       chr
    E_label         chr
    preBOY_delta    num   (CF - baseline, pre-BOY)
    BOY_delta       num   (CF - baseline, BOY-gammafree)
    abs_change      num   (BOY_delta - preBOY_delta)
    pct_change      num   (100 * abs_change / abs(preBOY_delta); NA if preBOY ~0)
  Print the SocialWelfare row for the headline $50k E scenario both ways and a
  one-line verdict on how much the BOY+gamma correction moved the headline
  welfare effect.

═══════════════════════════════════════════════════
STEP 4 — TX-LEVEL VALIDATION ON THE NEW FIT
═══════════════════════════════════════════════════
Re-run the C5 check (T012 / the appendix validation): compare the CF baseline
re-solve P_baseline to the canonical fit's $P_hat on TX cells (s_idx 17-32).
Acceptance: max|fit - resolve| < 1e-4 (confirms TX is correctly leveled in the CF
for the new fit). Report the max discrepancy.

═══════════════════════════════════════════════════
STEP 5 — DOCUMENT THE CAVEAT
═══════════════════════════════════════════════════
Print to the log (and a one-line note in the welfare summary's accompanying log):
  "Headline FF<->RB welfare (premium->maintain channel) is post-BOY clean.
   Replace-margin welfare carries the F_replace single-tank-reset caveat
   (replace off-support 98%, Ticket 014)."

WHAT NOT TO DO: do not change E_GRID, beta, the premium construction, Semantic-2,
or any estimator setting; do not re-run 04b/04o estimation; do not touch
F_replace or the Bellman/CCP code.
