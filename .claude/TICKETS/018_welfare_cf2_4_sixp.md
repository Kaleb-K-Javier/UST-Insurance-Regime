# TICKET 018 — CF2/CF3/CF4 welfare on the canonical 6p gammafree fit + welfare bar chart
# Created: 2026-06-02
# Status: AWAITING_RESEARCHER_APPROVAL (architect drafted; not yet handed to R1)
# Attempt: 0
# Assignee: R1
# Gate: re-solves CF equilibria + welfare (estimation logic) — goes through R1.
# Depends on: Ticket 016 (gammafree canonical fit) + Ticket 017 (04o CF1 machinery) — both landed.

═══════════════════════════════════════════════════
MOTIVATION
═══════════════════════════════════════════════════
T017 re-ran ONLY CF1 (TX flat-fee) on the canonical gammafree fit. The talk deck
still advertises four counterfactuals (CF1 flat-fee, CF2 subsidy, CF3 Pigouvian,
CF4 age-mandate) and a welfare bar chart, but CF2–CF4 exist only for the retired
4-parameter fit (04j_4p_Welfare.R) and no 6p welfare bar chart exists. This ticket
ports CF2/CF3/CF4 onto the 04o 6p machinery (canonical fit, TX-empirical mu,
Semantic-2, E_GRID {17k,50k}) and produces the CF1–CF4 welfare summary + bar chart.

CAVEAT SCOPE (state in outputs, do NOT bury — carry from T017/Ticket 014):
 - CF3 (Pigouvian) acts through the MAINTAIN channel (adds -h*E to u^M). That
   channel is post-BOY CLEAN. CF1 likewise clean.
 - CF2 (subsidy) and CF4 (mandate) act on the REPLACE margin, which carries the
   F_replace single-tank-reset caveat (replace off-support 98%, Ticket 014). Their
   welfare numbers are INDICATIVE, not fully validated. Footnote them in every
   deliverable; do not present CF2/CF4 replace-margin welfare as validated.

═══════════════════════════════════════════════════
ENVIRONMENT
═══════════════════════════════════════════════════
Rscript: C:\Program Files\R\R-4.5.2\bin\Rscript.exe
Source: Code/Helpers/improved_estimator_OPTIMIZED.r (sourceCpp cpp_engine.cpp first).
Reuse the EXISTING 04o machinery verbatim where possible:
  Code/Dynamic_Model/04o_CF_TX_FlatFee.R  — baseline solve (Sec 5), TX-empirical
  mu (Sec 8), welfare accounting (Sec 9). Do NOT change baseline, mu, E_GRID,
  Semantic-2 (alphas dropped in the re-solve), beta (read from fit$config),
  sigma2, or any tolerance.
Solver: solve_equilibrium_policy_replacement_6p (OPTIMIZED.r:4454) at fixed theta.
Canonical fit: Output/Estimation_Results/Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds
New driver: Code/Dynamic_Model/04r_CF234_Welfare_6p.R  (clone 04o scaffolding; do
  not edit 04o_CF_TX_FlatFee.R).

Constants (match 04o/04j exactly):
  SCALE_FACTOR = 10000
  E_GRID_DOLLARS = c(HEALTH_ONLY = 17000, HEALTH_PLUS_UNMEASURED = 50000)
  beta = fit$config$beta  (read; do not hardcode)
  PV_FACTOR = 1/(1-beta)
  SUBSIDY_BASE (CF2) = 0.50
  MANDATE_AGE_BIN (CF4) = 6   (A_bin >= 6, tanks 25+ yr)

═══════════════════════════════════════════════════
WELFARE ACCOUNTING (identical to 04o Sec 9; NPL_REFERENCE §5)
═══════════════════════════════════════════════════
For an equilibrium (P, V) and TX-empirical mu, per E in E_GRID:
  producer_surplus = sum(mu * V) * SCALE_FACTOR
  external_damage  = sum(mu * P[,"maintain"] * h_vec) * E * PV_FACTOR * SCALE_FACTOR
  govt_outlay      = sum(mu * P[,"replace"]  * K_BASELINE * SUBSIDY_BASE) * PV_FACTOR * SCALE_FACTOR
                     (CF2 ONLY; 0 for baseline/CF1/CF3/CF4)
                     K_BASELINE = exp(fit$theta_hat[["K_log_*"]]) per wall — the
                     BASELINE K (subsidy outlay is baseline_K*subsidy_fraction,
                     NOT the effective K the firm responds to — NPL_REFERENCE §5).
  social_welfare   = producer_surplus - external_damage - govt_outlay
Assert |social - (producer - external - govt)| <= 1 (as 04o already does).

═══════════════════════════════════════════════════
CF DEFINITIONS + PSEUDOCODE
═══════════════════════════════════════════════════
All re-solve V at fixed theta with the 6p solver; alphas excluded (Semantic-2).

CF2 — Replacement subsidy (s* = 0.50):
  theta_cf <- fit$theta_hat
  theta_cf[["K_log_SW"]] <- theta_cf[["K_log_SW"]] + log(1 - SUBSIDY_BASE)
  theta_cf[["K_log_DW"]] <- theta_cf[["K_log_DW"]] + log(1 - SUBSIDY_BASE)
  eq_cf2 <- solve_equilibrium_policy_replacement_6p(theta_cf, cache_baseline, config, ...)
  welfare: govt_outlay computed with K_BASELINE (NOT the reduced K) and SUBSIDY_BASE.

CF3 — Pigouvian (internalize unpriced damage E in the maintain payoff):
  The firm's maintain utility gains an EXOGENOUS additive penalty -h_vec*E with
  coefficient EXACTLY 1 (independent of gamma_risk):
      u^M_pig(s) = u^M(s) - h_vec(s) * E
  Implementation note (R1 question): the 6p solver builds u^M internally from
  (theta, cache). Add the penalty via an optional additive maintain offset.
  PREFERRED: build cache_cf3 = cache_baseline with an added field consumed by the
  solver as a per-state additive term on u^M, OR add a `u_maintain_offset` arg.
  Do NOT fold E into hazard_loss (that would scale it by gamma_risk — wrong).
  Solve ONE equilibrium per E in E_GRID (the penalty depends on E).

CF4 — Age mandate (no maintain for A_bin >= 6):
  Force P[,"maintain"] = 0 for mandated cells at EVERY solver iteration (not just
  the input P) by driving v_maintain below config$min_log_val for those cells, so
  the softmax + R[s] + M restriction follow automatically (NPL_REFERENCE §4
  constrained-action-set; eps_floor handles R[s] and M). The retired 4p
  solve_equilibrium_policy_replacement_CFM (04j Sec 4) is the reference pattern
  (P[mand,maintain]=0; renormalize exit/replace among feasible). PORT that
  forcing into a 6p variant or add a `mandate_idx` argument to the 6p solver path.
  mandated cells: state_lut$A_bin >= MANDATE_AGE_BIN.
  PRE-FLIGHT assert: number of mandated cells matches expectation (A_bin in {6,7,8}
  across 2 wall x 2 regime = 12 cells), as 04j Sec 3 does.

For every CF: assert eq converged; all P rows sum to 1; V finite.

═══════════════════════════════════════════════════
STEP 1 — DELIVERABLE: CF1–CF4 welfare summary
═══════════════════════════════════════════════════
Output/Tables/04r_CF234_Welfare_Summary.csv
  Rows: 40 — every (scenario x component x E_label):
    scenario  in {baseline, CF1_flatfee, CF2_subsidy, CF3_pigou, CF4_mandate}  (5)
    component in {ProducerSurplus_USD, ExternalDamage_USD, GovtOutlay_USD, SocialWelfare_USD} (4)
    E_label   in {HEALTH_ONLY, HEALTH_PLUS_UNMEASURED} (2)
  Cols (exact, types):
    scenario        chr
    component       chr
    E_label         chr
    E_external_USD  num   (17000 / 50000)
    level_USD       num   (the scenario's welfare component level, $/facility PV)
    delta_USD       num   (level_USD - baseline level for the same component+E_label;
                           0 for baseline rows)
  NOTE: CF1 level/delta must reproduce the T017 04o_CF_Welfare_Summary.csv numbers
  EXACTLY (ProducerSurplus delta +9213, SocialWelfare delta +9092 at E=50k) — this
  is the cross-check that the ported machinery matches 04o. Assert it in the log
  (|CF1 delta here - T017 delta| <= 1).

Output/Tables/04r_CF234_Welfare_Summary.tex
  Slide-ready booktabs table: rows = 4 CFs; cols = dProducerSurplus, dExternalDamage,
  dGovtOutlay, dSocialWelfare (all $/facility PV, E = 50k headline). One \textit{Notes:}
  line carrying the CF2/CF4 replace-margin caveat. tbl-cap short title.

═══════════════════════════════════════════════════
STEP 2 — DELIVERABLE: welfare bar chart (the slide figure)
═══════════════════════════════════════════════════
Output/Figures/04r_CF234_Welfare_BarChart.png  (AND .pdf)
  Grouped bar chart: x = CF {CF1, CF2, CF3, CF4}; fill = welfare component
    {Firm surplus, External damage, Govt outlay, Social welfare};
    y = delta_USD ($/facility PV) at E = 50k (HEALTH_PLUS_UNMEASURED).
  geom_col(position="dodge"); horizontal zero line; dollar y-axis.
  Palette (match 04j): Firm surplus #003262, External damage #3B7A57,
    Govt outlay #FDB515, Social welfare #8B1A1A.
  Caption/subtitle: E = $50k; CF2/CF4 replace-margin caveat in a footnote text.
  Size width=10 height=6 dpi=150.
  ALSO emit a second copy at E = 17k? NO — headline is 50k; one figure. (A 17k
  sensitivity figure is optional; do not block on it.)

═══════════════════════════════════════════════════
STEP 3 — VALIDATION
═══════════════════════════════════════════════════
  - CF1 reproduction check vs T017 (see Step 1 NOTE) — log PASS/FAIL.
  - Welfare identity (social = producer - external - govt) within $1 for all 5
    scenarios x 2 E (assert).
  - All equilibria converged; all P rows sum to 1.
  - Print the headline table (4 CFs x 4 deltas at E=50k) to the log.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════
C1  New driver 04r created by cloning 04o scaffolding; 04o/04j NOT edited; shared
    solver/config functions NOT edited (CF4 forcing added as a driver-level 6p
    variant or a new arg, not a change to the estimator's NPL loop).
C2  Canonical gammafree fit loaded; baseline/mu/E_GRID/Semantic-2/beta unchanged
    from 04o.
C3  CF2/CF3/CF4 equilibria converged; P rows sum to 1; V finite (assert).
C4  CF1 reproduction vs T017 within $1 (log).
C5  Welfare identity within $1 all scenarios x E (assert).
C6  04r_CF234_Welfare_Summary.csv: 40 rows, exact cols/types per Step 1.
C7  04r_CF234_Welfare_Summary.tex: 4-CF deltas table with caveat note.
C8  04r_CF234_Welfare_BarChart.png + .pdf per Step 2.
C9  CF2/CF4 replace-margin caveat present in .tex notes AND figure footnote AND log.

WHAT NOT TO DO: do not change E_GRID, beta, sigma2, any tolerance, the premium
construction, Semantic-2, mu, F_replace, or the Bellman/CCP estimator code; do not
re-estimate; do not re-run 04b/04o.
