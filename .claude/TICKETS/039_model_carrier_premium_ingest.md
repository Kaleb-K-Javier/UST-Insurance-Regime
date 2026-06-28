# TICKET 039 — Ingest carrier-specific premiums into the structural model (recover gamma_p)
# Created: 2026-06-27
# Status: SPEC READY — architect-approved design; GATED on ticket 036 producing
#         Data/Analysis/tx_facility_premium_rebuilt.csv. Needs researcher sign-off before coding.
# Type: MODEL CHANGE (estimator premium plumbing). Touches PM02_Lookups.R + PM08_Estimator_v4.R
#       + the TX env aggregation. Does NOT touch the Bellman/V-inversion/CCP/NPL math.
# Seat: architect spec -> R coder (3-session workflow).

═══════════════════════════════════════════════════
WHY (what this fixes)
═══════════════════════════════════════════════════
gamma_p (premium response) is UNIDENTIFIED, not zero. The structural premium is ONE
Mid-Continent card P_RB_all[pc, era] for every TX/RB facility, so the only within-TX premium
variation is age x wall x era — which is COLLINEAR with the hazard term (gamma_r * H * D, also
age x wall) and the age dynamics. The model cannot separate "responds to premium" from
"responds to retained risk." Assigning each facility its ACTUAL carrier's card adds premium
variation WITHIN a fixed (age x wall x era) cell — same risk cell, same era, different insurer,
different premium — which is ORTHOGONAL to the hazard (the leak hazard is physical, carrier-
invariant). That orthogonal variation is what identifies gamma_p separately from gamma_r.

═══════════════════════════════════════════════════
DESIGN (settled with researcher 2026-06-27 — do NOT re-open these)
═══════════════════════════════════════════════════
- Carrier enters EXACTLY like era: a finer ENVIRONMENT label on the premium card. The flow
  utility is UNCHANGED — u_M still has `gamma_p * prem_ej[[e]]`; only prem_ej's indexing gains
  a carrier dimension. This is the "fixed-type lookup, no kernel" pattern era already uses
  (verified: PM08:99 env_key = RB_<era>; PM08:199/403 prem = ss$P_RB_all[pc, era_str]).
- NO carrier transition kernel (static / myopic expectations): each (era, carrier) is its own
  stationary environment; a switch is the facility's later rows landing in a different env, never
  anticipated. Justified — the carrier exits/merger (Zurich ~2014, TOMICS->Mid-Continent ~2019-20)
  were UNANTICIPATED + news-documented. Carrier is NOT added to the 298k state space.
- NO carrier FE, NO facility FE. KEEP the existing 17 state FEs (alpha_TX = 0). Carrier
  variation is within-TX, so the state FE never touches it.
- Carrier selection is ENDOGENOUS (firms shop; shopping unobserved) — ACCEPTED as a data
  limitation, not corrected. gamma_p is therefore an ASSOCIATIONAL premium response, which is the
  intended object. (Robustness re-fit below isolates the supply-driven part.)
- gamma_r, H*D, hazard kernel, costs, tolerances, gates, alphas: ALL UNCHANGED.
- Coverage limit HELD FIXED at $1M (per ticket 036 diagnostic: 90.8% on $1M, modal $1M every
  year, no drift). Not a premium dimension.

═══════════════════════════════════════════════════
INPUTS
═══════════════════════════════════════════════════
- Data/Analysis/tx_facility_premium_rebuilt.csv (TICKET 036): panel_id, year, carrier,
  coverage_limit, premium_rebuilt, premium_imputed.  <-- HARD DEPENDENCY (this ticket is gated on it)
- PM02_Lookups.R: current premium card builder (pbar[16x3] -> P_RB_all[pc, era]).
- PM08_Estimator_v4.R: env_key / prem_ej / era_str construction (lines ~99, ~141, ~199, ~403).
- PM01 estimation-panel / pm_agg_counts: the aggregated env x cell x action counts the NLL reads.

═══════════════════════════════════════════════════
STEPS
═══════════════════════════════════════════════════
DATA FLOW (corrected per coder Qs 2026-06-27): the per-tank CARD comes from the transcription
ENGINES; the carrier ASSIGNMENT comes from the rebuilt CSV. You CANNOT aggregate the per-facility
rebuilt premium back to a per-cell card (a facility spans multiple cells). P_RB_all lives in
PM_StateSpace.rds (PM03), built from PM02's pbar — keep it there. pm_agg_counts is built in PM03
Section 9, not PM01. Canonical carrier keys C = {MID_CONTINENT, TOMICS, GREAT_AMERICAN, ZURICH, ACE,
AIG, IMPUTED}; ticket 036 owns the FR ISSUER_NAME -> key crosswalk (no string cleanup in 039).

1. PM02 — build the carrier-indexed per-tank card pbar[16 cells, era, carrier] from the transcription
   ENGINES Data/Analysis/rate_engines/<carrier>_engine.csv (cols carrier, wall, age_bin, era,
   premium_usd_per_tank_yr, $1M). Divide by SCALE (10000), exactly like the current pbar build.
   pc-cell order = today's 16 MARG cells (2 wall x 8 age). IMPUTED card = share-weighted priceable-
   market mean per (cell, era). 3D array, dimnames list(MARG, ERAS, C). Keep the legacy 2-D
   pbar[16x3] as the carrier-marginal fallback.
2. PM03 — build P_RB_all[pc, era, carrier] from pbar via the EXISTING portfolio-composition mapping
   applied per carrier; save into PM_StateSpace.rds (where P_RB_all already lives). Shape
   [C_comp x era x |C|], dimnames (NULL, ERAS, C).
3. PM03 Section 9 (agg counts) — stamp each obs with its carrier, then group with carrier in the key:
   - GATE: stopifnot(file.exists("Data/Analysis/tx_facility_premium_rebuilt.csv")) naming ticket 036;
     then fread (natural hard stop, no fallback — script is gated until 036 delivers).
   - JOIN the rebuilt CSV (panel_id, panel_year -> carrier, premium_imputed) onto the obs (incl)
     BEFORE the count aggregation. carrier already NORMALIZED to C by 036.
   - For g=="TX": group by (sidx, g, era, carrier, action). FF/controls: carrier = NA_character_,
     grouping unchanged. Carry premium_imputed (int 0/1) onto rows (for the robustness re-fit).
     pm_agg_counts gains a character `carrier` column (NA for FF). Preserves the carrier variation
     through aggregation (analog of era already being in the key).
4. PM08 — wire the carrier env (mechanical):
   - L99  env_key := ifelse(g=="TX", paste0("RB_", era, "_", carrier), as.character(g)).
   - L141 parse BOTH era_str AND carrier out of env_keep for RB envs.
   - L127 REPLACE stopifnot(length(rb_envs)==3L) with:
       stopifnot(length(rb_envs) >= 3L, all(rb_eras %in% ERAS), all(rb_carriers %in% C))
   - L199/L403 prem_ej[[e]] for RB env e = ss$P_RB_all[pc, ev$era_str, ev$carrier].
   - EVERYTHING ELSE byte-identical: u_vec = gamma_p*prem_ej + gamma_r*(-haz*D) + alpha; V-inversion
     (I-beta M)^-1 R, CCP softmax, NPL outer loop, GATE B/C, tolerances — untouched.
   - n_env: controls (17) + TX (era x carrier combos that OCCUR; sparse — Zurich early, TOMICS gone
     post-2019) ~ 12-16 TX -> ~30 total. PSOCK env-parallel already handles it (~2x TX-side solves).
5. RE-FIT two-gamma FE-on (PM08_POOLED unset, PM08_PSI unset). Report gamma_p vs the ~0 baseline.
   Save Model_Portfolio_v4_two_gamma_FE_on_carrier_<date>.rds. Runs on the SERVER (PM_* inputs server-only;
   coder implements + parse-checks locally, the fit + gates run on ucbare2).
6. ROBUSTNESS — re-fit on real-engine rows only (premium_imputed==0L; drop IMPUTED envs): confirm
   gamma_p is sign/magnitude-stable (not an imputation artifact).

═══════════════════════════════════════════════════
LOCKED DECISIONS
═══════════════════════════════════════════════════
- Flow-utility math UNCHANGED. Only prem_ej[[e]] gains a carrier index. No new Bellman/NPL math.
- No carrier kernel, no carrier FE, no facility FE; keep 17 state FEs. Static expectations.
- gamma_r / H*D / hazard / costs / tolerances / gates UNCHANGED.
- Limit fixed $1M. Imputed carriers -> market-mean card + premium_imputed flag (robustness re-fit).
- (coder Q1, 2026-06-27) age_bin = INTEGER model bin 1-8 EVERYWHERE — boy_composition_long AND the
  rate_engines/*.csv cards (NOT "0-5" strings). After `as.integer(age_bin)` add
  `stopifnot(!anyNA(bin), all(bin %in% 1:8))` so a type mismatch fails loud, not silent.
- (coder Q2) Unmatched TX facility-years (uninsured / NO COVERAGE / thin 2006-07) get
  carrier="IMPUTED", premium_imputed=1 — FILL the LEFT-join NA BEFORE the assertion; do NOT drop
  them (keeps the current model's TX sample; they take the share-weighted market-mean card like
  Colony/Ironshore). Then the guard is `stopifnot(!any(is.na(carrier)))` AFTER the fill.
- (coder Q3) Tolerances: keep the hardcoded defaults (env-overridable by design; changing the
  constant is approval-gated). Set env vars per RUN, but at the NO-DAMPING FLOOR ~5e-5 — PM08 has no
  CCP damping and oscillates below ~1e-5, so 1e-7 won't converge. CLAUDE.md's 1e-7 needs damping
  added first. The production fit itself is researcher-gated (CLAUDE.md approval gate #3).

═══════════════════════════════════════════════════
VALIDATION / ACCEPTANCE
═══════════════════════════════════════════════════
- [ ] PM08 gates pass byte-identical (GATE B V_basis==V_direct <1e-8; GATE C analytic==FD grad
      <1e-5; PM08_VALIDATE_B serial==parallel) — only prem_ej indexing changed, so they MUST still pass.
- [ ] Within-(TX, era, pc) premium CV > 0 from carrier (the new orthogonal variation exists);
      print it as a pre-fit diagnostic.
- [ ] n_env = 17 controls + expected TX (era x carrier) combos; no empty env feeds the NLL.
- [ ] Converged two-gamma fit; report gamma_p (and whether it moves off ~0 — report honestly either way).
- [ ] Robustness re-fit (real-engine only) sign-consistent with the headline.

═══════════════════════════════════════════════════
DOWNSTREAM (separate update, not this ticket)
═══════════════════════════════════════════════════
PM09_CF_Portfolio.R: the baseline CF must assign each TX facility its carrier card, and CF1
(premium change) perturbs the carrier-enriched premium. The current "FF-era = unweighted era-mean"
collapse rule generalizes to a carrier x era assignment from observed shares. Mirror of step 4 on
the CF side; gate against the carrier fit.

═══════════════════════════════════════════════════
NPL REFERENCE NOTE
═══════════════════════════════════════════════════
Per NPL_REFERENCE.md S1/S3: V = (I - beta*M)^-1 R, R[s] = sum_j P_j(u_j + sigma(gamma_E - log P_j)),
CCP softmax, NPL outer loop — ALL UNCHANGED. The premium enters u_M only (param_sign_conventions:
+gamma_p*P, gamma_p<0). This ticket changes WHICH premium vector each environment reads
(prem_ej[[e]] gains a carrier index), not the dynamic-choice machinery -> no eq-(2.3.25)-shaped risk.
