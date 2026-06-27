# EDITOR BRIEF — rewrite the structural-model section to the FINAL portfolio spec
# For: .claude/agents/jmp_editor.md   |   Created: 2026-06-26
# Hand this whole file to the jmp_editor agent as the task prompt.

---

You are editing the structural-model section of the JMP. The portfolio model's
specification has changed; your job is to bring the structural prose into line with
the FINAL spec, in your usual sandwich form and under your usual anchors.

## SOURCE OF TRUTH (read first; draft from it; do not re-derive)
- `Reports/Paper/Structural_Model_Current_Spec.md` — JUST UPDATED. Authoritative for
  the flow utility, identification, estimates-status, and counterfactual design. Every
  structural number and claim you write must trace to this file (or to the researcher).

## THE FIVE DELTAS TO ENFORCE (what changed vs. any structural prose now in the canvas)
1. OPERATING TERM = MEASURED REVENUE. The per-period operating payoff is
   `psi * R(G, state, era)` — one free revenue weight `psi` times a first-stage
   measured revenue object (representative capacity of bin G x the state-era fuel
   margin). This REPLACES the four free capacity-bin intercepts `phi_1..phi_4`, which
   came back flat with no size gradient and no revenue content.
2. RISK EXPOSURE = TWO SEPARATE WEIGHTS, not one pooled coefficient. `gamma_p` on the
   insurance premium `P`; `gamma_r` on the expected out-of-pocket cost `H * D`
   (hazard x deductible). They are not forced equal.
3. ASYMMETRIC IDENTIFICATION. `gamma_r` is IDENTIFIED (~4.32 at the FE-on anchor) off
   within-state hazard x deductible variation (wall type at fixed age; state-set
   deductible). `gamma_p` is NOT separately identified (premium is cross-state ->
   absorbed by the state FEs; the within-state remainder is collinear with hazard).
   Report `gamma_p` as not separately identified — never as a number.
4. MONEY-METRIC REFRAME + BRACKETED CF1. There is ONE marginal utility of money,
   identified as `gamma_r` off the out-of-pocket channel and IMPOSED on the premium.
   The Texas-to-uniform counterfactual (CF1) is therefore a BRACKET, not a point:
   `gamma_p = 0` (premium is a pure transfer — lower bound on the behavioral response)
   to `gamma_p = gamma_r` (full money-metric — upper bound). Report CF1 as an interval.
   This REPLACES the older "turn off gamma_RB" mechanism decomposition.
5. POOLED SPEC IS A REJECTED NEGATIVE RESULT. A single pooled coefficient on
   premium-plus-out-of-pocket converged but diluted to ~0: premium dominates the pooled
   sum in magnitude yet is behaviorally inert, dragging the one coefficient to zero and
   destroying the real out-of-pocket response (`gamma_r = 4.32`). Use this as the
   MOTIVATION for the two-channel spec; do not present a pooled number as a headline.

## SCOPE
The structural-model section/subsections only: flow utility, identification, the
estimates/status paragraph, and the counterfactual design. Do not touch the intro or
reduced-form prose except to fix a now-wrong structural claim it references.

## NUMBERS YOU MAY USE (everything else is PENDING — flag, do not invent)
- `gamma_r = 4.32`; `c_rem = 3.85`, `c_inst = 2.04`, `kappa_1 = 0.93`;
  `LL = -291,648.5`; `beta = 0.9957`; 17 environments; deductibles ~$5k-$55k.
- `psi` and the final two-gamma + revenue fit are GATED (tickets 028/029) and not yet
  estimated. Write `psi` as an estimated weight with the fit flagged as pending; do NOT
  fabricate a `psi` value, a refreshed `gamma_r`, SEs, or a final log-likelihood.

## CONFLICT TO RECONCILE — DO NOT SILENTLY WRITE AROUND IT
Your own LOCKED CONVENTION states: "Externality wedge E ≈ $50,000/release is
uninternalized even under perfect actuarial pricing; γ̂_r ≪ γ̂_p is the quantitative
statement of that residual wedge." That ordering CONTRADICTS this spec: here `gamma_r`
is the IDENTIFIED weight and `gamma_p` is bracketed at `[0, gamma_r]`, i.e.
`gamma_p ≤ gamma_r` — the opposite ordering. Before writing any welfare/externality
paragraph: surface this to the researcher and propose the reconciliation — the
externality wedge `E` is the UNINTERNALIZED leak damage that generates no insurance
claim and therefore sits OUTSIDE both `gamma_p` and `gamma_r` (it is a damage the
firm's payoff never sees), NOT a statement that `gamma_r ≪ gamma_p`. Do not emit prose
asserting `gamma_r ≪ gamma_p`, and flag the locked-convention line for update.

## DELIVERABLE
Sandwich-form outline/prose for the structural-model section written into
`Reports/Paper/# Paper editing notes.md`, plus your 2-3 sentence Editor's Note naming
the single most important structural change and the anchor that mandated it. Hold the
frontier-of-assumptions ordering (anchor 9): operating-value and risk weights are
structural objects; be explicit that `gamma_p`'s bracket is the honest cost of the
identification limit. Stop and ask if a pending number (`psi`, SEs, final LL) blocks a
specific paragraph.
