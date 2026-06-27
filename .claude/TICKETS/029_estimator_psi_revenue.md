# TICKET 029 — Estimator: replace phi_1..phi_4 with psi * R(G,g,era)
# Created: 2026-06-25
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0
# Type: ESTIMATION CORE (touches PM08 basis + gradient). GATED — full Sonnet review.
# SEAT: STRUCTURAL MODEL BUILDER (run_builder_pro_api.ps1), NOT the data coder — this
#       edits the estimator. Runs on the SERVER (Rcpp engine; PM00 preflight gate).
# Depends on: ticket 028 (R_rev in PM_Lookups.rds) + coordinate with the pooled
#       total-exposure risk change (gamma + 1[RB]gamma_RB): land that rename FIRST,
#       then this psi-for-phi swap (both touch PM08 param vector/basis — see Step 2).

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
Replace the four free operating constants phi_1..phi_4 (one per capacity bin G) with
a single revenue weight psi on the MEASURED revenue R(G,g,era) built in ticket 028.
This forces the operating payoff's capacity profile to the shape of measured fuel
margin x capacity, leaving psi as the one free dollar-to-utility weight on revenue,
on the same footing as the risk parameters. It is the over-identifying restriction of
sec-identification: phi_G (4 free numbers) -> psi (1 number) x measured R.

═══════════════════════════════════════════════════
MATH
═══════════════════════════════════════════════════
Operating (non-exit) flow utility, env e (state g(e)), era, capacity bin G:
  Eq. 1  u_oper(s) = psi * R[G, g(e), era] + alpha_{g(e)} - [risk terms] - k c_rem - m c_inst
  Eq. 2  u_X(s)    = kappa_1 * N(n)         (terminal; unchanged)
Derivatives that change:
  Eq. 3  d u_oper / d psi = R[G, g(e), era]          (for every work + maintain action)
  Eq. 4  d u_X    / d psi = 0
  (REPLACES phi_G, whose d u_oper / d phi_G = 1 on bin G, 0 elsewhere.)
Identification (sec-identification): psi is the operating-vs-exit response across G and
the state-era margin scaling; alpha_g (retained) absorbs the state-level operating
mean. kappa_0 := 0 and sigma = 1 unchanged. [risk terms] = whatever the CURRENT spec
is (gamma_p, gamma_r OR the pooled total-exposure gamma + 1[RB] gamma_RB); this ticket
does NOT touch the risk side.

═══════════════════════════════════════════════════
KEY INVARIANTS  (the spots a coder would otherwise guess — do not deviate)
═══════════════════════════════════════════════════
I1 G INDEX / TIMING: R_e_era[G] uses the CURRENT state's capacity bin G — the
   IDENTICAL index phi_G used. Do NOT compute or use a post-action G'. The model's
   capacity evolution is the Gmat transition (it bites through the continuation
   value), NOT the within-period flow. (The model sketch's "R(G')" is design
   narrative; the implemented flow utility uses the state's G, exactly as phi_G did.
   Mechanically: psi slots in wherever phi_G was, same G, nothing else moves.)
I2 COVERAGE: psi enters EVERY non-exit action — all work (k,m) AND maintain (0,0) —
   and is exactly 0 for exit X. Identical coverage to phi_G, whose basis is (1-P_X).
   Maintain earns revenue too; do not restrict psi to work actions.
I3 ERA: R is era-specific. Route R_e_era through the SAME env/era code path the
   premium term P already uses (pbar is [16 x 3 eras]); do NOT invent a new era
   mechanism. Read how prem enters per (env, era) and mirror it.
I4 PARAM PLUMBING: after editing param_names + rho_list, the W_list / V basis rebuild
   from param_names automatically. Ensure NO orphan "phi" remains in theta_test,
   optim bounds, or any printing/summary. If optim uses box bounds (L-BFGS-B), give
   psi a WIDE bound like the gammas — do not tightly box it.
I5 TX REFERENCE: TX (RB, regime=1) has alpha_TX == 0 by design (no alpha estimated
   for it). With phi_G gone, TX's operating payoff is purely psi*R_TX — correct (TX
   is the FE reference; psi*R is its measured operating level). Do not add a TX
   constant.

═══════════════════════════════════════════════════
PSEUDOCODE  (edits to Code/Dynamic_Model/PM08_Estimator_v4.R)
═══════════════════════════════════════════════════
Step 1 — Load R_rev:
  In the lookup-load section, read R_rev from PM_Lookups.rds. For env e and era,
  define R_e_era = R_rev[ , g(e), era]  (length-4 over G). Handle era EXACTLY as the
  premium term prem_ej handles era (same per-(env,era) indexing path).
  assert: R_e_era finite length-4 for every estimated env x era.

Step 2 — Parameter vector:
  param_struct: REMOVE "phi_1","phi_2","phi_3","phi_4"; ADD "psi" (first position).
  -> param_struct <- c("psi", "gamma_p", "gamma_r", "c_rem", "c_inst", "kappa_1")
     (keep the risk names matching the CURRENT spec; if the pooled-risk rename has
      landed, use those names instead — do not re-introduce phi.)
  Update n_param, param_names, and theta_test (drop phi_* lines; add
  theta_test["psi"] <- a sign-correct nonzero start, e.g. 1.0).

Step 3 — Basis rho_list (the theta-free du/dtheta aggregation):
  REMOVE the four rho_list[["phi_G'"]] entries (currently rho[c,G'] = 1 - P_X[c,G']).
  ADD rho_list[["psi"]]: a C x 4 matrix with
     rho_psi[c, G] = R_e_era[G] * (1 - P_X[c, G]).
  (i.e. the old phi_G basis summed over G, each bin weighted by R_e_era[G].)

Step 4 — coeff_p at observed states:
  REMOVE the phi_G branches (du_j/dphi_G = 1 for work in bin G).
  ADD: for pn == "psi": du_j/dpsi at obs state s = R_e_era[obs_G[s]] for work AND
  maintain actions j; 0 for exit j. (Parallel to phi but scaled by R_e_era[obs_G].)

Step 5 — ccp_update_env:
  Wherever phi_G was assembled (e.g. phi_G <- c(theta_k[["phi_1"]],..,[["phi_4"]]))
  and added to the operating value, REPLACE with psi_G <- theta_k[["psi"]] * R_e_era
  (length-4 over G), added identically to the operating value by bin.

Step 6 — Estimate:
  Run the existing NPL / optim path unchanged (tolerances per CLAUDE.md: tol_theta=1e-8,
  tol_P=1e-7, max_npl_iter=600, ccp_damping_lambda=0.6). Respect PM08_NO_ALPHA.
  Save fit to Output/Estimation_Results/pm_psi_revenue_<sample>_20260625.rds.

Step 7 — Print (CLAUDE.md output rules):
  Estimator name | Sample | Converged T/F at iter N | LL | theta_hat (names+values incl psi)
  | Elapsed | Saved path. NEVER print full matrices/CCPs/V.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Mirror the PREMIUM term's env+era handling for R_e_era — same indexing, same place
  in the basis assembly (the linear V = V_const + sum_p theta_p W_p machinery from the
  024p basis speedup; psi gets one W_p like any other param).
- The analytic gradient must be updated consistently: the finite-difference gradient
  check already in PM08 (ll_grad_e vs FD) MUST pass for psi (rel err < 1e-4) before
  the full run — this is the primary correctness gate.
- Do not touch the risk terms, kappa_1, alpha_g machinery, the C++ engine, or the
  state space. Only the operating-constant slot changes (phi -> psi*R).
- sourceCpp the engine first; run via the R-4.5.2 Rscript (env note in ticket 023).

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════
- [ ] param_names contains "psi", contains NO "phi_*".
- [ ] R_e_era loaded per (env,era), length-4, finite; era path matches premium term.
- [ ] Analytic-vs-FD gradient check passes for psi (rel err < 1e-4) and for all
      retained params (regression: still passes for gamma_*, c_*, kappa_1).
- [ ] LL finite at theta_test; estimation converges (optim$convergence==0 or NPL
      d_theta < tol_theta) at iter N reported.
- [ ] theta_hat saved to Output/Estimation_Results/ with psi; summary printed per
      output rules (no matrices/CCPs/V dumped).
- [ ] No tryCatch->NULL; tolerances unchanged from CLAUDE.md; log written.
- [ ] phi_G fully removed (no orphan references in rho_list, coeff_p, ccp_update_env,
      theta_test, param printing).

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[blank until first attempt]
