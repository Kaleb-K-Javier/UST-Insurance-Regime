# TICKET 024q — Pooled total-exposure gamma + RB-regime interaction (FE-on respec)
# Created: 2026-06-25
# Status: READY
# Attempt: 0
# Type: MODEL RESPEC (small). Replace the two separately-unidentified dollar
#   coefficients (gamma_p on premium, gamma_r on hazard*deductible) with ONE pooled
#   coefficient on TOTAL EXPOSURE, plus a risk-based-regime interaction. State FEs
#   STAY ON. No new Bellman/CCP math — the gradient pieces are recombinations of the
#   existing ones, so correctness is gated by the existing GATE C (analytic grad ==
#   finite-diff). NO change to the kernel, basis, PSOCK, preconditioner, or any
#   tolerance.

═══════════════════════════════════════════════════
WHY
═══════════════════════════════════════════════════
The full-data fits settled the identification question:
  - FE-on:  gamma_p ~= -0.019 (~0), gamma_r = 4.32   (within-state risk signal)
  - FE-off: gamma_p ~= 0.01, gamma_r ~= 0.02         (both collapse -> FEs needed)
Premium is not separately identified under the credible (FE-on, within-state)
design; the TX premium data is 2006+ only and pre-2006 is unfillable (TCEQ/PARIS
FR records empty pre-2006), so there is no clean within-TX FF benchmark. The data
supports ONE response to total dollar exposure, not two. We therefore pool premium
and OOP into total exposure and add a discrete RB-regime interaction (a strong
contrast) to retain a defensible "risk-based vs uniform" knob for the CFs.

Identification note (NOT a code task): gamma_RB is identified off the RB envs
(RB_2006/2014/2019, all TX) vs the FF controls. With FE-on (TX = reference,
alpha_TX = 0) it is the within-TX exposure-response increment. It carries a
between-state (RB == TX) confound — documented, not fixable here.

═══════════════════════════════════════════════════
THE MODEL CHANGE
═══════════════════════════════════════════════════
Define total exposure for action i at composition c, env e:
    E_i[c] = prem_ej[[e]][c, i] + haz_j[c, i] * D_e

Flow utility (was: -gamma_p*prem - gamma_r*haz*D):
    u = phi_G + alpha_e
        - (gamma_pool + gamma_RB * 1[RB_e]) * E_i
        - k_i*c_rem - m_i*c_inst
where 1[RB_e] = (env_tbl[e]$type == "RB").

Parameter vector (still 9 structural + alphas): replace gamma_p, gamma_r with
    param_struct = c("phi_1","phi_2","phi_3","phi_4",
                     "gamma_pool","gamma_RB","c_rem","c_inst","kappa_1")
Bounds: gamma_pool, gamma_RB in [-20, 20] (sign-free, same as the old gammas).

GRADIENT PIECES — pure recombination of what is already in the file:
  rho (C x N_G), per env e, with rb = 1[RB_e]:
    rho[["gamma_pool"]] = rho_gp + rho_gr            # = -sum_i P_i*(prem + haz*D)
    rho[["gamma_RB"]]   = rb * (rho_gp + rho_gr)     # zero matrix when rb == 0
  (rho_gp, rho_gr are the existing per-env gamma_p / gamma_r rho's — keep computing
   them as intermediates, just combine.)
  du at observed states (n_obs vector), per env e:
    du[["gamma_pool"]] = -(prem_obs + haz_obs * D_e) # = existing du_gp + du_gr
    du[["gamma_RB"]]   = rb * du[["gamma_pool"]]

═══════════════════════════════════════════════════
TOUCH POINTS (enumerated — hit ALL of them, a miss = GATE C fail)
═══════════════════════════════════════════════════
Gate behind a toggle PM08_POOLED=1 (default OFF preserves the existing two-gamma
path byte-for-byte; the FE-on baseline must remain reproducible with the toggle off).
  1. param_struct definition (~L153-154): conditional on PM08_POOLED.
  2. Flow utility u_mat in the per-env value assembly (~L855-859): pooled form above.
  3. rho build, Phase B test env (~L440-451): combine to gamma_pool / gamma_RB.
  4. rho build, build_basis_env / per-env function (~L740-745): same combination.
  5. du build, GATE C test env (~L602-603): gamma_pool / gamma_RB du.
  6. du build, per-env coeff function (~L817-818): same.
  7. bounds (~L896-897): set gamma_pool, gamma_RB to [-20,20].
  8. theta_test (~L508-512): give gamma_pool, gamma_RB test values (e.g. gamma_pool
     = 0.05, gamma_RB = 0.02) so GATE C exercises both new params.

DO NOT TOUCH: kernel, pm_matvec.cpp, basis/BiCGSTAB/P1, PSOCK block, alpha
profiling (use_alpha), tolerances, max_iter logic, the batched-matvec toggle.

═══════════════════════════════════════════════════
RUN (server ucbare2, FE-ON)
═══════════════════════════════════════════════════
  $env:PM08_POOLED  = "1"     # pooled exposure + RB interaction
  $env:PM08_MAXITER = "25"
  # (do NOT set PM08_NO_ALPHA — FEs must be ON)
  & $rs Code/Dynamic_Model/PM08_Estimator_v4.R
  Remove-Item Env:PM08_POOLED, Env:PM08_MAXITER   # clear after
Standalone R-4.5.2/4.4.3 x64; ONE monitor max on the run.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (reviewer)
═══════════════════════════════════════════════════
- [ ] PM08_POOLED OFF reproduces the existing two-gamma path unchanged (param names,
      GATE C, and the converged FE-on theta all identical to the prior run).
- [ ] PM08_POOLED ON: param_struct shows gamma_pool, gamma_RB (no gamma_p/gamma_r);
      "STATE FEs: ON".
- [ ] GATE C PASS for gamma_pool AND gamma_RB (analytic grad == finite-diff,
      max rel err < 1e-5). This is the correctness gate for the recombination.
- [ ] GATES A, B, D pass; NPL converges (dTheta, dP < 1e-5).
- [ ] Sanity on the converged fit: gamma_pool > 0 (more exposure -> less maintain);
      gamma_RB reported with sign (the RB increment). c_rem/c_inst/kappa_1/phi
      land near the FE-on two-gamma values (the pooling shouldn't move the
      cost/level params materially).
- [ ] Save the fit rds + a one-line theta summary (Estimator/Sample/Converged/LL/
      theta_hat/Elapsed/Saved path) per OUTPUT RULES.

═══════════════════════════════════════════════════
SEQUENCING / GATE
═══════════════════════════════════════════════════
This is Part A. Part B (024r) = the CF re-solve (CF1 TX-as-FF full-contract swap
with the gamma_RB knob decomposition; CF3 replacement subsidy; CF4 removal mandate)
runs on THIS fit. 024r is gated on this fit converging + GATE C passing. Architect
pulls NPL_REFERENCE before writing the 024r CF-equilibrium pseudocode (gate #8).

CF deliverables (for 024r, noted here so they're not lost): per CF, ONE
decomposition bar chart + ONE table. Reuse existing CF table/figure code
(04r/04v-style) where possible.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[reviewer fills]
