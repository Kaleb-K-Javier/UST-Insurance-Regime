# TICKET 001 — 8p + 17-control FE replacement DCM (25 params) + welfare with Marcus E sensitivity
# Created: 2026-05-13
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
The 8-parameter replacement DCM (no FE) yielded γ_price,FF ≈ -14.4 — economically
implausible. Inspection traced the inflation to across-state heterogeneity in
observed Maintain rates that the 8-parameter structural model cannot generate.
Adding state fixed effects α_g to the *maintain* flow utility — entering ONLY
the observational likelihood, NOT the structural equilibrium — absorbs this
nuisance variation. We expect γ to pull toward sensible magnitudes. We then
re-run the 03-style social welfare counterfactuals (A: TX-stays-FF; B: Pigouvian
internalising Marcus-2021 health externality E; C: K_SW subsidy; D: K tax) with
external damages calibrated to Marcus (2021) infant-health damages per reported
leak event, at both a conservative (E = $17K) and a robust (E = $50K) value.

═══════════════════════════════════════════════════
MATH
═══════════════════════════════════════════════════
Indexing: states s ∈ {1..32} (8 age bins × 2 wall × 2 regime).
Geo: g ∈ {0..17}; g = 0 ≡ TX (α_TX ≡ 0, fixed); g = 1..17 ≡ CONTROLSTATES in order
{AR, CO, ID, KS, KY, LA, MA, MD, ME, MN, MO, NC, OH, OK, SD, TN, VA}.
Actions: a ∈ {1 = maintain, 2 = exit, 3 = replace}.

Eq. 1  (Structural conditional value functions, no α):
  v_M(s) = u_M(s; θ) + β · F_M[s,·] · V(s')
  v_E(s) = u_E(s; θ)
  v_R(s) = u_R(s; θ) + β · F_R[s,·] · V(s')
  V(s)   = invert_value_function_replacement(P, U, cache, config)   (existing fn)

Eq. 2  (Observational likelihood with maintain-only FE — for inference ONLY):
  Pr(a = 1 | s, g) = exp(v_M(s) + α_g) / Z(s, g)
  Pr(a = 2 | s, g) = exp(v_E(s))        / Z(s, g)
  Pr(a = 3 | s, g) = exp(v_R(s))        / Z(s, g)
  Z(s, g) = exp(v_M(s) + α_g) + exp(v_E(s)) + exp(v_R(s))

Eq. 3  (Aggregated NLL over 576 (s,g) cells):
  NLL(θ_raw) = − Σ_{s=1..32} Σ_{g=0..17}
                  [ n_M(s,g)·log Pr(1|s,g) + n_E(s,g)·log Pr(2|s,g)
                    + n_R(s,g)·log Pr(3|s,g) ]
  where n_a(s,g) = # observations with (s_idx=s, graw=g, action=a).
  Implemented by nll_replacement8pfe_counts_cpp; α_TX = α_g[g=0] ≡ 0.

Eq. 4  (Geo-weighted CCP update — marginal CCP at state s):
  P_new(s, ·) = Σ_g w(g | s) · softmax(v_M(s) + α_g, v_E(s), v_R(s))
  P ← (1 − λ) · P_old + λ · P_new       (CCP damping; λ = 0.6, CLAUDE.md)
  Implemented by update_ccps_geoweighted_cpp.
  Weights: w(g|s) is column-stochastic 32×18, built from controls-only obs for
  FF states and all-sample for RB states (feweightsource = "controls").

Eq. 5  (NPL outer loop):
  Inner: θ ← argmin NLL(θ; P, cache) via L-BFGS-B, max_iter = 200.
  Outer: P ← geo-weighted update at new θ.
  Converged: max|Δθ| < tol_theta AND max|ΔP| < tol_P.

Eq. 6  (AM-2002 profile-likelihood SE):
  H[i,j] = ∂² ProfileNLL(θ_raw) / ∂θ_i ∂θ_j   (central FD, h = 1e-3)
  ProfileNLL(θ_raw) = NLL(θ_raw; P*(θ_struct), cache)
    where P*(θ_struct) = equilibrium solved at structural sub-vector only
    (cached on the structural sub-vector — α perturbations re-use the cache).
  SE = sqrt(diag(H⁻¹));  for K params: SE(K) = K · SE(K_log).

Eq. 7  (Marcus-2021 external damage per reported leak event):
  E = $17,000 (Marcus 2021 §VI infant-health only; HEALTH_ONLY)
  E = $50,000 (≈ 3× for unmeasured channels;       HEALTH_PLUS_UNMEASURED)
  In model units (1 unit = $10K): E ∈ {1.7, 5.0}.

Eq. 8  (Pigouvian γ_risk scaler — CF B):
  m_ρ(E) = Σ_{s ∈ ρ} μ₀(s) · P_M^{base}(s) · h(s) · (L(s) + E) /
           Σ_{s ∈ ρ} μ₀(s) · P_M^{base}(s) · h(s) · L(s)        ρ ∈ {FF, RB}
  γ_risk,ρ^{CF-B}(E) = γ_risk,ρ · m_ρ(E)   (re-solve equilibrium per E)

Eq. 9  (Social welfare at given E and scenario):
  μ̂(s) = wall × regime QSD on baseline-α-free transition kernel
         (chain decomposition, existing compute_weighted_qsd_replacement)
  Firm surplus, real     = Σ_s μ̂(s) · V_real(s) with U_real ≡ U(θ_ref_real_8p)
  External damages, flow = Σ_s μ̂(s) · P_M(s) · h(s) · E
  Social welfare         = Firm surplus, real − External damages, flow · 1/(1−β)
  All five scenarios are re-aggregated at each E in {1.7, 5.0}.

═══════════════════════════════════════════════════
PSEUDOCODE
═══════════════════════════════════════════════════
This ticket has TWO sequential stages. Stage A is the smoke test + plumbing
patches; Stage B is the full estimation + welfare. Stage A must pass before
Stage B runs. Both stages live in ONE attempt — do not split across attempts.

────────────────────────────────────────────────────────────
STAGE A — Plumbing patches + smoke test (must pass first)
────────────────────────────────────────────────────────────

Step A.1 — Delete dead FE block:
  File: Code/Helpers/improved_estimator_OPTIMIZED.r
  Delete lines 3399 through 3623 inclusive.
    These define an obsolete AR-base 16-FE spec (CONTROL_STATES,
    FE_PARAM_NAMES, FE_FREE_STATES, FE_BASE_STATE, N_FE_PARAMS, the older
    create_estimation_config_replacement_8p_fe, npl_likelihood_replacement_8p_fe,
    npl_estimator_replacement_8p_fe). Verified by grep that nothing outside
    this range references these symbols. The canonical TX-base 17-FE spec
    starts at line 3626 ("APPEND (2026-05-08)") and survives unchanged.
  assert: line count of improved_estimator_OPTIMIZED.r decreases by 225.
  assert: grep finds zero occurrences of "N_FE_PARAMS", "FE_PARAM_NAMES",
          "FE_FREE_STATES", "FE_BASE_STATE", "CONTROL_STATES",
          "STUDY_STATES" in Code/Helpers/improved_estimator_OPTIMIZED.r
          (note: CONTROLSTATES and STUDYSTATES without underscore must remain).

Step A.2 — Patch column-name check in .build_counts_weights_8p_fe:
  File: Code/Helpers/improved_estimator_OPTIMIZED.r
  The actual obs panel CSV columns are: s_idx, state, y_it, I_replace
  (with underscores). The current code at the line that reads
    stopifnot(all(c("sidx", "state", "yit", "Ireplace") %in% names(dt)))
  rejects this. Replace the stopifnot and the subsequent uses inside this
  function so that the function:
    (a) accepts the actual column names s_idx, y_it, I_replace,
    (b) keeps internal variable names (aidx, graw, etc.) unchanged,
    (c) reads dt[, s_idx] not dt[, sidx]; dt[, y_it] not dt[, yit];
        dt[, I_replace] not dt[, Ireplace].
  After computing aidx and graw, rename or alias them so the rest of the
  function (which uses sidx and graw as data.table keys) still works.
  Minimal-touch approach: at the top of .build_counts_weights_8p_fe, after
  dt <- as.data.table(obs_panel), add:
      data.table::setnames(dt, c("s_idx","y_it","I_replace"),
                              c("sidx","yit","Ireplace"), skip_absent = TRUE)
  Then the existing stopifnot, aidx computation, and graw computation work
  unchanged. Verify no other column reads break.
  assert: no stopifnot fails when called with obs panel CSV columns as-is.

Step A.3 — Remove silent tryCatch from 04i profile_neg_loglik_8p_fe:
  File: Code/Dynamic_Model/04i_8param_FE_and_Welfare.R, §3.
  At the block currently containing
    eq <- tryCatch(
      solve_equilibrium_policy_replacement_8p(...),
      error = function(e) NULL)
  replace with a bare call:
    eq <- solve_equilibrium_policy_replacement_8p(theta_struct, cache,
                                                  config_8p_fe,
                                                  max_iter = 500, tol = 1e-7)
  Keep the !isTRUE(eq$converged) check (returns 1e10 for non-convergence,
  not for errors). CLAUDE.md forbids silent error catching; non-convergence
  is data, not error.
  assert: grep "tryCatch.*error.*function.*NULL" in 04i returns no matches.

Step A.4 — Regenerate 8p no-FE fit on observed sample:
  Required because Output/Estimation_Results/Model_Replacement_8param_observed.rds
  does not exist, and 04i depends on it for (a) FE θ initialisation,
  (b) θ_ref_real for "real-utility" welfare reference, (c) the 8p-vs-FE
  comparison table.
  Run: source("Code/Dynamic_Model/04h_Replacement_8param_Estimation.R")
    — produces Model_Replacement_8param_observed.rds AND
      Model_Replacement_8param_extended_2000plus.rds.
    — also produces 04h_Theta_Table_8param_AM_SE.{csv,tex} (acceptable side
      output; not asserted).
  This is a real estimation run (8-param L-BFGS-B on 2.3M obs + AM SE
  Hessian). Budget: ~10–30 minutes wall time.
  Log to: logs/04h_<timestamp>.log per CLAUDE.md logging block.
  assert: file.exists(file.path(OUT_FIT, "Model_Replacement_8param_observed.rds"))
  assert: readRDS(...) has $theta_raw (length 8, named) and $theta_hat (named).
  assert: $converged == TRUE.

Step A.5 — Smoke test (no optim) for FE estimator plumbing:
  Write a NEW script: Code/Dynamic_Model/04i_smoke_test.R.
  Operations (in order):
    1. Source improved_estimator_OPTIMIZED.r (this auto-compiles cpp_engine
       via the .try_source_cpp_engine() helper).
       After source, assert exists("nll_replacement8pfe_counts_cpp",
                                   mode = "function") == TRUE.
       Assert exists("update_ccps_geoweighted_cpp", mode = "function") == TRUE.
       If either is FALSE → stop("cpp_engine.cpp did not compile; STOP").
    2. Load primitives and obs panel:
         prims_obs <- readRDS(here::here("Output","Estimation_Results",
                                         "DCM_Primitives_Replacement_observed.rds"))
         obs_obs   <- fread(here::here("Data","Analysis",
                                       "dcm_obs_panel_observed.csv"))
    3. Build FE config (mirrors 04i §C2):
         cfg <- create_estimation_config_replacement_8p_fe(
                  beta=0.95, sigma2=1.0, npl_iter=200,
                  feweightsource="controls", ccp_damping_lambda=0.6,
                  epsprob=1e-12, alphabounds=c(-20,20),
                  tol_theta=1e-5, tol_P=1e-5)
       Assert cfg$n_params == 25L.
       Assert length(cfg$param_names) == 25L.
       Assert all(cfg$fe_param_names == paste0("alpha",
         c("AR","CO","ID","KS","KY","LA","MA","MD","ME","MN",
           "MO","NC","OH","OK","SD","TN","VA"))).
    4. Build cache + counts via the FE estimator's internal helpers. The
       estimator's npl_estimator_replacement_8p_fe constructs both; replicate
       the cache-building code from inside that function up to (but not
       including) the optim loop. Specifically:
         cache <- primitives copy
         cw    <- .build_counts_weights_8p_fe(obs_obs, prims_obs,
                                              cfg$feweightsource)
         cache$countsdt8pfe <- cw$countsdt
         cache$wsg8pfe      <- cw$wsg
         std_cache <- create_estimation_cache_replacement_8p(prims_obs,
                                                             obs_obs,
                                                             config_4p, cfg)
         (merge std_cache into cache as in lines 3996–4001).
       Assert dim(cache$wsg8pfe) == c(32L, 18L).
       Assert all(abs(rowSums(cache$wsg8pfe) - 1.0) < 1e-8).
       Assert nrow(cache$countsdt8pfe) == 576L.
       Assert nrow(unique(cache$countsdt8pfe[, .(sidx, graw)])) == 576L.
       Assert all(cache$countsdt8pfe$graw %in% 0:17).
       Assert all(cache$countsdt8pfe$sidx %in% 1:32).
    5. Load fit_8p from Step A.4 output. Build theta_init_fe:
         theta_init_fe <- c(fit_8p$theta_raw,
                            setNames(rep(0, 17L), cfg$fe_param_names))
       Assert length(theta_init_fe) == 25L and !anyNA(theta_init_fe).
    6. Build initial P from structural equilibrium at θ_struct_init:
         eq0 <- solve_equilibrium_policy_replacement_8p(
                  fit_8p$theta_raw, cache, cfg, max_iter=500, tol=1e-7)
       Assert isTRUE(eq0$converged).
       Assert all(abs(rowSums(eq0$P) - 1.0) < 1e-8).
       Assert all(eq0$P >= 0 & eq0$P <= 1).
       Assert all(is.finite(eq0$V)).
    7. ONE likelihood eval (do NOT loop):
         nll0 <- npl_likelihood_replacement_8p_fe(theta_init_fe, eq0$P,
                                                  cache, cfg)
       Assert is.finite(nll0) && nll0 > 0.
       Print: cat(sprintf("[smoke] nll0 = %.3f\n", nll0))
    8. ONE CCP update eval:
         P1 <- .update_ccps_geoweighted_8p_fe(theta_init_fe, eq0$P, cache, cfg)
       Assert dim(P1) == c(32L, 3L).
       Assert all(abs(rowSums(P1) - 1.0) < 1e-8).
       Assert all(P1 >= 0 & P1 <= 1).
       Assert all(is.finite(P1)).
    9. Print summary line and exit 0.
  assert: 04i_smoke_test.R runs to completion with no errors and no warnings
          beyond "Failed to compile cpp_engine.cpp" (which would have been
          caught in step 1 above).

────────────────────────────────────────────────────────────
STAGE B — Full estimation + welfare with E sensitivity
────────────────────────────────────────────────────────────

Step B.1 — Run the FE estimation:
  Source the patched Code/Dynamic_Model/04i_8param_FE_and_Welfare.R sections 1–4
  end-to-end (the unchanged-by-this-ticket bulk of the script). Log to
  logs/04i_<timestamp>.log per CLAUDE.md logging block.
  This produces:
    Output/Estimation_Results/Model_Replacement_8paramFE_observed.rds
    Output/Tables/04i_Theta_Table_8paramFE_AM_SE.{csv,tex}
    Output/Tables/04iFETableAllControls.csv
  assert: fit_fe$converged == TRUE
  assert: length(fit_fe$theta_raw) == 25L
  assert: all(c("kappa_SW","kappa_DW","K_log_SW","K_log_DW",
                "gamma_price_FF","gamma_price_RB",
                "gamma_risk_FF","gamma_risk_RB") %in% names(fit_fe$theta_raw))
  assert: all(paste0("alpha",
                c("AR","CO","ID","KS","KY","LA","MA","MD","ME","MN",
                  "MO","NC","OH","OK","SD","TN","VA"))
              %in% names(fit_fe$theta_raw))
  assert: dim(fit_fe$cache$wsg8pfe) == c(32L,18L)
  assert: nrow(fit_fe$cache$countsdt8pfe) == 576L
  assert: AM SE table CSV has 25 rows and non-NA SE for all structural params.

Step B.2 — Refactor welfare section for E sensitivity:
  Replace 04i §5 (Welfare counterfactuals A/B/C/D) with an E-parametrised
  version. Specifically:
    (a) Define the E grid once at the top of §5:
          E_GRID_DOLLARS <- c(HEALTH_ONLY = 17000, HEALTH_PLUS_UNMEASURED = 50000)
          E_GRID         <- E_GRID_DOLLARS / SCALE_FACTOR   # = c(1.7, 5.0)
        Comment: "Marcus 2021 §VI. HEALTH_ONLY = $17K (infant LBW + preterm,
                  conservative). HEALTH_PLUS_UNMEASURED = $50K (≈3× for
                  adult/cancer/ecological/cognitive-development channels
                  not measured by Marcus)."
    (b) Solve E-independent equilibria ONCE:
          eq_base, eq_A, eq_C, eq_D (none use E).
        For each E in E_GRID:
          compute m_FF(E), m_RB(E) from eq_base + E,
          solve eq_B at γ_risk scaled by m_ρ(E),
          aggregate social welfare for {baseline, A, B(E), C, D} at this E.
    (c) Wrap compute_social_welfare_8p to accept Evec as argument; replace
        the module-level Evec with the function argument. Signature:
          compute_social_welfare_8p(eq_obj, label, cache, config_8p, Evec, pv_factor)
        All callers in Step B.2 pass the current loop's E expanded to
        a length-32 vector: rep(E, length(prims_obs$L_vec)).
    (d) Output table 04iWelfareSocial03Style.csv now has columns:
          E_label, E_dollars, Scenario, Converged, AvgP_M, AvgP_E, AvgP_R,
          ExpPrivateLossFlow, ExpExternalDamFlow, FirmSurplusPerceived,
          FirmSurplusReal, SocialWelfare, GovtCostImplied
        Long format: 5 scenarios × 2 E values = 10 rows.
    (e) Output table 04iWelfareSocialDecomp.csv now has columns:
          E_label, E_dollars, Scenario, dFirmReal, dExternalPV, dSocialWelfare
        4 non-baseline scenarios × 2 E values = 8 rows.
    (f) Output table 04iQSDmubyScenario.csv unchanged in structure; add
        a Pigouvian-CF-B duplication: rows for both B(17K) and B(50K)
        with Scenario column labelled "B_Pigouvian_internalizeE_E17K" and
        "B_Pigouvian_internalizeE_E50K". Other scenarios are E-independent
        so report each once with Scenario = original label.
  assert: all output CSVs in §5 have at least one row per (E, Scenario) pair.
  assert: SocialWelfare(B, E=17K) ≥ SocialWelfare(baseline, E=17K) IF the
          Pigouvian intuition holds; otherwise emit a WARNING with the
          baseline-vs-CF-B social welfare values printed. Do not abort.
  assert: For E_label == "HEALTH_ONLY", the equilibrium of CF B has
          theta differing from baseline only in gamma_risk_FF and gamma_risk_RB.
  assert: Output/Figures/04iWelfareSocialDecomposition.png is two facets
          (one per E_label), not one panel. ggplot facet_wrap(~ E_label).

Step B.3 — Per-cell welfare table (§6 of current 04i):
  Keep §6 essentially as-is (population-weighted by n_cell, no E dependence
  in this table — these are model-unit ΔV and ΔP comparisons). One change:
  for CF B specifically, use eq_B(E = HEALTH_ONLY) as the comparison point
  (and note this in a comment line in the CSV header). Add a column
  E_label = "HEALTH_ONLY" to all rows for transparency.
  assert: nrow(welfare_long) == 32 × 4 == 128.

Step B.4 — 8p vs 8p+FE comparison table (§9 of current 04i):
  Unchanged. Reads fit_8p (now exists from Step A.4) and fit_fe. Saves
  Output/Tables/04i_Compare_8p_vs_8pFE.csv.
  assert: file exists, 8 rows.

Step B.5 — Print final summary per CLAUDE.md output rules:
  "Estimator: npl_estimator_replacement_8p_fe | Sample: observed |
   Converged: TRUE at iter N | LL: <val>
   theta_hat: <8 names+values; 17 alphas printed as table>
   Elapsed: <N>s | Saved: <list of all output files>"

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Source order matters: source improved_estimator_OPTIMIZED.r FIRST. The file
  itself attempts to compile cpp_engine.cpp via .try_source_cpp_engine().
  If compilation fails, fallback to pure-R likelihood is automatic but is
  100× slower; in Stage A.5 we hard-stop if compilation fails (the FE block
  uses exists(...) to dispatch, so the compile failure is silent otherwise).
- Indexing: R is 1-based throughout. Inside cpp_engine.cpp, sidx is converted
  to 0-based and graw is already 0-based (graw=0 ≡ TX). The alphacpp vector
  is length 18, R-indexed [1..18]; C++ accesses alphacpp[g] where g=0..17.
  alphacpp[1] (R) = alphacpp[0] (C++) = 0 (TX FE fixed at 0).
- data.table: .build_counts_weights_8p_fe uses sidx, graw as keys. After
  Step A.2 setnames, the internal names are sidx/yit/Ireplace; do not
  rename downstream.
- Hard-stop policy: NO tryCatch returning NULL anywhere in code modified
  by this ticket. Non-convergence (eq$converged == FALSE) is signalled by
  returning 1e10 from the likelihood; everything else (NA in P, dimension
  mismatch, missing file) must stop().
- Logging: per CLAUDE.md, every script with expected runtime > 1 minute
  must open a sink-based log via the block in CLAUDE.md "LOGGING (any
  script > 1 min)". This applies to 04h (A.4) and 04i (B.1).
- Tolerances are inherited from CLAUDE.md and the existing config builder:
  beta=0.95, sigma2=1.0, tol_theta=1e-5, tol_P=1e-5, max_npl_iter=200,
  ccp_damping_lambda=0.6, alphabounds=c(-20,20), eps_prob=1e-12.
  DO NOT modify any of these.
- DO NOT touch cpp_engine.cpp. Both FE Rcpp exports are already correct
  and consistent with the R likelihood/CCP update.
- DO NOT introduce new dependencies. data.table, Matrix, here, ggplot2,
  gridExtra are the allowed set (already loaded by 04i).
- Equilibrium cache in §3 ProfileNLL: keep the local environment + key
  on structural-θ sub-vector. Do not extend the key to include α — the
  point is that α perturbations skip eq re-solve.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════
Stage A
- [ ] Code/Helpers/improved_estimator_OPTIMIZED.r line count = (previous − 225)
- [ ] grep "N_FE_PARAMS|FE_PARAM_NAMES|FE_FREE_STATES|FE_BASE_STATE|CONTROL_STATES|STUDY_STATES" returns 0 hits in improved_estimator_OPTIMIZED.r
- [ ] Step A.2 setnames present at top of .build_counts_weights_8p_fe
- [ ] grep "tryCatch.*error.*function.*NULL" in 04i returns 0 hits
- [ ] Output/Estimation_Results/Model_Replacement_8param_observed.rds exists
- [ ] readRDS(Model_Replacement_8param_observed.rds)$converged == TRUE
- [ ] Code/Dynamic_Model/04i_smoke_test.R exists and runs to completion exit 0
- [ ] cpp_engine functions exist after sourcing the library

Stage B
- [ ] Output/Estimation_Results/Model_Replacement_8paramFE_observed.rds exists
- [ ] readRDS(...)$converged == TRUE and length($theta_raw) == 25
- [ ] Output/Tables/04i_Theta_Table_8paramFE_AM_SE.csv has 25 rows
- [ ] Output/Tables/04i_Theta_Table_8paramFE_AM_SE.tex exists and is parseable LaTeX
- [ ] Output/Tables/04iFETableAllControls.csv has 17 rows
- [ ] Output/Tables/04iWelfareSocial03Style.csv has 10 rows (5 scenarios × 2 E)
- [ ] Output/Tables/04iWelfareSocialDecomp.csv has 8 rows (4 non-base × 2 E)
- [ ] Output/Tables/04i_Welfare_PerCell.csv has 128 rows
- [ ] Output/Tables/04i_Compare_8p_vs_8pFE.csv has 8 rows
- [ ] Output/Figures/04iWelfareSocialDecomposition.png exists, facet by E_label
- [ ] Output/Figures/04i_Welfare_DeltaV_byCell.png exists
- [ ] All P matrices in cached eq objects sum to 1 within 1e-8 per row
- [ ] No silent error catching anywhere in modified files
- [ ] Logs written to logs/04h_*.log and logs/04i_*.log
- [ ] Constants match CLAUDE.md: beta=0.95, sigma2=1.0, tol_theta=1e-5,
      tol_P=1e-5, max_npl_iter=200, ccp_damping_lambda=0.6, eps_prob=1e-12

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[Filled by reviewer after each implementation attempt. Leave blank until first attempt.]

### Attempt 1 — [DATE]
Transcript: 001_transcript.txt
Result: [PASS | TRANSLATION_FAIL | PSEUDOCODE_FAIL]

Criteria:
- [ ] [criterion]: [result]

---
### Attempt 2 — [DATE]
[same format]
