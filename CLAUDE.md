# CLAUDE.md — UST Insurance Regime
# Updated: 2026-05-13
# Location: repo root

═══════════════════════════════════════════════════
SESSION START
═══════════════════════════════════════════════════
Read: .claude/HANDOFF.md if present
Skills: load selectively from skills/ based on today's task
Agents: .claude/agents/ — architect (Opus), reviewer (Sonnet)
Stale — do NOT load: npl-ust-spec.md, cpp-engine-spec.md (rebuild after 04i complete)

═══════════════════════════════════════════════════
TOKEN BUDGET — NON-NEGOTIABLE
═══════════════════════════════════════════════════
Anthropic tokens (this session) → reasoning, math, economic logic, pseudocode only
OpenRouter tokens              → all R/Rcpp writing, running, output, file ops

NEVER burn Anthropic tokens on:
  reading large R files line by line | running code | checking output
  reformatting results | any mechanical work R1 can do from pseudocode

Close this session when: pseudocode is approved OR Sonnet signs off an implementation
  → NOT based on context % (that is an emergency trigger only at 70%)

═══════════════════════════════════════════════════
RESEARCH WORKFLOW — 3 SESSIONS
═══════════════════════════════════════════════════
1. ARCHITECT  (this session, Opus)
   You + Opus debate economics → agree on math → Opus writes pseudocode
   → you approve → STOP. Open OpenRouter session.
   Agent: .claude/agents/architect.md

2. IMPLEMENT  (OpenRouter / R1 — separate PowerShell tab)
   R1 reads pseudocode → asks R-implementation questions → writes code → runs it
   You watch and answer. R-specific questions only.
   If R1 asks about economics or math → STOP. Back to step 1.
   Runner: .claude/run_coder.ps1
   Template: .claude/TICKETS/spec_template.md

3. REVIEW  (this session, Sonnet)
   Sonnet reads pseudocode + code + run output
   Checks: does R code faithfully implement pseudocode? Mechanical only.
   Output: PASS | TRANSLATION_FAIL | PSEUDOCODE_FAIL
   Agent: .claude/agents/reviewer.md

Retry cap: 2 R1 attempts per ticket max. Escalate to Opus on 3rd failure.

═══════════════════════════════════════════════════
PRESENTATION-POLISH EXCEPTION  (Opus may bypass R1)
═══════════════════════════════════════════════════
Opus is permitted to edit and run R directly — no R1 ticket — ONLY when ALL
of the following hold:
  - The change is pure presentation: ggplot styling, axis units, color/font,
    LaTeX table sprintf, label/legend strings.
  - No estimation logic touched: nothing in Bellman, V inversion, CCP update,
    NPL outer loop, equilibrium solver, transition matrices, primitives,
    tolerances, or any code path that feeds Output/Estimation_Results/*.rds.
  - The script is a downstream consumer of an existing .rds fit (e.g.
    04j_4p_Welfare.R Section 14 bar-chart code) — not the estimator itself.
  - The user has explicitly waived the architect role for the turn
    ("you are not the architect right now", or equivalent).

If ANY of those fail, fall back to the 3-session workflow above. In particular:
unit conversions ARE polish; choice of welfare formula is NOT.

Each invocation must be logged in HANDOFF.md as "Polish-direct: <files>".

═══════════════════════════════════════════════════
UNIT CONVENTION  (do not forget — load-bearing for slides)
═══════════════════════════════════════════════════
1 model unit  =  annual net revenue R  =  \$10,000 / yr / facility.
  - Set in 04b_Replacement_Panel_Prep.R: `SCALE_FACTOR <- 10000`; L_vec, P_vec
    divided by 10,000 at panel-prep time.
  - Flow utility leading constant `u_M <- 1 + gp*P_vec - gr*hazard_loss`:
    the `1` IS R, normalized.
  - All theta entries, V, firm_surplus, ExternalPV, GovtOutlayPV, SocialWelfare
    are in this $10K scale. Multiply by 10,000 for $/facility (PV).
  - Slide-facing welfare numbers (04j outputs) are converted to full $/facility
    inside the figure/table code, not at Bellman time.

═══════════════════════════════════════════════════
SERVER / STORAGE PATHS  (ucbare2; verified 2026-06-24)
═══════════════════════════════════════════════════
Scripts authored on local laptop, RUN on server ucbare2 (Windows). Both clones
track origin/main. Server data physically lives on D: now (C: was full/shared);
the whole C:\Users\kalebkja profile is junctioned to D:\shares\Users\kalebkja\,
so C: paths work transparently. Verified targets:
  repo     C:\Users\kalebkja\ust_ins_move_to_github -> D:\shares\Users\kalebkja\ust_ins_move_to_github
  dewey    C:\Users\kalebkja\dewey-downloads        -> D:\shares\Users\kalebkja\dewey-downloads
  reduced  ...\dewey-downloads\_reduced_near_ust\<dataset>_near_ust\part_*.parquet
Keep large data on D: (old "C: only / D: not allowed" rule is dead). DEWEY_Z_ROOT
keeps the C: junction path (routes to D:). Local laptop inspects server outputs via
Z:\C_Drive_Portal\. Dewey downloads + *.parquet are gitignored — never commit them.
Admin: Gary -> Eric (2026-07-01).
Detail: memory server_dewey_data_moved_to_d + git_sync_local_server_topology.

═══════════════════════════════════════════════════
UST MODEL
═══════════════════════════════════════════════════
DCM: maintain(1) / exit(2) / retrofit-replace(3)
States: 9 age bins × 2 wall (SW/DW) × 2 regime (FF/RB) = 36
        replacement model: 32 (8 bins × 2 × 2)
beta=0.95 | sigma2=1.0

Model family:
  npl_estimator()                   theta=(phi_tilde, kappa_tilde)
  npl_estimator_nested()            + sigma1
  npl_estimator_replacement()       4p: kappa_exit, K_log, gamma_price, gamma_risk
  npl_estimator_replacement_8p()    8p: + kappa_DW, K_log_DW, _RB variants
  npl_estimator_replacement_8p_fe() 25p: + alpha_AR..alpha_VA (17 state FE)
  em_npl_estimator()                K unobserved types + pi_g

FE invariants:
  alpha_TX ≡ 0. Estimate alpha_AR..alpha_VA (17 params).
  FE enters uM ONLY. Not exit, not replace, not equilibrium solver.
  graw: 0=TX, 1..17=CONTROL_STATES. R: alpha_vec[graw+1].

═══════════════════════════════════════════════════
KEY FILES
═══════════════════════════════════════════════════
Code/Helpers/improved_estimator_OPTIMIZED.r    ← full estimation library
Code/Helpers/cpp_engine.cpp                    ← Rcpp; sourceCpp before estimation
Code/Dynamic_Model/04i_8param_FE_and_Welfare.R ← main 8p+FE pipeline
Output/Estimation_Results/                     ← all .rds outputs
.claude/NPL_REFERENCE.md                       ← math reference + code map +
                                                 invariant checklist. LOAD
                                                 before any spec involving flow
                                                 utilities, V inversion, CCP
                                                 updates, or CF equilibrium.
                                                 ~1500 tokens; cheaper than
                                                 re-deriving the algorithm.

DO NOT RUN:  04g_Fit_Numbers_and_CIs.R | 04h_Replacement_8param_Estimation.R
DO NOT EDIT: cpp_engine.cpp directly — append-only, full protocol required

C++ EXPORTS:
  e_step_cpp()                    EM E-step (~50x speedup)
  aggregate_weighted_counts_cpp() EM M-step (~100x speedup)
  compute_inclusive_value_cpp()   nested logit inclusive value
  simulate_panel_cpp()            panel simulation
  nllreplacement8pfecountscpp()   8p+FE aggregated NLL (576 rows)
  updateccpsgeoweightedcpp()      geo-weighted CCP update

═══════════════════════════════════════════════════
TOLERANCES — NEVER CHANGE WITHOUT APPROVAL
═══════════════════════════════════════════════════
tol_theta=1e-8 | tol_P=1e-7 | max_npl_iter=600 | eps_prob=1e-10
tol_em=1e-5    | max_em_iter=300
replacement: npl_iter=200 | sigma2=1.0 | ccp_damping_lambda=0.6

═══════════════════════════════════════════════════
R CODING STYLE
═══════════════════════════════════════════════════
LANGUAGE SCOPE (2026-06-24): the R-only style below governs the STRUCTURAL
ESTIMATOR (Bellman/NPL/CF + anything feeding Output/Estimation_Results). GIS /
POI / data-engineering work may use the best tool (Python + geopandas/duckdb-
spatial likely). For ANY non-R script: (1) inline R->language mapping comments so
the logic reads in R terms, and (2) a companion markdown doc (what it does +
language setup/gotchas). Detail: memory feedback_nonr_language_requires_mapping_and_md.

Naming:   snake_case only. No camelCase. No dots.
Errors:   Hard propagation only.
  FORBIDDEN: tryCatch(expr, error=function(e) NULL) | try(expr, silent=TRUE)
  REQUIRED:  let errors surface — real stop location must be visible
Sections: cat("=== SECTION ===\n") at every logical block
Packages: data.table | Matrix sparse (no as.matrix()) | here() | optim() L-BFGS-B

LOGGING (any script > 1 min):
```r
.log_path <- here::here("logs", paste0(
  tools::file_path_sans_ext(basename(rstudioapi::getSourceEditorContext()$path)),
  "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive=TRUE, showWarnings=FALSE)
.log <- file(.log_path, open="wt")
sink(.log, type="output"); sink(.log, type="message", append=TRUE)
on.exit({ sink(type="output"); sink(type="message"); close(.log) }, add=TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
    .log_path, rstudioapi::getSourceEditorContext()$path, R.version.string, getwd()))
```
Loop checkpoint: cat(sprintf("[%s] iter %d: LL=%.3f dTh=%.2e\n", format(Sys.time(),"%H:%M:%S"), iter, ll, d_theta))
Crash log to LLM: Get-Content logs\SCRIPT_*.log -Tail 60 | clip

ASSERTIONS:
```r
stopifnot(all(P>=0), all(P<=1), all(abs(rowSums(P)-1)<1e-8), !anyNA(P))
stopifnot(all(is.finite(V)), all(V > -1e6), all(V < 1e6))
stopifnot(all(abs(Matrix::rowSums(F_m)-1)<1e-8))
stopifnot(all(abs(rowSums(w)-1)<1e-6), all(w>=0))
if (opt$convergence!=0) warning(sprintf("optim code %d at iter %d", opt$convergence, it))
```

═══════════════════════════════════════════════════
OUTPUT RULES
═══════════════════════════════════════════════════
Never print: full matrices, CCPs, V, weights, panel data.
After estimation print only:
  Estimator: [name] | Sample: [label] | Converged: T/F at iter N | LL: [val]
  theta_hat: [names+values] | Elapsed: [N]s | Saved: [path]
Save: Output/Estimation_Results/[model]_[sample]_[YYYYMMDD].rds

═══════════════════════════════════════════════════
SESSION CHECKPOINT
═══════════════════════════════════════════════════
Write to .claude/HANDOFF.md at each stopping point:
  === CHECKPOINT: [name] ===
  Modified/Done/Results/Remaining/Issues
  Resume: "Load CLAUDE.md. [state in one sentence]. Today: [task]"
  ==========================

═══════════════════════════════════════════════════
APPROVAL GATES — HARD STOPS
═══════════════════════════════════════════════════
Stop and wait for explicit approval before:
  1. Handing pseudocode to R1 (you must approve the math)
  2. Running Sonnet review (R1 session complete)
  3. Running any estimation script
  4. Changing any tolerance/constant
  5. Appending to cpp_engine.cpp (full protocol required)
  6. Deviating from approved pseudocode
  7. Saving a rebuilt CLAUDE.md (show draft first)
  8. Writing pseudocode for Bellman / V inversion / CCP updates / NPL outer
     loop / CF equilibrium WITHOUT pulling NPL_REFERENCE.md into context
     (or citing the relevant identity/section from memory). Architect must
     reference the specific section being used before pseudocode goes to
     coder. Prevents subtle eq-(2.3.25)-shaped bugs.
