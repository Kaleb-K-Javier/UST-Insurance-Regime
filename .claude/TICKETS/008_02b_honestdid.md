# TICKET 008 — 02b refactor: HonestDiD sensitivity (raw package calls)
# Created: 2026-05-19
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0
# Depends on: 005 (helper file), 006 (ES fits .rds)

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
The standard event-study (Ticket 006) assumes parallel pre-trends.
HonestDiD (Rambachan-Roth 2023) inverts the question: GIVEN a
bound on the magnitude of pre-trend violations the analyst is
willing to entertain, what are the robust post-period confidence
intervals?

We report sensitivity under two bound families:
  - **Relative magnitude (RM)** — post-period violation ≤ M̄ × max
    pre-period violation. The "breakdown M̄" is the smallest M̄ at
    which the post-period CI no longer excludes zero.
  - **Smoothness (SD)** — successive deviations from parallel
    trends are bounded by M, smoothing out plausibly-jumpy
    pre-trend artifacts. Breakdown M reported analogously.

The original 02b in-script HonestDiD wrappers were brittle (per
user). This ticket replaces them with thin direct calls to
`HonestDiD::createSensitivityResults*` on the saved ES fits from
Ticket 006, saves raw package outputs to .rds, and produces only
a lightweight summary CSV. All sensitivity plots and the report
LaTeX summary table are produced INLINE in the report .qmd from
the .rds artifacts — not in this script.

This ticket covers the MAIN specification only (matching col 7 of
the stepped DiD), for both OLS and Cox, both at the default
reference ref = -1. ref = -2 robustness is in the Ticket 006 .rds
but is not subjected to HonestDiD here (user can add later if
needed; rare to do sensitivity on both ref periods for advisor
meetings).

═══════════════════════════════════════════════════
MATH
═══════════════════════════════════════════════════

Inputs needed by HonestDiD (verbatim from the package signature):

  betahat        — numeric vector of event-study coefficients EXCLUDING
                   the reference period
  sigma          — variance-covariance matrix of betahat (cluster-robust)
  numPrePeriods  — number of pre-period τs (τ < ref)
  numPostPeriods — number of post-period τs (τ ≥ 0)
  l_vec          — weighting vector for the post-period summary; we use
                   simple average: l_vec = rep(1/numPostPeriods, numPostPeriods)
  Mvec / Mbarvec — sensitivity grid

Eq. 1 — Smoothness (SD) sensitivity bound parameter family:
  Allows the (k+1)-th pre-period bias to deviate from the linear
  extrapolation of the k-th by at most M:
    |Δ^2 δ_τ| ≤ M for all τ
  HonestDiD::createSensitivityResults(..., Mvec = M_grid, method = "C-LF")

Eq. 2 — Relative magnitude (RM) sensitivity bound:
  Allows the maximum post-period violation to be at most M̄ times the
  maximum pre-period violation:
    max_{τ≥0} |δ_τ| ≤ M̄ · max_{τ<0} |δ_τ|
  HonestDiD::createSensitivityResults_relativeMagnitudes(..., Mbarvec = M_grid)

Eq. 3 — Breakdown:
  bd(family) := min{M (or M̄) : CI under that bound contains 0}
  If no grid point yields a CI containing 0, report bd = ">max_grid".

Eq. 4 — Sigma extraction:
  OLS:  Σ̂ = vcov(m_es_ols, type = "clustered")   [feols cluster-robust]
  Cox:  Σ̂ = vcov(m_es_cox)                       [coxph cluster-robust
                                                   sandwich; cluster = state
                                                   was set at fit time]
  Subset to rows/cols matching the τ ≠ ref columns of betahat.

  IMPORTANT: HonestDiD expects the betahat vector and sigma matrix
  to be in chronological order (τ = -L, -L+1, ..., -1 [excluded],
  0, 1, ..., +H). The reference period τ = ref is dropped from
  betahat and sigma BEFORE the call.

═══════════════════════════════════════════════════
PSEUDOCODE
═══════════════════════════════════════════════════

═══ Step 0 — Confirm helper file is sufficient (no extensions) ═══

This ticket does NOT extend reduced_form_utils.R. All work is
contained in the main script. If a small helper is needed mid-implementation, add it to the script as a script-local
function; do NOT modify the shared helper file.

═══ Step 1 — Create main script ═══

NEW FILE: Code/Analysis/02f_HonestDiD.R

Layout:
  1. Logging block → logs/02f_HonestDiD_<TIMESTAMP>.log
  2. Library loads (data.table, HonestDiD, here, doParallel)
  3. source(here("Code/Helpers/reduced_form_utils.R"))
  4. Load the Ticket 006 .rds:
       es_fits <- readRDS(file.path(here::here("Output", "Estimation_Results"),
                                     "Event_Study_Fits_active_at_treatment.rds"))
  5. Set up parallel:
       N_CORES <- max(1L, parallel::detectCores() - 1L)
       doParallel::registerDoParallel(cores = N_CORES)

═══ Step 2 — Set sensitivity grids (top of script as named constants) ═══

  HD_MVEC_SD   <- seq(0,   0.08, by = 0.005)  # SD smoothness bound grid (28 points)
  HD_MBAR_RM   <- seq(0,   3,    by = 0.1)    # RM relative-magnitude grid (31 points)
  HD_ALPHA     <- 0.05L
  HD_METHOD    <- "C-LF"                       # conditional / least-favorable

  Document choices inline in script comments (M_max = 0.08 in LPM
  units; M̄_max = 3 covers up to 3× the worst pre-period violation).

═══ Step 3 — Build inputs for HonestDiD from each ES fit ═══

Define a script-local helper:

  build_hd_inputs <- function(es_dt, ref_period = -1L, model, model_type) {
    # es_dt          — coefficient data.table from extract_es_coef_dt_{ols,cox}
    #                  (already includes the ref row with estimate=0, se=0)
    # ref_period     — the τ that is the omitted reference
    # model          — the fitted model (used for vcov)
    # model_type     — "ols" or "cox"

    # Drop reference row from betahat
    keep_dt <- es_dt[rel_year != ref_period][order(rel_year)]
    betahat <- setNames(keep_dt$estimate, as.character(keep_dt$rel_year))

    # Extract full vcov
    V_full <- if (model_type == "ols") {
      vcov(model, type = "clustered")
    } else {  # cox
      vcov(model)   # robust sandwich set at fit time
    }

    # Identify column names corresponding to each τ ≠ ref
    # OLS fixest names: "rel_year::-3:texas_treated"
    # Cox names: "es_tau_minus_3" or "es_tau_3"
    if (model_type == "ols") {
      pat <- "^rel_year::(-?[0-9]+):texas_treated$"
      col_names <- grep(pat, rownames(V_full), value = TRUE)
      tau_of_col <- as.integer(sub(pat, "\\1", col_names))
    } else {
      pat_minus <- "^es_tau_minus_([0-9]+)$"
      pat_pos   <- "^es_tau_([0-9]+)$"
      minus_names <- grep(pat_minus, rownames(V_full), value = TRUE)
      pos_names   <- grep(pat_pos,   rownames(V_full), value = TRUE)
      pos_names   <- setdiff(pos_names, minus_names)
      col_names   <- c(minus_names, pos_names)
      tau_of_col  <- c(-as.integer(sub(pat_minus, "\\1", minus_names)),
                        as.integer(sub(pat_pos,   "\\1", pos_names)))
    }
    ord <- order(tau_of_col)
    col_names <- col_names[ord]; tau_of_col <- tau_of_col[ord]

    Sigma <- V_full[col_names, col_names, drop = FALSE]
    rownames(Sigma) <- colnames(Sigma) <- as.character(tau_of_col)

    # Reorder betahat to match Sigma's order
    betahat <- betahat[as.character(tau_of_col)]

    numPre  <- sum(tau_of_col < ref_period)
    numPost <- sum(tau_of_col >= 0)
    l_vec   <- rep(1 / numPost, numPost)

    list(betahat = betahat, sigma = Sigma,
         numPrePeriods = numPre, numPostPeriods = numPost,
         l_vec = l_vec, ref_period = ref_period,
         tau = tau_of_col, model_type = model_type)
  }

═══ Step 4 — Compute point estimates + CIs for the post-period average ═══

  compute_point_estimate <- function(hd_inputs) {
    # Full l vector (zeros for pre, l_vec for post)
    l_full <- c(rep(0, hd_inputs$numPrePeriods),
                hd_inputs$l_vec)
    pe   <- as.numeric(l_full %*% hd_inputs$betahat)
    pe_se <- sqrt(as.numeric(t(l_full) %*% hd_inputs$sigma %*% l_full))
    list(point_est = pe,
         point_se  = pe_se,
         point_ci_lo = pe - 1.96 * pe_se,
         point_ci_hi = pe + 1.96 * pe_se)
  }

═══ Step 5 — Run HonestDiD calls ═══

For each model (OLS, Cox) at ref = -1:

  hd_ols_inputs <- build_hd_inputs(
    es_fits$es_dt$ols_ref_m1,
    ref_period = -1L,
    model      = es_fits$m_es_ols$main_ref_m1,
    model_type = "ols")

  hd_ols_point <- compute_point_estimate(hd_ols_inputs)

  # Drop ref column from betahat/sigma before passing to package
  # (build_hd_inputs already dropped the ref row; we pass as-is)

  hd_ols_sd <- HonestDiD::createSensitivityResults(
                  betahat        = hd_ols_inputs$betahat,
                  sigma          = hd_ols_inputs$sigma,
                  numPrePeriods  = hd_ols_inputs$numPrePeriods,
                  numPostPeriods = hd_ols_inputs$numPostPeriods,
                  l_vec          = hd_ols_inputs$l_vec,
                  Mvec           = HD_MVEC_SD,
                  method         = HD_METHOD,
                  alpha          = HD_ALPHA,
                  parallel       = TRUE)

  hd_ols_rm <- HonestDiD::createSensitivityResults_relativeMagnitudes(
                  betahat        = hd_ols_inputs$betahat,
                  sigma          = hd_ols_inputs$sigma,
                  numPrePeriods  = hd_ols_inputs$numPrePeriods,
                  numPostPeriods = hd_ols_inputs$numPostPeriods,
                  l_vec          = hd_ols_inputs$l_vec,
                  Mbarvec        = HD_MBAR_RM,
                  method         = HD_METHOD,
                  alpha          = HD_ALPHA,
                  parallel       = TRUE)

  # Repeat for Cox
  hd_cox_inputs <- build_hd_inputs(
    es_fits$es_dt$cox_ref_m1, -1L,
    es_fits$m_es_cox$main_ref_m1, "cox")
  hd_cox_point  <- compute_point_estimate(hd_cox_inputs)
  hd_cox_sd     <- HonestDiD::createSensitivityResults(...)         # same args
  hd_cox_rm     <- HonestDiD::createSensitivityResults_relativeMagnitudes(...)

  Log per call: model (ols/cox), bound type (SD/RM), grid length,
  elapsed seconds.

assert: all 4 sensitivity-results objects (hd_{ols,cox}_{sd,rm})
        are data.frames or compatible with data.table conversion
        and contain columns for lb, ub, and either M or Mbar

═══ Step 6 — Compute breakdown values ═══

  compute_breakdown <- function(res, grid_max) {
    dt <- as.data.table(res)
    mcol <- intersect(names(dt), c("M", "Mbar"))
    if (!length(mcol)) return(NA_real_)
    setnames(dt, mcol, "M_val")
    crosses <- dt[lb <= 0 & ub >= 0, M_val]
    if (!length(crosses)) NA_real_ else min(crosses)
  }

  bd_ols_sd <- compute_breakdown(hd_ols_sd, max(HD_MVEC_SD))
  bd_ols_rm <- compute_breakdown(hd_ols_rm, max(HD_MBAR_RM))
  bd_cox_sd <- compute_breakdown(hd_cox_sd, max(HD_MVEC_SD))
  bd_cox_rm <- compute_breakdown(hd_cox_rm, max(HD_MBAR_RM))

═══ Step 7 — Save raw outputs to .rds ═══

  saveRDS(
    list(
      ols = list(
        inputs       = hd_ols_inputs,
        point        = hd_ols_point,
        sd_results   = hd_ols_sd,
        rm_results   = hd_ols_rm,
        breakdown    = list(sd = bd_ols_sd, rm = bd_ols_rm)
      ),
      cox = list(
        inputs       = hd_cox_inputs,
        point        = hd_cox_point,
        sd_results   = hd_cox_sd,
        rm_results   = hd_cox_rm,
        breakdown    = list(sd = bd_cox_sd, rm = bd_cox_rm)
      ),
      config = list(
        ref_period      = -1L,
        Mvec_sd         = HD_MVEC_SD,
        Mbarvec_rm      = HD_MBAR_RM,
        alpha           = HD_ALPHA,
        method          = HD_METHOD,
        es_fits_source  = "Event_Study_Fits_active_at_treatment.rds",
        ticket          = "008",
        built_on        = Sys.time()
      )
    ),
    file.path(here::here("Output", "Estimation_Results"),
              "HonestDiD_main_active_at_treatment.rds")
  )

═══ Step 8 — Lightweight summary CSV ═══

  fmt_bd <- function(x, grid_max) if (is.na(x)) sprintf(">%.3f", grid_max) else sprintf("%.4f", x)

  hd_summary <- data.table(
    model       = c("OLS", "Cox"),
    point_est   = c(hd_ols_point$point_est,   hd_cox_point$point_est),
    point_se    = c(hd_ols_point$point_se,    hd_cox_point$point_se),
    point_ci_lo = c(hd_ols_point$point_ci_lo, hd_cox_point$point_ci_lo),
    point_ci_hi = c(hd_ols_point$point_ci_hi, hd_cox_point$point_ci_hi),
    breakdown_sd_raw = c(bd_ols_sd, bd_cox_sd),
    breakdown_sd_fmt = c(fmt_bd(bd_ols_sd, max(HD_MVEC_SD)),
                          fmt_bd(bd_cox_sd, max(HD_MVEC_SD))),
    breakdown_rm_raw = c(bd_ols_rm, bd_cox_rm),
    breakdown_rm_fmt = c(fmt_bd(bd_ols_rm, max(HD_MBAR_RM)),
                          fmt_bd(bd_cox_rm, max(HD_MBAR_RM)))
  )

  fwrite(hd_summary,
         file.path(OUTPUT_TABLES, "T_HonestDiD_Summary.csv"))

═══ Step 9 — Print summary block & close ═══

Log:
  Model | point | 95%CI | bd_SD | bd_RM
followed by OUTPUT SUMMARY footer.

NOTE TO REPORT AUTHOR: this script does NOT produce LaTeX tables
or sensitivity-plot figures. Those are produced INLINE in the
.qmd from the .rds output. Per user requirement: "in line of the
scprit pull out what we need to make figures and tables."

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Script path: Code/Analysis/02f_HonestDiD.R
- Sources Code/Helpers/reduced_form_utils.R (no extensions needed)
- Required packages: data.table, HonestDiD, here, doParallel
  - Install if missing: install.packages("HonestDiD")
- HonestDiD has multiple methods (C-F, C-LF, FLCI, Conditional).
  Use "C-LF" (conditional / least-favorable) — the recommended
  default in the Rambachan-Roth paper for general use.
- The parallel=TRUE argument inside createSensitivityResults*
  uses the foreach backend registered via doParallel. Register
  BEFORE calling the package functions.
- Cox vcov: coxph(..., cluster = state) automatically returns the
  Lin-Wei robust sandwich V̂ from vcov(). Do NOT pass type =
  "clustered" — that argument is fixest-specific. The build_hd_inputs
  helper handles this branch via model_type.
- Hard error propagation: no tryCatch silencing failed HonestDiD
  calls. If a call legitimately fails (e.g., singular Σ̂), let it
  surface; do not pretend success with NULL.
- This script depends on Ticket 006's .rds. If that file is missing,
  fail loudly with an informative message naming the file and the
  upstream ticket (006).
- Do NOT re-run the ES regressions in this script. Use the saved fits.
- Expected wall time: < 5 minutes total on the server (HonestDiD
  with parallel=TRUE on a small ES grid is fast).

═══════════════════════════════════════════════════
DELIVERABLES — ENUMERATED
═══════════════════════════════════════════════════

(1) NEW FILE: Code/Analysis/02f_HonestDiD.R
    Per Steps 1-9. Runnable end-to-end from a fresh R session
    PROVIDED Ticket 006's .rds exists at the expected path.

(2) Output/Estimation_Results/HonestDiD_main_active_at_treatment.rds
    R list per Step 7 schema. Required top-level names:
      ols    (list with: inputs, point, sd_results, rm_results, breakdown)
      cox    (list with same 5 sub-elements)
      config (list with 7 sub-elements per Step 7)
    Required structure of each `sd_results` / `rm_results` element:
      either a data.frame or as.data.table()-compatible object with
      at minimum columns: lb (num), ub (num), and one of {M, Mbar} (num)

(3) Output/Tables/T_HonestDiD_Summary.csv
    2 rows + header, 9 columns:
      model (chr), point_est (num), point_se (num),
      point_ci_lo (num), point_ci_hi (num),
      breakdown_sd_raw (num, NA if no crossing),
      breakdown_sd_fmt (chr — pretty-printed),
      breakdown_rm_raw (num),
      breakdown_rm_fmt (chr)

(4) Log file: logs/02f_HonestDiD_<TIMESTAMP>.log

NOT PRODUCED HERE (produced inline in .qmd from (2)):
  - sensitivity plot figures
  - HonestDiD LaTeX summary table

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA — MECHANICAL
═══════════════════════════════════════════════════

MAIN SCRIPT (1):
- [ ] Code/Analysis/02f_HonestDiD.R exists; runs end-to-end given
      Ticket 006's .rds at the expected path
- [ ] No tryCatch returning NULL; no try(silent = TRUE)
- [ ] Uses here::here()
- [ ] Does NOT modify Code/Helpers/reduced_form_utils.R

HONESTDID OBJECTS:
- [ ] hd_ols_sd, hd_ols_rm, hd_cox_sd, hd_cox_rm all exist after the
      script's main loop
- [ ] All four are data-frame-coercible with columns including lb, ub,
      and either M or Mbar
- [ ] hd_ols_sd has nrow == length(HD_MVEC_SD) (i.e., one row per grid point)
- [ ] hd_ols_rm has nrow == length(HD_MBAR_RM)
- [ ] Same for hd_cox_sd / hd_cox_rm

INPUTS:
- [ ] hd_ols_inputs$betahat has length == numPreOLS + numPostOLS
- [ ] hd_cox_inputs$betahat has length == numPreCox + numPostCox
- [ ] dim(hd_ols_inputs$sigma) matches length(betahat) on both axes
- [ ] hd_ols_inputs$betahat[as.character(-1L)] does NOT exist
      (ref period was dropped)

DELIVERABLES (2)-(4):
- [ ] .rds loads and contains exactly the named top-level structure
      from Step 7
- [ ] T_HonestDiD_Summary.csv has 2 rows + header and 9 columns
- [ ] breakdown columns: numeric ones are NA when no grid crossing
      and finite otherwise; formatted ones are character strings
      either ">{grid_max}" or "0.xxxx"
- [ ] Log contains "OUTPUT SUMMARY"

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[Filled by reviewer after each R1 attempt.]

### Attempt 1 — [DATE]
Transcript: 008_transcript.txt
Result: [PASS | TRANSLATION_FAIL | PSEUDOCODE_FAIL]
