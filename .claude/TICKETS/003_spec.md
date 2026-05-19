# TICKET 003 — Harmonized goodness-of-fit artifacts + 3-way model comparison
# Created: 2026-05-18
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
We need a self-contained fit-summary report comparing the three nested DCM
replacement specifications already estimated and saved to disk:

  - 4-parameter             (kappa, K_log, gamma_price, gamma_risk)
  - 8-parameter             (above split by wall and regime: 8 params)
  - 8-parameter + state FE  (above + 17 alpha_g controls, 25 params)

The report needs to (a) show side-by-side parameter tables, (b) show
information-criteria + LR tests across nested pairs, (c) show
model-implied-vs-empirical action shares at the (wall, regime, age-bin)
cell level in a directly comparable visual format across all three specs.
The 4p and 8p fit visuals already exist but were produced by different
code paths with different styling. The 8p+FE fit visual does not exist.
This ticket harmonizes all three via a single plotting function and
produces the missing FE artifacts.

═══════════════════════════════════════════════════
MATH
═══════════════════════════════════════════════════

Eq. 1 — empirical cell share (identical for all three models):

    p_emp[s, a]  =  ( 1 / n_s ) * sum_{i in cell s} 1{action_i = a}

    a in {M, E, R};   s in {1, ..., 32}   (8 age-bins × 2 wall × 2 regime)

Eq. 2 — 4p / 8p model-implied cell share:

    p_model[s, a]  =  fit$P_hat[s, a]

    Read directly from the saved fit object; this is the NPL-converged
    softmax of v_j(s; theta_hat).

Eq. 3 — 8p+FE model-implied cell share (measurement-equation marginal):

    p_model[s, a]  =  sum_{g=1}^{18}  w_g(s)  *  P(s, a, g; theta_hat, alpha_hat_g)

    where w_g(s) = n_{s,g} / n_s is the empirical g-composition of cell s.

    Per NPL_REFERENCE.md §4, alpha enters the MEASUREMENT equation (which
    is what GoF compares against data) but NOT the agent's CF equilibrium
    re-solve. The 8p+FE NPL outer loop's CCP update (improved_estimator
    _OPTIMIZED.r lines 3701-3750, `.update_ccps_geoweighted_8p_fe`)
    computes exactly Eq. 3 inside the loop, so fit_fe$P_hat at
    convergence IS already the (32 × 3) g-marginalized alpha-augmented
    measurement P. No recompute needed — just read it.

Eq. 4 — information criteria:

    AIC = -2 * log_lik + 2 * k
    BIC = -2 * log_lik + k * log(N)

    where k is param count (4, 8, 25 for the three specs) and N = nrow(obs panel).

Eq. 5 — nested likelihood ratio:

    LR  = 2 * ( log_lik_big - log_lik_small )
    df  = k_big - k_small
    p   = 1 - pchisq(LR, df)

    Tests: (8p vs 4p) and (8p+FE vs 8p). The 4p row has NA for LR.

═══════════════════════════════════════════════════
PSEUDOCODE
═══════════════════════════════════════════════════

Step 1 — Load fits, primitives, observed panel.
  Input: paths under Output/Estimation_Results/ and Data/Analysis/:
    - DCM_Primitives_Replacement_observed.rds       -> prims_obs
    - Model_Replacement_Estimates_observed.rds      -> fit_4p
    - Model_Replacement_8param_observed.rds         -> fit_8p
    - Model_Replacement_8paramFE_observed.rds       -> fit_fe
    - Data/Analysis/dcm_obs_panel_observed.csv      -> obs_obs (fread)
  Output: five objects loaded into the session.
  assert:
    - all four .rds files exist; obs csv exists
    - each fit list has $log_likelihood (numeric scalar), $theta_hat
      (named numeric), $P_hat (matrix)
    - fit_4p$P_hat and fit_8p$P_hat are dim 32 × 3
    - fit_fe$P_hat is dim 32 × 3 with rowSums within 1e-6 of 1
    - fit_fe has an alpha-related field (introspect: $alpha_hat or
      theta_hat slice with names alpha_AR..alpha_VA)
    - prims_obs$state_lut has 32 rows with columns A_bin, w_state, rho_state
    - obs_obs has columns y_it, I_replace, and state-identifying columns
      compatible with the 32-state cell definition used in 04h

Step 2 — Build empirical cell shares.
  Input: obs_obs, prims_obs$state_lut
  Output: emp (data.table, 32 rows, columns: s_idx, emp_M, emp_E, emp_R, n_cell)
  Operations:
    - Mirror Code/Dynamic_Model/04h_Replacement_8param_Estimation.R lines
      308-311 verbatim:
        emp <- obs_obs[, .(
          emp_M  = mean(y_it == 0L),
          emp_E  = mean(y_it == 1L & I_replace == 0L, na.rm = TRUE),
          emp_R  = mean(y_it == 1L & I_replace == 1L, na.rm = TRUE),
          n_cell = .N
        ), by = s_idx][order(s_idx)]
    - Reference: Eq. 1
  assert:
    - nrow(emp) == 32
    - all(abs(emp_M + emp_E + emp_R - 1) < 1e-10)   # shares are exhaustive
    - all(n_cell > 0)

Step 3 — Build per-fit cell tables.
  Input: fit_4p, fit_8p, fit_fe, emp, prims_obs$state_lut
  Output: three data.tables (cell_4p, cell_8p, cell_fe), each 32 rows.
          Each has columns: s_idx, regime, wall, age_bin, age_label, n_cell,
          model_maintain, emp_maintain, res_maintain,
          model_exit,     emp_exit,     res_exit,
          model_replace,  emp_replace,  res_replace
  Operations:
    - For each fit:
        P_hat_dt <- as.data.table(fit$P_hat)
        setnames(P_hat_dt, c("model_M", "model_E", "model_R"))
        P_hat_dt[, s_idx := seq_len(.N)]
        cell <- merge(state_lut, merge(P_hat_dt, emp, by = "s_idx"),
                      by = "s_idx")
        cell[, regime := fifelse(rho_state == 1L, "FF", "RB")]
        cell[, wall   := fifelse(w_state == 1L, "SW", "DW")]
        cell[, age_label := c("0-5","5-10","10-15","15-20",
                              "20-25","25-30","30-35","35+")[A_bin]]
        round to 5 decimals, compute residuals = emp - model
        setorder by regime, wall, age_bin
    - The three model_* columns are Eq. 2 for 4p/8p and Eq. 3 (already
      baked into fit_fe$P_hat) for 8p+FE
    - Reference: 04h_Replacement_8param_Estimation.R lines 305-328 as template
  assert:
    - nrow(cell_*) == 32 for all three
    - all model_* + emp_* values are in [0, 1]
    - all(abs(model_M + model_E + model_R - 1) < 1e-6) for all three
    - sum of absolute residuals is finite and reasonable (<6 per model, since
      max possible is 3 actions × 32 cells × 2 = 192)

Step 4 — Write 8p+FE per-cell fit CSV  (DELIVERABLE #7).
  Input: cell_fe
  Output: Output/Tables/04k_PerCell_Fit_8paramFE_Wide.csv
  Operations:
    - fwrite cell_fe with column order: s_idx, regime, wall, age_bin,
      age_label, n_cell, model_maintain, emp_maintain, res_maintain,
      model_exit, emp_exit, res_exit, model_replace, emp_replace, res_replace
  assert:
    - file exists at exact path
    - nrow == 32, ncol == 15

Step 5 — Build 8p+FE fit-quality summary  (DELIVERABLE #8).
  Input: cell_fe
  Output: Output/Tables/04k_FitQuality_8paramFE_byWallRegimeAction.csv
  Operations:
    - Mirror 04h_Replacement_8param_Estimation.R lines 331-352:
        Pivot cell_fe to long form (action in {Maintain, Exit, Replace})
        Compute by (wall, regime, action):
          N_cells               = .N
          total_n               = sum(n_cell)
          weighted_RMSE         = sqrt( sum(residual^2 * n_cell) / sum(n_cell) )
          weighted_mean_residual= sum(residual * n_cell) / sum(n_cell)
          max_abs_residual      = max(abs(residual))
        setorder(action, wall, regime)
        fwrite
  assert:
    - nrow == 12 (2 wall × 2 regime × 3 action), ncol == 7
    - all weighted_RMSE values finite and >= 0

Step 6 — Shared per-wall plotting function.
  Input: cell data.table (one of cell_4p / cell_8p / cell_fe),
         wall_label (string), outfile (path), model_tag (string)
  Output: PNG written to outfile (12 × 4.5 inches)
  Operations:
    - Lift the body of plot_one_wall from
      04h_Replacement_8param_Estimation.R lines 366-389 verbatim.
    - Parameterize the title: title = sprintf("%s fit — %s (observed sample)",
                                              model_tag, wall_label)
    - Keep facet, scales, color, point/line aesthetics IDENTICAL across
      all three calls. This is the whole point of "harmonized".
  Call six times to produce DELIVERABLES #1-#6:
    plot_one_wall(cell_4p[wall == "SW"], "Single-Walled (or Mixed)",
                  "Output/Figures/04k_Fit_4param_SW.png",     "4-param")
    plot_one_wall(cell_4p[wall == "DW"], "Double-Walled",
                  "Output/Figures/04k_Fit_4param_DW.png",     "4-param")
    plot_one_wall(cell_8p[wall == "SW"], "Single-Walled (or Mixed)",
                  "Output/Figures/04k_Fit_8param_SW.png",     "8-param")
    plot_one_wall(cell_8p[wall == "DW"], "Double-Walled",
                  "Output/Figures/04k_Fit_8param_DW.png",     "8-param")
    plot_one_wall(cell_fe[wall == "SW"], "Single-Walled (or Mixed)",
                  "Output/Figures/04k_Fit_8paramFE_SW.png",   "8-param + FE")
    plot_one_wall(cell_fe[wall == "DW"], "Double-Walled",
                  "Output/Figures/04k_Fit_8paramFE_DW.png",   "8-param + FE")
  assert:
    - all 6 PNG files exist after the call
    - any failure inside ggsave must propagate (no try/tryCatch)

Step 7 — 3-way model-comparison table  (DELIVERABLES #9 and #10).
  Input: fit_4p, fit_8p, fit_fe, N = nrow(obs_obs)
  Output: Output/Tables/04k_Model_Comparison_3way.csv
          Output/Tables/04k_Model_Comparison_3way.tex
  Operations:
    - Build data.table with 3 rows in this exact order:
        model        k_params  n_obs   log_lik          AIC      BIC      LR_vs_nested  df_LR  pvalue_LR
        "4-param"    4         N       fit_4p$log_lik   AIC_4    BIC_4    NA            NA     NA
        "8-param"    8         N       fit_8p$log_lik   AIC_8    BIC_8    LR_8v4        4      p_8v4
        "8-param+FE" 25        N       fit_fe$log_lik   AIC_25   BIC_25   LR_25v8       17     p_25v8
      AIC, BIC per Eq. 4. LR / p per Eq. 5.
    - fwrite to CSV (#9)
    - Build .tex tabular (#10):
        \begin{tabular}{lrrrrrrrr}
        \hline
        Model & $k$ & $N$ & $\log L$ & AIC & BIC & LR vs nested & df & $p$-value \\
        \hline
        4-param      & 4  & ... & ... & ... & ... & -- & -- & -- \\
        8-param      & 8  & ... & ... & ... & ... & ... & 4  & ... \\
        8-param + FE & 25 & ... & ... & ... & ... & ... & 17 & ... \\
        \hline
        \multicolumn{9}{l}{\footnotesize LR tests: 8p vs 4p, then 8p+FE vs 8p.}
        \end{tabular}
      Wrap in \resizebox{\textwidth}{!}{...} for guaranteed page fit.
      Format log_lik / AIC / BIC / LR as integers with comma thousand-separators.
      Format p-values with sprintf("%.3g", .).
      writeLines to #10 path.
  assert:
    - both files exist; CSV nrow == 3, ncol == 9
    - n_obs identical across all three CSV rows
    - LR_vs_nested[1] is NA
    - all log_lik values negative; AIC > BIC is allowed (BIC penalty stronger)
    - .tex contains "\\resizebox" and exactly 3 data rows

Step 8 — Compact FE alphas table  (DELIVERABLE #11).
  Input: Output/Tables/04iFETableAllControls.csv  (already exists)
  Output: Output/Tables/04k_FE_Alphas_Compact.tex
  Operations:
    - fread the CSV. Introspect column names; expect a state column and
      an alpha column with SE (could be combined "estimate (SE)" string
      or separate numeric columns — match whatever format is on disk).
    - Build 18-row .tex: TX as row 1 with body "0 (baseline)";
      other 17 states alphabetical, each row "STATE_CODE & alpha_value (SE) \\"
    - Wrap:
        \resizebox{\textwidth}{!}{
        \begin{tabular}{lc}
        \hline
        State & $\hat\alpha_g$ (SE) \\
        \hline
        ... 18 data rows ...
        \hline
        \multicolumn{2}{l}{\footnotesize Texas is the baseline (alpha set to 0).
          Other states are estimated jointly with structural theta in the
          25-parameter 8p+FE specification.}
        \end{tabular}
        }
    - writeLines to .tex path
  assert:
    - file exists; contains "\\resizebox"; contains exactly 18 data rows
      between the two \hline lines surrounding the body
    - TX appears as the first body row

Step 9 — Save & print one-line summary.
  Console output (per CLAUDE.md output rules):
    cat("== 04k Fit Report Artifacts ==\n")
    cat(sprintf("  4p:   k=4   logL=%.0f  AIC=%.0f  BIC=%.0f\n", ...))
    cat(sprintf("  8p:   k=8   logL=%.0f  AIC=%.0f  BIC=%.0f  LRvs4p=%.1f (p=%.3g)\n", ...))
    cat(sprintf("  8pFE: k=25  logL=%.0f  AIC=%.0f  BIC=%.0f  LRvs8p=%.1f (p=%.3g)\n", ...))
    cat("  Wrote 11 artifacts under Output/Figures and Output/Tables (04k_* prefix)\n")
  Log file: logs/04k_Fit_Report_Artifacts_<YYYYMMDD_HHMMSS>.log per
            the CLAUDE.md logging block.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Script path: Code/Dynamic_Model/04k_Fit_Report_Artifacts.R
- Use here::here() for all paths.
- Required packages: data.table, ggplot2, here. (No estimation packages
  loaded — this script does NOT call any NPL or Bellman routines.)
- DO NOT call sourceCpp(cpp_engine.cpp). This script does not invoke the
  C++ estimation engine; it only reads .rds outputs and reformats them.
- DO NOT call any function in improved_estimator_OPTIMIZED.r (no
  invert_value_function_replacement, no update_ccps_*, no nllreplacement*).
  All model-implied probabilities come from the converged $P_hat field
  already saved on disk. Per Eq. 3 explanation: fit_fe$P_hat already IS
  the alpha-augmented g-marginalized measurement P. Do not recompute it.
- For Step 3, mirror 04h_Replacement_8param_Estimation.R lines 305-328
  (cell-table construction). For Step 5, mirror lines 331-352. For
  Step 6, mirror lines 366-389. These three blocks are the canonical
  templates — copy their structure, don't reinvent.
- Hard error propagation only: no tryCatch returning NULL, no
  try(..., silent = TRUE).
- Logging block at top of script per CLAUDE.md:
    .log_path <- here::here("logs", paste0(
      "04k_Fit_Report_Artifacts_",
      format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
    dir.create(dirname(.log_path), recursive=TRUE, showWarnings=FALSE)
    .log <- file(.log_path, open="wt")
    sink(.log, type="output"); sink(.log, type="message", append=TRUE)
    on.exit({ sink(type="output"); sink(type="message"); close(.log) }, add=TRUE)
- Q on column-name match: 04iFETableAllControls.csv schema is not pre-
  declared here. Before parsing in Step 8, print colnames() and head(1)
  to the log so the reviewer can verify the format-match heuristic.
- If fit_fe stores alphas separately from theta_hat (e.g. $alpha_hat
  named vector), prefer that. Otherwise extract via grep("^alpha_",
  names(fit_fe$theta_hat)).

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════

DELIVERABLES (all 11 files at exact paths):
- [ ] Output/Figures/04k_Fit_4param_SW.png
- [ ] Output/Figures/04k_Fit_4param_DW.png
- [ ] Output/Figures/04k_Fit_8param_SW.png
- [ ] Output/Figures/04k_Fit_8param_DW.png
- [ ] Output/Figures/04k_Fit_8paramFE_SW.png
- [ ] Output/Figures/04k_Fit_8paramFE_DW.png
- [ ] Output/Tables/04k_PerCell_Fit_8paramFE_Wide.csv         (32 rows, 15 cols)
- [ ] Output/Tables/04k_FitQuality_8paramFE_byWallRegimeAction.csv (12 rows, 7 cols)
- [ ] Output/Tables/04k_Model_Comparison_3way.csv             (3 rows, 9 cols)
- [ ] Output/Tables/04k_Model_Comparison_3way.tex             (contains \resizebox)
- [ ] Output/Tables/04k_FE_Alphas_Compact.tex                 (contains \resizebox, 18 body rows)

NUMERIC INVARIANTS:
- [ ] For all three cell tables: all(abs(model_M + model_E + model_R - 1) < 1e-6)
- [ ] For empirical: all(abs(emp_M + emp_E + emp_R - 1) < 1e-10)
- [ ] In Model_Comparison_3way.csv: n_obs is identical across all 3 rows
- [ ] In Model_Comparison_3way.csv: row order is exactly 4-param, 8-param, 8-param+FE
- [ ] In Model_Comparison_3way.csv: LR_vs_nested[1] is NA, df_LR[1] is NA, pvalue_LR[1] is NA
- [ ] AIC ordering: lower is better; if 8p+FE truly fits best we expect AIC_25 < AIC_8 < AIC_4
      (not a hard requirement — reviewer just verifies the math, not the ordering)

CODE HYGIENE:
- [ ] No tryCatch(expr, error = function(e) NULL) anywhere
- [ ] No try(expr, silent = TRUE) anywhere
- [ ] No sourceCpp call
- [ ] No calls into improved_estimator_OPTIMIZED.r helpers
- [ ] Script uses here::here() (no absolute paths)
- [ ] Logging block present at top; log file created under logs/
- [ ] Single shared plot_one_wall function called 6 times (not 6 ad-hoc ggplots)

FIGURE PARITY:
- [ ] All 6 PNGs are 12 × 4.5 inches
- [ ] All 6 PNGs use the same facet (Maintain | Exit | Replace), same x-axis
      (A_bin 1..8 with labels "0-5" .. "35+"), same color mapping for
      regime, same shape/size convention for the empirical points
- [ ] Each PNG title contains the model_tag string ("4-param", "8-param",
      or "8-param + FE") and the wall label

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════

### Attempt 1 — 2026-05-18
Transcript: 003_transcript.txt
Result: PASS

Coder: Sonnet 4.6 via Anthropic Pro account (OAuth).
Reviewer: Opus (architect session, this conversation).

All 11 deliverables produced at exact spec paths. Numeric audit independently
re-computed all AIC / BIC / LR_vs_nested values to within rounding; figures
have structural parity via shared plot_one_wall function called six times.
Run completed without errors; one log file at logs/04k_Fit_Report_Artifacts_
20260518_130202.log.

Headline results (full audit numbers):

  Spec         k    log L         AIC        BIC        LR vs nested     df   p
  ----         --   ----------    -------    -------    -------------    --   --
  4-param      4    -268,481      536,971    537,021    --               --   --
  8-param      8    -261,770      523,557    523,658    13,422.2          4   ~0
  8-param+FE   25   -251,613      503,277    503,593    20,314.1         17   ~0
  N obs across all three rows: 2,282,735.

Three SPEC ERRORS surfaced during implementation. None were translation
failures — the coder identified each, asked the architect, and applied
minimal correct fixes. Recording here so the next ticket avoids them:

  (1) Step 1 assertion read `!is.null(fit_fe$P_hat)`, but the FE estimator
      (04i_8param_FE_and_Welfare.R, saveRDS at line 116) writes the
      converged CCP matrix to `fit_fe$Phat` (no underscore), while
      04g/04h save it as `$P_hat`. Coder added a one-line alias at load
      time: `fit_fe$P_hat <- fit_fe$Phat`. Substantive fix would be to
      rename the save in 04i.R, but that requires a re-estimation run
      (hours) and was correctly NOT done here. Logged as a future
      housekeeping item.

  (2) Step 5 acceptance said `ncol == 7` for the FitQuality CSV. The 04h
      template that Step 5 mirrors produces 8 columns (3 grouping cols
      wall/regime/action + 5 metrics N_cells/total_n/weighted_RMSE/
      weighted_mean_residual/max_abs_residual). Coder used `ncol == 8`,
      which matches the template. Spec acceptance criterion was wrong.

  (3) Step 3 invariant said `all(abs(model_M + model_E + model_R - 1) <
      1e-6)` on the cell tables. But cell tables apply `r5()` (round to
      5 decimals) to model_* columns; rounding three independent values
      can accumulate up to 1.5e-5 of sum-error. Coder's fix is the
      principled one: keep the tight 1e-6 check on the RAW `fit$P_hat`
      pre-rounding (the actual numerical invariant), relax only the
      post-rounding check on the printed table to 2e-5. Approved.

Substantive observation (not a defect): FE alpha estimates for MO
(alpha=8.18, SE=0.77) and SD (alpha=2.27, SE=0.08) are much larger than
the other 15 control-state alphas (which span roughly [-1.4, +0.8]).
With sigma2=1, these correspond to very strong Maintain-utility shifts
relative to TX baseline -- consistent with near-zero exit/replace
activity in MO and SD observational cells. Worth a footnote in the
qmd report so a reader doesn't mistake this for a numerical artifact.

Polish-direct task to follow this PASS (Opus, not coder):
  - Write Reports/Paper/Dynamic_Model_Fit_Report.qmd consuming the 11
    artifacts above. Pure presentation -- no Bellman/CCP/V touched.
  - Logged in HANDOFF.md as "Polish-direct".

