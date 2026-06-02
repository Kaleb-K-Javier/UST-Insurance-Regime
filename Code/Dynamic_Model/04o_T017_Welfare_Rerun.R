# ==============================================================================
# Code/Dynamic_Model/04o_T017_Welfare_Rerun.R
# TICKET 017 — Welfare CF re-run on the canonical BOY + gammafree fit
#
# Driver that wraps the UNCHANGED engine 04o_CF_TX_FlatFee.R (whose fit_path was
# re-pointed in-place to the gammafree fit, per STEP 2). In order:
#   STEP 0  guard: load gammafree fit, assert converged + both gammas interior.
#           If unresolved -> stop() with NO outputs (runs BEFORE any file move).
#   STEP 1  archive (MOVE) the 5 pre-BOY CF outputs to _pre_BOY/.
#   STEP 2  re-run 04o_CF_TX_FlatFee.R end-to-end (own Rscript subprocess, for
#           clean sink/log isolation) -> regenerates fresh BOY outputs.
#   STEP 3  deliverable T017_Welfare_PreBOY_vs_BOY.csv (pre-BOY vs BOY deltas).
#   STEP 4  C5 TX-level validation: fit$P_hat[17:32,] vs CF baseline re-solve.
#   STEP 5  document the F_replace caveat to the log + sidecar note.
# ==============================================================================

# ==============================================================================
# SECTION 1 — LOGGING
# ==============================================================================
.log_path <- here::here("logs", paste0(
  "04o_T017_Welfare_Rerun_",
  format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 04o_T017_Welfare_Rerun.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# ==============================================================================
# SECTION 2 — PACKAGES + PATHS
# ==============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(here)
})
setDTthreads(0L)

OUT_FIT <- here::here("Output", "Estimation_Results")
OUT_TAB <- here::here("Output", "Tables")

PREBOY_FIT <- file.path(OUT_FIT, "_pre_BOY")
PREBOY_TAB <- file.path(OUT_TAB, "_pre_BOY")

fit_path <- file.path(OUT_FIT, "Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds")

# ==============================================================================
# STEP 0 — SELECT + GUARD THE CANONICAL FIT  (runs BEFORE any file move)
# ==============================================================================
cat("=== STEP 0: CANONICAL FIT GUARD ===\n")

if (!file.exists(fit_path))
  stop(sprintf("STEP 0: canonical gammafree fit not found: %s", fit_path))

fit <- readRDS(fit_path)

# Converged + 6 named params
if (!isTRUE(fit$converged))
  stop("STEP 0: gammafree fit$converged is not TRUE; fit unresolved, no outputs written.")
stopifnot(length(fit$theta_hat) == 6L,
          all(c("gamma_price", "gamma_risk") %in% names(fit$theta_hat)))

# Bound-agnostic interior rule: g in (lo+tol, hi-tol), tol = 1e-3*(hi-lo).
# Read the box from the fit (04p driver override), do NOT hardcode 20.
gp_box <- fit$config$gamma_price_bounds
gr_box <- fit$config$gamma_risk_bounds
stopifnot(is.numeric(gp_box), length(gp_box) == 2L,
          is.numeric(gr_box), length(gr_box) == 2L)

assert_interior <- function(g, box, nm) {
  lo <- box[1]; hi <- box[2]; tol <- 1e-3 * (hi - lo)
  if (!(g > lo + tol && g < hi - tol))
    stop(sprintf(paste0("STEP 0: %s = %.5f is at/near its bound [%.3f, %.3f] ",
                        "(tol = %.4f). Fit unresolved -> aborting, NO outputs written."),
                 nm, g, lo, hi, tol))
  cat(sprintf("  %-11s = %8.5f  interior to [%.2f, %.2f] (tol=%.4f)  OK\n",
              nm, g, lo, hi, tol))
}

gp <- fit$theta_hat[["gamma_price"]]
gr <- fit$theta_hat[["gamma_risk"]]
assert_interior(gp, gp_box, "gamma_price")
assert_interior(gr, gr_box, "gamma_risk")
cat(sprintf("  fit converged=%s | LL=%.3f | guard PASS\n",
            fit$converged, fit$log_likelihood))

# ==============================================================================
# STEP 1 — ARCHIVE (MOVE) THE PRE-BOY CF OUTPUTS
# ==============================================================================
cat("=== STEP 1: ARCHIVE PRE-BOY OUTPUTS ===\n")

dir.create(PREBOY_FIT, recursive = TRUE, showWarnings = FALSE)
dir.create(PREBOY_TAB, recursive = TRUE, showWarnings = FALSE)

# (source path, destination dir).  Hard stop() on ANY missing source.
archive_specs <- list(
  list(src = file.path(OUT_FIT, "CF_TX_FlatFee_results.rds"),        dst = PREBOY_FIT),
  list(src = file.path(OUT_TAB, "04o_CF_Welfare_Summary.csv"),       dst = PREBOY_TAB),
  list(src = file.path(OUT_TAB, "04o_CF_Welfare_Summary.tex"),       dst = PREBOY_TAB),
  list(src = file.path(OUT_TAB, "04o_CF_ActionShares_byAge.csv"),    dst = PREBOY_TAB),
  list(src = file.path(OUT_TAB, "04o_CF_RemovalAge_Distribution.csv"), dst = PREBOY_TAB)
)

move_one <- function(src, dst_dir) {
  if (!file.exists(src))
    stop(sprintf("STEP 1 archive: required pre-BOY source MISSING: %s", src))
  dst <- file.path(dst_dir, basename(src))
  ok  <- file.rename(src, dst)
  if (!ok) {                       # cross-volume fallback: copy + unlink
    cp <- file.copy(src, dst, overwrite = TRUE)
    if (!cp) stop(sprintf("STEP 1 archive: copy fallback failed for %s", src))
    if (!unlink(src) == 0L) stop(sprintf("STEP 1 archive: unlink failed for %s", src))
  }
  cat(sprintf("  moved: %s\n     -> %s\n", src, dst))
  dst
}

for (sp in archive_specs) move_one(sp$src, sp$dst)

# Path to the archived pre-BOY welfare CSV (the STEP 3 pre-BOY source)
preboy_welfare_csv <- file.path(PREBOY_TAB, "04o_CF_Welfare_Summary.csv")
stopifnot(file.exists(preboy_welfare_csv))
cat(sprintf("  pre-BOY welfare source for STEP 3: %s\n", preboy_welfare_csv))

# ==============================================================================
# STEP 2 — RE-RUN 04o END-TO-END (own subprocess)
# ==============================================================================
cat("=== STEP 2: RE-RUN 04o_CF_TX_FlatFee.R ===\n")

engine_path <- here::here("Code", "Dynamic_Model", "04o_CF_TX_FlatFee.R")
stopifnot(file.exists(engine_path))

rscript <- file.path(R.home("bin"), "Rscript.exe")
if (!file.exists(rscript)) rscript <- file.path(R.home("bin"), "Rscript")
stopifnot(file.exists(rscript))

cat(sprintf("  launching: %s %s\n", rscript, engine_path))
status <- system2(rscript, args = shQuote(engine_path))
if (!identical(as.integer(status), 0L))
  stop(sprintf("STEP 2: 04o_CF_TX_FlatFee.R exited with non-zero status %s", status))
cat("  04o re-run: exit status 0 (BOY outputs regenerated at canonical paths)\n")

# New (BOY-gammafree) outputs now at canonical paths
boy_welfare_csv <- file.path(OUT_TAB, "04o_CF_Welfare_Summary.csv")
boy_results_rds <- file.path(OUT_FIT, "CF_TX_FlatFee_results.rds")
stopifnot(file.exists(boy_welfare_csv), file.exists(boy_results_rds))

# ==============================================================================
# STEP 3 — DELIVERABLE: pre-BOY vs BOY welfare comparison
# ==============================================================================
cat("=== STEP 3: PRE-BOY vs BOY WELFARE COMPARISON ===\n")

# Both summaries share schema: component, E_label, E_external_USD,
# baseline_USD, cf_USD, delta_USD.  Key on component x E_label; use delta_USD.
read_deltas <- function(path, delta_name) {
  dt <- fread(path)
  stopifnot(all(c("component", "E_label", "delta_USD") %in% names(dt)))
  out <- dt[, .(component, E_label, delta = as.numeric(delta_USD))]
  setnames(out, "delta", delta_name)
  out
}

pre <- read_deltas(preboy_welfare_csv, "preBOY_delta")
boy <- read_deltas(boy_welfare_csv,    "BOY_delta")

cmp <- merge(pre, boy, by = c("component", "E_label"))
stopifnot(nrow(cmp) == 8L)   # 4 components x 2 E_labels

cmp[, abs_change := BOY_delta - preBOY_delta]
cmp[, pct_change := fifelse(abs(preBOY_delta) < 1e-8,
                            NA_real_,
                            100 * abs_change / abs(preBOY_delta))]

setorder(cmp, component, E_label)
cmp <- cmp[, .(component, E_label, preBOY_delta, BOY_delta, abs_change, pct_change)]

path_cmp <- file.path(OUT_TAB, "T017_Welfare_PreBOY_vs_BOY.csv")
fwrite(cmp, path_cmp)
cat(sprintf("  Saved: %s\n", path_cmp))
print(cmp)

# Headline: SocialWelfare under the $50k E scenario (HEALTH_PLUS_UNMEASURED)
sw <- cmp[component == "SocialWelfare_USD" & E_label == "HEALTH_PLUS_UNMEASURED"]
stopifnot(nrow(sw) == 1L)
cat(sprintf(paste0("\n  HEADLINE ($50k E) SocialWelfare CF-vs-baseline delta:\n",
                   "    pre-BOY = %s USD   BOY-gammafree = %s USD\n",
                   "    change  = %s USD   (%s%%)\n"),
            format(round(sw$preBOY_delta), big.mark = ","),
            format(round(sw$BOY_delta),    big.mark = ","),
            format(round(sw$abs_change),   big.mark = ","),
            ifelse(is.na(sw$pct_change), "NA", sprintf("%.1f", sw$pct_change))))
cat(sprintf(paste0("  VERDICT: the BOY + gammafree correction moved the headline ",
                   "welfare effect by %s USD (%s%% of the pre-BOY effect).\n"),
            format(round(sw$abs_change), big.mark = ","),
            ifelse(is.na(sw$pct_change), "NA", sprintf("%.1f", sw$pct_change))))

# ==============================================================================
# STEP 4 — TX-LEVEL VALIDATION (C5 check) ON THE NEW FIT
# ==============================================================================
cat("=== STEP 4: TX-LEVEL VALIDATION (C5) ===\n")

# P_baseline persisted by the re-run; fit$P_hat from the canonical gammafree fit.
res        <- readRDS(boy_results_rds)
P_baseline <- res$P_baseline
P_hat      <- fit$P_hat

stopifnot(is.matrix(P_baseline), nrow(P_baseline) == 32L, ncol(P_baseline) == 3L)
stopifnot(is.matrix(P_hat),      nrow(P_hat)      == 32L, ncol(P_hat)      == 3L)

# TX cells are s_idx 17:32 in canonical order; full 3-column comparison.
tx_rows  <- 17:32
max_disc <- max(abs(P_hat[tx_rows, ] - P_baseline[tx_rows, ]))
cat(sprintf("  max|fit$P_hat - resolve P_baseline| over TX cells (s_idx 17-32): %.3e\n",
            max_disc))
if (max_disc >= 1e-4)
  stop(sprintf("STEP 4: TX-level validation FAILED: max discrepancy %.3e >= 1e-4", max_disc))
cat("  TX-level validation: PASS (< 1e-4) — TX correctly leveled in the CF re-solve.\n")

# ==============================================================================
# STEP 5 — DOCUMENT THE CAVEAT
# ==============================================================================
cat("=== STEP 5: CAVEAT ===\n")

caveat <- paste0(
  "Headline FF<->RB welfare (premium->maintain channel) is post-BOY clean. ",
  "Replace-margin welfare carries the F_replace single-tank-reset caveat ",
  "(replace off-support 98%, Ticket 014).")
cat(sprintf("  %s\n", caveat))

path_note <- file.path(OUT_TAB, "T017_Welfare_caveat_note.txt")
writeLines(caveat, path_note)
cat(sprintf("  Saved caveat note: %s\n", path_note))

# ==============================================================================
# DONE
# ==============================================================================
cat("\n=== 04o_T017_Welfare_Rerun COMPLETE ===\n")
cat(sprintf("  Comparison CSV:   %s\n", path_cmp))
cat(sprintf("  Caveat note:      %s\n", path_note))
cat(sprintf("  BOY welfare CSV:  %s\n", boy_welfare_csv))
cat(sprintf("  Archived pre-BOY: %s , %s\n", PREBOY_FIT, PREBOY_TAB))
