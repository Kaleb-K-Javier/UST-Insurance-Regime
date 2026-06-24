# ==============================================================================
# AE01_Frame_Build.R  -- TICKET 021, script 1 of 5  (shared frame)
# ==============================================================================
# Builds the ONE canonical facility-year frame consumed by AE02/AE03/AE05.
# AE04 is self-contained (copies the action-coding block verbatim).
#
# CANONICAL CHOICES (defined ONCE here; every exhibit inherits them):
#   SAMPLE FRAME : facility-years in boy_composition_fy, panel_year >= 1999, N >= 1.
#   SIZE_BIN     : derived from boyfy$N via the 04b fcase -- the ONLY size_bin any
#                  exhibit uses (no per-analysis drift).
#   ACTION CODING: flags from facility_panel; magnitudes from the composition build.
#     Exit      = facility_complete_closure == 1
#     Replace   = any_closure == 1 & n_installs >  0 & !Exit
#     Downsize  = any_closure == 1 & n_installs == 0 & !Exit
#     Expansion = any_closure == 0 & n_installs >  0
#     Maintain  = remainder
#   SPINE        : dcm_obs_panel_observed (s_idx, A_bin, w_state, rho_state) where
#                  available; on_spine flags the regime-conditioned subset.
# READ-ONLY DESCRIPTIVE. No Output/Estimation_Results writes.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table); library(here) })

# ---- logging (hardcoded name; rstudioapi is NULL under Rscript) ----
.log_path <- here::here("logs", paste0("AE01_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
cat(sprintf("LOG START %s\nScript: AE01_Frame_Build.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== AE01 FRAME BUILD ===\n")
cat(sprintf("Start: %s\n", format(Sys.time())))

DATA_DIR <- here("Data", "Analysis")

# ==============================================================================
# 1. Read inputs (boyfy spine + facility_panel flags + dcm spine)
# ==============================================================================
cat("=== SECTION 1: read inputs ===\n")
boyfy <- fread(file.path(DATA_DIR, "boy_composition_fy.csv"))
boyfy[, panel_id := as.character(panel_id)]
boyfy <- boyfy[panel_year >= 1999L & N >= 1L]
cat(sprintf("  boyfy facility-years (year>=1999, N>=1): %s\n", format(nrow(boyfy), big.mark = ",")))

fac <- fread(file.path(DATA_DIR, "facility_panel.csv"),
             select = c("panel_id", "panel_year", "any_closure",
                        "facility_complete_closure", "n_installs"))
fac[, panel_id := as.character(panel_id)]

dcm <- fread(file.path(DATA_DIR, "dcm_obs_panel_observed.csv"),
             select = c("panel_id", "panel_year", "s_idx", "A_bin", "w_state",
                        "rho_state", "size_bin", "boy_stock"))
dcm[, panel_id := as.character(panel_id)]
setnames(dcm, "size_bin", "dcm_size_bin")

# key uniqueness -- stop, do NOT dedupe silently
stopifnot(!anyDuplicated(fac[, .(panel_id, panel_year)]))
stopifnot(!anyDuplicated(dcm[, .(panel_id, panel_year)]))
cat("  key uniqueness OK (facility_panel, dcm_obs)\n")

# ==============================================================================
# 2. Canonical size_bin (from N, 04b fcase) -- the ONLY size_bin used anywhere
# ==============================================================================
cat("=== SECTION 2: canonical size_bin from N ===\n")
boyfy[, size_bin := fcase(N == 1L, "1", N == 2L, "2", N == 3L, "3",
                          N >= 4L, "4+", default = NA_character_)]
stopifnot(!anyNA(boyfy$size_bin))

# ==============================================================================
# 3. Merge flags + targeted NA->0 (action flags + boyfy counts only; never blanket)
# ==============================================================================
cat("=== SECTION 3: merge + targeted NA->0 ===\n")
frame <- merge(boyfy, fac, by = c("panel_id", "panel_year"), all.x = TRUE)
for (cc in c("any_closure", "facility_complete_closure", "n_installs"))
  frame[is.na(get(cc)), (cc) := 0L]
for (cc in c("n_shed_total", "n_inst", "n_inst_dw", "has_SW"))
  frame[is.na(get(cc)), (cc) := 0L]

# ==============================================================================
# 4. Action coding (flags from facility_panel; Exit precedence)
# ==============================================================================
cat("=== SECTION 4: action coding ===\n")
frame[, action := fcase(
  facility_complete_closure == 1L,                "Exit",
  any_closure == 1L & n_installs >  0L,           "Replace",
  any_closure == 1L & n_installs == 0L,           "Downsize",
  any_closure == 0L & n_installs >  0L,           "Expansion",
  default                                          = "Maintain")]
frame[, group := fifelse(state == "TX", "TX", "Control")]
cat("  action distribution (full frame):\n")
print(frame[, .(n = .N, share = round(.N / nrow(frame), 4)), by = action][order(-n)])

# ==============================================================================
# 5. Merge dcm spine + on_spine flag
# ==============================================================================
cat("=== SECTION 5: dcm spine merge ===\n")
frame <- merge(frame, dcm, by = c("panel_id", "panel_year"), all.x = TRUE)
frame[, on_spine := as.integer(!is.na(s_idx))]
frame[, regime_rb := fifelse(is.na(rho_state), NA_integer_, as.integer(rho_state == 2L))]
cat(sprintf("  facility-years: %s | on dcm spine: %s | TX: %s | Control: %s\n",
            format(nrow(frame), big.mark = ","),
            format(sum(frame$on_spine), big.mark = ","),
            format(frame[group == "TX", .N], big.mark = ","),
            format(frame[group == "Control", .N], big.mark = ",")))

# ==============================================================================
# 6. Diagnostics (cross-check, do NOT mix; print only)
# ==============================================================================
cat("=== SECTION 6: diagnostics ===\n")
sp <- frame[on_spine == 1L & !is.na(dcm_size_bin) & dcm_size_bin != ""]
agree <- mean(sp$size_bin == sp$dcm_size_bin)
cat(sprintf("  size_bin agreement (derived vs dcm) on spine: %.4f (expect ~0.98; no hard stop)\n", agree))
disagree_inst <- mean((frame$n_installs > 0L) != (frame$n_inst > 0L))
cat(sprintf("  disagreement rate (n_installs>0 vs n_inst>0): %.4f (diagnostic only; not reconciled)\n",
            disagree_inst))

# ==============================================================================
# 7. Save ae_frame.csv
# ==============================================================================
cat("=== SECTION 7: save ===\n")
keep <- c("panel_id", "panel_year", "state", "group", "N", "Q", "has_SW", "size_bin",
          "n_shed_total", "n_inst", "n_inst_dw", "any_closure",
          "facility_complete_closure", "n_installs", "action", "on_spine",
          "s_idx", "A_bin", "w_state", "rho_state", "regime_rb", "dcm_size_bin", "boy_stock")
out <- frame[, ..keep]
fwrite(out, file.path(DATA_DIR, "ae_frame.csv"))
cat(sprintf("  saved %s (%s rows x %d cols)\n",
            file.path(DATA_DIR, "ae_frame.csv"), format(nrow(out), big.mark = ","), ncol(out)))
cat("=== AE01 DONE ===\n")

sink(type = "message"); sink(type = "output"); close(.log)
