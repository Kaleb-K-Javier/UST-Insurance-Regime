# M03_revenue_lookup.R  — Ticket 028
# Build R_rev[G, state, era] = capbar_G * era-averaged fuel margin / SCALE, and ATTACH
# it to PM_Lookups.rds (adds one element; existing elements left byte-identical).
# DATA CONSTRUCTION ONLY — no estimation, no Bellman, no C++. Consumed by ticket 029 (psi*R).
# Inputs : Data/Macro/wholesale_margin_state_year.csv (M02), Data/Analysis/panel_dt.csv,
#          Data/Analysis/pm_panel.csv, Output/Estimation_Results/PM_Lookups.rds
# Outputs: R_rev added to PM_Lookups.rds + Data/Macro/R_rev_long.csv
# Run    : Rscript Code/Macro/M03_revenue_lookup.R  (on whatever machine the psi fit reads from)
#
# R-mapping note: pure R; no cross-language mapping needed. Hard error propagation
# (no tryCatch->NULL): a missing/NA margin for a non-excluded study state STOPS the run.

suppressPackageStartupMessages({ library(data.table); library(here) })

# --- logging (CLAUDE.md, any script that may exceed 1 min) ---
.log_path <- here::here("logs", paste0("M03_revenue_lookup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: M03_revenue_lookup.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

SCALE <- 10000L
ERAS  <- c("2006", "2014", "2019")
# era_of_year MUST byte-match PM01/PM02 era_of_year (ticket 028 / 029 I3)
era_of_year <- function(y) ifelse(y <= 2013L, "2006", ifelse(y <= 2018L, "2014", "2019"))

# === SECTION: STUDY STATES + EXCLUDED ===
cat("=== SECTION: STUDY STATES ===\n")
pm_panel <- fread(here::here("Data", "Analysis", "pm_panel.csv"),
                  select = c("panel_id", "panel_year", "g", "G",
                             "excl_state", "excl_expansion", "excl_bigN", "excl_offmenu", "excl_k0shed"))
study_states <- sort(unique(pm_panel$g))
lk0          <- readRDS(here::here("Output", "Estimation_Results", "PM_Lookups.rds"))
excl_states  <- lk0$excluded_states                         # KS, MD (out of the likelihood)
non_excl     <- setdiff(study_states, excl_states)
cat(sprintf("  study states (%d): %s\n", length(study_states), paste(study_states, collapse = ",")))
cat(sprintf("  excluded (zero-fill if margin missing): %s\n", paste(excl_states, collapse = ",")))

# === STEP 1: MARGIN SERIES (nominal margin_usd_gal, study states, 1999-2020) ===
cat("\n=== STEP 1: MARGIN SERIES ===\n")
m <- fread(here::here("Data", "Macro", "wholesale_margin_state_year.csv"),
           select = c("state", "year", "margin_usd_gal"))
stopifnot(all(c("state", "year", "margin_usd_gal") %in% names(m)))
m <- m[state %in% study_states & year >= 1999L & year <= 2020L]
neg <- m[!is.na(margin_usd_gal) & margin_usd_gal < 0]
if (nrow(neg) > 0) { cat(sprintf("  NOTE: %d negative-margin state-years (reported, not dropped):\n", nrow(neg))); print(neg[order(state, year)]) } else cat("  no negative-margin state-years\n")
miss <- m[state %in% non_excl & is.na(margin_usd_gal)]
if (nrow(miss) > 0) { cat("  *** MISSING margin for NON-excluded study state-years: ***\n"); print(miss[order(state, year)]); stop("non-excluded study state has NA margin in 1999-2020 — real gap; fix 025/026 before 028") }
cat(sprintf("  margin rows: %d | states: %d | years %d-%d\n", nrow(m), length(unique(m$state)), min(m$year), max(m$year)))

# === STEP 2: ERA-AVERAGE MARGIN (Eq.1) ===
cat("\n=== STEP 2: ERA-AVERAGE MARGIN ===\n")
m[, era := era_of_year(year)]
margin_era <- m[!is.na(margin_usd_gal), .(margin = mean(margin_usd_gal)), by = .(state, era)]
for (s in non_excl) {
  got <- margin_era[state == s, era]
  if (!all(ERAS %in% got)) stop(sprintf("non-excluded state %s missing era(s): %s", s, paste(setdiff(ERAS, got), collapse = ",")))
}
stopifnot(all(is.finite(margin_era$margin)))
cat("  margin_era: all non-excluded study states x 3 eras present & finite [OK]\n")
for (e in ERAS) { v <- margin_era[era == e, margin]; cat(sprintf("  era %s: margin/gal range [%.4f, %.4f]\n", e, min(v), max(v))) }

# === STEP 3: capbar_G (Eq.2) — mean capped facility-year capacity per G bin ===
cat("\n=== STEP 3: capbar_G ===\n")
pdt <- fread(here::here("Data", "Analysis", "panel_dt.csv"), select = c("panel_id", "panel_year", "capacity"))
pdt <- pdt[panel_year >= 1999L]
pdt[, capped_cap := pmin(capacity, 60000L)]                 # per-tank winsor at 60k (PM01)
fac <- pdt[, .(total_cap_capped = sum(capped_cap)), by = .(panel_id, panel_year)]  # sum to facility-year
pm  <- merge(pm_panel, fac, by = c("panel_id", "panel_year"), all.x = TRUE)         # pm_panel is 1 row / fac-year
excl_cols <- c("excl_state", "excl_expansion", "excl_bigN", "excl_offmenu", "excl_k0shed")
incl_mask <- !is.na(pm$G) & !is.na(pm$total_cap_capped) & pm$total_cap_capped > 0
for (cc in excl_cols) incl_mask <- incl_mask & (pm[[cc]] == 0L) & !is.na(pm[[cc]])
inc <- pm[incl_mask]
capbar_dt <- inc[, .(capbar = mean(total_cap_capped)), by = G][order(G)]
capbar_G  <- capbar_dt$capbar
stopifnot(nrow(capbar_dt) == 4L, identical(capbar_dt$G, 1:4),
          all(is.finite(capbar_G)), all(capbar_G > 0), all(diff(capbar_G) > 0))
cat(sprintf("  included facility-years: %d\n", nrow(inc)))
cat(sprintf("  capbar_G (G=1..4, gallons): %s  [4 finite, positive, monotone increasing OK]\n",
            paste(round(capbar_G, 0), collapse = ", ")))

# === STEP 4: BUILD R_rev (Eq.3) + ATTACH ===
cat("\n=== STEP 4: BUILD R_rev + ATTACH ===\n")
R_rev <- array(NA_real_, dim = c(4L, length(study_states), 3L),
               dimnames = list(G = as.character(1:4), g = study_states, era = ERAS))
for (gi in seq_along(study_states)) {
  s <- study_states[gi]
  for (ei in seq_along(ERAS)) {
    e  <- ERAS[ei]
    mv <- margin_era[state == s & era == e, margin]
    if (length(mv) == 1L && is.finite(mv)) {
      R_rev[, gi, ei] <- capbar_G * mv / SCALE
    } else if (s %in% excl_states) {
      R_rev[, gi, ei] <- 0.0                                # KS/MD: never enter 029; zero-fill if missing
      cat(sprintf("  NOTE: excluded state %s era %s margin missing -> R_rev=0\n", s, e))
    } else {
      stop(sprintf("non-excluded state %s era %s has no finite margin", s, e))
    }
  }
}
for (s in non_excl) {                                       # assert finite & >0 for every non-excluded cell
  blk <- R_rev[, match(s, study_states), ]
  if (!all(is.finite(blk)) || !all(blk > 0)) stop(sprintf("R_rev not finite/positive for non-excluded state %s", s))
}
cat("  R_rev: finite & > 0 for all non-excluded states x 3 era x 4 G [OK]\n")
for (e in ERAS) { v <- R_rev[, , e]; cat(sprintf("  era %s: R_rev (model units) range [%.4f, %.4f]\n", e, min(v), max(v))) }

# attach to PM_Lookups.rds: ADD R_rev only; verify every pre-existing element byte-identical
lk_path <- here::here("Output", "Estimation_Results", "PM_Lookups.rds")
lk      <- readRDS(lk_path)
pre     <- lk[setdiff(names(lk), "R_rev")]                  # snapshot existing elements
lk$R_rev <- R_rev
saveRDS(lk, lk_path)
lk2 <- readRDS(lk_path)
for (nm in names(pre)) if (!identical(pre[[nm]], lk2[[nm]])) stop(sprintf("PM_Lookups element '%s' changed on re-save — abort", nm))
stopifnot("R_rev" %in% names(lk2), identical(dim(lk2$R_rev), c(4L, length(study_states), 3L)))
cat(sprintf("  PM_Lookups.rds: R_rev added; %d pre-existing elements byte-identical; names: %s [OK]\n",
            length(pre), paste(names(lk2), collapse = ",")))

# R_rev_long.csv (G, g, era, R)
long <- as.data.table(as.data.frame.table(R_rev, responseName = "R"))
setnames(long, c("G", "g", "era", "R"))
long[, G := as.integer(as.character(G))]
fwrite(long, here::here("Data", "Macro", "R_rev_long.csv"))
cat(sprintf("  R_rev_long.csv: %d rows written\n", nrow(long)))

# === STEP 5: SUMMARY (CLAUDE.md output rules) ===
cat("\n=== SUMMARY ===\n")
cat(sprintf("  states: %d (excluded zero-filled: %s)\n", length(study_states), paste(excl_states, collapse = ",")))
cat(sprintf("  capbar_G: %s\n", paste(round(capbar_G, 0), collapse = ", ")))
cat(sprintf("  R_rev dim: %s | overall range [%.4f, %.4f]\n", paste(dim(R_rev), collapse = "x"), min(R_rev), max(R_rev)))
cat(sprintf("  Saved: %s + Data/Macro/R_rev_long.csv\n", lk_path))
cat("=== M03 COMPLETE ===\n")
