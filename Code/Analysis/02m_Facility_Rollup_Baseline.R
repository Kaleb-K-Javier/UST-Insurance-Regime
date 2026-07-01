# 02m_Facility_Rollup_Baseline.R
# Baseline / descriptive balance table for the TICKET-038 FACILITY ROLL-UP sample.
# Sample = data_C_active = the matched birth-CEM tanks (install_yr<1999 & cem_weight>0) that are
#   ACTIVE at the 1998 reform, rolled up to facilities = 117,250 stations (UNWEIGHTED).
# This is the EXACT 02l sample, reconstructed here independently (mirror 02b:3965-3988 / 02l Section 1),
#   so the descriptive table and the regressions describe the same stations.
# Mirrors the paper format of T_Baseline_Characteristics_Slide.R BUT on the 038 sample and UNWEIGHTED
#   (the old T_Baseline used fac_cem_matched + fac_cem_weight = the abandoned 02j facility-CEM route).
# Characteristics are FACILITY-level (each station weighted equally, matching the unweighted facility DiD).
#
# Run from repo root ON THE SERVER, AFTER 02l finishes (avoid two 4.6 GB reads at once):
#   & "C:/Program Files/R/R-4.4.3/bin/x64/Rscript.exe" Code/Analysis/02m_Facility_Rollup_Baseline.R
# Prints to console (the balance table IS the deliverable). Outputs:
#   Output/Tables/T_Facility_Rollup_Baseline.csv   (machine-readable)
#   Output/Tables/T_Facility_Rollup_Baseline.tex   (booktabs tabular; editor wraps caption/notes)

suppressPackageStartupMessages({ library(data.table); library(here) })
cat("=== 02m: FACILITY ROLL-UP BASELINE BALANCE TABLE (038 sample, unweighted) ===\n")

ANALYSIS_DIR <- here("Data", "Analysis")
OUT_TAB      <- here("Output", "Tables")
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
fmt_n <- function(x) formatC(as.numeric(x), format = "d", big.mark = ",")

# ---------------------------------------------------------------------------
# 1. Reconstruct data_C_active (mirror 02b:3965-3988 / 02l Section 1)
# ---------------------------------------------------------------------------
mt_path <- file.path(ANALYSIS_DIR, "matched_tanks_birth_cem.csv")
hdr      <- names(fread(mt_path, nrows = 0))
wall_col <- if ("single_wall" %in% hdr) "single_wall" else if ("mm_wall" %in% hdr) "mm_wall" else NA_character_
base_need <- c("panel_id", "tank_panel_id", "texas_treated", "install_yr_int", "cem_weight",
               "panel_year", "closure_event", "first_year_churn")
opt_need  <- intersect(c("capacity", wall_col), hdr)
need      <- intersect(c(base_need, opt_need), hdr)
stopifnot(all(base_need %in% need))   # the sample-defining columns must be present

mt <- fread(mt_path, na.strings = c("", "NA"), select = need)
mt <- mt[first_year_churn == 0L | is.na(first_year_churn)]
dC  <- mt[install_yr_int < 1999L & cem_weight > 0]
act <- union(dC[panel_year == 1998L & closure_event == 0L, unique(panel_id)],
             dC[install_yr_int == 1998L, unique(panel_id)])
dca <- dC[panel_id %in% act]
cat(sprintf("  SAMPLE FUNNEL: data_C facilities = %s  ->  active-at-1998 (038 sample) = %s\n",
            fmt_n(uniqueN(dC$panel_id)), fmt_n(uniqueN(dca$panel_id))))

# ---------------------------------------------------------------------------
# 2. One row per matched tank; tank-level attributes
# ---------------------------------------------------------------------------
keep_tk <- intersect(c("panel_id", "tank_panel_id", "texas_treated",
                       "install_yr_int", "capacity", wall_col), names(dca))
tk <- unique(dca[, ..keep_tk], by = "tank_panel_id")
tk[, pre89      := as.integer(install_yr_int < 1989L)]
tk[, age_reform := 1999L - install_yr_int]
if (!is.na(wall_col)) {
  if (wall_col == "single_wall") tk[, sw := as.integer(single_wall)]
  else                           tk[, sw := as.integer(mm_wall == "Single-Walled")]
}

# ---------------------------------------------------------------------------
# 3. Facility-level characteristics (one row per facility; stations equal-weighted)
# ---------------------------------------------------------------------------
agg <- list(
  texas_treated = quote(texas_treated[1L]),
  n_tanks       = quote(.N),
  avg_age       = quote(mean(age_reform, na.rm = TRUE)),
  pre89_share   = quote(mean(pre89, na.rm = TRUE))
)
if ("capacity" %in% names(tk)) agg$total_cap_k <- quote(sum(capacity, na.rm = TRUE) / 1000)
if ("sw" %in% names(tk))       agg$sw_share    <- quote(mean(sw, na.rm = TRUE))
fac <- tk[, eval(as.call(c(quote(list), agg))), by = panel_id]
cat(sprintf("  Facilities: TX = %s | Control = %s\n",
            fmt_n(fac[texas_treated == 1L, .N]), fmt_n(fac[texas_treated == 0L, .N])))

# ---------------------------------------------------------------------------
# 4. TX vs control balance (UNWEIGHTED) + standardized mean difference
# ---------------------------------------------------------------------------
VARS <- c(n_tanks     = "Tanks per station",
          total_cap_k = "Total capacity (000 gal)",
          avg_age     = "Mean tank age at reform (yrs)",
          sw_share    = "Single-walled share",
          pre89_share = "Pre-1989 vintage share")
VARS <- VARS[names(VARS) %in% names(fac)]   # keep only computed characteristics

smd <- function(x, g) {
  a <- x[g == 1L]; b <- x[g == 0L]
  d <- sqrt((var(a, na.rm = TRUE) + var(b, na.rm = TRUE)) / 2)
  if (!is.finite(d) || d == 0) return(NA_real_)
  (mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE)) / d
}
bal <- rbindlist(lapply(names(VARS), function(v) data.table(
  characteristic = VARS[[v]],
  texas          = mean(fac[texas_treated == 1L][[v]], na.rm = TRUE),
  control        = mean(fac[texas_treated == 0L][[v]], na.rm = TRUE),
  smd            = smd(fac[[v]], fac$texas_treated)
)))
bal <- rbind(
  data.table(characteristic = "N stations",
             texas = fac[texas_treated == 1L, .N],
             control = fac[texas_treated == 0L, .N], smd = NA_real_),
  bal)
fwrite(bal, file.path(OUT_TAB, "T_Facility_Rollup_Baseline.csv"))
cat("\n  --- Balance table (TX vs control, unweighted, 038 sample) ---\n")
print(bal)

# ---------------------------------------------------------------------------
# 5. Paper-format booktabs tabular (.tex); editor adds caption + \textit{Notes:}
# ---------------------------------------------------------------------------
cellf <- function(char, val) ifelse(char == "N stations", fmt_n(val),
                                    formatC(val, format = "f", digits = 2, big.mark = ","))
rows <- sprintf("%s & %s & %s & %s \\\\",
                bal$characteristic, cellf(bal$characteristic, bal$texas),
                cellf(bal$characteristic, bal$control),
                ifelse(is.na(bal$smd), "", formatC(bal$smd, format = "f", digits = 2)))
writeLines(c("\\begin{tabular}{lrrr}", "\\toprule",
             " & Texas & Control & SMD \\\\", "\\midrule",
             rows, "\\bottomrule", "\\end{tabular}"),
           file.path(OUT_TAB, "T_Facility_Rollup_Baseline.tex"))
cat(sprintf("\n  Written: T_Facility_Rollup_Baseline.{csv,tex} (%d rows)\n", nrow(bal)))
cat("=== 02m DONE ===\n")
