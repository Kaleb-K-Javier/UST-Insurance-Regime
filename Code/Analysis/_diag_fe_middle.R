# _diag_fe_middle.R  — test MIDDLE-GROUND facility FEs between single rep-tank and full portfolio.
# All anchored on the OLDEST tank; all keep EXACT make_model x install_yr cells (no cohort coarsening).
# THROWAWAY diagnostic. Run from repo root on the server:
#   & "C:/Program Files/R/R-4.4.3/bin/x64/Rscript.exe" Code/Analysis/_diag_fe_middle.R
suppressMessages({ library(data.table); library(here) })
f  <- function(x) formatC(as.numeric(x), format = "d", big.mark = ",")
ad <- here::here("Data", "Analysis")

need <- c("panel_id","tank_panel_id","install_yr_int","cem_weight","panel_year",
          "closure_event","make_model_noage","first_year_churn")
mt <- fread(file.path(ad, "matched_tanks_birth_cem.csv"), na.strings = c("","NA"), select = need)
mt <- mt[first_year_churn == 0L | is.na(first_year_churn)]
dC  <- mt[install_yr_int < 1999L & cem_weight > 0]
act <- union(dC[panel_year == 1998L & closure_event == 0L, unique(panel_id)],
             dC[install_yr_int == 1998L, unique(panel_id)])
dca <- dC[panel_id %in% act]

tk <- unique(dca[, .(panel_id, tank_panel_id, make_model_noage, install_yr_int)], by = "tank_panel_id")
tk[, cell := paste(make_model_noage, install_yr_int, sep = "@")]

# per-facility keys, all anchored on the OLDEST tank (min install_yr; tie-break min make_model)
setorder(tk, panel_id, install_yr_int, make_model_noage)
key_old <- tk[, .(oldest = cell[1L], n_tanks = .N), by = panel_id]
setorder(tk, panel_id, -install_yr_int, make_model_noage)
key_new <- tk[, .(newest = cell[1L]), by = panel_id]
key_cmp <- tk[, .(comp   = paste(sort(cell), collapse = "|")), by = panel_id]
K <- key_old[key_new, on = "panel_id"][key_cmp, on = "panel_id"]
K[, size_bin   := cut(n_tanks, c(0,1,3,6,Inf), labels = c("1","2-3","4-6","7+"))]
K[, k_oldest   := oldest]
K[, k_oldnew   := paste(oldest, newest,   sep = "||")]
K[, k_oldsize  := paste(oldest, size_bin, sep = "||")]
K[, k_full     := comp]

fy <- unique(dca[, .(panel_id, panel_year)])
fy <- K[fy, on = "panel_id"]
N  <- nrow(fy)

report <- function(keyvar, label) {
  fy[, .n := .N, by = c("panel_year", keyvar)]
  kept <- fy[.n > 1, .N]; cells <- uniqueN(fy[[keyvar]])
  cat(sprintf("  %-30s distinct cells = %-8s KEPT %s (%.1f%%)\n",
              label, f(cells), f(kept), 100 * kept / N))
  fy[, .n := NULL]
}
cat(sprintf("facility-years total: %s\n", f(N)))
cat("KEPT = facility-years in a (key x year) cell with >= 2 facilities (NOT dropped as singletons):\n")
report("k_oldest",  "oldest cell x year (rep)")
report("k_oldnew",  "oldest + newest x year (MG1)")
report("k_oldsize", "oldest + size_bin x year (MG2)")
report("k_full",    "full portfolio x year (ceiling)")
