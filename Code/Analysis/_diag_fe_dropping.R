# _diag_fe_dropping.R  — why does the composition x year FE drop ~42% of facility-years?
# THROWAWAY diagnostic (delete after). Run from repo root on the server:
#   & "C:/Program Files/R/R-4.4.3/bin/x64/Rscript.exe" Code/Analysis/_diag_fe_dropping.R
# Reconstructs data_C_active (02b:3965-3988) and compares singleton dropping under
#   (a) FULL portfolio multiset x year   [current FE-A]
#   (b) DOMINANT tank-cell x year        [keeps EXACT make_model x cohort, no coarsening]
# and shows where the dropping concentrates by facility tank-count.
suppressMessages({ library(data.table); library(here) })
f  <- function(x) formatC(as.numeric(x), format = "d", big.mark = ",")
pc <- function(a, b) sprintf("%s (%.1f%%)", f(a), 100 * a / b)
ad <- here::here("Data", "Analysis")

need <- c("panel_id","tank_panel_id","install_yr_int","cem_weight","panel_year",
          "closure_event","make_model_noage","first_year_churn")
mt <- fread(file.path(ad, "matched_tanks_birth_cem.csv"), na.strings = c("","NA"), select = need)
mt <- mt[first_year_churn == 0L | is.na(first_year_churn)]
dC  <- mt[install_yr_int < 1999L & cem_weight > 0]
act <- union(dC[panel_year == 1998L & closure_event == 0L, unique(panel_id)],
             dC[install_yr_int == 1998L, unique(panel_id)])
dca <- dC[panel_id %in% act]

# one row per matched tank; cell = make_model_noage x install_yr (the TANK FE cell)
tk <- unique(dca[, .(panel_id, tank_panel_id, make_model_noage, install_yr_int)], by = "tank_panel_id")
tk[, cell := paste(make_model_noage, install_yr_int, sep = "@")]

# (1) facilities by # matched tanks
ntk <- tk[, .(n_tanks = .N), by = panel_id]
cat("=== facilities by # matched tanks ===\n")
print(ntk[, .(facilities = .N), by = n_tanks][order(n_tanks)])
cat(sprintf("single-tank: %s | multi-tank: %s\n\n",
            pc(ntk[n_tanks == 1, .N], nrow(ntk)), pc(ntk[n_tanks > 1, .N], nrow(ntk))))

# (2) cardinality: tank cells vs dominant cells vs full portfolios
comp <- tk[, .(comp = paste(sort(cell), collapse = "|")), by = panel_id]
dom  <- tk[, .N, by = .(panel_id, cell)][order(panel_id, -N)][, .(dom_cell = cell[1L]), by = panel_id]
cat(sprintf("distinct TANK cells (make_model x install_yr): %s\n", f(uniqueN(tk$cell))))
cat(sprintf("distinct DOMINANT cells:                       %s\n", f(uniqueN(dom$dom_cell))))
cat(sprintf("distinct FULL portfolios (multiset):           %s\n\n", f(uniqueN(comp$comp))))

# (3) singleton facility-years under each FE scheme
fy <- unique(dca[, .(panel_id, panel_year)])
fy <- dom[comp[fy, on = "panel_id"], on = "panel_id"]
fy[, n_full := .N, by = .(panel_year, comp)]
fy[, n_dom  := .N, by = .(panel_year, dom_cell)]
N <- nrow(fy)
cat(sprintf("facility-years total: %s\n", f(N)))
cat(sprintf("  FULL portfolio x year   (current FE-A): singleton %s | KEPT %s\n",
            pc(fy[n_full == 1, .N], N), pc(fy[n_full > 1, .N], N)))
cat(sprintf("  DOMINANT cell x year    (exact, proposed): singleton %s | KEPT %s\n\n",
            pc(fy[n_dom == 1, .N], N),  pc(fy[n_dom > 1, .N], N)))

# (4) where FULL-portfolio singletons concentrate, by facility tank-count
fy <- ntk[fy, on = "panel_id"]
cat("=== singleton rate by facility tank count (full vs dominant) ===\n")
print(fy[, .(facility_years = .N,
             pct_singleton_full = round(100 * mean(n_full == 1L), 1),
             pct_singleton_dom  = round(100 * mean(n_dom  == 1L), 1)),
         by = n_tanks][order(n_tanks)])
