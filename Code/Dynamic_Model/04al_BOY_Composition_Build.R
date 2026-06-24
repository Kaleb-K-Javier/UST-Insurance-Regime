# ==============================================================================
# 04al_BOY_Composition_Build.R  -- TICKET 020, script 1 of 4
# ==============================================================================
# The ONLY script allowed to read panel_dt.csv (the 12.7M-row tank-year panel).
# Builds beginning-of-year (BOY) composition counts per facility-year x cell,
# where cell = (wall in {SW,DW}) x (model age bin 1..8). Writes two intermediate
# files under Data/Analysis/. Everything downstream (04am/04an/04ao) reads these.
#
# Membership (decision-time, T013 convention):
#   tank j in facility i's BOY(t) set  <=>  install_yr_int < t  AND
#                                           (is.na(close_yr_int) OR close_yr_int >= t)
#   BOY age_j(t) = t - install_yr_int ; binned with 04b's AGE_BREAKS.
#   wall_j: SW if grepl("single", mm_wall, ignore.case=TRUE) else DW.
#   n_shed_cell(i,t): tanks active at BOY(t) that have closure_event==1 in year t,
#     assigned to their BOY(t) cell (so sheds are a subset of BOY by construction).
#
# READ-ONLY DESCRIPTIVE. No Output/Estimation_Results writes.
# ==============================================================================

suppressMessages({
  library(data.table)
  library(here)
})

cat("=== 04al BOY COMPOSITION BUILD ===\n")
cat(sprintf("Start: %s | R: %s\n\n", format(Sys.time()), R.version.string))

# ------------------------------------------------------------------------------
# Constants copied VERBATIM from 04b_Replacement_Panel_Prep.R (lines 64-70)
# Reviewer will diff: do NOT re-invent these breaks.
# ------------------------------------------------------------------------------
N_AGE      <- 8L
AGE_BREAKS <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
AGE_LABELS <- c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+")

bin_age <- function(a) {
  # 04b idiom: cut(age, AGE_BREAKS, labels=1:N_AGE, right=FALSE, include.lowest=TRUE);
  # missing/age-out -> oldest bin (conservative). (04b:147-150)
  b <- as.integer(cut(a, AGE_BREAKS, labels = 1:N_AGE,
                      right = FALSE, include.lowest = TRUE))
  b[is.na(b)] <- N_AGE
  b
}

# ==============================================================================
# 1. Read panel_dt.csv (the only heavy read) and build the static tank table
# ==============================================================================
cat("=== SECTION 1: load panel_dt + static tank table ===\n")
panel_path <- here("Data", "Analysis", "panel_dt.csv")
stopifnot(file.exists(panel_path))

p <- fread(panel_path, select = c("tank_panel_id", "panel_id", "panel_year",
  "state", "mm_wall", "capacity", "tank_age", "closure_event",
  "install_yr_int", "close_yr_int"))
cat(sprintf("  panel_dt rows: %s | tanks: %s | years: %d-%d\n",
            format(nrow(p), big.mark = ","), format(uniqueN(p$tank_panel_id), big.mark = ","),
            min(p$panel_year), max(p$panel_year)))

# Static (time-invariant) tank attributes: one row per tank.
tank_static <- p[, .(panel_id   = panel_id[1L],
                     state      = state[1L],
                     mm_wall    = mm_wall[1L],
                     capacity   = capacity[1L],
                     install    = install_yr_int[1L],
                     close      = close_yr_int[1L]),
                 by = tank_panel_id]
tank_static[, wall := fifelse(grepl("single", mm_wall, ignore.case = TRUE), "SW", "DW")]
cat(sprintf("  static tanks: %s | SW: %s | DW: %s\n",
            format(nrow(tank_static), big.mark = ","),
            format(sum(tank_static$wall == "SW"), big.mark = ","),
            format(sum(tank_static$wall == "DW"), big.mark = ",")))

# Shed events: (tank, year) with closure_event==1. Used to flag BOY tanks that
# shed in the year. Keyed for fast per-year subsetting.
shed_events <- unique(p[closure_event == 1L, .(tank_panel_id, panel_year)])
setkey(shed_events, panel_year, tank_panel_id)
cat(sprintf("  shed events (tank-years closure_event==1): %s\n",
            format(nrow(shed_events), big.mark = ",")))

rm(p); invisible(gc())

# ==============================================================================
# 2. Year-by-year BOY membership (NO tank x year cross join; iterate years)
# ==============================================================================
cat("=== SECTION 2: year-by-year BOY membership (1999..max) ===\n")
years <- 1999L:max(tank_static$close, na.rm = TRUE)
years <- years[years <= 2020L]   # panel observed through 2020
cat(sprintf("  building BOY for years %d..%d\n", min(years), max(years)))

long_list <- vector("list", length(years))
fy_list   <- vector("list", length(years))

for (k in seq_along(years)) {
  t <- years[k]

  # BOY membership at year t (close never NA in this panel; rule still written
  # with the is.na branch for fidelity to the T013 definition).
  act <- tank_static[install < t & (is.na(close) | close >= t)]
  if (nrow(act) == 0L) next

  act[, age     := t - install]
  act[, age_bin := bin_age(age)]

  # which active tanks shed during year t
  sk <- shed_events[.(t), tank_panel_id, nomatch = NULL]
  act[, shed := as.integer(tank_panel_id %chin% sk)]

  # LONG: one row per (panel_id, state, wall, age_bin)
  cell <- act[, .(n_boy = .N, n_shed = sum(shed)),
              by = .(panel_id, state, wall, age_bin)]
  cell[, panel_year := t]
  long_list[[k]] <- cell

  # facility-year BOY summary
  fy_boy <- act[, .(N = .N, Q = sum(capacity),
                    has_SW = as.integer(any(wall == "SW")),
                    n_shed_total = sum(shed)),
                by = .(panel_id, state)]

  # installs entering at year t (install_yr_int == t)
  inst_t <- tank_static[install == t,
                        .(n_inst = .N, n_inst_dw = sum(wall == "DW")),
                        by = .(panel_id, state)]

  fy_t <- merge(fy_boy, inst_t, by = c("panel_id", "state"), all = TRUE)
  fy_t[is.na(N), N := 0L]
  fy_t[is.na(Q), Q := 0]
  fy_t[is.na(has_SW), has_SW := 0L]
  fy_t[is.na(n_shed_total), n_shed_total := 0L]
  fy_t[is.na(n_inst), n_inst := 0L]
  fy_t[is.na(n_inst_dw), n_inst_dw := 0L]
  fy_t[, panel_year := t]
  fy_list[[k]] <- fy_t

  if (k %% 5L == 0L || k == length(years))
    cat(sprintf("    [%s] year %d: BOY facility-years=%s\n",
                format(Sys.time(), "%H:%M:%S"), t,
                format(nrow(fy_boy), big.mark = ",")))
}

boy_long <- rbindlist(long_list, use.names = TRUE)
boy_fy   <- rbindlist(fy_list,   use.names = TRUE)

# Column order / types per spec deliverable
boy_long[, panel_id := as.character(panel_id)]
boy_fy[,   panel_id := as.character(panel_id)]
setcolorder(boy_long, c("panel_id", "panel_year", "state", "wall", "age_bin",
                        "n_boy", "n_shed"))
setcolorder(boy_fy,   c("panel_id", "panel_year", "state", "N", "Q",
                        "has_SW", "n_shed_total", "n_inst", "n_inst_dw"))
setorder(boy_long, panel_id, panel_year, wall, age_bin)
setorder(boy_fy,   panel_id, panel_year)

cat(sprintf("  boy_long rows: %s | boy_fy facility-years: %s\n",
            format(nrow(boy_long), big.mark = ","),
            format(nrow(boy_fy), big.mark = ",")))

# ==============================================================================
# 3. Cross-check N vs dcm_obs boy_stock (wiring tripwire; hard stop < 0.95)
# ==============================================================================
cat("=== SECTION 3: cross-check N vs dcm_obs boy_stock ===\n")
dcm <- fread(here("Data", "Analysis", "dcm_obs_panel_observed.csv"),
             select = c("panel_id", "panel_year", "boy_stock"))
dcm[, panel_id := as.character(panel_id)]

chk <- merge(boy_fy[, .(panel_id, panel_year, N)],
             dcm, by = c("panel_id", "panel_year"))
match_share <- chk[, mean(N == boy_stock)]
cat(sprintf("  merged rows: %s | exact-match share N==boy_stock: %.4f\n",
            format(nrow(chk), big.mark = ","), match_share))
if (match_share <= 0.95) {
  cat("  MISMATCH DETAIL by year:\n")
  print(chk[, .(n = .N, match = mean(N == boy_stock),
                mean_N = mean(N), mean_boy = mean(boy_stock)),
            by = panel_year][order(panel_year)])
}
stopifnot(match_share > 0.95)

# ==============================================================================
# 4. Save intermediates (Data/Analysis/, regenerable, do not commit)
# ==============================================================================
cat("=== SECTION 4: write outputs ===\n")
out_long <- here("Data", "Analysis", "boy_composition_long.csv")
out_fy   <- here("Data", "Analysis", "boy_composition_fy.csv")
fwrite(boy_long, out_long)
fwrite(boy_fy,   out_fy)
cat(sprintf("  saved: %s (%s rows)\n", out_long, format(nrow(boy_long), big.mark = ",")))
cat(sprintf("  saved: %s (%s rows)\n", out_fy,   format(nrow(boy_fy),   big.mark = ",")))
cat(sprintf("  years covered: %d-%d | facility-years: %s | match_share: %.4f\n",
            min(boy_fy$panel_year), max(boy_fy$panel_year),
            format(nrow(boy_fy), big.mark = ","), match_share))
cat("=== 04al DONE ===\n")
