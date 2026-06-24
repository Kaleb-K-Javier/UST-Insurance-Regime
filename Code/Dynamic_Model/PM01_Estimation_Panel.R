# PM01_Estimation_Panel.R — Portfolio model estimation panel
# TICKET 023 B1. One row per facility-year; composition state + (k,m)/X action coding.
# Outputs: Data/Analysis/pm_panel.csv, logs/PM01_*.log
suppressPackageStartupMessages({ library(data.table); library(here) })

.log_path <- here::here("logs", paste0("PM01_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: PM01_Estimation_Panel\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

# --- constants ---
N_BAR   <- 6L
K_BAR   <- 4L
M_BAR   <- 4L
DATA_DIR <- here("Data", "Analysis")

# 04a ERA_BOUNDS (lines 82-87): era_2006 to 2014-04-30, era_2014 to 2019-01-31,
# era_2019/era_2021 thereafter. 2021 filing byte-identical to 2019 (04a line 266).
# Portfolio model uses 3 labels only; collapse era_2021 into "2019".
era_of_year <- function(y) {
  fcase(y <= 2013L, "2006",
        y <= 2018L, "2014",
        default    = "2019")
}

# MARG order: SW_8 (oldest) .. SW_1, DW_8 .. DW_1
MARG <- c(sprintf("SW_%d", 8:1), sprintf("DW_%d", 8:1))

# ==============================================================================
# 1. Read inputs
# ==============================================================================
cat("=== SECTION 1: read inputs ===\n")
boyfy <- fread(file.path(DATA_DIR, "boy_composition_fy.csv"))
boyfy[, panel_id := as.character(panel_id)]
boyfy <- boyfy[panel_year >= 1999L & N >= 1L]
cat(sprintf("  boyfy (year>=1999, N>=1): %s rows\n", format(nrow(boyfy), big.mark=",")))

boylong <- fread(file.path(DATA_DIR, "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
boylong <- boylong[panel_year >= 1999L]
cat(sprintf("  boylong (year>=1999): %s rows\n", format(nrow(boylong), big.mark=",")))

fac <- fread(file.path(DATA_DIR, "facility_panel.csv"),
             select = c("panel_id", "panel_year", "any_closure",
                        "facility_complete_closure", "n_installs"))
fac[, panel_id := as.character(panel_id)]
stopifnot(!anyDuplicated(fac[, .(panel_id, panel_year)]))

dcm <- fread(file.path(DATA_DIR, "dcm_obs_panel_observed.csv"),
             select = c("panel_id", "panel_year", "rho_state"))
dcm[, panel_id := as.character(panel_id)]
stopifnot(!anyDuplicated(dcm[, .(panel_id, panel_year)]))
cat(sprintf("  facility_panel: %s | dcm: %s\n",
            format(nrow(fac), big.mark=","), format(nrow(dcm), big.mark=",")))

# per-tank capacity from panel_dt: winsorize at 60k (physical per-tank max ~50k;
# capacity==999999 is a sentinel for "large tank, exact unknown" — not garbage)
cat("  reading panel_dt.csv for per-tank capacity...\n")
pd_cap <- fread(file.path(DATA_DIR, "panel_dt.csv"),
                select = c("panel_id", "panel_year", "capacity"))
pd_cap[, panel_id := as.character(panel_id)]
pd_cap <- pd_cap[panel_year >= 1999L]
pd_cap[, capped_cap := pmin(capacity, 60000L)]
cap_agg <- pd_cap[, .(total_cap_capped = sum(capped_cap, na.rm = TRUE),
                       cap_applied      = as.integer(any(capacity > 60000L, na.rm = TRUE))),
                  by = .(panel_id, panel_year)]
cat(sprintf("  panel_dt (year>=1999): %s tank-rows -> %s facility-years aggregated\n",
            format(nrow(pd_cap), big.mark=","), format(nrow(cap_agg), big.mark=",")))
rm(pd_cap); invisible(gc())

# ==============================================================================
# 2. Wide pivot boylong -> 16-cell counts in MARG order
# ==============================================================================
cat("=== SECTION 2: pivot boylong to wide composition ===\n")
boylong[, cellname := sprintf("%s_%d", wall, age_bin)]
wide <- dcast(boylong, panel_id + panel_year ~ cellname, value.var = "n_boy", fill = 0L)
# ensure all 16 MARG columns present and integer
for (mc in MARG) if (!mc %in% names(wide)) wide[, (mc) := 0L]
for (mc in MARG) wide[, (mc) := as.integer(get(mc))]
# rename to n_SW8..n_DW1 (no underscore in wall part)
old_nms <- MARG
new_nms <- c(paste0("n_SW", 8:1), paste0("n_DW", 8:1))
setnames(wide, old_nms, new_nms)
CELL_COLS <- new_nms
cat(sprintf("  wide: %s rows x 16 cells\n", format(nrow(wide), big.mark=",")))

# ==============================================================================
# 3. Merge: boyfy spine + fac flags + wide cells
# ==============================================================================
cat("=== SECTION 3: merge frames ===\n")
frame <- merge(boyfy, fac, by = c("panel_id", "panel_year"), all.x = TRUE)
# targeted NA -> 0 for action flags
for (cc in c("any_closure", "facility_complete_closure", "n_installs"))
  frame[is.na(get(cc)), (cc) := 0L]
for (cc in c("n_shed_total", "n_inst"))
  frame[is.na(get(cc)), (cc) := 0L]

frame <- merge(frame, wide, by = c("panel_id", "panel_year"), all.x = TRUE)
# fill any unmatched wide cells with 0
for (cc in CELL_COLS) frame[is.na(get(cc)), (cc) := 0L]

# merge winsorized capacity aggregates
frame <- merge(frame, cap_agg, by = c("panel_id", "panel_year"), all.x = TRUE)
frame[is.na(cap_applied), cap_applied := 0L]
cat(sprintf("  merged frame: %s rows\n", format(nrow(frame), big.mark=",")))

# ==============================================================================
# 4. Action coding
# ==============================================================================
cat("=== SECTION 4: action coding ===\n")
# derive k_raw and m_raw (from boyfy columns merged in)
frame[, k_raw := n_shed_total]
frame[, m_raw := n_inst]

# action classification
frame[, action_type := fcase(
  facility_complete_closure == 1L,                  "Exit",
  any_closure == 1L & n_installs >  0L,             "Replace",
  any_closure == 1L & n_installs == 0L,             "Downsize",
  any_closure == 0L & n_installs >  0L,             "Expansion",
  default                                           = "Maintain")]

# coded k, m: cap to K_BAR, M_BAR, N-1
frame[, k := fcase(
  action_type == "Exit",      0L,
  action_type == "Maintain",  0L,
  action_type == "Expansion", 0L,
  default = pmin(k_raw, K_BAR, N - 1L))]
frame[, m := fcase(
  action_type == "Exit",      0L,
  action_type == "Maintain",  0L,
  action_type == "Downsize",  0L,
  action_type == "Expansion", pmin(m_raw, M_BAR),  # expansion; excluded downstream
  action_type == "Replace",   pmin(m_raw, M_BAR),
  default = 0L)]

# canonical action column
frame[, action := fcase(
  action_type == "Exit",     "X",
  default                   = paste0(k, ",", m))]

# ==============================================================================
# 5. Exclusion flags (kept as columns; rows NOT deleted)
# ==============================================================================
cat("=== SECTION 5: exclusion flags ===\n")
frame[, excl_state     := as.integer(state %chin% c("KS", "MD"))]
frame[, excl_expansion := as.integer(action_type == "Expansion")]
frame[, excl_bigN      := as.integer(N > N_BAR)]
frame[, excl_offmenu   := as.integer(action_type != "Exit" & (N - k + m) > N_BAR)]
# k0shed: Downsize or Replace coded but k_raw == 0 (flag/composition mismatch)
frame[, excl_k0shed    := as.integer(
  action_type %chin% c("Downsize", "Replace") & k_raw == 0L)]

n_k0shed <- sum(frame$excl_k0shed)
cat(sprintf("  excl_k0shed count: %d\n", n_k0shed))

excl_tab <- frame[, .(
  excl_state     = sum(excl_state),
  excl_expansion = sum(excl_expansion),
  excl_bigN      = sum(excl_bigN),
  excl_offmenu   = sum(excl_offmenu),
  excl_k0shed    = sum(excl_k0shed),
  any_excl       = sum(pmax(excl_state, excl_expansion, excl_bigN, excl_offmenu, excl_k0shed)))]
cat("  exclusion counts:\n"); print(excl_tab)

# ==============================================================================
# 6. era, regime, g; cross-check regime vs rho_state
# ==============================================================================
cat("=== SECTION 6: era, regime, g + rho_state cross-check ===\n")
frame[, era    := era_of_year(panel_year)]
frame[, regime := as.integer(state == "TX")]  # 1=TX(RB), 0=control(FF)
frame[, g      := state]

merged_dcm <- merge(frame[, .(panel_id, panel_year, regime)],
                    dcm, by = c("panel_id", "panel_year"), all.x = FALSE)
merged_dcm <- merged_dcm[!is.na(rho_state)]
# rho_state: 1=FF, 2=RB; regime=1 is TX=RB, regime=0 is control=FF
# cross-check: regime == rho_state - 1
agree_rate <- mean(merged_dcm$regime == (merged_dcm$rho_state - 1L))
cat(sprintf("  regime / rho_state agreement on merged rows: %.4f (require >0.999)\n", agree_rate))
stopifnot(agree_rate > 0.999)
cat("  CROSS-CHECK PASS\n")

# ==============================================================================
# 7. G bin: winsorized (per-tank cap 60k) capacity quartiles on included sample; save breaks
# ==============================================================================
cat("=== SECTION 7: G bin from winsorized capacity quartiles ===\n")
# cap_applied diagnostic: facility-years where any per-tank capacity was winsorized (>60k)
cap_diag <- frame[cap_applied == 1L, .(n = .N), by = g]
setorder(cap_diag, -n)
cat("  Facility-years with any per-tank capacity winsorized (>60k gal), by state:\n")
print(cap_diag)
cat(sprintf("  Total winsorized facility-years: %d\n", frame[cap_applied == 1L, .N]))
cat("  (capacity==999999 is a sentinel for large tank / unknown exact; capped to 60k)\n")

excl_any <- (frame$excl_state | frame$excl_expansion | frame$excl_bigN |
               frame$excl_offmenu | frame$excl_k0shed)
incl_idx <- which(!excl_any & !is.na(frame$total_cap_capped) & frame$total_cap_capped > 0)
G_breaks <- quantile(frame$total_cap_capped[incl_idx], c(0, 0.25, 0.5, 0.75, 1),
                     na.rm = TRUE)
G_breaks <- unique(G_breaks)
cat(sprintf("  G_breaks from capped capacity (on %s included rows): %s\n",
            format(length(incl_idx), big.mark=","),
            paste(round(G_breaks), collapse=" | ")))
frame[, G := as.integer(cut(total_cap_capped, breaks = G_breaks,
                             include.lowest = TRUE, labels = FALSE))]
frame[is.na(G), G := NA_integer_]  # facility-years with missing/zero capped capacity get NA G
cat(sprintf("  G distribution: %s\n",
            paste(names(table(frame$G)), "=", table(frame$G), collapse="; ")))

# ==============================================================================
# 8. Assertion: action cells + exclusions == nrow
# ==============================================================================
cat("=== SECTION 8: count assertion ===\n")
n_exit   <- frame[action == "X", .N]
n_work   <- frame[action != "X", .N]
n_excl_any <- frame[pmax(excl_state, excl_expansion, excl_bigN,
                         excl_offmenu, excl_k0shed) == 1L, .N]
cat(sprintf("  Total rows: %d\n  Exit: %d | Work: %d\n  Any exclusion flag set: %d\n",
            nrow(frame), n_exit, n_work, n_excl_any))
cat("  Action distribution (included rows, action type):\n")
print(frame[excl_state == 0L & excl_expansion == 0L & excl_bigN == 0L &
              excl_offmenu == 0L & excl_k0shed == 0L,
            .(n = .N), by = action][order(-n)])

# ==============================================================================
# 9. Build output with exact enumerated columns
# ==============================================================================
cat("=== SECTION 9: write pm_panel.csv ===\n")
out_cols <- c("panel_id", "panel_year", "g", "regime", "era", "G",
              CELL_COLS, "N", "action", "k", "m",
              "excl_state", "excl_expansion", "excl_bigN", "excl_offmenu", "excl_k0shed")
pm_panel <- frame[, ..out_cols]
# enforce types
pm_panel[, panel_id   := as.character(panel_id)]
pm_panel[, panel_year := as.integer(panel_year)]
pm_panel[, g          := as.character(g)]
pm_panel[, regime     := as.integer(regime)]
pm_panel[, era        := as.character(era)]
pm_panel[, G          := as.integer(G)]
pm_panel[, N          := as.integer(N)]
pm_panel[, action     := as.character(action)]
pm_panel[, k          := as.integer(k)]
pm_panel[, m          := as.integer(m)]

out_path <- file.path(DATA_DIR, "pm_panel.csv")
fwrite(pm_panel, out_path)
cat(sprintf("  saved %s  (%s rows x %d cols)\n",
            out_path, format(nrow(pm_panel), big.mark=","), ncol(pm_panel)))

# save G_breaks as a separate artefact for CF reuse
G_breaks_path <- file.path(DATA_DIR, "pm_G_breaks.rds")
saveRDS(G_breaks, G_breaks_path)
cat(sprintf("  G_breaks saved: %s\n", G_breaks_path))

cat("=== PM01 DONE ===\n")
