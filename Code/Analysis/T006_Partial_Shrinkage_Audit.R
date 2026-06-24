################################################################################
# T006_Partial_Shrinkage_Audit.R
# TICKET 006 — Audit: partial shrinkage real choice vs panel-build artifact
# Implements: .claude/TICKETS/006_spec.md (Attempt 1)
################################################################################

#### LOGGING ####
.log_path <- here::here("logs", paste0(
  "T006_Partial_Shrinkage_Audit_",
  format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: T006_Partial_Shrinkage_Audit.R\nR: %s\nWD: %s\n\n",
  .log_path, R.version.string, getwd()))

#### PACKAGES ####
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(here)
})
setDTthreads(0L)

#### CONSTANTS ####
GLOBAL_PANEL_END_YEAR <- 2020L
ANALYSIS_DIR          <- here("Data", "Analysis")
AUDIT_DIR             <- here("Reports", "Audits")
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
set.seed(20260527L)

fmt_n <- function(x) format(as.integer(x), big.mark = ",")


########################################################################
cat("=== LOAD DATA ===\n")
########################################################################

cat("  Loading panel_dt.csv (12.7M rows)...\n")
panel_dt <- fread(file.path(ANALYSIS_DIR, "panel_dt.csv"),
                  na.strings = c("", "NA", "N/A"))
panel_dt[, tank_closed_date    := as.IDate(tank_closed_date)]
panel_dt[, tank_installed_date := as.IDate(tank_installed_date)]
cat(sprintf("  panel_dt: %s rows | %s tanks | %s facilities\n",
  fmt_n(nrow(panel_dt)),
  fmt_n(uniqueN(panel_dt$tank_panel_id)),
  fmt_n(uniqueN(panel_dt$panel_id))))
cat(sprintf("  Columns: %s\n", paste(names(panel_dt), collapse = ", ")))

cat("  Loading facility_panel.csv...\n")
fp <- fread(file.path(ANALYSIS_DIR, "facility_panel.csv"),
            na.strings = c("", "NA", "N/A"))
cat(sprintf("  facility_panel: %s rows | %s facilities\n",
  fmt_n(nrow(fp)), fmt_n(uniqueN(fp$panel_id))))

cat("  Loading dcm_obs_panel_observed.csv...\n")
dcm_obs <- fread(file.path(ANALYSIS_DIR, "dcm_obs_panel_observed.csv"),
                 na.strings = c("", "NA", "N/A"))
cat(sprintf("  dcm_obs: %s rows\n", fmt_n(nrow(dcm_obs))))

# Facility_panel restricted to DCM observed sample (for Task 3)
dcm_keys  <- unique(dcm_obs[, .(panel_id, panel_year)])
fp_dcm    <- fp[dcm_keys, on = c("panel_id", "panel_year"), nomatch = NULL]
cat(sprintf("  fp_dcm (facility_panel ∩ DCM obs): %s rows\n", fmt_n(nrow(fp_dcm))))
cat("\n")

# Verify expected columns exist
stopifnot(
  "n_tanks_active missing"           = "n_tanks_active"           %in% names(fp),
  "n_tanks_eoy missing"              = "n_tanks_eoy"              %in% names(fp),
  "n_closures missing"               = "n_closures"               %in% names(fp),
  "n_installs missing"               = "n_installs"               %in% names(fp),
  "any_closure missing"              = "any_closure"              %in% names(fp),
  "facility_complete_closure missing"= "facility_complete_closure" %in% names(fp),
  "replacement_closure_year missing" = "replacement_closure_year" %in% names(fp),
  "permanent_closure_year missing"   = "permanent_closure_year"   %in% names(fp),
  "texas_treated missing"            = "texas_treated"            %in% names(fp),
  "tank_closed_date in panel_dt"     = "tank_closed_date"         %in% names(panel_dt),
  "tank_installed_date in panel_dt"  = "tank_installed_date"      %in% names(panel_dt)
)
cat("  Column presence checks passed.\n\n")


########################################################################
cat("=== TASK 1: Tank lifecycle integrity ===\n")
########################################################################

# Aggregate panel_dt by tank: first/last active year, closed year
# tank_closed_date is a fixed attribute of the tank (constant across all panel rows)
setkey(panel_dt, tank_panel_id, panel_year)

tank_lifecycle <- panel_dt[, .(
  tank_first_active_year = min(panel_year),
  tank_last_active_year  = max(panel_year),
  tank_closed_year       = {
    cd <- tank_closed_date[!is.na(tank_closed_date)]
    if (length(cd) > 0L) year(cd[1L]) else NA_integer_
  }
), by = tank_panel_id]

cat(sprintf("  Unique tanks in panel_dt: %s\n", fmt_n(nrow(tank_lifecycle))))

# Three-category classification
tank_lifecycle[, category := fcase(
  # (c) Still operating at panel end, no close date
  tank_last_active_year == GLOBAL_PANEL_END_YEAR & is.na(tank_closed_year),
  "c_persistent_open",
  # (b) Disappeared before 2020, no close date — silent disappearance
  is.na(tank_closed_year) & tank_last_active_year < GLOBAL_PANEL_END_YEAR,
  "b_silent_disappearance",
  # (a) Has a recorded closure date
  !is.na(tank_closed_year),
  "a_has_closure",
  # Residual: last_year == 2020 but has a close date (closed at panel end)
  default = "a_has_closure"
)]

task1_summary <- tank_lifecycle[, .N, by = category]
task1_summary[, pct := round(100 * N / sum(N), 1)]
setorder(task1_summary, category)
cat("  Three-category breakdown:\n")
print(task1_summary)

# Histogram data: category (a) — gap between last_active and closed year
t1_has_closure <- tank_lifecycle[category == "a_has_closure"]
t1_has_closure[, year_diff := tank_last_active_year - tank_closed_year]

diff_summary <- t1_has_closure[, .(
  N        = .N,
  mean_gap = round(mean(year_diff, na.rm = TRUE), 2),
  median   = as.integer(median(year_diff, na.rm = TRUE)),
  p5       = as.integer(quantile(year_diff, 0.05, na.rm = TRUE)),
  p95      = as.integer(quantile(year_diff, 0.95, na.rm = TRUE)),
  exact_0  = sum(year_diff == 0L, na.rm = TRUE),
  within1  = sum(abs(year_diff) <= 1L, na.rm = TRUE)
)]
cat("  Gap summary (last_active_year − closed_year) for category (a):\n")
print(diff_summary)

hist_plot <- ggplot(t1_has_closure[abs(year_diff) <= 10L], aes(x = year_diff)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title    = "Tank last-active-year minus recorded closure-year",
    subtitle = "Category (a): tanks with a recorded closure date; clamped to [-10, +10]",
    x        = "last_active_year - tank_closed_year",
    y        = "Count"
  ) +
  theme_minimal(base_size = 12)
hist_path <- file.path(AUDIT_DIR, "task1_lifecycle_histogram.png")
ggsave(hist_path, hist_plot, width = 7, height = 4, dpi = 150)
cat(sprintf("  Saved: %s\n", hist_path))

t1_silent <- tank_lifecycle[category == "b_silent_disappearance"]
cat(sprintf("  Silent disappearances (category b): %s tanks (%.1f%%)\n\n",
  fmt_n(nrow(t1_silent)),
  task1_summary[category == "b_silent_disappearance", pct]))


########################################################################
cat("=== TASK 2: Sample-event traceback ===\n")
########################################################################

# Reproduce partial-shrinkage classification on facility_panel
fp_d <- fp[((texas_treated == 1L & panel_year >= 2006L) |
            (texas_treated == 0L & panel_year >= 1999L)) &
           n_tanks_active > 0L]
setorder(fp_d, panel_id, panel_year)

fp_d[, install_year_of := fifelse(n_installs > 0L, panel_year, NA_integer_)]
fp_d[, years_to_next_install := {
  iy <- install_year_of[!is.na(install_year_of)]
  vapply(panel_year, function(y) {
    fut <- iy[iy > y]
    if (length(fut) == 0L) NA_integer_ else as.integer(min(fut) - y)
  }, integer(1L))
}, by = panel_id]

fp_d[, is_partial := any_closure == 1L &
                     is.na(years_to_next_install) &
                     facility_complete_closure == 0L]
fp_d[, gap := (n_tanks_active - n_closures + n_installs) - n_tanks_eoy]

ps_bad <- fp_d[is_partial == TRUE & gap > 0L]
cat(sprintf("  Partial-shrinkage events with gap > 0: %s\n", fmt_n(nrow(ps_bad))))
gap_dist <- ps_bad[, .N, by = .(gap_stratum = fcase(
  gap == 1L, "1", gap == 2L, "2", gap == 3L, "3",
  gap == 4L, "4", gap >= 5L, "5+"))][order(gap_stratum)]
print(gap_dist)

# Stratified sample: 10 per stratum; fill shortfall from gap=1 then gap=2
ps_bad[, gap_stratum := fcase(
  gap == 1L, "1", gap == 2L, "2", gap == 3L, "3",
  gap == 4L, "4", gap >= 5L, "5+"
)]
strata_order  <- c("1", "2", "3", "4", "5+")
sample_pieces <- vector("list", length(strata_order))
shortfall_n   <- 0L

for (s in strata_order) {
  pool   <- ps_bad[gap_stratum == s]
  n_take <- min(10L, nrow(pool))
  shortfall_n <- shortfall_n + (10L - n_take)
  if (n_take > 0L) sample_pieces[[which(strata_order == s)]] <- pool[sample(.N, n_take)]
}
sample_done <- rbindlist(Filter(Negate(is.null), sample_pieces))

if (shortfall_n > 0L) {
  taken_key <- paste(sample_done$panel_id, sample_done$panel_year)
  refill    <- ps_bad[
    gap_stratum %in% c("1", "2") &
    !paste(panel_id, panel_year) %in% taken_key
  ]
  n_refill <- min(shortfall_n, nrow(refill))
  if (n_refill > 0L) {
    sample_done <- rbindlist(list(sample_done, refill[sample(.N, n_refill)]))
    cat(sprintf("  Shortfall %d: refilled %d from gap=1/2\n", shortfall_n, n_refill))
  }
}
cat(sprintf("  Final sample: %d events\n", nrow(sample_done)))

# Check if panel_id_history column exists in panel_dt
has_pid_history <- "panel_id_history" %in% names(panel_dt)
cat(sprintf("  panel_id_history column in panel_dt: %s\n", has_pid_history))

# Trace missing tanks for each sampled event
setkey(panel_dt, panel_id, panel_year)

trace_rows <- vector("list", nrow(sample_done))
for (ev_i in seq_len(nrow(sample_done))) {
  sid   <- sample_done$panel_id[ev_i]
  syr   <- sample_done$panel_year[ev_i]
  gap_i <- sample_done$gap[ev_i]

  tanks_t   <- panel_dt[.(sid, syr),        tank_panel_id]
  tanks_tp1 <- panel_dt[.(sid, syr + 1L),   tank_panel_id]
  missing   <- setdiff(tanks_t, tanks_tp1)

  if (length(missing) == 0L) {
    trace_rows[[ev_i]] <- data.table(
      sample_panel_id        = sid,
      sample_panel_year      = syr,
      gap                    = gap_i,
      missing_tank_id        = NA_character_,
      tank_closed_date       = NA_character_,
      mm_wall                = NA_character_,
      mm_fuel                = NA_character_,
      mm_capacity            = NA_character_,
      disappearance_category = "no_missing_in_t_plus_1",
      notes                  = "gap>0 but tank set unchanged year-on-year; disappearance may be year t+2+"
    )
    next
  }

  setkey(panel_dt, tank_panel_id, panel_year)
  event_rows <- vector("list", length(missing))

  for (tk_i in seq_along(missing)) {
    this_tank <- missing[tk_i]
    meta      <- panel_dt[.(this_tank)]  # all rows for this tank

    close_date_val  <- NA_character_
    close_yr        <- NA_integer_
    if (nrow(meta) > 0L && any(!is.na(meta$tank_closed_date))) {
      cd_vals      <- meta$tank_closed_date[!is.na(meta$tank_closed_date)]
      close_date_val <- as.character(cd_vals[1L])
      close_yr       <- year(cd_vals[1L])
    }
    wall_val     <- if (nrow(meta) > 0L) meta$mm_wall[1L]     else NA_character_
    fuel_val     <- if (nrow(meta) > 0L) meta$mm_fuel[1L]     else NA_character_
    cap_val      <- if (nrow(meta) > 0L) meta$mm_capacity[1L] else NA_character_

    # Check reassignment: tank appears at a different panel_id in year syr+1
    setkey(panel_dt, tank_panel_id, panel_year)
    later_yr <- panel_dt[.(this_tank, syr + 1L)]
    panel_id_tp1 <- if (nrow(later_yr) > 0L) later_yr$panel_id[1L] else NA_character_

    cat_i <- if (!is.na(close_yr) && close_yr == syr) {
      "i_closure_recorded"
    } else if (!is.na(close_yr) && abs(close_yr - syr) == 1L) {
      "ii_closure_year_off"
    } else if (!is.na(close_yr)) {
      "iii_closure_way_off"
    } else if (!is.na(panel_id_tp1) && panel_id_tp1 != sid) {
      "iv_panel_id_reassignment"
    } else if (!is.na(wall_val) && grepl("Unknown", wall_val, ignore.case = TRUE)) {
      "v_unknown_field_flip"
    } else {
      "vi_silent"
    }

    notes_i <- switch(cat_i,
      "i_closure_recorded"     = sprintf("closed %s; 02b should have counted this", close_date_val),
      "ii_closure_year_off"    = sprintf("closed %s; off by 1yr from gap year %d", close_date_val, syr),
      "iii_closure_way_off"    = sprintf("closed %s; gap year %d (diff=%d)", close_date_val, syr,
                                   abs(close_yr - syr)),
      "iv_panel_id_reassignment" = sprintf("moved to panel_id=%s in yr %d", panel_id_tp1, syr + 1L),
      "v_unknown_field_flip"   = sprintf("mm_wall=%s at last observation", wall_val),
      "vi_silent"              = "no close date, no reassignment, no unknown-wall flag"
    )

    event_rows[[tk_i]] <- data.table(
      sample_panel_id        = sid,
      sample_panel_year      = syr,
      gap                    = gap_i,
      missing_tank_id        = this_tank,
      tank_closed_date       = close_date_val,
      mm_wall                = wall_val,
      mm_fuel                = fuel_val,
      mm_capacity            = cap_val,
      disappearance_category = cat_i,
      notes                  = notes_i
    )
  }
  setkey(panel_dt, panel_id, panel_year)  # restore key
  trace_rows[[ev_i]] <- rbindlist(event_rows)
}

trace_table <- rbindlist(trace_rows, fill = TRUE)
cat(sprintf("  Trace table: %d rows\n", nrow(trace_table)))

task2_summary <- trace_table[
  !is.na(missing_tank_id),
  .N, by = disappearance_category
][order(-N)]
task2_summary[, pct := round(100 * N / sum(N), 1)]
cat("  Category breakdown:\n")
print(task2_summary)

fwrite(trace_table, file.path(AUDIT_DIR, "trace_table.csv"))
cat(sprintf("  Saved: %s\n\n", file.path(AUDIT_DIR, "trace_table.csv")))


########################################################################
cat("=== TASK 3: Action-flag reconciliation ===\n")
########################################################################

# Pre-compute facility-level max install year for Replace/Permanent checks
fac_install <- panel_dt[!is.na(tank_installed_date), .(
  max_install_yr = max(year(tank_installed_date))
), by = panel_id]
setkey(fac_install, panel_id)
setkey(panel_dt, panel_id, panel_year)

reconcile_flag <- function(flag_name, flag_col, fp_dt, n_sample = 100L) {
  cat(sprintf("  Flag: %s\n", flag_name))
  pool <- fp_dt[get(flag_col) == 1L]
  if (nrow(pool) == 0L) {
    cat(sprintf("    No events found for %s\n", flag_name))
    return(data.table(flag_name = flag_name, N_sampled = 0L,
                      N_flag_ok = NA_integer_, pct_ok = NA_real_,
                      notes_on_failures = "no events found in fp_dcm"))
  }
  n_take <- min(n_sample, nrow(pool))
  samp   <- pool[sample(.N, n_take)]

  flag_ok_vec <- logical(n_take)
  note_vec    <- character(n_take)

  for (i in seq_len(n_take)) {
    s_id  <- samp$panel_id[i]
    s_yr  <- samp$panel_year[i]
    tanks <- panel_dt[.(s_id, s_yr)]

    if (flag_col == "facility_complete_closure") {
      # All tanks in panel_dt at this (facility, year) must have close date <= s_yr
      if (nrow(tanks) == 0L) {
        flag_ok_vec[i] <- FALSE
        note_vec[i]    <- "no tanks in panel_dt"
      } else {
        all_closed <- all(!is.na(tanks$tank_closed_date) &
                          year(tanks$tank_closed_date) <= s_yr)
        flag_ok_vec[i] <- all_closed
        if (!all_closed) {
          n_open    <- sum(is.na(tanks$tank_closed_date) |
                           year(tanks$tank_closed_date) > s_yr)
          note_vec[i] <- sprintf("%d tank(s) not closed by %d", n_open, s_yr)
        }
      }

    } else if (flag_col == "replacement_closure_year") {
      # At least one tank closes at s_yr AND facility has an install >= s_yr
      # (02b uses date-level comparison so same-year installs qualify as "replacement")
      has_close <- nrow(tanks) > 0L &&
                   any(!is.na(tanks$tank_closed_date) & year(tanks$tank_closed_date) == s_yr)
      max_inst_vec <- fac_install[.(s_id), max_install_yr]
      max_inst     <- if (length(max_inst_vec) == 0L || is.na(max_inst_vec[1L]))
        NA_integer_ else max_inst_vec[1L]
      has_later_install <- !is.na(max_inst) && max_inst >= s_yr
      flag_ok_vec[i]    <- has_close && has_later_install
      if (!flag_ok_vec[i])
        note_vec[i] <- sprintf("close_at_t=%s install_gte_t=%s (max=%s)",
          has_close, has_later_install,
          if (is.na(max_inst)) "NA" else as.character(max_inst))

    } else if (flag_col == "permanent_closure_year") {
      # At least one tank closes at s_yr AND no later install
      has_close <- nrow(tanks) > 0L &&
                   any(!is.na(tanks$tank_closed_date) & year(tanks$tank_closed_date) == s_yr)
      max_inst_vec <- fac_install[.(s_id), max_install_yr]
      max_inst     <- if (length(max_inst_vec) == 0L || is.na(max_inst_vec[1L]))
        NA_integer_ else max_inst_vec[1L]
      has_later_install <- !is.na(max_inst) && max_inst > s_yr
      flag_ok_vec[i]    <- has_close && !has_later_install
      if (!flag_ok_vec[i])
        note_vec[i] <- sprintf("close_at_t=%s later_install=%s (max=%s)",
          has_close, has_later_install,
          if (is.na(max_inst)) "NA" else as.character(max_inst))
    }
  }

  n_ok     <- sum(flag_ok_vec)
  pct      <- round(100 * n_ok / n_take, 1)
  fails    <- unique(note_vec[!flag_ok_vec & nchar(note_vec) > 0L])
  top_fail <- paste(head(fails, 3L), collapse = " | ")

  cat(sprintf("    %d/%d OK (%.1f%%)\n", n_ok, n_take, pct))
  data.table(flag_name         = flag_name,
             N_sampled         = n_take,
             N_flag_ok         = n_ok,
             pct_ok            = pct,
             notes_on_failures = top_fail)
}

recon_exit    <- reconcile_flag("facility_complete_closure", "facility_complete_closure", fp_dcm)
recon_replace <- reconcile_flag("replacement_closure_year",  "replacement_closure_year",  fp_dcm)
recon_perm    <- reconcile_flag("permanent_closure_year",    "permanent_closure_year",    fp_dcm)

reconciliation_rates <- rbindlist(list(recon_exit, recon_replace, recon_perm))
cat("  Full reconciliation table:\n")
print(reconciliation_rates[, .(flag_name, N_sampled, N_flag_ok, pct_ok)])

fwrite(reconciliation_rates, file.path(AUDIT_DIR, "reconciliation_rates.csv"))
cat(sprintf("  Saved: %s\n\n", file.path(AUDIT_DIR, "reconciliation_rates.csv")))


########################################################################
cat("=== TASK 4: Source of tank disappearance ===\n")
########################################################################

silent_tanks <- t1_silent[, .(tank_panel_id, last_yr = tank_last_active_year)]
cat(sprintf("  Total silent tanks: %s\n", fmt_n(nrow(silent_tanks))))

if (nrow(silent_tanks) > 10000L) {
  cat("  Capping at 10,000 (random sample)\n")
  silent_tanks <- silent_tanks[sample(.N, 10000L)]
}

# Pull all panel_dt rows for these tanks at once (vectorized)
setkey(panel_dt, tank_panel_id, panel_year)
silent_rows <- panel_dt[tank_panel_id %in% silent_tanks$tank_panel_id]
cat(sprintf("  panel_dt rows for silent tanks: %s\n", fmt_n(nrow(silent_rows))))

# Per-tank: last panel_id and metadata at last year
silent_meta <- silent_rows[, .(
  last_panel_id   = panel_id[which.max(panel_year)],
  last_yr_check   = max(panel_year),
  last_mm_wall    = mm_wall[which.max(panel_year)],
  last_mm_fuel    = mm_fuel[which.max(panel_year)],
  last_mm_cap     = mm_capacity[which.max(panel_year)],
  prev_mm_fuel    = if (.N >= 2L) mm_fuel[order(panel_year)][.N - 1L]    else NA_character_,
  prev_mm_cap     = if (.N >= 2L) mm_capacity[order(panel_year)][.N - 1L] else NA_character_
), by = tank_panel_id]

silent_meta <- merge(silent_meta, silent_tanks, by = "tank_panel_id")

# Mechanism 2 proxy: did facility's n_unk_wall_active increase in the year after disappearance?
has_unk_wall_col <- "n_unk_wall_active" %in% names(fp)
cat(sprintf("  n_unk_wall_active column in facility_panel: %s\n", has_unk_wall_col))
if (!has_unk_wall_col) {
  fp[, n_unk_wall_active := 0L]
  cat("  WARNING: n_unk_wall_active not found; mechanism 2 proxy disabled\n")
}
fp_unk <- fp[, .(panel_id, panel_year, n_unk_wall_active)]
setkey(fp_unk, panel_id, panel_year)

unk_at    <- fp_unk[.(silent_meta$last_panel_id, silent_meta$last_yr)][
  , .(unk_at = n_unk_wall_active)
]
unk_after <- fp_unk[.(silent_meta$last_panel_id, silent_meta$last_yr + 1L)][
  , .(unk_after = n_unk_wall_active)
]

silent_meta[, unk_at_val    := unk_at$unk_at]
silent_meta[, unk_after_val := unk_after$unk_after]
silent_meta[is.na(unk_at_val),    unk_at_val    := 0L]
silent_meta[is.na(unk_after_val), unk_after_val := 0L]

# Mechanism 3: fuel or capacity flipped between last two years in panel_dt
silent_meta[, m3_flip := (!is.na(prev_mm_fuel) & !is.na(last_mm_fuel) &
                           prev_mm_fuel != last_mm_fuel) |
                         (!is.na(prev_mm_cap)  & !is.na(last_mm_cap)  &
                           prev_mm_cap  != last_mm_cap)]

# Mechanism 4: state-year spike >= 50 tanks disappearing in same (state, year)
tank_state <- silent_rows[, .(state = first(state), last_yr2 = max(panel_year)),
                           by = tank_panel_id]
mech4_agg  <- tank_state[, .N, by = .(state, last_yr2)][N >= 50L][order(-N)]
cat("  Mechanism 4 candidates (state-year spikes >= 50):\n")
if (nrow(mech4_agg) > 0L) print(mech4_agg) else cat("  (none)\n")

spike_keys <- if (nrow(mech4_agg) > 0L)
  mech4_agg[, paste(state, last_yr2)] else character(0)

silent_meta <- merge(silent_meta, tank_state[, .(tank_panel_id, state, last_yr2)],
                     by = "tank_panel_id", all.x = TRUE)
silent_meta[, in_spike := paste(state, last_yr2) %in% spike_keys]

# Assign mechanism in priority order: 4 > 2 > 3 > 5
# Mechanism 1 (panel_id reassignment) cannot be detected from panel_dt alone since
# tank_panel_id is constructed to include facility_id; a reassigned tank would have
# a DIFFERENT tank_panel_id and would not appear in silent_rows under the old ID.
# This limitation is documented in the report.
silent_meta[, mechanism := fcase(
  in_spike == TRUE,                                      4L,
  unk_after_val > unk_at_val,                            2L,
  m3_flip == TRUE,                                       3L,
  default =                                              5L
)]

# Build evidence strings for each mechanism separately (avoid non-vectorized fcase lookup)
silent_meta[, evidence := NA_character_]
silent_meta[mechanism == 2L,
  evidence := sprintf("n_unk_wall_active: %d (yr %d) -> %d (yr %d) [proxy]",
    unk_at_val, last_yr, unk_after_val, last_yr + 1L)]
silent_meta[mechanism == 3L,
  evidence := sprintf("fuel: %s->%s; cap: %s->%s",
    prev_mm_fuel, last_mm_fuel, prev_mm_cap, last_mm_cap)]
silent_meta[mechanism == 5L,
  evidence := sprintf("last_active_yr=%d; no wall-flip proxy, no state-yr spike, no close_date",
    last_yr)]
# Mechanism 4: per-row lookup into mech4_agg (uses scalar state/last_yr2 inside sapply)
if (any(silent_meta$mechanism == 4L)) {
  silent_meta[mechanism == 4L, evidence := {
    sapply(seq_len(.N), function(i) {
      st <- state[i]; yr <- last_yr2[i]
      n_spike <- mech4_agg[state == st & last_yr2 == yr, N]
      n_spike  <- if (length(n_spike) == 0L) 0L else n_spike[1L]
      sprintf("state=%s year=%d; %d tanks disappeared this state-year", st, yr, n_spike)
    })
  }]
}

disappearance_mechanisms <- silent_meta[, .(tank_panel_id, mechanism, evidence)]

mech_summary <- disappearance_mechanisms[, .N, by = mechanism][order(mechanism)]
mech_summary[, pct := round(100 * N / sum(N), 1)]
cat("  Mechanism summary:\n")
print(mech_summary)

fwrite(disappearance_mechanisms, file.path(AUDIT_DIR, "disappearance_mechanisms.csv"))
cat(sprintf("  Saved: %s\n\n", file.path(AUDIT_DIR, "disappearance_mechanisms.csv")))


########################################################################
cat("=== TASK 5: 02b code audit (static) ===\n")
cat("  (Findings documented in 02b_field_audit.md)\n\n")
########################################################################

# Verify key line numbers by checking actual code (read-only)
code_02b <- readLines(here("Code", "Analysis", "02b_Tank_level_Panel_Build.R"))
grep_02b <- function(pattern) which(grepl(pattern, code_02b, fixed = TRUE))

lines_ntanks_active  <- grep_02b("n_tanks_active        = .N")
lines_ntanks_eoy     <- grep_02b("n_tanks_eoy  = n_tanks_active - n_closures")
lines_nclosures      <- grep_02b("n_closures   = sum(closure_event")
lines_ninstalls      <- grep_02b("n_installs         = .N")
lines_sw_installs    <- grep_02b("n_sw_installs")
lines_dw_installs    <- grep_02b("n_dw_installs")
lines_perm           <- grep_02b("permanent_closure_year   = as.integer")
lines_replace        <- grep_02b("replacement_closure_year = as.integer")
lines_fcc            <- grep_02b("facility_complete_closure := as.integer")

cat(sprintf("  n_tanks_active: line(s) %s\n",    paste(lines_ntanks_active, collapse=",")))
cat(sprintf("  n_tanks_eoy:    line(s) %s\n",    paste(lines_ntanks_eoy,    collapse=",")))
cat(sprintf("  n_closures:     line(s) %s\n",    paste(lines_nclosures,     collapse=",")))
cat(sprintf("  n_installs:     line(s) %s\n",    paste(lines_ninstalls,     collapse=",")))
cat(sprintf("  n_sw_installs:  line(s) %s\n",    paste(lines_sw_installs,   collapse=",")))
cat(sprintf("  n_dw_installs:  line(s) %s\n",    paste(lines_dw_installs,   collapse=",")))

# Check sw/dw label swap
sw_install_line <- if (length(lines_sw_installs) > 0L) code_02b[lines_sw_installs[1L]] else ""
dw_install_line <- if (length(lines_dw_installs) > 0L) code_02b[lines_dw_installs[1L]] else ""
cat(sprintf("  sw_install filter: %s\n", trimws(sw_install_line)))
cat(sprintf("  dw_install filter: %s\n", trimws(dw_install_line)))

# Check first_year_churn exclusion in n_closures
churn_excl_lines <- grep_02b("first_year_churn == 0L")
cat(sprintf("  first_year_churn exclusion lines: %s\n\n", paste(churn_excl_lines, collapse=",")))


########################################################################
cat("=== WRITE DELIVERABLES ===\n")
########################################################################

# Convenience variables for report
pct_silent   <- task1_summary[category == "b_silent_disappearance", pct]
n_silent_val <- task1_summary[category == "b_silent_disappearance", N]
n_closure_v  <- task1_summary[category == "a_has_closure",          N]
n_persist_v  <- task1_summary[category == "c_persistent_open",      N]
total_tanks  <- sum(task1_summary$N)

if (length(pct_silent) == 0L || is.na(pct_silent)) pct_silent <- 0

get_pct <- function(cat_nm) {
  v <- task2_summary[disappearance_category == cat_nm, pct]
  if (length(v) == 0L) 0 else v[1L]
}
pct_cat_i   <- get_pct("i_closure_recorded")
pct_cat_iv  <- get_pct("iv_panel_id_reassignment")
pct_cat_vi  <- get_pct("vi_silent")

verdict <- if (pct_cat_i >= 50) {
  "PANEL_BUG"
} else if ((pct_cat_iv + pct_cat_vi) >= 50) {
  "DATA_NOISE"
} else {
  "MIX"
}
cat(sprintf("  Verdict: %s\n", verdict))

t3_reliable   <- reconciliation_rates[pct_ok >= 95, flag_name]
t3_unreliable <- reconciliation_rates[!is.na(pct_ok) & pct_ok < 95, flag_name]

# ---- 02b_field_audit.md ----
sw_line_actual <- trimws(sw_install_line)
dw_line_actual <- trimws(dw_install_line)
swap_note <- if (grepl("Double-Walled", sw_line_actual)) {
  "CONFIRMED SWAP: n_sw_installs filter selects Double-Walled"
} else if (grepl("Single-Walled", sw_line_actual)) {
  "Labels appear correct (Single-Walled for n_sw_installs)"
} else {
  "Could not verify from grep result"
}

audit_02b_lines <- c(
  "# 02b Field Audit — Count Field Construction",
  sprintf("_Generated: %s | Script: T006_Partial_Shrinkage_Audit.R_", Sys.Date()),
  "",
  "## Count field sources",
  "",
  "### n_tanks_active",
  sprintf("- **Line(s)**: %s (`n_tanks_active = .N`)", paste(lines_ntanks_active, collapse=", ")),
  "- **Source**: `tank_year_panel[state %in% STUDY_STATES]`",
  "- **Filters**: STUDY_STATES only. Includes `mm_wall == 'Unknown-Wall'`,",
  "  `make_model_tank == NA`, and `first_year_churn == 1` tanks.",
  "- **Note**: This is `.N` — every tank-year row present for this facility-year.",
  "",
  "### n_tanks_eoy",
  sprintf("- **Line(s)**: %s (`n_tanks_eoy = n_tanks_active - n_closures`)",
    paste(lines_ntanks_eoy, collapse=", ")),
  "- **Source**: Derived arithmetic — NOT aggregated directly from tank data.",
  "- **Filters**: Inherits asymmetry from n_tanks_active and n_closures (see below).",
  "",
  "### n_closures",
  sprintf("- **Line(s)**: %s", paste(lines_nclosures, collapse=", ")),
  "- **Source**: `sum(closure_event[first_year_churn == 0L | is.na(first_year_churn)])` in `tank_year_panel`",
  sprintf("- **Churn exclusion line(s)**: %s", paste(churn_excl_lines, collapse=", ")),
  "- **CRITICAL ASYMMETRY**: `first_year_churn == 1` tanks are counted in `n_tanks_active`",
  "  (their row is present) but their `closure_event` is EXCLUDED from `n_closures`.",
  "  Result: `n_tanks_eoy = n_tanks_active - n_closures` overcounts by the number of",
  "  churn tanks at this facility-year (they are gone by EOY but not subtracted).",
  "",
  "### n_sw_closures",
  "- **Source**: `tank_year_panel`, filtered to `mm_wall == 'Single-Walled'` AND NOT first_year_churn.",
  "- **Filters**: Consistent with n_closures (same churn exclusion).",
  "",
  "### n_dw_closures",
  "- **Source**: Same as n_sw_closures but `mm_wall == 'Double-Walled'`.",
  "- **Filters**: Consistent.",
  "",
  "### n_installs",
  sprintf("- **Line(s)**: %s (S12.3)", paste(lines_ninstalls, collapse=", ")),
  "- **Source**: `study_tanks[!is.na(tank_installed_date)]`, grouped by `year(tank_installed_date)`.",
  "- **DIFFERENT SOURCE**: `study_tanks` is the master tank file, NOT `tank_year_panel`.",
  "  Requires non-NA install date. Includes Unknown-Wall tanks.",
  "- **FIRST_YEAR_CHURN COUNTS HERE**: churn tank installs ARE counted in n_installs.",
  "  Combined with the exclusion from n_closures, this is the direct driver of gap > 0:",
  "  gap = (n_tanks_active - n_closures + n_installs) - n_tanks_eoy = n_installs",
  "  (since n_tanks_eoy = n_tanks_active - n_closures by construction).",
  "  For partial-shrinkage events where n_installs > 0 (current-year installs with no",
  "  future installs), gap == n_installs > 0 by arithmetic identity.",
  "",
  "### n_sw_installs",
  sprintf("- **Line(s)**: %s", paste(grep_02b("n_sw_installs      = sum"), collapse=", ")),
  sprintf("- **Filter line**: `%s`", sw_line_actual),
  sprintf("- **%s**", swap_note),
  "",
  "### n_dw_installs",
  sprintf("- **Line(s)**: %s", paste(grep_02b("n_dw_installs      = sum"), collapse=", ")),
  sprintf("- **Filter line**: `%s`", dw_line_actual),
  "",
  "---",
  "",
  "## Findings",
  "",
  "### Finding 1 — first_year_churn asymmetry (HIGH SEVERITY — PANEL_BUG candidate)",
  "",
  sprintf("**Lines involved**: n_tanks_active=%s | n_closures=%s | n_tanks_eoy=%s | n_installs=%s",
    paste(lines_ntanks_active, collapse=","),
    paste(lines_nclosures, collapse=","),
    paste(lines_ntanks_eoy, collapse=","),
    paste(lines_ninstalls, collapse=",")),
  "",
  "The count identity gap = n_installs (algebraic consequence when n_tanks_eoy is defined",
  "as n_tanks_active - n_closures). For partial-shrinkage events, gap > 0 iff n_installs > 0",
  "in the current year. This is not a noise issue — it is a direct consequence of the",
  "first_year_churn exclusion being applied asymmetrically:",
  "  - Churn installs counted in n_installs (S12.3)",
  "  - Churn closures excluded from n_closures (S12.1)",
  "  - Churn tanks included in n_tanks_active (S12.1)",
  "",
  "**Fix (Option A)**: Exclude churn tanks from n_tanks_active.",
  "  Change line %s from `.N` to",
  "  `sum(first_year_churn == 0L | is.na(first_year_churn), na.rm = TRUE)`",
  "",
  "**Fix (Option B)**: Include churn closures in n_closures.",
  "  Remove the `first_year_churn == 0L | is.na(first_year_churn)` filter on line %s.",
  "",
  "**Fix (Option C)**: Exclude churn installs from n_installs.",
  "  In S12.3 (line %s), filter out tanks where `year(tank_installed_date) == year(tank_closed_date)`.",
  "",
  "All three fixes make the count identity self-consistent. Choose based on economic intent.",
  "",
  "### Finding 2 — n_installs from different source table (MEDIUM SEVERITY)",
  "",
  "`n_installs` aggregates from `study_tanks` (master file); `n_tanks_active` aggregates",
  "from `tank_year_panel`. `tank_year_panel` excludes tanks with `expand_start > expand_end`",
  "(impossible date ordering). `study_tanks` does not apply this filter. If any tanks have",
  "install dates after close dates (data entry errors), they appear in n_installs but not",
  "n_tanks_active — another source of count inconsistency.",
  "",
  "### Finding 3 — n_sw_installs/n_dw_installs label swap (LOW SEVERITY)",
  sprintf("Lines %s and %s — verify that n_sw_installs selects the intended wall type.",
    paste(grep_02b("n_sw_installs      = sum"), collapse=","),
    paste(grep_02b("n_dw_installs      = sum"), collapse=","))
)

# Fill in placeholder line numbers for fix instructions
audit_02b_lines[grepl("Change line %s from", audit_02b_lines)] <- sprintf(
  "  Change line %s from `.N` to", paste(lines_ntanks_active, collapse=","))
audit_02b_lines[grepl("Remove the.*filter on line %s", audit_02b_lines)] <- sprintf(
  "  Remove the `first_year_churn == 0L | is.na(first_year_churn)` filter on line %s.",
  paste(lines_nclosures, collapse=","))
audit_02b_lines[grepl("In S12.3 \\(line %s\\)", audit_02b_lines)] <- sprintf(
  "  In S12.3 (line %s), filter out tanks where `year(tank_installed_date) == year(tank_closed_date)`.",
  paste(lines_ninstalls, collapse=","))

writeLines(audit_02b_lines, file.path(AUDIT_DIR, "02b_field_audit.md"))
cat(sprintf("  Saved: %s\n", file.path(AUDIT_DIR, "02b_field_audit.md")))


# ---- Partial_Shrinkage_Panel_Audit.md ----
n_traceable <- nrow(trace_table[!is.na(missing_tank_id)])

build_cat_row <- function(cat_nm, label) {
  r <- task2_summary[disappearance_category == cat_nm]
  if (nrow(r) == 0L) sprintf("| %s | 0 | 0.0%% |", label)
  else sprintf("| %s | %d | %.1f%% |", label, r$N, r$pct)
}

mech_labels <- c(
  "1" = "1: panel_id reassignment (not detectable from panel_dt)",
  "2" = "2: wall_type field flip (proxy via n_unk_wall_active)",
  "3" = "3: fuel/capacity field flip",
  "4" = "4: known data-source transition (state-year spike)",
  "5" = "5: true silent (unexplained)"
)

verdict_evidence <- if (verdict == "PANEL_BUG") {
  c(
    sprintf("- Task 2 traceback: %.0f%% of missing tanks classified as `i_closure_recorded`.", pct_cat_i),
    "  These tanks have `tank_closed_date` set to the gap year but were not counted in",
    "  02b's `n_closures`. This is consistent with the first_year_churn asymmetry (Task 5).",
    "",
    "- Task 5 (02b audit): `n_closures` explicitly excludes `first_year_churn == 1` events",
    "  but `n_tanks_active` includes the same tanks. Since `n_tanks_eoy = n_tanks_active - n_closures`",
    "  by construction (line 947), and `n_installs` counts churn installs (line 984),",
    "  `gap = n_installs` by algebra. Every partial-shrinkage year with n_installs > 0",
    "  produces gap > 0 regardless of real data quality.",
    "",
    "- This is a systematic code-level bug, not random noise.",
    sprintf("- Task 1: %.1f%% silent-disappeared tanks (category b) — some residual noise exists.", pct_silent)
  )
} else if (verdict == "DATA_NOISE") {
  c(
    sprintf("- Task 2 traceback: %.0f%% reassignment + silent (non-closure reasons).", pct_cat_iv + pct_cat_vi),
    sprintf("- Task 1: %.1f%% silent-disappeared tanks, indicating broad panel churn.", pct_silent),
    "- Task 5: No critical bug found in 02b's count arithmetic for the common case."
  )
} else {
  c(
    sprintf("- Task 2 traceback: %.0f%% closure_recorded (PANEL_BUG signal)", pct_cat_i),
    sprintf("  + %.0f%% reassignment/silent (DATA_NOISE signal).", pct_cat_iv + pct_cat_vi),
    "- Task 5: first_year_churn asymmetry is a real bug and contributes to the gap.",
    sprintf("- Task 1: %.1f%% silent-disappeared tanks signals residual data noise.", pct_silent),
    "- Both mechanisms are present. Fix 1 reduces but likely does not eliminate the 47% rate."
  )
}

next_steps_bug <- if (verdict %in% c("PANEL_BUG", "MIX")) c(
  "### Fix 02b (PANEL_BUG component)",
  "",
  "**Fix 1 — Reconcile first_year_churn in count fields** (see `02b_field_audit.md` Finding 1).",
  "  Three options; recommended is Option A (exclude churn from n_tanks_active) because it",
  "  preserves the intent that first_year_churn events are economically meaningless:",
  "",
  sprintf("  Line %s: change `.N` to `sum(first_year_churn == 0L | is.na(first_year_churn))`.",
    paste(lines_ntanks_active, collapse=",")),
  "",
  "  After fixing: regenerate facility_panel.csv and re-run T005 estimation suite.",
  "",
  "**Fix 2 — Verify n_sw_installs/n_dw_installs wall filters** (see Finding 3 in `02b_field_audit.md`).",
  sprintf("  Lines %s and %s.",
    paste(grep_02b("n_sw_installs      = sum"), collapse=","),
    paste(grep_02b("n_dw_installs      = sum"), collapse=","))
) else character(0)

next_steps_noise <- if (verdict %in% c("DATA_NOISE", "MIX")) c(
  "### Document data noise (DATA_NOISE component)",
  "",
  sprintf("- Silent disappearance rate: %.1f%% of panel_dt tanks. Document in data appendix.", pct_silent),
  "- For robustness, restrict partial-shrinkage analysis to events where gap == 0.",
  "  Report both full-sample and identity-passing estimates.",
  "- Mechanism 4 state-year spikes may reflect administrative data transitions;",
  "  add year-of-record-system-change controls if identifiable."
) else character(0)

report_md <- c(
  "# Partial Shrinkage Panel Audit",
  sprintf("_Generated: %s | Script: T006_Partial_Shrinkage_Audit.R_", Sys.Date()),
  "",
  "---",
  "## §1 Setup and Definitions",
  "",
  "This audit investigates why 47% of partial-shrinkage facility-year events fail the",
  "count identity `n_tanks_eoy == n_tanks_active − n_closures + n_installs`. Partial",
  "shrinkage is defined as: `any_closure==1` AND `is.na(years_to_next_install)` AND",
  "`facility_complete_closure==0`. The gap variable `(n_tanks_active - n_closures +",
  "n_installs) - n_tanks_eoy` measures unexplained tank disappearances.",
  "",
  "Two hypotheses: **(A) PANEL_BUG** — 02b under-counts closures, inflating n_tanks_eoy;",
  "**(B) DATA_NOISE** — tanks disappear for non-closure reasons (panel_id reassignment,",
  "filter dropouts, record revisions).",
  "",
  "**Data (read-only):**",
  sprintf("- `panel_dt.csv`: %s tank-year rows", fmt_n(nrow(panel_dt))),
  sprintf("- `facility_panel.csv`: %s facility-year rows", fmt_n(nrow(fp))),
  sprintf("- `dcm_obs_panel_observed.csv`: %s rows", fmt_n(nrow(dcm_obs))),
  "",
  "---",
  "## §2 Task 1 — Tank Lifecycle Integrity",
  "",
  sprintf("Unique tanks in `panel_dt`: **%s**", fmt_n(total_tanks)),
  "",
  "| Category | N | % |",
  "|---|---|---|",
  sprintf("| (a) Has closure record | %s | %.1f%% |",
    fmt_n(n_closure_v), task1_summary[category=="a_has_closure", pct]),
  sprintf("| (b) Silent disappearance | %s | **%.1f%%** |", fmt_n(n_silent_val), pct_silent),
  sprintf("| (c) Persistent open (2020) | %s | %.1f%% |",
    fmt_n(n_persist_v), task1_summary[category=="c_persistent_open", pct]),
  "",
  sprintf("**%.1f%% of panel_dt tanks disappeared before 2020 without a closure date.**", pct_silent),
  "",
  "For category (a) tanks, histogram of `last_active_year - tank_closed_year`:",
  "",
  "![Task 1 lifecycle histogram](task1_lifecycle_histogram.png)",
  "",
  sprintf("- Mean gap: %.2f yrs | Median: %d | Exact 0: %s (%.1f%%) | Within ±1: %s (%.1f%%)",
    diff_summary$mean_gap, diff_summary$median,
    fmt_n(diff_summary$exact_0), 100*diff_summary$exact_0/diff_summary$N,
    fmt_n(diff_summary$within1), 100*diff_summary$within1/diff_summary$N),
  "",
  "---",
  "## §3 Task 2 — Sample-Event Traceback",
  "",
  sprintf("Partial-shrinkage events with gap > 0: **%s**", fmt_n(nrow(ps_bad))),
  sprintf("Sample: %d events | Missing-tank observations: %s", nrow(sample_done), fmt_n(n_traceable)),
  "",
  "| Category | N | % |",
  "|---|---|---|",
  build_cat_row("i_closure_recorded",       "i — closure recorded (02b should count)"),
  build_cat_row("ii_closure_year_off",      "ii — closure year off by ±1"),
  build_cat_row("iii_closure_way_off",      "iii — closure year far off"),
  build_cat_row("iv_panel_id_reassignment", "iv — panel_id reassignment"),
  build_cat_row("v_unknown_field_flip",     "v — unknown-wall field flip"),
  build_cat_row("vi_silent",               "vi — silent (unexplained)"),
  build_cat_row("no_missing_in_t_plus_1",  "no missing tanks in t+1"),
  "",
  "_Full trace: `Reports/Audits/trace_table.csv`_",
  "",
  "**Interpretation**: Category (i) dominates → PANEL_BUG. Categories (iv)+(v)+(vi)",
  "dominate → DATA_NOISE.",
  "",
  "---",
  "## §4 Task 3 — Action-Flag Reconciliation",
  "",
  "| Flag | N_sampled | N_ok | pct_ok | Notes |",
  "|---|---|---|---|---|",
  paste0(apply(reconciliation_rates, 1, function(r)
    sprintf("| %s | %s | %s | %.1f%% | %s |",
      r["flag_name"], r["N_sampled"], r["N_flag_ok"],
      as.numeric(r["pct_ok"]), r["notes_on_failures"])), collapse = "\n"),
  "",
  if (length(t3_unreliable) > 0L)
    sprintf("**FLAG(S) UNRELIABLE (<95%%): %s** — implications in §7.",
      paste(t3_unreliable, collapse=", "))
  else
    "All three flags reconcile at ≥ 95%. Flag data is reliable.",
  "_Note: Verification is within panel\\_dt's filtered view (known-wall tanks only).",
  "Facilities with Unknown-Wall tanks may show lower reconciliation._",
  "",
  "---",
  "## §5 Task 4 — Disappearance Mechanisms",
  "",
  sprintf("Silent-disappeared tanks: %s (capped at 10,000)", fmt_n(nrow(disappearance_mechanisms))),
  "",
  "| Mechanism | N | % |",
  "|---|---|---|",
  paste0(apply(mech_summary, 1, function(r) {
    lbl <- mech_labels[as.character(r["mechanism"])]
    if (is.na(lbl)) lbl <- as.character(r["mechanism"])
    sprintf("| %s | %s | %.1f%% |", lbl, fmt_n(as.integer(r["N"])), as.numeric(r["pct"]))
  }), collapse = "\n"),
  "",
  "**Mechanism 1 (panel_id reassignment) not detectable from `panel_dt` alone**: since",
  "`tank_panel_id` is constructed as `paste(facility_id, state, tank_id)`, a reassigned",
  "tank would get a new `tank_panel_id`. Confirmation requires unfiltered master_tanks.",
  "",
  if (nrow(mech4_agg) > 0L) c(
    "**Mechanism 4 state-year spikes (≥50 tanks):**",
    "",
    "| State | Year | N |",
    "|---|---|---|",
    paste0(apply(head(mech4_agg, 10L), 1, function(r)
      sprintf("| %s | %d | %s |", r["state"], as.integer(r["last_yr2"]), fmt_n(as.integer(r["N"])))),
      collapse = "\n")
  ) else "_No state-year spikes ≥ 50 tanks detected._",
  "",
  "_Mechanisms 2 and 3 use proxies (facility n_unk_wall_active and within-panel_dt field_",
  "_comparison). Definitive confirmation requires the unfiltered tank_year_panel._",
  "",
  "---",
  "## §6 Task 5 — 02b Code Audit",
  "",
  "_Full field documentation: `Reports/Audits/02b_field_audit.md`_",
  "",
  "**Key findings:**",
  "",
  "1. **first_year_churn asymmetry (HIGH SEVERITY)**:",
  sprintf("   - `n_tanks_active` (line %s): counts ALL tanks including `first_year_churn == 1`",
    paste(lines_ntanks_active, collapse=",")),
  sprintf("   - `n_closures` (line %s): EXCLUDES `first_year_churn == 1` closure events",
    paste(lines_nclosures, collapse=",")),
  sprintf("   - `n_tanks_eoy` (line %s): derived as `n_tanks_active - n_closures`",
    paste(lines_ntanks_eoy, collapse=",")),
  sprintf("   - `n_installs` (line %s, S12.3): from `study_tanks`; INCLUDES churn installs",
    paste(lines_ninstalls, collapse=",")),
  "   - **Consequence**: `gap = n_installs` algebraically. Any partial-shrinkage year",
  "     with `n_installs > 0` has `gap > 0` regardless of data quality.",
  "",
  "2. **n_installs from different source table (MEDIUM)**: S12.3 uses `study_tanks`;",
  "   `n_tanks_active` uses `tank_year_panel`. Tanks with impossible dates may appear",
  "   in one but not the other.",
  "",
  sprintf("3. **n_sw/dw_installs label check**: see `02b_field_audit.md` Finding 3 (%s).", swap_note),
  "",
  "---",
  "## §7 Verdict",
  "",
  sprintf("## **VERDICT: %s**", verdict),
  "",
  "**Evidence:**",
  "",
  verdict_evidence,
  "",
  "---",
  "## §8 Recommended Next Steps",
  "",
  next_steps_bug,
  next_steps_noise,
  ""
)

writeLines(report_md, file.path(AUDIT_DIR, "Partial_Shrinkage_Panel_Audit.md"))
cat(sprintf("  Saved: %s\n", file.path(AUDIT_DIR, "Partial_Shrinkage_Panel_Audit.md")))

cat("\n=== ALL DELIVERABLES WRITTEN ===\n")
cat(sprintf("  %s\n", file.path(AUDIT_DIR, "Partial_Shrinkage_Panel_Audit.md")))
cat(sprintf("  %s\n", file.path(AUDIT_DIR, "trace_table.csv")))
cat(sprintf("  %s\n", file.path(AUDIT_DIR, "reconciliation_rates.csv")))
cat(sprintf("  %s\n", file.path(AUDIT_DIR, "disappearance_mechanisms.csv")))
cat(sprintf("  %s\n", file.path(AUDIT_DIR, "02b_field_audit.md")))
cat(sprintf("  %s\n", hist_path))
cat(sprintf("\nVerdict: %s\n", verdict))
cat(sprintf("Silent disappeared: %.1f%%\n", pct_silent))
cat(sprintf("Category (i) pct: %.1f%%\n",  pct_cat_i))
