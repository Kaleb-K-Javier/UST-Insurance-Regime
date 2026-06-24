# ==============================================================================
# 04am_Identification_Budget.R  -- TICKET 020, script 2 of 4  (E1 + E5)
# ==============================================================================
# E1 = identification budget: action frequencies, shed/install simultaneity,
#      events by identifying contrast (size x has_SW).
# E5 = occupancy: how concentrated is the composition state space.
#
# Frame: dcm_obs_panel_observed (the >=1999 estimation population, all states)
#   spined with facility_panel flags (action coding) and boy_composition_* (cells).
# READ-ONLY DESCRIPTIVE. No Output/Estimation_Results writes.
# ==============================================================================

suppressMessages({
  library(data.table)
  library(ggplot2)
  library(here)
  library(scales)
})

cat("=== 04am IDENTIFICATION BUDGET (E1 + E5) ===\n")
cat(sprintf("Start: %s\n\n", format(Sys.time())))

FF_COL <- "#E76F51"; RB_COL <- "#2A9D8F"
fig_dir <- here("Output", "Figures"); tab_dir <- here("Output", "Tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

# 019 .tex helper: centered, \small, arraystretch, escaped %/_, <=8 rows.
esc_tex <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("_", "\\\\_", x)
  x
}
write_tex_table <- function(dt, path, align, header, caption_note = NULL) {
  con <- file(path, open = "wt")
  on.exit(close(con))
  writeLines("\\begin{table}[ht]", con)
  writeLines("\\centering", con)
  writeLines("\\small", con)
  writeLines("\\renewcommand{\\arraystretch}{1.15}", con)
  writeLines(sprintf("\\begin{tabular}{%s}", align), con)
  writeLines("\\hline", con)
  writeLines(paste0(paste(header, collapse = " & "), " \\\\"), con)
  writeLines("\\hline", con)
  for (i in seq_len(nrow(dt))) {
    writeLines(paste0(paste(esc_tex(unlist(dt[i])), collapse = " & "), " \\\\"), con)
  }
  writeLines("\\hline", con)
  writeLines("\\end{tabular}", con)
  if (!is.null(caption_note)) writeLines(sprintf("\\\\[2pt]{\\footnotesize %s}", caption_note), con)
  writeLines("\\end{table}", con)
}

# ==============================================================================
# 1. Build the analysis frame (>=1999 estimation population)
# ==============================================================================
cat("=== SECTION 1: build frame ===\n")
dcm <- fread(here("Data", "Analysis", "dcm_obs_panel_observed.csv"),
             select = c("panel_id", "panel_year", "state", "size_bin",
                        "rho_state", "boy_stock"))
dcm[, panel_id := as.character(panel_id)]
stopifnot(dcm[, all(panel_year >= 1999)])

fac <- fread(here("Data", "Analysis", "facility_panel.csv"),
             select = c("panel_id", "panel_year", "any_closure",
                        "facility_complete_closure", "n_installs"))
fac[, panel_id := as.character(panel_id)]
for (cc in c("any_closure", "facility_complete_closure", "n_installs"))
  fac[is.na(get(cc)), (cc) := 0L]

boyfy <- fread(here("Data", "Analysis", "boy_composition_fy.csv"))
boyfy[, panel_id := as.character(panel_id)]

frame <- merge(dcm, fac, by = c("panel_id", "panel_year"), all.x = TRUE)
for (cc in c("any_closure", "facility_complete_closure", "n_installs"))
  frame[is.na(get(cc)), (cc) := 0L]
frame <- merge(frame,
               boyfy[, .(panel_id, panel_year, N, has_SW, n_shed_total, n_inst, n_inst_dw)],
               by = c("panel_id", "panel_year"), all.x = TRUE)
for (cc in c("N", "has_SW", "n_shed_total", "n_inst", "n_inst_dw"))
  frame[is.na(get(cc)), (cc) := 0L]

# size_bin: take dcm_obs value; fill blanks from boy_stock
frame[size_bin == "" | is.na(size_bin),
      size_bin := fcase(boy_stock >= 4L, "4+",
                        boy_stock >= 1L, as.character(boy_stock),
                        default = NA_character_)]

# regime cross-check (#6): derive then assert agreement with dcm_obs rho_state
frame[, regime_rb := as.integer(state == "TX" & panel_year >= 1999)]
agree <- frame[!is.na(rho_state), mean(regime_rb == (rho_state - 1L))]
cat(sprintf("  regime agreement (derived TX&>=1999 vs dcm rho_state): %.5f\n", agree))
stopifnot(agree > 0.999)

# 6-category action coding (Exit takes precedence)
frame[, action := fcase(
  facility_complete_closure == 1L,                               "Exit",
  any_closure == 1L & n_installs > 0L,                           "Replace",
  any_closure == 1L & n_installs == 0L,                          "Downsize",
  any_closure == 0L & n_installs > 0L,                           "Expansion",
  default                                                         = "Maintain")]
cat(sprintf("  frame facility-years: %s\n", format(nrow(frame), big.mark = ",")))
print(frame[, .N, by = action][order(-N)])

ACTION_LEV <- c("Maintain", "Exit", "Downsize", "Replace", "Expansion")
SIZE_LEV   <- c("1", "2", "3", "4+")

# ==============================================================================
# 2. E1a -- action frequencies: action x size{1,2,3,4+,All}
# ==============================================================================
cat("=== SECTION 2: E1a action frequencies ===\n")
e1a_by <- frame[!is.na(size_bin), .(n = .N), by = .(action, size_bin)]
e1a_all <- frame[, .(size_bin = "All", n = .N), by = .(action)]
e1a <- rbind(e1a_by, e1a_all)
# rate_pct = within-size-column share (each size column sums to 100)
e1a[, rate_pct := round(100 * n / sum(n), 4), by = size_bin]
e1a[, action   := factor(action, levels = ACTION_LEV)]
e1a[, size_bin := factor(size_bin, levels = c(SIZE_LEV, "All"))]
setorder(e1a, action, size_bin)
e1a[, action := as.character(action)]; e1a[, size_bin := as.character(size_bin)]
fwrite(e1a, file.path(tab_dir, "T020_E1_Action_Frequencies.csv"))
cat("  saved T020_E1_Action_Frequencies.csv\n"); print(e1a)

# starred .tex: the "All" column block (5 rows)
e1a_all_tex <- e1a[size_bin == "All", .(Action = action, N = format(n, big.mark = ","),
                                        `Rate (\\%)` = sprintf("%.2f", rate_pct))]
e1a_all_tex <- e1a_all_tex[match(ACTION_LEV, e1a_all_tex$Action)]
write_tex_table(e1a_all_tex, file.path(tab_dir, "T020_E1_Action_Frequencies.tex"),
                align = "lrr", header = c("Action", "N", "Rate (\\%)"),
                caption_note = "Facility-year action frequencies, all sizes pooled, 1999--2020.")

# ==============================================================================
# 3. E1b -- shed/install simultaneity grid (operating facility-years)
# ==============================================================================
cat("=== SECTION 3: E1b simultaneity grid ===\n")
op <- frame[facility_complete_closure == 0L]   # operating = not Exit
bin3 <- function(x) fcase(x >= 3L, "3+", default = as.character(x))
op[, sheds_bin    := bin3(pmin(n_shed_total, 3L))]
op[, installs_bin := bin3(pmin(n_inst, 3L))]
BIN_LEV <- c("0", "1", "2", "3+")
grid <- CJ(sheds_bin = BIN_LEV, installs_bin = BIN_LEV)
cnt  <- op[, .(n = .N), by = .(sheds_bin, installs_bin)]
e1b  <- merge(grid, cnt, by = c("sheds_bin", "installs_bin"), all.x = TRUE)
e1b[is.na(n), n := 0L]
e1b[, share := round(n / sum(n), 6)]
setorder(e1b, sheds_bin, installs_bin)
fwrite(e1b, file.path(tab_dir, "T020_E1_Simultaneity.csv"))
headline <- op[n_shed_total >= 2L & n_inst >= 2L, .N]
cat(sprintf("  HEADLINE: facility-years with >=2 sheds AND >=2 installs = %s (E1 rule <~200 -> drop F^R)\n",
            format(headline, big.mark = ",")))
cat("  saved T020_E1_Simultaneity.csv\n")

# figure E1: heat-tile
e1b[, sheds_bin := factor(sheds_bin, levels = BIN_LEV)]
e1b[, installs_bin := factor(installs_bin, levels = BIN_LEV)]
p_e1 <- ggplot(e1b, aes(installs_bin, sheds_bin, fill = share)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(n > 0, format(n, big.mark = ","), "0")), size = 3.3) +
  scale_fill_gradient(low = "#F1FAEE", high = RB_COL, labels = percent) +
  labs(x = "Installs in year", y = "Sheds in year", fill = "Share",
       title = "Within-year shed / install simultaneity (operating facility-years)") +
  theme_minimal(base_size = 13)
ggsave(file.path(fig_dir, "T020_E1_Simultaneity_Grid.png"), p_e1,
       width = 7, height = 5.5, dpi = 150)
cat("  saved T020_E1_Simultaneity_Grid.png\n")

# ==============================================================================
# 4. E1c -- events by identifying contrast (action x size x has_SW)
# ==============================================================================
cat("=== SECTION 4: E1c events by size x has_SW ===\n")
denom <- frame[!is.na(size_bin), .(n_fy = .N), by = .(size_bin, has_SW)]
ev_list <- list()
for (act in c("Exit", "Downsize", "Replace")) {
  ev <- frame[!is.na(size_bin), .(n_events = sum(action == act)), by = .(size_bin, has_SW)]
  ev[, action := act]
  ev_list[[act]] <- ev
}
e1c <- rbindlist(ev_list)
e1c <- merge(e1c, denom, by = c("size_bin", "has_SW"))
e1c[, event_rate := round(n_events / n_fy, 6)]
e1c[, action   := factor(action, levels = c("Exit", "Downsize", "Replace"))]
e1c[, size_bin := factor(size_bin, levels = SIZE_LEV)]
setorder(e1c, action, size_bin, has_SW)
e1c <- e1c[, .(action = as.character(action), size_bin = as.character(size_bin),
               has_SW, n_events, event_rate)]
fwrite(e1c, file.path(tab_dir, "T020_E1_Events_by_Index.csv"))
cat("  saved T020_E1_Events_by_Index.csv\n"); print(e1c)

# ==============================================================================
# 5. E5 -- occupancy of the composition state space
# ==============================================================================
cat("=== SECTION 5: E5 occupancy ===\n")
boylong <- fread(here("Data", "Analysis", "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
boylong[, cellname := sprintf("%s_%d", wall, age_bin)]

# 16-cell wide count matrix per facility-year
ALL_CELLS <- as.vector(t(outer(c("SW", "DW"), 1:8, function(w, b) sprintf("%s_%d", w, b))))
w <- dcast(boylong, panel_id + panel_year ~ cellname, value.var = "n_boy", fill = 0)
miss <- setdiff(ALL_CELLS, names(w))
if (length(miss)) for (mc in miss) w[, (mc) := 0L]
setcolorder(w, c("panel_id", "panel_year", ALL_CELLS))
cm <- as.matrix(w[, ..ALL_CELLS])
w[, comp_key := apply(cm, 1L, paste, collapse = "-")]
w[, n_occ    := rowSums(cm > 0)]
w[, N        := rowSums(cm)]

keytab <- w[, .(nfy = .N), by = comp_key][order(-nfy)]
keytab[, cumshare := cumsum(nfy) / sum(nfy)]
tot_fy <- nrow(w)
cov_at <- function(k) round(sum(keytab$nfy[seq_len(min(k, nrow(keytab)))]) / tot_fy, 6)
n_for  <- function(p) which(keytab$cumshare >= p)[1]

e5 <- data.table(
  metric = c("n_compositions", "top10_cov", "top100_cov", "top500_cov",
             "n_for_95pct", "n_for_99pct", "share_single_cell",
             "share_N_gt4", "share_N_gt6", "p50_N", "p90_N", "p99_N"),
  value = c(nrow(keytab), cov_at(10), cov_at(100), cov_at(500),
            n_for(0.95), n_for(0.99), round(mean(w$n_occ == 1L), 6),
            round(mean(w$N > 4L), 6), round(mean(w$N > 6L), 6),
            as.numeric(quantile(w$N, 0.50)), as.numeric(quantile(w$N, 0.90)),
            as.numeric(quantile(w$N, 0.99))))
fwrite(e5, file.path(tab_dir, "T020_E5_Occupancy.csv"))
cat("  saved T020_E5_Occupancy.csv\n"); print(e5)

# figure E5: coverage curve (cum share vs rank, log x)
keytab[, rank := .I]
p_e5 <- ggplot(keytab, aes(rank, cumshare)) +
  geom_line(color = RB_COL, linewidth = 0.9) +
  geom_hline(yintercept = c(0.95, 0.99), linetype = "dashed", color = "grey50") +
  scale_x_log10(labels = comma) + scale_y_continuous(labels = percent) +
  labs(x = "Composition rank (log scale)", y = "Cumulative share of facility-years",
       title = "Composition occupancy coverage") +
  theme_minimal(base_size = 13)
ggsave(file.path(fig_dir, "T020_E5_Coverage_Curve.png"), p_e5,
       width = 7, height = 5, dpi = 150)
cat("  saved T020_E5_Coverage_Curve.png\n")

cat("=== 04am DONE ===\n")
