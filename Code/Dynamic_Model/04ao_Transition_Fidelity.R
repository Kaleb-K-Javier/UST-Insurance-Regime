# ==============================================================================
# 04ao_Transition_Fidelity.R  -- TICKET 020, script 4 of 4  (E3 + E4)
# ==============================================================================
# E3 = is the marginal-rule shed transition true? Compare the multiset of cells
#   the deterministic rule WOULD shed (SW oldest->youngest, then DW oldest->
#   youngest; k = n_shed_total) against the realized shed cells, at the CELL
#   level (no tank-level identity match).
# E4 = replace wall outcome (upgrade vs same-wall) is an accounting consequence
#   of composition, not a choice; and installs are overwhelmingly DW.
#
# Sample frame: panel_year >= 1999. READ-ONLY DESCRIPTIVE.
# ==============================================================================

suppressMessages({
  library(data.table)
  library(ggplot2)
  library(here)
  library(scales)
})

cat("=== 04ao TRANSITION FIDELITY (E3 + E4) ===\n")
cat(sprintf("Start: %s\n\n", format(Sys.time())))

FF_COL <- "#E76F51"; RB_COL <- "#2A9D8F"
fig_dir <- here("Output", "Figures"); tab_dir <- here("Output", "Tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

esc_tex <- function(x) { x <- as.character(x); x <- gsub("\\\\", "", x)
  x <- gsub("%", "\\\\%", x); gsub("_", "\\\\_", x) }
write_tex_table <- function(dt, path, align, header, caption_note = NULL) {
  con <- file(path, open = "wt"); on.exit(close(con))
  writeLines(c("\\begin{table}[ht]", "\\centering", "\\small",
               "\\renewcommand{\\arraystretch}{1.15}",
               sprintf("\\begin{tabular}{%s}", align), "\\hline",
               paste0(paste(header, collapse = " & "), " \\\\"), "\\hline"), con)
  for (i in seq_len(nrow(dt)))
    writeLines(paste0(paste(esc_tex(unlist(dt[i])), collapse = " & "), " \\\\"), con)
  writeLines(c("\\hline", "\\end{tabular}"), con)
  if (!is.null(caption_note)) writeLines(sprintf("\\\\[2pt]{\\footnotesize %s}", caption_note), con)
  writeLines("\\end{table}", con)
}

# ==============================================================================
# 1. Build facility-year event table (action coding + size_bin + has_SW)
# ==============================================================================
cat("=== SECTION 1: build event table ===\n")
boyfy <- fread(here("Data", "Analysis", "boy_composition_fy.csv"))
boyfy[, panel_id := as.character(panel_id)]
boyfy <- boyfy[panel_year >= 1999L]

dcm <- fread(here("Data", "Analysis", "dcm_obs_panel_observed.csv"),
             select = c("panel_id", "panel_year", "size_bin", "rho_state", "boy_stock"))
dcm[, panel_id := as.character(panel_id)]

fac <- fread(here("Data", "Analysis", "facility_panel.csv"),
             select = c("panel_id", "panel_year", "any_closure",
                        "facility_complete_closure", "n_installs", "single_to_double_year"))
fac[, panel_id := as.character(panel_id)]
for (cc in c("any_closure", "facility_complete_closure", "n_installs", "single_to_double_year"))
  fac[is.na(get(cc)), (cc) := 0L]

ev <- merge(boyfy, fac, by = c("panel_id", "panel_year"), all.x = TRUE)
for (cc in c("any_closure", "facility_complete_closure", "n_installs", "single_to_double_year"))
  ev[is.na(get(cc)), (cc) := 0L]
ev <- merge(ev, dcm[, .(panel_id, panel_year, size_bin)],
            by = c("panel_id", "panel_year"), all.x = TRUE)
ev[size_bin == "" | is.na(size_bin),
   size_bin := fcase(N >= 4L, "4+", N >= 1L, as.character(N), default = NA_character_)]
ev[, action := fcase(
  facility_complete_closure == 1L,                "Exit",
  any_closure == 1L & n_installs > 0L,            "Replace",
  any_closure == 1L & n_installs == 0L,           "Downsize",
  any_closure == 0L & n_installs > 0L,            "Expansion",
  default                                          = "Maintain")]
ev[, group := fifelse(state == "TX", "TX", "Control")]
SIZE_LEV <- c("1", "2", "3", "4+")

# ==============================================================================
# 2. E3 -- shed fidelity (Downsize & Replace with >=1 shed)
# ==============================================================================
cat("=== SECTION 2: E3 shed fidelity ===\n")
e3pop <- ev[action %chin% c("Downsize", "Replace") & n_shed_total >= 1L]
cat(sprintf("  E3 events: %s (Downsize=%s, Replace=%s)\n",
            format(nrow(e3pop), big.mark = ","),
            format(e3pop[action == "Downsize", .N], big.mark = ","),
            format(e3pop[action == "Replace",  .N], big.mark = ",")))

# wide 16-cell BOY and shed counts for the event facility-years
boylong <- fread(here("Data", "Analysis", "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
bl_e <- boylong[e3pop[, .(panel_id, panel_year)], on = c("panel_id", "panel_year"), nomatch = NULL]
bl_e[, cell := sprintf("%s_%d", wall, age_bin)]

# marginal order: SW oldest(8)->youngest(1), then DW oldest->youngest
MARG <- c(sprintf("SW_%d", 8:1), sprintf("DW_%d", 8:1))
marg_wall <- c(rep("SW", 8), rep("DW", 8))
marg_bin  <- c(8:1, 8:1)

wB <- dcast(bl_e, panel_id + panel_year ~ cell, value.var = "n_boy",  fill = 0)
wR <- dcast(bl_e, panel_id + panel_year ~ cell, value.var = "n_shed", fill = 0)
for (mc in setdiff(MARG, names(wB))) wB[, (mc) := 0L]
for (mc in setdiff(MARG, names(wR))) wR[, (mc) := 0L]
setkey(wB, panel_id, panel_year); setkey(wR, panel_id, panel_year)

# align event order to wB
e3 <- merge(e3pop[, .(panel_id, panel_year, action, size_bin, n_shed_total)],
            wB[, c("panel_id", "panel_year", MARG), with = FALSE],
            by = c("panel_id", "panel_year"))
Rmat <- as.matrix(merge(e3[, .(panel_id, panel_year)],
                        wR[, c("panel_id", "panel_year", MARG), with = FALSE],
                        by = c("panel_id", "panel_year"))[, ..MARG])
B <- as.matrix(e3[, ..MARG])
R <- Rmat
storage.mode(B) <- "double"; storage.mode(R) <- "double"
k <- rowSums(R)
stopifnot(all(k == e3$n_shed_total))           # sheds subset of BOY by construction

# predicted shed multiset under marginal order (greedy fill of first k)
CB     <- t(apply(B, 1L, cumsum))
before <- CB - B
P      <- pmin(pmax(k - before, 0), B)
stopifnot(all(rowSums(P) == k))                 # rule can always shed k (k <= N)

# per-event metrics
e3[, exact_match := as.integer(rowSums(abs(P - R)) == 0)]
e3[, L1         := rowSums(abs(P - R)) / 2]
is_sw <- marg_wall == "SW"; is_dw <- marg_wall == "DW"
KEPT <- B - R
e3[, wall_violation := as.integer((rowSums(R[, is_dw, drop = FALSE]) > 0) &
                                  (rowSums(KEPT[, is_sw, drop = FALSE]) > 0))]
# ageband violation: within a wall, shed a younger bin while an older same-wall
# bin still had a kept tank. Columns within each wall run oldest(8)->youngest(1).
ageband_viol <- function(Rw, Kw) {
  # Rw, Kw: events x 8, columns ordered bin 8..1 (oldest first). A violation is
  # shedding a younger bin (column j) while an older same-wall bin (column i<j)
  # still has a kept tank. cs[, j] = kept summed over columns strictly older than j.
  cs <- t(apply(Kw, 1L, function(z) cumsum(c(0, z[-8]))))
  rowSums((Rw > 0) & (cs > 0)) > 0
}
av_sw <- ageband_viol(R[, is_sw, drop = FALSE], KEPT[, is_sw, drop = FALSE])
av_dw <- ageband_viol(R[, is_dw, drop = FALSE], KEPT[, is_dw, drop = FALSE])
e3[, ageband_violation := as.integer(av_sw | av_dw)]
e3[, k_shed := fifelse(n_shed_total == 1L, "1", "2+")]

# aggregate by action x size x k_shed
e3agg <- e3[!is.na(size_bin), .(
  n_events = .N,
  exact_match_share    = round(mean(exact_match), 6),
  mean_L1              = round(mean(L1), 6),
  wall_violation_share = round(mean(wall_violation), 6),
  ageband_violation_share = round(mean(ageband_violation), 6)),
  by = .(action, size_bin, k_shed)]
e3agg[, action   := factor(action, levels = c("Downsize", "Replace"))]
e3agg[, size_bin := factor(size_bin, levels = SIZE_LEV)]
e3agg[, k_shed   := factor(k_shed, levels = c("1", "2+"))]
setorder(e3agg, action, size_bin, k_shed)
e3agg <- e3agg[, .(action = as.character(action), size_bin = as.character(size_bin),
                   k_shed = as.character(k_shed), n_events, exact_match_share,
                   mean_L1, wall_violation_share, ageband_violation_share)]
fwrite(e3agg, file.path(tab_dir, "T020_E3_Shed_Fidelity.csv"))
cat("  saved T020_E3_Shed_Fidelity.csv\n"); print(e3agg)
cat(sprintf("  POOLED exact-match share: %.4f | mean L1: %.4f | wall-viol: %.4f | ageband-viol: %.4f\n",
            e3[, mean(exact_match)], e3[, mean(L1)],
            e3[, mean(wall_violation)], e3[, mean(ageband_violation)]))

# starred .tex: collapsed action x size (<=8 rows)
e3tex <- e3[!is.na(size_bin), .(n = .N, match = round(mean(exact_match), 3),
                                wallv = round(mean(wall_violation), 3)),
            by = .(action, size_bin)][order(action, size_bin)]
e3tex_out <- e3tex[, .(Action = action, Size = size_bin, N = format(n, big.mark = ","),
                       `Match` = sprintf("%.3f", match), `Wall viol.` = sprintf("%.3f", wallv))]
write_tex_table(e3tex_out, file.path(tab_dir, "T020_E3_Shed_Fidelity.tex"),
                align = "llrrr", header = c("Action", "Size", "N", "Match", "Wall viol."),
                caption_note = "Cell-level shed fidelity vs marginal rule (SW oldest first, then DW).")

# kernel: predicted-cell x realized-cell shed counts (marginal-order pairing)
cat("  building 16x16 shedding kernel...\n")
Kpool <- matrix(0L, 16, 16)
for (i in seq_len(nrow(e3))) {
  pl <- rep.int(1:16, P[i, ]); rl <- rep.int(1:16, R[i, ])
  for (m in seq_along(pl)) Kpool[pl[m], rl[m]] <- Kpool[pl[m], rl[m]] + 1L
}
kdt <- as.data.table(which(Kpool > 0, arr.ind = TRUE))
setnames(kdt, c("pidx", "ridx"))
kdt[, n := Kpool[cbind(pidx, ridx)]]
kdt[, `:=`(pred_wall = marg_wall[pidx], pred_bin = marg_bin[pidx],
           real_wall = marg_wall[ridx], real_bin = marg_bin[ridx])]
kdt[, share_within_pred := round(n / sum(n), 6), by = pidx]
kdt <- kdt[, .(pred_wall, pred_bin, real_wall, real_bin, n, share_within_pred)]
setorder(kdt, pred_wall, pred_bin, real_wall, real_bin)
fwrite(kdt, file.path(tab_dir, "T020_E3_Shedding_Kernel.csv"))
cat(sprintf("  saved T020_E3_Shedding_Kernel.csv (%d nonzero cells; diagonal share %.4f)\n",
            nrow(kdt), sum(diag(Kpool)) / sum(Kpool)))

# figure E3: fidelity bars by size, facet action
e3bar <- e3[!is.na(size_bin), .(Match = mean(exact_match),
                                `Wall viol.` = mean(wall_violation),
                                `Ageband viol.` = mean(ageband_violation)),
            by = .(action, size_bin)]
e3barm <- melt(e3bar, id.vars = c("action", "size_bin"), variable.name = "metric",
               value.name = "share")
e3barm[, size_bin := factor(size_bin, levels = SIZE_LEV)]
p_e3 <- ggplot(e3barm, aes(size_bin, share, fill = metric)) +
  geom_col(position = "dodge") +
  facet_wrap(~action) +
  scale_fill_manual(values = c(Match = RB_COL, `Wall viol.` = FF_COL, `Ageband viol.` = "#E9C46A")) +
  scale_y_continuous(labels = percent) +
  labs(x = "Portfolio size", y = "Share of events", fill = NULL,
       title = "Shed-rule fidelity by size") +
  theme_minimal(base_size = 13)
ggsave(file.path(fig_dir, "T020_E3_Fidelity_bySize.png"), p_e3,
       width = 8, height = 5, dpi = 150)
cat("  saved T020_E3_Fidelity_bySize.png\n")

# ==============================================================================
# 3. E4a -- replace wall outcome 2x2
# ==============================================================================
cat("=== SECTION 3: E4a replace wall 2x2 ===\n")
rep_ev <- ev[action == "Replace"]
rep_ev[, outcome := fifelse(single_to_double_year == 1L, "upgrade", "samewall")]
e4a <- rep_ev[, .(n = .N), by = .(has_SW, outcome)]
e4a <- merge(CJ(has_SW = c(0L, 1L), outcome = c("upgrade", "samewall")),
             e4a, by = c("has_SW", "outcome"), all.x = TRUE)
e4a[is.na(n), n := 0L]
e4a[, row_share := round(n / sum(n), 6), by = has_SW]
setorder(e4a, has_SW, outcome)
fwrite(e4a, file.path(tab_dir, "T020_E4_Replace_Wall.csv"))
sw_skip <- e4a[has_SW == 1L & outcome == "samewall", row_share]
cat(sprintf("  saved T020_E4_Replace_Wall.csv\n  SW-SKIP RATE  P(samewall | has_SW) = %.4f (E4 rule >=~0.10 -> wall sub-choice needed)\n",
            sw_skip))
print(e4a)

# starred .tex
e4a_tex <- e4a[, .(`has SW` = has_SW, Outcome = outcome, N = format(n, big.mark = ","),
                   `Row share` = sprintf("%.3f", row_share))]
write_tex_table(e4a_tex, file.path(tab_dir, "T020_E4_Replace_Wall.tex"),
                align = "llrr", header = c("has SW", "Outcome", "N", "Row share"),
                caption_note = sprintf("Replace wall outcome by BOY composition. SW-skip rate = %.3f.", sw_skip))

# ==============================================================================
# 4. E4b -- install DW share by year x group
# ==============================================================================
cat("=== SECTION 4: E4b install DW share by year x group ===\n")
e4b <- ev[n_inst > 0L, .(n_installs = sum(n_inst), n_dw = sum(n_inst_dw)),
          by = .(panel_year, group)]
e4b[, dw_share := round(n_dw / n_installs, 6)]
e4b <- e4b[, .(panel_year, group, n_installs, dw_share)]
setorder(e4b, panel_year, group)
fwrite(e4b, file.path(tab_dir, "T020_E4_Install_Wall.csv"))
cat("  saved T020_E4_Install_Wall.csv\n")
print(e4b[panel_year %in% c(1999, 2005, 2010, 2015, 2020)])

p_e4 <- ggplot(e4b, aes(panel_year, dw_share, color = group)) +
  geom_line(linewidth = 1) + geom_point(size = 1.5) +
  scale_color_manual(values = c(TX = RB_COL, Control = FF_COL)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Year", y = "DW share of installs", color = "Group",
       title = "New installs are overwhelmingly double-walled") +
  theme_minimal(base_size = 13)
ggsave(file.path(fig_dir, "T020_E4_Install_DW_Share.png"), p_e4,
       width = 7.5, height = 5, dpi = 150)
cat("  saved T020_E4_Install_DW_Share.png\n")

cat("=== 04ao DONE ===\n")
