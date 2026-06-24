# 04ag_Size_Stickiness_Diagnostic.R
# Tests the FIXED-TYPE assumption for facility size in the size-scaled portfolio
# model: is size constant over a facility's OPERATING SPELL (not just year-to-year)?
# A fixed type must be constant over the whole spell, so a 98% one-year diagonal is
# NOT sufficient -- it compounds (0.98^20 ~ 0.67). We measure:
#   (D1) one-year size-bin transition matrix, conditional on continuing (count + capacity)
#   (D2) spell-level fixed-type: % of facilities that NEVER change bin; compounding
#   (D3) count vs capacity stickiness (consolidation => capacity should be stickier)
#   (D4) direction of changes + coincidence with a coded action (replace/install/closure)
#   (D5) by initial size bin and regime
# Read-only. Frame = the estimation sample (dcm_obs_panel_observed).

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
cat("=== 04ag SIZE STICKINESS (fixed-type test) ===\n")

OUT_T <- here("Output","Tables"); OUT_F <- here("Output","Figures")
dir.create(OUT_T, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_F, recursive = TRUE, showWarnings = FALSE)

# ---- load estimation-sample frame + facility capacity / action flags ----
dcm <- fread(here("Data","Analysis","dcm_obs_panel_observed.csv"),
             select = c("panel_id","panel_year","size_bin","boy_stock",
                        "rho_state","y_it","I_replace"))
fac <- fread(here("Data","Analysis","facility_panel.csv"),
             select = c("panel_id","panel_year","total_capacity",
                        "n_installs","n_closures_permanent"))
dcm <- merge(dcm, fac, by = c("panel_id","panel_year"), all.x = TRUE)
rm(fac)
for (c0 in c("n_installs","n_closures_permanent")) dcm[is.na(get(c0)), (c0) := 0L]
n_pre <- nrow(dcm)
dcm <- dcm[boy_stock >= 1L & !is.na(total_capacity)]   # drop entrant/zero-stock rows w/o a valid BOY size
cat(sprintf("  dropped %s rows (%.2f%%) with missing BOY stock/capacity\n",
            format(n_pre - nrow(dcm), big.mark=","), 100*(n_pre - nrow(dcm))/n_pre))
setorder(dcm, panel_id, panel_year)
cat(sprintf("  facility-years: %s | facilities: %s\n",
            format(nrow(dcm), big.mark=","), format(uniqueN(dcm$panel_id), big.mark=",")))

# count bin = size_bin (1/2/3/4+); capacity bin = sample quartiles of total_capacity
dcm[, count_bin := factor(pmin(boy_stock, 4L), levels = 1:4, labels = c("1","2","3","4+"))]
cap_brk <- quantile(dcm$total_capacity, c(0,.25,.5,.75,1), na.rm = TRUE)
dcm[, cap_bin := cut(total_capacity, breaks = unique(cap_brk), include.lowest = TRUE,
                     labels = c("Q1","Q2","Q3","Q4")[seq_len(length(unique(cap_brk))-1)])]
dcm[, regime := factor(rho_state, levels = c(1L,2L), labels = c("FF","RB"))]

# ---- D1: one-year transition matrices, conditional on continuing ----
cat("\n--- D1: one-year transition (conditional on operating next year) ---\n")
trans_mat <- function(dt, col) {
  dt[, nx  := shift(get(col), type = "lead"), by = panel_id]
  dt[, nyr := shift(panel_year, type = "lead"), by = panel_id]
  tr <- dt[!is.na(nx) & nyr == panel_year + 1L]            # consecutive operating years
  m  <- tr[, .N, by = .(from = get(col), to = nx)]
  m[, frac := N / sum(N), by = from]
  list(long = m[order(from, to)],
       diag = m[from == to, .(from, stay = frac)],
       n_trans = nrow(tr))
}
tc <- trans_mat(copy(dcm), "count_bin")
tk <- trans_mat(copy(dcm), "cap_bin")
fwrite(tc$long, file.path(OUT_T, "04ag_SizeTransition_Count.csv"))
fwrite(tk$long, file.path(OUT_T, "04ag_SizeTransition_Capacity.csv"))
cat("  count-bin one-year stay probs:\n");    print(tc$diag)
cat("  capacity-bin one-year stay probs:\n"); print(tk$diag)

# ---- D2 + D3: spell-level fixed-type test (count vs capacity) ----
cat("\n--- D2/D3: spell-level fixed type (never changed bin over the whole spell) ---\n")
spell <- dcm[, .(n_years    = .N,
                 first_yr   = min(panel_year),
                 chg_count  = uniqueN(count_bin) > 1L,
                 chg_cap    = uniqueN(cap_bin)   > 1L,
                 first_count= count_bin[which.min(panel_year)],
                 first_cap  = cap_bin[which.min(panel_year)]),
             by = panel_id]
diag_c <- tc$long[from == to, sum(N)] / tc$long[, sum(N)]   # overall 1-yr diagonal (count)
diag_k <- tk$long[from == to, sum(N)] / tk$long[, sum(N)]   # overall 1-yr diagonal (cap)
mean_spell <- mean(spell$n_years)
fixed_summary <- data.table(
  measure        = c("Count (tanks)","Capacity (quartile)"),
  one_year_stay  = c(diag_c, diag_k),
  mean_spell_yrs = mean_spell,
  implied_if_iid = c(diag_c, diag_k)^mean_spell,          # naive compounding
  actual_never_changed = c(mean(!spell$chg_count), mean(!spell$chg_cap)))
fwrite(fixed_summary, file.path(OUT_T, "04ag_FixedType_Summary.csv"))
cat("  one-year stay vs spell-level never-changed (the real fixed-type test):\n")
print(fixed_summary)

# share of OPERATING facility-years spent in the facility's first bin
dcm <- merge(dcm, spell[, .(panel_id, first_count, first_cap)], by = "panel_id")
cat(sprintf("  facility-years in first count bin: %.1f%% | in first capacity bin: %.1f%%\n",
            100*mean(dcm$count_bin == dcm$first_count),
            100*mean(dcm$cap_bin   == dcm$first_cap)))

# ---- D4: direction of changes + coincidence with a coded action ----
cat("\n--- D4: when size changes, direction + does it coincide with an action? ---\n")
dcm[, cb_next := shift(as.integer(count_bin), type = "lead"), by = panel_id]
dcm[, nyr2    := shift(panel_year, type = "lead"), by = panel_id]
chg <- dcm[!is.na(cb_next) & nyr2 == panel_year + 1L]
chg[, dir := fcase(cb_next > as.integer(count_bin), "up (expand)",
                   cb_next < as.integer(count_bin), "down (shrink)",
                   default = "same")]
chg[, coded_action := (I_replace == 1L) | (n_installs > 0L) | (n_closures_permanent > 0L)]
d4 <- chg[dir != "same", .(n = .N,
                           pct_with_coded_action = 100*mean(coded_action)), by = dir]
d4[, pct_of_all_changes := 100*n/sum(n)]
fwrite(d4, file.path(OUT_T, "04ag_SizeChange_ActionCoincidence.csv"))
print(d4[order(-n)])
cat(sprintf("  overall: %.2f%% of operating-year transitions change the count bin; of those, %.1f%% coincide with a coded action\n",
            100*mean(chg$dir != "same"),
            100*mean(chg[dir != "same"]$coded_action)))

# ---- D5: stickiness by initial size and regime ----
cat("\n--- D5: spell-level never-changed by initial size bin and regime ---\n")
d5 <- merge(spell, unique(dcm[, .(panel_id, regime,
                                  init_size = first_count)]), by = "panel_id")
d5tab <- d5[, .(n_fac = .N,
                never_changed_count = round(100*mean(!chg_count),1),
                never_changed_cap   = round(100*mean(!chg_cap),1)),
            by = .(init_size, regime)][order(init_size, regime)]
fwrite(d5tab, file.path(OUT_T, "04ag_Stickiness_byInitSize_Regime.csv"))
print(d5tab)

# ---- Figure: fraction still in ORIGINAL bin vs years since entry (count vs cap) ----
cat("\n--- figure: fixed-type survival (still in original bin) ---\n")
dcm[, yr_since := panel_year - min(panel_year), by = panel_id]
# at horizon h, the rows present are exactly facilities observed >= h+1 years;
# mean(bin == first_bin) among them = fraction still in original bin at horizon h
surv <- dcm[yr_since <= 15, .(count    = mean(count_bin == first_count, na.rm = TRUE),
                              capacity = mean(cap_bin   == first_cap,   na.rm = TRUE),
                              n = .N), by = yr_since][order(yr_since)]
setnames(surv, "yr_since", "h")
surv_long <- melt(surv[, .(h, count, capacity)], id.vars = "h",
                  variable.name = "size_measure", value.name = "frac")
p <- ggplot(surv_long, aes(h, frac, color = size_measure)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_color_manual(values = c(count = "#E76F51", capacity = "#2A9D8F"),
                     labels = c("Tank count", "Total capacity")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(NA, 1)) +
  labs(x = "Years since first observed", y = "Still in original size bin",
       color = "Size measure",
       title = "Facility size is a near-fixed type, especially by capacity",
       subtitle = "Among facilities observed at least h years; consolidation preserves capacity, so capacity is stickier than count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"), panel.grid.minor = element_blank())
ggsave(file.path(OUT_F, "04ag_FixedType_Survival.png"), p, width = 8.5, height = 5, dpi = 150)
cat("  saved 04ag_FixedType_Survival.png\n")

# ---- compact .tex for the identification doc + slide ----
ft <- fixed_summary
tex <- c("\\begin{center}\\small\\renewcommand{\\arraystretch}{1.2}",
  "\\begin{tabular}{lrrr}", "\\toprule",
  "\\textbf{Size measure} & \\textbf{1-yr stay} & \\textbf{Implied over spell} & \\textbf{Actual never changed} \\\\",
  "\\midrule",
  sprintf("%s & %.1f\\%% & %.1f\\%% & %.1f\\%% \\\\", ft$measure[1],
          100*ft$one_year_stay[1], 100*ft$implied_if_iid[1], 100*ft$actual_never_changed[1]),
  sprintf("%s & %.1f\\%% & %.1f\\%% & %.1f\\%% \\\\", ft$measure[2],
          100*ft$one_year_stay[2], 100*ft$implied_if_iid[2], 100*ft$actual_never_changed[2]),
  "\\bottomrule", "\\end{tabular}", "\\end{center}",
  sprintf("\\vspace{0.1cm}{\\centering\\scriptsize Mean operating spell $=$ %.1f years. ``Implied over spell'' compounds the one-year stay rate; ``actual'' is the share of facilities whose bin never changes over their full spell.\\par}", ft$mean_spell_yrs[1]))
writeLines(tex, file.path(OUT_T, "04ag_FixedType_Summary.tex"))
cat("  saved 04ag_FixedType_Summary.tex\n=== 04ag DONE ===\n")
