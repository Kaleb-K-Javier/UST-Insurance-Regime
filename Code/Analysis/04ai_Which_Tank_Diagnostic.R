# 04ai_Which_Tank_Diagnostic.R
# Validates the Layer-2 marginal-tank state + consolidation transition: when a
# multi-tank facility closes ONE tank (the resize/downsize margin), is the chosen
# tank PREDICTABLE -- the oldest / the single-walled / the smallest? If yes, the
# representative state can be defined on the marginal (oldest) tank, and "replace"
# can step the state down to the next-oldest, all within the single-agent model.
# Tank-level source: panel_dt.csv (3.9 GB). Read-only.

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
cat("=== 04ai WHICH TANK DO THEY CLOSE? ===\n")
OUT_T <- here("Output","Tables"); OUT_F <- here("Output","Figures")

dt <- fread(here("Data","Analysis","panel_dt.csv"),
            select = c("panel_id","panel_year","mm_wall","capacity","tank_age","closure_event"))
cat(sprintf("  tank-years read: %s\n", format(nrow(dt), big.mark=",")))
cat("  mm_wall values:\n"); print(dt[, .N, by = mm_wall][order(-N)])

dt <- dt[!is.na(tank_age) & tank_age >= 0 & !is.na(capacity) & capacity > 0 & !is.na(closure_event)]
dt[, sw := grepl("single|^sw$|^s$", mm_wall, ignore.case = TRUE)]   # single-walled flag

# ---- cheap facility-year aggregates FIRST, then rank only the target subset ----
# (ranking all 12.7M rows by group blows memory; filter to the which-tank margin first)
fy <- dt[, .(n_active = .N, n_closed = sum(closure_event)), by = .(panel_id, panel_year)]
base_oldest <- fy[n_active >= 2L & n_closed == 1L, mean(1 / n_active)]   # random P(tank = oldest)
tgt <- fy[n_active >= 2L & n_closed == 1L, .(panel_id, panel_year)]      # multi-tank, single closure
sub <- dt[tgt, on = .(panel_id, panel_year)]                            # tanks in those facility-years only
cat(sprintf("  target facility-years: %s | tanks in them: %s\n",
            format(nrow(tgt), big.mark=","), format(nrow(sub), big.mark=",")))

sub[, n_active     := .N,                 by = .(panel_id, panel_year)]
sub[, fac_mixed    := any(sw) & any(!sw), by = .(panel_id, panel_year)]
sub[, sw_share_fac := mean(sw),           by = .(panel_id, panel_year)]
sub[, age_rank := (frank(tank_age, ties.method = "average") - 1) / pmax(n_active - 1L, 1L), by = .(panel_id, panel_year)]
sub[, cap_rank := (frank(capacity, ties.method = "average") - 1) / pmax(n_active - 1L, 1L), by = .(panel_id, panel_year)]
sub[, is_oldest   := tank_age == max(tank_age), by = .(panel_id, panel_year)]
sub[, is_smallest := capacity == min(capacity), by = .(panel_id, panel_year)]

mc <- sub[closure_event == 1L]   # the single closed tank per target facility-year
cat(sprintf("  single-closure events (multi-tank facility-years): %s\n", format(nrow(mc), big.mark=",")))
sw_base <- mc[fac_mixed == TRUE, mean(sw_share_fac)]                    # base SW share in mixed facilities

summ <- data.table(
  metric = c("Mean AGE rank of closed tank (0=youngest,1=oldest)",
             "P(closed tank is the OLDEST)",
             "  vs random baseline ~E[1/n]",
             "P(closed tank is SINGLE-WALLED | mixed-wall facility)",
             "  vs base SW share in mixed facilities",
             "Mean CAPACITY rank of closed tank (0=smallest,1=largest)",
             "P(closed tank is the SMALLEST capacity)"),
  value = c(mc[, mean(age_rank, na.rm = TRUE)],
            mc[, mean(is_oldest)],
            base_oldest,
            mc[fac_mixed == TRUE, mean(sw)],
            sw_base,
            mc[, mean(cap_rank, na.rm = TRUE)],
            mc[, mean(is_smallest)]))
summ[, value := round(value, 3)]
fwrite(summ, file.path(OUT_T, "04ai_WhichTank_Summary.csv"))
cat("\n--- which-tank predictability ---\n"); print(summ)

# by regime is not in panel_dt directly; split by facility size instead
bysize <- mc[, .(n_events = .N,
                 mean_age_rank = round(mean(age_rank, na.rm = TRUE), 3),
                 p_oldest      = round(mean(is_oldest), 3),
                 p_sw_if_mixed = round(mean(sw[fac_mixed], na.rm = TRUE), 3)),
             by = .(size = pmin(n_active, 5L))][order(size)]
fwrite(bysize, file.path(OUT_T, "04ai_WhichTank_bySize.csv"))
cat("\n--- by facility size (active tanks; 5 = 5+) ---\n"); print(bysize)

# save mc (tiny) so figures can be re-built without re-reading panel_dt
fwrite(mc[, .(panel_id, panel_year, n_active, sw, fac_mixed, age_rank, cap_rank, is_oldest, is_smallest)],
       file.path(OUT_T, "04ai_mc.csv"))

# ---- figure: what kind of tank gets shed, observed vs a random-tank benchmark ----
comp <- rbindlist(list(
  data.table(characteristic = "Single-walled\n(mixed facilities)", which = "Observed",  pct = mc[fac_mixed == TRUE, mean(sw)]),
  data.table(characteristic = "Single-walled\n(mixed facilities)", which = "If random",  pct = sw_base),
  data.table(characteristic = "Smallest\ncapacity",                which = "Observed",  pct = mc[, mean(is_smallest)]),
  data.table(characteristic = "Smallest\ncapacity",                which = "If random",  pct = base_oldest),
  data.table(characteristic = "Oldest",                            which = "Observed",  pct = mc[, mean(is_oldest)]),
  data.table(characteristic = "Oldest",                            which = "If random",  pct = base_oldest)))
comp[, characteristic := factor(characteristic,
       levels = c("Single-walled\n(mixed facilities)","Smallest\ncapacity","Oldest"))]
comp[, which := factor(which, levels = c("If random","Observed"))]
p <- ggplot(comp, aes(characteristic, pct, fill = which)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  geom_text(aes(label = sprintf("%.0f%%", 100*pct)), position = position_dodge(width = 0.75),
            vjust = -0.3, size = 3.5, color = "grey25") +
  scale_fill_manual(values = c("If random" = "#BBBBBB", "Observed" = "#2A9D8F")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = NULL, y = "Share of single-closure events", fill = NULL,
       title = "The shed tank is the small, single-walled one --- not random",
       subtitle = "When a multi-tank facility closes one tank, what kind is it? Observed vs a random-tank benchmark. (Age is weaker --- facilities install tanks together, so \"oldest\" is often tied.)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"), panel.grid.minor = element_blank())
ggsave(file.path(OUT_F, "04ai_WhichTank_Shed.png"), p, width = 8.5, height = 5, dpi = 150)
cat("\n  saved 04ai_WhichTank_Shed.png + 04ai_mc.csv\n")

# ---- compact .tex ----
tex <- c("\\begin{center}\\small\\renewcommand{\\arraystretch}{1.25}",
  "\\begin{tabular}{lrr}", "\\toprule",
  "\\textbf{The closed tank is\\ldots} & \\textbf{Observed} & \\textbf{If random} \\\\", "\\midrule",
  sprintf("single-walled (in mixed-wall facilities) & %.0f\\%% & %.0f\\%% \\\\",
          100*mc[fac_mixed == TRUE, mean(sw)], 100*sw_base),
  sprintf("the smallest-capacity tank & %.0f\\%% & %.0f\\%% \\\\",
          100*mc[, mean(is_smallest)], 100*base_oldest),
  sprintf("the oldest tank & %.0f\\%% & %.0f\\%% \\\\",
          100*mc[, mean(is_oldest)], 100*base_oldest),
  "\\bottomrule", "\\end{tabular}", "\\end{center}",
  "\\vspace{0.1cm}{\\centering\\scriptsize Single-closure events in multi-tank facility-years (panel\\_dt, $n=19{,}744$). The shed tank is far more often the small, single-walled one than a random draw; the age signal is weaker because most facilities install their tanks together, so ``oldest'' is often tied. Supports a marginal-tank representation keyed on wall and size, with a capacity-preserving step-down (consolidation) transition.\\par}")
writeLines(tex, file.path(OUT_T, "04ai_WhichTank_Summary.tex"))
cat("  saved 04ai_WhichTank_Summary.tex\n=== 04ai DONE ===\n")
