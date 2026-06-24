# 04ab_State_Transitions_Canonical.R
# Recompute the STATE-space diagnostics from the canonical estimation panel
# (dcm_obs_panel_observed.csv) -- do NOT reuse old T013 outputs.
# Question: when a facility is coded MAINTAIN, does its state (age, wall) move in
# ways the model's Maintain transition forbids? And is the wall flip a downsizing
# artifact (drop the SW tank -> facility becomes all-DW -> SW->DW under Maintain)?

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
cat("=== 04ab STATE TRANSITIONS (canonical) ===\n")

obs <- fread(here("Data","Analysis","dcm_obs_panel_observed.csv"),
             select = c("panel_id","panel_year","A_bin","w_state","rho_state","y_it","I_replace"))
fp  <- fread(here("Data","Analysis","facility_panel.csv"),
             select = c("panel_id","panel_year","any_closure","facility_complete_closure",
                        "n_sw_closures","n_closures"))
m <- merge(obs, fp, by = c("panel_id","panel_year"), all.x = FALSE)
for (c0 in c("any_closure","facility_complete_closure","n_sw_closures","n_closures"))
  m[is.na(get(c0)), (c0) := 0L]

setorder(m, panel_id, panel_year)
m[, `:=`(next_w = shift(w_state, -1L), next_A = shift(A_bin, -1L),
         next_year = shift(panel_year, -1L)), by = panel_id]
m[, consec := !is.na(next_year) & next_year == panel_year + 1L]

# MAINTAIN transitions only (model says: wall fixed, age +1)
mt <- m[y_it == 0L & consec == TRUE]
N <- nrow(mt)
mt[, is_downsizing := any_closure == 1L & facility_complete_closure == 0L]  # closed a tank, kept operating
mt[, wall_flip_sw_dw := w_state == 1L & next_w == 2L]
mt[, wall_flip_dw_sw := w_state == 2L & next_w == 1L]
mt[, age_back        := next_A < A_bin]

cat(sprintf("Maintain transitions (consecutive years): %d\n", N))
cat(sprintf("  SW->DW wall flip:  %d (%.3f%%)\n", sum(mt$wall_flip_sw_dw), 100*mean(mt$wall_flip_sw_dw)))
cat(sprintf("  DW->SW wall flip:  %d (%.3f%%)\n", sum(mt$wall_flip_dw_sw), 100*mean(mt$wall_flip_dw_sw)))
cat(sprintf("  backward age move: %d (%.3f%%)\n", sum(mt$age_back), 100*mean(mt$age_back)))
cat(sprintf("Downsizing maintain-years: %d\n", sum(mt$is_downsizing)))
cat(sprintf("  of SW->DW flips, share that are downsizing: %.1f%%; share that closed an SW tank: %.1f%%\n",
            100*mean(mt[wall_flip_sw_dw == TRUE]$is_downsizing),
            100*mean(mt[wall_flip_sw_dw == TRUE]$n_sw_closures > 0L)))

# violation rate among true-maintain vs downsizing-coded-maintain
viol <- mt[, .(
  `SW->DW wall flip` = 100*mean(wall_flip_sw_dw),
  `Backward age move` = 100*mean(age_back)
), by = .(grp = ifelse(is_downsizing, "Downsizing (closed a tank,\nkept operating)", "True maintain (no closure)"))]
vl <- melt(viol, id.vars = "grp", variable.name = "violation", value.name = "pct")
p <- ggplot(vl, aes(x = violation, y = pct, fill = grp)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), position = position_dodge(width = 0.7),
            vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("True maintain (no closure)" = "#4C78A8",
                               "Downsizing (closed a tank,\nkept operating)" = "#E45756")) +
  labs(x = NULL, y = "Share of \"Maintain\" years with a forbidden state move", fill = NULL,
       title = "The state also breaks: forbidden moves hide inside \"Maintain\"",
       subtitle = "Model Maintain keeps wall fixed and ages the tank +1; downsizing years break both rules") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"), panel.grid.major.x = element_blank())
ggsave(here("Output","Figures","04ab_StateViolations_inMaintain.png"), p, width = 9.5, height = 5.2, dpi = 150)
cat("  saved 04ab_StateViolations_inMaintain.png\n=== 04ab DONE ===\n")
