# 04z_Model_vs_Reality.R
# Teaching device: where does each REAL facility-year action get coded in the
# 3-action model? Cross-tab true facility event (mutually exclusive) x model action.
# Also: the model action coded for DW-tank closures (answers "is DW shrink coded replace?").
# Merge = obs estimation panel + facility_panel flags (the T011 audit join).

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
cat("=== 04z MODEL vs REALITY ===\n")

obs <- fread(here("Data","Analysis","dcm_obs_panel_observed.csv"),
             select = c("panel_id","panel_year","w_state","rho_state","y_it","I_replace"))
fp  <- fread(here("Data","Analysis","facility_panel.csv"),
             select = c("panel_id","panel_year","any_closure","facility_complete_closure",
                        "replacement_closure_year","permanent_closure_year","n_installs",
                        "single_to_double_year"))
m <- merge(obs, fp, by = c("panel_id","panel_year"), all.x = FALSE)
for (c0 in c("any_closure","facility_complete_closure","replacement_closure_year",
             "permanent_closure_year","n_installs","single_to_double_year")) m[is.na(get(c0)), (c0) := 0L]

# model action (the 3-way partition the estimator uses)
m[, model_action := fcase(y_it == 0L, "Maintain",
                          y_it == 1L & I_replace == 0L, "Exit",
                          default = "Replace")]
# true facility event (mutually exclusive, priority order); split replace into the
# SW->DW upgrade the model ASSUMES vs same-wall replacements
m[, true_event := fcase(
  any_closure == 0L,                                              "No tank closed",
  replacement_closure_year == 1L & single_to_double_year == 1L,   "Replace: SW->DW upgrade",
  replacement_closure_year == 1L,                                 "Replace: same wall",
  facility_complete_closure == 1L,                                "Full exit (all tanks closed)",
  default                                                       = "Downsizing (kept operating)")]
m[, model_action := factor(model_action, levels = c("Maintain","Exit","Replace"))]
m[, true_event := factor(true_event, levels = c(
  "No tank closed","Replace: SW->DW upgrade","Replace: same wall",
  "Full exit (all tanks closed)","Downsizing (kept operating)"))]

N <- nrow(m)
ct <- m[, .N, by = .(true_event, model_action)]
ct_wide <- dcast(ct, true_event ~ model_action, value.var = "N", fill = 0)
cat("\n--- Cross-tab: true event (rows) x model action (cols), counts ---\n"); print(ct_wide)
tot <- m[, .(N = .N, share = round(100*.N/N, 2)), by = true_event][order(-N)]
cat("\n--- True-event totals ---\n"); print(tot)
fwrite(ct_wide, here("Output","Tables","04z_Confusion_TrueEvent_x_ModelAction.csv"))

# ---- frequency table (.tex): real action -> model action, % of sample, FF & RB rates ----
m[, regime := ifelse(rho_state == 2L, "RB", "FF")]
Nff <- m[regime == "FF", .N]; Nrb <- m[regime == "RB", .N]
ft <- m[, .(model = as.character(model_action[1L]), n = .N,
            share = 100 * .N / N,
            ff = 100 * sum(regime == "FF") / Nff,
            rb = 100 * sum(regime == "RB") / Nrb), by = true_event]
setorder(ft, -n)
cat("\n--- regime-rate table ---\n"); print(ft)
fmt_lbl <- c("No tank closed" = "No tank closed",
             "Replace: SW->DW upgrade" = "Replace: SW$\\to$DW upgrade $^{\\ddagger}$",
             "Replace: same wall" = "Replace: same wall (DW$\\to$DW, SW$\\to$SW)",
             "Full exit (all tanks closed)" = "Full exit (all tanks closed)",
             "Downsizing (kept operating)" = "Downsizing (closed some, kept op.) $^{\\dagger}$")
mdl_lbl <- c("Maintain" = "Maintain", "Exit" = "Exit", "Replace" = "Replace")
hd <- function(s) sprintf("%.3f\\%%", s)
rows <- vapply(seq_len(nrow(ft)), function(i) {
  te <- as.character(ft$true_event[i])
  sprintf("%s & %s & %s & %s & %s \\\\", fmt_lbl[[te]], mdl_lbl[[ft$model[i]]],
          hd(ft$share[i]), hd(ft$ff[i]), hd(ft$rb[i]))
}, character(1))
tex <- c("\\begin{center}", "\\small", "\\renewcommand{\\arraystretch}{1.25}",
  "\\begin{tabular}{llrrr}", "\\toprule",
  "\\textbf{What the facility did} & \\textbf{Model bin} & \\textbf{\\% sample} & \\textbf{FF rate} & \\textbf{RB rate} \\\\",
  "\\midrule", rows, "\\bottomrule", "\\end{tabular}", "\\end{center}",
  "\\vspace{0.1cm}",
  "{\\centering\\scriptsize $^{\\dagger}$ Downsizing is coded Maintain (no partial-closure action). ",
  "$^{\\ddagger}$ The model's Replace assumes every replace is an SW$\\to$DW reset --- but only $\\sim$46\\% are. ",
  "FF/RB rate $=$ share of that regime's facility-years.\\par}")
writeLines(tex, here("Output","Tables","04z_ActionMap_RegimeRates.tex"))
cat("  saved 04z_ActionMap_RegimeRates.tex\n")

# ---- split variants for the talk: (1) taxonomy [What did | Model bin | % sample]
#      (2) regime rates [What did | FF rate | RB rate]. Same rows/values as the
#      combined table; partitioned by column so each slide carries one message. ----
rows_tax <- vapply(seq_len(nrow(ft)), function(i) {
  te <- as.character(ft$true_event[i])
  sprintf("%s & %s & %s \\\\", fmt_lbl[[te]], mdl_lbl[[ft$model[i]]], hd(ft$share[i]))
}, character(1))
tex_tax <- c("\\begin{center}", "\\small", "\\renewcommand{\\arraystretch}{1.25}",
  "\\begin{tabular}{llr}", "\\toprule",
  "\\textbf{What the facility did} & \\textbf{Model bin} & \\textbf{\\% sample} \\\\",
  "\\midrule", rows_tax, "\\bottomrule", "\\end{tabular}", "\\end{center}",
  "\\vspace{0.1cm}",
  "{\\centering\\scriptsize $^{\\dagger}$ Downsizing is coded Maintain (no partial-closure action). ",
  "$^{\\ddagger}$ The model's Replace assumes every replace is an SW$\\to$DW reset --- but only $\\sim$46\\% are.\\par}")
writeLines(tex_tax, here("Output","Tables","04z_ActionMap_Taxonomy.tex"))
cat("  saved 04z_ActionMap_Taxonomy.tex\n")

rows_reg <- vapply(seq_len(nrow(ft)), function(i) {
  te <- as.character(ft$true_event[i])
  sprintf("%s & %s & %s \\\\", fmt_lbl[[te]], hd(ft$ff[i]), hd(ft$rb[i]))
}, character(1))
tex_reg <- c("\\begin{center}", "\\small", "\\renewcommand{\\arraystretch}{1.25}",
  "\\begin{tabular}{lrr}", "\\toprule",
  "\\textbf{What the facility did} & \\textbf{FF rate} & \\textbf{RB rate} \\\\",
  "\\midrule", rows_reg, "\\bottomrule", "\\end{tabular}", "\\end{center}",
  "\\vspace{0.1cm}",
  "{\\centering\\scriptsize FF/RB rate $=$ share of that regime's facility-years. ",
  "The RB response concentrates in \\textbf{full exit} and \\textbf{SW$\\to$DW upgrade} (both rise); downsizing \\emph{falls} under RB.\\par}")
writeLines(tex_reg, here("Output","Tables","04z_ActionMap_RegimeRates_split.tex"))
cat("  saved 04z_ActionMap_RegimeRates_split.tex\n")

# DW question: among facility-years that close a DW representative tank, model action split
dw_close <- m[w_state == 2L & any_closure == 1L]
cat(sprintf("\n--- DW representative-tank facilities WITH a closure: %d ---\n", nrow(dw_close)))
print(dw_close[, .(N = .N, pct = round(100*.N/nrow(dw_close),1)), by = .(true_event, model_action)][order(-N)])

# ---- figure: where each real action is coded (share within true event) ----
ct[, prop := N / sum(N), by = true_event]
pal <- c("Maintain" = "#4C78A8", "Exit" = "#E45756", "Replace" = "#54A24B")
p <- ggplot(ct, aes(y = true_event, x = prop, fill = model_action)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = ifelse(prop > 0.04, sprintf("%.0f%%", 100*prop), "")),
            position = position_stack(vjust = 0.5), color = "white", size = 4, fontface = "bold") +
  scale_x_continuous(labels = scales::percent, expand = expansion(c(0,0.02))) +
  scale_fill_manual(values = pal) +
  scale_y_discrete(limits = rev) +
  labs(x = "Share coded as each model action", y = NULL, fill = "Model codes it as",
       title = "Where each real facility action is coded in the 3-action model",
       subtitle = "A facility that downsizes but keeps operating has no model action -- it is coded Maintain") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"), panel.grid.major.y = element_blank())
ggsave(here("Output","Figures","04z_Confusion_TrueEvent_x_ModelAction.png"), p, width = 10.5, height = 4.6, dpi = 150)
cat("\n  saved 04z_Confusion_TrueEvent_x_ModelAction.png\n=== 04z DONE ===\n")
