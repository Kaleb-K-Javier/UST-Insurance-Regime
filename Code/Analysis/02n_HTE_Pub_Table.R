# 02n_HTE_Pub_Table.R
# Blessed facility-HTE publication table (tbl-fac-hte) built straight from the 02l CSV.
# Presentation only — no estimation. 02l writes T_Facility_HTE.csv (indicator margins, oldtank_size
# FE, no Yhat0); the old T_Facility_HTE_*_Pub.tex are dead 02j (Predicted baseline / Mix x year FE).
# Output: Output/Tables/T_Facility_HTE_Pub.tex (booktabs tabular; editor wraps caption + label tbl-fac-hte).
#
#   Rows: reform-x-post effect in PERCENTAGE POINTS. Per characteristic, the first (baseline) row is
#         the effect at the reference level; subsequent rows are the differential vs reference.
#         SE (clustered by state) in parentheses under each value; stars from p.
#   Order follows the prose: WHO (size -> vintage -> fuel), then WHERE (thin market -> rural ->
#         low-pop -> low-income -> high-poverty).
#   Cols: any_closure, facility_exit, downsize, consolidate, any_replace (reconfigure_up dropped).

suppressPackageStartupMessages({ library(data.table); library(here) })
hte <- fread(here("Output", "Tables", "T_Facility_HTE.csv"),
             colClasses = list(character = c("margin", "dimension", "level")))

MARG <- c("any_closure", "facility_exit", "downsize", "consolidate", "any_replace")
HEAD <- c("Any closure", "Facility exit", "Downsize", "Consolidate", "Any replacement")

# dimension -> (title, ref-level string, ordered c(level = display label)); ref level marked "(baseline)"
CFG <- list(
  list(dim="cap_G",        title="Size (total capacity)",   ref="G1_lt9k",
       lv=c(G1_lt9k="Under 9k gal", G2_9to20k="9--20k gal", G3_20to30k="20--30k gal", G4_gt30k="Over 30k gal")),
  list(dim="vintage",      title="Vintage (oldest tank)",   ref="1989-1998",
       lv=c(`1989-1998`="1989--1998", `1985-1988`="1985--1988", `1975-1984`="1975--1984", `Pre-1975`="Pre-1975")),
  list(dim="has_gasoline", title="Fuel",                    ref="0",
       lv=c(`0`="Non-gasoline", `1`="Gasoline")),
  list(dim="thin_market",  title="Local competition",       ref="0",
       lv=c(`0`="Competitive", `1`="Thin ($\\le$1 gas competitor)")),
  list(dim="rural",        title="Rural",                   ref="0",
       lv=c(`0`="Urban", `1`="Rural")),
  list(dim="low_pop",      title="Population density",       ref="0",
       lv=c(`0`="Higher density", `1`="Low density")),
  list(dim="low_income",   title="Income",                  ref="0",
       lv=c(`0`="Higher income", `1`="Low income")),
  list(dim="high_pov",     title="Poverty",                 ref="0",
       lv=c(`0`="Lower poverty", `1`="High poverty"))
)
WHO_DIMS <- c("cap_G", "vintage", "has_gasoline")   # panel split

star <- function(p) if (is.na(p)) "" else if (p < .01) "$^{***}$" else if (p < .05) "$^{**}$" else if (p < .1) "$^{*}$" else ""
getv <- function(dim, lvl, m) {
  r <- hte[dimension == dim & level == lvl & margin == m]
  if (nrow(r) == 0L) list(e = NA_real_, s = NA_real_, p = NA_real_) else list(e = r$estimate, s = r$std_error, p = r$p_value)
}

emit_dim <- function(cfg) {
  out <- sprintf("\\addlinespace\n\\multicolumn{6}{@{}l}{\\textit{%s}} \\\\", cfg$title)
  for (lvl in names(cfg$lv)) {
    lab <- cfg$lv[[lvl]]; if (lvl == cfg$ref) lab <- paste0(lab, " (baseline)")
    est_cells <- se_cells <- character(length(MARG))
    for (j in seq_along(MARG)) {
      v <- getv(cfg$dim, lvl, MARG[j])
      est_cells[j] <- if (is.na(v$e)) "" else sprintf("%+.2f%s", 100 * v$e, star(v$p))
      se_cells[j]  <- if (is.na(v$s)) "" else sprintf("(%.2f)", 100 * v$s)
    }
    out <- c(out,
      sprintf("\\quad %s & %s \\\\", lab, paste(est_cells, collapse = " & ")),
      sprintf(" & %s \\\\", paste(se_cells, collapse = " & ")))
  }
  out
}

NOTES <- paste0(
  "\\textit{Notes:} Cells are the reform-$\\times$-post effect on 1/0 facility-year outcome ",
  "indicators, in percentage points. For each characteristic the first (baseline) row is the effect ",
  "at the reference level; each subsequent row is the differential relative to that reference. ",
  "Standard errors, clustered by state, in parentheses. $^{*}p<0.1$, $^{**}p<0.05$, $^{***}p<0.01$. ",
  "Fixed effects: facility $+$ portfolio-composition$\\times$year (headline \\texttt{oldtank\\_size}). ",
  "With a single treated cluster (Texas), analytic standard errors over-reject and are provisional. ",
  "Thin market $=$ at most one gasoline competitor within one mile.")

lines <- c(
  "\\begin{tabular}{lccccc}", "\\toprule",
  paste0(" & ", paste(HEAD, collapse = " & "), " \\\\"), "\\midrule",
  "\\multicolumn{6}{@{}l}{\\textbf{Who closes}} \\\\",
  unlist(lapply(CFG[sapply(CFG, function(c) c$dim %in% WHO_DIMS)], emit_dim)),
  "\\addlinespace", "\\multicolumn{6}{@{}l}{\\textbf{Where}} \\\\",
  unlist(lapply(CFG[sapply(CFG, function(c) !(c$dim %in% WHO_DIMS))], emit_dim)),
  "\\bottomrule",
  sprintf("\\multicolumn{6}{@{}p{0.95\\textwidth}@{}}{\\footnotesize %s} \\\\", NOTES),
  "\\end{tabular}")

writeLines(lines, here("Output", "Tables", "T_Facility_HTE_Pub.tex"))
cat(sprintf("Wrote T_Facility_HTE_Pub.tex (%d dimensions, %d margins)\n", length(CFG), length(MARG)))
