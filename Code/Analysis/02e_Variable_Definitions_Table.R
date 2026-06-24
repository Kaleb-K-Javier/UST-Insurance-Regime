################################################################################
# 02e_Variable_Definitions_Table.R
#
# Builds the variable-definitions (codebook) table for the paper's Data section.
# Content is a static codebook (no data read); emitted as a booktabs +
# threeparttable .tex that the qmd \input's on its own page. The AER/QJE-style
# footnote (roman body) lives in the threeparttable tablenotes; the short caption
# is set by the qmd chunk (#| tbl-cap:). Three columns only (no Source column);
# sources are summarized in the note.
#
# Writes: Output/Tables/T_Variable_Definitions.tex
################################################################################

suppressPackageStartupMessages({ library(here) })

OUT <- here::here("Output", "Tables")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# one body row: Variable | What it measures | Uses in analysis
row <- function(v, meas, use)
  sprintf("%s & %s & %s \\\\", v, meas, use)

# section header spanning all three columns
sec <- function(t)
  sprintf("\\addlinespace\\multicolumn{3}{@{}l}{\\textit{%s}}\\\\[1pt]", t)

note <- paste(
  "\\item \\textit{Notes:} Unit of analysis is the facility-year, 1990--2021,",
  "across 18 study states (Texas and 17 flat-fee control states). Sources: tank",
  "attributes, closures, and confirmed releases from the EPA LUST Finder (state",
  "UST administrative registries harmonized by the EPA); Texas premiums from TDI",
  "rate manuals and TCEQ contracts; control-state fees and deductibles from the",
  "ASTSWMO fund survey; cleanup cost from state trust-fund claims. Premium,",
  "hazard, deductible, and cleanup liability enter the dynamic model as calibrated",
  "first-stage inputs, not estimated parameters."
)

tex <- c(
  "\\begin{threeparttable}",
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\renewcommand{\\arraystretch}{1.02}",
  "\\begin{tabular}{@{}p{2.9cm}p{7.4cm}p{3.7cm}@{}}",
  "\\toprule",
  "\\textbf{Variable} & \\textbf{What it measures} & \\textbf{Uses in analysis} \\\\",
  "\\midrule",

  sec("Outcomes"),
  row("Tank closure",
      "Equals one in the year a tank is closed.",
      "Outcome (DiD)"),
  row("Facility exit",
      "Equals one in a facility's last operating year.",
      "Outcome (DiD); exit action (structural)"),
  row("Replacement vs.\\ permanent closure",
      "A closure is a replacement if the facility installs a tank afterward; otherwise permanent.",
      "Outcome (DiD); replace action (structural)"),
  row("Leak discovery",
      "Equals one in a year the facility has a confirmed release.",
      "Outcome (DiD)"),
  row("First release",
      "Equals one in the first year a never-yet-leaked facility has a confirmed release.",
      "Outcome (leak hazard)"),

  sec("Treatment and regime"),
  row("Texas",
      "Equals one for Texas, zero for the 17 control states.",
      "Treatment (DiD)"),
  row("Post",
      "Equals one from 1999 on (Texas fund closed Dec.\\ 1998).",
      "Treatment (DiD)"),
  row("Texas $\\times$ Post",
      "Interaction; its coefficient is the average treatment effect.",
      "Treatment (DiD)"),
  row("Regime",
      "Flat-fee (control states; Texas pre-1999) vs.\\ risk-based (Texas post-1999).",
      "State (structural model)"),

  sec("Tank and facility characteristics"),
  row("Tank age",
      "Mean age of a facility's active tanks; grouped into three-year bins.",
      "Control (DiD); state (structural)"),
  row("Wall type",
      "Single- vs.\\ double-walled construction (facility flag: any single-walled).",
      "Control (DiD); state (structural)"),
  row("Capacity",
      "Total facility tank capacity in gallons; binned for the structural state.",
      "Control (DiD); state (structural)"),
  row("Fuel",
      "Gasoline, diesel, or other stored product.",
      "Control (DiD)"),
  row("Portfolio composition",
      "Tank counts in each wall $\\times$ age cell, the facility's full fleet.",
      "State (structural model)"),
  row("Install vintage",
      "Year the facility's first tank was installed, banded.",
      "Heterogeneity (DiD)"),
  row("Federal compliance mandates",
      "Whether a tank met the EPA spill, overfill, and release-detection deadlines.",
      "Control (DiD)"),

  sec("Priced and calibrated inputs"),
  row("Premium $P$",
      "Annual insurance bill for the facility.",
      "Maintain payoff (structural)"),
  row("Hazard $\\hat{h}$",
      "Predicted annual probability of a first release, by age and wall.",
      "Maintain payoff (structural)"),
  row("Deductible $D$",
      "Out-of-pocket cost per claim ($\\approx$\\$5{,}000 in Texas; varies across funds).",
      "Maintain payoff (structural)"),
  row("Cleanup liability $L$",
      "Remediation cost per release.",
      "Welfare accounting"),

  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\small",
  note,
  "\\end{tablenotes}",
  "\\end{threeparttable}"
)

writeLines(tex, file.path(OUT, "T_Variable_Definitions.tex"))
cat(sprintf("Saved: %s (%d lines)\n",
            file.path(OUT, "T_Variable_Definitions.tex"), length(tex)))
