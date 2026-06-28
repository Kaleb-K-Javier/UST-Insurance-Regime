# profile_texas_data.R
# ------------------------------------------------------------------------------
# Auto-profiles the Texas UST data on the SERVER and writes the live column +
# value-distribution profile to Docs/Texas_Data_Characteristics_profile.md.
# Pairs with the curated Docs/Texas_Data_Characteristics.md. Re-run after any data
# refresh so rate-engine sessions always have the true schema + codings.
#
# RUN (server, repo root):
#   & "C:\Program Files\R\R-4.4.3\bin\x64\Rscript.exe" Code/Cleaning/profile_texas_data.R
# Needs duckdb + data.table. DuckDB streams the 22GB FR panel (one scan -> compact
# table -> tabulate); small files profiled directly.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  if (!requireNamespace("duckdb", quietly = TRUE)) stop("install.packages('duckdb') first")
  library(duckdb); library(data.table)
})

root   <- getwd()
out_md <- file.path(root, "Docs", "Texas_Data_Characteristics_profile.md")
con    <- dbConnect(duckdb::duckdb()); on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
t0     <- Sys.time()

# ---- targets: a search pattern + the "interesting" columns to tabulate --------
targets <- list(
  fr_panel = list(
    label = "FR facility-month panel (22 GB)",
    rx    = "texas_fr_facility_month_panel\\.csv$",
    int   = "ISSUER|CATEGORY|DETAIL_TYPE|COVER|_met$|prepaid|uses_|fr_covered|deduct|mechanism|proof|multiple_contracts"),
  fr_contract = list(
    label = "FR contract-month panel (engine's fa_monthly_contract)",
    rx    = "contract_month_panel\\.csv$|fa_monthly_contract\\.csv$",
    int   = "ISSUER|COVER|DEDUCT|EFF|EXP|LIMIT|_met$|MECH"),
  raw_tank = list(
    label = "RAW tank registry (construction/detection detail)",
    rx    = "raw_pst_ust\\.csv$|pst_ust\\.(csv|txt)$",
    int   = "TANK|PIP|DET_|CORR|MAT|PIPE|WALL|COMP|FRP|STATUS|SUMP|INTERST|SPILL|OVERFILL|CATHOD|CONTAIN|SUBSTANCE|CAPAC|SECONDARY"),
  raw_fr = list(
    label = "RAW financial-responsibility records",
    rx    = "pst_fin_assur\\.(csv|txt)$",
    int   = "ISSUER|COVER|DEDUCT|CATEGORY|DETAIL|_met$|EFF|EXP|MECH|LIMIT"),
  model_tank = list(
    label = "Model tank/facility panels",
    rx    = "(^|/)panel_dt\\.csv$|(^|/)pm_panel\\.csv$",
    int   = "wall|age|capac|cap_|^sw|^dw|vintage|cohort|comp|regime")
)

# ---- helpers ------------------------------------------------------------------
md  <- character(0)
add <- function(...) md <<- c(md, sprintf(...))

relcsv <- function(p)
  sprintf("read_csv_auto('%s', all_varchar=true, ignore_errors=true, header=true)", gsub("\\\\", "/", p))

find_one <- function(rx) {
  hits <- list.files(file.path(root, "Data"), pattern = rx, recursive = TRUE,
                     full.names = TRUE, ignore.case = TRUE)
  hits <- hits[!grepl("\\.(rds|jpg|png|url|docx|pdf|zip|gz)$", hits, ignore.case = TRUE)]
  hits[order(!grepl("\\.csv$", hits, ignore.case = TRUE))]   # prefer parsed .csv over raw .txt (fixed-width)
}

# tabulate one column from a (small) duckdb table/relation, NA included
tab_col <- function(tbl, col) {
  nd <- dbGetQuery(con, sprintf('SELECT COUNT(DISTINCT "%s") k FROM %s', col, tbl))$k
  nd <- ifelse(is.na(nd), 0L, nd)
  if (nd > 60) {
    top <- as.data.table(dbGetQuery(con, sprintf(
      'SELECT "%s" v, COUNT(*) n FROM %s GROUP BY 1 ORDER BY n DESC LIMIT 8', col, tbl)))
    add("- **`%s`** — high-cardinality (%s distinct). Top: %s", col, format(nd, big.mark = ","),
        paste(sprintf("`%s` (%s)", ifelse(is.na(top$v), "<NA>", top$v), format(top$n, big.mark = ",")),
              collapse = ", "))
  } else {
    tt <- as.data.table(dbGetQuery(con, sprintf(
      'SELECT "%s" v, COUNT(*) n FROM %s GROUP BY 1 ORDER BY n DESC LIMIT 30', col, tbl)))
    add("- **`%s`** (%d distinct): %s", col, nd,
        paste(sprintf("`%s`=%s", ifelse(is.na(tt$v), "<NA>", tt$v), format(tt$n, big.mark = ",")),
              collapse = " · "))
  }
}

profile_one <- function(key) {
  spec <- targets[[key]]
  cat(sprintf("[%s] %s ...\n", format(Sys.time(), "%H:%M:%S"), key))
  add("\n---\n## %s  \n_(%s)_", key, spec$label)
  hits <- find_one(spec$rx)
  if (!length(hits)) { add("**NOT FOUND** — searched `Data/` for `%s`.", spec$rx); return(invisible()) }
  path <- hits[1]
  add("Path: `%s`  (%.2f GB)", path, file.info(path)$size / 1e9)
  if (length(hits) > 1) add("Other matches: %s", paste(sprintf("`%s`", hits[-1]), collapse = ", "))

  rel  <- relcsv(path)
  cols <- tryCatch(as.data.table(dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM %s", rel))),
                   error = function(e) { add("_Could not read: %s_", conditionMessage(e)); NULL })
  if (is.null(cols)) return(invisible())

  realcols <- cols[!grepl("^(ISSUER_NAME|CATEGORY|DETAIL_TYPE)_.", column_name), column_name]  # drop one-hots
  add("\n### Columns (%d total; %d after dropping one-hot dummies)", nrow(cols), length(realcols))
  add("```\n%s\n```", paste(realcols, collapse = ", "))

  intcols <- head(realcols[grepl(spec$int, realcols, ignore.case = TRUE)], 40)
  if (!length(intcols)) { add("\n_(no interesting columns matched the profile pattern)_"); return(invisible()) }

  # one scan -> compact temp table, then fast per-column tabulation
  sel <- paste(sprintf('"%s"', intcols), collapse = ", ")
  dbExecute(con, "DROP TABLE IF EXISTS cmp")
  dbExecute(con, sprintf("CREATE TEMP TABLE cmp AS SELECT %s FROM %s", sel, rel))
  nrows <- dbGetQuery(con, "SELECT COUNT(*) n FROM cmp")$n
  add("\n### Key field value distributions (rows = %s; `<NA>` shown)", format(nrows, big.mark = ","))
  for (cc in intcols) tab_col("cmp", cc)
}

# ---- run ----------------------------------------------------------------------
add("# Texas UST data — auto profile")
add("_Generated %s on `%s` by `Code/Cleaning/profile_texas_data.R`. Curated companion:_", format(Sys.time()), root)
add("_`Docs/Texas_Data_Characteristics.md`._")
for (k in names(targets)) profile_one(k)
add("\n---\n_Done in %.1f min._", as.numeric(difftime(Sys.time(), t0, units = "mins")))

dir.create(dirname(out_md), showWarnings = FALSE, recursive = TRUE)
writeLines(md, out_md)
cat(sprintf("\nWrote %s (%.1f min)\n", out_md, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
