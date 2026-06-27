# fr_carrier_diagnostics.R
# ------------------------------------------------------------------------------
# Ticket 036 pre-flight. Runs ON THE SERVER against the 20GB TX FR facility-month
# panel (no streaming over Z). EVERYTHING IS BY YEAR -- the FR data densifies over
# time (near-empty pre-2006, ramps after ~2007), so a pooled number is misleading.
#
#   A  by YEAR: total fac-years, INSURED (real carrier), insured%, PRICEABLE among
#      insured (Mid-Continent / TOMICS / Great American / Zurich / ACE / AIG), and
#      priceable% of insured  <- the go/no-go, as a time series
#   B  carrier-family insured fac-years BY YEAR (Zurich early -> TOMICS mid ->
#      Mid-Continent late churn)
#   C  carrier catalog (insured, pooled) to refine the family patterns
#   D  where Zurich/TOMICS books go (year-over-year switches)
#   E  coverage-limit distribution
#
# ISSUER_NAME is the FR *mechanism*, not carrier-among-insured: most facility-years
# are NO COVERAGE / SELF / PARENT (state-fund era / self-insured / out of
# compliance) and are NOT premium-bearing. "Insured" = the value names a real
# carrier (contains INS / ASSURANCE / UNDERWRITER).
#
# Engine: DuckDB streams the CSV (fread OOMs). ONE full scan -> compact table;
# every group-by hits that. ~0.5 min on 22GB.
#
# RUN (server, repo root):
#   $rs = (Get-ChildItem "C:\Program Files\R\*\bin\x64\Rscript.exe" |
#          Sort-Object FullName -Descending | Select-Object -First 1).FullName
#   & $rs Code/Macro/fr_carrier_diagnostics.R
#   (needs duckdb + data.table)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  if (!requireNamespace("duckdb", quietly = TRUE)) stop("install.packages('duckdb') first")
  library(duckdb)
  library(data.table)
})

cat("=== FR carrier diagnostics (by year) ===\n")
t0 <- Sys.time()

fr_path <- "Data/Processed/texas_fr_facility_month_panel.csv"
stopifnot(file.exists(fr_path))
cat(sprintf("file: %s (%.1f GB)\n", fr_path, file.info(fr_path)$size / 1e9))

YEAR_LO <- 2000L   # show the full ramp; model window is 2006-2020 (widen/narrow freely)
YEAR_HI <- 2022L

con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

# all_varchar=true -> no type-sniff failures on a messy 694-col CSV; cast only what we need.
rel <- sprintf("read_csv_auto('%s', all_varchar=true, ignore_errors=true, header=true)", fr_path)

# ---- 1. sniff substantive columns (exclude the ~600 ISSUER_NAME_* dummies) ---
cat("\n--- substantive columns (find the coverage-limit fields here) ---\n")
cols <- as.data.table(dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM %s", rel)))
print(cols[!grepl("^ISSUER_NAME_.", column_name, ignore.case = TRUE), column_name])

# ---- CONFIG: hard-set any column if the auto-pick below is wrong -------------
COL <- list(facility = NA_character_, year = NA_character_, issuer = NA_character_,
            occ = "max_COVER_OCC", agg = "max_COVER_AGG", begin = NA_character_)  # found in sec.1
cn   <- cols$column_name
pick <- function(rx) { h <- cn[grepl(rx, cn, ignore.case = TRUE)]; if (length(h)) h[1] else NA_character_ }
if (is.na(COL$facility)) COL$facility <- pick("^facility_?id|fac_?id|facility_?number")
if (is.na(COL$issuer))   COL$issuer   <- pick("issuer_?name|insurer_?name|carrier_?name|company_?name")
if (is.na(COL$occ))      COL$occ      <- pick("occurrence|per_?occ|occ.*amount|amount.*occ|cov.*occur")
if (is.na(COL$agg))      COL$agg      <- pick("aggregate|per_?agg|agg.*amount|amount.*agg|cov.*aggreg")
if (is.na(COL$year))     COL$year     <- pick("^year$|policy_?year|coverage_?year|report_?year")
if (is.na(COL$begin))    COL$begin    <- pick("begin_?date|effective_?begin|coverage_?effective|effective_?date")

cat("\n--- columns chosen (EDIT CONFIG + rerun if any are wrong) ---\n")
print(unlist(COL))
stopifnot(!is.na(COL$facility), !is.na(COL$issuer))
if (is.na(COL$year) && is.na(COL$begin)) stop("no YEAR or begin-date column auto-found; set COL$year in CONFIG")

# ---- 2. one full scan -> compact facility-month table -----------------------
num <- function(c) if (is.na(c)) "NULL" else   # keep e/E/sign so sci-notation "1e+06" -> 1000000
  sprintf("TRY_CAST(regexp_replace(\"%s\", '[^0-9.eE+-]', '', 'g') AS DOUBLE)", c)
year_expr <- if (!is.na(COL$year))
  sprintf("TRY_CAST(regexp_replace(\"%s\", '[^0-9]', '', 'g') AS INTEGER)", COL$year) else
  sprintf("TRY_CAST(substr(\"%s\", 1, 4) AS INTEGER)", COL$begin)

cat("\n--- scanning CSV -> compact table (the only full pass) ---\n")
dbExecute(con, sprintf("
  CREATE TEMP TABLE fm AS
  SELECT CAST(\"%s\" AS VARCHAR)               AS facility,
         %s                                     AS year,
         UPPER(TRIM(CAST(\"%s\" AS VARCHAR)))   AS issuer,
         %s                                     AS occ_limit,
         %s                                     AS agg_limit
  FROM %s
  WHERE \"%s\" IS NOT NULL AND TRIM(\"%s\") <> ''",
  COL$facility, year_expr, COL$issuer, num(COL$occ), num(COL$agg), rel, COL$issuer, COL$issuer))
rng <- dbGetQuery(con, "SELECT MIN(year) lo, MAX(year) hi, COUNT(*) n FROM fm WHERE year IS NOT NULL")
cat(sprintf("  facility-months: %s | raw year range [%s, %s] (1901 etc = sentinel, ignored by window)\n",
            format(rng$n, big.mark = ","), rng$lo, rng$hi))

# collapse to facility-year: modal carrier + modal limits
dbExecute(con, "
  CREATE TEMP TABLE fy AS
  SELECT facility, year, mode(issuer) AS carrier,
         mode(occ_limit) AS occ_limit, mode(agg_limit) AS agg_limit
  FROM fm WHERE year IS NOT NULL GROUP BY facility, year")

# carrier-family map. ACE has its own Iowa rate manuals (like Zurich) -> priceable.
# AIG family appears as CHARTIS / AIG / COMMERCE & INDUSTRY / AMERICAN INTL (abbrev).
fam <- function(x) fcase(
  grepl("MID.?CONT", x), "Mid-Continent",
  grepl("TOMIC|TANK ?OWNER", x), "TOMICS",
  grepl("GREAT ?AMERICAN", x), "Great American",
  grepl("ZURICH", x), "Zurich",
  grepl("\\bACE\\b|ILLINOIS UNION", x), "ACE (Iowa proxy)",
  grepl("COMMERCE|CHARTIS|\\bAIG\\b|AMERICAN INTL|AMERICAN INTERNATIONAL|C ?& ?I", x), "AIG/C&I",
  default = "OTHER (impute)")
INS_RX     <- "INS|ASSURANCE|UNDERWRITER"                                   # R-side insured test
INS_FILTER <- "(carrier LIKE '%INS%' OR carrier LIKE '%ASSURANCE%' OR carrier LIKE '%UNDERWRITER%')"  # SQL

# pull year x carrier counts once; classify in R
yc <- as.data.table(dbGetQuery(con, sprintf(
  "SELECT year, carrier, COUNT(*) n FROM fy WHERE year BETWEEN %d AND %d GROUP BY year, carrier",
  YEAR_LO, YEAR_HI)))
yc[, insured   := grepl(INS_RX, carrier)]
yc[, family    := fam(carrier)]
yc[, priceable := insured & family != "OTHER (impute)"]

# ---- A. coverage + priceability BY YEAR (the headline) ----------------------
cat("\n=== A. BY YEAR: insured coverage + priceable share (data improves over time) ===\n")
byyr <- yc[, .(total     = sum(n),
               insured   = sum(n[insured]),
               priceable = sum(n[priceable])), by = year][order(year)]
byyr[, `ins_%`       := round(100 * insured   / total, 1)]
byyr[, `price_%_ins` := round(100 * priceable / pmax(insured, 1L), 1)]
print(byyr)

# ---- B. carrier-family insured fac-years BY YEAR ----------------------------
cat("\n=== B. carrier-family INSURED fac-years BY YEAR ===\n")
famyr <- yc[insured == TRUE, .(n = sum(n)), by = .(year, family)]
print(dcast(famyr, year ~ family, value.var = "n", fill = 0))

# ---- C. carrier catalog (insured, pooled) -> refine fam() patterns ----------
cat("\n=== C. top carriers (insured, pooled over window) ===\n")
catl <- yc[insured == TRUE, .(fac_years = sum(n)), by = carrier][order(-fac_years)]
catl[, pct := round(100 * fac_years / sum(fac_years), 2)]
print(head(catl, 40))

# ---- D. where Zurich / TOMICS / Tank-Owners books go ------------------------
cat("\n=== D. carrier switches out of Zurich / TOMICS (year-over-year, insured) ===\n")
dbExecute(con, sprintf("CREATE TEMP TABLE ins AS
  SELECT * FROM fy WHERE %s AND year BETWEEN %d AND %d", INS_FILTER, YEAR_LO, YEAR_HI))
flows <- dbGetQuery(con, "
  WITH t AS (
    SELECT a.carrier AS from_c, b.carrier AS to_c
    FROM ins a JOIN ins b ON a.facility = b.facility AND b.year = a.year + 1
    WHERE a.carrier <> b.carrier)
  SELECT from_c, to_c, COUNT(*) n FROM t
  WHERE from_c LIKE '%ZURICH%' OR from_c LIKE '%TOMIC%' OR from_c LIKE '%TANK OWNER%'
  GROUP BY from_c, to_c ORDER BY n DESC LIMIT 30")
print(flows)

# ---- E. coverage-limit distribution + modal limit BY YEAR -------------------
cat("\n=== E. per-occurrence coverage-limit: top values (pooled, insured) ===\n")
print(dbGetQuery(con, "
  SELECT occ_limit, COUNT(*) n, ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) pct
  FROM ins WHERE occ_limit IS NOT NULL GROUP BY occ_limit ORDER BY n DESC LIMIT 15"))
cat("\n--- modal per-occurrence limit BY YEAR (does it drift up over time?) ---\n")
print(dbGetQuery(con, "
  SELECT year, mode(occ_limit) AS modal_occ, COUNT(*) n_with_limit
  FROM ins WHERE occ_limit IS NOT NULL GROUP BY year ORDER BY year"))

cat(sprintf("\n=== done in %.1f min ===\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
