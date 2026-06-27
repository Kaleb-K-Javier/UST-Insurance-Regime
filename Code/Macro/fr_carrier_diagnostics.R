# fr_carrier_diagnostics.R
# ------------------------------------------------------------------------------
# Ticket 036 pre-flight. Runs ON THE SERVER against the 20GB TX FR facility-month
# panel (no streaming over Z). Answers the "add the real carriers to the premium
# term" questions:
#   Q1  which carriers are in the data + each one's share of insured fac-years
#       -> % of the market we can PRICE (Mid-Continent / TOMICS / Great American /
#          AIG-Commerce&Industry / Zurich) vs OTHER (Ironshore/Colony/tail = impute)
#       This is the go/no-go coverage number.
#   Q2  carrier composition by ERA (2006/2014/2019) -> how much era-varying premium
#       variation the carrier dimension adds + the Zurich-decline / TOMICS->Mid-
#       Continent churn.
#   Q3  within-facility carrier change -> does the premium term actually move within
#       a facility over time (descriptive only; selection is endogenous, accepted).
#   Q4  coverage-limit distribution -> fix limit at modal, or carry it as a dim.
#
# Engine: DuckDB streams the CSV from disk (fread OOMs on 20GB). ONE full scan
# materializes a compact 5-col table; every group-by hits that, not the CSV.
#
# RUN (server, from repo root):
#   & "C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe" Code/Macro/fr_carrier_diagnostics.R
#   (use your server Rscript; 4.4.3 path works too. Needs duckdb + data.table:
#    install.packages(c("duckdb","data.table")) if missing.)
#
# R<->SQL note (per CLAUDE.md non-R mapping rule): the DuckDB SQL below is doing
# what data.table would do in-memory if the file fit -- read_csv = fread; the
# CREATE TABLE ... GROUP BY fy = dt[, .(carrier=Mode(issuer)), by=.(facility,year)];
# the window pct = dt[, n/sum(n)]. We push it to SQL only because of the 20GB size.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  if (!requireNamespace("duckdb", quietly = TRUE))
    stop("install.packages('duckdb') first")
  library(duckdb)
  library(data.table)
})

cat("=== FR carrier diagnostics ===\n")
t0 <- Sys.time()

fr_path <- "Data/Processed/texas_fr_facility_month_panel.csv"
stopifnot(file.exists(fr_path))
cat(sprintf("file: %s (%.1f GB)\n", fr_path, file.info(fr_path)$size / 1e9))

con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

# all_varchar=true -> no type-sniff failures on a messy 694-col CSV; we cast only
# the handful of columns we actually need. ignore_errors=true skips broken rows.
rel <- sprintf("read_csv_auto('%s', all_varchar=true, ignore_errors=true, header=true)", fr_path)

# ---- 1. sniff column names so we lock onto the right ones --------------------
cat("\n--- candidate columns (verify the chosen names below) ---\n")
cols <- as.data.table(dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM %s", rel)))
patt <- "issuer|carrier|company|insurer|naic|coverage|amount|occur|aggreg|facility|fac|year|begin|expir|effect|date|prepaid|mechanism|method"
print(cols[grepl(patt, column_name, ignore.case = TRUE), .(column_name)])

# ---- CONFIG: if any auto-pick below is wrong, hard-set it from the list above -
COL <- list(facility = NA_character_, year = NA_character_, issuer = NA_character_,
            occ = NA_character_, agg = NA_character_, begin = NA_character_)

cn   <- cols$column_name
pick <- function(rx) { h <- cn[grepl(rx, cn, ignore.case = TRUE)]; if (length(h)) h[1] else NA_character_ }
if (is.na(COL$facility)) COL$facility <- pick("^facility_?id|fac_?id|facility_?number|facility_?id_?number")
if (is.na(COL$issuer))   COL$issuer   <- pick("issuer_?name|insurer_?name|carrier_?name|company_?name")
if (is.na(COL$occ))      COL$occ      <- pick("occurrence")
if (is.na(COL$agg))      COL$agg      <- pick("aggregate")
if (is.na(COL$year))     COL$year     <- pick("^year$|policy_?year|coverage_?year|report_?year")
if (is.na(COL$begin))    COL$begin    <- pick("begin_?date|effective_?begin|coverage_?effective|effective_?date")

cat("\n--- columns chosen (EDIT CONFIG + rerun if any are wrong) ---\n")
print(unlist(COL))
stopifnot(!is.na(COL$facility), !is.na(COL$issuer))
if (is.na(COL$year) && is.na(COL$begin)) stop("no YEAR or begin-date column auto-found; set COL$year in CONFIG")

# ---- 2. one full scan -> compact facility-month table -----------------------
num <- function(c) if (is.na(c)) "NULL" else
  sprintf("TRY_CAST(regexp_replace(\"%s\", '[^0-9.]', '', 'g') AS DOUBLE)", c)   # strip $ and commas
year_expr <- if (!is.na(COL$year))
  sprintf("TRY_CAST(regexp_replace(\"%s\", '[^0-9]', '', 'g') AS INTEGER)", COL$year) else
  sprintf("TRY_CAST(substr(\"%s\", 1, 4) AS INTEGER)", COL$begin)   # fallback: leading YYYY of a date

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

n_fm <- dbGetQuery(con, "SELECT COUNT(*) n FROM fm")$n
rng  <- dbGetQuery(con, "SELECT MIN(year) lo, MAX(year) hi FROM fm WHERE year IS NOT NULL")
cat(sprintf("  insured facility-months: %s | year range [%s, %s]  <- sanity-check the years\n",
            format(n_fm, big.mark = ","), rng$lo, rng$hi))

# collapse to facility-year: modal carrier + modal limits; era matches PM02_Lookups
dbExecute(con, "
  CREATE TEMP TABLE fy AS
  SELECT facility, year,
         CASE WHEN year <= 2013 THEN '2006' WHEN year <= 2018 THEN '2014' ELSE '2019' END AS era,
         mode(issuer)    AS carrier,
         mode(occ_limit) AS occ_limit,
         mode(agg_limit) AS agg_limit
  FROM fm WHERE year IS NOT NULL
  GROUP BY facility, year")
n_fy <- dbGetQuery(con, "SELECT COUNT(*) n, COUNT(DISTINCT facility) f FROM fy")
cat(sprintf("  insured facility-years: %s  (distinct facilities: %s)\n",
            format(n_fy$n, big.mark = ","), format(n_fy$f, big.mark = ",")))

# carrier-family map (refine these patterns after eyeballing Q1)
fam <- function(x) fcase(
  grepl("MID.?CONT", x), "Mid-Continent",
  grepl("TOMIC|TANK ?OWNER", x), "TOMICS",
  grepl("GREAT ?AMERICAN", x), "Great American",
  grepl("ZURICH", x), "Zurich",
  grepl("COMMERCE|CHARTIS|\\bAIG\\b|AMERICAN INTERNATIONAL|C ?& ?I", x), "AIG/C&I",
  default = "OTHER (impute)")

# ---- Q1. carrier share + priceable coverage ---------------------------------
cat("\n=== Q1. carrier share of insured facility-years (top 40) ===\n")
q1 <- as.data.table(dbGetQuery(con, "
  SELECT carrier, COUNT(*) fac_years,
         ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) pct
  FROM fy GROUP BY carrier ORDER BY fac_years DESC LIMIT 40"))
print(q1)

allc <- as.data.table(dbGetQuery(con, "SELECT carrier, COUNT(*) fac_years FROM fy GROUP BY carrier"))
allc[, family := fam(carrier)]
cov <- allc[, .(fac_years = sum(fac_years)), by = family][, pct := round(100 * fac_years / sum(fac_years), 2)][order(-pct)]
cat("\n--- priceable coverage by carrier family (THE go/no-go number) ---\n")
print(cov)

# ---- Q2. carrier composition by era -----------------------------------------
cat("\n=== Q2. carrier-family facility-year counts by ERA ===\n")
allc_era <- as.data.table(dbGetQuery(con, "
  SELECT era, carrier, COUNT(*) n FROM fy WHERE era IS NOT NULL GROUP BY era, carrier"))
allc_era[, family := fam(carrier)]
print(dcast(allc_era[, .(n = sum(n)), by = .(era, family)], family ~ era, value.var = "n", fill = 0))

# ---- Q3. within-facility carrier movement -----------------------------------
cat("\n=== Q3. within-facility carrier movement ===\n")
ever <- dbGetQuery(con, "
  SELECT COUNT(*) n_fac, SUM(CASE WHEN nc > 1 THEN 1 ELSE 0 END) n_switchers
  FROM (SELECT facility, COUNT(DISTINCT carrier) nc FROM fy GROUP BY facility)")
cat(sprintf("  facilities that ever change carrier: %s of %s (%.1f%%)\n",
            format(ever$n_switchers, big.mark = ","), format(ever$n_fac, big.mark = ","),
            100 * ever$n_switchers / ever$n_fac))
cat("\n--- where Zurich / TOMICS / Tank-Owners books go (year-over-year switches) ---\n")
flows <- dbGetQuery(con, "
  WITH t AS (
    SELECT a.facility, a.carrier AS from_c, b.carrier AS to_c
    FROM fy a JOIN fy b ON a.facility = b.facility AND b.year = a.year + 1
    WHERE a.carrier <> b.carrier)
  SELECT from_c, to_c, COUNT(*) n FROM t
  WHERE from_c LIKE '%ZURICH%' OR from_c LIKE '%TOMIC%' OR from_c LIKE '%TANK OWNER%'
  GROUP BY from_c, to_c ORDER BY n DESC LIMIT 30")
print(flows)

# ---- Q4. coverage-limit distribution ----------------------------------------
cat("\n=== Q4. per-occurrence coverage-limit distribution (top 20) ===\n")
print(dbGetQuery(con, "
  SELECT occ_limit, COUNT(*) n, ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) pct
  FROM fy WHERE occ_limit IS NOT NULL GROUP BY occ_limit ORDER BY n DESC LIMIT 20"))

cat(sprintf("\n=== done in %.1f min ===\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
