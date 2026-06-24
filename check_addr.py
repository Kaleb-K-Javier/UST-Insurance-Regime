import duckdb
src = "Data/Raw/Facilities.csv"
print("== overall ==")
print(duckdb.sql(f"""SELECT count(*) n,
  round(100.0*count(*) FILTER(WHERE Address  IS NOT NULL AND length(trim(Address))>0)/count(*),1)  pct_addr,
  round(100.0*count(*) FILTER(WHERE City     IS NOT NULL AND length(trim(City))>0)/count(*),1)     pct_city,
  round(100.0*count(*) FILTER(WHERE Zip_Code IS NOT NULL AND length(trim(Zip_Code))>0)/count(*),1) pct_zip
  FROM read_csv_auto('{src}', all_varchar=true, ignore_errors=true)"""))
print("== by state ==")
print(duckdb.sql(f"""SELECT State, count(*) n,
  round(100.0*count(*) FILTER(WHERE Address IS NOT NULL AND length(trim(Address))>0)/count(*),1) pct_addr
  FROM read_csv_auto('{src}', all_varchar=true, ignore_errors=true)
  GROUP BY State ORDER BY n DESC""").show(max_rows=60))
