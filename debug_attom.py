import duckdb, glob

files = glob.glob(r"D:/Dewey/ATTOM/Assessor History/TX_post17/*.parquet")[:5]
flist = ", ".join("'" + f.replace("\\", "/") + "'" for f in files)
GRID   = "C:/Users/kalebkja/dewey-downloads/_reduced_near_ust/facility_grid.parquet"
DELTA  = 0.0025
STATES = ['TX','AL','AR','CO','LA','ME','NJ','NM','OK','PA','TN',
          'ID','KS','KY','MD','MA','MN','NC','ND','OH','VA','AZ','CT','FL','IA','MI']
state_list = ",".join(repr(s) for s in STATES)

con = duckdb.connect()

print("=== 1. Total rows in first 5 TX files ===")
print(f"  {con.execute(f'SELECT count(*) FROM read_parquet([{flist}])').fetchone()[0]:,}")

print("\n=== 2. SITUSSTATECODE — top 10 values ===")
rows = con.execute(f"SELECT SITUSSTATECODE, count(*) n FROM read_parquet([{flist}]) GROUP BY 1 ORDER BY 2 DESC LIMIT 10").fetchall()
for r in rows: print(f"  {r}")

print("\n=== 3. Lat/lon range ===")
r = con.execute(f"SELECT min(LATITUDE), max(LATITUDE), min(LONGITUDE), max(LONGITUDE) FROM read_parquet([{flist}]) WHERE LATITUDE IS NOT NULL").fetchone()
print(f"  lat {r[0]} to {r[1]}, lon {r[2]} to {r[3]}")

print("\n=== 4. Sample computed grid cells from data ===")
rows = con.execute(f"SELECT (CAST(floor(LATITUDE/{DELTA}) AS BIGINT) || '_' || CAST(floor(LONGITUDE/{DELTA}) AS BIGINT)) AS cell FROM read_parquet([{flist}]) WHERE LATITUDE IS NOT NULL LIMIT 5").fetchall()
for r in rows: print(f"  {r}")

print("\n=== 5. Sample cells from facility grid ===")
rows = con.execute(f"SELECT cell FROM read_parquet('{GRID}') LIMIT 5").fetchall()
for r in rows: print(f"  {r}")

print("\n=== 6. Rows passing STATE filter only ===")
print(f"  {con.execute(f'SELECT count(*) FROM read_parquet([{flist}]) WHERE SITUSSTATECODE IN ({state_list})').fetchone()[0]:,}")

print("\n=== 7. Rows passing GRID filter only (no state filter) ===")
print(f"  {con.execute(f'SELECT count(*) FROM read_parquet([{flist}]) WHERE (CAST(floor(LATITUDE/{DELTA}) AS BIGINT) || chr(95) || CAST(floor(LONGITUDE/{DELTA}) AS BIGINT)) IN (SELECT cell FROM read_parquet(chr(39)||{repr(GRID)}||chr(39)))').fetchone()[0]:,}")
