import duckdb
p = r"C:\Users\kalebkja\dewey-downloads\_reduced_near_ust\attom_tax_assessor_near_ust\part_y2024_tx_ca.parquet"
print(duckdb.sql(f"select count(*) rows, count(distinct ATTOMID) parcels from read_parquet('{p}')"))
print(duckdb.sql(f"select SITUSSTATECODE s, count(*) n from read_parquet('{p}') group by 1 order by 2 desc"))
print(duckdb.sql(f"select count(*) filter(where LATITUDE is not null) lat_ok, round(min(LATITUDE),2) la0, round(max(LATITUDE),2) la1, round(min(LONGITUDE),2) lo0, round(max(LONGITUDE),2) lo1 from read_parquet('{p}')"))
print([c[0] for c in duckdb.sql(f"describe select * from read_parquet('{p}')").fetchall()])
