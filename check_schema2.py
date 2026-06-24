import duckdb
res = duckdb.sql("""
  SELECT column_name, column_type FROM (
    DESCRIBE SELECT * FROM read_parquet(
      'D:/Dewey/ATTOM/Assessor History/TX_post17/assessor-history_0_0_0.snappy.parquet'
    )
  ) WHERE column_name >= 'H' AND column_name < 'P'
  ORDER BY column_name
""")
res.show(max_rows=100)
