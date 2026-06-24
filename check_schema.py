import duckdb
duckdb.sql("""
  DESCRIBE SELECT * FROM read_parquet(
    'D:/Dewey/ATTOM/Assessor History/TX_post17/assessor-history_0_0_0.snappy.parquet'
  )
""").show(max_rows=200)
