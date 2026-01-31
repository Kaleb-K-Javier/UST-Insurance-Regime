# ============================================================================
# DIAGNOSTIC SCRIPT: Iowa UST Database Explorer
# ============================================================================
# This script will:
# 1. Restore the BAK file
# 2. List all tables
# 3. Show head() and glimpse() for each table
# ============================================================================

library(DBI)
library(odbc)
library(dplyr)

# Configuration
bak_file_path <- "C:\\Users\\kaleb\\Documents\\ust_ins_move_to_github\\Data\\Raw\\State_Databases\\Iowa\\REQUEST_USTLUST_20241030.bak"
db_name <- "IowaUST"
server_name <- "localhost"  # Change if needed

cat("============================================================================\n")
cat("IOWA UST DATABASE DIAGNOSTIC TOOL\n")
cat("============================================================================\n\n")

# ============================================================================
# STEP 1: Get logical file names from BAK file
# ============================================================================
cat("STEP 1: Analyzing BAK file structure...\n")
cat("BAK file:", bak_file_path, "\n\n")

con_master <- dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = server_name,
                        Database = "master",
                        Trusted_Connection = "yes")

# Get the logical file names
file_list <- dbGetQuery(con_master, sprintf("
  RESTORE FILELISTONLY 
  FROM DISK = '%s'
", bak_file_path))

cat("Logical files in BAK:\n")
print(file_list[, c("LogicalName", "Type", "PhysicalName")])
cat("\n")

# ============================================================================
# STEP 2: Restore the database
# ============================================================================
cat("STEP 2: Restoring database...\n")

# Get the logical names for data and log files
data_file <- file_list$LogicalName[file_list$Type == "D"][1]
log_file <- file_list$LogicalName[file_list$Type == "L"][1]

# Create restore command
restore_query <- sprintf("
RESTORE DATABASE %s
FROM DISK = '%s'
WITH REPLACE,
MOVE '%s' TO 'C:\\SQLData\\%s.mdf',
MOVE '%s' TO 'C:\\SQLData\\%s_log.ldf'
", db_name, bak_file_path, data_file, db_name, log_file, db_name)

cat("Executing restore...\n")
tryCatch({
  dbExecute(con_master, restore_query)
  cat("✓ Database restored successfully!\n\n")
}, error = function(e) {
  cat("Note: If database already exists, continuing with existing database...\n")
  cat("Error message:", e$message, "\n\n")
})

dbDisconnect(con_master)

# ============================================================================
# STEP 3: Connect to restored database
# ============================================================================
cat("STEP 3: Connecting to database...\n")

con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = server_name,
                 Database = db_name,
                 Trusted_Connection = "yes")

cat("✓ Connected to", db_name, "\n\n")

# ============================================================================
# STEP 4: List all tables
# ============================================================================
cat("============================================================================\n")
cat("STEP 4: TABLES IN DATABASE\n")
cat("============================================================================\n\n")

tables <- dbGetQuery(con, "
  SELECT TABLE_SCHEMA, TABLE_NAME 
  FROM INFORMATION_SCHEMA.TABLES 
  WHERE TABLE_TYPE = 'BASE TABLE'
  ORDER BY TABLE_SCHEMA, TABLE_NAME
")

cat("Found", nrow(tables), "tables:\n\n")
for(i in 1:nrow(tables)) {
  cat(sprintf("  %d. [%s].[%s]\n", i, tables$TABLE_SCHEMA[i], tables$TABLE_NAME[i]))
}
cat("\n")

# ============================================================================
# STEP 5: Explore each table
# ============================================================================
cat("============================================================================\n")
cat("STEP 5: DETAILED TABLE EXPLORATION\n")
cat("============================================================================\n\n")

for(i in 1:nrow(tables)) {
  schema <- tables$TABLE_SCHEMA[i]
  table <- tables$TABLE_NAME[i]
  full_name <- paste0("[", schema, "].[", table, "]")
  
  cat("\n")
  cat("============================================================================\n")
  cat("TABLE", i, "of", nrow(tables), ":", full_name, "\n")
  cat("============================================================================\n\n")
  
  # Get row count
  tryCatch({
    row_count <- dbGetQuery(con, sprintf("SELECT COUNT(*) as count FROM %s", full_name))
    cat("Row count:", format(row_count$count, big.mark=","), "\n\n")
    
    # Read the table
    df <- dbReadTable(con, SQL(full_name))
    
    # Show structure with glimpse()
    cat("GLIMPSE (structure and first values):\n")
    cat("--------------------------------------\n")
    glimpse(df)
    cat("\n")
    
    # Show head()
    cat("HEAD (first 6 rows):\n")
    cat("--------------------\n")
    print(head(df))
    cat("\n")
    
    # Show column names for easy reference
    cat("COLUMN NAMES:\n")
    cat("-------------\n")
    cat(paste(names(df), collapse = ", "), "\n")
    
  }, error = function(e) {
    cat("Error reading table:", e$message, "\n")
  })
  
  cat("\n")
}

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n")
cat("============================================================================\n")
cat("SUMMARY\n")
cat("============================================================================\n")
cat("Database:", db_name, "\n")
cat("Total tables:", nrow(tables), "\n")
cat("\nTable list:\n")
for(i in 1:nrow(tables)) {
  cat(sprintf("  • [%s].[%s]\n", tables$TABLE_SCHEMA[i], tables$TABLE_NAME[i]))
}

# Close connection
dbDisconnect(con)

cat("\n✓ Diagnostic complete!\n")
cat("============================================================================\n")