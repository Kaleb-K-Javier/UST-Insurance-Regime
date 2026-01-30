###############################################################################
# 01_pull_ca_cers_ust.R
# Purpose: Download CA CERS Permitted UST Facilities via ArcGIS REST API.
#          Adapted from EPA UST Finder logic.
# Source:  California State Geoportal (Item ID: 52f1ca9a13d848daaee8f6232ea39aed)
###############################################################################

library(httr)
library(jsonlite)
library(data.table)
library(here)
library(sf) # Required for CERS coordinate conversion

# 1. Setup --------------------------------------------------------------------
# CERS UST Facilities (Active & Closed)
ITEM_ID     <- "52f1ca9a13d848daaee8f6232ea39aed" 
CHUNK_SIZE  <- 1000  

# Paths - CORRECTED to match your folder structure
raw_dir  <- here("Data", "Raw", "state_databases", "California")

# Create directory if it doesn't exist
if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)
# 2. Helper Functions ---------------------------------------------------------

get_service_url <- function(item_id) {
  message("Locating Service URL...")
  url <- paste0("https://www.arcgis.com/sharing/rest/content/items/", item_id, "?f=json")
  resp <- tryCatch(GET(url), error = function(e) stop("Connection failed"))
  
  if (status_code(resp) != 200) stop("Failed to query ArcGIS Online.")
  meta <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  
  if (is.null(meta$url)) stop("No Service URL found.")
  message(sprintf("Found Service: %s", meta$url))
  return(meta$url)
}

download_layer_sequential <- function(service_url, layer_id, layer_name) {
  safe_name <- gsub("[^A-Za-z0-9]", "_", layer_name)
  file_path <- file.path(raw_dir, paste0(safe_name, ".csv"))
  
  message(sprintf("\nProcessing '%s' (Layer ID: %s)...", layer_name, layer_id))
  
  # A. Get Total Count
  count_url <- paste0(service_url, "/", layer_id, "/query?where=1=1&returnCountOnly=true&f=json")
  count_res <- fromJSON(content(GET(count_url), "text", encoding = "UTF-8"))
  total_records <- count_res$count
  
  if (is.null(total_records) || total_records == 0) {
    message("  - Skipped (0 records).")
    return(NULL)
  }
  
  # B. Check for Resume
  offset <- 0
  if (file.exists(file_path)) {
    existing_rows <- tryCatch({
      if (file.size(file_path) < 100) 0 else nrow(fread(file_path, select = 1L))
    }, error = function(e) 0)
    
    if (existing_rows >= total_records) {
      message(sprintf("  ✓ Already complete (%s records). Skipping.", format(existing_rows, big.mark=",")))
      return(NULL)
    }
    offset <- existing_rows
    message(sprintf("  - Resuming from record %s / %s", format(offset, big.mark=","), format(total_records, big.mark=",")))
  } else {
    message(sprintf("  - Starting download (%s records)", format(total_records, big.mark=",")))
  }
  
  # C. Download Loop
  while (offset < total_records) {
    cat(sprintf("\r    Fetching %s / %s (%.1f%%)", 
                format(offset, big.mark=","), 
                format(total_records, big.mark=","), 
                100 * offset / total_records))
    
    success <- FALSE
    attempt <- 1
    
    while(attempt <= 5 && !success) {
      tryCatch({
        query_url <- paste0(service_url, "/", layer_id, "/query")
        res <- GET(query_url, query = list(
          where = "1=1", 
          outFields = "*", 
          f = "json",
          resultOffset = offset, 
          resultRecordCount = CHUNK_SIZE
        ), timeout(60))
        
        if (status_code(res) == 200) {
          data_chunk <- fromJSON(content(res, "text", encoding = "UTF-8"))
          
          if (!is.null(data_chunk$features$attributes)) {
            dt <- as.data.table(data_chunk$features$attributes)
            
            # Geometry Handling (CERS specific)
            if (!is.null(data_chunk$features$geometry)) {
              geom <- data_chunk$features$geometry
              # CERS often returns 'x' and 'y' (Web Mercator)
              if ("x" %in% names(geom)) dt$Shape_X <- geom$x
              if ("y" %in% names(geom)) dt$Shape_Y <- geom$y
            }
            
            # WRITE IMMEDIATELY
            write_mode <- if (offset == 0 && !file.exists(file_path)) "w" else "a"
            append_flag <- if (offset == 0 && !file.exists(file_path)) FALSE else TRUE
            
            fwrite(dt, file_path, append = append_flag)
            success <- TRUE
          } else {
            success <- TRUE # End of data
          }
        }
      }, error = function(e) {
        Sys.sleep(2) 
      })
      attempt <- attempt + 1
    }
    
    if (!success) stop("Failed to fetch chunk. Check internet.")
    offset <- offset + CHUNK_SIZE
  }
  
  cat("\n  ✓ Finished.\n")
}

# 3. Execution ----------------------------------------------------------------
tryCatch({
  # Get Service URL dynamically using CERS Item ID
  service_url <- get_service_url(ITEM_ID)
  
  # Pull Layer Metadata
  layers_json <- fromJSON(content(GET(paste0(service_url, "?f=json")), "text", encoding = "UTF-8"))
  all_items <- rbindlist(list(layers_json$layers, layers_json$tables), fill = TRUE)
  
  # Download Loop
  if (nrow(all_items) > 0) {
    for (i in 1:nrow(all_items)) {
      name <- all_items$name[i]
      id   <- all_items$id[i]
      
      # Target everything, but especially "Facilities" (usually Layer 0)
      # CERS layer naming varies, so we pull all available layers in this service.
      download_layer_sequential(service_url, id, name)
    }
  }
  message("\nAll downloads complete.")
}, error = function(e) {
  message("\nCRITICAL ERROR: ", e$message)
})

# =============================================================================
# 4. POST-PROCESS: Fix Coordinate Reference System (EPSG:3857 -> EPSG:4326)
# =============================================================================
message("\nStep 4: Converting Coordinates (Web Mercator -> Lat/Long)...")

# CERS usually outputs "Shape_X" and "Shape_Y" or "x" and "y"
files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)

for (f in files) {
  dt_fac <- fread(f)
  
  # Detect if we have Web Mercator coordinates (Large numbers)
  # Modify column names below if CERS returns 'x'/'y' or 'POINT_X'/'POINT_Y'
  if (exists("Shape_X", dt_fac) && exists("Shape_Y", dt_fac)) {
     if (any(abs(dt_fac$Shape_Y) > 90, na.rm = TRUE)) {
      message(sprintf("  - Converting %s...", basename(f)))
      
      valid_indices <- which(!is.na(dt_fac$Shape_Y) & !is.na(dt_fac$Shape_X))
      
      if (length(valid_indices) > 0) {
        dt_sf <- st_as_sf(dt_fac[valid_indices], 
                          coords = c("Shape_X", "Shape_Y"), 
                          crs = 3857) # Web Mercator
        
        dt_sf_geo <- st_transform(dt_sf, crs = 4326) # WGS84
        
        coords <- st_coordinates(dt_sf_geo)
        
        dt_fac[valid_indices, Longitude := coords[, 1]]
        dt_fac[valid_indices, Latitude  := coords[, 2]]
        
        fwrite(dt_fac, f)
      }
    }
  }
}
message("  ✓ Coordinate conversion complete.")