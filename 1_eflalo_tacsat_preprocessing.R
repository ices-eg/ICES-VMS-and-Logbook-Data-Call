# 1  Load the data  -----------------------------------------------------------

# 1.1 Load vmstools underlying data ===========================================

# data(euharbours); if(substr(R.Version()$os,1,3)== "lin")
data(harbours)
data(ICESareas)
data(europa)

# 2 Clean the TACSAT and EFLALO data  ----------------------------------------------------------------------------------
 
#  Looping through the data years
#for(year in yearsToSubmit){
  print(year)

# 2.1 load tacsat and eflalo data from file (they need to be in tacsat2 and eflalo2 format already ==================
# see https://github.com/nielshintzen/vmstools/releases/ -> downloads ->Exchange_EFLALO2_v2-1.doc for an example
   
  tacsat_name <-
    load(
      file.path(
        dataPath,
        paste0("tacsat_", year, ".RData")
      )); #- data is saved as tacsat_2009, tacsat_2010 etc
  eflalo_name <-
    load(
      file.path(
        dataPath,
        paste0("eflalo_", year, ".RData")
      )); #- data is saved as eflalo_2009, eflalo_2010 etc
  
  tacsat <- get(tacsat_name) # rename to tacsat
  eflalo <- get(eflalo_name) # rename to eflalo
  
  #- Make sure data is in right format
  tacsat <- formatTacsat(tacsat)
  eflalo <- formatEflalo(eflalo)
  
# 2.2 Take only VMS pings in the ICES areas ==============================================
  
  # Transform ICESareas and tacsat to sf objects
  ia <- transform_to_sf(ICESareas, coords = c("SI_LONG", "SI_LATI"))
  
  original_coords <- tacsat[, c("SI_LONG", "SI_LATI")]
  
  # Transform tacsat to an sf object
  tacsat <- transform_to_sf(tacsat, coords = c("SI_LONG", "SI_LATI"))
  
  # Add the original longitude and latitude columns back to the sf object
  tacsat$SI_LONG <- original_coords$SI_LONG
  tacsat$SI_LATI <- original_coords$SI_LATI

  # Make ia valid and transform it
  ia <- ia %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326)
  
  # Find intersections
  overs <- sf::st_intersects(tacsat, ia)
  
  # Filter tacsat
  tacsat <- tacsat[lengths(overs) > 0,]
  
  ## Portuguese data has extra quotation marks around the stat rectangle
  ##  REMOVE THIS LINE FROM THE FINAL VERSION
  eflalo$LE_RECT <- substr(eflalo$LE_RECT, 2, 5)
  
  
  # Remove NA and get unique LE_RECT
  unique_LE_RECT <- na.omit(unique(eflalo$LE_RECT))
  
  # Get coordinates and filter out NA
  coordsEflalo <- ICESrectangle2LonLat(unique_LE_RECT)
  coordsEflalo$LE_RECT <- unique_LE_RECT
  coordsEflalo <- coordsEflalo[complete.cases(coordsEflalo[, c("SI_LONG", "SI_LATI")]),]
  
  # Generate corner points
  cornerPoints <- lapply(1:nrow(coordsEflalo), function(i) {
    cbind(
      SI_LONG = coordsEflalo[i, "SI_LONG"] + c(0, 0.5, 1, 1, 0),
      SI_LATI = coordsEflalo[i, "SI_LATI"] + c(0, 0.25, 0, 0.5, 0.5),
      LE_RECT = coordsEflalo[i, "LE_RECT"]
    )
  })
  
  # Combine corner points and convert to numeric
  coordsEflalo <- do.call(rbind, cornerPoints)
  coordsEflalo <- transform(coordsEflalo, SI_LONG = as.numeric(SI_LONG), SI_LATI = as.numeric(SI_LATI))
  
  # Convert coordsEflalo to an sf object
  coordsEflalo_sf <- st_as_sf(coordsEflalo, coords = c("SI_LONG", "SI_LATI"), crs = st_crs(ICESareas))
  
  # Get index of points within ICESareas
  idxI <- unlist(st_intersects(coordsEflalo_sf, ICESareas))
  
  # Subset eflalo
  eflalo <- subset(eflalo, LE_RECT %in% unique(coordsEflalo[idxI, "LE_RECT"]))
  
#   2.2 Clean the tacsat data  ============================================================================
  
  
# 2.2.1 Keep track of removed points ----------------------------------------------------------------- 
  
  remrecsTacsat <-
    matrix(
      NA,
      nrow = 6, ncol = 2,
      dimnames =
        list(
          c("total", "duplicates", "notPossible", "pseudoDuplicates", "harbour", "land"),
          c("rows", "percentage"))
    )
  remrecsTacsat["total", ] <- c(nrow(tacsat), "100%")
  
# 2.2.2 Remove duplicate records ---------------------------------------------------------------------- 
  
  # Convert SI_DATE and SI_TIME to POSIXct
  tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, tacsat$SI_TIME), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  
  # Create a unique identifier for each row
  tacsat$unique_id <- paste(tacsat$VE_REF, tacsat$SI_LATI, tacsat$SI_LONG, tacsat$SI_DATIM)
  
  # Remove duplicates based on the unique identifier
  tacsat <- tacsat[!duplicated(tacsat$unique_id), ]
  
  # Calculate the percentage of remaining records
  percentage_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  
  # Update remrecsTacsat
  remrecsTacsat["duplicates",] <- c(nrow(tacsat), percentage_remaining)
  
 
# 2.2.3 Remove points that cannot be possible -----------------------------------------------------------
  
  # Extract coordinates from tacsat
  coords <- st_coordinates(tacsat)
  
  # Check for impossible positions
  invalid_positions <- which(coords[,2] > 90 | coords[,2] < -90 | coords[,1] > 180 | coords[,1] < -180)
  
  # Print the invalid positions
  print(tacsat[invalid_positions,])
  
  if (length(invalid_positions) > 0) {
    # Remove points with impossible positions
    tacsat <- tacsat[-invalid_positions,]
  }
  
  # Calculate the percentage of remaining records
  percentage_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  
  # Update remrecsTacsat
  remrecsTacsat["notPossible",] <- c(nrow(tacsat), percentage_remaining)
  
# 2.2.4 Remove points which are pseudo duplicates as they have an interval rate < x minutes ------------------
  
  # Sort tacsat and calculate intervals
  tacsat <- sfsortTacsat(tacsat)
  tacsat$INTV <- intervalTacsat(as.data.frame(tacsat), level = "vessel", fill.na = TRUE)$INTV
  
  # Remove rows with small intervals
  tacsat <- tacsat[tacsat$INTV >= intThres, ]
  
  # Calculate the percentage of remaining records
  percentage_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  
  # Update remrecsTacsat
  remrecsTacsat["pseudoDuplicates",] <- c(nrow(tacsat), percentage_remaining)
  
  # Remove INTV column from tacsat
  tacsat$INTV <- NULL
  
  
  
# 2.2.5 Remove points in harbour -----------------------------------------------------------------------------
  
  # Identify points in harbour
  tacsat$PIH <- pointInHarbour(tacsat$SI_LONG, tacsat$SI_LATI, harbours)
  
  # Save points in harbour
  pih <- tacsat %>% filter(PIH == 1)
  save(pih, file = file.path(outPath, paste0("pointInHarbour", year, ".RData")))
  
  # Remove points in harbour from tacsat
  #tacsat <-
    
    tacsat <- tacsat %>% filter(PIH == 0)
    tacsat <- tacsat[, !(names(tacsat) %in% c("bbox_xmin", "bbox_ymin", "bbox_xmax", "bbox_ymax", "PIH"))]
  
  # Calculate the percentage of remaining records
  percentage_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  
  # Update remrecsTacsat
  remrecsTacsat["harbour",] <- c(nrow(tacsat), percentage_remaining)
  
 
# 2.2.6 Remove points on land -----------------------------------------------------------------------------
  
  # Define the columns to be used for coordinates
  coord_cols <- c("SI_LONG", "SI_LATI")
  
  # Extract coordinates from tacsat data frame
  sp_pts <- tacsat[, coord_cols]
  
  # Convert to simple features object and set CRS
  sp_pts <- st_as_sf(sp_pts, coords = coord_cols, crs = 4326)
  
  # Check which points overlap with europa
  sp_pt_over <- st_over(sp_pts, europa)
  
  # Subset tacsat for points that overlap with europa
  pol <- tacsat[!is.na(sp_pt_over), ]
  
  # Save pol object to .RData file
  save(pol, file = file.path(outPath, paste0("pointOnLand", year, ".RData")))
  
  # Subset tacsat for points that do not overlap with europa
  tacsat <- tacsat[is.na(sp_pt_over), ]
  
  # Update remrecsTacsat with the number of remaining records and the percentage removed
  remrecsTacsat["land", ] <- c(
    nrow(tacsat),
    100 + round((nrow(tacsat) - as.numeric(remrecsTacsat["total", 1])) / as.numeric(remrecsTacsat["total", 1]) * 100, 2)
  )
  
 
#  Save the remrecsTacsat file
 
  save(
    remrecsTacsat,
    file = file.path(outPath, paste0("remrecsTacsat", year, ".RData"))
  )
  
#  Save the cleaned tacsat file
 
  save(
    tacsat,
    file = file.path(outPath, paste("cleanTacsat", year, ".RData", sep = ""))
  )
  
  message("Cleaning tacsat completed")

  
# 2.3 Clean the eflalo data  ============================================================================
  
 
# 2.3.1 Keep track of removed points -------------------------------------------------------------------- 
  
  remrecsEflalo <-
    matrix(
      NA,
      nrow = 5, ncol = 2,
      dimnames =
        list(
          c("total", "duplicated", "impossible time", "before 1st Jan", "departArrival"),
          c("rows", "percentage"))
    )
  remrecsEflalo["total", ] <- c(nrow(eflalo), "100%")
  
 
# 2.3.2 Warn for outlying catch records ----------------------------------------------------------
  
  
  # Define a function to get the indices of columns that match a pattern
  get_indices <- function(pattern, col_type, data) {
    grep(paste0("LE_", col_type, "_", pattern), colnames(data))
  }
  
  # Define a function to get the species names
  get_species <- function(data) {
    substr(grep("KG", colnames(data), value = TRUE), 7, 9)
  }
  
  # Define a function to get the bounds for each species
  get_bounds <- function(specs, data) {
    sapply(specs, function(x) {
      idx <- get_indices(x, "KG", data)  # specify "KG" as the col_type
      if (length(idx) > 0) {
        wgh <- sort(unique(unlist(data[data[, idx] > 0, idx])))
        # Exclude 0 values before applying log10
        wgh <- wgh[wgh > 0]
        if (length(wgh) > 0) {
          log_wgh <- log10(wgh)
          difw <- diff(log_wgh)
          if (any(difw > lanThres)) {
            # Return the next value in wgh after the last value that had a difference less than or equal to lanThres
            wgh[max(which(difw <= lanThres)) + 1]
          } else {
            # If no outliers, return the maximum value in wgh
            max(wgh, na.rm = TRUE)
          }
        } else {
          0
        }
      } else {
        0
      }
    })
  }
  
  
  # Define a function to replace outliers with NA
  replace_outliers <- function(data, specBounds, idx) {
    for (iSpec in idx) {
      outlier_idx <- which(data[, iSpec] > as.numeric(specBounds[(iSpec - idx[1] + 1), 2]))
      if (length(outlier_idx) > 0) {
        data[outlier_idx, iSpec] <- NA
      }
    }
    data
  }
  
  # Main script
  idxkg <- get_indices("", "KG", eflalo)
  idxeur <- get_indices("", "EURO", eflalo)
  idxoth <- setdiff(1:ncol(eflalo), c(idxkg, idxeur))
  eflalo <- eflalo[, c(idxoth, idxkg, idxeur)]
  
  specs <- get_species(eflalo)
  specBounds <- get_bounds(specs, eflalo)
  specBounds <- cbind(specs, specBounds)
  specBounds[is.na(specBounds[, 2]), 2] <- "0"
  
  idx <- unlist(lapply(specs, function(x) get_indices(x, "KG", eflalo)))
  
  eflalo <- replace_outliers(eflalo, specBounds, idx)
  
  
# 2.3.3  Remove non-unique trip numbers -----------------------------------------------------------------------------
 
  # Apply the trip ID function to the eflalo data frame
  trip_id <- create_trip_id(eflalo)
  
  # Remove records with non-unique trip identifiers
  eflalo <- eflalo[!duplicated(trip_id), ]
  
  # Calculate the number of remaining records and the percentage of records removed
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  # Update the remrecsEflalo data frame with these values
  remrecsEflalo["duplicated", ] <- c(num_records, 100+ percent_removed)
  
# 2.3.4 Remove impossible time stamp records ----------------------------------------------------------------------------
  
  # Apply the convert to date-time function to the FT_DDAT and FT_DTIME columns
  eflalo$FT_DDATIM <- convert_to_datetime(eflalo$FT_DDAT, eflalo$FT_DTIME)
  # Apply the function to the FT_LDAT and FT_LTIME columns
  eflalo$FT_LDATIM <- convert_to_datetime(eflalo$FT_LDAT, eflalo$FT_LTIME)
  
  # Remove records with NA in either FT_DDATIM or FT_LDATIM
  eflalo <- eflalo[!is.na(eflalo$FT_DDATIM) & !is.na(eflalo$FT_LDATIM), ]
  
  # Calculate the number of remaining records and the percentage of records removed
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  # Update the remrecsEflalo data frame with these values
  remrecsEflalo["impossible time", ] <- c(num_records, 100 + percent_removed)
  
# 2.3.5 Remove trip starting before 1st Jan ------------------------------------------------------------------------------

  # Call the remove before january function with the appropriate arguments
  eflalo <- remove_before_jan(eflalo, year)
  
  # Calculate the number of remaining records and the percentage of records removed
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  # Update the remrecsEflalo data frame with these values
  remrecsEflalo["before 1st Jan", ] <- c(num_records, 100 + percent_removed)
  

# 2.3.6 Remove records with arrival date before departure date  ------------------------------------------------------------
 
   # Find the indices of rows where 'FT_LDATIM' is greater than or equal to 'FT_DDATIM'
  idx <- which(eflalo$FT_LDATIM >= eflalo$FT_DDATIM)
  
  # Keep only the rows in 'eflalo' where 'FT_LDATIM' is greater than or equal to 'FT_DDATIM'
  eflalo <- eflalo[idx,]
  
  # Calculate the number of rows and the percentage change in the number of rows
  # Store these values in the 'departArrival' row of 'remrecsEflalo'
  remrecsEflalo["departArrival", ] <- c(
    nrow(eflalo), # Number of rows in the updated 'eflalo'
    100 + round(
      (nrow(eflalo) - as.numeric(remrecsEflalo["total", 1])) / # Change in number of rows
        as.numeric(remrecsEflalo["total", 1]) * 100, # Relative to the original number of rows
      2) # Rounded to 2 decimal places
  )
  
# 2.3.7 Remove trip with overlap with another trip --------------------------------------------------------------------------- 
  
  # Order 'eflalo' by 'VE_COU', 'VE_REF', 'FT_DDATIM', and 'FT_LDATIM'
  eflalo <- orderBy(~ VE_COU + VE_REF + FT_DDATIM + FT_LDATIM, data = eflalo)
  
  # Create a data table 'dt1' with the necessary columns from 'eflalo'
  dt1 <- data.table(ID = eflalo$VE_REF, FT = eflalo$FT_REF,
                    startdate = eflalo$FT_DDATIM,
                    enddate = eflalo$FT_LDATIM)
  
  # Remove duplicate rows from 'dt1'
  dt1 <- dt1[!duplicated(paste(dt1$ID, dt1$FT)), ]
  
  # Set keys for 'dt1' for efficient joining and overlapping
  setkey(dt1, ID, startdate, enddate)
  
  # Find overlapping trips in 'dt1'
  result <- foverlaps(dt1, dt1, by.x = c("ID", "startdate", "enddate"),
                      by.y = c("ID", "startdate", "enddate"))
  
  # Filter 'result' to get only the rows where trips overlap
  overlapping.trips <- subset(result, startdate < i.enddate & enddate > i.startdate & FT != i.FT)
  
  # If there are overlapping trips, remove them from 'eflalo' and save them to a file
  if (nrow(overlapping.trips) > 0) {
    eflalo <- eflalo[!eflalo$FT_REF %in% overlapping.trips$FT, ]
    
    print("THERE ARE OVERLAPPING TRIPS IN THE DATASET -> SEE THE FILE overlappingTrips SAVED IN THE RESULTS FOLDER")
    
    save(overlapping.trips, file = file.path(outPath, paste0("overlappingTrips", year, ".RData")))
  } 
 
 
#   Save the remrecsEflalo file 
  
  save(
    remrecsEflalo,
    file = file.path(outPath, paste0("remrecsEflalo", year, ".RData"))
  )
  
#   Save the cleaned eflalo file 
  
  save(
    eflalo,
    file = file.path(outPath,paste0("cleanEflalo",year,".RData"))
  )
  
  message("Cleaning eflalo completed")
} 
