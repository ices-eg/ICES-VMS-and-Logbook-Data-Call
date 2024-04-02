#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 1: Pre-processing and cleaning TACSAT and EFLALO data                     ----
#
#'------------------------------------------------------------------------------

#'------------------------------------------------------------------------------
# 1.0 Preparations                                                          ----
#'------------------------------------------------------------------------------
# Load the data underlying VMStools    
# data(euharbours); if(substr(R.Version()$os,1,3)== "lin")
data(harbours)
data(ICESareas)
data(europa)

year <- 2022          ##
yearsToSubmit <- 2022 ##TESTING - DELETE FROM PUBLISHED VERSION

# Looping through the years to submit
#for(year in yearsToSubmit){
  print(paste0("Start loop for year ",year))
  #'----------------------------------------------------------------------------
  # 1.1 load TACSAT and EFLALO data from file                               ----
  #'----------------------------------------------------------------------------
  # The files need to be in tacsat2 and eflalo2 format already)
  # see https://github.com/nielshintzen/vmstools/releases/ -> downloads -> Exchange_EFLALO2_v2-1.doc for an example
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
  
  tacsat <- data.frame(get(tacsat_name)) # rename to tacsat
  eflalo <- data.frame(get(eflalo_name)) # rename to eflalo
  ## Portuguese data has extra quotation marks around the stat rectangle
  eflalo$LE_RECT <- substr(eflalo$LE_RECT, 2, 5)
  # Reshape the Portuguese 2022 data

  eflalo <- as.data.frame(eflalo)
  colnames(eflalo)[11] <- "FT_DTIME"
  colnames(eflalo)[15] <- "FT_LTIME"

  # Replace empty strings with NA in specific columns
  eflalo <- mutate(eflalo, 
                   FT_DDAT = na_if(FT_DDAT, ""), 
                   FT_DTIME = na_if(FT_DTIME, ""), 
                   FT_LDAT = na_if(FT_LDAT, ""), 
                   FT_LTIME = na_if(FT_LTIME, ""))
  
  # Remove rows with NA in specific columns
  eflalo <- eflalo[complete.cases(eflalo$FT_DDAT, eflalo$FT_DTIME, eflalo$FT_LDAT, eflalo$FT_LTIME), ]
  
  # Remove duplicates
  eflalo <- distinct(eflalo)
  
  eflalo <- subset(eflalo, !(FT_DTIME == "00:00" & FT_LTIME == "00:00"))
  eflalo <- subset(eflalo, !(paste(FT_DDAT, FT_DTIME) == paste(FT_LDAT, FT_LTIME)))
  
  eflalo <- as.data.frame(eflalo %>%
    group_by(VE_REF, VE_FLT, VE_COU, VE_LEN, VE_KW, VE_TON, FT_REF, FT_DCOU, FT_DHAR, FT_DDAT, FT_DTIME, 
             FT_LCOU, FT_LHAR, FT_LDAT, FT_LTIME, LE_CDAT, LE_STIME, LE_ETIME, LE_SLAT, LE_SLON, LE_ELAT, 
             LE_ELON, LE_GEAR, LE_MSZ, LE_RECT, LE_DIV, LE_MET, SP_SPX) %>%
    summarise(LE_ID = first(LE_ID),
              LE_KG = sum(LE_KG),
              LE_EURO = sum(LE_EURO)))
  
  # First, create a data frame that summarises the first value for each LE_ID
  eflalo_summary <- eflalo %>%
    group_by(LE_ID) %>%
    summarise(across(everything(), ~ first(.x[!is.na(.x)])))
  eflalo_summary <- as.data.frame(eflalo_summary)
  
  # Next, create a second data frame that aggregates LE_KG and LE_EURO for each LE_ID and SP_SPX
  eflalo_wide <- eflalo %>%
    group_by(LE_ID, SP_SPX) %>%
    summarise(LE_KG = sum(LE_KG, na.rm = TRUE), 
              LE_EURO = sum(LE_EURO, na.rm = TRUE)) %>%
    pivot_wider(names_from = SP_SPX, values_from = c(LE_KG, LE_EURO))
  
  # Finally, join the two data frames together using LE_ID
  eflalo_final <- left_join(eflalo_summary, eflalo_wide, by = "LE_ID")
  
  eflalo <- as.data.frame(eflalo_final)
  eflalo <- eflalo %>% dplyr::select(-SP_SPX, -LE_KG, -LE_EURO)
  
   
  #- Make sure data is in right format
  tacsat <- formatTacsat(tacsat)
  eflalo <- formatEflalo(eflalo)
  
  #'----------------------------------------------------------------------------
  # 1.2 Clean the TACSAT data                                               ----
  #'----------------------------------------------------------------------------
  # 1.2.0 Keep track of removed points -----------------------------------------
  remrecsTacsat <-
    matrix(
      NA,
      nrow = 6, ncol = 2,
      dimnames =
        list(
          c("total", "outsideICESarea", "duplicates", "notPossible", "pseudoDuplicates", "harbour"),
          c("rows", "percentage"))
    )
  
  remrecsTacsat["total", ] <- c(nrow(tacsat), "100%")
  
  # 1.2.1 Remove VMS pings outside the ICES areas ------------------------------
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
    sf::st_transform(4326) |> 
    sf::st_zm()
  
  # Find intersections
  overs <- sf::st_intersects(tacsat, ia)
  
  # See what points fall out the ICES area
  tacsatx <- tacsat[!(lengths(overs) > 0),]
  
  # Filter tacsat
  tacsat <- tacsat[lengths(overs) > 0,]
  
  # Calculate the percentage of remaining records 
  percentage_remaining <- round(nrow(tacsat)/as.numeric(remrecsTacsat["total",1])*100,2)
  
  # Update remrecsTacsat
  remrecsTacsat["outsideICESarea",] <- c(nrow(tacsat), percentage_remaining)
  
  # 1.2.2 Remove duplicate records ---------------------------------------------
  
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
  
  
  # 1.2.3 Remove points that have impossible coordinates -----------------------
  
  # Extract coordinates from tacsat
  coords <- st_coordinates(tacsat)
  
  # Check for impossible positions
  invalid_positions <- which(coords[,2] > 90 | coords[,2] < -90 | coords[,1] > 180 | coords[,1] < -180)
  
  
  if (length(invalid_positions) > 0) {
    # Print the invalid positions
    print(tacsat[invalid_positions,])
    
    # Remove points with impossible positions
    tacsat <- tacsat[-invalid_positions,]
  }
  
  # Calculate the percentage of remaining records
  percentage_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  
  # Update remrecsTacsat
  remrecsTacsat["notPossible",] <- c(nrow(tacsat), percentage_remaining)
  
  # 1.2.4 Remove points which are pseudo duplicates as they have an interval rate < x minutes ------------------
  
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
  
  
  
  # 1.2.5 Remove points in harbour ---------------------------------------------
  
  # Identify points in harbour
      tacsat <- tacsatInHarbour(tacsat, harbours)

  # Save points in harbour
  pih <- tacsat %>% filter(inHarbour == TRUE)
  save(pih, file = file.path(outPath, paste0("pointInHarbour", year, ".RData")))
  
  # Remove points in harbour from tacsat
  tacsat <- tacsat %>% filter(inHarbour == FALSE)
  tacsat <- tacsat[, !(names(tacsat) %in% c("bbox_xmin", "bbox_ymin", "bbox_xmax", "bbox_ymax", "inHarbour"))]

  # Calculate the percentage of remaining records
  percentage_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  
  # Update remrecsTacsat
  remrecsTacsat["harbour",] <- c(nrow(tacsat), percentage_remaining)
  
  #  Save the remrecsTacsat file
  save(
    remrecsTacsat,
    file = file.path(outPath, paste0("remrecsTacsat", year, ".RData"))
  )
    tacsat <- as.data.frame(tacsat)
  tacsat <- tacsat %>% dplyr::select(-geometry)
  tacsat <- tacsat %>% dplyr::select(-unique_id)
#  Save the cleaned tacsat file
 
  save(
    tacsat,
    file = file.path(outPath, paste("cleanTacsat", year, ".RData", sep = ""))
  )
  
  message("Cleaning tacsat completed for year ", year)
  print(remrecsTacsat)
  
  
  #'----------------------------------------------------------------------------
  # 1.3 Clean the EFLALO data --------------------------------------------------
  #'----------------------------------------------------------------------------
  # 1.3.1 Keep track of removed points -----------------------------------------
  remrecsEflalo <-
    matrix(
      NA,
      nrow = 6, ncol = 2,
      dimnames =
        list(
          c("total", "duplicated", "impossible time", "before 1st Jan", "departArrival", "overlappingTrips"),
          c("rows", "percentage"))
    )
  remrecsEflalo["total", ] <- c(nrow(eflalo), "100%")
  
 

  # 1.3.2 Warn for outlying catch records --------------------------------------

  # 
  # Main script - remove change of outliers - should be checked in the logbook instead.
  idxkg <- get_indices("", "KG", eflalo)
  idxeur <- get_indices("", "EURO", eflalo)
  idxoth <- setdiff(1:ncol(eflalo), c(idxkg, idxeur))
  eflalo <- eflalo[, c(idxoth, idxkg, idxeur)]

  specs <- get_species(eflalo)
  specBounds <- get_bounds(specs, eflalo)
  specBounds <- cbind(specs, specBounds)
  specBounds[is.na(specBounds[, 2]), 2] <- "0"

  idx <- unlist(lapply(specs, function(x) get_indices(x, "KG", eflalo)))

  eflalo2 <- replace_outliers(eflalo, specBounds, idx)
  
  if(!identical(eflalo, eflalo2)){
    warning(paste("There are unrealistic landings in the eflalo data, please check f4"))
    f4 <- generics::setdiff(eflalo, eflalo2)
    View(f4)
  }
  
    # 
  # 
  # 1.3.3 Remove non-unique trip numbers --------------------------------------
  
  # Apply the trip ID function to the eflalo data frame
  trip_id <- create_trip_id(eflalo)
  
  # Remove records with non-unique trip identifiers
  eflalo <- eflalo[!duplicated(trip_id), ]
  
  # Calculate the number of remaining records and the percentage of records removed
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  # Update the remrecsEflalo data frame with these values
  remrecsEflalo["duplicated", ] <- c(num_records, 100+ percent_removed)
  
  # 1.3.4 Remove impossible time stamp records ---------------------------------
  
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
  
  # 1.3.5 Remove trip starting before 1st Jan ----------------------------------
  
  # Call the remove before january function with the appropriate arguments
  eflalo <- remove_before_jan(eflalo, year)
  
  # Calculate the number of remaining records and the percentage of records removed
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  # Update the remrecsEflalo data frame with these values
  remrecsEflalo["before 1st Jan", ] <- c(num_records, 100 + percent_removed)
  
  
  # 1.3.6 Remove records with arrival date before departure date  --------------
  
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
  
  # 1.3.7 Remove trip with overlap with another trip ---------------------------
  
  # Order 'eflalo' by 'VE_COU', 'VE_REF', 'FT_DDATIM', and 'FT_LDATIM'
  eflalo <- orderBy(~ VE_COU + VE_REF + FT_DDATIM + FT_LDATIM, data = eflalo)
  
  # If a trip (same depart and return times) has more than one FT_REF, make them all into the same (first) FT_REF. 
  dt1 <- data.table(eflalo)[,.(VE_REF, FT_REF, FT_DDATIM, FT_LDATIM)]
  
  dt1 <- unique(dt1, by = c("VE_REF", "FT_REF"))
  
  setkey(dt1, VE_REF, FT_DDATIM, FT_LDATIM)
  dt2 <- dt1[, ref := .N > 1, by = key(dt1)][ref == T]
  
  dt3 <- dt2[,.(FT_REF_NEW = FT_REF[1]), by = .(VE_REF, FT_DDATIM, FT_LDATIM)]
  
  dt4 <- merge(dt2, dt3)
  
  eflalo2 <- merge(data.table(eflalo), dt4, all.x = T)
  eflalo2[!is.na(FT_REF_NEW), FT_REF := FT_REF_NEW]
  eflalo2[, FT_REF_NEW := NULL]
  
  eflalo <- data.frame(eflalo2)
  
  
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
  
  # Calculate the number of remaining records and the percentage of records removed
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  # Update the remrecsEflalo data frame with these values
  remrecsEflalo["overlappingTrips",] <- c(num_records, (100 + percent_removed))
  
  
  # Save the remrecsEflalo file 
  save(
    remrecsEflalo,
    file = file.path(outPath, paste0("remrecsEflalo", year, ".RData"))
  )
  
  #   Save the cleaned eflalo file 
  save(
    eflalo,
    file = file.path(outPath,paste0("cleanEflalo",year,".RData"))
  )
  message("Cleaning eflalo completed for year ", year)
  print(remrecsEflalo)

  } 

# Housekeeping
rm(harbours, europa, ICESareas, tacsat_name, eflalo_name, tacsat, eflalo, remrecsTacsat, remrecsEflalo,
   ia, overs, tacsatx, coords, invalid_positions, pih,
   trip_id, percent_removed, num_records, idx, dt1, result, overlapping.trips)
rm(list = ls(pattern = "_20"))

#'------------------------------------------------------------------------------
# End of script                                                             
#'------------------------------------------------------------------------------

