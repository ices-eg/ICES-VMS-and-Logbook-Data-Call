#-------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
#
# By: Niels Hintzen, Katell Hamon, Marcel Machiels#
# Code by: Niels Hintzen
# Contact: niels.hintzen@wur.nl
#
# Date: 25-Jan-2017
# Update Date: 29-Jan-2019 ; Updated by: Roi Martinez
# Update Date: 04-Feb-2020 ; Updated by: Colin Millar
# Update Date: 07-Feb 2020 ; Updated by: Neil Campbell
# Client: ICES
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is a proposed workflow example to processes the ICES
# VMS datacall request. It is not an exact template to be applied to data from
# every member state and needs to be adjusted according to the data availability
# and needs of every member state.
#-------------------------------------------------------------------------------


#- Clear workspace
rm(list=ls())

library(vmstools) #- download from www.vmstools.org
library(Matrix)   #- available on CRAN
library(ggplot2)  #- available on CRAN
library(dplyr)    #- available on CRAN
library(doBy)
library(mixtools)
library(tidyr)
library(glue)
library(gt)
library(raster)
library(sf)
library(data.table)
library(progressr)
library(units)
library(geosphere)
library(purrr) 


#- Settings paths
codePath  <- ""          #Location where you store R scripts
dataPath  <- "Data"       #Location where you store tacsat (VMS) and eflalo (logbook) data
outPath   <- "Results"    #Location where you want to store the results

#- Setting specific thresholds
spThres       <- 20   #Maximum speed threshold in analyses in nm
intThres      <- 5    #Minimum difference in time interval in minutes to prevent pseudo duplicates
intvThres     <- 240  #Maximum difference in time interval in minutes to prevent intervals being too large to be realistic
lanThres      <- 1.5  #Maximum difference in log10-transformed sorted weights

#- Re-run all years as we have new field for no. vessels
yearsToSubmit <- c(2018,2022)

#- Set the gear names for which automatic fishing activity is wanted
#  It is important to fill out the gears you want to apply auto detection for
autoDetectionGears        <- c("TBB","OTB","OTT","SSC","SDN","DRB","PTB","HMD", "MIS")

#- Decide if you want to visualy analyse speed-histograms to identify fishing activity
#  peaks or have prior knowledge and use the template provided around lines 380 below
visualInspection          <- FALSE

#- Specify how landings should be distributed over the VMS pings: By day, ICES rectangle, trip basis or otherwise
linkEflaloTacsat          <- c("day","ICESrectangle","trip")
# other options
# linkEflaloTacsat          <- c("day","ICESrectangle","trip")
# linkEflaloTacsat          <- c("ICESrectangle","trip")
# linkEflaloTacsat          <- c("day","trip")
# linkEflaloTacsat          <- c("trip")

# Define a function to transform spatial data
transform_to_sf <- function(data, coords, crs = 4326) {
  data %>%
    sf::st_as_sf(coords = coords) %>%
    sf::st_set_crs(crs)
}



# Define a function to calculate species bounds
get_spec_bounds <- function(specs, eflalo, lanThres) {
  lapply(
    as.list(specs),
    function(x) {
      specs_cols <- grep(x, colnames(eflalo))
      idx <- specs_cols[grep("KG", colnames(eflalo)[specs_cols])]
      wgh <- sort(unique(eflalo[which(eflalo[, idx] > 0), idx]))
      difw <- diff(log10(wgh))
      ifelse(
        any(difw > lanThres),
        wgh[rev(which(difw <= lanThres)[1] + 1)],  # Return only the first value
        ifelse(
          length(wgh) == 0,
          0,
          max(wgh, na.rm = TRUE)
        )
      )
    }
  )
}





# Define a function to get the index (column number) of each of the species
get_species_indices <- function(specs, eflalo) {
  sapply(specs, function(spec) {
    # Find the column indices that contain the current species name and "KG"
    grep(spec, colnames(eflalo)[grep("KG", colnames(eflalo))])
  })
}



# Define a function to get the indices of KG and EURO columns
kgeur <- function(cols) {
  grep("KG|EURO", cols)
}



# Define a function to create a unique trip identifier
create_trip_id <- function(eflalo) {
  paste(eflalo$LE_ID, eflalo$LE_CDAT, sep="-")
}


# Define a function to convert date and time columns to POSIXct
convert_to_datetime <- function(date_col, time_col) {
  as.POSIXct(paste(date_col, time_col, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
}


# Define a function to remove records starting before the 1st of January
remove_before_jan <- function(eflalo, year) {
  # Convert the start of the year to a POSIXct datetime object
  start_of_year <- as.POSIXct(paste(year, "-01-01 00:00:00", sep = ""), format = "%Y-%m-%d %H:%M")
  
  # Ensure FT_DDATIM is in the correct datetime format
  eflalo$FT_DDATIM <- as.POSIXct(eflalo$FT_DDATIM, format = "%Y-%m-%d %H:%M")
  
  # Remove records with FT_DDATIM before the start of the year
  eflalo <- eflalo[eflalo$FT_DDATIM >= start_of_year,]
  
  return(eflalo)
}



# Define a function to merge EFLALO and TACSAT objects

mergeEfTac <- function (eflalo2, tacsat) {
  if (!"D_DATIM" %in% colnames(eflalo2)) 
    eflalo2$D_DATIM <- as.POSIXct(paste(eflalo2$FT_DDAT, 
                                        eflalo2$FT_DTIME, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  if (!"L_DATIM" %in% colnames(eflalo2)) 
    eflalo2$L_DATIM <- as.POSIXct(paste(eflalo2$FT_LDAT, 
                                        eflalo2$FT_LTIME, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  if (!"SI_DATIM" %in% colnames(tacsat)) 
    tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, tacsat$SI_TIME, 
                                        sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  if (class(eflalo2$VE_REF) != "character") 
    eflalo2$VE_REF <- ac(eflalo2$VE_REF)
  if (class(tacsat$VE_REF) != "character") 
    
  # Convert 'VE_REF' columns to character if they are not already
  if (class(eflalo2$VE_REF) != "character") {
    eflalo2$VE_REF <- as.character(eflalo2$VE_REF)
  }
  if (class(tacsat$VE_REF) != "character") {
    tacsat$VE_REF <- as.character(tacsat$VE_REF)
  }
  
  
  
  # Order 'eflalo2' and 'tacsat' by 'VE_REF' and date columns
  eflalo <- orderBy(~VE_REF + FT_DDATIM + FT_LDATIM, data = eflalo2)
  
  # Convert 'tacsat' to a data frame
  tacsat <- as.data.frame(tacsat)
  
  # Order 'tacsat_df' by 'VE_REF' and 'SI_DATIM'
  tacsat <- orderBy(~VE_REF + SI_DATIM, data = tacsat_df)
  
  # Split 'eflalo2' and 'tacsat' by 'VE_REF'
  splitEf <- split(eflalo2, eflalo2$VE_REF)
  splitTa <- split(tacsat, tacsat$VE_REF)
  
  # Find matching 'VE_REF' values in 'eflalo2' and 'tacsat'
  tacefmatch <- pmatch(sort(unique(tacsat$VE_REF)), sort(unique(eflalo2$VE_REF)))
  
  # For each matching 'VE_REF' value, find overlapping trips and assign 'FT_REF' values accordingly
  for (i in 1:length(tacefmatch)) {
    if (!is.na(tacefmatch[i])) {
      eftim <- splitEf[[tacefmatch[i]]][!duplicated(splitEf[[tacefmatch[i]]]$FT_REF), c("FT_DDATIM", "FT_LDATIM", "FT_REF")]
      smdtime <- outer(splitTa[[i]]$SI_DATIM, eftim$FT_DDATIM, "-")
      gtltime <- outer(eftim$FT_LDATIM, splitTa[[i]]$SI_DATIM, "-")
      st <- apply(smdtime, 1, function(x) which(x >= 0)[1])
      en <- apply(gtltime, 1, function(x) rev(which(x >= 0))[1])
      subse <- which(!is.na(st <= en) & (st <= en))
      if (length(subse) > 0) {
        idx <- unlist(mapply(seq, st[subse], en[subse], SIMPLIFY = FALSE))
        splitTa[[i]]$FT_REF <- 0
        splitTa[[i]]$FT_REF[idx] <- rep(eftim$FT_REF[subse], table(idx))
      }
    } else {
      splitTa[[i]]$FT_REF <- 0
    }
  }
  
  # Combine 'FT_REF' values from all 'splitTa' data frames into 'tacsat'
  tacsat$FT_REF <- unlist(lapply(splitTa, `[[`, "FT_REF"))
  
  return(tacsat)
}



## Define a function to calculate intervals in the TACSAT data

intvTacsat <- function (tacsat, level = "trip", weight = c(1, 0), fill.na = FALSE) {
  # Check if 'weight' is a length 2 numeric vector
  if (length(weight) != 2) 
    stop("weight must be specified as a length 2 numeric vector")
  
  # Normalize 'weight' to sum to 1
  weight <- weight/sum(weight, na.rm = TRUE)
  
  # Sort 'tacsat' (assuming 'sortTacsat' is a function that sorts 'tacsat')
  tacsat <- sortTacsat(tacsat)
  
  # Convert 'SI_DATE' and 'SI_TIME' to POSIXct if 'SI_DATIM' is not already present
  if (!"SI_DATIM" %in% colnames(tacsat)) 
    tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, tacsat$SI_TIME, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  
  # If level is 'trip', calculate intervals for each trip
  if (level == "trip") {
    # Check if 'FT_REF' is present
    if (is.null(tacsat$FT_REF)) 
      stop("no trip number available to merge on trip level")
    
    # Split 'tacsat' by 'VE_REF'
    sptacsat <- split(tacsat, tacsat$VE_REF)
    
    # Calculate intervals for each trip
    tacsat$INTV <- unlist(lapply(sptacsat, function(x) {
      # Convert 'FT_REF' to factor
      FT_REF <- as.factor(x$FT_REF)
      
      # Calculate intervals for each 'FT_REF'
      res <- by(x, FT_REF, function(y) {
        # If there is more than one row, calculate intervals
        if (nrow(y) > 1) {
          # Calculate differences in 'SI_DATIM' for each row
          difftime_xmin1 <- c(NA, difftime(y$SI_DATIM[2:nrow(y)], y$SI_DATIM[1:(nrow(y) - 1)], units = "mins"))
          difftime_xplus1 <- c(difftime_xmin1[-1], NA)
          
          # Calculate intervals based on 'weight'
          if (any(weight == 0)) {
            if (weight[1] == 0) 
              INTV <- difftime_xplus1
            if (weight[2] == 0) 
              INTV <- difftime_xmin1
          } else {
            INTV <- rowSums(cbind(difftime_xmin1 * weight[1], difftime_xplus1 * weight[2]))
          }
          
          # If 'fill.na' is TRUE, fill NA values in 'INTV'
          if (fill.na) {
            idx <- which(is.na(INTV))
            INTV[idx] <- rowSums(cbind(difftime_xmin1[idx], difftime_xplus1[idx]), na.rm = TRUE)
            INTV[idx][which(INTV[idx] == 0)] <- NA
          }
          
          return(INTV)
        } else {
          return(NA)
        }
      })
      
      return(unsplit(res, FT_REF))
    }))
    
    # Set 'INTV' to NA where 'FT_REF' equals 0
    tacsat$INTV[which(tacsat$FT_REF == "0")] <- NA
  }
  
  # If level is 'vessel', calculate intervals for each vessel
  if (level == "vessel") {
    # Calculate differences in 'SI_DATIM' for each row
    difftime_xmin1 <- c(NA, difftime(tacsat$SI_DATIM[2:nrow(tacsat)], tacsat$SI_DATIM[1:(nrow(tacsat) - 1)], units = "mins"))
    difftime_xplus1 <- c(difftime_xmin1[-1], NA)
    
    # Calculate intervals based on 'weight'
    if (any(weight == 0)) {
      if (weight[1] == 0) 
        INTV <- difftime_xplus1
      if (weight[2] == 0) 
        INTV <- difftime_xmin1
    } else {
      INTV <- rowSums(cbind(difftime_xmin1 * weight[1], difftime_xplus1 * weight[2]))
    }
    
    # If 'fill.na' is TRUE, fill NA values in 'INTV'
    if (fill.na) {
      idx <- which(is.na(INTV))
      INTV[idx] <- rowSums(cbind(difftime_xmin1[idx], difftime_xplus1[idx]), na.rm = TRUE)
      INTV[idx][which(INTV[idx] == 0)] <- NA
    }
    
    # Assign 'INTV' to 'tacsat'
    tacsat$INTV <- INTV
    
    # Get unique vessels
    vessels <- unique(tacsat$VE_REF)
    
    # Get first and last rows for each vessel
    first.vessels <- unlist(lapply(as.list(vessels), function(x) which(tacsat$VE_REF == x)[1]))
    last.vessels <- unlist(lapply(as.list(vessels), function(x) rev(which(tacsat$VE_REF == x))[1]))
    
    # Set 'INTV' to NA for first and last rows of each vessel based on 'weight'
    if (weight[1] != 0) 
      tacsat$INTV[first.vessels] <- NA
    if (weight[2] != 0) 
      tacsat$INTV[last.vessels] <- NA
    
    # If 'fill.na' is TRUE, fill NA values in 'INTV' for first and last rows of each vessel
    if (fill.na) {
      tacsat$INTV[first.vessels] <- difftime_xplus1[first.vessels]
      tacsat$INTV[last.vessels] <- difftime_xmin1[last.vessels]
    }
  }
  
  return(tacsat)
}



sfsortTacsat <- function(dat) {
  if (!"SI_DATIM" %in% colnames(dat)) {
    dat$SI_DATIM <- as.POSIXct(paste(dat$SI_DATE, dat$SI_TIME, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  }
  if ("VE_REF" %in% colnames(dat)) {
    dat <- arrange(dat, VE_REF, SI_DATIM)
  }
  if ("OB_REF" %in% colnames(dat)) {
    dat <- arrange(dat, OB_REF, SI_DATIM)
  }
  return(dat)
}


