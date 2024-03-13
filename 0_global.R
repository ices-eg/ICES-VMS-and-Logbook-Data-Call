
#--------------------READ ME----------------------------------------------------
# The following script is a proposed workflow example to processes the ICES
# VMS datacall request. It is not an exact template to be applied to data from
# every member state and needs to be adjusted according to the data availability
# and needs of every member state.
#-------------------------------------------------------------------------------

#'------------------------------------------------------------------------------
# 0.1 Preparations                                                          ----
#'------------------------------------------------------------------------------


rm(list=ls())

# Install packages from repository if necessary
# install.packages("sfdSAR", repos = "https://ices-tools-prod.r-universe.dev")
# install.packages("icesVocab", repos = "https://ices-tools-prod.r-universe.dev")

# Download pacman, and let that install the needed packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(vmstools, sf, data.table, raster, terra, mapview, Matrix, dplyr, 
               doBy, mixtools, tidyr, glue, gt, progressr, geosphere, purrr, 
               ggplot2, sfdSAR, icesVocab, generics, icesConnect, icesVMS,
               tidyverse, units, tcltk)

#- Settings paths
path <- paste0(getwd(), "/") # Your path / working directory

codePath  <- paste0(path, "Scripts/")   #Location where you store R scripts
dataPath  <- paste0(path, "Data/")      #Location where you store tacsat (VMS) and eflalo (logbook) data
outPath   <- paste0(path, "Results/")   #Location where you want to store the results
plotPath  <- paste0(path, "Plots/") 

dir.create(codePath, showWarnings = T)
dir.create(dataPath, showWarnings = T)
dir.create(outPath, showWarnings = T)
dir.create(plotPath, showWarnings = T)

#'------------------------------------------------------------------------------
# 0.2 Settings for analysis                                                 ----
#'------------------------------------------------------------------------------


#- Setting specific thresholds
spThres       <- 20   #Maximum speed threshold in analyses in nm
intThres      <- 5    #Minimum difference in time interval in minutes to prevent pseudo duplicates
intvThres     <- 240  #Maximum difference in time interval in minutes to prevent intervals being too large to be realistic
lanThres      <- 1.5  #Maximum difference in log10-transformed sorted weights

#- Test version REMOVE BEFORE RELEASE
yearsToSubmit <- c(2018,2022)

#- Re-run all years as we have new field for no. vessels
# yearsToSubmit <- 2009:2023

#- Set the gear names for which automatic fishing activity is wanted
#  It is important to fill out the gears you want to apply auto detection for
autoDetectionGears        <- c("TBB","OTB","OTT","SSC","SDN","DRB","PTB","HMD", "MIS")

#- Decide if you want to visualy analyse speed-histograms to identify fishing activity
#  peaks or have prior knowledge and use the template provided around lines 380 below
visualInspection          <- FALSE

#- Specify how landings should be distributed over the VMS pings: By day, ICES rectangle, trip basis or otherwise
# Available Options:
# linkEflaloTacsat          <- c("day","ICESrectangle","trip")
# linkEflaloTacsat          <- c("day","ICESrectangle","trip")
# linkEflaloTacsat          <- c("ICESrectangle","trip")
 linkEflaloTacsat          <- c("day","trip")
# linkEflaloTacsat          <- c("trip")

 
 # Extract valid level 6 metiers 
 valid_metiers <- fread("https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv")$Metier_level6
 
 # Necessary setting for spatial operations
 sf::sf_use_s2(FALSE)
 
 
 
 #'------------------------------------------------------------------------------
 # 0.3 Defining functions                                                    ----
 #'------------------------------------------------------------------------------
 # Define function for NOT in data 
 '%!in%' <- function(x,y)!('%in%'(x,y)) 
 
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
      specs_cols <- unlist(sapply(x, grep, colnames(eflalo)))
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


# Define a function to  sort Tacsat data 
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





## Define a function to identify peaks in speed distributions
callNumberPeak <- function(){
  tt <- tktoplevel()
  peaks <- tclVar(5)
  
  f1 <- tkframe(tt)
  tkpack(f1, side='top')
  tkpack(tklabel(f1, text='peaks: '), side='left')
  tkpack(tkentry(f1, textvariable=peaks), side='left')
  
  tkpack(tkbutton(tt, text='Next', command=function() tkdestroy(tt)),
         side='right', anchor='s')
  
  tkwait.window(tt)
  return(as.numeric(tclvalue(peaks)))}





ac.tac.anal <- function (tacsat, units = "year", storeScheme = storeScheme, analyse.by = "LE_L5MET", identify = "peaks") 
{
  if (!"LE_L5MET" %in% colnames(tacsat)) 
    stop("Provide gear type (as column 'LE_L5MET' and if unknown, provide it as 'MIS'")
  if (!analyse.by %in% c("LE_L5MET", "VE_REF")) 
    warning("Analysing by unknown column variable, please check!")
  if (analyse.by %in% colnames(tacsat)) {
    if (units == "all") {
      yrs <- 0
      mths <- 0
      wks <- 0
    }
    if (units == "year") {
      yrs <- sort(unique(format(tacsat$SI_DATIM, "%Y")))
      mths <- 0
      wks <- 0
    }
    if (units == "month") {
      yrs <- sort(unique(format(tacsat$SI_DATIM, "%Y")))
      mths <- sort(unique(month(tacsat$SI_DATIM)))
      wks <- 0
    }
    if (units == "week") {
      yrs <- sort(unique(format(tacsat$SI_DATIM, "%Y")))
      wks <- sort(unique(week(tacsat$SI_DATIM)))
      mths <- 0
    }
    runScheme <- expand.grid(years = yrs, months = mths, 
                             weeks = wks, stringsAsFactors = FALSE)
    storeScheme <- expand.grid(years = yrs, months = mths, 
                               weeks = wks, analyse.by = unique(tacsat[, analyse.by]), 
                               stringsAsFactors = FALSE)
    storeScheme$peaks <- NA
    storeScheme$fixPeaks <- FALSE
    storeScheme$sigma0 <- 0.911
    if (identify == "means") 
      storeScheme$means <- NA
    for (iRun in 1:nrow(runScheme)) {
      yr <- runScheme[iRun, "years"]
      mth <- runScheme[iRun, "months"]
      wk <- runScheme[iRun, "weeks"]
      if (nrow(runScheme) == 1) {
        sTacsat <- tacsat
      }
      else {
        if (mth == 0 & wk == 0) 
          sTacsat <- subset(tacsat, format(tacsat$SI_DATIM, 
                                           "%Y") == yr)
        if (mth == 0 & wk != 0) 
          sTacsat <- subset(tacsat, format(tacsat$SI_DATIM, 
                                           "%Y") == yr & week(tacsat$SI_DATIM) == wk)
        if (mth != 0 & wk == 0) 
          sTacsat <- subset(tacsat, format(tacsat$SI_DATIM, 
                                           "%Y") == yr & month(tacsat$SI_DATIM) == mth)
      }
      for (iBy in na.omit(unique(sTacsat[, analyse.by]))) {
        dat <- subset(sTacsat, sTacsat[, analyse.by] == 
                        iBy)
        datmr <- dat
        datmr$SI_SP <- -1 * dat$SI_SP
        datmr <- rbind(dat, datmr)
        xrange <- pmin(20, range(datmr$SI_SP), na.rm = TRUE)
        datmr$SI_SP[which(abs(datmr$SI_SP) > 20)] <- NA
        hist(datmr$SI_SP, breaks = seq(-20, 20, 0.5), 
             main = paste(iBy, ifelse(yr > 0, yr, ""), ifelse(mth > 
                                                                0, mth, ""), ifelse(wk > 0, wk, "")), xaxt = "n")
        axis(1, at = seq(-20, 20, 1))
        require(tcltk)
        pks <- callNumberPeak()
        storeScheme[which(storeScheme$years == yr & storeScheme$months == 
                            mth & storeScheme$weeks == wk & storeScheme$analyse.by == 
                            iBy), "peaks"] <- pks
        if (identify == "means") {
          valPeaks <- callPeakValue(pks)
          if (substr(valPeaks, 1, 1) == " ") 
            valPeaks <- substr(valPeaks, 2, nchar(valPeaks))
          storeScheme[which(storeScheme$years == yr & 
                              storeScheme$months == mth & storeScheme$weeks == 
                              wk & storeScheme$analyse.by == iBy), "means"] <- valPeaks
        }
      }
    }
  }
  else {
    stop("analyse.by statement not found as a column in the specified tacsat dataset")
  }
  return(storeScheme)
}


# Define a function to assign tripnumber to Tacsat data
trip_assign <- function(tacsatp, eflalo, col = "LE_GEAR", trust_logbook = T){
  
  if(col == "LE_MET"){
    tst <- data.table(eflalo)[get(col) %in% valid_metiers & !is.na(get(col)) ,.(uniqueN(get(col))), by=.(FT_REF)]
  }else{
    tst <- data.table(eflalo)[!is.na(get(col)),.(uniqueN(get(col))), by=.(FT_REF)]
  }
  if(nrow(tst[V1>1])==0){
    warning(paste("No duplicate", col, "in tacsatp"))
    return(data.frame())
  }
  
  e <- data.table(eflalo)[FT_REF %in% tst[V1>1]$FT_REF]
  
  tz <- data.table(tacsatp)[FT_REF  %in% tst[V1>1]$FT_REF]
  suppressWarnings(tz[, (col) := NULL])
  if(trust_logbook){
    
    ## First bind by landing date
    e2 <- e[,.(get(col)[length(unique(get(col))) == 1]), by = .(FT_REF, LE_CDAT)]
    names(e2) <- c("FT_REF", "LE_CDAT", col)
    
    tz <- tz |> 
      left_join(e2, by = c("FT_REF" = "FT_REF", "SI_DATE" = "LE_CDAT"), relationship = "many-to-many")
    
    tz <- unique(tz)
    
    #If some are still missing, use haul information to get the closest time
    if(nrow(tz[is.na(get(col))]) > 0){
      #set formats right
      e$FT_DDATIM <- as.POSIXct(paste(e$FT_DDAT, e$FT_DTIME, 
                                      sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
      e$FT_LDATIM <- as.POSIXct(paste(e$FT_LDAT, e$FT_LTIME, 
                                      sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
      
      e$LE_SDATTIM <- as.POSIXct(paste(e$LE_SDAT, e$LE_STIME, 
                                       sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
      e$LE_EDATTIM <- as.POSIXct(paste(e$LE_EDAT, e$LE_ETIME, 
                                       sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
      
      tx <- tz[is.na(get(col))]
      tx[, (col) := NULL]
      
      mx <- rbind(e[,.(meantime = LE_SDATTIM), by = .(get(col))], e[,.(meantime = LE_EDATTIM), by = .(get(col))])
      names(mx) <-  c(col, "meantime")
      
      tx[, time := SI_DATIM]
      
      setkey(tx, time)
      setkey(mx, meantime)
      tx <- mx[tx, roll="nearest"]
      tx$meantime <- NULL
      tz <- rbindlist(list(tz[!is.na(get(col))], tx), fill = T)
      
    }
    
  }else{
    tz[, (col) := NA]
  }
  
  # Bind to the category with most value
  if(nrow(tz[is.na(get(col))]) > 0){
    
    if(!"LE_KG_TOT" %in% names(e)){
      idxkgeur <- colnames(e)[grepl("LE_KG_|LE_EURO_", colnames(e))]
      # Calculate the total KG and EURO for each row
      e$LE_KG_TOT <- rowSums(e[,..idxkgeur], na.rm = TRUE)
    }
    
    highvalue <- e[,.(LE_KG_TOT = sum(LE_KG_TOT, na.rm = T)), by = .(FT_REF, get(col))]
    highvalue <- highvalue[,.(get[which.max(LE_KG_TOT)]), by = .(FT_REF)]
    names(highvalue) <-  c("FT_REF", col)
    
    tx <- tz[is.na(get(col))]
    tx[, (col) := NULL]
    tz <- merge(tx, highvalue)
  }
  return(tz)
}

# Define a function to add gear width to metier
add_gearwidth <- function(x, met_name = "LE_MET", oal_name = "VE_LEN", kw_name = "VE_KW"){
  
  require(data.table)
  require(dplyr)
  require(sfdSAR)
  require(icesVMS)
  
  setDT(x)
  ID <- c(oal_name, kw_name)
  x[,(ID):= lapply(.SD, as.numeric), .SDcols = ID]
  x[, Metier_level6 := get(met_name)]
  
  
  #Updated metiers
  metier_lookup <- fread("https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv")
  
  if(any(x[, get(met_name)] %!in% metier_lookup$Metier_level6))
    stop(paste("Non valid metiers in tacsatEflalo:", paste(x[x[, get(met_name)] %!in% metier_lookup$Metier_level6][, get(met_name)], collapse = ", ")))
  
  gear_widths <- get_benthis_parameters()
  
  aux_lookup <- data.table(merge(gear_widths, metier_lookup, by.x = "benthisMet", by.y = "Benthis_metiers", all.y = T))
  aux_lookup <- aux_lookup[,.(Metier_level6, benthisMet, avKw, avLoa, avFspeed, subsurfaceProp, gearWidth, firstFactor, secondFactor, gearModel, 
                              gearCoefficient, contactModel)]
  
  aux_lookup <<- unique(aux_lookup)
  
  
  aux_lookup <- data.table(merge(gear_widths, metier_lookup, by.x = "benthisMet", by.y = "Benthis_metiers", all.y = T))
  aux_lookup <- aux_lookup[,.(Metier_level6, benthisMet, avKw, avLoa, avFspeed, subsurfaceProp, gearWidth, firstFactor, secondFactor, gearModel, 
                              gearCoefficient, contactModel)]
  
  aux_lookup[gearCoefficient == "avg_kw", gearCoefficient := kw_name]
  aux_lookup[gearCoefficient == "avg_oal", gearCoefficient := oal_name]
  
  aux_lookup <- unique(aux_lookup)
  
  vms <- x |> 
    left_join(aux_lookup, by = "Metier_level6")
  
  vms$gearWidth_model <-
    predict_gear_width(vms$gearModel, vms$gearCoefficient, vms)
  
  if("avg_gearWidth" %!in% names(vms))
    vms[, avg_gearWidth := NA]
  
  
  gearWidth_filled <-
    with(vms,
         ifelse(!is.na(avg_gearWidth), avg_gearWidth,
                ifelse(!is.na(gearWidth_model), gearWidth_model,
                       gearWidth)
         ))
  
  return(gearWidth_filled)
}

# Define a function to check for missing columns in clean Tacsat data
tacsat_clean <- function(tacsat){
  '%!in%' <- function(x,y)!('%in%'(x,y))
  cols <- c("SI_LATI", "SI_LONG", "SI_SP", "SI_HE")
  if(any(cols %!in% names(tacsat)))
    stop("Column missing in tacsat:", paste(cols[cols %!in% names(tacsat)], collapse = ", "))
  
  tacsat[,(cols):= lapply(.SD, function(x) as.numeric(x)), .SDcols=cols]
  
}

# Define a function to check for missing columns in clean Eflalo data
eflalo_clean <- function(eflalo){
  '%!in%' <- function(x,y)!('%in%'(x,y))
  cols <- c("VE_KW", "VE_LEN", "VE_TON", "LE_MSZ", grep("KG|EURO", colnames(eflalo), value = T))
  
  if(any(cols %!in% names(eflalo)))
    stop("Column missing in eflalo:", paste(cols[cols %!in% names(eflalo)], collapse = ", "))
  
  eflalo[,(cols):= lapply(.SD, function(x) as.numeric(x)), .SDcols=cols]
  
  if(nrow(eflalo[is.na(VE_KW)]) != 0){
    warning(paste("No engine recorded for the following vessels:", paste(unique(eflalo[is.na(VE_KW)]$VE_REF), collapse = ", ")), 
            ". Setting their KW to 0")
    eflalo[is.na(VE_KW), VE_KW := 0]
  }
  
  if(nrow(eflalo[is.na(LE_MSZ)]) != 0){
    warning(paste("No mesh size recorded for the following vessels:", paste(unique(eflalo[is.na(LE_MSZ)]$VE_REF), collapse = ", ")))
    warning("Setting their mesh size to 0")
    eflalo[is.na(VE_KW), VE_KW := 0]
  }
  
  if(nrow(eflalo[is.na(VE_LEN)]) != 0){
    stop(paste("No lengths recorded for the following vessels:", paste(unique(eflalo[is.na(VE_LEN)]$VE_REF), collapse = ", ")), 
         ". Vessels needs a length, please supply one")
  }
  
  if(nrow(eflalo[is.na(VE_TON)]) != 0){
    stop(paste("No weight recorded for the following vessels:", paste(unique(eflalo[is.na(VE_TON)]$VE_REF), collapse = ", ")), 
         ". Vessels needs a weight, please supply one")
  }
  
  if("FT_DDATIM" %!in% names(eflalo))
    eflalo[, FT_DDATIM := as.POSIXct(
      paste(FT_DDAT, FT_DTIME, sep = " "),
      tz = "GMT",
      format = "%d/%m/%Y  %H:%M")]
  
  if("FT_LDATIM" %!in% names(eflalo))
    eflalo[, FT_LDATIM := as.POSIXct(
      paste(FT_LDAT, FT_LTIME, sep = " "),
      tz = "GMT",
      format = "%d/%m/%Y  %H:%M")]
  
}

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

#'------------------------------------------------------------------------------
# End of script                                                             
#'------------------------------------------------------------------------------
