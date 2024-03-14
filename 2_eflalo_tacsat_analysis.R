

#for(year in yearsToSubmit)  {
  
  print(year)
  
  load(file = paste0(outPath,paste0("/cleanEflalo",year,".RData")) )
  load(file = paste0(outPath, paste0("/cleanTacsat", year, ".RData")) )
  
  
  # 2.1 Merge the TACSAT and EFLALO data together --------------------------------------------
  
  # Merge eflalo and tacsat =================================
  
  tacsatp <- mergeEflalo2Tacsat(eflalo,tacsat)
  
  # Assign gear and length to tacsat =================================
  
  # Define the columns to be added
  cols <- c("LE_GEAR", "LE_MSZ", "VE_LEN", "VE_KW", "LE_RECT", "LE_MET", "LE_WIDTH", "VE_FLT", "LE_CDAT", "VE_COU")
  
  # Use a loop to add each column
  for (col in cols) {
    # Match 'FT_REF' values in 'tacsatp' and 'eflalo' and use these to add the column from 'eflalo' to 'tacsatp'
    tacsatp[[col]] <- eflalo[[col]][match(tacsatp$FT_REF, eflalo$FT_REF)]
  }
  

  # Save not merged tacsat data
  # Subset 'tacsatp' where 'FT_REF' equals 0 (not merged)
  tacsatpmin <- subset(tacsatp, FT_REF == 0)
  
  # Save 'tacsatpmin' to a file named "tacsatNotMerged<year>.RData" in the 'outPath' directory
  save(
    tacsatpmin,
    file = file.path(outPath, paste0("tacsatNotMerged", year, ".RData"))
  )
  
  # Subset 'tacsatp' where 'FT_REF' does not equal 0 (merged)
  tacsatp <- subset(tacsatp, FT_REF != 0)
  
  # Save 'tacsatp' to a file named "tacsatMerged<year>.RData" in the 'outPath' directory
  save(
    tacsatp,
    file = file.path(outPath, paste0("tacsatMerged", year, ".RData"))
  )
  
  # 2.2  Define activity  ---------------------------------------------------------------------
  
  
  # Calculate time interval between points ===================================
  tacsatp <- intvTacsat(tacsatp, level = "trip", fill.na = TRUE)
  
  # Reset values that are simply too high to 2x the regular interval rate  
  
  
  tacsatp$INTV[tacsatp$INTV > intvThres] <- 2 * intvThres
  
  
  # Remove points with NA's in them in critial places ========================

  idx <-
    which(
      is.na(tacsatp$VE_REF) == TRUE |
        is.na(tacsatp$SI_LONG) == TRUE |
        is.na(tacsatp$SI_LATI) == TRUE |
        is.na(tacsatp$SI_DATIM) == TRUE |
        is.na(tacsatp$SI_SP) == TRUE
    )
  if (length(idx) > 0) {
    tacsatp <- tacsatp[-idx, ]
  }
  
  
  # Define speed thresholds associated with fishing for gears =====================
  
  
  # Investigate speed pattern through visual inspection of histograms # 
  
  # Create a histogram of speeds for different gears
  # Start a new PNG device
  # Create a histogram of speeds for different gears
  diag.plot <- ggplot(data = tacsatp, aes(SI_SP)) +
    geom_histogram(aes(fill = LE_GEAR), breaks = seq(0, 20, by = 1), color = "white") +
    facet_wrap(~ LE_GEAR, ncol = 4, scales = "free_y") +
    labs(x = "Speed (knots)", y = "Frequency", title = "Histogram of Speeds by Gear") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(colour = "black"),
      axis.text.x = element_text(colour = "black"),
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, size = 20),
      strip.text.x = element_text(size = 12, face = "bold"),
      strip.background = element_rect(fill = "grey60", colour = "black", linewidth = 1),
      panel.background = element_blank()
    ) +
    scale_fill_manual(values = c("#000000", "#FCCF3F", "#FF0000", "#00FF00", "#0000FF",
                                 "#FF00FF", "#808080", "#800000", "#808000",
                                 "#008000", "#800080", "#008080", "#000080", "#666699", "#808080",
                                 "#003366", "#CCA099", "#333300", "#993300", "#993366", "#333399",
                                 "#333333"))
  
  ggsave(diag.plot, filename = file.path(outPath, paste0("SpeedHistogram_", year, ".jpg")))
  
  # Create speed threshold object # 
  
  # start by correctly formatting the level 5 metier
  tacsatp$LE_L5MET <-  sapply(strsplit(tacsatp$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  
  
  # Create a data frame with minimum and maximum speed thresholds for each gear
  speedarr <- as.data.frame(
    cbind(
      LE_L5MET = sort(unique(tacsatp$LE_L5MET)),
      min = NA,
      max = NA
    ),
    stringsAsFactors = FALSE
  )
  
  # Fill out the minimum and maximum speed thresholds
  speedarr$min <- rep(1, nrow(speedarr)) # It is important to fill out the personally inspected thresholds here!
  speedarr$max <- rep(6, nrow(speedarr))
  
  
  # Analyse activity automated for common gears only. Use the speedarr for the other gears =============== 
  
  subTacsat <- subset(tacsatp, LE_GEAR %in% autoDetectionGears)
  nonsubTacsat <- subset(tacsatp, !LE_GEAR %in% autoDetectionGears)
  
 if (visualInspection == TRUE)
  {
    storeScheme <-
      ac.tac.anal(
        subTacsat,
        units = "year",
        analyse.by = "LE_L5MET",
        identify = "means")
  } else
  {
    storeScheme <-
      expand.grid(
        years = year,
        months = 0,
        weeks = 0,
        analyse.by = unique(subTacsat[,"LE_L5MET"])
      )
    
    storeScheme$peaks <- NA
    storeScheme$means <- NA
    storeScheme$fixPeaks <- FALSE
    storeScheme$sigma0 <- 0.911
    
    
    # Fill the storeScheme values based on analyses of the pictures = 
    
    storeScheme$LE_GEAR <- sapply(strsplit(as.character(storeScheme$analyse.by), "_"), `[`, 1)
    
    # Define mean values of the peaks and the number of peaks when they are different from 5 # 
    
    
    storeScheme$means[which(storeScheme$LE_GEAR == "TBB")] <- c("-11.5 -6 0 6 11.5")
    storeScheme$means[which(storeScheme$LE_GEAR == "OTB")] <- c("-9 -3 0 3 9")
    storeScheme$means[which(storeScheme$LE_GEAR == "OTT")] <- c("-9 -3 0 3 9")
    storeScheme$means[which(storeScheme$LE_GEAR == "MIS")] <- c("-9 -3 0 3 9")
    storeScheme$means[which(storeScheme$LE_GEAR == "SSC")] <- c("-9 0 9")
    storeScheme$means[which(storeScheme$LE_GEAR == "PTB")] <- c("-10 -3 0 3 10")
    storeScheme$means[which(storeScheme$LE_GEAR == "DRB")] <- c("-10 0 10")
    storeScheme$means[which(storeScheme$LE_GEAR == "HMD")] <- c("-9 0 9")
    storeScheme$peaks[which(storeScheme$LE_GEAR == "SSC")] <- 3
    storeScheme$peaks[which(storeScheme$LE_GEAR == "DRB")] <- 3
    storeScheme$peaks[which(storeScheme$LE_GEAR == "HMD")] <- 3
    storeScheme$peaks[which(is.na(storeScheme$peaks) == TRUE)] <- 5
    storeScheme <- storeScheme[,-(dim(storeScheme)[2])]
  }
  
#  acTa <- ac.tac.anal(subTacsat, units = "year", storeScheme = storeScheme, analyse.by = "LE_L5MET", identify = "peaks")
  
  acTa <-
    act.tac(
      subTacsat,
      units = "year",
      analyse.by = "LE_L5MET",
      storeScheme = storeScheme,
      plot = TRUE,
      level = "all")
  subTacsat$SI_STATE <- acTa
  subTacsat$ID <- 1:nrow(subTacsat)
  
  # Check results, and if results are not satisfactory, run analyses again but now with fixed peaks # 
  
  for (iGear in autoDetectionGears) {
    subDat <- subset(subTacsat, LE_GEAR == iGear)
    
    # Check if there are non-missing values for "s" state
    if (any(!is.na(subDat$SI_SP[which(subDat$SI_STATE == "s")]))) {
      minS <- min(subDat$SI_SP[which(subDat$SI_STATE == "s")], na.rm = TRUE)
    } else {
      minS <- Inf  # or assign a default value or handle the case accordingly
    }
    
    # Check if there are non-missing values for "f" state
    if (any(!is.na(subDat$SI_SP[which(subDat$SI_STATE == "f")]))) {
      minF <- min(subDat$SI_SP[which(subDat$SI_STATE == "f")], na.rm = TRUE)
    } else {
      minF <- Inf  # or assign a default value or handle the case accordingly
    }
    
    if (minS < minF) {
      storeScheme$fixPeaks[which(storeScheme$analyse.by == iGear)] <- TRUE
      subacTa <- activityTacsat(
        subDat,
        units = "year",
        analyse.by = "LE_GEAR",
        storeScheme,
        plot = FALSE,
        level = "all"
      )
      subTacsat$SI_STATE[subDat$ID] <- subacTa
    }
  }  
  subTacsat <-
    subTacsat[,
              -rev(grep("ID", colnames(subTacsat)))[1]
    ]
  
  # Assign for visually inspected gears a simple speed rule classification =============== 
  
  
  
  metiers <- unique(nonsubTacsat$LE_l5MET)
  nonsubTacsat$SI_STATE <- NA
  for (mm in metiers) {
    nonsubTacsat$SI_STATE[
      nonsubTacsat$LE_GEAR == mm &
        nonsubTacsat$SI_SP >= speedarr[speedarr$LE_GEAR == mm, "min"] &
        nonsubTacsat$SI_SP <= speedarr[speedarr$LE_GEAR == mm, "max"]
    ] <- "f";
  }
  nonsubTacsat$SI_STATE[
    nonsubTacsat$LE_GEAR == "NA" &
      nonsubTacsat$SI_SP >= speedarr[speedarr$LE_GEAR == "MIS", "min"] &
      nonsubTacsat$SI_SP <= speedarr[speedarr$LE_GEAR == "MIS", "max"]
  ] <- "f"
  nonsubTacsat$SI_STATE[ is.na(nonsubTacsat$SI_STATE) ] <- "s"
  
  
  # Combine the two dataset together again =============== 
  
  
  tacsatp <- rbindTacsat(subTacsat, nonsubTacsat)
  tacsatp <- orderBy( ~ VE_REF + SI_DATIM, data = tacsatp)
  
  # Set fishing sequences with hauling in the middle to "f" ##################
  
  
  idx <-
    which(
      tacsatp$SI_STATE[2:(nrow(tacsatp) - 1)] == "h" &
        tacsatp$SI_STATE[1:(nrow(tacsatp) - 2)] == "f" &
        tacsatp$SI_STATE[3:(nrow(tacsatp))    ] == "f" &
        tacsatp$VE_REF[2:(nrow(tacsatp) - 1)] == tacsatp$VE_REF[1:(nrow(tacsatp) - 2)] &
        tacsatp$VE_REF[2:(nrow(tacsatp) - 1)] == tacsatp$VE_REF[3:(nrow(tacsatp))]
    ) + 1
  tacsatp$SI_STATE[idx] <- "f"
  
  save(
    tacsatp,
    file = file.path(outPath, paste0("tacsatActivity", year, ".RData"))
  )
  
  message("Defining activity completed")
  
  
  # 2.3 Dispatch landings of merged eflalo at the ping scale
  # -------------------------------------------------
  
  # Get the indices of columns in eflalo that contain "LE_KG_" or "LE_EURO_"
  idxkg <- grep("LE_KG_", colnames(eflalo))
  idxeur <- grep("LE_EURO_", colnames(eflalo))
  
  # Calculate the total KG and EURO for each row
  eflalo$LE_KG_TOT <- rowSums(eflalo[, idxkg], na.rm = TRUE)
  eflalo$LE_EURO_TOT <- rowSums(eflalo[, idxeur], na.rm = TRUE)
  
  # Remove the columns used for the total calculation
  eflalo <- eflalo %>% select(!all_of(c(colnames(eflalo)[idxkg], colnames(eflalo)[idxeur])))
  
  # Split eflalo into two data frames based on the presence of FT_REF in tacsatp
  eflaloNM <- subset(eflalo, !FT_REF %in% unique(tacsatp$FT_REF))
  eflaloM <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))
  
  # Convert SI_STATE to binary (0/1) format
  tacsatp$SI_STATE <- ifelse(tacsatp$SI_STATE == "f", 1, 0)
  
  # Filter rows where SI_STATE is 1
  tacsatEflalo <- tacsatp[tacsatp$SI_STATE == 1,]
  
  # Check the type of linking required and call splitAmongPings accordingly
  if (!"trip" %in% linkEflaloTacsat) stop("trip must be in linkEflaloTacsat")
  
  if (all(c("day", "ICESrectangle", "trip") %in% linkEflaloTacsat)) {
    level <- "day"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
  } else if (all(c("day","trip") %in% linkEflaloTacsat) & !"ICESrectangle" %in% linkEflaloTacsat) {
    level <- "day"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
    tmpTa$LE_RECT <- "ALL"
    tmpEf$LE_RECT <- "ALL"
  } else if (all(c("ICESrectangle", "trip") %in% linkEflaloTacsat) & !"day" %in% linkEflaloTacsat) {
    level <- "ICESrectangle"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
  } else if (linkEflaloTacsat == "trip" & length(linkEflaloTacsat) == 1) {
    level <- "trip"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
  }
  
  tacsatEflalo <- splitAmongPings(
    tacsat = tmpTa,
    eflalo = tmpEf,
    variable = "all",
    level = level,
    conserve = level != "trip"
  )
  
  save(
    tacsatEflalo,
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )
  
  print("Dispatching landings completed")
  
  
  # 2.4 Assign c-square, year, month, quarter, area and create table 1
  # ------------------------------------------------------------------
  
  # Calculate the c-square based on longitude and latitude
  tacsatEflalo$Csquare <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
  
  # Extract the year and month from the date-time
  tacsatEflalo$Year <- year(tacsatEflalo$SI_DATIM)
  tacsatEflalo$Month <- month(tacsatEflalo$SI_DATIM)
  
  # Calculate the kilowatt-hour and convert interval to hours
  tacsatEflalo$kwHour <- tacsatEflalo$VE_KW * tacsatEflalo$INTV / 60
  tacsatEflalo$INTV <- tacsatEflalo$INTV / 60
  
  # Define the record type
  RecordType <- "VE"
  
  # Define the columns to be included in the table
  cols <- c(
    "VE_REF", "VE_COU", "Year", "Month", "Csquare", "LE_GEAR",
    "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT"
  )
  
  # Create or append to table1 based on the year
  if (year == yearsToSubmit[1]) {
    table1 <- cbind(RT = RecordType, tacsatEflalo[, cols])
  } else {
    table1 <- rbind(table1, cbind(RT = RecordType, tacsatEflalo[, cols]))
  }
  
  
  # Save table1   ====================
  
  
  
  save(
    table1,
    file = file.path(outPath, "table1.RData" )
  )
  
  message(glue ("Table 1 for year {year} is completed") )
  
  
  # 2.5 Assign year, month, quarter, area and create table 2
  # --------------------------------------------------------
  
  # Extract the year and month from the date-time
  eflalo$Year <- year(eflalo$FT_LDATIM)
  eflalo$Month <- month(eflalo$FT_LDATIM)
  
  # Set interval to 1 day
  eflalo$INTV <- 1
  
  # Create a dummy variable for aggregation
  eflalo$dummy <- 1
  
  # Aggregate the dummy variable by VE_COU, VE_REF, and LE_CDAT
  res <- aggregate(
    eflalo$dummy,
    by = as.list(eflalo[, c("VE_COU", "VE_REF", "LE_CDAT")]),
    FUN = sum,
    na.rm = TRUE
  )
  
  # Rename the columns of the aggregated data frame
  colnames(res) <- c("VE_COU", "VE_REF", "LE_CDAT", "nrRecords")
  
  # Merge the aggregated data frame with eflalo
  eflalo <- merge(eflalo, res, by = c("VE_COU", "VE_REF", "LE_CDAT"))
  
  # Adjust the interval and calculate kilowatt-days
  eflalo$INTV <- eflalo$INTV / eflalo$nrRecords
  eflalo$kwDays <- eflalo$VE_KW * eflalo$INTV
  
  # Check if FT_REF is in tacsatp
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatp$FT_REF, "Y", "N")
  
  # Define the record type
  RecordType <- "LE"
  
  # Define the columns to be included in the table
  cols <- c(
    "VE_REF", "VE_COU", "Year", "Month", "LE_RECT", "LE_GEAR", "LE_MET",
    "VE_LEN", "tripInTacsat", "INTV", "kwDays", "LE_KG_TOT", "LE_EURO_TOT"
  )
  
  # Create or append to table2 based on the year
  if (year == yearsToSubmit[1]) {
    table2 <- cbind(RT = RecordType, eflalo[, cols])
  } else {
    table2 <- rbind(table2, cbind(RT = RecordType, eflalo[, cols]))
  }
  
  
  # Save table2   ====================
  
  
  
  save(
    table2,
    file = file.path(outPath, "table2.RData" )
  )
  
  message(glue ("Table 2 for year {year} is completed") )
  
  
  
  
}
