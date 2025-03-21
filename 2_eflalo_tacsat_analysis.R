#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 2: Linking TACSAT and EFLALO data                                       ----
#
#'------------------------------------------------------------------------------

# Looping through the years to submit
for(year in yearsToSubmit){
  print(paste0("Start loop for year ",year))
  
  #'----------------------------------------------------------------------------
  # 2.1  load TACSAT and EFLALO data from file                             ----
  #'----------------------------------------------------------------------------
  load(file = paste0(outPath,paste0("/cleanEflalo",year,".RData")) )
  load(file = paste0(outPath, paste0("/cleanTacsat", year,".RData")) )
  
  # Assign geometry column to tacsat for later operations
  tacsat$geometry <- NULL
  
  #'----------------------------------------------------------------------------
  # 2.2  Assign EFLALO Fishing trip information (gear, vessel, lenght, etc. ) to VMS records in TACSAT                                           ----
  #'----------------------------------------------------------------------------
  
  #'----------------------------------------------------------------------------
  # 2.2.1  Assign EFLALO Fishing Trip identifiers to TACSAT records                                           ----
  #'----------------------------------------------------------------------------
  #'
  #'Assign a EFLALO trip identifier (FT_REF)  at each VMS record in TACSAT . 
  #'Methods asign fishing trip to VMS records which  date/time  is between the Trip dates of departure and return to port  .
  #'VMSTools::mergeEflalo2Tacsat function 
  
    tacsatp <- mergeEflalo2Tacsat(eflalo,tacsat)
    tacsatp <- data.frame(tacsatp)
  
  
  
  # Filter TACSAT data with assigned EFLALO fishing trips identifiers ----
   
  
    # Save not merged tacsat data
    # Subset 'tacsatp' where 'FT_REF' equals 0 (not merged)
  
    tacsatpmin <- subset(tacsatp, FT_REF == 0)
    
    #' Attention: Check the number of records that were not assigned with a Fishing trip identifier from EFLALO. 
    #' A large proportion of VMS records matched with a FT_REF value (FT_REF == 0) indicates something wrong 
    #' Review dates/time fields content and formats both in EFLALO and TACSAT 
    
    cat(sprintf("%.2f%% of of the tacsat data did not merge\n", (nrow(tacsatpmin) / (nrow(tacsatpmin) + nrow(tacsatp))) * 100))
  
    # Save 'tacsatpmin' to a file named "tacsatNotMerged<year>.RData" in the 'outPath' directory
    save(
      tacsatpmin,
      file = file.path(outPath, paste0("tacsatNotMerged", year, ".RData"))
    )
    
    
    
    # Subset TACSAT (tacsatp)  records  with Fishing Trip identifiers assigned 
    #' FT_REF distinct to  0 , measn records where successfully assigned with FT_REF identifier
    #' Attention: Only the data with assigned FT_REF is retained for further analysis
    
    tacsatp <- subset(tacsatp, FT_REF != 0)
    
  #'----------------------------------------------------------------------------
  # 2.2.2 Assign EFLALO - Fishing Trip information ( e.g. gear and length ) to TACSAT records  ----
  #'----------------------------------------------------------------------------
   
    #'----------------------------------------------------------------------------
    # 2.2.2.1 Assign Fishing Trip and Vessel Details at Trip Level
    #'----------------------------------------------------------------------------
    #' The gear , mesh size, used during a fishing trip ,  the ICES rectangle reported and 
    #' Vessel Characteristics are assigned to each VMS record part of a Fishign Trip. 
    #' Attention: Over 12 m commonly fish in several ICES Rectangles during a trip and 
    #' also could  use different gears during a trip. See section 2.2.2.2
    
    # Define the columns to be added
    cols <- c("LE_GEAR", "LE_MSZ", "VE_LEN", "VE_KW", "LE_RECT", "LE_MET", "LE_WIDTH", "VE_FLT", "VE_COU")
    
    # Use a loop to add each column
    for (col in cols) {
      # Match 'FT_REF' values in 'tacsatp' and 'eflalo' and use these to add the column from 'eflalo' to 'tacsatp'
      tacsatp[[col]] <- eflalo[[col]][match(tacsatp$FT_REF, eflalo$FT_REF)]
    }
    
  
      #'----------------------------------------------------------------------------
      # 2.2.2.2 Assign to TACSAT the Fishing Trips using more than one gear and fishing in several ICES Rectangles   
      #'----------------------------------------------------------------------------
      #' For trips using more than one fear ( mesh size/metier) and fishign in more than one ICES Rectangles
      #' The gears and ICES Rectangle are assigned to TACSAT using the LE_CDAT date (Fishing-Log Event Date) 
      #' that match the TACSAT/VMS date/time record date . 
      #' Attention: It is common that VMS records part of a fishing trip , do not have match the dates
      #' recorded in LE_CDAT attribute. The function "trip_assign" ensure to assign the "most used" gear or
      #' "most visited" ICES rectangles to the TACSAT records with not matchLE_CDAT within a trip. This ensure 
      #' all TACSAT records part of Fishin Trip have an assigned value. 
    
      tacsatpa_LE_GEAR <- trip_assign(tacsatp, eflalo, col = "LE_GEAR", trust_logbook = T)
      tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_GEAR$FT_REF,], tacsatpa_LE_GEAR), fill = T)
      
      tacsatpa_LE_MSZ <- trip_assign(tacsatp, eflalo, col = "LE_MSZ", trust_logbook = T)
      tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MSZ$FT_REF,], tacsatpa_LE_MSZ), fill = T)
      
      tacsatpa_LE_RECT <- trip_assign(tacsatp, eflalo, col = "LE_RECT", trust_logbook = T)
      tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_RECT$FT_REF,], tacsatpa_LE_RECT), fill = T)
      
      tacsatpa_LE_MET <- trip_assign(tacsatp, eflalo, col = "LE_MET", trust_logbook = T)
      tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MET$FT_REF,], tacsatpa_LE_MET), fill = T)
      
      if("LE_WIDTH" %in% names(eflalo)){
        tacsatpa_LE_WIDTH <- trip_assign(tacsatp, eflalo, col = "LE_WIDTH", trust_logbook = T)
        tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_WIDTH$FT_REF,], tacsatpa_LE_WIDTH), fill = T)
      }
      
      
      
      tacsatp <- as.data.frame(tacsatp)
      
      # Save 'tacsatp' to a file named "tacsatMerged<year>.RData" in the 'outPath' directory
      save(
        tacsatp,
        file = file.path(outPath, paste0("tacsatMerged", year, ".RData"))
      )
      
  
  #'----------------------------------------------------------------------------
  # 2.1.4 Define activity                                                   ----
  #'----------------------------------------------------------------------------
  # Calculate time interval between points
  tacsatp <- intvTacsat(tacsatp, level = "trip", fill.na = TRUE)
  
  # Reset values that are simply too high to 2x the regular interval rate  
  tacsatp$INTV[tacsatp$INTV > intvThres] <- 2 * intvThres
  
  # Assume that pings with NA in INTV has the normal interval value
  tacsatp$INTV[is.na(tacsatp$INTV)] <- intvThres
 
  # Remove points with NA's in them in critical places
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
  diag.plot
  
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
  
 if (visualInspection == TRUE){
    storeScheme <-
      ac.tac.anal(
        subTacsat,
        units = "year",
        analyse.by = "LE_L5MET",
        identify = "means")
 }else  {
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
    storeScheme$means[which(storeScheme$LE_GEAR == "OTM")] <- c("-9 -3 0 3 9")
    storeScheme$means[which(storeScheme$LE_GEAR == "MIS")] <- c("-9 -3 0 3 9")
    storeScheme$means[which(storeScheme$LE_GEAR == "SSC")] <- c("-9 0 9")
    storeScheme$means[which(storeScheme$LE_GEAR == "LLD")] <- c("-9 0 9")
    storeScheme$means[which(storeScheme$LE_GEAR == "LLS")] <- c("-9 0 9")
    storeScheme$means[which(storeScheme$LE_GEAR == "PTB")] <- c("-10 -3 0 3 10")
    storeScheme$means[which(storeScheme$LE_GEAR == "DRB")] <- c("-10 0 10")
    storeScheme$means[which(storeScheme$LE_GEAR == "HMD")] <- c("-9 0 9")
    storeScheme$peaks[which(storeScheme$LE_GEAR == "SSC")] <- 3
    storeScheme$peaks[which(storeScheme$LE_GEAR == "LLD")] <- 3
    storeScheme$peaks[which(storeScheme$LE_GEAR == "LLS")] <- 3
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
  
  summary_table <- subTacsat %>%
    filter(SI_STATE == "f") %>%
    group_by(LE_L5MET) %>%
    dplyr::summarise(
      min_SI_SP = min(SI_SP),
      max_SI_SP = max(SI_SP)
    )
  print(summary_table)
  message(paste("These are your maximum and minimum fishing speeds (in knots), as defined by the autodetection algorithm, for ", year, ". Check they look realistic!", sep  =""))

    # Write the summary table to a text file
  cat("\n\nYear:", year, "\n", file = file.path(outPath, "fishing_speeds_by_metier_and_year.txt"), append = TRUE)
  write.table(summary_table, file = file.path(outPath, "fishing_speeds_by_metier_and_year.txt"), 
              append = TRUE, sep = "\t", row.names = FALSE, col.names = !file.exists(file.path(outPath, "fishing_speeds_by_metier_and_year.txt")))
  cat("\n", file = file.path(outPath, "fishing_speeds_by_metier_and_year.txt"), append = TRUE)
                                
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
  
  # This next step is retained from previous code. The new function to assign
  # fishing activity states does not use "h" (harbour), but if you are using your
  # own workflow code, you may wish to look for this. We do not recommend it.
  #
  # Set fishing sequences with hauling in the middle to "f" ##################
  #
  # idx <-
  # which(
  #   tacsatp$SI_STATE[2:(nrow(tacsatp) - 1)] == "h" &
  #     tacsatp$SI_STATE[1:(nrow(tacsatp) - 2)] == "f" &
  #     tacsatp$SI_STATE[3:(nrow(tacsatp))    ] == "f" &
  #     tacsatp$VE_REF[2:(nrow(tacsatp) - 1)] == tacsatp$VE_REF[1:(nrow(tacsatp) - 2)] &
  #     tacsatp$VE_REF[2:(nrow(tacsatp) - 1)] == tacsatp$VE_REF[3:(nrow(tacsatp))]
  #  ) + 1
  # tacsatp$SI_STATE[idx] <- "f"
  
  save(
    tacsatp,
    file = file.path(outPath, paste0("tacsatActivity", year, ".RData"))
  )
  
  message("Defining activity completed")
  
  

  # 2.2 Dispatch EFLALO landings at VMS position scale ( SplitAmongPing)
  # ------------------------------------------------------------------------
    
  #'----------------------------------------------------------------------------
  # 2.2.1 continued, filter out invalid metier level 6 codes                           
  #'----------------------------------------------------------------------------
    kept <- nrow(tacsatp)
    removed <- nrow(tacsatp %>% filter(LE_MET %!in% valid_metiers))
    tacsatp <- tacsatp %>% filter(LE_MET %in% valid_metiers)
    cat(sprintf("%.2f%% of of the tacsatp removed due to invalid metier l6 \n", (removed / (removed + kept) * 100)))
  
  # 2.2.2 Dispatch landings of merged eflalo at the ping scale
  # -------------------------------------------------
    
    # Input data preparation
    
    
    ## 2.2.2.1 Creates EFLALO LE_KG_TOT and LE_EURO_TO if not created yet. 
    
      #' Attention: Only applies if you have EFLALO LE_KG and LE_EURO by species . If not species columns and 
      #' LE_KG_TOT and LE_KG_EURO are already calculated this section wont change the data 
    
      # Get the indices of columns in eflalo that contain "LE_KG_" or "LE_EURO_"
      idx_kg <- grep("LE_KG_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_KG_TOT")])
      idx_euro <- grep("LE_EURO_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_EURO_TOT")])
      
      # Calculate the total KG and EURO for each row
      if("LE_KG_TOT" %!in% names(eflalo))
        eflalo$LE_KG_TOT <- rowSums(eflalo[, idx_kg], na.rm = TRUE)
      if("LE_EURO_TOT" %!in% names(eflalo))
        eflalo$LE_EURO_TOT <- rowSums(eflalo[, idx_euro], na.rm = TRUE)
  
      # Remove the columns used for the total calculation
      eflalo <- eflalo[, -c(idx_kg, idx_euro)]
      
      
    ## 2.2.2.2 Retain EFLALO/LB records with related TACSAT/VMS records in EFLALOM ( Eflalo Merged)
      
      #' Only records in EFLALOM are taking forward for further analysis 
      #' The EFLALO/Logbook records with not related VMS records are retained in EFLALONM ( Eflalo Not Merged)
      #' Attention: Only Logbook records with related VMS ( Fishing or not fishing ) are retained for further analysis
      
      eflaloM  <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))
      eflaloNM <- subset(eflalo, !FT_REF %in% unique(tacsatp$FT_REF))

      #' Attention: Check the number of records in EFLALOM and EFLALONM are reasonable. 
      #' e.g. Considering the EFLALO records are mainly related to fleet of over 12 meters vessels
      #' the majority of the EFLALO/LB records must have related VMS records
       
      message(sprintf("%.2f%% of the eflalo data not in tacsat\n", (nrow(eflaloNM) / (nrow(eflaloNM) ))))

      
    ## 2.2.2.3 Filter the 
      
  # Convert SI_STATE to binary (0/1) format
  tacsatp$SI_STATE <- ifelse(tacsatp$SI_STATE == "f", 1, 0)
  
  # Filter rows where SI_STATE is 1
  tacsatEflalo <- tacsatp[tacsatp$SI_STATE == 1,]
  
  tacsatp <- tacsatp[!is.na(tacsatp$INTV),]
  
  # Distribute landings among pings, first by day, metier and trip; then by metier and trip; then by trip
  tacsatEflalo <- splitAmongPings2(tacsatp, eflalo)
  
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatEflalo$FT_REF, "Y", "N")

  
  save(
    tacsatEflalo,
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )

  save(
    eflalo,
    file = file.path(outPath, paste0("/cleanEflalo", year, ".RData"))
  )
  
  
  print("Dispatching landings completed")
  
  
  
  print("")
  
}



#'------------------------------------------------------------------------------
# 2.3 Add information to tacsatEflalo                                     ----
#'------------------------------------------------------------------------------
# If you already have cleaned tacsatEflalo files elsewhere, 
# change file location below, and make sure data is called tacsatEflalo
# Loop trough years to submit
for(year in yearsToSubmit){
  print(paste0("Start loop for year ",year))
  load(file = paste0(outPath,"tacsatEflalo",year,".RData"))
  
  # 2.3.1 Assign c-square, year, month, quarter, area and create table 1
  # ------------------------------------------------------------------
  # Add habitat and bathymetry to the tacsatEflalo file
  tacsatEflalo <- tacsatEflalo |> 
    sf::st_as_sf(coords = c("SI_LONG", "SI_LATI"), remove = F) |> 
    sf::st_set_crs(4326) |> 
    st_join(eusm, join = st_intersects) |> 
    st_join(bathy, join = st_intersects) |> 
    mutate(geometry = NULL) |> 
    data.frame()
  
  # Calculate the c-square based on longitude and latitude
  tacsatEflalo$Csquare <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
  
  # Extract the year and month from the date-time
  tacsatEflalo$Year <- year(tacsatEflalo$SI_DATIM)
  tacsatEflalo$Month <- month(tacsatEflalo$SI_DATIM)
  
  # Calculate the kilowatt-hour and convert interval to hours
  tacsatEflalo$kwHour <- tacsatEflalo$VE_KW * tacsatEflalo$INTV / 60
  tacsatEflalo$INTV <- tacsatEflalo$INTV / 60
  
  # Add the calculated gear width to each fishing point
  tacsatEflalo$GEARWIDTH <- add_gearwidth(tacsatEflalo)
  
  # Add swept area(m2) for each point in the tacsateflalo
  tacsatEflalo$SA_M2 <- tacsatEflalo$GEARWIDTH * tacsatEflalo$INTV * tacsatEflalo$SI_SP * 1852
  
  # Check if logical
  tacsatEflalo[,.(min = min(GEARWIDTH), max = max(GEARWIDTH)), by = .(LE_MET)]
  
    
  save(
    tacsatEflalo,
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )
  

}


# Housekeeping
rm(speedarr, tacsatp, tacsatEflalo,
    eflalo, eflaloM, eflaloNM)


#----------------
# End of file
#----------------
