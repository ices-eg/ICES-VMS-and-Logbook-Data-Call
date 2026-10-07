
    #'----------------------------------------------------------------------------
    # 2.3 Define TACSAT - Fishing Effort and  Activity status                ----
    #'----------------------------------------------------------------------------
    
          
    # 2.3.1 Calculate time interval between points
    #--------------------------------------------
    #' Calculate the time different betwee 2 consecutive TACSAT/VMS Records
    #' The functions return the difference in MINUTES for each TACSAT Records
    #' Attention: The function output the time difference in MINUTES in the INTV field
    #' 
    #' FUNCTION REQUIRED: global::intvTacsat
    
      
    tacsatp <- intvTacsat(tacsatp, level = "trip", fill.na = TRUE)
    
    # TACSAT INTV assigned with too high time difference
    #-----------------------------------------------------
    #' TACSAT records with INTV too high  ( defined in intvThres in 0_global.R )
    #' are assigned with a value 2x the regular interval rate  ( intvThres )
    #' Attention: This value is used to calculate the Fishing Effort indicator, be sure the 
    #' values are realistic with Fishing Operation duration of your fleet
    #' 
    #' GLOBAL VARIABLE REQUIRED: intvThres
    
    tacsatp$INTV[tacsatp$INTV > intvThres] <- 2 * intvThres
    
    # TACSAT records with not assigned INTV 
    #--------------------------------------
    #'The TACSAT Records with not assigned INTV values take the value set in intvThres
    
    tacsatp$INTV[is.na(tacsatp$INTV)] <- intvThres
   
    # 2.3.2 Remove TACSAT points with no values (NA) in them in ESSENTIAL VMS ATTRIBUTES
    #-----------------------------------------------------------------------------
    #' TACSAT records with not vessel reference (VE_REF), latitude (SI_LAT), longitude (SI_LONG) ,
    #' date (SI_DATE) or speed (SI_SP) cannot be used for further analysis so are excluded from TACSATP records 
    #' Attention: Check the records missing essential information are not a significant part of the total TACSAT records
   
  
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
    
 
    
 
    # 2.3.3 Define TACSAT record vessel location Fishing Status ( Fishing or Steaming) 
    #-----------------------------------------------------------------------------
    #' This section define if a TACSAT/VMS records represents a fishign vessel location during steaming
    #' to fishing grounds/port or engage in a fishing operation ( e.g. set, haul , trawl)
    #' The methods analyse the vessel speed profiles and based in analyst inspection 
    #' identify the speed ranges for steaming or engaged in fishing operations. 
    #' For simplicity , the current code aims to distinguish between vessel positions representing steaming or 
    #' fishing ( trawling for active gears and setting/hauling for passive gears)
    #'
    #' Attention: Since majority of the Datacall submitter do not run this section into the years LOOP. We highly recommend: 
    #' 1) Run this code section as a separated ad-hoc analysis. Speed profiles and ranges shouldn't vary significantly 
    #' among years. And variability is often decimals of a knot, which is not significant for activity detection.
    #' 2) Once you identify the speed ranges for your fleet metier category ( recommended METIER Level 5 
    #' or length based categories, etc), use the speed ranges matrix you created to assign 'f' ( Fishing) or 's' (Steaming)
    #' to each TACSAT record in the  SI_STATE field. 
    #' 3) Once you have your TACSATP with a SI_STATE assigned you could follow the Workflow at Section  2.3.4
    
    # 2.3.3.1 Define speed thresholds associated with fishing for gears
      
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
            panel.background = element_blank()) +
          scale_fill_manual(values = c("#000000", "#FCCF3F", "#FF0000", "#00FF00", "#0000FF",
                                       "#FF00FF", "#808080", "#800000", "#808000",
                                       "#008000", "#800080", "#008080", "#000080", "#666699", "#808080",
                                       "#003366", "#CCA099", "#333300", "#993300", "#993366", "#333399",
                                       "#333333"))
        
        ggsave(diag.plot, filename = file.path(outPath, paste0("SpeedHistogram_", year, ".jpg")))
        diag.plot
        
        # start by correctly formatting the level 5 metier
        tacsatp$LE_L5MET <-  sapply(strsplit(tacsatp$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  
        tacsatp$LE_L5MET2 <- gsub("^([A-Z]+_[A-Z]+)_.*$", "\\1", tacsatp$LE_MET)
        
        
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
        # Extract the gear part from LE_MET
        gear_from_met <- sapply(strsplit(tacsatp$LE_MET, "_"), `[`, 1)
        
        # Identify records where LE_GEAR matches the first part of LE_MET
        matching_records <- tacsatp$LE_GEAR == gear_from_met
        
        # Print summary of records to be removed
        removed_count <- sum(!matching_records)
        total_count <- length(matching_records)
        cat("Removing", removed_count, "out of", total_count, "records (", 
            round(removed_count/total_count*100, 2), "%)\n")
        
        # Keep only the records where the gear codes match
        tacsatp <- tacsatp[matching_records, ]
        
        
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
