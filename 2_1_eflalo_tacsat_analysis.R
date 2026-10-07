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
  #'
  #'FUNCTION REQUIRED : VMSTools::mergeEflalo2Tacsat 
  
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
  
    #' Intermediate data save:
    #' Save 'tacsatpmin' to a file named "tacsatNotMerged<year>.RData" in the 'outPath' directory
    
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
      #'        
      #' FUNCTION REQUIRED: global::trip_assign
    
      tacsatpa_LE_GEAR <- trip_assign(tacsatp, eflalo, col = "LE_GEAR",  haul_logbook = F)
      tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_GEAR$FT_REF,], tacsatpa_LE_GEAR), fill = T)
      
      tacsatpa_LE_MSZ <- trip_assign(tacsatp, eflalo, col = "LE_MSZ",  haul_logbook = F)
      tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MSZ$FT_REF,], tacsatpa_LE_MSZ), fill = T)
      
      tacsatpa_LE_RECT <- trip_assign(tacsatp, eflalo, col = "LE_RECT",  haul_logbook = F)
      tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_RECT$FT_REF,], tacsatpa_LE_RECT), fill = T)
      
      tacsatpa_LE_MET <- trip_assign(tacsatp, eflalo, col = "LE_MET",  haul_logbook = F)
      tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MET$FT_REF,], tacsatpa_LE_MET), fill = T)
      
      if("LE_WIDTH" %in% names(eflalo)){
        tacsatpa_LE_WIDTH <- trip_assign(tacsatp, eflalo, col = "LE_WIDTH",  haul_logbook = F)
        tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_WIDTH$FT_REF,], tacsatpa_LE_WIDTH), fill = T)
      }
      
      
      
      
      
      #' Intermediate data format and save:
      #' Save 'tacsatp' to a file named "tacsatMerged<year>.RData" in the 'outPath' directory
      
      tacsatp <- as.data.frame(tacsatp)
      
      save(
        tacsatp,
        file = file.path(outPath, paste0("tacsatMerged", year, ".RData"))
      )
      
 }   
