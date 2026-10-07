
    
        
        
        
      
      # 2.3.4 Remove the records with invalid METIER LEVEL 6 codes                           
      #----------------------------------------------------------------------------
      #' The 'valid_metiers' list is created in 0_global.R and takes the valid list from EU RCG's   
      #' This facilitate the standardization of metiers and posterior aggregation of Datacall submissions with data 
      #' different countries. 
      #' 
      #' GLOBAL VARIABLE REQUIRED: valid_metiers 
      
      kept <- nrow(tacsatp)
      removed <- nrow(tacsatp %>% filter(LE_MET %!in% valid_metiers))
      
      tacsatp <- tacsatp %>% filter(LE_MET %in% valid_metiers)
      
      # Check the number of TACSAT records with invalid METIER Level 6 is not significantly 
      cat(sprintf("%.2f%% of of the tacsatp removed due to invalid metier l6 \n", (removed / (removed + kept) * 100)))    
  

  # 2.4 Dispatch EFLALO landings at VMS position scale ( SplitAmongPing)
  # ------------------------------------------------------------------------
  #' This is an essential analysis step that distribute the landings reported in fishers logbooks by 
  #' VMS vessel position location identified as engaged in fishing location . 
  #' The dispatch of landings among VMS records ( pings) requires EFLALO and TACSATP data preparation 
  #' before to input in the SplitAmongPing function. 
  #' 
  #' FUNCTION REQUIRED: VMSTools::SplitAmongPings
    
  
  
    # TACSAT and EFLALO data preparation for SplitAmongPings function
    
    
    ## 2.4.1 Creates EFLALO LE_KG_TOT and LE_EURO_TO if not created yet. 
    
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
      
      
    # 2.4.2 Retain EFLALO/LB records with related TACSAT/VMS records in EFLALOM ( Eflalo Merged)
    #------------------------------------------------------------------------------------------
    
    #' Only records in EFLALOM are taking forward for further analysis 
    #' The EFLALO/Logbook records with not related VMS records are retained in EFLALONM ( Eflalo Not Merged)
    #' Attention: Only Logbook records with related VMS ( Fishing or not fishing ) are retained for further analysis
      
      eflaloM  <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))
      eflaloNM <- subset(eflalo, !FT_REF %in% unique(tacsatp$FT_REF))

      #' Attention: Check the number of records in EFLALOM and EFLALONM are reasonable. 
      #' e.g. Considering the EFLALO records are mainly related to fleet of over 12 meters vessels
      #' the majority of the EFLALO/LB records must have related VMS records
       
      message(sprintf("%.2f%% of the eflalo data not in tacsat\n", (nrow(eflaloNM) / (nrow(eflaloNM) ))))

      
    # 2.4.3 Filter the TACSAT records identified as vessel positions engaged in Fishing Operations
    #------------------------------------------------------------------------------------------------
    #' Attention: Several fishing trips will lost part or the total of their VMS records.
    #' This means several EFLALO Fishing Trips  could be input in SplitAmongPings with reduced or not related TACSAT records.
    #' If CONSERVE option is not used the landings related to these Fishing Trips will be excluded and the 
    #' landings values ( weigh and sales value) is not considered in the final output. 
    #' Thus, it can be expected a significant difference between the Total Landings values (KG, EURO) in EFLALOM
    #' in comparison to the output of SplitamongPings function.
    #' Consider the CONSERVE option in SplitAmongPings if appropriate following expert criteria.
    #' Also you can investigate the reason of the significant VMS records missed due to not been identified as fishing. 
    #' e.g. Narrow fishing speed ranges, etc. 
      
    
      # Convert SI_STATE to binary (0/1) format
      tacsatp$SI_STATE <- ifelse(tacsatp$SI_STATE == "f", 1, 0)
      
      # Filter TACSAT records which SI_STATE is fishing (SI_STATE ==1 )
      tacsatp <- tacsatp[tacsatp$SI_STATE == 1,]
      
    # 2.4.4 Filter TACSAT records which SI_STATE is not NA.
    #' No INTV values means not Fishign effort allocation , so cannot be used in further analysis. 
      
      tacsatp <- tacsatp[!is.na(tacsatp$INTV),]
      
      
      
    # 2.4.5 Distribute landings among pings
    #---------------------------------------
    #' Run the function SplitAmongPings using EFLALOM and TACSAT with valid fishing positions.
    #' Read the documentation of VMSTools::SplitAmongPings and the ICES SFD 2025 report 
    #' for details on the function settings, match levels and more options. 
    #' CONSERVE will retain the landings from ELALO records with not related VMS records. These VMS records could exists 
    #' with the original raw data but were lost due to Quality Control cleaning  or activity identification process.
    #' If you use CONSERVER and  want to use all EFLALO records use EFLALO instead EFLALOM. 
      if((sum(tacsatp$INTV == 0) > 0) || (sum(is.na(tacsatp$INTV)) > 0)){
        message(sprintf("%.2f%% of the intervals in tacsatp contain NA's or zeros and these records have been discarded.\n", 
                        (sum(tacsatp$INTV == 0) + sum(is.na(tacsatp$INTV))) / nrow(tacsatp) * 100))
        tacsatp <- tacsatp %>% filter(!is.na(INTV) & INTV > 0)
      }
      
      tacsatEflalo <-  splitAmongPings(
                          tacsat = tacsatp,
                          eflalo = eflaloM,
                          variable = "all",
                          level = c("day","ICESrectangle","trip"),
                          conserve = TRUE, 
                          by = "INTV" ) 
      
      eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatEflalo$FT_REF, "Y", "N")
    
      #' Intermediate data format and save:
      #' Save 'tacsatEflalo' to a file named "tacsatEflalo<year>.RData" in the 'outPath' directory
      #' Save 'eflalo' to a file named "eflalo<year>.RData" in the 'outPath' directory
      
      
      save(
        tacsatEflalo,
        file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
      )

      save(
        eflalo,
        file = file.path(outPath, paste0("/processedEflalo", year, ".RData"))
      )
    
    
      print("Dispatching landings completed")
  
  
  
  print("")
  
}  ## END OF THE 1st YEAR LOOP 



#'------------------------------------------------------------------------------
# 2.5 Add additional information to tacsatEflalo                             ----
#'------------------------------------------------------------------------------
#' Add additional to TACSATEFLALO dataset. 
#' Habitat data
#' Depth Data 
#' Refinement of effort data values prepared for data aggregation
 



# Loop trough years to submit

for(year in yearsToSubmit){
  
  print(paste0("Start loop for year ",year))
  
  # Load tacsatEflalo output from outPath location 
  load(file = paste0(outPath,"tacsatEflalo",year,".RData"))
  
  # 2.5.1 Add Habitat and Bathymetry data values to TACSATEFLALO 
  # ------------------------------------------------------------------
  #' Habitat and depth values are extracted from EU Habitat Map and GEBCO Bathymetry sources
  #' The Habitat class and Depth ranges are used later as aggregation classes
  #' Ensure the sf::sf_use_s2(FALSE) function in 0_global.R is run . More detail in 0_global.R
  #' GLOBAL VARIABLE REQUIRED: "eusm" and "bathy"  variables created in 0_global.R
 
  tacsatEflalo <- tacsatEflalo |> 
    sf::st_as_sf(coords = c("SI_LONG", "SI_LATI"), remove = F) |> 
    sf::st_set_crs(4326) |> 
    st_join(eusm, join = st_intersects) |> 
    st_join(bathy, join = st_intersects) |> 
    mutate(geometry = NULL) |> 
    data.frame()
  
  # 2.5.2 Calculate the C-SQUARE by TACSAT record based on longitude and latitude of VMS data
  #' FUNCTION REQUIRED: VMSTools::CSquare
  
  tacsatEflalo$Csquare <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
  
  # 2.5.3 Extract the year and month from the date-time
  #' FUNTION REQUIRED: year and month from LUBRIDATE 
  
  tacsatEflalo$Year <- year(tacsatEflalo$SI_DATIM)
  tacsatEflalo$Month <- month(tacsatEflalo$SI_DATIM)
  
  # 2.5.4 Calculate the kilowatt-hour and convert interval to hours
  #' Attention: This step transform the TACSAT data value in INTV field
  #' The INTV include the fishing effort by TACSAT/VMS position. It was calcualted in step 2.3.1 
  #' The INTV calculation in 2.3.1 is given in minute. This step transform it in hours. 
  #' If you calculated INTV elsewhere in another unit ( e.g. hours ) , modify or skip this transformation
 
  
  tacsatEflalo$kwHour <- tacsatEflalo$VE_KW * tacsatEflalo$INTV / 60
  tacsatEflalo$INTV <- tacsatEflalo$INTV / 60
  
  # 2.5.5  Calculated gear width to each fishing point
  #' Calculate the gear width using ICES R Package SFDSAR. Methods estimates the gear width based on the 
  #' vessel length or vessel engine power based on the metier used.
  #' Attention: The output provides the gear width in KILOMETERS If you get the GEAR WIDTH information 
  #' using other method or you provide the gear width , ensure the values are supplied in KILOMETERS before submission. 
  #' Attention: If user prefer to provide its own gear width it must be provided in a FIELD called LE_GEARWIDTH 
  #' 1) The function will check if LE_GEARWIDTH field exists and has values , so will prioritise these values to be assigned 
  #' 2) If LE_GEARWIDTH do not exist or is NA , will assign the modeled gear width using benthis methods in ICES  SFDSAR Package
  #' 3) If not possible to obtain model gear widths , teh function assigns the default average gear width by metier provided in benthis
  #' metier auxiliary lookup table available in ICESVMS R PAckage. 
  #'   
  #'FUNCTION REQUIRED: global::add_gearwidth()

  tacsatEflalo$GEARWIDTHKM <- add_gearwidth(tacsatEflalo)
  
# 2.5.6  Calculates Swept Area (Km2) for each record in the TACSATEFLALO
#' Calculate the area swept by mobile bottom contact gears in Km2. 
#' 
#' Different gear types require different swept area calculations:
#' - Trawls: SA = gear_width * time * speed * 1.852 (standard towing calculation)
#' - Danish seine (SDN): Uses rope loop geometry, SA = (time / 2.591234) * gear_width^2 / pi / 4
#' - Scottish seine (SSC): Uses rope loop geometry with 1.5 multiplier for the "splitting" phase
#' 
#' For seines, GEARWIDTHKM represents total rope length (may exceed 6 km), not net width.
#' In case of emergency, use standard haul durations from Eigaard et al. (2016): Danish seine 2.59h, Scottish seine 1.91h.
#' 
#' Attention: Remember that GEARWIDTH must be in KILOMETRES, INTV in HOURS, and SI_SP in KNOTS.
#' See WGSFD 2025 Report for full discussion of this change. ICES Scientific Reports https://doi.org/10.17895/ices.pub.3073475

tacsatEflalo$SA_KM2 <- case_when(
  # Danish seine - rope loop geometry
  tacsatEflalo$LE_GEAR == "SDN" ~ danish_seine_contact(
    fishing_hours = tacsatEflalo$INTV,
    gear_width = tacsatEflalo$GEARWIDTHKM,
    fishing_speed = tacsatEflalo$SI_SP
  ),
  # Scottish seine - rope loop geometry with splitting multiplier
  tacsatEflalo$LE_GEAR == "SSC" ~ scottish_seine_contact(
    fishing_hours = tacsatEflalo$INTV,
    gear_width = tacsatEflalo$GEARWIDTHKM,
    fishing_speed = tacsatEflalo$SI_SP
  ),
  # All other gears - standard trawl calculation
  TRUE ~ trawl_contact(
    fishing_hours = tacsatEflalo$INTV,
    gear_width = tacsatEflalo$GEARWIDTHKM,
    fishing_speed = tacsatEflalo$SI_SP
  )
)
  # Check if the minimum and maximum gear width are reasonable size by METIER
  
  tacsatEflalo[,.(min = min(GEARWIDTHKM), max = max(GEARWIDTHKM)), by = .(LE_MET)]
  
  
  
  
  #' FINAL data  save:
  #' Save 'tacsatEflalo' to a file named "tacsatEflalo<year>.RData" in the 'outPath' directory
  #' This is the final output of WORKFLOW BLOCK 2.EFLALO_TACSAT_ANALYSIS.R
    
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
