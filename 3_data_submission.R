#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 3: Construct and collect tables                                         ----
#
#'------------------------------------------------------------------------------

# Loop through years to submit
for(year in yearsToSubmit){
  
  # load data
  load(file = paste0(outPath,paste0("/processedEflalo",year,".RData")))
  load(file = paste0(outPath,paste0("/tacsatEflalo",year,".RData")))  
  #'----------------------------------------------------------------------------
  # 3.1 Create table 2                                                    ----
  #'----------------------------------------------------------------------------
  # Extract the year and month from the date-time column
  eflalo$Year <- year(eflalo$FT_LDATIM)
  eflalo$Month <- month(eflalo$FT_LDATIM)
  
  # Set interval to 1 day for later caculation of kwDays
  eflalo$INTV <- 1
  
  # Create a record variable for aggregation of records per vessel
  eflalo$record <- 1
  
  # Aggregate the dummy variable by VE_COU, VE_REF, and LE_CDAT
  res <- aggregate(
    eflalo$record,
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
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatEflalo$FT_REF, "Y", "N")
  
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
  
  
  # Save table2 
  save(
    table2,
    file = file.path(outPath, "table2.RData" )
  )
  
  message(glue ("Table 2 for year {year} is completed") )
  
  
  #'----------------------------------------------------------------------------
  # 3.2   Create table 1                                                  ----
  #'----------------------------------------------------------------------------
    tacsatEflalo <- data.frame(tacsatEflalo)
  
  # Define the record type
  RecordType <- "VE"
  
  # Define the columns to be included in the table
  cols <- c(
    "VE_REF", "VE_COU", "Year", "Month", "Csquare", "MSFD_BBHT", "depth", "LE_GEAR",
    "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT",
    "GEARWIDTHKM", "SA_KM2")
  
  
  # Create or append to table1 based on the year
  if (year == yearsToSubmit[1]) {
    table1 <- cbind(RT = RecordType, tacsatEflalo[,cols])
  } else {
    table1 <- rbind(table1, cbind(RT = RecordType, tacsatEflalo[,cols]))
  }
  
  # Save
    save(
    table1,
    file = file.path(outPath, "table1.RData" )
  )
  
  message(glue("Table 1 for year {year} is completed") )
}


# Check if TABLE 1 fishing hours > 0

table( table1$INTV > 0  )

# Check if TABLE 2 fishing days  > 0

table( table2$INTV > 0  )

# End of QC checks

#'------------------------------------------------------------------------------
# 3.2.1 Load TABLE 1 (VMS) and TABLE 2 (logbook)                            ----
#'------------------------------------------------------------------------------
load(file = paste0(outPath, "/table1.RData"))
load(file = paste0(outPath, "/table2.RData"))



#'------------------------------------------------------------------------------
# 3.2.2 Replace vessel ID by an anonymized ID column                        ----
#'------------------------------------------------------------------------------
# New field added for the 2020 data call including unique vessels id's  
# This vessel id is used to calculate unique vessels in a c-square and 
VE_lut <- data.frame(VE_REF = unique(c(table1$VE_REF, table2$VE_REF)))
fmt <- paste0("%0", floor(log10(nrow(VE_lut))) + 1, "d")
VE_lut$VE_ID <- paste0(table1$VE_COU[1], sprintf(fmt, 1:nrow(VE_lut))) # use relevant country code!

# join onto data tables
table1 <- left_join(table1, VE_lut)
table2 <- left_join(table2, VE_lut)


#'------------------------------------------------------------------------------
# 3.3 Assign the vessel length category based in DATSU vocabulary           ----
#'------------------------------------------------------------------------------
#  Use of the "icesVocab" ICES developed R package that fetch the DATSU vocabulary values for a given vocabulary theme #

# Get the values accepted in this vocabulary dataset
vlen_ices <- getCodeList("VesselLengthClass") ### Get DATSU Vocabulary list for selected data set


# Filter the vessel length categories required  by  ICES VMS& Logbook datacall 
vlen_icesc =  vlen_ices%>%
  filter ( Key %in% c("VL0006", "VL0608", "VL0810", "VL1012", "VL1215" ,"VL1518", "VL1824" ,"VL2440" ,"VL40XX"))%>%
  dplyr::select(Key)%>%
  dplyr::arrange(Key)

# TABLE 1. Add the vessel length category using  LENGTHCAT field, aligned with VESSEL LENGTH categories selected from ICES Vocabulary 
table1$LENGTHCAT <-  table1$VE_LEN%>%cut(    breaks=c(0, 6, 8, 10, 12, 15, 18, 24, 40, 'inf' ), 
                                             right = FALSE    ,include.lowest = TRUE,
                                             labels =  vlen_icesc$Key 
)


# TABLE 2. Add the vessel length category using  LENGTHCAT field
table2$LENGTHCAT <-  table2$VE_LEN%>%cut(   breaks=c(0, 6, 8, 10, 12, 15, 18, 24, 40, 'inf' ), 
                                            right = FALSE    ,include.lowest = TRUE,
                                            labels =  vlen_icesc$Key 
)


#'------------------------------------------------------------------------------
# 3.4 Aggregate and summarise TABLE 1 and TABLE2                            ----
#'------------------------------------------------------------------------------


##--------------
## Save Table 1
##--------------

table1Save <- table1 %>%
  # Separate LE_MET into met4 and met5, dropping extra pieces
  separate(col = LE_MET, c("MetierL4", "MetierL5"), sep = '_', extra = "drop", remove = FALSE) %>%
  # Group by several variables
  group_by(RecordType = RT, CountryCode = VE_COU, Year, Month, Csquare, MetierL4, MetierL5, MetierL6 = LE_MET, VesselLengthRange = LENGTHCAT, Habitat = MSFD_BBHT, Depth = depth) %>%
  # Summarise the grouped data
  summarise(
    No_Records = n(),
    AverageFishingSpeed = mean(SI_SP),
    FishingHour = sum(INTV, na.rm = TRUE),
    AverageInterval = mean(INTV, na.rm = TRUE),
    AverageVesselLength = mean(VE_LEN, na.rm = TRUE),
    AveragekW = mean(VE_KW, na.rm = TRUE),
    kWFishingHour = sum(kwHour, na.rm = TRUE),
    SweptArea = sum(SA_KM2, na.rm = T),
    TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
    TotValue = sum(LE_EURO_TOT, na.rm = TRUE),
    NoDistinctVessels = n_distinct(VE_ID, na.rm = TRUE),
    AnonymizedVesselID = ifelse(n_distinct(VE_ID) < 3, paste(unique(VE_ID), collapse = ";"), 'not_required'),
    AverageGearWidth = mean(GEARWIDTHKM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Relocate NoDistinctVessels and AnonymizedVesselID before Csquare
  relocate(NoDistinctVessels, AnonymizedVesselID, .before = Csquare) %>%
  as.data.frame()

  table1Save <- as.data.frame(table1Save)

  
 ##--------------
 ## Save Table 2
 ##--------------

table2Save <- table2 %>%
  # Separate LE_MET into met4 and met5
  separate(col = LE_MET, c("MetierL4", "MetierL5"), sep = '_', remove = FALSE) %>%
  # Group by several variables
  group_by(RecordType = RT, CountryCode = VE_COU, Year, Month, ICESrectangle = LE_RECT, MetierL4, MetierL5, MetierL6 = LE_MET, VesselLengthRange = LENGTHCAT, VMSEnabled = tripInTacsat) %>%
  # Summarise the grouped data
  summarise(
    FishingDays = sum(INTV, na.rm = TRUE),
    kWFishingDays = sum(kwDays, na.rm = TRUE),
    TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
    TotValue = sum(LE_EURO_TOT, na.rm = TRUE),
    NoDistinctVessels = n_distinct(VE_ID, na.rm = TRUE),
    AnonymizedVesselID = ifelse(n_distinct(VE_ID) < 3, paste(unique(VE_ID), collapse = ";"), 'not_required'),
    .groups = "drop"
  ) %>%
  # Relocate NoDistinctVessels and AnonymizedVesselID before ICESrectangle
  relocate(NoDistinctVessels, AnonymizedVesselID, .before = ICESrectangle) %>%
  as.data.frame()
 #ignore the warnings - just the spare mesh sizes

# Save 
saveRDS(table1Save, paste0(outPath, "table1Save.rds"))
saveRDS(table2Save, paste0(outPath, "table2Save.rds"))


#'------------------------------------------------------------------------------
# 3.5   ICES DATSU VOCABULARY CHECKS BEFORE DATA SUBMISSION                 ----
#'------------------------------------------------------------------------------
## Get vocabulary for mandatory and fields with associated vocabulary using the DATSU API

### 3.5.0 Keep track of removed points -----------------------------------------
# Table 1
colnames <- c("RowsRemaining", "PercentageRemaining")
rownames <- c("Total", "csquaresEcoregion", "VesselLengthClass", "MetierL4", "MetierL5", "MetierL6", "CountryCodes")

remrecsTable1 <-
  as.data.frame(matrix(NA,
                       nrow = length(rownames), ncol = length(colnames),
                       dimnames = list(rownames, colnames)))

remrecsTable1["Total",] <- c(as.numeric(nrow(table1Save)), 100)

# Table 2
colnames <- c("RowsRemaining", "PercentageRemaining")
rownames <- c("Total", "ICESrectangles", "VesselLengthClass", "MetierL4", "MetierL5", "MetierL6", "VMSEnabled", "CountryCodes")

remrecsTable2 <-
  as.data.frame(matrix(NA,
                       nrow = length(rownames), ncol = length(colnames),
                       dimnames = list(rownames, colnames)))

remrecsTable2["Total",] <- c(as.numeric(nrow(table2Save)), 100)



# TABLE 1 ======================================================================

### 3.5.1 Check if C-Squares are within ICES Ecoregions ------------------------
csquares_d      <-  table1Save%>%
  dplyr::select('Csquare')%>%
  dplyr::distinct( )

csquares_dcoord <-  cbind ( csquares_d ,  CSquare2LonLat (csqr = csquares_d$Csquare ,degrees =  0.05)   )
valid_csquare   <-  csquares_dcoord%>%
  filter(SI_LATI >= 30 & SI_LATI <= 90  )%>%
  dplyr::select('Csquare')%>%
  pull()

table1Save      <-  table1Save %>%
  dplyr::filter(Csquare %in% valid_csquare)

# Add to remrecsTable1
remrecsTable1["csquaresEcoregion",] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total","RowsRemaining"])*100)


### 3.5.2 Check Vessel Lengths categories are accepted -------------------------

vlen_ices       <-  getCodeList("VesselLengthClass")
table ( table1Save$VesselLengthRange%in%vlen_ices$Key )  # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table1Save [ !table1Save$VesselLengthRange %in%vlen_ices$Key,]%>%
  dplyr::group_by(VesselLengthRange)%>%
  dplyr::select(VesselLengthRange)%>%
  tally()

# Correct them if any not valid and filter only valid ones
table1Save      <-  table1Save%>%filter(VesselLengthRange %in% vlen_ices$Key)

# Add to remrecsTable1
remrecsTable1["VesselLengthClass",] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total","RowsRemaining"])*100)

### 3.5.3 Check Metier L4 (Gear) categories are accepted -----------------------

m4_ices         <-  getCodeList("GearType")
table (table1Save$MetierL4 %in%m4_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table1Save [ !table1Save$MetierL4 %in%m4_ices$Key,]%>%group_by(MetierL4)%>%dplyr::select(MetierL4)%>%tally()

# Correct them if any not valid and filter only valid ones
table1Save      <-  table1Save%>%filter(MetierL4 %in% m4_ices$Key)

# Add to remrecsTable1
remrecsTable1["MetierL4",] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total","RowsRemaining"])*100)


### 3.5.4 Check Metier L5 (Target Assemblage) categories are accepted ----------

m5_ices         <-  getCodeList("TargetAssemblage")

table (table1Save$MetierL5 %in%m5_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table1Save [ !table1Save$MetierL5 %in%m5_ices$Key,]%>%group_by(MetierL5)%>%dplyr::select(MetierL5)%>%tally()

# Correct them if any not valid and filter only valid ones
table1Save      <-  table1Save%>%filter(MetierL5 %in% m5_ices$Key)

# Add to remrecsTable1
remrecsTable1["MetierL5",] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total","RowsRemaining"])*100)

### 3.5.5 Check Metier L6 (Fishing Activity) categories are accepted -----------

m6_ices         <-  getCodeList("Metier6_FishingActivity")

table (table1Save$MetierL6 %in%m6_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table1Save [ !table1Save$MetierL6 %in%m6_ices$Key,]%>%group_by(MetierL6)%>%dplyr::select(MetierL6)%>%tally()

# Correct them if any not valid and filter only valid ones
table1Save      <-  table1Save%>%filter(MetierL6 %in% m6_ices$Key)

# Add to remrecsTable1
remrecsTable1["MetierL6",] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total","RowsRemaining"])*100)

### 3.5.6 Check country codes --------------------------------------------------
#table1Save <- table1Save %>%
#  rename(CountryCode = VE_COU)

cntrcode <- getCodeList("ISO_3166")

table (table1Save$CountryCode %in%cntrcode$Key )   # TRUE records accepted in DATSU, FALSE aren't

# If you have not accepted country codes, consider replacing with recognized DATSU country codes 
# table1Save$CountryCode <- sub("NLD", "NL", table1Save$CountryCode)

# Get summary  of   DATSU valid/not valid records
table1Save [ !table1Save$CountryCode %in% cntrcode$Key,]%>% group_by(CountryCode) %>% dplyr::select(CountryCode) %>% tally()

# Correct them if any not valid and filter only valid ones
table1Save      <-  table1Save%>%filter(CountryCode %in% cntrcode$Key)

# Add to remrecsTable1
remrecsTable1["CountryCodes",] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total","RowsRemaining"])*100)

### 3.5.7 Check removed records ------------------------------------------------
print(remrecsTable1)

# TABLE 2  =====================================================================

### 3.5.8 Check ICES rectangles are valid---------------------------------------
#table2Save <- table2Save %>%
#  rename(ICESrectangle = LE_RECT)

statrect_ices <- getCodeList("StatRec")

table (table2Save$ICESrectangle %in%statrect_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [!table2Save$ICESrectangle %in%statrect_ices$Key,]%>%group_by(ICESrectangle)%>%dplyr::select(ICESrectangle)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(ICESrectangle %in% statrect_ices$Key)

# Add to remrecsTable2
remrecsTable2["ICESrectangles",] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total","RowsRemaining"])*100)

### 3.5.9 Check Vessel Lengths categories are accepted -------------------------
#table2Save <- table2Save %>%
#  rename(VesselLengthRange = LENGTHCAT)

vlen_ices       <-  getCodeList("VesselLengthClass")
table ( table2Save$VesselLengthRange%in%vlen_ices$Key )  # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$VesselLengthRange %in%vlen_ices$Key,]%>%group_by(VesselLengthRange)%>%dplyr::select(VesselLengthRange)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(VesselLengthRange %in% vlen_ices$Key)

# Add to remrecsTable2
remrecsTable2["VesselLengthClass",] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total","RowsRemaining"])*100)

### 3.5.10 Check Metier L4 (Gear) categories are accepted -----------------------
#table2Save <- table2Save %>%
#  rename(MetierL4 = met4)

m4_ices         <-  getCodeList("GearType")
table (table2Save$MetierL4 %in%m4_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$MetierL4 %in%m4_ices$Key,]%>%group_by(MetierL4)%>%dplyr::select(MetierL4)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(MetierL4 %in% m4_ices$Key)

# Add to remrecsTable2
remrecsTable2["MetierL4",] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total","RowsRemaining"])*100)

### 3.5.11 Check Metier L5 (Target Assemblage) categories are accepted ----------
m5_ices         <-  getCodeList("TargetAssemblage")

table (table2Save$MetierL5 %in%m5_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$MetierL5 %in%m5_ices$Key,]%>%group_by(MetierL5)%>%dplyr::select(MetierL5)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(MetierL5 %in% m5_ices$Key)

# Add to remrecsTable2
remrecsTable2["MetierL5",] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total","RowsRemaining"])*100)


### 3.5.12 Check Metier L6 (Fishing Activity) categories are accepted ----------

m6_ices         <-  getCodeList("Metier6_FishingActivity")

table (table2Save$MetierL6 %in%m6_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$MetierL6 %in%m6_ices$Key,]%>%group_by(MetierL6)%>%dplyr::select(MetierL6)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(MetierL6 %in% m6_ices$Key)

# Add to remrecsTable2
remrecsTable2["MetierL6",] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total","RowsRemaining"])*100)


### 3.5.13 Check VMSEnabled categories are accepted ----------------------------
#table2Save <- table2Save %>%
#  rename(VMSEnabled = tripInTacsat)

yn <- getCodeList("YesNoFields")

table (table2Save$VMSEnabled %in%yn$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$VMSEnabled %in%yn$Key,]%>%group_by(VMSEnabled)%>%dplyr::select(VMSEnabled)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(VMSEnabled %in% yn$Key)


# Add to remrecsTable2
remrecsTable2["VMSEnabled",] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total","RowsRemaining"])*100)


### 3.5.15 Check country codes -------------------------------------------------
#table2Save <- table2Save %>%
#  rename(CountryCode = VE_COU)

cntrcode <- getCodeList("ISO_3166")
table (table2Save$CountryCode %in%cntrcode$Key )   # TRUE records accepted in DATSU, FALSE aren't

# If you have not accepted country codes, consider replacing with recognized DATSU country codes 
# table2Save$CountryCode <- sub("NLD", "NL", table2Save$CountryCode)

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$VMSEnabled %in% cntrcode$Key,]%>% group_by(CountryCode) %>% dplyr::select(CountryCode) %>% tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(CountryCode %in% cntrcode$Key)

# Add to remrecsTable2
remrecsTable2["CountryCodes",] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total","RowsRemaining"])*100)

# DATSU Vocabulary check finished

### 3.5.16 Check removed records ------------------------------------------------
print(remrecsTable2)

#'------------------------------------------------------------------------------
# # 3.6 DATA QC REPORT (OPTIONAL)                                           ----
#'------------------------------------------------------------------------------

# Null values are only accepted for NON MANDATORY fields

# TABLE 1 ======================================================================
#table1Save <- table1Save %>%
#  rename(TotValue = sum_le_euro_tot)

# Create the table to check fields formats and number of NA's
table_nas <- NULL
for ( nn in colnames(table1Save)) {
  table_na <- table(table1Save[, nn]%>%is.na() )
  row <- c(field = nn, is_na =  ifelse(is.na (table_na['TRUE']), 0, table_na['TRUE'] ), total_records =  table1Save[, nn]%>%length(), field_type =class(  table1Save[, nn]  ) )
  table_nas <- rbind(table_nas,  row)
}

# Print a summary table in Viewer
gt(
  table_nas%>%as_tibble(),
  rowname_col = 'field'
) %>%
  tab_header(
    title = md('Summary of **Table 1**  number of NA and records types')
  ) %>%
  cols_label(  `is_na.NA`=  md('Number of  <br> NA\'s') ,
               total_records = md('Total <br> records'),
               field_type = md('Field <br> type')
  ) %>%
  tab_footnote(
    footnote = md('Non mandatory fields can include null values if not available'),
    locations = cells_stub( rows = c( 'TotValue', 'AverageGearWidth', 'Habitat'))
  )


# TABLE 2 ======================================================================
#table2Save <- table2Save %>%
#  rename(TotValue = sum_le_euro_tot)

# Create the table to check fields formats and number of NA's
table_nas <- NULL
for ( nn in colnames(table2Save)) {
  table_na <- table(table2Save[, nn]%>%is.na() )
  row <- c(field = nn, is_na =  ifelse(is.na (table_na['TRUE']), 0, table_na['TRUE'] ), total_records =  table2Save[, nn]%>%length(), field_type =class(  table2Save[, nn]  ) )
  table_nas <- rbind(table_nas,  row)
}

# Print a summary table in Viewer

gt(
  table_nas%>%as_tibble(),
  rowname_col = 'field'
) %>%
  tab_header(
    title = md('Summary of **Table 2**  number of NA and records types')
  ) %>%
  cols_label(  `is_na.NA`=  md('Number of  <br> NA\'s') ,
               total_records = md('Total <br> records'),
               field_type = md('Field <br> type')
  ) %>%
  tab_footnote(
    footnote = md('Non mandatory fields can include null values if not available'),
    locations = cells_stub( rows = c('TotValue'))
  )


# Check if TABLE 1 fishing hours > 0

table( table1$INTV > 0  )

# Check if TABLE 2 fishing days  > 0

table( table2$INTV > 0  )

# End of QC checks

#'------------------------------------------------------------------------------
# 3.7 Save the final TABLE 1 and TABLE 2 for data call submission           ----
#'------------------------------------------------------------------------------

# Headers and quotes have been removed to be compatible with required submission and ICES SQL DB format.
write.table(table1Save, file.path(outPath, "table1Save.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(table2Save, file.path(outPath, "table2Save.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)

#'------------------------------------------------------------------------------
# 3.8 Data call submission using ICESVMS package (OPTIONAL)                 ----
#'------------------------------------------------------------------------------

# R packages required to be installed:
# install.packages(c("icesVMS", "icesConnect"), repos = "https://ices-tools-prod.r-universe.dev")  

#library(icesVMS)

# Replace with your ICES user name and you will be requested with your password
#icesConnect::set_username('submitter_ices_user_id') # e.g., 'lastname' not 'ices\lastname'

#icesConnect::ices_token(refresh = TRUE)
#icesConnect::decode_token()$Email # Check the email associated to your ices user name is the correct one

#screen_vms_file(file.path(outPath, "table1Save.csv"))  # Submit for screening Table 1
#screen_vms_file(file.path(outPath, "table2Save.csv"))  # Submit for screening Table 2


#'------------------------------------------------------------------------------
# 3.8 Data call submission using ICESVMS package (OPTIONAL)                 ----
#'------------------------------------------------------------------------------

# R packages required to be installed:
# install.packages(c("icesVMS", "icesConnect"), repos = "https://ices-tools-prod.r-universe.dev")  

library(icesVMS)

# Replace with your ICES user name and you will be requested with your password
#icesConnect::set_username('submitter_ices_user_id') # e.g., 'lastname' not 'ices\lastname'

#icesConnect::ices_token(refresh = TRUE)
#icesConnect::decode_token()$Email # Check the email associated to your ices user name is the correct one

#screen_vms_file(file.path(outPath, "table1Save.csv"))  # Submit for screening Table 1
#screen_vms_file(file.path(outPath, "table2Save.csv"))  # Submit for screening Table 2

#'------------------------------------------------------------------------------
# End of script                                                             
#'------------------------------------------------------------------------------
