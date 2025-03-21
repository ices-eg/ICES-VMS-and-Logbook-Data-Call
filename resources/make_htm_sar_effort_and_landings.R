######################################
##  This script will help you plot  ##
##  some basic maps to explore      ##
##  how the data you have prepared  ##
##  looks, prior to uploading to    ##
##  the ICES database.              ##
######################################

## This does not form a part of the formal data submission process
## It is more to help you visualise your own data and identify any obvious errors.


rm(list=ls())



if (!require("pacman")) install.packages("pacman")
p_load(data.table, dplyr, sf, mapview, stars, raster, terra, leaflet, leafem, vmstools, icesVMS)

'%!in%' <- function(x,y)!('%in%'(x,y))

path <- paste0(getwd(), "/") # Working directory
codePath  <- paste0(path, "Scripts/")   # Location to store R scripts
dataPath  <- paste0(path, "Data/")      # Location to store tacsat (VMS) and eflalo (logbook) data
outPath   <- paste0(path, "Results/")   # Location to store the results
plotPath  <- paste0(path, "Plots/") 


table1Save <- read.csv(file = paste0(outPath, "table1Save.csv"), header=T)

table1Save <- as.data.frame(table1Save)

head(table1Save)

# lst <- paste0(outPath, "tacsatEflalo_", 2009:2023, ".rds")
# te <- rbindlist(lapply(lst, readRDS), fill = T)
table1 <- table1Save %>%
  group_by(CountryCode, Year, Month, Csquare, MetierL4, MetierL5, MetierL6, VesselLengthRange) %>%
  summarise(
    TotalRecords = sum(No_Records),
    TotalDistance = sum(AverageFishingSpeed * FishingHour),
    TotalTime = sum(FishingHour),
    AverageFishingSpeed = TotalDistance / TotalTime,
    FishingHour = sum(FishingHour),
    TotalInterval = sum(AverageInterval * No_Records),
    AverageInterval = TotalInterval / TotalRecords,
    AverageVesselLength = sum(AverageVesselLength * No_Records) / TotalRecords,
    AveragekW = sum(AveragekW * No_Records) / TotalRecords,
    kWFishingHour = sum(kWFishingHour),
    SweptArea = sum(SweptArea),
    TotWeight = sum(TotWeight),
    TotValue = sum(TotValue),
    AverageGearWidth = sum(AverageGearWidth * No_Records) / TotalRecords,
    AnonymizedVesselID = {
      distinct_vessels <- unique(unlist(strsplit(AnonymizedVesselID, ";")))
      if (length(distinct_vessels) >= 3 || "not_required" %in% distinct_vessels) {
        "not_required"
      } else {
        paste(sort(distinct_vessels), collapse = ";")
      }
    },
    NoDistinctVessels = {
      distinct_vessels <- unique(unlist(strsplit(AnonymizedVesselID, ";")))
      if (length(distinct_vessels) >= 3 || "not_required" %in% distinct_vessels) {
        "3+"
      } else {
        as.character(length(distinct_vessels))
      }
    },
    RecordType = "VE"
  ) %>%
  dplyr::select(
    RecordType,
    CountryCode,
    Year,
    Month,
    NoDistinctVessels,
    AnonymizedVesselID,
    Csquare,
    MetierL4,
    MetierL5,
    MetierL6,
    VesselLengthRange,
    AverageFishingSpeed,
    FishingHour,
    AverageInterval,
    AverageVesselLength,
    AveragekW,
    kWFishingHour,
    TotWeight,
    TotValue,
    AverageGearWidth
  ) %>%
  as.data.frame() %>%
  mutate(SA_M2 = (AverageGearWidth * 1000) * (AverageFishingSpeed * 1852) * FishingHour,  ## gear width is in km, speed is in nautical miles per hour
         Csq_area_M2 = sfdSAR::csquare_area(Csquare) * 1000000,
         SAR = SA_M2/Csq_area_M2)

grd_size <- 0.05


table1$SI_LONG <- round(CSquare2LonLat(table1$Csquare ,grd_size)$SI_LONG,2)
table1$SI_LATI <- round(CSquare2LonLat(table1$Csquare ,grd_size)$SI_LATI,2)

t1 <- data.table(table1)

table(t1[!is.na(SA_M2)]$LE_MET)

# start by making a list of fleet segments from lvl 5 metiers
t1$LE_SEG <-  sapply(strsplit(t1$MetierL6, "_"), function(x) paste(x[1:2], collapse = "_"))  


#Group some metiers into lvl4
table(t1$LE_SEG)
t1[LE_SEG %like% "FPN", LE_SEG := "FPN"]
t1[LE_SEG %like% "FPO", LE_SEG := "FPO"]
t1[LE_SEG %like% "GNS", LE_SEG := "GNS"]
t1[LE_SEG %like% c("GNS|GNC|GND"), LE_SEG := "GNS"]
t1[LE_SEG %like% c("LHP|LLD|LLS"), LE_SEG := "LL"]
t1[LE_SEG %like% c("MIS"), LE_SEG := "MIS"]
t1[LE_SEG %like% c("SDN"), LE_SEG := "SDN"]
t1[LE_SEG %like% c("SSC"), LE_SEG := "SSC"]

###############


table(t1$LE_SEG)


## VALUE plots
for(i in unique(t1$LE_SEG)){
  cat("")
  print(i)
  sub <- t1[LE_SEG == i & !is.na(TotValue) & TotValue != 0]
  
  if(nrow(sub)==0)
    next
  
  #change to wide format
  sa <- dcast(sub, SI_LONG+SI_LATI~Year, value.var = "TotValue", fun = sum)
  
  sa[sa == 0] <- NA
  
  pts <- sa %>%
    sf::st_as_sf(coords = c("SI_LONG","SI_LATI")) %>%
    sf::st_set_crs(4326)
  
  rast <- st_rasterize(pts, dx = grd_size, dy = grd_size,
                       xlim = c(st_bbox(pts)[1] -grd_size/2, st_bbox(pts)[3] +grd_size/2),
                       ylim = c(st_bbox(pts)[2] -grd_size/2, st_bbox(pts)[4] +grd_size/2) 
  )
  rast$cellsize <- terra::cellSize(rast(rast), unit="m")
  r <- rast(rast)
  rb <- raster::brick(r)
  
  risk.bins <-c(0,10, 50, 100, 500, 1000, 2000, 5000, 10000, 50000, 100000000)
  pal <- colorBin("magma", bins = risk.bins, na.color = "transparent")
  
  m <- leaflet() |> 
    addProviderTiles(providers$Esri.WorldGrayCanvas)  |> 
    addProviderTiles(providers$OpenSeaMap, options = providerTileOptions(
      updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
      updateWhenIdle = TRUE           # map won't load new tiles when panning
    )) 
  
  y <- 2009
  ys <- sort(unique(sub$Year))
  for(y in ys){
    cat(paste(y, ""))
    
    
    rb[[paste0("X", y)]] <- rb[[paste0("X", y, "_", y)]] / rb$cellsize_cellsize * 1000000
    
    m <- m |> 
      addRasterImage(rb[[paste0("X", y)]], colors = pal, project = TRUE, group = as.character(y),
                     layerId = as.character(y)) |> 
      addImageQuery(rb[[paste0("X", y)]], project = TRUE,
                    layerId = as.character(y))
    
  }
  
  m <- m |> 
    addLayersControl(baseGroups = ys,
                     options = layersControlOptions(collapsed = F)) %>%
    addLegend(pal=pal, values = values(r), title = paste0("Value (EURO) / km2 - ", i))
  
  # m
  
  htmlwidgets::saveWidget	(m, file=paste0(plotPath, "Value_", i, ".html"))
}

## Effort plots
for(i in unique(t1$LE_SEG)){
  cat("")
  print(i)
  sub <- t1[LE_SEG == i & !is.na(FishingHour) & FishingHour != 0]
  
  if(nrow(sub)==0)
    next
  
  #change to wide format
  sa <- dcast(sub, SI_LONG+SI_LATI~Year, value.var = "FishingHour", fun = sum)
  
  sa[sa == 0] <- NA
  
  pts <- sa %>%
    sf::st_as_sf(coords = c("SI_LONG","SI_LATI")) %>%
    sf::st_set_crs(4326)
  
  rast <- st_rasterize(pts, dx = grd_size, dy = grd_size,
                       xlim = c(st_bbox(pts)[1] -grd_size/2, st_bbox(pts)[3] +grd_size/2),
                       ylim = c(st_bbox(pts)[2] -grd_size/2, st_bbox(pts)[4] +grd_size/2) 
  )
  rast$cellsize <- terra::cellSize(rast(rast), unit="m")
  r <- rast(rast)
  rb <- raster::brick(r)
  
  risk.bins <-c(0,1,2,5,10,20,50,100,500,1000,5000, 1000000)
  pal <- colorBin("magma", bins = risk.bins, na.color = "transparent")
  
  m <- leaflet() |> 
    addProviderTiles(providers$Esri.WorldGrayCanvas)  |> 
    addProviderTiles(providers$OpenSeaMap, options = providerTileOptions(
      updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
      updateWhenIdle = TRUE           # map won't load new tiles when panning
    )) 
  
  y <- 2009
  ys <- sort(unique(sub$Year))
  for(y in ys){
    cat(paste(y, ""))
    
    
    rb[[paste0("X", y)]] <- rb[[paste0("X", y, "_", y)]] / rb$cellsize_cellsize * 1000000 * 60
    
    m <- m |> 
      addRasterImage(rb[[paste0("X", y)]], colors = pal, project = TRUE, group = as.character(y),
                     layerId = as.character(y)) |> 
      addImageQuery(rb[[paste0("X", y)]], project = TRUE,
                    layerId = as.character(y))
    
  }
  
  m <- m |> 
    addLayersControl(baseGroups = ys,
                     options = layersControlOptions(collapsed = F)) %>%
    addLegend(pal=pal, values = values(r), title = paste0("Effort (minutes) / km2 - ", i))
  
  # m
  
  htmlwidgets::saveWidget	(m, file=paste0(plotPath, "Effort_", i, ".html"))
}



  ## SAR plots
for(i in unique(t1$LE_SEG)){
  cat("")
  print(i)
  sub <- t1[LE_SEG == i & !is.na(SAR) & SAR != 0]
  
  if(nrow(sub)==0)
    next
  
  #change to wide format
  sa <- dcast(sub, SI_LONG+SI_LATI~Year, value.var = "SAR", fun = sum)
  
  sa[sa == 0] <- NA
  
  pts <- sa %>%
    sf::st_as_sf(coords = c("SI_LONG","SI_LATI")) %>%
    sf::st_set_crs(4326)
  
  rast <- st_rasterize(pts, dx = grd_size, dy = grd_size,
                       xlim = c(st_bbox(pts)[1] -grd_size/2, st_bbox(pts)[3] +grd_size/2),
                       ylim = c(st_bbox(pts)[2] -grd_size/2, st_bbox(pts)[4] +grd_size/2) 
  )
  rast$cellsize <- terra::cellSize(rast(rast), unit="m")
  r <- rast(rast)
  rb <- raster::brick(r)
  
  risk.bins <-c(0,0.010, 0.050, 0.100, 0.500, 1.000, 2.000, 5.000, 10.000, 50.000, 100.000000)
  pal <- colorBin("magma", bins = risk.bins, na.color = "transparent")
  
  m <- leaflet() |> 
    addProviderTiles(providers$Esri.WorldGrayCanvas)  |> 
    addProviderTiles(providers$OpenSeaMap, options = providerTileOptions(
      updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
      updateWhenIdle = TRUE           # map won't load new tiles when panning
    )) 
  
  y <- 2009
  ys <- sort(unique(sub$Year))
  for(y in ys){
    cat(paste(y, ""))
    
    
    rb[[paste0("X", y)]] <- rb[[paste0("X", y, "_", y)]] 
    
    m <- m |> 
      addRasterImage(rb[[paste0("X", y)]], colors = pal, project = TRUE, group = as.character(y),
                     layerId = as.character(y)) |> 
      addImageQuery(rb[[paste0("X", y)]], project = TRUE,
                    layerId = as.character(y))
    
  }
  
  m <- m |> 
    addLayersControl(baseGroups = ys,
                     options = layersControlOptions(collapsed = F)) %>%
    addLegend(pal=pal, values = values(r), title = paste0("Swept Area Ratio (SAR) ", i))
  
  # m
  
  htmlwidgets::saveWidget	(m, file=paste0(plotPath, "SAR_", i, ".html"))
}


## Total SAR plot

# Create a subset with all values of t1$LE_SEG
# Exclude rows where SAR is NA or 0
sub_total <- t1[!is.na(SAR) & SAR != 0]

# Calculate the total SAR by summing across all values of t1$LE_SEG
# Use dcast to reshape the data, with SI_LONG and SI_LATI as row identifiers and Year as column identifier
# Apply the sum function to the SAR values for each combination of SI_LONG, SI_LATI, and Year
sa_total <- dcast(sub_total, SI_LONG+SI_LATI~Year, value.var = "SAR", fun = sum)

# Replace 0 values with NA in sa_total
sa_total[sa_total == 0] <- NA

# Convert sa_total to a spatial points data frame (sf object)
# Use SI_LONG and SI_LATI columns as coordinates
# Set the coordinate reference system (CRS) to EPSG:4326 (WGS84)
pts_total <- sa_total %>%
  sf::st_as_sf(coords = c("SI_LONG","SI_LATI")) %>%
  sf::st_set_crs(4326)

# Rasterize the spatial points data frame (pts_total) to create a raster object (rast_total)
# Use the specified grid size (grd_size) for cell dimensions
# Set the extent of the raster based on the bounding box of pts_total, with a buffer of half the grid size
rast_total <- st_rasterize(pts_total, dx = grd_size, dy = grd_size,
                           xlim = c(st_bbox(pts_total)[1] -grd_size/2, st_bbox(pts_total)[3] +grd_size/2),
                           ylim = c(st_bbox(pts_total)[2] -grd_size/2, st_bbox(pts_total)[4] +grd_size/2)
)

# Calculate the cell size of the raster in meters and store it as a new column in rast_total
rast_total$cellsize <- terra::cellSize(rast(rast_total), unit="m")

# Convert rast_total to a SpatRaster object (r_total)
r_total <- rast(rast_total)

# Convert r_total to a RasterBrick object (rb_total)
rb_total <- raster::brick(r_total)

# Create a Leaflet map object (m_total)
# Add base map tiles from Esri.WorldGrayCanvas and OpenSeaMap providers
m_total <- leaflet() |>
  addProviderTiles(providers$Esri.WorldGrayCanvas) |>
  addProviderTiles(providers$OpenSeaMap, options = providerTileOptions(
    updateWhenZooming = FALSE, # map won't update tiles until zoom is done
    updateWhenIdle = TRUE # map won't load new tiles when panning
  ))


# Get unique years from sub_total$Year and sort them
ys <- sort(unique(sub_total$Year))

# Loop through each year in ys
for (y in ys) {
  cat(paste(y, ""))
  
  # Get the layer name for the current year from rb_total
  layer_name <- names(rb_total)[grep(paste0("X", y), names(rb_total))]
  
  # Check if the layer name exists
  if (length(layer_name) > 0) {
    # Add the raster layer for the current year to the Leaflet map
    # Use the specified color palette (pal) and project the raster
    # Set the group and layerId to the current year
    # Add an image query to display values on click
    m_total <- m_total |>
      addRasterImage(rb_total[[layer_name]], colors = pal, project = TRUE, group = as.character(y),
                     layerId = as.character(y)) |>
      addImageQuery(rb_total[[layer_name]], project = TRUE,
                    layerId = as.character(y))
  }
}

# Add a layers control to the Leaflet map
# Use the unique years (ys) as base groups
# Set the layers control options to not be collapsed
m_total <- m_total |>
  addLayersControl(baseGroups = ys,
                   options = layersControlOptions(collapsed = F)) %>%
  # Add a legend to the map
  # Use the specified color palette (pal) and values from r_total
  # Set the legend title to "Total Swept Area Ratio (SAR)"
  addLegend(pal=pal, values = values(r_total), title = "Total Swept Area Ratio (SAR)")

# Save the Leaflet map as an HTML file using htmlwidgets
# Use the specified plot path and file name
htmlwidgets::saveWidget(m_total, file=paste0(plotPath, "SAR_Total.html"))


## End of script.

