if (!require("pacman")) install.packages("pacman")
p_load(data.table, dplyr, sf, mapview, stars, raster, terra, leaflet, leafem)

'%!in%' <- function(x,y)!('%in%'(x,y))

load(file = paste0(outPath, "table1.RData")  )

# lst <- paste0(outPath, "tacsatEflalo_", 2009:2023, ".rds")
# te <- rbindlist(lapply(lst, readRDS), fill = T)


grd_size <- 0.05


table1$SI_LONG <- round(CSquare2LonLat(table1$Csquare ,grd_size)$SI_LONG,2)
table1$SI_LATI <- round(CSquare2LonLat(table1$Csquare ,grd_size)$SI_LATI,2)


t1 <- data.table(table1)

table(t1[!is.na(SA_M2)]$LE_MET)

# start by making a list of fleet segments from lvl 5 metiers
t1$LE_SEG <-  sapply(strsplit(t1$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  


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
  sub <- t1[LE_SEG == i & !is.na(LE_EURO_TOT) & LE_EURO_TOT != 0]
  
  if(nrow(sub)==0)
    next
  
  #change to wide format
  sa <- dcast(sub, SI_LONG+SI_LATI~Year, value.var = "LE_EURO_TOT", fun = sum)
  
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
  sub <- t1[LE_SEG == i & !is.na(INTV) & INTV != 0]
  
  if(nrow(sub)==0)
    next
  
  #change to wide format
  sa <- dcast(sub, SI_LONG+SI_LATI~Year, value.var = "INTV", fun = sum)
  
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
    sub <- t1[LE_SEG == i & !is.na(SA_M2) & SA_M2 != 0]
    
  if(nrow(sub)==0)
    next
  
  #change to wide format
  sa <- dcast(sub, SI_LONG+SI_LATI~Year, value.var = "SA_M2", fun = sum)
  
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
  
  risk.bins <-c(0,0.1,0.2,0.5,1,2,5,10,20,50,10000)
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
    
    
    rb[[paste0("X", y)]] <- rb[[paste0("X", y, "_", y)]] / rb$cellsize_cellsize
    
    m <- m |> 
      addRasterImage(rb[[paste0("X", y)]], colors = pal, project = TRUE, group = as.character(y),
                     layerId = as.character(y)) |> 
      addImageQuery(rb[[paste0("X", y)]], project = TRUE,
                    layerId = as.character(y))
    
  }
  
  m <- m |> 
    addLayersControl(baseGroups = ys,
                     options = layersControlOptions(collapsed = F)) %>%
    addLegend(pal=pal, values = values(r), title = paste0("Swept area ratio (SAR) - ", i))
  
  m
  
  htmlwidgets::saveWidget	(m, file=paste0(plotPath, "SAR_", i, ".html"))
  
}

