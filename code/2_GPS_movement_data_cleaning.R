# DTRA-NK - Data Preliminary
# Sirimon Thomas
# April 2024

#load packages
library(pacman)
p_load(tidyverse,
       #robotoolbox,
       here,
       sf,
       #httr,
       #amt,
       tmap,
       tmaptools,
       ctmm)

#import gps track csv files
files <- list.files(here('input','raw','spatial','gps_collars'), full.names = T)
gps <- list()
for (i in 1:length(files)) {
  gps[[i]] <- read.csv(files[i])
}

#create lists for results of spatial analysis
mk.track <- vector("list",length(gps)) #tracks

hr.50 <- vector("list",length(gps)) #home range
hr.95 <- vector("list",length(gps))

kde.50 <- vector("list",length(gps)) #kernel density estimate
kde.95 <- vector("list",length(gps))

mcp.50 <- vector("list",length(gps)) #minimum convex polygon
mcp.95 <- vector("list",length(gps))

iso.50 <- vector("list",length(gps)) #isopleths
iso.95 <- vector("list",length(gps))

gps.summary <- data.frame()

#sf_use_s2(FALSE)

for (i in 1:length(gps)) {
  #### data cleaning & spatial analysis ####
  
  #gps[[i]] <- gps[[i]][!is.na(gps[[i]]$LONGITUDE.E.W),] #remove nas
  
  #remove N or S from value and make southern points negative
  # gps[[i]]$LATITUDE.N.S <- ifelse(substr(gps[[i]]$LATITUDE.N.S,10,10) == "N",
  #                                 as.numeric(substr(gps[[i]]$LATITUDE.N.S,1,9)),
  #                                 as.numeric(substr(gps[[i]]$LATITUDE.N.S,1,9))*-1) 
  gps[[i]] <- gps[[i]] %>% mutate(
    #tidy date and time columns
    TIME = format(strptime(sprintf("%06d", TIME), format="%H%M%S"), format = "%H:%M:%S"),
    DATE = as.Date(as.POSIXlt(strptime(DATE, format="%y%m%d"))),
    DATETIME = as.POSIXlt(paste(DATE, TIME), format = "%Y-%m-%d %H:%M:%S"),
    #remove N or S from latitude and make southern points negative
    LATITUDE.N.S = ifelse(substr(LATITUDE.N.S,10,10) == "N",
                           as.numeric(substr(LATITUDE.N.S,1,9)),
                           as.numeric(substr(LATITUDE.N.S,1,9))*-1) 
  )
  
  #remove points within 20m of boma to accomodate GPS variability
  gps[[i]] <- filter(gps[[i]], (gps[[i]]$LATITUDE.N.S>=(gps[[i]]$LATITUDE.N.S[1]+0.0002) | gps[[i]]$LATITUDE.N.S<=(gps[[i]]$LATITUDE.N.S[1]-0.0002)) 
                     & (gps[[i]]$LONGITUDE.E.W>=(gps[[i]]$LONGITUDE.E.W[1]+0.0002) | gps[[i]]$LONGITUDE.E.W<=(gps[[i]]$LONGITUDE.E.W[1]-0.0002)))
  
  #take 1 point per minute
  #gps[[i]] <- gps[[i]][seq(1, nrow(gps[[i]]), 60), ]
  
  #update date & time
  #gps[[i]]$TIME <- (gps[[i]]$TIME)+30000
  #gps[[i]]$TIME <- format(strptime(sprintf("%06d", gps[[i]]$TIME), format="%H%M%S"), format = "%H:%M:%S")
  
  #gps[[i]]$DATE <- gps[[i]]$DATE+20000000
  #gps[[i]]$DATE <- as.Date(as.POSIXlt(strptime(gps[[i]]$DATE, format="%y%m%d")))
  
  #gps[[i]]$DATETIME <- as.POSIXlt(paste(gps[[i]]$DATE, gps[[i]]$TIME), format = "%Y-%m-%d %H:%M:%S")
  
  #reset row indeces & INDEX values
  if(nrow(gps[[i]])>0){
  row.names(gps[[i]]) <- NULL
  gps[[i]]$INDEX <- seq(1:nrow(gps[[i]]))
  }

  
  #}  
  
  #### GIS analysis ####
  
  #create tracks for amt package
  mk.track[[i]] <- mk_track(tbl = gps[[i]], .x = LONGITUDE.E.W, .y = LATITUDE.N.S, .t = DATETIME, crs = 4326, all_cols = T)
  
  #kernel density estimates at 95 and 50 levels
  kde.50[[i]] <- hr_kde(mk.track[[i]], levels = 0.50)
  kde.95[[i]] <- hr_kde(mk.track[[i]], levels = 0.95)
  
  #minimum convex polygon at 95 and 50 levels
  mcp.50[[i]] <- hr_mcp(mk.track[[i]], levels = 0.50) 
  mcp.95[[i]] <- hr_mcp(mk.track[[i]], levels = 0.95) 
  
  #isopleths at 50 and 95 levels, based on kde
  iso.50[[i]] <- hr_isopleths(kde.50[[i]])
  iso.95[[i]] <- hr_isopleths(kde.95[[i]])
  
  #area calculations based on kde
  hr.50[[i]] <- hr_area(kde.50[[i]])
  hr.50[[i]]$area.km2 <- hr.50[[i]]$area*(1e-6)
  hr.95[[i]] <- hr_area(kde.95[[i]])
  hr.95[[i]]$area.km2 <- hr.95[[i]]$area*(1e-6)
  
  gps.summary$ID[i] <- str_remove_all(list.files(here('input','raw','spatial','gps_collars'))[i],'.csv')
  
  #kde area estimates from 50 and 95 isopleths
  gps.summary$hr.kde.50.m2[i] <- hr_area(kde.50[[i]])$area
  gps.summary$hr.kde.95.m2[i] <- hr_area(kde.95[[i]])$area
  
  #mcp area estimates from 50 and 95 levels
  gps.summary$hr.mcp.50.m2[i] <- hr_area(mcp.50[[i]])$area
  gps.summary$hr.mcp.95.m2[i] <- hr_area(mcp.95[[i]])$area
  
  #### movement calculations ####
  
  #claculate length of the track
  gps.summary$dist.m[i] <- st_length(as_sf_lines(mk.track[[i]]))
  
  #calcuate time difference
  gps.summary$time.h[i] <- difftime(gps[[i]]$DATETIME[nrow(gps[[i]])], gps[[i]]$DATETIME[1], units = "hours")
  
  #calculate speed
  gps.summary$speed.kmh[i] <- (gps.summary$dist.m[i]/1000)/gps.summary$time.h[i]
  
  # #### NDVI ####
  # 
  # #import lansat rasters
  # lansat.red <- raster("C:/Users/sirim/Downloads/LC09_L2SP_168060_20220711_20220713_02_T1_SR_B4.TIF")
  # lansat.nir <- raster("C:/Users/sirim/Downloads/LC09_L2SP_168060_20220711_20220713_02_T1_SR_B5.TIF")
  # 
  # pts <- st_transform(as_sf_points(mk.track[[i]]), crs = st_crs(32637))
  # 
  # #crop rasters
  # lansat.red <- crop(lansat.red,extent(st_bbox(pts))+100)
  # lansat.nir <- crop(lansat.nir,extent(st_bbox(pts))+100)
  # 
  # #ndvi
  # ndvi <- (lansat.nir - lansat.red)/(lansat.nir + lansat.red)
  # 
  # #extract ndvi values
  # pts$NDVI <- raster::extract(ndvi, pts)
  # 
  # #calculate proportion of points that fall in each ndvi class
  # herd.data$ndvi1[i] <- (sum(pts$NDVI>0.0 & pts$NDVI<=0.1))/nrow(pts)
  # herd.data$ndvi2[i] <- (sum(pts$NDVI>0.1 & pts$NDVI<=0.2))/nrow(pts)
  # herd.data$ndvi3[i] <- (sum(pts$NDVI>0.2 & pts$NDVI<=0.3))/nrow(pts)
  # herd.data$ndvi4[i] <- (sum(pts$NDVI>0.3 & pts$NDVI<=0.4))/nrow(pts)
  # herd.data$ndvi5[i] <- (sum(pts$NDVI>0.4 & pts$NDVI<=0.5))/nrow(pts)
  # 
  # max(pts$NDVI) <=0.5
  # 
  # herd.data$mean.ndvi[i] <- mean(pts$NDVI)
  # #plot(lansat.red)
  # #plot(pts$geometry, add = T)
  
  
  #### mapping & image export ####
  
  tm <- tm_shape(kde.95[[i]]$ud$lyr.1) +
    tm_raster(palette = "BuPu", colorNA = "white", n = 20, legend.show = F) +
    tm_shape(mcp.95[[i]]$mcp) + tm_borders(col = "Red") +
    tm_shape(mcp.50[[i]]$mcp) + tm_borders(col = "Red", lty = "dashed") +
    tm_shape(iso.95[[i]]) + tm_borders(col = "blue") +
    tm_shape(iso.50[[i]]) + tm_borders(col = "blue", lty = "dashed") +
    tm_layout(title = paste("Home Range - ",gps.summary$ID[i]), asp = 1, frame = T) +
    tm_scale_bar(position = c("left", "bottom")) +
    tm_compass(position = c("right", "bottom"))

  #save the file
  tmap_save(tm, filename = here('output','spatial',paste0(gps.summary$ID[i],'_all.jpg')))

  #mapping & image export
  tm.mcp <- tm_shape(kde.95[[i]]$ud$lyr.1) +
    tm_raster(palette = "BuPu", colorNA = "white", n = 20, legend.show = F) +
    tm_shape(mcp.95[[i]]$mcp) + tm_borders(col = "Red") +
    tm_shape(mcp.50[[i]]$mcp) + tm_borders(col = "Red", lty = "dashed") +
    tm_layout(title = paste("Home Range Minimum Convex Polygons - ",gps.summary$ID[i]), asp = 1, frame = T) +
    tm_scale_bar(position = c("left", "bottom"))

  #save the file
  tmap_save(tm.mcp, filename = here('output','spatial',paste0(gps.summary$ID[i],'_MCP.jpg')))

  #mapping & image export
  tm.kde <- tm_shape(kde.95[[i]]$ud$lyr.1) +
    tm_raster(palette = "BuPu", colorNA = "white", n = 20, legend.show = F) +
    tm_shape(iso.95[[i]]) + tm_borders(col = "blue") +
    tm_shape(iso.50[[i]]) + tm_borders(col = "blue", lty = "dashed") +
    tm_layout(title = paste("Home Range by Kernel Density Estimation - ",gps.summary$ID[i]), asp = 1, frame = T) +
    tm_scale_bar(position = c("left", "bottom"))

  #save the file
  tmap_save(tm.kde, filename = here('output','spatial',paste0(gps.summary$ID[i],'_KDE.jpg')))

  
  
  ############ NDVI images ###########
  
} 


# new code for GPS data####

## Short term GPS trackers####

livestock.track <- st_read(here('output','spatial','DTRANK_livestock_GPS_vector_multipoint.gpkg'))

livestock.pts <- readRDS(here('output','spatial','DTRANK_livestock_pts_all.RDS'))
human.pts <- readRDS(here('output','spatial','DTRANK_human_pts_all.RDS'))

livestock






## Long term ceres tags ####
