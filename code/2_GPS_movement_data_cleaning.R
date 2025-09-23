# DTRA-NK - Data Preliminary
# Sirimon Thomas
# April 2024

###setup ----
#load packages
pacman::p_load(tidyverse,
               #robotoolbox,
               here,
               sf,
               #httr,
               #amt,
               #tmap,
               #tmaptools,
               ctmm,
               move2,
               units,
               leaflet,
               geodata,
               htmlwidgets)

# Arc 1960/UTM zone 37N : 'EPSG:21097'
# WGS 84 : 'EPSG:4326'

# new code for GPS data#####

#import GPS data and tidy for movebank upload####
##short term GPS trackers----

#livestock.track <- st_read(here('output','spatial','DTRANK_livestock_GPS_vector_multipoint.gpkg'))
#human.track <- st_read(here('output','spatial','DTRANK_human_GPS_multipoint.gpkg'))

livestock.pts <- readRDS(here('output','spatial','DTRANK_livestock_pts_all.RDS'))

human.pts <- readRDS(here('output','spatial','DTRANK_human_pts_all.RDS'))

#create tracks from GPS points

#activity space mapping data
act.space.map <- readRDS(here('output','spatial','DTRANK_activity_space_mapping.RDS'))
#strip labels
for (data in seq_along(act.space.map)) {
  act.space.map[[data]] <- act.space.map[[data]] %>% haven::zap_labels()
}

#community reference data
community.ref <- read.csv(here('output','DTRANK_community_household_id_reference.csv'))

####human----

## all tracks as same table
human.mt <- bind_rows(human.pts) %>% 
  mutate(date_time = as_datetime(date_time)) %>%
  #thin data to every 30 seconds
  #mt_filter_min_time_lag(min_lag = 30, units = "secs") %>%
  #add livestock details
  left_join(read.csv(here('output','DTRANK_individual_human.csv')) %>%
              filter(movement_study == 'yes') %>%
              mutate(id = paste0(hh_id,'_human_',gps_logger_id)) %>% select(-date, -county, -ward),
            by = c('id','hh_id')) %>%
  mt_as_move2(coords = c('lon','lat'), crs = 4326, 
              time_column = 'date_time', 
              track_id_column = 'id',
              track_attributes = c('hh_id','county','ward','gps_logger_id_human', 'gender','participant_age')) %>%
  #thin data to every 20 seconds
  mt_filter_per_interval(unit = '20 secs', criterion = 'first') %>%
  #calculate azimuth while in geographic CRS
  mutate(azimuth = mt_azimuth(.),
         turn_angle = mt_turnangle(.)) %>%
  #transform to equidistant projection for distance calculation
  st_transform(crs = 21097) %>%
  mutate(distance = mt_distance(., units = as_units('m')), #need to project first to give meaningful distance measurement
         time_lag = mt_time_lags(.)) %>%
  st_transform(crs = 4326)

#save move2 object as RDS file
saveRDS(human.mt, here('output','spatial','DTRANK_human_pts_all_move2.RDS'))

mt_n_tracks(human.mt)
hist(human.mt$time_lag)

#only track level data
mt_track_data(human.mt)

#make line object from points
mt_track_lines(human.mt)

#autocorellation function acf()
acf(human.mt$azimuth)


#ctmm

#test - from raw points
human.tele <- human.pts[[1]] %>%
  rename(timestamp = date_time,
         longitude = lon,
         latitude = lat,
         individual_id = id) %>%
  mutate(timestamp = as.POSIXct(timestamp, tz = 'UTC')) %>%
  as.telemetry()

#test - from move2 - not working
human.tele <- human.mt %>%
  rename(timestamp = date_time,
         longitude = lon,
         latitude = lat,
         individual_id = id) %>%
  mutate(timestamp = as.POSIXct(timestamp, tz = 'UTC')) %>%
  as.telemetry()

human.tele <- list()
human.ctmm.models <- list()
human.akde <- list()
human.akde.summary <- data.frame(id = character(),
                                 adke.est = numeric(),
                                 akde.ci.low = numeric(),
                                 adke.ci.high = numeric(),
                                 unit = character())

for (track in names(human.pts)) {
  human.tele[[track]] <- human.pts[[track]] %>%
    rename(timestamp = date_time,
           longitude = lon,
           latitude = lat,
           individual_id = id) %>%
    mutate(timestamp = as.POSIXct(timestamp, tz = 'UTC')) %>%
    as.telemetry()
  
  human.ctmm.models[[track]] <- ctmm.fit(human.tele[[track]], 
                                         ctmm.guess(human.tele[[track]], interactive = FALSE))
  
  human.akde[[track]] <- akde(human.tele[[track]], human.ctmm.models[[track]])
  
}

human.akde.summary <- data.frame(id = character(),
                                 adke.est = numeric(),
                                 akde.ci.low = numeric(),
                                 adke.ci.high = numeric(),
                                 unit = character())

for (i in 1:nrow(human.akde)) {
  sum <- summary(human.akde[i])
  
  
}

test <- ctmm.fit(human.tele[[1]], ctmm.guess(human.tele[[1]], interactive = FALSE))

auto.kde <- akde(human.tele[[track]], test)
summary(auto.kde)


####livestock----
## all tracks as same table
livestock.mt <- bind_rows(livestock.pts) %>% 
  mutate(datetime = as_datetime(datetime)) %>%
  #add livestock details
  left_join(read.csv(here('output','DTRANK_individual_livestock.csv')) %>%
              filter(short_term_selected == 'yes') %>%
              mutate(livestock_age = case_when(
                livestock_species == "cattle" ~ cattle_age,
                livestock_species == "camel" ~ camel_age,
                livestock_species == "sheep" ~ sheep_age,
                livestock_species == "goat" ~ goat_age
              ),
              livestock_breed = case_when(
                livestock_species == "cattle" ~ cattle_breed,
                livestock_species == "camel" ~ camel_breed,
                livestock_species == "sheep" ~ sheep_breed,
                livestock_species == "goat" ~ goat_breed
              ),
              id = paste0(hh_id,'_',livestock_species,'_',gps_logger_id)) %>%
              select(id,sex,livestock_age,livestock_breed),
            by = 'id') %>%
  mt_as_move2(coords = c('longitude.e.w','latitude.n.s'), crs = 4326, 
              time_column = 'datetime', 
              track_id_column = 'id',
              track_attributes = c('hh_id','county','ward','community','livestock_species','livestock_breed','livestock_age','gps_logger_id')) %>% #,'sex'
  #sex throwing an error due to both a male and female sheep being recorded as carrying GPS 8 in the individual livestock data
  #calculate azimuth while in geographic CRS
  mutate(azimuth = mt_azimuth(.),
         turn_angle = mt_turnangle(.)) %>%
  #transform to equidistant projection for distance calculation
  st_transform(crs = 21097) %>%
  mutate(distance = mt_distance(., units = as_units('m')), #need to project first to give meaningful distance measurement
         time_lag = mt_time_lags(.)) %>%
  st_transform(crs = 4326)

#save move2 object as RDS file
saveRDS(livestock.mt, here('output','spatial','DTRANK_livestock_pts_all_move2.RDS'))

mt_n_tracks(livestock.mt)
hist(livestock.mt$time_lag)

#only track level data
mt_track_data(livestock.mt)

#make line object from points
mt_track_lines(livestock.mt)

#autocorellation function acf()
acf(livestock.mt$azimuth)




##long term ceres tags----
gps.long <- read_delim(last(list.files(here('input','raw','spatial','long_term'), full.names = T)), 
                       delim = "\t", 
                       locale = locale(encoding = "UTF-16LE")) %>%
  mutate(across(where(is.character), ~ str_trim(str_replace_all(., '^="|"$', '')))) %>%
  #make move2 object
  mt_as_move2(coords = c('lon','lat'), crs = 4326, 
              time_column = 'local_datetime', 
              track_id_column = 'vid',
              track_attributes = 'esn') %>%
  #calculate azimuth while in geographic CRS
  mutate(azimuth = mt_azimuth(.),
         turn_angle = mt_turnangle(.)) %>%
  #transform to equidistant projection for distance calculation
  st_transform(crs = 21097) %>%
  mutate(distance = mt_distance(., units = as_units('m')), #need to project first to give meaningful distance measurement
         time_lag = mt_time_lags(.)) %>%
  #attach livestock information
  left_join(read.csv(here('output','DTRANK_individual_livestock.csv')) %>% 
              select(hh_id, county, ward, livestock_species, sex, gps_logger_id, ceres_tag_vid),
            by = c('vid'='ceres_tag_vid'))


##community maps----
community.id <- read.csv(here('output','DTRANK_community_household_id_reference.csv'))
st_layers(here('output','spatial','DTRANK_activity_space_mapping.gpkg'))
space.map.pts <- st_read(here('output','spatial','DTRANK_activity_space_mapping.gpkg'), layer = 'main_household')# %>%
left_join(select(community.id,-date), by = 'hh_id')
space.map.poly <- st_read(here('output','spatial','DTRANK_activity_space_mapping.gpkg'), layer = 'activity_space_mapping_polygons')# %>%
left_join(select(community.id,-date), by = 'hh_id')



##interactive map----
#import study site squares
study.sites <- st_read(here('input','spatial','longitudinal','5x5_final.shp'))
#### for longitudinal study 
study.sites.long <- st_read(here('input','spatial','longitudinal','longitudinal.shp')) %>% st_transform(crs = 4326)

long.hh <- read.csv(here('input','spatial','longitudinal','longitudinal_HHs.csv'))

#create colour palette
group_colors <- c(
  "Humans"      = "blue",
  "Cattle"      = "brown",
  "Camels"      = "orange",
  "Sheep"       = "grey",
  "Goats"       = "green",
  "Household"   = "red",
  "Waterpoints" = "blue",
  "Grazing"     = "darkgreen",
  "Crops"       = "brown",
  "Ticks"       = "black",
  "Rodents"     = "pink",
  "Hares"       = "lightblue",
  "Study Sites" = "red"
  #"Bats"        = "yellow"
)

#palette function
dtra.map.pal <- leaflet::colorFactor(
  palette = unname(group_colors),
  domain  = names(group_colors),
  levels = names(group_colors)
)

#choose county data to map
county.filter <- 'samburu'

#create map
dtra.map <- leaflet() %>%
  addTiles(group = "Base Map") %>%
  
  ## add sample sites
  addPolygons(data = study.sites.long %>% 
                st_zm(drop = T),
              color = group_colors[['Study Sites']],
              #stroke = F,
              weight = 2,
              fillOpacity = 0,
              label = "Study Sites",
              group = "Study Sites") %>%
  
  # Humans
  # addCircleMarkers(data = human.mt %>% filter_track_data(hh_id == 'DHH010007'),
  #                  color = "blue",
  #                  radius = 2,
  #                  label = "Humans",
  #                  group = "Humans") %>%
  addPolylines(data = mt_track_lines(human.mt %>% 
                                      # filter_track_data(county == 'samburu')
                                     filter_track_data(hh_id %in% long.hh$hh_id)),# 
  color = group_colors[['Humans']],
  weight = 1,
  label = "Humans",
  group = "Humans") %>%
  
  # Livestock species
  #cattle
  # addCircleMarkers(data = livestock.mt %>% filter_track_data(livestock_species == 'cattle' & hh_id == 'DHH010007'),
  #                  color = "brown",
  #                  radius = 2,
  #                  label = "Cattle",
  #                  group = "Cattle") %>%
  addPolylines(data = mt_track_lines(livestock.mt %>% 
                                       filter_track_data(livestock_species == 'cattle' & 
                                                           hh_id %in% long.hh$hh_id)),
                                                          # county == 'samburu'
               color = group_colors[['Cattle']],
               weight = 1,
               label = "Cattle",
               group = "Cattle") %>%
  
  #camel
  # addCircleMarkers(data = livestock.mt %>% filter_track_data(livestock_species == 'camel'& hh_id == 'DHH010007'),
  #                  color = "orange",
  #                  radius = 2,
  #                  label = "Camels",
  #                  group = "Camels") %>%
  addPolylines(data = mt_track_lines(livestock.mt %>% filter_track_data(livestock_species == 'camel' & 
                                                                          hh_id %in% long.hh$hh_id)),
                                                                        #county == 'samburu'#
               color = group_colors[['Camels']],
               weight = 1,
               label = "Camels",
               group = "Camels") %>%
  #sheep
  # addCircleMarkers(data = livestock.mt %>% filter_track_data(livestock_species == 'sheep'& hh_id == 'DHH010007'),
  #                  color = "grey",
  #                  radius = 2,
  #                  label = "Sheep",
  #                  group = "Sheep") %>%
  addPolylines(data = mt_track_lines(livestock.mt %>% filter_track_data(livestock_species == 'sheep' & 
                                                                          hh_id %in% long.hh$hh_id)),
                                                                          #county == 'samburu'#
               color = group_colors[['Sheep']],
               weight = 1,
               label = "Sheep",
               group = "Sheep") %>%
  #goat
  # addCircleMarkers(data = livestock.mt %>% filter_track_data(livestock_species == 'goat'& hh_id == 'DHH010007'),
  #                  color = "green",
  #                  radius = 2,
  #                  label = "Goats",
  #                  group = "Goats") %>%
  addPolylines(data = mt_track_lines(livestock.mt %>% filter_track_data(livestock_species == 'goat' & 
                                                                          hh_id %in% long.hh$hh_id)),
                                                                          #county == 'samburu'#
               color = group_colors[['Goats']],
               weight = 1,
               label = "Goats",
               group = "Goats") %>%
  
  #add activity space mapping
  #main households
  addCircleMarkers(data = act.space.map$main %>% filter(hh_id %in% long.hh$hh_id),# county == 'samburu'
                   color = group_colors[['Household']],
                   radius = 5,
                   stroke = F,
                   fillOpacity = 1,
                   label = "Household",
                   group = "Household") %>%
  #water
  addCircleMarkers(data = act.space.map$water %>% filter(hh_id %in% long.hh$hh_id),# county == 'samburu'
                   color = group_colors[['Waterpoints']],
                   radius = 4,
                   stroke = F,
                   fillOpacity = 0.8,
                   label = ~type_water,
                   group = "Waterpoints") %>%
  
  #grazing
  addPolygons(data = act.space.map$grazing %>% filter(hh_id %in% long.hh$hh_id) %>% #county == 'samburu'
                st_zm(drop = T),
              color = group_colors[['Grazing']],
              stroke = F,
              label = ~seasonality_grazing,
              group = "Grazing") %>%
  #crops
  addPolygons(data = act.space.map$crops %>% filter(hh_id %in% long.hh$hh_id) %>% #county == 'samburu'
                st_zm(drop = T),
              color = group_colors[['Crops']],
              stroke = F,
              label = "Crops",
              group = "Crops") %>%
  #ticks
  addCircleMarkers(data = act.space.map$ticks %>% filter(hh_id %in% long.hh$hh_id),# county == 'samburu'
                   color = group_colors[['Ticks']],
                   radius = 4,
                   stroke = F,
                   fillOpacity = 0.8,
                   label = "Ticks",
                   group = "Ticks") %>%
  #rodents
  addCircleMarkers(data = act.space.map$rodents %>% filter(hh_id %in% long.hh$hh_id),# county == 'samburu'
                   color = group_colors[['Rodents']],
                   radius = 4,
                   stroke = F,
                   fillOpacity = 0.8,
                   label = "Rodents",
                   group = "Rodents") %>%
  #hares
  addCircleMarkers(data = act.space.map$hares %>% filter(hh_id %in% long.hh$hh_id),# county == 'samburu'
                   color = group_colors[['Hares']],
                   radius = 4,
                   stroke = F,
                   fillOpacity = 0.8,
                   label = "Hares",
                   group = "Hares") %>%
  #bats
  # addCircleMarkers(data = act.space.map$bats %>% filter(county == 'samburu'),# 
  #                  color = group_colors[['Bats']],
  #                  radius = 4,
  #                  stroke = F,
  #                  fillOpacity = 0.8,
  #                  label = "Bats",
  #                  group = "Bats") %>%
  
  #add layer controls
  addLayersControl(
    overlayGroups = names(group_colors),
    # c('Humans', 'Cattle', 'Camels', 'Sheep', 'Goats',
    #                 'Household', 'Waterpoints','Grazing','Crops','Ticks','Rodents','Hares','Bats'),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  #add legend
  addLegend(
    position = "bottomright",
    pal      = dtra.map.pal,
    values   = names(group_colors),
    title    = "Map Layers",
    opacity  = 1
  )


dtra.map

#save map
saveWidget(dtra.map, file = here('docs','DTRANK_longitudinal_spatial_data.html'), selfcontained = T)

#adding activity space maps in loop
for (i in seq_along(activity.space.map)) {
  layer <- activity.space.map[[i]]
  lname <- paste0("Activity Space ", i)
  
  if (inherits(layer, "sf")) {
    if (any(st_geometry_type(layer) %in% c("POINT", "MULTIPOINT"))) {
      m <- m %>% addCircleMarkers(data = layer,
                                  color = "purple",
                                  radius = 3,
                                  label = ~lname,
                                  group = lname)
    } else {
      m <- m %>% addPolygons(data = layer,
                             color = "purple",
                             fillOpacity = 0.3,
                             label = ~lname,
                             group = lname)
    }
  }
}


##old code----------------------------------------
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
  #### data cleaning & spatial analysis #
  
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
  
  #### GIS analysis #
  
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
  
  #### movement calculations #
  
  #claculate length of the track
  gps.summary$dist.m[i] <- st_length(as_sf_lines(mk.track[[i]]))
  
  #calcuate time difference
  gps.summary$time.h[i] <- difftime(gps[[i]]$DATETIME[nrow(gps[[i]])], gps[[i]]$DATETIME[1], units = "hours")
  
  #calculate speed
  gps.summary$speed.kmh[i] <- (gps.summary$dist.m[i]/1000)/gps.summary$time.h[i]
  
  # #### NDVI ##
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
  
  
  #### mapping & image export #
  
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
  
  
  ######### NDVI
  
} 


