# DTRA-NK - Data Download and Preliminary Cleaning
# Sirimon Thomas
# April 2024

#need the development version of robotoolbox
#remotes::install_gitlab("dickoa/robotoolbox")

library(tidyverse)
library(here)
library(robotoolbox)
library(dm)
library(sf)
library(httr)
library(googledrive)

# #import functions
source(here('code','0_functions.R'))
#set username and password for Kobo account
#!! SENSITIVE PASSWORD INFORMATION !!
uname <-  "dtrank_master"
pwd <-  "M!tchbc13579"
url <-  "kf.kobotoolbox.org"
####setup----
#for robotoolbox package
kobo_setup(url = "https://kf.kobotoolbox.org/",
           token = "1123cf35535bde15c4452187ba990a616dc492c0")
#get list of assets
kobo.projects <- kobo_asset_list()

####data download-----
#forms with repeats
##activity space mapping

#forms with media
##GPS vector- gps files
##wildlife sampling- images
##ecotones- images
##FGD- audio, images


#download all data and write to .csv
#takes a long time!
kobo.data <- list()

for (i in 1:nrow(kobo.projects)) {
  print(paste(as.character(i),':',kobo.projects$name[i]))
  if (kobo.projects$submissions[i]>0){#this excludes projects with no submission as they cause an error with the download
    data <- kobo_data(
      kobo.projects$uid[i],
      all_versions = FALSE,
      #lang = 'xml',
      progress = TRUE
    ) 
    if (kobo.projects$name[i] != 'DTRANK_activity_space_mapping'){
      data <- data %>%
        #merge scanned and manually entered barcodes
        tidy_barcodes() %>%
        #extract details of media attachments from list-type column
        unnest_wider(`_attachments`) %>%
        #remove unneeded columns
        tidy_strip_pii()
    }
    
    ## misc data cleaning
    #GPS-vector - camel with ceres tag but no short term collar in DHH0100053
    if (kobo.projects$name[i] == 'DTRANK_GPS_vector'){
      data[data$`_uuid`=='aab5f780-58a5-464f-936c-cae41509539f','gps_logger_id_initial'] <- NA
    }
    
    #individual_human - tidy ages
    if (kobo.projects$name[i] == 'DTRANK_individual_human'){
      data <- data %>% mutate(
        participant_age = ifelse(is.na(participant_age),as.numeric(age),participant_age),
        participant_age = round(participant_age)) %>% 
        select(-c(dob, participant_dob, age))
    }
    
    #store datasets in list
    kobo.data[[kobo.projects$name[i]]] <- data
    
    #extract main dataset from activity space mapping data to save to .csv
    if (kobo.projects$name[i] == 'DTRANK_activity_space_mapping'){
      data <- data$main %>% tidy_barcodes() %>% unnest_wider(`_attachments`) %>%
        #remove unneeded columns
        tidy_strip_pii()
    }
    
    
    #save
    write.csv(data,
              file = here('input','raw', paste0(kobo.projects$name[i],'.csv')),
              row.names = F,
              na = '')
    
    print(paste(as.character(i),':',kobo.projects$name[i],'download and writing completed successfully'))
  }
  else {print(paste('There are no submissions for',kobo.projects$name[i],'so it has not been downloaded'))}
}

#### blood sample daughter codes & data checks -----
# re-attach serum sample daughter ID to individual human and livestock data,
# check for data entry errors
missing.entries <- list()
for (j in c('DTRANK_wildlife_sampling','DTRANK_individual_human','DTRANK_individual_livestock')) {
  
  #change wildlife column name to allow joining
  if(j == 'DTRANK_wildlife_sampling') {kobo.data[[j]] <- rename(kobo.data[[j]], Serum_blood_parentID = secd_serum)}
  
  #add daughter sample codes
  kobo.data[[j]] <- kobo.data[[j]] %>% 
    add_daughter_samples()
  
  
  ####data quality checks
  
  #check for duplicate parent blood IDs
  if(TRUE %in% duplicated(kobo.data[[j]]$Serum_blood_parentID)){
    print('There are duplicated parent blood sample IDs')
    print(kobo.data[[j]] %>% filter(duplicated(Serum_blood_parentID)))
  }
  
  #check for individual data entries without matching serum sample booking
  if(nrow(kobo.data$DTRANK_serum_sample_booking %>% filter(sample == ifelse(j == 'DTRANK_wildlife_sampling','wildlife',
                                                                            ifelse(j == 'DTRANK_individual_human','human',
                                                                                   'livestock')))) != nrow(kobo.data[[j]])){
    print(paste0('The number of ',ifelse(j == 'DTRANK_wildlife_sampling','wildlife',
                                         ifelse(j == 'DTRANK_individual_human','human',
                                                'livestock')),' entries in the serum_sample_booking data are different to the number of entries in the ',
                 j,' data'))
    
    print(paste0('serum_sample_booking: ',nrow(kobo.data$DTRANK_serum_sample_booking %>% filter(sample == ifelse(j == 'DTRANK_wildlife_sampling','wildlife',
                                                                                                                 ifelse(j == 'DTRANK_individual_human','human',
                                                                                                                        'livestock'))))))
    print(paste0(j,': ',nrow(kobo.data[[j]])))
    
    #entries in serum_sample_booking that are not in individual sample datasets
    if(nrow(kobo.data$DTRANK_serum_sample_booking %>% filter(sample == ifelse(j == 'DTRANK_wildlife_sampling','wildlife',
                                                                              ifelse(j == 'DTRANK_individual_human','human',
                                                                                     'livestock')) & 
                                                             (Serum_blood_parentID %!in% kobo.data[[j]]$Serum_blood_parentID)))>0){
      print(paste0('These entries are in the serum_sample_booking data but are not in the ',j,' data'))
      print(kobo.data$DTRANK_serum_sample_booking %>% filter(sample == ifelse(j == 'DTRANK_wildlife_sampling','wildlife',
                                                                              ifelse(j == 'DTRANK_individual_human','human',
                                                                                     'livestock')) & 
                                                               (Serum_blood_parentID %!in% kobo.data[[j]]$Serum_blood_parentID)))
      
      missing.entries[['DTRANK_serum_sample_booking']] <- kobo.data$DTRANK_serum_sample_booking %>% filter(sample == ifelse(j == 'DTRANK_wildlife_sampling','wildlife',
                                                                                                                            ifelse(j == 'DTRANK_individual_human','human',
                                                                                                                                   'livestock')) & 
                                                                                                             (Serum_blood_parentID %!in% kobo.data[[j]]$Serum_blood_parentID))
    }
    
    #entries in individual sample datasets that are not in the serum_sample_booking data
    if(nrow(kobo.data[[j]] %>% filter(Serum_blood_parentID %!in% kobo.data$DTRANK_serum_sample_booking$Serum_blood_parentID))>0){
      print(paste0('These entries are in the ',j,' data but not in the serum_sample_booking data'))
      print(kobo.data[[j]] %>% filter(Serum_blood_parentID %!in% kobo.data$DTRANK_serum_sample_booking$Serum_blood_parentID))
      missing.entries[[j]] <- kobo.data[[j]] %>% filter(Serum_blood_parentID %!in% kobo.data$DTRANK_serum_sample_booking$Serum_blood_parentID)
    }
  }    
  
  #save
  write.csv(kobo.data[[j]],
            file = here('output',paste0(j,'.csv')),
            row.names = F,
            na = '')
} 

#list2env(kobo.data,globalenv()) #only needed for testing- creates objects of all data in the global environment

#### activity space mapping ----
#sort spatial data in repeats into combined point and polygon objects
#create empty lists for point and area data
pts <- list()
area <- list()

for (type in names(kobo.data$DTRANK_activity_space_mapping)[2:length(kobo.data$DTRANK_activity_space_mapping)]) {
  #note - for some reason when downloading from the server, some of the wrong columns are included in the repeat datasets
  
  #points data
  if (type %!in% c('grazing_rep','crops_rep')) {
    pts[[type]] <- dm_flatten_to_tbl(kobo.data$DTRANK_activity_space_mapping,.start = !!as.name(type),.join=left_join) %>%
      dplyr::mutate(
        location_type = str_remove(type,'_rep')) %>%
      select(-starts_with('_'), -contains(paste0('.',type)))
    #change column names so row_binding will create a single column for repeated variables
    names(pts[[type]]) <- str_remove_all(names(pts[[type]]),paste(paste0('_',str_remove_all(type,'_rep')),
                                                                  paste0(str_remove_all(type,'_rep'),'_'),
                                                                  '.main',
                                                                  sep = '|'))
    
    #area/geoshape/polygon data
  } else if (type %in% c('grazing_rep','crops_rep')) {
    area[[type]] <- dm_flatten_to_tbl(kobo.data$DTRANK_activity_space_mapping,.start = !!as.name(type),.join=left_join) %>%
      dplyr::mutate(
        location_type = str_remove(type,'_rep')) %>%
      select(-starts_with('_'), -contains(paste0('.',type)))
    #change column names so row_binding will create a single column for repeated variables
    names(area[[type]]) <- str_remove_all(names(area[[type]]),paste(paste0('_',str_remove_all(type,'_rep')),
                                                                    paste0(str_remove_all(type,'_rep'),'_'),
                                                                    '.main',
                                                                    sep = '|'))
  }
}

#combine points data and create spatial object
points <- bind_rows(pts) %>%
  select(location_type, hh_id,date,
         everything(),
         -contains(c('yn','action_taken_','_count','_rep_count','action_taken.')),
         -c(start,end,dev_id,uuid,instanceID),
         'waterpoint_type'='type','water_use_livestock_human'='livestock_human') %>%
  tidy_barcodes() %>%
  st_as_sf(wkt = 'gps_wkt', crs = 'epsg:4326', remove = F)
#write to gpkg
st_write(points,
         dsn = here('output','spatial','DTRANK_activity_space_mapping.gpkg'),
         layer = 'activity_space_mapping_points',
         append = F)

#combine polygon data and create spatial object
areas <- bind_rows(area) %>%
  select(location_type, hh_id,date,
         everything(),
         -contains(c('yn','action_taken_','_count','_rep_count')),
         -c(start,end,dev_id,uuid,instanceID),
         'geometry' = 'gps_area_wkt') %>%
  tidy_barcodes() %>%
  st_as_sf(wkt = 'geometry', crs='epsg:4326')
#write to gpkg
st_write(areas,
         dsn = here('output','spatial','DTRANK_activity_space_mapping.gpkg'),
         layer = 'activity_space_mapping_polygons',
         append = F)

### mapping activity space mapping data for each community??????----



#### GPS_vector data tidying ----
#separate gps.vector data sections and save to a list
gps.vector.data <- list()
for (section in unique(kobo.data$DTRANK_GPS_vector$collection)) {
  if (section == 'initial'){
    gps.vector.data[[section]] <- kobo.data$DTRANK_GPS_vector %>% 
      select(date,county,ward,collection,hh_id,
             contains('acaracide'),
             contains(section)) %>%
      filter(collection == section & gps_tick_initial == 'gps_tick')# %>%
    #rename_with(~str_remove_all(., c('.gps_info_initial|.vectors_initial|.gps_info_final|.vectors_final')))
  } else if (section == 'environment'){
    gps.vector.data[[section]] <- kobo.data$DTRANK_GPS_vector %>% 
      select(date,county,ward,collection,hh_id,
             contains('tick_trap_gps'),
             temp, humidity,
             contains('tick_env')) %>%
      filter(collection == section)
  } else {
    gps.vector.data[[section]] <- kobo.data$DTRANK_GPS_vector %>% 
      select(date,county,ward,collection,hh_id,
             contains(section),
             download_url, mimetype) %>%
      filter(collection == section)
  }
}
#combine initial and final collar data
gps.vector.data$livestock <- full_join(gps.vector.data[['initial']],
                                       gps.vector.data[['final']],
                                       by=c('county',
                                            'ward',
                                            'hh_id',
                                            'livestock_species_initial'='livestock_species_final',
                                            'gps_logger_id_initial'='gps_logger_id_final')) %>% 
  select(hh_id,county,ward,
         'livestock_species'='livestock_species_initial','gps_logger_id'='gps_logger_id_initial',
         'date_deployed'='date.x','date_retrieved'='date.y',
         'ceres_tag_id'='ceres_tag_vid_initial',
         contains('ticks'),
         download_url, mimetype,
         -'gps_tick_initial',-'gps_tick_final')


#### download movement tracks ----
######livestock movement ----
gps.livestock.data <- list()
for (i in 1:nrow(gps.vector.data$livestock)) {
  if (paste0(gps.vector.data$livestock$hh_id[i],'_',
             gps.vector.data$livestock$livestock_species[i],'_',
             gps.vector.data$livestock$gps_logger_id[i],'.csv') %!in% list.files(here('input','raw','spatial','livestock')) &
      
      !is.na(gps.vector.data$livestock$download_url[i])) {
    
    # deal with .zip files
    #if (str_detect(gps.vector.data$livestock$download_url[i],'.zip')){#.zip files - DHH010038_sheep_7.csv
    if (gps.vector.data$livestock$mimetype[i] == 'application/zip'){#.zip files - DHH010038_sheep_7.csv
      media.file <- content(GET(url = gps.vector.data$livestock$download_url[i],
                                config = authenticate(user=uname,password=pwd)),
                            as='raw') %>% 
        #write .zip files
        writeBin(here('input','raw','spatial','livestock','zip_files',paste0(gps.vector.data$livestock$hh_id[i],
                                                                             '_',
                                                                             gps.vector.data$livestock$livestock_species[i],
                                                                             '_',
                                                                             gps.vector.data$livestock$gps_logger_id[i],
                                                                             '.zip')))
      
      #create a temporary folder to dump extracted .csv files
      temp <- tempfile()
      #unzip downloaded .zip file
      unzip(here('input','raw','spatial','livestock','zip_files',paste0(gps.vector.data$livestock$hh_id[i],
                                                                        '_',
                                                                        gps.vector.data$livestock$livestock_species[i],
                                                                        '_',
                                                                        gps.vector.data$livestock$gps_logger_id[i],
                                                                        '.zip')), exdir = temp)
      #read and row bind all extracted .csv files
      media.file <- do.call(rbind, lapply(list.files(temp, full.names = T), read.csv))
      #remove temporary folder
      unlink(temp)
      
    } else {
      ##  #download media file
      media.file <- content(GET(url = gps.vector.data$livestock$download_url[i],
                                config = authenticate(user=uname,password=pwd)),
                            as='text', encoding = 'UTF-8') %>%
        #parse to dataframe
        read.table(text=., header = TRUE, fill = TRUE, sep = ",")
    }
    
    if (nrow(media.file)>0) {#DHH010038_sheep_7 - zip file
      
      #save to list for further cleaning
      gps.livestock.data[[i]] <- media.file
      
      #export to folder
      write.csv(media.file,here('input','raw','spatial','livestock',paste0(gps.vector.data$livestock$hh_id[i],
                                                                           '_',
                                                                           gps.vector.data$livestock$livestock_species[i],
                                                                           '_',
                                                                           gps.vector.data$livestock$gps_logger_id[i],
                                                                           '.csv')),
                row.names = F,
                na = '')
    }
  }
}


######human movement ----
gps.human.data <- list()
for (i in 1:nrow(gps.vector.data$human)) {
  if (paste0(gps.vector.data$human$hh_id[i],'_',
             gps.vector.data$human$collection[i],'_',
             gps.vector.data$human$gps_logger_id_human[i],'.csv') %!in% list.files(here('input','raw','spatial','human')) &
      
      !is.na(gps.vector.data$human$gps_track_file_human[i]) & 
      
      gps.vector.data$human$gps_track_file_human[i] != '')
  {
    #download media file
    #if (str_detect(gps.vector.data$human$gps_track_file_human[i],'.csv')){#.csv files
    if (gps.vector.data$human$mimetype[i] == 'text/comma-separated-values'){#.csv files
      media.file <- content(GET(url = gps.vector.data$human$download_url[i],
                                config = authenticate(user=uname,password=pwd)),
                            as='text', encoding = 'UTF-8') %>% 
        #parse to dataframe
        read.table(text=., header = TRUE, fill = TRUE, sep = ",") %>%
        mutate(Time = as_datetime(str_remove_all(Time,'"|='), tz = "africa/nairobi"),
               Altitude.m. = ifelse(TRUE %in% str_detect(names(.),'Altitude.ft.'), .$Altitude.ft.,.$Altitude.m.)) %>%
        select(date_time = Time, lat = Latitude, lon = Longitude, elevation = Altitude.m.,satellites = Satellites.CN.22.,hdop = HDOP)
      
    } else if (gps.vector.data$human$mimetype[i] == 'application/octet-stream'){
      #if (str_detect(gps.vector.data$human$gps_track_file_human[i],'.bin')){#.gpx files
      content(GET(url = gps.vector.data$human$download_url[i],
                  config = authenticate(user=uname,password=pwd)),
              as='raw') %>% 
        
        #write .gpx files
        writeBin(here('input','raw','spatial','human','gpx_files',paste0(gps.vector.data$human$hh_id[i],
                                                                         '_',
                                                                         gps.vector.data$human$collection[i],
                                                                         '_',
                                                                         gps.vector.data$human$gps_logger_id_human[i],
                                                                         '.gpx')))
      #read from written .gpx file
      media.file <- st_read(here('input','raw','spatial','human','gpx_files',paste0(gps.vector.data$human$hh_id[i],
                                                                                    '_',
                                                                                    gps.vector.data$human$collection[i],
                                                                                    '_',
                                                                                    gps.vector.data$human$gps_logger_id_human[i],
                                                                                    '.gpx')), layer = 'track_points') %>%
        mutate(lon = sf::st_coordinates(.)[,1],
               lat = sf::st_coordinates(.)[,2],
               time = force_tz(time, tzone = "africa/nairobi")) %>%
        select(Time = time, lat, lon, elevation = ele, satellites = sat, hdop)
    }
    
    #save to list for further cleaning
    gps.human.data[[i]] <- media.file
    
    #export to folder
    #might need to edit this if files are sent to Kobo as .gpx
    write.csv(media.file,here('input','raw','spatial','human',paste0(gps.vector.data$human$hh_id[i],
                                                                     '_',
                                                                     gps.vector.data$human$collection[i],
                                                                     '_',
                                                                     gps.vector.data$human$gps_logger_id_human[i],
                                                                     '.csv')),
              row.names = F,
              na = '')
  }
}

#### load & attach movement tracks ----

##### livestock ----
for (file in list.files(here('input','raw','spatial','livestock'), pattern = '.csv')) {
  gps.data <- read.csv(here('input','raw','spatial','livestock',file), na.strings = '') %>% 
    mutate(
      #tidy date and time columns
      #TIME = strptime(TIME, format = "%H:%M:%S"),
      TIME = format(strptime(sprintf("%06d", TIME), format="%H%M%S"), format = "%H:%M:%S"),
      DATE = as.Date(as.POSIXlt(strptime(DATE, format="%y%m%d"))),
      DATETIME = as.POSIXlt(paste(DATE, TIME), format = "%Y-%m-%d %H:%M:%S"),
      #remove N or S from latitude and make southern points negative
      LATITUDE.N.S = case_when(grepl('N', LATITUDE.N.S) ~ as.numeric(gsub('N','', LATITUDE.N.S)),
                               grepl('S', LATITUDE.N.S) ~ as.numeric(gsub('S','', LATITUDE.N.S)) *-1),

      #remove E from longitude
      LONGITUDE.E.W = as.numeric(gsub('E','', LONGITUDE.E.W))
    ) %>%
    st_as_sf(coords = c('LONGITUDE.E.W', 'LATITUDE.N.S'), crs = 4326) %>% 
    summarise(geometry = st_combine(geometry)) %>%
    st_cast('MULTIPOINT') %>%
    mutate(hh_id = str_split(file,'_')[[1]][1],
           livestock_species = str_split(file,'_')[[1]][2],
           gps_logger_id = as.double(gsub('.csv','',str_split(file,'_')[[1]][3])))
  
  if (file == list.files(here('input','raw','spatial','livestock'), pattern = '.csv')[1]){
      gps.multipoints.livestock <- gps.data
  } else {
    gps.multipoints.livestock <- bind_rows(gps.multipoints.livestock, gps.data)
      }
}

gps.vector.data$livestock.sf <- left_join(gps.vector.data$livestock, gps.multipoints.livestock,
                                          by = c('hh_id','livestock_species','gps_logger_id')) %>%
  st_as_sf()

##### humans ----
for (file in list.files(here('input','raw','spatial','human'), pattern = '.csv')) {
  gps.data <- read.csv(here('input','raw','spatial','human',file), na.strings = '') %>% 

    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
    summarise(geometry = st_combine(geometry)) %>%
    st_cast('MULTIPOINT') %>%
    mutate(hh_id = str_split(file,'_')[[1]][1],
           gps_logger_id_human = as.double(gsub('.csv','',str_split(file,'_')[[1]][3])))
  
  if (file == list.files(here('input','raw','spatial','human'), pattern = '.csv')[1]){
    gps.multipoints.human <- gps.data
  } else {
    gps.multipoints.human <- bind_rows(gps.multipoints.human, gps.data)
  }
}

gps.vector.data$human.sf <- left_join(gps.vector.data$human, gps.multipoints.human,
                                          by = c('hh_id','gps_logger_id_human')) %>%
  st_as_sf()


# save separate GPS & vector csv files
write.csv(gps.vector.data$livestock, here('output','DTRANK_livestock_GPS_vector.csv'), row.names = F, na = '')
write.csv(gps.vector.data$human, here('output','DTRANK_human_GPS.csv'), row.names = F, na = '')
write.csv(gps.vector.data$environment, here('output','DTRANK_evironment_vector.csv'), row.names = F, na = '')

# save human and livestock spatial files
st_write(gps.vector.data$livestock.sf,
         dsn = here('output','spatial','DTRANK_livestock_GPS_vector_multipoint.gpkg'),
         layer = 'livestock_GPS_vector_multipoint',
         append = F)
st_write(gps.vector.data$human.sf,
         dsn = here('output','spatial','DTRANK_human_GPS_multipoint.gpkg'),
         layer = 'human_GPS_multipoint',
         append = F)




#### download image attachments ----
#download images for ecotones
for (i in 1:nrow(kobo.data$DTRANK_ecotones)) {
  if (kobo.data$DTRANK_ecotones$ecotone_image[i] %!in% list.files(here('output','images','ecotones'))) {
    GET(url = kobo.data$DTRANK_ecotones$download_large_url[i],
        config = authenticate(user=uname,password=pwd),
        write_disk(here(here('output','images','ecotones'), kobo.data$DTRANK_ecotones$ecotone_image[i]), overwrite = T))
  }
}

#download images for wildlife
for (i in 1:nrow(kobo.data$DTRANK_wildlife_sampling)) {
  if (!is.na(kobo.data$DTRANK_wildlife_sampling$filename[i])){#skip rows with no images
    for (y in 1:length(kobo.data$DTRANK_wildlife_sampling$filename[[i]])) {#loop through list of files in each row
      if (sub(".*/", "",kobo.data$DTRANK_wildlife_sampling$filename[[i]][y]) %!in% list.files(here('output','images','wildlife'))) {
        
        GET(url = kobo.data$DTRANK_wildlife_sampling$download_large_url[[i]][y],
            config = authenticate(user=uname,password=pwd),
            write_disk(here('output','images','wildlife', sub(".*/", "",kobo.data$DTRANK_wildlife_sampling$filename[[i]][y])), overwrite = T))
        
      }
    } 
  } 
} 


####summary table-----
human <- kobo.data$DTRANK_individual_human %>%
  summarise(blood.samples = n(),
            GPS.tracks = sum(movement_study=='yes')) %>%
  mutate(ID = 'Human')

# ticks <- gps.collars %>%
#   group_by(livestock_species) %>%
#   summarise(blood.samples = n(),
#             Ceres.tags = sum(!is.na(ceres_tag_id)),
#             Ticks = sum(ticks_initial_total, na.rm = T) + 
#               sum(ticks_final_total, na.rm = T))
## NB 25 goats in GPS_vector data but only 24 in livestock data---??
livestock <- kobo.data$DTRANK_individual_livestock %>%
  group_by(livestock_species) %>%
  summarise(blood.samples = n(),
            GPS.tracks = sum(short_term_selected=='yes'),
            Ceres.tags = sum(long_term_selected=='yes')) %>%
  left_join(gps.vector.data$livestock %>%
              group_by(livestock_species) %>%
              summarise(Ticks = sum(ticks_initial_total, na.rm = T) + 
                          sum(ticks_final_total, na.rm = T))) %>%
  mutate(across(where(is.numeric), ~ na_if(., 0))) %>%
  rename(ID = livestock_species)
#create total row
# live.tot <- livestock %>% summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
#   mutate(ID = "Livestock Total")
# livestock <- bind_rows(livestock, live.tot)
# rm(live.tot)

wildlife <- kobo.data$DTRANK_wildlife_sampling %>%
  group_by(secb_aclass) %>%
  summarise(blood.samples = n(),
            Ticks = sum(secd_ectoparasites, na.rm = T)) %>%
  mutate(secb_aclass = c('rodents','birds')) %>%
  rename(ID = secb_aclass)
#create total row
# wild.tot <- wildlife %>% summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
#   mutate(ID = "Wildlife Total")
# wildlife <- bind_rows(wildlife,wild.tot)
# rm(wild.tot)

#create summary table
sample.sum <- bind_rows(human,
                        livestock,
                        livestock %>% summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
                          mutate(ID = "Livestock Total"),
                        wildlife,
                        wildlife %>% summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
                          mutate(ID = "Wildlife Total")) %>%
  select(Species = ID,
         `Blood Samples` = blood.samples,
         `GPS Trackers/Collars` = GPS.tracks,
         `GPS Ear Tags` = Ceres.tags,
         everything()) %>%
  mutate(Species = str_to_title(Species))
write.csv(sample.sum, here('output','DTRANK_sample_summary.csv'), row.names = F, na='')

#### create SQL database -----

# TO DO !!!!!!!

#### upload to Google Drive ----

#tidy final datasets & print to output folder

for (dataset in c('DTRANK_RVFV_rapid_test','DTRANK_HH_valuechain','DTRANK_HH_demography_livestock','DTRANK_ecotones', 'DTRANK_input_suppliers','DTRANK_extension_service_providers','DTRANK_trap_booking')) {
  
  write.csv(kobo.data[[dataset]] %>% tidy_strip_pii(),
            here('output',paste0(dataset,'.csv')),
            row.names = F,
            na = '')
  
}

#authenticate to DTRA google account !! ONLY RUN ONCE
#drive_auth()


#upload main .csv data files
upload_to_google(source.folder = here('output'),
                 dest.folder = 'DTRA-NK/Data/',
                 pattern = '.csv')

#upload spatial objects
upload_to_google(source.folder = here('output','spatial'),
                 dest.folder = 'DTRA-NK/Data/Spatial/',
                 pattern = '.gpkg')

#upload ecotone images
upload_to_google(source.folder = here('output','images','ecotones'),
                 dest.folder = 'DTRA-NK/Data/Images/Ecotones/',
                 pattern = '.jpg|.png')

#upload wildlife images
upload_to_google(source.folder = here('output','images','wildlife'),
                 dest.folder = 'DTRA-NK/Data/Images/Wildlife/',
                 pattern = '.jpg|.png')




##end-------------------------------------------------------------------


## testing -----



# #load packages
# library(pacman)
# p_load(tidyverse,
#        #KoboconnectR,
#        robotoolbox,
#        dm,
#        here,
#        sf,
#        httr
#        #mailR
# )
# library(KoboconnectR)
# 
# #import functions
# source(here('code','0_functions.R'))
# 
# ## setup ----
# #set username and password for Kobo account
# #!! SENSITIVE PASSWORD INFORMATION !!
# uname <-  "dtrank_master"
# pwd <-  "M!tchbc13579"
# url <-  "kf.kobotoolbox.org"
# 
# #get Kobo API token
# get_kobo_token(url=url, uname=uname, pwd=pwd)
# #"1123cf35535bde15c4452187ba990a616dc492c0"
# 
# ##data download from Kobo server ------
# 
# #get projects available in the Kobo account
# kobo.projects <- kobotools_api(url=url, simplified=T, uname=uname, pwd=pwd)
# 
# kobo.data <- list()
# #download all data and save to .csv files
# #NB takes a long time
# for (i in 1:nrow(kobo.projects)) { #this excludes projects with no submission as they cause an error with the download
#   if (kobo.projects$submissions[i]>0){
#     data <- kobo_df_download(
#       url = "kf.kobotoolbox.org",
#       uname = uname,
#       pwd = pwd,
#       assetid = kobo.projects[i,2],
#       all = "false",
#       lang = "_xml",
#       hierarchy = 'false',#ifelse(kobo.projects[i,2]=='aLLhyj2ffYCFkJSwxbuGH4','true',"false"), #to get hierarchy column names for GPS_vector data
#       include_grp = "true",
#       grp_sep = "/",
#       fsep = ";",
#       multi_sel = "both",
#       media_url = "true",
#       fields = NULL,
#       sub_ids = NULL,
#       sleep = 30 #increase this if internet is slow or errors occur
#     ) %>%
#       mutate(across(everything(), ~ replace(.x, (.x==''), NA))) %>%
#       tidy_barcodes()
#     
#     kobo.data[[kobo.projects$name[i]]] <- data
#     
#     write.csv(data,
#               file = here('input','raw',paste0(kobo.projects[i,1],'.csv')),
#               row.names = F,
#               na = '')
#     
#     print(paste(as.character(i),':',kobo.projects[i,1],'download and writing completed successfully'))
#   }
#   else {print(paste('There are no submissions for',kobo.projects[i,1],'so it has not been downloaded'))}
# }
# 
# ## for loading data without downloading from server
# files <- list.files(here('input','raw'), full.names = T, pattern = '.csv')
# kobo.data <- list()
# for (i in 1:length(files)) {
#   kobo.data[[str_remove_all(list.files(here('input','raw'))[i],'.csv')]] <- read.csv(files[i])
# }
# 
# #forms with repeats
# ##activity space mapping
# #forms with media
# ##GPS vector- gps files
# ##wildlife sampling- images
# ##ecotones- images
# ##FGD- audio, images
# ##Cryobox sample booking
# 
# 
# 
# #activity space mapping with repeats -----
# act.space.map <- kobo_xls_dl(
#   url = url,
#   uname = uname,
#   pwd = pwd,
#   assetid = "aHh7JzCQYQgFnrctbrNwGr",
#   all = "false",
#   lang = "_xml",
#   hierarchy = "false",
#   include_grp = "true",
#   grp_sep = "/",
#   multi_sel = "both",
#   media_url = "true",
#   fields = NULL,
#   sub_ids = NULL,
#   sleep = 2
# )
# 
# #list2env(data,globalenv()) #only needed for testing- creates objects of all data in the global environment
# 
# #list of repeats with geoshape data
# areas.data <- c("grazing_rep","crops_rep")
# 
# #sort spatial data in repeats into combined point and polygon objects
# #create empty lists for point and area data
# pts <- list()
# area <- list()
# 
# for (type in names(act.space.map)[2:length(act.space.map)]) {
#   #points data
#   if (type %in% areas.data == F) {
#     pts[[type]] <- left_join(act.space.map$DTRANK_activity_space_mapping,act.space.map[[type]], by=c('_id'='_submission__id')) %>%
#       dplyr::mutate(
#         location_type = str_remove(names(act.space.map[type]),'_rep'))# %>%
#     #select(-starts_with('_'))
#     #change column names so row_binding will create a single column for repeated variables
#     names(pts[[type]]) <- str_remove_all(names(pts[[type]]),paste(paste0('_',str_remove_all(names(act.space.map[type]),'_rep')),
#                                                                   paste0(str_remove_all(names(act.space.map[type]),'_rep'),'_'),
#                                                                   sep = '|'))
#     #area/geoshape/polygon data
#   } else if (type %in% areas.data) {
#     area[[type]] <- left_join(act.space.map$DTRANK_activity_space_mapping,act.space.map[[type]], by=c('_id'='_submission__id')) %>%
#       dplyr::mutate(
#         location_type = str_remove(names(act.space.map[type]),'_rep'))# %>%
#     #select(-starts_with('_'))
#     #change column names so row_binding will create a single column for repeated variables
#     names(area[[type]]) <- str_remove_all(names(area[[type]]),paste(paste0('_',str_remove_all(names(act.space.map[type]),'_rep')),
#                                                                     paste0(str_remove_all(names(act.space.map[type]),'_rep'),'_'),
#                                                                     sep = '|'))
#   }
# }
# 
# #combine points data and create spatial object
# points <- bind_rows(pts) %>%
#   select(location_type, hh_id,date,
#          everything(),
#          -contains(c('yn','action_taken_','_count','_rep_count')),
#          -c(start,end,dev_id),
#          'waterpoint_type'='type','water_use_livestock_human'='livestock_human', 'gps_latitude'='_gps_latitude','gps_longitude'='_gps_longitude') %>%
#   tidy_barcodes() %>%
#   select(-starts_with('_'),-'gps') %>%
#   filter(!is.na(gps_latitude)) %>%
#   st_as_sf(coords = c('gps_latitude','gps_longitude'), crs = 'epsg:4326', remove = F)
# #write to gpkg
# st_write(points,
#          dsn = here('input','raw','spatial','DTRANK_activity_space_mapping.gpkg'),
#          layer = 'activity_space_mapping_points',
#          append = F)
# 
# #combine polygon data and create spatial object
# areas <- bind_rows(area) %>% 
#   select(location_type, hh_id,date,
#          everything(),
#          -contains(c('yn','action_taken_','_count','_rep_count')),
#          -c(start,end,dev_id)) %>%
#   filter(!is.na(gps_area)) %>%
#   select(-starts_with('_')) %>%
#   tidy_barcodes() %>%
#   mutate(geometry = wkt_geoshape(gps_area)) %>%
#   st_as_sf(wkt = 'geometry', crs='epsg:4326')
# #write to gpkg
# st_write(areas,
#          dsn = here('input','raw','spatial','DTRANK_activity_space_mapping.gpkg'),
#          layer = 'activity_space_mapping_polygons',
#          append = F)
# 
# ## GPS & vector data -----
# # #download data for GPS_vector Kobo project
# gps.vector <- kobo_df_download(
#   url = "kf.kobotoolbox.org",
#   uname = "dtrank_master",
#   pwd = "M!tchbc13579",
#   assetid = "aLLhyj2ffYCFkJSwxbuGH4", #change this to match the asset ID of the project which you want to download data for
#   all = "false",
#   lang = "_xml",
#   hierarchy = "true",
#   include_grp = "true",
#   grp_sep = "/",
#   fsep = ";",
#   multi_sel = "both",
#   media_url = "true",
#   fields = NULL,
#   sub_ids = NULL,
#   sleep = 10 #increase this if internet is slow or errors occur
# ) %>%
#   mutate(across(everything(), ~ replace(.x, (.x==''), NA))) %>%
#   tidy_barcodes()
# 
# ##create Ceres tag metadata table
# #download existing metadata file from Mapipedia, copy this data into that file by matching the VIDs, then re-upload to Mapipedia
# #this is important for the format of the csv file for Mapipedia to accept it
# kobo.data[['DTRANK_individual_livestock']] %>%
#   filter(long_term_selected == 'yes') %>%
#   select('vid'='ceres_tag_vid',livestock_species,sex,county) %>%
#   write.csv(file = here('output','ceres_metadata.csv'), row.names = F)
# 
# ##create combined datasets of animals with GPS collars
# #extract GPS vector <- from list of datasets
# #gps.vector <- kobo.data[['DTRANK_GPS_vector']] %>% tidy_barcodes() #this might not work because no hierarchical names are downloaded in the full download
# 
# 
# #separate gps.vector data sections and save to a list
# gps.vector.data <- list()
# for (section in unique(gps.vector$collection)) {
#   gps.vector.data[[section]] <- gps.vector %>% 
#     select(date,county,ward,collection,hh_id,starts_with(paste0(ifelse(section=='initial'|section=='final', section,
#                                                                        ifelse(section == 'environment','vectors_environment',
#                                                                               'human_gps')),'.'))) %>%
#     filter(collection == section) %>%
#     rename_with(~str_remove_all(., c('.gps_info_initial|.vectors_initial|.gps_info_final|.vectors_final')))
# }
# 
# #combine initial and final collar data
# gps.collars <- full_join(gps.vector.data[['initial']],
#                          gps.vector.data[['final']],
#                          by=c('county',
#                               'ward',
#                               'hh_id',
#                               'initial.livestock_species_initial'='final.livestock_species_final',
#                               'initial.gps_logger_id_initial'='final.gps_logger_id_final')) %>% 
#   select(hh_id,county,ward,
#          'livestock_species'='initial.livestock_species_initial','gps_logger_id'='initial.gps_logger_id_initial',
#          'date_deployed'='date.x','date_retrieved'='date.y',
#          'ceres_tag_id'='initial.ceres_tag_vid_initial','ceres_tag_phone'='initial.ceres_tag_phone',
#          'last_acaracide_treatment'='initial.acaracide_last_treat','acaracide_freq'='initial.acaracide_freq','acaracide_freq_other'='initial.acaracide_freq_other','acaracide_name'='initial.acaracide_name',
#          contains('ticks'),
#          'gps_track_file'='final.gps_track_file','gps_track_file_URL'='final.gps_track_file_URL',
#          -'initial.gps_tick_initial',-'final.gps_tick_final')
# 
# write.csv(gps.collars, here('output','spatial','gps_collar_data.csv'), row.names = F)
# 
# 
# #send email with GPS_vector data to @Abby Lilak ----- DOES NOT WORK ---
# # send.mail(from = 'sirimonfbt@outlook.com',
# #          to = c('a.lilak@ufl.edu','sirimon.thomas@ed.ac.uk'),
# #          subject = 'DTRA-NK GPS_vector - latest data',
# #          body = 'Dear Abby, Here is the latest GPS and vector data from the DTRA-NK project. Kind regards, Sirimon',
# #          smtp = list(host.name = "smtp-mail.outlook.com", port = 587,
# #                      user.name = "sirimonfbt@outlook.com",
# #                      passwd = "Ziwani1011!", ssl = TRUE),
# #          authenticate = TRUE,
# #          send = TRUE,
# #          attach.files = c(here('output','spatial','gps_collar_data.csv'),here('input','raw','DTRANK_GPS_vector.csv')))
# 
# 
# #download each media file (.csv file with the movement tracks) from the URLs in gps.collars
# #livestock movement
# gps.collar.data <- list()
# for (i in 1:nrow(gps.collars)) {
#   if (!is.na(gps.collars$gps_track_file_URL[i]) & gps.collars$gps_track_file_URL[i] != ''){
#     #download media file
#     media.file <- content(GET(url = gps.collars$gps_track_file_URL[i],
#                               config = authenticate(user=uname,password=pwd)),
#                           as='text', encoding = 'UTF-8') %>% 
#       #parse to dataframe
#       read.table(text=., header = TRUE, fill = TRUE, sep = ",")
#     #save to list for further cleaning
#     gps.collar.data[[i]] <- media.file
#     
#     #export to folder
#     write.csv(media.file,here('input','raw','spatial','gps_collars',paste0(gps.collars$hh_id[i],
#                                                                            '_',
#                                                                            gps.collars$livestock_species[i],
#                                                                            '_',
#                                                                            gps.collars$gps_logger_id[i],
#                                                                            '.csv')),
#               row.names = F,
#               na = '')
#   }
# }
# 
# #human movement
# gps.human.data <- list()
# for (i in 1:nrow(gps.vector.data$human)) {
#   if (!is.na(gps.vector.data$human$human_gps.gps_track_file_human_URL[i]) & 
#       gps.vector.data$human$human_gps.gps_track_file_human_URL[i] != ''){
#     #download media file
#     media.file <- content(GET(url = gps.vector.data$human$human_gps.gps_track_file_human_URL[i],
#                               config = authenticate(user=uname,password=pwd)),
#                           as='text', encoding = 'UTF-8') %>% 
#       #parse to dataframe
#       read.table(text=., header = TRUE, fill = TRUE, sep = ",")
#     #save to list for further cleaning
#     gps.human.data[[i]] <- media.file
#     
#     #export to folder
#     #might need to edit this if files are sent ot Kobo as .gpx
#     write.csv(media.file,here('input','raw','spatial','human_movement',paste0(gps.vector.data$human$hh_id[i],
#                                                                               '_',
#                                                                               gps.vector.data$human$collection[i],
#                                                                               '_',
#                                                                               gps.vector.data$human$human_gps.gps_logger_id_human[i],
#                                                                               '.csv')),
#               row.names = F,
#               na = '')
#   }
# }
# 
