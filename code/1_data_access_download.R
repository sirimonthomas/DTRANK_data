# DTRA-NK - Data Download and Preliminary Cleaning
# Sirimon Thomas
# April 2024


#### setup ----
library(tidyverse)
library(here)
library(robotoolbox)
library(dm)
library(sf)
library(httr)

# #import functions
source(here('code','0_functions.R'))
#set username and password for Kobo account
#!! SENSITIVE PASSWORD INFORMATION !!
uname <-  "dtrank_master"
pwd <-  "M!tchbc13579"
url <-  "kf.kobotoolbox.org"
# ####setup----
# #for robotoolbox package
kobo_setup(url = "https://kf.kobotoolbox.org/",
           token = "1123cf35535bde15c4452187ba990a616dc492c0")
#get list of assets
kobo.projects <- kobo_asset_list()
# 
# ####data download-----
# #forms with repeats
# ##activity space mapping
# #forms with media
# ##GPS vector- gps files
# ##wildlife sampling- images
# ##ecotones- images
# ##FGD- audio, images


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
    ) %>%
      #mutate(across(everything(), ~ replace(.x, (.x==''), NA))) %>%
      tidy_barcodes()
    
    kobo.data[[kobo.projects$name[i]]] <- data  
    
    if (kobo.projects$name[i] == 'DTRANK_activity_space_mapping'){
      data <- data$main
    }
    
    #save
    write.csv(data %>% select(-`_attachments`),
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
for (j in c('DTRANK_wildlife_sampling','DTRANK_individual_human','DTRANK_individual_livestock')) {
  
  #change wildlife column name to allow joining
  if(j == 'DTRANK_wildlife_sampling') {kobo.data[[j]] <- rename(kobo.data[[j]], Serum_blood_parentID = secd_serum)}
  
  #add daughter sample codes
  kobo.data[[j]] <- kobo.data[[j]] %>% add_daughter_samples()
  
  #data quality check
  
  
  #check for duplicate parent blood IDs
  if(TRUE %in% duplicated(kobo.data[[j]]$Serum_blood_parentID)){
    print('There are duplicated parent blood sample IDs')
    print(kobo.data[[j]] %>% filter(duplicated(Serum_blood_parentID)))
  }
  
  #check for individual data entries without matching serum sample booking
  if(nrow(kobo.data$DTRANK_serum_sample_booking %>% filter(sample == ifelse(j == 'DTRANK_wildlife_sampling','wildlife',
                                                                            ifelse(j == 'DTRANK_individual_human','human',
                                                                                   'livestock')))) != nrow(kobo.data[[j]])){
    print('The number of ',ifelse(j == 'DTRANK_wildlife_sampling','wildlife',
                                  ifelse(j == 'DTRANK_individual_human','human',
                                         'livestock')),' entries in the serum_sample_booking data are different to the number of entries in the ',
          j,' data')
    print(paste0('serum_sample_booking: ',nrow(DTRANK_serum_sample_booking %>% filter(sample == ifelse(j == 'DTRANK_wildlife_sampling','wildlife',
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
    }
    
    #entries in individual sample datasets that are not in the serum_sample_booking data
    if(nrow(kobo.data[[j]] %>% filter(Serum_blood_parentID %!in% kobo.data$DTRANK_serum_sample_booking$Serum_blood_parentID))>0){
      print(paste0('These entries are in the ',j,' data but not in the serum_sample_booking data'))
      print(kobo.data[[j]] %>% filter(Serum_blood_parentID %!in% kobo.data$DTRANK_serum_sample_booking$Serum_blood_parentID))
    }
  }    
  
  #save
  write.csv(kobo.data[[j]] %>% select(-`_attachments`),
            file = here('input','sample_matched',paste0(j,'.csv')),
            row.names = F,
            na = '')
} 

#list2env(kobo.data,globalenv()) #only needed for testing- creates objects of all data in the global environment

#### activity space mapping data ----
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
         dsn = here('input','raw','spatial','DTRANK_activity_space_mapping.gpkg'),
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
         dsn = here('input','raw','spatial','DTRANK_activity_space_mapping.gpkg'),
         layer = 'activity_space_mapping_polygons',
         append = F)

### mapping activity space mapping data for each community??????----



##### GPS_vector data tidying ----
#separate gps.vector data sections and save to a list
gps.vector.data <- list()
for (section in unique(kobo.data$DTRANK_GPS_vector$collection)) {
  if (section == 'initial'){
    gps.vector.data[[section]] <- kobo.data$DTRANK_GPS_vector %>% 
      select(date,county,ward,collection,hh_id,
             contains('acaracide'),
             contains(section)) %>%
      filter(collection == section)# %>%
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
             `_attachments`) %>%
      filter(collection == section)
  }
}
#combine initial and final collar data
gps.collars <- full_join(gps.vector.data[['initial']],
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
         `_attachments`,
         -'gps_tick_initial',-'gps_tick_final')

write.csv(select(gps.collars,-`_attachments`), here('output','spatial','gps_collar_data.csv'), row.names = F)


#send email with GPS_vector data to @Abby Lilak ----- DOES NOT WORK ---
# send.mail(from = 'sirimonfbt@outlook.com',
#          to = c('a.lilak@ufl.edu','sirimon.thomas@ed.ac.uk'),
#          subject = 'DTRA-NK GPS_vector - latest data',
#          body = 'Dear Abby, Here is the latest GPS and vector data from the DTRA-NK project. Kind regards, Sirimon',
#          smtp = list(host.name = "smtp-mail.outlook.com", port = 587,
#                      user.name = "sirimonfbt@outlook.com",
#                      passwd = "Ziwani1011!", ssl = TRUE),
#          authenticate = TRUE,
#          send = TRUE,
#          attach.files = c(here('output','spatial','gps_collar_data.csv'),here('input','raw','DTRANK_GPS_vector.csv')))


#### download movement tracks ----
##livestock movement
gps.collar.data <- list()
for (i in 1:nrow(gps.collars)) {
  #download media file
  media.file <- content(GET(url = gps.collars$`_attachments`[[i]]$download_url,
                            config = authenticate(user=uname,password=pwd)),
                        as='text', encoding = 'UTF-8') %>% 
    #parse to dataframe
    read.table(text=., header = TRUE, fill = TRUE, sep = ",")
  #save to list for further cleaning
  gps.collar.data[[i]] <- media.file
  
  #export to folder
  write.csv(media.file,here('input','raw','spatial','gps_collars',paste0(gps.collars$hh_id[i],
                                                                         '_',
                                                                         gps.collars$livestock_species[i],
                                                                         '_',
                                                                         gps.collars$gps_logger_id[i],
                                                                         '.csv')),
            row.names = F,
            na = '')
}

##human movement
gps.human.data <- list()
for (i in 1:nrow(gps.vector.data$human)) {
  if (!is.na(gps.vector.data$human$gps_track_file_human[i]) & 
      gps.vector.data$human$gps_track_file_human[i] != ''){
    #download media file
    if (str_detect(gps.vector.data$human$gps_track_file_human[i],'.csv')){#.csv files
      media.file <- content(GET(url = gps.vector.data$human$`_attachments`[[i]]$download_url,
                                config = authenticate(user=uname,password=pwd)),
                            as='text', encoding = 'UTF-8') %>% 
        #parse to dataframe
        read.table(text=., header = TRUE, fill = TRUE, sep = ",") %>%
        mutate(Time = as_datetime(str_remove_all(Time,'"|='), tz = "africa/nairobi"),
               Altitude.m. = ifelse(TRUE %in% str_detect(names(.),'Altitude.ft.'), .$Altitude.ft.,.$Altitude.m.)) %>%
        select(date_time = Time, lat = Latitude, lon = Longitude, elevation = Altitude.m.,satellites = Satellites.CN.22.,hdop = HDOP)
      
    } else if (str_detect(gps.vector.data$human$gps_track_file_human[i],'.bin')){#.gpx files
      content(GET(url = gps.vector.data$human$`_attachments`[[i]]$download_url,
                  config = authenticate(user=uname,password=pwd)),
              as='raw') %>% 
        #parse to dataframe - .gpx files
        
        writeBin(here('input','raw','spatial','human_movement','GPX_files',paste0(gps.vector.data$human$hh_id[i],
                                                                                  '_',
                                                                                  gps.vector.data$human$collection[i],
                                                                                  '_',
                                                                                  gps.vector.data$human$gps_logger_id_human[i],
                                                                                  '.gpx')))
      media.file <- st_read(here('input','raw','spatial','human_movement','GPX_files',paste0(gps.vector.data$human$hh_id[i],
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
    write.csv(media.file,here('input','raw','spatial','human_movement',paste0(gps.vector.data$human$hh_id[i],
                                                                              '_',
                                                                              gps.vector.data$human$collection[i],
                                                                              '_',
                                                                              gps.vector.data$human$gps_logger_id_human[i],
                                                                              '.csv')),
              row.names = F,
              na = '')
  }
}


##end------


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
