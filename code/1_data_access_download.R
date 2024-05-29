# DTRA-NK - Data Preliminary
# Sirimon Thomas
# April 2024

#load packages
library(pacman)
p_load(tidyverse,
       KoboconnectR,
       robotoolbox,
       dm,
       here,
       sf,
       httr)

#import functions
source(here('code','0_functions.R'))

### USING KOBOCOLLECTR #######

## setup ----
#set username and password for Kobo account
#!! SENSITIVE PASSWORD INFORMATION !!
uname <-  "dtrank_master"
pwd <-  "M!tchbc13579"
url <-  "kf.kobotoolbox.org"

#get Kobo API token
get_kobo_token(url=url, uname=uname, pwd=pwd)
#setup KobocollectR settings
kobo_setup(url = url,
           token = get_kobo_token(url=url, uname=uname, pwd=pwd))

##data download ------

#get projects available in the Kobo account
kobo.projects <- kobotools_api(url=url, simplified=T, uname=uname, pwd=pwd)

kobo.data <- list()
#download all data and save to .csv files
#NB takes a long time
for (i in 1:nrow(kobo.projects)) {
  data <- kobo_df_download(
    url = "kf.kobotoolbox.org",
    uname = uname,
    pwd = pwd,
    assetid = kobo.projects[i,2], #change this to match the asset ID of the project which you want to download data for
    all = "false",
    lang = "_xml",
    hierarchy = 'false',#ifelse(kobo.projects[i,2]=='aLLhyj2ffYCFkJSwxbuGH4','true',"false"), #might be able to scrap this ifelse statement
    include_grp = "true",
    grp_sep = "/",
    fsep = ";",
    multi_sel = "both",
    media_url = "true",
    fields = NULL,
    sub_ids = NULL,
    sleep = 20 #increase this if internet is slow or errors occur
  ) %>%
    mutate(across(everything(), ~ replace(.x, (.x==''), NA))) %>%
    tidy_barcodes()
  
  kobo.data[[kobo.projects$name[i]]] <- data
  
  write.csv(data,
            file = here('input','raw',paste0(kobo.projects[i,1],'.csv')),
            row.names = F,
            na = '')
  
  print(paste(as.character(i),':',kobo.projects[i,1],'download and writing completed successfully'))
}

#forms with repeats
##activity space mapping
#forms with media
##GPS vector- gps files
##wildlife sampling- images
##ecotones- images
##FGD- audio, images
##Cryobox sample booking



#activity space mapping with repeats -----
act.space.map <- kobo_xls_dl(
  url = url,
  uname = uname,
  pwd = pwd,
  assetid = "aHh7JzCQYQgFnrctbrNwGr",
  all = "false",
  lang = "_xml",
  hierarchy = "false",
  include_grp = "true",
  grp_sep = "/",
  multi_sel = "both",
  media_url = "true",
  fields = NULL,
  sub_ids = NULL,
  sleep = 2
)

#list2env(data,globalenv()) #only needed for testing- creates objects of all data in the global environment

#list of repeats with geoshape data
areas.data <- c("grazing_rep","crops_rep")

#sort spatial data in repeats into combined point and polygon objects
#create empty lists for point and area data
pts <- list()
area <- list()

for (type in names(act.space.map)[2:length(act.space.map)]) {
  #points data
  if (type %in% areas.data == F) {
    pts[[type]] <- left_join(act.space.map$DTRANK_activity_space_mapping,act.space.map[[type]], by=c('_id'='_submission__id')) %>%
      dplyr::mutate(
        location_type = str_remove(names(act.space.map[type]),'_rep'))# %>%
    #select(-starts_with('_'))
    #change column names so row_binding will create a single column for repeated variables
    names(pts[[type]]) <- str_remove_all(names(pts[[type]]),paste(paste0('_',str_remove_all(names(act.space.map[type]),'_rep')),
                                                                  paste0(str_remove_all(names(act.space.map[type]),'_rep'),'_'),
                                                                  sep = '|'))
    #area/geoshape/polygon data
  } else if (type %in% areas.data) {
    area[[type]] <- left_join(act.space.map$DTRANK_activity_space_mapping,act.space.map[[type]], by=c('_id'='_submission__id')) %>%
      dplyr::mutate(
        location_type = str_remove(names(act.space.map[type]),'_rep'))# %>%
    #select(-starts_with('_'))
    #change column names so row_binding will create a single column for repeated variables
    names(area[[type]]) <- str_remove_all(names(area[[type]]),paste(paste0('_',str_remove_all(names(act.space.map[type]),'_rep')),
                                                                    paste0(str_remove_all(names(act.space.map[type]),'_rep'),'_'),
                                                                    sep = '|'))
  }
}

#combine points data and create spatial object
points <- bind_rows(pts) %>%
  select(location_type, hh_id,date,
         everything(),
         -contains(c('yn','action_taken_','_count','_rep_count')),
         -c(start,end,dev_id),
         'waterpoint_type'='type','water_use_livestock_human'='livestock_human', 'gps_latitude'='_gps_latitude','gps_longitude'='_gps_longitude') %>%
  tidy_barcodes() %>%
  select(-starts_with('_'),-'gps') %>%
  filter(!is.na(gps_latitude)) %>%
  st_as_sf(coords = c('gps_latitude','gps_longitude'), crs = 'epsg:4326', remove = F)
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
         -c(start,end,dev_id)) %>%
  filter(!is.na(gps_area)) %>%
  select(-starts_with('_')) %>%
  tidy_barcodes() %>%
  mutate(geometry = wkt_geoshape(gps_area)) %>%
  st_as_sf(wkt = 'geometry', crs='epsg:4326')
#write to gpkg
st_write(areas,
         dsn = here('input','raw','spatial','DTRANK_activity_space_mapping.gpkg'),
         layer = 'activity_space_mapping_polygons',
         append = F)

## GPS & vector data -----
# #download data for GPS_vector Kobo project
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
#   sleep = 4 #increase this if internet is slow or errors occur
# ) %>%
#   mutate(across(everything(), ~ replace(.x, (.x==''), NA))) %>%
#   tidy_barcodes()

##create Ceres tag metadata table
#download existing metadata file from Mapipedia, copy this data into that file by matching the VIDs, then re-upload to Mapipedia
#this is important for the format of the csv file for Mapipedia to accept it
kobo.data[['DTRANK_individual_livestock']] %>%
  filter(long_term_selected == 'yes') %>%
  select('vid'='ceres_tag_vid',livestock_species,sex,county) %>%
  write.csv(file = here('output','ceres_metadata.csv'), row.names = F)

##create combined datasets of animals with GPS collars
#extract GPS vector <- from list of datasets
gps.vector <- kobo.data[['DTRANK_GPS_vector']] %>% tidy_barcodes()

#separate gps.vector data sections and save to a list
gps.vector.data <- list()
for (section in unique(gps.vector$collection)) {
  gps.vector.data[[section]] <- gps.vector %>% 
    select(date,county,ward,collection,hh_id,starts_with(paste0(ifelse(section=='initial'|section=='final', section,
                                                                       ifelse(section == 'environment','vectors_environment',
                                                                              'human_gps')),'.'))) %>%
    filter(collection == section) %>%
    rename_with(~str_remove_all(., c('.gps_info_initial|.vectors_initial|.gps_info_final|.vectors_final')))
}

#combine initial and final collar data
gps.collars <- full_join(gps.vector.data[['initial']],
                         gps.vector.data[['final']],
                         by=c('county',
                              'ward',
                              'hh_id',
                              'initial.livestock_species_initial'='final.livestock_species_final',
                              'initial.gps_logger_id_initial'='final.gps_logger_id_final')) %>% 
  select(hh_id,county,ward,
         'livestock_species'='initial.livestock_species_initial','gps_logger_id'='initial.gps_logger_id_initial',
         'date_deployed'='date.x','date_retrieved'='date.y',
         'ceres_tag_id'='initial.ceres_tag_vid_initial','ceres_tag_phone'='initial.ceres_tag_phone',
         'last_acaracide_treatment'='initial.acaracide_last_treat','acaracide_freq'='initial.acaracide_freq','acaracide_freq_other'='initial.acaracide_freq_other','acaracide_name'='initial.acaracide_name',
         contains('ticks'),
         'gps_track_file'='final.gps_track_file','gps_track_file_URL'='final.gps_track_file_URL',
         -'initial.gps_tick_initial',-'final.gps_tick_final')

#download each media file (.csv file with the movement tracks) from the URLs in gps.collars
gps.collar.data <- list()
for (i in 1:nrow(gps.collars)) {
  if (!is.na(gps.collars$gps_track_file_URL[i]) & gps.collars$gps_track_file_URL[i] != ''){
    #download media file
    media.file <- content(GET(url = gps.collars$gps_track_file_URL[i],
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
}
  
  

  ##### testing #####  
  # 
  # kobo_media(
  #   url = "kf.kobotoolbox.org",
  #   uname = "dtrank_master",
  #   pwd = "M!tchbc13579",
  #   assetid = "aLLhyj2ffYCFkJSwxbuGH4", #asset ID for GPS_vector form
  #   fsep = ";",
  #   sleep = 2,
  #   identifier = "URL",
  #   timeoutval = 300,
  #   destfolder = "input/gps_files"
  # )
  # 

  
  # #remotes::install_github("cynkra/dm")
  # remotes::install_github("dickoa/robotoolbox")
  # library(robotoolbox)
  # 
  # #import functions
  # source(here('code','0_functions.R'))
  # 
  # ####setup----
  # #for robotoolbox package
  # kobo_setup(url = "kf.kobotoolbox.org",
  #            token = kobo_token(username = "dtrank_master", #SENSITIVE!!!
  #                               password = "M!tchbc13579", #SENSITIVE!!!
  #                               url = "kf.kobotoolbox.org"))
  # #get list of assets
  # kobo.assets <- kobo_asset_list()
  # 
  # ####data download-----
  # #forms with repeats
  # ##activity space mapping
  # #forms with media
  # ##GPS vector- gps files
  # ##wildlife sampling- images
  # ##ecotones- images
  # ##FGD- audio, images
  # 
  # #download all data and write to .csv
  # #takes a long time!
  # for (i in 1:nrow(kobo.projects)) {
  #   if (kobo.assets$name[i] != 'DTRANK_activity_space_mapping') {
  #     data <- kobo_data(
  #       kobo.assets$uid[i], 
  #       all_versions = FALSE,
  #       progress = TRUE
  #     )
  #     
  #     write.csv(data,
  #               file = here('input','raw',paste0(kobo.assets$name[i],'.csv')),
  #               row.names = F)
  #   }
  # }
  # 
  # ####activity space mapping----
  # #get activity space mapping data
  # act.space.map <- kobo_data(
  #   'aHh7JzCQYQgFnrctbrNwGr', #assetid for activity space mapping asset
  #   all_versions = TRUE,
  #   progress = TRUE
  # )
  # 
  # #write parent (main) data to .csv
  # write.csv(act.space.map$main,
  #           file = here('input','raw','DTRANK_activity_space_mapping.csv'),
  #           row.names = F)
  # 
  # #sort spatial data in repeats into combined point and polygon objects
  # #create empty lists for point and area data
  # pts <- list()
  # area <- list()
  # 
  # for (type in names(act.space.map)) {
  #   #points data
  #   if (type %in% areas.data == F) {
  #     pts[[type]] <- dm_flatten_to_tbl(act.space.map,.start = !!as.name(names(act.space.map[type])),.join=left_join) %>%
  #       dplyr::mutate(
  #         location_type = str_remove(names(act.space.map[type]),'_rep')) %>%
  #       select(-starts_with('_'))
  #     #change column names so row_binding will create a single column for repeated variables
  #     names(pts[[type]]) <- str_remove_all(names(pts[[type]]),paste(paste0('_',str_remove_all(names(act.space.map[type]),'_rep')),
  #                                                                   paste0(str_remove_all(names(act.space.map[type]),'_rep'),'_'),
  #                                                                   sep = '|'))
  #     #area/geoshape/polygon data
  #   } else if (type %in% areas.data) {
  #     area[[type]] <- dm_flatten_to_tbl(act.space.map,.start = !!as.name(names(act.space.map[type])),.join=left_join) %>%
  #       dplyr::mutate(
  #         location_type = str_remove(names(act.space.map[type]),'_rep')) %>%
  #       select(-starts_with('_'))
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
  #          -c(start,end,dev_id,uuid,instanceID),
  #          'waterpoint_type'='type','water_use_livestock_human'='livestock_human') %>%
  #   tidy_barcodes() %>%
  #   st_as_sf(coords = c('gps_latitude','gps_longitude'), crs = 'epsg:4326', remove = F)
  # #write to gpkg
  # st_write(points,
  #          dsn = here('input','raw','spatial','DTRANK_activity_space_mapping_points.gpkg'),
  #          layer = 'activity_space_mapping_points')
  # 
  # #combine polygon data and create spatial object
  # areas <- bind_rows(area) %>% 
  #   select(location_type, hh_id,date,
  #          everything(),
  #          -contains(c('yn','action_taken_','_count','_rep_count')),
  #          -c(start,end,dev_id,uuid,instanceID),
  #          'geometry' = 'gps_area_wkt') %>%
  #   tidy_barcodes() %>%
  #   st_as_sf(wkt = 'geometry', crs='epsg:4326')
  # #write to gpkg
  # st_write(areas,
  #          dsn = here('input','raw','spatial','DTRANK_activity_space_mapping_polygons.gpkg'),
  #          layer = 'activity_space_mapping_polygons')
  # 
  # ####media download-----
  # 
  # kobo_media(
  #   url = "kf.kobotoolbox.org",
  #   uname = "dtrank_master",
  #   pwd = "M!tchbc13579",
  #   assetid = "aLLhyj2ffYCFkJSwxbuGH4", #asset ID for GPS_vector form
  #   fsep = ";",
  #   sleep = 2,
  #   identifier = "URL",
  #   timeoutval = 300,
  #   destfolder = "input/gps_files"
  # )
  # 
  # kobo_attachment_download('aLLhyj2ffYCFkJSwxbuGH4',
  #                          folder = here('input','gps_files'),
  #                          progress = T)
  # 
  # ######################### testing ###########
  # gps.vect <- kobo_data(
  #   'aLLhyj2ffYCFkJSwxbuGH4', #assetid
  #   all_versions = TRUE,
  #   progress = TRUE
  # )
  
  
  ##end------
  