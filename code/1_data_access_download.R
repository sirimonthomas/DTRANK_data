# DTRA-NK - Data Download and Preliminary Cleaning
# Sirimon Thomas
# April 2024

####setup----
#need the development version of robotoolbox
#remotes::install_gitlab("dickoa/robotoolbox", force = T)

# library(tidyverse)
# library(here)
# library(robotoolbox)
# library(dm)
# library(sf)
# library(httr)
# library(googledrive)
# library(DBI)
# library(RSQLite)
# library(readxl)

pacman::p_load(tidyverse,
               here,
               robotoolbox,
               dm,
               sf,
               httr,
               googledrive,
               DBI,
               RSQLite,
               readxl)

# #import functions
source(here('code','0_functions.R'))
#set username and password for Kobo account
#!! SENSITIVE PASSWORD INFORMATION !!
uname <-  "dtrank_master"
pwd <-  "M!tchbc13579"
url <-  "kf.kobotoolbox.org"

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
  #exclude projects with no submission and datasets that are specific to Dickson
  if (kobo.projects$submissions[i]>0 &
      kobo.projects$name[i] != 'DTRANK_extension_service_providers' &
      kobo.projects$name[i] != 'DTRANK_input_suppliers'){
    data <- kobo_data(
      kobo.projects$uid[i],
      #all_versions = kobo.projects$name[i] == 'DTRANK_HH_valuechain',
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
    # #GPS-vector - camel with ceres tag but no short term collar in DHH0100053
    # if (kobo.projects$name[i] == 'DTRANK_GPS_vector'){
    #   data[data$`_uuid`=='aab5f780-58a5-464f-936c-cae41509539f','gps_logger_id_initial'] <- NA
    # }
    
    #individual_human - tidy ages
    if (kobo.projects$name[i] == 'DTRANK_individual_human'){
      data <- data %>% mutate(
        participant_age = ifelse(is.na(participant_age),as.numeric(age),participant_age),
        participant_age = round(participant_age)) %>% 
        select(-c(dob, participant_dob, age))
    }
    
    #HH_valuechain - calculate FCS and HWISE
    if (kobo.projects$name[i] == 'DTRANK_HH_valuechain'){
      data <- data %>% calculate_fcs() %>% calculate_hwise()
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

#### blood sample daughter codes -----
# re-attach serum sample daughter ID to individual human and livestock data,

for (j in c('DTRANK_wildlife_sampling','DTRANK_individual_human','DTRANK_individual_livestock')) {
  
  #change wildlife column name to allow joining
  if(j == 'DTRANK_wildlife_sampling') {kobo.data[[j]] <- rename(kobo.data[[j]], Serum_blood_parentID = secd_serum)}
  
  #add daughter sample codes
  kobo.data[[j]] <- kobo.data[[j]] %>% 
    add_daughter_samples()
  
  #save
  write.csv(kobo.data[[j]],
            file = here('output',paste0(j,'.csv')),
            row.names = F,
            na = '')
} 

#### community ID reference ----
community.ref <- kobo.data$DTRANK_HH_demography_livestock %>%
            arrange(ward, date) %>%
            group_by(ward) %>%
            mutate(
              day_diff = as.numeric(difftime(as_date(date), lag(as_date(date)), units = "days")),
              session_break = ifelse(is.na(day_diff) | day_diff > 1, 1, 0),
              session_id = cumsum(session_break),
              session_multi = max(session_id)
            ) %>%
            ungroup() %>%
            mutate(
              community = ifelse(session_multi > 1,
                                 paste(ward, as.character(session_id), sep = '_'),
                                 ward)) %>%
            select(date, county, ward, community, hh_id) %>%
            arrange(date, hh_id)

write.csv(community.ref,
          here('output','DTRANK_community_household_id_reference.csv'),
          row.names = F,
          na = '')


#### data quality checks ----
# check for data entry errors
missing.entries <- list()
for (j in c('DTRANK_wildlife_sampling','DTRANK_individual_human','DTRANK_individual_livestock')) {
  
  #check for duplicate parent blood IDs
  if(TRUE %in% duplicated(kobo.data[[j]]$Serum_blood_parentID)){
    print(paste('There are duplicated parent blood sample IDs in',j))
    print(kobo.data[[j]] %>% filter(duplicated(Serum_blood_parentID)))
    print(kobo.data[[j]] %>% filter(duplicated(Serum_blood_parentID)) %>% select(Serum_blood_parentID))
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
  
}


#duplicated sample codes between human, livestock and wildlife datasets

#parent blood
check_duplicate_codes(kobo.data$DTRANK_individual_livestock,'Serum_blood_parentID',kobo.data$DTRANK_individual_human,'Serum_blood_parentID')  
check_duplicate_codes(kobo.data$DTRANK_individual_livestock,'Serum_blood_parentID',kobo.data$DTRANK_wildlife_sampling,'Serum_blood_parentID')  
check_duplicate_codes(kobo.data$DTRANK_individual_human,'Serum_blood_parentID',kobo.data$DTRANK_wildlife_sampling,'Serum_blood_parentID')

#EDTA 1
check_duplicate_codes(kobo.data$DTRANK_individual_livestock,'EDTA_blood_aliquot1',kobo.data$DTRANK_individual_human,'EDTA_blood_aliquot1')  
check_duplicate_codes(kobo.data$DTRANK_individual_livestock,'EDTA_blood_aliquot1',kobo.data$DTRANK_wildlife_sampling,'secd_edta_1')  
check_duplicate_codes(kobo.data$DTRANK_individual_human,'EDTA_blood_aliquot1',kobo.data$DTRANK_wildlife_sampling,'secd_edta_1')

#EDTA 2
check_duplicate_codes(kobo.data$DTRANK_individual_livestock,'EDTA_blood_aliquot2',kobo.data$DTRANK_individual_human,'EDTA_blood_aliquot2')  
check_duplicate_codes(kobo.data$DTRANK_individual_livestock,'EDTA_blood_aliquot2',kobo.data$DTRANK_wildlife_sampling,'secd_edta_2')  
check_duplicate_codes(kobo.data$DTRANK_individual_human,'EDTA_blood_aliquot2',kobo.data$DTRANK_wildlife_sampling,'secd_edta_2')

#Serum ali 1
check_duplicate_codes(kobo.data$DTRANK_individual_livestock,'Serum_blood_aliquot1',kobo.data$DTRANK_individual_human,'Serum_blood_aliquot1')  
check_duplicate_codes(kobo.data$DTRANK_individual_livestock,'Serum_blood_aliquot1',kobo.data$DTRANK_wildlife_sampling,'Serum_blood_aliquot1')  
check_duplicate_codes(kobo.data$DTRANK_individual_human,'Serum_blood_aliquot1',kobo.data$DTRANK_wildlife_sampling,'Serum_blood_aliquot1')

#Serum ali 2
check_duplicate_codes(kobo.data$DTRANK_individual_livestock,'Serum_blood_aliquot2',kobo.data$DTRANK_individual_human,'Serum_blood_aliquot2')  
check_duplicate_codes(kobo.data$DTRANK_individual_livestock,'Serum_blood_aliquot2',kobo.data$DTRANK_wildlife_sampling,'Serum_blood_aliquot2')  
check_duplicate_codes(kobo.data$DTRANK_individual_human,'Serum_blood_aliquot2',kobo.data$DTRANK_wildlife_sampling,'Serum_blood_aliquot2')


## check for duplicated HH IDs##

for (data in c("DTRANK_HH_demography_livestock","DTRANK_HH_valuechain")) {
  if(TRUE %in% duplicated(kobo.data[[data]]$hh_id)){
    print(paste('There are duplicated HH IDs in ',data))
    print('The following HH IDs are duplicated:')
    print(kobo.data[[data]]$hh_id[duplicated(kobo.data[[data]]$hh_id)])
  }
}

## HH IDs missing from datasets

#in valuechain but missing from demography livestock #DHH010008
kobo.data$DTRANK_HH_valuechain$hh_id[kobo.data$DTRANK_HH_valuechain$hh_id %!in% kobo.data$DTRANK_HH_demography_livestock$hh_id]
#in demography livestock but missing from valuechain #DHH010006 and DHH010024
kobo.data$DTRANK_HH_demography_livestock$hh_id[kobo.data$DTRANK_HH_demography_livestock$hh_id %!in% kobo.data$DTRANK_HH_valuechain$hh_id]

#list2env(kobo.data,globalenv()) #only needed for testing- creates objects of all data in the global environment

## errors in GPS livestock and individual livestock data

#### summary stats & graphs ----
#average household structure

hh_pop_structure <- ggplot(kobo.data$DTRANK_HH_demography_livestock %>%
                             select(hh_num, starts_with('m_'), starts_with('f_'), hh_num_calc) %>%
                             pivot_longer(cols = c(starts_with('m_'), starts_with('f_')), names_to = 'age_class', values_to = 'number') %>%
                             group_by(age_class) %>%
                             summarise(number = mean(number)) %>%
                             mutate(gender = case_when(
                               str_starts(age_class, "m_") ~ "M",
                               str_starts(age_class, "f_") ~ "F"),
                               age_class = factor(str_remove_all(age_class,c('f_|m_')), levels = c('under_5','5_18','18_50','over_50'))), 
                           aes(x = age_class, fill = gender,
                               y = ifelse(gender == "M",
                                          -number, number))) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(
    x = "Age", 
    y = "Average number of individuals", 
    fill = "Gender", 
    title = "Average Household Population Structure"
  )

#save
ggsave(here('output','graphics','household_population_structure.png'), hh_pop_structure)

#sampled poulation structure

sampled_indiv_pop_structure <- ggplot(kobo.data$DTRANK_individual_human %>%
                                        group_by(gender, participant_age) %>%
                                        summarise(n = n()) %>%
                                        mutate(population = n/sum(n)*100), 
                                      aes(x = participant_age, fill = gender,
                                          y = ifelse(gender == "male",-population,population))) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(x = "Age", 
       y = "Percent of population",
       fill = 'Gender',
       title = "Population Structure of Sampled Individuals")
#save
ggsave(here('output','graphics','sampled_individuals_population_structure.png'), sampled_indiv_pop_structure)

#sampled pop structure by county
sampled_indiv_pop_structure_county <- ggplot(kobo.data$DTRANK_individual_human %>%
                                               group_by(county, gender, participant_age) %>%
                                               summarise(n = n()) %>%
                                               mutate(population = n/sum(n)*100), 
                                             aes(x = participant_age, fill = gender,
                                                 y = ifelse(gender == "male",-population,population))) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(x = "Age", 
       y = "Percent of population",
       fill = 'Gender',
       title = "Population Structure of Sampled Individuals") +
  facet_wrap(~county)

#save
ggsave(here('output','graphics','sampled_individuals_population_structure_by_county.png'), sampled_indiv_pop_structure_county)


#livestock populations
livestock_sex_counts <- ggplot(kobo.data$DTRANK_individual_livestock %>%
                                 group_by(livestock_species, sex) %>%
                                 summarise(n = n()), 
                               aes(x = livestock_species, y = n, fill = sex)) +
  geom_col(position = "dodge") +
  labs(title = "Livestock Species and Sex",
       fill = 'Sex',
       x = "Species",
       y = "Count")

#save
ggsave(here('output','graphics','livestock_species_sex_counts.png'), livestock_sex_counts)


#### lab results ----
lab <- list()
lab$lab.control <- read.csv(list.files(here('input','lab_controls'), full.names = T), na = '') %>%
  slice(which(xPONENT == "Net MFI"):which(xPONENT == 'Count'))%>%
  select(1:6) %>%
  { colnames(.) <- .[2, ]; . } %>%
  rename_with(~'RVFV', contains('RVFV')) %>%
  rename_with(~'CCHFV', contains('CCHFV')) %>%
  rename_with(~'TULI', contains('Tuli')) %>% 
  mutate(Sample = str_replace_all(Sample, c("POS" = "Positive", "NEG" = "Negative"))) %>%
  filter(!is.na(Location) & Location != 'DataType:' & Location != 'Location') %>%
  group_by(Sample) %>%
  #calculate mean results for duplicate samples
  summarise(RVFV = mean(as.numeric(RVFV)),
            CCHFV = mean(as.numeric(CCHFV)),
            TULI = mean(as.numeric(TULI)))

for (i in 1:nrow(kobo.data$DTRANK_lab_results)) {
  
  if (str_detect(kobo.data$DTRANK_lab_results$mimetype[i], 'csv')){
    #.csv files
    media.file <- content(GET(url = kobo.data$DTRANK_lab_results$download_url[i],
                              config = authenticate(user=uname,password=pwd)),
                          as='text', encoding = 'UTF-8') %>%
      #parse to dataframe
      read.table(text=., header = TRUE, fill = TRUE, sep = ",", na.strings = '')
    
  } else {
    #.xlsx files
    content(GET(url = kobo.data$DTRANK_lab_results$download_url[i],
                config = authenticate(user=uname,password=pwd),
                write_disk(here('input','raw','lab_results',kobo.data$DTRANK_lab_results$cryobox_samples_file[i]), overwrite = TRUE)))
    
    media.file <- read_xlsx(here('input','raw','lab_results',kobo.data$DTRANK_lab_results$cryobox_samples_file[i]))
    
  }
  
  # extract plate details and add to lab_results dataframe
  kobo.data$DTRANK_lab_results$serial.number[i] <- media.file %>% filter(Program == 'SN') %>% pull(xPONENT)
  kobo.data$DTRANK_lab_results$batch[i] <- media.file %>% filter(Program == 'Batch') %>% pull(xPONENT)
  kobo.data$DTRANK_lab_results$run.date[i] <- media.file %>% filter(Program == 'Date') %>% pull(xPONENT)
  
  
  media.file <- media.file %>%
    slice(which(xPONENT == "Net MFI"):which(xPONENT == 'Count')) %>%
    select(1:6) %>%
    { colnames(.) <- .[2, ]; . } %>%
    rename_with(~'RVFV', contains('RVF')) %>%
    rename_with(~'CCHFV', contains('CCHF')) %>%
    rename_with(~'TULI', contains('TULI')) %>%
    filter(!is.na(Location) & Location != 'DataType:' & Location != 'Location') %>%
    mutate(Sample = str_replace_all(Sample, c("POS" = "Positive", "NEG" = "Negative")))%>%
    mutate(across(contains(c("RVF", "CCHF", "TULI")), as.double))
  
  #QC check controls
  qc.test <- media.file %>% 
    filter(Sample == 'Negative'| Sample == 'Positive') %>% select(-'Location',-'Total Events') %>%
    left_join(lab$lab.control, by = 'Sample') %>%
    rename_with(~str_replace_all(.,c('.x' = '.batch','.y'='.ref'))) %>%
    mutate(RVFV.pass = RVFV.batch < RVFV.ref*1.2 & RVFV.batch > RVFV.ref*0.8,
           CCHFV.pass = CCHFV.batch < CCHFV.ref*1.2 & CCHFV.batch > CCHFV.ref*0.8,
           TULI.pass = TULI.batch < TULI.ref*1.2 & TULI.batch > TULI.ref*0.8,
           serial.number = kobo.data$DTRANK_lab_results$serial.number[i],
           batch = kobo.data$DTRANK_lab_results$batch[i],
           run.date = kobo.data$DTRANK_lab_results$run.date[i]) %>%
    select(serial.number,batch,run.date,sample = Sample, matches('RVFV'),matches('CCHFV'),matches('TULI'))
  
  # print results
  kobo.data$DTRANK_lab_results$quality.control[i] <- ifelse(any(qc.test == FALSE),'Fail','Pass')
  
  media.file <- media.file %>%
    group_by(Sample) %>%
    #calculate mean results for duplicate samples
    summarise(RVFV.raw = mean(as.numeric(RVFV)),
              CCHFV.raw = mean(as.numeric(CCHFV)),
              TULI.raw = mean(as.numeric(TULI))) %>%
    #calculate ratio of signal to noise & binary pos/neg
    mutate(RVFV.ratio = RVFV.raw/RVFV.raw[str_detect(Sample, "Negative")],
           RVFV.pos = ifelse(RVFV.ratio >20,1,0),
           CCHFV.ratio = CCHFV.raw/CCHFV.raw[str_detect(Sample, "Negative")],
           CCHFV.pos = ifelse(CCHFV.ratio >20,1,0),
           TULI.ratio = TULI.raw/TULI.raw[str_detect(Sample, "Negative")],
           TULI.pos = ifelse(TULI.ratio >20,1,0),
           serial.number = kobo.data$DTRANK_lab_results$serial.number[i],
           batch = kobo.data$DTRANK_lab_results$batch[i],
           run.date = kobo.data$DTRANK_lab_results$run.date[i]
    ) %>%
    filter(!str_detect(Sample, "Negative|Positive")) %>%
    #filter(Sample != 'POS' & Sample != 'NEG') %>%
    select(sample = Sample, matches('RVFV'),matches('CCHFV'),matches('TULI'),
           serial.number,
           batch,
           run.date)
  #combine results from different plates
  lab$lab.qc <- if (i == 1) qc.test else bind_rows(lab$lab.qc, qc.test)
  lab$lab.results <- if (i == 1) media.file else bind_rows(lab$lab.results, media.file)
  lab$lab.results <-  lab$lab.results %>% filter(str_starts(sample, "DLQ"))
}

write.csv(lab$lab.results, here('output','DTRANK_lab_results.csv'), row.names = F)
write.csv(lab$lab.qc, here('output','DTRANK_lab_quality_control.csv'), row.names = F)


#### activity space mapping ----
#sort spatial data in repeats into combined point and polygon objects
#create empty lists for point and area data
activity.space.map <- list()

#extract and tidy main table
activity.space.map$main <- kobo.data$DTRANK_activity_space_mapping$main %>% 
  tidy_barcodes() %>% 
  #unnest attachment columns to be able to access urls
  unnest_wider(`_attachments`) %>%
  #attach HH GPS coordinates
  left_join(kobo.data$DTRANK_HH_demography_livestock %>% 
              select(county, 
                     ward, 
                     hh_id, 
                     starts_with('gps_')), by = 'hh_id')
#manually add gps coordinates for household which was missing data
activity.space.map$main[8,c('county','ward','gps_latitude','gps_longitude','gps_altitude','gps_precision','gps_wkt')] <- list('samburu','ndoto',1.640515,37.38376,0,0,'POINT (37.38376 1.640515 0)')
activity.space.map$main <- st_as_sf(activity.space.map$main,
                                    wkt = 'gps_wkt', 
                                    crs = 4326, 
                                    remove = F)

#write to gpkg
st_write(activity.space.map$main %>% tidy_strip_pii(),
         dsn = here('output','spatial','DTRANK_activity_space_mapping.gpkg'),
         layer = 'main_household',
         append = F)

#add hh id to each child table and make spatial
for (rep in dm_get_all_fks(kobo.data$DTRANK_activity_space_mapping)$child_table) {
  activity.space.map[[str_remove(rep,'_rep')]] <- kobo.data$DTRANK_activity_space_mapping[[rep]] %>%
    left_join(activity.space.map$main %>%
                st_drop_geometry() %>%
                select('hh_id', 'county','ward', '_index'),
              by = c('_parent_index'='_index'),
              relationship = 'many-to-many') %>%
    mutate(location_type = str_remove(rep,'_rep')) %>%
    tidy_strip_pii() %>%
    st_as_sf(wkt = select(.,contains('wkt')) %>% colnames(), crs=4326, remove = F)
  
  #write to gpkg
  st_write(activity.space.map[[str_remove(rep,'_rep')]],
           dsn = here('output','spatial','DTRANK_activity_space_mapping.gpkg'),
           layer = str_remove(rep,'_rep'),
           append = F)
}

#strip pii and tidy main activity space map
activity.space.map$main <- activity.space.map$main %>% tidy_strip_pii()

#save RDS file for spatial data analysis
saveRDS(activity.space.map, file = here('output','spatial','DTRANK_activity_space_mapping.RDS'))


# for (type in names(kobo.data$DTRANK_activity_space_mapping)[2:length(kobo.data$DTRANK_activity_space_mapping)]) {
#   #note - for some reason when downloading from the server, some of the wrong columns are included in the repeat datasets
#   
#   #points data
#   if (type %!in% c('grazing_rep','crops_rep')) {
#     activity.space.map$pts.raw[[type]] <- dm_flatten_to_tbl(kobo.data$DTRANK_activity_space_mapping,.start = !!as.name(type),.join=left_join) %>%
#       dplyr::mutate(
#         location_type = str_remove(type,'_rep')) %>%
#       select(-starts_with('_'), -contains(paste0('.',type)))
#     #change column names so row_binding will create a single column for repeated variables
#     names(activity.space.map$pts.raw[[type]]) <- str_remove_all(names(activity.space.map$pts.raw[[type]]),paste(paste0('_',str_remove_all(type,'_rep')),
#                                                                                                                 paste0(str_remove_all(type,'_rep'),'_'),
#                                                                                                                 '.main',
#                                                                                                                 sep = '|'))
#     
#     #area/geoshape/polygon data
#   } else if (type %in% c('grazing_rep','crops_rep')) {
#     activity.space.map$areas.raw[[type]] <- dm_flatten_to_tbl(kobo.data$DTRANK_activity_space_mapping,.start = !!as.name(type),.join=left_join) %>%
#       dplyr::mutate(
#         location_type = str_remove(type,'_rep')) %>%
#       select(-starts_with('_'), -contains(paste0('.',type)))
#     #change column names so row_binding will create a single column for repeated variables
#     names(activity.space.map$areas.raw[[type]]) <- str_remove_all(names(activity.space.map$areas.raw[[type]]),paste(paste0('_',str_remove_all(type,'_rep')),
#                                                                                                                     paste0(str_remove_all(type,'_rep'),'_'),
#                                                                                                                     '.main',
#                                                                                                                     sep = '|'))
#   }
# }
# 
# #combine points data and create spatial object
# activity.space.map$points <- bind_rows(activity.space.map$pts.raw) %>%
#   select(location_type, hh_id,date,
#          everything(),
#          -contains(c('yn','action_taken_','_count','_rep_count','action_taken.')),
#          -c(start,end,dev_id,uuid,instanceID),
#          'waterpoint_type'='type','water_use_livestock_human'='livestock_human') %>%
#   tidy_barcodes() %>%
#   st_as_sf(wkt = 'gps_wkt', crs = 'epsg:4326', remove = F)
# #write to gpkg
# st_write(activity.space.map$points,
#          dsn = here('output','spatial','DTRANK_activity_space_mapping.gpkg'),
#          layer = 'activity_space_mapping_points',
#          append = F)
# 
# #combine polygon data and create spatial object
# activity.space.map$areas <- bind_rows(activity.space.map$areas.raw) %>%
#   select(location_type, hh_id,date,
#          everything(),
#          -contains(c('yn','action_taken_','_count','_rep_count')),
#          -c(start,end,dev_id,uuid,instanceID),
#          'geometry' = 'gps_area_wkt') %>%
#   tidy_barcodes() %>%
#   st_as_sf(wkt = 'geometry', crs='epsg:4326')
# #write to gpkg
# st_write(activity.space.map$areas,
#          dsn = here('output','spatial','DTRANK_activity_space_mapping.gpkg'),
#          layer = 'activity_space_mapping_polygons',
#          append = F)

#### GPS_vector data tidying ----
#separate gps.vector data sections and save to a list
gps.vector.data <- list()
for (section in unique(kobo.data$DTRANK_GPS_vector$collection)) {
  if (section == 'initial'){
    gps.vector.data[[section]] <- kobo.data$DTRANK_GPS_vector %>% 
      # camel with ceres tag but no short term collar in DHH0100053
      mutate(gps_logger_id_initial = case_when(
        #`_uuid` == 'aab5f780-58a5-464f-936c-cae41509539f' ~ NA,
        `hh_id` == 'DHH010053' ~ NA,
        TRUE ~ gps_logger_id_initial
      )) %>%
      select(date,county,ward,collection,hh_id,
             contains('acaracide'),
             contains(section)) %>%
      filter(collection == section & gps_tick_initial == 'gps_tick')
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
         'ceres_tag_id'='ceres_tag_vid_initial',
         'date_deployed'='date.x','date_retrieved'='date.y',
         contains('acaracide'),
         contains('ticks'),
         download_url, mimetype,
         -'gps_tick_initial',-'gps_tick_final')

# data quality check on GPS collar data
if (sum(is.na(gps.vector.data$livestock$gps_logger_id))>1|anyNA(gps.vector.data$livestock$date_deployed)|sum(is.na(gps.vector.data$livestock$date_retrieved))>3) {# accounts for 1 entry (camel in DHH0100053) with ceres tag but no collar
  print("There are errors in matching initial GPS deployment data to final GPS retrieval data. This could be due to duplicates, data not sent, or incorrect livestock species or collar ID entered on the forms.")
  
  print(paste('Entries in initial collar deployment data: ',nrow(gps.vector.data$initial)))
  print(paste('Entries in final collar retrieval data: ',nrow(gps.vector.data$final)))
  print(paste('NAs in gps_logger_id column: ',sum(is.na(gps.vector.data$livestock$gps_logger_id))))
  print(paste('NAs in date_deployed column: ',sum(is.na(gps.vector.data$livestock$date_deployed))))
  print(paste('NAs in date_retrieved column: ',sum(is.na(gps.vector.data$livestock$date_retrieved))))
}

# there will always be 1 NA in gps_logger_id and 3 (4) NA in date_retrieved due to:
#a ceres tag deployed without a collar (camel from DHH0100053)
#a lost collar #4 from a sheep in DHH0100073
#a lost collar #1 from a goat in DHH010175
#possibly another collar #3 from a goat in DHH010187 stopped working so no data

## check livestock GPS vector data against individual livestock data to see if there are data entry errors
gps.vect.ids <- paste0(gps.vector.data$livestock$hh_id,'_',
                       gps.vector.data$livestock$livestock_species,'_',
                       gps.vector.data$livestock$gps_logger_id)

indiv.livestock <-kobo.data$DTRANK_individual_livestock %>%
  filter(short_term_selected == 'yes') %>%
  mutate(id = paste0(hh_id,'_',livestock_species,'_',gps_logger_id)) %>% pull(id)


#in GPS_vector data but not in individual livestock data
gps.vect.ids[!gps.vect.ids %in% indiv.livestock]

#in individual livestock data but not in GPS_vector
indiv.livestock[!indiv.livestock %in% gps.vect.ids]

# check human GPS data against individual human data to check for data entry errors
gps.vect.id.human <- paste0(gps.vector.data$human$hh_id,'_',
                       gps.vector.data$human$gps_logger_id_human)

indiv.human <-kobo.data$DTRANK_individual_human %>%
  filter(movement_study == 'yes') %>%
  mutate(id = paste0(hh_id,'_',gps_logger_id)) %>% pull(id)

#in GPS_vector data but not in individual human data
gps.vect.id.human[!gps.vect.id.human %in% indiv.human]

#in individual human data but not in GPS_vector
indiv.human[!indiv.human %in% gps.vect.id.human]


#### download movement tracks ----
######livestock movement ----
#gps.livestock.data <- list()
for (i in 1:nrow(gps.vector.data$livestock)) {
  if (paste0(gps.vector.data$livestock$hh_id[i],'_',
             gps.vector.data$livestock$livestock_species[i],'_',
             gps.vector.data$livestock$gps_logger_id[i],'.csv') %!in% list.files(here('input','raw','spatial','livestock')) &
      
      !is.na(gps.vector.data$livestock$download_url[i])) {
    
    # deal with .zip files
    if (gps.vector.data$livestock$mimetype[i] == 'application/zip'){
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
      
    } else { #csv files
      ##  #download media file
      media.file <- content(GET(url = gps.vector.data$livestock$download_url[i],
                                config = authenticate(user=uname,password=pwd)),
                            as='text', encoding = 'UTF-8') %>%
        #parse to dataframe
        read.table(text=., header = TRUE, fill = TRUE, sep = ",")
    }
    
    if (nrow(media.file)>0) {# do not save blank files
      
      #save to list for further cleaning
      #gps.livestock.data[[i]] <- media.file
      
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
#gps.human.data <- list()
for (i in 1:nrow(gps.vector.data$human)) {
  if (paste0(gps.vector.data$human$hh_id[i],'_',
             gps.vector.data$human$collection[i],'_',
             gps.vector.data$human$gps_logger_id_human[i],'.csv') %!in% list.files(here('input','raw','spatial','human')) &
      
      !is.na(gps.vector.data$human$gps_track_file_human[i]) & 
      
      gps.vector.data$human$gps_track_file_human[i] != '')
  {
    #download media file
    if (gps.vector.data$human$mimetype[i] == 'text/comma-separated-values'){#.csv files
      
      
      media.file <- content(GET(url = gps.vector.data$human$download_url[i],
                                config = authenticate(user=uname,password=pwd)),
                            as='text', encoding = 'UTF-8') %>% 
        #parse to dataframe
        read.table(text=., header = TRUE, fill = TRUE, sep = ",")
      
      if ('Latitude' %in% names(media.file)) {
        media.file <- media.file %>%
          mutate(Time = as_datetime(str_remove_all(Time,'"|='), tz = "africa/nairobi"),
                 Altitude.m. = ifelse(TRUE %in% str_detect(names(.),'Altitude.ft.'), .$Altitude.ft.,.$Altitude.m.)) %>%
          select(date_time = Time, lat = Latitude, lon = Longitude, elevation = Altitude.m.,satellites = Satellites.CN.22.,hdop = HDOP)
      } else {
        # for csv files converted to csv from gpx in QGIS
        media.file <- media.file %>%
          mutate(date_time = as_datetime(time, tz = "africa/nairobi")) %>%
          select(date_time, lat = X, lon = Y, elevation = ele, satellites = sat, hdop)
      }
    } else if (gps.vector.data$human$mimetype[i] == 'application/octet-stream'){
      
      file_path <- here('input','raw','spatial','human','gpx_files',paste0(gps.vector.data$human$hh_id[i],
                                                                           '_',
                                                                           gps.vector.data$human$collection[i],
                                                                           '_',
                                                                           gps.vector.data$human$gps_logger_id_human[i],
                                                                           '.gpx'))
      #download
      content(GET(url = gps.vector.data$human$download_url[i],
                  config = authenticate(user=uname,password=pwd)),
              as='raw') %>% 
        
        #write .gpx files
        writeBin(file_path)
      #read from written .gpx file
      if (st_layers(file_path)$features[1]>0) {
        
      }
      
      #some gpx files had to be loaded in QGIS and when saved, it output the data to the 'waypoints' layer, instead of the 'track_points' layer  
      media.file <- st_read(file_path, layer = ifelse(st_layers(file_path)$features[1]>0,'waypoints','track_points')) %>%
        mutate(lon = sf::st_coordinates(.)[,1],
               lat = sf::st_coordinates(.)[,2],
               date_time = force_tz(time, tzone = "africa/nairobi")) %>%
        st_drop_geometry() %>%
        select(date_time, lat, lon, elevation = ele, satellites = sat, hdop)
    }
    
    #export to folder
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
      ID = gsub('.csv','',file),.before = 'DATE',
      HH_ID = str_split(file,'_')[[1]][1],
      LIVESTOCK_SPECIES = str_split(file,'_')[[1]][2],
      GPS_LOGGER_ID = as.double(gsub('.csv','',str_split(file,'_')[[1]][3])),
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
    select(-any_of(c('INDEX','TAG'))) %>%
    rename_with(tolower) %>%
    left_join(community.ref, by = 'hh_id')
  
  # create joined data file with all GPS points
  
  gps.vector.data$livestock.all.pts[[gsub('.csv','',file)]] <- gps.data
  # 
  # if (file == list.files(here('input','raw','spatial','livestock'), pattern = '.csv')[1]){
  #   gps.vector.data$livestock.all.pts.combined <- gps.data
  # } else {
  #   gps.vector.data$livestock.all.pts.combined <- bind_rows(gps.vector.data$livestock.all.pts.combined, gps.data)
  # }
  
  #collapse points into multipoint object
  gps.data <- gps.data %>%
    st_as_sf(coords = c('longitude.e.w', 'latitude.n.s'), crs = 4326) %>% 
    summarise(geometry = st_combine(geometry)) %>%
    st_cast('MULTIPOINT') %>%
    mutate(hh_id = str_split(file,'_')[[1]][1],
           livestock_species = str_split(file,'_')[[1]][2],
           gps_logger_id = as.double(gsub('.csv','',str_split(file,'_')[[1]][3])))
  
  if (file == list.files(here('input','raw','spatial','livestock'), pattern = '.csv')[1]){
    gps.vector.data$livestock.multipoint <- gps.data
  } else {
    gps.vector.data$livestock.multipoint <- bind_rows(gps.vector.data$livestock.multipoint, gps.data)
  }
}

#right join to multipoint data to remove entries with no GPS data file
gps.vector.data$livestock.multipoint <- right_join(gps.vector.data$livestock, gps.vector.data$livestock.multipoint,
                                                   by = c('hh_id','livestock_species','gps_logger_id')) %>%
  st_as_sf()

#write all livestock gps point dataset to csv
#!!this is a huge csv file - DO NOT open this in excel or you will lose data!!
write.csv(bind_rows(gps.vector.data$livestock.all.pts),
          here('output','spatial','DTRANK_livestock_pts_all.csv'),
          row.names = F, na = '')

#save RDS file for spatial data analysis
saveRDS(gps.vector.data$livestock.all.pts, file = here('output','spatial','DTRANK_livestock_pts_all.RDS'))

##### humans ----
for (file in list.files(here('input','raw','spatial','human'), pattern = '.csv')) {
  gps.data <- read.csv(here('input','raw','spatial','human',file), na.strings = '', row.names = NULL) %>% 
    mutate(id = gsub('.csv','',file),.before = 'date_time',
           hh_id = str_split(file,'_')[[1]][1],
           gps_logger_id_human = as.double(gsub('.csv','',str_split(file,'_')[[1]][3])),
           date = as.Date(date_time),
           time = format(as.POSIXct(date_time), format="%H:%M:%S"))#%>%
    #left_join(community.ref, by = 'hh_id')
  
  # create joined data file with all GPS points
  
  gps.vector.data$human.all.pts[[gsub('.csv','',file)]] <- gps.data
  
  # if (file == list.files(here('input','raw','spatial','livestock'), pattern = '.csv')[1]){
  #   gps.vector.data$human.all.pts <- gps.data
  # } else {
  #   gps.vector.data$human.all.pts <- bind_rows(gps.vector.data$human.all.pts, gps.data)
  # }
  
  #collapse points into multipoint object
  gps.data <- gps.data %>%
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
    summarise(geometry = st_combine(geometry)) %>%
    st_cast('MULTIPOINT') %>%
    mutate(hh_id = str_split(file,'_')[[1]][1],
           gps_logger_id_human = as.double(gsub('.csv','',str_split(file,'_')[[1]][3])))
  
  if (file == list.files(here('input','raw','spatial','human'), pattern = '.csv')[1]){
    gps.vector.data$human.multipoint <- gps.data
  } else {
    gps.vector.data$human.multipoint <- bind_rows(gps.vector.data$human.multipoint, gps.data)
  }
}

#right join to drop any entries without GPS data
gps.vector.data$human.multipoint <- right_join(gps.vector.data$human, gps.vector.data$human.multipoint,
                                               by = c('hh_id','gps_logger_id_human')) %>%
  st_as_sf()

#write all human gps point dataset to csv
write.csv(bind_rows(gps.vector.data$human.all.pts),
          here('output','spatial','DTRANK_human_pts_all.csv'),
          row.names = F, na = '')

#save RDS file for spatial data analysis
saveRDS(gps.vector.data$human.all.pts, file = here('output','spatial','DTRANK_human_pts_all.RDS'))

# save separate GPS & vector csv files
write.csv(gps.vector.data$livestock, here('output','DTRANK_livestock_GPS_vector.csv'), row.names = F, na = '')
write.csv(gps.vector.data$human, here('output','DTRANK_human_GPS.csv'), row.names = F, na = '')
write.csv(gps.vector.data$environment, here('output','DTRANK_evironment_vector.csv'), row.names = F, na = '')

# save human and livestock spatial files
st_write(gps.vector.data$livestock.multipoint,
         dsn = here('output','spatial','DTRANK_livestock_GPS_vector_multipoint.gpkg'),
         layer = 'livestock_GPS_vector_multipoint',
         append = F)
st_write(gps.vector.data$human.multipoint,
         dsn = here('output','spatial','DTRANK_human_GPS_multipoint.gpkg'),
         layer = 'human_GPS_multipoint',
         append = F)

#### cryobox sample data ----

#take only the latest entry i.e. most recently uploaded cryobox file
kobo.data$DTRANK_cryobox_sample_booking <- kobo.data$DTRANK_cryobox_sample_booking %>% arrange(desc(date)) %>% slice_head()

#download cryobox file
content(GET(url = kobo.data$DTRANK_cryobox_sample_booking$download_url[1],
            config = authenticate(user=uname,password=pwd),
            write_disk(here('input','raw','lab_results','cryobox.xlsx'), overwrite = TRUE)))

cryobox <- list()

for (sheet in excel_sheets(here('input','raw','lab_results','cryobox.xlsx'))) {
  #read each sheet
  cryobox[[sheet]] <- read_xlsx(here('input','raw','lab_results','cryobox.xlsx'), sheet = sheet, na = '', skip = 1) 
  tables_list <- list()
  
  for (col in which(str_detect(colnames(cryobox[[sheet]]), "Date Scanned"))) {
    #add an extra column if the last notes column is blank and therefore doesn't load
    if (ncol(cryobox[[sheet]])<max(which(str_detect(colnames(cryobox[[sheet]]), "Date Scanned")) +4)) { 
      cryobox[[sheet]] <- cryobox[[sheet]] %>% mutate(extra_col = NA)
    }
    subset_df <- cryobox[[sheet]][, col:min(col + 4, ncol(cryobox[[sheet]]))] %>% #take the 'Date Scanned column plus the next 4 columns
      rename_with(~c('date_scanned','aliquot','box_id','position','notes')) %>%
      mutate(date_scanned = as.character(date_scanned),
             notes = as.character(notes))
    tables_list[[length(tables_list) + 1]] <- subset_df
  }
  
  #bind separated tables together
  cryobox[[sheet]] <- bind_rows(tables_list) %>%
    filter(!is.na(aliquot)) %>%
    fill(date_scanned,.direction = 'down')
  
}

#combine all datasets into one table
cryobox$all_samples <- bind_rows(cryobox) %>%
  arrange(box_id, position) %>%
  mutate(box_id = toupper(box_id),
         aliquot = toupper(aliquot))

#duplicated aliquot codes in cryobox file
cryobox.duplicates <- cryobox$all_samples %>%
  group_by(aliquot) %>%
  filter(n() > 1) %>%
  ungroup()


#####livestock samples with box and position ----
#add serum blood 1
cryobox$livestock <- left_join(kobo.data$DTRANK_individual_livestock %>% 
                                 select(hh_id, species = livestock_species, serum_parent = Serum_blood_parentID, aliquot = Serum_blood_aliquot1) %>%
                                 mutate(aliquot_type = 'serum_blood_1'),
                               cryobox$all_samples,
                               by = 'aliquot') %>%
  bind_rows(
    #add serum blood 2
    left_join(kobo.data$DTRANK_individual_livestock %>% 
                select(hh_id, species = livestock_species,serum_parent = Serum_blood_parentID, aliquot = Serum_blood_aliquot2) %>%
                mutate(aliquot_type = 'serum_blood_2'),
              cryobox$all_samples,
              by = 'aliquot')) %>%
  
  bind_rows(
    #add EDTA 1
    left_join(kobo.data$DTRANK_individual_livestock %>% 
                select(hh_id, species = livestock_species,serum_parent = Serum_blood_parentID, aliquot = EDTA_blood_aliquot1) %>%
                mutate(aliquot_type = 'EDTA_blood_1'),
              cryobox$all_samples,
              by = 'aliquot')) %>%
  
  bind_rows(
    #add EDTA 2
    left_join(kobo.data$DTRANK_individual_livestock %>% 
                select(hh_id, species = livestock_species,serum_parent = Serum_blood_parentID, aliquot = EDTA_blood_aliquot2) %>%
                mutate(aliquot_type = 'EDTA_blood_2'),
              cryobox$all_samples,
              by = 'aliquot'))

##### human samples with box and position----
#add serum blood 1
cryobox$human <- left_join(kobo.data$DTRANK_individual_human %>% 
                             select(hh_id, serum_parent = Serum_blood_parentID, aliquot = Serum_blood_aliquot1) %>%
                             mutate(aliquot_type = 'serum_blood_1',
                                    species = 'human'),
                           cryobox$all_samples,
                           by = 'aliquot') %>%
  bind_rows(
    #add serum blood 2
    left_join(kobo.data$DTRANK_individual_human %>% 
                select(hh_id, serum_parent = Serum_blood_parentID, aliquot = Serum_blood_aliquot2) %>%
                mutate(aliquot_type = 'serum_blood_2',
                       species = 'human'),
              cryobox$all_samples,
              by = 'aliquot')) %>%
  
  bind_rows(
    #add EDTA 1
    left_join(kobo.data$DTRANK_individual_human %>% 
                select(hh_id, serum_parent = Serum_blood_parentID, aliquot = EDTA_blood_aliquot1) %>%
                mutate(aliquot_type = 'EDTA_blood_1',
                       species = 'human'),
              cryobox$all_samples,
              by = 'aliquot')) %>%
  
  bind_rows(
    #add EDTA 2
    left_join(kobo.data$DTRANK_individual_human %>% 
                select(hh_id, serum_parent = Serum_blood_parentID, aliquot = EDTA_blood_aliquot2) %>%
                mutate(aliquot_type = 'EDTA_blood_2',
                       species = 'human'),
              cryobox$all_samples,
              by = 'aliquot'))

#####wildlife samples with box and position----
#add serum blood 1
cryobox$wildlife <- left_join(kobo.data$DTRANK_wildlife_sampling %>% 
                                select(seca_S_id, secb_aclass, serum_parent = Serum_blood_parentID, aliquot = Serum_blood_aliquot1) %>%
                                mutate(aliquot_type = 'serum_blood_1',
                                       secb_aclass = as.character(secb_aclass),
                                       species = case_when(secb_aclass == '3' ~ 'avian',
                                                           secb_aclass == '1' ~ 'rodent')),
                              cryobox$all_samples,
                              by = 'aliquot') %>%
  bind_rows(
    #add serum blood 2
    left_join(kobo.data$DTRANK_wildlife_sampling %>% 
                select(seca_S_id, secb_aclass, serum_parent = Serum_blood_parentID, aliquot = Serum_blood_aliquot2) %>%
                mutate(aliquot_type = 'serum_blood_2',
                       secb_aclass = as.character(secb_aclass),
                       species = case_when(secb_aclass == '3' ~ 'avian',
                                           secb_aclass == '1' ~ 'rodent')),
              cryobox$all_samples,
              by = 'aliquot')) %>%
  
  bind_rows(
    #add EDTA 1
    left_join(kobo.data$DTRANK_wildlife_sampling %>% 
                select(seca_S_id, secb_aclass, serum_parent = Serum_blood_parentID, aliquot = secd_edta_1) %>%
                mutate(aliquot_type = 'EDTA_blood_1',
                       secb_aclass = as.character(secb_aclass),
                       species = case_when(secb_aclass == '3' ~ 'avian',
                                           secb_aclass == '1' ~ 'rodent')),
              cryobox$all_samples,
              by = 'aliquot')) %>%
  
  bind_rows(
    #add EDTA 2
    left_join(kobo.data$DTRANK_wildlife_sampling %>% 
                select(seca_S_id, secb_aclass, serum_parent = Serum_blood_parentID, aliquot = secd_edta_2) %>%
                mutate(aliquot_type = 'EDTA_blood_2',
                       secb_aclass = as.character(secb_aclass),
                       species = case_when(secb_aclass == '3' ~ 'avian',
                                           secb_aclass == '1' ~ 'rodent')),
              cryobox$all_samples,
              by = 'aliquot')) %>%
  
  #add HH ID
  left_join(kobo.data$DTRANK_wildlife_site %>% select(hh_id, secb_S_id),
            by = c('seca_S_id' = 'secb_S_id')) %>%
  select(-seca_S_id, -secb_aclass)

#####vector samples with box and position----

cryobox$vector <- left_join(gps.vector.data$livestock %>%
                              pivot_longer(
                                cols = all_of(grep("sample_id$", names(.), value = TRUE)),
                                names_to = "vector_sample",
                                values_to = "vector_sample_id"
                              ) %>%
                              bind_cols(vector_count = gps.vector.data$livestock %>%
                                          pivot_longer(
                                            cols = names(.)[which(names(.) %in% grep("_sample_id$", names(.), value = TRUE)) - 1],  
                                            #cols = all_of(names(.)[match(all_of(grep("sample_id$", names(.), value = TRUE)), names(.)) - 1]),
                                            names_to = "vector_sample",
                                            values_to = "vector_count"
                                          ) %>% select(vector_count)) %>%
                              select(hh_id,species = livestock_species,serum_parent = vector_sample,aliquot = vector_sample_id,vector_count) %>%
                              mutate(aliquot_type = 'vector'),
                            cryobox$all_samples,
                            by = 'aliquot') %>%
  bind_rows(left_join(
    gps.vector.data$environment %>%
      select(hh_id,aliquot=tick_env_sample_id,vector_count=tick_env_count,notes=tick_env_notes) %>%
      mutate(aliquot_type = 'vector_environment'),
    cryobox$all_samples %>% select(-notes),
    by = 'aliquot')) %>%
  filter(!is.na(aliquot))

# combine livestock, human, wildlife and vector samples

cryobox$all_samples_box_position <- bind_rows(cryobox$livestock,cryobox$human) %>%
  bind_rows(cryobox$wildlife) %>%
  bind_rows(cryobox$vector) %>%
  arrange(box_id, position) %>%
  select(box_id, position, aliquot, aliquot_type, species, serum_parent, hh_id) %>%
  unique()

write.csv(cryobox$all_samples_box_position,
          here('output','DTRANK_cryobox_sample_positions.csv'),
          na = '',
          row.names = F)


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
         `Short term GPS Tracks` = GPS.tracks,
         `Long term GPS Ear Tags` = Ceres.tags,
         everything()) %>%
  mutate(Species = str_to_title(Species))
write.csv(sample.sum, here('output','DTRANK_sample_summary.csv'), row.names = F, na='')

#### tidy & print remaining datasets ----

#tidy remaining datasets & print to output folder

for (dataset in c('DTRANK_RVFV_rapid_test','DTRANK_HH_valuechain','DTRANK_HH_demography_livestock','DTRANK_ecotones', 'DTRANK_trap_booking','DTRANK_wildlife_site')) {
  kobo.data[[dataset]] <- kobo.data[[dataset]] %>% tidy_strip_pii()
  
  write.csv(kobo.data[[dataset]],
            here('output',paste0(dataset,'.csv')),
            row.names = F,
            na = '')
  
}
#### create SQL database -----

#create database
sql.db <- dbConnect(drv = RSQLite::SQLite(),
                    dbname = here('output','DTRANK_SQL_database.sqlite'))

#add final datasets
for (dataset in c('DTRANK_ecotones',
                  #'DTRANK_extension_service_providers',
                  'DTRANK_HH_demography_livestock',
                  'DTRANK_HH_valuechain',
                  'DTRANK_individual_human',
                  'DTRANK_individual_livestock',
                  #'DTRANK_input_suppliers',
                  'DTRANK_RVFV_rapid_test',
                  'DTRANK_trap_booking',
                  'DTRANK_wildlife_sampling',
                  'DTRANK_wildlife_site')) {
  
  dbWriteTable(conn = sql.db, 
               name = dataset,
               value =unnest_wider(kobo.data[[dataset]],kobo.data[[dataset]] %>% select_if(is.list) %>% names(), names_sep = '_'),
               # value = ifelse(any(sapply(kobo.data[[dataset]], is.list)),
               #                unnest_wider(kobo.data[[dataset]],kobo.data[[dataset]] %>% select_if(is.list) %>% names(), names_sep = '_'),
               #                kobo.data[[dataset]]),
               overwrite = T)
}

#add spatial datasets
dbWriteTable(conn = sql.db, 
             name = 'DTRANK_livestock_GPS_vector',
             value = gps.vector.data$livestock,
             overwrite = T)
dbWriteTable(conn = sql.db, 
             name = 'DTRANK_human_GPS',
             value = gps.vector.data$human,
             overwrite = T)
dbWriteTable(conn = sql.db, 
             name = 'DTRANK_evironment_vector',
             value = gps.vector.data$environment,
             overwrite = T)
dbWriteTable(conn = sql.db, 
             name = 'DTRANK_livestock_pts_all',
             value = bind_rows(gps.vector.data$livestock.all.pts),
             overwrite = T)
dbWriteTable(conn = sql.db, 
             name = 'DTRANK_human_pts_all',
             value = bind_rows(gps.vector.data$human.all.pts),
             overwrite = T)

#add activity space mapping
# dbWriteTable(conn = sql.db, 
#              name = 'DTRANK_activity_space_mapping',
#              value = activity.space.map %>%
#                st_drop_geometry(),
#              overwrite = T)
for (reps in names(activity.space.map)) {
  dbWriteTable(conn = sql.db, 
               name = paste0('DTRANK_activity_space_mapping_',reps),
               value = activity.space.map[[reps]] %>%
                 st_drop_geometry(),
               overwrite = T)
}


#add lab results
dbWriteTable(conn = sql.db, 
             name = 'DTRANK_lab_results',
             value = lab$lab.results,
             overwrite = T)
dbWriteTable(conn = sql.db, 
             name = 'DTRANK_human_pts_all',
             value = lab$lab.qc,
             overwrite = T)

#disconnect SQL database
dbDisconnect(sql.db)


#### upload to Google Drive ----

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

#upload spatial objects
upload_to_google(source.folder = here('output','spatial'),
                 dest.folder = 'DTRA-NK/Data/Spatial/',
                 pattern = '.csv')

#upload spatial objects
upload_to_google(source.folder = here('output','spatial'),
                 dest.folder = 'DTRA-NK/Data/Spatial/',
                 pattern = '.RDS')

#upload ecotone images
upload_to_google(source.folder = here('output','images','ecotones'),
                 dest.folder = 'DTRA-NK/Data/Images/Ecotones/',
                 pattern = '.jpg|.png')

#upload wildlife images
upload_to_google(source.folder = here('output','images','wildlife'),
                 dest.folder = 'DTRA-NK/Data/Images/Wildlife/',
                 pattern = '.jpg|.png')

#upload SQLite database - THIS IS 500MB PLUS SO AVOID UPLOADING
upload_to_google(source.folder = here('output'),
                 dest.folder = 'DTRA-NK/Data/',
                 pattern = '.sqlite')



##end-------------------------------------------------------------------

