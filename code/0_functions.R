# DTRA-NK - Data Preliminary
# Sirimon Thomas
# April 2024

#this script contains all the custom functions for accessing and cleaning the data for the DTRA-NK project

#NOT IN operator
`%!in%` <- Negate(`%in%`)

#function to incorporate manually entered barcodes and delete the manual entry column
#identifier is the string in the column names which identifies the manual entry columns. This can be a string or a vector of strings.
tidy_barcodes <- function(data, identifier = c('_m','_manual')){
  for (id in identifier) {
    barcode.cols <- names(data) %>% str_subset(paste0(id,'$')) %>% str_extract(paste0("^.*(?=",id,")")) %>% unique()
    for (col in barcode.cols) {
      for (i in 1:nrow(data)) {
        
        if (!is.na(data[paste0(col,id)][i,1]) & data[paste0(col,id)][i,1]!='' & is.na(data[col][i,1])){
          data[paste0(col)][i,1] <- data[paste0(col,id)][i,1]
        }
      }
      data <- data %>% select(-paste0(col,id))
    }
  }
  return(data)
}

#function for adding identifier to separate communities in same ward
# add_community_id <- function(data) {
#   
#   data <- data %>%
#     arrange(ward, as_date(date)) %>%
#     group_by(ward) %>%
#     mutate(
#       day_diff = as.numeric(difftime(as_date(date), lag(as_date(date)), units = "days")),
#       session_break = ifelse(is.na(day_diff) | day_diff > 3, 1, 0),
#       session_id = cumsum(session_break),
#       session_multi = max(session_id)
#     ) %>%
#     ungroup() %>%
#     mutate(
#       ward = ifelse(session_multi > 1, 
#                     paste(ward, as.character(session_id), sep = '_'), 
#                     ward))%>%
#     select(-c(day_diff, session_break, session_id, session_multi)) %>%
#     arrange(as_date(date))
#   
#   return(data)
# }

add_community_id <- function(data, column_name = 'ward') {
  if (column_name %in% names(data)) {
    data <- data %>%
      arrange(!!sym(column_name), as_date(date)) %>%
      group_by(!!sym(column_name)) %>%
      mutate(
        day_diff = as.numeric(difftime(as_date(date), lag(as_date(date)), units = "days")),
        session_break = ifelse(is.na(day_diff) | day_diff > 3, 1, 0),
        session_id = cumsum(session_break),
        session_multi = max(session_id)
      ) %>%
      ungroup() %>%
       mutate(
        !!sym(column_name) := ifelse(session_multi > 1,
                                    paste(!!sym(column_name), as.character(session_id), sep = '_'),
                                    !!sym(column_name))
      ) %>%
      select(-c(day_diff, session_break, session_id, session_multi)) %>%
      arrange(as_date(date))
    
    return(data)
    
  } else {
    stop("The specified column name is not present in the data.")
  }
}


#function for Kobo media download
#depends on KobocollectR
#need to edit for robotoolbox
kobo_media <- function (url = "kf.kobotoolbox.org", uname, pwd, assetid, fsep = ";", 
                        sleep = 2, identifier = "URL", timeoutval = 300, destfolder = "media") 
{
  dat <- kobo_df_download(url = url, uname = uname, pwd = pwd, 
                          assetid = assetid, lang = "_xml", sleep = sleep, 
                          fsep = fsep)
  if (!is.null(dat)) {
    cnamesdat <- colnames(dat)
    urlcols <- cnamesdat[grepl(paste0("*", identifier), cnamesdat)]
    options(timeout = max(timeoutval, getOption("timeout")))
    if (!file.exists(destfolder)) {
      dir.create(destfolder)
    }
    for (i in 1:nrow(dat)) {
      if (dat[,urlcols[1]][i] != ''){
        media.file <- content(GET(url = dat[,urlcols[1]][i], config = authenticate(user=uname,password=pwd)),
                              as='raw')
        writeBin(media.file,here(destfolder,paste0(dat$hh_id[i],
                                                   '_',
                                                   dat$livestock_species_final[i],#need to edit to make this more robust to different column names
                                                   '_',
                                                   dat$gps_logger_id_initial[i],
                                                   '.csv')))
      }
    }
    return(TRUE)
  }
  else {
    print("Data could not be downloaded. Please try again or check the parameters.")
    return(FALSE)
  }
}

#function for adding serum sample daughter samples

add_daughter_samples <- function(data){
  data <- data %>% 
    left_join(kobo.data$DTRANK_serum_sample_booking %>% 
                select('sample', 'Serum_blood_parentID','Serum_blood_aliquot1','Serum_blood_aliquot2'),
              by = 'Serum_blood_parentID') %>%
    relocate(c('Serum_blood_aliquot1','Serum_blood_aliquot2'), .after = 'Serum_blood_parentID')
}



# #function for data frame download
# kobo_df_download_2 <- function (url = "eu.kobotoolbox.org", uname = "", pwd = "", assetid = "", 
#           all = "false", lang = "_default", hierarchy = "false", include_grp = "true", 
#           grp_sep = "/", fsep = ";", multi_sel = "both", media_url = "true", 
#           fields = NULL, sub_ids = NULL, sleep = 2) 
# {
#   new_export_details <- export_creator(url = url, uname = uname, 
#                                        pwd = pwd, assetid = assetid, type = "csv", all = all, 
#                                        lang = lang, hierarchy = hierarchy, include_grp = include_grp, 
#                                        grp_sep = grp_sep, multi_sel = multi_sel, fields = fields, 
#                                        media_url = media_url, sub_ids = sub_ids, sleep = sleep)
#   Sys.sleep(sleep)
#   if (is.null(new_export_details)) {
#     print("export creation was not successful")
#     return(NULL)
#   }
#   else {
#     dff <- export_downloader(new_export_details[[1]], fsep, 
#                              uname, pwd, sleep)
#     deleteact <- DELETE(url = paste0(url, "/api/v2/assets/", 
#                                      assetid, "/exports/", new_export_details[[2]], "/"), 
#                         authenticate(user = uname, password = pwd), progress())
#     while (is.na(deleteact$status_code) | is.null(deleteact$status_code)) {
#       print("Attempting export deletion \n")
#     }
#     warn_for_status(deleteact, "delete export. Please delete manually.")
#     if (deleteact$status_code == 204) 
#       print("Export deleted from server")
#     return(dff)
#   }
# }
# 


#' 
#' # extract geoshape to wkt
#' #' @noRd
#' #' @importFrom dplyr rename_with
#' #' @importFrom tidyr separate_wider_delim
#' #' @importFrom tidyselect all_of
#' #' @importFrom labelled set_variable_labels var_label
#' extract_geoshape <- function(x, form) {
#'   cond <- form$type %in% "geoshape"
#'   nm <- unique(form$name[cond])
#'   nm <- intersect(names(x), nm)
#'   if (any(cond) && length(nm) > 0) {
#'     wkt_geoshape <- function(x) {
#'       x <- strsplit(x, ";")
#'       pattern <- "^(\\s*-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)(\\s+-?\\d+(?:\\.\\d+)?)$"
#'       replacement <- "\\2 \\1 \\3"
#'       x <- lapply(x, \(s) gsub(pattern, replacement, s))
#'       x <- vapply(x, \(s) paste0("POLYGON ((", paste0(s, collapse = ", "), "))"),
#'                   character(1))
#'       x
#'     }
#' 
#'     separate_geoshape <- function(x, col) {
#'       lbl <- var_label(select(x, all_of(col)))
#'       lbl <- set_names(lbl, paste0(col, "_wkt"))
#'       lbl <- lapply(lbl, function(x)  paste0(x, "::", "wkt"))
#' 
#'       x <- x |>
#'         mutate(across(.cols = all_of(col),
#'                       .fns = ~ wkt_geoshape(.x),
#'                       .names = "{.col}_wkt")) |>
#'         set_variable_labels(.labels = lbl, .strict = FALSE)
#'       x
#'     }
#'     x <- separate_geoshape(x, nm)
#'   }
#'   x
#' }

wkt_geoshape <- function(x) {
  x <- strsplit(x, ";")
  pattern <- "^(\\s*-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)(\\s+-?\\d+(?:\\.\\d+)?)$"
  replacement <- "\\2 \\1 \\3"
  x <- lapply(x, \(s) gsub(pattern, replacement, s))
  x <- vapply(x, \(s) paste0("POLYGON ((", paste0(s, collapse = ", "), "))"),
              character(1))
  x
}

# separate_geoshape <- function(x, col) {
#   lbl <- var_label(select(x, all_of(col)))
#   lbl <- set_names(lbl, paste0(col, "_wkt"))
#   lbl <- lapply(lbl, function(x)  paste0(x, "::", "wkt"))
#   
#   x <- x |>
#     mutate(across(.cols = all_of(col),
#                   .fns = ~ wkt_geoshape(.x),
#                   .names = "{.col}_wkt")) |>
#     set_variable_labels(.labels = lbl, .strict = FALSE)
#   x
# }