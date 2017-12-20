# Download, merge, and returnwork-in-progress google sheets
# INPUT: sheet_names: vector of sheet names
#        google_auth_key: a google auth key, usually stored in ".httr-oauth" file
#        tempdir: the directory where the files should be downloaded
# OUTPUT: a data frame that contains all work-in-progress screening data for all team members
# EXAMPLE: get_screening_sheets(sheets)
# TODO: Add error handling
library(dplyr)
library(purrr)
library(googlesheets)

get_screening_sheets <-
    function(sheet_names,
             sheet_keys,
             google_auth_key = ".httr-oauth",
             temp_dir = "temp_gs",
             keep_temp = FALSE){
        
    gs_auth(cache = google_auth_key)
    # If temp dir exists, remove, and recreate
    if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
    dir.create(temp_dir)
    # Download all work-in-progress google sheets
    for (i in seq_along(sheet_names)) {
        gs_key(sheet_keys[i]) %>% 
        gs_download(ws = 1, to = paste(temp_dir, sheet_names[i], sep = "/"), overwrite = TRUE)
    }
    # Read all downloaded files and bind them together
    output <- map(list.files(temp_dir, "_articles.csv"),
        ~read_csv(paste0(temp_dir, "/",.x)) %>% 
            mutate(decision = as.character(decision) %>% trimws()),
                   reason = reason %>% trimws()) %>% 
        bind_rows()
    # Remove temporary directory if keep == FALSE
    if (keep_temp == FALSE) unlink(temp_dir, recursive = TRUE)
    output    
}