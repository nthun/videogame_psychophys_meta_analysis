# Download, merge, and returnwork-in-progress google sheets
# INPUT: sheet_names: vector of sheet names
#        google_auth_key: a google auth key, usually stored in ".httr-oauth" file
#        tempdir: the directory where the files should be downloaded
# OUTPUT: a data frame that contains all work-in-progress screening data for all team members
# EXAMPLE: get_screening_sheets(sheets)
library(dplyr)
library(purrr)
library(googlesheets)

get_screening_sheets <-
    function(sheet_names,
             google_auth_key = ".httr-oauth",
             tempdir = "temp_gs",
             keep_temp = FALSE){
        
    gs_auth(cache = google_auth_key)
    # If temp dir exists, remove, and recreate
    if (dir.exists(tempdir)) unlink(tempdir, recursive = TRUE)
    dir.create(tempdir)
    # Download all work-in-progress google sheets
    for (i in sheet_names){
        gs_title(i) %>% 
        gs_download(ws = 1,to = paste(tempdir, i, sep = "/"), overwrite = TRUE)
    }
    # Read all downloaded files and bind them together
    output <- map(list.files(tempdir, "_articles.csv"),
        ~read_csv(paste0(tempdir(), "/",.x)) %>% 
            mutate(decision = as.character(decision) %>% trimws()),
                   reason = reason %>% trimws()) %>% 
        bind_rows()
    # Remove temporary directory if keep == FALSE
    if (keep == FALSE) unlink(tempdir, recursive = TRUE)
    output    
}