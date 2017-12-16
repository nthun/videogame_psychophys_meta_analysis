# Download and merge work-in-progress google sheets
# INPUT: sheet_names: vector of sheet names
#        google_auth_key: a google auth key, usually stored in ".httr-oauth" file
# OUTPUT: a data frame that contains all work-in-progress screening data for all team members
# EXAMPLE: get_screening_sheets(sheets)

get_screening_sheets <- function(sheet_names, google_auth_key = ".httr-oauth"){
    gs_auth(cache = google_auth_key)
    # If temp dir exists, remove, and recreate
    if (dir.exists("temp_gs")) unlink("temp_gs", recursive = TRUE)
    dir.create("temp_gs")
    # Download all work-in-progress google sheets
    for (i in sheet_names){
        gs_title(i) %>% 
        gs_download(ws = 1,to = paste("temp_gs", i, sep = "/"), overwrite = TRUE)
    }
    # Read all downloaded files and bind them together
    output <- map(list.files("temp_gs", "_articles.csv"),
        ~read_csv(paste0(tempdir(), "/",.x)) %>% 
            mutate(decision = as.character(decision) %>% trimws()),
                   reason = reason %>% trimws()) %>% 
        bind_rows()
    # Remove temporary directory
    unlink("temp_gs", recursive = TRUE)
    output    
}


