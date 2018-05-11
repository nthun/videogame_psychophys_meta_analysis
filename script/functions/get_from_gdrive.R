# Get sheets from google drive

# A generic function to get all spreadsheets in a gdrive and collapse them into one object
# INPUT: A google drive path where the sheets are stored
# OUTPUT: A nested tibble which contains the content of all files.
# EXAMPLE: get_from_gdrive("Research/Video game psychophysiology meta/Literature search/Consensus meetings/")
# TODO: error handling: check if all files are google sheets
# TODO: data validation, e.g. check if all the files have the right format

library(dplyr)

get_from_gdrive <- function(gdrive_path, all_char = FALSE){
    
    # Run listing safely, so if fails, does not stop the function
    safe_drive_ls <- purrr::safely(googledrive::drive_ls)
    drive_list <- safe_drive_ls(gdrive_path)
    
    # If there is an error, throw a warning
    if (!is.null(drive_list$error)) warning("The specified google drive inventory does not exist")
    
    # If there are no errors in the listing, download the files
    if (is.null(drive_list$error)){
        if (all_char == FALSE){
        drive_list$result %>% 
            transmute(file = name,
                      # Try to guess the col_types
                      sheet = map(id, ~googlesheets::gs_key(.x) %>% googlesheets::gs_read(1)))
        } else {
            drive_list$result %>% 
                transmute(file = name,
                          # All col_types are read as character
                          sheet = map(id, ~googlesheets::gs_key(.x) %>% googlesheets::gs_read(1, col_types = cols(.default = "c"))))
            
            }
    }
}


