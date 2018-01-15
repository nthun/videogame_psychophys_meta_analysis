# Get consensus sheets
# INPUT: A google drive path where the consensus sheets are stored
# OUTPUT: An unnested tibble which contains the content of all consensus files, without the decisions and the reasons.
# EXAMPLE: get_consensus_sheets("Research/Video game psychophysiology meta/Literature search/Consensus meetings/")
# TODO: error handling, e.g. check if all the consensus files have the right format

library(googledrive)
library(dplyr)
library(tidyr)
library(purrr)

get_consensus_sheets <- function(gdrive_path){

    # Run listing safely, so if fails, does not stop the function
    safe_drive_ls <- safely(drive_ls)
    drive_list <- safe_drive_ls(gdrive_path)
    
    # If there is an error, throw a warning
    if (!is.null(drive_list$error)) warning("The specified google drive inventory does not exist")
    
    # If there are no errors in the listing, download the files
    if (is.null(drive_list$error)){
    
        drive_list$result %>% 
        transmute(file = name,
                  consensus_sheet = map(id, ~gs_key(.x) %>% 
                                            gs_read(1))) %>% 
        mutate(consensus_sheet = map(consensus_sheet, 
                                     ~ .x %>% 
                                         select(-starts_with("decision"), -starts_with("reason")))) %>%
        unnest(consensus_sheet)
    }
}

