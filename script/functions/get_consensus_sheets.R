# Get consensus sheets
# INPUT: A google drive path where the consensus sheets are stored
# OUTPUT: An unnested tibble which contains the content of all consensus files, without the decisions and the reasons.
# EXAMPLE: get_consensus_sheets("Research/Video game psychophysiology meta/Literature search/Consensus meetings/")
# TODO: error handling, e.g. check if all the consensus files have the right format
# TODO: download files to a temporary directory if requested (?)

library(googledrive)
library(dplyr)
library(tidyr)
library(purrr)

get_consensus_sheets <- function(gdrive_path){
    
    drive_ls(gdrive_path) %>% 
    transmute(file = name,
              consensus_sheet = map(id, ~gs_key(.x) %>% 
                                        gs_read(1))) %>% 
    mutate(consensus_sheet = map(consensus_sheet, ~ .x %>% select(-starts_with("decision"), -starts_with("reason")))
    ) %>% 
    unnest(consensus_sheet)
}

