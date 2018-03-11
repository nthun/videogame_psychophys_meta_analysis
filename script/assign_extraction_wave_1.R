# Assign data extraction wave 1
library(tidyverse)
library(googlesheets)

source("script/functions/add_columns.R")
source("script/functions/save_locally.R")
source("script/functions/upload_to_gdrive.R")

articles <- 
    gs_key("1eP6hBLNj82jrlzH1QiVAOdmnaii2-JWaNINweefwi0k") %>% 
    gs_read(1)

team_df <- gs_key("14DNNS7BCDA18Q9EgT6TlkeSSIR6JZINlxJSyTxV94xg") %>% 
    gs_read(1)

articles <- 
    articles %>% 
    filter(fulltext == 1)

team_df


set.seed(180311)
temp <-
articles %>% 
    mutate(reviewer = base::sample(team_df$name, replace = TRUE, size = nrow(articles), prob = team_df$extraction_effort),
           name = reviewer) %>% 
    select(-approx_filename) %>% 
    add_columns(c("physiological_variables","subjctive_emotion_variables","game","has_violence"))

local_path <- "d:/Documents/GitHub/videogame_psychophys_meta_analysis/data_extaction/wave_1/"
gdrive_path <- "Research/Video game psychophysiology meta/Data extraction/Wave 1/"

save_locally(temp, local_path, postfix = "extract_1",overwrite = TRUE)
upload_to_gdrive(local_path = local_path, gdrive_path = gdrive_path)





