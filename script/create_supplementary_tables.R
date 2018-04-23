# Get extraction wave 1 tables and summarise them

library(tidyverse)
source("script/functions/get_from_gdrive.R")
source("script/functions/add_columns.R")
source("script/functions/upload_to_gdrive.R")

wave1 <- 
    get_from_gdrive("Research/Video game psychophysiology meta/Data extraction/Wave 1/") %>% 
    mutate(citation = str_glue("{authors}({year}).{title} {id}"))

# Excluded studies
excluded <-
    wave1 %>% 
    drop_na(exclude_reason)

# Included studies
included <-
    wave1 %>% 
    mutate_if(is.character, str_squish) %>% 
    filter(is.na(exclude_reason))

# Create game table

local_path <- "d:/Documents/GitHub/videogame_psychophys_meta_analysis/data_extaction/supplementary_data/"
gdrive_path <- "Research/Video game psychophysiology meta/Data extraction/Supplementary data TEMP/"

game_table <-
    included %>% 
    transmute( games = str_split(game, ","),
             citation) %>% 
    unnest() %>% 
    mutate(games = str_squish(games) %>% str_to_title()) %>% 
    group_by(games) %>% 
    transmute(articles = paste(citation, collapse = ";\n")) %>% 
    add_columns(c("aliases","publisher","year","genre","esrb_rating","esrb_descriptors","adaptation","exergame","vr_game","handheld_game","game_imprecise"),before = TRUE) %>% 
    arrange(games) %>% 
    distinct(games, .keep_all = TRUE) %>% 
    select(game = games, everything())
    
# DO NOT RUN THIS!    
# upload_to_gdrive(local_path = local_path, gdrive_path = gdrive_path)
    
    

# Outcome measures table
# Subjective
included %>% 
    mutate(subjctive_emotion_variables = subjctive_emotion_variables %>% str_to_lower() %>% str_squish()) %>% 
    separate_rows(subjctive_emotion_variables, sep = ", ") %>% 
    count(subjctive_emotion_variables, sort = TRUE) %>% 
    drop_na(subjctive_emotion_variables) %>% 
    print(n = 200)

# Physiological
included %>% 
    mutate(physiological_variables = physiological_variables %>% str_to_lower() %>% str_squish()) %>% 
    separate_rows(physiological_variables, sep = ", ") %>% 
    mutate(physiological_variables = case_when(
        physiological_variables %in% c("heart rate", "pulse rate") ~ "heart rate",
                                        TRUE ~ physiological_variables)) %>% 
    count(physiological_variables, sort = TRUE) %>% 
    drop_na(physiological_variables) %>% 
    print(n = 200)

included %>% 
    unite(outcome, physiological_variables, subjctive_emotion_variables, sep = ", ") %>% 
    mutate(outcome = outcome %>% str_to_lower() %>% str_squish()) %>% 
    separate_rows(outcome, sep = ", ") %>% 
    filter(outcome != "na") %>% 
    count(outcome, sort = TRUE) %>% 
    # filter(n >= 3) %>% 
    print(n = 200)
        
# Aggregate outcomes
included %>% 
    mutate(subjctive_emotion_variables = subjctive_emotion_variables %>% str_to_lower() %>% str_squish()) %>% 
    separate_rows(subjctive_emotion_variables, sep = ", ") %>% 
    drop_na(subjctive_emotion_variables) %>% 
    mutate(subjective = case_when(
        subjctive_emotion_variables %in% c("positive affect", "positive mood") ~ "positive affect",
        subjctive_emotion_variables %in% c("negative affect", "negative mood") ~ "negative affect",
        subjctive_emotion_variables %in% c("arousal", "excitement") ~ "arousal",
        subjctive_emotion_variables %in% c("valence") ~ "valence",
        subjctive_emotion_variables %in% c("enjoyment", "pleasure", "joy", "happiness") ~ "enjoyment",
        subjctive_emotion_variables %in% c("anger", "aggression") ~ "anger",
        subjctive_emotion_variables %in% c("anxiety", "stress", "tenseness", "tension", "upset") ~ "stress",
        TRUE ~ subjctive_emotion_variables)) %>% 
    drop_na(subjective) %>% 
    count(subjective, sort = TRUE) %>% 
    print(n = 100)


