# Prepare wave 2
library(tidyverse)
library(googlesheets)

source("script/functions/add_columns.R")
source("script/functions/save_locally.R")
source("script/functions/upload_to_gdrive.R")
source("script/functions/get_from_gdrive.R")

local_path <- "d:/Documents/GitHub/videogame_psychophys_meta_analysis/data_extaction/wave_2/"
wave1_path <- "Research/Video game psychophysiology meta/Data extraction/Wave 1/"
wave2_path <- "Research/Video game psychophysiology meta/Data extraction/Wave 2 temp/"

variables_to_add <-
    c(
        "context_place",
        "context_timing",
        "context_length",
        "context_mode",
        "context_opponent",
        "context_competititve",
        "subsample_id",
        "subsample_age",
        "subsample_female%",
        "subsample_non-caucasian%",
        "subsample_overweigh%",
        "measurement_timing",
        "measurement_sampling",
        "measurement_scale",
        "measurement_n",
        "measurement_sd",
        "measurement_precision",
        "measurement_corr",
        "measurement_change_stat_type",
        "measurement_change_stat_value",
        "study_design",
        "study_quality"
    )

team_df <- gs_key("14DNNS7BCDA18Q9EgT6TlkeSSIR6JZINlxJSyTxV94xg") %>% 
    gs_read(1)

# Get game df, aggregate aliases, and add the correct names. This can be merged to articles
game_df <- gs_key("1JVUPh1YxAnCmF2LWhTo9Qb7thDO_WsSaq3PufYjw_90") %>% 
    gs_read(1) %>% 
    # If game has a corrected title, use that instead
    mutate(game_corrected_title = if_else(is.na(game_corrected_title), 
                                          game_title, 
                                          game_corrected_title)) %>% 
    # Add the aliases to the possible game title
    unite(game, game_title, aliases, sep = ", ") %>% 
    mutate(game = str_replace_all(game, ", NA|NA, ", "")) %>% 
    separate_rows(game, sep = ", ") %>% 
    # Correct the aliases to actual game title
    select(game, game_corrected_title)

# Get an clean articles
articles <- 
    get_from_gdrive(wave1_path) %>% 
    # Remove articles that turne out to be uneligible
    filter(is.na(exclude_reason)) %>% 
    # Remove articles with unidentifiable games
    filter(game != "unknown (request details)") %>% 
    # Removing unnecessary whitespaces and making all output measures lowercase
    mutate_if(is.character, str_squish) %>%
    mutate_at(vars(physiological_variables, subjctive_emotion_variables), str_to_lower) %>% 
    # Remove variables that are not necessary anymore
    select(-file, -fulltext, -abstract) %>% 
    # Replacing a reviewer as he is out of the project
    mutate(name = if_else(name == "Ali", "Andreas", name))

# Reassign articles to new coders
set.seed(1)
# additional <- 
#     articles %>% 
#     filter(is.na(name)) %>% 
#     mutate(name = base::sample(team_df$name, replace = TRUE, size = n(), prob = team_df$extraction_effort))

if (!dir.exists(local_path)) dir.create(local_path)

# Create Wave 2 templates
# temp <- 
articles %>% 
    unite(col = measurement_outcome, 
        physiological_variables,
        subjctive_emotion_variables,
        sep = ", "
    ) %>%
    mutate(measurement_outcome = str_replace_all(measurement_outcome, ", NA|NA, ", "")) %>% 
    separate_rows(measurement_outcome, sep = ", ") %>%
    separate_rows(game, sep = ", ") %>%
    fuzzyjoin::stringdist_left_join(game_df, by = "game") %>% 
    select(-game.x, -game.y, game = game_corrected_title) %>% 
    mutate(`1` = NA_character_, `2` = NA_character_) %>%
    gather(measurement_point, measurement_mean, `1`:`2`) %>%
    arrange(id, game, measurement_outcome) %>%
    add_columns(variables_to_add) %>%
    select(
        exclude_reason,
        id,
        identifier,
        authors,
        year,
        title,
        journal,
        source,
        starts_with("context"),
        starts_with("subsample"),
        measurement_game = game,
        measurement_outcome,
        measurement_point,
        measurement_mean,
        measurement_sd,
        starts_with("measurement"),
        starts_with("study"),
        name
    ) %>%
    mutate(reviewer = name) %>%
    # drop_na(measurement_game) %>% 
    save_locally(local_path, postfix = "extract_2", overwrite = TRUE)

# Upload it to gdrive (existing files will be overwritten!!!!)
upload_to_gdrive(local_path = local_path, gdrive_path = wave2_path, overwrite = TRUE)







