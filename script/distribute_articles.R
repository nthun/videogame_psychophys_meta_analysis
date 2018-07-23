# Prepare screening
library(tidyverse)
library(googlesheets)

source("script/functions/assign_articles.R")
source("script/functions/save_locally.R")
source("script/functions/upload_to_gdrive.R")

# Create team -------------------------------------------------------------
# Download the team info sheet
team_df <- gs_key("14DNNS7BCDA18Q9EgT6TlkeSSIR6JZINlxJSyTxV94xg") %>% 
    gs_read(1) %>% 
    mutate(effort = screening_effort)

# article_assign <-
merged_records %>%
    assign_articles(team_df, seed = 1) %>%
    add_columns(c("decision", "reason")) %>%
    save_locally(
        local_path = "temp/screening/",
        nesting = "reviewer",
        postfix = "screening",
        overwrite = TRUE
    )

upload_to_gdrive(local_path = "temp/screening/", gdrive_path = , overwrite = TRUE)

# Save articles 
dir.create("temp_screening")
walk2(article_assign$reviewer, article_assign$data, ~write_csv(.y, str_glue("temp_screening/{.x}_articles.csv", na = "")))


# TODO: Write a re-assign function for the records that are not yet rated

library(googlesheets)

tempdir <- "temp_gs"
gs_key(i) %>% 
    gs_download(from = ., ws = 1, to = paste(tempdir, i, sep = "/"), overwrite = TRUE)



old_sheets <- df



