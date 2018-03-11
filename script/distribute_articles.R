# Prepare screening
library(tidyverse)
library(googlesheets)
library(glue)

# Create team -------------------------------------------------------------
# Download the team info sheet
team_df <- gs_key("14DNNS7BCDA18Q9EgT6TlkeSSIR6JZINlxJSyTxV94xg") %>% 
    gs_read(1)

article_assign <-
    merged_records %>% 
    assign_articles(., team_df, 1) %>% 
    group_by(reviewer) %>% 
    nest()

# Save articles 
dir.create("temp_screening")
walk2(article_assign$reviewer, article_assign$data, ~write_csv(.y, glue("temp_screening/{.x}_articles.csv", na = "")))


# TODO: Write a re-assign function for the records that are not yet rated

library(googlesheets)

tempdir <- "temp_gs"
gs_key(i) %>% 
    gs_download(from = ., ws = 1, to = paste(tempdir, i, sep = "/"), overwrite = TRUE)



old_sheets <- df



