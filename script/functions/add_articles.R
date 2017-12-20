# Add new literature to the existing and work-in-progress files and return an updated df
# INPUT: old_article_df: a single df that contains all work-in-progress data, retrieved by get_screening_sheets()
#        new_article_df: a df that contains new articles, and has only one identifier
#        team_df: the team file that contains name<chr>, and effort<int> variables to be used when assigning the new articles
#        seed: random seed to use to make the assignment reproducible
# OUTPUT: a data frame containing nested data frames for each revier with updated data
# TODO: Add error handling
# EXAMPLE: add_articles(old_articles, psycinfo_df, team, 1)
library(tidyr)
library(dplyr)
source("script/functions/assign_articles.R")

add_articles <- function(old_article_df, new_article_df, team_df, seed = 1){
    # The work-in-progress files are in duplicate, so duplicates should be removed first
    old_single <- 
        old_article_df %>% 
        filter(!(duplicated(id, incomparables = NA) |
                     duplicated(title, incomparables = NA)))
    # Then remove the overlap between the old and new articles
    bind_rows(old_single, new_article_df) %>% 
        filter(!(duplicated(id, incomparables = NA) |
                     duplicated(title, incomparables = NA))) %>% 
        # Keep only the new articles after removing all duplicates
        anti_join(old_single, by = c("title","id")) %>% 
        # Remove position and name that came with the old df
        select(-one_of("position", "name")) %>% 
        # Assign new reviewers based on the team_df
        assign_articles(team_df, seed) %>% 
        # Add the old literature on the top, but first, re-add the reviewer column for nesting
        bind_rows(old_article_df %>% mutate(reviewer = name), .)
}
# Possible bug!
# THe old df contains the records in duplicate for a reason, so not all duplicates should be removed

