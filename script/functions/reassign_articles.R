# Reassign articles using updated team file. Team members can be added or removed, and effort can be adjusted. All unscreened articles will be reshuffled. Note: screened articles of removed team members will also remain in the df (under the name of the removed team member). Note2: As a side effect, the articles that were screened will get to the beginning of the list for the other reviewer(s)
# INPUT: articles: data frame of articles, with at least id<chr>, name<chr>, title<chr>, decision<chr> columns
# OUTPUT: a data frame with updated assignments
# EXAMPLE: reassign_articles(old_sheets, new_team_df, 1) 
library(dplyr)
library(tidyr)
source("script/functions/assign_articles.R")

reassign_articles <- function(screening_df, new_team_df, seed = 1){
    stopifnot(has_name(screening_df, c("id","name","title","decision", "position")),
              has_name(new_team_df, c("name","effort")),
              is.numeric(seed),
              sum(new_team_df$effort) == 1)
    
    # Get screened articles 
    screened <-
        screening_df %>% 
        drop_na(decision) %>% 
        mutate(reviewer = name)
    
    # Reassign the undecided articles
    reassigned <-
        screening_df %>% 
        # Get the undecided articles
        filter(is.na(decision)) %>% 
        select(-name, -position) %>%
        # Keep only one record for the article
        filter(!duplicated(id)) %>% 
        # Reassign the articles
        assign_articles(new_team_df, seed = seed)
    
    # Remove the unnecessary reviewers that were reassigned to an article with decision
    reassigned_2rev <-
        reassigned %>% 
        filter(id %in% screened$id) %>% 
        anti_join(screened, by = c("id","position")) %>% 
        bind_rows(filter(reassigned, !(id %in% screened$id)), .)

    # Return the articles, with screened articles first
    bind_rows(screened, reassigned_2rev) %>% 
        arrange(reviewer, decision, title)
}