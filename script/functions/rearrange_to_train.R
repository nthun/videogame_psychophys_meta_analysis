# Rearrange articles to train team members for screening. The team members in the role of "trainer" will get their articles reordered, so they can also screen the articles. This can be used in the beginning of a screening process to give an early feedback on the work of the trainees.
# INPUT: articles: a data frame with articles, containing id<chr>, name<chr>, title<chr>, and decision<chr> columns
# OUTPUT: The article df, but for the trainer the the articles are rearranged
# EXAMPLE: rearrange_to_train(old_sheets, team)
# TODO: currently it can only accomodate one trainer
library(dplyr)
library(tidyr)

rearrange_to_train <- function(articles, team_df){
    stopifnot(has_name(team_df, c("name","role")),
              has_name(articles, c("id","name","title","decision")))
    
    # Get articles that the trainees already screened
    trainee_decided <-
        articles %>% 
        filter(role == "trainee") %>% 
        drop_na(decision)
    
    # Rearrange the trainer articles to get the already screened articles first 
    trainer_check <-
        articles %>% 
        filter(role == "trainer") %>% 
        # Create a new variable for the articles already screened by the trainees (and the trainer also got it for screening)
        mutate(check_first = id %in% unique(trainee_decided$id)) %>% 
        arrange(decision, -check_first, title) %>% 
        select(-check_first)
    
    # Return the original articles, but for trainers, the articles that were screened by trainees will get on top (after the records they already screened)
    articles %>% 
        filter(role != "trainer") %>% 
        bind_rows(trainer_check)
}



