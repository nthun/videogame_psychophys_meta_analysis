# Assign articles to raters 
# INPUT: df: a data frame of articles
#        team: a data.frame of team members with name<chr>, and effort <dbl>:0-1 variables
#        seed: a random seed <int> for reproducibility
# OUTPUT: a data frame that contains the article info and 

library(tidyverse)
library(glue)
assign_articles <- function(df, team_df, seed){
    stopifnot(has_name(df, c("title", "abstract")),
              is.numeric(seed))
    
    set.seed(seed)
        df %>% 
        rowwise() %>%
        mutate(reviewer1 = sample(team_df$name, size = 1, prob = team_df$effort)) %>% 
        mutate(reviewer2 = sample(team_df$name[team_df$name != reviewer1], size = 1, prob = team_df$effort[team_df$name != reviewer1])) %>%
        gather(position, reviewer, reviewer1:reviewer2) %>% 
        mutate(decision = "",
               reason = "") %>% 
        select(decision, reason, title, abstract, everything()) %>% 
        group_by(reviewer) %>%
        mutate(name = reviewer) %>% 
        ungroup()
}

