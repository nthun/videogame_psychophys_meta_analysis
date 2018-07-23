# Assign articles to raters reproducibly in a team that can have members with different workload
# INPUT: df: a data frame of articles
#        team: a data.frame of team members with name<chr>, and effort <dbl>:0-1 variables
#        seed: a random seed <int> for reproducibility
# OUTPUT: a data frame that contains the article info and the assigned reviewers
# EXAMPLE: assign_articles(merged_articles, team, 1)
# TODO: Feature: Possibility to assign an article to more than two reviewers
library(tidyr)
library(dplyr)
library(glue)
library(tibble)

assign_articles <- function(articles, team_df, seed = 1){
    stopifnot(has_name(articles, c("title", "abstract")),
              has_name(team_df, c("name","effort")),
              is.numeric(seed),
              sum(team_df$effort) == 1)

    # Make distribution reproducible
    set.seed(seed)
    articles %>%
        rowwise() %>%
        # Assign two different reviewers to the article
        mutate(reviewer1 = sample(team_df$name, size = 1, prob = team_df$effort)) %>%
        mutate(reviewer2 = sample(team_df$name[team_df$name != reviewer1], size = 1, prob = team_df$effort[team_df$name != reviewer1])) %>%
        gather(position, reviewer, reviewer1:reviewer2) %>%
        group_by(reviewer) %>%
        # Duplicate the reviewer variable, to keep name in df even after nesting
        mutate(name = reviewer) %>%
        ungroup()
}