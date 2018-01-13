# Show decisions for all reviewers
# iNPUT: articles: a dataset containing the screened articles, and having a name <chr> column and a decision <chr> column
# OUTPUT: a tibble containing the decisions and the number of all screened articles
# EXAMPLE: show_screening_decisions(articles)

library(dplyr)
library(tibble)
library(tidyr)

show_screening_decisions <- function(articles)
{
    stopifnot(has_name(articles, c("name","decision")))
    
    articles %>% 
        filter(!is.na(decision)) %>% 
        mutate(decision = decision %>% as.integer()) %>% 
        count(name, decision) %>%
        group_by(name) %>% 
        mutate(all = sum(n)) %>% 
        spread(decision, n)
}