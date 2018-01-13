# Show reasons for excluded studies for all reviewers
# iNPUT: articles: a dataset containing the screened articles, and having a name <chr> column and a reason <chr> column, correct_terms <chr>: a vector containing the correct terms.
# OUTPUT: a tibble containing the corrected reasons and the number of all articles with a reason
# EXAMPLE: show_screening_reasons(articles, read_lines("vg-meta-reasons.txt"))

library(dplyr)
library(tibble)
library(tidyr)
library(readr)

show_screening_reasons <- function(articles, correct_terms)
{
    stopifnot(has_name(articles, c("name","reason")),
              is.character(correct_terms))
    
    articles %>% 
        filter(!is.na(reason)) %>% 
        mutate(corrected_reason = correct_categories(reason, correct_terms)) %>% 
        count(name, corrected_reason) %>% 
        group_by(name) %>% 
        mutate(all = sum(n)) %>% 
        ungroup() %>% 
        spread(corrected_reason, n)
}