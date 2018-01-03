# Create consensus tables
# iNPUT: articles: a dataset that contains the screened articles, and having a name<chr> column
# OUTPUT: A nested data frame containing the mutual records for all pairs of screning team members
# EXAMPLE: create_consensus(articles)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

create_consensus <- function(articles){
    stopifnot(has_name(articles, c("name","decision","reason", "title","abstract")))
    name_pairs <- 
        articles %>% 
        distinct(name) %>% 
        pull() %>% 
        combn(., 2) %>% 
        t() %>% 
        as_data_frame() %>% 
        set_names(c("name1", "name2")) %>% 
        mutate(name_pair = paste(name1, name2, sep = "_")) %>% 
        group_by(name_pair) %>% 
        nest()
    
    suppressMessages(
        name_pairs %>% 
        mutate(decision_table = map(data, 
                                    ~ articles %>% 
                                        drop_na(decision) %>%
                                        select(-position, -reason) %>%
                                        filter(name %in% (.x %>% c())) %>% 
                                        spread(name, decision, sep = "_") %>% 
                                        set_names(str_replace(names(.), "name", "decision")) %>% 
                                        drop_na(contains("decision"))),
               reason_table = map(data, 
                                  ~ articles %>% 
                                      drop_na(decision) %>%
                                      select(-position, -decision) %>%
                                      filter(name %in% (.x %>% c())) %>% 
                                      spread(name, reason, sep = "_") %>% 
                                      set_names(str_replace(names(.), "name", "reason")))) %>% 
        transmute(
            name_pair, 
            consensus_table = map2(decision_table, reason_table, 
                                   ~ left_join(.x, .y) %>% 
                                       select(contains("decision"), contains("reason"), title, abstract, everything())
            )
        )
    )
}


