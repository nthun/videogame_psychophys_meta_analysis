# Select the best id based on a hierarchy, and keep only that for the article
# INPUT: A data frame that has identifiers 
# OUTPUT: a data frame without individual id columns, and with new identifier and id columns that contain the best available identifier and the id, respectively
# EXAMPLE: make_id(df)

library(tidyr)
library(dplyr)
id_hierarchy <- tibble(identifier = c("doi","pmid","psyid","eid","pq_id","no_id"),
                       id_rank = c(1, 2, 3, 4, 5, 6))

make_id <- function(df){
    # Stop if there are no valid identifiers in the data frame
    stopifnot(names(df) %>% 
                  intersect(id_hierarchy$identifier) %>% 
                  length() > 0) 
    
    df %>% 
        gather(identifier, id, intersect(names(.), id_hierarchy$identifier)) %>%
        drop_na(id) %>% 
        left_join(id_hierarchy, by = "identifier") %>% 
        group_by(title) %>%
        mutate(best_id = min(id_rank)) %>% 
        ungroup() %>%
        filter(id_rank == best_id) %>% 
        select(-id_rank, -best_id) %>% 
        select(identifier, id, source, everything())
}