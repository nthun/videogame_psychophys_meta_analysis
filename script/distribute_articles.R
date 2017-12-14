# Prepare screening
library(tidyverse)
library(glue)

# Create team -------------------------------------------------------------
team <- tibble(name = c("Andreas", "Ali", "Lydia", "Tamas"),
               effort = c(.25,.25,.25,.25))


# Assign articles reproducibly --------------------------------------------
set.seed(1)
article_assign <-
    clean_records %>% 
    rowwise() %>%
    mutate(reviewer1 = sample(team$name, size = 1, prob = team$effort)) %>% 
    mutate(reviewer2 = sample(team$name[team$name != reviewer1], size = 1, prob = team$effort[team$name != reviewer1])) %>%
    gather(position, reviewer, reviewer1:reviewer2) %>% 
    mutate(decision = "") %>% 
    select(decision, title, abstract, everything()) %>% 
    group_by(reviewer) %>%
    mutate(name = reviewer) %>% 
    nest()

walk2(article_assign$reviewer, article_assign$data, ~write_csv(.y, glue("screening/{.x}_articles.csv", na = "")))


# TODO: Write a re-assign function for the records that are not yet rated

