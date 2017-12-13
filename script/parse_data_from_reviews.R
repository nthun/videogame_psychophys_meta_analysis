# Parsing data from previous reviews
library(tidyverse)
library(googlesheets)

anderson_gid <-  "1351941637"
anderson_gs <- "anderson2010 article list"

anderson2010 <- 
    gs_title(anderson_gs) %>% 
    gs_read("Sheet1") %>% 
    mutate(source = "anderson2010",
           authors = str_replace_all(citation, pattern = "^(.*) \\(.*", "\\1") %>% 
                     str_replace_all("& ",""), # Remove &
           year = str_replace(citation, pattern = "^.*\\((\\d{4})\\).*", "\\1"),
           type = case_when(str_detect(citation %>% str_to_lower(), "dissertation|thesis") ~ "THES",
                            TRUE ~ NA_character_),
           title = str_replace(citation, pattern = "^.*\\)\\. (.*[\\?\\.]) .*, .*", "\\1"), 
           journal = str_replace(citation, pattern = "^.*\\)\\. .*[\\?\\.] (.*), \\d+,? .*", "\\1")) %>% 
    select(source, doi, pmid, title, journal, year, type, authors, abstract) %>% 
    mutate_if(is.character, trimws)


write_csv(anderson2010, "screening_data/anderson2010.csv")
