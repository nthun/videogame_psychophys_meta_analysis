# Parsing data from previous reviews
library(tidyverse)
library(googlesheets)
library(base64enc)


# Anderson 2010 article data ----------------------------------------------
previous_meta <- "previous meta analysis"

anderson2010 <- 
    gs_title(previous_meta) %>% 
    gs_read("anderson2010") %>% 
    mutate(source = "anderson2010",
           authors = str_replace(citation, "^(.*) \\(\\d{4}\\).*", "\\1") %>% 
                     str_replace_all("& ",""), # Remove &
           year = str_replace(citation, pattern = "^.*\\((\\d{4})\\).*", "\\1") %>% as.integer(),
           type = case_when(str_detect(citation %>% str_to_lower(), "dissertation|thesis") ~ "THES",
                            TRUE ~ NA_character_),
           title = str_replace(citation, pattern = "^.*\\)\\. (.*[\\?\\.]) .*, .*", "\\1"), 
           journal = str_replace(citation, pattern = "^.*\\)\\. .*[\\?\\.] (.*), \\d+,? .*", "\\1"),
           no_id = if_else(is.na(doi) | is.na(pmid), paste0(source,"_",row_number()), NA_character_)
           ) %>% 
    select(source, doi, pmid, no_id, title, journal, year, type, authors, abstract) %>% 
    mutate_if(is.character, trimws)



write_csv(anderson2010, "screening_data/anderson2010.csv")

# Nagy 2016 data ----------------------------------------------------------
nagy2015 <- 
    gs_title(previous_meta) %>% 
    gs_read("nagy2015") %>% 
    mutate(source = "nagy2015",
           authors = str_replace(citation, pattern = "^(.*) \\(\\d{4}\\).*", "\\1") %>% 
                     str_replace_all("& ",""), # Remove &
           year = str_replace(citation, pattern = "^.*\\((\\d{4})\\).*", "\\1") %>% as.integer(),
           # type = case_when(str_detect(citation %>% str_to_lower(), "dissertation|thesis") ~ "THES", TRUE ~ NA_character_),
           title = str_match(citation, pattern = "^.*\\)\\. (.*[\\?\\.]) .*\\b.*")[,2], 
           journal = str_match(citation, pattern = ".*\\d{4}\\)\\. .*[\\?\\.] (\\D*), .*$")[,2],
           doi = str_match(citation, ".* doi:(.*)$")[,2],
           no_id = if_else(is.na(doi), paste0(source,"_",row_number()), NA_character_)
           ) %>% 
    select(source, doi, no_id, title, journal, year, authors, abstract) %>% 
    mutate_if(is.character, trimws)

write_csv(nagy2015, "screening_data/nagy2015.csv")    
    
    
    
