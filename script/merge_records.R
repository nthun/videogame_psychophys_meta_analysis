# Merge records, remove duplicates
library(tidyverse)
library(openxlsx)

source("script/functions/make_id.R")
setwd("d:/Documents/GitHub/videogame_psychophys_meta_analysis/")

# Read all data sources ---------------------------------------------------
scopus <- read_csv("screening_data/scopus_raw.csv") %>% 
    select(source = Source,
           doi = DOI,
           pmid = `PubMed ID`,
           eid = EID,
           title = Title,
           journal = `Source title`,
           authors = Authors,
           year = Year,
           abstract = Abstract,
           # language = `Language of Original Document`,
           type = `Document Type`
           )

pubmed <- read.xlsx("screening_data/pubmed_raw.xlsx", 1) %>% 
            as_tibble() %>% 
            select(source,
                   doi,
                   pmid,
                   title,
                   journal,
                   year,
                   authors,
                   abstract)


proquest_dt <- read_csv("screening_data/proquest_df.csv") %>% 
               select(source,
                      pq_id,
                      title,
                      type,
                      level,
                      year,
                      authors = author,
                      abstract
                      )


psycinfo_dt <- 
    bind_rows(
        read_csv("screening_data/psycinfo_journals_raw.csv"),
        read_csv("screening_data/psycinfo_books_raw.csv")
    ) %>% 
    select(source,
           psyid,
           doi,
           pmid,
           title,
           journal,
           year,
           authors,
           abstract
    )
    
# write_csv(psycinfo_dt, "screening_data/psychinfo_all.csv")


anderson2010 <- read_csv("screening_data/anderson2010.csv")
nagy2015 <- read_csv("screening_data/nagy2015.csv")    

# Merging and duplicate removal -------------------------------------------
# Removing duplicates based on title, doi, pmid
# clean_records <-
temp <- 
    bind_rows(scopus, 
              pubmed, 
              proquest_dt, 
              # psycinfo_dt,
              anderson2010,
              nagy2015) %>% 
    mutate(title = title %>% str_to_title()) %>% 
    filter(!(
            duplicated(doi, incomparables = NA) |
            duplicated(pmid, incomparables = NA) |
            duplicated(title, incomparables = NA))
           ) %>%
    # Select the best identifier and keep only that
    make_id(.,
            identifier = c("doi","pmid","psyid","eid","pq_id","no_id")) %>% 
    arrange(title)

temp %>% count(identifier)
    

