# Merge records, remove duplicates
library(tidyverse)
library(openxlsx)


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

anderson2010 <- read_csv("screening_data/anderson2010.csv")
nagy2015 <- read_csv("screening_data/nagy2015.csv")    


# Merging and duplicate removal -------------------------------------------
# Removing duplicates based on title, doi, pmid
merged_records <-
    bind_rows(scopus, 
              pubmed, 
              proquest_dt, 
              anderson2010,
              nagy2015) %>% 
    mutate(title = title %>% str_to_title()) %>% 
    filter(!(
            duplicated(doi, incomparables = NA) |
            duplicated(pmid, incomparables = NA) |
            duplicated(title, incomparables = NA))
           ) %>%
    arrange(title)
    

# Create an identifier hierarchy and keep only the best --------------------
id_hierarchy <- tibble(identifier = c("doi","pmid","psyid","eid","pq_id","no_id"),
                       id_rank = c(1, 2, 3, 4, 5, 6))

clean_records <-
    merged_records %>% 
    gather(identifier, id, c(doi:eid, pq_id, no_id)) %>%
    drop_na(id) %>% 
    left_join(id_hierarchy, by = "identifier") %>% 
    group_by(title) %>%
    mutate(best_id = min(id_rank)) %>% 
    ungroup() %>%
    filter(id_rank == best_id) %>% 
    select(-id_rank, -best_id) %>% 
    select(identifier, id, source, everything())
