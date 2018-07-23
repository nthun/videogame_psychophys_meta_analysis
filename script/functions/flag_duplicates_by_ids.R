# Flag duplicates by id(s)
# By using several databases, it is possible to have several duplicates of the same article with the same id. As there can be several ids, removing duplicates is not that straightforward.
# INPUT: df data frame with potential duplicates
#        keys a character vector of identifier variables in the data frame that 
# OUTPUT: A data frame without duplicate elements
# EXAMPLE: flag_duplicates_by_ids(df, c("doi","pmid","psyid","eid"))


flag_duplicates_by_ids <- function(df, keys){
    
    stopifnot(is.data.frame(df),
              is.character(keys),
              length(keys) > 0,
              all(rlang::has_name(df, keys)))
    
    # Remove whitespace and convert key variables to lowercase for the filtering of duplicates
    # These changes are not present in the duplicate removed dataframe
    dplyr::filter_at(df, vars(!!keys), 
                     dplyr::any_vars(stringr::str_squish(.) %>% 
                                       stringr::str_to_lower() %>% 
                                       duplicated(incomparables = NA))) %>% 
    # Keep ony the keys and duplicate info
    dplyr::transmute(!!!syms(keys),
                     duplicate_by_id = 1) %>% 
    # Join the duplicate info back to the original df, using all keys
    dplyr::left_join(df, ., by = keys)
}



