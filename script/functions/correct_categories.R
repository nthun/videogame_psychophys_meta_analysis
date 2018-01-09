# Correct reasons for pre-defined reason names. This is a wrapper for stringdist, where certain parameters are preset, and can be used easily in a tidyverse pipe. Using cosine matching to disregard word order.
# INPUT: to_be_corrected <str>: vector containing the strings to be corrected, correct_terms <chr>: a vector containing the correct terms that should.
# OUTPUT: a corrected string vector that can only contain the correct terms
# EXAMPLE: articles %>% mutate(correct_reason = correct_categories(reason, read_lines("vg-meta-reasons.txt")))

library(stringdist)
library(dplyr)
library(stringr)

correct_categories <- function(to_be_corrected, correct_terms) {
    stopifnot(is.character(to_be_corrected),
              is.character(correct_terms))
    
    correct_terms[amatch(
                    to_be_corrected %>% 
                        str_to_lower() %>% 
                        str_replace("\\?", NA_character_),
                    correct_terms,
                    maxDist = 2,
                    method = "cosine"
    )]
}