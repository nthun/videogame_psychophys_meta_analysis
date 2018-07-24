# Flag duplicates by title

# Note that this function can also be used to find duplicates based on the abstract or any other unique text field. However, this might prove to be slow due to the string distance calculations.
# INPUT: df a data frame of articles
#        title the <chr> column in df  that has the title of studies
#        max_distance the maximum difference of titles to be flagged as duplicate
# OUTPUT: the original df, augmented with a "duplicate" column, that can have 1 or 0 values
# EXAMPLE: flag_duplicates_by_title(df, "title", max_distance = 3)
flag_duplicates_by_title <- function(df, title = "title", max_distance = 5L){

    # Function for removing diagonal values in a matrix. This is needed for piping
    stopifnot(is.data.frame(df),
              is.character(title),
              rlang::has_name(df, title),
              is.numeric(max_distance))

    df %>% 
        # Create all combinations of titles
        tidystringdist::tidy_comb_all(!!sym(title)) %>% 
        # Calculate string distances using OSA
        tidystringdist::tidy_stringdist(method = "osa") %>% 
        dplyr::select(title1 = 1, title2 = 2, distance = 3) %>% 
        # Keep only similar titles
        dplyr::filter(distance <= max_distance) %>%
        dplyr::transmute(!!title := title2,
                         duplicate_by_title = 1L) %>%
        # Make sure that each title is only present once, so no new duplicates are created
        distinct(!!rlang::sym(title), .keep_all = TRUE) %>% 
        # Join to original data frame
        dplyr::left_join(df, ., by = title) %>%
        # Fill NA with 0-s
        dplyr::mutate(duplicate_by_title = dplyr::if_else(is.na(duplicate_by_title),
                                                          0L,
                                                          duplicate_by_title)) %>%
        # Make duplicates more identifiable by arranging by title
        dplyr::arrange(!!rlang::sym(title))
}
