# Age can be written as an exact number (12.45), or open (18- or -13) or closed (18-35) interval.
# Hardcoded function to create age groups from subsample_age data
add_age_groups <- function(df){
    
df %>% 
    mutate( exact_age = str_extract(subsample_age, "^\\d+\\.*\\d*$"),
            min_age = str_extract(subsample_age, "\\d+(?=-)"),
            max_age = str_extract(subsample_age, "(?<=-)\\d+"),
            age_group = case_when(exact_age>=13 & exact_age < 18 |
                                     min_age >= 13 & max_age < 18 ~ "adolescent",
                                  exact_age>=18 | min_age >= 18  ~ "adult",
                                  exact_age < 13 | max_age < 13 ~ "child",
                                  TRUE ~ NA_character_))
    }