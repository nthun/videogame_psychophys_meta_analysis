# Verify data structure of imported sheets has the same variable names and same classes
# INPUT: A data frame with all data sources imported from google drive using the get_from_gdrive() function as nested tbls
# OUTPUT: A dataset that compares variable names and classes pairwisely
# EXAMPLE: verify_sheets(articles)

library(dplyr)

verify_sheets <- function(nested_df){
    df_info <-
        nested_df %>% 
        transmute( file,
                   vars_classes = map(sheet, ~summarise_all(.x, class) %>% 
                                          gather(var_name, var_class)))
    
    df_info %>% 
        expand(crossing(file1 = file, file2 = file)) %>% 
        filter(file1 != file2) %>% 
        left_join(df_info, by = c("file1" = "file")) %>% 
        left_join(df_info, by = c("file2" = "file"), suffix = c("1","2")) %>% 
        mutate(name_eq = map2_lgl(vars_classes1, vars_classes2,
                                  ~setequal(.x$var_name, .y$var_name)),
               class_eq = map2_lgl(vars_classes1, vars_classes2,
                                  ~setequal(.x$var_class, .y$var_class))
        )
}


