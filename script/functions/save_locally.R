# Save assigned articles locally in a specified directory
# iNPUT: df: a dataset that contains the assigned articles, and having a reviewer<chr> column
# OUTPUT: No output, only side effect (saved files in the folder)
# EXAMPLE: save_locally(assigned_articles, "screening")
library(dplyr)
library(tidyr)
library(tibble)

save_locally <- function(df, dir = NA_character_, overwrite = FALSE){
    # Checking predicaments
    stopifnot(has_name(df, "reviewer"),
              !is.na(dir),
              !(dir.exists(dir) & overwrite == FALSE))
    # Create a nested tibble 
    df_nested <- 
        df %>% 
        group_by(reviewer) %>% 
        nest()
    # Create the directory
    if (dir.exists(dir) & overwrite == TRUE) unlink(dir, recursive = TRUE)
    dir.create(dir)
    # Save the screening filess
    purrr::walk2(df_nested$reviewer, df_nested$data, ~readr::write_excel_csv(.y, glue::glue("{dir}/{.x}_articles.csv"), na = ""))
    glue::glue("{nrow(df_nested)} files saved in {dir}")
}