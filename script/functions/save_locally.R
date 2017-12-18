# Save assigned articles locally in a specified directory
# iNPUT: df: a dataset that contains the assigned articles, and having a reviewer<chr> column
# OUTPUT: No output, only side effect (saved files in the folder)
# EXAMPLE: save_locally(assigned_articles, "screening")
save_locally <- function(df, save_dir = NA_character_, overwrite = FALSE){
    # Checking predicaments
    stopifnot(has_name(df, "reviewer"),
              !is.na(save_dir),
              !(dir.exists(save_dir) & overwrite == FALSE))
    # Create a nested tibble 
    df_nested <- 
        df %>% 
        group_by(reviewer) %>% 
        nest()
    # Create the directory
    if (dir.exists(save_dir) & overwrite == TRUE) dir.create(save_dir)
    if (!dir.exists(save_dir)) dir.create(save_dir)
    # Save the screening filess
    walk2(df_nested$reviewer, df_nested$data, ~write_csv(.y, glue("{dir}/{.x}_articles.csv"), na = ""))
    glue("{nrow(df_nested)} files saved in {dir}")
}