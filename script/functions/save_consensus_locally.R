# Save consensus tavles articles locally in a specified directory
# iNPUT: tables: a dataset that contains the assigned articles, and having a reviewer<chr> column
# OUTPUT: No output, only side effect (saved files in the folder)
# EXAMPLE: save_locally(consensus_tables, "consensus_tables")
# TODO: Change to work on unnested tables?
# TODO: Add automatic correction of the reason column?
library(dplyr)
library(tidyr)
library(tibble)

save_consensus_locally <- function(consensus_tables, dir = NA_character_, overwrite = FALSE){
    # Checking predicaments
    stopifnot(has_name(consensus_tables, c("name_pair", "consensus_table")),
              !is.na(dir),
              !(dir.exists(dir) & overwrite == FALSE))
   # Create the directory
    if (dir.exists(dir) & overwrite == TRUE) unlink(dir, recursive = TRUE)
    dir.create(dir)
    # Save the screening filess
    purrr::walk2(consensus_tables$name_pair, consensus_tables$consensus_table, ~readr::write_excel_csv(.y, glue::glue("{dir}/{.x}_consensus.csv"), na = ""))
    glue::glue("{nrow(consensus_tables)} files saved in {dir}")
}
