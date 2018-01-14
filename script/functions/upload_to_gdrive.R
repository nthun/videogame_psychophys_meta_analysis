# Upload all .csv-s as google sheets from a local folder to a google drive folder
# INPUT: local_path <chr>: a full path that contains .csv-s to upload to gdrive
#        gdrive_path <chr>: a full gdrive path, preferably with / at the end
# OUTPUT: no output, this function exerts side-effect
# EXAMPLE: upload_to_gdrive("D:/Documents/GitHub/videogame_psychophys_meta_analysis/temp_cons/", "Research/Video game psychophysiology meta/Literature search/consensus_meetings/", overwrite = TRUE)

library(dplyr)
library(googledrive)
library(purrr)

upload_to_gdrive <- function(local_path, gdrive_path, overwrite = FALSE){
    stopifnot(dir.exists(local_path),
              is.logical(overwrite))

    # Run listing safely, so if fails, does not stop the function
    safe_drive_ls <- safely(drive_ls)
    drive_list <- safe_drive_ls(gdrive_path)
    
    # If folder does not exist, create it
    if (!is.null(drive_list$error)) drive_mkdir(gdrive_path)
    
    # If the gdrive folder exists, and contains files, and can overwrite, delete all content
    if ((nrow(drive_list$result) > 0) & overwrite == TRUE) {
        drive_trash(gdrive_path)
        drive_mkdir(gdrive_path)
    }
    
    # Now that the folder is clean, upload the files
    # Beware that if overwrite = FALSE, and the folder is not empty, new files may have the same name as the old ones
        walk(
            list.files(local_path, pattern = ".csv", full.names = TRUE),
            ~ drive_upload(
                media = .x,
                path = gdrive_path,
                type = "spreadsheet"
            )
        )
}
