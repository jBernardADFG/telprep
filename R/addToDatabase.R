#' Insert something
#'
#' @param database The filepath of the database
#' @param add_df The data.frame to add to the database
#' @return Insert something
#' @export

add.to.database <- function(database, add_df){
  main_df <- read.csv(database)
  if (sum(names(main_df) != names(add_df)) != 0) {
    stop("The column headers of the database and the file are different. The process has been terminated with no changes to the database.")
  }
  n_dups <- nrow(merge(main_df, add_df))
  if (n_dups != 0) {
    stop(paste("The file and the database contain",
               n_dups, "common detections. The process has been terminated with no changes to the database."))
  }
  new_main_df <- rbind(main_df, add_df)
  dt <- paste(as.character(new_main_df$Date), as.character(new_main_df$Time))
  date_string <- paste(as.character(new_main_df$Date), as.character(new_main_df$Time))
  date_posixct <- as.POSIXct(date_string, format = "%m/%d/%y %H:%M:%S")
  new_main_df <- new_main_df[order(date_posixct), ]
  return(new_main_df)
}
