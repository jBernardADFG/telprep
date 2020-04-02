#' add.to.data.database
#'
#' Add the contents of a file to a database
#' @param database The filepath of the database
#' @param add_df The data.frame to add to the database
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
  write.csv(new_main_df, )
  help("write.csv", file=database)
  print("The contents of the file have been added to the database")
}
