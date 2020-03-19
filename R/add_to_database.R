# -------------------------------------------------------------------------
# Functions to add the best locations to the main database
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
#' Add a "best locations" or "best detection" data.frame to a data.frame version of the database
#'
#' @param main_df A data.frame version of the main database (eg database="S:/Jordy/data/database_name.csv). The database file will be overwritten to include the new data.
#' @param add_df The data.frame to add to the database. Note: names(main_df) must be equal to names(add_df).
#' @examples
#' input_file <- "S:/Jordy/telprep/data/laura/Tanana Burbot flight Dec 2019 - BELLY.txt"
#' main_df <- prep.flight.df(input_file)$best_locs
#' input_file <- "S:/Jordy/telprep/data/laura/Tanana burbot flight October 2019 - BELLY.txt"
#' add_df <- prep.flight.df(input_file, mort_sig=T)$best_locs
#' add.to.database.df(main_df, add_df)
#' @export
add.to.database.df <- function(main_df, add_df){
  if (sum(names(main_df)!=names(add_df))!=0){
    stop("The column headers of the database and the file are different. The process has been terminated with no changes to the database.")
  }
  n_dups <- nrow(merge(main_df, add_df))
  if (n_dups!=0){
    stop(paste("The file and the database contain", n_dups, "common detections. The process has been terminated with no changes to the database."))
  }
  new_main_df <- rbind(main_df, add_df)
  dt <- paste(as.character(new_main_df$Date), as.character(new_main_df$Time))
  date_string <- paste(as.character(new_main_df$Date),as.character(new_main_df$Time))
  date_posixct <- as.POSIXct(date_string, format="%m/%d/%y %H:%M:%S")
  new_main_df <- new_main_df[order(date_posixct),]
  return(new_main_df)
}

# -------------------------------------------------------------------------
#' Add a "best locations" or "best detection" file to a database
#'
#' @param database String specifying the file path of the main database (eg database="S:/Jordy/data/database_name.csv). The database file will be overwritten to include the new data.
#' @param file_name The file path of the "best location" or "best detection" file. The column names of the file must coorespond to the column headings of the database.
#' @export

add.to.database.file <- function(database, filename){
  database <- "S:/Jordy/telprep/data/andy/all_detects.csv"
  db <- read.csv(database)
  filename <- "S:/Jordy/telprep/data/andy/best_detects.csv"
  file <- read.csv(filename)
  if (sum(names(db)!=names(file))!=0){
    stop("The column headers of the database and the file are different. The process has been terminated with no changes to the database.")
  }
  n_dups <- nrow(merge(db, file))
  if (n_dups!=0){
    stop(paste("The file and the database contain", n_dups, "common detections. The process has been terminated with no changes to the database."))
  }
  new_db <- rbind(db, file)
  dt <- paste(as.character(new_db$Date), as.character(new_db$Time))
  date_string <- paste(as.character(new_db$Date),as.character(new_db$Time))
  date_posixct <- as.POSIXct(date_string, format="%m/%d/%y %H:%M:%S")
  new_db <- new_db[order(date_posixct),]
  write.csv(new_db, file=database, row.names=F)
}
