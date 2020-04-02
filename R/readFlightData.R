#' read.flight.data
#'
#' Read telemetry flight txt files into R
#'
#' @param folder_path The path of the folder where the txt files are located.
#' @param remove_999 Use remove_999=T to remove all 999 tag IDs. Use remove_999=F otherwise.
#' @param mort_sig When Tag IDs > 100 signal mortality, use mort_sig=T. Use mort_sig=F otherwise.
#' @return Returns a list of data.frames containing the raw data from each file in the folder. To see how the data is organized use names().
#' @export

read.flight.data <- function(folder_path, remove_999=T, mort_sig=T){
  flight_df <- list()
  files <- list.files(folder_path)
  for(i in 1:length(files)){
    file <- files[i]
    file_path <- paste(folder_path, file, sep="")
    flight_df[[i]] <- read.flight.file(file_path, remove_999, mort_sig)
  }
  names(flight_df) <- list.files(folder_path)
  return(flight_df)
}
