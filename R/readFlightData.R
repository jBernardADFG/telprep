#' Read raw txt files into R
#'
#' @param folder_path The path of the folder where the txt files are located.
#' @param crs_in The txt file CRS proj4string.
#' @param remove_999 Use remove_999=T to remove all 999 tag IDs. Use remove_999=F otherwise.
#' @param mort_sig When Tag IDs > 100 signal mortality, use mort_sig=T. Use mort_sig=F otherwise.
#' @return Returns a list of data.frames containing the raw data from each file in the folder. To see how the data is organized use names(output). Note: the coordinates are converted to a UTM projection.
#' @export

read_flight_data <- function(folder_path, crs_in="+proj=longlat +datum=WGS84 +no_defs", remove_999=T, mort_sig=T){
  flight_df <- list()
  files <- list.files(folder_path)
  for(i in 1:length(files)){
    file <- files[i]
    file_path <- paste(folder_path, file, sep="")
    flight_df[[i]] <- read_flight_file(file_path, remove_999, mort_sig)
  }
  for(i in 1:length(flight_df)){
    dat <- flight_df[[i]]
    coords <- data.frame(
      lon = dat$Longitude,
      lat = dat$Latitude
    )
    s_pts <- suppressWarnings(sp::SpatialPoints(coords, proj4string = sp::CRS(crs_in)))
    crs_out <- sp::CRS("+proj=utm +zone=5 +datum=WGS84")
    s_pts <- sp::spTransform(s_pts, crs_out)
    coords <- s_pts@coords
    flight_df[[i]]$Longitude <- coords[,1]
    flight_df[[i]]$Latitude <- coords[,2]
    names(flight_df[[i]])[names(flight_df[[i]])=="Latitude"] <- "Y"
    names(flight_df[[i]])[names(flight_df[[i]])=="Longitude"] <- "X"
  }
  names(flight_df) <- list.files(folder_path)
  attr(flight_df, "crs") <- sp::CRS("+proj=utm +zone=5 +datum=WGS84")
  return(flight_df)
}





