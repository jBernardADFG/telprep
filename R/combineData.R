#' Combine data between receivers and across telemetry flights
#'
#' @param df_list A list of data.frames to merge. The list will be the output of \code{\link{read_flight_data}}, \code{\link{channels_merge}}, or \code{\link{replace_date}}.
#' @param source_vec A vector of strings to specify the source of each data.frame in the list. The ordering of df_list and source_vec should coorespond (eg: to merge list(belly_df, wing_df), use source_vec=c("belly", "wing")).
#' @export
#' @examples
#' names(raw_data)
#' source_vec <- c(rep(c("belly","wing"), 10), rep(c("wing","belly"),2), c("belly","wing"))
#' source_vec
#' all_data <- combine_data(raw_data, source_vec)
#' head(all_data)

combine_data <- function(df_list, source_vec=NA){
  r_df <- data.frame()
  if(is.na(source_vec[1])){
    for (i in 1:length(df_list)){
      r_df <- rbind(r_df, df_list[[i]])
    }
  }else{
    for (i in 1:length(df_list)){
      df_list[[i]]$Source <- source_vec[i]
      r_df <- rbind(r_df, df_list[[i]])
    }
  }
  r_df <- r_df[order(r_df$DateTime),]
  r_df <- unique(r_df)
  attr(r_df, "crs") <- sp::CRS("+proj=utm +zone=5 +datum=WGS84")
  return(r_df)
}

