#' combine.data
#'
#' Combine the raw data between receivers and telemetry flights
#' @param df_list A list of data.frames to merge. The list will generally be the output of read.flight.data.
#' @param source_vec A vector of strings to specify the source of each data.frame in the list. The ordering of df_list and source_vec should coorespond (eg: to merge list(belly_df, wing_df), use source_vec=c("belly", "wing")).
#' @return Returns a dataframe containing the merged data. The data will be ordered by detection time.
#' @export

combine.data <- function(df_list, source_vec=NA){
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
  attr(r_df, "crs") <- CRS("+proj=utm +zone=5 +datum=WGS84")
  return(r_df)
}

