#' Combine the raw data across telemetry flights
#'
#' @param df_list A list of data.frames to merge. The list will generally be the output of read.flight.data.
#' @param source_vec A vector of strings to specify the source of each data.frame in the list. The ordering of df_list and source_vec should coorespond (eg: to merge list(belly_df, wing_df), use source_vec=c("belly", "wing")).
#' @return Returns a dataframe containing the merged data. The data will be ordered by detection time.
#' @export

combine.data <- function(df_list, source_vec){
  r_df <- data.frame()
  for (i in 1:length(df_list)){
    df_list[[i]]$Source <- source_vec[i]
    r_df <- rbind(r_df, df_list[[i]])
  }
  r_df <- r_df[order(r_df$DateTime),]
  return(r_df)
}

