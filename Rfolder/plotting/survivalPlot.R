#' Plots the number of expired fish (as determined by the viterbi path) by flight period.
#'
#' @param viterbi output of \code{\link{viterbi}} or \code{\link{hmm_survival}}.
#' @export

survival.plot <- function(viterbi){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  df <- data.frame(rep(NA, length(unique(viterbi$FlightNum))), rep(NA, length(unique(viterbi$FlightNum))), rep(NA, length(unique(viterbi$FlightNum))))
  names(df) <- c("date", "n_alive", "n_dead")
  df[,1]<-rep(viterbi$DateTime[1], length(unique(viterbi$FlightNum)))
  for (i in unique(viterbi$FlightNum)){
    fish <- viterbi[viterbi$FlightNum==i,]
    df[i,1] <- median(fish$DateTime)
    df[i,2] <- sum(fish$Viterbi==1)
    df[i,3] <- sum(fish$Viterbi==2)
  }
  ggplot(data=df, aes(x=date, y=n_dead))+
    geom_line()+
    geom_point()+
    labs(y="number of expired fish detected", x="date")+
    ggtitle("Survival over Time")+
    theme(plot.title = element_text(hjust=0.5))
}
