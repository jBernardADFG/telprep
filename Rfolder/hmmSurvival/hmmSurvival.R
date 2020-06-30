#' need to document
hmm.survival <- function(best_detects, t_star=1, cl=0.95){
  msm_data <- org.dat.msm(best_detects, t_star = t_star)
  fit_mod <- bwv(msm_data)
  results <- extract.results(fit_mod)
  v <- viterbi.msm(fit_mod)
  v <- as.data.frame(cbind(TagID=msm_data$TagID, Channel=msm_data$Channel, Subject=msm_data$subject, FlightNum=msm_data$FlightNum, Fitted=v$fitted))
  # Map the viterbi path back to best_detects
  best_detects$Viterbi <- NA
  for (i in 1:nrow(best_detects)){
    best_detects$Viterbi[i] <- v$Fitted[v$TagID==best_detects$TagID[i] & v$Channel==best_detects$Channel[i] & v$FlightNum==best_detects$FlightNum[i]]
  }
  return(list(results=results, viterbi=best_detects))
}

