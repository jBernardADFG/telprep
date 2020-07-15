#' Determine the most likely path of survival states for each fish
#'
#' @param fit_mod Output of \code{\link{fit_hmm}}.
#' @param msm_data Output of \code{\link{org_dat_msm}}.
#' @param best_detects Output of \code{\link{get_best_locations}} or \code{\link{get_locations}}.
#' @return Returns a data.frame where $Viterbi encodes the most likely path of survival states (1 means alive whereas 2 means expired). The returned data can be input into \code{\link{make_plot}}.
#' @export
#' @examples
#' viterbi <- get_viterbi(fit_mod, org_dat, best_detects)

get_viterbi <- function(fit_mod, msm_data, best_detects){
  if (!requireNamespace("msm", quietly = TRUE)) {
    stop("Package \"msm\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("sfsmisc", quietly = TRUE)) {
    stop("Package \"sfsmisc\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  v <- viterbi.msm(fit_mod)
  v <- as.data.frame(cbind(TagID=msm_data$TagID, Channel=msm_data$Channel, Subject=msm_data$subject, FlightNum=msm_data$FlightNum, Fitted=v$fitted))
  best_detects$Viterbi <- NA
  for (i in 1:nrow(best_detects)){
    best_detects$Viterbi[i] <- v$Fitted[v$TagID==best_detects$TagID[i] & v$Channel==best_detects$Channel[i] & v$FlightNum==best_detects$FlightNum[i]]
  }
  return(best_detects)
}
