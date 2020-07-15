#' Use a Hidden Markov Model to determine the living status of the fish
#'
#' @description This function is a wrapper calling \code{\link{org_dat_msm}}, \code{\link{fit_hmm}}, \code{\link{get_viterbi}}, and \code{\link{extract_results}}. See the help files for these functions for more information.
#' @param best_detects Output of \code{\link{get_locations}} or \code{\link{get_best_locations}}.
#' @param t_star The distance in km at which a fish is considered to have stayed in place between detection periods.
#' @param cl The desired confidence level.
#' @return Returns a list where $results provides a summary of the fit HMM and $viterbi returns a data.frame where the most likely path of survival states (i.e. the viterbi path) is added to the best_detects (1 means alive whereas 2 means expired). $viterbi can be input into \code{\link{make_plot}}.
#' @export
#' @examples
#' hmm_survival(best_detects)

hmm_survival <- function(best_detects, t_star=1, cl=0.95){
  if (!requireNamespace("msm", quietly = TRUE)) {
    stop("Package \"msm\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  msm_data <- org_dat_msm(best_detects, t_star = t_star)
  fit_mod <- fit_hmm(msm_data)
  results <- extract_results(fit_mod)
  v <- get_viterbi(fit_mod, msm_data, best_detects)
  return(list(results=results, viterbi=v))
}

