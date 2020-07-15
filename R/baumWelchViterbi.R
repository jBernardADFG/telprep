#' Fits a Hidden Markov Model to estimate survival and detection probabilities of fish and evaluate the faultiness of mortality sensors.
#'
#' @description The function uses \code{\link[msm]{msm}} to fit the hidden Markov Model. In the output, live fish are in state 1 while expired fish are in state 2. For the emission matrices, outcome 1 relates fish movement information to the survival state: P(1) is the probabilty of no detection, P(2) is the probability that a fish moved, and P(3) is the probabilty that a fish stayed. Outcome 2 relates mortality sensor information to the survival state: P(1) is the probability of no detection, P(2) is the probability that the mortality signal is off, and P(3) is the probabilty that the morality signal is on.
#' @param msm_data Output of \code{\link{org_dat_msm}}.
#' @return returns a data.frame to be input into \code{\link{extract_results}}.
#' @export
#' @examples
#' fit_mod <- fit_hmm(org_dat)

fit_hmm <- function(msm_data){
  if (!requireNamespace("msm", quietly = TRUE)) {
    stop("Package \"msm\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  obs <- msm_data$obs
  time <- msm_data$time
  subject <- msm_data$subject
  fit_mod <- msm::msm(obs ~ time,
                  subject=subject,
                  initprobs = c(0.999,0.001),
                  qmatrix = rbind(c(0.999,0.001),c(0,1)),
                  hmodel = list(hmmMV(hmmCat(prob=c(0.5,0.05,0.25)), # Alive - move
                                      hmmCat(prob=c(0.5,0.05,0.25))), # Alive - mort-sig
                                hmmMV(hmmCat(prob=c(0.4,0.4,0)), # Dead - move
                                      hmmCat(prob=c(0.3,0.25,0.05)))), # Dead -mort-sig
                  method="BFGS", control=list(maxit=15000))
  return(fit_mod)
}

