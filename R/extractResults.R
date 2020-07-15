#' Summarize the output of a fit Hidden Markov Model
#'
#' @param fit_mod Output of \code{\link{fit_hmm}}.
#' @param cl The desired confidence level.
#' @return Returns fish survival, detection, and mortality signal related information.
#' @export
#' @examples
#' extract_results(fit_mod, cl=0.90)

extract_results <- function(fit_mod, cl=0.95){
  if (!requireNamespace("msm", quietly = TRUE)) {
    stop("Package \"msm\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Survival and mortality rates
  T_mat <- pmatrix.msm(fit_mod, t=365, ci="normal", cl=cl)
  surv_est <- rbind(T_mat[1,1], T_mat[1,2])
  row.names(surv_est) <- c("annual survival rate", "annual mortality rate")

  # Emission probabilities
  msm_output <- capture.output(fit_mod)
  g_211 <- msm_output[22]
  g_212 <- msm_output[23]
  g_213 <- msm_output[24]
  g_221 <- msm_output[35]
  g_222 <- msm_output[36]
  g_223 <- msm_output[37]
  g_mat <- rbind(g_211, g_221, g_212, g_213, g_222, g_223)
  e_mat <- matrix(NA, nrow=nrow(g_mat),ncol=4)
  for (i in 1:nrow(g_mat)){
    temp <- unlist(strsplit(g_mat[i,], split=" "))
    names(temp) <- NULL
    e_mat[i,] <- temp[1:4]
  }
  e_mat <- e_mat[,-1]
  e_mat <- matrix(as.numeric(e_mat), ncol=3)
  e_mat[1,] <- 1-e_mat[1,c(1,3,2)]
  e_mat[2,] <- 1-e_mat[2,c(1,3,2)]
  row.names(e_mat) <- c("1-g_211", "1-g_221", "g_212", "g_213", "g_222", "g_223")
  # Detection probabilities
  detect_probs <- e_mat[1:2,]
  row.names(detect_probs) <- c("detection probability live fish",
                        "detection probability expired fish")
  colnames(detect_probs) <- c("estimate", "lower", "upper")

  # prob mort sig on given fish is alive
  num_1 <- e_mat[4,1]/(e_mat[3,1]+e_mat[4,1])
  num_2 <- e_mat[5,1]/(e_mat[5,1]+e_mat[6,1])
  sig_work <- matrix(NA, nrow=2, ncol=1)
  sig_work[1,1]<-paste("the mortality signals work for live fish approximately", round(num_1,2)*100 , "percent of the time")
  sig_work[2,1]<-paste("the mortality signals work for expired fish approximately", round(num_2,2)*100 , "percent of the time")

  # Return the results
  return(list(surv_est, detect_probs, sig_work))
}
