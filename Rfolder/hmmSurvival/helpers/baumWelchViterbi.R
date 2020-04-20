# Pick up here tomorrow...

bwv <- function(msm_data){
  obs <- msm_data$obs
  time <- msm_data$time
  subject <- msm_data$subject
  q_mat <- rbind(c(0.9,0.1), c(0,1))
  hmodel = list(hmmMV(hmmCat(prob=c(0.3,0.3,0.3)),
                      hmmCat(prob=c(0.3,0.3,0.3))),
                hmmMV(hmmCat(prob=c(0.3,0.3,0.3)),
                      hmmCat(prob=c(0.3,0.3,0.3))))
  fit_mod <- msm(obs ~ time, subject=subject, qmatrix=q_mat,
           hmodel = hmodel, method="BFGS", control=list(maxit=5000))
  return(fit_mod)
}

fit_mod <- bwv(msm_data)
viterbi.msm(fit_mod)


