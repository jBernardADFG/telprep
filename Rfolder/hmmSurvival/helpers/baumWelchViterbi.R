# Pick up here tomorrow...

bwv <- function(msm_data){
  q_mat <-
  x <- msm(obs ~ time, subject=subject, data=msm_data, qmatrix=q_mat,
           hmodel = list(hmmBinom(size=3, prob=0.2),
                         hmmBinom(size=2, prob=0.2)))
  viterbi.msm(x)
}

