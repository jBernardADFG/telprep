# Pick up here tomorrow...
# 1-no-detect
# 2-stay
# 3-move

# 1 no lat-long
# 2 mort sig off
# 3 mor sig on

# Use a single categorical variable so that we can use the functionality of ematrix
# Think we also want to add a version with an "emmigrated from study area" state

bwv <- function(msm_data){
  obs <- msm_data$obs
  time <- msm_data$time
  subject <- msm_data$subject
  fit_mod <- msm(obs ~ time,
                 subject=subject,
                 initprobs = c(0.999,0.001),
                 qmatrix = rbind(c(0.999,0.001),c(0,1)),
                 hmodel = list(hmmMV(hmmCat(prob=c(0.7,0.05,0.25)), # Alive - move
                                     hmmCat(prob=c(0.7,0.05,0.25))), # Alive - mort-sig
                               hmmMV(hmmCat(prob=c(0.4,0.4,0)), # Dead - move
                                     hmmCat(prob=c(0.3,0.25,0.05)))), # Dead -mort-sig
                 method="BFGS", control=list(maxit=5000))
}

