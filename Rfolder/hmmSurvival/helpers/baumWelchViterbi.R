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
                 hmodel = list(hmmMV(hmmCat(prob=c(0.7,0.05,0.25)),
                                     hmmCat(prob=c(0.7,0.25,0.05))),
                               hmmMV(hmmCat(prob=c(0.7,0.05,0.25)),
                                     hmmCat(prob=c(0.7,0.25,0.05)))),
                 method="BFGS", control=list(maxit=5000))
}
msm_data <- org.dat.msm(best_detects, t_star = 0.3)
fit_mod <- bwv(msm_data)
v <- viterbi.msm(fit_mod)
data <- msm_data
data$state <- v$fitted
pmatrix.msm(fit_mod, t=365, ci="bootstrap", B=10)
pmatrix.msm()
plot(fit_mod)
