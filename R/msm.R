# # Probably want to move some of this over into org...
#
#
# library(msm)
# ## Simulate data from a Markov model
# nsubj <- 30; nobspt <- 5
# sim.df <- data.frame(subject = rep(1:nsubj, each=nobspt),
#                      time = seq(0, 20, length=nobspt))
# set.seed(1)
# two.q <- rbind(c(-0.1, 0.1), c(0, 0))
# dat <- simmulti.msm(sim.df[,1:2], qmatrix=two.q, drop.absorb=FALSE)
#
# ### EXAMPLE 1
# ## Generate two observations at each time from the same outcome
# ## distribution:
# ## Bin(40, 0.1) for state 1, Bin(40, 0.5) for state 2
# dat$obs1[dat$state==1] <- rbinom(sum(dat$state==1), 40, 0.1)
# dat$obs2[dat$state==1] <- rbinom(sum(dat$state==1), 40, 0.1)
# dat$obs1[dat$state==2] <- rbinom(sum(dat$state==2), 40, 0.5)
# dat$obs2[dat$state==2] <- rbinom(sum(dat$state==2), 40, 0.5)
# dat$obs <- cbind(obs1 = dat$obs1, obs2 = dat$obs2)
#
#
#
#
# # organize into org.dat.msm
#
# msm_data_full <- org.dat.msm(best_detects)
# msm_data_reduced <- msm_data_full[,c(3,5,8,9)]
# msm_data_reduced$Displacement <- msm_data_reduced$Displacement>1
# msm_data_reduced$TagStatus <- round(as.numeric(msm_data_reduced$TagStatus))
# for (i in 1:nrow(msm_data_reduced)){
#   if (is.na(msm_data_reduced$Displacement[i])){
#     msm_data_reduced$Displacement[i] <- 1
#   } else if (msm_data_reduced$Displacement[i]){
#     msm_data_reduced$Displacement[i] <- 2
#   } else{
#     msm_data_reduced$Displacement[i] <- 3
#   }
# }
# new_tag_status <- rep(2, nrow(msm_data_reduced))
# new_tag_status[msm_data_reduced$TagStatus=="Mort"] <-3
# msm_data_reduced$TagStatus <- new_tag_status
#
#
#
# fish <- sort(unique(msm_data_reduced$Subject))
# detect_periods <- sort(unique(msm_data_reduced$Day))
#
# library(sfsmisc)
# df <- as.data.frame(xy.grid(fish, detect_periods))
# names(df) <- c("subject", "time")
# df$obs1 <- 1
# df$obs2 <- 1
#
# for (i in 1:nrow(msm_data_reduced)){
#   s <- msm_data_reduced$Subject[i]
#   d <- msm_data_reduced$Day[i]
#   if(!is.na(msm_data_reduced$Displacement[msm_data_reduced$Subject==s & msm_data_reduced$Day==d])){
#     obs1 <- msm_data_reduced$Displacement[msm_data_reduced$Subject==s & msm_data_reduced$Day==d]
#   }
#   if(!is.na(msm_data_reduced$TagStatus[msm_data_reduced$Subject==s & msm_data_reduced$Day==d])){
#     obs2 <- msm_data_reduced$TagStatus[msm_data_reduced$Subject==s & msm_data_reduced$Day==d]
#   }
#   df$obs1[df$subject==s & df$time==d] <- obs1
#   df$obs2[df$subject==s & df$time==d] <- obs2
# }
#
# df$obs <- cbind(obs1 = df$obs1, obs2 = df$obs2)
#
# dat <- df
# ## Fitted model should approximately recover true parameters
# x <- msm(obs ~ time, subject=subject, data=dat, qmatrix=two.q,
#     hmodel = list(hmmBinom(size=3, prob=0.2),
#                   hmmBinom(size=2, prob=0.2)))
# viterbi.msm(x)

