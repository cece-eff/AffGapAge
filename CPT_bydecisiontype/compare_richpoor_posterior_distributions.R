##Compare posterior distributions of affect-rich vs affect-poor collapsed across all participants
##CCF 7.15.2024

#install packages
library(rjags)
library(R2jags)
library(rlist)

##read in data files
all_poor<-readRDS('all_poor.rds')
all_rich<-readRDS('all_rich.rds')

#AFFECT RICH VS AFFECT POOR
##All ages

  #calculate difference in mean estimated parameters
rich.poor.mu.alpha.diff<-all_rich$mu.alpha-all_poor$mu.alpha
rich.poor.mu.delta.loss.diff<-all_rich$mu.delta.loss-all_poor$mu.delta.loss
rich.poor.mu.gamma.diff<-all_rich$mu.gamma-all_poor$mu.gamma
rich.poor.mu.theta.diff<-all_rich$mu.theta-all_poor$mu.theta

  #plot differences in posterior distributions
source("DBDA2E-utilities.R")
plotPost(as.vector(all_rich$mu.alpha)-as.vector(all_poor$mu.alpha), main ="Rich [alpha]- Poor [alpha]", xlab=bquote(alpha), cenTend = "mean")
plotPost(as.vector(all_rich$mu.delta.loss)-as.vector(all_poor$mu.delta.loss), main ="Rich [delta loss]- Poor [delta loss]", xlab=bquote(delta), cenTend = "mean")
plotPost(as.vector(all_rich$mu.gamma)-as.vector(all_poor$mu.gamma), main ="Rich [gamma]- Poor [gamma]", xlab=bquote(gamma), cenTend = "mean")
plotPost(as.vector(all_rich$mu.theta)-as.vector(all_poor$mu.theta), main ="Rich [theta]- Poor [theta]", xlab=bquote(theta), cenTend = "mean")

