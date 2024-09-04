##COMPARE PARAMETERS
##CCF 1.3.24

#install packages
library(rjags)
library(R2jags)
library(rlist)

##read in data files
YA_poor<-readRDS('YA_poor.rds')
YA_rich<-readRDS('YA_rich.rds')
OA_poor<-readRDS('OA_poor.rds')
OA_rich<-readRDS('OA_rich.rds')

#AFFECT RICH VS AFFECT POOR_____________________________________________________________________
##Younger Adults
  #calculate difference in mean estimated parameters
rich.poor.ya.mu.alpha.diff<-YA_rich$mu.alpha-YA_poor$mu.alpha
rich.poor.ya.mu.delta.loss.diff<-YA_rich$mu.delta.loss-YA_poor$mu.delta.loss
rich.poor.ya.mu.gamma.diff<-YA_rich$mu.gamma-YA_poor$mu.gamma
rich.poor.ya.mu.theta.diff<-YA_rich$mu.theta-YA_poor$mu.theta
  #plot differences in posterior distributions
source("DBDA2E-utilities.R")
plotPost(YA_rich$mu.alpha-YA_poor$mu.alpha, main ="YA Rich [alpha]- YA Poor [alpha]", xlab=bquote(alpha), cenTend = "mean")
plotPost(YA_rich$mu.delta.loss-YA_poor$mu.delta.loss, main ="YA Rich [delta loss]- YA Poor [delta loss]", xlab=bquote(delta), cenTend = "mean")
plotPost(YA_rich$mu.gamma-YA_poor$mu.gamma, main ="YA Rich [gamma]- YA Poor [gamma]", xlab=bquote(gamma), cenTend = "mean")
plotPost(YA_rich$mu.theta-YA_poor$mu.theta, main ="YA Rich [theta]- YA Poor [theta]", xlab=bquote(theta), cenTend = "mean")

##Older Adults
  #calculate difference in mean estimated parameters
rich.poor.oa.mu.alpha.diff<-OA_rich$mu.alpha-OA_poor$mu.alpha
rich.poor.oa.mu.delta.loss.diff<-OA_rich$mu.delta.loss-OA_poor$mu.delta.loss
rich.poor.oa.mu.gamma.diff<-OA_rich$mu.gamma-OA_poor$mu.gamma
rich.poor.oa.mu.theta.diff<-OA_rich$mu.theta-OA_poor$mu.theta
  #plot differences in posterior distributions
source("DBDA2E-utilities.R")
plotPost(OA_rich$mu.alpha-OA_poor$mu.alpha, main ="OA Rich [alpha]- OA Poor [alpha]", xlab=bquote(alpha), cenTend = "mean")
plotPost(OA_rich$mu.delta.loss-OA_poor$mu.delta.loss, main ="OA Rich [delta loss]- OA Poor [delta loss]", xlab=bquote(delta), cenTend = "mean")
plotPost(OA_rich$mu.gamma-OA_poor$mu.gamma, main ="OA Rich [gamma]- OA Poor [gamma]", xlab=bquote(gamma), cenTend = "mean")
plotPost(OA_rich$mu.theta-OA_poor$mu.theta, main ="OA Rich [theta]- OA Poor [theta]", xlab=bquote(theta), cenTend = "mean")

## OLDER ADULTS VS YOUNGER ADULTS__________________________________________________________
##Affect Rich
  #calculate difference in mean estimated parameters
ya.oa.rich.mu.alpha.diff<-YA_rich$mu.alpha-OA_rich$mu.alpha
ya.oa.rich.mu.delta.loss.diff<-YA_rich$mu.delta.loss-OA_rich$mu.delta.loss
ya.oa.rich.mu.gamma.diff<-YA_rich$mu.gamma-OA_rich$mu.gamma
ya.oa.rich.mu.theta.diff<-YA_rich$mu.theta-OA_rich$mu.theta
  #plot differences in posterior distributions
source("DBDA2E-utilities.R")
plotPost(YA_rich$mu.alpha-OA_rich$mu.alpha, main ="YA Rich [alpha]- OA Rich [alpha]", xlab=bquote(alpha), cenTend = "mean")
plotPost(YA_rich$mu.delta.loss-OA_rich$mu.delta.loss, main ="YA Rich [delta loss]- OA Rich [delta loss]", xlab=bquote(delta), cenTend = "mean")
plotPost(YA_rich$mu.gamma-OA_rich$mu.gamma, main ="YA Rich [gamma]- OA Rich [gamma]", xlab=bquote(gamma), cenTend = "mean")
plotPost(YA_rich$mu.theta-OA_rich$mu.theta, main ="YA Rich [theta]- OA Rich [theta]", xlab=bquote(theta), cenTend = "mean")

##Affect Poor
  #calculate difference in mean estimated parameters
ya.oa.poor.mu.alpha.diff<-YA_poor$mu.alpha-OA_poor$mu.alpha
ya.oa.poor.mu.delta.loss.diff<-YA_poor$mu.delta.loss-OA_poor$mu.delta.loss
ya.oa.poor.mu.gamma.diff<-YA_poor$mu.gamma-OA_poor$mu.gamma
ya.oa.poor.mu.theta.diff<-YA_poor$mu.theta-OA_poor$mu.theta
  #plot differences in posterior distributions
source("DBDA2E-utilities.R")
plotPost(YA_poor$mu.alpha-OA_poor$mu.alpha, main ="YA Poor [alpha]- OA Poor [alpha]", xlab=bquote(alpha), cenTend = "mean")
plotPost(YA_poor$mu.delta.loss-OA_poor$mu.delta.loss, main ="YA Poor [delta loss]- OA Poor [delta loss]", xlab=bquote(delta), cenTend = "mean")
plotPost(YA_poor$mu.gamma-OA_poor$mu.gamma, main ="YA Poor [gamma]- OA Poor [gamma]", xlab=bquote(gamma), cenTend = "mean")
plotPost(YA_poor$mu.theta-OA_poor$mu.theta, main ="YA Poor [theta]- OA Poor [theta]", xlab=bquote(theta), cenTend = "mean")
