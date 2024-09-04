####YOUNGER ADULTS AFFECT-RICH CPT MODELING

#install.packages("readxl")
library(readxl)
library(rjags)
library(R2jags)
library(rlist)

#import data for choices
data.choices<-read.csv("choicedata_greys_complete_ordered.csv")
#work computer
data.choices<-read.csv("/Users/ccf160030/Library/CloudStorage/Box-Box/GreysAnatomy/CPTanalyses1.5.24/choicedata_greys_complete_ordered.csv")

#Create the new variables
data.choices$oA1_se<-ifelse(data.choices$type=="medchoice",data.choices$opta, 0)
data.choices$oB1_se<-ifelse(data.choices$type=="medchoice",data.choices$optb, 0)
data.choices$oA1_money<-ifelse(data.choices$type=="monchoice",data.choices$wtpamount_opta, 0)
data.choices$oB1_money<-ifelse(data.choices$type=="monchoice",data.choices$wtpamount_optb, 0)
data.choices$oA1_money <- as.numeric(data.choices$oA1_money)
data.choices$oB1_money <- as.numeric(data.choices$oB1_money)
data.choices$oa1 <- as.numeric(data.choices$oa1)
data.choices$ob1 <- as.numeric(data.choices$ob1)

##filter so we only include younger adults
ya<-data.choices[data.choices$group == 'young',]

#Prepare the data
ya_gambles <- cbind(ya$oa1, ya$pa1, rep(0,length(ya$oa1)), rep(0,length(ya$oa1)), rep(0,length(ya$oa1)), ya$pa2,  
                        ya$ob1, ya$pb1, rep(0,length(ya$ob1)), rep(0,length(ya$ob1)), rep(0,length(ya$ob1),1), ya$pb2)

##create matrices for ordered data
ya_gambles.ordered.A <- matrix(nrow=nrow(ya_gambles),ncol = 6)
ya_gambles.ordered.B <- matrix(nrow=nrow(ya_gambles),ncol = 6)

for (i in 1:nrow(ya_gambles)) {
  ya_out <- sort(ya_gambles[i,c(1,3,5)], index.return=TRUE,decreasing = T)
  ya_gambles.ordered.A[i,] <- c(ya_out$x[1], ya_gambles[i,ya_out$ix[1]*2], ya_out$x[2], ya_gambles[i,ya_out$ix[2]*2], ya_out$x[3], ya_gambles[i,ya_out$ix[3]*2])
  ya_out <- sort(ya_gambles[i,c(7,9,11)], index.return=TRUE,decreasing = T)
  ya_gambles.ordered.B[i,] <- c(ya_out$x[1], ya_gambles[i,ya_out$ix[1]*2+6], ya_out$x[2], ya_gambles[i,ya_out$ix[2]*2+6], ya_out$x[3], ya_gambles[i,ya_out$ix[3]*2+6])  
}
ya_gambles <- cbind(ya_gambles.ordered.A, ya_gambles.ordered.B)

##separate choice data for affect rich and affect poor
ya_gambles.money <- ya_gambles[ya$type=="monchoice",]
ya_gambles.se <- ya_gambles[ya$type=="medchoice",]

#create array for money/poor
ya_gambles.3d.money <- array(NA, dim = c(44,12,100))  
for (i in 1:100){
  ya_gambles.3d.money[,,i] <- ya_gambles.money[(i*44-43):(i*44),]
}

#create array for side effects/rich
ya_gambles.3d.se <- array(NA, dim = c(44,12,100))  
for (i in 1:100){
  ya_gambles.3d.se[,,i] <- ya_gambles.se[(i*44-43):(i*44),]
}

#isolate choice trials
ya_choices.se<- ya$choice[ya$type=="medchoice"]
ya_choices.money <- ya$choice[ya$type=="monchoice"]
ya_choices.se <- matrix(ya_choices.se, nrow = 44, byrow = F)
ya_choices.money <- matrix(ya_choices.money, nrow = 44, byrow = F)

#__________________________________________________________________________________________________#
#When modeling affect-rich data, point to correct choice data and array
choices <- ya_choices.se
prospectsA <- ya_gambles.3d.se[,1:6,]
prospectsB <- ya_gambles.3d.se[,7:12,]

#Determine cumulative probabilities
cumprobsA = cbind(ya_gambles.3d.se[,2,1], rowSums(ya_gambles.3d.se[,c(2,4),1]), rowSums(ya_gambles.3d.se[,c(2,4,6),1]), rowSums(ya_gambles.3d.se[,c(4,6),1]), ya_gambles.3d.se[,6,1])
cumprobsB = cbind(ya_gambles.3d.se[,8,1], rowSums(ya_gambles.3d.se[,c(8,10),1]), rowSums(ya_gambles.3d.se[,c(8,10,12),1]), rowSums(ya_gambles.3d.se[,c(10,12),1]), ya_gambles.3d.se[,12,1])

#set number of subjects and number of problems
nSubj <- 100
nItems <- 44

#Initial values
#oneinits <- list(alpha.phi =  rep(qnorm(.4),nSubj), gamma.phi = rep(qnorm(.5),nSubj), delta.loss.phi = rep(qnorm(.7),nSubj), theta.phi = rep(qnorm(.001),nSubj),
 #                mu.phi.alpha = qnorm(.4), mu.phi.gamma = qnorm(.5), mu.phi.delta.loss = qnorm(.7), mu.phi.theta = qnorm(.001))
#oneinits <- list(alpha.phi =  rep(qnorm(.2),nSubj), gamma.phi = rep(qnorm(.5),nSubj), delta.loss.phi = rep(qnorm(.7),nSubj), theta.phi = rep(qnorm(.000001),nSubj),
 #                mu.phi.alpha = qnorm(.2), mu.phi.gamma = qnorm(.5), mu.phi.delta.loss = qnorm(.7), mu.phi.theta = qnorm(.000001))


#oneinits <- list(alpha.phi =  rep(qnorm(.4),nSubj), gamma.phi = rep(qnorm(.5),nSubj), delta.loss.phi = rep(qnorm(.7),nSubj), theta.phi = rep(qnorm(.001),nSubj),
#                 mu.phi.alpha = qnorm(.4), mu.phi.gamma = qnorm(.5), mu.phi.delta.loss = qnorm(.7), mu.phi.theta = qnorm(.001))

#oneinits <- list(alpha.phi =  rep(qnorm(.5),nSubj), gamma.phi = rep(qnorm(.5),nSubj), delta.loss.phi = rep(qnorm(.7),nSubj), theta.phi = rep(qnorm(.001),nSubj),
#                mu.phi.alpha = qnorm(.5), mu.phi.gamma = qnorm(.5), mu.phi.delta.loss = qnorm(.7), mu.phi.theta = qnorm(.001))

#oneinits <- list(alpha.phi =  rep(qnorm(.4),nSubj), gamma.phi = rep(qnorm(.5),nSubj), delta.loss.phi = rep(qnorm(.7),nSubj), theta.phi = rep(qnorm(.999999),nSubj),
 #                mu.phi.alpha = qnorm(.4), mu.phi.gamma = qnorm(.5), mu.phi.delta.loss = qnorm(.7), mu.phi.theta = qnorm(.999999))

#oneinits <- list(alpha.phi =  rep(qnorm(.4),nSubj), gamma.phi = rep(qnorm(.5),nSubj), delta.loss.phi = rep(qnorm(.7),nSubj), theta.phi = rep(5,nSubj),
#                 mu.phi.alpha = qnorm(.4), mu.phi.gamma = qnorm(.5), mu.phi.delta.loss = qnorm(.7), mu.phi.theta = 5)

##THIS WORKS WITH DIVIDE BY THETA
oneinits <- list(alpha.phi =  rep(qnorm(.2),nSubj), gamma.phi = rep(qnorm(.5),nSubj), delta.loss.phi = rep(qnorm(.7),nSubj), theta.phi = rep(qnorm(.99999),nSubj),
                 mu.phi.alpha = qnorm(.2), mu.phi.gamma = qnorm(.5), mu.phi.delta.loss = qnorm(.7), mu.phi.theta = qnorm(.99999))

#Parameters to be monitored	
parameters <- c("alpha","gamma","delta.loss","theta","mu.alpha","mu.gamma","mu.delta.loss","mu.theta")

#set number of chains--4 minimum, 10 is better if have core---use detectCores() on library(parallel) to find out
nChains <- 10

#put initial values into a list 
myinits <- list(oneinits)

#Organize the data to be handed over to JAGS
data = list(choices=choices,nSubj=nSubj,prospectsA=prospectsA,prospectsB=prospectsB,nItems=nItems,cumprobsA=cumprobsA,cumprobsB=cumprobsB)

##affect rich
samples.ya.rich <- jags.parallel(data=data, inits=myinits, parameters=parameters,
                                 model.file ="CPT_hierarchical_GEd_negative_reparam.txt", n.chains=nChains, n.iter=50000, 
                                 n.burnin=2000, n.thin=2, DIC=T)
##save
save(samples.ya.rich, file='samples.ya.rich.RData')

##summarize and plot
samples.ya.rich$BUGSoutput$summary
print(samples.ya.rich, dig=3)
plot(samples.ya.rich)

#check effective samples
coda::effectiveSize(samples.ya.rich)

#all mean posterior distributions 
samples.ya.rich$BUGSoutput$mean

#create object with list of posterior distributions for all parameters
YA_rich<-samples.ya.rich$BUGSoutput$sims.list

##save list as file to reopen later
list.save(YA_rich, 'YA_rich.rds')

#density plots for posterior distributions of group level parameters
plot(density(YA_rich$mu.alpha))
plot(density(YA_rich$mu.delta.loss))
plot(density(YA_rich$mu.gamma))
plot(density(YA_rich$mu.theta))

#create MCMC samples from posterior distribution to run Kruschke code 
jagsfit_ya_rich.mcmc<-as.mcmc(samples.ya.rich)

#create graphs with convergence diagnostics for output of JAGS for group-level parameters including parameter value, autocorrelation and effective samples, shrink factor and density
source("DBDA2E-utilities.R")
diagMCMC(codaObject = jagsfit_ya_rich.mcmc, parName="mu.alpha")
diagMCMC(codaObject = jagsfit_ya_rich.mcmc, parName="mu.delta.loss")
diagMCMC(codaObject = jagsfit_ya_rich.mcmc, parName="mu.gamma")
diagMCMC(codaObject = jagsfit_ya_rich.mcmc, parName="mu.theta")

#plot the posterior distribution based on the JAGS output and 95% HDI
plotPost(jagsfit_ya_rich.mcmc[,"mu.alpha"], main="ya.rich.mu.alpha", xlab=bquote(alpha), cenTend = "mean")
plotPost(jagsfit_ya_rich.mcmc[,"mu.delta.loss"], main="ya.rich.mu.delta.loss", xlab=bquote(delta),  cenTend = "mean")
plotPost(jagsfit_ya_rich.mcmc[,"mu.gamma"], main="ya.rich.mu.gamma", xlab=bquote(gamma),  cenTend = "mean")
plotPost(jagsfit_ya_rich.mcmc[,"mu.theta"], main="ya.rich.mu.theta", xlab=bquote(theta),  cenTend = "mean")
