####ALL AGES AFFECT-POOR CPT MODELING
##ccf 7.15.24

#install.packages("readxl")
library(readxl)
library(rjags)
library(R2jags)
library(rlist)

#import data for choices
new_data.choices<-read.csv("choicedata_greys_complete_ordered.csv")

#Create the new variables
new_data.choices$oA1_se<-ifelse(new_data.choices$type=="medchoice",new_data.choices$opta, 0)
new_data.choices$oB1_se<-ifelse(new_data.choices$type=="medchoice",new_data.choices$optb, 0)
new_data.choices$oA1_money<-ifelse(new_data.choices$type=="monchoice",new_data.choices$wtpamount_opta, 0)
new_data.choices$oB1_money<-ifelse(new_data.choices$type=="monchoice",new_data.choices$wtpamount_optb, 0)
new_data.choices$oA1_money <- as.numeric(new_data.choices$oA1_money)
new_data.choices$oB1_money <- as.numeric(new_data.choices$oB1_money)
new_data.choices$oa1 <- as.numeric(new_data.choices$oa1)
new_data.choices$ob1 <- as.numeric(new_data.choices$ob1)


all<-new_data.choices

#Prepare the data
all_new_gambles <- cbind(all$oa1, all$pa1, rep(0,length(all$oa1)), rep(0,length(all$oa1)), rep(0,length(all$oa1)), all$pa2,  
                        all$ob1, all$pb1, rep(0,length(all$ob1)), rep(0,length(all$ob1)), rep(0,length(all$ob1),1), all$pb2) 

all_new_gambles.ordered.A <- matrix(nrow=nrow(all_new_gambles),ncol = 6)
all_new_gambles.ordered.B <- matrix(nrow=nrow(all_new_gambles),ncol = 6)

for (i in 1:nrow(all_new_gambles)) {
  all_new_out <- sort(all_new_gambles[i,c(1,3,5)], index.return=TRUE,decreasing = T)
  all_new_gambles.ordered.A[i,] <- c(all_new_out$x[1], all_new_gambles[i,all_new_out$ix[1]*2], all_new_out$x[2], all_new_gambles[i,all_new_out$ix[2]*2], all_new_out$x[3], all_new_gambles[i,all_new_out$ix[3]*2])
  all_new_out <- sort(all_new_gambles[i,c(7,9,11)], index.return=TRUE,decreasing = T)
  all_new_gambles.ordered.B[i,] <- c(all_new_out$x[1], all_new_gambles[i,all_new_out$ix[1]*2+6], all_new_out$x[2], all_new_gambles[i,all_new_out$ix[2]*2+6], all_new_out$x[3], all_new_gambles[i,all_new_out$ix[3]*2+6])  
}
all_new_gambles <- cbind(all_new_gambles.ordered.A, all_new_gambles.ordered.B)

##separate choice data for affect rich and affect poor
all_new_gambles.money <- all_new_gambles[all$type=="monchoice",]
all_new_gambles.se <- all_new_gambles[all$type=="medchoice",]

#create array for money/poor
all_new_gambles.3d.money <- array(NA, dim = c(44,12,200))  
for (i in 1:200){
  all_new_gambles.3d.money[,,i] <- all_new_gambles.money[(i*44-43):(i*44),]
}

#create array for side effects/rich
all_new_gambles.3d.se <- array(NA, dim = c(44,12,200))  
for (i in 1:200){
  all_new_gambles.3d.se[,,i] <- all_new_gambles.se[(i*44-43):(i*44),]
}

#isolate choice trials
all_new_choices.se<- all$choice[all$type=="medchoice"]
all_new_choices.money <- all$choice[all$type=="monchoice"]
all_new_choices.se <- matrix(all_new_choices.se, nrow = 44, byrow = F)
all_new_choices.money <- matrix(all_new_choices.money, nrow = 44, byrow = F)

#__________________________________________________________________________________________________#
#When modeling affect-poor data, point to correct choice data and array
choices <- all_new_choices.money
prospectsA <- all_new_gambles.3d.money[,1:6,]
prospectsB <- all_new_gambles.3d.money[,7:12,]

#Determine cumulative probabilities
cumprobsA = cbind(all_new_gambles.3d.money[,2,1], rowSums(all_new_gambles.3d.money[,c(2,4),1]), rowSums(all_new_gambles.3d.money[,c(2,4,6),1]), rowSums(all_new_gambles.3d.money[,c(4,6),1]), all_new_gambles.3d.money[,6,1])
cumprobsB = cbind(all_new_gambles.3d.money[,8,1], rowSums(all_new_gambles.3d.money[,c(8,10),1]), rowSums(all_new_gambles.3d.money[,c(8,10,12),1]), rowSums(all_new_gambles.3d.money[,c(10,12),1]), all_new_gambles.3d.money[,12,1])

#set number of subjects and number of problems
nSubj <- 200
nItems <- 44

#Initial values
#oneinits <- list(alpha.phi =  rep(qnorm(.4),nSubj), gamma.phi = rep(qnorm(.5),nSubj), delta.loss.phi = rep(qnorm(.7),nSubj), theta.phi = rep(qnorm(.000001),nSubj),
#                mu.phi.alpha = qnorm(.4), mu.phi.gamma = qnorm(.5), mu.phi.delta.loss = qnorm(.7), mu.phi.theta = qnorm(.000001))

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

##affect poor
samples.all.poor <- jags.parallel(data=data, inits=myinits, parameters=parameters,
                                 model.file ="CPT_hierarchical_GEd_negative_reparam.txt", n.chains=nChains, n.iter=50000, 
                                 n.burnin=2000, n.thin=2, DIC=T)
##save
save(samples.all.poor, file='samples.all.poor.RData')

##summarize and plot
samples.all.poor$BUGSoutput$summary
print(samples.all.poor, dig=3)
plot(samples.all.poor)

#check effective samples
coda::effectiveSize(samples.all.poor)

#all mean posterior distributions 
samples.all.poor$BUGSoutput$mean

#create object with list of posterior distributions for all parameters
all_poor<-samples.all.poor$BUGSoutput$sims.list

##save list as file to reopen later
list.save(all_poor, 'all_poor.rds')

#density plots for posterior distributions of group level parameters
plot(density(all_poor$mu.alpha))
plot(density(all_poor$mu.delta.loss))
plot(density(all_poor$mu.gamma))
plot(density(all_poor$mu.theta))

#create MCMC samples from posterior distribution to run Kruschke code 
jagsfit_all_poor.mcmc<-as.mcmc(samples.all.poor)

#create graphs with convergence diagnostics for output of JAGS for group-level parameters including parameter value, autocorrelation and effective samples, shrink factor and density
source("DBDA2E-utilities.R")
diagMCMC(codaObject = jagsfit_all_poor.mcmc, parName="mu.alpha")
diagMCMC(codaObject = jagsfit_all_poor.mcmc, parName="mu.delta.loss")
diagMCMC(codaObject = jagsfit_all_poor.mcmc, parName="mu.gamma")
diagMCMC(codaObject = jagsfit_all_poor.mcmc, parName="mu.theta")

#plot the posterior distribution based on the JAGS output and 95% HDI
plotPost(jagsfit_all_poor.mcmc[,"mu.alpha"], main="all.poor.mu.alpha", xlab=bquote(alpha), cenTend = "mean")
plotPost(jagsfit_all_poor.mcmc[,"mu.delta.loss"], main="all.poor.mu.delta.loss", xlab=bquote(delta),  cenTend = "mean")
plotPost(jagsfit_all_poor.mcmc[,"mu.gamma"], main="all.poor.mu.gamma", xlab=bquote(gamma),  cenTend = "mean")
plotPost(jagsfit_all_poor.mcmc[,"mu.theta"], main="all.poor.mu.theta", xlab=bquote(theta),  cenTend = "mean")
