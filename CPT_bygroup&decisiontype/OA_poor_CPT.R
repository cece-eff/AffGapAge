###OLDER ADULTS AFFECT-POOR CPT MODELING

#install.packages("readxl")
library(readxl)
library(rjags)
library(R2jags)
library(rlist)

#import data for choices
new_data.choices<-read.csv("choicedata_greys_complete_ordered.csv")
#work computer
new_data.choices<-read.csv("/Users/ccf160030/Library/CloudStorage/Box-Box/GreysAnatomy/choicedata_greys_complete_ordered.csv")

#Create the new variables
new_data.choices$oA1_se<-ifelse(new_data.choices$type=="medchoice",new_data.choices$opta, 0)
new_data.choices$oB1_se<-ifelse(new_data.choices$type=="medchoice",new_data.choices$optb, 0)
new_data.choices$oA1_money<-ifelse(new_data.choices$type=="monchoice",new_data.choices$wtpamount_opta, 0)
new_data.choices$oB1_money<-ifelse(new_data.choices$type=="monchoice",new_data.choices$wtpamount_optb, 0)
new_data.choices$oA1_money <- as.numeric(new_data.choices$oA1_money)
new_data.choices$oB1_money <- as.numeric(new_data.choices$oB1_money)
new_data.choices$oa1 <- as.numeric(new_data.choices$oa1)
new_data.choices$ob1 <- as.numeric(new_data.choices$ob1)

##filter so we only include older adults
oa<-new_data.choices[new_data.choices$group == 'old',]

#Prepare the data
oa_new_gambles <- cbind(oa$oa1, oa$pa1, rep(0,length(oa$oa1)), rep(0,length(oa$oa1)), rep(0,length(oa$oa1)), oa$pa2,  
                        oa$ob1, oa$pb1, rep(0,length(oa$ob1)), rep(0,length(oa$ob1)), rep(0,length(oa$ob1),1), oa$pb2) 

oa_new_gambles.ordered.A <- matrix(nrow=nrow(oa_new_gambles),ncol = 6)
oa_new_gambles.ordered.B <- matrix(nrow=nrow(oa_new_gambles),ncol = 6)

for (i in 1:nrow(oa_new_gambles)) {
  oa_new_out <- sort(oa_new_gambles[i,c(1,3,5)], index.return=TRUE,decreasing = T)
  oa_new_gambles.ordered.A[i,] <- c(oa_new_out$x[1], oa_new_gambles[i,oa_new_out$ix[1]*2], oa_new_out$x[2], oa_new_gambles[i,oa_new_out$ix[2]*2], oa_new_out$x[3], oa_new_gambles[i,oa_new_out$ix[3]*2])
  oa_new_out <- sort(oa_new_gambles[i,c(7,9,11)], index.return=TRUE,decreasing = T)
  oa_new_gambles.ordered.B[i,] <- c(oa_new_out$x[1], oa_new_gambles[i,oa_new_out$ix[1]*2+6], oa_new_out$x[2], oa_new_gambles[i,oa_new_out$ix[2]*2+6], oa_new_out$x[3], oa_new_gambles[i,oa_new_out$ix[3]*2+6])  
}
oa_new_gambles <- cbind(oa_new_gambles.ordered.A, oa_new_gambles.ordered.B)

##separate choice data for affect rich and affect poor
oa_new_gambles.money <- oa_new_gambles[oa$type=="monchoice",]
oa_new_gambles.se <- oa_new_gambles[oa$type=="medchoice",]

#create array for money/poor
oa_new_gambles.3d.money <- array(NA, dim = c(44,12,100))  
for (i in 1:100){
  oa_new_gambles.3d.money[,,i] <- oa_new_gambles.money[(i*44-43):(i*44),]
}

#create array for side effects/rich
oa_new_gambles.3d.se <- array(NA, dim = c(44,12,100))  
for (i in 1:100){
  oa_new_gambles.3d.se[,,i] <- oa_new_gambles.se[(i*44-43):(i*44),]
}

#isolate choice trials
oa_new_choices.se<- oa$choice[oa$type=="medchoice"]
oa_new_choices.money <- oa$choice[oa$type=="monchoice"]
oa_new_choices.se <- matrix(oa_new_choices.se, nrow = 44, byrow = F)
oa_new_choices.money <- matrix(oa_new_choices.money, nrow = 44, byrow = F)

#__________________________________________________________________________________________________#
#When modeling affect-poor data, point to correct choice data and array
choices <- oa_new_choices.money
prospectsA <- oa_new_gambles.3d.money[,1:6,]
prospectsB <- oa_new_gambles.3d.money[,7:12,]

#Determine cumulative probabilities
cumprobsA = cbind(oa_new_gambles.3d.money[,2,1], rowSums(oa_new_gambles.3d.money[,c(2,4),1]), rowSums(oa_new_gambles.3d.money[,c(2,4,6),1]), rowSums(oa_new_gambles.3d.money[,c(4,6),1]), oa_new_gambles.3d.money[,6,1])
cumprobsB = cbind(oa_new_gambles.3d.money[,8,1], rowSums(oa_new_gambles.3d.money[,c(8,10),1]), rowSums(oa_new_gambles.3d.money[,c(8,10,12),1]), rowSums(oa_new_gambles.3d.money[,c(10,12),1]), oa_new_gambles.3d.money[,12,1])

#set number of subjects and number of problems
nSubj <- 100
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
samples.oa.poor <- jags.parallel(data=data, inits=myinits, parameters=parameters,
                                 model.file ="CPT_hierarchical_GEd_negative_reparam.txt", n.chains=nChains, n.iter=50000, 
                                 n.burnin=2000, n.thin=2, DIC=T)
##save
save(samples.oa.poor, file='samples.oa.poor.RData')

##summarize and plot
samples.oa.poor$BUGSoutput$summary
print(samples.oa.poor, dig=3)
plot(samples.oa.poor)

#check effective samples
coda::effectiveSize(samples.oa.poor)

#all mean posterior distributions 
samples.oa.poor$BUGSoutput$mean

#create object with list of posterior distributions for all parameters
OA_poor<-samples.oa.poor$BUGSoutput$sims.list

##save list as file to reopen later
list.save(OA_poor, 'OA_poor.rds')

#density plots for posterior distributions of group level parameters
plot(density(OA_poor$mu.alpha))
plot(density(OA_poor$mu.delta.loss))
plot(density(OA_poor$mu.gamma))
plot(density(OA_poor$mu.theta))

#create MCMC samples from posterior distribution to run Kruschke code 
jagsfit_oa_poor.mcmc<-as.mcmc(samples.oa.poor)

#create graphs with convergence diagnostics for output of JAGS for group-level parameters including parameter value, autocorrelation and effective samples, shrink factor and density
source("DBDA2E-utilities.R")
diagMCMC(codaObject = jagsfit_oa_poor.mcmc, parName="mu.alpha")
diagMCMC(codaObject = jagsfit_oa_poor.mcmc, parName="mu.delta.loss")
diagMCMC(codaObject = jagsfit_oa_poor.mcmc, parName="mu.gamma")
diagMCMC(codaObject = jagsfit_oa_poor.mcmc, parName="mu.theta")

#plot the posterior distribution based on the JAGS output and 95% HDI
plotPost(jagsfit_oa_poor.mcmc[,"mu.alpha"], main="oa.poor.mu.alpha", xlab=bquote(alpha), cenTend = "mean")
plotPost(jagsfit_oa_poor.mcmc[,"mu.delta.loss"], main="oa.poor.mu.delta.loss", xlab=bquote(delta),  cenTend = "mean")
plotPost(jagsfit_oa_poor.mcmc[,"mu.gamma"], main="oa.poor.mu.gamma", xlab=bquote(gamma),  cenTend = "mean")
plotPost(jagsfit_oa_poor.mcmc[,"mu.theta"], main="oa.poor.mu.theta", xlab=bquote(theta),  cenTend = "mean")
