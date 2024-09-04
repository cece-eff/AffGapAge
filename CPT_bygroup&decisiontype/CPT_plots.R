# Plot the weighting functions
#import r data
load("/Users/ccf160030/Library/CloudStorage/Box-Box/GreysAnatomy/CPTanalyses1.5.24/samples.ya.poor.RData")
load("/Users/ccf160030/Library/CloudStorage/Box-Box/GreysAnatomy/CPTanalyses1.5.24/samples.oa.poor.RData")
load("/Users/ccf160030/Library/CloudStorage/Box-Box/GreysAnatomy/CPTanalyses1.5.24/samples.ya.rich.RData")
load("/Users/ccf160030/Library/CloudStorage/Box-Box/GreysAnatomy/CPTanalyses1.5.24/samples.oa.rich.RData")

##for older adults
samples.money<-samples.oa.poor
samples.se<-samples.oa.rich

## for younger adults
samples.money<-samples.ya.poor
samples.se<-samples.ya.rich

## reset
samples.money<-NULL
samples.se<-NULL

##probability weighting function
par(mfrow=c(1,2))
##affect poor
a <- seq(from=0, to= 1, by=.001)
plot(a,a,"l",xlab='',ylab='',cex.axis=.7,lty=1,lwd = 2, col = "white",axes=F)
title('Affect Poor',cex.main = 1.5, col.main="blue")
mtext(side = 1, text = "Objective Probability", line = 1.7)
mtext(side = 2, text = "w(p)", line = 1.7)
for (subj in 1:100) {
  gamma <- samples.money$BUGSoutput$median$gamma[subj]
  delta <- samples.money$BUGSoutput$median$delta.loss[subj]
  b <- (c(delta)*a^c(gamma))/(c(delta)*a^c(gamma)+(1-a)^c(gamma))
  par(new=T)
  lines(a,b,col=rgb(0,0,255,40,maxColorValue=255),lty=1,lwd = 1)
}
lines(a,a,col="black",lty=2,lwd =1)
gamma <- samples.money$BUGSoutput$median$mu.gamma
delta <- samples.money$BUGSoutput$median$mu.delta
b <- (c(delta)*a^c(gamma))/(c(delta)*a^c(gamma)+(1-a)^c(gamma))
lines(a,b,col="blue",lty=1,lwd =2)
axis(1, line = 0, cex.axis = .8, mgp=c(2,.6,0))
axis(2, line = 0, cex.axis = .8, mgp=c(2,.6,0))
box(lty = 1, col = 'black')

##affect-rich
plot(a,a,"l",xlab='',ylab='',cex.axis=.7,lty=1,lwd = 2, col = "white",axes=F)
title('Affect Rich',cex.main = 1.5, col.main="red")
mtext(side = 1, text = "Objective Probability", line = 1.7)
mtext(side = 2, text = "w(p)", line = 1.7)

for (subj in 1:100) {
  gamma <- samples.se$BUGSoutput$median$gamma[subj]
  delta <- samples.se$BUGSoutput$median$delta.loss[subj]
  b <- (c(delta)*a^c(gamma))/(c(delta)*a^c(gamma)+(1-a)^c(gamma))
  par(new=T)
  lines(a,b,col=rgb(255,0,0,40,maxColorValue=255),lty=1,lwd = 1)
}
lines(a,a,col="black",lty=2,lwd =1)
gamma <- samples.se$BUGSoutput$median$mu.gamma
delta <- samples.se$BUGSoutput$median$mu.delta
b <- (c(delta)*a^c(gamma))/(c(delta)*a^c(gamma)+(1-a)^c(gamma))
lines(a,b,col="red",lty=1,lwd =2)
axis(1, line = 0, cex.axis = .8, mgp=c(2,.6,0))
axis(2, line = 0, cex.axis = .8, mgp=c(2,.6,0))
box(lty = 1, col = 'black')


#### Value function 4.6*8.5
par(mfrow=c(1,2))
#Affect Poor
a <- c(seq(0, 100, by=1))
plot(a,a,"n",axes=FALSE,xlab='',ylab='',cex.axis=.7,lty=2,lwd = 1, ylim=c(0, 10),xlim=c(0, 100), col = "white")
title('Affect Poor',cex.main = 1.5,col.main="blue")
axis(1, seq(from = 0, to = 100, by = 20),  pos=0,cex.axis=.8, mgp=c(2, .6, 0))
axis(2, seq(from = 0, to = 10, by = 2), pos=0,cex.axis=.8, mgp=c(2, .6, 0))
mtext(side = 1, text = "Objective Loss", line = 1.2)
mtext(side = 2, text = "Subjective Value", line = 1.2)
for (subj in 1:100) {
  alpha <- samples.money$BUGSoutput$median$alpha[subj]
  lambda <- 1
  b <- c(a^c(alpha))
  par(new=T)
  lines(a,b,col=rgb(0,0,255,40,maxColorValue=255),lty=1,lwd=1)
}
#lines(a,a,col="#838B8B",lty=2,lwd=1)
alpha <- samples.money$BUGSoutput$median$mu.alpha
lambda <- 1
b <- c(a^c(alpha))
lines(a,b,col="blue",lty=1,lwd =2)


#Affect-rich
a <- c(seq(0, 100, by=1))
#mat <- matrix(c(2,2),  nrow = 2, ncol = 2)
#layout(mat)
plot(a,a,"l",axes=FALSE,xlab='',ylab='',cex.axis=.7,lty=2,lwd = 1, ylim=c(0, 10),xlim=c(0, 100), col = "white")
title('Affect Rich',cex.main = 1.5,col.main="red")
axis(1, seq(from = 0, to = 100, by = 20), pos=0,cex.axis=.8, mgp=c(2,.6,0))
axis(2, seq(from = 0, to = 10, by = 2), pos=0,cex.axis=.8, mgp=c(2,.6,0))
mtext(side = 1, text = "Objective Loss", line = 1.2)
mtext(side = 2, text = "Subjective Value", line = 1.2)
for (subj in 1:100) {
  alpha <- samples.se$BUGSoutput$median$alpha[subj]
  lambda <- 1
  b <- c(a^c(alpha))
  par(new=T)
  lines(a,b,col=rgb(255,0,0,40,maxColorValue=255),lty=1,lwd=1)
}
#lines(a,a,col="#838B8B",lty=2,lwd=1)
alpha <- samples.se$BUGSoutput$median$mu.alpha
lambda <- 
b <- c(a^c(alpha))
lines(a,b,col="red",lty=1,lwd =2)
