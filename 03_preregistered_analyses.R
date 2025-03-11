#preregistered analyses
library(here)
library(tidyverse)
library(Rmisc)
library(brms)
library(ggplot2)
library(sjPlot)
library(plotrix)
library(lme4)
library(lmerTest)
library(MASS)
library(papaja)

##import data
#set path
choice_eval_wtp_all<-read.csv("~/Library/CloudStorage/Box-Box/Analyses/GreysAnatomy/choicedata_greys_complete.csv")

#PREREGISTERED: compare probability of choosing higher expected value (decision quality)
#run logistic regression
##with glmer
binom_dq<-glmer(EVchoice~group*type+CVchoice+sex+(1|participant)+(1|gambno), data=choice_eval_wtp_all, family="binomial")
summary(binom_dq)
sjPlot::plot_model(binom_dq, show.values = TRUE)

##with brms
bayes_dq<-brm(EVchoice~group*type+CVchoice+sex + (1|participant)+ (1|gambno), 
              data=choice_eval_wtp_all, 
              family = bernoulli(link = "logit"), 
              warmup=500, 
              iter=2000, 
              chains=2, 
              init="0", 
              cores=2, 
              seed=123)
summary(bayes_dq)
plot_model(bayes_dq)
sjPlot::plot_model(bayes_dq, show.values=TRUE)
mcmc_plot(bayes_dq, type="areas", prob=.95)

##calculate the probability of choosing the higher expected value choice by age group and gamble type
dqsum<-summarySE(choice_eval_wtp_all, measurevar="EVchoice", groupvars=c("group", "type"), na.rm = TRUE)
dqsumage<-summarySE(choice_eval_wtp_all, measurevar="EVchoice", groupvars= "group", na.rm = TRUE)
dqsumtype<-summarySE(choice_eval_wtp_all, measurevar="EVchoice", groupvars="type", na.rm = TRUE)



##graph of probability of choosing the higher expected value choice by age group and gamble type with 95% CI bars
dqsum[dqsum == 'old'] <- 'Older Adults'
dqsum[dqsum == 'young'] <- 'Younger Adults'
dqsum$group<-factor(dqsum$group, levels=c("Younger Adults", "Older Adults"))
dqsum$type<-factor(dqsum$type, levels=c("monchoice", "medchoice"))

ggplot(dqsum, aes(x=group, y=EVchoice, fill=type))+ 
  geom_bar(stat="identity", position= "dodge")+ 
  coord_cartesian(ylim = c(.6,.9)) + 
  geom_errorbar(aes(ymin=EVchoice-se, ymax=EVchoice+se), width=.2, position=position_dodge(.9)) +
  scale_fill_discrete(labels = c("Affect-Poor", "Affect-Rich"), type= c("blue", "red")) + theme_apa()+
  theme(plot.title = element_text(hjust = 0.5)) + theme(text= element_text(size=22), legend.position = "bottom") +
  labs(title= "Decision Quality", x = NULL, y= "Probability of Choosing the Better Option", fill = NULL)


bar<-table(choice_eval_wtp_all$group, choice_eval_wtp_all$EVchoice)
barplot(bar)

#PREREGISTERED: compare probability of choosing higher coefficient of variance (risk attitudes)
#run logistic regression
##with glmer
binom_ra<-glmer(CVchoice~group*type+EVchoice+sex+ (1|participant) + (1|gambno),
                data=choice_eval_wtp_all, family="binomial")
summary(binom_ra)
sjPlot::plot_model(binom_ra, show.values=TRUE)

##with brms
bayes_ra<-brm(CVchoice~group*type+EVchoice+sex +(1|participant) + (1|gambno), 
              data=choice_eval_wtp_all, 
              family = bernoulli(link = "logit"), 
              warmup=500, 
              iter=2000, 
              chains=2, 
              init="0", 
              cores=2, 
              seed=123)
summary(bayes_ra)
mcmc_plot(bayes_ra, type="areas", prob=.95)
sjPlot::plot_model(bayes_ra, show.values=TRUE)

##calculate the probability of choosing the higher expected value choice by age group and gamble type
rasum<-summarySE(choice_eval_wtp_all, measurevar="CVchoice", groupvars=c("group", "type"), na.rm = TRUE)
rasumage<-summarySE(choice_eval_wtp_all, measurevar="CVchoice", groupvars= "group", na.rm = TRUE)
rasumtype<-summarySE(choice_eval_wtp_all, measurevar="CVchoice", groupvars="type", na.rm = TRUE)

##line graph of probability of choosing the riskier choice by age group and gamble type with 95% CI bars
ggplot(rasum, aes(x=type, y=CVchoice, color=group))+ geom_point() + geom_errorbar(aes(ymin=CVchoice-ci, ymax=CVchoice+ci, width=.1))+ ylim(.35,.75)
rasum$group

rasum[rasum == 'old'] <- 'Older Adults'
rasum[rasum == 'young'] <- 'Younger Adults'
rasum$group<-factor(rasum$group, levels=c("Younger Adults", "Older Adults"))
rasum$type<-factor(rasum$type, levels=c("monchoice", "medchoice"))

ggplot(rasum, aes(x=group, y=CVchoice, fill=type))+ 
  geom_bar(stat="identity", position= "dodge")+ 
  coord_cartesian(ylim = c(.2,.9)) + 
  geom_errorbar(aes(ymin=CVchoice-se, ymax=CVchoice+se), width=.2, position=position_dodge(.9)) +
  scale_fill_discrete(labels = c("Affect-Poor", "Affect-Rich"), type= c("blue", "red")) + theme_apa()+
  theme(plot.title = element_text(hjust = 0.5)) + theme(text= element_text(size=22), legend.position = "bottom") +
  labs(title= "Risk Aversion", x = NULL, y= "Probability of Choosing the Riskier Option", fill = NULL)

