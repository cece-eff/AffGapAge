#prep data age affect gap CCF 4/30/24

## load required libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(brms)
library(ggplot2)
library(sjPlot)
library(plotrix)
library(lme4)
library(MASS)
library(Rmisc)

#IMPORT CLEANED 
#work computer
merge <- read.csv("/Users/ccf160030/Library/CloudStorage/Box-Box/Analyses/GreysAnatomy/OSFscripts/demo_and_task_data_cleaned.csv")
merge$participant<-as.factor(merge$participant)
merge$group<-as.factor(merge$group)
merge$sename<-as.factor(merge$sename)
merge$amount<-as.factor(merge$amount)

#PREP AFFECTIVE RATINGS
#gather from wide to long format for ratings
ratings<-merge %>% gather(evaltype, rating, moneval, medeval)

#remove nonratings
ratings<- ratings %>% filter(!is.na(rating))

#make evaltype a factor
ratings$evaltype<-as.factor(ratings$evaltype)

ratings<- ratings %>% mutate(sideeffect = ifelse(amount== 'wtp1', 'depression',
                                                 ifelse(amount =='wtp2', 'fever',
                                                        ifelse(amount == 'wtp3', 'memory loss', 
                                                               ifelse(amount == 'wtp4', 'hallucinations', 
                                                                      ifelse(amount =='wtp5', 'itching',
                                                                             ifelse(amount == 'wtp6', 'fatigue', 
                                                                                    ifelse(amount == 'wtp7', 'insomnia',
                                                                                           ifelse(amount == 'wtp8', 'flatulence',
                                                                                                  ifelse(amount == 'wtp9', 'diarrhea',
                                                                                                         ifelse(amount == 'wtp10', 'tremor',
                                                                                                                ifelse(amount == 'wtp11', 'impaired balance',
                                                                                                                       ifelse(amount == 'wtp12', 'slurred speech', as.character(sename))))))))))))))
ratings$sideeffect<-as.factor(ratings$sideeffect)




##PREP CHOICE DATA
##select choice data to analyze
choicedata<- merge %>% dplyr::select(participant, gambno, medchoice, oa1, ob1, monchoice, pa1, pa2, lossa, pb1, pb2, lossb)
choicedata<- choicedata %>% filter(!is.na(gambno))

##replace the words with numbers so we can calculate
choicedata$pa1<-sub('out of 1000','', choicedata$pa1)
choicedata$pa2<-sub('out of 1000','', choicedata$pa2)  
choicedata$pb1<-sub('out of 1000','', choicedata$pb1)  
choicedata$pb2<-sub('out of 1000','', choicedata$pb2)  

#replace _2 so it's just pickA or pickB
choicedata$monchoice<-sub('_2', '', choicedata$monchoice)

#calculate proportion by dividing the number out of 1000 for each probability
choicedata$pa1<-as.numeric(choicedata$pa1)/1000
choicedata$pa2<-as.numeric(choicedata$pa2)/1000
choicedata$pb1<-as.numeric(choicedata$pb1)/1000
choicedata$pb2<-as.numeric(choicedata$pb2)/1000

##replace sideeffect1 with wtp1 and so on
choicedata$oa1<-sub('sideeffect','wtp', choicedata$oa1)
choicedata$ob1<-sub('sideeffect','wtp', choicedata$ob1)
choicedata<-choicedata %>% unite("opta", c("oa1", "lossa"), sep="")
choicedata<-choicedata %>% unite("optb", c("ob1", "lossb"), sep="")

#switch wide to long format for type of trial
choicedata$medchoice<-as.factor(choicedata$medchoice)
choicedata$monchoice<-as.factor(choicedata$monchoice)
choicedata<-choicedata %>% gather(type, choice, medchoice, monchoice)
choicedata<-choicedata %>%filter(choice != "")
#but now there is an extra space after the mongambles wtp values so must remove
choicedata$opta<-trimws(choicedata$opta, which = c("both"))
choicedata$optb<-trimws(choicedata$optb, which = c("both"))
choicedata$gambno<-trimws(choicedata$gambno, which = c("both"))


##isolate affective and monetary evaluations and combine with choice data
#create dfs that have separate ratings for opt1 and opt2
#first isolate opt a
evals_a<- ratings %>% dplyr::select(participant, group, sex, amount, rating)
evals_a$eval_opta<-evals_a$rating
evals_a$wtp_opta<-evals_a$amount 
evals_a$rating<-NULL
evals_a$amount<-NULL

##now with option b, create separate df that just has ratings for each
evals_b<- ratings %>% dplyr::select(participant, group, sex, amount, rating)
evals_b$eval_optb<-evals_b$rating
evals_b$wtp_optb<-evals_b$amount 
evals_b$rating<-NULL
evals_b$amount<-NULL
evals_b$group<-NULL
evals_b$sex<-NULL

#merge ratings with choice data
#merge option a rating with choice data
choiceeval_a<-left_join(choicedata, evals_a, by=c("participant"="participant", "opta"="wtp_opta"))
#merge option2 wtp values with choice data and wtp_a values
choice_eval_all<-left_join(choiceeval_a, evals_b, by=c("participant"="participant", "optb"="wtp_optb"))

#make factors, factors
choice_eval_all$group<-as.factor(choice_eval_all$group)
choice_eval_all$sex<-as.factor(choice_eval_all$sex)
choice_eval_all$type<-as.factor(choice_eval_all$type)

#save clean choice data with evals as separate file
write.csv(choice_eval_all, here::here('choicedata_evals_cleaned.csv'), row.names = FALSE)


### PREP WILLINGNESS TO PAY VALUES
#isolate just the willingness to pay (WTP) values for each participant for comparison.
wtps<-merge %>% gather(wtp, wtpamount, wtp1, wtp2, wtp3, wtp4, wtp5, wtp6, wtp7, wtp8, wtp9, wtp10, wtp11, wtp12)
wtps$wtpamount<-as.numeric(wtps$wtpamount)
wtps$wtp<-as.factor(wtps$wtp)
wtps<- wtps %>% filter(!is.na(wtpamount))
wtps$weighttrial<-as.numeric(wtps$weighttrial)

#people did the WTP values more than once if they couldn't follow instructions so need to make sure we are taking the wtp values from the last time they did it--when it was accepted 
wtpmax<-wtps %>% dplyr::group_by(participant, group) %>% dplyr::summarise(max=max(weighttrial))
wtpmax<-merge(wtps, wtpmax, by= c('participant', 'group'))
wtpmax$lasttrial <- ifelse(wtpmax$max == wtpmax$weighttrial, "1",NA)
wtpmax<-wtpmax %>% filter(!is.na(lasttrial))

#create separate df that just has wtp values for each participant
wtpvalues<-wtpmax %>% dplyr::select(participant, group, wtp, wtpamount)
wtpvalues<- wtpvalues %>% mutate(sideeffect = ifelse(wtp== 'wtp1', 'depression',
                                                     ifelse(wtp =='wtp2', 'fever',
                                                            ifelse(wtp == 'wtp3', 'memory loss', 
                                                                   ifelse(wtp == 'wtp4', 'hallucinations', 
                                                                          ifelse(wtp =='wtp5', 'itching',
                                                                                 ifelse(wtp == 'wtp6', 'fatigue', 
                                                                                        ifelse(wtp == 'wtp7', 'insomnia',
                                                                                               ifelse(wtp == 'wtp8', 'flatulence',
                                                                                                      ifelse(wtp == 'wtp9', 'diarrhea',
                                                                                                             ifelse(wtp == 'wtp10', 'tremor',
                                                                                                                    ifelse(wtp == 'wtp11', 'impaired balance',
                                                                                                                           ifelse(wtp == 'wtp12', 'slurred speech', NA)))))))))))))
wtpvalues$sideeffect<-as.factor(wtpvalues$sideeffect)


#WTP values
#restructure wtpvalues df to allow for combo of both wtp and wtpamount
#create separate df that just has wtp values for each participant separately for options a and b
wtpvalues_a<-wtpmax %>% dplyr::select(participant, group, sex, wtp, wtpamount)
wtpvalues_a$wtp_opta<-wtpvalues_a$wtp
wtpvalues_a$wtpamount_opta<-wtpvalues_a$wtpamount
wtpvalues_a$wtp<-NULL
wtpvalues_a$wtpamount<-NULL

##now with option 2, create separate df that just has wtp values for each participant
wtpvalues_b<-wtpmax %>% dplyr::select(participant, group, sex, wtp, wtpamount)
wtpvalues_b$wtp_optb<-wtpvalues_b$wtp
wtpvalues_b$wtpamount_optb<-wtpvalues_b$wtpamount
wtpvalues_b$wtp<-NULL
wtpvalues_b$wtpamount<-NULL
wtpvalues_b$group<-NULL
wtpvalues_b$sex<-NULL

##MERGE WTP AND CHOICE DATA
#merge option a wtp values with choice + eval data
choice_eval_wtp_a<-left_join(choice_eval_all, wtpvalues_a, by=c("participant"="participant", "opta"="wtp_opta", "sex" ="sex", "group" = "group"))
#merge option2 wtp values with choice data and wtp_a values
choice_eval_wtp_all<-left_join(choice_eval_wtp_a, wtpvalues_b, by=c("participant"="participant", "optb"="wtp_optb"))

#add minus signs!
choice_eval_wtp_all$wtpamount_opta<-choice_eval_wtp_all$wtpamount_opta*-1
choice_eval_wtp_all$wtpamount_optb<-choice_eval_wtp_all$wtpamount_optb*-1

#make factors, factors
choice_eval_wtp_all$group<-as.factor(choice_eval_wtp_all$group)
choice_eval_wtp_all$sex<-as.factor(choice_eval_wtp_all$sex)
choice_eval_wtp_all$type<-as.factor(choice_eval_wtp_all$type)

#save clean choice data as separate file
write.csv(choice_eval_wtp_all, here::here('choicedata_evals_wtp_cleaned.csv'), row.names = FALSE)

##choice values
choice_eval_wtp_all$choice[choice_eval_wtp_all$choice=="pickA"] <- 1
choice_eval_wtp_all$choice[choice_eval_wtp_all$choice=="pickB"] <- 0

##new column for med choice only, first convert wtp into actual side effect
choice_eval_wtp_all[choice_eval_wtp_all=="wtp1"] <- "Depression"
choice_eval_wtp_all[choice_eval_wtp_all=="wtp2"] <- "Fever"
choice_eval_wtp_all[choice_eval_wtp_all=="wtp3"] <- "Memory Loss"
choice_eval_wtp_all[choice_eval_wtp_all=="wtp4"] <- "Hallucinations"
choice_eval_wtp_all[choice_eval_wtp_all=="wtp5"] <- "Itching"
choice_eval_wtp_all[choice_eval_wtp_all=="wtp6"] <- "Fatigue"
choice_eval_wtp_all[choice_eval_wtp_all=="wtp7"] <- "Insomnia"
choice_eval_wtp_all[choice_eval_wtp_all=="wtp8"] <- "Flatulence"
choice_eval_wtp_all[choice_eval_wtp_all=="wtp9"] <- "Diarrhea"
choice_eval_wtp_all[choice_eval_wtp_all=="wtp10"] <- "Tremor"
choice_eval_wtp_all[choice_eval_wtp_all=="wtp11"] <- "Impaired Balance"
choice_eval_wtp_all[choice_eval_wtp_all=="wtp12"] <- "Slurred Speech"

#create numeric numbers for wtp values of options
choice_eval_wtp_all$oa1 <- as.numeric(choice_eval_wtp_all$wtpamount_opta)
choice_eval_wtp_all$ob1 <- as.numeric(choice_eval_wtp_all$wtpamount_optb)

##CALCULATE DECISION QUALITY AND RISK AVERSION SCORES
##calculate EVs and CoVs calculate expected values and coefficients of variation for each gamble for each participant
for (i in 1:nrow(choice_eval_wtp_all)) {
  choice_eval_wtp_all$CV.A[i] <- sqrt((choice_eval_wtp_all$oa1[i]^2*choice_eval_wtp_all$pa1[i]-(choice_eval_wtp_all$oa1[i]*choice_eval_wtp_all$pa1[i])^2)/abs(choice_eval_wtp_all$oa1[i]*choice_eval_wtp_all$pa1[i]))
  choice_eval_wtp_all$CV.B[i] <- sqrt((choice_eval_wtp_all$ob1[i]^2*choice_eval_wtp_all$pb1[i]-(choice_eval_wtp_all$ob1[i]*choice_eval_wtp_all$pb1[i])^2)/abs(choice_eval_wtp_all$ob1[i]*choice_eval_wtp_all$pb1[i]))
  choice_eval_wtp_all$CVhigh[i] <- ifelse(choice_eval_wtp_all$CV.A[i] > choice_eval_wtp_all$CV.B[i], 1, ifelse(choice_eval_wtp_all$CV.A[i] < choice_eval_wtp_all$CV.B[i], 0, NA))
  choice_eval_wtp_all$CVdiff[i] <- choice_eval_wtp_all$CV.A[i] - choice_eval_wtp_all$CV.B[i]
  choice_eval_wtp_all$CVchoice[i] <- (choice_eval_wtp_all$choice[i]==choice_eval_wtp_all$CVhigh[i])*1
  choice_eval_wtp_all$EV.A[i] <- (choice_eval_wtp_all$oa1[i] * choice_eval_wtp_all$pa1[i])
  choice_eval_wtp_all$EV.B[i] <- (choice_eval_wtp_all$ob1[i] * choice_eval_wtp_all$pb1[i])
  choice_eval_wtp_all$EVhigh[i] <- ifelse(choice_eval_wtp_all$EV.A[i] > choice_eval_wtp_all$EV.B[i], 1, ifelse(choice_eval_wtp_all$EV.A[i] < choice_eval_wtp_all$EV.B[i], 0, NA))
  choice_eval_wtp_all$EVdiff[i] <- choice_eval_wtp_all$EV.A[i] - choice_eval_wtp_all$EV.B[i]
  choice_eval_wtp_all$EVlogdiff[i]<-log(abs(choice_eval_wtp_all$EV.A[i] - choice_eval_wtp_all$EV.B[i]))
  choice_eval_wtp_all$EVchoice[i] <- (choice_eval_wtp_all$choice[i]==choice_eval_wtp_all$EVhigh[i])*1
  choice_eval_wtp_all$evaldiff[i]<- abs(choice_eval_wtp_all$eval_opta[i] - choice_eval_wtp_all$eval_optb[i])
}

##Save choice data file with EVs and COVs
write.csv(choice_eval_wtp_all, here::here('choicedata_greys_complete.csv'), row.names = FALSE)


##AFFECTIVE EVALUATION ANALYSES
##run brms testing difference between age groups
bmodel<-brm(rating~evaltype*group + (1|sideeffect) + (1|participant),
            data=ratings, 
            warmup=500, 
            iter=2000, 
            chains=2, 
            init="0", 
            cores=2, 
            seed=123)
summary(bmodel)
sjPlot::plot_model(bmodel, show.values=TRUE)
mcmc_plot(bmodel, type="areas", prob=.95)
#calculate means for ratings by type(monetary and medical evaluations) and group (old, young)
group_eval <- ratings %>% group_by(group, evaltype) %>% summarise(rating = mean(rating, na.rm=TRUE), sd=sd(rating))
group_eval <- ratings %>% dplyr::group_by(group, evaltype) %>% dplyr::summarise(uci=CI(rating)['upper'],lci=CI(rating)['lower'],rating=CI(rating)['mean'])
#calculate means for monetary and medical affective evaluations by participant
mean_eval <- ratings %>% group_by(participant, group, sename, amount) %>% summarise(rating = mean(rating, na.rm=TRUE), sd=sd(rating))
##graph and save
segroup<-ratings %>% dplyr::group_by(group, evaltype, sideeffect) %>% dplyr::summarise(uci=CI(rating)['upper'],lci=CI(rating)['lower'],rating=CI(rating)['mean'])
levels(segroup$evaltype)<- c("Affect-Rich (Side Effect)", "Affect-Poor (Monetary Loss)")
levels(segroup$group)<- c("Older Adults", "Younger Adults")
segroup<-segroup %>% dplyr::rename("Age Group" = group)
ggplot(segroup, aes(x=rating, y=sideeffect, fill=`Age Group`)) + geom_bar(position='dodge', stat='identity')+ labs(x="Affective Rating",  y=NULL)+ facet_wrap(~evaltype ~ .)+scale_fill_manual(values=c("darkslategray4", "darkslategrey"))+ theme_bw(base_size = 24)
ggsave("evalratings.png")

levels(ratings$evaltype)<- c("Affect-Rich (Side Effect)", "Affect-Poor (Monetary Loss)")
levels(ratings$group)<- c("Older Adults", "Younger Adults")
ratings<-ratings %>% dplyr::rename("Age Group" = group)
ggplot(ratings, aes(x=rating, y=sideeffect, fill=`Age Group`)) + geom_boxplot(stat="boxplot", position="dodge2", outliers=TRUE, outlier.shape=1)+ labs(x="Affective Rating",  y=NULL)+ xlim(1, 10)+facet_wrap(~evaltype ~ .)+scale_fill_manual(values=c("darkslategray3", "darkslategrey"))+ theme_bw(base_size = 24)
ggsave("evalratings.png")


#MONETARY EVALUATION ANALYSES
##mean wtp values by side effect
meanwtp <- wtpvalues %>% group_by(sideeffect) %>% dplyr::summarise(se=std.error(wtpamount), sd=sd(wtpamount), wtpamount = mean(wtpamount, na.rm=TRUE))
#first log transform wtp amounts
wtpvalues$logwtpamount<-log(wtpvalues$wtpamount)
##run brms
bwtp<-brm(logwtpamount~group + (1|sideeffect) + (1|participant),
          data=wtpvalues, 
          warmup=500, 
          iter=2000, 
          chains=2, 
          init="0", 
          cores=2, 
          seed=123)
summary(bwtp)
sjPlot::plot_model(bwtp, show.values=TRUE)
mcmc_plot(bwtp, type="areas", prob=.95)
##mean wtp values by group
groupwtp <- wtpvalues %>% group_by(group) %>% dplyr::summarise(uci=CI(wtpamount)['upper'],lci=CI(wtpamount)['lower'],wtpamount=CI(wtpamount)['mean'])
##mean wtp values by side effect and group
groupmeanwtp <- wtpvalues %>% dplyr::group_by(group, sideeffect) %>% dplyr::summarise(uci=CI(wtpamount)['upper'],lci=CI(wtpamount)['lower'],wtpamount=CI(wtpamount)['mean']) 
levels(groupmeanwtp$group)<- c("Older Adults", "Younger Adults")
groupmeanwtp<-groupmeanwtp %>% dplyr::rename("Age Group" = group)
#graph and save
ggplot(groupmeanwtp, aes(x=wtpamount, y=sideeffect, color=`Age Group`, xlower = lci, xupper=uci)) + geom_boxplot(stat="boxplot", position="dodge2")+ labs(x="Willingness to Pay ($)",  y="Side Effect")+scale_color_manual(values=c("darkslategray4", "darkslategrey"))+ theme_bw(base_size = 24)
ggsave("wtp.png")

levels(wtpvalues$group)<- c("Older Adults", "Younger Adults")
wtpvalues<-wtpvalues %>% dplyr::rename("Age Group" = group)
ggplot(wtpvalues, aes(x=wtpamount, y=sideeffect, fill=`Age Group`)) + geom_boxplot(stat="boxplot", position="dodge2", outliers=TRUE, outlier.shape=1)+ xlim(0, 500)+ labs(x="Willingness to Pay ($)",  y="Side Effect")+scale_fill_manual(values=c("darkslategray3", "darkslategrey"))+ theme_bw(base_size = 24)
ggsave("wtp.png")


##PREFERENCE REVERSALS
reverse<-choice_eval_wtp_all[c('participant','gambno','group','type', 'EVchoice')] 
reverse<-reshape(reverse, idvar=c("participant", "gambno", "group"), timevar = "type", direction="wide")
reverse$reversal<-ifelse(reverse$EVchoice.medchoice != reverse$EVchoice.monchoice, 1, 0)

agereversals<-reverse %>% dplyr::group_by(group) %>% dplyr::summarise(rev=mean(reversal, na.rm=TRUE), revsd = sd(reversal, na.rm=TRUE))

bmodel<-brm(reversal~group + (1|participant),
            data=reverse, 
            warmup=500,
            family = bernoulli(),
            iter=2000, 
            chains=2, 
            init="0", 
            cores=2, 
            seed=123)
summary(bmodel)
sjPlot::plot_model(bmodel, show.values=TRUE)

