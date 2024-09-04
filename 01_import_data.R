#import data age affect gap CCF 4/30/24

# load required packages
library(here)
library(tidyverse)
library(Rmisc)

#import task data
#change to the path to your raw data
path_to_data<- "~/Library/CloudStorage/Box-Box/Analyses/GreysAnatomy/GreysAnatomyData/" 

# read data in, clean, and concatenate into one file
files <- list.files(path_to_data, pattern = ".csv")
dt <- data.frame()
for (f in files) {
  d1 <- read.csv(paste0(path_to_data, f)) # read in data table
  #select which columns we want, in the order we want 
  d1 <- d1[c('participant', 'r1', 'r2', 'r3', 'r4', 'r5', 'r6', 'r7', 'r8', 'r9', 'r10', 'r11',
             'r12', 'ranktryagain.thisN', 'wtp1', 'wtp2', 'wtp3','wtp4', 'wtp5', 'wtp6', 'wtp7',
             'wtp8', 'wtp9', 'wtp10', 'wtp11', 'wtp12', 'weighttryagain.thisN', 'rank1wtp', 'rank2wtp', 
             'rank3wtp', 'rank4wtp', 'rank5wtp', 'rank6wtp', 'rank7wtp', 'rank8wtp', 'rank9wtp',
             'rank10wtp', 'rank11wtp', 'rank12wtp', 'gambno', 'medgamtrials.thisN',	'medgamtrials.thisIndex','mouse_3.clicked_name',
             'oa1', 'ob1', 'mongamtrials.thisN', 'mongamtrials.thisIndex', 'mouse_4.clicked_name','pa1', 'pa2', 'lossa', 'pb1',
             'pb2', 'lossb','slider_3.response', 'sename',  'slider_2.response','amount')]
  dt<- rbind(dt, d1)
}

# rename variables
names(dt)[names(dt) == 'ranktryagain.thisN'] <- 'ranktrial'
names(dt)[names(dt) == 'weighttryagain.thisN'] <- 'weighttrial'
names(dt)[names(dt) == 'mouse_3.clicked_name'] <- 'medchoice'
names(dt)[names(dt) == 'mouse_4.clicked_name'] <- 'monchoice'
names(dt)[names(dt) == 'slider_3.response'] <- 'medeval'
names(dt)[names(dt) == 'slider_2.response'] <- 'moneval'

#save cleaned task data no demographics
write.csv(dt, "~/Library/CloudStorage/Box-Box/Analyses/GreysAnatomy/OSFscripts/task_data_cleaned.csv")

##import demographic data from Prolific
#set path
demo<- read.csv("~/Library/CloudStorage/Box-Box/Analyses/GreysAnatomy/demo_data_cleaned.csv")

#merge with demographics with task data
merge<-merge(demo, dt, by.y = "participant", all.x=TRUE, all.y=TRUE)

##save merged demo/task data
write.csv(merge, "~/Library/CloudStorage/Box-Box/Analyses/GreysAnatomy/OSFscripts/demo_and_task_data_cleaned.csv")
