#MAF visualisation
library(tidyverse)
library(lubridate)
library(chron)
library(hms)
library(lme4)
library(dplyr)
library(plyr)

#working directory
setwd("C:/Users/LauraAcer/Documents/Data Science/MAF")

#initial data frames
aug16<-read_csv("2017_08_16_08_21_42.csv")
aug16$date<-as.Date("2017_08_16","%Y_%m_%d")
aug17<-read_csv("2017_08_17_08_42_56.csv")
aug17$date<-as.Date("2017_08_17","%Y_%m_%d")
aug18<-read_csv("2017_08_18_08_41_26.csv")
aug18$date<-as.Date("2017_08_18","%Y_%m_%d")
aug22<-read_csv("2017_08_22_08_44_59.csv")
aug22$date<-as.Date("2017_08_22","%Y_%m_%d")
aug24<-read_csv("2017_08_24_12_56_14.csv")
aug24$date<-as.Date("2017_08_24","%Y_%m_%d")
aug26<-read_csv("2017_08_26_17_30_07.csv")
aug26$date<-as.Date("2017_08_26","%Y_%m_%d")
aug28<-read_csv("2017_08_28_09_53_26.csv")
aug28$date<-as.Date("2017_08_28","%Y_%m_%d")
aug30<-read_csv("2017_08_30_08_57_02.csv")
aug30$date<-as.Date("2017_08_30","%Y_%m_%d")
sep13<-read_csv("2017_09_13_11_05_18.csv")
sep13$date<-as.Date("2017_09_13","%Y_%m_%d")
sep15<-read_csv("2017_09_15_10_29_23.csv")
sep15$date<-as.Date("2017_09_15","%Y_%m_%d")
sep16<-read_csv("2017_09_16_10_47_44.csv")
sep16$date<-as.Date("2017_09_16","%Y_%m_%d")
sep17<-read_csv("2017_09_17_17_35_00.csv")
sep17$date<-as.Date("2017_09_17","%Y_%m_%d")

#manipulations on all data frames
df<-rbind(aug16,aug17,aug18,aug22,aug24,aug26,aug28,aug30,
          sep13,sep15,sep16,sep17)
df<-df[c("date","secs","km","hr","slope")]
df$mins<-df$secs/60
df$pace<-df$secs/df$km
df$consthr<-ave(df$hr,df$date)
df$constslope<-ave(df$slope,df$date)
df$maxkm<-ave(df$km,df$date,FUN=max)
#sort and save edited data
edf<-df[order(df$date,df$secs),]

edf<-subset(edf,edf$secs>300)
#visualise data
ggplot(data=edf,aes(x=km,y=pace,group=date,colour=factor(date)))+
  geom_line(size=1.5)+scale_colour_brewer(palette="Greens","Date")
#save unedited dataframe
save(edf,file="mafRunsUnedited.Rda")


#start all runs at the time when MAF heartrate is attained
#i.e. formal start to the MAF run
fdf<-subset(edf,((date==as.Date("2017-08-16") & edf$secs>405) | 
                   (date==as.Date("2017-08-17") & edf$secs>435) |
                   (date==as.Date("2017-08-18") & edf$secs>600) |
                   (date==as.Date("2017-08-22") & secs>600 & secs<1600) |
                   (date==as.Date("2017-08-24") & secs>600) |
                   (date==as.Date("2017-08-26") & secs>720) |
                   (date==as.Date("2017-08-28") & secs>600) |
                   (date==as.Date("2017-08-30") & secs>720) |
                   (date==as.Date("2017-09-13") & secs>300 & secs<2360) |
                   (date==as.Date("2017-09-15") & secs>600) |
                   (date==as.Date("2017-09-16") & secs>600) |
                   (date== as.Date("2017-09-17") & secs>600)) & 
              abs(edf$slope<5))
#visualise edited data
ggplot(data=fdf,aes(x=km,y=pace,group=date,colour=factor(date)))+
  geom_line(size=1.5)+scale_colour_brewer(palette="Greens","Date")

save(fdf,file="mafRuns.Rda")
