#packages
library(tidyverse)
library(dplyr)
library(lme4)

#working directory
setwd("C:/Users/LauraAcer/Documents/Data Science/MAF")

#load data frame
load("mafRuns.Rda")

#training set
maf<-transform(maf,id=as.numeric(factor(date)))
ids<-c(1:max(maf$id))
ids<-as.data.frame(ids)
indexes=sample(1:nrow(ids),size=0.4*nrow(ids))
id<-ids[indexes,]
maf_test<-as.data.frame(id)
maf_test<-merge(maf,maf_test,by="id")
id<-ids[-indexes,]
maf_train<-as.data.frame(id)
maf_train<-merge(maf,maf_train,by="id")

#visualise final data
ggplot(data=maf_train,aes(x=km,y=pace,group=date,colour=factor(date)))+
  geom_line(size=1)+scale_colour_brewer(palette="Greens","Date")+
  scale_y_time(limits=c(360,480))

#mixed linear models
#fit model for training data
#assess random effects
m1<-lmer(pace~hr+slope+km+(1+km|date),REML=FALSE,data=maf_train)
#summary
summary(m1)
#predictions
maf_train$predict<-predict(m1)
ggplot(data=subset(maf_train,id==12),aes(x=km,group=date,colour=factor(date)))+
  geom_line(aes(y=pace),colour="darkgreen")+
  geom_line(aes(y=predict),colour="darkgreen",size=2)+
  scale_y_time()

#predict pace for test data
maf_test$hr<-maf_test$con_hr
maf_test$slope<-maf_test$con_slope
maf_test$pacehat<-predict(m1,maf_test,allow.new.levels=TRUE)

#overall graph
ggplot(data=maf_test,aes(group=date,colour=factor(date)))+
  geom_line(aes(x=km,y=pace),alpha=0.7)+
  geom_line(aes(x=km,y=pacehat),size=1.5)+
  scale_y_time()+
  scale_colour_brewer(palette="Greens")+
  labs(title="Counterfactual MAF pace over distance assuming constant mean heart rate and constant mean slope",
       x="Distance (km)",y="Pace",colour="Date")
