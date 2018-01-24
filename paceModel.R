#packages
library(tidyverse)
library(dplyr)
library(lme4)

#working directory
#setwd("C:/Users/LauraAcer/Documents/Data Science/MAF")
setwd("O:/Documents/Personal/Projects/Data Science/Drive/MAF")

#load data frame
load("mafRuns.Rda")

#subset of runs for visualisation
set.seed(3)
maf<-transform(maf,id=as.numeric(factor(date)))
ids<-c(1:max(maf$id))
ids<-as.data.frame(ids)
indexes<-sample(1:nrow(ids),size=0.4*nrow(ids))
id<-ids[indexes,]
maf_sub<-as.data.frame(id)
maf_sub<-merge(maf,maf_sub,by="id")

#visualise final data
ggplot(data=maf_sub,aes(x=km,y=pace,group=date,colour=factor(date)))+
  geom_line(size=1)+scale_colour_brewer(palette="Greens","Date")+
  scale_y_time(limits=c(375,450))

#summarise the data
ggplot(data=maf,aes(x=date,y=mean_pace))+
  geom_point(colour=rgb(0,0.69,0.314),size=3)+
  geom_line(colour=rgb(0,0.69,0.314),size=2)+
  scale_y_time()+
  theme(panel.background=element_rect(fill="white"),
        panel.grid=element_line(colour="grey50"))+
  labs(title="Pace across runs",x="Run date",y="Mean pace")
ggsave("pace.png")
ggplot(data=maf,aes(x=date,y=con_slope))+
  geom_point(colour=rgb(0,0.69,0.314),size=3)+
  geom_line(colour=rgb(0,0.69,0.314),size=2)+
  theme(panel.background=element_rect(fill="white"),
        panel.grid=element_line(colour="grey50"))+
  labs(title="Slope across runs",x="Run date",y="Mean slope")
ggsave("slope.png")
ggplot(data=maf,aes(x=date,y=con_hr))+
  geom_point(colour=rgb(0,0.69,0.314),size=3)+
  geom_line(colour=rgb(0,0.69,0.314),size=2)+
  theme(panel.background=element_rect(fill="white"),
        panel.grid=element_line(colour="grey50"))+
  labs(title="Heart rate across runs",x="Run date",y="Mean heart rate")
ggsave("hr.png")
ggplot(data=maf,aes(x=date,y=max_km))+
  geom_point(colour=rgb(0,0.69,0.314),size=3)+
  geom_line(colour=rgb(0,0.69,0.314),size=2)+
  theme(panel.background=element_rect(fill="white"),
        panel.grid=element_line(colour="grey50"))+
  labs(title="Distance of runs",x="Run date",y="Run distance (km)")
ggsave("distance.png")

#mixed linear models
#quadratic and cubic terms to account for slight non-linearity
maf$km2<-maf$km*maf$km
maf$km3<-maf$km2*maf$km
#fit model for training data
#assess random effects
m1<-lmer(pace~hr+slope+km+km2+km3+(1+km|date),data=maf)
#summary
summary(m1)
#predictions
maf$predict<-predict(m1)
ggplot(data=subset(maf,id==12),aes(x=km,group=date,colour=factor(date)))+
  geom_line(aes(y=pace),colour=rgb(0,0.69,0.314))+
  geom_line(aes(y=predict),colour=rgb(0,0.69,0.314),size=2)+
  scale_y_time()

#plot pace for subset of runs if had been constant slope and constant heartrate
maf_st<-maf_sub
maf_st$km2<-maf_st$km*maf_st$km
maf_st$km3<-maf_st$km2*maf_st$km
maf_st$hr<-maf_st$con_hr
maf_st$slope<-maf_st$con_slope
maf_st$pacehat<-predict(m1,maf_st,allow.new.levels=TRUE)

#overall graph
ggplot(data=maf_st,aes(group=date,colour=factor(date)))+
  geom_line(aes(x=km,y=pace),size=2,alpha=0.75)+
  geom_line(aes(x=km,y=pacehat),size=2.5)+
  scale_y_time()+
  scale_colour_manual(values=c(rgb(0.73,0.89,0.7),rgb(0.45,0.77,0.46),rgb(0.19,0.64,0.33),rgb(0,0.43,0.17)))+
  theme(panel.background=element_rect(fill="white"),
        panel.grid=element_line(colour="grey50"))+
  labs(title="Counterfactual MAF pace over distance assuming constant mean heart rate and constant mean slope",
       x="Distance (km)",y="Pace",colour="Date")
ggsave("maf_improve.png")
