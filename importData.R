#packages
library(tidyverse)

#working directory
setwd("C:/Users/LauraAcer/Documents/Data Science/MAF")

#read in data
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

#create list of all data
runlist<-list(aug16,aug17,aug18,aug22,aug24,aug26,aug28,aug30,
             sep13,sep15,sep16,sep17)
runlist<-lapply(runlist,"[",
               c("date","secs","km","hr","slope"))
runlist<-lapply(seq_along(runlist),
               function(i){
                 runlist[[i]]$pace<-runlist[[i]]$secs/runlist[[i]]$km
                 runlist[[i]]$con_hr<-mean(runlist[[i]]$hr)
                 runlist[[i]]$con_slope<-mean(runlist[[i]]$slope)
                 runlist[[i]]$max_km<-max(runlist[[i]]$km)
                 return(runlist[[i]])
               }
)

#visualise individually
ggplot(data=runlist[[1]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[1]],aes(x=secs,y=hr))+geom_line()
ggplot(data=runlist[[2]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[2]],aes(x=secs,y=hr))+geom_line()
ggplot(data=runlist[[3]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[3]],aes(x=secs,y=hr))+geom_line()
ggplot(data=runlist[[4]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[4]],aes(x=secs,y=hr))+geom_line()
ggplot(data=runlist[[5]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[5]],aes(x=secs,y=hr))+geom_line()
ggplot(data=runlist[[6]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[6]],aes(x=secs,y=hr))+geom_line()
ggplot(data=runlist[[7]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[7]],aes(x=secs,y=hr))+geom_line()
ggplot(data=runlist[[8]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[8]],aes(x=secs,y=hr))+geom_line()
ggplot(data=runlist[[9]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[9]],aes(x=secs,y=hr))+geom_line()
ggplot(data=runlist[[10]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[10]],aes(x=secs,y=hr))+geom_line()
ggplot(data=runlist[[11]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[11]],aes(x=secs,y=hr))+geom_line()
ggplot(data=runlist[[12]],aes(x=secs,y=pace))+geom_line()+scale_y_time()
ggplot(data=runlist[[12]],aes(x=secs,y=hr))+geom_line()
#see there is not consistency in the shape
#need to remove warm up where one occurred
#can be fairly systematic
#add systematic rule:
#fastest pace onwards
#bottom of heart rate range
#whichever is earliest

#apply rule
runlist<-lapply(seq_along(runlist),
                function(i){
                  runlist[[i]]$minpace<-match(min(runlist[[i]]$pace),
                                              runlist[[i]]$pace)
                  runlist[[i]]$minhrr<-match(143,floor(runlist[[i]]$hr))
                  runlist[[i]]$minpacei<-ifelse(
                    runlist[[i]]$secs>=runlist[[i]]$minpace,1,0)
                  runlist[[i]]$minhrri<-ifelse(
                    runlist[[i]]$secs>=runlist[[i]]$minhrr,1,0)
                  return(runlist[[i]])
                }
)
#main dataset
maf<-do.call("rbind",runlist)
maf<-subset(maf,maf$minhrri==1 & maf$minpacei==1)
maf<-maf[c("date","secs","km","hr","slope","pace",
           "con_hr","con_slope","max_km")]
maf<-maf[order(maf$date,maf$secs),]

#save
save(maf,file="mafRuns.Rda")
