rm(list=ls())
setwd("~/Projects/SideProjects/data/labInterns")
setwd("~/Documents/GitRepos/SideProjects/data/LabIntern")
myDatam <- read.csv("BR_ManualHyphalDat_032717.csv")

names(myDatam)
newData <- myDatam[(myDatam$PDAConc)=="0.75",]
#on March 07 we started the 0:10 numbering (previously ~0:5)
unique(myDatam$Date)
newData <- newData[newData$Date %in% c("4-Apr","7-Mar","28-Mar","21-Mar", "28-Apr", "10-Jun", "16-Jun", "9-Jul", "14-Jul", "21-Jul", "27-Jul", "5-Aug", "18-Aug"), ]

write.csv(newData, "BR_JV_ManualHyphalDat_032817.csv")
#remove rows with NA
myFigDatm <- newData[complete.cases(newData),]

head(myFigDatm)
table(myFigDatm$Isolate)/5

library(plyr)
myFigDat2 <- ddply(myFigDatm, c("Isolate"), summarise,
                  N    = sum(!is.na(Phenotype)) ,
                  mean = mean(Phenotype, na.rm=T),
                  sd   = sd(Phenotype, na.rm=T),
                  se   = sd / sqrt(N))

#reorder bars by mean waviness
myFigDat2 <- transform(myFigDat2, 
                          Isolate = reorder(Isolate, mean))

limits <- aes(ymax = mean + se, ymin=mean - se)
library(ggplot2)
ggplot(myFigDat2, aes(x = factor(Isolate), y = mean))+
  geom_bar(stat="identity", fill="dodgerblue3")+
  theme_bw()+
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 45, hjust = 1))+
labs(y=expression(Mean ~ Wavy ~ Index), x=element_blank())+
  geom_errorbar(limits, width=0.25)

myFigDatm[myFigDatm$Isolate=="2.04.17",]
