rm(list=ls())
setwd("~/Projects/WavyHyphae")
#myDatam <- read.csv("data/BR_ManualHyphalDat_032717.csv")
myDatam <- read.csv("data/BR_JV_ManualHyphalDat_032817.csv")
#------------------------------------------------------------
library(ggplot2); library(plyr)

names(myDatam)
newData <- myDatam[(myDatam$PDAConc)=="0.75",]
#on March 07 we started the 0:10 numbering (previously ~0:5)
unique(myDatam$Date)
newData <- newData[newData$Date %in% c("4-Apr","7-Mar","28-Mar","21-Mar", "28-Apr", "10-Jun", "16-Jun", "9-Jul", "14-Jul", "21-Jul", "27-Jul", "5-Aug", "18-Aug"), ]

#write.csv(newData, "BR_JV_ManualHyphalDat_032817.csv")
#remove rows with NA
myFigDatm <- newData[complete.cases(newData),]

names(myFigDatm)
myFigDatb <- ddply(myFigDatm, c("Isolate", "PlateBlock", "Date"), summarise,
                   PlatePheno = mean(Phenotype, na.rm=T))

table(myFigDatb$Isolate)

myFigDat2 <- ddply(myFigDatb, c("Isolate"), summarise,
                  N    = sum(!is.na(PlatePheno)) ,
                  mean = mean(PlatePheno, na.rm=T),
                  sd   = sd(PlatePheno, na.rm=T),
                  se   = sd / sqrt(N))

#reorder bars by mean waviness
myFigDat2 <- transform(myFigDat2, 
                          Isolate = reorder(Isolate, mean))

limits <- aes(ymax = mean + se, ymin=mean - se)
ggplot(myFigDat2, aes(x = factor(Isolate), y = mean))+
  geom_bar(stat="identity", fill="dodgerblue3")+
  theme_bw()+
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 45, hjust = 1))+
labs(y=expression(Mean ~ Wavy ~ Index), x=element_blank())+
  geom_errorbar(limits, width=0.25)

#more exploration
myFigDatb <- transform(myFigDatb, 
                       Isolate = reorder(Isolate, PlatePheno))
plot(myFigDatb$PlatePheno ~ myFigDatb$Isolate)

g <- myFigDatb$PlatePheno
h <- hist(g, breaks=30)
xfit<-seq(min(g),max(g),length=40) 
yfit<-dnorm(xfit,mean=mean(g),sd=sd(g)) 
yfit <- yfit*diff(h$mids[1:2])*length(g) 
lines(xfit, yfit, col="black", lwd=2)

unique(myFigDatb$Isolate)
