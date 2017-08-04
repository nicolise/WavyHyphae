#Created on R by Josue Vega Jul 2017
#Current Purpose
  #Individual Scripts for Data Analysis of Hyphal Waviness

library(readr)
mydata <- read_csv("C:/Users/vegaj/Desktop/BR_JV_ManualHyphalDat_032817.csv") #finds and opens csv file
mydata <- read.csv("~/Projects/WavyHyphae/data/BR_JV_ManualHyphalDat_032817.csv")
recodedate <- read.csv("~/Projects/WavyHyphae/data/RecodeDate.csv")
library(dplyr)
library(ggplot2)
library(tidyverse)
#check library open first

#Phenotype (1-10) ->
  #10 = shortest wavelength
  #1 = no waviness (longest wavelength)
#PlateBlock - 5 Blocks per plate
#DotRep - Numbering order of PlateBlock spore
#Rep - PlateBlock + DotRep (Concatinated)


ls(mydata) #lists variables
###moved this step up in the analysis
mydata$Date <- paste(mydata$Date, "-16", sep="") 
### and added this:
mydata <- merge(mydata, recodedate, by="Date")
### check that it worked:
unique(mydata$NewDate)
### and recode it as a date variable in R
### NOTE: this is optional. it will scale your observations depending on how distant the days were (as opposed to an evenly-spaced axis that just lists all dates in order)
mydata$NewDate <- as.Date(mydata$NewDate)

mydata$Rep <- paste(mydata$PlateBlock, mydata$DotRep) #finishes Rep by concatinating PlateBlock+DotRep

#Creates table of averages under PlateBlock (mean, min, max, sd, n)
plateBlock<- mydata %>% 
  group_by(PlateBlock) %>%
    summarise(avg_pheno = mean(Phenotype, na.rm = TRUE), 
              min_pheno = min(Phenotype, na.rm = TRUE), 
              max_pheno = max(Phenotype, na.rm = TRUE),
              sd_pheno = sd(Phenotype, na.rm = TRUE),
              total = n())

#Creates table of averages under Isolate (mean, min, max, n)
isolate <- mydata %>% 
  group_by(Isolate) %>%
  summarise(avg_pheno = mean(Phenotype, na.rm = TRUE), 
            min_pheno = min(Phenotype, na.rm = TRUE), 
            max_pheno = max(Phenotype, na.rm = TRUE),
            sd_pheno = sd(Phenotype, na.rm = TRUE),
            total = n())

#Creates table of averages under Date (mean, min, max, n)
### modified this to use the NewDate variable
date <- mydata %>% 
  group_by(NewDate) %>%
  summarise(avg_pheno = mean(Phenotype, na.rm = TRUE), 
            min_pheno = min(Phenotype, na.rm = TRUE), 
            max_pheno = max(Phenotype, na.rm = TRUE),
            sd_pheno = sd(Phenotype, na.rm = TRUE),
            total = n())



dts[rev(order(as.Date(dts, format = "%m-%d-%Y")))]
as.Date(date$Date)

#Plotting by Date but cannot get order
###recoded this to use NewDate
p <- ggplot(date, aes(x=(NewDate), y=avg_pheno, fill=NewDate)) + 
  geom_bar(colour="black", stat="identity") + 
  theme_bw()+ ### I added this- it looks cleaner than the default grey background
  xlab("Date") + ylab("Average Phenotype (1 - 10)") +
  ggtitle("Average Hyphal Phenotype based on Date") + 
  geom_errorbar(aes(ymin = avg_pheno-sd_pheno, ymax = avg_pheno+sd_pheno), width=.2, position=position_dodge(.9)) +        guides(fill=FALSE) + #for removal of the legend
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
print(p)

#Graph of Isolate p
p <- ggplot(isolate, aes(x=(Isolate), y=avg_pheno, fill=Isolate)) + 
  geom_bar(colour="black", stat="identity") + 
  xlab("Isolate") + ylab("Average Phenotype (1 - 10)") +
  ggtitle("Average Hyphal Phenotype based on Isolate") + 
  geom_errorbar(aes(ymin = avg_pheno-sd_pheno, ymax = avg_pheno+sd_pheno), width=.2, position=position_dodge(.9)) +        guides(fill=FALSE) + #for removal of the legend
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
print(p)

#Graphs Isolate in order Average Hyphal Phenotype compared to Isolate in Increasing

###look at the function transform {base} and "reorder" within that to work on sorting by isolate and by date

#Finished Graph of Phenotype based on Isolate
p <- ggplot(isolate, aes(x=(Isolate), y=avg_pheno, fill=Isolate)) + 
  geom_bar(colour="black", stat="identity") +
  xlab("Isolate") + ylab("Average Phenotype (1 - 10)") +
  ggtitle("Average Hyphal Phenotype based on Isolate") + 
  geom_errorbar(aes(ymin = avg_pheno-sd_pheno, ymax = avg_pheno+sd_pheno), width=.2, position=position_dodge(.9)) +        guides(fill=FALSE) + #for removal of the legend
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
print(p)

#Calculating One Way ANOVA (dep=Pheno, indep=Isolate
hyphalAOV = aov(Phenotype ~ Isolate, mydata)
summary(hyphalAOV)
TukeyHSD(hyphalAOV)

#Isolating Information from AOV

###before running ANOVA: check assumptions of A) Normality and B) Homoskedasticity for your variables
### check my script BasicANOVA.R to see how to do this, and add those into your script here
names(mydata)

### exclude DotRep. This is jsut a way of counting observations within a single plate
fullmod <- aov(Phenotype ~ Isolate + PlateBlock + DotRep + Date, data = mydata) #all
fullmod

###next, try expanding your model. See README.txt for ideas on what to include there.

lm(Phenotype ~ Isolate + Date, mydata) # Isolate and Date as seperate factors
lm(Phenotype ~ Isolate * Date, mydata) #date and isolate interacting
summary(aov(Phenotype ~ Isolate * Date, mydata))
