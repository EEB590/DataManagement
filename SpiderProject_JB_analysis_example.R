#Transplant and Prey capture analysis
###first created by Jeff Brown 2013###
###updated by Haldre 22Jan2014########
###updated 15Nov2014 #################

#set working directory (change for your own directory)
setwd("~/Box Sync/Iowa State University/Teaching/Rstats/Data Management") 

#load libraries
library(reshape2) #for melt
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)

#may need to install tibble first, and may need to restart R session to clear conflicting packages

#################################
#Research questions: 
#1) Does the duration of time a spider stays on its web differ between island or treatment? 
#using all data
#Total.Days~island*netting, family=poisson (could do this as a hazard rate, but I don't see a reason to)

#2) If a spider is missing, is the web more likely to be present without a spider inhabiting it (indicative of predation) on Saipan than on Guam? 
#using subset of data with spiders missing (omit ones where spiders remained entire time)
#WebPresBin~island*Netting, family=binomial

#Notes about the study
#Transplanting was done in two sites on Guam and two sites on Saipan 

#read in data
transplant<-read.csv("Saipan_Guam_Transplant_asentered.csv")

#look at data
str(transplant)
summary(transplant)

#some data cleaning needed
levels(transplant$Island) <- gsub("Gaum", "Guam", levels(transplant$Island))
transplant$Site<-tolower(transplant$Site)
#what else needs to be cleaned? 

#create column of 1/0 for web present/absent and spider pres/absent
transplant$SpidPresBin[transplant$SpidPres=="no"] <-0 #if spider is absent at end, put 0
transplant$SpidPresBin[transplant$SpidPres=="yes"] <-1 #if spider is present at end, put 1
transplant$WebPresBin[transplant$WebPres=="no"] <-0 #if web is absent, put 0
transplant$WebPresBin[transplant$WebPres=="yes"] <-1 #if web is present, put 1
transplant$WebPresBin[transplant$SpidPres=="yes"] <-NA #adds a NA for all webs where spider was present at the end. 

#check out data again
with(transplant, table(Site, Native))
with(transplant, table(Island, Native))
#what is sample size for the transplanted spiders? 
with(transplant[transplant$Native=="no",], table(Site, Netting))
with(transplant[transplant$Native=="no" & transplant$SpidPres=="no",], table(Netting, Island, WebPres))
#compare ftable to table
with(transplant[transplant$Native=="no" & transplant$SpidPres=="no",], ftable(Netting, Island, WebPres))

#get subset that was actually transplanted rather than ones that were observed in place        
truetrans<-transplant[transplant$Native=="no",]


###########################################
####### Prey Capture Analysis #############
###########################################
#Research Questions
#1) Do webs on Guam capture more prey than webs on Saipan? We want to know whether the number of prey captured varies by island (and thus, by bird presence/absence). 
#Model: preynum~island, family=poisson

#data notes
# obs1, obs2, obs3 etc is when the same web was revisted on different days.
# Jeff took samples at 3 sites per island. I think the only data for preynum comes from the spiders that were transplanted- need to check with him.  

######################
#load data
preycap<-read.csv("preycapture_asentered.csv", header=T)

#totalprey is not raw data - shouldn't be in dataset
preycap<-preycap[,-4] #remove column 4

#Change data from wide to long format
#use reshape package (old way)
preycapL <- melt(preycap, id.vars=c("island","site","web"),                                 measure.vars=c("obs1", "obs2", "obs3","obs4","obs5", "obs6", "obs7", "obs8"),   variable.name="obs", value.name="preynum")

#Alternative method - use tidyr (new way- much more efficient code!)
preycapL<-gather(preycap, "obs", "preynum", 4:11)

#write.csv(preycapL,"preycapturelong.csv")

#####Clean up data ############
summary(preycapL)# Look at data
str(preycapL) #check to make sure factors are factors and numbers are numeric or integer
preycapL$preynum<-as.numeric(preycapL$preynum)
#what other components should be in a different class? 

#clean data entry errors
levels(preycapL$site) <- gsub("forbid", "forbi", levels(preycapL$site))
preycapL$site<-tolower(preycap$site)

#Look at response (preynum)
summary(preycapL$preynum) #where are all these NA's coming from? webs that were not observed multiple times - not useful data. 
preycapL<-preycapL[!is.na(preycapL$preynum),]

#####Look at data ############
table(preycapL$preynum,preycapL$island) #shows preynum by island-
with(preycapL, table(obs, site)) #concerns me that no obs2 through obs8 data on Guam- i.e. no data from transplanted spiders.
#see how preynum varies by site
with(preycapL, table(preynum, site))

#some exploratory graphs
plot(preycapL$preynum~preycapL$obs)

boxplot(preycapL$preynum~ preycapL$island, varwidth=T)

ggplot(preycapL, aes(x=site, y=preynum, fill=island))+
  geom_bar(stat="identity") #shows sum of all counts per site. Not the best graph, but may be useful for identifying any errors. 

ggplot(preycapL, aes(x=site, y=preynum, fill=island))+
  geom_boxplot() #shows boxplot of prey captured per site

