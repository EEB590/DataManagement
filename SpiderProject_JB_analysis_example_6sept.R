#Transplant and Prey capture analysis
###first created by Jeff Brown 2013###
###updated by Haldre 22Jan2014########
## Updated for R stats class in Sept 2016 

#load libraries
library(reshape2) #for melt - note that many reshape2 functions are now available using tidyr, so probably best to use just tidyr
library(tidyr)
library(dplyr) #note that many functions are similar for plyr and dplyr but have slightly different syntax in dplyr. Use the newer one (dplyr) to avoid issues between two packages. 
library(ggplot2)
library(lubridate)

#may need to install tibble first, and may need to restart R session to clear conflicting packages

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
preycapL<-gather(preycap, "obs", "preynum", 4:11, factor_key=T)


###############################
#####Clean up data ############
###############################
summary(preycapL)# Look at data
str(preycapL) #check to make sure factors are factors and numbers are numeric or integer
preycapL$preynum<-as.numeric(preycapL$preynum)

with(preycapL, table(site, obs))

#delete "obs" from start of variable describing observation number. There are a lot of different ways of doing this. Here are a few possible methods. 
#this uses gsub to substitute all instances of "obs" with nothing (""). Note that gsub works on character vectors. If a vector is not a character, it will be coerced into being a character vector by gsub. 
preycapL$obs <- gsub("obs", "", preycapL$obs)
#with(preycapL, table(site, obs2)) - checked to make sure was correct. 

#\\D means select all non-digits, and then substitute with nothing ("")
preycapL$obs <- gsub("\\D", "", preycapL$obs)
#Look at this website for other codes for substituting in gsub 
# http://www.endmemo.com/program/R/gsub.php 

#using substr- this is saying "keep the 4th element (start at 4, stop at 4)". 
preycapL$obs<-substr(preycapL$obs, 4, 4)

#all three of these methods would work to remove the "obs". Now we need to change obs from character to numeric. 

#Remember to change to numeric vector if you use gsub, because the output from gsub will be a character vector
preycapL$obs<-as.numeric(preycapL$obs)
#should any other components be in a different class? 

#clean data entry errors, starting with site
levels(as.factor(preycapL$site))
#first of all, have some upper and some lower case. Change all to lower case
preycapL$site<-as.factor(tolower(preycap$site))

#now have issues with the "forbi" site, which has a data entry error
#can fix this a bunch of ways, one option is with gsub
preycapL$site <- gsub("forbid", "forbi",preycapL$site)
#another method using indexing
levels(preycapL$site)[levels(preycapL$site)=="forbid"] <- "forbi"
levels(preycapL$site)[levels(preycapL$site)=="ritd"] <- "ritd"
#can you think of any other options? 

#check class again
str(preycapL)
preycapL$site<-as.factor(preycapL$site)

#Your turn- do the same thing for island - find mistakes, and fix them!

#Look at response (preynum)
summary(preycapL$preynum) #where are all these NA's coming from? webs that were not observed multiple times - not useful data. 
preycapL<-preycapL[!is.na(preycapL$preynum),]
preycapL3<-na.omit(preycapL) #deletes entire row if NA is present in any cell in that row

#One final look to see if everything is in right format
str(preycapL)
summary(preycapL)

#####Look at data ############
#shows preynum by island
table(preycapL$preynum,preycapL$island) 
#this code does the same thing, here showing observations by site
with(preycapL, table(obs, site)) #concerns me that no obs2 through obs8 data on Guam- i.e. no data from transplanted spiders.

#see how preynum varies by site
with(preycapL, table(preynum, site))

#some exploratory graphs
#show the number of prey in webs by island using boxplots
boxplot(preycapL$preynum~ preycapL$island, varwidth=T)
#show the number of prey in webs by site using boxplot
boxplot(preycapL$preynum~ preycapL$site, varwidth=T)

#use ggplot
ggplot(preycapL, aes(x=site, y=preynum, fill=island))+
  geom_boxplot() #shows boxplot of prey captured per site



#######################################
### Spider Transplant Experiment ######
#######################################
#Research questions: 
#1) Does the duration of time a spider stays on its web differ between island or treatment? 
#using all data

#2) If a spider is missing, is the web more likely to be present without a spider inhabiting it (indicative of predation) on Saipan than on Guam? 
#using subset of data with spiders missing (omit ones where spiders remained entire time)

#Notes about the study
#Transplanting was done at two sites on Guam and two sites on Saipan

#read in data
transplant<-read.csv("Saipan_Guam_Transplant_asentered.csv")

#data dictionary: 
#island = island where study was conducted, Guam or Saipan
#site = site where study was conducted
#web = Jeff's naming convention for webs, each web has a unique ID
#native = was this spider found in place (yes) or moved to the location (no)?
#netting = was this spider in a netting treatment? 
#startdate = date experiment started
#enddate = last date spider was checked (note after spider disappeared, checks stopped)
#spidpres = was spider present on enddate? 
#webpres = was web present on enddate? 
#websize = how big was the web (cm)?

#look at data
str(transplant)
summary(transplant)

#rename columns so they are lower case. There are a bunch of ways to do this. Here is one using the rename function in the dplyr package (note that this function has slightly different syntax than the one in the ddply package)
transplant<-rename(transplant, island=Island, site=Site, web=Web.., native=Native, netting=Netting, startdate=Start.Date, enddate=End.Date, totaldays=Total.Days, spidpres=SpidPres, webpres=WebPres, websize=WebSize.cm.)

#some data cleaning needed
levels(transplant$island) <- gsub("Gaum", "Guam", levels(transplant$island))
levels(transplant$island) <- gsub("Siapan", "Saipan", levels(transplant$island))
transplant$site<-as.factor(tolower(transplant$site))
transplant$island<-as.factor(tolower(transplant$island))

# remove trailing whitespace
levels(transplant$site)
transplant$site<-as.factor(trimws(transplant$site))
#there are many other methods to do this as well. 

######### Dates ###################
#change date format to standard yyyymmdd format
#helpful site: https://www.r-bloggers.com/date-formats-in-r/

levels(as.factor(transplant$startdate))
class(transplant$startdate)

#problems- missing year. Date format is okay, but wnat to change to a more standard yyyymmdd format

transplant$year<-2013 #this experiment happened in 2013. Added this.
#change both startdate & year to character
transplant$year<-as.character(transplant$year)
transplant$startdate<-as.character(transplant$startdate)
#combine using unite in tidyr package
transplant<-unite(transplant, startdate, c(startdate, year), sep="-")
#now, tell R startdate is a real date in a specific format
transplant$startdate<-as.Date(as.character(transplant$startdate), "%d-%b-%Y")
class(transplant$startdate) #now R sees startdate2 as a Date


#repeat for end date, using lubridate instead of as.Date
transplant$year<-2013 #this experiment happened in 2013. Added this.
#change both startdate & year to character
transplant$year<-as.character(transplant$year)
transplant$enddate<-as.character(transplant$enddate)
transplant<-unite(transplant, enddate, c(enddate, year), sep="-")

#challenge: try changing to date with lubridate package (instead of as.Date)
library(lubridate)
today() #gives today's date
transplant$enddate<-dmy(transplant$enddate)

#Now, create a new column called duration
transplant$duration<-transplant$enddate-transplant$startdate

######### Merging ###################
#merge with another database
#there are several functions that merge, including "join" in the plyr package, "merge" in the base package, and various join functions in the dplyr package. We're going to use dplyr, but each of these functions quite similarly. 

island_bird<-read.csv("island_birdstatus.csv")
str(island_bird)
levels(island_bird$island)

#guidelines: a is one table, b is another table, x1 is the column they share (could be multiple columns)
# left_join(a, b, by = "x1") # Join matching rows from b to a; keeps all columns from both a and b. 
# right_join(a, b, by = "x1") #Join matching rows from a to b; keeps all columns from both a and b. 
# inner_join(a, b, by = "x1") #Join data. Retain only rows that are in both sets; keeps all columns in both dataframes. 
# full_join(a, b, by = "x1") #Join data. Retain all values, all rows & columns.

#before you run this, guess how many rows and columns will be in each new dataframe
transplant2<-left_join(transplant, island_bird, by="island")
transplant3<-right_join(transplant, island_bird, by="island")
transplant4<-inner_join(transplant, island_bird, by="island")
transplant5<-full_join(transplant, island_bird, by="island")

##
#these two functions are slightly different- they don't really join, but allow you to keep only subsets of your main dataframe based on what is either in or not in another dataframe. 
# semi_join(a, b, by="x1") #returns all rows in a that have a match in b, while keeping only the columns in a. This is a way to filter out your data based on a look-up table, for example. 
# anti_join (a, b, by="x1") #filter out so you retain all rows in a that do not have a match in b, keeping only columns in a

transplant6<-semi_join(transplant, island_bird, by="island")
transplant7<-anti_join(transplant, island_bird, by="island")

#If your columns have different names in each database, just give this information to the join function in an argument: by = c("a" = "b") will match a column named "a" in your "x" dataframe to a column named "b" in your "y" dataframe.
transplant2<-left_join(transplant, island_bird, by=c("island"="island")) #example

######### Subsetting #############
#get subset that was actually transplanted rather than ones that were observed in place. 
#first option- use square brackets
truetrans<-transplant[transplant$native=="no",]
#alternatively, use subset function in base 
truetrans2<-subset(transplant, native=="no")
#alternatively, use filter in dplyr package
truetrans3<-filter(transplant, native=="no")

#subset on multiple arguments
filter(transplant, site=="ladt" | site=="forbi")
filter(transplant, site=="ladt" & netting=="no")

########################
# Select - use to select columns from a dataframe to include or drop. 
#here, remove total days because it is not raw data, but calculated data, so shouldn't be in final dataset
transplant2<-select(transplant2, -totaldays)
select(transplant2, site, native, netting, duration) #keep just these 4 columns

########################
#separating one column into two columns
transplant2<-separate(transplant2, col=web, into=c("web_a", "web_b"), sep="'", remove=F)

#combining two columns into one column (note the underscore after unite)
transplant2<-unite_(transplant2, "webv2", c("web_a", "web_b"), sep="", remove=T)
#######################

#write csv file with final, cleaned dataset that we want to work with in the future. In this case, we want the full transplant dataset that has been merged with island. 

write.csv(transplant2, "transplant2.csv") #this will automatically be saved in the project folder. 
