#script to estimate proportion sampled from data sheet "GPSSurveys2015.clownfish.csv" 8/3/17 --> MICHELLE SAYS THIS SHEET HAS MANY ERRORS
#set working directory
setwd("~/Documents/GradSchool/parentage")

#load packages
library(dplyr)
library(tidyr)
library(data.table)

#join Amphiprion
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)


#need to use 2015 survey data to get a count of each population
surv <- read.csv(file="GPSSurveys2015.clownfish.csv", header=TRUE)
#load the dive table info to connect to survey
c1 <- collect(leyte %>% tbl("diveinfo") %>% select(divenum, site, date))
#dive sites have inconsistent names. rather than change this in the database, write c1 as a text file, regex fix it. just to save time instead of figuring out an R solution
write.table(c1, file="diveinfoleyte.txt")
c1 <- read.table(file="diveinfoleyte.txt", header=TRUE)
total <- left_join(surv, c1, by=c(DiveNum = "divenum"))
total <- total %>% group_by(DiveNum, ObsTime) %>% filter((row_number() == 1))
total <- total %>% select(DiveNum, date, ObsTime, Spp, site, Size1, Size2, Size3, Size4, Size5, Size6)
#need to tidy the data so that each fish observation is a row, and variables are site, obs time/date, spp, and size, then you can sort and tally based on that
setnames(total, old = c('Size1','Size2', 'Size3', 'Size4', 'Size5', 'Size6'), new = c('one','two', 'three', 'four', 'five', 'six'))
total3 <- as.data.frame(gather(total, fishnum, size, one:six))

#create a data frame with total APCL fish observations for each site
total4 <- total3 %>% filter(size != "NA")
total4 <- total4 %>% filter(Spp == "APCL")

#create a data frame with the total adult of breeding size fish (>=8 cm) per site
totalbig <- subset(total4, size >= 8)
totaladults <- totalbig %>%  count(site) 



#create a data frame with the total juvenile fish (<=8.5 cm) per site
totalsmall <- subset(total4, size <= 8.5)
totaladults <- totalsmall %>%  count(site) 

total4 %>% group_by(site) %>% count(site) 


