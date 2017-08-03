#to calculate the number of fish were sampled and then processed in each field season, for estimates of proportion sampled
#set working directory
setwd("~/Documents/GradSchool/parentage")

#load packages
library(dplyr)
#join Amphiprion
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
#create potential adults file
c1 <- leyte %>% tbl("diveinfo") %>% select(id, date, name)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, ObsTime)
c3 <- left_join(c2, c1, by = c(dive_table_id = "id"))
c4 <- tbl(leyte, sql("SELECT fish_table_id, anem_table_id, Sample_ID, Size FROM clownfish where Sample_ID is not NULL"))
first <- collect(left_join(c4, c3, by = "anem_table_id"))

c1 <- leyte %>% tbl("clownfish") %>% select(size, sample_id, color, fish_spp)
c2 <-collect(c1)
clowns<- filter(c2, Spp=="APCL")
clownA <- filter(clowns, size >= "8")

#get rid of rows with NA in sample_id
adultID <-filter(clownA, sample_id != "NA")
#now that I have adult IDs, need to connect these to ligation IDs

c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
c5 <- collect(left_join(c4, c3, by = "digest_id"))
adults <- inner_join(adultID, c5, by= "sample_id")
adults2 <- inner_join(adults, first, by=c(sample_id = "Sample_ID"))
adults12 <-adults2 %>% filter(date >= as.Date("2012-04-12") & date <= as.Date("2012-07-31"))
#get rid of rows with NA in sample_id
adults12 <-filter(adults12, sample_id != "NA")
#get rid of repeats
adults12 <- adults12 %>% group_by(sample_id) %>% filter((row_number() == 1))


#now want to create juveniles file
c1 <- leyte %>% tbl("clownfish") %>% select(size, sample_id, col, Spp)
c2 <-collect(c1)
clowns<- filter(c2, Spp=="APCL")
clownJ <- anti_join(clowns, adults2, by = "sample_id")
juvs <- left_join(clownJ, c5, by= "sample_id")
juvs2 <- left_join(juvs, first, by=c(sample_id = "Sample_ID"))

#YEAR 2012
#get rid of rows with NA in sample_id
juvs12 <-juvs2 %>% filter(date > as.Date("2012-04-12") & date < as.Date("2012-07-12"))
juvID12 <-filter(juvs12, sample_id != "NA")
#get rid of repeats
juv12 <- juvID12 %>% group_by(sample_id) %>% filter((row_number() == 1))




#YEAR 2013
adults13 <-adults2 %>% filter(date > as.Date("2013-04-12") & date < as.Date("2013-07-12"))
#get rid of rows with NA in sample_id
adults13 <-filter(adults13, sample_id != "NA")
#get rid of repeats
adults13 <- adults13 %>% group_by(sample_id) %>% filter((row_number() == 1))

juvs13 <-juvs2 %>% filter(date > as.Date("2013-04-12") & date < as.Date("2013-07-12"))
#get rid of rows with NA in sample_id
juvID13 <-filter(juvs13, sample_id != "NA")
#get rid of repeats
juv13 <- juvID13 %>% group_by(sample_id) %>% filter((row_number() == 1))


#YEAR 2014
adults14 <- adults2 %>% filter(date > as.Date("2014-04-12") & date < as.Date("2014-07-12"))
#get rid of rows with NA in sample_id
adults14 <-filter(adults14, sample_id != "NA")
#get rid of repeats
adults14 <- adults14 %>% group_by(sample_id) %>% filter((row_number() == 1))

juvs14 <-juvs2 %>% filter(date > as.Date("2014-04-12") & date < as.Date("2014-07-12"))
#get rid of rows with NA in sample_id
juvID14 <-filter(juvs14, sample_id != "NA")
#get rid of repeats
juv14 <- juvID14 %>% group_by(sample_id) %>% filter((row_number() == 1))



#YEAR 2015
adults15 <-adults2 %>% filter(date > as.Date("2015-04-12") & date < as.Date("2015-07-12"))
#get rid of rows with NA in sample_id
adults15 <-filter(adults15, sample_id != "NA")
#get rid of repeats
adults15 <- adults15 %>% group_by(sample_id) %>% filter((row_number() == 1))

juvs15 <-juvs2 %>% filter(date > as.Date("2015-04-12") & date < as.Date("2015-07-12"))
#get rid of rows with NA in sample_id
juvID15 <-filter(juvs15, sample_id != "NA")
#get rid of repeats
juv15 <- juvID15 %>% group_by(sample_id) %>% filter((row_number() == 1))




#estimate local retention
dim(juv12) #188 offspring sampled
dim(juv13) #526 offspring sampled
dim(juv14) #410 offspring sampled
dim(juv15) #401 offspring sampled

#2012
1/188

#2013
7/526

#2014
8/410

#2015



#number and percentage of siblings
fullsib <- read.table(file= "174HQloci_adult_sep_2013WP1.FullSibDyad.txt", header= TRUE)
halfsib <- read.table(file= "174HQ_parentage_2014.HalfSibDyad.txt", header= TRUE)
fs <- fullsib %>% filter(Probability > 0.98)
hs <- halfsib %>% filter(Probability > 0.98)
dim(fs)
dim(hs)

dim(adults12) #93
dim(adults13) #131
dim(adults14) #130
dim(adults15) #227

#NUMBERS FROM EACH YEAR
#2012
#fs
6
#fs/adults+juvs
6/(93+188)
#hs
124
#hs/adults+juvs
124/(93+188)

#2013
#fs
50
#fs/adults+juvs
50/(131+526)
#hs
332
#hs/adults+juvs
332/(131+526)


#2014
#fs
94
#fs/adults+juvs
94/(130+410)
#hs
144
#hs/adults+juvs
144/(130+410)

#2015
#fs

#fs/adults+juvs
/(227+401)
#hs
/(227+401)
#hs/adults+juvs

#need to use 2015 survey data to get a count of each population
surv <- read.csv(file="GPSSurveys2015.clownfish.csv", header=TRUE)
#load the dive table info to connect to survey
c1 <- collect(leyte %>% tbl("diveinfo") %>% select(DiveNum, name))
total <- left_join(surv, c1, by="DiveNum")
total <- total %>% group_by(DiveNum, ObsTime) %>% filter((row_number() == 1))
totalmelt <- melt(total, id=c("DiveNum","ObsTime"))

#now find number adults and juvs from each population each year
count(adults13, name == "Palanas")
count(total, name == "Palanas")

count(juvID13, name == "Sitio Baybayon")
