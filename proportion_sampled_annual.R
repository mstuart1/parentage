#migest proportion sampled
#set working directory
setwd("~/Documents/GradSchool/parentage")

#load packages
library(dplyr)
library(lubridate)
#join Amphiprion
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

c1 <- leyte %>% tbl("diveinfo") %>% select(dive_table_id, date, site)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, obs_time)
c3 <- as.data.frame(left_join(c2, c1, by = "dive_table_id"))
c4 <- as.data.frame(tbl(leyte, sql("SELECT fish_table_id, anem_table_id, sample_id, size, fish_spp FROM clownfish")))
fieldid <- collect(left_join(c4, c3, by = "anem_table_id"))
allfish <- fieldid %>% filter(date != 'NA', fish_spp == 'APCL')
which(fieldid$date=='NA') #returns 0
#allfish is a table of all fish observations with all data associated with them. Now filter for the 2015 survey dates and get counts per site
allfish$odate <- ymd(allfish$date)

allfish <- allfish %>%
  mutate(year=year(odate)) %>%
  mutate(month = month(odate)) %>%
  mutate(day = day(odate))
 
surv <-as.data.frame(allfish %>% filter(year=='2015', month =="1"))
surv$size <- as.numeric(surv$size)
juv <-as.data.frame(surv %>% filter(size <= 7))
juv %>% group_by(site) %>% count(site)
ads <- as.data.frame(surv %>% filter(size > 7))
ads %>% group_by(site) %>% count(site)


#all above is for 2015 survey data. Now want to see how many adults and juveniles were sampled from each population in each year from the files I ran through COLONY
samps <- read.table(file="175HQloci2015_ALL.txt", header=TRUE)

samps <- samps %>% select(ID)
c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
labid <- as.data.frame(left_join(c4, c3, by = "digest_id"))
allid <- left_join(fieldid, labid, by="sample_id")
sampid <- left_join(samps, allid, by=c("ID"= "ligation_id"))

#count
sampid$size <- as.numeric(sampid$size)
juv <-as.data.frame(sampid %>% filter(size <= 7))
juv %>% group_by(site) %>% count(site)
ads <- as.data.frame(sampid %>% filter(size > 7))
ads %>% group_by(site) %>% count(site)