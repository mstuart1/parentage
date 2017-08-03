#code to pull 2015 January survey data from amphiprion and use to estimate population size and annual sampling proportion
setwd("~/Documents/GradSchool/parentage")

#load packages
library(dplyr)
#join Amphiprion
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
#create all fish observation table, pull from amphiprion clownfish table, where each row is an observation of a single fish
fish <- collect(leyte %>% tbl("clownfish") %>% select(fish_spp, size, anem_table_id))
anems <- collect(leyte %>% tbl("anemones") %>% select(anem_table_id, dive_table_id))
sites <- collect(leyte %>% tbl("diveinfo") %>% select(site, date, dive_table_id))
first <- left_join(fish, anems, by="anem_table_id")
second <- left_join(first, sites, by="dive_table_id")
allfishobs <- second %>% select(fish_spp, size, site, date) %>% filter(fish_spp == "APCL")
fish15 <- as.data.frame(allfishobs %>% filter(date >= as.Date("2015-01-01") & date <= as.Date("2015-03-31")), stringsAsFactors=FALSE)

#counts don't look quite right, not all sites represented. gsub out name inconsistencies, then write to a text file to take a closer look
#write.table(fish15, file="fish15.txt")
fish15 <- read.table(file="fish15.txt", header=TRUE)

#create a data frame with the total adult of breeding size fish (>=8 cm) per site
totalbig <- subset(fish15, size >= 8)
totaladults <- totalbig %>%  count(site) 

#create a data frame with the total juvenile fish (<=8.5 cm) per site
totalsmall <- subset(fish15, size <= 8)
totaljuvs <- totalsmall %>%  count(site)



