#12292016 how to pull adult and juvenile files based on sampling years

#set working directory
setwd("~/Documents/GradSchool/parentage")

#load packages
library(dplyr)
#join Amphiprion
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
#create potential adults file
c1 <- leyte %>% tbl("diveinfo") %>% select(dive_table_id, date, site)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, obs_time)
c3 <- left_join(c2, c1, by = "dive_table_id")
c4 <- tbl(leyte, sql("SELECT fish_table_id, anem_table_id, sample_id, size FROM clownfish where sample_id is not NULL"))
c3 <- collect(c3)
c4 <- collect(c4)
fieldid <- (left_join(c4, c3, by = "anem_table_id"))

c1 <- leyte %>% tbl("clownfish") %>% select(size, sample_id, color, fish_spp)
c2 <-collect(c1)
clowns <- filter(c2, fish_spp=="APCL")
clownF <- filter(clowns, color == "O")
clownM <- filter(clowns, color == "YP")
#get rid of rows with NA in sample_id
adultF <-collect(filter(clownF, sample_id != "NA"))
adultM <-collect(filter(clownM, sample_id != "NA"))

#now that I have adult IDs, need to connect these to ligation IDs

#YEAR 2015
c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
c5 <- collect(left_join(c4, c3, by = "digest_id"))
adultsF <- left_join(adultF, c5, by= "sample_id")
adultsM <- left_join(adultM, c5, by= "sample_id")

adultsF2 <- left_join(adultsF, fieldid, by="sample_id")
adultsM2 <- left_join(adultsM, fieldid, by="sample_id")

a15F <-adultsF2 %>% filter(date >= as.Date("2015-04-15") & date <= as.Date("2015-07-31"))
a15M <-adultsM2 %>% filter(date >= as.Date("2015-04-15") & date <= as.Date("2015-07-31"))

#write ligation id file
ligidF <- select(a15F, ligation_id)
ligidM <- select(a15M, ligation_id)
ligidF <- ligidF %>% filter(ligation_id != 'NA')
ligidF <- distinct(ligidF)
ligidM <- ligidM %>% filter(ligation_id != 'NA')
ligidM <- distinct(ligidM)
write.table(ligidF, file="adults15F.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)
write.table(ligidM, file="adults15M.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)

#now want to create juveniles file
c1 <- leyte %>% tbl("clownfish") %>% select(size, sample_id, color, fish_spp)
c2 <-collect(c1)
clowns<- filter(c2, fish_spp=="APCL")
#make full list of adults exclude
adults <- bind_rows(adultsF, adultsM)

clownJ <- anti_join(clowns, adults, by = "sample_id")
juvs <- left_join(clownJ, c5, by= "sample_id")
juvs2 <- left_join(juvs, fieldid, by="sample_id")
#get rid of rows with NA in sample_id
juvID <-filter(juvs2, ligation_id != "NA")
juvID <-filter(juvs2, sample_id != "NA")
juvs15 <-juvs2 %>% filter(date > as.Date("2015-04-15") & date < as.Date("2015-07-15"))


###FOR ALL YEARS####

ligidJ <- select(juvs15, ligation_id)
ligid2 <-filter(ligidJ, ligation_id != "NA")
ligid2 <- distinct(ligid2)
write.table(ligid2, file="juv15.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)

#now just replace all numbers with what your desired year is

#JUST FOR 2012###
#in 2012, only O tail colors were noted. So anti join the female data with the all adults of 2012 data to generate the males. 
adults12M <- anti_join(clowns, adultsF)
a12M<- left_join(adults12M, fieldid, by="sample_id")
a12M$date <- ymd(a12M$date)
a12M <-a12M %>% filter(date >= as.Date("2012-04-12") & date <= as.Date("2012-07-31"))
a12M <- a12M %>% filter(size.x >= 8)
a12M <- left_join(a12M, c5)
ligidM <- select(a12M, ligation_id)
ligidM <- ligidM %>% filter(ligation_id != 'NA')
ligidM <- distinct(ligidM)
write.table(ligidM, file="adults12M.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)
