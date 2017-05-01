#12292016 how to pull adult and juvenile files based on sampling years

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

c1 <- leyte %>% tbl("clownfish") %>% select(size, sample_id, col, Spp)
c2 <-collect(c1)
clowns<- filter(c2, Spp=="APCL")
clownA <- filter(clowns, size >= "8")

#get rid of rows with NA in sample_id
adultID <-filter(clownA, sample_id != "NA")

#now that I have adult IDs, need to connect these to ligation IDs
#YEAR 2012
c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
c5 <- collect(left_join(c4, c3, by = "digest_id"))
adults <- inner_join(adultID, c5, by= "sample_id")
adults2 <- inner_join(adults, first, by=c(sample_id = "Sample_ID"))
adults12 <-adults2 %>% filter(date >= as.Date("2012-04-12") & date <= as.Date("2012-07-31"))
ligidA <- select(adults12, ligation_id)

write.table(ligidA, file="adults12.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)

#now want to create juveniles file
c1 <- leyte %>% tbl("clownfish") %>% select(size, sample_id, col, Spp)
c2 <-collect(c1)
clowns<- filter(c2, Spp=="APCL")
clownJ <- anti_join(clowns, adults, by = "sample_id")
juvs <- left_join(clownJ, c5, by= "sample_id")
juvs2 <- left_join(juvs, first, by=c(sample_id = "Sample_ID"))
#get rid of rows with NA in sample_id
juvID <-filter(juvs2, ligation_id != "NA")
juvID <-filter(juvs2, sample_id != "NA")
juvs12 <-juvs2 %>% filter(date > as.Date("2012-04-12") & date < as.Date("2012-07-12"))



ligidJ <- select(juvs12, ligation_id)
ligid2 <-filter(ligidJ, ligation_id != "NA")
write.table(ligid2, file="juv12.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)


#YEAR 2013
adults13 <-adults2 %>% filter(date > as.Date("2013-04-12") & date < as.Date("2013-07-12"))
ligidA <- select(adults13, ligation_id)

write.table(ligidA, file="adults13.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)

juvs13 <-juvs2 %>% filter(date > as.Date("2013-04-12") & date < as.Date("2013-07-12"))

#get rid of rows with NA in sample_id
juvID <-filter(juvs, sample_id != "NA")

ligidJ <- select(juvs13, ligation_id)
ligid2 <-filter(ligidJ, ligation_id != "NA")
write.table(ligid2, file="juv13.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)

#YEAR 2014
adults14 <- adults2 %>% filter(date > as.Date("2014-04-12") & date < as.Date("2014-07-12"))
ligidA <- select(adults14, ligation_id)

write.table(ligidA, file="adults14.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)
juvs14 <-juvs2 %>% filter(date > as.Date("2014-04-12") & date < as.Date("2014-07-12"))
#get rid of rows with NA in sample_id
juvID <-filter(juvs14, sample_id != "NA")

ligidJ <- select(juvID, ligation_id)
ligid2 <-filter(ligidJ, ligation_id != "NA")
write.table(ligid2, file="juv14.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)


#YEAR 2015
adults15 <-adults2 %>% filter(date > as.Date("2015-04-12") & date < as.Date("2015-07-12"))
ligidA <- select(adults15, ligation_id)

write.table(ligidA, file="adults15.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)
juvs15 <-juvs2 %>% filter(date > as.Date("2015-04-12") & date < as.Date("2015-07-12"))

#get rid of rows with NA in sample_id
juvID <-filter(juvs15, sample_id != "NA")

ligidJ <- select(juvs15, ligation_id)
ligid2 <-filter(ligidJ, ligation_id != "NA")
write.table(ligid2, file="juv15.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)

#YEAR 2016
adults16 <-adults2 %>% filter(date >= as.Date("2016-04-12") & date <= as.Date("2016-07-12"))
ligidA <- select(adults15, ligation_id)

write.table(ligidA, file="adults16.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)
juvs16 <-juvs2 %>% filter(date >= as.Date("2016-04-12") & date <= as.Date("2016-07-12"))

#get rid of rows with NA in sample_id
juvID <-filter(juvs16, sample_id != "NA")
ligidJ <- select(juvID, ligation_id)

write.table(ligidJ, file="juv16.txt", row.names = FALSE, col.names = FALSE, quote= FALSE)

