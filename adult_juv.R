#12/21/16 Need to re-run the parentage analysis without trouble samples and 
#then re-run for annual parentage analysis. To do this, need to create new 
#data sheets, one adult and one juvenile to run through CERVUS against the 
#genepop file

#set working directory
setwd("~/Documents/Grad School/parentage")

#load packages
library(dplyr)
library(data.table)

#join Amphiprion
labor <- src_mysql(dbname = "Laboratory", host = "amphiprion.deenr.rutgers.edu", 
                   user = "katrinac", password = "yetistomp", port = 3306, create = F)
leyte <- src_mysql(dbname = "Leyte", host = "amphiprion.deenr.rutgers.edu", 
                   user = "katrinac", password = "yetistomp", port = 3306, create = F)

#create potential adults file
c1 <- leyte %>% tbl("clownfish") %>% select(size, sample_id, col, Spp)
c2 <-collect(c1)
clowns<- filter(c2, Spp=="APCL")
clownA <- filter(clowns, size >= "7" | col== "YP" | col== "TE" | col== "YPO" | col== "OP"| col== "OR")

#get rid of rows with NA in sample_id
adultID <-filter(clownA, sample_id != "NA")

#now that I have adult IDs, need to connect these to ligation IDs
c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
c5 <- collect(left_join(c4, c3, by = "digest_id"))
adults <- inner_join(adultID, c5, by= "sample_id")
ligidA <- select(adults, ligation_id)

write.table(ligidA, file="adults.txt", row.names = FALSE, col.names = FALSE)

#now want to create juveniles file
c1 <- leyte %>% tbl("clownfish") %>% select(size, sample_id, col, Spp)
c2 <-collect(c1)
clowns<- filter(c2, Spp=="APCL")
clownJ <- anti_join(clowns, adults, by = "sample_id")
juvs <- inner_join(clownJ, c5, by= "sample_id")
#get rid of rows with NA in sample_id
juvID <-filter(juvs, sample_id != "NA")

ligidJ <- select(juvID, ligation_id)

write.table(ligidJ, file="juv.txt", row.names = FALSE, col.names = FALSE)
