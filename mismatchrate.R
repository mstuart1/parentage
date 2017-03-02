#02/20/2017
#calculate the actual error rate in our genotyping methods by 
#comparing matching loci in regenotyped individuals
#whole code is messy and incomplete, don't trust
library(dplyr)
setwd("~/Documents/GradSchool/parentage")
leyte <- src_mysql(dbname = "Leyte", host = "amphiprion.deenr.rutgers.edu", 
                   user = "katrinac", password = "yetistomp&33", port = 3306, create = F)
c1 <- leyte %>% tbl("identity") %>% select(FirstSampleID, SecondSampleID, MismatchingLoci)
ident<-as.data.frame(collect(c1))
#write the table then delete quotes in a text editor
write.table(ident, file="ident.txt", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")
ident<-read.table(file="ident.txt", col.names=c("FirstSampleID", "SecondSampleID","Mismatch"))
#now load 58 loci genepop and pull the first and second recaptures of individuals
genepop<-readGenepop(file='seq17_03_58loci.gen')
first<-select(ident, FirstSampleID)

#need to connect sampleID to ligationID
# add lab IDs
labor <- src_mysql(dbname = "Laboratory", host = "amphiprion.deenr.rutgers.edu", 
                   user = "katrinac", password = "yetistomp&33", port = 3306, create = F)

c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
c5 <- collect(left_join(c4, c3, by = "digest_id"))

#drop columns for a table of just ligation ids
firstly<-firstligid[-c(2:5)]

#recapture genotypes
second<-select(ident, SecondSampleID)
ligationid<- left_join(second, c5, by=c(SecondSampleID = "sample_id"))
secondligid<-left_join(ligationid, genepop, by=c(ligation_id = "names"))

write.csv(firstligid, file="firstcapture.csv", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")
write.csv(secondligid, file="secondcapture.csv", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")



firstgenes<-semi_join()
summary(ident)

31/809
