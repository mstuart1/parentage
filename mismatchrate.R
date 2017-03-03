#02/20/2017
#calculate the actual error rate in our genotyping methods by 
#comparing matching loci in regenotyped individuals FOR 809 15x loci
#whole code is messy and incomplete, don't trust
library(dplyr)
setwd("~/Documents/GradSchool/parentage")
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
c1 <- leyte %>% tbl("identity") %>% select(FirstSampleID, SecondSampleID, MismatchingLoci)
ident<-as.data.frame(collect(c1))
#write the table then delete quotes in a text editor
write.table(ident, file="ident.txt", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")
ident<-read.table(file="ident.txt", col.names=c("FirstSampleID", "SecondSampleID","Mismatch"))

genepop<-readGenepop(file='seq17_03_58loci.gen')
head(genepop)
first<-select(ident, FirstSampleID)
second<-select(ident, SecondSampleID)

#need to connect sampleID to ligationID
# add lab IDs
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
c5 <- collect(left_join(c4, c3, by = "digest_id"))
#semijoin c5 with your genepop

#drop columns for a table of just ligation ids
#firstly<-firstligid[-c(2:5)]

#recapture genotypes to compare against Cervus identity analysis
ligationid<- left_join(second, c5, by=c(SecondSampleID = "ligation_id"))
secondligid<-left_join(ligationid, genepop, by=c(SecondSampleID = "names"))


#with Cervus identity results, filter for known recapture pairs and get the mismatch rates
#then you can get an average mismatch rate
IDanalysis<-read.csv(file='58lociID.csv')
allpairs<-left_join(secondligid, IDanalysis, by=c(SecondSampleID = "Second.ID"))
allpairs2<-select(allpairs, SecondSampleID, First.ID, Mismatching.loci)
pair<-filter(allpairs2, SecondSampleID == "L1733", First.ID == "L0465")
#for some reason, when searching for known pairs that I got from amphiprion, those known pairs don't come up as matched by the cervus identity analysis....maybe need to ask Michelle. 03/03/17 
class(IDanalysis)


