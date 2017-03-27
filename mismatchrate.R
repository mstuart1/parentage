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

#03/13/17 I've decided to move forward by just using R to count the mismatches between genotypes manually
#to do this, need to use the trimmed genepop from amphiprion, with 58 loci and all individuals genotyped (seq17_03_58loci.gen)
genepop<-readGenepop(file='seq17_03_58loci.gen')
head(genepop)
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
ident<-read.table(file="ident.txt", col.names=c("FirstSampleID", "SecondSampleID","Mismatch"))
first<-select(ident, FirstSampleID)
second<-select(ident, SecondSampleID)
#now want to join the first capture and second capture lists of lig IDs to the genpop to attach them to their genotypes
firstc<-as.matrix(left_join(first, genepop, by=c(FirstSampleID = "names")))
secondc<-as.matrix(left_join(second, genepop, by=c(SecondSampleID = "names")))

matches1<- (firstc == secondc)
matches2<-matches1[-1,]
head(matches2)
hist(rowSums(matches2))
matches<-(rowSums(matches2))
write.csv(matches, file="mismatchrate.csv")
locimatches<-(colSums(matches2))
matches


#WHOOPs so turns out the identity database I pulled from in everything above this isn't reliable, instead want to use capID in the clownfish database
c1 <- leyte %>% tbl("clownfish") %>% select(sample_id, capid)
clowns<-as.data.frame(collect(c1))
recaps<-filter(clowns, capid != NA)
suppressWarnings(c4 <- tbl(leyte, sql("SELECT fish_table_id, anem_table_id, sample_id, capid FROM clownfish where capid is not NULL")))
recaps<-as.data.frame(collect(c4))

#filter out fish just captured once, put all recaptures in recaps2
recaps2 <- data.frame()
for(i in 1:nrow(recaps)){
x <- recaps[which(recaps$capid == recaps$capid[i]),]
recaps2 <- rbind(recaps2, x)
} 

#now attach ligation IDs so you can attach genepop
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
c5 <- collect(left_join(c4, c3, by = "digest_id"))
recaplig <- left_join(recaps2, c5, by = "sample_id")
recaplig2 <- select(recaplig, sample_id, capid, ligation_id)
allrecap <- left_join(recaplig2, genepop, by=c(ligation_id = "names"))
head(recaplig2)
#03/14/17 --> seq17_03_58loci.gen is filtered for recaps, need to get a genepop not filtered to look at mismatch rate
