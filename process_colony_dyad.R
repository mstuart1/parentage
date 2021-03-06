###03/22/17 code to assign locations and sizes to sibling pairs in colony results, identify parents by size and tail color
# Set up working space ----------------------------------------------------
setwd("~/Documents/GradSchool/parentage")
source("~/Documents/GradSchool/parentage/samplefromlig.R")
source("~/Documents/GradSchool/parentage/conleyte.R")
source("~/Documents/GradSchool/parentage/conlabor.R")
#source("~/Documents/GradSchool/parentage/readGenepop_space.R")
suppressMessages(library(dplyr))
library(igraph)

#import data
pairs <- read.table(file= "20170831colony15.Maternity.txt", header= TRUE)	
labor <- conlabor()



# add lab IDs

c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
c5 <- collect(left_join(c4, c3, by = "digest_id"))


# for First.IDs
lab1 <- c5
names(lab1) <- paste("offs.", names(lab1), sep = "")

withoff <- left_join(pairs, lab1, by = c("OffspringID" = "offs.ligation_id"))

# for Second.IDs
lab2 <- c5
names(lab2) <- paste("par.", names(lab2), sep = "")

withpar <- left_join(withoff, lab2, by = c("InferredMum1" = "par.ligation_id"))

idcsv <- withpar
# Add field data
# ----------------------------------------------------------
leyte <- conleyte()

c1 <- leyte %>% tbl("diveinfo") %>% select(dive_table_id, date, site)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, obs_time)
c3 <- as.data.frame(left_join(c2, c1, by = "dive_table_id"))
c4 <- as.data.frame(tbl(leyte, sql("SELECT fish_table_id, anem_table_id, Sample_ID, Size FROM clownfish where Sample_ID is not NULL")))
first <- as.data.frame((left_join(c4, c3, by = "anem_table_id")))

### WAIT ###

second <- as.data.frame(left_join(c4, c3, by = "anem_table_id"))



### WAIT ###
names(first) <- paste("offs.", names(first), sep = "")
names(second) <- paste("par.", names(second), sep = "")
idcsv <- left_join(idcsv, first, by = c(offs.sample_id = "offs.Sample_ID"))
idcsv <- left_join(idcsv, second, by = c(par.sample_id = "par.Sample_ID"))

c1 <- leyte %>% tbl("clownfish") %>% select(size, sample_id, color, fish_spp)
c2 <-collect(c1)
clowns<- filter(c2, fish_spp=="APCL")
clownA <- filter(clowns, size >= "8" | color== "OR")

#get rid of rows with NA in sample_id
adultID <-filter(clownA, sample_id != "NA")

#get rid of needless columns in idcsv
idcsv$offs.digest_id <- NULL
idcsv$par.digest_id <- NULL
idcsv$offs.extraction_id <- NULL
idcsv$par.extraction_id <- NULL

first <- adultID
second <- adultID
#add morphology data to colony pairs
names(first) <- paste("offs.", names(first), sep = "")
names(second) <- paste("par.", names(second), sep = "")

idcsv <- left_join(idcsv, second, by = c("par.sample_id"))
idcsv <- left_join(idcsv, first, by =  c(OffspringID = "offs.sample_id"))

##what's the distribution of parent and offspring sizes look like?

mean(idcsv$offs.Size)
mean(idcsv$par.Size)
hist(idcsv$offs.Size, breaks=seq(-0.5,14.5), xlim=c(0, 14))#bimodally distributed...hmmm
hist(idcsv$par.Size, breaks=seq(-0.5,14.5), xlim=c(0, 14))#bimodally distributed again...hmmm

#want to see if the parent offspring pairs have one large, one small. If so, makes sense
idcsv$ratio <- NA

for (i in 1:nrow(idcsv))
{
  if ((idcsv$offs.Size[i] < idcsv$par.Size[i]) &  8.75 < idcsv$par.Size[i])
  {
    idcsv$ratio[i] <- "GOOD"
  }
}
good <- idcsv %>% filter(ratio == "GOOD") %>%
	select(OffspringID, InferredMum1, offs.name, par.name, offs.Size, par.Size, ratio)
good

#about one quarter of the calculated parent offspring pairs make sense. The remainder could be siblings, so let's look at the full sibs and half sibs. There are more half sibs than full sibs, which could make some sense if the parent pairs aren't consistent year to year. In which case, a you're more likely to get more half sibs than full sibs because the type 1 survivorship of clownfish in general. let's take a look

fullsib <- read.table(file= "20170831colony15.FullSibDyad.txt", header= TRUE)
fullsibOFF <- fullsib
names(fullsibOFF) <- paste("fullsibOFFS.", names(fullsibOFF), sep = "")

	
halfsib <- read.table(file= "20170831colony14.HalfSibDyad.txt", header= TRUE)	
halfsibOFF <- halfsib
names(halfsibOFF) <- paste("halfsibOFFS.", names(halfsibOFF), sep = "")

fullsibPAR <- fullsib
names(fullsibPAR) <- paste("fullsibPAR.", names(fullsibPAR), sep = "")

halfsibPAR <- halfsib
names(halfsibPAR) <- paste("halfsibPAR.", names(halfsibPAR), sep = "")

withfsoff <- left_join(idcsv, fullsibOFF, by=c(OffspringID = "fullsibOFFS.sib1"))
withhsoff <- left_join(withfsoff, halfsibOFF, by=c(OffspringID = "halfsibOFFS.hsib1"))
withfspar <- left_join(withhsoff, fullsibPAR, by=c(InferredMum1 = "fullsibPAR.sib1"))
withhspar <- left_join(withfspar, halfsibPAR, by=c(InferredMum1 = "halfsibPAR.hsib1"))



#goodfs <- idcsv %>%	select(OffspringID, InferredMum1, offs.name, par.name, offs.Size, par.Size, ratio, sib2.x)
good

write.csv(withhspar, file="20170831colony15.csv", quote=FALSE, col.names=TRUE)


#now look at the difference between full likelihood parentage and pairwise parentage
fs <- as.matrix(fullsib)
hs <- as.matrix(halfsib)
fs <- fullsib %>% filter(Probability > 0.98)
hs <- halfsib %>% filter(Probability > 0.98)

par <- as.matrix(pairs)
match <- left_join(pairs, fullsib, by=c(OffspringID="sib1", CandidateID="sib2"))
match2 <- left_join(pairs, fullsib, by=c(OffspringID="sib2", CandidateID="sib1"))
match3 <- match %>% filter(Probability > 0.95)
match4 <- match2 %>% filter(Probability > 0.95)	
#looks like 12 parent pairs are also identified as full siblings by FL method
match <- left_join(pairs, halfsib, by=c(OffspringID="hsib1", CandidateID="hsib2"))
match2 <- left_join(pairs, halfsib, by=c(OffspringID="hsib2", CandidateID="hsib1"))
match3 <- match %>% filter(Probability > 0.95)
match4 <- match2 %>% filter(Probability > 0.95)	

#now check out how different analysis parameters compare
fullsib <- read.table(file= "175HQloci_adults_sep_2015.FullSibdyad.txt", header= TRUE)
halfsib <- read.table(file= "175HQloci_adults_sep_2015.HalfSibdyad.txt", header= TRUE)	
fsNP <- fullsib %>% filter(Probability > 0.98)
hsNP <- halfsib %>% filter(Probability > 0.98)
fsWP1 <- fullsib %>% filter(Probability > 0.98)
hsWP1 <- halfsib %>% filter(Probability > 0.98)
fsMP1 <- fullsib %>% filter(Probability > 0.98)
hsMP1 <- halfsib %>% filter(Probability > 0.98)
fsMP10 <- fullsib %>% filter(Probability > 0.98)
hsMP10 <- halfsib %>% filter(Probability > 0.98)

match <- semi_join(fsNP, fsMP1, by=c("OffspringID1", "OffspringID2"))


####Colony error rate analysis
er <- read.table(file='174HQ_ErrorRate.txt', header=TRUE)

mean(er$OtherErrorRateEst)
hist(er$OtherErrorRateEst, breaks=(40), xlim=c(0,  .40))

#how many siblings are also related to parentage


