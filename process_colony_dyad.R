###03/22/17 code to assign locations and sizes to sibling pairs in colony results, identify parents by size and tail color
# Set up working space ----------------------------------------------------
setwd("~/Documents/GradSchool/parentage")
#source("~/Documents/GradSchool/parentage/readGenepop_space.R")
suppressMessages(library(dplyr))

#import data
pairs <- read.table(file= "174HQ_PairwisePaternity.txt", header= TRUE)	
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)



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

withpar <- left_join(withoff, lab2, by = c("CandidateID" = "par.ligation_id"))

idcsv <- withpar
# Add field data
# ----------------------------------------------------------
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)


c1 <- leyte %>% tbl("diveinfo") %>% select(id, date, name)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, ObsTime)
c3 <- left_join(c2, c1, by = c(dive_table_id = "id"))
c4 <- tbl(leyte, sql("SELECT fish_table_id, anem_table_id, Sample_ID, Size FROM clownfish where Sample_ID is not NULL"))
first <- collect(left_join(c4, c3, by = "anem_table_id"))

### WAIT ###

second <- collect(left_join(c4, c3, by = "anem_table_id"))



### WAIT ###
names(first) <- paste("offs.", names(first), sep = "")
names(second) <- paste("par.", names(second), sep = "")
idcsv <- left_join(idcsv, first, by = c(offs.sample_id = "offs.Sample_ID"))
idcsv <- left_join(idcsv, second, by = c(par.sample_id = "par.Sample_ID"))


#null to remove column, NA to create
idcsv$offs.lat <- NULL
idcsv$offs.lon <- NULL
idcsv$par.lat <- NULL
idcsv$par.lon <- NULL



latlong <- data.frame(leyte %>% tbl("GPX") %>% collect(n = Inf))
# latlong <- leyte %>% tbl('GPX')

### WAIT ###

# Add lat long for first.id
# -----------------------------------------------
for(i in 1:nrow(idcsv)){
  #Get date and time information for the anemone
  date <- as.character(idcsv$offs.date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(idcsv$offs.ObsTime[i])
  timesplit <- strsplit(time, ":", fixed = T)[[1]]
  hour <- as.numeric(timesplit[1])
  min <- as.numeric(timesplit[2])
  sec <- as.numeric(timesplit[3])
  
  # Convert time to GMT
  hour <- hour - 8
  if(!is.na(hour) & hour <0){
    day <- day-1
    hour <- hour + 24
  }


  # Find the location records that match the date/time stamp (to nearest second)
  latlongindex <- which(latlong$year == year & latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
  i2 <- which.min(abs(latlong$sec[latlongindex] - sec))
  
  # Calculate the lat/long for this time
  if(length(i2)>0){
    idcsv$offs.lat[i] = latlong$lat[latlongindex][i2]
    idcsv$offs.lon[i] = latlong$long[latlongindex][i2]
  }
}

# Add lat long for second.id
# -----------------------------------------------
for(i in 1:nrow(idcsv)){
  #Get date and time information for the anemone
  date <- as.character(idcsv$par.date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(idcsv$par.ObsTime[i])
  timesplit <- strsplit(time, ":", fixed = T)[[1]]
  hour <- as.numeric(timesplit[1])
  min <- as.numeric(timesplit[2])
  sec <- as.numeric(timesplit[3])
  
  # Convert time to GMT
  hour <- hour - 8
  if(!is.na(hour) & hour <0){
    day <- day-1
    hour <- hour + 24
  }
  
  # Find the location records that match the date/time stamp (to nearest second)
  latlongindex <- which(latlong$year == year & latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
  i2 <- which.min(abs(latlong$sec[latlongindex] - sec))
  
  # Calculate the lat/long for this time
  if(length(i2)>0){
    idcsv$par.lat[i] = latlong$lat[latlongindex][i2]
    idcsv$par.lon[i] = latlong$long[latlongindex][i2]
  }
}

#create potential adults file
c1 <- leyte %>% tbl("diveinfo") %>% select(id, date, name)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, ObsTime)
c3 <- left_join(c2, c1, by = c(dive_table_id = "id"))
c4 <- tbl(leyte, sql("SELECT fish_table_id, anem_table_id, Sample_ID, Size FROM clownfish where Sample_ID is not NULL"))
first <- collect(left_join(c4, c3, by = "anem_table_id"))

c1 <- leyte %>% tbl("clownfish") %>% select(size, sample_id, col, Spp)
c2 <-collect(c1)
clowns<- filter(c2, Spp=="APCL")
clownA <- filter(clowns, size >= "8" | col== "OR")

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

#left off here, have to figure out how to join and get the adultID table info into idcsv, then need to filter with a for loop that flags matches with high prob and matches with parent/offspring pairs, based on if one is an adult, so will want to flag adults first thing
idcsv <- left_join(idcsv, second, by = c("Second.sample_id"))
idcsv <- left_join(idcsv, first, by =  c(OffspringID1 = "First.sample_id"))

#what's the distribution of parent and offspring sizes look like?

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
	select(OffspringID, CandidateID, offs.name, par.name, offs.Size, par.Size, ratio)
good

#about one quarter of the calculated parent offspring pairs make sense. The remainder could be siblings, so let's look at the full sibs and half sibs. There are more half sibs than full sibs, which could make some sense if the parent pairs aren't consistent year to year. In which case, a you're more likely to get more half sibs than full sibs because the type 1 survivorship of clownfish in general. let's take a look

fullsib <- read.table(file= "174HQ_FullSibDyad.txt", header= TRUE)
fullsibOFF <- fullsib
names(fullsibOFF) <- paste("fullsibOFFS.", names(fullsibOFF), sep = "")

	
halfsib <- read.table(file= "174HQ_HalfSibDyad.txt", header= TRUE)	
halfsibOFF <- halfsib
names(halfsibOFF) <- paste("halfsibOFFS.", names(halfsibOFF), sep = "")

fullsibPAR <- fullsib
names(fullsibPAR) <- paste("fullsibPAR.", names(fullsibPAR), sep = "")

halfsibPAR <- halfsib
names(halfsibPAR) <- paste("halfsibPAR.", names(halfsibPAR), sep = "")

withfsoff <- left_join(idcsv, fullsibOFF, by=c(OffspringID = "fullsibOFFS.sib1"))
withhsoff <- left_join(withfsoff, halfsibOFF, by=c(OffspringID = "halfsibOFFS.hsib1"))
withfspar <- left_join(withhsoff, fullsibPAR, by=c(CandidateID = "fullsibPAR.sib1"))
withhspar <- left_join(withfspar, halfsibPAR, by=c(CandidateID = "halfsibPAR.hsib1"))



goodfs <- idcsv %>%	select(OffspringID, CandidateID, offs.name, par.name, offs.Size, par.Size, ratio, sib2.x)
good

none <- withhspar %> filter()
write.csv(withhspar, file="174_colonypars_sibs.csv", quote=FALSE, col.names=TRUE)


####Colony error rate analysis
er <- read.table(file='174HQ_ErrorRate.txt', header=TRUE)

mean(er$OtherErrorRateEst)
hist(er$OtherErrorRateEst, breaks=(40), xlim=c(0,  .40))
 