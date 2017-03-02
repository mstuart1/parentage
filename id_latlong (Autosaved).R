# set working directory
setwd("/Users/kat1/Documents/Grad School/parentageanalysis")
getwd()
# add data.table package

install.packages("data.table")
install.packages("fields")
install.packages("diagonals")
install.packages("xlsx")
install.packages("rJava")
install.packages("formatR")
install.packages("shiny")
library(data.table)
library(dplyr)
library(fields)
library(xlsx)
library(rJava)
library(formatR)
library(shiny)
formatR::tidy_app()
# Import cervus identity results
# ------------------------------------------
idcsv <- read.csv(file = "09152016_seq_parent-2.csv", stringsAsFactors = F)


# Add metadata
# ------------------------------------------------------------

# Connect to database
# -----------------------------------------------------

suppressMessages(library(dplyr))
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)


# load data.table package

# add lab IDs

c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
c5 <- data.frame(left_join(c4, c3, by = "digest_id"))


# for First.IDs
lab1 <- c5
names(lab1) <- paste("First.", names(lab1), sep = "")

# First.candidate.ID is the column label for first candidate ligation
# ID in cervus csv file, First.ligation_id is the name of the column in
# lab1 I want to join with
idcsv <- merge(idcsv, lab1, by.x = "First.candidate.ID", by.y = "First.ligation_id", 
               all.x = T)

### WAIT ###

# For Second.IDs
lab2 <- c5
names(lab2) <- paste("Second.", names(lab2), sep = "")

idcsv <- merge(idcsv, lab2, by.x = "Second.candidate.ID", by.y = "Second.ligation_id", 
               all.x = T)

### WAIT ####

# For Offspring.IDs
lab3 <- c5
names(lab3) <- paste("Offs.", names(lab3), sep = "")

idcsv <- merge(idcsv, lab3, by.x = "Offspring.ID", by.y = "Offs.ligation_id", 
               all.x = T)

# Add field data
# ----------------------------------------------------------
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)



c1 <- leyte %>% tbl("diveinfo") %>% select(id, Date, Name)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, 
                                           ObsTime)
c3 <- left_join(c2, c1, by = c(dive_table_id = "id"))
# c4 <- leyte %>% tbl('clownfish') %>% select(fish_table_id,
# anem_table_id, Sample_ID, Size)
c4 <- tbl(leyte, sql("SELECT fish_table_id, anem_table_id, Sample_ID, Size FROM clownfish where Sample_ID is not NULL"))
first <- data.frame(left_join(c4, c3, by = "anem_table_id"))

### WAIT ###

second <- data.frame(left_join(c4, c3, by = "anem_table_id"))

offs <- data.frame(left_join(c4, c3, by = "anem_table_id"))
idcsv

### WAIT ###

names(first) <- paste("First.", names(first), sep = "")
names(second) <- paste("Second.", names(second), sep = "")
names(offs) <- paste("Offs.", names(offs), sep = "")
idcsv <- left_join(idcsv, first, by = c(First.sample_id = "First.Sample_ID"))
idcsv <- left_join(idcsv, second, by = c(Second.sample_id = "Second.Sample_ID"))
idcsv <- left_join(idcsv, offs, by = c(Offs.sample_id = "Offs.Sample_ID"))



idcsv$First.lat <- NA
idcsv$First.lon <- NA
idcsv$Second.lat <- NA
idcsv$Second.lon <- NA
idcsv$Offs.lat <- NA
idcsv$Offs.lon <- NA

write.csv(idcsv, file = "idanalysis.csv", sep = "", row.names = F)

latlong <- data.frame(leyte %>% tbl("GPX") %>% collect(n = Inf))
# latlong <- leyte %>% tbl('GPX')

### WAIT ###

# Add lat long for first.id
# -----------------------------------------------
for(i in 1:nrow(idcsv)){
  #Get date and time information for the anemone
  date <- as.character(idcsv$First.Date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(idcsv$First.ObsTime[i])
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
    idcsv$First.lat[i] = latlong$lat[latlongindex][i2]
    idcsv$First.lon[i] = latlong$long[latlongindex][i2]
  }
}

# Add lat long for second.id
# -----------------------------------------------
for(i in 1:nrow(idcsv)){
  #Get date and time information for the anemone
  date <- as.character(idcsv$Second.Date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(idcsv$Second.ObsTime[i])
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
    idcsv$Second.lat[i] = latlong$lat[latlongindex][i2]
    idcsv$Second.lon[i] = latlong$long[latlongindex][i2]
  }
}

# Add lat long for offs.id
# -----------------------------------------------
for(i in 1:nrow(idcsv)){
  #Get date and time information for the anemone
  date <- as.character(idcsv$Offs.Date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(idcsv$Offs.ObsTime[i])
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
    idcsv$Offs.lat[i] = latlong$lat[latlongindex][i2]
    idcsv$Offs.lon[i] = latlong$long[latlongindex][i2]
  }
}
write.csv(idcsv, file = "id_latlong.csv", sep = "", row.names = F)


write.csv(idcsv, file = "idanal.csv")

# Flag matches that were caught more than 250m apart
# ----------------------


# library(fields)
source("greatcircle_funcs.R")  # alternative, probably faster
alldists <- fields::rdist.earth(as.matrix(idcsv[, c("First.lon", "First.lat")]), 
                                as.matrix(idcsv[, c("Second.lon", "Second.lat")]), miles = FALSE, R = 6371)  # see http://www.r-bloggers.com/great-circle-distance-calculations-in-r/ # slow because it does ALL pairwise distances, instead of just in order
idcsv$distkm <- diag(alldists)

idcsv$disteval <- NA  # placeholder
for (i in 1:nrow(idcsv))
{
  if (!is.na(idcsv$distkm[i]) & 0.1 <= idcsv$distkm[i])
  {
    idcsv$disteval[i] <- "FAIL"
  }
}
# getwd()
library(utils)

write.csv(idcsv, file = paste("identity/", "_idanalyis.csv", sep = ""), 
          row.names = F)

