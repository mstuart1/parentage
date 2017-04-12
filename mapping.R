#mapping connectivity
# Set up working space ----------------------------------------------------
setwd("~/Documents/GradSchool/parentage")
source("~/Documents/GradSchool/parentage/samplefromlig.R")
source("~/Documents/GradSchool/parentage/conleyte.R")
source("~/Documents/GradSchool/parentage/conlabor.R")
suppressMessages(library(dplyr))
library(lubridate)
library(stringr)
library(maps)
library(mapdata)
library(maptools)
library(scales)
library(network)

#need to get the lat long id for all parentage matches and map, then use igraph to connect results

leyte <- conleyte()

# code for loading gps coords from csv #load csv with lat lon points
latlon <- read.csv(file="id_distcalc.csv", header=TRUE)

latlono <- latlon %>% select(Offspring.ID, Offs.lat, Offs.lon)
latlonf <- latlon %>% select(First.candidate.ID, First.lat, First.lon)
latlons <- latlon %>% select(Second.candidate.ID, Second.lat, Second.lon)
names(latlono) <- c("id", "lat", "lon")
names(latlonf) <- c("id", "lat", "lon")
names(latlons) <- c("id", "lat", "lon")
latlon1 <- bind_rows(latlono, latlonf)
coor <- bind_rows(latlon1, latlons)
coord <- unique(coor)

#load tables with parent offspring matches and attach lat lon
#prepare full likelihood parentage/sibship results for mapping
fullsib <- read.table(file= "174HQloci_adult_sep_2013.FullSibDyad.txt", header= TRUE)
halfsib <- read.table(file= "174HQloci_adult_sep_2013.HalfSibDyad.txt", header= TRUE)	
pairs <- read.table(file= "174HQloci_adult_sep_2013.Paternity.txt", header= TRUE)

#fs <- sampfromlig(fullsib[1])#for some reason, not getting all the sample ids with this method, going to try old way
#fs2 <- sampfromlig(fullsib[2])
first <- c5
names(first) <- paste("sib1.", names(first), sep = "")
fullsib <- left_join(fullsib, first, by=c(sib1="sib1.ligation_id"))
sec <- c5
names(sec) <- paste("sib2.", names(sec), sep = "")
fullsib <- left_join(fullsib, sec, by=c(sib2="sib2.ligation_id"))

c1 <- leyte %>% tbl("diveinfo") %>% select(id, date, name)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, ObsTime)
c3 <- left_join(c2, c1, by = c(dive_table_id = "id"))
c4 <- tbl(leyte, sql("SELECT fish_table_id, anem_table_id, Sample_ID, Size FROM clownfish where Sample_ID is not NULL"))
first <- collect(left_join(c4, c3, by = "anem_table_id"))

### WAIT ###

second <- collect(left_join(c4, c3, by = "anem_table_id"))

names(first) <- paste("sib1.", names(first), sep = "")
fullsib <- left_join(fullsib, first, by=c(sib1.sample_id="sib1.Sample_ID"))
names(second) <- paste("sib2.", names(second), sep = "")
fullsib <- left_join(fullsib, second, by=c(sib2.sample_id="sib2.Sample_ID"))

#now halfsibs
first <- c5
names(first) <- paste("hsib1.", names(first), sep = "")
halfsib <- left_join(halfsib, first, by=c(hsib1="hsib1.ligation_id"))
sec <- c5
names(sec) <- paste("hsib2.", names(sec), sep = "")
halfsib <- left_join(halfsib, sec, by=c(hsib2="hsib2.ligation_id"))

c1 <- leyte %>% tbl("diveinfo") %>% select(id, date, name)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, ObsTime)
c3 <- left_join(c2, c1, by = c(dive_table_id = "id"))
c4 <- tbl(leyte, sql("SELECT fish_table_id, anem_table_id, Sample_ID, Size FROM clownfish where Sample_ID is not NULL"))
first <- collect(left_join(c4, c3, by = "anem_table_id"))

### WAIT ###

second <- collect(left_join(c4, c3, by = "anem_table_id"))

names(first) <- paste("hsib1.", names(first), sep = "")
halfsib <- left_join(halfsib, first, by=c(hsib1.sample_id="hsib1.Sample_ID"))
names(second) <- paste("hsib2.", names(second), sep = "")
halfsib <- left_join(halfsib, second, by=c(hsib2.sample_id="hsib2.Sample_ID"))

#now parents
first <- c5
names(first) <- paste("offs.", names(first), sep = "")
pairs <- left_join(pairs, first, by=c(OffspringID="offs.ligation_id"))
sec <- c5
names(sec) <- paste("par.", names(sec), sep = "")
pairs <- left_join(pairs, sec, by=c(InferredDad1="par.ligation_id"))

c1 <- leyte %>% tbl("diveinfo") %>% select(id, date, name)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, ObsTime)
c3 <- left_join(c2, c1, by = c(dive_table_id = "id"))
c4 <- tbl(leyte, sql("SELECT fish_table_id, anem_table_id, Sample_ID, Size FROM clownfish where Sample_ID is not NULL"))
first <- collect(left_join(c4, c3, by = "anem_table_id"))

### WAIT ###

second <- collect(left_join(c4, c3, by = "anem_table_id"))

names(first) <- paste("offs.", names(first), sep = "")
pairs <- left_join(pairs, first, by=c(offs.sample_id="offs.Sample_ID"))
names(second) <- paste("par.", names(second), sep = "")
pairs <- left_join(pairs, second, by=c(par.sample_id="par.Sample_ID"))


#clean up, remove unnecessary columns
pairs <- pairs %>% select(OffspringID, InferredDad1, offs.ObsTime, offs.date, par.ObsTime, par.date, offs.Size, par.Size, ProbDad1)
fullsib <- fullsib %>% select(sib1, sib2, sib1.ObsTime, sib1.date, sib2.ObsTime, sib2.date, sib1.Size, sib2.Size, Probability)
halfsib <- halfsib %>% select(hsib1, hsib2, hsib1.ObsTime, hsib1.date, hsib2.ObsTime, hsib2.date, hsib1.Size, hsib2.Size, Probability)

#attach lat/lon for parentage
#idcsv <- data.frame(read.csv("174_colonypars_sibs_adultsep13.csv", header = TRUE), stringsAsFactors = FALSE)	
pairs$offs.date <- as.character(pairs$offs.date)
pairs$offs.ObsTime <- as.character(pairs$offs.ObsTime)
pairs$par.date <- as.character(pairs$par.date)
pairs$par.ObsTime <- as.character(pairs$par.ObsTime)


# combine date and time to form a dttm column ####
  # set time zone to PHT, Asia/Manila
pairs$otime <- str_c(pairs$offs.date, pairs$offs.ObsTime, sep = " ")
pairs$ptime <- str_c(pairs$par.date, pairs$par.ObsTime, sep = " ")

pairs$otime <- ymd_hms(pairs$otime)
pairs$ptime <- ymd_hms(pairs$ptime)

pairs$otime <- force_tz(pairs$otime, tzone = "Asia/Manila")
pairs$ptime <- force_tz(pairs$ptime, tzone = "Asia/Manila")

# change time zone to UTC ####
pairs$otime <- with_tz(pairs$otime, tzone = "UTC")
pairs$ptime <- with_tz(pairs$ptime, tzone = "UTC")

pairs <- pairs %>%
  mutate(omonth = month(otime)) %>%
  mutate(oday = day(otime)) %>%
  mutate(ohour = hour(otime)) %>%
  mutate(omin = minute(otime)) %>%
  mutate(osec = second(otime)) %>%
  mutate(pmonth = month(ptime)) %>%
  mutate(pday = day(ptime)) %>%
  mutate(phour = hour(ptime)) %>%
  mutate(pmin = minute(ptime)) %>%
  mutate(psec = second(ptime))

#trim the data to keep only time and parent offspring matches, and then elminate repeats
pairs <- distinct(pairs)

latlong <- data.frame(leyte %>% tbl("GPX") %>% collect(n = Inf))

latlongo <- latlong %>%
  mutate(month = month(time)) %>%
  mutate(day = day(time)) %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time))
  
names(latlongo) <- paste("offs.", names(latlongo), sep = "")

# find matches for times to assign lat long - there are more than one set of seconds (sec.y) that match
pairlatlon <- left_join(pairs, latlongo, by = c(omonth="offs.month", oday="offs.day", ohour="offs.hour", omin="offs.min"))
latlongp <- latlong %>%
  mutate(month = month(time)) %>%
  mutate(day = day(time)) %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time))
names(latlongp) <- paste("par.", names(latlongp), sep = "")
pairlatlon <- left_join(pairlatlon, latlongp, by = c(pmonth="par.month", pday="par.day", phour="par.hour", pmin="par.min"))

#now trim to just necessary columns
#### HERE CAN I USE GROUP BY ID OR OBSTIME AND THEN SUMMARISE TO GET THE MEAN LAT LON OR MIN LAT LON AND CREATE A NEW TABLE WITH ALL COLUMNS BUT ONLY ONE LAT LON PER OBS
# because all of the lat longs for the 4 observations are basically the same, remove sec.y and find distinct values
pairlatlon <- pairlatlon %>%
  #select(-sec.y) %>%  # remove sec.y column
  #%>% # keep all unique observance events (need that id col in the excel sheet) %>%
 select(OffspringID, InferredDad1, offs.lat, offs.lon, par.lat, par.lon, ProbDad1)
  #rename(sec = sec.x)


  #attach lat/lon for siblings
  #idcsv <- data.frame(read.csv("174_colonypars_sibs_adultsep13.csv", header = TRUE), stringsAsFactors = FALSE)	
fullsib$sib1.date <- as.character(fullsib$sib1.date)
fullsib$sib1.ObsTime <- as.character(fullsib$sib1.ObsTime)
fullsib$sib2.date <- as.character(fullsib$sib2.date)
fullsib$sib2.ObsTime <- as.character(fullsib$sib2.ObsTime)


  # combine date and time to form a dttm column ####
    # set time zone to PHT, Asia/Manila
fullsib$otime <- str_c(fullsib$sib1.date, fullsib$sib1.ObsTime, sep = " ")
fullsib$ptime <- str_c(fullsib$sib2.date, fullsib$sib2.ObsTime, sep = " ")

fullsib$otime <- ymd_hms(fullsib$otime)
fullsib$ptime <- ymd_hms(fullsib$ptime)

fullsib$otime <- force_tz(fullsib$otime, tzone = "Asia/Manila")
fullsib$ptime <- force_tz(fullsib$ptime, tzone = "Asia/Manila")

  # change time zone to UTC ####
fullsib$otime <- with_tz(fullsib$otime, tzone = "UTC")
fullsib$ptime <- with_tz(fullsib$ptime, tzone = "UTC")

fullsib <- fullsib %>%
    mutate(omonth = month(otime)) %>%
    mutate(oday = day(otime)) %>%
    mutate(ohour = hour(otime)) %>%
    mutate(omin = minute(otime)) %>%
    mutate(osec = second(otime)) %>%
    mutate(pmonth = month(ptime)) %>%
    mutate(pday = day(ptime)) %>%
    mutate(phour = hour(ptime)) %>%
    mutate(pmin = minute(ptime)) %>%
    mutate(psec = second(ptime))

  #trim the data to keep only time and parent offspring matches, and then elminate repeats
fullsib<- distinct(fullsib)

latlong <- data.frame(leyte %>% tbl("GPX") %>% collect(n = Inf))

  latlongo <- latlong %>%
    mutate(month = month(time)) %>%
    mutate(day = day(time)) %>%
    mutate(hour = hour(time)) %>%
    mutate(min = minute(time)) %>%
    mutate(sec = second(time))
  
 names(latlongo) <- paste("sib1.", names(latlongo), sep = "")

  # find matches for times to assign lat long - there are more than one set of seconds (sec.y) that match
  fslatlon <- left_join(fullsib, latlongo, by = c(omonth="sib1.month", oday="sib1.day", ohour="sib1.hour", omin="sib1.min"))
  latlongp <- latlong %>%
    mutate(month = month(time)) %>%
    mutate(day = day(time)) %>%
    mutate(hour = hour(time)) %>%
    mutate(min = minute(time)) %>%
    mutate(sec = second(time))
  names(latlongp) <- paste("sib2.", names(latlongp), sep = "")
  fslatlon <- left_join(fslatlon, latlongp, by = c(pmonth="sib2.month", pday="sib2.day", phour="sib2.hour", pmin="sib2.min"))

  #now trim to just necessary columns
  #### HERE CAN I USE GROUP BY ID OR OBSTIME AND THEN SUMMARISE TO GET THE MEAN LAT LON OR MIN LAT LON AND CREATE A NEW TABLE WITH ALL COLUMNS BUT ONLY ONE LAT LON PER OBS
  # because all of the lat longs for the 4 observations are basically the same, remove sec.y and find distinct values
  fslatlon<- fslatlon %>%
    #select(-sec.y) %>%  # remove sec.y column
   #%>% # keep all unique observance events (need that id col in the excel sheet) %>%
   select(sib1, sib2, sib1.lat, sib1.lon, sib2.lat, sib2.lon, Probability)
    #rename(sec = sec.x)
  
  
    #attach lat/lon for half siblings
    #idcsv <- data.frame(read.csv("174_colonypars_sibs_adultsep13.csv", header = TRUE), stringsAsFactors = FALSE)	
  halfsib$hsib1.date <- as.character(halfsib$hsib1.date)
  halfsib$hsib1.ObsTime <- as.character(halfsib$hsib1.ObsTime)
  halfsib$hsib2.date <- as.character(halfsib$hsib2.date)
  halfsib$hsib2.ObsTime <- as.character(halfsib$hsib2.ObsTime)


    # combine date and time to form a dttm column ####
      # set time zone to PHT, Asia/Manila
  halfsib$otime <- str_c(halfsib$hsib1.date, halfsib$hsib1.ObsTime, sep = " ")
  halfsib$ptime <- str_c(halfsib$hsib2.date, halfsib$hsib2.ObsTime, sep = " ")

  halfsib$otime <- ymd_hms(halfsib$otime)
  halfsib$ptime <- ymd_hms(halfsib$ptime)

  halfsib$otime <- force_tz(halfsib$otime, tzone = "Asia/Manila")
  halfsib$ptime <- force_tz(halfsib$ptime, tzone = "Asia/Manila")

    # change time zone to UTC ####
  halfsib$otime <- with_tz(halfsib$otime, tzone = "UTC")
  halfsib$ptime <- with_tz(halfsib$ptime, tzone = "UTC")

  halfsib <- halfsib %>%
      mutate(omonth = month(otime)) %>%
      mutate(oday = day(otime)) %>%
      mutate(ohour = hour(otime)) %>%
      mutate(omin = minute(otime)) %>%
      mutate(osec = second(otime)) %>%
      mutate(pmonth = month(ptime)) %>%
      mutate(pday = day(ptime)) %>%
      mutate(phour = hour(ptime)) %>%
      mutate(pmin = minute(ptime)) %>%
      mutate(psec = second(ptime))

    #trim the data to keep only time and parent offspring matches, and then elminate repeats
  halfsib<- distinct(halfsib)

  latlong <- data.frame(leyte %>% tbl("GPX") %>% collect(n = Inf))

    latlongo <- latlong %>%
      mutate(month = month(time)) %>%
      mutate(day = day(time)) %>%
      mutate(hour = hour(time)) %>%
      mutate(min = minute(time)) %>%
      mutate(sec = second(time))
  
   names(latlongo) <- paste("hsib1.", names(latlongo), sep = "")

    # find matches for times to assign lat long - there are more than one set of seconds (sec.y) that match
    hslatlon <- left_join(halfsib, latlongo, by = c(omonth="hsib1.month", oday="hsib1.day", ohour="hsib1.hour", omin="hsib1.min"))
    latlongp <- latlong %>%
      mutate(month = month(time)) %>%
      mutate(day = day(time)) %>%
      mutate(hour = hour(time)) %>%
      mutate(min = minute(time)) %>%
      mutate(sec = second(time))
    names(latlongp) <- paste("hsib2.", names(latlongp), sep = "")
    hslatlon <- left_join(hslatlon, latlongp, by = c(pmonth="hsib2.month", pday="hsib2.day", phour="hsib2.hour", pmin="hsib2.min"))

    #now trim to just necessary columns
    #### HERE CAN I USE GROUP BY ID OR OBSTIME AND THEN SUMMARISE TO GET THE MEAN LAT LON OR MIN LAT LON AND CREATE A NEW TABLE WITH ALL COLUMNS BUT ONLY ONE LAT LON PER OBS
    # because all of the lat longs for the 4 observations are basically the same, remove sec.y and find distinct values
    hslatlon<- hslatlon %>%
      #select(-sec.y) %>%  # remove sec.y column
     # %>% # keep all unique observance events (need that id col in the excel sheet) %>%
     select(hsib1, hsib2, hsib1.lat, hsib1.lon, hsib2.lat, hsib2.lon, Probability)
      #rename(sec = sec.x)






#make a map with connectivity
#start by defining edges and vertices
#first get rid of low probability matches
fslatlon <-filter(fslatlon, Probability > 0.98)
hslatlon <- filter(hslatlon, Probability > 0.98)
pairlatlon <- filter(pairlatlon, ProbDad1 > 0.98)

edgesp <- data.frame(select(pairlatlon, OffspringID, InferredDad1, offs.lat, offs.lon, par.lat, par.lon), stringsAsFactors = FALSE)
edgesp$type <- "parent"
edgesfs <- data.frame(select(fslatlon, sib1, sib2, sib1.lat, sib1.lon, sib2.lat, sib2.lon), stringsAsFactors = FALSE)
edgesfs$type <- "fullsib"
edgeshs <- select(hslatlon, hsib1, hsib2, hsib1.lat, hsib1.lon, hsib2.lat, hsib2.lon)
edgeshs$type <- "halfsib"
names(edgesp) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "type")
names(edgesfs) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "type")
names(edgeshs) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "type")
edges1 <- rbind(edgesp, edgesfs)
edges2 <- rbind(edges1, edgeshs)


nodes1 <- select(edgesp, one)
nodes2 <- select(edgesp, two)
nodes3 <- select(edgesfs, one)
nodes4 <- select(edgesfs, two)
nodes5 <- select(edgeshs, one)
nodes6 <- select(edgeshs, two)
nodesa <- full_join(nodes1, nodes2, by=c(one = "two"))
nodesb <-full_join(nodesa, nodes3, by=c(one = "one"))
nodesc <-full_join(nodesb, nodes4, by=c(one = "two"))
nodesd <-full_join(nodesc, nodes5, by=c(one = "one"))
nodese <-full_join(nodesd, nodes6, by=c(one = "two"))


#remove duplicate vertexs and nodes
nodes <- distinct(nodesc)
#group by the ids that repeat, and then only keep the first line of them, elminates duplicates
edges <- edges2 %>% group_by(one, two) %>% filter(row_number() == 1) 



#read in high res leyte map to map lat/long points on
map <- readShapeSpatial("coastlines_leyte1.shp") 
map(map)
points(dat$lon, dat$lat, pch=19, col="red", cex=0.1)


library(marmap) ; library(mapdata);library(RColorBrewer);library(classInt)
dat <- getNOAA.bathy(-78,-64,37,43,res=1, keep=TRUE)
# Create color palettes
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))
plot(dat, xlim=c(-76.5,-65.5), ylim=c(38,41.5),im=TRUE, land=TRUE, bpal=list(c(min(dat),0,blues),c(0,max(dat),greys)), lwd=.09, las=1,cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75 )
map("worldHires", res=0, lwd=0.7, add=TRUE)
plot(dat, deep=-25, shallow=-25, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE)
plot(dat, deep=-50, shallow=-50, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE)
plot(dat, deep=-75, shallow=-75, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE)
plot(dat, deep=-100, shallow=-100, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE)
