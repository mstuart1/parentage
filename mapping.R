#mapping connectivity
# Set up working space ----------------------------------------------------
setwd("~/Documents/GradSchool/parentage")
source("~/Documents/GradSchool/parentage/samplefromlig.R")
source("~/Documents/GradSchool/parentage/conleyte.R")
source("~/Documents/GradSchool/parentage/conlabor.R")
source("~/Documents/GradSchool/parentage/katPlot.R")

suppressMessages(library(dplyr))
library(lubridate)
library(stringr)
library(maps)
library(mapdata)
library(maptools)
library(geosphere)
library(igraph)
library(sp)
library(RColorBrewer)
library(igraph)
library(scales)
library(ggplot2)
library(ggmap)
library(plotrix)
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
fullsib <- read.table(file= "174HQ_parentage_2014.FullSibDyad.txt", header= TRUE)
halfsib <- read.table(file= "174HQloci_adult_sep_2013WP1.HalfSibDyad.txt", header= TRUE)	
pairs <- read.table(file= "174HQ_parentage_2014.Maternity.txt", header= TRUE)

#fs <- sampfromlig(fullsib[1])#for some reason, not getting all the sample ids with this method, going to try old way
#fs2 <- sampfromlig(fullsib[2])
# add lab IDs
labor <- conlabor()
c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
c5 <- collect(left_join(c4, c3, by = "digest_id"))

first <- c5
names(first) <- paste("sib1.", names(first), sep = "")
fullsib <- left_join(fullsib, first, by=c(sib1="sib1.ligation_id"))
sec <- c5
names(sec) <- paste("sib2.", names(sec), sep = "")
fullsib <- left_join(fullsib, sec, by=c(sib2="sib2.ligation_id"))

c1 <- leyte %>% tbl("diveinfo") %>% select(id, date, name)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, ObsTime, GPS)
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
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, ObsTime, GPS)
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
pairs <- left_join(pairs, sec, by=c(InferredMum1="par.ligation_id"))

c1 <- leyte %>% tbl("diveinfo") %>% select(id, date, name)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, ObsTime, GPS)
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
pairs <- pairs %>% select(OffspringID, InferredMum1, offs.ObsTime, offs.date, par.ObsTime, par.date, offs.Size, par.Size, ProbMum1, par.name, offs.name)#, GPS)
fullsib <- fullsib %>% select(sib1, sib2, sib1.ObsTime, sib1.date, sib2.ObsTime, sib2.date, sib1.Size, sib2.Size, Probability, sib1.name, sib2.name)#, GPS)
halfsib <- halfsib %>% select(hsib1, hsib2, hsib1.ObsTime, hsib1.date, hsib2.ObsTime, hsib2.date, hsib1.Size, hsib2.Size, Probability, hsib1.name, hsib2.name)#, GPS)

#attach lat/lon for parentage
#idcsv <- data.frame(read.csv("174_colonypars_sibs_adultsep13.csv", header = TRUE), stringsAsFactors = FALSE)	
#pairs$offs.date <- as.character(pairs$offs.date)
#pairs$offs.ObsTime <- as.character(pairs$offs.ObsTime)
#pairs$par.date <- as.character(pairs$par.date)
#pairs$par.ObsTime <- as.character(pairs$par.ObsTime)


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

pairs$offs.ObsTime <- hms(pairs$offs.ObsTime)
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
latlong$time <- ymd_hms(latlong$time)

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
 select(OffspringID, InferredMum1, offs.lat, offs.lon, par.lat, par.lon, offs.name, par.name, ProbMum1)
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
latlong$time <- ymd_hms(latlong$time)
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
 select(sib1, sib2, sib1.lat, sib1.lon, sib2.lat, sib2.lon, sib1.name, sib2.name, Probability)
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
latlong$time <- ymd_hms(latlong$time)

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

latlongo <-latlong 
#trim the data to keep only time and parent offspring matches, and then elminate repeats
halfsib<- distinct(halfsib)

latlong <- data.frame(leyte %>% tbl("GPX") %>% collect(n = Inf))
latlong$time <- ymd_hms(latlong$time)

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
#### HERE CAN I USE GROUP BY ID OR OBSTIME AND THEN SUMMARISE TO GET THE MEAN LAT LON OR MIN LAT LON AND CREATE A NEW TABLE WITH ALL COLUMNS
	#BUT ONLY ONE LAT LON PER OBS
# because all of the lat longs for the 4 observations are basically the same, remove sec.y and find distinct values
hslatlon<- hslatlon %>%
  #select(-sec.y) %>%  # remove sec.y column
 # %>% # keep all unique observance events (need that id col in the excel sheet) %>%
select(hsib1, hsib2, hsib1.lat, hsib1.lon, hsib2.lat, hsib2.lon, hsib1.name, hsib2.name, Probability)
   #rename(sec = sec.x)






#make a map with connectivity of INDIVIDUALS
#start by defining edges and vertices
#first get rid of low probability matches
fslatlon <-filter(fslatlon, Probability > 0.98)
fslatlon <- fslatlon %>% group_by(sib1, sib2) %>% filter(row_number() == 1)
hslatlon <- filter(hslatlon, Probability > 0.98)
hslatlon <- hslatlon %>% group_by(hsib1, hsib2) %>% filter(row_number() == 1)
pairlatlon <- filter(pairlatlon, ProbMum1 > 0.98)
pairlatlon <- pairlatlon %>% group_by(OffspringID, InferredMum1) %>% filter(row_number() == 1)

	
edgesp <- data.frame(select(pairlatlon, OffspringID, InferredMum1, offs.lat, offs.lon, par.lat, par.lon), stringsAsFactors = FALSE)
#edgesp$type <- "parent"
edgesfs <- data.frame(select(fslatlon, sib1, sib2, sib1.lat, sib1.lon, sib2.lat, sib2.lon), stringsAsFactors = FALSE)
#edgesfs$type <- "fullsib"
edgeshs <- data.frame(select(hslatlon, hsib1, hsib2, hsib1.lat, hsib1.lon, hsib2.lat, hsib2.lon), stringsAsFactors = FALSE)
#edgeshs$type <- "halfsib"
names(edgesp) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon")
names(edgesfs) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon")
names(edgeshs) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon")
edges1 <- bind_rows(edgesp, edgesfs)
edges2 <- bind_rows(edges1, edgeshs)

#now nodes need to be in format (id, lon, lat), then make layout from nodes and edges need to be (one, two)

#group by the ids that repeat, and then only keep the first line of them, elminates duplicates
edges <- edges2 %>% group_by(one, two) %>% filter(row_number() == 1) 

nodesone <- edges %>% group_by(one) %>% filter(row_number() == 1) 
nodestwo <- edges %>% group_by(two) %>% filter(row_number() == 1) 
nodes <- full_join(nodesone, nodestwo)

edges <- edges%>% group_by(one, two) %>% filter(row_number() == 1) 
edges <- data.frame(select(edges, one, two), stringsAsFactors= FALSE)

nodes <- as.data.frame(nodes)
nodes1 <- data.frame(select(nodes, one, one.lon, one.lat), stringsAsFactors= FALSE)
nodes2 <- data.frame(select(nodes, two, two.lon, two.lat), stringsAsFactors= FALSE)
names(nodes2) <- c("one", "one.lon", "one.lat")
nodes <- bind_rows(nodes1, nodes2)
nodes <-distinct(nodes)


meta <- nodes
lo <- as.matrix(meta[,2:3])
#get rid of quotes, delete first column when you write csv
write.csv(lo, file="lo2014.csv", col.names=TRUE)
lo <- read.csv(file="lo2014.csv", header=TRUE)


#read in high res leyte map to map lat/long points on
library(rgdal)
map_new <- readOGR("leyte", "coastlines_leyte")

plot(map_new, xlim=c(124.63, 124.8 ), ylim=c(10.8,10.83))


a<- graph_from_data_frame(vertices = nodes, d= edges, directed = TRUE)
plot(a, rescale=FALSE, asp=0, add=TRUE, edge.arrow.size=.2, vertex.size=.4, vertex.frame.color=NA, 
	 vertex.label.cex=.6, vertex.label.degree=pi, vertex.label.dist=.08, edge.width=edges$number, 
		edge.color="slate grey", layout=lo, edge.curved=.1, vertex.label.font=2, vertex.label.color="gray40")
#TO DO: MAKE A MAP OF SIBLING AND HALFSIBLING RELATEDNESS ACCORING TO SITE, RUN MIGEST


#map connections by location
edgesp <- data.frame(select(pairlatlon, OffspringID, InferredMum1, offs.lat, offs.lon, par.lat, par.lon, par.name, offs.name), stringsAsFactors = FALSE)
edgesfs <- data.frame(select(fslatlon, sib1, sib2, sib1.lat, sib1.lon, sib2.lat, sib2.lon, sib1.name, sib2.name), stringsAsFactors = FALSE)
edgeshs <- data.frame(select(hslatlon, hsib1, hsib2, hsib1.lat, hsib1.lon, hsib2.lat, hsib2.lon, hsib1.name, hsib2.name), stringsAsFactors=FALSE)
names(edgesp) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "one.name", "two.name")#"type")
names(edgesfs) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "one.name", "two.name")# "type")
edgesfs<- edgesfs %>% group_by(one, two) %>% filter(row_number() == 1) 
names(edgeshs) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "one.name", "two.name")# "type")
edgeshs<- edgeshs %>% group_by(one, two) %>% filter(row_number() == 1) 

#for parents
edgesp$one <-edgesp$one.name
edgesp$two <- edgesp$two.name
nodesp <-edgesp[,1:6]
nodesp1 <-nodesp %>% select(one, one.lon, one.lat)
nodesp2 <-nodesp %>% select(two, two.lon, two.lat)
names(nodesp2) <- c("one", "one.lon", "one.lat")
nodes <- bind_rows(nodesp1, nodesp2)

#rearrange # nodes <- nodes[c(1,3,2)]
nodesp<- nodes %>% group_by(one) %>% filter(row_number() == 1) 

#the lo and nodes is weird lat/lon, so load the edits csv where I fixed the spacing and have all of the labels, then join with the nodes
edits <- read.csv(file="editlabels13.csv", header=TRUE)
nodes1 <-inner_join(nodes, edits, by=c(one="site"))
nodes1 <- select(nodes1, one, lon, lat)
nodesp<- nodes1 %>% group_by(one) %>% filter(row_number() == 1) 

meta <- nodesp
lo <- as.matrix(meta[,2:3])

#get rid of quotes, delete first column when you write csv
write.csv(lo, file="lo2014locationsp.csv", col.names=TRUE)
lo <- read.csv(file="lo2014locationsp.csv", header=TRUE)

edgesp <- edgesp %>% group_by(one, two) %>% filter(row_number() == 1) 
edgesp <- edgesp %>% count(one, two)

plot(map_new, xlim=c(124.7, 124.8 ), ylim=c(10.65,10.85))

a <- graph_from_data_frame(d=edgesp, vertices=nodesp, directed=TRUE)

katPlot(a, rescale=FALSE, asp=0, add=TRUE, edge.arrow.size=.2, vertex.size=.4, vertex.frame.color=NA, 
	 vertex.label.cex=.7, vertex.color= "orange", vertex.label.degree=pi, vertex.label.dist=.1, edge.width=edgesp$n,
		edge.color="slate gray", layout=lo, edge.curved=.4, vertex.label.font=2, vertex.label.color="black")

#plot siblings and half siblings on a map as points with graduated size to show density of siblings
edgesfs$one <-edgesfs$one.name
edgesfs$two <- edgesfs$two.name
nodesfs <-edgesfs[,1:6]
nodesfs <-as.data.frame(nodesfs)
nodesfs1 <-nodesfs %>% select(one, one.lon, one.lat)
nodesfs2 <-nodesfs %>% select(two, two.lon, two.lat)
names(nodesfs2) <- c("one", "one.lon", "one.lat")
nodesfs <- bind_rows(nodesfs1, nodesfs2)
nodesfs1 <- nodesfs %>% count(one) 
nodesfs <-left_join(nodesfs, nodesfs1)
nodesfs<- nodesfs %>% group_by(one) %>% filter(row_number() == 1) 

#gather 
plot(map_new, xlim=c(124.7, 124.8 ), ylim=c(10.60,10.89))
points(nodesfs$one.lon, nodesfs$one.lat, pch=10, col=alpha("dark blue", 0.3), cex=nodesfs$n/6)
points(nodesfs$one.lon, nodesfs$one.lat, pch=19, col=alpha("blue", 0.3), cex=nodesfs$n/6)

#move points for crunchy sites

edits <- read.csv(file="editlabels13.csv", header=TRUE)

text(edits$lon, edits$lat, labels=edits$site, col="black", cex=.6, offset=.2, pos=2)

#for half siblings
edgeshs$one <-edgeshs$one.name
edgeshs$two <- edgeshs$two.name
nodeshs <-edgeshs[,1:6]
nodeshs <-as.data.frame(nodeshs)
nodeshs1 <-nodeshs %>% select(one, one.lon, one.lat)
nodeshs2 <-nodeshs %>% select(two, two.lon, two.lat)
names(nodeshs2) <- c("one", "one.lon", "one.lat")
nodeshs <- bind_rows(nodeshs1, nodeshs2)
nodeshs1 <- nodeshs %>% count(one) 
nodeshs <-left_join(nodeshs, nodeshs1)
nodeshs<- nodeshs %>% group_by(one) %>% filter(row_number() == 1) 

#gather 
plot(map_new, xlim=c(124.7, 124.8 ), ylim=c(10.60,10.89))
points(nodeshs$one.lon, nodeshs$one.lat, pch=10, col=alpha("dark blue", 0.3), cex=nodeshs$n/6)
points(nodeshs$one.lon, nodeshs$one.lat, pch=19, col=alpha("blue", 0.3), cex=nodeshs$n/6)

#move points for crunchy sites

edits <- read.csv(file="editlabels13.csv", header=TRUE)

text(edits$lon, edits$lat, labels=edits$site, col="black", cex=.6, offset=.2, pos=2)




#connectivity for siblings (kinda messy)
edgesfs$one <-edgesfs$one.name
edgesfs$two <- edgesfs$two.name
nodesfs <-edgesfs[,1:6]
nodesfs1 <-nodesfs %>% select(one, one.lon, one.lat)
nodesfs2 <-nodesfs %>% select(two, two.lon, two.lat)
names(nodesfs2) <- c("one", "one.lon", "one.lat")
nodes <- bind_rows(nodesfs1, nodesfs2)

#rearrange # nodes <- nodes[c(1,3,2)]
nodesfs<- nodes %>% group_by(one) %>% filter(row_number() == 1) 

#the lo and nodes is weird lat/lon, so load the edits csv where I fixed the spacing and have all of the labels, then join with the nodes
edits <- read.csv(file="editlabels13.csv", header=TRUE)
nodes1 <-inner_join(nodesfs, edits, by=c(one="site"))
nodes1 <- select(nodes1, one, lon, lat)
nodesfs<- nodes1 %>% group_by(one) %>% filter(row_number() == 1) 

meta <- nodesfs
lo <- as.matrix(meta[,2:3])

#get rid of quotes, delete first column when you write csv
write.csv(lo, file="lo2014locationsfs.csv", col.names=TRUE)
lo <- read.csv(file="lo2014locationsfs.csv", header=TRUE)

edgesfs <- edgesfs %>% group_by(one, two) %>% filter(row_number() == 1) 
edgesfs <- edgesfs %>% count(one, two)

plot(map_new, xlim=c(124.7, 124.8 ), ylim=c(10.65,10.85))

a <- graph_from_data_frame(d=edgesfs, vertices=nodesfs, directed=TRUE)

katPlot(a, rescale=FALSE, asp=0, add=TRUE, edge.arrow.size=.2, vertex.size=.4, vertex.frame.color=NA, 
	 vertex.label.cex=.7, vertex.color= "orange", vertex.label.degree=pi, vertex.label.dist=.1, edge.width=edgesp$n,
		edge.color="slate gray", layout=lo, edge.curved=.4, vertex.label.font=2, vertex.label.color="black")
