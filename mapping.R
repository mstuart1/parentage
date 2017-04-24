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
library(geosphere)
library(igraph)
library(sp)

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
fullsib <- read.table(file= "174HQloci_adult_sep_2013WP1.FullSibDyad.txt", header= TRUE)
halfsib <- read.table(file= "174HQloci_adult_sep_2013WP1.HalfSibDyad.txt", header= TRUE)	
pairs <- read.table(file= "174HQloci_adult_sep_2013WP1.Maternity.txt", header= TRUE)

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
pairs <- pairs %>% select(OffspringID, InferredMum1, offs.ObsTime, offs.date, par.ObsTime, par.date, offs.Size, par.Size, ProbMum1)#, GPS)
fullsib <- fullsib %>% select(sib1, sib2, sib1.ObsTime, sib1.date, sib2.ObsTime, sib2.date, sib1.Size, sib2.Size, Probability)#, GPS)
halfsib <- halfsib %>% select(hsib1, hsib2, hsib1.ObsTime, hsib1.date, hsib2.ObsTime, hsib2.date, hsib1.Size, hsib2.Size, Probability)#, GPS)

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
 select(OffspringID, InferredMum1, offs.lat, offs.lon, par.lat, par.lon, ProbMum1)
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
select(hsib1, hsib2, hsib1.lat, hsib1.lon, hsib2.lat, hsib2.lon, Probability)
   #rename(sec = sec.x)






#make a map with connectivity
#start by defining edges and vertices
#first get rid of low probability matches
fslatlon <-filter(fslatlon, Probability > 0.98)
fslatlon <- fslatlon %>% group_by(sib1, sib2) %>% filter(row_number() == 1)
hslatlon <- filter(hslatlon, Probability > 0.98)
hslatlon <- hslatlon %>% group_by(hsib1, hsib2) %>% filter(row_number() == 1)
pairlatlon <- filter(pairlatlon, ProbMum1 > 0.98)
pairlatlon <- pairlatlon %>% group_by(OffspringID, InferredMum1) %>% filter(row_number() == 1)

	
edgesp <- data.frame(select(pairlatlon, OffspringID, InferredMum1, offs.lat, offs.lon, par.lat, par.lon), stringsAsFactors = FALSE)
edgesp$type <- "parent"
edgesfs <- data.frame(select(fslatlon, sib1, sib2, sib1.lat, sib1.lon, sib2.lat, sib2.lon), stringsAsFactors = FALSE)
edgesfs$type <- "fullsib"
edgeshs <- select(hslatlon, hsib1, hsib2, hsib1.lat, hsib1.lon, hsib2.lat, hsib2.lon)
edgeshs$type <- "halfsib"
names(edgesp) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "type")
names(edgesfs) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "type")
names(edgeshs) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "type")
edges1 <- bind_rows(edgesp, edgesfs)
edges2 <- bind_rows(edges1, edgeshs)


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
edges <- distinct(edges2)
matches <- edges 
matches <- matches %>% select(one, two, type) 
nodes <- edges %>% group_by(one) %>% filter(row_number() == 1) 
edges <- matches%>% group_by(one, two) %>% filter(row_number() == 1) 

matches <- distinct(matches)
write.csv(edges, file="2013edges.csv", col.names=TRUE)

#create data frame OR
nodes <- edges$one
x <- edges$one.lat
y <- edges$one.lon
from <- matches$one
to <- matches$two
NodeList <- data.frame(nodes, x ,y)
EdgeList <- data.frame(from, to)

#just load data frames
nodes <- read.csv(file="2013edges.csv", header=TRUE)
matches <- read.csv(file="2013matches.csv", header=TRUE)

#remove duplicates
edges <- matches%>% group_by(one, two) %>% filter(row_number() == 1) 
nodes <- nodes %>% group_by(id) %>% filter(row_number() == 1) 


library(rgdal)
map_new <- readOGR("leyte", "coastlines_leyte")

plot(map_new)
#read in high res leyte map to map lat/long points on
meta <- nodes
lo <- as.matrix(meta[,2:3])
plot(nodes$x, nodes$y, type="n")
a<- graph_from_data_frame(vertices = nodes, d= edges, directed = TRUE)
plot(a, rescale=FALSE, asp=0, add=TRUE, edge.arrow.size=.1, vertex.size=.25, vertex.frame.color="gray", 
		vertex.label.color="black", vertex.label.cex=0.2, vertex.label.dist=0, edge.width=.5, 
		edge.color=c("orange", "green", "blue")[(E(a)$type=="parent")+2], layout=lo)

plot(map_new, xlim=c(124.684279, 124.829 ), ylim=c(10.621,10.999))

xlim=c(124.684279, 124.829 ), ylim=c(10.621,10.999),




edge.color=c("orange", "green", "blue")[(E(a)$type=="parent")+2],

fsub <- edges[edges$type == "parent",]
for (j in 1:length(edges)) {
    nodes1 <- edges[edges$one == fsub[j,]$one,]
    nodes2 <- edges[edges$two == fsub[j,]$two,]

    inter <- gcIntermediate(c(edges[1,]$one.lon, edges[1,]$one.lat), c(edges[1,]$two.lon, edges[1,]$two.lat), n=7, addStartEnd=TRUE)
      
    lines(inter, col="black", lwd=0.8)
}

for(i in 1:nrow(flights))  {
    node1 <- airports[airports$ID == flights[i,]$Source,]
    node2 <- airports[airports$ID == flights[i,]$Target,]
    
    arc <- gcIntermediate( c(node1[1,]$longitude, node1[1,]$latitude), 
                           c(node2[1,]$longitude, node2[1,]$latitude), 
                           n=1000, addStartEnd=TRUE )
    edge.ind <- round(100*flights[i,]$Freq / max(flights$Freq))
    
    lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}
	   
   
	   
	   
	   
	   
	   
	   
	   
	   
	   


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

coocNet<-network(edges,
                 matrix.type='edgelist',
                 directed=FALSE,  # this will be an undirected network
                 ignore.eval=TRUE,  # confusingly, this tells it to include edge weights
                 )
				
plot.network(coocNet,  # pass in the network
# don't erase the map before drawing the network
new=FALSE, 
# get coordiantes from vertices and pass in as 2-col matrix
coord=cbind(coocNet%v%'lon',coocNet%v%'lat'),  
# ---- all the rest of these are optional to make it look nice ------
 # set a semi-transparent edge color
 edge.col='#AA555555',
 # specifiy an edge width scaled as fraction of total co-occurence
 edge.lwd=coocNet%e%'Tot_cooc'/500,
 # set the vertex size
 vertex.cex=0.5,
 # set a semi transparent vertex color
 vertex.col='#AA555555',
 vertex.border='white',
 # please don't jitter the points around
 jitter=FALSE)


