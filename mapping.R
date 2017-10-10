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
library(igraph)
library(scales)
library(plotrix)
#need to get the lat long id for all parentage matches and map, then use igraph to connect results

leyte <- conleyte()
labor <- conlabor()


#load tables with parent offspring matches and attach lat lon
#prepare full likelihood parentage/sibship results for mapping
pairs <- read.table(file= "20170927colony13.Maternityc.txt", header= TRUE)

# add lab IDs

c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
labid <- collect(left_join(c4, c3, by = "digest_id"))

first <- labid
names(first) <- paste("offs.", names(first), sep = "")
pairs <- left_join(pairs, first, by=c(OffspringID="offs.ligation_id"))
sec <- labid
names(sec) <- paste("par.", names(sec), sep = "")
pairs <- left_join(pairs, sec, by=c(InferredMum1="par.ligation_id"))

c1 <- leyte %>% tbl("diveinfo") %>% select(dive_table_id, date, site, gps, dive_type)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, obs_time)
c3 <- as.data.frame(left_join(c2, c1, by ="dive_table_id"))
c4 <- as.data.frame(tbl(leyte, sql("SELECT fish_table_id, anem_table_id, sample_id, size FROM clownfish where sample_id is not NULL")))
fieldid <- as.data.frame((left_join(c4, c3, by = "anem_table_id")))
dim(fieldid)
stuff <- fieldid %>% group_by(dive_type) %>% filter(dive_type=="C")
#now add parent info

first <- stuff
names(first) <- paste("offs.", names(first), sep = "")
pairs <- left_join(pairs, first, by="offs.sample_id")
second <- stuff
names(second) <- paste("par.", names(second), sep = "")
pairs <- left_join(pairs, second, by="par.sample_id")
#2014 L1250 looses it's site name in this process... can't figure out why. If you go back through the script you can see L1250 came from Carirdad Proper

#clean up, remove unnecessary columns
pairs <- pairs %>% select(OffspringID, InferredMum1, offs.obs_time, offs.date, par.obs_time, par.date, offs.size, par.size, ProbMum1, par.site, offs.site, par.gps, offs.gps)

# combine date and time to form a dttm column ####
  # set time zone to PHT, Asia/Manila
pairs$otime <- str_c(pairs$offs.date, pairs$offs.obs_time, sep = " ")
pairs$ptime <- str_c(pairs$par.date, pairs$par.obs_time, sep = " ")

pairs$otime <- ymd_hms(pairs$otime)
pairs$ptime <- ymd_hms(pairs$ptime)

pairs$otime <- force_tz(pairs$otime, tzone = "Asia/Manila")
pairs$ptime <- force_tz(pairs$ptime, tzone = "Asia/Manila")

# change time zone to UTC ####
pairs$otime <- with_tz(pairs$otime, tzone = "UTC")
pairs$ptime <- with_tz(pairs$ptime, tzone = "UTC")

pairs$offs.obs_time <- hms(pairs$offs.obs_time)
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
pairs <- distinct(pairs) #seems to be no need for this line but keep it in here as a reminder if I run into other problems

#20170905 checked through here... thinking problem is in the gps unit number matching
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
pairlatlon <- left_join(pairs, latlongo, by = c(omonth="offs.month", oday="offs.day", ohour="offs.hour", omin="offs.min", offs.gps="offs.unit"))
latlongp <- latlong %>%
  mutate(month = month(time)) %>%
  mutate(day = day(time)) %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time))
  

names(latlongp) <- paste("par.", names(latlongp), sep = "")
pairlatlon <- left_join(pairlatlon, latlongp, by = c(pmonth="par.month", pday="par.day", phour="par.hour", pmin="par.min",  par.gps="par.unit"))

#now trim to just necessary columns
#### HERE CAN I USE GROUP BY ID OR OBSTIME AND THEN SUMMARISE TO GET THE MEAN LAT LON OR MIN LAT LON AND CREATE A NEW TABLE WITH ALL COLUMNS BUT ONLY ONE LAT LON PER OBS
# because all of the lat longs for the 4 observations are basically the same, remove sec.y and find distinct values
pairlatlon <- pairlatlon %>%
  #select(-sec.y) %>%  # remove sec.y column
  #%>% # keep all unique observance events (need that id col in the excel sheet) %>%
 select(OffspringID, InferredMum1, offs.lat, offs.lon, par.lat, par.lon, offs.site, par.site, ProbMum1) %>% filter(ProbMum1 >= .98)
  #rename(sec = sec.x)


#map connections by location
edgesp <- data.frame(select(pairlatlon, InferredMum1 , OffspringID , par.lat, par.lon, offs.lat, offs.lon, par.site, offs.site), stringsAsFactors = FALSE)
names(edgesp) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "one.name", "two.name")#"type")
edgesp<- edgesp %>% group_by(one, two) %>% filter(row_number() == 1) 

#for parents
edgesp$one <-edgesp$one.name
edgesp$two <- edgesp$two.name
write.csv(edgesp, file="edgesp.csv", col.names=TRUE, quote=TRUE)
edgesp <- as.data.frame(read.csv(file="edgesp.csv", header=TRUE))
edgesp <- edgesp[,2:7]#, [,2:7]
nodesp <-edgesp#[,1:6]
nodesp1 <-nodesp %>% ungroup() %>% select(one, one.lon, one.lat)
nodesp2 <-nodesp %>%  ungroup() %>% select(two, two.lon, two.lat)
names(nodesp2) <- c("one", "one.lon", "one.lat")
nodes <- bind_rows(nodesp1, nodesp2)
names(nodes) <- c("one", "lon", "lat")

nodesp<- nodes %>% group_by(one) %>% filter(row_number() == 1) 
#names are inconsistent, write csv, edit, reload, MAY NOT HAVE TO DO THIS IF DID IT WITH EDGES ABOVE
#write.csv(nodesp, file="nodesp.csv", col.names=TRUE, quote=TRUE)
#nodesp <- as.data.frame(read.csv(file="nodesp.csv", header=TRUE))
#nodesp <- nodesp[,2:4]
edits <- read.csv(file="editlabels13.csv", header=TRUE)
nodes1 <-semi_join(edits, nodesp, by=c(site="one"))
nodes1 <- select(nodes1, site, lon, lat)
edgesp <- edgesp %>% count(one, two)
edgesp$one <- as.character(edgesp$one)
edgesp$two <- as.character(edgesp$two)

meta <- nodesp
lo <- meta
write.csv(lo, file="lo.csv", col.names=TRUE)
lo <- as.data.frame(read.csv(file="lo.csv", header=TRUE))
lo <- lo[,2:3]
lo$lon <- as.numeric(lo$lon)
lo$lat <- as.numeric(lo$lat)
lo$one <- as.character(lo$one)
#names(lo) <- c("lon", "lat")
#lo <- lo %>% filter(lon != 'NA', lat != 'NA')

#the lo and nodes is weird lat/lon, so load the edits csv where I fixed the spacing and have all of the labels, then join with the nodes
library(rgdal)
map_new <- readOGR("leyte", "coastlines_leyte")
plot(map_new, xlim=c(124.79, 124.8 ), ylim=c(10.6,10.9))
text(edits$lon, edits$lat, labels=edits$site, col="black", cex=.7, offset=1.8, pos=4)

#edgesp <- edgesp %>% group_by(one, two) %>% filter(row_number() == 1) 
edgesp <- ungroup(edgesp)
nodesp <- ungroup(nodesp)

a <- graph_from_data_frame(d=edgesp, vertices=nodesp, directed=TRUE)

katPlot(a, rescale=FALSE, asp=0, add=TRUE, edge.arrow.size=.4, vertex.size=0, vertex.frame.color=NA, 
	 vertex.label=NA, vertex.color= "orange", vertex.label.dist=.1, edge.width=edgesp$n,
		edge.color="slate grey", layout=lo, edge.curved=-.8, vertex.label.font=0, vertex.label.color="black")

#Warning messages:
#1: In Ops.factor(layout[loops.v, 1], cos(la) * vs) :
#  ‘+’ not meaningful for factors
#2: In Ops.factor(layout[, 1], label.dist * cos(-label.degree) * (vertex.size +  :
#  ‘+’ not meaningful for factors
#this meant that the layout had site names still (just get rid of them), so make sure there are no factor




####HERE IS CODE FOR GROUPING THE SITES INTO THREE BINS, TO MAKE A BETTER VISUAL MAP OF CONNECTIVITY

twelve <- read.csv("pairdist2012.csv", header=TRUE)
thirteen <- read.csv("pairdist2013.csv", header=TRUE)
fourteen <- read.csv("pairdist2014.csv", header=TRUE)
fifteen <- read.csv("pairdist2015.csv", header=TRUE)

#add year grouping variables
twelve$year <- '2012'
thirteen$year <- '2013'
fourteen$year <- '2014'
fifteen$year <- '2015'

all <- bind_rows(twelve, thirteen, fourteen, fifteen)
all <- all %>% select(-X)
head(all)
#break matches up as region to region rather than site to site. Use geographic barriers like river north of visca and sand flats south of gabas
#for parents
target1 <- c("Palanas", "Wangag", "Magbangon", "Elementary School", "Sitio Tugas", "Sitio Lonas", "Cabatoan", "Caridad Cemetery",  "Cardidad Proper", "Poroc Rose", "Poroc San Flower", "San Agustin", "Hicgop", "Hicgop South")
north <- all %>% filter(par.site %in% target)
north$par.region <- "north"
target2 <- c("Visca")
mid <- all %>% filter(par.site %in% target2)
mid$par.region <- ("mid")
target3 <- c("Sitio Baybayon", "Tamakin Dacot", "Haina", "Tamakin Dacat")
south <- all%>% filter(par.site %in% target3)
south$par.region <- "south"

#for offspring
target1 <- c("Palanas", "Wangag", "Magbangon", "Elementary School", "Sitio Tugas", "Sitio Lonas", "Cabatoan", "Caridad Cemetery",  "Cardidad Proper", "Poroc Rose", "Poroc San Flower", "San Agustin", "Hicgop", "Hicgop South")
north2 <- all %>% filter(offs.site %in% target)
north2$offs.region <- "north"
target2 <- c("Visca")
mid2 <- all %>% filter(offs.site %in% target2)
mid2$offs.region <- ("mid")
target3 <- c("Sitio Baybayon", "Tamakin Dacot", "Haina", "Tamakin Dacat")
south2 <- all%>% filter(offs.site %in% target3)
south2$offs.region <- "south"

alloffs <- bind_rows(north2, mid2, south2)
allpars1 <- bind_rows(north, mid, south)
allpars <- allpars1 %>% select(OffspringID, InferredMum1, par.region)
allregions <- left_join(alloffs, allpars, by=c("OffspringID" , "InferredMum1"))
head(allregions)
#map connections by location
edgesp <- data.frame(select(allregions, InferredMum1 , OffspringID , par.lat, par.lon, offs.lat, offs.lon, par.region, offs.region), stringsAsFactors = FALSE)
names(edgesp) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "one.name", "two.name")#"type")
edgesp<- edgesp %>% group_by(one, two) %>% filter(row_number() == 1) 
head(edgesp)
#for parents
edgesp$one <-edgesp$one.name
edgesp$two <- edgesp$two.name
write.csv(edgesp, file="edgesp.csv", col.names=TRUE, quote=TRUE)
edgesp <- as.data.frame(read.csv(file="edgesp.csv", header=TRUE))
edgesp <- edgesp[,2:7]#, [,2:7]
nodesp <-edgesp#[,1:6]
nodesp1 <-nodesp %>% ungroup() %>% select(one, one.lon, one.lat)
nodesp2 <-nodesp %>%  ungroup() %>% select(two, two.lon, two.lat)
names(nodesp2) <- c("one", "one.lon", "one.lat")
nodes <- bind_rows(nodesp1, nodesp2)
names(nodes) <- c("one", "lon", "lat")

nodesp<- nodes %>% group_by(one) %>% filter(row_number() == 1) 
#names are inconsistent, write csv, edit, reload, MAY NOT HAVE TO DO THIS IF DID IT WITH EDGES ABOVE
#write.csv(nodesp, file="nodesp.csv", col.names=TRUE, quote=TRUE)
#nodesp <- as.data.frame(read.csv(file="nodesp.csv", header=TRUE))
#nodesp <- nodesp[,2:4]
edits <- read.csv(file="editlabels13.csv", header=TRUE)
nodes1 <-semi_join(edits, nodesp, by=c(site="one"))
nodes1 <- select(nodes1, site, lon, lat)
edgesp <- edgesp %>% count(one, two)
edgesp$one <- as.character(edgesp$one)
edgesp$two <- as.character(edgesp$two)

meta <- nodesp
lo <- meta
write.csv(lo, file="lo.csv", col.names=TRUE)
lo <- as.data.frame(read.csv(file="lo.csv", header=TRUE))
lo <- lo[,2:4]
lo$lon <- as.numeric(lo$lon)
lo$lat <- as.numeric(lo$lat)
lo$one <- NULL #as.character(lo$one)
#names(lo) <- c("lon", "lat")
#lo <- lo %>% filter(lon != 'NA', lat != 'NA')

#the lo and nodes is weird lat/lon, so load the edits csv where I fixed the spacing and have all of the labels, then join with the nodes

#it would be nice to code the site names by region. Let's give it a shot.
target1 <- c("Palanas", "Wangag", "Magbangon", "Elementary School", "Sitio Tugas", "Sitio Lonas", "Cabatoan", "Caridad Cemetery",  "Cardidad Proper", "Poroc Rose", "Poroc San Flower", "San Agustin", "Hicgop", "Hicgop South")
north <- edits %>% filter(site %in% target)
north$region <- "north"
target2 <- c("Visca", "Gabas")
mid <- edits %>% filter(site %in% target2)
mid$region <- ("mid")
target3 <- c("Sitio Baybayon", "Tamakin Dacot", "Haina", "Tamakin Dacat")
south <- edits%>% filter(site %in% target3)
south$region <- "south"
alllabs <- bind_rows(north, mid, south)
#make a factor to color by levels
alllabs$region <- as.factor(alllabs$region)

#add color
cols<-wes_palette(n=3,name="FantasticFox")

colors<-cols[alllabs$region]

library(rgdal)
map_new <- readOGR("leyte", "coastlines_leyte")
plot(map_new, xlim=c(124.79, 124.8 ), ylim=c(10.6,10.9))
text(alllabs$lon, alllabs$lat, labels=alllabs$site, cex=.7, offset=2.1, pos=4, font=2, col=colors)

#edgesp <- edgesp %>% group_by(one, two) %>% filter(row_number() == 1) 
edgesp <- ungroup(edgesp)
nodesp <- ungroup(nodesp)

a <- graph_from_data_frame(d=edgesp, vertices=nodesp, directed=TRUE)

katPlot(a, rescale=FALSE, asp=0, add=TRUE, edge.arrow.size=.6, vertex.size=0, vertex.frame.color=NA, 
	 vertex.label=NA, vertex.color= "orange", vertex.label.dist=1, edge.width=edgesp$n,
		edge.color="slate grey", layout=lo, edge.curved=-.8, vertex.label.font=0, vertex.label.color="black")
#edgesp is has 46 matches going north-north, would look better if I scaled it down. Divide by 10 below and then run above plot again
edgesp$n <- (edgesp$n)/5 


##########
#now do the same but with annual data
#break matches up as region to region rather than site to site. Use geographic barriers like river north of visca and sand flats south of gabas
#for parents
target1 <- c("Palanas", "Wangag", "Magbangon", "Elementary School", "Sitio Tugas", "Sitio Lonas", "Cabatoan", "Caridad Cemetery",  "Cardidad Proper", "Poroc Rose", "Poroc San Flower", "San Agustin", "Hicgop", "Hicgop South")
north <- fifteen %>% filter(par.site %in% target)
north$par.region <- "north"
target2 <- c("Visca")
mid <- fifteen %>% filter(par.site %in% target2)
mid$par.region <- ("mid")
target3 <- c("Sitio Baybayon", "Tamakin Dacot", "Haina", "Tamakin Dacat")
south <- fifteen%>% filter(par.site %in% target3)
south$par.region <- "south"

#for offspring
target1 <- c("Palanas", "Wangag", "Magbangon", "Elementary School", "Sitio Tugas", "Sitio Lonas", "Cabatoan", "Caridad Cemetery",  "Cardidad Proper", "Poroc Rose", "Poroc San Flower", "San Agustin", "Hicgop", "Hicgop South")
north2 <- fifteen %>% filter(offs.site %in% target)
north2$offs.region <- "north"
target2 <- c("Visca")
mid2 <- fifteen %>% filter(offs.site %in% target2)
mid2$offs.region <- ("mid")
target3 <- c("Sitio Baybayon", "Tamakin Dacot", "Haina", "Tamakin Dacat")
south2 <- fifteen%>% filter(offs.site %in% target3)
south2$offs.region <- "south"

alloffs <- bind_rows(north2, mid2, south2)
allpars1 <- bind_rows(north, mid, south)
allpars <- allpars1 %>% select(OffspringID, InferredMum1, par.region)
allregions <- left_join(alloffs, allpars, by=c("OffspringID" , "InferredMum1"))
head(allregions)
#map connections by location
edgesp <- data.frame(select(allregions, InferredMum1 , OffspringID , par.lat, par.lon, offs.lat, offs.lon, par.region, offs.region), stringsAsFactors = FALSE)
names(edgesp) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "one.name", "two.name")#"type")
edgesp<- edgesp %>% group_by(one, two) %>% filter(row_number() == 1) 
head(edgesp)
#for parents
edgesp$one <-edgesp$one.name
edgesp$two <- edgesp$two.name
write.csv(edgesp, file="edgesp.csv", col.names=TRUE, quote=TRUE)
edgesp <- as.data.frame(read.csv(file="edgesp.csv", header=TRUE))
edgesp <- edgesp[,2:7]#, [,2:7]
nodesp <-edgesp#[,1:6]
nodesp1 <-nodesp %>% ungroup() %>% select(one, one.lon, one.lat)
nodesp2 <-nodesp %>%  ungroup() %>% select(two, two.lon, two.lat)
names(nodesp2) <- c("one", "one.lon", "one.lat")
nodes <- bind_rows(nodesp1, nodesp2)
names(nodes) <- c("one", "lon", "lat")

nodesp<- nodes %>% group_by(one) %>% filter(row_number() == 1) 
#names are inconsistent, write csv, edit, reload, MAY NOT HAVE TO DO THIS IF DID IT WITH EDGES ABOVE
#write.csv(nodesp, file="nodesp.csv", col.names=TRUE, quote=TRUE)
#nodesp <- as.data.frame(read.csv(file="nodesp.csv", header=TRUE))
#nodesp <- nodesp[,2:4]
edits <- read.csv(file="editlabels13.csv", header=TRUE)
nodes1 <-semi_join(edits, nodesp, by=c(site="one"))
nodes1 <- select(nodes1, site, lon, lat)
edgesp <- edgesp %>% count(one, two)
edgesp$one <- as.character(edgesp$one)
edgesp$two <- as.character(edgesp$two)

meta <- nodesp
lo <- meta
write.csv(lo, file="lo.csv", col.names=TRUE)
lo <- as.data.frame(read.csv(file="lo.csv", header=TRUE))
lo <- lo[,2:4]
lo$lon <- as.numeric(lo$lon)
lo$lat <- as.numeric(lo$lat)
lo$one <- NULL #as.character(lo$one)
#names(lo) <- c("lon", "lat")
#lo <- lo %>% filter(lon != 'NA', lat != 'NA')

#the lo and nodes is weird lat/lon, so load the edits csv where I fixed the spacing and have all of the labels, then join with the nodes

#it would be nice to code the site names by region. Let's give it a shot.
target1 <- c("Palanas", "Wangag", "Magbangon", "Elementary School", "Sitio Tugas", "Sitio Lonas", "Cabatoan", "Caridad Cemetery",  "Cardidad Proper", "Poroc Rose", "Poroc San Flower", "San Agustin", "Hicgop", "Hicgop South")
north <- edits %>% filter(site %in% target)
north$region <- "north"
target2 <- c("Visca", "Gabas")
mid <- edits %>% filter(site %in% target2)
mid$region <- ("mid")
target3 <- c("Sitio Baybayon", "Tamakin Dacot", "Haina", "Tamakin Dacat")
south <- edits%>% filter(site %in% target3)
south$region <- "south"
alllabs <- bind_rows(north, mid, south)
#make a factor to color by levels
alllabs$region <- as.factor(alllabs$region)

#add color
cols<-wes_palette(n=3,name="FantasticFox")

colors<-cols[alllabs$region]

library(rgdal)
map_new <- readOGR("leyte", "coastlines_leyte")
plot(map_new, xlim=c(124.79, 124.8 ), ylim=c(10.6,10.9))
text(alllabs$lon, alllabs$lat, labels=alllabs$site, cex=.7, offset=1.2, pos=2, font=2, col=colors)

a <- graph_from_data_frame(d=edgesp, vertices=nodesp, directed=TRUE)

katPlot(a, rescale=FALSE, asp=0, add=TRUE, edge.arrow.size=.6, vertex.size=0, vertex.frame.color=NA, 
	 vertex.label=NA, vertex.color= NA, vertex.label.dist=1, edge.width=edgesp$n,
		edge.color="slate grey", layout=lo, edge.curved=.8, vertex.label.font=0, vertex.label.color="black")
#edgesp is has 46 matches going north-north, would look better if I scaled it down. Divide by 10 below and then run above plot again
edgesp$n <- (edgesp$n)/2






#start adding sibling data
fullsib <- read.table(file= "20170831colony13.FullSibDyad.txt", header= TRUE)
halfsib <- read.table(file= "20170831colony13.HalfSibDyad.txt", header= TRUE)	

names(first) <- paste("sib1.", names(first), sep = "")
fullsib <- left_join(fullsib, first, by=c(sib1="sib1.ligation_id"))
sec <- labid
names(sec) <- paste("sib2.", names(sec), sep = "")
fullsib <- left_join(fullsib, sec, by=c(sib2="sib2.ligation_id"))

first <- fieldid
second <- fieldid
names(first) <- paste("sib1.", names(first), sep = "")
fullsib <- left_join(fullsib, first, by=c(sib1.sample_id="sib1.Sample_ID"))
names(second) <- paste("sib2.", names(second), sep = "")
fullsib <- left_join(fullsib, second, by=c(sib2.sample_id="sib2.Sample_ID"))

#now halfsibs
first <- labid
names(first) <- paste("hsib1.", names(first), sep = "")
halfsib <- left_join(halfsib, first, by=c(hsib1="hsib1.ligation_id"))
sec <- labid
names(sec) <- paste("hsib2.", names(sec), sep = "")
halfsib <- left_join(halfsib, sec, by=c(hsib2="hsib2.ligation_id"))


first <- fieldid
second <- fieldid
names(first) <- paste("hsib1.", names(first), sep = "")
halfsib <- left_join(halfsib, first, by=c(hsib1.sample_id="hsib1.Sample_ID"))
names(second) <- paste("hsib2.", names(second), sep = "")
halfsib <- left_join(halfsib, second, by=c(hsib2.sample_id="hsib2.Sample_ID"))

fullsib <- fullsib %>% select(sib1, sib2, sib1.obs_time, sib1.date, sib2.obs_time, sib2.date, sib1.Size, sib2.Size, Probability, sib1.name, sib2.name)#, GPS)
halfsib <- halfsib %>% select(hsib1, hsib2, hsib1.obs_time, hsib1.date, hsib2.obs_time, hsib2.date, hsib1.Size, hsib2.Size, Probability, hsib1.name, hsib2.name)#, GPS)


#MESSY BUT WORKS
#make a map with connectivity of INDIVIDUALS
#start by defining edges and vertices
#first get rid of low probability matches
edgesfs <- data.frame(select(fslatlon, sib1, sib2, sib1.lat, sib1.lon, sib2.lat, sib2.lon, sib1.name, sib2.name), stringsAsFactors = FALSE)
edgeshs <- data.frame(select(hslatlon, hsib1, hsib2, hsib1.lat, hsib1.lon, hsib2.lat, hsib2.lon, hsib1.name, hsib2.name), stringsAsFactors=FALSE)
names(edgesfs) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "one.name", "two.name")# "type")
edgesfs<- edgesfs %>% group_by(one, two) %>% filter(row_number() == 1) 
names(edgeshs) <- c("one", "two", "one.lat", "one.lon", "two.lat","two.lon", "one.name", "two.name")# "type")
edgeshs<- edgeshs %>% group_by(one, two) %>% filter(row_number() == 1) 

fslatlon <-filter(fslatlon, Probability > 0.98)
fslatlon <- fslatlon %>% group_by(sib1, sib2) %>% filter(row_number() == 1)
hslatlon <- filter(hslatlon, Probability > 0.98)
hslatlon <- hslatlon %>% group_by(hsib1, hsib2) %>% filter(row_number() == 1)
pairlatlon <- filter(pairlatlon, ProbMum1 > 0.98)
pairlatlon <- pairlatlon %>% group_by(OffspringID, InferredMum1) %>% filter(row_number() == 1)

	
edgesp <- data.frame(select(pairlatlon, OffspringID, InferredMum1, offs.lat, offs.lon, par.lat, par.lon), stringsAsFactors = FALSE)
edgesfs <- data.frame(select(fslatlon, sib1, sib2, sib1.lat, sib1.lon, sib2.lat, sib2.lon), stringsAsFactors = FALSE)
edgeshs <- data.frame(select(hslatlon, hsib1, hsib2, hsib1.lat, hsib1.lon, hsib2.lat, hsib2.lon), stringsAsFactors = FALSE)
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
write.csv(lo, file="lo.csv", col.names=TRUE)
lo <- as.data.frame(read.csv(file="lo.csv", header=TRUE))
lo <- lo[,2:3]
lo <- lo %>% filter(one.lon != 'NA', one.lat != 'NA')

#read in high res leyte map to map lat/long points on
library(rgdal)
map_new <- readOGR("leyte", "coastlines_leyte")

plot(map_new, xlim=c(124.7, 124.8 ), ylim=c(10.6,11))

#call katPlot first
a<- graph_from_data_frame(vertices = nodes, d= edges, directed = TRUE)
katPlot(a, rescale=FALSE, asp=0, add=TRUE, edge.arrow.size=.2, vertex.size=.4, vertex.frame.color=NA, 
	 vertex.label.cex=.6, vertex.label.degree=pi, vertex.label.dist=.08, edge.width=edges$number, 
		edge.color="slate grey", layout=lo, edge.curved=.1, vertex.label.font=2, vertex.label.color="gray40")
#TO DO: MAKE A MAP OF SIBLING AND HALFSIBLING RELATEDNESS ACCORING TO SITE, RUN MIGEST







#NOT FOR CONNECTIVITY PAPER

  #attach lat/lon for siblings
  #idcsv <- data.frame(read.csv("174_colonypars_sibs_adultsep13.csv", header = TRUE), stringsAsFactors = FALSE)	
fullsib$sib1.date <- as.character(fullsib$sib1.date)
fullsib$sib1.obs_time <- as.character(fullsib$sib1.obs_time)
fullsib$sib2.date <- as.character(fullsib$sib2.date)
fullsib$sib2.obs_time <- as.character(fullsib$sib2.obs_time)


  # combine date and time to form a dttm column ####
    # set time zone to PHT, Asia/Manila
fullsib$otime <- str_c(fullsib$sib1.date, fullsib$sib1.obs_time, sep = " ")
fullsib$ptime <- str_c(fullsib$sib2.date, fullsib$sib2.obs_time, sep = " ")

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
halfsib$hsib1.obs_time <- as.character(halfsib$hsib1.obs_time)
halfsib$hsib2.date <- as.character(halfsib$hsib2.date)
halfsib$hsib2.obs_time <- as.character(halfsib$hsib2.obs_time)


# combine date and time to form a dttm column ####
  # set time zone to PHT, Asia/Manila
halfsib$otime <- str_c(halfsib$hsib1.date, halfsib$hsib1.obs_time, sep = " ")
halfsib$ptime <- str_c(halfsib$hsib2.date, halfsib$hsib2.obs_time, sep = " ")

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

#move points for crunchy names

edits <- read.csv(file="editlabels13.csv", header=TRUE)

text(edits$lon, edits$lat, labels=edits$name, col="black", cex=.7, offset=.2, pos=2)

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
points(nodeshs$one.lon, nodeshs$one.lat, pch=10, col=alpha("dark gray", 0.3), cex=nodeshs$n/6)
points(nodeshs$one.lon, nodeshs$one.lat, pch=19, col=alpha("gray", 0.3), cex=nodeshs$n/6)

#move points for crunchy names

edits <- read.csv(file="editlabels13.csv", header=TRUE)

text(edits$lon, edits$lat, labels=edits$name, col="black", cex=.7, offset=.2, pos=2)

#add parents
katPlot(a, rescale=FALSE, asp=0, add=TRUE, edge.arrow.size=.4, vertex.size=.4, vertex.frame.color=NA, 
	 vertex.label=NA, vertex.color= "orange", vertex.label.degree=pi, vertex.label.dist=.1, edge.width=edgesp$n,
		edge.color="blue", layout=lo, edge.curved=-.7, vertex.label.font=2, vertex.label.color="black")



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
nodes1 <-inner_join(nodesfs, edits, by=c(one="name"))
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


