#code to take 20170831 Colony runs and calculate all distances between parents and offspring. Then generate a frequency distribution of all distance possible to travel between anemones. Malin says make a N -> S gradient because we have such a nice linear coastline
setwd("~/Documents/GradSchool/parentage")
source("~/Documents/GradSchool/parentage/samplefromlig.R")
source("~/Documents/GradSchool/parentage/conleyte.R")
source("~/Documents/GradSchool/parentage/conlabor.R")
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(geosphere)
library(plotrix)
library(fields)
library(tidyr)
library(wesanderson)
#calculate the distance between all parent-offspring matches 

leyte <- conleyte()
labor <- conlabor()


#load tables with parent offspring matches and attach lat lon
#prepare full likelihood parentage/sibship results for mapping
pairs <- read.table(file= "20170927colony15.Maternityc.txt", header= TRUE)

# add lab IDs

c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
c3 <- left_join(c2, c1, by = "extraction_id")
c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
labid <- collect(left_join(c4, c3, by = "digest_id"))
head(labid)
first <- labid
names(first) <- paste("offs.", names(first), sep = "")
pairs <- left_join(pairs, first, by=c(OffspringID="offs.ligation_id"))
sec <- labid
names(sec) <- paste("par.", names(sec), sep = "")
pairs <- left_join(pairs, sec, by=c(InferredMum1="par.ligation_id"))
head(pairs)
c1 <- leyte %>% tbl("diveinfo") %>% select(dive_table_id, date, site, gps, dive_type)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, obs_time)
c3 <- as.data.frame(left_join(c2, c1, by ="dive_table_id"))
c4 <- as.data.frame(tbl(leyte, sql("SELECT fish_table_id, anem_table_id, sample_id, size FROM clownfish where sample_id is not NULL")))
fieldid <- as.data.frame((left_join(c4, c3, by = "anem_table_id")))
dim(fieldid)
head(fieldid)
#don't need this line in 2015, only clownfish dives that year
stuff <- fieldid %>% group_by(dive_type) %>% filter(dive_type=="C")
#now add parent info
ungroup(stuff)
head(stuff)
first <- stuff
names(first) <- paste("offs.", names(first), sep = "")
head(first)
pairs <- left_join(pairs, first, by="offs.sample_id")
head(pairs)
second <- stuff
head(second)
names(second) <- paste("par.", names(second), sep = "")
pairs <- left_join(pairs, second, by="par.sample_id")
#2015 L1550 looses it's site name in this process... can't figure out why. If you go back through the script you can see L1550 came from Carirdad Proper
head(pairs)
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
  mutate(oyear = year(otime)) %>%
  mutate(omonth = month(otime)) %>%
  mutate(oday = day(otime)) %>%
  mutate(ohour = hour(otime)) %>%
  mutate(omin = minute(otime)) %>%
  mutate(osec = second(otime)) %>%
  mutate(pyear = year(ptime)) %>%
  mutate(pmonth = month(ptime)) %>%
  mutate(pday = day(ptime)) %>%
  mutate(phour = hour(ptime)) %>%
  mutate(pmin = minute(ptime)) %>%
  mutate(psec = second(ptime))

#trim the data to keep only time and parent offspring matches, and then elminate repeats
pairs <- distinct(pairs) #seems to be no need for this line but keep it in here as a reminder if I run into other problems
head(pairs)
#20170905 checked through here... thinking problem is in the gps unit number matching
latlong <- data.frame(leyte %>% tbl("GPX") %>% collect(n = Inf))
latlong$time <- ymd_hms(latlong$time)

latlongo <- latlong %>%
  mutate(year = year(time)) %>%
  mutate(month = month(time)) %>%
  mutate(day = day(time)) %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time))
 
names(latlongo) <- paste("offs.", names(latlongo), sep = "")

# find matches for times to assign lat long - there are more than one set of seconds (sec.y) that match
pairlatlon <- left_join(pairs, latlongo, by = c(oyear= "offs.year", omonth="offs.month", oday="offs.day", ohour="offs.hour", omin="offs.min", offs.gps="offs.unit"))
latlongp <- latlong %>%
  mutate(year = year(time)) %>%
  mutate(month = month(time)) %>%
  mutate(day = day(time)) %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time))
  

names(latlongp) <- paste("par.", names(latlongp), sep = "")
pairlatlon <- left_join(pairlatlon, latlongp, by = c(pyear="par.year", pmonth="par.month", pday="par.day", phour="par.hour", pmin="par.min",  par.gps="par.unit"))
head(pairlatlon)
#now trim to just necessary columns
#### HERE CAN I USE GROUP BY ID OR OBSTIME AND THEN SUMMARISE TO GET THE MEAN LAT LON OR MIN LAT LON AND CREATE A NEW TABLE WITH ALL COLUMNS BUT ONLY ONE LAT LON PER OBS
# because all of the lat longs for the 4 observations are basically the same, remove sec.y and find distinct values
pairlatlon <- pairlatlon %>%
  #select(-sec.y) %>%  # remove sec.y column
  #%>% # keep all unique observance events (need that id col in the excel sheet) %>%
 select(OffspringID, InferredMum1, offs.lat, offs.lon, par.lat, par.lon, offs.site, par.site, ProbMum1) %>% filter(ProbMum1 >= .98)
  #rename(sec = sec.x)
pairlatlon<- pairlatlon %>% group_by(OffspringID, InferredMum1) %>% filter(row_number() == 1) 
head(pairlatlon)
pairlatlon$offs.lat <- as.numeric(pairlatlon$offs.lat)
pairlatlon$offs.lon <- as.numeric(pairlatlon$offs.lon)
pairlatlon$par.lat <- as.numeric(pairlatlon$par.lat)
pairlatlon$par.lon <- as.numeric(pairlatlon$par.lon)

alldists <- rdist.earth(as.matrix(pairlatlon[,c('offs.lon', 'offs.lat')]), as.matrix(pairlatlon[,c('par.lon', 'par.lat')]), miles=FALSE, R=6371)
pairlatlon$distkm <- diag(alldists)
head(pairlatlon)
#write distance files
write.csv(pairlatlon, file="pairdist2015.csv")
#calculate the distance between all anemones
#make a matrix with all anemones and lat lon

#call fieldid matrix
c1 <- leyte %>% tbl("diveinfo") %>% select(dive_table_id, date, site, gps)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, obs_time)
anems <- as.data.frame(left_join(c2, c1, by ="dive_table_id"))

anems$time <- str_c(anems$date, anems$obs_time, sep = " ")
anems$time <- ymd_hms(anems$time)
anems$time <- force_tz(anems$time, tzone = "Asia/Manila")

# change time zone to UTC ####
anems$time <- with_tz(anems$time, tzone = "UTC")

latlong <- data.frame(leyte %>% tbl("GPX") %>% collect(n = Inf))
latlong$time <- ymd_hms(latlong$time)

latlong <- latlong %>%
  mutate(year = year(time)) %>%
  mutate(month = month(time)) %>%
  mutate(day = day(time)) %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time))
  
anems <- anems %>%
  mutate(year = year(time)) %>%
  mutate(month = month(time)) %>%
  mutate(day = day(time)) %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time))

anems1 <- left_join(anems, latlong, by=c("year", "month", "day", "hour", "min", "sec", gps="unit"))
anems2 <- anems1 %>% filter(lat != 'NA' & lon != 'NA')
alldists <- rdist.earth(as.matrix(anems2[,c('lat', 'lon')]), miles=FALSE, R=6371)
anems2$distkm <- diag(alldists)

#write out table
write.csv(anems2, file="anemdist.csv")

########PLOT#########

#drop unecessary columns
anems1 <- anems1 %>% select(anem_table_id, site, lat, lon)

# load the year to work on

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
#find means 
all$year <- as.character(all$year)
means <- as.data.frame(aggregate(distkm ~ year, all, mean))
#find average dispersal mean
allmean <- mean(all$distkm)
#make a matrix of average mean, add it to the annual averages
addmean <- as.data.frame(allmean)
addmean$year <- "all"
colnames(addmean)<- c("distkm", "year")
allmean2 <- bind_rows(means, addmean)
head(allmean2)
#plot average dispersal distances
ggplot(allmean2, aes(x=distkm, fill=year)) +
    geom_histogram(binwidth=1, alpha=.5, position="dodge")
	
#plot the distances between sites
#anemsdist <- anems2 %>% group_by(site)

dist <- read.csv("sitedist.csv", header=TRUE)
dist$year <- 'site-site distance'
dist$OffspringId <- seq(from=1, to=137, by=1)
dist$InferredMum1 <- NULL
dist$offs.lat <- NULL
dist$offs.lon <- NULL
dist$par.lat <- NULL
dist$par.lon <- NULL
dist$offs.site <- NULL
dist$par.site <- NULL
dist$ProbMum1 <- NULL
head(dist)
#bin counts of possible distances in the population
dist1 <-as.data.frame(round(dist$distkm, digits=0))
names(dist1) <- "distkm"
dist1$distkm <- as.character(dist1$distkm)
dist2 <- dist1 %>% group_by(distkm) %>% count()
dist2
#bin observed dispersal distances
all2 <- all
all2 <-as.data.frame(round(all2$distkm, digits=0))
names(all2) <- "distkm"
all2$distkm <- as.character(all2$distkm)
all2 <- all2 %>% group_by(distkm) %>% count()
colors <- c("red", "blue", "orange", "green")

dist$rounded <-round(dist$distkm, digits=0)
all$rounded <-round(all$distkm, digits=2)

#need to get all distances on the same scale as observed dispersal.... need to get the yaxes the same...
fill <- "snow3"
colour <- "snow3"
ggplot()+ 
	geom_density(data=dist, aes(x=distkm), fill="snow3")+
			   theme_bw() + theme(panel.grid.major = element_blank(),
			   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text=element_text(family="Helvetica", size= 20), 
			   legend.position=c(.9, .8), legend.title=element_blank())+ theme(legend.key = element_blank())+
			   theme(panel.border = element_blank(),
			   axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
			   axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
			   xlab("Distance (km)") + ylab("Probability Density") + guides(fill=FALSE)+ scale_fill_brewer(palette="Set1")+
			   scale_x_continuous(limits = c(0,28), expand = c(0, 0)) +
		   	geom_vline(data=allmean2, aes(xintercept=distkm, colour=year),
		              linetype="dashed", size=1.5) + scale_colour_brewer(palette="Set1")
#yearly dispersal, add to above plot if you want
geom_histogram(data=all, aes(x=rounded, fill=year, colour=year), alpha=.35, position="stack") + 

#plot mean dispersal density and annual dispersal mean lines
ggplot()+ 
	geom_density(data=all, aes(x=rounded), fill="snow3")+
			   theme_bw() + theme(panel.grid.major = element_blank(),
			   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text=element_text(family="Helvetica", size= 20), 
			   legend.position=c(.9, .8), legend.title=element_blank())+ theme(legend.key = element_blank())+
			   theme(panel.border = element_blank(),
			   axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
			   axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
			   xlab("Distance (km)") + ylab("Probability Density") + guides(fill=FALSE)+scale_color_manual(values=wes_palette(n=4, name="Zissou"))+
			   scale_x_continuous(limits = c(0,28), expand = c(0, 0)) +
		   	geom_vline(data=allmean2, aes(xintercept=distkm, colour=year),
		              linetype="dashed", size=1.5) + scale_color_manual(values=wes_palette(n=5, name="Zissou"))
#plot annual density dispersal
#subset data so you can sequentially add years to a graph and get an idea of variability
#year needs to be a factor to get same color in line and fill
one <- bind_rows(thirteen, twelve)
one$year <- as.factor(one$year)
two <- bind_rows(thirteen, fourteen, twelve)
two$year <- as.factor(two$year)
three <- bind_rows(thirteen, fourteen, fifteen, twelve)
three$year <- as.factor(three$year)
#set colors
onec <- c("red")
twoc <- c("red", "blue")
threec<- c("red", "blue", "green")
ggplot()+ 
	geom_density(data=three, aes(x=distkm, color= year, postion="stack"), alpha=.5, size=2 )+
			   theme_bw() + theme(panel.grid.major = element_blank(),
			   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text=element_text(family="Helvetica", size= 20), 
			   legend.position=c(.9, .8), legend.title=element_blank()) + theme(legend.key = element_blank())+ 
			   theme(panel.border = element_blank(),
			   axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
			   axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
			   xlab("Distance (km)") + ylab("Probability Density") + guides(fill=FALSE)+scale_color_manual(values=wes_palette(n=4, name="Zissou"))+
			   scale_x_continuous(limits = c(0,28), expand = c(0, 0)) +
			   scale_y_continuous(limits = c(0,.5), expand = c(0, 0)) 
			   

#above plot is good
#this plot below sucks
ggplot()+ 
	geom_density(data=dist, aes(x=rounded), fill="snow3", alpha=.7)+
	geom_density(data=all, aes(x=rounded), fill="skyblue3", alpha=0.3) + 
    theme_bw() + theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text=element_text(family="Times New Roman", size= 20), 
    legend.position=c(.9, .85), legend.title=element_blank())+
    xlab("Distance (km)") + ylab("Frequency")+
	geom_vline(data=means, aes(xintercept=distkm, colour=year),
           linetype="dashed", size=1.5) +
		   scale_x_continuous(breaks = seq(0, 28, 2), limits=c(0, 28))+
		   scale_y_continuous(breaks = seq(0, 1, .2), limits=c(0, 1))
		   
	       
		   scale_colour_continuous(guide=FALSE)

		   			 
	       coord_cartesian(ylim=c(0, 20))+ coord_cartesian(xlim=c(0, 30))

dim(all)
nobs(mod)
#both the same, good

#anova to determine significance

mod <- lm(distkm ~ year, data = all)
anova(mod)
#significant

#now post hoc
TukeyHSD(x=mod)