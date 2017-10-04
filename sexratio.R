#set working directory
setwd("~/Documents/GradSchool/parentage")

#load packages
library(dplyr)
library(lubridate)
#join Amphiprion
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

c1 <- leyte %>% tbl("diveinfo") %>% select(dive_table_id, date, site)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, obs_time)
c3 <- as.data.frame(left_join(c2, c1, by = "dive_table_id"))
c4 <- as.data.frame(tbl(leyte, sql("SELECT fish_table_id, anem_table_id, sample_id, color, size, fish_spp FROM clownfish")))
fieldid <- collect(left_join(c4, c3, by = "anem_table_id"))
allfish <- fieldid %>% filter(date != 'NA', fish_spp == 'APCL')
which(fieldid$date=='NA') #returns 0

allfish$date <- ymd(allfish$date)

allfish <- allfish %>% group_by(color)
breeders <- allfish %>% filter(color=='O' | color=='YP')
breeders$year <- ymd(breeders$date)
breeders <- breeders %>%
  mutate(year=year(date)) 

fems <- breeders %>% group_by(year) %>% tally(color == 'O')
names(fems) <- c("year","fem")
men <- breeders %>% group_by(year) %>% tally(color == 'YP')
names(men) <- c("year","men")
totals <- left_join(fems, men)

####SEX RATIO#######
totals2 <- totals %>%
	mutate(ratio=(fem/men))

twelve <- allfish %>% 
		filter(date >= '2012-03-01' & date <= '2012-07-31' )
		
thirt <- allfish %>% 
		filter(date >= '2013-03-01' & date <= '2013-07-31' )

four <- allfish %>% 
		filter(date >= '2014-03-01' & date <= '2014-07-31' )
		
fift <- allfish %>% 
		filter(date >= '2015-03-01' & date <= '2015-07-31' )
		
head(fift)

fem12 <- twelve %>% 
		filter(color=="O")
man12 <- twelve %>%
		filter(color=="YP")
fem13 <- thirt %>% 
		filter(color=="O")
man13 <- thirt %>%
		filter(color=="YP")
fem14 <- four %>% 
		filter(color=="O")
man14 <- four %>%
		filter(color=="YP")
fem15 <- fift %>% 
		filter(color=="O")
man15 <- fift %>%
		filter(color=="YP")
fishsex <- row_bind()






nfem12 <- tally(fem12),
nfem13 <- tally(fem13),
nfem14 <- tally(fem14),
nfem15 <- tally(fem15),
nman12 <- tally(man12),
nman13 <- tally(man13),
nman14 <- tally(man14),
nman15 <- tally(man15)