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

