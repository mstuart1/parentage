#01/12/2016
#analyzing the raw parent-offspring matches 
setwd("/Users/kat1/Documents/Grad School/parentage")
library(dplyr)
idcsv <- read.csv(file="id_distcalc.csv", stringsAsFactors = F)
#eliminate NA
idcsvdist <- idcsv[!is.na(idcsv$Loci.typed.1),]


#so now filter by confidence in TRIO so you can get rid of unreasonable matches
idcsvconf <- idcsvdist[(idcsvdist$Trio.confidence=='+' | idcsvdist$Trio.confidence=='*'), ]
goodmatchestrio <- filter(idcsvconf, distfs<="0.250")
write.csv(goodmatchestrio, file = "goodmatchestrio.csv", sep = "", row.names = F)
summary(goodmatchestrio)
breaks = seq(0, 25, by=0.5)
dispdist<- table(cut(goodmatchestrio$distfs, breaks))
plot(dispdist)

write.csv(idcsvconf, file = "id_distcalcconf.csv", sep = "", row.names = F)

