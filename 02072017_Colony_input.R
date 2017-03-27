#02/07/2017
#select 60 random loci
setwd("/Users/kat1/Documents/GradSchool/parentage")
library(dplyr)
library(reshape)
loci<-read.table(file="02072017loci_names.txt")

#now randomly sample without replacement
sixty <- loci[sample(1:nrow(loci), 60, replace=TRUE),]
#not exactly what I wanted but whatever it worked kind of

#now need to make a marker file for each loci
#loci<-read.table(file="02072017_sixty_loci.txt")
#fliploci<-t(loci)
#markers<-as.matrix(fliploci)
markers <- matrix(nrow= 1, ncol= 176)
butt=matrix(nrow=3, ncol=176)
fullmarkers<-rbind(markers, butt)

#fill in with correct values based on Colony user manual
for (j in 1:176) {
  fullmarkers[1, j] = (1:176)
}

for (j in 1:176) {
  fullmarkers[2, j] = (0)
}

for (j in 1:176) {
  fullmarkers[3, j] = (0.000)
}

for (j in 1:176) {
  fullmarkers[4, j] = (0.0.012)
}
#make loci names compatible with Colony
fullmarkers2 <- fullmarkers
fullmarkers2[1,] <- gsub(pat1, "", fullmarkers2[1,])
fullmarkers2[1,] <- gsub(pat2, "", fullmarkers2[1,])
fullmarkers2[1,] <- gsub(pat3, "", fullmarkers2[1,])



write.csv(fullmarkers, file="colonymarkers.csv", row.names = FALSE, col.names = FALSE, quote= FALSE, sep = "")
markers <- read.csv(file="colonymarkers.csv", header=FALSE)
write.table(markers, file="colonymarkers.txt", col.names = FALSE, sep= " ")
colony <- read.csv(file="174HQ_norecap_convert.csv")
colony [,c("Population")] <- NULL
write.table(colony, file= '174HQ_norecap_convert.txt', sep = " ")