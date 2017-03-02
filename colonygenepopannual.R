#02/15/2017 Want to make an individual gene pop file to run annual 
#analysis in colony
setwd("~/Documents/GradSchool/parentage")
library(dplyr)
#trying to use Colony to infer parentage as well as sibship. So want to 
#run a file with ALL genotyped individuals in a singal year, so here
#code combines adult and juvenile ligation ids and joins this to the 
#genotype text file for the 60 random loci
#just juveniles
juv12<-read.table(file='juv12.txt', header = TRUE)

genes<-readGenepop(file='2017-02-21_noregeno.gen')

#pool adults and juveniles
ad12<-read.table(file='adults12.txt', header=TRUE)
allseq<-full_join(juv12, ad12)

genes12<-semi_join(genes, allseq, by=c("names"="lig"))
final<-genes12[-c(1,1)]

write.table(final, file="allseq12.txt", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")

convert<-read.csv(file="allseq12convert.csv")
write.table(convert, file="allseq12convert.txt", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")


