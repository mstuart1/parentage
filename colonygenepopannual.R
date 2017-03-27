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
#if starting from unconverted genepop
genes<-readGenepop(file='203HQloci_norecap.gen')
#ifstarting from convert file
genes <- read.csv(file='2017-03-20 56loci_HQ_gen.csv')
#pool adults and juveniles
ad12<-read.table(file='adults12.txt', header=TRUE)
allseq<-full_join(juv12, ad12)

genes12<-semi_join(genes, allseq, by=c("names"="lig")) #names if starting from genepop
final<-genes12[-c(1,1)]
final [,c("names.1")] <- NULL
write.table(final, file="203HQloci2012_norecap.gen", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")


#convert<-read.csv(file="allseq12convert.csv")
#write.table(convert, file="allseq12convert.txt", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")


