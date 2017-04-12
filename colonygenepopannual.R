#02/15/2017 Want to make an individual gene pop file to run annual 
#analysis in colony
setwd("~/Documents/GradSchool/parentage")
source("~/Documents/GradSchool/parentage/readGenepop_space.R")
library(dplyr)
#trying to use Colony to infer parentage as well as sibship. So want to 
#run a file with ALL genotyped individuals in a singal year, so here
#code combines adult and juvenile ligation ids and joins this to the 
#genotype text file for the 60 random loci

adults <-read.table(file='adults13.txt', header = TRUE)
#if starting from unconverted genepop
genes<-readGenepop(file='174HQloci_norecap.gen')
#ifstarting from convert file
#genes <- read.csv(file='2017-03-20 56loci_HQ_gen.csv')

genes13<-semi_join(genes, adults, by=c("names"="lig")) #names if starting from genepop
#test
dim(genes13)
which(is.na(genes13))
final<-genes13[-c(1,1)]
write.table(final, file="174HQloci2013_norecap_ADULTS.gen", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")


juvs <-read.table(file='juv13.txt', header = TRUE)
genes12<-semi_join(genes, juvs, by=c("names"="lig")) #names if starting from genepop
#pool adults and juveniles
ad12<-read.table(file='adults13.txt', header=TRUE)
allseq<-full_join(juvs, ad12)
genes13<-semi_join(genes, allseq, by=c("names"="lig")) #names if starting from genepop
#test
dim(genes12)
which(is.na(genes12))
genes13 <-genes13[-c(1,1)]	


write.table(genes13, file="174HQloci2013_norecap_ALL.gen", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")

#load convert csv from Cervus and write as text file
convert <- read.csv(file="174HQloci2013_norecap_ALL.csv")
converted <-convert[-c(1,1)]
write.table(converted, file="174HQloci2013_norecap_ALL.txt", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")

#make excluded paternity file
adults <-final[c(1,1)]
write.table(adults, file="174HQexcluded_pat13.txt", row.names = FALSE, col.names = TRUE, quote= FALSE, sep = " ")


