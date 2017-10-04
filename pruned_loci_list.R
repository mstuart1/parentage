#Pull loci from a plink LD pruned loci .map file. Generate a text file that can then be used in VCF tools position filtering.
setwd("~/Downloads")
library(dplyr)
dat <- as.data.frame(read.table(file="seq17_03_SNPs.LDpruned.map", header=FALSE))
dat <- rename(dat, chrom=V1, locus=V2, position=V3, dist=V4)

loci <- dat %>% select(locus)
write.table(loci, file="pruned_loci_list.txt", sep=" ")