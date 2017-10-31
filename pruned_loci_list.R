#Pull loci from a plink LD pruned loci .map file. Generate a text file that can then be used in VCF tools position filtering.
setwd("~/Downloads")
library(dplyr)
dat <- as.data.frame(read.table(file="seq17_03_SNPs.LDpruned.map", header=FALSE))
dat <- rename(dat, chrom=V1, locus=V2, position=V3, dist=V4)

loci <- dat %>%
select(locus) %>%
mutate(locus = stringr::str_replace_all(locus, ":", " ")) # remove the colons, replace with spaces

# eliminate header and quotes when writing to file
# MRS "I'm not sure if these copied and pasted " " are supposed to have a space between them or not"
write.table(loci, file="data/pruned_loci_list.txt", sep=" ", quote = F, col.names = F, row.names = F) 
