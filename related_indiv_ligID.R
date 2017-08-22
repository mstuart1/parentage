#script to pull list of putatively related individuals from COLONY results, and compile them into a list of individuals to filter for and run through the Amy Ko pedigree program
#08/22/2017
#set working directory, load libraries
setwd("~/Documents/GradSchool/pedigree_colony_trimming")
library(dplyr)

#load all years' match files
mat12 <- read.table(file="174HQ_parentage_adults_sep12.Maternity.txt", header=TRUE)
fs12 <- read.table(file="174HQ_parentage_adults_sep12.FullSibDyad.txt", header=TRUE)
hs12 <- read.table(file="174HQ_parentage_adults_sep12.HalfSibDyad.txt", header=TRUE)
mat13 <- read.table(file="174HQloci_adult_sep_2013WP1.Maternity.txt", header=TRUE)
fs13 <- read.table(file="174HQloci_adult_sep_2013WP1.FullSibDyad.txt", header=TRUE)
hs13 <- read.table(file="174HQloci_adult_sep_2013WP1.HalfSibDyad.txt", header=TRUE)
mat14 <- read.table(file="174HQ_parentage_2014.Maternity.txt", header=TRUE)
fs14 <- read.table(file="174HQ_parentage_2014.FullsibDyad.txt", header=TRUE)
hs14 <- read.table(file="174HQ_parentage_2014.HalfSibDyad.txt", header=TRUE)
mat15 <- read.table(file="175HQloci_adults_sep_2015.Maternity.txt", header=TRUE)
fs15 <- read.table(file="175HQloci_adults_sep_2015.FullSibDyad.txt", header=TRUE)
hs15 <- read.table(file="175HQloci_adults_sep_2015.HalfSibDyad.txt", header=TRUE)

#bind together all parent matches, all full sibs, then all half sibs and then join everything together
mat <- bind_rows(mat12, mat13, mat14, mat15)
hs <- bind_rows(hs12, hs13, hs14, hs15)
fs <- bind_rows(fs12, fs13, fs14, fs15)

#rename them so they are same column name scheme

mat <- rename(mat, one=OffspringID, two=InferredMum1, prob=ProbMum1)
fs <- rename(fs, one=sib1, two=sib2, prob=Probability)
hs <- rename(hs, one=hsib1, two=hsib2, prob=Probability)

#bind all together
all <- bind_rows(mat, fs, hs)

#filter for only high probability matches (.98 to be consistent with process_colony_dyad.R script) 
all1 <- all %>% filter(prob > 0.98) 
