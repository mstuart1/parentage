#use the noregeno genpop on the pinsky google drive to write a text file of ids to folter for in vcftools
setwd("~/Documents/GradSchool/parentage")
source("~/Documents/GradSchool/parentage/readGenepop_space.R")
source("~/Documents/GradSchool/parentage/samplefromlig.R")
library(dplyr)
genfile <- "~/Documents/GradSchool/parentage/2016-12-20_noregeno.gen"
gen <- readGenepop(genfile)
new <- sampfromlig(gen)
new <- select(new, sample_id, ligation_id)
head(new)
write.table(new, file="noregenoid.txt")
