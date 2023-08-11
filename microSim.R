library(data.table)
setDTthreads(10)

library(simPop)
library(readr)

sample_file <- data.table(read_csv("./Data Files/microdata.csv"))

constraints <- data.table(read_csv("./Data Files/constraintsAus.csv"))

sample_file$hsize <- 1
sample_file$weights <- 50

sample_file$Gender <- as.factor(sample_file$Gender)
sample_file$age_range <- as.factor(sample_file$age_range)
sample_file$Region <- as.factor(sample_file$Region)
sample_file$Income <- as.factor(sample_file$Income)

colnames(constraints) <- c("Income", "Gender", "age_range", "Postal", "Freq", "Region")

constraints$Gender <- as.factor(constraints$Gender)
constraints$age_range <- as.factor(constraints$age_range)
constraints$Region <- as.factor(constraints$Region)
constraints$Income <- as.factor(constraints$Income)
constraints$Postal <- as.factor(constraints$Postal)

inp <- specifyInput(sample_file, hhid="Ind", hhsize="hsize", pid="Ind", strata="Region", weight="weights")

print(inp)

library(dplyr)

constraints.regional <- constraints %>% 
  group_by(Gender, age_range, Region, Income) %>%
  summarise(Freq = sum(Freq))

constraints.regional = data.frame(constraints.regional)

addWeights(inp) <- calibSample(inp, constraints.regional, nr_cpus = 10)


