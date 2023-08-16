library(data.table)
setDTthreads(10)

library(simPop)

library(purrr)

library(MASS)

my_path <- c("./R/")             # set your path
source_files <- list.files(my_path, "\\.R$")  # locate all .R files
map(paste0(my_path, source_files), source)  # source all your R scripts!

library(readr)

library(glue)

library(dplyr)

incomeBin <- function(income) {
  if (income <= 20799) {
    val = 0
  } else if (income >= 20800 & income <= 41599) {
    val = 1
  } else if (income >= 41600 & income <= 64999) {
    val = 2
  } else if (income >= 65000 & income <= 90999) {
    val = 3
  } else if (income >= 91000 & income <= 181999) {
    val = 4
  } else {#} if (income >= 182000) {
    val = 5
  } 
  
  return(val)
}

region = 23

sample_fileFull <- data.table(read_csv("./Data Files/microdataMarried.csv"))

constraintsFull <- data.table(read_csv("./Data Files/constraintsAus.csv"))

colnames(constraintsFull) <- c("Income", "Gender", "age_range", "Postal", "Freq", "Region")

marginsFull <- data.table(read_csv("./Data Files/constraintsSimAnnealing.csv"))

colnames(marginsFull) <- c("Income", "Occ_code", "age_range", "Postal", "Freq", "Region")

synth <- function(region) {
  
  
glue("Starting {region}")

sample_file <- filter(sample_fileFull, Region == region)

constraints <- filter(constraintsFull, Region == region)



sample_file$hsize <- 1
sample_file$weights <- 50

sample_file$Region <- as.factor(sample_file$Region)

constraints$Region <- as.factor(constraints$Region)

sample_file$hhid = as.numeric(sample_file$Ind)
sample_file$pid = as.numeric(sample_file$Ind)

  inp <- simPop::specifyInput(sample_file, hhid="hhid", hhsize="hsize", pid="pid", strata="Region", weight="weights")
  
  print(inp)
  
  constraints.regional <- constraints %>% 
    group_by(Gender, age_range, Region, Income) %>%
    summarise(Freq = sum(Freq))
  
  constraints.regional = data.frame(constraints.regional)
  
  print("to weight")
  
  source("./R/calibSample.R")
  
  simPop::addWeights(inp) <- simPop::calibSample(inp = inp, totals=constraints.regional, nr_cpus = 8)#, maxit = 200)
  
  print("weighted")
  
  synthP <- simPop::simStructure(data = inp, method = "direct", basicHHvars = c("age_range", "Gender", "Region", "Partner_status"))
  
  print("sim struct")
  
  synthP <- simPop::simContinuous(synthP, additional = "Taxable_Income", equidist = FALSE, imputeMissings = FALSE)
  
  print("sim conti")
  
  scaledIncome <- mapply(incomeBin, synthP@pop@data$Taxable_Income)
  binned <- data.frame(scaledIncome)
  
  pop(synthP, var = "Income") = binned$scaledIncome
  
  print("before occ")
  
  synthP <- simPop::simCategorical(synthP, additional = c("Occ_code"), method = "xgboost")
  
  print("occ simmed")
  
  constraints.postal <- constraints %>%
    group_by(Region, Postal) %>%
    summarise(Freq = sum(Freq))
  
  constraints.postal = data.frame(constraints.postal)
  
  synthP <- simPop::simInitSpatial(synthP, additional = "Postal", region = "Region", tspatialP = constraints.postal)
  
  print("Postal simmed")
  
  #margins <- filter(marginsFull, Region == region)
  
  #margins <- margins %>% 
    #group_by(Income, age_range, Postal, Occ_code) %>%
    #summarise(Freq = sum(Freq))
  
  #margins <- data.frame(margins)
  
  #margins$Gender <- as.factor(margins$Gender)
  #margins$Occ_code <- as.factor(margins$Occ_code)
  #margins$Postal <- as.factor(margins$Postal)
  #margins$Income <- as.factor(margins$Income)
  
  #margins <- subset(margins, Postal %in% unique(synthP@pop@data$Postal))
  
  #synthP@table <- margins
  
  #synthP <- simPop::addKnownMargins(synthP, margins)
  
  #synthP@table <- margins
  
  #synthPAdj <- simPop::calibPop(synthP, split = "Postal", maxiter = 200, verbose = TRUE, nr_cpus = 8)
  
  print("Pop calibrated")
  
  pop = data.frame(synthP@pop@data)
  
  print("state done")
  
  write_csv(pop, "./Output Files/synthFailPostalPopulation.csv", append = T, col_names = T)
  
  glue("Finished {region}")
  
  #write.table(pop, "./Data Files/syntheticTempPopulation.csv", sep = ",", col.names = !file.exists("./Data Files/syntheticPopulation.csv"), append = T)
}

#problem 9, 19, 21, 23, 32

#isTRUE(all.equal(sort(unique(margins[["Postal"]])),sort(unique(synthP@pop@data[["Postal"]]))))
