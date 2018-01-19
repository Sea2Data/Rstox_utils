library(eca)
inpath <- "/Users/a5362/code/github/Rstox_utils/Work/output"
tmppath <- "/Users/a5362/code/github/Rstox_utils/Work/tmp/ECAres"
filename <- "ECA_sild_2015.RData"
tmp <- load(file.path(inpath, filename))
print(paste("Loaded from", filename, ":", tmp))

## Add extra information in GlobalParameters
GlobalParameters$maxlength <- max(AgeLength$DataMatrix$lengthCM,na.rm=T)
GlobalParameters$caa.burnin <- 0
GlobalParameters$resultdir <- tmppath
GlobalParameters$fitfile <- "fit"
GlobalParameters$predictfile <- "predict"
GlobalParameters$minage <- 1
GlobalParameters$maxage <- 20
GlobalParameters$delta.age <- 0.001
GlobalParameters$age.error <- FALSE
GlobalParameters$lgamodel <- "log-linear"
GlobalParameters$CC <- FALSE
GlobalParameters$CCerror <- FALSE
GlobalParameters$seed <- 1234


## Select covariates - not use haulweight and boat now
col <- 1:4
newAgeLength <- AgeLength
newAgeLength$CovariateMatrix <- AgeLength$CovariateMatrix[,col]
newAgeLength$info <- AgeLength$info[col,]

newWeightLength <- WeightLength
newWeightLength$CovariateMatrix <- WeightLength$CovariateMatrix[,col]
newWeightLength$info <- WeightLength$info[col,]


## Estimate model
fit <- eca.estimate(newAgeLength,newWeightLength,Landings,GlobalParameters)

## Predict
## Install new library
pred <- eca.predict(newAgeLength,newWeightLength,Landings,GlobalParameters)