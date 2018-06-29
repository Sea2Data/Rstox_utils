library(eca)
inpath <- "/Users/a5362/code/github/Rstox_utils/Work/output"
tmppath <- "/Users/a5362/code/github/Rstox_utils/Work/tmp"
dir <- "/Users/a5362/code/github/Rstox_utils/Work"
filename <- "ECA_torsk_2015.RData"
tmp <- load(file.path(inpath, filename))
print(paste("Loaded from", filename, ":", tmp))

setwd(tmppath)

## Add extra information in GlobalParameters
GlobalParameters$maxlength <- max(AgeLength$DataMatrix$lengthCM,na.rm=T)
GlobalParameters$caa.burnin <- 0
GlobalParameters$resultdir <- "ECAres" #Note, must be relative path
GlobalParameters$fitfile <- "ff"
GlobalParameters$predictfile <- "pp"
GlobalParameters$minage <- 1
GlobalParameters$maxage <- 20
GlobalParameters$delta.age <- 0.001
GlobalParameters$age.error <- FALSE
GlobalParameters$lgamodel <- "log-linear"
GlobalParameters$CC <- FALSE
GlobalParameters$CCerror <- FALSE
GlobalParameters$seed <- 1234


## Select covariates - not use haulweight and boat now
col <- c(1,2,3)
newAgeLength <- AgeLength
newAgeLength$CovariateMatrix <- AgeLength$CovariateMatrix[,col]
newAgeLength$info <- AgeLength$info[col,]

newWeightLength <- WeightLength
newWeightLength$CovariateMatrix <- WeightLength$CovariateMatrix[,col]
newWeightLength$info <- WeightLength$info[col,]

## Estimate model
fit <- eca.estimate(newAgeLength,newWeightLength,Landings,GlobalParameters)

## Predict
pred <- eca.predict(newAgeLength,newWeightLength,Landings,GlobalParameters)

source(file.path(dir, "plot.R"))
plot_pred_box(pred)

season_plot_test <- function(){
  par(mfrow=c(2,2))
  for (s in unique(Landings$AgeLengthCov$season)){
    print(paste0("Q", s))
    valuename <- AgeLength$resources$covariateLink$season$Covariate[AgeLength$resources$covariateLink$season$Numeric==s]
    sl <- Landings
    keep_alc <- sl$AgeLengthCov$season==s
    sl$AgeLengthCov <- sl$AgeLengthCov[keep_alc,]
    sl$WeightLengthCov <- sl$WeightLengthCov[keep_alc,]
    sl$LiveWeightKG <- sl$LiveWeightKG[keep_alc]
    print(sl)
    GlobalParameters$predictfile <- paste("predict",s,sep="_")
    predsl <- eca.predict(newAgeLength,newWeightLength,sl,GlobalParameters)  
    plot_pred_box(predsl, valuename)
  }
  par(mfrow=c(1,1))
}
