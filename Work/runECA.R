library(eca)

# workarounds
inpath <- "/Users/a5362/code/github/Rstox_utils/Work/output"
tmppath <- "/Users/a5362/code/github/Rstox_utils/Work/tmp"
dir <- "/Users/a5362/code/github/Rstox_utils/Work"

setwd(tmppath)

## Add extra information in GlobalParameters
#sett i prepECA
set_in_prep <- function(GlobalParameters, maxl){
  GlobalParameters$resultdir <- "ECAres" #Note, must be relative path
  GlobalParameters$maxlength <- maxl
  GlobalParameters$minage <- 1
  GlobalParameters$maxage <- 20
  GlobalParameters$delta.age <- 0.001
  GlobalParameters$age.error <- FALSE
  return(GlobalParameters)  
}

colsel <- c(1,2,3)
fix_in_prep_agelength<-function(AgeLength){
  ## Select covariates - not use haulweight and boat now
  newAgeLength <- AgeLength
  newAgeLength$CovariateMatrix <- AgeLength$CovariateMatrix[,colsel]
  newAgeLength$info <- AgeLength$info[colsel,]
  return(newAgeLength)  
}
fix_in_prep_weightlength <- function(WeightLength){
  newWeightLength <- WeightLength
  newWeightLength$CovariateMatrix <- WeightLength$CovariateMatrix[,colsel]
  newWeightLength$info <- WeightLength$info[colsel,]
  return(newWeightLength)
}


#/workarounds

warning("remember to clean run parameters from prep_ECA, write doc for runECA")

burnindefault=0
samplesdefault=201
thindefault=1
defaultfitfile="fit"
defaultpredfile="pred"
defaultlgamodel="log-linear"
defaultCC=FALSE
defaultCCError=FALSE
runECA <- function(datafilepath, burnin=burnindefault, nSamples=samplesdefault, thin=thindefault, fitfile=defaultfitfile, predfile=defaultpredfile, lgamodel=defaultlgamodel, CC=defaultCC, CCError=defaultCCError, seed=NULL){
  # Sett kjøreparametere her, sett dataparametere i prep_eca
  tmp <- load(datafilepath)
  write(paste("Loaded from", filename, ":", tmp), stderr())
  
  GlobalParameters <- set_in_prep(GlobalParameters, max(AgeLength$DataMatrix$lengthCM,na.rm=T))
  
  GlobalParameters$caa.burnin <- burnin
  GlobalParameters$burnin <- burnin
  GlobalParameters$nSamples <- nSamples
  GlobalParameters$thin <- thin
  GlobalParameters$fitfile <- fitfile
  GlobalParameters$predictfile <- predfile
  GlobalParameters$lgamodel <- lgamodel
  
  GlobalParameters$CC <- CC
  GlobalParameters$CCerror <- defaultCCError
  
  if (is.null(seed)){
    seed=""
  }
  GlobalParameters$seed <- seed
  
  AgeLength <- fix_in_prep_agelength(AgeLength)
  WeightLength <- fix_in_prep_weightlength(WeightLength)

  ## Estimate model
  fit <- eca.estimate(AgeLength,WeightLength,Landings,GlobalParameters)
  
  ## Predict
  pred <- eca.predict(AgeLength,WeightLength,Landings,GlobalParameters)
  
  l<-list()
  l$fit<-fit
  l$pred<-pred
  return(l)
}

filename <- "ECA_torsk_2015.RData"
filepath <- file.path(inpath, filename)
result <- runECA(filepath)

source(file.path(dir, "plot.R"))
plot_pred_box(result$pred)
