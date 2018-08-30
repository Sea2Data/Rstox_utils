library(eca)
library(Rstox)

#
# workarounds
#

#ecadir <- "/Users/a5362/code/github/Rstox_utils/Work/tmp/ECAres"
#inpath <- file.path(ecadir, "datafiles")
srcdir <- "/Users/a5362/code/github/Rstox_utils/Work"
source(file.path(srcdir, "plot_results_ECA.R"))

fix_in_prep_agelength<-function(AgeLength){
  ## Select covariates - not use haulweight and boat now
  newAgeLength <- AgeLength

  rownames(newAgeLength$info)[rownames(newAgeLength$info)=="temporal"]<-"season"
  names(newAgeLength$CovariateMatrix)[names(newAgeLength$CovariateMatrix)=="temporal"]<-"season"
  
  return(newAgeLength)  
}
fix_in_prep_weightlength <- function(WeightLength){
  newWeightLength <- WeightLength

  rownames(newWeightLength$info)[rownames(newWeightLength$info)=="temporal"]<-"season"
  names(newWeightLength$CovariateMatrix)[names(newWeightLength$CovariateMatrix)=="temporal"]<-"season"
  
  
  return(newWeightLength)
}
fix_in_prep_landings <- function(Landings){
  newLandings <- Landings

  names(newLandings$WeightLengthCov)[names(newLandings$WeightLengthCov)=="temporal"]<-"season"
  names(newLandings$AgeLengthCov)[names(newLandings$AgeLengthCov)=="temporal"]<-"season"
  
  return(newLandings)
}

#
# /workarounds
#

get_default_data_dir <- function(projectname, recadir=getProjectPaths(projectname)$RDataDir){
  return(file.path(recadir, "reca", "datafiles"))
}

burnindefault=10
samplesdefault=101
thindefault=1
defaultfitfile="fit"
defaultpredfile="pred"
defaultlgamodel="log-linear"
defaultCC=FALSE
defaultCCError=FALSE
age.error.default=FALSE
#' @param inputdir Defaults (if null) to ...
runECA <- function(projectname, inputdir=NULL, burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault, thin=thindefault, fitfile=defaultfitfile, predfile=defaultpredfile, lgamodel=defaultlgamodel, CC=defaultCC, CCError=defaultCCError, seed=NULL, age.error=age.error.default){
  warning("write doc for runECA")
  # Sett kjøreparametere her, sett dataparametere i prep_eca

  if (is.null(inputdir)){
    warning("temporally using non-default ecadir. source from prep_ECA")
    inputdir <- get_default_data_dir(projectname, ecadir)
  }
  if (!file.exists(inputdir)){
    stop(paste("Directory", inputdir, "does not exist."))
  }
  
  filename <- file.path(inputdir, paste0(projectname, ".RData"))
  tmp <- load(filename)
  write(paste("Data loaded from", filename, ":", tmp), stderr())
  print(GlobalParameters$resultdir)
  GlobalParameters$caa.burnin <- burnin
  GlobalParameters$burnin <- caa.burnin
  GlobalParameters$nSamples <- nSamples
  GlobalParameters$thin <- thin
  GlobalParameters$fitfile <- fitfile
  GlobalParameters$predictfile <- predfile
  GlobalParameters$lgamodel <- lgamodel
  GlobalParameters$CC <- CC
  GlobalParameters$CCerror <- defaultCCError
  GlobalParameters$age.error=age.error
  
  if (is.null(seed)){
    seed=""
  }
  GlobalParameters$seed <- seed
  
  AgeLength <- fix_in_prep_agelength(AgeLength)
  WeightLength <- fix_in_prep_weightlength(WeightLength)
  Landings <- fix_in_prep_landings(Landings)

  ## Estimate model
  fit <- eca.estimate(AgeLength,WeightLength,Landings,GlobalParameters)
  
  ## Predict
  pred <- eca.predict(AgeLength,WeightLength,Landings,GlobalParameters)
  
  l<-list()
  l$fit<-fit
  l$pred<-pred
  return(l)
}

projectname <- "ECA_torsk_2015"
result <- runECA(projectname)
plot_pred_box(result$pred)

#  tmp <- load(file.path(file.path(getProjectPaths(projectname)$RDataDir,"RECA", "datafiles"), paste0(projectname, ".RData")))
