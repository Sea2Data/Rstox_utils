library(eca)
library(Rstox)

#
# workarounds
#

fix_in_prep_agelength<-function(AgeLength){
  ## Select covariates - not use haulweight and boat now
  newAgeLength <- AgeLength
  #newAgeLength$info["spatial", "CAR"] <- 1

  return(newAgeLength)  
}
fix_in_prep_weightlength <- function(WeightLength){
  newWeightLength <- WeightLength

  #newWeightLength$info["spatial", "CAR"] <- 1

  return(newWeightLength)
}
fix_in_prep_landings <- function(Landings){
  newLandings <- Landings

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
runRECA <- function(projectname, burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault, thin=thindefault, fitfile=defaultfitfile, predfile=defaultpredfile, lgamodel=defaultlgamodel, CC=defaultCC, CCError=defaultCCError, seed=NULL, age.error=age.error.default){
  warning("write doc for runECA")
  # Sett kjøreparametere her, sett dataparametere i prep_eca

  prepdata <- loadProjectData(projectname, var="prepareRECA")
  GlobalParameters <- prepdata$prepareRECA$GlobalParameters
  AgeLength <- prepdata$prepareRECA$AgeLength
  WeightLength <- prepdata$prepareRECA$WeightLength
  Landings <- prepdata$prepareRECA$Landings
  
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
  
  setProjectData(projectName=projectname, var=list(fit=fit, pred=pred), name="runRECA")
}

projectname <- "ECA_torsk_2015"
runRECA(projectname)
