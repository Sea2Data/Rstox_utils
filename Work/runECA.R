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

burnindefault=100
samplesdefault=400
thindefault=1
defaultfitfile="fit"
defaultpredfile="pred"
defaultlgamodel="log-linear"
defaultCC=TRUE
defaultCCError=FALSE
age.error.default=FALSE

#' run fit and prediction. Save results to project data 'runRECA'
#' parameters not described below are defined in eca.estimate and eca.predict
#' @param export_only if not NULL this indicates that eca should not be run, but all parameters should be exported to the file export_only
runRECA <- function(projectname, burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault, thin=thindefault, fitfile=defaultfitfile, predfile=defaultpredfile, lgamodel=defaultlgamodel, CC=defaultCC, CCError=defaultCCError, seed=NULL, age.error=age.error.default, export_only=NULL){
  # Sett kjøreparametere her, sett dataparametere i prep_eca
    prepdata <- loadProjectData(projectname, var="prepareRECA")   
    prepareRECA <- prepdata$prepareRECA
    if (is.null(prepdata)){
      stop("Could not load project data")
    }
    GlobalParameters <- prepareRECA$GlobalParameters
    AgeLength <- prepareRECA$AgeLength
    WeightLength <- prepareRECA$WeightLength
    Landings <- prepareRECA$Landings

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

  if (!is.null(export_only)){
    save(GlobalParameters, AgeLength, WeightLength, Landings, file=export_only)
  }
  else{
    ## Estimate model
    fit <- eca.estimate(AgeLength,WeightLength,Landings,GlobalParameters)
    
    ## Predict
    pred <- eca.predict(AgeLength,WeightLength,Landings,GlobalParameters)
    
    setProjectData(projectName=projectname, var=list(fit=fit, pred=pred), name="runRECA")
  }
}

#' @param runfile run with parameters stored in file, and runfiledir as GlovalParameters$resultdir (will be created if does not exist)
runRECA_file <- function(runfile=NULL, runfiledir=NULL){
  write(paste("Loading from file:", runfile), stderr())
  load(runfile)
  GlobalParameters$resultdir <- runfiledir
  if(!(file.exists(GlobalParameters$resultdir))){
    dir.create(GlobalParameters$resultdir, recursive=T)
  }
  ## Estimate model
  fit <- eca.estimate(AgeLength,WeightLength,Landings,GlobalParameters)
  
  pred <- eca.predict(AgeLength,WeightLength,Landings,GlobalParameters)
}

#runRECA(projectname, seed=42, export_only = "/Users/a5362/Desktop/torskesett_prepdata_mac.rdata")
#runRECA_file(runfile = "/Users/a5362/Desktop/torskesett_prepdata_mac.rdata", runfiledir="/Users/a5362/code/github/Rstox_utils/Work/reca")
runRECA(projectname, seed=42)
