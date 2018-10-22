library(Rstox)
setJavaMemory(size=6e+09)
burnindefault=100
samplesdefault=500
projectname <- "ECA_torsk_2015"
#projectname <- "ECA_torsk_2015_cc"
#projectname <- "ECA_sild_2015"
#baselineOutput <- getBaseline(projectname)

runproject <- function(projectname){
  prepareRECA(projectname)
  diagnosticsRECA(projectname)
  runRECA(projectname, burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault)
  plotRECAresults(projectname)
}

#' @param runfile run with parameters stored in file, and runfiledir as GlobalParameters$resultdir (will be created if does not exist)
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


#loadProjectData(projectname, var="prepareRECA") 
#runRECA(projectname, seed=42, export_only = "/Volumes/KINGSTON/torsk_fra_mac.Rdata", burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault)
#runRECA_file(runfile = "/Volumes/KINGSTON/torsk2.Rdata", runfiledir="/Users/a5362/code/github/Rstox_utils/Work/eca")
runproject(projectname)
