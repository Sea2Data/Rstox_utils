library(Rstox)
setJavaMemory(size=6e+09)
burnindefault=100
samplesdefault=500
projectname <- "ECA_torsk_2015"
#projectname <- "ECA_torsk_2015_cc"
#projectname <- "ECA_sild_2015"
#baselineOutput <- getBaseline(projectname)

runproject <- function(projectname, burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault){
  prepareRECA(projectname)
  diagnosticsRECA(projectname)
  runRECA(projectname, burnin=burnin, caa.burnin=caa.burnin, nSamples=nSamples)
  plotRECAresults(projectname)
}

#' @param runfile run with parameters stored in file, and runfiledir as GlobalParameters$resultdir (will be created if does not exist)
runRECA_file <- function(runfile=NULL, runfiledir=NULL){
  require(eca)
  require(Rstox)
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

#runproject(projectname)
#loadProjectData(projectname, var="prepareRECA") 
#runRECA(projectname, seed=42, export_only = "~/Desktop/torskesett_prepdata_mac.rdata", burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault)
#runRECA_file(runfile = "~/Desktop/torskesett_prepdata_mac.rdata", runfiledir="/Users/a5362/code/github/Rstox_utils/Work/tmp")