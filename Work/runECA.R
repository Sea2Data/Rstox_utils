library(eca)
library(Rstox)

burnindefault=100
samplesdefault=400

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
runRECA(projectname, seed=42, burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault)
