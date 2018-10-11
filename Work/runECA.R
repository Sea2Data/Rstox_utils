library(Rstox)
setJavaMemory(size=6e+09)
burnindefault=100
samplesdefault=400
projectname <- "ECA_torsk_2015"
#projectname <- "ECA_torsk_2015_cc"
#projectname <- "ECA_sild_2015"
#baselineOutput <- getBaseline(projectname)

runproject <- function(projectname){
  prepareRECA(projectname)
  diagnosticsRECA(projectname)
  runRECA(projectname, seed=42, burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault)
  plotRECAresults(projectname)
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
runproject(projectname)


#noter til windowstesting:
#Etter test 6. sept 2018 oppdaterte jeg biotic filen for å få inn datakorreksjoner (ca 11. sept). Noen av disse datakorreksjonene vil føre til at filter i oppdatert prosjektfil ikke vil virke. Så:
#- installer oppdatert eca
#- installer Rstox på nytt fra develop
#- last ned ny biotic fil fra api
#- oppdater prosjektfil fra Rstox_utils/stox_processfiles
#- oppdater ressursfiler fra Rstox_utils/resources
#- sett projectname og kjør dette scriptet