library(Rstox)
setJavaMemory(size=10e+09)
burnindefault=100
samplesdefault=500
tempresdefault=92
projectname <- "ECA_torsk_2015"
#projectname <- "ECA_torsk_2015_cc"w
#projectname <- "ECA_sild_2015"
#baselineOutput <- getBaseline(projectname)

runproject <- function(projectname, burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault, seed=NULL, tempres=tempresdefault){
  prepareRECA(projectname, temporalresolution=tempres)
  runRECA(projectname, burnin=burnin, caa.burnin=caa.burnin, nSamples=nSamples, seed=seed)
  getReports(projectname)
  getPlots(projectname)
}

#' @param runfile run with parameters stored in file, and runfiledir as GlobalParameters$resultdir (will be created if does not exist)
runRECA_file <- function(runfile=NULL, runfiledir=NULL){
  require(eca)
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

#runproject(projectname, tempres = 46)
#loadProjectData(projectname, var="prepareRECA")
#runRECA(projectname, seed=42, export_only = "testfiles/torsk_nonstd_tempres.Rdata", burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault)
#runRECA_file(runfile = "bugreports/mactest20181122_reca0.8/length_str.rdata", runfiledir="/Users/a5362/code/github/Rstox_utils/Work/tmp/t")
