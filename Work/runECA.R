library(Rstox)
setJavaMemory(size=10e+09)
burnindefault=100
samplesdefault=500
tempresdefault=92
lgamodeldefault = "log-linear"
#projectname <- "ECA_nvg_2017"
#projectname <- "ECA_makrell_2017"
#projectname <- "ECA_makrell_2018"
#projectname <- "ECA_NSSK_sei_2018"
#projectname <- "ECA_NSSK_hyse_2018"
#projectname <- "ECA_NSSK_torsk_2018_IV"
#projectname <- "ECA_kolmule_2017"
#projectname <- "ECA_torsk_2015"
#projectname <- "ECA_torsk_2015_cc"
#projectname <- "ECA_sild_2015"
#baselineOutput <- getBaseline(projectname)
#projectname <- "ECA_sild_2015_errorcheck"
projectname <- "ECA_makrell_2018_sindre_nov_2019"

runproject <- function(projectname, burnin=burnindefault, caa.burnin=burnindefault, nSamples=samplesdefault, seed=NULL, tempres=tempresdefault, lgamodel=lgamodeldefault){
  prepareRECA(projectname, temporalresolution=tempres)
  runRECA(projectname, burnin=burnin, caa.burnin=caa.burnin, nSamples=nSamples, seed=seed, lgamodel = lgamodel)
  #getReports(projectname)
  #getPlots(projectname)
}

#' @param runfile run with parameters stored in file, and runfiledir as GlobalParameters$resultdir (will be created if does not exist)
runRECA_file <- function(runfile=NULL, runfiledir=NULL){
  require(Reca)
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
#runRScripts(projectname)
#Rstox:::saveDecomposedCatchMatrix(projectname, "~/test.csv", addQuarterToDecomp = T, plusgr=6)
#loadProjectData(projectname, var="prepareRECA")
#runRECA(projectname, lgamodel = "non-linear", burnin = 1000, nSamples = 500, seed=42, export_only = "bugreports/mactest20190524_sild.rdata")
#runRECA_file(runfile = "testfiles/mactest20190524_sild.rdata", runfiledir="/Users/a5362/temp/ecat")
