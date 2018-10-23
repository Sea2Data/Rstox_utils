#' Tests running eca formatted data. Simply runs eca.estiamate and eca.fit.
#' @param file file with eca formatted data
#' @param runfiledir direcotry to use for eca output
testEcaFormatted <- function(file, runfiledir){
  require(eca)
  require(Rstox)
  write(paste("Loading from file:", file), stderr())
  load(file)
  GlobalParameters$resultdir <- runfiledir
  if(!(file.exists(GlobalParameters$resultdir))){
    stop(paste("Directory", runfiledir, "does not exist."))
  }
  ## Estimate model
  fit <- eca.estimate(AgeLength,WeightLength,Landings,GlobalParameters)
  pred <- eca.predict(AgeLength,WeightLength,Landings,GlobalParameters)
}

runTest <- function(testfile, testfiles="./testfiles", tmpdir="./tmp"){
  if(!(file.exists(tmpdir))){
    stop(paste("Directory", tmpdir, "does not exist."))
  }
  if(!(file.exists(testfiles))){
    stop(paste("Directory", testfiles, "does not exist."))
  }
    ecadir <- gsub(".Rdata", "", testfile)
    if (!(file.exists(file.path(tmpdir, ecadir)))){
      dir.create(file.path(tmpdir, ecadir))
    }
    testEcaFormatted(file.path(testfiles,testfile), file.path(tmpdir, ecadir))
  }

#' @param tmpdir location where tests will generate output
runAllTests <- function(testfiles="./testfiles", tmpdir="./tmp"){
  if(!(file.exists(tmpdir))){
    stop(paste("Directory", tmpdir, "does not exist."))
  }
  if(!(file.exists(testfiles))){
    stop(paste("Directory", testfiles, "does not exist."))
  }
  for (testfile in list.files(testfiles)){
    runTest(testfile, testfiles, tmpdir)
  }
}
#runAllTests()
runTest("herring_2015_tempfixed_gearrandom_100samples.Rdata")