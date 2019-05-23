library(Rstox)
bioticurl <- "http://tomcat7-test.imr.no:8080/apis/nmdapi/biotic/v3/2017/164774/search?version=3.0"
landingurl <- "http://tomcat7.imr.no:8080/apis/nmdapi/landing/v2?version=2.0&type=search&Art_kode=1038&Fangstar=2017"
projectfile <- "kolmule/stox_2017/project.xml"
projectname <- "ECA_kolmule_2017"
resultdest <- "kolmule/stox_2017/results/"

get_data <- function(){
  landingsfile = file.path("tempfiles", "landings.xml")
  download.file(url = landingurl, landingsfile)
  bioticfile = file.path("tempfiles", "biotic.xml")
  download.file(url = bioticurl, bioticfile)
  
  files <- list()
  files$landnings <- landingsfile
  files$biotic <- bioticfile
  return(files)
}

createStoxProject <- function(bioticfile, landingsfile, projectfile=projectfile){
  files <- list()
  files$biotic <- bioticfile
  files$landing <- landingsfile
  files$process <- projectfile
  createProject(projectName = projectname, files = files, dir=file.path("tempfiles",projectname), ow=T)
  return(projectname)
}

runProject <- function(projectname, iterations=10, destination=resultdest){
  
  #check for existing resul files and terminate with error
  if (length(list.files(destination))>0){
    stop(paste("Destination (", destination, ") already populated", sep=""))
  }
  
  prepareRECA(projectname)
  for (i in 1:iterations){
    runRScripts(projectname)
    getReports(projectname)
    
    resultfile <- file.path(getProjectPaths(projectname)$RReportDir, "means_Abundance_millions.txt")
    destfile <- file.path(destination, paste(i,"means_Abundance_millions.txt", sep="_"))
    print(resultfile)
    print(destfile)
    file.copy(resultfile, destfile)
  }
  closeProject(projectname)
}
#files <- get_data()
# createStoxProject(files$biotic, files$landing)

# createStoxProject not working yet, set up manuall for now
runProject(projectname)
