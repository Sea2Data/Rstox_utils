source("run_project.R")
#correct snapshot for landings not available through API for this estimate. Handle manually
bioticurl <- "http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/2018/164727/search?version=3.0"
landingurl <- NULL # "http://tomcat7.imr.no:8080/apis/nmdapi/landing/v2?version=2.0&type=search&Art_kode=1032&Fangstar=2018"
projectfile <- "sei_nssk/stox_2018/project.xml"
projectname <- "ECA_NSSK_sei_2018"
resultdest <- "sei_nssk/stox_2018/results/"

#files <- get_data()
# createStoxProject(files$biotic, files$landing)

# createStoxProject not working yet, set up manuall for now
setJavaMemory(size=10e+09)
runProject(projectname)
