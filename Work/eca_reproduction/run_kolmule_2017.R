source("run_project.R")
bioticurl <- "http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/2017/164774/search?version=3.0"
landingurl <- "http://tomcat7.imr.no:8080/apis/nmdapi/landing/v2?version=2.0&type=search&Art_kode=1038&Fangstar=2017"
projectfile <- "kolmule/stox_2017/project.xml"
projectname <- "ECA_kolmule_2017"
resultdest <- "kolmule/stox_2017/results/"

#files <- get_data()
# createStoxProject(files$biotic, files$landing)

# createStoxProject not working yet, set up manuall for now
runProject(projectname)
