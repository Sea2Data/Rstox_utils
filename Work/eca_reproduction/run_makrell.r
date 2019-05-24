source("run_project.R")
bioticurl <- "http://tomcat7-test.imr.no:8080/apis/nmdapi/biotic/v3/2017/172414/search?version=3.0"
landingurl <- "http://tomcat7.imr.no:8080/apis/nmdapi/landing/v2?version=2.0&type=search&Art_kode=2013&Fangstar=2017"
projectfile <- "makrell/stox_2017/project.xml"
projectname <- "ECA_makrell_2017"
resultdest <- "makrell/stox_2017/results/"

#files <- get_data()
# createStoxProject(files$biotic, files$landing)

# createStoxProject not working yet, set up manuall for now
runProject(projectname)
