source("run_project.R")
bioticurl <- "http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/2018/172414/search?version=3.0"
landingurl <- "http://tomcat7.imr.no:8080/apis/nmdapi/landing/v2?version=2.0&type=search&Art_kode=2013&Fangstar=2018"
projectfile <- "makrell/stox_2018/project.xml"
projectname <- "ECA_makrell_2018"
resultdest <- "makrell/stox_2018/results/"

#files <- get_data()
# createStoxProject(files$biotic, files$landing)

# createStoxProject not working yet, set up manuall for now
runProject(projectname)
