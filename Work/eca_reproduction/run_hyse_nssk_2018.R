source("run_project.R")
bioticurl <- ""
landingurl <- ""
projectfile <- "hyse_nssk/stox_2018/project.xml"
projectname <- "ECA_NSSK_hyse_2018"
resultdest <- "hyse_nssk/stox_2018/results/"

#files <- get_data()
# createStoxProject(files$biotic, files$landing)

# createStoxProject not working yet, set up manuall for now
runProject(projectname, destination = resultdest)
