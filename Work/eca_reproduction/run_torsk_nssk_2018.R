source("run_project.R")
bioticurl <- ""
landingurl <- ""
projectfile <- "torsk_nssk/stox_2018/project_IV.xml"
projectname <- "ECA_NSSK_torsk_2018_IV"
resultdest <- "torsk_nssk/stox_2018/results/"

#files <- get_data()
# createStoxProject(files$biotic, files$landing)

# createStoxProject not working yet, set up manuall for now
runProject(projectname)
