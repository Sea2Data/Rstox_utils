library(Rstox)
srcdir <- "/Users/a5362/code/github/Rstox_utils/Work"
source(file.path(srcdir, "plot_results_ECA.R"))

reportRECA <- function(projectname){
  rundata <- loadProjectData(projectname, var="runRECA")
  plot_pred_box(rundata$runRECA$pred) 
}
projectname <- "ECA_torsk_2015"
reportRECA(projectname)