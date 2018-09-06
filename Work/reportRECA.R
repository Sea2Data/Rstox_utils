library(Rstox)
srcdir <- "/Users/a5362/code/github/Rstox_utils/Work"
source(file.path(srcdir, "plot_results_ECA.R"))
source(file.path(srcdir, "plotWrapper.R"))

#' Generates plots and reports from RECA prediction
#' @param projectname name of stox project
#' @param verbose logical, if TRUE info is written to stderr()
#' @param format function defining filtetype for plots, supports grDevices::pdf, grDevices::png, grDevices::jpeg, grDevices::tiff, grDevices::bmp
#' @param ... parameters passed on plot function and format
plotRECAresults <- function(projectName, verbose=F, format="png", ...){
  rundata <- loadProjectData(projectName, var="runRECA")
  prep <- loadProjectData(projectName, var="prepareRECA")
  
  if (format=="png"){
    #dimension in pixels
    width=5000
    height=5000
    res=500
  }
  if (format=="pdf"){
    #dimension in inches
    width=10
    height=10
    res=NULL
  }
  
  formatPlot(projectname, "RECA_results", function(){plot_RECA_results_panel(rundata$runRECA$pred, prep$prepareRECA$StoxExport$biotic, ...)}, verbose=verbose, format=format, height=height, width=width, res=res, ...)
  
  warning("Implement save catch matrix")
  warning("Fix defaults for units kt and mt. Consider adding lengthunits")
  #save catch matrix
}
projectName <- "ECA_torsk_2015"
plotRECAresults(projectName)