library(Rstox)
srcdir <- "/Users/a5362/code/github/Rstox_utils/Work"
source(file.path(srcdir, "plot_results_ECA.R"))
source(file.path(srcdir, "plotWrapper.R"))

#' Generates plots and reports from RECA prediction
#' @param projectname name of stox project
#' @param var A key string indicating the variable to plot. ´Abundance´ and ´Weight´ is implemented. 
#' @param unit A unit key string indicating the unit (see getPlottingUnit()$definitions$unlist.units for available key strings)
#' @param verbose logical, if TRUE info is written to stderr()
#' @param format function defining filtetype for plots, supports grDevices::pdf, grDevices::png, grDevices::jpeg, grDevices::tiff, grDevices::bmp
#' @param ... parameters passed on plot function and format
plotCatchByAge <- function(projectName, var = "Abundance", unit="millions", verbose=F, format="png", ...){
  rundata <- loadProjectData(projectName, var="runRECA")
  
  if (format=="png"){
    #dimension in pixels
    width=5000
    height=3000
    res=500
  }
  if (format=="pdf"){
    #dimension in inches
    width=10
    height=6
    res=NULL
  }
  
  formatPlot(projectname, "catch_by_age", function(){plot_pred_ci(rundata$runRECA$pred, var=var, unit=unit, ...)}, verbose=verbose, format=format, height=height, width=width, res=res)

  warning("Implement save catch matrix")
  #save catch matrix
}
projectName <- "ECA_torsk_2015"
plotCatchByAge(projectName)