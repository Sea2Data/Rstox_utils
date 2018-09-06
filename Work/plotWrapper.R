library(Rstox)

#' Saves plot to stox project location (getProjectPaths(projectname)$RReportDir) to be localised by getPlots in stox.
#' @param projectname name of stox project
#' @param plotname filename for plot without format suffix
#' @param draw function for drawing the plot. Takes no arguments
#' @param format function defining plotting device and file suffix, supports grDevices::pdf, grDevices::png, grDevices::jpeg, grDevices::tiff, grDevices::bmp
#' @param verbose logical, if TRUE info is written to stderr()
#' @param ... parameters to be passed on to format
formatPlot <-
  function(projectname,
           plotname,
           draw,
           format = "png",
           verbose = F,...) {
    lll <- list(...)
    
    filenamebase <-
      file.path(getProjectPaths(projectname)$RReportDir, plotname)
    filename <- paste(filenamebase, format, sep = ".")
    
    if (format %in% c("bmp", "jpeg", "png", "tiff")) {
      moveToTrash(filename)
      do.call(format, c(list(filename = filename), applyParlist(lll,
                                                                format)))
    }
    else if (format %in% c("pdf")){
      moveToTrash(filename)
      do.call(format, c(list(file = filename), applyParlist(lll,
                                                                format)))
    }
      
    tryCatch({
      if (verbose) {
        write(paste("Writing plot to:", filename), stderr())
      }
      draw()},
      finally={
      if (length(format)) {
        dev.off()
      }
    }
    )
    
  }
