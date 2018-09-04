library(Rstox)
plotWrapper <-
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
      if (!("width" %in% names(lll))) {
        lll$width <- 5000
      }
      if (!("height" %in% names(lll))) {
        lll$height <- 3000
      }
      if (!("res" %in% names(lll))) {
        lll$res <- 500
      }
      moveToTrash(filename)
      do.call(format, c(list(filename = filename), applyParlist(lll,
                                                                format)))
    }
    else if (format %in% c("pdf")){
      if (!("width" %in% names(lll))) {
        lll$width <- 5000/500
      }
      if (!("height" %in% names(lll))) {
        lll$height <- 3000/500
      }
      moveToTrash(filename)
      do.call(format, list(file = filename, width=lll$width, height=lll$height))
    }
      
    tryCatch({
      if (verbose) {
        write(paste("Plot written to:", filename), stderr())
      }
      draw()},
      finally={
      if (length(format)) {
        dev.off()
      }
    }
    )
    
  }
