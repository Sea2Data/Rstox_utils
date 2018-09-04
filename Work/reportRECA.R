library(Rstox)
srcdir <- "/Users/a5362/code/github/Rstox_utils/Work"
source(file.path(srcdir, "plot_results_ECA.R"))
source(file.path(srcdir, "plotWrapper.R"))

reportRECA <- function(projectname, verbose=F, format="png", ...){
  rundata <- loadProjectData(projectname, var="runRECA")
  plotWrapper(projectname, "catch_by_age", function(){plot_pred_box(rundata$runRECA$pred, ...)}, verbose=verbose, format=format, ...)
  
  #write results as in examples
  
  #ordne plot etter oppskrift fra plotAbundance_AcousticTrawl
  #legg png plot i getProjectPaths(projectName)$RReportDir
  
}
projectname <- "ECA_torsk_2015"
reportRECA(projectname)

example <- function (projectName, var = "Abundance", unit = NULL, baseunit = NULL, 
          grp1 = "age", grp2 = NULL, xlab = NULL, ylab = NULL, main = "", 
          format = "png", maxcv = 1, log = NULL, filetag = NULL, ...) 
{
  lll <- list(...)
  if ("numberscale" %in% names(lll)) {
    warning("The argument numberscale is deprecated. Use the new argument 'unit' instead.")
    unit <- lll$numberscale
    lll$numberscale <- NULL
  }
  plottingUnit <- getPlottingUnit(unit = unit, var = var, baseunit = baseunit, 
                                  def.out = FALSE)
  temp <- reportAbundance(projectName, grp1 = grp1, grp2 = grp2, 
                          numberscale = plottingUnit$scale, plotOutput = TRUE, 
                          msg = FALSE)
  if (length(temp) == 0) {
    warning("No plots generated. Possibly due to mismatch between the parameter 'bootstrapMethod' in the bootstrapping and in the plotting function.")
  }
  outList <- list(filename = NULL, data = NULL)
  for (i in seq_along(temp)) {
    level <- names(temp)[i]
    out <- temp[[i]]$abnd
    thisgrp1 <- temp[[i]]$grp1
    thisgrp2 <- temp[[i]]$grp2
    abundanceSum <- temp[[i]]$abundanceSum
    cat("Abundance by age for ", level, "\n", se0 = "")
    xlab <- paste(thisgrp1)
    abundanceSum[[thisgrp1]] <- factorNAfirst(abundanceSum[[thisgrp1]])
    out[[thisgrp1]] <- factorNAfirst(out[[thisgrp1]])
    if (!is.empty(grp2)) {
      xlab <- paste(thisgrp1, "by", grp2)
      abundanceSum[[thisgrp2]] <- factorNAfirst(abundanceSum[[thisgrp2]])
      out[[thisgrp2]] <- factorNAfirst(out[[thisgrp2]])
    }
    if (length(ylab) == 0) {
      ylab <- paste0(plottingUnit$var, " (", plottingUnit$unit, 
                     ")")
    }
    filenamebase <- file.path(getProjectPaths(projectName)$RReportDir, 
                              paste0(c(level, plottingUnit$var, thisgrp1, grp2, 
                                       filetag), collapse = "_"))
    filename <- paste(filenamebase, format, sep = ".")
    if (!all(c("width", "height") %in% names(lll))) {
      lll$width <- 5000
      lll$height <- 3000
      lll$res <- 500
    }
    if (length(format)) {
      do.call(format, c(list(filename = filename), applyParlist(lll, 
                                                                format)))
      moveToTrash(filename)
    }
    maxcv <- min(maxcv, max(out$Ab.Sum.cv, na.rm = TRUE))
    if (maxcv == 0) {
      maxcv <- 1
    }
    cvLabels <- pretty(c(0, maxcv))
    if (isTRUE(log)) {
      log <- "y"
    }
    if ("y" %in% log) {
      ylim <- range(abundanceSum$Ab.Sum, na.rm = TRUE)
    }
    else {
      ylim <- c(0, max(abundanceSum$Ab.Sum, na.rm = TRUE))
    }
    cvScalingFactor <- max(ylim)/maxcv
    ylim[2] <- ylim[2] * (1 + 1e-12)
    outtmp <- out
    outtmp$Ab.Sum.cv <- outtmp$Ab.Sum.cv * cvScalingFactor
    tryCatch({
      if (is.empty(grp2)) {
        pl <- ggplot() + geom_boxplot(data = abundanceSum, 
                                      aes_string(x = thisgrp1, y = "Ab.Sum"), outlier.shape = 18) + 
          theme_bw() + scale_x_discrete(drop = FALSE) + 
          geom_line(aes_string(x = thisgrp1, y = "Ab.Sum.cv", 
                               group = 1), data = outtmp, show.legend = FALSE) + 
          geom_point(aes_string(x = thisgrp1, y = "Ab.Sum.cv", 
                                group = 1), data = outtmp, show.legend = FALSE)
      }
      else {
        pl <- ggplot() + geom_boxplot(data = abundanceSum, 
                                      aes_string(x = thisgrp1, y = "Ab.Sum", fill = grp2), 
                                      outlier.shape = 18) + theme_bw() + scale_x_discrete(drop = FALSE) + 
          scale_fill_discrete(name = grp2) + geom_line(aes_string(x = thisgrp1, 
                                                                  y = "Ab.Sum.cv", group = grp2, colour = out[[grp2]]), 
                                                       data = outtmp, show.legend = FALSE) + geom_point(aes_string(x = thisgrp1, 
                                                                                                                   y = "Ab.Sum.cv", group = grp2, colour = out[[grp2]]), 
                                                                                                        data = outtmp, show.legend = FALSE)
      }
      pl <- pl + scale_y_continuous(trans = if ("y" %in% 
                                                log) 
        "log10"
        else "identity", sec.axis = sec_axis(~./cvScalingFactor, 
                                             name = "CV")) + coord_cartesian(ylim = ylim) + 
        xlab(xlab) + ylab(ylab) + ggtitle(main)
      pl + theme(axis.text = element_text(size = 1.5), 
                 axis.title = element_text(size = 2, face = "bold"), 
                 , legend.text = element_text(size = 2))
      suppressWarnings(print(pl))
    }, finally = {
      if (length(format)) {
        dev.off()
      }
    })
    outList$filename[[level]] <- filename
    outList$data[[level]] <- temp[[i]]
  }
}