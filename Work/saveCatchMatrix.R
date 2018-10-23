#' @title Save catch at age matrix
#' @description Write catch at age predicted by \code{\link[eca]{eca.predict}} as csv file.
#' @details Catch at age matrix is written as comma-separated file with quoted strings as row/column names.
#'    Each row correspond to an age group, and columns to either means or an iteration of the Monte Carlo simulation.
#'    Units are controlled by parameters, and written as metainformation in a preamble identified by the comment charater '#', along with any text provided in other arguments (parameter main).
#'@param pred as returned by \code{\link[eca]{eca.predict}}
#'@param filename name of file to save to.
#'@param var Variable to extract. Allows for Abundance, Count or Weight
#'@param unit Unit for extracted variable. See \code{\link{getPlottingUnit}}
#'@param main Title for the analysis, to be included as comment in saved file (e.g. species and year)
#'@param savemeans If True, only means for each age group will be saved, otherwise extracted variable is saved for each iteration of the Monte Carlo simulation.
saveCatchMatrix <- function(pred, filename, var="Abundance", unit="ones", main="", savemeans=F){
  comments <- c()
  if (savemeans){
    title <- "Mean catch at age estimates"
  }
  else{
    title <- "Catch at age estimates"
  }
  
  if (var=="Abundance" | var=="Count"){
    plottingUnit=getPlottingUnit(unit=unit, var=var, baseunit="ones", def.out = F)
    caa <- round(apply(pred$TotalCount, c(2,3), sum))
    
    if (unit=="ones"){
      comments <- c(paste(title, "as", var))
    }
    if (unit!="ones"){
      comments <- c(paste(title, "as", var, "in", unit))
    }
    
  }
  else if (var=="Weight"){
    caa <- apply(pred$TotalCount, c(2,3), sum)*pred$MeanWeight
    plottingUnit=getPlottingUnit(unit=unit, var=var, baseunit="kilograms", def.out = F)
    comments <- c(paste(title, "as", var, "in", unit))
  }
  else{
    stop("Not implemented")
  }
  comments <- c(main, comments)
  caa_scaled <- caa/plottingUnit$scale
  means <- as.data.frame(list(mean=rowMeans(caa_scaled)))
  rownames(means) <- paste("Age", pred$AgeCategories)
  rownames(caa_scaled) <- paste("Age", pred$AgeCategories)
  colnames(caa_scaled) <- paste("Iteration", 1:ncol(caa_scaled))
  
  f <- file(filename, open="w")
  write(paste("#", comments), f)
  if (savemeans){
    write.csv(means, file = f, row.names = T)
    close(f)
  }
  else{
    write.csv(caa_scaled, file = f, row.names = T)
    close(f)
  }
}
projectname="ECA_torsk_2015"
pd<-loadProjectData(projectname, var="prepareRECA") 
saveCatchMatrix(pd$runRECA$pred, "testmatrix.csv", main="COD 2015", savemeans = T)