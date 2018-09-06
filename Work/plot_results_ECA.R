#' Constructs equal tailed credibility intervals. The probability mass above and below the interval is approximatelty the same.
#' @param age agegroups integer vector
#' @param values matrix where age indexes rows, and columns index points in the posterior distribution
#' @param alpha value for the credibility intervals
#' @return list with means, upper_ci and lower_ci all numeric vectors corresponding to age
get_eq_tail_ci <- function(age, values, alpha){
  postm <- c()
  upper <- c()
  lower <- c()
  for (i in 1:length(age)){
    postm <- c(postm, mean(values[i,]))
    upper <- c(upper, quantile(values[i,], 1-(alpha/2.0)))
    lower <- c(lower, quantile(values[i,], (alpha/2.0)))
  }
  res <- list()
  res$means <- postm
  res$upper_ci <- upper
  res$lower_ci <- lower
  return(res)
}

#' Plots means with error bars
plot_ci <- function(x, means, upper_ci, lower_ci, ...){
  args <- list(...)
  
  args$x=x
  args$y=upper_ci
  args$type="n"
  do.call(plot, args)
  args$y=means
  args$type="p"
  do.call(points, args)
  
  segments(x, upper_ci, x, lower_ci, lwd = 1.5, ...)
  arrows(x, upper_ci, x,
         lower_ci, lwd = 1.5, angle = 90,
         code = 3, length = 0.05, ...)
  
}

#' Plot catch by age prediction as boxplots
#' @param pred RECA prediction object as returned by eca::eca.predict
#' @param var A key string indicating the variable to plot. ´Abundance´ and ´Weight´ is implemented. 
#' @param unit A unit key string indicating the unit (see getPlottingUnit()$definitions$unlist.units for available key strings)
plot_pred_box <- function(pred, var, unit, xlab="age", ylab=paste("posterior catch", unit), ...){
  
  if (var=="Abundance" | var=="Count"){
    plottingUnit=getPlottingUnit(unit=unit, var=var, baseunit="ones", def.out = F)
    caa <- apply(pred$TotalCount, c(2,3), sum)
  }
  else if (var=="Weight"){
    caa <- apply(pred$TotalCount, c(2,3), sum)*pred$MeanWeight
    plottingUnit=getPlottingUnit(unit=unit, var=var, baseunit="kilograms", def.out = F)
  }
  else{
    stop("Not implemented")
  }
  caa_scaled <- caa/plottingUnit$scale
  
  post <- c()
  age <- c()
  for (i in 1:length(pred$AgeCategories)){
    post <- c(post, caa_scaled[i,])
    age <- c(age, rep(pred$AgeCategories[i], length(caa_scaled[i,])))
  }
  
  args <- list(...)
  if (!("las" %in% names(args))){
    args$las=2
  }
  args$xlab=xlab
  args$ylab=ylab
  
  do.call(boxplot, c(post~age, args))

}

#' Plot equal tailed credible intervals for a catch by age prediction
#' @param pred RECA prediction object as returned by eca::eca.predict
#' @param var A key string indicating the variable to plot. ´Abundance´ and ´Weight´ is implemented. 
#' @param unit A unit key string indicating the unit (see getPlottingUnit()$definitions$unlist.units for available key strings)
#' @param alpha
plot_catch_at_age_ci <- function(pred, var, unit, alpha=0.1, xlab="age", ylab=paste("posterior catch", unit), ...){
  if (var=="Abundance" | var=="Count"){
    plottingUnit=getPlottingUnit(unit=unit, var=var, baseunit="ones", def.out = F)
    caa <- apply(pred$TotalCount, c(2,3), sum)
  }
  else if (var=="Weight"){
    caa <- apply(pred$TotalCount, c(2,3), sum)*pred$MeanWeight
    plottingUnit=getPlottingUnit(unit=unit, var=var, baseunit="kilograms", def.out = F)
  }
  else{
    stop("Not implemented")
  }
  caa_scaled <- caa/plottingUnit$scale

  res <- get_eq_tail_ci(pred$AgeCategories, caa_scaled, alpha)

  args <- alist(...)
  if (!("las" %in% names(args))){
    args$las=1
  }
  if (!("pch" %in% names(args))){
    args$pch=19
  }
  if (!("lty" %in% names(args))){
    args$lty=1
  }
  if (!("main" %in% names(args))){
    args$main=paste("Catch at age (", var, ")", sep="")
  }
  args$xlab=xlab
  args$ylab=ylab
  
  args$x=pred$AgeCategories
  args$means <- res$means
  args$upper_ci <- res$upper_ci
  args$lower_ci <- res$lower_ci
  
  do.call(plot_ci, args)
  legend("topright", pch=c(args$pch, NA), lty=c(NA, args$lty), legend=c("mean", paste(100-alpha*100, "% interval", sep="")), bty="n")
  
}


#' Plots mean weight at age and sample range
#' @param biotic indiviuals as exported by stox
#' @param pred RECA prediction object as returned by eca::eca.predict
plot_weight_at_age <- function(biotic, pred, unit, alpha=0.01, xlab="age", ylab=paste("ind. weight", unit), ...){
  
  plottingUnitEca=getPlottingUnit(unit=unit, var="Weight", baseunit="kilograms", def.out = F)
  plottingUnitBiotic=getPlottingUnit(unit=unit, var="Weight", baseunit="grams", def.out = F)
  
  #sample weights by age
  maxweights <- aggregate(list(weight=biotic$weight), by=list(age=biotic$age), FUN=function(x){max(x, na.rm=T)})
  minweights <- aggregate(list(weight=biotic$weight), by=list(age=biotic$age), FUN=function(x){min(x, na.rm=T)})
  maxweights$weight <- maxweights$weight/plottingUnitBiotic$scale
  minweights$weight <- minweights$weight/plottingUnitBiotic$scale
  
  #prediction mewans
  weight_scaled <- pred$MeanWeight/plottingUnitEca$scale
  res <- get_eq_tail_ci(pred$AgeCategories, weight_scaled, alpha)
  
  means <- data.frame(age=pred$AgeCategories, means=res$means)
  upper <- data.frame(age=maxweights$age, upper=maxweights$weight)
  lower <- data.frame(age=minweights$age, lower=minweights$weight)
  
  comp <- merge(means, merge(upper, lower), all.x=T)
  
  args <- alist(...)
  
  if (!("las" %in% names(args))){
    args$las=1
  }
  if (!("pch" %in% names(args))){
    args$pch=19
  }
  if (!("lty" %in% names(args))){
    args$lty="dotted"
  }
  if (!("ylim" %in% names(args))){
    args$ylim=c(min(min(lower, na.rm=T), 0), max(0,max(upper, na.rm=T)))
  }
  if (!("main" %in% names(args))){
    args$main="Weight at age"
  }
  
  args$xlab=xlab
  args$ylab=ylab
  
  args$x=comp$age
  args$means <- comp$means
  args$upper_ci <- comp$upper
  args$lower_ci <- comp$lower
  
  do.call(plot_ci, args)
  legend("topleft", pch=c(args$pch, NA), lty=c(NA, args$lty), legend=c("model mean", "sample range"), bty="n")
  
} 

#' Plots mean length at age and sample range
#' @param biotic indiviuals as exported by stox
#' @param pred RECA prediction object as returned by eca::eca.predict
plot_length_at_age <- function(biotic, pred, xlab="age", alpha=0.01, ylab=paste("length cm"), ...){
  
  bioticscale = 1
  ecascale = 1
  
  #sample length by age
  maxlengths <- aggregate(list(length=biotic$length), by=list(age=biotic$age), FUN=function(x){max(x, na.rm=T)})
  minlengths <- aggregate(list(length=biotic$length), by=list(age=biotic$age), FUN=function(x){min(x, na.rm=T)})
  maxlengths$length <- maxlengths$length/bioticscale
  minlengths$length <- minlengths$length/bioticscale
  
  #prediction mewans
  length_scaled <- pred$MeanLength/ecascale
  res <- get_eq_tail_ci(pred$AgeCategories, length_scaled, alpha)
  
  means <- data.frame(age=pred$AgeCategories, means=res$means)
  upper <- data.frame(age=maxlengths$age, upper=maxlengths$length)
  lower <- data.frame(age=minlengths$age, lower=minlengths$length)
  
  comp <- merge(means, merge(upper, lower), all.x=T)
  
  args <- alist(...)
  
  if (!("las" %in% names(args))){
    args$las=1
  }
  if (!("pch" %in% names(args))){
    args$pch=19
  }
  if (!("lty" %in% names(args))){
    args$lty="dotted"
  }
  if (!("ylim" %in% names(args))){
    args$ylim=c(min(min(lower, na.rm=T), 0), max(0,max(upper, na.rm=T)))
  }
  if (!("main" %in% names(args))){
    args$main="Length at age"
  }
  
  args$xlab=xlab
  args$ylab=ylab
  
  args$x=comp$age
  args$means <- comp$means
  args$upper_ci <- comp$upper
  args$lower_ci <- comp$lower
  
  do.call(plot_ci, args)
  legend("topleft", pch=c(args$pch, NA), lty=c(NA, args$lty), legend=c("model mean", "sample range"), bty="n")
  
} 

#' Plots a panel of RECA results
#' @param biotic indiviuals as exported by stox
#' @param pred RECA prediction object as returned by eca::eca.predict
plot_RECA_results_panel <- function(pred, biotic, ...){
  par.old <- par(no.readonly = T)
  par(mfrow=c(2,2))
  plot_catch_at_age_ci(pred, var="Abundance", unit="millions", ...)
  plot_catch_at_age_ci(pred, var="Weight", unit="tt", ...)
  plot_weight_at_age(biotic, pred, unit="kg", ...)
  plot_length_at_age(biotic, pred, ...)
  par(par.old)
}

season_plot_test <- function(){
  stop("Not implemented")
  par(mfrow=c(2,2))
  for (s in unique(Landings$AgeLengthCov$season)){
    print(paste0("Q", s))
    valuename <- AgeLength$resources$covariateLink$season$Covariate[AgeLength$resources$covariateLink$season$Numeric==s]
    sl <- Landings
    keep_alc <- sl$AgeLengthCov$season==s
    sl$AgeLengthCov <- sl$AgeLengthCov[keep_alc,]
    sl$WeightLengthCov <- sl$WeightLengthCov[keep_alc,]
    sl$LiveWeightKG <- sl$LiveWeightKG[keep_alc]
    print(sl)
    GlobalParameters$predictfile <- paste("predict",s,sep="_")
    predsl <- eca.predict(newAgeLength,newWeightLength,sl,GlobalParameters)  
    plot_pred_box(predsl, valuename)
  }
  par(mfrow=c(1,1))
}


#plots needed:
# from raw data age length and length weight plots to determine applicability to species
# caa in numbers
# caa in weight
# mean weight pr age
# mean length pr age

# other ideas
# shape parameters for caa or prop at age (total catch is fixed, so samples are correlated)