#' Plot catch by age prediction as boxplots
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
#' @param var A key string indicating the variable to plot. ´Abundance´ and ´Weight´ is implemented. 
#' @param unit A unit key string indicating the unit (see getPlottingUnit()$definitions$unlist.units for available key strings)
#' @param alpha
plot_pred_ci <- function(pred, var, unit, alpha=0.1, xlab="age", ylab=paste("posterior catch", unit), ...){
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

  postm <- c()
  upper <- c()
  lower <- c()
  for (i in 1:length(pred$AgeCategories)){
    postm <- c(postm, mean(caa_scaled[i,]))
    upper <- c(upper, quantile(caa_scaled[i,], 1-(alpha/2.0)))
    lower <- c(lower, quantile(caa_scaled[i,], (alpha/2.0)))
  }
  
  args <- alist(...)
  if (!("las" %in% names(args))){
    args$las=1
  }
  if (!("pch" %in% names(args))){
    args$pch=19
  }
  args$xlab=xlab
  args$ylab=ylab
  args$x=pred$AgeCategories
  args$y=upper
  args$type="n"
  
  do.call(plot, args)
  args$y=postm
  args$type="p"
  do.call(points, args)
  
  segments(pred$AgeCategories, upper, pred$AgeCategories, lower, lwd = 1.5)
  
  arrows(pred$AgeCategories, upper, pred$AgeCategories,
         lower, lwd = 1.5, angle = 90,
         code = 3, length = 0.05)
  
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