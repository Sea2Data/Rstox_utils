plot_pred_hist <- function(pred, title=""){
  caa <- apply(pred$TotalCount, c(2,3), sum)
  caa_kt <- caa/1e6
  par(mfcol=c(round(mage/2),2))
  mm <- max(caa_kt)+5
  bins <- seq(0,mm,5)
  for (i in 1:max(pred$AgeCategories)){
    hist(caa_kt[i,], breaks=bins, xlab="posterior catch mFish", main=paste("age", pred$AgeCategories[i]), xlim=c(0,200), main=title)
  }
  par(mfcol=c(1,1))
}
plot_pred_box <- function(pred, title=""){
  caa <- apply(pred$TotalCount, c(2,3), sum)
  caa_kt <- caa/1e6
  
  post <- c()
  age <- c()
  for (i in 1:length(pred$AgeCategories)){
    post <- c(post, caa_kt[i,])
    age <- c(age, rep(pred$AgeCategories[i], length(caa_kt[i,])))
  }
  
  boxplot(post~age, las=2, xlab="age", ylab="posterior catch Mfish", main=title)
}

plot_pred_ci <- function(pred, title="", alpha=.1){
  caa <- apply(pred$TotalCount, c(2,3), sum)
  caa_kt <- caa/1e6
  
  post <- c()
  upper <- c()
  lower <- c()
  age <- c()
  for (i in 1:length(pred$AgeCategories)){
    post <- c(post, caa_kt[i,])
    upper <- quantile(caa_kt[i,], 1-(alpha/2.0))
    lower <- quantile(caa_kt[i,], (alpha/2.0))
    age <- c(age, rep(pred$AgeCategories[i], length(caa_kt[i,])))
  }
  plot(age, sapply(upper, min), las=2, xlab="age", ylab="posterior catch Mfish", main=title)
  points(age, sapply(lower, min))
  
  #boxplot(post~age, las=2, xlab="age", ylab="posterior catch Mfish", main=title)
}

season_plot_test <- function(){
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