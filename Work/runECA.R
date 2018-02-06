library(eca)
inpath <- "/Users/a5362/code/github/Rstox_utils/Work/output"
tmppath <- "/Users/a5362/code/github/Rstox_utils/Work/tmp/ECAres"
dir <- "/Users/a5362/code/github/Rstox_utils/Work"
filename <- "ECA_sild_2015.RData"
tmp <- load(file.path(inpath, filename))
print(paste("Loaded from", filename, ":", tmp))

## Add extra information in GlobalParameters
GlobalParameters$maxlength <- max(AgeLength$DataMatrix$lengthCM,na.rm=T)
GlobalParameters$caa.burnin <- 0
GlobalParameters$resultdir <- tmppath
GlobalParameters$fitfile <- "fit"
GlobalParameters$predictfile <- "predict"
GlobalParameters$minage <- 1
GlobalParameters$maxage <- 20
GlobalParameters$delta.age <- 0.001
GlobalParameters$age.error <- FALSE
GlobalParameters$lgamodel <- "log-linear"
GlobalParameters$CC <- FALSE
GlobalParameters$CCerror <- FALSE
GlobalParameters$seed <- 1234


## Select covariates - not use haulweight and boat now
col <- 1:4
newAgeLength <- AgeLength
newAgeLength$CovariateMatrix <- AgeLength$CovariateMatrix[,col]
newAgeLength$info <- AgeLength$info[col,]

newWeightLength <- WeightLength
newWeightLength$CovariateMatrix <- WeightLength$CovariateMatrix[,col]
newWeightLength$info <- WeightLength$info[col,]


## Estimate model
fit <- eca.estimate(newAgeLength,newWeightLength,Landings,GlobalParameters)

## Predict
## Install new library
pred <- eca.predict(newAgeLength,newWeightLength,Landings,GlobalParameters)

sort.mat<-function(DataMatrix){
  DataMatrix<-DataMatrix
  DataMatrix$age<-as.integer(DataMatrix$age)
  DataMatrix$otolithtype<-as.integer(DataMatrix$otolithtype)
  haul<-DataMatrix$samplingID
  no.age<-is.na(DataMatrix$age)
  order<-order(haul,no.age)
  DataMatrix<-DataMatrix[order,]
  haul<-haul[order]
  tab<-table(haul,DataMatrix$age,exclude=NULL)
  nc<-ncol(tab)
  num.noAge<-tab[-nrow(tab),nc]
  nFishBoat<-table(haul)
  haulstart<-match(unique(haul),haul)
  start.noAge<-haulstart+nFishBoat-num.noAge-1
  start.noAge[num.noAge==0]<-0
  list(n.col.data=as.integer(ncol(DataMatrix)),DataMatrix=DataMatrix,num.noAge=as.integer(num.noAge),
       nFishBoat=as.integer(nFishBoat),start.noAge=as.integer(start.noAge))
}

source(file.path(dir, "plot.R"))
plot_pred_box(pred)