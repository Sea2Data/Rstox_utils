#
# Functions for reading model configuration from old eca (v.3.?)
# These were internal to old eca, and not fully documented. Refer to email from Hanne and implement checks on assumptions made
#

#' Check that assumptions on fit.params are met
#' @keywords internal
check <- function(fp){
  if (!all(fp$pars$minage:fp$pars$maxage == fp$age.vec)){
    stop("age.vec does not match parameters in pars")
  }
  
  warning("Implement checks")
  #check that Int is identical for agemodel and lgamodel
  
  #check that Slp only exists if correpsond Int exists
  
  #check that covariate levels are the same for wgl and (wglindex) and agl (index)
  
}

#'
#' @keywords internal
get_covariates <- function(model){
  covs <- names(model$Int[unlist(model$Int)])
  return(covs[!(covs %in% c("cell", "haul"))])
}

#' @keywords internal
get_info_skeleton <- function(covs){
  covs <- c("constant", covs)
  dimnames <- list()
  dimnames[[1]] <- covs 
  dimnames[[2]] <- c("random", "CAR", "continuous", "in.landings", "nlev", "interaction", "in.slopeModel")
  info <- matrix(nrow=length(covs), ncol=7)
  dimnames(info) <- dimnames
  info[,]<-0
  return(info)
}

#' @keywords internal
load_info_oldECA <- function(fit.params, model){
  covs <- get_covariates(fit.params$fit.input[[model]])
  info <- get_info_skeleton(covs)
  info["constant",] <- c(0,0,0,1,1,0,1)

  if (fit.params$fit.input$rand.seas){
    info["seas","random"] <- 1
  }
  if (fit.params$fit.input$rand.gear){
    info["gear","random"] <- 1
  }
  if (!is.null(info["boat",])){
    info["boat","random"] <- 1
    warning("Not setting nlev for boat")
    info["boat","nlev"]<-NA
  }
  
  
  if (fit.params$fit.input$sim.ar){
    info["area","CAR"] <- 1
  }
  
  info["seas","in.landings"] <- 1
  info["area","in.landings"] <- 1
  info["gear","in.landings"] <- 1
  
  if (fit.params$fit.input[[model]]$Int$cell){
    info["seas", "interaction"]<-1
    info["area", "interaction"]<-1
    info["gear", "interaction"]<-1
  }
  
  if (!is.null(info["gear",])){
    info["gear", "nlev"] <- max(fit.params$covtabs$Gear$index)
  }
  if (!is.null(info["area",])){
    info["area", "nlev"] <- max(fit.params$covtabs$Area$index)
  }
  if (!is.null(info["seas",])){
    info["seas", "nlev"] <- max(fit.params$covtabs$Season$index)
  }
  if (!is.null(info["year",])){
    info["year", "nlev"] <- max(fit.params$covtabs$Year$index)
  }
  
  
  return(info)
}

#' @keywords internal
load_globalParameters_oldECA <- function(fit.params){
  
  GlobalParameters <- list()
  GlobalParameters$resultdir  <- fit.params$fit.input$rootfolder
  GlobalParameters$maxlength <- fit.params$maxlength
  GlobalParameters$minage <- fit.params$pars$minage
  GlobalParameters$maxage <- fit.params$pars$maxage
  GlobalParameters$delta.age <- fit.params$fit.input$delta.age
  GlobalParameters$burnin <- fit.params$fit.input$burnin
  if (!is.null(fit.params$predict.input)){
    GlobalParameters$caa.burnin <- fit.params$predict.input$burnin    
  }
  else{
    GlobalParameters$caa.burnin <- NULL
  }
  GlobalParameters$nSamples <- fit.params$fit.input$nsamples
  GlobalParameters$thin <- fit.params$fit.input$thin
  GlobalParameters$fitfile <- fit.params$fit.input$filename
  if (!is.null(fit.params$predict.input)){
    GlobalParameters$predictfile <- paste(fit.params$predict.input$fit.filename, fit.params$predict.input$predict.filename, sep="")
  }
  else{
    GlobalParameters$predictfile <- NULL
  }
  GlobalParameters$lgamodel <- fit.params$fit.input$lgarel
  GlobalParameters$CC <- fit.params$fit.input$cc
  GlobalParameters$CCerror <- !is.null(fit.params$fit.input$CCerror)
  GlobalParameters$age.error <- !is.null(fit.params$fit.input$A2A)
  
  return(GlobalParameters) 
}

#' Loads parameter file described in the config. Note that files with suffix .param is written for both fit and predict
#' But the former does not contain parameters for predict
#' If parameters for predict are not available, these are set to NULL
load_old_config_fit <- function(file){
  load(file)
  #fit.params$pars read from parameter configuration file, it seems. 
  check(fit.params)
  ageLenghtInfo <- load_info_oldECA(fit.params, "lgamodel")
  weightLenghtInfo <- load_info_oldECA(fit.params, "wglmodel")
  
  GlobalParameters <- load_globalParameters_oldECA(fit.params)

  print("AgeLength")
  print(ageLenghtInfo)
  print("WeightLength")
  print(weightLenghtInfo)
  print("GlobalParameters")
  print(GlobalParameters)
  print("Grouping")
  print(fit.params$covtabs)
}

load_old_config_fit("~/hi_sync/bunnfisk/nssk_2019/North_Sea/ECA/Analysis/predict/HYSE2018_ns_v00.fit.2018_allgears_27fire_season1234.params")