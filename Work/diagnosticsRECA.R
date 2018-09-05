library(Rstox)
library(plotrix)
srcdir <- "/Users/a5362/code/github/Rstox_utils/Work"
source(file.path(srcdir, "plotWrapper.R"))

#get matrix of sample and landings from the subset of biotic that contains aged individuals
get_g_s_a_frame <- function(eca){
  
  agedb <- eca$biotic[!is.na(eca$biotic$age),]
  
  cols <- c("temporal", "gearfactor", "spatial")
  if(!all(cols %in% names(eca$covariateMatrixBiotic)) | !all(cols %in% names(eca$covariateMatrixLanding))){
    stop("Covariates temporal, gearfactor and spatial needs to be defined for this plot.")
  }
  if (any(is.na(eca$landing$temporal)) | any(is.na(eca$landing$gearfactor)) | any(is.na(eca$landing$spatial)) | any(is.na(eca$biotic$temporal)) | any(is.na(eca$biotic$gearfactor)) | any(is.na(eca$biotic$spatial))){
    warning("NAs in covariates")  
  }

  totland <- aggregate(list(landed_kt=eca$landing$rundvekt/(1000*1000)), by=list(temporal=eca$landing$temporal, gearfactor=eca$landing$gearfactor, spatial=eca$landing$spatial), FUN=sum)
  totsamp <- aggregate(list(sampled_t=agedb$catchweight/1000), by=list(temporal=agedb$temporal, gearfactor=agedb$gearfactor, spatial=agedb$spatial), FUN=function(x){sum(x, na.rm=T)})
  totvessel <- aggregate(list(vessels=agedb$platform), by=list(temporal=agedb$temporal, gearfactor=agedb$gearfactor, spatial=agedb$spatial), FUN=function(x){length(unique(x))})
  tothaul <- aggregate(list(hauls=agedb$serialno), by=list(temporal=agedb$temporal, gearfactor=agedb$gearfactor, spatial=agedb$spatial), FUN=function(x){length(unique(x))})
  totaged <- aggregate(list(aged=agedb$age), by=list(temporal=agedb$temporal, gearfactor=agedb$gearfactor, spatial=agedb$spatial), FUN=function(x){sum(!is.na(x))})
  
  m <- merge(totland, totvessel, by=cols, all=T)
  m <- merge(totsamp, m, by=cols, all=T)
  m <- merge(m, tothaul, by=cols, all=T)
  m <- merge(m, totaged, by=cols, all=T)
  
  m$landed_kt[is.na(m$landed_kt)] <- rep(0, sum(is.na(m$landed_kt)))
  m$landed_fr <- m$landed_kt/sum(m$landed_kt)
  
  m[is.na(m)]<-0
  return(m)
}

#' show samples wrp common covariates gear, area and temporal
plot_gear_temporal_area <- function(eca, titletext="gear/temporal - area\nlanded (kt)\nage samples: #vessels,#catches,#individuals", colgood="green4", colok="green2", colbarely="yellow", colbad="orange", colempty="gray", colwrong="white"){
  
  m <- get_g_s_a_frame(eca)
  m$desc <- paste(m$landed_kt, "\n", m$vessels, ", ", m$hauls, ",", m$aged, sep="")
  m$sd <- paste(m$gear, m$temporal, sep="/")
  
  landed <- xtabs(m$landed_kt~m$sd+m$spatial)
  landed[landed<.001]<-0
  vessels <- xtabs(m$vessels~m$sd+m$spatial)
  hauls <- xtabs(m$hauls~m$sd+m$spatial)
  aged <- xtabs(m$aged~m$sd+m$spatial)
  
  col <- landed*NA
  col[landed==0 & vessels>0 ]<-colwrong
  col[landed==0 & vessels==0 ]<-colempty
  col[landed>0 & vessels==0]<-colbad
  col[landed>0 & vessels==1 & hauls==1]<-colbarely
  col[landed>0 & vessels==1 & hauls>1]<-colok
  col[landed>0 & vessels>1 & hauls>1]<-colgood
  
  descr <- `dim<-`(sprintf("%.0f\n%d,%d,%d", landed, vessels, hauls, aged), dim(landed))
  descr <- descr[rowSums(landed)>0 | rowSums(aged)>0,colSums(landed)>0 | colSums(aged)>0]
  col <- col[rowSums(landed)>0 | rowSums(aged)>0,colSums(landed)>0 | colSums(aged)>0]
  landed <- landed[rowSums(landed)>0 | rowSums(aged)>0,colSums(landed)>0 | colSums(aged)>0]
  colnames(descr) <- colnames(landed)
  rownames(descr) <- rownames(landed)
  
  #deal with sizing and such when output device is clear
  #calculate plot size
  plot(1:100, axes = FALSE, xlab = "", ylab = "", type = "n", main=titletext)
  addtable2plot(x = "topleft", table = descr,
                bty = "o", display.rownames = TRUE, display.colnames = TRUE,
                hlines = TRUE, vlines = TRUE,
                bg = col,
                xjust = 2, yjust = 1, cex=0.5)
  
  return(descr)
}

plot_cell_landings <- function(eca, xlab="Cells (gear/temp/spatial)", ylab="landed (kt)", frac=0.001, titletext=paste("top", 100-frac*100, "weigth-% cells"), legendtitle="sample clustering", colgood="green4", colok="green2", colbarely="yellow", colbad="orange", colempty="gray", colwrong="white", gooddesc="> 1 vessel", okdesc="> 1 catch", barelydesc="> 0 catch", baddesc="0 samples"){
  
  mm <- get_g_s_a_frame(eca)
  mm<-mm[order(mm$landed_kt, decreasing = T),]
  mm<-mm[mm$landed_kt/sum(mm$landed_kt)>frac,]
  mm$col <- NA
  
  mm[mm$landed_kt==0 & mm$vessels >0,"col"]<-colwrong
  mm[mm$landed_kt==0 & mm$vessels ==0 ,"col"]<-colempty
  mm[mm$landed_kt>0 &  mm$vessels ==0,"col"]<-colbad
  mm[mm$landed_kt>0 &  mm$vessels ==1 & mm$hauls==1,"col"]<-colbarely
  mm[mm$landed_kt>0 &  mm$vessels ==1 & mm$hauls>1,"col"]<-colok
  mm[mm$landed_kt>0 &  mm$vessels >1 &  mm$hauls>1,"col"]<-colgood
  
  barplot(mm$landed_kt, col=mm$col, xlab=xlab, ylab=ylab, main=titletext, border = NA)
  legend("topright", legend=c(gooddesc, okdesc, barelydesc, baddesc), fill=c(colgood, colok, colbarely, colbad), title = legendtitle)
}

plot_cell_coverage <- function(eca, xlab="sample clustering", ylab="Fraction landed (vekt-%)", titletext="Coverage w age\ncells (gear/temp/spatial)", colgood="green4", colok="green2", colbarely="yellow", colbad="orange", colempty="gray", colwrong="white", gooddesc="> 1 vessel", okdesc="> 1 catch", barelydesc="> 0 catch", baddesc="0 samples"){
  
  mm <- get_g_s_a_frame(eca)
  mm<-mm[order(mm$landed_kt, decreasing = T),]
  mm$col <- NA
  mm$desc <- NA
  
  mm[mm$landed_kt==0 & mm$vessels >0,"col"]<-colwrong
  mm[mm$landed_kt==0 & mm$vessels >0,"descr"]<-""
  mm[mm$landed_kt==0 & mm$vessels ==0 ,"col"]<-colempty
  mm[mm$landed_kt==0 & mm$vessels ==0 ,"descr"]<-""
  mm[mm$landed_kt>0 &  mm$vessels ==0,"col"]<-colbad
  mm[mm$landed_kt>0 &  mm$vessels ==0,"descr"]<-baddesc
  mm[mm$landed_kt>0 &  mm$vessels ==1 & mm$hauls==1,"col"]<-colbarely
  mm[mm$landed_kt>0 &  mm$vessels ==1 & mm$hauls==1,"descr"]<-barelydesc
  mm[mm$landed_kt>0 &  mm$vessels ==1 & mm$hauls>1,"col"]<-colok
  mm[mm$landed_kt>0 &  mm$vessels ==1 & mm$hauls>1,"descr"]<-okdesc
  mm[mm$landed_kt>0 &  mm$vessels >1 &  mm$hauls>1,"col"]<-colgood
  mm[mm$landed_kt>0 &  mm$vessels >1 &  mm$hauls>1,"descr"]<-gooddesc
  
  rankdesc <- function(desc){
    if (desc==gooddesc){
      return(5)
    }
    if (desc==okdesc){
      return(4)
    }
    if (desc==barelydesc){
      return(3)
    }
    if (desc==baddesc){
      return(2)
    }
    if (desc==""){
      return(1)
    }
    else{
      stop()
    }
  }
  rankdesc <- Vectorize(rankdesc)
  
  tot <- aggregate(list(landed_fr=mm$landed_fr), by=list(samples=mm$col, desc=mm$descr), FUN=sum)
  tot <- tot[order(rankdesc(tot$desc)),]
  
  barplot(tot$landed_fr*100, col=tot$sample, xlab=xlab, ylab=ylab, names=tot$desc, main=titletext)
}

plot_sampling_prop <- function(eca, xlab="Landet landet i celle (kt)", ylab="Fangstvekter samplet (kt)", title="Totale fangstvekter for aldersdata i celler (redskap/sesong/område)"){
  m <- get_g_s_a_frame(eca)
  mm<-mm[order(mm$landed_kt, decreasing = T),]
  plot(mm$landed_kt, mm$sampled_t/1000, xlab=xlab, ylab=ylab)
}

#' Get aggregated landings for fixed effects
get_fixed_effects_landings <- function(stoxexport){
  fixed_effects <- stoxexport$resources$covariateInfo[stoxexport$resources$covariateInfo$covType=="Fixed", "name"]
  landedcol <- lapply(fixed_effects, FUN=function(x){stoxexport$landing[[x]]})
  names(landedcol)=fixed_effects
  print(fixed_effects)
  aggland <- aggregate(list(landed_kt=stoxexport$landing$rundvekt), by=landedcol, FUN=sum)
  return(aggland)
}

#' @param eca
#' @param indparameter the parameters for which data needs to be available
plot_fixed_effect_coverage <- function(stoxexport, indparameters=c("age"), titletext="Samples for fixed effects", okcol="green", wrongcol="white", undersampledcol="red"){
  fixed_effects <- stoxexport$resources$covariateInfo[stoxexport$resources$covariateInfo$covType=="Fixed", "name"]
  if (any(is.na(stoxexport$biotic[,fixed_effects]) | any(is.na(stoxexport$landing[,fixed_effects])))){
    stop("NAs for covariates")
  }
  
  aggland <- get_fixed_effects_landings(stoxexport)
  
  #discard samples without target parameters (indparameters)
  samples <- stoxexport$biotic
  for (p in indparameters){
    samples <- samples[!is.na(samples[[p]]),]    
  }
  
  biocol <- lapply(fixed_effects, FUN=function(x){samples[[x]]})
  names(biocol)=fixed_effects
  aggsamp <- aggregate(list(catchsamples=samples$serialno), by=biocol, FUN=function(x){length(unique(x))})
  
  print(fixed_effects)
  print(names(aggland))
  print(names(aggsamp))
  
  agg <- merge(aggsamp, aggland, by=fixed_effects, all=T)
  agg$landed_kt[is.na(agg$landed_kt)] <- rep(0, sum(is.na(agg$landed_kt)))
  agg$catchsamples[is.na(agg$catchsamples)] <- rep(0, sum(is.na(agg$catchsamples)))
  
  agg <- agg[order(agg$catchsamples),]
  
  color <- rep(okcol, nrow(agg))
  color[agg$catchsamples==0 & agg$landed_kt>0] <- undersampledcol
  color[agg$catchsamples>0 & agg$landed_kt==0] <- wrongcol
  
  plot.new()
  addtable2plot(x = "topleft", table = agg,
                bty = "o", display.rownames = FALSE,
                hlines = TRUE, vlines = TRUE,
                bg = color,
                xjust = 2, yjust = 1)
  title(titletext)
}

#
#
#

plot_sampling_diagnostics <- function(eca){
  par.old <- par(no.readonly = T)
  plot_gear_temporal_area(eca)
  par(mfrow=c(2,1))
  plot_cell_coverage(eca)
  plot_cell_landings(eca)
  par(par.old)
  par(mfrow=c(1,1))
}

plot_model_diagnostics <- function(eca){
  # check that all fixed effect are sampled at all levels
  plot_fixed_effect_coverage(eca, indparameters=c("age", "length"), titletext = "Age samples for fixed effects")
  plot_fixed_effect_coverage(eca, indparameters=c("weight", "length"), titletext = "Weight samples for fixed effects")
}

diagnosticsSamplesRECA <- function(){}
diagnosticsCoverageRECA <- function(){}

#'Plots diagnostics for model configuration. Whether all combinations of fixed effects are sampled
diagnostics_model_configuration <- function(stoxexport){
  par.old <- par(no.readonly = T)
  par(mfrow=c(1,2))
  plot_fixed_effect_coverage(stoxexport, indparameters=c("age", "length"), titletext = "Age samples for fixed effects")
  plot_fixed_effect_coverage(stoxexport, indparameters=c("weight", "length"), titletext = "Weight samples for fixed effects")
  par(par.old) 
}

#' @param projectname name of stox project
#' @param verbose logical, if TRUE info is written to stderr()
#' @param format function defining filtetype for plots, supports grDevices::pdf, grDevices::png, grDevices::jpeg, grDevices::tiff, grDevices::bmp
#' @param ... parameters passed on plot function and format
diagnosticsRECA <- function(projectName, verbose=T, format="png", ...){
  prep <- loadProjectData(projectName, var="prepareRECA")
  stoxexp <- prep$prepareRECA$StoxExpor
  
  #for testing different configs
  #stoxexp$resources$covariateInfo[2,"covType"] <- "Fixed"
  #stoxexp$resources$covariateInfo[3,"covType"] <- "Fixed"
  print(stoxexp$resources$covariateInfo)
  
  #calculate plot dimensions for table
  rows = nrow(get_fixed_effects_landings(stoxexp))
  cols = ncol(get_fixed_effects_landings(stoxexp))+2
  
  if (format=="png"){
    #dimension in pixels
    res=500
    width=(res/1.5)*(cols+2)*2
    height=(res/4.9)*(rows+7)
  }
  if (format=="pdf"){
    #dimension in inches
    width=(cols+2)*2/1.3
    height=(rows+7)/5
    res=NULL
  }
  
  formatPlot(projectname, "RECA_model_configuration", function(){diagnostics_model_configuration(stoxexp, ...)}, verbose=verbose, format=format, height=height, width=width, res=res, ...)
  
}
projectname <- "ECA_torsk_2015"
diagnosticsRECA(projectname)