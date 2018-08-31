library(Rstox)
library(plotrix)
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
plot_gear_temporal_area <- function(eca, titletext="Samples gear/temporal - area\nlanded (kt)\nvessels,hauls,aged", colgood="green4", colok="green2", colbarely="yellow", colbad="orange", colempty="gray", colwrong="white"){
  
  
  m <- get_g_s_a_frame(eca)
  m$desc <- paste(m$landed_kt, "\n", m$vessels, ", ", m$hauls, ",", m$aged, sep="")
  m$sd <- paste(m$gear, m$temporal, sep="/")
  
  landed <- xtabs(m$landed_kt~m$sd+m$spatial)
  landed[landed<1]<-0
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
  plot(1:100, axes = FALSE, xlab = "", ylab = "", type = "n", main=titletext)
  addtable2plot(x = "topleft", table = descr,
                bty = "o", display.rownames = TRUE, display.colnames = TRUE,
                hlines = TRUE, vlines = TRUE,
                bg = col,
                xjust = 2, yjust = 1, cex=0.5)
  
  return(descr)
}

plot_cell_landings <- function(eca, xlab="Redskap/sesong/område", ylab="landet (kt)", titletext="Landinger", legendtitle="aldersprøver", colgood="green4", colok="green2", colbarely="yellow", colbad="orange", colempty="gray", colwrong="white", gooddesc="> 1 fartøy", okdesc="> 1 fangst", barelydesc="> 0 fangst", baddesc="0 prøver"){
  
  mm <- get_g_s_a_frame(eca)
  mm<-mm[order(mm$landed_kt, decreasing = T),]
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

plot_cell_coverage <- function(eca, xlab="prøvetaking i celle", ylab="Andel landet i celle (vekt-%)", titletext="Dekning aldersdata celler\n(redskap/sesong/område)", colgood="green4", colok="green2", colbarely="yellow", colbad="orange", colempty="gray", colwrong="white", gooddesc="> 1 fartøy", okdesc="> 1 fangst", barelydesc="> 0 fangst", baddesc="0 prøver"){
  
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
  
  tot <- aggregate(list(landed_fr=mm$landed_fr), by=list(samples=mm$col, desc=mm$descr), FUN=sum)
  tot <- tot[order(tot$landed_fr, decreasing=T),]
  
  barplot(tot$landed_fr*100, col=tot$sample, xlab=xlab, ylab=ylab, names=tot$desc, main=titletext)
}

plot_sampling_prop <- function(eca, xlab="Landet landet i celle (kt)", ylab="Fangstvekter samplet (kt)", title="Totale fangstvekter for aldersdata i celler (redskap/sesong/område)"){
  m <- get_g_s_a_frame(eca)
  mm<-mm[order(mm$landed_kt, decreasing = T),]
  plot(mm$landed_kt, mm$sampled_t/1000, xlab=xlab, ylab=ylab)
}

#' @param eca
#' @param indparameter the parameters for which data needs to be available
plot_fixed_effect_coverage <- function(eca, indparameters=c("age"), titletext="Samples for fixed effects", okcol="green", wrongcol="white", undersampledcol="red"){
  fe <- eca$resources$covariateInfo[eca$resources$covariateInfo$covType=="Fixed", "name"]
  
  if (any(is.na(eca$biotic[,fe]) | any(is.na(eca$landing[,fe])))){
    stop("NAs for covariates")
  }
  
  a <- lapply(fe, FUN=function(x){eca$landing[[x]]})
  names(a)=fe
  aggland <- aggregate(list(landed_kt=eca$landing$rundvekt), by=a, FUN=sum)
  
  samples <- eca$biotic
  for (p in indparameters){
    samples <- samples[!is.na(samples[[p]]),]    
  }
  
  
  a <- lapply(fe, FUN=function(x){samples[[x]]})
  names(a)=fe
  aggsamp <- aggregate(list(catchsamples=samples$serialno), by=a, FUN=function(x){length(unique(x))})
  
  agg <- merge(aggsamp, aggland, by=fe, all=T)
  agg$landed_kt[is.na(agg$landed_kt)] <- rep(0, sum(is.na(agg$landed_kt)))
  agg$catchsamples[is.na(agg$catchsamples)] <- rep(0, sum(is.na(agg$catchsamples)))
  
  agg <- agg[order(agg$catchsamples),]
  
  color <- rep(okcol, nrow(agg))
  color[agg$catchsamples==0 & agg$landed_kt>0] <- undersampledcol
  color[agg$catchsamples>0 & agg$landed_kt==0] <- wrongcol
  
  plot(1:10, axes = FALSE, xlab = "", ylab = "", type = "n", main=titletext)
  addtable2plot(x = "topleft", table = agg,
                bty = "o", display.rownames = FALSE,
                hlines = TRUE, vlines = TRUE,
                bg = color,
                xjust = 2, yjust = 1)
}

#
#
#

plot_sampling_diagnostics <- function(eca){
  plot_gear_temporal_area(eca)
  par.old <- par(no.readonly = T)
  par(mfrow=c(1,2))
  plot_cell_coverage(eca)
  plot_cell_landings(eca)
  par(par.old)
}

plot_model_diagnostics <- function(eca){
  # check that all fixed effect are sampled at all levels
  plot_fixed_effect_coverage(eca, indparameters=c("age", "length"), titletext = "Age samples for fixed effects")
  plot_fixed_effect_coverage(eca, indparameters=c("weight", "length"), titletext = "Weight samples for fixed effects")
}


diagnosticsRECA <- function(projectname){
  eca <- baseline2eca(projectname)  
  plot_sampling_diagnostics(eca)
  plot_model_diagnostics(eca)
}
projectname <- "ECA_torsk_2015"
diagnosticsRECA(projectname)