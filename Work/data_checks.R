library(Rstox)
library(grid)
library(gridExtra)
# We need to implement reports that users can use to decide on data filtration and model parameters.
# Breakdown on samples pr. area and gear, etc.
# We should have a look at current eca reports for that purpose

#get matrix of sample and landings from the subset of biotic that contains aged individuals
get_g_s_a_frame <- function(eca){
  
  agedb <- eca$biotic[!is.na(eca$biotic$age),]
  
  cols <- c("season", "gearfactor", "spatial")
  if(!all(cols %in% names(eca$covariateMatrixBiotic)) | !all(cols %in% names(eca$covariateMatrixLanding))){
    stop("Covariates season, gearfactor and landing needs to be defined for this plot.")
  }
  if (any(is.na(eca$landing$season)) | any(is.na(eca$landing$gearfactor)) | any(is.na(eca$landing$spatial)) | any(is.na(eca$biotic$season)) | any(is.na(eca$biotic$gearfactor)) | any(is.na(eca$biotic$spatial))){
    warning("NAs in covariates")  
  }
  
  totland <- aggregate(list(landed_kt=eca$landing$rundvekt/(1000*1000)), by=list(season=eca$landing$season, gearfactor=eca$landing$gearfactor, spatial=eca$landing$spatial), FUN=sum)
  totsamp <- aggregate(list(sampled_t=agedb$catchweight/1000), by=list(season=agedb$season, gearfactor=agedb$gearfactor, spatial=agedb$spatial), FUN=function(x){sum(x, na.rm=T)})
  totvessel <- aggregate(list(vessels=agedb$platform), by=list(season=agedb$season, gearfactor=agedb$gearfactor, spatial=agedb$spatial), FUN=function(x){length(unique(x))})
  tothaul <- aggregate(list(hauls=agedb$serialno), by=list(season=agedb$season, gearfactor=agedb$gearfactor, spatial=agedb$spatial), FUN=function(x){length(unique(x))})
  totaged <- aggregate(list(aged=agedb$age), by=list(season=agedb$season, gearfactor=agedb$gearfactor, spatial=agedb$spatial), FUN=function(x){sum(!is.na(x))})
  
  m <- merge(totland, totvessel, by=cols, all=T)
  m <- merge(totsamp, m, by=cols, all=T)
  m <- merge(m, tothaul, by=cols, all=T)
  m <- merge(m, totaged, by=cols, all=T)
  
  m$landed_kt[is.na(m$landed_kt)] <- rep(0, sum(is.na(m$landed_kt)))
  m$landed_fr <- m$landed_kt/sum(m$landed_kt)
  
  m[is.na(m)]<-0
  return(m)
}

#' show samples wrp common covariates gear, area and season
plot_gear_season_area <- function(eca, titletext="Prøvetaking redskap/sesong - område\nlandet (kt)\nfartøy,hal,alder", colgood="green4", colok="green2", colbarely="yellow", colbad="orange", colempty="gray", colwrong="white"){
  

  m <- get_g_s_a_frame(eca)
  m$desc <- paste(m$landed_kt, "\n", m$vessels, ", ", m$hauls, ",", m$aged, sep="")
  m$sd <- paste(m$gear, m$season, sep="/")
  
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
  names(descr) <- names(landed)
  
  t1 <- ttheme_default(core=list(
    bg_params = list(fill=col)
  ))
  
  plot.new()
  grid.table(descr, theme=t1, cols=colnames(landed), rows=rownames(landed))
  title(titletext)
  return(descr)
}

plot_cell_landings <- function(eca, xlab="Redskap/sesong/område", ylab="landet (kt)", titletext="Landinger", legendtitle="aldersprøver", colgood="green4", colok="green2", colbarely="yellow", colbad="orange", colempty="gray", colwrong="white", gooddesc="> 1 fartøy", okdesc="> 1 fangst", barelydesc="> 0 fangst", baddesc="0 prøver"){
  
  m <- get_g_s_a_frame(eca)
  mm<-mm[order(mm$landed_kt, decreasing = T),]
  mm$col <- NA

  mm[mm$landed_kt==0 & mm$vessels >0,"col"]<-colwrong
  mm[mm$landed_kt==0 & mm$vessels ==0 ,"col"]<-colempty
  mm[mm$landed_kt>0 &  mm$vessels ==0,"col"]<-colbad
  mm[mm$landed_kt>0 &  mm$vessels ==1 & mm$hauls==1,"col"]<-colbarely
  mm[mm$landed_kt>0 &  mm$vessels ==1 & mm$hauls>1,"col"]<-colok
  mm[mm$landed_kt>0 &  mm$vessels >1 &  mm$hauls>1,"col"]<-colgood
  
  barplot(mm$landed_kt, col=mm$col, xlab=xlab, ylab=ylab, main=titletext)
  legend("topright", legend=c(gooddesc, okdesc, barelydesc, baddesc), fill=c(colgood, colok, colbarely, colbad), title = legendtitle)
}

plot_cell_coverage <- function(eca, xlab="prøvetaking i celle", ylab="Andel landet i celle (vekt-%)", titletext="Dekning aldersdata celler\n(redskap/sesong/område)", colgood="green4", colok="green2", colbarely="yellow", colbad="orange", colempty="gray", colwrong="white", gooddesc="> 1 fartøy", okdesc="> 1 fangst", barelydesc="> 0 fangst", baddesc="0 prøver"){
  
  m <- get_g_s_a_frame(eca)
  mm<-mm[order(mm$landed_kt, decreasing = T),]
  mm$col <- NA
  m$desc <- NA

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
    
  tot <- aggregate(list(landed_fr=mm$landed_fr), by=list(samples=mm$col, desc=mm$desc), FUN=sum)
  tot <- tot[order(tot$landed_fr, decreasing=T),]
  
  barplot(tot$landed_fr*100, col=tot$sample, xlab=xlab, ylab=ylab, names=tot$desc, main=titletext)
}

plot_sampling_prop <- function(eca, xlab="Landet landet i celle (kt)", ylab="Fangstvekter samplet (kt)", title="Totale fangstvekter for aldersdata i celler (redskap/sesong/område)"){
  m <- get_g_s_a_frame(eca)
  mm<-mm[order(mm$landed_kt, decreasing = T),]
  plot(mm$landed_kt, mm$sampled_t/1000, xlab=xlab, ylab=ylab)
}

#' @param eca
#' @param indparameter the parameter for which data needs to be available
plot_fixed_effect_coverage <- function(eca, indparameter="age", titletext="Samples for fixed effects", okcol="green", wrongcol="white", undersampledcol="red"){
  fe <- eca$resources$covariateInfo[eca$resources$covariateInfo$covType=="Fixed", "name"]
  
  if (any(is.na(eca$biotic[,fe]) | any(is.na(eca$landing[,fe])))){
    stop("NAs for covariates")
  }
  
  a <- lapply(fe, FUN=function(x){eca$landing[[x]]})
  names(a)=fe
  aggland <- aggregate(list(landed_kt=eca$landing$rundvekt), by=a, FUN=sum)
  
  samples <- eca$biotic[!is.na(eca$biotic[[indparameter]]),]
  a <- lapply(fe, FUN=function(x){samples[[x]]})
  names(a)=fe
  aggsamp <- aggregate(list(samples=samples$serialno), by=a, FUN=function(x){length(unique(x))})
  
  agg <- merge(aggsamp, aggland, by=fe, all=T)
  agg$landed_kt[is.na(agg$landed_kt)] <- rep(0, sum(is.na(agg$landed_kt)))
  agg$samples[is.na(agg$samples)] <- rep(0, sum(is.na(agg$samples)))
  
  agg <- agg[order(agg$samples),]
  
  color <- rep(okcol, nrow(agg))
  color[agg$samples==0 & agg$landed_kt>0] <- undersampledcol
  color[agg$samples>0 & agg$landed_kt==0] <- wrongcol
  
  t1 <- ttheme_default(core=list(
    bg_params = list(fill=color)
  ))
  
  plot.new()
  grid.table(agg, theme=t1, rows=NULL)
  title(titletext)
  
}

#
#
#

plot_sampling_diagnostics <- function(eca){
  plot_gear_season_area(eca)
  par.old <- par(no.readonly = T)
  par(mfrow=c(1,2))
  plot_cell_coverage(eca)
  plot_cell_landings(eca)
  par(par.old)
}

plot_model_diagnostics <- function(eca){
  # check that all fixed effect are sampled at all levels
  plot_fixed_effect_coverage(eca)
}



#
# Test eksempler
#

runbl <- function(){
  options(java.parameters="-Xmx8g")
  # Edvin:
  dir <- "/Users/a5362/code/github/Rstox_utils/Work"
  outpath <- "/Users/a5362/code/github/Rstox_utils/Work/output"
  # Arne Johannes:
  #dir <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox_utils/Rstox_utils/Work"
  #outpath <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox_utils/output"
  #sildeprosjekt: /delphi/Felles/alle/stox/ECA/2015/ECA_sild_2015. Legg til sild == '161724' i filter (annen kode for sild'g03)
  
  projectname <- "ECA_torsk_2015"
  #projectname <- "ECA_sild_2015"
  baselineOutput <- getBaseline(projectname)
  eca <- baseline2eca(projectname)
  return(eca)
}


ex <- function(eca){
  plot_sampling_diagnostics(eca)
  plot_model_diagnostics(eca)
}
eca <- runbl()
ex(eca)