library(Rstox)
default_blankcode="<>"


#
# Funksjoner for Landings
#

#' Partitioning of produkt types where live weight is missing, colored by main product and by-product
#' @param fisk
#' @param mainprod label for main product categories
#' @param biprod label for by-poduct categories
#' @param maincol color for main-product categories
#' @param bicol color for by-product categories
#' @param has_liveweight text to use when all lines have live-weight
#' @param title title for plot
#' @param xlab label for x-axis log number of sales-note-lines
plot_missing <- function(fisk, mainprod="hovedprodukt", biprod="biprodukt", maincol="red", bicol="blue", has_liveweight="m/rundvekt", title="Produkttilstander \n mangler rundvekt", xlab="log # seddellinjer"){
  missing_rv <- fisk[is.na(fisk$rundvekt),]
  t <- table(missing_rv$tilstand)
  
  if (length(t)==0){
    pie(c(nrow(fisk), nrow(missing_rv)), label=c(has_liveweight, ""), main=has_liveweight)
  }
  else{
    t <- sort(t, decreasing=T)
    col<-rep(biprod, length(t))
    col[names(t) %in% c(211, 110, 210, 100, 622, 512, 311, 212, 313, 710, 830, 645, 215, 511, 510, 513, 412)]<-maincol
    
    barplot(log(t), names=names(t), col=col, horiz=T, xlab=xlab, cex.name=cex.name, main=title, las=1, main=title)
    legend("topright", fill=c(maincol, bicol), legend=c(mainprod, biprod))
  }
}

#' Partitioning of species codes
#' @param fisk
#' @param xlab label for x-axis (weight in kt)
#' @param title tite for plot
#' @param cex.names expansion factor for labels
plot_speccat <- function(fisk, xlab="vekt kt", title="Artskoder", cex.names=0.8){
  
  w <- aggregate(list(weight=fisk$rundvekt/(1000*1000)), by=list(speccat=fisk$fisk), FUN=function(x){sum(x, na.rm=T)})
  
  if (nrow(w)>1){
    barplot(w$weight, xlab=xlab, names=w$speccat, horiz = T, las=1, main=title, cex.names = cex.names)  
  }
  else{
    pie(w$weight, labels=w$speccat, main=title)
  }
}

#' Partitioning of nationality for fishing vessel
#' @param seddel
#' @param xlab label for x-axis (weight in kt)
#' @param title tite for plot
#' @param cex.names expansion factor for labels
plot_fart_land <- function(seddel, xlab="# sluttsedler", title="Fartøy nasjonalitet"){
  tt <- table(seddel$fartland)
  
  if (length(tt)>1){
    barplot(tt, names=names(T), xlab=xlab, horiz=T, main=title, las=1)
  }
  else{
    pie(t, main=title)
  }
}

#' Partitioning of nationality for landing site
#' @param seddel
#' @param xlab label for x-axis (weight in kt)
#' @param title tite for plot
#' @param cex.names expansion factor for labels
plot_mottak_land <- function(seddel, xlab="# sluttsedler", title="Mottak nasjonalitet"){
  tt <- table(seddel$kjopland)
  
  if (length(tt)>1){
    barplot(tt, names=names(T), xlab=xlab, horiz=T, main=title, las=1)
  }
  else{
    pie(t, main=title)
  }
}

#' Partitioning of main ecosoze for catch
#' @param seddel
#' @param xlab label for x-axis (weight in kt)
#' @param title tite for plot
#' @param cex.names expansion factor for labels
plot_ecozone <- function(seddel, xlab="# sluttsedler", title="Økonomisk sone"){
  tt <- table(seddel$fangstsone)
  
  if (length(tt)>1){
    barplot(tt, names=names(T), xlab=xlab, horiz=T, main=title, las=1)
  }
  else{
    pie(t, main=title)
  }
}

#' Partitioning on gears. Colored by aquacultre and harvest
#' @param seddel
#' @param xlab
#' @param title
#' @param aquacol color used for aquaculture gearcodes
#' @param harvestcol color used for harvest gears
#' @param aquaname name used for aquaculture label
#' @param harvestname name used for harvest label
#' @param cex.name expansion factor for labels
plot_gear <- function(seddel, xlab="# sluttsedler", title="Redskaper", aquacol="red", harvestcol="blue", aquaname="oppdrett", harvestname="fangst", cex.name=0.4){
  t <- table(seddel$redskap)
  t <- sort(t, decreasing=T)
  col<-rep(harvestcol, length(t))
  col[names(t)==90]<-harvestcol
  
  barplot(t, names=names(t), col=col, horiz=T, xlab=xlab, cex.name=cex.name, main=title, las=1)
  legend("topright", fill=c(harvestcol, aquacol), legend=c(harvestname, aquaname))
}

plot_quoata <- function(seddel, xlab="# sluttsedler", title="Kvotetyper", quota_special="red", quota_normal="blue", quota_special_name="ikke.komm.", quota_normal_name="komm. kvote", cex.name=0.8){
  t <- table(seddel$kvotetype)
  t <- sort(t, decreasing=T)
  col<-rep(quota_normal, length(t))
  col[names(t)==6 | names(t)==2 | names(t)==3] <- quota_special
  
  barplot(t, names=names(t), col=col, horiz=T, xlab=xlab, cex.name=cex.name, main=title, las=1)
  legend("topright", fill=c(quota_normal, quota_special), legend=c(quota_normal_name, quota_special_name))
}

plot_overwiew <- function(fisk, seddel){
  par.old <- par(no.readonly = T)
  par(mfrow=c(2,2))
  plot_speccat(fisk)
  plot_fart_land(seddel)
  plot_mottak_land(seddel)
  plot_ecozone(seddel)
  plot_gear(seddel)
  plot_quoata(seddel)
  plot_missing(fisk)
  par(par.old)
}

#
# Test-eksempler
#

runbl <- function(){
  options(java.parameters="-Xmx6g")
  # Edvin:
  dir <- "/Users/a5362/code/github/Rstox_utils/Work"
  outpath <- "/Users/a5362/code/github/Rstox_utils/Work/output"
  # Arne Johannes:
  #dir <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox_utils/Rstox_utils/Work"
  #outpath <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox_utils/output"
  #sildeprosjekt: /delphi/Felles/alle/stox/ECA/2015/ECA_sild_2015. Legg til sild == '161724' i filter (annen kode for sild'g03)
  
  #projectname <- "ECA_torsk_2015"
  projectname <- "ECA_sild_2015"
  return(getBaseline(projectname))
  
}

ex <- function(baselineOutput){
  fisk <- baselineOutput$outputData$FilterLanding$FilterLanding_LandingData_FiskeLinje.txt
  seddel <- baselineOutput$outputData$FilterLanding$FilterLanding_LandingData_SluttSeddel.txt
  plot_overwiew(fisk, seddel)
}
baselineOutput <- runbl()
ex(baselineOutput)