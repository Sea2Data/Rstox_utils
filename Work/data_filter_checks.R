library(Rstox)
default_blankcode="<>"

#' Composition of sample types (Fangstnivå: prøvetype) in data
#' @param catchsample
#' @param title title for plot
#' @param xlab label for x axis
#' @param blankcode code for NA / not registered
#' @param cex.names expansion factor for bar labels
plot_sample_types <- function(catchsample, title="prøvetyper", xlab="# fangstprøver", blankcode=default_blankcode, cex.names=0.8){

  #Example for use with stox baseline
  #catchsample=baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_CatchSample.txt
  #
  
  tt <- as.character(catchsample$sampletype)
  tt[is.na(tt)]<-""
  tt <- table(tt)
  tt <- sort(tt, decreasing=T)
  
  labels <- getNMDinfo("sampletype")
  labels <- labels[labels$name %in% names(tt),c("name", "shortname")]
  labels <- labels[match(labels$name, names(tt)),]
  
  names(tt)[names(tt)==""] <- blankcode
  labels[labels$name=="","name"] <- blankcode
  
  barplot(tt, xlab=xlab, names=paste(labels$shortname, " (", names(tt), ")", sep=""), horiz = T, las=1, main=title, cex.names = cex.names)
}

#' Composition of station types (Stasjons: stasjonstype) in data
#' @param catchsample
#' @param title title for plot
#' @param xlab label for x axis
#' @param blankcode code for NA / not registered
#' @param cex.names expansion factor for bar labels
plot_station_types <- function(station, title="statsjonstyper", xlab="# stasjoner", blankcode=default_blankcode, cex.names=0.8){
  #Example for use with stox baseline
  #station=baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_FishStation.txt
  #
  
  tt <- as.character(station$fishstationtype)
  tt[is.na(tt)]<-""
  tt <- table(tt)
  tt <- sort(tt, decreasing=T)
  
  labels <- getNMDinfo("fishstationtype")
  labels <- labels[labels$name %in% names(tt),c("name", "shortname")]
  labels <- labels[match(labels$name, names(tt)),]
  
  names(tt)[names(tt)==""] <- blankcode
  labels[labels$name=="","name"] <- blankcode
  
  barplot(tt, xlab=xlab, names=paste(labels$shortname, " (", names(tt), ")", sep=""), horiz = T, las=1, main=title, cex.names = cex.names)
  
}

#' Composition of taxa types (Fangstnivå: prøvetype) in data
#' @param catchsample
#' @param title title for plot
#' @param xlab label for x axis
#' @param blankcode code for NA / not registered
#' @param cex.names expansion factor for bar labels
plot_taxa_comp <- function(catchsample, title="taxa", xlab="# fangstprøver", blankcode=default_blankcode, cex.names=0.8){

  tt <- table(as.character(catchsample$noname))

  barplot(tt, xlab=xlab, names=names(tt), horiz = T, las=1, main=title, cex.names = cex.names)
    
}

#' Plots composition in catchsamples of parameters that determines what kind fraction of catches or landings are sampled
#' @param station
#' @param catchsample
#' @param title title for plot
#' @param xlab label for x axis
#' @param allname name to use for samples where all catch was sampled
#' @param landname name to use for samples where only landed fraction was sampled
#' @param retainname name to use for retained fraction of catch when samples where taken after sorting
#' @param discname name to use for discared fraction of cathc when samples where taken after sorting
#' @param blankcode code to use when fraction is not coded in data.
#' @param allcol color to use for samples where all catch was sampled
#' @param landcol
#' @param disccol
#' @param retaincol
#' @param unkwoncol color to use when sampled fraction is not coded in data.
plot_catch_fractions <- function(station, catchsample, title="Fangstfraksjoner samplet\n(platform/kvalitet/gruppe)", xlab="# fangstprøver", allname="All fangst", landname="Landet", retainname="Beholdt", discname="Ikke beholdt", allcol="red3", disccol="red", landcol="blue4", retaincol="blue", unkowncol="white", blankcode=default_blankcode){
  station <- merge(station, catchsample, by=c("cruise", "serialno"), suffixes = c(".station", ".catchsample"))
  station[is.na(station$trawlquality), "trawlquality"] <- rep(blankcode, sum(is.na(station$trawlquality)))
  station[is.na(station$group), "group"] <- rep(blankcode, sum(is.na(station$group)))
  
  counts = aggregate(list(count=station$serialno), by=list(missiontype=station$missiontype, quality=station$trawlquality, group=station$group), FUN=length)
  counts$label <- paste(counts$missiontype, counts$quality, counts$group, sep="/")
  counts$catchrep <- rep(NA, nrow(counts))
  counts$color <- rep(NA, nrow(counts))
  
  counts[counts$quality==8, "catchrep"] <- landname
  counts[counts$quality==8, "color"] <- landcol
  
  counts[counts$missiontype=="Referanseflåten-Hav" & counts$quality==7, "catchrep"] <- rep(allname, sum(counts$missiontype=="Referanseflåten-Hav" & counts$quality==7))
  counts[counts$missiontype=="Referanseflåten-Hav" & counts$quality==7, "color"] <- rep(allcol, sum(counts$missiontype=="Referanseflåten-Hav" & counts$quality==7))
  
  counts[counts$missiontype!="Referanseflåten-Hav" & counts$quality==7 & counts$group %in% c(26,27,28), "catchrep"] <- retainname
  counts[counts$missiontype!="Referanseflåten-Hav" & counts$quality==7 & counts$group %in% c(26,27,28), "color"] <- retaincol
  
  counts[counts$missiontype!="Referanseflåten-Hav" & counts$quality!=blankcode & counts$quality==7 & counts$group!=blankcode & counts$group %in% c(23,24,25), "catchrep"] <- discname
  counts[counts$missiontype!="Referanseflåten-Hav" & counts$quality!=blankcode & counts$quality==7 & counts$group!=blankcode & counts$group %in% c(23,24,25), "color"] <- disccol
  
  counts[counts$missiontype!="Referanseflåten-Hav" & counts$quality!=blankcode & counts$quality==7 & (counts$group==blankcode | !(counts$group %in% c(23,24,25,26,27,28))), "catchrep"] <- allname
  counts[counts$missiontype!="Referanseflåten-Hav" & counts$quality!=blankcode & counts$quality==7 & (counts$group==blankcode | !(counts$group %in% c(23,24,25,26,27,28))), "color"] <- allcol
  
  counts[is.na(counts$color), "catchrep"] <- rep(blankcode, sum(is.na(counts$color)))
  counts[is.na(counts$color), "color"] <- rep(unkowncol, sum(is.na(counts$color)))
  
  counts <- counts[order(counts$count, decreasing = T),]
  barplot(counts$count, names=counts$label, col=counts$color, horiz=T, las=1, xlab=xlab, main=title)
  
  leg <- unique(counts[,c("color", "catchrep")])
  legend("topright", fill=leg$color, legend=leg$catchrep)
}

#' Plots overview of potenital filtering parameters in samples
plot_sample_comp <- function(station, catchsample){
  old.par <- par(no.readonly = T)
  par(mfrow=c(2,2))
  par(mar=c(5.1,7,4.1,2.1))
  plot_station_types(station)
  par(mar=c(5.1,12,4.1,2.1))
  plot_sample_types(catchsample)
  par(mar=c(5.1,7,4.1,2.1))
  plot_taxa_comp(catchsample)
  par(mar=c(5.1,12,4.1,2.1))
  plot_catch_fractions(station, catchsample)
  par(old.par)
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
  stations <- baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_FishStation.txt
  catchsample <- baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_CatchSample.txt
  plot_sample_comp(stations, catchsample)  
}
baselineOutput <- runbl()
ex(baselineOutput)