library(Rstox)
library(MASS)
library(RColorBrewer)
default_blankcode="<>"

#
# Diverse plot for å vise prøveheterogenitet og enkle feilsjekker som bør håndteres med datafiltrering, datakorreksjon eller datakonvertering.
#

#' Composition of length measurements for samples
#' @param catchsample
#' @param title title for plot
#' @param xlab label for x axis
#' @param blankcode code for NA / not registered
#' @param cex.names expansion factor for bar labels
plot_length_measurements <- function(catchsample, title="lengdemål", xlab="# fangstprøver", blankcode=default_blankcode, cex.names=0.8){
  tt <- as.character(catchsample$lengthmeasurement)
  tt[is.na(tt)]<-""
  tt <- table(tt)
  tt <- sort(tt, decreasing=T)
  
  labels <- getNMDinfo("Lengthmeasurementtype")
  labels <- labels[labels$name %in% names(tt),c("name", "description")]
  labels <- labels[match(labels$name, names(tt)),]
  
  names(tt)[names(tt)==""] <- blankcode
  labels[labels$name=="","name"] <- blankcode
  
  if (length(tt)>1){
    barplot(tt, xlab=xlab, names=paste(labels$description, " (", names(tt), ")", sep=""), horiz = T, las=1, main=title, cex.names = cex.names)  
  }
  else{
    pie(tt, labels=paste(labels$description, " (", names(tt), ")", sep=""))  
  }
}

#' Composition of product types samples
plot_product_types <- function(catchsample, title="produkttyper", xlab="# fangstprøver", blankcode=default_blankcode, cex.names=0.8){
  tt <- as.character(catchsample$samplemeasurement)
  tt[is.na(tt)]<-""
  tt <- table(tt)
  tt <- sort(tt, decreasing=T)
  
  labels <- getNMDinfo("Vektmetode")
  labels <- labels[labels$name %in% names(tt),c("name", "description")]
  labels <- labels[match(labels$name, names(tt)),]
  
  names(tt)[names(tt)==""] <- blankcode
  labels[labels$name=="","name"] <- blankcode
  
  if (length(tt)>1){
    barplot(tt, xlab=xlab, names=paste(labels$description, " (", names(tt), ")", sep=""), horiz = T, las=1, main=title, cex.names = cex.names)  
  }
  else{
    pie(tt, labels=paste(labels$description, " (", names(tt), ")", sep=""))    
  }
}

#' Plots partition of catchsamples which have individals, ages or only total catch sampled
#' @param catchsample
#' @param individual
#' @param has_a_name label for bar of catchsamples with age read individuals
#' @param has_i_name label for bar of catchsamples with individuals sampled, but no age read
#' @param notname label for bar of catchsamples with no individuals sampled
#' @param ylab label for y axis
#' @param title title for plot
plot_individuals <- function(catchsample, individual, has_a_name="alder", has_i_name="ind. u alder", notname="kun fangst", ylab="# fangstprøver", title="Individprøvetaking"){
  ages <- individual[!is.na(individual$age),]
  has_i <- merge(catchsample, unique(individual[,c("species", "serialno", "samplenumber")]), by=c("species", "serialno", "samplenumber"))
  has_a <- merge(catchsample, unique(ages[,c("species", "serialno", "samplenumber")]), by=c("species", "serialno", "samplenumber"))
  n_not_i <- nrow(catchsample) - nrow(has_i)
  n_i_not_a <- nrow(has_i) - nrow(has_a)
  n_a <- nrow(has_a)
  barplot(c(n_a, n_i_not_a, n_not_i), names=c(has_a_name, has_i_name, notname), ylab=ylab, main=title)
}

#' Weight length relations in the data set
plot_age_length <- function(individual, xlab="alder", ylab="lengde", title="Alle individer"){
  
  plot(individual$age, individual$length, xlab=xlab, ylab=ylab, main=title, pch=".")
}

#' Age length relations in the data set
#' @param individual
#' @param xlab label for x-axis (weight)
#' @param ylab label for y-axis (length)
#' @param title title for plot
#' @param density logical: if T, data points will be overlayed with density plot for densest 1-alpha fraction
#' @param alpha see density
plot_weight_length <- function(individual, xlab="vekt", ylab="lengde", title="Alle individer", density=T, alpha=0.01){
  plot(individual$weight, individual$length, xlab=xlab, ylab=ylab, main=title, pch=".")
  
  if (density){
    m<-kde2d(individual$weight, individual$length, n=100)
    
    ss <- sort(m$z)
    cs <- cumsum(ss)
    limz <-ss[cs<=alpha]
    pp<-RColorBrewer::brewer.pal(name="Greys",n=9)[2:9]
    image(m, col=pp, zlim=c(limz[length(limz)],max(m$z)+min(m$z)), add=T)
  }
}


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
  
  if (length(tt)>1){
    barplot(tt, xlab=xlab, names=paste(labels$shortname, " (", names(tt), ")", sep=""), horiz = T, las=1, main=title, cex.names = cex.names)    
  }
  else{
    pie(tt, labels=paste(labels$shortname, " (", names(tt), ")", sep=""))  
  }
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
  
  if(length(tt)>1){
    barplot(tt, xlab=xlab, names=paste(labels$shortname, " (", names(tt), ")", sep=""), horiz = T, las=1, main=title, cex.names = cex.names)  
  }
  else{
    pie(tt, labels=paste(labels$shortname, " (", names(tt), ")", sep=""))
  }
  
  
}

#' Composition of taxa types (Fangstnivå: prøvetype) in data
#' @param catchsample
#' @param title title for plot
#' @param xlab label for x axis
#' @param blankcode code for NA / not registered
#' @param cex.names expansion factor for bar labels
plot_taxa_comp <- function(catchsample, title="taxa", xlab="# fangstprøver", blankcode=default_blankcode, cex.names=0.8){

  tt <- table(as.character(catchsample$noname))

  if (length(tt)>1){
    barplot(tt, xlab=xlab, names=names(tt), horiz = T, las=1, main=title, cex.names = cex.names)  
  }
  else{
    pie(tt)
  }
    
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
plot_catch_fractions <- function(station, catchsample, title="Fangstfraksjoner\n(platform/kvalitet/gruppe)", xlab="# fangstprøver", allname="All fangst", landname="Landet", retainname="S. landet", discname="S. ikke landet", allcol="red3", disccol="red", landcol="blue4", retaincol="blue", unkowncol="white", blankcode=default_blankcode){
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
  legend("topright", fill=c(unkowncol, landcol, retaincol, disccol, allcol), legend=c(blankcode, landname, retainname, discname, allname))
}

#' Plots overview of potential filtering parameters in samples
plot_sample_comp <- function(catchsample, individual){
  catchsample <- merge(catchsample, unique(individual[,c("serialno","species","samplenumber")]), by=c("serialno","species","samplenumber"))
  old.par <- par(no.readonly = T)
  par(mfrow=c(2,2))
  par(mar=c(5.1,12,4.1,2.1))
  plot_sample_types(catchsample, xlab="# fangstprøver m/ind")
  par(mar=c(5.1,7,4.1,2.1))
  plot_product_types(catchsample, xlab="# fangstprøver m/ind")
  par(mar=c(5.1,14,4.1,2.1))
  plot_length_measurements(catchsample, xlab="# fangstprøver m/ind")
  par(mar=c(5.1, 4.1,4.1,2.1))
  plot_weight_length(individual, title="Alle ind. 1% ekstr. obs", alpha=0.01, density=T)
  par(old.par)
}

plot_measurement_comp <- function(station, catchsample, individual){
  old.par <- par(no.readonly = T)
  par(mfrow=c(2,2))
  par(mar=c(5.1,4.1,4.1,2.1))
  plot_individuals(catchsample, individual)
  par(mar=c(5.1,7,4.1,2.1))
  plot_taxa_comp(catchsample)
  par(mar=c(5.1,12,4.1,2.1))
  plot_catch_fractions(station, catchsample)
  par(mar=c(5.1,7,4.1,2.1))
  plot_station_types(station)
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
  station <- baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_FishStation.txt
  catchsample <- baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_CatchSample.txt
  individual <- baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_Individual.txt
  plot_measurement_comp(station, catchsample, individual)
  plot_sample_comp(catchsample, individual) 
}
baselineOutput <- runbl()
ex(baselineOutput)