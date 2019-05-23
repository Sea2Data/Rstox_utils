eca_results <- list()
eca_results$kolmule2017 <- "./kolmule/eca_2017/KOLMULE2017.fit.2017_allgears_totalarea_season1234.caa.txt"
stox_results <- list()
stox_results$kolmule2017 <- "./kolmule/stox_2017/means_Abundance_millions.txt"

parse_eca <- function(report){
  return(read.csv(report, sep="\t", header=T, stringsAsFactors = F, strip.white = T))
}

parse_stox <- function(report){
  return(read.csv(report, sep="\t", header=T, stringsAsFactors = F, strip.white = T, comment.char = "#"))
}

load_comptable <- function(estimate){
  eca <- parse_eca(eca_results[[estimate]])
  eca$cv <- eca$sd/eca$mean
  stox <- parse_stox(stox_results[[estimate]])
  
  eca_stox <- merge(eca, stox, by="age", suffixes=c(".eca", ".stoxreca"))
  eca_stox <- eca_stox[order(as.integer(eca_stox$age)),]
}

compmeans <- function(comptable, title=""){
  eca_stox <- comptable
  barplot(rbind(eca_stox$mean.eca,eca_stox$mean.stoxreca), beside=T, names.arg = eca_stox$age, ylab="Abundance (millions)", legend.text = c("ECA", "STOX-RECA"), main=title, xlab="age")
}

compsds <- function(comptable, title=""){
  eca_stox <- comptable
  barplot(rbind(eca_stox$sd.eca,eca_stox$sd.stoxreca), beside=T, names.arg = eca_stox$age, ylab="sd (millions)", legend.text = c("ECA", "STOX-RECA"), main=title, xlab="age")
}


compareCaa <- function(){
  
  if (!all(names(eca_results %in% names(stox_results)))){
    stop("eca_results and stox_resultds are not in correspondance")
  }
  if (!all(names(stox_results %in% names(eca_results)))){
    stop("eca_results and stox_resultds are not in correspondance")
  }
  
  for (n in names(eca_results)){
    eca_stox <- load_comptable(n)
    compmeans(eca_stox, n)
    compsds(eca_stox, n)
  }
  
}
compareCaa()