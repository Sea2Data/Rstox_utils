eca_results <- list()

eca_results$kolmule2017 <- "./kolmule/eca_2017/KOLMULE2017.fit.2017_allgears_totalarea_season1234.caa.txt"
eca_results$makrell2017<- "./makrell/eca_2017/MAKRELL2017.fit.2017_allgears_totalarea_season1234.caa.txt"
eca_results$sei.nssk.2018<- "./sei_nssk/eca_2018/SEI2018_ns_v00.fit.2018_allgears_totalarea_season1234.caa.txt"
eca_results$hyse.nssk.2018 <- "./hyse_nssk/eca_2018/HYSE2018_ns_v00.fit.2018_allgears_27fire_season1234.caa.txt"
stox_results <- list()
stox_results$kolmule2017 <- "./kolmule/stox_2017/results"
stox_results$makrell2017 <- "./makrell/stox_2017/results"
stox_results$sei.nssk.2018 <- "./sei_nssk/stox_2018/results/"
stox_results$hyse.nssk.2018 <- "./hyse_nssk/stox_2018/results/"

parse_eca <- function(report){
  return(read.csv(report, sep="\t", header=T, stringsAsFactors = F, strip.white = T))
}

parse_stox <- function(report){
  return(read.csv(report, sep="\t", header=T, stringsAsFactors = F, strip.white = T, comment.char = "#"))
}

load_comptable <- function(ecaresult, stoxfile){
  
  stox <- parse_stox(stoxfile)
  
  eca_stox <- merge(ecaresult, stox, by="age", suffixes=c(".eca", ".stoxreca"))
  eca_stox <- eca_stox[order(as.integer(eca_stox$age)),]
}

comppar <- function(estimate, parameter){
  
  eca <- parse_eca(eca_results[[estimate]])
  eca$cv <- eca$sd / eca$mean
  ecabase <- load_comptable(eca, list.files(stox_results[[estimate]], full.names = T)[[1]])
  
  ecaparname <- paste(parameter, "eca", sep=".")
  stoxparname <- paste(parameter, "stoxreca", sep=".")
  
  stoxest <- list()
  stoxages <- list()
  ymax <- max(ecabase[[ecaparname]])
  for (f in list.files(stox_results[[estimate]], full.names = T)){
    comtable <- load_comptable(eca, f)
    stoxest[[f]]<-comtable[[stoxparname]]
    stoxages[[f]]<-comtable$age
    if (max(comtable[[stoxparname]])>ymax){
      ymax <- max(comtable[[stoxparname]])
    }
  }
  
  plot(ecabase$age, ecabase[[ecaparname]], ylim=c(0,ymax), col="black", xlab="age", ylab=parameter, type="n", main=paste(parameter, "estimates", estimate))
  for (n in names(stoxest)){
    points(stoxages[[n]], stoxest[[n]], col="grey", pch=1)
  }
  points(ecabase$age, ecabase[[ecaparname]], col="black", pch=4)
  legend("topright", col=c("black", "grey"), legend = c("eca", "stox.Reca"), pch=c(4,1))
}



compareCaa <- function(){
  
  if (!all(names(eca_results %in% names(stox_results)))){
    stop("eca_results and stox_resultds are not in correspondance")
  }
  if (!all(names(stox_results %in% names(eca_results)))){
    stop("eca_results and stox_resultds are not in correspondance")
  }
  
  for (n in names(eca_results)){
    print(n)
    comppar(n, "mean")
    comppar(n, "sd")
  }
  
}