#' Plot results from eca.predict run
#' 
#' @param pred list from running eca.predict
#' @export   
predictPlot <- function(pred,box=TRUE,cred=TRUE,cred.p=0.9,title=NULL,fileName=NULL)
{
  ages<-unique(pred$AgeCategories)
  nage<-length(ages)
  ncaa <- dim(pred$TotalCount)[2]
  if(ncaa==2*nage){
    cc <- TRUE
    txt <- "Skrei"
  }
  else {
    cc <- FALSE
    txt <- title
  }
  
  ind <- 1:nage
  print(ind)
  A <- pred$TotalCount[,ind,]
  L <- pred$MeanLength[ind,]
  W <- pred$MeanWeight[ind,]
  P <- list(TotalCount=A,MeanLength=L,MeanWeight=W,AgeCategories=pred$AgeCategories)
  predictPlotPart(P,box=box,cred=cred,cred.p=cred.p,title=txt,fileName=fileName)
  if(cc){
    ind <- (nage+1):ncaa
    print(ind)
    A <- pred$TotalCount[,ind,]
    L <- pred$MeanLength[ind,]
    W <- pred$MeanWeight[ind,]
    P <- list(TotalCount=A,MeanLength=L,MeanWeight=W,AgeCategories=pred$AgeCategories)
    predictPlotPart(P,box=box,cred=cred,cred.p=cred.p,title="Coastal cod",fileName=fileName)
  }
}
  
#' @export   
predictPlotPart <- function(pred,box=TRUE,cred=TRUE,cred.p=0.9,title=NULL,fileName=NULL)
{
  ages <- unique(pred$AgeCategories)
  nage <- length(ages)
  caa <- apply(pred$TotalCount,2:3,sum)/1e6
  caa.mean <- rowMeans(caa)
  caa.sd <- apply(caa,1,sd)
  
  dimnames <- list(rep("",nage),
                   c("age","mean","sd",paste(0.5*(1-cred.p)*100,"%",sep=''),
                     paste(0.5*(1+cred.p)*100,"%",sep='')))
  leg <- c("mean",paste(cred.p*100,"% interval",sep=''))
  q <- NULL
  for(i in 1:nrow(caa))
    q <- rbind(q,quantile(caa[i,],prob=c((1-cred.p)/2,(1+cred.p)/2)))
  

  if(!is.null(fileName)){
    pdf(fileName)
  }
  
  if(!is.null(title))
    txt<-paste("Catch at age:",title)
  else txt<-""
  if(box){
    boxplot(as.vector(caa)~rep(ages,ncol(caa)),xlab="Age",ylab="Millions")
    title(txt)
  }
  if(cred){
    error.bar(ages,caa.mean,q[,1],q[,2],incr=F,xlab="Age",ylab="Millions")
    legend("topright",leg=leg,pch=c(1,-1),lty=c(0,1))
    title(txt)
  }
  
  meanLength <- pred$MeanLength
  l.mean <- rowMeans(meanLength)
  l.sd <- apply(meanLength,1,sd)
  q <- NULL
  for(i in 1:nrow(meanLength))
    q <- rbind(q,quantile(meanLength[i,],prob=c((1-cred.p)/2,(1+cred.p)/2)))
  
  error.bar(ages,l.mean,q[,1],q[,2],incr=F,xlab="Age",ylab="Centimeters")
  legend("topleft",leg=leg,pch=c(1,-1),lty=c(0,1))
  if(!is.null(title))
    txt<-paste("Length given age:",title)
  else txt<-""
  title(txt)
  
  meanWeight <- pred$MeanWeight
  w.mean <- rowMeans(meanWeight)
  w.sd <- apply(meanWeight,1,sd)
  q <- NULL
  for(i in 1:nrow(meanWeight))
    q <- rbind(q,quantile(meanWeight[i,],prob=c((1-cred.p)/2,(1+cred.p)/2)))
  
  error.bar(ages,w.mean,q[,1],q[,2],incr=F,xlab="Age",ylab="Kilogram")
  legend("topleft",leg=leg,pch=c(1,-1),lty=c(0,1))
  if(!is.null(title))
    txt<-paste("Weight given length:",title)
  else txt<-""
  title(txt)

  if(!is.null(fileName)){
    dev.off()
  }
}

#' @export   
plotCov <- function(cov,txt)
{
  pnames <- names(cov)
  for(i in 1:length(pnames)){
    if(!pnames[i]%in%c("cell","catchSample")){
      m <- cov[[i]]
      nlev <- dim(m)[2]
      rc <- getrc.bin(nlev)
      par(mfrow=c(rc$r,rc$c))
      for(j in 1:nlev){
        tsplot.bin(t(m[,j,]))
        title(paste(txt,": effects - ",pnames[i],sep=""))
      }
    }
  }
}
#' @export   
plotTau <- function(tau,txt)
{
  pnames <- names(tau)
  for(i in 1:length(pnames)){
    m <- tau[[i]]
    nlev <- length(m)
    par(mfrow=c(1,1))
    tsplot(m)
    title(paste(txt,": precision - ",pnames[i],sep=""))
  }
}
#' @export   
plotCAR <- function(CAR,txt)
{
  pnames <- names(CAR)  
  for(i in 1:length(pnames)){
    m <- CAR[[i]]
    nlev <- length(m)
    par(mfrow=c(1,1))
    tsplot(m)
    title(paste(txt,": CAR - ",pnames[i],sep=""))
  }
}

#' Plot results from eca.estimate run
#' 
#' @param params list from running eca.estimate
#' @export   
fitPlot <- function(params,plot.age=TRUE,plot.lga=TRUE,plot.wgl=TRUE,plot.cc=FALSE,plot.nonlin=FALSE,fileName=NULL)
{
  if(plot.age){
    par <- params$ProportionAtAge$Intercept
    cov <- par$cov
    tau <- par$tau
    CAR <- par$CAR
    plotCov(cov,"ProportionAtAge")
    if(!is.null(tau)){
      plotTau(tau,"ProportionAtAge")
    }
    if(!is.null(CAR)){
      plotCAR(CAR,"ProportionAtAge")
    }
  }
  if(plot.lga){
    for(k in 1:2){
      cov <- params$LengthGivenAge[[k]]$cov
      tau <- params$LengthGivenAge[[k]]$tau
      CAR <- params$LengthGivenAge[[k]]$CAR
      if(k==1)
        txt <- "LengthGivenAge - intercept"
      else if(k==2)
        txt <- "LengthGivenAge - slope"
      plotCov(cov,txt)
      if(!is.null(tau)){
        plotTau(tau,txt)
      }
      if(!is.null(CAR)){
        plotCAR(CAR,txt)
      }
    }
  }
  if(plot.nonlin){
    tsplot.bin(params$LengthGivenAge$nonlin)
    title(paste("LengthGivenAge - nonlinear: gamma",sep=""))
  }
  if(plot.cc){
    for(k in 1:2){
      cov <- params$LengthGivenAgeCC[[k]]$cov
      tau <- params$LengthGivenAgeCC[[k]]$tau
      CAR <- params$LengthGivenAgeCC[[k]]$CAR
      if(k==1)
        txt <- "LengthGivenAge Coastal cod - intercept"
      else if(k==2)
        txt <- "LengthGivenAge Coastal cod - slope"
      plotCov(cov,txt)
      if(!is.null(tau)){
        plotTau(tau,txt)
      }
      if(!is.null(CAR)){
        plotCAR(CAR,txt)
      }
    }
    if(plot.nonlin){
      tsplot.bin(params$LengthGivenAgeCC$nonlin)
      title(paste("LengthGivenAge Coastal cod - nonlinear: gamma",sep=""))
    }
  }
  if(plot.wgl){
    for(k in 1:2){
      cov <- params$WeightGivenLength[[k]]$cov
      tau <- params$WeightGivenLength[[k]]$tau
      CAR <- params$WeightGivenLength[[k]]$CAR
      if(k==1)
        txt <- "WeightGivenLength - intercept"
      else if(k==2)
        txt <- "WeightGivenLength - slope"
      plotCov(cov,txt)
      if(!is.null(tau)){
        plotTau(tau,txt)
      }
      if(!is.null(CAR)){
        plotCAR(CAR,txt)
      }
    }
  }
  if(plot.cc){
    for(k in 1:2){
      cov <- params$WeightGivenLengthCC[[k]]$cov
      tau <- params$WeightGivenLengthCC[[k]]$tau
      CAR <- params$WeightGivenLengthCC[[k]]$CAR
      if(k==1)
        txt <- "WeightGivenLength Coastal cod - intercept"
      else if(k==2)
        txt <- "WeightGivenLength Coastal cod - slope"
      plotCov(cov,txt)
      if(!is.null(tau)){
        plotTau(tau,txt)
      }
      if(!is.null(CAR)){
        plotCAR(CAR,txt)
      }
    }
  }
  
}

#' @export   
tsplot.bin<-function(x)
{
  if(is.array(x)){
    if(dim(x)[1]==1)tsplot(as.vector(x))
    else tsplot(x)
  } else tsplot(x)
}
#' @export   
tsplot <- function(x)
{
  if (is.matrix(x)) 
    cols <- bowrain(dim(x)[2]) else cols <- 1
  ts.plot(x, col=cols)
}
#' @export   
bowrain <- function(n=100)
{
  n0 <- floor(n*1.25)
  breaks <- floor(n0*c(0.3,0.7))
  use <- c(1:breaks[1], seq(breaks[1]+1,breaks[2],by=2),(breaks[2]+1):n0)
  rev(rainbow(n0,start=.03,end=.6))[use]
}
#' @export   
getrc.bin<-function(nplot)
{
  if(nplot==1){r<-c<-1}
  if(nplot==2){r<-2;c<-1}
  if(nplot==3){r<-3;c<-1}
  if(nplot==4)r<-c<-2
  if(nplot>=5&nplot<=6){r<-3;c<-2}
  if(nplot>=7)r<-c<-3
  return(list(r=r,c=c))
}
#' @export   
error.bar<-function(x, y = NULL, lower, upper, incr = TRUE, bar.ends = TRUE, gap = TRUE, add = FALSE,
                    horizontal = FALSE, ..., xlab = deparse(substitute(x)), xlim, ylim,size.bar=NULL)
{
  draw.null.warn <- function(draw, gap)
  {
    if(!any(draw)) {
      warning("Not enough room for a gap.")
      draw <- !draw
      gap <- 0
    }
    invisible(list(draw = draw, gap = gap))
  }
  if(missing(x))
                stop("no data for x or y")
        if(missing(y)) {
                if(missing(xlab))
                        xlab <- "Index"
                y <- x
                x <- time(x)
        }
        n <- length(x)
        if(length(y) != n)
                stop("length of y must equal the length of x")
        center <- if(horizontal) x else y
        if(missing(lower))
                stop("you must provide lower")
        if(length(lower) > 1 && length(lower) != n)
                stop("length of lower must be 1 or equal to the length of x")
        #if incr=T lower is assumed >=0
        if(incr) lower <- center - abs(lower) else lower <- rep(lower, length
                         = n)
        if(any(lower >= center))
                warning(paste(
                        "There are values of 'lower' which are greater or equal to ",
                        if(horizontal) "x" else "y"))
        if(missing(upper))
                upper <- 2 * center - lower
        else {
                if(length(upper) > 1 && length(upper) != n)
                        stop("length of upper must be 1 or equal to the length of x"
                                )
                if(incr)
                        upper <- center + upper
                else upper <- rep(upper, length = n)
        }
        if(any(upper <= center))
                warning(paste(
                        "There are values of 'upper' which are smaller or\nequal to ",
                        if(horizontal) "x" else "y"))
        if(!add)
                if(horizontal) {
                        if(missing(ylim))
                                plot(x, y, xlim = if(missing(xlim)) range(
                                                c(lower, upper), na.rm = TRUE)
                                         else xlim, xlab = xlab, ...)
                        else plot(x, y, xlim = if(missing(xlim)) range(c(lower,
                                                upper), na.rm = TRUE) else xlim,
                                        ylim = ylim, xlab = xlab, ...)
                }
                else {
                        if(missing(xlim))
                                plot(x, y, ylim = if(missing(ylim)) range(
                                                c(lower, upper), na.rm = TRUE)
                                         else ylim, xlab = xlab, ...)
                        else plot(x, y, ylim = if(missing(ylim)) range(c(lower,
                                                upper), na.rm = TRUE) else ylim,
                                        xlim = xlim, xlab = xlab, ...)
                }
        if(horizontal) {
                 if(gap)
                        gap <- 0.75 * par("cxy")[1]
                draw <- x - lower > gap
                z <- draw.null.warn(draw, gap)
                draw <- z$draw
                gap <- z$gap
                segments(lower[draw], y[draw], x[draw] - gap, y[draw],...)
                draw <- upper - x > gap
                z <- draw.null.warn(draw, gap)
                draw <- z$draw
                gap <- z$gap
                segments(x[draw] + gap, y[draw], upper[draw], y[draw],...)
                if(bar.ends) {
                        if(is.null(size.bar))size.bar <- par("cxy")[2]
                        segments(lower, y - size.bar, lower, y + size.bar,...)
                        segments(upper, y - size.bar, upper, y + size.bar,...)
                }
        }
        else {
                if(gap)
                        gap <- 0.75 * par("cxy")[2]
                draw <- upper - y > gap
                z <- draw.null.warn(draw, gap)
                draw <- z$draw
                gap <- z$gap
                segments(x[draw], y[draw] + gap, x[draw], upper[draw],...)
                draw <- y - lower > gap
                z <- draw.null.warn(draw, gap)
                draw <- z$draw
                gap <- z$gap
                segments(x[draw], y[draw] - gap, x[draw], lower[draw],...)
                if(bar.ends) {
                        if(is.null(size.bar))size.bar <- par("cxy")[1]
                        segments(x - size.bar, upper, x + size.bar, upper,...)
                        segments(x - size.bar, lower, x + size.bar, lower,...)
                }
        }
}
