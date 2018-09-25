# Sensitivity of survey time series using Rstox for ICES ACS in Hamburg Sept 2018:
library(Rstox)
library(ggplot2)

# Run one survey time series:
runOneProject <- function(pr, ...){
	cat(pr, "\n")
	print(str(list(...)))
	b <- runBootstrap(pr, ...)
	i <- imputeByAge(pr, ...)
	saveProjectData(pr)
	AbundanceByAge <- reportAbundance(pr, var="Abundance", ...)
	TSN <- reportAbundance(pr, grp1=NULL, var="Abundance", ...)
	WeightByAge <- reportAbundance(pr, var="Weight", ...)
	TSB <- reportAbundance(pr, grp1=NULL, var="Weight", ...)
	print(lapply(TSN, "[[", "abnd"))
	print(lapply(TSB, "[[", "abnd"))
	closeProject(pr)
	list(AbundanceByAge=AbundanceByAge, TSN=TSN, WeightByAge=WeightByAge, TSB=TSB)
}
# Create a data frame of year, mean, mean +- 1.96 sd, and cv from the survey time series:
# getTotalSurveyEstimates <- function(STS, removeMissingAge=TRUE){
getTotalSurveyEstimates <- function(STS, level="bootstrapImpute"){
	# Function for extracting all by age data frames:
	getAllByAge <- function(name, STS, years){
		# Get the number of years:
		numSTS <- length(STS)
		# Get all by age data frames:
		ByAge_all <- lapply(STS, function(x) x[[name]][[level]]$abnd)
		# Add years:
		ByAge_all <- lapply(seq_along(ByAge_all), function(i) cbind(ByAge_all[[i]], year=years[i]))
		
		# Get all ages from the colnames:
		#ages <- lapply(ByAge_all, rownames)
		ages <- lapply(ByAge_all, "[[", "age")
		allages <- sort(as.numeric(unique(unlist(ages))), na.last=TRUE)
		numages <- length(allages)
		
		cols <- colnames(ByAge_all[[1]])
		numcols <- length(cols)
		# Fill in the data frames in an array with years along the third dimension:
		out <- array(NA, c(numages, numcols, numSTS))
		for(i in seq_len(numSTS)){
			thisrows <- match(ByAge_all[[i]]$age, allages)
			#thisrows <- seq_len(nrow(ByAge_all[[i]]))
			out[thisrows,,i] <- data.matrix(ByAge_all[[i]])
		}
		
		list(ByAge=out, allages=allages, cols=cols, ages=ages)
	}
	
	# Function for extracting all total survey biomass/number single row data frames:
	getAllTSN <- function(name, STS, years){
		out <- lapply(STS, function(x) x[[name]][[level]]$abnd)
		out <- do.call(rbind, out)
		out <- cbind(years=years, out)
		out
	}
	
	# Get years:
	n <- names(STS)
	years <- as.numeric(substring(n, nchar(n)-3))
	
	# All AbundanceByAge:
	temp <- getAllByAge("AbundanceByAge", STS=STS, years=years)
	ages <- temp$ages
	cols <- temp$cols
	AbundanceByAge_all <- temp$ByAge
	# All WeightByAge:
	WeightByAge_all <- getAllByAge("WeightByAge", STS=STS, years=years)$ByAge
	
	#if(removeMissingAge){
	#	AbundanceByAge_all <- lapply(AbundanceByAge_all, function(x) x[!is.na(x$age),])
	#	WeightByAge_all <- lapply(WeightByAge_all, function(x) x[!is.na(x$age),])
	#}
	#
	
	# All TSN:
	TSN_all <- getAllTSN("TSN", STS=STS, years=years)
	# All TSB:
	TSB_all <- getAllTSN("TSB", STS=STS, years=years)
	
	
	list(
		ages = ages, 
		cols = cols, 
		years = years, 
		AbundanceByAge_all = AbundanceByAge_all, 
		TSN_all = TSN_all, 
		WeightByAge_all = WeightByAge_all, 
		TSB_all = TSB_all)
}
# Function for plotting the output from getTotalSurveyEstimates():
plotMeanAndCV <- function(report, ...){
	plot(report$years, report$TSB_mean, type="o", ...)
	lines(report$years, report$TSB_LowerConf, lty=2, ...)
	lines(report$years, report$TSB_UpperConf, lty=2, ...)
}
# Function for constructing labels containing the parameters used for the current sensitivity run:
getOneParLabel <- function(ind, par){
	parlist <- lapply(par, function(x) lapply(x, "[", ind))
	paste(paste(names(parlist), sapply(parlist, names), unlist(parlist), sep="_"), collapse=", ")
}
stripParLabel <- function(x){
	out <- strsplit(x, ", ")
	out <- lapply(out, function(y) strsplit(y, "_"))
	out <- lapply(out, function(y) lapply(y, "[", -1))
	out <- lapply(out, function(y) lapply(y, paste, collapse="_"))
	out <- sapply(out, paste, collapse=", ")
	out
}
# Function for running a whole survey time series for one parameter specification:
runOnePar <- function(ind, projectNames, par, ...){
	print(paste("Running parameter set nr ", ind))
	parlist <- lapply(par, function(x) lapply(x, "[", ind))
	print(parlist)
	# Run the whole time series:
	system.time(STS <- lapply(projectNames, runOneProject, parlist=parlist, ...))
	names(STS) <- basename(projectNames)
	
	# Save the data to file:
	#path <- dirname(projectNames[1])
	#suffix <- paste(paste(names(parlist), unlist(parlist), sep="_"), collapse=", ")
	#RDataFile <- file.path(path, paste0(basename(path), "_", suffix, ".RData"))
	#save(STS, file=RDataFile)
	
	# Get the data frame of means and precision:
	report <- getTotalSurveyEstimates(STS)
	
	list(parlist=parlist, report=report, STS=STS)
}
# Function running all parameter specifications:
runSTS <- function(projectNames, par, parInd=NULL, ...){
	if(length(parInd)){
		par <- lapply(par, function(x) lapply(x, "[", parInd))
	}
	
	npar <- length(par[[1]][[1]])
	abnd <- lapply(seq_len(npar), runOnePar, projectNames=projectNames, par=par, ...)
	
	path <- dirname(projectNames[1])
	RDataFile <- file.path(path, paste0(basename(path), ".RData"))
	save(abnd, file=RDataFile)
	
	# Add names to the output as concatination of the parameters:
	names(abnd) <- sapply(seq_len(npar), getOneParLabel, par=par)
	abnd
}
extractOnePar <- function(x, yind=NULL){
	if(length(yind)){
		x$STS <- x$STS[yind]
	}
	# Get the data frame of means and precision:
	x$report <- getTotalSurveyEstimates(x$STS)
	x
}
extractSTS <- function(abnd, yind=NULL){
	abnd <- lapply(abnd, extractOnePar, yind=yind)
	abnd
}
# Functions for reporting and plotting the time series TSB or TSN for a specific parameter set index 'ind':
reportTSB <- function(abnd, ind=NULL, name="TSB_all", age=NULL, year=NULL, yearClass=NULL, parlabels=NULL){
	
	# Extract year classes:
	#extractYearClasses <- function(x, dims, var="Ab.Sum.mean"){
	extractYearClasses <- function(d, dims){
		# Number of ages and years:
		nages <- length(dims$ages)
		nyears <- length(dims$years)
	
		# Get matrices of the ages and years, as would outer do:
		yearMatrix <- matrix(dims$years, nrow=nages, ncol=nyears, byrow=TRUE)
		ageMatrix <- matrix(dims$ages, nrow=nages, ncol=nyears, byrow=FALSE)
		birthYears <- yearMatrix - ageMatrix
	
		#colmatch <- which(dims$cols == var)
		var <- aperm(d, c(1,3,2))
		dimvar <- dim(var)
		dim(var) <- c(prod(dimvar[1:2]), dimvar[3])
		var <- as.data.frame(var)
		colnames(var) <- dims$cols
	
		out <- cbind(
			birthYears = c(birthYears), 
			ages = c(ageMatrix), 
			years = c(yearMatrix), 
			var
		)
	
		out <- by(out, out$birthYears, function(x) x)
		#out <- lapply(out, plyr::rename, c(var=var))
	
		out
	}
	
	# If data by age are requested, extract the given year class:
	getSurveyDataMatrix <- function(d, dims, age=NULL, year=NULL, yearClass=NULL){
		if(length(dim(d))==3){
			if(length(age)){
				agematch <- which(dims$age==age)
				out <- d[agematch, , ]
				out <- as.data.frame(out)
				names(out) <- dims$years
			}
			else if(length(year)){
				browser()
				yearmatch <- which(dims$year==year)
				out <- d[, , yearmatch]
				out <- as.data.frame(out)
				names(out) <- dims$cols
			}
			else if(length(yearClass)){
				temp <- extractYearClasses(d, dims)
				out <- temp[[as.character(yearClass)]]
			}
			else{
				stop("'age', 'year' or 'yearClass' must be given when the data requested are in 3 dimensions (age, variables, years)")
			}
			
		}
		else{
			out <- d
		}
		out
	}
	
	# Get case labels:
	if(length(parlabels)==0){
		parlabels <- stripParLabel(names(abnd))
	}
	
	if(length(ind)==0){
		ind <- seq_along(abnd)
	}
	# Get age, cols and year definitions:
	temp <- abnd[[ind[1]]]$report
	years <- temp$years
	cols <- temp$cols
	if("allages" %in% names(temp)){
		allages <- temp$allages
	}
	else{
		ages <- temp$ages
		allages <- as.numeric(unique(unlist(ages)))
	}
	dims <- list(ages=allages, cols=cols, years=years)
	
	# Only one case_
	#if(length(ind)==1){
	#	d <- abnd[[ind]]$report[[name]]
	#}
	#else if(length(ind)>1){
		d <- lapply(abnd[ind], function(x) x$report[[name]])
		d <- lapply(d, getSurveyDataMatrix, dims=dims, age=age, year=year, yearClass=yearClass)
		d <- lapply(seq_along(d), function(i) cbind(d[[i]], Case=parlabels[ind[i]]))
		d <- do.call(rbind, d)
		d
		#}
	
	
	if(name=="TSB_all"){
		scale <- 1e-3
		ylab <- "Total biomass (thousand tonnes)"
	}
	else{
		scale <- 1
		ylab <- "Total abundance (millions)"
	}
	d[startsWith(colnames(d), "Ab")] <- d[startsWith(colnames(d), "Ab")] * scale
	d$ylab = ylab
	d$name = name
	d$year <- factor(d$year)
	
	d
}

plotTSB <- function(d=NULL, ind, abnd, var="Ab.Sum.mean", conf=c("Ab.Sum.5%", "Ab.Sum.95%"), name="TSB_all", linesize=1.5, new=FALSE, yind=NULL, age=NULL, year=NULL, yearClass=NULL, normalizeYearClass=FALSE, parlabels=NULL){
	
	divideByMax <- function(d, var, fun){
		maxVar <- c(by(d[[var]], d$Case, fun, na.rm=TRUE))
		maxVar <- rep(maxVar, table(d$Case))
		d[[var]] / maxVar
	}
	
	if(new){
		dev.new()
		plot.new() 
	}
	
	if(length(d)==0){
		d <- reportTSB(ind=ind, abnd=abnd, name=name, age=age, year=year, yearClass=yearClass, parlabels=parlabels)
	}
	if(!isFALSE(normalizeYearClass)){
		if(is.numeric(normalizeYearClass)){
			fun <- function(x, ...) x[normalizeYearClass]
		}
		else{
			fun <- max
		}
		d[[var]] <- divideByMax(d, var, fun=fun)
		if(length(conf)==2){
			d[[conf[1]]] <- divideByMax(d, conf[1], fun=fun)
			d[[conf[2]]] <- divideByMax(d, conf[2], fun=fun)
		}
	}
	# Allow for the "%" in the colnames:
	colnames(d) <- sub("%", "", colnames(d), fixed=TRUE)
	#
	#if(length(yind)){
	#	d <- d[yind, ]
	#}
	#
	
	d$years <- factor(d$years)
	
	
	if(length(d$Case)){
		p <- ggplot(data=d, aes_string(x="years", y=var, group="Case", colour="Case"))
	}
	else{
		p <- ggplot(data=d, aes_string(x="years", y=var, group=1))
	}
	
	p <- p + 
	geom_path(size=linesize) + 
	geom_point(size=2*linesize) + 
	expand_limits(y=0) + 
	ylab(d$ylab[1]) + xlab("Year") + 
	theme(axis.text=element_text(size=18, angle=90), axis.title=element_text(size=21,face="bold"))
	
	if(length(conf)==2){
		# Allow for the "%" in the conf:
		conf <- sub("%", "", conf, fixed=TRUE)
		p <- p +
		geom_path(data=d, aes_string(x="years", y=conf[1]), linetype="dashed") + 
		geom_path(data=d, aes_string(x="years", y=conf[2]), linetype="dashed")
	}
	
	p <- p + theme(legend.text=element_text(size=18), legend.title=element_text(size=21,face="bold"))
	
	
	print(p)
}





dir <- "~/workspace/stox/project/2018-09-17 Sensitivity analyses of survey time series"

#allSTS <- getNMDinfo("sts")
#names(allSTS)

sts1 <- "Barents Sea Northeast Arctic cod bottom trawl index in winter"
sts2 <- "North Sea NOR lesser sandeel acoustic abundance estimate in spring"

sts <- list.files(dir, full.names=TRUE, recursive=FALSE)






#################################################
##### Experiment 1: Cod in the Barents Sea: #####
#################################################
# system.time(pr1 <- getNMDdata(sts1, abbrev=TRUE, subdir=TRUE, dir=dir, ow=TRUE))
# #   user  system elapsed 
# #  7.605   8.282  63.061 
# lapply(pr1, updateProject)


# Run the whole series with nboot=5, and one core:
path <- sts[1]
pr <- list.dirs(path, full.names=TRUE, recursive=FALSE)

# Chech parameters for all years:
#param <- lapply(pr, getBaseline, input="par", proc=NULL, close=TRUE)

# Many differences:
# sapply(param, function(x) all.equal(x[names(x) != "ReadBioticXML"], param[[2]][names(param[[2]]) != "ReadBioticXML"]))

#do.call(rbind, sapply(param, function(x) x$Density))

# Run the sandeel STS at two different frequencies. The first value of 'a' (TS at length 1 cm) is used in the official estimates for sandeel, and should be the one reported in MacLennan and Simmonds 2006:


# Define the parameter span for the sensitivity analysis:
Nrun <- 7
Midrun <- ceiling(Nrun/2)
LMin0 <- 15
Alpha0 <- 5.91
scale <-  seq(0.4, 1.6, length.out=Nrun)
LMax0 <- 62
LMax <- round(LMax0 * scale)
LMax
Beta0 <- 0.43
Beta <- round(Beta0 * scale, digits=2)
Beta
#LMaxBeta <- expand.grid(LMax, Beta)
LMax <- c(LMax, rep(LMax[Midrun], Nrun - 1))
Beta <- c(rep(Beta[Midrun], Nrun), Beta[-Midrun])


# Plot of the sweep width function for use in the presentation for Hamburg:
SweepWidth <- function(L, Alpha, Beta, LMin, LMax){
	w <- function(L, Alpha, Beta){
		Alpha * L^Beta
	}
	below <- L < LMin
	above <- L > LMax
	out <- w(L, Alpha, Beta)
	out[below] <- w(LMin, Alpha, Beta)
	out[above] <- w(LMax, Alpha, Beta)
	out
}
L <- 0:100
W <- SweepWidth(L, Alpha0, Beta0, LMin0, LMax0)
d <- data.frame(L=L, W=W)
# The plot
ggplot(data=d, aes(x=L, y=W)) + 
geom_path() + expand_limits(y=0) + 
ylab("Sweep width (m)") + xlab("Fish length (cm)") + 
theme(axis.text=element_text(size=18), axis.title=element_text(size=21,face="bold"))


# Define the parameters:
#par <- list(
#	SweptAreaDensity = list(LMax = as.character(LMaxBeta[,1])),
#	SweptAreaDensity = list(Beta = as.character(LMaxBeta[,2])) 
#)
par <- list(
	SweptAreaDensity = list(LMax = as.character(LMax)),
	SweptAreaDensity = list(Beta = as.character(Beta)) 
)
#par <- lapply(par, function(x) lapply(x, "[", Midrun))

valid <- c(-1, length(pr))

system.time(abnd1 <- runSTS(pr[valid], par=par, nboot=5, cores=1, bootstrapMethod="SweptAreaLength"))
#system.time(abnd1_20 <- runSTS(pr[valid], par=par, nboot=20, cores=2))

# Write the data to file:
RDataFile <- file.path(path, paste0(basename(path), "STS_Cod_04-16.RData"))
save(abnd1, file=RDataFile)



l <- load(RDataFile)






atLMax <- seq_len(Nrun)
seqMidrun1 <- seq_len(Midrun-1)
atBeta <- c(
	Nrun + seqMidrun1, 
	Midrun, 
	Nrun + Midrun-1 + seqMidrun1
)






#########################################################
########## Plots for the Hamburg presentation: ##########
#########################################################
test <- plotTSB(ind=atLMax, abnd=abnd1_temp2, name="TSB_all")
test <- plotTSB(ind=atBeta, abnd=abnd1_temp2, name="TSB_all")

test <- plotTSB(ind=atLMax, abnd=abnd1_temp2, name="TSB_all", normalizeYearClass=24, conf=NULL)
test <- plotTSB(ind=atBeta, abnd=abnd1_temp2, name="TSB_all", normalizeYearClass=24, conf=NULL)

test <- plotTSB(ind=atLMax, abnd=abnd1_temp2, name="AbundanceByAge_all", yearClass=2006, normalizeYearClass=5)
test <- plotTSB(ind=atBeta, abnd=abnd1_temp2, name="AbundanceByAge_all", yearClass=2006, normalizeYearClass=5)
#########################################################






###################################################
##### Experiment 2: Sandeel in the North Sea: #####
###################################################
# system.time(pr2 <- getNMDdata(sts2, abbrev=TRUE, subdir=TRUE, dir=dir, ow=TRUE))
# #   user  system elapsed 
# #  7.605   8.282  63.061 
# lapply(pr2, updateProject)


# Run the whole series with nboot=5, and one core:
path <- sts[2]
pr <- list.dirs(path, full.names=TRUE, recursive=FALSE)

# Chech parameters for all years:
#param <- lapply(pr, getBaseline, input="par", proc=NULL, close=TRUE)

# Many differences:
# sapply(param, function(x) all.equal(x[names(x) != "ReadBioticXML"], param[[2]][names(param[[2]]) != "ReadBioticXML"]))

#do.call(rbind, sapply(param, function(x) x$Density))

# Run the sandeel STS at two different frequencies. The first value of 'a' (TS at length 1 cm) is used in the official estimates for sandeel, and should be the one reported in MacLennan and Simmonds 2006:
par <- list(
	FilterAcoustic = list(FreqExpr = c("frequency == 38000", "frequency == 200000")), 
	AcousticDensity = list(a = c(-93, -93.1))
)

system.time(abnd2 <- runSTS(pr, par=par, nboot=5, cores=1, bootstrapMethod="AcousticTrawl"))
#system.time(abnd2 <- runSTS(pr, par=par, parInd=1, nboot=5, cores=1, bootstrapMethod="AcousticTrawl"))

# Write the data to file:
RDataFile <- file.path(path, paste0(basename(path), "STS_Sandeel.RData"))
save(abnd2, file=RDataFile)

l <- load(RDataFile)



npar <- length(par[[1]][[1]])
parlabels <- par$FilterAcoustic$FreqExpr
TSB_all <- lapply(seq_len(npar), reportTSB, abnd=abnd2, name="TSB_all")
TSB_all <- lapply(seq_along(TSB_all), function(ind) cbind(TSB_all[[ind]], Case=parlabels[ind]))
TSB_all <- do.call(rbind, TSB_all)


plotTSB(TSB_all)


lapply(abnd2, "[[", "report")

abnd2_temp <- extractSTS(abnd2)


plot(abnd2[[1]]$report$TSB_mean, type="o", ylim=c(0,45000))
lines(abnd2[[2]]$report$TSB_mean, type="o", col=2)

###################################################



#########################################################
########## Plots for the Hamburg presentation: ##########
#########################################################
test <- plotTSB(ind=NULL, abnd=abnd2_temp, name="TSB_all", parlabels=c("38 kHz", "200 kHz"))

#test <- plotTSB(ind=NULL, abnd=abnd2, name="AbundanceByAge_all", yearClass=2009)
#########################################################






