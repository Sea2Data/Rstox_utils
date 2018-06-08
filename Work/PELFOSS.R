# Fix the notes in the code such as "##### NOTE 1: #####"

library(XML)
library(Rstox)
library(ncdf4)
library(sp)
library(ggplot2)
library(maptools)


#*********************************************
#*********************************************
#' Read a NORWECOM file.
#'
#' \code{readNORWECOMbiomass} reads a NORWECOM biomass file, with biomass on a irregular grid Long, Latt.
#' \code{readNORWECOMsuperind} reads a NORWECOM super individual file.
#' \code{readNcVarsAndAttrs} extracts variables with long names as attributes.
#' \code{addTimeHoursFrom1950} adds time from the "hours since 1950"-variable of the NORWECOM data.
#' \code{getBiomassXY} converts to Cartesian coordinates for the biomass data.
#' \code{getSuperIndXY} converts to Cartesian coordinates for the superindividual data.
#'
#' @param x	The input NetCDF4 file.
#' 
#' @return
#'
#' @export
#' @rdname readNORWECOMbiomass
#' 
readNORWECOMbiomass <- function(
	ncfile, centroid, 
	vars = c("MACbiom", "Long", "Latt"), 
	rename = list(MACbiom="Biom")){
		
	# Read the NORWECOM superindividual file:
	out <- readNcVarsAndAttrs(ncfile, vars=vars, rename=rename)
	# Add time:
	out <- addTimeHoursFrom1950(out, timedim="T")
	
	# Use only the data object, and discard the nc object:
	out <- out$data
	
	##### NOTE 1: This should be fixed in the data: #####
	out$Long[168, 206] <- mean(out$Long[ 167:169, 206], na.rm=TRUE)
	
	# Get Cartesian coordinates:
	out <- getBiomassXY(out, centroid=centroid)
	
	# Return:
	out
}
#'
#' @export
#' @rdname readNORWECOMbiomass
#' 
readNORWECOMsuperind <- function(
	ncfile, centroid, 
	vars = c("xpos", "ypos", "Long", "Latt", "female", "age", "inumb", "length", "sweight", "pweight"), 
	rename = list(xpos="gridLongInd", ypos="gridLattInd", Long="gridLong", Latt="gridLatt"), 
	removeIntitialNA = TRUE){
	
	# Read the NORWECOM superindividual file:
	out <- readNcVarsAndAttrs(ncfile, vars=vars, rename=rename)
	# Add time:
	out <- addTimeHoursFrom1950(out, timedim="time")
	
	# Use only the data object, and discard the nc object:
	out <- out$data
	
	# Remove superindividuals which are initially NA:
	if(removeIntitialNA){
		out <- getSuperIndOfDay(out, day=NULL, maxValue=1e10, NAsByFirst=TRUE)
	}
	
	# Get Cartesian coordinates:
	out <- getSuperIndXY(out, centroid=centroid)
	
	# Return:
	out
}
#'
#' @export
#' @importFrom ncdf4 nc_open,ncvar_get,ncatt_get
#' @keywords internal
#' @rdname readNORWECOMbiomass
#' 
readNcVarsAndAttrs <- function(ncfile, vars, rename=NULL){
	getNcVarAndAttr <- function(y, nc){
		out <- ncvar_get(nc, y)
		att <- ncatt_get(nc, y)$long_name
		attr(out, "long_name") <- att
		out
	}
	# Read the NORWECOM superindividual file:
	nc <- nc_open(ncfile)
	data <- lapply(vars, getNcVarAndAttr, nc=nc)
	
	if(length(rename)){
		rename <- rename[intersect(names(rename), vars)]
		if(length(rename)){
			toBeRenamed <- match(names(rename), vars)
			vars[toBeRenamed] <- unlist(rename)
		}
	}
	
	names(data) <- vars
	
	list(nc=nc, data=data)
}
#'
#' @export
#' @keywords internal
#' @rdname readNORWECOMbiomass
#' 
addTimeHoursFrom1950 <- function(x, timedim="time"){
	# Add time from the dimensions:
	x$data$time <- as.POSIXct("1950-01-01", tz = "UTC") + x$nc$dim[[timedim]]$vals * 60^2
	
	x
}
#'
#' @export
#' @keywords internal
#' @rdname readNORWECOMbiomass
#' 
getBiomassXY <- function(biomass, centroid, proj="aeqd", units="kmi", x_0=0, y_0=0, ellps="WGS84", datum="WGS84"){
	# Convert the locations to x,y using the 'aeqd' projection and centered at 0, 68:
	thisLonLat <- data.frame(lon=c(biomass$Long), lat=c(biomass$Latt))
	xy_new <- Rstox::geo2xy(thisLonLat, list(proj=proj, units=units, lon_0=centroid[1], lat_0=centroid[2], x_0=x_0, y_0=y_0, ellps=ellps, datum=datum), data.frame.out=TRUE)

	# Define the grid to interpolate onto:
	biomass$X <- xy_new$x
	biomass$Y <- xy_new$y
	dim(biomass$X) <- dim(biomass$Long)
	dim(biomass$Y) <- dim(biomass$Long)
	
	biomass
}
#'
#' @export
#' @keywords internal
#' @rdname readNORWECOMbiomass
#' 
getSuperIndXY <- function(superInd, centroid, proj="aeqd", units="kmi", x_0=0, y_0=0, ellps="WGS84", datum="WGS84"){
	# Get the dimensions of the irregular geographical grid:
	dimxy <- dim(superInd$gridLong)
	# Get the sequence spanning the dimensions of the irregular grid (i.e., indices of the grid, which are the units of the positions xpos and ypos):
	seqx <- seq_len(dimxy[1])
	seqy <- seq_len(dimxy[2])
	# Define two lists of data, where x and y are the sequences of indices defined above in both lists, and the z is the actual longitude and latitude values respectively of the irregular grid:
	dataLon <- list(x=seqx, y=seqy, z=superInd$gridLong)
	dataLat <- list(x=seqx, y=seqy, z=superInd$gridLatt)
	
	# Define the matrix of points given as partial indices in the irregular grid:
	outpos <- cbind(c(superInd$gridLongInd), c(superInd$gridLattInd))
	
	# Interpolate each of longitude and latitude onto the superindividual positions:
	superInd$Long <- fields::interp.surface(dataLon, outpos)
	superInd$Latt <- fields::interp.surface(dataLat, outpos)
	
	# Reset dimensions:
	dimout <- dim(superInd$gridLongInd)
	dim(superInd$Long) <- dimout
	dim(superInd$Latt) <- dimout
	
	# Define the geographical coordinates of the superindividuals:
	#x$superIndLonLat <- data.frame(lon=c(outLon), lat=c(outLat))
	# lonlat <- interp::interpp(x=x$lonlat, z=z, xo=xo, yo=yo, output="points")
	
	# Convert to cartesian coordinates:
	thisLonLat <- data.frame(lon=c(superInd$Long), lat=c(superInd$Latt))
	xy_new <- Rstox::geo2xy(thisLonLat, list(proj=proj, units=units, lon_0=centroid[1], lat_0=centroid[2], x_0=x_0, y_0=y_0, ellps=ellps, datum=datum), data.frame.out=TRUE)
	
	superInd$X <- xy_new[,1]
	dim(superInd$X) <- dimout
	superInd$Y <- xy_new[,2]
	dim(superInd$Y) <- dimout
	
	superInd
}


#*********************************************
#*********************************************
#' Interpolate NORWECOM biomass onto log distances.
#'
#' @param x	The input NetCDF4 file.
#' 
#' @return
#'
#' @export
#' @importFrom interp interp
#' @rdname interpolateBiomassToTransects
#' 
interpolateBiomassToTransects <- function(biomass, transect, centroid, dayvec, margin=0.02, proj="aeqd", units="kmi", x_0=0, y_0=0, ellps="WGS84", datum="WGS84"){
	# Function for interpolating the biomass values onto the log distances for one specific day 'day', which is looked for in the vector 'dayvec', which is the days for each log distance:
	interpolateOneDay <- function(day, biomass, transect, dayvec, margin){
		# Get the locations of the requested day, and convert NA to 0:
		z <- c(biomass$Biom[,,day])
		z[is.na(z)] <- 0
	
		# Interpolate only based on the points in proximety to the output points:
		inxo <- which(dayvec==day)
		#margin <- 0.1
		rangex <- range(transect$x_mid[inxo])
		rangex <- rangex + c(-margin[1], margin[1])# * diff(rangex)
		rangey <- range(transect$y_mid[inxo])
		rangey <- rangey + c(-margin[2], margin[2])# * diff(rangey)
		valid <- c(biomass$X) >= rangex[1] & c(biomass$X) <= rangex[2] & c(biomass$Y) >= rangey[1] & c(biomass$Y) <= rangey[2]
		
	
		# Interpolate onto the output grid:
		#zo <- akima::interp(x=c(coords$x), y=c(coords$y), z=z, xo=xo, yo=yo)
		x <- c(biomass$X)[valid]
		y <- c(biomass$Y)[valid]
		z <- z[valid]
		xo=transect$x_mid[inxo]
		yo=transect$y_mid[inxo]
		zo <- interp::interp(x=x, y=y, z=z, xo=xo, yo=yo, output="points")
		out <- cbind(inxo, zo$z)
		
		return(out)
	}
	
	margin <- margin * c(diff(range(biomass$X)), diff(range(biomass$Y)))
	
	# Run through the 
	udays <- unique(dayvec)
	bio <- do.call(rbind, lapply(udays, interpolateOneDay, biomass=biomass, transect=transect, dayvec=dayvec, margin=margin))
	
	# Order by the days:
	bio[order(bio[,1]), 2]
}


#*********************************************
#*********************************************
#' Extract one day of valid superindividuals.
#'
#' @param x	The input NetCDF4 file.
#' 
#' @return
#'
#' @export
#' @keywords internal
#' @rdname getSuperIndOfDay
#' 
getSuperIndOfDay <- function(superInd, day=NULL, maxValue=1e10, NAsByFirst=FALSE){
	# Get the number of days:
	numDays <- length(superInd$time)
	if(length(day) == 0){
		day <- seq_len(numDays)
	}
	# Get the indices of variables with days at the second dimension:
	withDays <- which(sapply(superInd, function(x) identical(ncol(x), numDays)))
	# Get the indices of superindividuals with valid data of the given day:
	if(NAsByFirst){
		valid <- which(superInd[[withDays[1]]][, 1] < maxValue)
	}
	else{
		valid <- which(superInd[[withDays[1]]][, day] < maxValue)
	}
	
	# Extract the valid values of the given day:
	superInd[withDays] <- lapply(superInd[withDays], "[", valid, day)
	# Return:
	superInd
}


#*********************************************
#*********************************************
#' Interpolate NORWECOM biomass onto log distances.
#'
#' @param x	The input NetCDF4 file.
#' 
#' @return
#'
#' @export
#' @importFrom sp point.in.polygon
#' @rdname getTotalBiomass
#' 
getTotalBiomass <- function(superInd, stratum, days=NULL, type=c("biomass", "length")){
	
	sumBiomass <- function(d, inside){
		sum(d$pweight[inside] * d$inumb[inside])
	}
	
	meanLengthByTS <- function(d, inside){
		sqrt(weighted.mean(d$length[inside]^2, d$inumb[inside]))
	}
	
	
	# Function for getting the biomass of all strata of one day:
	getTotalBiomassStratum <- function(day, superInd, stratum){
		# Get the indices in the variable in thisSuperInd which are inside the straum:
		getBiomassInside <- function(stratum, thisSuperInd){
			if(length(stratum)){
				inside <- point.in.polygon(
					#point.x = thisSuperInd$X, 
					#point.y = thisSuperInd$Y, 
					point.x = thisSuperInd$Long, 
					point.y = thisSuperInd$Latt, 
					pol.x = stratum[,1], 
					pol.y = stratum[,2])
				inside <- which(inside > 0)
			}
			else{
				inside <- seq_along(thisSuperInd$X)
			}
			
			# Get the total biomass:
			if(type[1] == "biomass"){
				total <- sumBiomass(thisSuperInd, inside)
			}
			else if(type[1] == "length"){
				total <- meanLengthByTS(thisSuperInd, inside)
			}
			else{
				stop("Invalid type in getTotalBiomass()")
			}
			#total <- sum(thisSuperInd$pweight[inside] * thisSuperInd$inumb[inside])
			total
		}
		
		# Get valid values of the given day:
		thisSuperInd <- getSuperIndOfDay(superInd, day)
	
		# Get the total biomass of the entire stock:
		total <- getBiomassInside(NULL, thisSuperInd=thisSuperInd)
		
		# Get the biomass of each stratum:
		byStratum <- sapply(stratum, getBiomassInside, thisSuperInd=thisSuperInd)
		# Return the biomass in the strata, the sum of all strata, and the total biomass:
		c(byStratum, sum(byStratum), total)
	}
	
	# Get a sequence of the days:
	if(length(days)==0){
		days <- seq_along(superInd$time)
	}
	
	# Get the biomass for all days and combine to a data frame:
	biomass <- lapply(days, getTotalBiomassStratum, superInd=superInd, stratum=stratum)
	biomass <- do.call("rbind", biomass)
	biomass <- as.data.frame(biomass)
	biomassNames <- c(if(length(names(stratum))) names(stratum) else seq_along(stratum), "AllStrata", "Total")
	names(biomass) <- biomassNames
	# Return:
	biomass
}


#*********************************************
#*********************************************
#' Simulate trawl stations given NORWECOM superindividuals.
#'
#' @param x	The input NetCDF4 file.
#' 
#' @return
#'
#' @export
#' @importFrom Rstox getSeedV
#' @rdname simulateTrawl
#'
simulateTrawl <- function(superInd, fishStation, seed=0, radius=10, nn=NULL, N=100, lengthunit=2){
	# Small funciton for calcualting the Eucledian distance given x, y, ...:
	edist <- function(...){
		x <- do.call(cbind, list(...))
		sqrt(rowSums(x^2))
	}
	mround <- function(x, base){
		base * round(x / base)
	}
	getValidLengthunit <- function(lengthunit){
		# From getNMDinfo("Lengdeenhet"):
		# name description
		#    1        1 mm
		#    2        5 mm
		#    3        1 cm
		#    4        3 cm
		#    5        5 cm
		#    6      0.5 mm
		#    7      0.1 mm
		validLengthunit <- c(1e-3, 5e-3, 10e-3, 30e-3, 50e-3, 0.5e-3, 0.1e-3)
		validLengthunit[as.numeric(lengthunit)]
	}
	
	# Function for simulating one trawl sample based on the surrounding superindividuals:
	simulateTrawlOne <- function(ind, superInd, fishStation, seedV, radius=10, nn=NULL, N=100, validLengthunit=2){
		
		# Get the day of the trawl station:
		day <- findInterval(fishStation$starttime[ind], superInd$time)
		#thisSuperInd <- lapply(superInd[c("X", "Y", "inumb", "female", "age", "length", "pweight")], function(x) x[, day])
		thisSuperInd <- getSuperIndOfDay(superInd, day)
		
		# First find the superindividuals within the radius around the station:
		d <- edist(
			thisSuperInd$X - fishStation$x_mid[ind], 
			thisSuperInd$Y - fishStation$y_mid[ind]
		)
		
		# Get the superindividuals inside the radius:
		# Also discard invalid superindividuals:
		if(length(nn)){
			inside <- which(thisSuperInd$pweight < 1e10)
			inside <- order(d[inside])[seq_len(nn)]
		}
		else{
			inside <- which(d <= radius & thisSuperInd$pweight < 1e10)
		}
		superIndCount = length(inside)
		
		# Get the number of fish of the valid superindividuals:
		size <- thisSuperInd$inumb[inside]
		
		# We could consider weighting by the distance in the below probability of selecting a fish from the superindividual:
		prob <- size / sum(size)
		set.seed(seedV[ind])
		print(superIndCount)
		if(superIndCount == 0){
			return(data.frame())
		}
		else if(superIndCount == 1){
			s <- rep(inside, N)
		}
		else{
			s <- sample(inside, size=N, prob=prob, replace=TRUE)
		}
		
		# Generate the individual samples:
		individual <- data.frame(
			serialno = fishStation$serialno[ind], 
			superInd = s,
			superIndCount = superIndCount,
			specimenno = seq_len(N), 
			lengthunit = lengthunit, 
			length = thisSuperInd$length[s],
			sex = 2 - thisSuperInd$female[s], # The definition of biotic xml has male=2 and female=1. In the Norwecom model male=0 and female=1.
			weight.individual = thisSuperInd$pweight[s],
			age..agedetermination.1 = thisSuperInd$age[s]
		)
		
		# Round off length to the lengthunit (given in cm in the NORWECOM model output):
		individual$length <- individual$length * 1e-2
		individual$length <- mround(individual$length, validLengthunit)
		# Round off weight to grams (given in grams in the MORWECOM output):
		individual$weight.individual <- individual$weight.individual * 1e-3
		individual$weight.individual <- round(individual$weight.individual, digits=3)
		
		
		individual
	}
	
	# Get the length unit from the reference data:
	### validLengthunit <- getNMDinfo("Lengdeenhet")
	### validLengthunit <- validLengthunit$description[validLengthunit$name == lengthunit]
	### # Extract the number and unit:
	### validLengthunit <- strsplit(validLengthunit, " ")[[1]]
	### validLengthunit <- as.numeric(validLengthunit[1]) * ifelse(validLengthunit[2] == "cm", 10, 1)
	### # Convert to meters:
	### validLengthunit <- validLengthunit * 1e-3
	validLengthunit <- getValidLengthunit(lengthunit)
	
	print("trawlsim")
	# Draw seeds:
	nstations <- nrow(fishStation)
	seedV <- Rstox::getSeedV(seed, nstations)
	
	# Draw trawl samples:
	individual <- lapply(seq_len(nstations), simulateTrawlOne, superInd=superInd, fishStation=fishStation, seedV=seedV, radius=radius, nn=nn, N=N, validLengthunit=validLengthunit)
	superIndCount <- sapply(individual, function(x) if(length(x$superIndCount)) head(x$superIndCount, 1) else 0)
	individual <- do.call("rbind", individual)
	
	# Merge the fish stations and individuals
	out <- list(biotic=merge(fishStation, individual), superIndCount=superIndCount)
	out
}
#'
#' @export
#' @rdname simulateTrawl
#'
drawTrawlStationInd <- function(nasc, nstations=1, seed=0, probfact=0){
	# Generate the cummulative probability distribution as the cumsum of the nasc, and draw based on this distribution:
	nasc0 <- nasc
	nasc0[is.na(nasc0)] <- 0
	
	prob <- nasc0 / sum(nasc0)
	# Add a small probability to all positions:
	prob <- prob + probfact / length(prob)
	
	if(nstations > length(prob)){
		warning("The number of trawl stations to draw exceeds the number og log distances. All log distances drawn (without replacement).")
		nstations <- length(prob)
	}
	
	ind <- sample(seq_along(prob), nstations, prob=prob, replace=FALSE)
	# Return:
	ind
}


#*********************************************
#*********************************************
#' Funciton for converting from a biomass density to nautical area scattering coefficient NASC (linear values, not dB).
#'
#' @param Wg	The biomass area density in units of g/m^2 (gram per square meter in the horizontal plane). 
#' @param a,b	Parameters of the length-weight relationship Wg = a * Lcm^b, where Wg is the weight in grams and Lcm is the length in cm. Typical values are e.g., a = 0.01 and b = 3.
#' @param m,TS0	The parameters of the target strength-length relationship TS = m * log10(Lcm) + TS0, typically m = 20 and TS0 = -71.9 (herring, from Foote, K. G. 1987. Fish target strengths for use in echo integrator surveys. Journal of the Acoustical Society of America, 82: 981– 987.)
#' 
#' @return
#'
#' @export
#' @rdname biomass2nasc
#'
biomass2nasc <- function(Wg, LcmOne, a, b, m=20, TS0=-71.9){
	# Function for converting from length in cm to weight in g:
	Lcm2Wg <- function(Lcm, a, b){
		# Standard length - weight relationship:
		# 	w = a * L^b
		a * Lcm^b
	}
	# Target strength of a fish with length given in cm:
	TS <- function(Lcm, m=20, TS0=-71.9){
		TS <- m * log10(Lcm) + TS0
	}
	# Backscattering cross section of a fish with length given in cm:
	sigmabs <- function(Lcm, m=20, TS0=-71.9){
		10^(TS(Lcm=Lcm, m=m, TS0=TS0) / 10)
	}
	
	# Get the weight of one fish:
	WgOne <- Lcm2Wg(Lcm=LcmOne, a=a, b=b)
	
	# Get the fractional number of fish:
	numFish <- Wg / WgOne
	
	# Get the backscattering of one fish:
	sigmabsOne <- sigmabs(LcmOne, m=m, TS0=TS0)
	
	# Convert to NASC:
	nasc <- 4 * pi * 1852^2 * numFish * sigmabsOne
	
	return(nasc)
}
biomass2nasc_error <- function(Wg, a, b, m=20, TS0=-71.9){
	# Function for converting from weight in g to length in cm:
	Wg2Lcm <- function(Wg, a, b){
		# Standard length - weight relationship:
		# 	w = a * L^b
		(Wg / a)^(1 / b)
	}
	# Target strength of a fish with length given in cm:
	TS <- function(Lcm, m=20, TS0=-71.9){
		TS <- m * log10(Lcm) + TS0
	}
	# Backscattering cross section of a fish with length given in cm:
	sigmabs <- function(Lcm, m=20, TS0=-71.9){
		10^(TS(Lcm=Lcm, m=m, TS0=TS0) / 10)
	}
	
	# Get the length in cm:
	Lcm <- Wg2Lcm(Wg=Wg, a=a, b=b)
	# Convert to NASC:
	nasc <- 4 * pi * 1852^2 * sigmabs(Lcm=Lcm, m=m, TS0=TS0)
	
	return(nasc)
}


plotPELFOSS <- function(
	p, 
	x, 
	biomass, 
	day, 
	superInd = NULL, 
	ncolour=20, 
	maxBiom=700, 
	maxNasc=4000, 
	col=list(h=c(0.6, 0.98), s=c(0.35, 1), v=c(0.9, 0.3)), 
	logColscale = TRUE, 
	firstcol=NA, 
	NASCthr=0.001, 
	NASCexp=2, 
	NASCmax_size=2, 
	biomassAlpha=0.03, 
	biomassShape=20, 
	trawlSize=2){
	
	biomassOfDay <- data.frame(
		Long = c(biomass$Long), 
		Latt = c(biomass$Latt), 
		Biom = c(biomass$Biom[,,day])
	)
	
	if(logColscale){
		BiomSeq <- seq(0, 10*log10(maxBiom), length.out=ncolour)
		BiomSeq <- 10^(BiomSeq/10)
	}
	else{
		BiomSeq <- seq(0, maxBiom, length.out=ncolour)
	}
	
	if(is.list(col) && all(c("h", "s", "v") %in% names(col))){
		col <- hsv(h=seq(col$h[1], col$h[2], l=ncolour), s=seq(col$s[1], col$s[2], l=ncolour), v=seq(col$v[1], col$v[2], l=ncolour))
	}
	
	if(length(firstcol)){
		col[1] <- firstcol
	}
	colorInterval <- findInterval(biomassOfDay$Biom, BiomSeq)
	
	if(length(superInd)){
		p <- p + geom_point(data=as.data.frame(getSuperIndOfDay(superInd, 51)[c("Long", "Latt")]), aes(x=Long, y=Latt), shape=2, color="black", size=0.5)
	}
	
	# Add biomass to the transect plot:
	for(i in seq_len(ncolour)){
		p <- p + geom_point(data=biomassOfDay[colorInterval==i, ], aes(x=Long, y=Latt), colour=col[i], alpha=biomassAlpha, shape=biomassShape)
	}
	
	# Add NASC values larger than NASCthr of the fraction of NASC relative to maxNASC:
	#browser()
	#if(logColscale){
	#	pointSize <- 10*log10(nasc)
	#}
	#else{
		pointSize <- (x$Transect$nasc/maxNasc)^NASCexp
		#}
	#
	pointSize[pointSize < NASCthr] <- NA
	x$Transect <- cbind(x$Transect, pointSize=pointSize)
	p <- p + geom_point(data=x$Transect, aes(x=lon_mid, y=lat_mid, size=pointSize), shape=20)  +  scale_size_area(max_size=NASCmax_size, guide=FALSE)
	
	# Plot the trawl stations
	p <- p + geom_point(data=x$Transect[x$Transect$trawl, ], aes(x=lon_mid, y=lat_mid), shape=42, color="red", size=trawlSize)
	
	print(p)
	p
}


#*********************************************
#*********************************************
#' Simulate survey based on NORWECOM data.
#'
#' @param projectName		The biomass area density in units of g/m^2 (gram per square meter in the horizontal plane). 
#' @param xmlOutputDir		The biomass area density in units of g/m^2 (gram per square meter in the horizontal plane). 
#' @param biomass			The path to a NORWECOM NetCDF4 file with biomass in grams per square meter in a irregular geographical grid. The file should contain the variables "Biom", "Long" and "Latt".
#' @param superInd			The path to a NORWECOM NetCDF4 file with superindividuals. The file should contain the variables "gridLongInd", "gridLattInd", "gridLong", "gridLatt", "female", "age", "inumb", "length", "sweight" and "pweight".
#' @param stratum			The path to a file with the stratum polygons given as a two column matrix with stratum name in the first column and a MULTIPOLYGON wkt string in the second conlumn.
#' @param startdate			The start date of the survey, given as "%d/%m", e.g., 31 January is "31/1".
#' @param dayshift			A numberic giving the number of days to shift the start date 
#' @param centroid			The centroid of the data, given in longitude, latitude.
#' @param seeds				A list of seeds used in the funciton, including seeds ('transect') for drawing transects using surveyPlanner(), ('trawl') for drawing trawl stations along the transects with probability as a funciton of the NASC (see \code{probfact}), and ('bootstrap') for getting the final estimate using runBootstrap() (see \code{nboot}).
#' @param xsd				A named list of xsd versions of the acoustic and biotic file format.
#' @param nTrawl			The number of trawls to draw. Implies a penalty on the total transect time by \code{hoursPerTrawl}.
#' @param type,knots,equalEffort,bearing,distsep,margin	See \code{\link{surveyPlanner}}
#' @param tsn				The tsn code of the species.
#' @param m,TS0				The parameters of the target strength-length relationship TS = m * log10(Lcm) + TS0, typically m = 20 and TS0 = -71.9 (herring, from Foote, K. G. 1987. Fish target strengths for use in echo integrator surveys. Journal of the Acoustical Society of America, 82: 981– 987.)
#' @param platform			The platform to use, defaulted to G.O.Sars. Only kept for cosmetic reasons.
#' @param distance,sweepWidth	The trawled distance and the seew width of the simulated trawl.
#' @param probfact			A numeric indicating an addition in the probability of selecting a log distance for trawling. I.e., add probfact / numberOfTransects to each log distance probability NASC / sum(NASC), and normalize to toal probability = 1.
#' @param radius			The radius around the trawl station within which individuals are drawn from the superindividuals.
#' @param N					The number of individuals to draw for each trawl station.
#' @param ...				Arguments passed to \code{\link{surveyPlanner}}. 
#' 
#' @return
#'
#' @export
#' @rdname biomass2tsb
#'
biomass2tsb <- function(
	# For the StoX project and writing XML files:
	projectName, xmlOutputDir, 
	# For the biomass and superindividuals and other global options:
	biomass, superInd, stratum, startdate = "31/1", dateshift = 0, centroid = c(0, 68), seeds = list(transect=0, trawl=1, bootstrap=2), nboot=10, xsd = list(acoustic="1", biotic="1.4"),
	# For transects and NASC:
	nTrawl = 50, hours = list(240), type = "RectEnclZZ", knots = 10, equalEffort = TRUE, bearing = "along", distsep = 1, margin = 0.1, tsn = 161722, m = 20, TS0 = -71.9, 
	# For trawl:
	platform = 4174, distance = 5, sweepWidth = 25, probfact = 1, radius=10, 
	N=100, cores=list(biotic=1, acoustic=1, bootstrap=1), 
	...){
	
	# Not used, calculate the pure transect time outside of the function, and do not consider the time used by trawling as a delay of the acoustic logging.
	# hoursPerTrawl = 2, 
	#' @param daysOfSurvey		The number of days reserved for the survey.
		
		
	# Hard coded:
	pel_ch_thickness = 100
	##### NOTE 4: Find the meaning of acocat in some file from Rolf, but it has no effec unless filtered on: #####
	acocat = 99
	# The TS-length relations require 38 kHz:
	freq = 38000
	# For now we only consider one channel, and that this is a pelagic channel:
	nChannels = 1
	channelType = "P"
	byStratum = FALSE
	keepTransport = FALSE
	missiontype = 4
	missionnumber = 1
	samplenumber = 1
	
	if(!is.list(cores)){
		cores <- as.list(rep(cores, length.out=3))
		names(cores) <- c("biotic", "acoustic", "bootstrap")
	}
	
	
	#########################################################
	##### 1. Read NORWECOM files (convert to Cartesian) #####
	#########################################################

	# Read the biomass:
	##### NOTE 2: Remove the rename parameter once the files have the requested variable names, also in the readNORWECOMsuperind(): #####
	if(is.character(biomass) && file.exists(biomass)){
		cat("Read biomass file...\n")
		biomass <- readNORWECOMbiomass(
			biomass, centroid=centroid, 
			#vars=c("HERbiom", "Long", "Latt"), 
			#rename=list(HERbiom="Biom")
			vars=c("Biom", "Long", "Latt")
		)
	}


	# Read the superindividuals:
	##### NOTE 3: Remove the rename parameter once the files have the requested variable names, also in the readNORWECOMsuperind(): #####
	if(is.character(superInd) && file.exists(superInd)){
		cat("Read superInd file...\n")
		superInd <- readNORWECOMsuperind(
			superInd, centroid=centroid, 
			vars = c("xpos", "ypos", "Long", "Latt", "female", "age", "inumb", "length", "sweight", "pweight"), 
			rename=list(xpos="gridLongInd", ypos="gridLattInd", Long="gridLong", Latt="gridLatt")
		)
	}

	#########################################################
	#########################################################


	############################################################################
	##### 2. Run surveyPlanner(), extract NASC, return acoustic data frame #####
	############################################################################

	cat("Run surveyPlanner()...\n")
	
	##### Parameters: #####
	# For the surveyPlanner():
	#timePenalty <- nTrawl * hoursPerTrawl
	#totalhours <- 24 * totalhours - timePenalty
	#hours = list(24 * daysOfSurvey)

	##### surveyPlanner: #####
	year = format(as.Date(head(biomass$time, 1)),"%Y")
	startdate <- paste(year, startdate, sep="/")
	startdate <- as.Date(startdate, format="%Y/%d/%m")
	t0 <- as.POSIXct(paste(startdate, "00:00:00"), format="%Y-%m-%d %H:%M:%S", tz="UTC")
	# Add the dateshift:
	t0 <- t0 + dateshift * 24 * 60 * 60

	transects <- surveyPlanner(
		projectName = stratum, 
		type = type, 
		bearing = bearing, 
		hours = hours, 
		t0 = t0, 
		knots = knots, 
		seed = seeds$transect, 
		equalEffort = equalEffort, 
		distsep = distsep, 
		margin = margin, 
		byStratum = byStratum, 
		keepTransport = keepTransport, 
		centroid = centroid, 
		...
	)
	
	
	##### Create NASC values: #####
	# Interpolate directly onto the log distances:
	dayvec <- findInterval(transects$Transect$time_start, biomass$time)
	Bg <- interpolateBiomassToTransects(biomass, transect=transects$Transect, centroid=centroid, dayvec=dayvec)
	# NORWECOM biomass is given in grams
	#Wkg <- Wg * 1e-3

	# Get condition exponent from the superindividuals:
	s <- getSuperIndOfDay(superInd=superInd, day=1)
	Lcm <- s$length
	Wg <- s$sweight
	#pW <- s$pweight * 1e-3
	#plot((Lcm^3), Wg)
	b_true <- 3
	a_true <- Wg / Lcm^3
	summary(a_true)
	a_true <- median(a_true)

	# Get the typical length of the fish, by a weighted average of the superindividuals in the survey region:
	id <- double(length(transects$Input$lonlat_stratumSP))
	# Get the union of the SpatialPolygons of the strata:
	stratumUnion <- maptools::unionSpatialPolygons(transects$Input$lonlat_stratumSP, id)
	# Select the fist in case there are holes in the union:
	stratumUnionMatrix <- getMatrixList(stratumUnion)[1]

	midDayOfSurvey <- as.numeric(strftime(median(transects$Tra$time_mid), format = "%j"))
	LcmMean <- getTotalBiomass(superInd=superInd, stratum=stratumUnionMatrix, day=midDayOfSurvey, type="length")$AllStrata

	
	# Get the NASC values from the biomass horizontal area density:
	#nasc <- biomass2nasc(Wg, a=a_true, b=b_true, m=m, TS0=TS0)
	nasc <- biomass2nasc(Bg, LcmOne=LcmMean, a=a_true, b=b_true, m=m, TS0=TS0)
	if(!any(nasc>0)){
		warning("No biomass on the transects")
		return(NULL)
	}
	
	
	transects$Transect <- cbind(transects$Transect, nasc=nasc)


	# Create the acoustic data frame to write to LUF20 file:
	numt <- nrow(transects$Transect)
	allZeros <- double(numt)
	allOnes <- allZeros + 1
	transceiver <- length(freq)
	
	acoustic <- data.frame(
		#distance_list = NA, 
		#ch_type = NA, 
		cruise = transects$Transect$cruise[1], 
		integrator_dist = transects$Transect$dist_stop - transects$Transect$dist_start, 
		pel_ch_thickness = rep(pel_ch_thickness, length.out=numt),
		log_start = transects$Transect$log_start,
		lon_start = transects$Transect$lon_start,
		lon_stop = transects$Transect$lon_stop,
		lat_start = transects$Transect$lat_start,
		#start_time = transects$Transect$time_start,
		start_time = format(transects$Transect$time_start),
		num_pel_ch = allOnes,
		upper_interpret_depth = allZeros,
		upper_integrator_depth = allZeros, 
		acocat = acocat, 
		freq = freq,
		type = channelType,
		transceiver = transceiver,
		sa..ch.1 = round(nasc, digits=6), 
		nasc = nasc, stringsAsFactors=FALSE
	)
	
	############################################################################
	############################################################################


	############################################
	##### 3. Get total theoretical biomass #####
	############################################

	cat("Get total biomass...\n")
	
	totBiom <- getTotalBiomass(superInd=superInd, stratum=transects$Input$lonlat_stratum, type="biomass")
	
	############################################
	############################################


	################################################################
	##### 4. Simulate trawl stations, return biotic data frame #####
	################################################################

	cat("Draw trawl stations...\n")
	
	# Draw trawl stations (get the indices of the log distances with trawl):
	trawlInd <- drawTrawlStationInd(nasc, nTrawl, probfact=probfact)
	# Add to the transect matrix:
	transects$Transect$trawl <- FALSE
	transects$Transect$trawl[trawlInd] <- TRUE
	
	# The following gives the required attributes: readHIXSD("1.4", "biotic")$attrs_required.
	# Here we include the 'mission' info in the fish station data frame:
	fishStation <- data.frame(
		missiontype = missiontype, 
		cruise = transects$Transect$cruise[1], 
		year = format(as.Date(head(transects$Transect$time_start, 1)),"%Y"),
		platform = platform,
		missionnumber = missionnumber, 
		# Ordinary fish station variables and attributes:
		serialno = seq_along(trawlInd), 
		starttime = transects$Transect$time_mid[trawlInd],
		longitudestart = transects$Transect$lon_start[trawlInd],
		latitudestart = transects$Transect$lat_start[trawlInd],
		x_mid = transects$Transect$x_mid[trawlInd],
		y_mid = transects$Transect$y_mid[trawlInd], 
		species = tsn, 
		samplenumber = samplenumber, 
		distance = distance, stringsAsFactors=FALSE
	)

	# Simulate the trawls:
	biotic <- simulateTrawl(superInd, fishStation, seed=seeds$trawl, radius=radius, N=N)
	superIndCount <- biotic$superIndCount
	biotic <- biotic$biotic
	
	### Add trawl info to the transects$Transect: ###
	# Add superindividual count for the trawl stations:
	transects$Transect$superIndCount <- NA
	transects$Transect$superIndCount[trawlInd] <- superIndCount
	# Add serialno for the trawl stations:
	transects$Transect$serialno <- NA
	transects$Transect$serialno[trawlInd] <- fishStation$serialno
	
	################################################################
	################################################################


	# The plotting should be revised, and possibly put inside the plotStratum(), and maybe put outside of the function:
	
	#########################
	##### 5. Visualize: #####
	#########################

	#p <- plotStratum(transects)
    #
	## Plot with the trals and NASC:
	#p1 <- plotPELFOSS(
	#	p, 
	#	transects,
	#	biomass,
	#	day=222, 
	#	trawlInd=trawlInd, 
	#	ncolour=40, 
	#	maxBiom=700, 
	#	maxNasc=4000, 
	#	#col=list(h=c(0.95, 0.98), s=c(0.35, 1), v=c(0.9, 0.5)), 
	#	NASCthr=0.001, 
	#	NASCexp=2
	#)

	#########################
	#########################


	########################
	##### 6. write xml #####
	########################

	tempfile_biotic <- file.path(xmlOutputDir, "biotic.xml")
	tempfile_acoustic <- file.path(xmlOutputDir, "acoustic.xml")

	cat("Write biotic xml...\nTime used:\n")
	temp <- system.time(writeBioticXML(biotic, tempfile_biotic, xsd=xsd$biotic, cores=cores$biotic))
	cat("Time used:\n")
	print(temp)
	
	cat("Write acoustic xml...\nTime used:\n")
	temp <- system.time(writeAcousticXML(acoustic, tempfile_acoustic, xsd=xsd$acoustic, cores=cores$acoustic))
	cat("Time used:\n")
	print(temp)
	
	
	########################
	########################


	#######################################
	##### 7. Generate and run project #####
	#######################################

	cat("Create and run the StoX project...\n")
	
	# Create the NORWECOM project:
	input <- file.path(getProjectPaths(projectName)$projectPath, "input", c("acoustic", "biotic"))
	names(input) <- basename(input)
	# define the paths to the input files:
	file_biotic <- file.path(input["biotic"], "biotic.xml")
	file_acoustic <- file.path(input["acoustic"], "acoustic.xml")


	model <- list(
		# Baseline: 
		"ReadProcessData", 
		ReadAcousticXML = list(
			FileName1 = file.path(input["acoustic"], "acoustic.xml")
		), 
		NASC = list(
			AcousticData = "ReadAcousticXML", 
			LayerType = "WaterColumn"
		), 
		ReadBioticXML = list(
			FileName1 = file.path(input["biotic"], "biotic.xml")
		), 
		StationLengthDist = list(
			BioticData = "ReadBioticXML", 
			LengthDistType = "PercentLengthDist"
		), 
		RegroupLengthDist = list(
			LengthDist = "StationLengthDist", 
			LengthInterval = "1.0"
		), 
		StratumArea = list(
			ProcessData = "ReadProcessData", 
			AreaMethod = "Accurate"
		), 
		DefineAcousticPSU = list(
			ProcessData = "ReadProcessData", 
			AcousticData = "ReadAcousticXML", 
			DefinitionMethod = "UseProcessData"
		),
		MeanNASC = list(
			ProcessData = "ReadProcessData", 
			NASC = "NASC", 
			SampleUnitType = "PSU"
		), 
		BioStationAssignment = list(
			ProcessData = "ReadProcessData", 
			BioticData = "ReadBioticXML", 
			AssignmentMethod = "UseProcessData", 
			EstLayers = "1~PELBOT"
		), 
		TotalLengthDist = list(
			ProcessData = "ReadProcessData", 
			LengthDist = "RegroupLengthDist"
		), 
		SweptAreaDensity = list(
			ProcessData = "ReadProcessData", 
			SweptAreaMethod = "LengthDependent", 
			BioticData = "ReadBioticXML", 
			LengthDist = "TotalLengthDist", 
			DistanceMethod = "FullDistance",
			sweepWidthMethod = "Constant", 
			sweepWidth = sweepWidth
		), 
		AcousticDensity = list(
			ProcessData = "ReadProcessData", 
			LengthDist = "TotalLengthDist", 
			NASC = "MeanNASC", 
			m = m, 
			a = TS0
		),  
		MeanDensity = list(
			ProcessData = "ReadProcessData", 
			Density = "AcousticDensity", 
			SampleUnitType = "Stratum"
		), 
		SumDensity = list(
			Density = "MeanDensity"
		), 
		Abundance = list(
			Density = "SumDensity", 
			PolygonArea = "StratumArea"
		), 
		IndividualDataStations = list(
			ProcessData = "ReadProcessData", 
			Abundance = "Abundance"
		), 
		IndividualData = list(
			BioticData = "ReadBioticXML", 
			IndividualDataStations = "IndividualDataStations"
		), 
		SuperIndAbundance = list(
			Abundance = "Abundance", 
			IndividualData = "IndividualData", 
			ProcessData = "ReadProcessData", 
			AbundWeightMethod = "Equal"
		), 
		"WriteProcessData", 
		# Baseline report: 
		#FillMissingData = list(
		#	SuperIndividuals = "SuperIndAbundance", 
		#	FillVariables = "ImputeByAge", 
		#	Seed = "1", 
		#	FillWeight = "Regression"
		#), 
		#EstimateByPopulationCategory = list(
		#	SuperIndividuals = "FillMissingData", 
		#	LengthInterval = "2.0", 
		#	Scale = "1000", 
		#	Dim1 = "LenGrp", 
		#	Dim2 = "age", 
		#	Dim3 = "SpecCat"
		#)
		EstimateByPopulationCategory = list(
			SuperIndividuals = "SuperIndAbundance", 
			LengthInterval = "1.0", 
			Scale = "1000", 
			Dim1 = "LenGrp", 
			Dim2 = "age", 
			Dim3 = "SpecCat"
		)
	)

	# Create the project skeleton:
	createProject(projectName, model=model, ow=TRUE)


	# Copy the input files to the project input directory:
	file.copy(tempfile_biotic, file_biotic, overwrite=TRUE)
	file.copy(tempfile_acoustic, file_acoustic, overwrite=TRUE)



	##### Modify and run the StoX project:
	# Get the path to the project.xml file_
	projectXML <- getProjectPaths(projectName)$projectXML

	# Write the stratum polygons, the edsupsu, the psustratum, the suassignment, and the bioticassignment to the project.xml file directly:
	insertStratumpolygon(transects$Input$lonlat_stratum, file=projectXML)

	insertEdsupsu(transects$Transect, file=projectXML)

	insertPsustratum(transects$Transect, file=projectXML)

	insertSuassignment(transects$Transect, file=projectXML)

	# Write the bioticassignment
	
	#Stations <- data.frame(
	#	stratum = transects$Stratum$stratum, 
	#	stationID = paste(biotic$cruise, biotic$serialno, sep="/"), stringsAsFactors=FALSE
	#)
	
	hasFish <- which(transects$Transect$superIndCount > 0)
	Stations <- data.frame(
		stratum = transects$Transect$stratum[hasFish], 
		stationID = paste(transects$Transect$cruise[hasFish], transects$Transect$serialno[hasFish], sep="/"), stringsAsFactors=FALSE
	)
	insertBioticassignment(Stations, file=projectXML)


	# Reopen and save the project. This is required to order the process data properly:
	reopenProject(projectName)
	saveProject(projectName)

	# Get the baseline:
	#g <- runBaseline(projectName, exportCSV=TRUE)

	# Bootstrap:
	boot <- runBootstrap(projectName, nboot=nboot, seed=seeds$bootstrap, bootstrapMethod="AcousticTrawl", cores=cores$bootstrap)

	# Get the total weight with precision:
	#reports <- getReports(projectName)
	#reports <- getReports(projectName, grp1=NULL)
	#reports <- getReports(projectName, var="Weight")
	TSB <- getReports(projectName, var="Weight", grp1=NULL)

	#   TSB Ab.Sum.5% Ab.Sum.50% Ab.Sum.95% Ab.Sum.mean Ab.Sum.sd Ab.Sum.cv
	# 1 TSB   4124917    6376444    9903694     6864052   2228430 0.3246522
	#######################################
	#######################################
	
	
	
	
	
	list(TSB=TSB, totBiom=totBiom, superInd=superInd, transects=transects, biomass=biomass, t0=t0, daysOfSurvey=unique(dayvec), midDayOfSurvey=midDayOfSurvey, projectName=projectName)
}



# Herring_IESNS:		International ecosystem survey in the Nordic Seas
# Herring_NASSHS:		Norwegian acoustic spring spawning herring survey 
# Mackerel_IESSNS:	The International Ecosystem Summer Survey in the Nordic Seas


# Get the paths:
getNorwecomPaths <- function(dir, survey = "Herring_IESNS", year = 2010, res = 4){
	species <- strsplit(survey, "_", fixed=TRUE)[[1]][1]
	data <- list.files(file.path(dir, "data", species, paste0("Res_", res, "km"), year), full.names=TRUE)
	type <- sapply(strsplit(basename(data), "_", fixed=TRUE), head, 1)
	biomass <- data[tolower(type) == "biomass"]
	superInd <- data[tolower(type) == "superind"]
	stratum <- list.files(file.path(dir, "stratum", survey), full.names=TRUE)[1]
	list(biomass=biomass, superInd=superInd, stratum=stratum, species=species, survey=survey)
}



setSurveyParameters <- function(survey = "Herring_IESNS", reverse=FALSE){
	if(survey == "Herring_IESNS"){
		nstrata <- 4
		strata <- as.character(c(4, 3, 1, 2))
		rev <- c(FALSE, TRUE, FALSE, FALSE)
		tsn <- 161722
		m <- 20
		TS0 <- -71.9
		hours <- list(28*24) # Four weeks
		knots <- 15 
		centroid <- c(0, 68)
		startdate <- "1/5"
		trawlDens=0.013
	}
	else if(survey == "Herring_NASSHS"){
		nstrata <- 13
		strata <- as.character(c(3, 2, 4, 5, 6, 7, 8, 17, 10, 9, 11, 13, 14))
		rev <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
		tsn <- 161722
		m <- 20
		TS0 <- -71.9
		hours <- list(10 * 24)
		knots <- 20
		centroid <- c(10, 75)
		startdate <- "15/2"
		trawlDens=0.03
	}
	else if(survey == "Mackerel_IESSNS"){
		nstrata <- 10
		strata <- as.character(c(11, 10, 4, 5, 6, 3, 2, 1, 7, 9))
		rev <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,FALSE, TRUE)
		#strata <- c(4, 3, 1, 2)
		#rev <- c(FALSE, TRUE, FALSE, FALSE)
		tsn <- 172414
		m <- 20
		TS0 <- -71.9
		hours = list(28*24) # Same time spent as for the Herring_IESNS (approximately 4 weeks)
		knots = 2 * 15 # Twice the speed of the Herring_IESNS to account for approximately double area.
		centroid <- c(10, 65)
		startdate <- "1/7"
		trawlDens=0.013
	}
	else {
		stop(paste("Survey", survey, "not implemented (use 'Herring_IESNS', 'Herring_NASSHS' or 'Mackerel_IESSNS')."))
	}
	if(reverse){
		strata <- rev(strata)
		rev <- !rev
	}
	
	out <- list(
		nstrata = nstrata, 
		strata = strata,
		rev = rev,
		tsn = tsn,
		m = m,
		TS0 = TS0,
		hours = hours,
		knots = knots,
		centroid = centroid,
		startdate = startdate, 
		trawlDens = trawlDens
	)
	
	# Chech that the length of strata matches the nstrata:
	if(out$nstrata != length(out$strata)){
		warning("Mismatch between nstrata and strata.")
	}
	
	# Add number of trawls from the trawling density input:
	sumHours <- if(is.list(out$hours)) unlist(out$hours) else sum(rep(out$hours, length.out=out$nstrata))
	out$nTrawl <- round(out$trawlDens * sumHours * out$knots)
	
	return(out)
}








# To change for each survey:

# 1. Seeds: 10 different seeds (seeds in biomass2tsb)

# 2. Timing: +- one month (dateshift in biomass2tsb)

# 3. Design: Parallel, RectEnclZZ (type in biomass2tsb -> surveyPlanner)

# 4. Strata order: Normal, reversed (reverse in setSurveyParameters)












# biomassFile <- "/Users/arnejh/Documents/Produktivt/Prosjekt/PHD_Holmin/PROJECTS/PELFOSS/2017-12-04 PELFOSS SurveyPlanner/From Norwecom/m01.nc" # This is mackerel
#biomass <- "~/Documents/Produktivt/Prosjekt/PHD_Holmin/PROJECTS/PELFOSS/2017-12-04 PELFOSS SurveyPlanner/From Norwecom/2D_20104km_her.nc"
#
#superInd <- "~/Documents/Produktivt/Prosjekt/PHD_Holmin/PROJECTS/PELFOSS/2017-12-04 PELFOSS SurveyPlanner/From Norwecom/herring2010_01.nc"
#
#stratum <- "~/workspace/stox/polygon/norwegian_sea2014.txt"
#


##################################
##### 0. System parameters: #####
##################################
xsd = list(acoustic="1", biotic="1.4")
##################################



##################################
##### 1. General parameters: #####
##################################
dir <- "~/Documents/Produktivt/Prosjekt/PHD_Holmin/PROJECTS/PELFOSS/delphi"
xmlOutputDir <- "~/Documents/Produktivt/Prosjekt/PHD_Holmin/PROJECTS/PELFOSS/2017-12-04 PELFOSS SurveyPlanner/XMLfiles"
projectName <- "NORWECOM_Rstox"
nboot=100
##################################


###############################
##### 2. Case parameters: #####
###############################
year = 2010 # 2010 the high resolution (res = 4), and 2010 - 2014 for the low resolution (res = 10)
res = 4 # Use the high resolution for the first experiment, and low resolution for the experiment focusing on changes between years
survey <- "Herring_IESNS" # "Herring_IESNS", "Herring_NASSHS" or "Mackerel_IESSNS"
reverse <- FALSE # FALSE or TRUE
seeds = list(transect=0, trawl=1, bootstrap=2) # Maybe use 10 different seed lists?
type = "RectEnclZZ" # "RectEnclZZ" or "Parallel"
###############################




###############################
##### 3. Ymse parameters: #####
###############################
equalEffort = TRUE # Re-evaluate this, and possibly specify effort as hours per stratum
margin=0.1 # Related to equalEffort
bearing = "along"
distsep=1 # One nmi log distance is standard
probfact=1 # This reserves half of the probability of drawing trawls to the NASC and half to chance
radius=10 # Increase this to include more superindividuals in the trawl sample
N=100 # Draw 100 fish per trawl
cores=6 # Use 6 cores for both biotic and acoustic xml and bootstrap
###############################


###################################
##### 4. Cosmetic parameters: #####
###################################
platform=4174 # G.O.Sars
distance = 5 # Trawling distance, equal for all stations, thus ineffective in an acousic-trawl survey
sweepWidth = 25 # Seewp width, see distance above


files <- getNorwecomPaths(dir, survey=survey, year=year, res=res)
surveyPar <- setSurveyParameters(survey=survey, reverse=reverse)







system.time(d <- biomass2tsb(
	# For the StoX project and writing XML files:
	projectName=projectName, xmlOutputDir=xmlOutputDir, 
	# For the biomass and superindividuals and other global options:
	biomass=files$biomass, superInd=files$superInd, stratum=files$stratum, startdate=surveyPar$startdate, centroid=surveyPar$centroid, seeds=seeds, nboot=nboot, xsd=xsd,
	# For transects and NASC:
	nTrawl=surveyPar$nTrawl, hours=surveyPar$hours, type=type, knots=surveyPar$knots, equalEffort=equalEffort, bearing=bearing, distsep=distsep, margin=margin, tsn=surveyPar$tsn, m=surveyPar$m, TS0=surveyPar$TS0, 
	# For trawl:
	platform=platform, distance=distance, sweepWidth=sweepWidth, probfact=probfact, radius=radius, N=N, cores=cores, 
	strata=surveyPar$strata, rev=surveyPar$rev
	)
)


# Spawning
#TSB Ab.Sum.5% Ab.Sum.50% Ab.Sum.95% Ab.Sum.mean Ab.Sum.sd  Ab.Sum.cv
#1 TSB   1935985    2014091    2093834     2013628  48407.77 0.02404008
#




Vedlagd er eit plot for sildetoktet, som viser biomasse-feltet frå NORWECOM, stratum-systemet, transektlinene, og log-distansar langs transektlinene med akustiske NASC-verdiar over ein viss terskel (og med storleik på punkta aukande med aukande NASC).


Det totale toktestimatet vart her på 5.11 millionar tonn, medan summen av vektene frå superindivida innanfor stratasystemet var 4.65 millionar tonn. Det vert spanande å 


# May:
  TSB Ab.Sum.5% Ab.Sum.50% Ab.Sum.95% Ab.Sum.mean Ab.Sum.sd Ab.Sum.cv
1 TSB   4137045    5150720    6335306     5106671  712440.1 0.1395117


d$totBiom$AllStrata[d$midDayOfSurvey] # 4.653889e+12

plot(d$totBiom$Total, ylim=c(0, max(d$totBiom$Total)))
points(d$totBiom$AllStrata, col=2)
TSB <- d$TSB$bootstrap$scale * d$TSB$bootstrap$abnd$'Ab.Sum.50%'
abline(h = TSB)
abline(v = range(d$daysOfSurvey))






dev.new()
p <- plotStratum(d$transects)

# Plot with the trals and NASC:
p1 <- plotPELFOSS(
	p, 
	d$transects,
	d$biomass,
	day=d$midDayOfSurvey, 
	superInd = d$superInd, 
	ncolour=40, 
	maxBiom=700, 
	maxNasc=4000, 
	col=list(h=c(0.6, 1), s=c(.3, 0.99), v=c(0.9, 0.2)), 
	NASCthr=0.001, 
	NASCexp=2, biomassAlpha=0.1
)





g <- getBaseline(projectName <- "NORWECOM_Rstox")
TSD::dim_all(g$outputData)
TSD::dim_all(g$processData)
head(g$outputData$ReadBioticXML$ReadBioticXML_BioticData_FishStation.txt)
sort(g$outputData$ReadBioticXML$ReadBioticXML_BioticData_FishStation.txt$serialno)

sort(as.numeric(substring(g$processData$bioticassignment$Station, nchar("surveyPlanner/") + 1)))













system.time(d <- biomass2tsb(
	# For the StoX project and writing XML files:
	projectName = projectName, xmlOutputDir = xmlOutputDir, 
	# For the biomass and superindividuals and other global options:
	biomass = biomass, superInd = superInd, stratum = stratum, centroid = c(0, 68), seeds = list(transect=10, trawl=11, bootstrap=12), nboot=100, xsd = list(acoustic="1", biotic="1.4"),
	# For transects and NASC:
	nTrawl = 130, daysOfSurvey = 30, type = "RectEnclZZ", knots = 16, equalEffort = TRUE, bearing = "along", distsep = 1, margin = 0.1, byStratum = FALSE, keepTransport = FALSE, tsn = 161722, m = 20, TS0 = -71.9, 
	# For trawl:
	platform = 4174, missiontype = 4, missionnumber = 1, samplenumber = 1, distance = 5, sweepWidth = 25, probfact = 1, radius=10, 
	N=100, cores=6, 
	strata = strata, rev = rev
	)
)


system.time(d <- biomass2tsb(
	# For the StoX project and writing XML files:
	projectName = projectName, xmlOutputDir = xmlOutputDir, 
	# For the biomass and superindividuals and other global options:
	biomass = biomass, superInd = superInd, stratum = stratum, centroid = c(0, 68), seeds = list(transect=10, trawl=11, bootstrap=12), nboot=100, xsd = list(acoustic="1", biotic="1.4"),
	# For transects and NASC:
	nTrawl = 130, daysOfSurvey = 30, type = "RectEnclZZ", knots = 16, equalEffort = TRUE, bearing = "along", distsep = 1, margin = 0.1, byStratum = FALSE, keepTransport = FALSE, tsn = 161722, m = 20, TS0 = -71.9, 
	# For trawl:
	platform = 4174, missiontype = 4, missionnumber = 1, samplenumber = 1, distance = 5, sweepWidth = 25, probfact = 1, radius=10, 
	N=100, cores=6, 
	strata = rev(strata), rev = !rev
	)
)
























#####################################################
##### 1. Decide effort in survey Herring_IESNS: #####
#####################################################

g <- getBaseline("Test_Rstox")


wkt <- list.files("~/Documents/Produktivt/Prosjekt/PHD_Holmin/PROJECTS/PELFOSS/delphi/stratum/Herring_IESNS", full.names=TRUE)[1]
transects <- surveyPlanner(projectName=wkt, hours=list(1000), type="RectEnclZZ", equalEffort=TRUE, bearing="along", strata=c(4, 3, 1, 2), rev=c(FALSE, TRUE, FALSE, FALSE))
# Get survey area:
transects$Survey$area # 421 272.1

p <- plotStratum(transects)

p1 <- p + geom_point(data=g$outputData$ReadAcousticXML$ReadAcousticXML_AcousticData_DistanceFrequency.txt, aes(x=lon_start, y=lat_start), shape=".", col="darkblue")

p1 

# It seems reasonable with hours=list(1000), However, we use only one ship, and we want to spekd 4 weeks on the survey (comparable to the effort spent by each of the four vessels in the real survey (Iceland, Faroe Islands, Denmark, Norway)), so we increase the speed to 15 knots, giving a total of 1000/24 / 1.5 = 27.77778 days spent. So the final values will be hours = list(28*24) and speed = 1.5

# The density of trawl station is 

totalLog <- diff(range(g$outputData$ReadAcousticXML$ReadAcousticXML_AcousticData_DistanceFrequency.txt$log_start))
numTrawls <- nrow(g$outputData$ReadBioticXML$ReadBioticXML_BioticData_FishStation.txt)
trawlDens <- numTrawls / totalLog


#######################################################
#######################################################





######################################################
##### 2. Decide effort in survey Herring_NASSHS: #####
######################################################

ssp <- "Norwegian Sea NOR Norwegian spring-spawning herring acoustic abundance index in Feb-Mar"
pr <- getNMDdata(ssp)
pr <- "Norwegian Sea NOR Norwegian spring-spawning herring acoustic abundance index in Feb-Mar_2018"
g <- getBaseline(pr)
write.table(g$processData$stratumpolygon[names(g$processData$stratumpolygon) != "IncludeInTotal"], "~/Documents/Produktivt/Prosjekt/PHD_Holmin/PROJECTS/PELFOSS/delphi/stratum/Herring_NASSHS/gytetokt2017_final.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t")


wkt <- list.files("~/Documents/Produktivt/Prosjekt/PHD_Holmin/PROJECTS/PELFOSS/delphi/stratum/Herring_NASSHS", full.names=TRUE)[1]
transects <- surveyPlanner(projectName=wkt, hours=list(3 * 7 * 24), type="RectEnclZZ", equalEffort=TRUE, bearing="along")
# Get survey area:
transects$Survey$area # 38443.66

p <- plotStratum(transects)

p1 <- p + geom_point(data=g$outputData$ReadAcousticXML$ReadAcousticXML_AcousticData_DistanceFrequency.txt, aes(x=lon_start, y=lat_start), shape=".", col="darkblue")

p1  

# Seems reasonable with hours=list(3 * 7 * 24), corresponding to 2 vessels spending 7 full days og acoustic transects (the survey lasts about 10 days but some time is spent trawlining: range(g$outputData$ReadAcousticXML$ReadAcousticXML_AcousticData_DistanceFrequency.txt$start_time))
# This will however result in 21 days survey time, so we speed up to 20 knots and reduce the time to hours = list(10 * 24).

totalLog <- diff(range(g$outputData$ReadAcousticXML$ReadAcousticXML_AcousticData_DistanceFrequency.txt$log_start))
numTrawls <- nrow(g$outputData$ReadBioticXML$ReadBioticXML_BioticData_FishStation.txt)
trawlDens <- numTrawls / totalLog


######################################################
######################################################



#######################################################
##### 3. Decide effort in survey Mackerel_IESSNS: #####
#######################################################

# Decide effort in survey Mackerel_IESSNS:
g <- getBaseline("~/Documents/Produktivt/Prosjekt/PHD_Holmin/PROJECTS/PELFOSS/2017-12-04 PELFOSS SurveyPlanner/Stratum/Mackerel/IESSNS2017_Mac_EEZ")

# Skip the costal stratum nr 8, since this is small in area and suffers from too little effort when equalEffort=TRUE. Re-evaluate this and possibly specify the hours in each stratum:
wkt <- list.files("~/Documents/Produktivt/Prosjekt/PHD_Holmin/PROJECTS/PELFOSS/delphi/stratum/Mackerel_IESSNS", full.names=TRUE)[1]
transects <- surveyPlanner(projectName=wkt, hours=list(28*24), knots=30, type="RectEnclZZ", equalEffort=TRUE, bearing="along", strata=-8)
# Get survey area:
transects$Survey$area 

# The survey area is 806 712, so approximately double that of the Herring_IESNS, and lasts for one month, which is comparable to the Herring_IESNS, so we simply double the speed of the vessel compared to the Herring_IESNS and use the same hours: 

a <- as.POSIXct(paste(g$outputData$ReadBioticXML$ReadBioticXML_BioticData_FishStation.txt$startdate, g$outputData$ReadBioticXML$ReadBioticXML_BioticData_FishStation.txt$starttime), format="%d/%m/%Y %H:%M:%S")

range(a) # "2017-07-03 17:17:00 CEST" "2017-08-04 19:40:35 CEST"



p <- plotStratum(transects)

p1 <- p + geom_point(data=g$outputData$ReadBioticXML$ReadBioticXML_BioticData_FishStation.txt, aes(x=longitudestart, y=latitudestart), size=0.01)

p1 # Less relevant plot since there are no acoustic data in the mackerel cruise








# Errors realted to too little effort in one or more strata:
# Several: Error in bb[1, ] : incorrect number of dimensions
# One: Error in seq.default(corners$xmin - 2 * firstTransectPos, corners$xmax +  :  wrong sign in 'by' argument






















s222 <- getSuperIndOfDay(superInd=d$superInd, day=222)
marAdd <- c(0,0,0,3)
par(mar=par("mar") + marAdd)
h1 <- 0.2
h2 <- 1
plot(s222$X, s222$Y, col=hsv(TSD::setrange(s222$length, h1, h2)), cex=0.3)
fields::image.plot(legend.only=TRUE, zlim=range(s222$length), col=hsv(seq(h1, h2, l=100)))
par(mar=par("mar") - marAdd)

	
	











