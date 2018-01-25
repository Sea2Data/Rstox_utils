library(Rstox)
options(java.parameters="-Xmx6g")
dir <- "/Users/a5362/code/github/Rstox_utils/Work"
outpath <- "/Users/a5362/code/github/Rstox_utils/Work/output"
#sildeprosjekt: /delphi/Felles/alle/stox/ECA/2015/ECA_sild_2015. Legg til sild == '161724' i filter (annen kode for sild'g03)

# Get ECA output using Rstox 1.5.2, which does not contain the hierarchy matrix, and has discrepancy between the defintion and values for covariate Season:
#projectname <- "ECA_torsk_2015"
projectname <- "ECA_sild_2015"
baselineOutput <- getBaseline(projectname)
eca <- baseline2eca(projectname)


#
# workarounds
# Should be eliminated (moved to stox-processes, baseline2eca or functions in this script)
#
# Fix the links in eca$resources$covariateLink$season:
# Change the Season covariate from numeric to Q1, ..., Q4 (due to a bug in StoX):
#eca$biotic$season <- paste0("Q", eca$biotic$season)
#eca$landing$season <- paste0("Q", eca$landing$season)
# Should they not be numeric 1-4 ?
eca$resources$covariateLink$season$Covariate <- paste0("Q", 1:4)

source(paste(dir, "workarounds.R", sep="/"))

#corrects formatting of covariates in eca$landing (and eca$biotic). Probably only needed for aggregate_landings to work, which should be tossed out anyway
eca <- fix_missing_data(eca) #fix in stox
if (projectname=="ECA_torsk_2015"){ #must be preceeeded by fix missing data
	eca <- fix_cod(eca)
}

#Fjern i fra baseline2ECA / stox
eca <- drop_year(eca) #fix in stox

# Skal eca$landingAggregated, eller eca$covariateMatrixLanding brukes til i getLandings ?
# Om eca$landingAggregated må kovariatene ha annen type (num eller int, ikke faktor)
eca <- aggregate_landings(eca) #renames rundvekt. Probably works if other issues are fixed, but still ned to rename somewhere.

#Fixed in baseline2ECA. Check and remove.
eca <- rearrange_resources(eca) 

# filter in stox. (JIRA 150) Sjekk hva disse er (2015, snr: 39002-39080)
eca <- impute_catchweight(eca) 
#estimate in stox. (JIRA 150)
eca <- estimate_catchcount(eca) 

eca <- fix_otolithtypes(eca)

# Diskuter utforming av kovariatdefinisjon for platform. Edvin avklarer med Hanne at boat kan behandles som faktor (JIRA 151)
eca <- set_platform_factor(eca) # treat as covariate in stox ?

#define once stratumNeighbour gets populated by stox
makeNeighbourLists <- fake_neighbourmatrix

if(all(is.na(eca$covariateMatrixBiotic$spatial))){
	stop("spatial can not all be na")	
}
#/workarounds

# Functions:

#' checks that column names are present on datamatrix
check_columns_present <- function(datamatrix, columns){
	errors <- ""
	for (col in columns){
		if (!(col %in% attributes(datamatrix)$names)){
			errors <- paste(errors, "column", col, "missing.\n")
		}
	}
	if (errors != ""){
		stop(errors)
	}
}
#columns does not have missing values
check_none_missing <- function(datamatrix, columns){
	errors <- ""
	for (col in columns){
		if (any(is.na(datamatrix[[col]]))){
			errors <- paste(errors, "column", col, "has missing value.\n")
		}
	}
	if (errors != ""){
		stop(errors)
	}
}

#checks that agelenght is configured correctly
checkAgeLength<-function(agelength){
	check_columns_present(agelength$DataMatrix, c("age", "realage", "lengthCM", "samplingID", "partnumber", "partcount"))
	check_none_missing(agelength$DataMatrix, c("lengthCM", "samplingID", "partnumber", "partcount"))
	if ("otolithtype" %in% attributes(agelength$datamatrix)$names){
		check_none_missing(agelength, c("otolithtype"))
	}
}
#checks that weightlenght is configured correctly
checkWeightLength<-function(weightlength){
	check_columns_present(weightlength$DataMatrix, c("weightKG", "lengthCM", "samplingID", "partnumber", "partcount"))
	check_none_missing(weightlength$DataMatrix, c("lengthCM", "samplingID", "partnumber", "partcount", "weightKG"))
	if ("otolithtype" %in% attributes(weightlength$datamatrix)$names){
		check_none_missing(weightlength, c("otolithtype"))
	}
}

# Function used for combining hard coded parameter values and user defeined parameter values:
getHardCoded <- function(info){
	hardcoded <- as.data.frame(matrix(
		c(
			# Boat is always random whereas constant and haulcount is always fixed:
			"random", "constant", 0, 
			"random", "haulcount", 0, 
			"random", "boat", 1,
			# "Conditional autoregressive" is 1 for spatial:							
			"CAR", "spatial", 1, 
			"random", "spatial", 1,
			# Only haulcount can be continuous for the moment:
			"continuous", "haulcount", 1, 

			# Include slope for the constant:
			"in.slopeModel", "constant", 1
			), 
		byrow=TRUE, ncol=3), stringsAsFactors=FALSE)
	hardcoded[,3] <- as.numeric(hardcoded[,3])
	hardcoded <- hardcoded[hardcoded[,2] %in% rownames(info),]
	
	for(i in seq_len(nrow(hardcoded))){
		info[hardcoded[i,2], hardcoded[i,1]] <- hardcoded[i,3]
	}
	
	return(info)
}

# Function extracting the mid of a season. Use this in sapply():
getMidSeason <- function(x, tz="UTC", format="%d/%m/%Y"){
	x <- as.Date(strsplit(x, "-")[[1]], "%d/%m")
	x <- as.POSIXlt(x, tz=tz, format=format)
	yearday <- x$yday
	# Trick to get one day for "01/01-02/01"
	yearday[1] <- yearday[1] + 1
	mean(yearday)
}

# Function used for extracting the correct covariate value from the inidces used in the covariate matrix passed to ECA:
getCovariateValue <- function(index, eca, cov="season", type="biotic"){
	# Get first the covariate symbolic value (such as Q3 for "01/07-30/09"):
	posInLinkMatrix <- match(index, eca$resources$covariateLink[[cov]]$Numeric)
	symbol <- eca$resources$covariateLink[[cov]]$Covariate[posInLinkMatrix]
	# Then link to the value:
	posInDefinitionMatrix <- match(symbol, eca$resources$covariateDefinition[[cov]][[type]]$Covariate)
	value <- eca$resources$covariateDefinition[[cov]][[type]]$Value[posInDefinitionMatrix]
	value
}

# Function for getting the mode of a vector:
getMode <- function(x){
	as.numeric(names(table(x))[which.max(table(x))])
}

getGlobalParameters <- function (eca, ecaParameters){
	#serialno is there only to enforce return type for getVar
	getnames <- c("lengthunitmeters", "serialno")
	usenames <- c("lengthresM", "samplingID")
	DataMatrix <- getVar(eca$biotic, getnames)
	names(DataMatrix) <- usenames
		
	lengthresM <- getMode(DataMatrix$lengthresM)
	lengthresCM <- lengthresM*100
	if(!all(DataMatrix$lengthresM == head(DataMatrix$lengthresM, 1))){
		warning(paste0("Several length resolusions applied in the data (", paste(table(DataMatrix$lengthresCM), collapse=", "), "). The mode (", lengthresCM,") used in the ECA"))
	}
	
	# Convert to centimeters in the lengthunit:
	Gparams <- list(nSamples=ecaParameters$nSamples, thin=ecaParameters$thin, burnin=ecaParameters$burnin, lengthresCM=lengthresCM)
	
	return(Gparams)
}

getLandings <- function(eca, ecaParameters){
	
	### landingAggregated: ###
	landingAggregated <- cbind(constant=1, eca$landingAggregated, midseason=sapply(getCovariateValue(eca$landingAggregated$season, eca, cov="season", type="landing"), getMidSeason))
	
	weight <- landingAggregated$liveweight
	landingAggregated$liveweight <- NULL
	landingAgeLength <- landingAggregated
	landingWeightLength <- landingAggregated
	
	### Return a list of the data: ###
	landings <- list(AgeLengthCov=landingAgeLength, WeightLengthCov=landingWeightLength, LiveWeightKG=weight)
	return(landings)
}

# Function for converting to the input format required by ECA (this is the main function):
getAgeLengthBiotic <- function(eca, ecaParameters){
	
	### 1. DataMatrix: ###
	getnames <- c("age", "yearday", "length", "serialno", "samplenumber", "catchcount", if(ecaParameters$use_otolithtype) "otolithtype")
	usenames <- c("age", "realage", "lengthCM", "samplingID", "partnumber", "partcount", if(ecaParameters$use_otolithtype) "otolithtype")
	
	DataMatrix <- getVar(eca$biotic, getnames)
	names(DataMatrix) <- usenames

	# Estimate the real age by use of the hatchDaySlashMonth:
	numDaysOfYear <- 365
	DataMatrix$realage <- DataMatrix$age + (DataMatrix$realage - getMidSeason(ecaParameters$hatchDaySlashMonth)) / numDaysOfYear
	
	
	### 2. CovariateMatrix: ###
	# Ad first the samplingID to the object eca$covariateMatrixBiotic:
	CovariateMatrix <- eca$covariateMatrixBiotic
	# Add samplingID, which will be removed at the end:
	CovariateMatrix$samplingID <- DataMatrix$samplingID
	# Add boat and haul size:
	if(ecaParameters$includeHaulcount){
		# Aggregate the catchcount for all values of samplenumber:
		haulcount <- by(eca$biotic, eca$biotic$serialno, function(x) sum(by(x$catchcount, x$samplenumber, head, 1)))
		CovariateMatrix$haulcount <- haulcount[match(eca$biotic$serialno, names(haulcount))]
	}
	if(ecaParameters$includeBoat){
		CovariateMatrix$boat <- eca$biotic$platform
	}
	CovariateMatrix <- CovariateMatrix[!duplicated(CovariateMatrix$samplingID),]

	# Add the first column, which is only of zeros:
	CovariateMatrix <- cbind(constant=1, CovariateMatrix)

	# Convert to 1, 2, 3, and implement an automatic function for this later:
	DataMatrix$samplingID <- match(DataMatrix$samplingID, CovariateMatrix$samplingID)
	CovariateMatrix$samplingID <- NULL
	
	### 4. info: ###
	ncov <- length(names(CovariateMatrix))
	infonames <- c("random", "CAR", "continuous", "in.landings", "nlev", "interaction", "in.slopeModel")
	ninfo <- length(infonames)
	info <- array(0L, dim=c(ncov, ninfo))
	#info <- info + seq_along(info)
	colnames(info) <- infonames
	rownames(info) <- names(CovariateMatrix)

	# 4.1. random: 
	info[eca$resources$covariateInfo$name, "random"] <- eca$resources$covariateInfo$covType=="Random"

	# 4.2. CAR:
	carneighbours <- makeNeighbourLists(eca)
	info[eca$resources$covariateInfo$name, "CAR"] <- eca$resources$covariateInfo$CAR

	# 4.3. continuous:
	if(length(ecaParameters$continuous)){
		info[names(ecaParameters$continuous), "continuous"] <- unlist(ecaParameters$continuous)
	}

	# 4.4. in.landings:
	info[rownames(info), "in.landings"] <- as.integer(rownames(info) %in% names(eca$landingAggregated))
	info["constant", "in.landings"] <- 1

	# 4.5. interaction:
	if(length(ecaParameters$interaction)){
		info[names(ecaParameters$interaction), "interaction"] <- unlist(ecaParameters$interaction)
	}

	# 4.6. include.slope:
	if(length(ecaParameters$in.slopeModel)){
		info[names(ecaParameters$in.slopeModel), "in.slopeModel"] <- unlist(ecaParameters$in.slopeModel)
	}
	
	info <- getHardCoded(info)
	# 4.7. nlev:
	info[rownames(info), "nlev"] <- apply(CovariateMatrix, 2, function(x) max(x))
	# Continuous covariates should have only one level:
	info[info[, "continuous"]==1, "nlev"] <- 1
	
	# random covariates should have levels equal to max of landing and max of observations (not sure if the latter is necessary
	if (sum(info[, "random"]==1 & info[, "in.landings"]==1)>0){
		for (n in rownames(info)){
			if (info[n, "random"]==1 & info[n, "in.landings"]==1){
				info[n,"nlev"] <- max(eca$landingAggregated[[n]], CovariateMatrix[[n]])				
			}
		}
	}


	### 5. parameters: ###
		
	# Group the parameters into a list:
	parameters <- list()
		
	### Return a list of the data: ###
	agelength <- list(DataMatrix=DataMatrix, CovariateMatrix=CovariateMatrix, CARNeighbours=carneighbours, AgeErrorMatrix=eca$ageError, info=info, parameters=parameters, resources=eca$resources)
	if (ecaParameters$use_otolithtype){
		agelength$ClassificationErrorVector=eca$otholiterror
	}
	return(agelength)
}



ecaParameters <- list(includeBoat=TRUE, includeHaulcount=TRUE, use_otolithtype=TRUE, nSamples=1000, burnin=500, thin=1, hatchDaySlashMonth="01/01")


#################################
####### Global parameters: ######
#################################
GlobalParameters <- getGlobalParameters(eca, ecaParameters)

#################################
########### Landings: ###########
#################################
Landings <- getLandings(eca, ecaParameters)

#################################
########## Age-length: ##########
#################################
AgeLength <- getAgeLengthBiotic(eca, ecaParameters)


####################################
########## Weight-length: ##########
####################################
# Generate also the weight-length list:
AgeLength2WeightLength <- function(AgeLength, eca){
	# Declare the output:
	WeightLength <- AgeLength
	# Remove the ages:
	toBeRemoved <- c("age", "realage")
	for(var in toBeRemoved){
		WeightLength$DataMatrix[[var]] <- NULL
	}
	
	# Add the weight:
	# Hard code the weight to KG, since it is in grams in StoX:
	weightunit <- 1e-3
	WeightLength$DataMatrix <- cbind(weightKG=eca$biotic$weight * weightunit, WeightLength$DataMatrix)
	
	# Remove missing ind weight
	WeightLength$DataMatrix <- WeightLength$DataMatrix[!is.na(WeightLength$DataMatrix$weightKG),]
	
	# Detect columns of all zeros in the covariate matrix, and remove these columns along with the correponding columns in the meta vectors:
	collapsed <- apply(WeightLength$CovariateMatrix, 2, function(x) all(x==0))
	if(any(collapsed)){
		# Remove from CovariateMatrix:
		WeightLength$CovariateMatrix <- WeightLength$CovariateMatrix[, !collapsed, drop=FALSE]
		# Remove from meta vectors$:
		WeightLength$info <- WeightLength$info[!collapsed, , drop=FALSE]
	}
	
	# Return the WeightLength data:
	WeightLength$AgeErrorMatrix <- NULL
	
	WeightLength
}
WeightLength <- AgeLength2WeightLength(AgeLength, eca)

checkAgeLength(AgeLength)
checkWeightLength(WeightLength)

save(GlobalParameters, Landings, WeightLength, AgeLength, file=file.path(outpath, paste0(projectname, ".RData")))