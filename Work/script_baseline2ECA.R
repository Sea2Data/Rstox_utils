library(Rstox)
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
baselineOutput <- getBaseline(projectname)
eca <- baseline2eca(projectname)


#
# workarounds
# Should be eliminated (moved to stox-processes, baseline2eca or functions in this script)
#

# Løses i stox (STOX-84)
eca$resources$covariateLink$season$Covariate <- paste0("Q", 1:4)

source(paste(dir, "workarounds.R", sep="/"))

# replace by data filters in stox or extend reference lists
# e.g.: stations missing both area and position
# 
eca <- filter_missing_data(eca)
eca <- impute_catchweight(eca) #Sjekk hva disse er (2015, snr: 39002-39080)

#estimate in stox. (STOX-150)
eca <- estimate_catchcount(eca) 

#hack for specific cod-set
if (projectname=="ECA_torsk_2015"){ #must be preceeeded by fix missing data
	eca <- fix_cod(eca)
}

#Ordne i defineTemporal STOX-153
eca <- drop_year(eca) #fix in stox

# Koding og filtrering av otolitter må håndteres før use_otolit=TRUE can brukes.
# ECA crasher om ikke otlittkolonne eskisterer avklar med Hanne
# eca <- fix_otolithtypes(eca)


# Diskuter utforming av kovariatdefinisjon for platform. (STOX-150) Edvin avklarer med Hanne at boat kan behandles som faktor (JIRA 151)
eca <- set_platform_factor(eca) # treat as covariate in stox!

#/workarounds


#
# Tests for data format to be exported to ECA
#

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
		if (any(is.na(datamatrix[,col]))){
			errors <- paste(errors, "column", col, "has missing value.\n")
		}
	}
	if (errors != ""){
		stop(errors)
	}
}
check_cov_vs_info <- function(modelobj){
  if (!("constant" %in% names(modelobj$CovariateMatrix)) | !("constant" %in% rownames(modelobj$info))){
    stop("No constant column provided in covariate matrix or info matrix")
  }
  if (modelobj$info["constant","in.landings"]!=1 | modelobj$info["constant","in.slopeModel"]!=1 | modelobj$info["constant","random"]!=0 | modelobj$info["constant","CAR"]!=0 | modelobj$info["constant","continuous"]!=0){
    stop("Constant covariate is not configured correctly")
  }
  for (co in names(modelobj$CovariateMatrix)){
    if (!co %in% rownames(modelobj$info)){
      stop(paste("Covariate", co, "not in info matrix"))
    }
    if (any(is.na(modelobj$CovariateMatrix[,co]))){
      stop(paste("NAs for covariate", co))
    }
    ma <- max(modelobj$CovariateMatrix[,co])
    mi <- min(modelobj$CovariateMatrix[,co])
    num_unique <- length(unique(modelobj$CovariateMatrix[,co]))
    
    if (modelobj$info[co,"continuous"]==0 & ma > modelobj$info[co,"nlev"]){
      stop(paste("Max value higher than nlev for covariate", co))
    }
    if (modelobj$info[co,"continuous"]==0 & mi < 1){
      stop(paste("Min value lower than 1 for covariate", co))
    }
    if (modelobj$info[co,"CAR"]==1 & modelobj$info[co,"random"]!=1){
      stop("CAR variable not designated as random effect.")
    }
    if (modelobj$info[co,"CAR"]==1 & modelobj$info[co,"random"]!=1){
      stop("CAR variable not designated as random effect.")
    }
    if (modelobj$info[co,"continuous"]==1 & modelobj$info[co,"nlev"]!=1){
      stop(paste("nlev wrongly configured for continuous variable", co))
    }
    if (modelobj$info[co,"interaction"]==1 & modelobj$info[co,"in.landings"]!=1){
      stop(paste("Interaction specified for covariate that are not in landings", co))
    }
    if (modelobj$info[co,"random"]==0 & modelobj$info[co,"continuous"]==0 & num_unique!=modelobj$info[co,"nlev"]){
      stop(paste("Not all values present for fixed covariate", co))
    }
    if (modelobj$info[co,"CAR"]==1 & is.null(modelobj$CARNeighbours)){
      stop(paste("CAR variable specified as", co, "but CARneighbours not specified"))
    }
    if (modelobj$info[co,"CAR"]==1 & (max(modelobj$CARNeighbours$idNeighbours)>modelobj$info[co,"nlev"] | max(modelobj$CARNeighbours$idNeighbours)<1)){
      stop(paste("Neigbour matrix not consistent with nlev for CAR vairable", co))
    }
    if (modelobj$info[co,"CAR"]==1 & (any(modelobj$CARNeighbours$numNeighbours<1) | length(modelobj$CARNeighbours$numNeighbours) < modelobj$info[co,"nlev"])){
      stop(paste("CAR variable specified as", co, "but some areas are missing neighbours"))
    }
  }
}
check_data_matrix <- function(modelobj){
  #if ("otolithtype" %in% names(modelobj$DataMatrix)){
  #  check_none_missing(modelobj$DataMatrix, c("otolithtype"))
  #}
  warning("Clarify need for otolithtype check with NR. rECA currently not behaving consistnently with documentation")
  lastsample <- max(modelobj$DataMatrix$samplingID)
  if (!lastsample==nrow(modelobj$CovariateMatrix)){
    stop("sampling ids does not equals the number of rows in covariate matrix")
  }
  
}
#checks that specification of covariates are OK
check_covariates <- function(modelobject){
  check_cov_vs_info(modelobject)
}

#checks that agelenght is configured correctly
checkAgeLength<-function(agelength, num_tolerance = 1e-10){
	check_columns_present(agelength$DataMatrix, c("age", "realage", "lengthCM", "samplingID", "partnumber", "partcount"))
	check_none_missing(agelength$DataMatrix, c("lengthCM", "samplingID", "partnumber", "partcount"))
	check_data_matrix(agelength)
	check_covariates(agelength)
	if (any(is.na(agelength$AgeErrorMatrix)) || any(agelength$AgeErrorMatrix>1) || any(agelength$AgeErrorMatrix<0)){
	  stop("Invalid values in age error matrix")
	}
	if (any(abs(colSums(agelength$AgeErrorMatrix)-1)>num_tolerance)){
	  stop("Columns of age error matrix does not sum to 1")
	}
}
#checks that weightlenght is configured correctly
checkWeightLength<-function(weightlength, landings){
	check_columns_present(weightlength$DataMatrix, c("weightKG", "lengthCM", "samplingID", "partnumber", "partcount"))
	check_none_missing(weightlength$DataMatrix, c("lengthCM", "samplingID", "partnumber", "partcount", "weightKG"))
	check_data_matrix(weightlength)
	check_covariates(weightlength)
}
#checks that covariates are compatible between model and landings
checkCovariateConsistency <- function(modelobj, landingscov){
  
  inlandings <- rownames(modelobj$info[modelobj$info[,"in.landings"]==1,])
  if (any(!(inlandings %in% names(landingscov)))){
    stop("some covariates labeled as in.landings are not found in corresponding covariate matrix in landings")
  }
  
  landingscoovariates <- names(landingscov)[names(landingscov) %in% inlandings]
  if (!all(inlandings==landingscoovariates)){
    stop("Covariates are not ordered consistently in model and landings")
  }
  
  #check that all level are present for all fixed effects
  fixedeffects <- rownames(modelobj$info[modelobj$info[,"random"]==0,])
  for (co in fixedeffects){
    if (modelobj$info[co,"continuous"]==0){
      num_unique <- length(unique(landingscov[,co]))
      if (num_unique!=modelobj$info[co,"nlev"]){
        stop(paste("Fixed effect", co, "does not have values for all corresponding landings"))
      }
    }
  }
  
}
#checks that landings are specified correctly
checkLandings <- function(landings){
  if (nrow(landings$AgeLengthCov) != nrow(landings$WeightLengthCov)){
    stop("number of rows landings covariate matrices does not match")
  }
  if (nrow(landings$AgeLengthCov) != length(landings$LiveWeightKG)){
    stop("length of weight vector does not match number of rows in covariate matrices in landings.")
  }
}
checkGlobalParameters <- function(globalparameters){
  if (is.na(globalparameters$lengthresCM)){
    stop("Length resolution not set (lengthresCM)")
  }
}

#
# Functions for coverting data from stox-structure to ECA structure
#

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
	
	weight <- landingAggregated$rundvekt
	landingAggregated$rundvekt <- NULL
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
	#carneighbours <- makeNeighbourLists(eca)
	# Make sure the neighbours are ordered according to the 1:n values in the covariateLink:
	ord <- order(match(eca$stratumNeighbour[,1], eca$resources$covariateLink$spatial[,2]))
	eca$stratumNeighbour <- eca$stratumNeighbour[ord, ]
	temp <- lapply(eca$stratumNeighbour[,2], function(x) strsplit(x, ",")[[1]])
	numNeighbours <- sapply(temp, length)
	idNeighbours <- unlist(temp)
	idNeighbours <- eca$resources$covariateLink$spatial[match(idNeighbours, eca$resources$covariateLink$spatial[, 2]), 1]
	carneighbours <- list(numNeighbours=numNeighbours, idNeighbours=idNeighbours)
	
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


#
# Run checks
#

checkAgeLength(AgeLength)
checkWeightLength(WeightLength)
checkCovariateConsistency(AgeLength, Landings$AgeLengthCov)
checkCovariateConsistency(WeightLength, Landings$WeightLengthCov)
checkLandings(Landings)
checkGlobalParameters(GlobalParameters)


#
# save data
#

save(GlobalParameters, Landings, WeightLength, AgeLength, file=file.path(outpath, paste0(projectname, ".RData")))