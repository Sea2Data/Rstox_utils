#
# Functions for coverting data from stox-structure to ECA structure
#

# Function used for combining hard coded parameter values and user defeined parameter values:
getHardCoded <- function(info){
  warning("Ovverriding settings for spatial")
  hardcoded <- as.data.frame(matrix(
    c(
      # Boat is always random whereas constant and haulcount is always fixed:
      "random", "constant", 0, 
      # "Conditional autoregressive" is 1 for spatial:							
      "CAR", "spatial", 1, 
      "random", "spatial", 1,
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
  numDaysOfYear <- 365
  warning("Re-implement setting of midseason once NR updates documentation.")
  landingAggregated <- cbind(constant=1, eca$landingAggregated, midseason=sapply(getCovariateValue(eca$landingAggregated$temporal, eca, cov="temporal", type="landing"), getMidSeason))
  landingAggregated$midseason <- landingAggregated$midseason / numDaysOfYear
  
  weight <- landingAggregated$rundvekt
  landingAggregated$rundvekt <- NULL
  landingAgeLength <- landingAggregated
  landingWeightLength <- landingAggregated
  
  ### Return a list of the data: ###
  landings <- list(AgeLengthCov=landingAgeLength, WeightLengthCov=landingWeightLength, LiveWeightKG=weight)
  return(landings)
}


#################################################################################################################################
########## New functions as of 2018-05-07 (extracting DataMatrix separately for LengthGivenAge and WeightGivenLength): ##########
#################################################################################################################################
# Funciton for extracting the DataMatrix for the given variable ("age" or "weight", length is requested in both):
getDataMatrixANDCovariateMatrix <- function(eca, var="age", ecaParameters){
  
  #partcount
  
  # Define variables to include in the DataMatrix, where the variable specified in the input 'var' is included:
  getnames <- c("yearday", "length", "serialno", "samplenumber", "lengthsamplecount", "lengthsampleweight", "catchweight", if(ecaParameters$use_otolithtype) "otolithtype")
  usenames <- c("realage", "lengthCM", "samplingID", "partnumber", "samplecount", "sampleweight", "catchweight", if(ecaParameters$use_otolithtype) "otolithtype")
  getnames <- c(var, getnames)
  usenames <- c(var, usenames)
  
  # Extract the data matrix:
  DataMatrix <- getVar(eca$biotic, getnames)
  names(DataMatrix) <- usenames
  
  #Estimate catch sample number: partcount
  DataMatrix$partcount <- DataMatrix$catchweight * DataMatrix$samplecount / DataMatrix$sampleweight
  #Drop columns only used for estimating partcount
  DataMatrix <- DataMatrix[, !(names(DataMatrix) %in% c("catchweight", "samplecount", "sampleweight"))]
  
  # Add first the samplingID to the object eca$covariateMatrixBiotic:
  CovariateMatrix <- eca$covariateMatrixBiotic
  # Add samplingID, which will be removed at the end:
  CovariateMatrix$samplingID <- DataMatrix$samplingID
  CovariateMatrix <- CovariateMatrix[!duplicated(CovariateMatrix$samplingID),]
  
  # Add the first column, which is only of ones:
  CovariateMatrix <- cbind(constant=1, CovariateMatrix)
  
  # Convert to 1, 2, 3, and implement an automatic function for this later:
  DataMatrix$samplingID <- match(DataMatrix$samplingID, CovariateMatrix$samplingID)
  CovariateMatrix$samplingID <- NULL
  
  return(list(DataMatrix=DataMatrix, CovariateMatrix=CovariateMatrix))
}

# Funciton for extracting the CARNeighbours and info:
getInfo<- function(eca, CovariateMatrix, ecaParameters){
  ### 3. info: ###
  ncov <- length(names(CovariateMatrix))
  Infonames <- c("random", "CAR", "continuous", "in.landings", "nlev", "interaction", "in.slopeModel")
  nInfo <- length(Infonames)
  info <- array(0L, dim=c(ncov, nInfo))
  #info <- info + seq_along(info)
  colnames(info) <- Infonames
  rownames(info) <- names(CovariateMatrix)
  
  # 3.1. random: 
  info[eca$resources$covariateInfo$name, "random"] <- eca$resources$covariateInfo$covType=="Random"
  
  # 3.2. CAR:
  # Make sure the neighbours are ordered according to the 1:n values in the covariateLink:
  ind <- match(as.numeric(names(eca$stratumNeighbour)), eca$resources$covariateLink$spatial[,2])
  if (!all(sort(ind)==ind)){
    stop("covariate values are ordered differently in stratumneighbour and covariatelink spatial")
  }
  names(eca$stratumNeighbour) <- eca$resources$covariateLink$spatial[ind,1]
  numNeighbours <- unlist(lapply(eca$stratumNeighbour, length), use.names = F)
  idNeighbours <- unlist(eca$stratumNeighbour, use.names=F)
  idNeighbours <- eca$resources$covariateLink$spatial[match(idNeighbours, eca$resources$covariateLink$spatial[, 2]), 1]
  CARNeighbours <- list(numNeighbours=numNeighbours, idNeighbours=idNeighbours)
  
  info[eca$resources$covariateInfo$name, "CAR"] <- eca$resources$covariateInfo$CAR
  
  # 3.3. continuous:
  if(length(ecaParameters$continuous)){
    info[names(ecaParameters$continuous), "continuous"] <- unlist(ecaParameters$continuous)
  }
  
  # 3.4. in.landings:
  info[rownames(info), "in.landings"] <- as.integer(rownames(info) %in% names(eca$landingAggregated))
  info["constant", "in.landings"] <- 1
  
  # 3.5. interaction:
  if(length(ecaParameters$interaction)){
    info[names(ecaParameters$interaction), "interaction"] <- unlist(ecaParameters$interaction)
  }
  
  # 3.6. include.slope:
  if(length(ecaParameters$in.slopeModel)){
    info[names(ecaParameters$in.slopeModel), "in.slopeModel"] <- unlist(ecaParameters$in.slopeModel)
  }
  
  info <- getHardCoded(info)
  # 3.7. nlev:
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
  
  return(
    list(
      info = info, 
      CARNeighbours = CARNeighbours
    )
  )
}

# Function for converting to the input format required by ECA (this is the main function):
getLengthGivenAge_Biotic <- function(eca, ecaParameters){
  
  # Extract the non-NAs:
  var <- "age"
  # Remove missing values from the DataMatrix and from the eca$covariateMatrixBiotic:
  valid <- !is.na(eca$biotic[[var]])
  eca$biotic <- eca$biotic[valid, , drop=FALSE]
  eca$covariateMatrixBiotic <- eca$covariateMatrixBiotic[valid, , drop=FALSE]
  
  ### 1. DataMatrix: ###
  temp <- getDataMatrixANDCovariateMatrix(eca, var=var, ecaParameters)
  DataMatrix <- temp$DataMatrix
  CovariateMatrix <- temp$CovariateMatrix
  
  
  #DataMatrix <- getDataMatrix(eca, var=var, ecaParameters)
  
  # Estimate the real age by use of the hatchDaySlashMonth:
  numDaysOfYear <- 365
  DataMatrix$realage <- DataMatrix$age + (DataMatrix$realage - getMidSeason(ecaParameters$hatchDaySlashMonth)) / numDaysOfYear
  DataMatrix$realage
  DataMatrix$part.year <- DataMatrix$realage - DataMatrix$age
  
  ### 2. CovariateMatrix: ###
  #CovariateMatrix <- getCovariateMatrix(eca, DataMatrix, ecaParameters)
  
  ### 3. info: ### 
  info <- getInfo(eca, CovariateMatrix, ecaParameters)
  
  
  ### Return a list of the data: ###
  out <- list(
    DataMatrix = DataMatrix, 
    CovariateMatrix = CovariateMatrix, 
    CARNeighbours = info$CARNeighbours, 
    AgeErrorMatrix = eca$ageError, 
    info = info$info, 
    resources = eca$resources
  )
  if (ecaParameters$use_otolithtype){
    out$ClassificationErrorVector <- eca$otholiterror
  }
  return(out)
}

# Function for converting to the input format required by ECA (this is the main function):
getWeightGivenLength_Biotic <- function(eca, ecaParameters){
  
  # Extract the non-NAs:
  var <- "weight"
  # Remove missing values from the DataMatrix and from the eca$covariateMatrixBiotic:
  valid <- !is.na(eca$biotic[[var]])
  eca$biotic <- eca$biotic[valid, , drop=FALSE]
  eca$covariateMatrixBiotic <- eca$covariateMatrixBiotic[valid, , drop=FALSE]
  
  ### 1. DataMatrix: ###
  temp <- getDataMatrixANDCovariateMatrix(eca, var=var, ecaParameters)
  DataMatrix <- temp$DataMatrix
  CovariateMatrix <- temp$CovariateMatrix
  
  
  #DataMatrix <- getDataMatrix(eca, var=var, ecaParameters)
  # Hard code the weight to KG, since it is in grams in StoX:
  weightunit <- 1e-3
  DataMatrix <- cbind(weightKG=eca$biotic$weight * weightunit, DataMatrix)
  
  ### 2. CovariateMatrix: ###
  #CovariateMatrix <- getCovariateMatrix(eca, DataMatrix, ecaParameters)
  
  ### 3. info: ### 
  info <- getInfo(eca, CovariateMatrix, ecaParameters)
  
  ### Return a list of the data: ###
  out <- list(
    DataMatrix = DataMatrix, 
    CovariateMatrix = CovariateMatrix, 
    CARNeighbours = info$CARNeighbours, 
    #AgeErrorMatrix = eca$ageError, # This is not needed for WeightGivenLength
    info = info$info, 
    resources = eca$resources
  )
  if (ecaParameters$use_otolithtype){
    out$ClassificationErrorVector <- eca$otholiterror
  }
  return(out)
}