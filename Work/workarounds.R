# Functions to work around things that is not yet implemented in Stox.
# load in data that is not read in stox
# impute data where needed
# mock data where needed
# 
# Author: a5362
###############################################################################
require(readr)

drop_year <- function(ecaobj){
  ecaobj$landingAggregated$year <- NULL
	ecaobj$covariateMatrixBiotic$year <- NULL
	ecaobj$covariateMatrixLanding$year <- NULL
	ecaobj$resources$covariateDefinition$year <- NULL
	ecaobj$resources$covariateLink$year <- NULL
	ecaobj$resources$covariateInfo <- ecaobj$resources$covariateInfo[-c(1),]
	return(ecaobj)
}

estimate_catchcount <- function(ecaobj){
  
  missing <- is.na(ecaobj$biotic$catchcount)
  
  if (any(is.na(ecaobj$biotic[missing,"catchweight"])) | any(is.na(ecaobj$biotic[missing,"lengthsampleweight"])) | any(is.na(ecaobj$biotic[missing,"lengthsamplecount"]))){
    stop("Could not estimate catchcount")
  }
  ecaobj$biotic[ecaobj$biotic$lengthsampleweight==0, "lengthsampleweight"] <- ecaobj$biotic[ecaobj$biotic$lengthsampleweight==0, "weight"]
  ecaobj$biotic[missing,"catchcount"] <- eca$biotic[missing, "lengthsamplecount"] * eca$biotic[missing, "catchweight"] / eca$biotic[missing, "lengthsampleweight"]
  
	return(ecaobj)
}

impute_catchweight <- function(ecaobj){
  missing <- is.na(ecaobj$biotic$catchweight)
  ecaobj$biotic[missing,"catchweight"] <- sample(ecaobj$biotic[!missing, "catchweight"], sum(missing), replace=T)
  return(ecaobj)
}

impute_indweight <- function(ecaobj){
	lw_key <- ecaobj$biotic[!is.na(ecaobj$biotic$weight),c("weight", "length")]
	lw_key <- lw_key[!duplicated(lw_key$length),]
	missing <- ecaobj$biotic[is.na(ecaobj$biotic$weight),]
	
	lengths <- missing$length
	weights <- c()
	for (l in lengths){
		weights <- c(weights, lw_key[which(abs(lw_key$length-l)==min(abs(lw_key$length-l))),"weight"][[1]])
	}
	ecaobj$biotic[is.na(ecaobj$biotic$weight),"weight"] <- weights
	return(ecaobj)
}

fake_neighbourmatrix <- function(ecaobj){
	#set up neighbours like a circle
	areas <- unique(ecaobj$landing$spatial)
	num <- rep(2, length(areas))
	map <- c(length(areas),2)
	for (n in 2:(length(areas)-1)){
		map <- c(map, c(n-1, n+1))
	}
	map <- c(map, c(length(areas)-1,1))
	
	return(list(numNeighbours=num, idNeighbours=map))
}

aggregate_landings <- function(ecaobject){
	ecaobject$landingAggregated <- aggregate(list("liveweight"=ecaobject$landing$rundvekt), by=list("season"=ecaobject$landing$season, "gearfactor"=ecaobject$landing$gearfactor, "spatial"=as.numeric(ecaobject$landing$spatial)), FUN=sum)
	return(ecaobject)
}


read_biotic_tables <- function(filename, coltypes=NULL){
	loc <- default_locale()
	tab <- read_delim(filename, delim="\t", col_names = TRUE,
			col_types=coltypes,
			escape_backslash = T, escape_double=F,
			locale = loc, na = c("", "NA"),
			comment = "#", trim_ws = TRUE)
	return(tab)
}

fix_missing_data <- function(ecaobj){
	#mission <- read_biotic_tables("data/Mission.csv")
	#station <- read_biotic_tables("/Users/a5362/code/github/eca_testset_generation/data/Station.csv", paste(rep("c", 46), collapse="")) #coltypes forced because of unexpected type Oppdrag type.
	#catch <- read_biotic_tables("data/Catch.csv")
	#ind <- read_biotic_tables("data/Individual.csv")
	#age <- read_biotic_tables("data/Age.csv")
	
	#add platfrom
	#vessel <- station[,c("serialno.St", "platform.St")]
	#vessel <- vessel[vessel$serialno.St %in% unlist(ecaobj$biotic$serialno),]
	#map <- data.frame(list(platform.St=unique(vessel$platform.St), vessel=rank(unique(vessel$platform.St))))
	#vessel <- merge(vessel, map, all.x=T)
	#ecaobj$biotic <- merge(ecaobj$biotic, vessel, by.x="serialno", by.y="serialno.St")
	
	#add spatial
	map <-data.frame(list(area=unique(ecaobj$landing$fangsthomr)), spatfact=rank(unique(eca$landing$fangsthomr)))
	ecaobj$biotic <- merge(ecaobj$biotic, map, all.x=T)
	ecaobj$biotic$spatial <- ecaobj$biotic$spatfact
	ecaobj$biotic$spatfact <- NULL
	ecaobj$covariateMatrixBiotic$spatial <- ecaobj$biotic$spatial
	
	ecaobj$landing <- merge(ecaobj$landing, map, all.x=T, by.x="fangsthomr", by.y="area")
	ecaobj$landing$spatial <- ecaobj$landing$spatfact
	ecaobj$landing$spatfact <- NULL
	ecaobj$covariateMatrixLanding$spatial <- ecaobj$landing$spatial
	
	return(ecaobj)
}

set_platform_factor <- function(ecaobj){
  map <- data.frame(list(platform.St=unique(ecaobj$biotic$platform), platfact=rank(unique(ecaobj$biotic$platform))))
  ecaobj$biotic <- merge(ecaobj$biotic, map, by.x="platform", by.y="platform.St", all.x=T)
  ecaobj$biotic$platform <- ecaobj$biotic$platfact
  ecaobj$biotic$platfact <- NULL
  return(ecaobj)
}

fix_otolithtypes <- function(ecaobj){
	ecaobj$biotic[!is.na(ecaobj$biotic$otolithtype) & ecaobj$biotic$otolithtype!="1" & ecaobj$biotic$otolithtype!="2" & ecaobj$biotic$otolithtype!="5" & ecaobj$biotic$otolithtype!="4","otolithtype"]<-NA
	warning(paste(sum(is.na(ecaobj$biotic$otolithtype)),"missing otolithtypes set to 4."))
	ecaobj$biotic[is.na(ecaobj$biotic$otolithtype), "otolithtype"]<-4
	
	ecaobj$otholiterror <- c(1,.7,.7,1)
	return(ecaobj)
}

#Filters that need to be applied in data filtering
filter_missing_data <- function(ecaobj){
  warning(paste("filtering NAs for biotic temporal:", sum(is.na(ecaobj$covariateMatrixBiotic$temporal))))
  ecaobj$covariateMatrixBiotic <- ecaobj$covariateMatrixBiotic[!is.na(ecaobj$covariateMatrixBiotic$temporal),]
  ecaobj$biotic <- ecaobj$biotic[!is.na(ecaobj$biotic$temporal),]
  
  warning(paste("filtering NAs for biotic spatial:", sum(is.na(ecaobj$covariateMatrixBiotic$spatial))))
  if (!all(is.na(ecaobj$biotic$spatial) == is.na(ecaobj$covariateMatrixBiotic$spatial))){
    stop("NAs in biotic does not correspond to NAs in covariateMatrixBiotic")
  }
  ecaobj$covariateMatrixBiotic <- ecaobj$covariateMatrixBiotic[!is.na(ecaobj$covariateMatrixBiotic$spatial),]
  ecaobj$biotic <- ecaobj$biotic[!is.na(ecaobj$biotic$spatial),]

  warning(paste("filtering NAs for biotic gear:", sum(is.na(ecaobj$covariateMatrixBiotic$gear))))
  ecaobj$covariateMatrixBiotic <- ecaobj$covariateMatrixBiotic[!is.na(ecaobj$covariateMatrixBiotic$gear),]
  ecaobj$biotic <- ecaobj$biotic[!is.na(ecaobj$biotic$gear),]
  
    
  #warning("imputing gears in landings")
  #ecaobj$landing[is.na(ecaobj$landing$gearfactor), "gearfactor"] <- 2
  #ecaobj$covariateMatrixLanding[is.na(ecaobj$landing$gearfactor), "gearfactor"]<-2
  
  return(ecaobj)
}