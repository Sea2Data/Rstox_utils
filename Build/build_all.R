# Build Rstox:
RstoxBuild::buildRstoxPackage(
	"Rstox", 
	Rversion="3.5", 
	version="1.11.1", 
	imports = list(
		data.table = "1.10.4-3", 
		MASS = NULL,
		methods = NULL,
		RColorBrewer = NULL,
		ggplot2 = NULL,
		gridExtra = NULL,
		plotrix = NULL,
		pbapply = NULL,
		rgdal = NULL,
		rgeos = NULL,
		rJava = NULL,
		readr = NULL,
		sp = NULL,
		XML = NULL,
		scatterpie = NULL
	), 
	rootDir = "~/Code/Github/Rstox", 
	accountName = "Sea2Data", 
	suggests = c(
		"ncdf4", 
		"pgirmess", 
		"Reca", 
		"plotrix",
		"testthat"
	), 
	check=FALSE
)


# Build RstoxData:
RstoxBuild::buildRstoxPackage(
	"RstoxData", 
	Rversion = "3.6", 
	imports = list(
		data.table = "1.12.6", 
		Rcpp = "1.0.0", 
		xml2 = "1.2.2", 
		readr = "1.3.1"
	), 
	suggests = "testthat", 
	linkingto = "Rcpp",
	globalVariables = c("RstoxDataEnv", "xsdObjects", ".", "..allDuplicated", "..colAgg", "..colList", "..columns", "..digits", "..keep", "..key", "..signifDigits", "..sourceColumns", "..targetAndSourceVariables", "..varToExtract", "..x", "AcousticCategory", "Addition", "age", "agingstructure", "ap", "aphia", "BeamKey", "bottomdepthstart", "bottomdepthstop", "catCatchWgt", "catchcount", "catchpartnumber", "catchproducttype", "CatchSpeciesCategoryNumber", "CatchSpeciesCategoryWeight", "CatchSpeciesCode", "CatchSubsampledNumber", "CatchSubsampleWeight", "catchweight", "cc", "Constant", "Country", "cruise", "Cruise", "CruiseKey", "cw", "DateTime", "direction", "DoorType", "EchoType", "EDSU", "FishID", "fishingdepthmax", "fishingdepthmin", "freq", "g", "gear", "Gear", "gearcondition", "GearExp", "gearflow", "HaulNo", "HaulNumber", "HaulVal", "HaulValidity", "hv", "inapplicableFormats", "individualweight", "isCrustacean", "isHerringOrSprat", "isHerringOrSpratOrMackerel", "latitudeend", "latitudestart", "LengthClass", "LengthCode", "lengthmeasurement", "lengthsamplecount", "lengthsampleweight", "lenInterval", "level", "lngtClass", "lngtCode", "LocalID", "LogDuration", "LogKey", "LogOrigin", "longitudeend", "longitudestart", "lsc", "lsCountTot", "maturationstage", "maturity", "meanW", "MiddleDateTime", "missionstartdate", "missionstopdate", "ms", "N", "nation", "nInd", "noMeas", "NumberAtLength", "nWithWeight", "parasite", "platformname", "preferredagereading", "Quarter", "readability", "ReplaceBy", "reportInMM", "res", "rowIndex", "s", "SaCategory", "sampleFac", "samplequality", "sampletype", "Scaling", "serialnumber", "sex", "Ship", "sp", "specialstage", "specimenid", "SpecVal", "start_time", "StartDateTime", "startyear", "station", "stationstartdate", "stationstarttime", "stationstopdate", "stationstoptime", "stationtype", "StatRec", "stomach", "StopDateTime", "stoxBioticObject", "subFactor", "SubsampledNumber", "subWeight", "suffixes", "Survey", "SweepLngt", "target", "Time", "tissuesample", "totalNo", "totWeight", "transceiver", "trawldoorarea", "trawldoorspread", "trawldoortype", "trawldoorweight", "VariableName", "verticaltrawlopening", "WeightMeasurement", "winddirection", "windspeed", "wingspread", "wiredensity", "wirediameter", "wirelength"), 
	addManual = TRUE, 
	check = FALSE
)


# Build RstoxBase:
RstoxBuild::buildRstoxPackage(
	"RstoxBase", 
	Rversion = "3.6", 
	imports = list(
		data.table ="1.10.4-3", 
		rgdal = "1.4.7",
		rgeos = "0.5.2",
		sp = "1.3.2"
	), 
	importToNamespace = "data.table", 
	suggests = "testthat", 
	internal.dependencies = "RstoxData", 
	additional_repositories = "https://stoxproject.github.io/repo", 
	globalVariables = c(
	    ":=", ".", "..abundanceGrouping", "..atMissingLengthGroup", "..by", "..columnOrder",
	    "..extract", "..extractFromDataCopy", "..haulGrouping", "..Hauls", "..intervalVector",
	    "..keys", "..keysSansSample", "..LengthDistributionType", "..LengthInterval",
	    "..LengthIntervalWidths", "..lengthVar", "..presentResolutionVariables", "..refvar",
	    "..relevantVariables", "..resolutionVar", "..RowIndex", "..Stratum", "..sumWeigthsBy",
	    "..toAdd", "..validVariables", "..variablesToGetFromAbundanceData", "..vars",
	    "..VerticalResolutionMax", "..VerticalResolutionMin", "..WeightingFactors", ".N",
	    "abundanceWeightFactor", "AllStrata", "approx", "Area", "assignmentID", "assignmentPasted",
	    "backscatteringCrossSection", "Biomass", "CatchFractionCount", "CatchFractionWeight",
	    "ChannelReferenceDepth", "ChannelReferenceOrientation", "crossSection", "Cruise", "Density",
	    "DensityWeight", "Depth", "DepthExponent", "distance", "EDSU", "EDSUIIndex",
	    "EffectiveLogDistance", "EffectiveTowedDistance", "functionName",
	    "getRstoxFrameworkDefinitions", "Haul", "haulWeightFactor", "imputeSeed", "Individual",
	    "individualCount", "IndividualIndex", "IndividualKey", "IndividualRoundWeight",
	    "IndividualTotalLength", "IndividualTotalLengthMiddle", "inside", "intervalIndex", "Layer",
	    "LengthDistributionType", "LengthDistributionWeight", "LengthExponent", "LengthGroup",
	    "LengthResolution", "MaxChannelDepth", "MaxChannelRange", "MeanLengthDistributionWeight",
	    "MeanNASCWeight", "mergeDataTables", "MergeStoxBiotic", "midDepth", "midIndividualTotalLength",
	    "MinChannelDepth", "MinChannelRange", "multiple", "na.omit", "NASCDistributed", "NASCWeight",
	    "NewValue", "packageName", "PSU", "quantile", "raisingFactor", "ReplaceLevel",
	    "ReplaceRowIndex", "representativeBackscatteringCrossSection",
	    "representativeBackscatteringCrossSectionNormalized", "RowIndex", "RstoxBaseEnv", "sd",
	    "StartDateTime", "StopDateTime", "Stratum", "SummedWeights", "sumWeightedCount", "Survey",
	    "SweepWidthNauticalMile", "TargetStrength", "TargetStrength0", "TargetStrengthFunction",
	    "TotalLength", "V1", "Value", "weighted", "WeightedCount", "WeightingFactor",
	    "weightingParameter", "x", "y"), 
	addManual = TRUE, 
	check = FALSE
)






	
# Build RstoxFramework:
RstoxBuild::buildRstoxPackage(
	"RstoxFramework", 
	Rversion = "3.6", # This is due to change in formals() which now includes the 'envir' argument which we have employed, and the fact that sampling has changed as of R 3.6.
	imports = list(
		data.table = "1.12.6", 
		geojsonio = "0.8.0", 
		jsonlite = "1.6", 
		jsonvalidate = "1.1.0", 
		sp = "1.3.2",
		xml2 ="1.2.2"
	), 
	importToNamespace = "data.table", 
	suggests = "testthat", 
	internal.dependencies = c(
		"RstoxData", 
		"RstoxBase"
	), 
	additional_repositories = "https://stoxproject.github.io/repo", 
	globalVariables = c("RstoxFrameworkEnv", ":=", ".", "..PSU", "..activeProcessID", "..clickPointNames", "..coordinateNames", "..functionInputs", "..functionName", "..functionParameters", "..infoToKeep", "..processDirty", "..newProcessName", "CruiseKey", "Latitude", "Latitude2", "LogOrigin", "LogOrigin2", "Longitude", "Longitude2", "PSU", "atRemove", "canShowInMap", "filePahts", "functionName", "functionOutputDataType", "hasBeenRun", "hasProcessData", "modelName", "processDirty", "name", "possibleValues", "processID", "projectPath", "value"), 
	check = FALSE
)


# Build RstoxFDA:
RstoxBuild::buildRstoxPackage(
	"RstoxFDA",
	Rversion = "3.6",
	imports = list(
		readr = "1.3.1",
		data.table = "1.12.6",
		ggplot2 = "3.2.1",
		RColorBrewer = "1.1-2",
		gridExtra = "2.3",
		sp = "1.3.2"
	),
	internal.dependencies = c(
		"RstoxData", 
		"RstoxBase"
	), 
	suggests = c("testthat", "Reca"),
	check = FALSE
)


# Build RstoxTempdoc:
RstoxBuild::buildRstoxPackage(
	"RstoxTempdoc", 
	Rversion = "3.6", 
	check = FALSE
)


# Build RstoxBuild:
RstoxBuild::buildRstoxPackage(
	"RstoxBuild", 
	Rversion = "3.6", 
	check = FALSE, 
	imports = c("usethis", "devtools"), 
	suggests = c("Rstox", "png", "jpeg", "tiff", "rJava", "callr")
)




# Build RstoxAPI:
RstoxBuild::buildRstoxPackage(
	"RstoxAPI", 
	Rversion = "3.6", 
	check = FALSE, 
	imports = list(
		devtools = NULL, 
		data.table ="1.10.4-3"
	), 
	internal.dependencies = "RstoxFramework", 
	additional_repositories = "https://stoxproject.github.io/repo"
)
