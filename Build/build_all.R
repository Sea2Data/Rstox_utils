feil

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
		xml2 = "1.2.2"
	), 
	suggests = list(
		readr = "1.3.1", 
		testthat = NULL
	), 
	linkingto = "Rcpp",
	globalVariables = c(".", "..Country", "..Organisation", "..SurveyName", "..allDuplicated", "..colAgg", "..colList", "..columns", "..digits", "..keep", "..key", "..signifDigits", "..sourceColumns", "..targetAndSourceVariables", "..varToExtract", "..x", "AcousticCategory", "Addition", "BeamKey", "Constant", "Country", "Cruise", "CruiseKey", "DateTime", "DoorType", "EDSU", "EchoType", "FishID", "Gear", "GearExp", "HaulNo", "HaulVal", "LengthClass", "LengthCode", "LocalID", "LogKey", "N", "Number", "NumberAtLength", "Quarter", "ReplaceBy", "SaCategory", "Scaling", "Ship", "SpecVal", "SpeciesCategoryNumber", "SpeciesCategoryWeight", "SpeciesCode", "StatRec", "SubsampleWeight", "SubsampledNumber", "Survey", "SweepLngt", "Time", "TransducerOrientation", "Validity", "VariableName", "WeightMeasurement", "age", "agingstructure", "ap", "aphia", "bottomdepthstart", "bottomdepthstop", "catCatchWgt", "catchcount", "catchpartnumber", "catchproducttype", "catchweight", "cc", "cruise", "cw", "direction", "fishingdepthmax", "fishingdepthmin", "freq", "g", "gear", "gearcondition", "gearflow", "hv", "inapplicableFormats", "individualweight", "isCrustacean", "isHerringOrSprat", "isHerringOrSpratOrMackerel", "latitudeend", "latitudestart", "lenInterval", "lengthmeasurement", "lengthsamplecount", "lengthsampleweight", "level", "lngtClass", "lngtCode", "longitudeend", "longitudestart", "lsCountTot", "lsc", "maturationstage", "maturity", "meanW", "missionstartdate", "missionstopdate", "ms", "nInd", "nWithWeight", "nation", "noMeas", "parasite", "platformname", "preferredagereading", "readability", "reportInMM", "res", "rowIndex", "s", "sampleFac", "samplequality", "sampletype", "serialnumber", "sex", "sp", "specialstage", "specimenid", "start_time", "startyear", "station", "stationstartdate", "stationstarttime", "stationstopdate", "stationstoptime", "stationtype", "stomach", "stoxBioticObject", "subFactor", "subWeight", "suffixes", "target", "tissuesample", "totWeight", "totalNo", "transceiver", "trawldoorarea", "trawldoorspread", "trawldoortype", "trawldoorweight", "verticaltrawlopening", "winddirection", "windspeed", "wingspread", "wiredensity", "wirediameter", "wirelength"), 
	addManual = TRUE, 
	check = FALSE
)


# Build RstoxBase:
RstoxBuild::buildRstoxPackage(
	"RstoxBase", 
	Rversion = "3.6", 
	imports = list(
		data.table ="1.10.4-3", 
		#rgdal = "1.4.7",
		rgeos = "0.5.2",
		sp = "1.3.2", 
		sf = "0.9.0", 
		geojsonsf = "2.0.0"
	), 
	importToNamespace = "data.table", 
	suggests = "testthat", 
	internal.dependencies = "RstoxData", 
	additional_repositories = "https://stoxproject.github.io/repo", 
	globalVariables = c(".", "..Hauls", "..LengthDistributionType", "..LengthInterval", "..LengthIntervalWidths", "..RowIndex", "..Stratum", "..VerticalResolutionMax", "..VerticalResolutionMin", "..WeightingFactors", "..atMissingLengthGroup", "..by", "..extract", "..extractFromDataCopy", "..haulGrouping", "..intervalVector", "..keys", "..keysSansSample", "..lengthVar", "..presentResolutionVariables", "..refvar", "..resolutionVar", "..sumWeigthsBy", "..variablesToGetFromAbundanceData", "..vars", "AllStrata", "Area", "Biomass", "CatchFractionCount", "CatchFractionWeight", "ChannelReferenceDepth", "ChannelReferenceTilt", "Cruise", "DateTime", "Density", "DensityWeight", "Depth", "DepthExponent", "EffectiveLogDistance", "EffectiveTowedDistance", "Haul", "Individual", "IndividualIndex", "IndividualKey", "IndividualRoundWeight", "IndividualTotalLength", "IndividualTotalLengthMiddle", "Layer", "LengthDistributionType", "LengthExponent", "LengthGroup", "LengthResolution", "LogDuration", "LogOrigin", "MaxChannelDepth", "MaxChannelRange", "MeanLengthDistributionWeight", "MeanNASCWeight", "MergeStoxBiotic", "MiddleDateTime", "MinChannelDepth", "MinChannelRange", "NASCDistributed", "NASCWeight", "NewValue", "PSU", "ReplaceLevel", "ReplaceRowIndex", "RowIndex", "SSU", "SSUIndex", "StartDateTime", "StationLevel", "StopDateTime", "Stratum", "SummedWeights", "Survey", "SweepWidthNauticalMile", "TargetStrength", "TargetStrength0", "TargetStrengthFunction", "TotalLength", "V1", "Value", "WeightedCount", "WeightingFactor", "assignmentID", "assignmentPasted", "backscatteringCrossSection", "crossSection", "distance", "functionName", "getRstoxFrameworkDefinitions", "haulWeightFactor", "imputeSeed", "individualCount", "inside", "intervalIndex", "midDepth", "midIndividualTotalLength", "multiple", "packageName", "raisingFactor", "representativeBackscatteringCrossSection", "representativeBackscatteringCrossSectionNormalized", "sd", "sumWeightedCount", "weighted", "weightingParameter", "x", "y"), 
	addManual = TRUE, 
	check = FALSE
)




# Build RstoxFramework:
RstoxBuild::buildRstoxPackage(
	"RstoxFramework", 
	Rversion = "3.6", # This is due to change in formals() which now includes the 'envir' argument which we have employed, and the fact that sampling has changed as of R 3.6.
	imports = list(
		data.table = "1.12.6", 
		#geojsonio = "0.8.0", 
		geojsonsf = "2.0.0",
		jsonlite = "1.6", 
		jsonvalidate = "1.1.0", 
		sp = "1.3.2",
		sf = "0.9.0", 
		xml2 = "1.2.2", 
		stringr = "1.0.0"
	), 
	importToNamespace = "data.table", 
	suggests = "testthat", 
	internal.dependencies = c(
		"RstoxData", 
		"RstoxBase"
	), 
	additional_repositories = "https://stoxproject.github.io/repo", 
	globalVariables = c("RstoxFrameworkEnv", ":=", ".", "..PSU", "..activeProcessID", "..clickPointNames", "..coordinateNames", "..functionInputs", "..functionName", "..functionParameters", "..infoToKeep", "..processDirty", "..newProcessName", "CruiseKey", "Latitude", "Latitude2", "LogOrigin", "LogOrigin2", "Longitude", "Longitude2", "PSU", "atRemove", "canShowInMap", "filePahts", "functionName", "functionOutputDataType", "hasBeenRun", "hasProcessData", "modelName", "processDirty", "name", "possibleValues", "processID", "projectPath", "value", "..EDSUInfoToKeep", "..haulInfoToKeep", "..ind", "..propertyDirty", "..stationInfoToKeep", "..validInd", "BootstrapID", "JavaJEXL2R", "ProcessName", "ResampleFunction", "Stratum", "col2rgb", "colorRampPalette", "dataTable2SpatialPolygonsDataFrame", "dataType", "functionArguments", "packageVersion", "processIndex", "processName", "readProjectDescriptionXML", "resampledCountWithUniqueName", "writeProjectXML"), 
	check = FALSE
)


# Build RstoxFDA:
RstoxBuild::buildRstoxPackage(
	"RstoxFDA",
	Rversion = "3.6",
	imports = list(
	  stats = "3.5.0",
	  methods = "3.5.0",
	  utils = "3.5.0",
	  readr = "1.3.1",
	  data.table = "1.12.6",
	  ggplot2 = "3.2.1",
	  RColorBrewer = "1.1-2",
	  gridExtra = "2.3",
	  sp = "1.3.2",
	  sf = "0.9.0",
	  RstoxData = "1.0.25"
	),
	internal.dependencies = c(
		"RstoxData", 
		"RstoxBase"
	), 
	suggests = c("testthat", "Reca", "rnaturalearth"),
	check = FALSE
)


# Build RstoxTempdoc:
RstoxBuild::buildRstoxPackage(
	"RstoxTempdoc", 
	Rversion = "3.6", 
	check = FALSE
)




## Build RstoxAPI:
#RstoxBuild::buildRstoxPackage(
#	"RstoxAPI", 
#	Rversion = "3.6", 
#	check = FALSE, 
#	imports = list(
#		devtools = NULL, 
#		data.table ="1.10.4-3"
#	), 
#	internal.dependencies = "RstoxFramework", 
#	additional_repositories = "https://stoxproject.github.io/repo"
#)



convertGlobalVariables <- function(x = "~/ttt.txt") {
	d <- unlist(lapply(readLines(x), strsplit, " ", fixed = TRUE))
	d <- paste0("\"", paste0(d, collapse = "\", \""), "\"")
	writeLines(d, x)
}
convertGlobalVariables()



