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
		xml2 = "1.2.2", 
		xslt = "1.4",
		units = "0.7", 
		#stringr = "1.0.0", 
		stringi = "1.4.3"
	), 
	suggests = "testthat", 
	linkingto = "Rcpp",
	globalVariables = c(".", "..Country", "..Organisation", "..SurveyName", "..allDuplicated", "..colAgg", "..colList", "..columns", "..digits", "..keep", "..key", "..parameterNames", "..signifDigits", "..sourceColumns", "..targetAndSourceVariables", "..toKeep", "..valueVariablesInTranslation", "..varToExtract", "..variableKeys", "..variablesInTable", "..x", "AcousticCategory", "Addition", "BeamKey", "Channel", "ChannelDepthLower", "ChannelDepthUpper", "Constant", "Country", "Cruise", "CruiseKey", "DateTime", "DoorType", "EDSU", "EchoType", "FishID", "Gear", "GearEx", "HaulNo", "HaulVal", "LengthClass", "LengthCode", "LocalID", "LogKey", "N", "Number", "NumberAtLength", "Quarter", "ReplaceBy", "SaCategory", "Scaling", "Ship", "SpecVal", "SpeciesCategoryNumber", "SpeciesCategoryWeight", "SpeciesCode", "StatRec", "SubsampleWeight", "SubsampledNumber", "Survey", "SweepLngt", "Time", "TransducerOrientation", "Validity", "VariableName", "WeightMeasurement", "age", "agingstructure", "ap", "aphia", "bottomdepthstart", "bottomdepthstop", "catCatchWgt", "catchcount", "catchpartnumber", "catchproducttype", "catchweight", "cc", "cruise", "cw", "direction", "fishingdepthmax", "fishingdepthmin", "freq", "g", "gear", "gearcondition", "gearflow", "hv", "inapplicableFormats", "individualweight", "isCrustacean", "isHerringOrSprat", "isPelagic", "latitudeend", "latitudestart", "lenInterval", "lengthCode", "lengthmeasurement", "lengthresolution", "lengthsamplecount", "lengthsampleweight", "level", "lngtClass", "lngtCode", "logstart", "longitudeend", "longitudestart", "lsCountTot", "lsc", "maturationstage", "maturity", "maxFishID", "meanW", "missionstartdate", "missionstopdate", "ms", "nInd", "nWithWeight", "nation", "noMeas", "parasite", "platformname", "preferredagereading", "readability", "reportInMM", "reportingUnit", "res", "rowIndex", "s", "sampleFac", "samplequality", "sampletype", "serialnumber", "sex", "shortname", "sp", "specialstage", "specimenid", "start_time", "startyear", "station", "stationstartdate", "stationstarttime", "stationstopdate", "stationstoptime", "stomach", "stoxBioticObject", "subFactor", "subWeight", "suffixes", "sweeplength", "target", "tissuesample", "totWeight", "totalNo", "transceiver", "trawldoorarea", "trawldoorspread", "trawldoortype", "trawldoorweight", "verticaltrawlopening", "winddirection", "windspeed", "wingspread", "wiredensity", "wirediameter", "wirelength"), 
	addManual = TRUE, 
	check = FALSE
)


# Build RstoxBase:
RstoxBuild::buildRstoxPackage(
	"RstoxBase", 
	Rversion = "3.6", 
	imports = list(
		data.table = "1.12.6", 
		geojsonsf = "2.0.0", 
		rgeos = "0.5.2",
		rgdal = "1.5.0",
		sp = "1.3.2", 
		sf = "0.9.0", 
		ggplot2 = "3.0.0", 
		units = "0.7"
	), 
	importToNamespace = "data.table", 
	suggests = "testthat", 
	internal.dependencies = "RstoxData", 
	additional_repositories = "https://stoxproject.github.io/repo", 
	globalVariables = c(".", "..Hauls", "..LengthDistributionType", "..LengthInterval", "..LengthIntervalWidths", "..VerticalResolutionMax", "..VerticalResolutionMin", "..WeightingFactors", "..acceptedColumns", "..atMissingLengthGroup", "..by", "..cols", "..columnsToKeep", "..extract", "..extractFromDataCopy", "..haulGrouping", "..intervalVector", "..keys", "..keysSansSample", "..lengthVar", "..locatedStratum", "..meanBy", "..paddingVariables", "..presentResolutionVariables", "..refvar", "..resolutionVar", "..sumBy", "..variablesToGetFromAbundanceData", "..vars", "AcousticCategory", "AcousticCategoryKey", "Area", "Beam", "BeamKey", "Biomass", "CatchFractionCount", "CatchFractionWeight", "Channel", "ChannelReferenceDepth", "ChannelReferenceKey", "ChannelReferenceTilt", "ChannelReferenceType", "Cruise", "CruiseKey", "DateTime", "Density", "DensityWeight", "Depth", "DepthExponent", "EDSU", "EffectiveLogDistance", "EffectiveTowDistance", "Haul", "Individual", "IndividualIndex", "IndividualKey", "IndividualRoundWeight", "IndividualTotalLength", "IndividualTotalLengthMiddle", "Layer", "LengthDistributionType", "LengthExponent", "LengthGroup", "LengthResolution", "LogDuration", "LogKey", "LogOrigin", "MaxChannelDepth", "MaxChannelRange", "MeanLengthDistributionWeight", "MeanNASCWeight", "MergeStoxBiotic", "MiddleDateTime", "MinChannelDepth", "MinChannelRange", "NASCKey", "NASCWeight", "NewValue", "PSU", "ReplaceIndividual", "ReplaceIndividualIndex", "ReplaceLevel", "SSU", "SSUIndex", "SplitAcousticCategory", "StartDateTime", "StationLevel", "StopDateTime", "Stratum", "SummedWeights", "Survey", "SweepWidth", "TargetStrength", "TargetStrength0", "TargetStrengthFunction", "TotalLength", "V1", "Value", "WeightedCount", "WeightingFactor", "aggregationVariables", "assignmentID", "assignmentPasted", "backscatteringCrossSection", "crossSection", "distance", "functionName", "haulWeightFactor", "imputeSeed", "individualCount", "inside", "intervalIndex", "midDepth", "midIndividualTotalLength", "minDistance", "multiple", "packageName", "raisingFactor", "representativeBackscatteringCrossSection", "representativeBackscatteringCrossSectionNormalized", "sd", "sumWeightedCount", "weighted", "weightingParameter", "x", "y"), 
	addManual = TRUE, 
	check = FALSE
)




# Build RstoxFramework:
system.time(RstoxBuild::buildRstoxPackage(
	"RstoxFramework", 
	Rversion = "3.6", # This is due to change in formals() which now includes the 'envir' argument which we have employed, and the fact that sampling has changed as of R 3.6.
	imports = list(
		data.table = "1.12.6", 
		#geojsonio = "0.8.0", 
		geojsonsf = "2.0.0",
		jsonlite = "1.6", 
		jsonvalidate = "1.1.0", 
		rgdal = "1.5.0",
		sp = "1.3.2",
		sf = "0.9.0", 
		xml2 = "1.3.0", 
		stringr = "1.0.0", 
		stringi = "1.4.3"
	), 
	importToNamespace = "data.table", 
	suggests = c(
		"testthat"
	), 
	internal.dependencies = c(
		"RstoxData", 
		"RstoxBase"
	), 
	additional_repositories = "https://stoxproject.github.io/repo", 
	globalVariables = c(".", "..Cruise", "..DateTime", "..EDSUInfoToKeep", "..PSU", "..activeProcessID", "..clickPointNames", "..coordinateNames", "..functionInputs", "..functionName", "..functionParameters", "..haulInfoToKeep", "..ind", "..newProcessName", "..processDirty", "..propertyDirty", "..stationInfoToKeep", "..validInd", "BootstrapID", "CruiseKey", "EDSU", "JavaJEXL2R", "Latitude", "Latitude2", "Layer", "LogOrigin", "LogOrigin2", "Longitude", "Longitude2", "PSU", "Package", "ProcessName", "ResampleFunction", "StoX", "Version", "attriributes<-", "canShowInMap", "capture.output", "col2rgb", "colorRampPalette", "dataTable2SpatialPolygonsDataFrame", "dataType", "download.file", "functionName", "functionOutputDataType", "hasProcessData", "install.packages", "modelName", "models", "name", "newVersion", "possibleValues", "processID", "processIndex", "processName", "processNames", "projectList", "projectPath", "read.table", "remove.packages", "resampledCountWithUniqueName", "value"), 
	check = FALSE, 
	parallelTest = TRUE
))



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




# Build RstoxBuild:
RstoxBuild::buildRstoxPackage(
	"RstoxBuild", 
	version = "1.0", 
	Rversion = "3.5", 
	check = FALSE, 
	imports = c("usethis", "devtools"), 
	suggests = c("Rstox", "png", "jpeg", "tiff", "rJava", "callr")
)




convertGlobalVariables <- function(x = "~/ttt.txt") {
	d <- unlist(lapply(readLines(x), strsplit, " ", fixed = TRUE))
	d <- paste0("\"", paste0(d, collapse = "\", \""), "\"")
	writeLines(d, x)
}
convertGlobalVariables()



