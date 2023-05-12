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
		"Reca", 
		"plotrix",
		"testthat"
	), 
	check=FALSE
)



# install.packages(c("devtools", "remotes", "data.table", "semver", "geojsonsf", "ggplot2", "sp", "sf", "maps", "lwgeom", "ncdf4", "jsonvalidate"))


# Build RstoxData:
system.time(RstoxBuild::buildRstoxPackage(
	"RstoxData", 
	Rversion = "3.6", 
	type = "minor", 
	prerelease = TRUE, 
	#version = "1.9.0", 
	imports = list(
		data.table = "1.12.0", #"1.12.6", 
		Rcpp = "1.0.0", 
		xml2 = "1.2.0", #"1.2.2", 
		#xslt = "1.4",
		units = "0.7", 
		#stringr = "1.0.0", 
		stringi = "1.4.0" #"1.4.3"
	), 
	suggests = list(
	    #testthat = "2.0.0", 
	    tinytest = "1.2.0"
	), 
	linkingto = "Rcpp",
	globalVariables = c(".", "..Country", "..NAToInsert", "..Organisation", "..SurveyName", "..atResolution", "..colList", "..columns", "..digits", "..groupingVariables", "..keep", "..parameterNames", "..relevantColumns", "..replacement", "..signifDigits", "..sourceColumns", "..targetAndSourceVariables", "..toKeep", "..valueVariablesInTranslation", "..varToExtract", "..variableKeys", "..variablesInTable", "..x", "AcousticCategory", "Addition", "BeamKey", "CANoAtLngt", "Channel", "ChannelDepthLower", "ChannelDepthUpper", "ChannelReferenceKey", "ChannelReferenceType", "Constant", "Country", "Cruise", "CruiseKey", "DateTime", "DoorType", "EDSU", "EchoType", "EffectiveTowDistance", "FishID", "Frequency", "Gear", "GearEx", "HLNoAtLngt", "HaulNo", "ID", "LngtClass", "LngtCode", "LocalID", "LogKey", "N", "NewValue", "NumberAtLength", "NumberOfIndividualsToGenerate", "NumberOfSampledIndividuals", "Quarter", "ReplaceBy", "ResolutionCode", "SaCategory", "Scaling", "Ship", "SpecVal", "SpeciesCategoryNumber", "SpeciesCategoryWeight", "SpeciesCode", "StatRec", "SubsampleWeight", "SubsampledNumber", "Survey", "SweepLngt", "Time", "TransducerOrientation", "Value", "VariableName", "WeightMeasurement", "acocat", "age", "agingstructure", "aphia", "bottomdepthstart", "bottomdepthstop", "catCatchWgt", "catchcount", "catchpartnumber", "catchproducttype", "catchweight", "ch", "cruise", "currentReportingUnit", "direction", "distance", "fishingdepthcount", "fishingdepthmax", "fishingdepthmin", "freq", "gear", "gearflow", "inapplicableFormats", "individualweight", "integrator_dist", "iskey", "lat_start", "lat_stop", "latitudeend", "latitudestart", "lengthCode", "lengthmeasurement", "lengthresolution", "lengthsamplecount", "lengthsampleweight", "level", "liverweight", "lngtClass", "lngtCode", "log_start", "logstart", "lon_start", "lon_stop", "longitudeend", "longitudestart", "lsCountTot", "maturity", "maxFishID", "max_bot_depth", "meanW", "missionstartdate", "missionstopdate", "nInd", "nation", "newClass", "newLngtCode", "newLngtCodeNumeric", "newReportingUnit", "noMeas", "parasite", "pel_ch_thickness", "platform", "platformname", "preferredagereading", "readability", "reportingUnit", "res", "sa", "sampleFac", "serialnumber", "sex", "shortname", "specialstage", "specimenid", "start_time", "startyear", "station", "stationstartdate", "stationstarttime", "stationstopdate", "stationstoptime", "stomach", "stoxBioticObject", "subFactor", "subWeight", "suffixes", "sum_sa.x", "sum_sa.y", "sweeplength", "tagid", "tagtype", "target", "threshold", "tissuesample", "totalNo", "transceiver", "translationListOne", "trawldoorarea", "trawldoorspread", "trawldoortype", "trawldoorweight", "variableToTranslate", "verticaltrawlopening", "vesselspeed", "winddirection", "windspeed", "wingspread", "wiredensity", "wirediameter", "wirelength", "xsdObjects"
), 
	addManual = TRUE, 
	avoid_compileAttributes_error = TRUE, 
	check = FALSE
))

  


# Build RstoxBase:
system.time(RstoxBuild::buildRstoxPackage(
	"RstoxBase", 
	Rversion = "3.6", 
	#type = "minor", 
	#prerelease = TRUE, 
	type = "patch", 
	version = "1.11.2", 
	imports = list(
		data.table = "1.12.0", #"1.12.6", 
		geojsonsf = "2.0.0", 
		#rgeos = "0.5.2",
		#rgdal = "1.5.0",
		sp = "1.3.0", #"1.3.2", 
		sf = "0.9.0", 
		lwgeom = "0.2-0", # Used for calculation of area of sf object in simplifyStratumPolygon(). Added here since it is only suggested by sf, but RstoxBase needs it.
		maps = "0.2-0", # Only suggested by ggplot2, but needed for map_data in plotting functions.
		methods = "3.6.0", 
		ggplot2 = "3.0.0", 
		units = "0.7", 
		#methods = "3.6.2", Do we really need to specify this minimum version? I do not remember why.
		xml2 = "1.2.0", #"1.2.2", 
		stringi = "1.4.0" #"1.4.3"
	), 
	importToNamespace = "data.table", 
	suggests = list(
	    #testthat = "2.0.0", 
	    tinytest = "1.2.0"
	), 
	internal.dependencies = "RstoxData", 
	additional_repositories = "https://stoxproject.github.io/repo", 
	globalVariables = c(".", "..Cruise", "..DateTime", "..DensityType", "..Hauls", "..LengthDistributionType", "..SpeciesCategoryCatchType", "..VerticalResolutionMax", "..VerticalResolutionMin", "..WeightingFactors", "..acceptedColumns", "..atMissingLengthGroup", "..by", "..cols", "..columnsToKeep", "..extract", "..extractFromDataCopy", "..haulGrouping", "..horizontalResolution", "..keys", "..keysSansSample", "..lengthInterval", "..lengthIntervalWidths", "..lengthVar", "..locatedStratum", "..meanBy", "..paddingVariables", "..presentResolutionVariables", "..refvar", "..resolutionVar", "..sumBy", "..tomerge", "..variablesToGetFromQuantityData", "..vars", "Abundance", "AcousticCategory", "AcousticCategoryKey", "AcousticTargetStrength", "AllHaulsHaveAllSpeciesCategory", "AllHaulsHaveAnySpeciesCategory", "Area", "Beam", "BeamKey", "Biomass", "CatchFractionNumber", "CatchFractionWeight", "Channel", "ChannelReferenceDepth", "ChannelReferenceKey", "ChannelReferenceTilt", "ChannelReferenceType", "ContainsAllSpeciesCategory", "ContainsAnySpeciesCategory", "Cruise", "CruiseKey", "CruiseKey1", "DateTime", "Density", "DensityType", "DensityWeight", "Depth", "DepthExponent", "EDSU", "EffectiveLogDistance", "EffectiveTowDistance", "EstimationMethod", "Haul", "HaulKey", "Individual", "IndividualIndex", "IndividualKey", "IndividualRoundWeight", "IndividualTotalLength", "IndividualTotalLengthMiddle", "Latitude", "Layer", "LengthDistributionType", "LengthExponent", "LengthResolution", "LogDuration", "LogKey", "LogOrigin", "Longitude", "MaxChannelDepth", "MaxChannelRange", "MeanNASCWeight", "MiddleDateTime", "MinChannelDepth", "MinChannelRange", "N", "NASCKey", "NASCWeight", "NumberOfAssignedHauls", "PSU", "ReplaceIndividual", "ReplaceIndividualIndex", "ReplaceLevel", "SSU", "SSUIndex", "Sample", "SampleNumber", "SampleWeight", "SpeciesCategory", "SpeciesCategoryCatchWeight", "SplitAcousticCategory", "StartDateTime", "Station", "StationLevel", "StopDateTime", "Stratum", "StratumPolygon", "SummedWeights", "Survey", "SweepWidth", "TableName", "TargetStrength", "TargetStrength0", "TargetStrengthFunction", "TempLengthGroupUsedInSuperIndividuals", "TotalLength", "V1", "WeightedNumber", "WeightingFactor", "assignmentID", "assignmentPasted", "backscatteringCrossSection", "crossSection", "distance", "functionName", "getTrueCentroid", "ggtitle", "haulWeightFactor", "imputeSeed", "includeintotal", "individualNumber", "individualWeightFactor", "inside", "insideRadius", "intervalIndex", "median", "midIndividualTotalLength", "minDistance", "missingAssignment", "missingSpecies", "multiple", "numberOfIndividuals", "numberOfSubSamples", "packageName", "raisingFactor", "representativeBackscatteringCrossSection", "representativeBackscatteringCrossSectionNormalized", "setUnit", "specificationParameter", "specified", "sumIndividualWeightFactor", "sumWeightedNumber", "weighted", "weightingParameter", "x", "y"
), 
	addManual = TRUE, 
	check = FALSE
))


# Build RstoxFramework:
system.time(RstoxBuild::buildRstoxPackage(
    "RstoxFramework", 
    Rversion = "3.6", # This is due to change in formals() which now includes the 'envir' argument which we have employed, and the fact that sampling has changed as of R 3.6.
    version = "3.6.1", 
    type = "patch", 
    prerelease = TRUE, 
    imports = list(
        data.table = "1.12.0", #"1.12.6", 
        #geojsonio = "0.8.0", 
        geojsonsf = "2.0.0",
        jsonlite = "1.6", 
        jsonvalidate = "1.3.0", #"1.3.2", 
        #rgdal = "1.5.0",
        sp = "1.3.0", #"1.3.2",
        sf = "0.9.0", 
        methods = "3.6.0", # To avoid message during testing:  '::' or ':::' import not declared from: ‘methods’
        scales = "1.1.0", # To avoid message during testing:  '::' or ':::' import not declared from: ‘scales’
        #xml2 = "1.2.2", 
        #stringr = "1.0.0", 
        stringi = "1.4.0", #"1.4.3", 
        ggplot2 = "3.0.0", 
        semver = "0.2.0", # Used to sort versions when applying backward compatibility, as per https://www.r-bloggers.com/2022/07/overview-sorting-version-codes/ 
        ncdf4 = "1.18"
    ), 
    importToNamespace = "data.table", 
    suggests = list(
        tinytest = "1.2.0", 
        remotes = "2.0.0"#, 
        #Rstox = "1.11.1"
    ), 
    #linkingto = "cpp11",
    internal.dependencies = c(
        "RstoxData", 
        "RstoxBase"
    ), 
    internal.suggests = c(
        "RstoxFDA"
    ), 
    additional_repositories = "https://stoxproject.github.io/repo", 
    globalVariables = c("..BootstrapID", "..EDSUInfoToKeep", "..PSU", "..activeProcessID", "..clickPointNames", "..coordinateNames", "..functionInputs", "..functionName", "..functionParameters", "..haulInfoToKeep", "..ind", "..newProcessName", "..presentVariables", "..processDirty", "..propertyDirty", "..skipNAAt", "..stationInfoToKeep", "..subsetByNAOn_New", "..subsetByNAOn_Old", "..toKeep", "..validInd", "AcoCat", "BeamKey", "BootstrapData", "BootstrapID", "BootstrapSampleFactor", "CruiseKey", "Haul", "Layer", "LogKey", "LogOrigin", "LogOrigin2", "PSU", "Package", "PolygonKey", "ProcessName", "ResampleFunction", "SampleUnit", "Station", "StationWeight", "StoX", "Stratum", "Version", "WeightingFactor", "assignmentPasted", "binaryPath", "canShowInMap", "capture.output", "ch", "col2rgb", "colorRampPalette", "download.file", "filePath", "freq", "functionInputError", "functionName", "functionOutputDataType", "hasProcessData", "modelName", "name", "newVersion", "numStations", "offset", "possibleValues", "processID", "processIndex", "processName", "projectPath", "read.table", "remove.packages", "resampledCountWithUniqueName", "start_time", "transceiver", "value", "variable", "verbose", "weightsPasted"), 
    check = FALSE
))




# Build RstoxFDA:
system.time(RstoxBuild::buildRstoxPackage(
	"RstoxFDA",
	Rversion = "3.6",
	type = "minor", 
	prerelease = TRUE, 
	imports = list(
	  stats = "3.5.0",
	  methods = "3.5.0",
	  utils = "3.5.0",
	  data.table = "1.12.6",
	  ggplot2 = "3.2.1",
	  RColorBrewer = "1.1-2",
	  gridExtra = "2.3",
	  sp = "1.3.2",
	  sf = "0.9.0",
	  rgdal = "1.5.0",
	  rgeos = "0.5",
	  RstoxData = "1.0.25"
	),
	internal.dependencies = c(
		"RstoxData", 
		"RstoxBase"
	), 
	suggests = c("testthat", "Reca", "rnaturalearth", "rnaturalearthdata"),
	additional_repositories = "https://stoxproject.github.io/repo",
	check = FALSE
))



# Build RstoxNMD:
RstoxBuild::buildRstoxPackage(
    "RstoxSurveyPlanner", 
    Rversion = "3.6", 
    version = "1.0.0",
    #type = "minor", 
    imports = list(
        XML = NULL, 
        xml2 = "1.2.2", 
        pbapply = "1.4-0"
    ), 
    suggests = "testthat", 
    internal.dependencies = "RstoxBase", 
    additional_repositories = "https://stoxproject.github.io/repo", 
    globalVariables = c(), 
    check = FALSE
)


# Build RstoxNMD:
RstoxBuild::buildRstoxPackage(
    "RstoxNMD", 
    Rversion = "3.6", 
    version = "1.0.0",
    #type = "minor", 
    imports = list(
        XML = NULL, 
        xml2 = "1.2.2", 
        pbapply = "1.4-0"
    ), 
    suggests = "testthat", 
    internal.dependencies = "RstoxBase", 
    additional_repositories = "https://stoxproject.github.io/repo", 
    globalVariables = c(), 
    check = FALSE
)




# Build RstoxBuild:
RstoxBuild::buildRstoxPackage(
	"RstoxBuild", 
	version = "1.0", 
	Rversion = "3.6", 
	check = FALSE, 
	importToNamespace = "data.table", 
	imports = c("usethis", "devtools", "data.table", "semver"), 
	suggests = c("Rstox", "png", "jpeg", "tiff", "rJava", "callr")
)






# Prepare StoX:
RstoxBuild::prepareStoX(
    #version = "current", 
    version = "3.6.1", 
    type = "minor", 
    prerelease = FALSE
)





convertGlobalVariables <- function(x = "~/ttt.txt") {
	d <- unlist(lapply(readLines(x), strsplit, " ", fixed = TRUE))
	d <- paste0("\"", paste0(d, collapse = "\", \""), "\"")
	writeLines(d, x)
}
convertGlobalVariables()







#pr <- c("~/Code/Github/RstoxFramework/RstoxFramework/inst/test/coastalCod_20", "~/Code/Github/RstoxFramework/RstoxFramework/inst/test/export_ICESbiotic", "~/Code/Github/RstoxFramework/RstoxFramework/inst/test/splitNASC_18", "~/Code/Github/RstoxFramework/RstoxFramework/inst/test/tobis_20", "~/Code/Github/RstoxFramework/RstoxFramework/inst/test/tobis_20_depth")
#d <- lapply(pr, RstoxFramework::runProject, close = TRUE)


