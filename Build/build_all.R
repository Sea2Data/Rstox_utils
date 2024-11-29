# pkgFiles <- c(
#     "~/Downloads/rgdal_1.6-7.tar.gz", 
#     "~/Downloads/rgeos_0.6-4.tar.gz"
# )
# install.packages(pkgFiles, repos = NULL, type = "source")

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
    Rversion = "4.3", 
    type = "patch", 
    #type = "minor", 
    prerelease = TRUE, 
    #prerelease = FALSE, 
    #version = "1.10.0-9006", 
    #version = "2.1.0", 
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
        tinytest = "1.2.0", 
        knitr = NULL,
        rmarkdown = NULL,
        httr = NULL
    ), 
    linkingto = "Rcpp",
    VignetteBuilder = "knitr", 
    globalVariables = c(".", "..Country", "..NAToInsert", "..Organisation", "..SurveyName", "..atResolution", "..attribsNames", "..colList", "..columns", "..groupingVariables", "..keep", "..keys", "..parameterNames", "..relevantColumns", "..replacement", "..simpletags", "..sourceColumns", "..targetAndSourceVariables", "..toKeep", "..valueVariablesInTranslation", "..varToExtract", "..variableKeys", "..variablesInTable", "..x", "AcousticCategory", "Addition", "AphiaIDPredator", "AphiaIDPrey", "BeamKey", "CANoAtLngt", "Channel", "ChannelDepthLower", "ChannelDepthUpper", "ChannelReferenceKey", "ChannelReferenceType", "Constant", "Count", "Country", "Cruise", "CruiseKey", "DateTime", "Day", "DigestionStage", "DoorType", "EDSU", "EchoType", "EffectiveTowDistance", "FishID", "Frequency", "Gear", "GearEx", "GravMethod", "HLNoAtLngt", "HaulNo", "ID", "IdentMet", "LngtClass", "LngtCode", "LocalID", "LogKey", "Month", "N", "NewValue", "Number", "NumberAtLength", "NumberOfIndividualsToGenerate", "NumberOfSampledIndividuals", "PreySequence", "Quarter", "ReplaceBy", "ResolutionCode", "SaCategory", "Scaling", "Ship", "SpecVal", "SpeciesCategoryNumber", "SpeciesCategoryWeight", "SpeciesCode", "StatRec", "StationNumber", "StomachFullness", "SubFactor", "SubsampleWeight", "SubsampledNumber", "Survey", "SweepLngt", "Time", "TotalCount", "TransducerOrientation", "Value", "VariableName", "WeightMeasurement", "Year", "acocat", "age", "agingstructure", "aphia", "bottomdepthstart", "bottomdepthstop", "catCatchWgt", "catchcount", "catchpartnumber", "catchproducttype", "catchweight", "ch", "cruise", "currentReportingUnit", "direction", "distance", "fishingdepthcount", "fishingdepthmax", "fishingdepthmin", "freq", "gear", "gearflow", "i.replace_number", "inapplicableFormats", "individualweight", "integrator_dist", "interval", "iskey", "lat_start", "lat_stop", "latitudeend", "latitudestart", "lengthCode", "lengthintervalcount", "lengthintervalstart", "lengthmeasurement", "lengthresolution", "lengthsamplecount", "lengthsampleweight", "level", "liverweight", "lngtClass", "lngtCode", "log_start", "logstart", "lon_start", "lon_stop", "longitudeend", "longitudestart", "lsCountTot", "maturity", "maxFishID", "max_bot_depth", "meanW", "missionstartdate", "missionstopdate", "nInd", "nation", "newClass", "newLngtCode", "newLngtCodeNumeric", "newReportingUnit", "noMeas", "parasite", "pel_ch_thickness", "platform", "platformname", "preferredagereading", "preycategory", "preydigestion", "preyforeignobject", "preysampleid", "readability", "reportingUnit", "res", "sa", "sampleFac", "serialnumber", "sex", "shortname", "specialstage", "specimenid", "start_time", "startyear", "station", "stationstartdate", "stationstarttime", "stationstopdate", "stationstoptime", "stomach", "stomachfillfield", "stomachweight", "stoxBioticObject", "subFactor", "subWeight", "suffixes", "sum_sa.x", "sum_sa.y", "sweeplength", "tagid", "tagtype", "target", "threshold", "tissuesample", "totalNo", "totalweight", "transceiver", "translationListOne", "trawldoorarea", "trawldoorspread", "trawldoortype", "trawldoorweight", "variableToTranslate", "verticaltrawlopening", "vesselspeed", "weightresolution", "winddirection", "windspeed", "wingspread", "wiredensity", "wirediameter", "wirelength", "xsdObjects"

    ), 
    addManual = FALSE, 
    avoid_compileAttributes_error = TRUE, 
    check = FALSE
))





 


# Build RstoxBase:
system.time(RstoxBuild::buildRstoxPackage(
	"RstoxBase", 
	Rversion = "4.3", 
	#type = "minor", 
	prerelease = TRUE, 
	#prerelease = FALSE, 
	type = "patch", 
	#version = "1.11.3-9003", 
	imports = list(
		data.table = "1.12.0", #"1.12.6", 
		geojsonsf = "2.0.0", 
		#rgeos = "0.5.2",
		#rgdal = "1.5.0",
		#sp = "1.3.0", #"1.3.2", 
		sf = "0.9.0", 
		lwgeom = "0.2-0", # Used for calculation of area of sf object in simplifyStratumPolygon(). Added here since it is only suggested by sf, but RstoxBase needs it.
		maps = "0.2-0", # Only suggested by ggplot2, but needed for map_data in plotting functions.
		#methods = "3.6.0", 
		ggplot2 = "3.0.0", 
		units = "0.7", 
		#methods = "3.6.2", Do we really need to specify this minimum version? I do not remember why.
		xml2 = "1.2.0", #"1.2.2", 
		stringi = "1.4.0", #"1.4.3", 
		jsonlite = "1.6"
	), 
	importToNamespace = "data.table", 
	suggests = list(
	    #testthat = "2.0.0", 
	    tinytest = "1.2.0"
	), 
	internal.dependencies = "RstoxData", 
	# This is used by R CMD check to verify that any suggested or enhanced packages that are located elsewhere than on CRAN can be found. There is no need to point to the appropriate repo here, just ONE repo that has the internal.dependencies and internal.suggests. This was removed on 2023-10-28 as there are no Rstox packages suggested by RstoxBase:
	additional_repositories = "https://stoxproject.github.io/repo", 
	globalVariables = c(".", "..Cruise", "..DateTime", "..DensityType", "..Hauls", "..LengthDistributionType", "..SpeciesCategoryCatchType", "..VerticalResolutionMax", "..VerticalResolutionMin", "..WeightingFactors", "..acceptedColumns", "..atMissingLengthGroup", "..by", "..cols", "..columnNames", "..columnsToKeep", "..extract", "..extractFromDataCopy", "..haulGrouping", "..keys", "..keysSansSample", "..lengthInterval", "..lengthIntervalWidths", "..lengthVar", "..locatedStratum", "..meanBy", "..paddingVariables", "..presentResolutionVariables", "..refvar", "..resolutionVar", "..sumBy", "..tomerge", "..variablesToGetFromQuantityData", "..vars", "Abundance", "AcousticCategory", "AcousticCategoryKey", "AcousticTargetStrength", "Area", "Beam", "BeamKey", "Biomass", "CatchFractionNumber", "CatchFractionWeight", "Channel", "ChannelReferenceDepth", "ChannelReferenceKey", "ChannelReferenceTilt", "ChannelReferenceType", "Cruise", "CruiseKey", "CruiseKey1", "DateTime", "Density", "DensityType", "DensityWeight", "Depth", "DepthExponent", "EDSU", "EffectiveLogDistance", "EffectiveTowDistance", "EstimationMethod", "Haul", "HaulKey", "ImputationMethod", "Individual", "IndividualIndex", "IndividualKey", "IndividualRoundWeight", "IndividualTotalLength", "IndividualTotalLengthMiddle", "L1", "Latitude", "Layer", "LengthDistributionType", "LengthExponent", "LengthResolution", "LogDuration", "LogKey", "LogOrigin", "Longitude", "MaxChannelDepth", "MaxChannelRange", "MeanNASCWeight", "MiddleDateTime", "MinChannelDepth", "MinChannelRange", "NASCKey", "NASCWeight", "PSU", "PreySpeciesCategoryCatchWeightingFactor", "ReplaceIndividual", "ReplaceIndividualIndex", "ReplaceLevel", "ReplaceStratumLayerIndividual", "ReplaceStratumLayerIndividualIndex", "SSU", "SSUIndex", "Sample", "SampleNumber", "SampleWeight", "SpeciesCategory", "SpeciesCategoryCatchWeight", "SplitAcousticCategory", "StartDateTime", "Station", "StationLevel", "StopDateTime", "Stratum", "StratumLayerIndividual", "StratumLayerIndividualIndex", "StratumPolygon", "SummedWeights", "Survey", "SweepWidth", "TargetStrength", "TargetStrength0", "TargetStrengthFunction", "TempLengthGroupUsedInSuperIndividuals", "TotalLength", "V1", "WeightedNumber", "WeightingFactor", "area", "area_hole", "assignmentID", "assignmentPasted", "backscatteringCrossSection", "crossSection", "distance", "ggtitle", "haulWeightFactor", "imputeSeed", "includeintotal", "individualNumber", "individualWeightFactor", "inside", "insideRadius", "intervalIndex", "midIndividualTotalLength", "minDistance", "missingAssignment", "missingSpecies", "numberOfBeams", "numberOfIndividuals", "numberOfSubSamples", "polygonAreaSP_simple", "raisingFactor", "representativeBackscatteringCrossSection", "representativeBackscatteringCrossSectionNormalized", "setUnit", "sumArea", "sumIndividualWeightFactor", "sumWeightedNumber", "temporary_denominator_column_name", "temporary_numerator_column_name", "x", "y"
), 
	addManual = FALSE, 
	check = FALSE
))





# Build RstoxFramework:
system.time(RstoxBuild::buildRstoxPackage(
    "RstoxFramework", 
    Rversion = "4.3", # This is due to change in formals() which now includes the 'envir' argument which we have employed, and the fact that sampling has changed as of R 3.6.
    prerelease = TRUE, 
    #prerelease = FALSE, 
    type = "patch", 
    
    imports = list(
        data.table = "1.12.0", #"1.12.6", 
        #geojsonio = "0.8.0", 
        geojsonsf = "2.0.0",
        jsonlite = "1.6", 
        jsonvalidate = "1.3.0", #"1.3.2", 
        #rgdal = "1.5.0",
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
    globalVariables = c("..EDSUInfoToKeep", "..PSU", "..activeProcessID", "..areNumeric", "..digits", "..functionInputs", "..functionInputs_UseProcessData", "..functionName", "..functionParameters", "..haulInfoToKeep", "..idCol", "..newProcessName", "..presentVariables", "..processDirty", "..propertyDirty", "..signifDigits", "..skipNAAt", "..stationInfoToKeep", "..subsetByNAOn_New", "..subsetByNAOn_Old", "..temporaryScaleFromResampling", "..validInd", "..x1x2y1y2", "AcoCat", "BeamKey", "BootstrapID", "BootstrapSampleFactor", "CruiseKey", "DateTime", "Haul", "Layer", "LogKey", "LogOrigin", "LogOrigin2", "N", "PSU", "PolygonKey", "ProcessName", "ResampleFunction", "SampleUnit", "Station", "StationWeight", "StoX", "Stratum", "WeightingFactor", "assignmentPasted", "canShowInMap", "ch", "currentVersion", "enabled", "freq", "functionInputError", "functionInputProcessIDs", "functionInputsRecursive", "functionInputs_UseProcessData", "functionName", "functionOutputDataType", "functionParameters", "hasBeenRun", "hasProcessData", "listOf", "modelName", "name", "numStations", "offset", "oldCurrentVersion", "packageName", "possibleValues", "processDirty", "processID", "processIndex", "processName", "resamplingFactor", "start_time", "tableName", "terminalProcess", "transceiver", "usedInProcessIDs", "usedInProcessIndices", "usedInRecursiveProcessIndices", "usedInRecursiveProcessNames", "value", "variableName", "verbose", "weightsPasted"
), 
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



# Build RstoxTesting:
RstoxBuild::buildRstoxPackage(
    "RstoxTesting", 
    version = "1.1", 
    Rversion = "4.2", 
    prerelease = FALSE, 
    type = "patch", 
    check = FALSE, 
    importToNamespace = "data.table", 
    imports = list(
        RstoxFramework = "3.6.2", 
        semver = "0.2.0"
    ), 
    suggests = list(
        tinytest = "1.2.0"
    )
)






# Prepare StoX:
RstoxBuild::prepareStoX(
    version = "current", 
    #version = "3.6.1", 
    #type = "patch", 
    type = "minor", 
    prerelease = FALSE
    #prerelease = TRUE
)





convertGlobalVariables <- function(x = "~/ttt.txt") {
	d <- unlist(lapply(readLines(x), strsplit, " ", fixed = TRUE))
	d <- paste0("\"", paste0(d, collapse = "\", \""), "\"")
	writeLines(d, x)
}
convertGlobalVariables()







pr <- c(
    "~/Code/Github/RstoxFramework/testProjects/Old_2024-05-23/BIAS_19_ICES",
    "~/Code/Github/RstoxFramework/testProjects/Old_2024-05-23/coastalCod_20",
    "~/Code/Github/RstoxFramework/testProjects/Old_2024-05-23/cod_19",
    "~/Code/Github/RstoxFramework/testProjects/Old_2024-05-23/tobis_20_depth",
    "~/Code/Github/RstoxFramework/testProjects/Old_2024-05-23/WP_CSHER_2019"
)
d <- lapply(pr, RstoxFramework::runProject, close = TRUE)



# Unzipping through R is no problem but using Finder leaves .DS_Store files that may be the cause of problems with closeProject actually being able to delete the projectSession folder (after emptying it). On Holmin's new Mac this was a problem in one or more of the 10 example projects of StoX 4.0.0 in more than 50% of the attempts to unzip using Finder and run the code below:
#initialTests <- list()
#
#for(ind in seq_len(10)) {
    
    #zips <- list.files("~/Code/Github/StoXExamples/StoXExamples/Examples_StoX_v4.0.0", full.names = TRUE, pattern = "*.zip")
    #temp <- lapply(zips, utils::unzip, exdir = tempdir())
    #dirs <- file.path(tempdir(), tools::file_path_sans_ext(basename(zips)))

    dirs <- list.dirs("~/Code/Github/StoXExamples/StoXExamples/Examples_StoX_v4.0.0", recursive = FALSE)
    # Re-run all Example StoX projects:
    d <- lapply(dirs, RstoxFramework::closeProject)
    d <- lapply(dirs, RstoxFramework::openProject)
    d <- lapply(dirs, RstoxFramework::closeProject, force.save = TRUE)
    d <- lapply(dirs, RstoxFramework::runProject, close = TRUE, save = FALSE)
    #d <- lapply(dirs, RstoxFramework::runProject, save = FALSE)
    #d <- lapply(dirs, RstoxFramework::closeProject, save = FALSE)
    initialTest <- structure(lapply(dirs, RstoxTesting::checkStoxProject_initialCheck), names = basename(dirs))
    #initialTests[[ind]] <- initialTest
    #temp <- unlink(dirs, recursive = TRUE)
    #print(temp)
    #temp <- unlink(dirs, recursive = TRUE)
    #print(temp)
#}


    
    

deleteDS_StoreFiles <- function(x) {
    l <- list.files(x, recursive=TRUE, full.names=TRUE, all.files =TRUE)
    DS_Store_files <- l[endsWith(l,  ".DS_Store")]
    file.remove(DS_Store_files)
}

lapply(dirs, deleteDS_StoreFiles)


zips <- paste0(dirs, ".zip")
mapply(zip::zip, zipfile = zips, basename(dirs), root = dirname(dirs[1]))







# ...
# StoX: Running report process 3: ReportSuperIndividuals_weighted.mean_IndividualTotalLength_By_Stratum_IndividualAge...
# StoX: (time used: 0.029 s)
# (time used(project) : 30.964 s)
# StoX: Running project /Users/arnejh/Code/Github/StoXExamples/StoXExamples/Examples_StoX_v4.0.0/Example_StoX_v4.0.0_swept-area_cod_total_catch...
# Error: j (the 2nd argument inside [...]) is a single symbol but column name 'processID' is not found. If you intended to select columns using a variable in calling scope, please try DT[, ..processID]. The .. prefix conveys one-level-up similar to a file system path.
# In addition: There were 17 warnings (use warnings() to see them)


deleteDS_StoreFiles <- function(x) {
    l <- list.files(x, recursive=TRUE, full.names=TRUE, all.files =TRUE)
    DS_Store_files <- l[endsWith(l,  ".DS_Store")]
    file.remove(DS_Store_files)
}

lapply(dirs, deleteDS_StoreFiles)


detectDS_StoreFiles <- function(x) {
    l <- list.files(x, recursive=TRUE, full.names=TRUE, all.files =TRUE)
    DS_Store_files <- l[endsWith(l,  ".DS_Store")]
}

lapply(dirs, detectDS_StoreFiles)



zips <- paste0(dirs, ".zip")
mapply(zip::zip, zipfile = zips, basename(dirs), root = dirname(dirs[1]))





# Run 
dirs <- list.dirs("~/Code/Github/StoXExamples/StoXExamples/Examples_StoX_v4.1.0", recursive = FALSE)
# Re-run all Example StoX projects:
d <- lapply(dirs[1], RstoxFramework::runProject, close = TRUE, save = FALSE)





