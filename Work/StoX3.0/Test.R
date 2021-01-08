feil

library(RstoxFramework)
options(deparse.max.lines = 10)

inputFile <- function(fileName, projectPath = NULL, folder = "acoustic") {
	inputFolder <- getRstoxFrameworkDefinitions("paths")$stoxFolders[["Input"]]
	if(length(projectPath)) {
		folder <- file.path(projectPath, inputFolder, folder)
		dir.create(folder, recursive = TRUE, showWarnings = FALSE)
	}
	else {
		folder <- file.path(inputFolder, folder)
	}
	
	file.path(folder, basename(fileName))
}

projectPath <- "~/workspace/stox/project/Test_Rstox3"
resourceProjectPath <- "~/workspace/stox/project/Test_Rstox3files2"
bioticresourceProjectPath <- file.path(resourceProjectPath, "biotic")
acousticresourceProjectPath <- file.path(resourceProjectPath, "acoustic")
inputresourceProjectPath <- file.path(resourceProjectPath, "input")

system.time(stoxLibrary <- getRstoxFrameworkDefinitions("stoxLibrary"))
getAvailableStoxFunctionNames("baseline")
getAvailableStoxFunctionNames("analysis")
getAvailableStoxFunctionNames("report")

system.time(RstoxFramework::createProject(projectPath, template = "UserDefined", ow = TRUE))

#RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox3")
##### Create the test project: #####

# Add process 3, DefineStratumPolygon:
inputFileNames <- list.files(inputresourceProjectPath, full.names = TRUE)
file.copy(
	inputFileNames, 
	inputFile(inputFileNames, projectPath = projectPath, folder = "input")
)
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineStratumPolygon", 
		functionName = "DefineStratumPolygon", 
		processParameters = list(
			showInMap = TRUE
		),
		functionParameters = list(
			DefinitionMethod = "ResourceFile", 
			FileName = inputFile(inputFileNames, projectPath = NULL, folder = "input")
		)
	), 
	returnProcessTable = FALSE
)

# Add process 4, StratumArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "StratumArea", 
		functionName = "StratumArea", 
		functionInputs = list(
			StratumPolygon = "DefineStratumPolygon"
		),
		functionParameters = list(
			AreaMethod = "Accurate"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 1, ReadBiotic:
#bioticFileNames <- c(
#	"biotic_cruiseNumber_2017209_Johan+Hjort_2019-11-26T23.04.18.548Z.xml", 
#	"biotic_cruiseNumber_2017208_Johan+Hjort_2019-02-19T19.17.33.548Z.xml", 
#	"biotic_cruiseNumber_2017207_Johan+Hjort_2019-02-19T19.17.45.916Z.xml", 
#	"biotic_cruiseNumber_2017102_G+O+Sars_2019-01-15T03.27.17.822Z.xml"
#)
bioticFileNames <- list.files(bioticresourceProjectPath, full.names = TRUE)
file.copy(
	bioticFileNames, 
	inputFile(bioticFileNames, projectPath = projectPath, folder = "biotic")
)
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "ReadBiotic", 
		functionName = "ReadBiotic", 
		functionParameters = list(
			FileNames = inputFile(bioticFileNames, projectPath = NULL, folder = "biotic")
		)
	), 
	returnProcessTable = FALSE
)

# Add process 24, FilterBiotic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "FilterBiotic", 
		functionName = "FilterBiotic", 
		functionInputs = list(
			BioticData = "ReadBiotic"
		), 
		functionParameters = list(
			FilterExpression = list(
				"biotic_cruiseNumber_2017838_Eros_2019-02-19T08.33.14.905Z.xml/individual" = "length < 0.12", 
				"biotic_cruiseNumber_2017839_Kings+Bay_2019-02-19T08.31.05.461Z.xml/fishstation" = "latitudestart < 30", 
				"biotic_cruiseNumber_2017839_Kings+Bay_2019-02-19T08.31.05.461Z.xml/individual" = "length > 0.03"
			), 
			FilterUpwards = FALSE
		)
	), 
	returnProcessTable = FALSE
)

# Add process 2, StoxBiotic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "StoxBiotic", 
		functionName = "StoxBiotic", 
		processParameters = list(
			showInMap = TRUE
		),
		functionInputs = list(
			BioticData = "ReadBiotic"
		),
		functionParameters = list(
			NumberOfCores = 1
		)
	), 
	returnProcessTable = FALSE
)


# Add process 2, AddToStoxBiotic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "AddToStoxBiotic", 
		functionName = "AddToStoxBiotic", 
		functionParameters = list(
			VariableName = c(
				"length", 
				"lengthmeasurement"
			), 
			NumberOfCores = 1
		),
		functionInputs = list(
			StoxBioticData = "StoxBiotic", 
			BioticData = "ReadBiotic"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 37, Redefine StoxBiotic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "RedefineStoxBiotic", 
		functionName = "RedefineStoxBiotic", 
		functionInputs = list(
			StoxBioticData = "AddToStoxBiotic", 
			BioticData = "ReadBiotic"
		), 
		functionParameters = list(
			RedefinitionTable = data.table::data.table(
				VariableName = "SpeciesCategory", 
				ReplaceBy = "catchcategory"
			)
		)
	), 
	returnProcessTable = FALSE
)


# Add process 37, Define StoxBiotic translation:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineStoxBioticTranslation", 
		functionName = "DefineStoxBioticTranslation", 
		functionParameters = list(
			DefinitionMethod = "ResourceFile", 
			FileName = "~/workspace/stox/project/SpeciesCategoryCatch_Barents_Sea_2019/input/artsliste BH 190520.csv"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 37, Translate StoxBiotic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "TranslateStoxBioticFromFile", 
		functionName = "TranslateStoxBiotic", 
		functionInputs = list(
			StoxBioticData = "RedefineStoxBiotic", 
			StoxBioticTranslation = "DefineStoxBioticTranslation"
		), 
		functionParameters = list(
			TranslationDefinition = "FunctionInput"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 37, Translate StoxBiotic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "TranslateStoxBiotic", 
		functionName = "TranslateStoxBiotic", 
		functionInputs = list(
			StoxBioticData = "RedefineStoxBiotic"
		), 
		functionParameters = list(
			TranslationDefinition = "FunctionParameter",
			TranslationTable = data.table::data.table(
				VariableName = "SpeciesCategory", 
				Value = 167612, 
				NewValue = "testestestestest"
			)
		)
	), 
	returnProcessTable = FALSE
)



# Add process 37, Convert StoxBiotic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "ConvertStoxBiotic", 
		functionName = "ConvertStoxBiotic", 
		functionInputs = list(
			StoxBioticData = "RedefineStoxBiotic"
		), 
		functionParameters = list(
			ConversionFunction = "Scaling", 
			GruopingVariables = c("commonname", "lengthmeasurement"), 
			ConversionTable = data.table::data.table(
				commonname = "makrell", 
				lengthmeasurement = "F", 
				TargetVariable = "IndividualTotalLengthCentimeter", 
				SourceVariable = "length", 
				Scaling = 1111, 
				RoundOffTo = "LengthResolutionCentimeter"
			)
		)
	), 
	returnProcessTable = FALSE
)





## Add process 2, StoxBiotic2:
#temp <- addProcess(
#	projectPath = projectPath, 
#	modelName = "baseline", 
#	values = list(
#		processName = "StoxBiotic2", 
#		functionName = "StoxBiotic2", 
#		functionInputs = list(
#			BioticData = "ReadBiotic"
#		)
#	)
#)


# Add process 24, FilterStoxBiotic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "FilterStoxBiotic", 
		functionName = "FilterStoxBiotic", 
		functionInputs = list(
			StoxBioticData = "StoxBiotic"
		), 
		functionParameters = list(
			FilterExpression = list(
				#SpeciesCategory = "SpeciesCategory %in% c(\"sild'G03/161722.G03/126417/NA\", \"torsk/164712/126436/NA\")", 
				SpeciesCategory = "SpeciesCategory %in% c(\"torsk/164712/126436/NA\", \"sild'G03/161722.G03/126417/NA\")", 
				Individual = "IndividualSex %in% NA"
			), 
			FilterUpwards = FALSE
		)
	), 
	returnProcessTable = FALSE
)





# Add process 4, StratumArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "StratumAreaSimple", 
		functionName = "StratumArea", 
		functionInputs = list(
			StratumPolygon = "DefineStratumPolygon"
		), 
		functionParameters = list(
			AreaMethod = "Simple"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 5, ReadAcoustic:
#acousticFileName <- "libas_ListUserFile20__L40.0-2259.9.txt"
acousticFileName <- list.files(acousticresourceProjectPath, full.names = TRUE)
file.copy(
	acousticFileName,  
	inputFile(acousticFileName, projectPath = projectPath, folder = "acoustic")
)
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "ReadAcoustic", 
		functionName = "ReadAcoustic", 
		functionParameters = list(
			FileNames = inputFile(acousticFileName, projectPath = NULL, folder = "acoustic")
		)
	), 
	returnProcessTable = FALSE
)

# Add process 24, FilterAcoustic:
#temp <- addProcess(
#	projectPath = projectPath, 
#	modelName = "baseline", 
#	values = list(
#		processName = "FilterAcoustic", 
#		functionName = "FilterAcoustic", 
#		functionInputs = list(
#			AcousticData = "ReadAcoustic"
#		), 
#		functionParameters = list(
#			FilterExpression = list("echosounder_cruiseNumber_2017838_Eros.xml/ch_type" = "type == #\"P\"")
#		), 
#		FilterUpwards = FALSE
#	), 
#	returnProcessTable = FALSE
#)

# Add process 6, StoxAcoustic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "StoxAcoustic", 
		functionName = "StoxAcoustic", 
		processParameters = list(
			showInMap = TRUE
		),
		functionInputs = list(
			AcousticData = "ReadAcoustic"
		),
		functionParameters = list(
			NumberOfCores = 1
		)
	), 
	returnProcessTable = FALSE
)

# Add process 24, FilterStoxAcoustic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "FilterStoxAcoustic", 
		functionName = "FilterStoxAcoustic", 
		functionInputs = list(
			StoxAcousticData = "StoxAcoustic"
		), 
		functionParameters = list(
			FilterExpression = list(
				ChannelReference = "ChannelReferenceType == \"P\"", 
				AcousticCategory = "AcousticCategory %in% c(\"12\", \"21\", \"24\", \"31\")"
			), 
			FilterUpwards = FALSE
		)
	), 
	returnProcessTable = FALSE
)


# # Add process 6b, MergeStoxAcoustic:
# system.time(temp <- addProcess(
# 	projectPath = projectPath, 
# 	modelName = "baseline", 
# 	values = list(
# 		processName = "MergeStoxAcoustic", 
# 		functionName = "MergeStoxAcoustic", 
# 		functionInputs = list(
# 			StoxAcousticData = "StoxAcoustic"
# 		)
# 	)
# ))

## Add process 6c, MergeStoxBiotic:
#system.time(temp <- addProcess(
#	projectPath = projectPath, 
#	modelName = "baseline", 
#	values = list(
#		processName = "MergeStoxBiotic", 
#		functionName = "MergeStoxBiotic", 
#		functionInputs = list(
#			StoxBioticData = "StoxBiotic"
#		)
#	)
#))

# Add process 7, DefineAcousticPSU:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineAcousticPSU", 
		functionName = "DefineAcousticPSU", 
		functionParameters = list(
			DefinitionMethod = "EDSUToPSU"
			#DefinitionMethod = "None"
		), 
		functionInputs = list(
			StratumPolygon = "DefineStratumPolygon", 
			StoxAcousticData = "StoxAcoustic"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 8, DefineAcousticLayer:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineAcousticLayer", 
		functionName = "DefineAcousticLayer", 
		functionInputs = list(
			StoxAcousticData = "StoxAcoustic"
		), 
		functionParameters = list(
			DefinitionMethod = "HighestResolution"
			#DefinitionMethod = "WaterColumn"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 9, DefineBioticPSU:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineBioticPSU", 
		functionName = "DefineBioticPSU", 
		functionInputs = list(
			StratumPolygon = "DefineStratumPolygon", 
			StoxBioticData = "FilterStoxBiotic"
		), 
		functionParameters = list(
			DefinitionMethod = "StationToPSU"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 10, DefineBioticLayer:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineBioticLayer", 
		functionName = "DefineBioticLayer", 
		functionInputs = list(
			StoxBioticData = "FilterStoxBiotic"
		), 
		functionParameters = list(
			#DefinitionMethod = "WaterColumn"
			DefinitionMethod = "HighestResolution"
		)
	), 
	returnProcessTable = FALSE
)


# Add process 11, LengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "LengthDistribution", 
		functionName = "LengthDistribution", 
		functionInputs = list(
			StoxBioticData = "FilterStoxBiotic"
		), 
		functionParameters = list(
			LengthDistributionType = "Normalized", 
			RaisingFactorPriority = "Weight"
		)
	), 
	returnProcessTable = FALSE
)


# Add process 11, LengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "LengthDistributionStandard", 
		functionName = "LengthDistribution", 
		functionInputs = list(
			StoxBioticData = "FilterStoxBiotic"
		), 
		functionParameters = list(
			LengthDistributionType = "Standard", 
			RaisingFactorPriority = "Weight"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 11, LengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "LengthDistributionPercent", 
		functionName = "LengthDistribution", 
		functionInputs = list(
			StoxBioticData = "FilterStoxBiotic"
		), 
		functionParameters = list(
			LengthDistributionType = "Percent", 
			RaisingFactorPriority = "Weight"
		)
	), 
	returnProcessTable = FALSE
)


# Add process 12, RegroupLengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "RegroupLengthDistribution", 
		functionName = "RegroupLengthDistribution", 
		functionInputs = list(
			LengthDistributionData = "LengthDistribution"
		), 
		functionParameters = list(
			LengthIntervalCentimeter = 5
		)
	), 
	returnProcessTable = FALSE
)

# Add process 13, LengthDependentCatchCompensation:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "LengthDependentCatchCompensation", 
		functionName = "LengthDependentCatchCompensation", 
		functionInputs = list(
			LengthDistributionData = "RegroupLengthDistribution"
		), 
		functionParameters = list(
			CompensationMethod = "LengthDependentSweepWidth", 
			LengthDependentSweepWidthParameters = data.table::data.table(
				SpeciesCategory = c(
					"sild'G03/161722.G03/126417/NA", 
					"laksesild/162187/127312/NA", 
					"torsk/164712/126436/NA"
				), 
				Alpha = 5.91, 
				Beta = 0.43, 
				LMin = c(15, 14), 
				LMax = 62
			)
		)
	), 
	returnProcessTable = FALSE
)

# Add process 16, RelativeLengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "RelativeLengthDistribution", 
		functionName = "RelativeLengthDistribution", 
		functionInputs = list(
			LengthDistributionData = "LengthDependentCatchCompensation"
		)
	), 
	returnProcessTable = FALSE
)


# Add process 14, SumLengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "SumLengthDistribution", 
		functionName = "SumLengthDistribution", 
		functionInputs = list(
			LengthDistributionData = "RegroupLengthDistribution"
		), 
		functionParameters = list(
			LayerDefinition = "FunctionParameter", 
			LayerDefinitionMethod = "HighestResolution"
		)
	), 
	returnProcessTable = FALSE
)


# Add process 15, MeanLengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "MeanLengthDistribution", 
		functionName = "MeanLengthDistribution", 
		functionInputs = list(
			SumLengthDistributionData = "SumLengthDistribution", 
			StratumPolygon = "DefineStratumPolygon"
		), 
		functionParameters = list(
			LayerDefinition = "PreDefined", 
			PSUDefinition = "FunctionParameter", 
			PSUDefinitionMethod = "StationToPSU"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 17, NASC:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "NASC", 
		functionName = "NASC", 
		functionInputs = list(
			StoxAcousticData = "FilterStoxAcoustic"
		)
	), 
	returnProcessTable = FALSE
)



# Add process 18, SumNASC:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "SumNASC", 
		functionName = "SumNASC", 
		functionInputs = list(
			NASCData = "NASC"
		), 
		functionParameters = list(
			LayerDefinition = "FunctionParameter", 
			LayerDefinitionMethod = "HighestResolution"
		)
	), 
	returnProcessTable = FALSE
)


# Add process 19, MeanNASC:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "MeanNASC", 
		functionName = "MeanNASC", 
		functionInputs = list(
			NASCData = "NASC", 
			AcousticPSU = "DefineAcousticPSU"
		), 
		functionParameters = list(
			LayerDefinition = "FunctionParameter", 
			LayerDefinitionMethod = "HighestResolution", 
			PSUDefinition = "FunctionInput"
		)
	), 
	returnProcessTable = FALSE
)




# Add process 20, DefineAcousticTargetStrength:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineAcousticTargetStrength", 
		functionName = "DefineAcousticTargetStrength", 
		functionParameters = list(
			TargetStrengthMethod = "LengthDependent", 
			DefinitionMethod = "Table", 
			TargetStrengthDefinitionTable = data.table::data.table(
				AcousticCategory = "12", 
				Frequency = 38000, 
				LengthExponent = 20, 
				TargetStrength0 = -71.9
			)
		)
	), 
	returnProcessTable = FALSE
)


# Add process 20, SweptAreaDensity:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "SweptAreaDensityPreDefined", 
		functionName = "SweptAreaDensity", 
		functionInputs = list(
			MeanLengthDistributionData = "MeanLengthDistribution"
		), 
		functionParameters = list(
			SweepWidthMethod = "Constant", 
			SweepWidthMeter = 100
		)
	), 
	returnProcessTable = FALSE
)


# Add process 21, DefineBioticAssignment:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineBioticAssignment", 
		functionName = "DefineBioticAssignment", 
		functionInputs = list(
			StratumPolygon = "DefineStratumPolygon", 
			AcousticPSU = "DefineAcousticPSU", 
			AcousticLayer = "DefineAcousticLayer", 
			StoxBioticData = "FilterStoxBiotic"
		), 
		functionParameters = list(
			DefinitionMethod = "Stratum"
		)
	), 
	returnProcessTable = FALSE
)


## Add process 21, DefineBioticAssignment:
#temp <- addProcess(
#	projectPath = projectPath, 
#	modelName = "baseline", 
#	values = list(
#		processName = "DefineBioticAssignmentRadius", 
#		functionName = "DefineBioticAssignment", 
#		functionInputs = list(
#			AcousticPSU = "DefineAcousticPSU", 
#			AcousticLayer = "DefineAcousticLayer", 
#			StoxBioticData = "FilterStoxBiotic", 
#			StoxAcousticData = "FilterStoxAcoustic"
#		), 
#		functionParameters = list(
#			DefinitionMethod = "Radius", 
#			Radius = 25, 
#			MinNumberOfHauls = 10
#		)
#	), 
#	returnProcessTable = FALSE
#)


## Add process 21, DefineBioticAssignment:
#temp <- addProcess(
#	projectPath = projectPath, 
#	modelName = "baseline", 
#	values = list(
#		processName = "DefineBioticAssignmentEllipsoidalDistance", 
#		functionName = "DefineBioticAssignment", 
#		functionInputs = list(
#			AcousticPSU = "DefineAcousticPSU", 
#			AcousticLayer = "DefineAcousticLayer", 
#			StoxBioticData = "FilterStoxBiotic", 
#			StoxAcousticData = "FilterStoxAcoustic"
#		), 
#		functionParameters = list(
#			DefinitionMethod = "EllipsoidalDistance", 
#			DistanceNauticalMiles = 50, 
#			TimeDifferenceHours = 12, 
#			BottomDepthDifferenceMeters = 500, 
#			LongitudeDifferenceDegrees = 5, 
#			LatitudeDifferenceDegrees = 5, 
#			MinNumberOfHauls = 10
#		)
#	), 
#	returnProcessTable = FALSE
#)


# Add process 4, BioticAssignmentWeighting:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "BioticAssignmentWeighting", 
		functionName = "BioticAssignmentWeighting", 
		functionInputs = list(
			BioticAssignment = "DefineBioticAssignment",
			StoxBioticData = "StoxBiotic"
		),
		functionParameters = list(
			WeightingMethod = "NumberOfLengthSamples"
		)
	), 
	returnProcessTable = FALSE
)


# Add process 4, BioticAssignmentWeighting:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "BioticAssignmentWeightingNASC", 
		functionName = "BioticAssignmentWeighting", 
		functionInputs = list(
			BioticAssignment = "DefineBioticAssignment",
			StoxBioticData = "StoxBiotic", 
			LengthDistributionData = "LengthDistribution", 
			StoxAcousticData = "FilterStoxAcoustic"
		),
		functionParameters = list(
			WeightingMethod = "NASC", 
			Radius = 20, 
			LengthExponentTable = data.table::data.table(
				SpeciesCategory = c(
					"sild'G03/161722.G03/126417/NA", 
					"torsk/164712/126436/NA"
				), 
				LengthExponent = 2
			)
		)
	), 
	returnProcessTable = FALSE
)


# Add process 22, AssignmentLengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "AssignmentLengthDistribution", 
		functionName = "AssignmentLengthDistribution", 
		functionInputs = list(
			LengthDistributionData = "LengthDistributionPercent", 
			BioticAssignment = "BioticAssignmentWeighting"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 20, AcousticDensity:
system.time(temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "AcousticDensity", 
		functionName = "AcousticDensity", 
		functionInputs = list(
			MeanNASCData = "MeanNASC", 
			AssignmentLengthDistributionData = "AssignmentLengthDistribution", 
			AcousticTargetStrength = "DefineAcousticTargetStrength"
		), 
		functionParameters = list(
			SpeciesLinkTable = data.table::data.table(
				SpeciesCategory = c(
					"torsk/164712/126436/NA", 
					"sild'G03/161722.G03/126417/NA", 
					"kolmule/164774/126439/NA", 
					"makrell/172414/127023/NA"
				), 
				AcousticCategory = c(
					"31",
					"12", 
					"24", 
					"21"
				)
			)
		)
	)
))

# Add process 23, MeanDensity:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "MeanDensity", 
		functionName = "MeanDensity", 
		functionInputs = list(
			DensityData = "SweptAreaDensityPreDefined"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 24, Abundance:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "Abundance", 
		functionName = "Abundance", 
		functionInputs = list(
			MeanDensityData = "MeanDensity", 
			StratumAreaData = "StratumArea"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 24, IndividualsSweptArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "IndividualsSweptArea", 
		functionName = "Individuals", 
		functionInputs = list(
			StoxBioticData = "FilterStoxBiotic", 
			MeanLengthDistributionData = "MeanLengthDistribution"
		), 
		functionParameters = list(
			AbundanceType = "SweptArea"
		)
	), 
	returnProcessTable = FALSE
)


# Add process 24, IndividualsAcoustic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "IndividualsAcoustic", 
		functionName = "Individuals", 
		functionInputs = list(
			StoxBioticData = "FilterStoxBiotic", 
			BioticAssignment = "DefineBioticAssignment"
		), 
		functionParameters = list(
			AbundanceType = "Acoustic"
		)
	), 
	returnProcessTable = FALSE
)



# Add process 24, SuperIndividualsSweptArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "SuperIndividualsSweptAreaEqual", 
		functionName = "SuperIndividuals", 
		functionInputs = list(
			IndividualsData = "IndividualsSweptArea", 
			AbundanceData = "Abundance", 
			LengthDistributionData = "LengthDistribution"
		), 
		functionParameters = list(
			DistributionMethod = "Equal"
		)
	), 
	returnProcessTable = FALSE
)

# Add process 24, SuperIndividualsSweptArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "SuperIndividualsSweptAreaHaulDensity", 
		functionName = "SuperIndividuals", 
		functionInputs = list(
			IndividualsData = "IndividualsSweptArea", 
			AbundanceData = "Abundance", 
			LengthDistributionData = "LengthDistribution"
		), 
		functionParameters = list(
			DistributionMethod = "HaulDensity"
		)
	), 
	returnProcessTable = FALSE
)


# Add process 37, SuperIndividualsSweptArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "SpeciesCategoryCatch", 
		functionName = "SpeciesCategoryCatch", 
		functionInputs = list(
			StoxBioticData = "StoxBiotic"
		), 
		functionParameters = list(
			CatchVariable = "Count"
		)
	), 
	returnProcessTable = FALSE
)



##### End of test project: #####

projectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "baseline"
system.time(processTable <- getProcessTable(projectPath, modelName))
#system.time(processTable2 <- scanForModelError(projectPath, modelName))
#system.time(processTable2 <- RstoxFramework:::getProcessesSansProcessData(projectPath, modelName))

#RstoxFramework::saveProject(projectPath, "JSON")
#RstoxFramework::saveProject(projectPath)

system.time(closeProject(projectPath, save = TRUE))
system.time(o <- openProject(projectPath))

#system.time(f <- runModel(projectPath, modelName, 1, 14))
system.time(f <- runModel(projectPath, modelName, fileOutput = TRUE))

#system.time(f <- runModel(projectPath, modelName, 1, 34))


# Test overriding parameters:
system.time(f2 <- runModel(projectPath, modelName, fileOutput = FALSE, BioticAssignmentWeightingNASC = list(Radius = 100), save = FALSE))






# TestSpeciesCategoryCatch:
projectPath <- "~/workspace/stox/project/TestSpeciesCategoryCatch"
modelName <- "baseline"
openProject(projectPath)
system.time(f <- runModel(projectPath, modelName))



projectPath <- "~/workspace/stox/project/Example_North Sea_Lesser_sandeel_2020_reordered_TS_L"
modelName <- "baseline"
system.time(processTable <- getProcessTable(projectPath, modelName))
processTable





# Test the StoX 2.7 sandeel against StoX 2.9.12:

# Run in  StoX 2.9.12: 
projectPath <- "~/workspace/stox/project/Test30_sandeel2020"
modelName <- "baseline"
openProject(projectPath)
system.time(f <- runModel(projectPath, modelName))

# Run in  StoX 2.7: 
library(Rstox)
pr <- "North Sea NOR lesser sandeel acoustic abundance estimate in spring_2020"
system.time(g <- getBaseline(pr))

# Get abundance:
AbundanceOld <- data.table::as.data.table(g$outputData$Abundance)
AbundanceNew <- data.table::as.data.table(f$Abundance)
data.table::setorderv(AbundanceOld, c("SampleUnit", "SpecCat", "LengthGroup"))
data.table::setorderv(AbundanceNew, c("Stratum", "SpeciesCategory", "IndividualTotalLengthCentimeter"))

# Use equal names and SpeciesCategory:
data.table::setnames(AbundanceOld, c("SampleUnit", "SpecCat", "LengthGroup"), c("Stratum", "SpeciesCategory", "IndividualTotalLengthCentimeter"))
AbundanceNew$SpeciesCategory <- fileParts(AbundanceNew$SpeciesCategory, 1)
d <- merge(
	AbundanceOld, 
	AbundanceNew, 
	by = c("Stratum", "SpeciesCategory", "IndividualTotalLengthCentimeter")
)

# Only minor differences:
plot(d$Abundance.x, type = "l")
points(d$Abundance.y, col = 2, type = "l")

plot(d$Abundance.x / d$Abundance.y, type = "l")





g$outputData$MeanNASC <- data.table::as.data.table(g$outputData$MeanNASC)
d <- merge(g$outputData$MeanNASC, f$MeanNASC, by.x = "SampleUnit", by.y = "PSU")
d <- d[AcousticCategory == 27]
plot(d$NASC.x)
points(d$NASC.x, col = 2, cex = 2)

all.equal(d$NASC.x, d$NASC.y)
# [1] "Mean relative difference: 9.237291e-08"


fileParts <- function(x, ind, paste0 = TRUE, collapse = "/") {
	x <- strsplit(x, "/")
	x <- lapply(x, "[", ind)
	if(paste0) {
		x <- sapply(x, paste0, collapse = collapse)
	}
	return(x)
}


g$outputData$RegroupLengthDist <- data.table::as.data.table(g$outputData$RegroupLengthDist)
f$RegroupLengthDistribution$Haul2 <- fileParts(f$RegroupLengthDistribution$Haul, c(1,6))
d <- merge(g$outputData$RegroupLengthDist, f$RegroupLengthDistribution, by.x = "Station", by.y = "Haul2", allow.cartesian=TRUE)

plot(d$WeightedCount.x)
points(d$WeightedCount.y, col = 2, cex = 0.7)




ass <- data.table::as.data.table(merge(g$processData$bioticassignment, g$processData$suassignment))
f$BioticAssignmentWeighting$Haul2 <- fileParts(f$BioticAssignmentWeighting$Haul, c(1,6))
d <- merge(ass, f$BioticAssignmentWeighting, by.x = "Station", by.y = "Haul2", allow.cartesian=TRUE)

plot(d$StationWeight)
points(d$WeightingFactor, col = 2, cex = 0.7)






g$outputData$TotalLengthDist <- data.table::as.data.table(g$outputData$TotalLengthDist)
#g$outputData$TotalLengthDist[, PSU := paste0("T", AssignmentID)]
tot <- merge(g$outputData$TotalLengthDist, g$processData$suassignment, by = "AssignmentID", allow.cartesian = TRUE)

data.table::setorderv(tot, c("SampleUnit", "SpecCat", "LengthGroup"))
data.table::setorderv(f$AssignmentLengthDistribution, c("PSU", "SpeciesCategory", "IndividualTotalLengthCentimeter"))

tot
f$AssignmentLengthDistribution





plot(tot[SampleUnit == "T47"]$LengthGroup, tot[SampleUnit == "T47"]$WeightedCount)
points(f$AssignmentLengthDistribution[PSU == "T47"]$IndividualTotalLengthCentimeter, f$AssignmentLengthDistribution[PSU == "T47"]$WeightedCount * 100, col =2)

# Observe that the new code does not sum to 100:
summary(tot[, sum(WeightedCount, na.rm = TRUE), by = c("SampleUnit")])
summary(f$AssignmentLengthDistribution[, sum(WeightedCount, na.rm = TRUE), by = c("PSU")])




acd <- data.table::as.data.table(g$outputData$AcousticDensity)
data.table::setnames(acd, c("SampleUnit", "SpecCat", "LengthGroup"), c("PSU", "SpeciesCategory", "IndividualTotalLengthCentimeter"))

f$AcousticDensity$SpeciesCategory <- fileParts(f$AcousticDensity$SpeciesCategory, 1)
d <- merge(acd, f$AcousticDensity, by = c("PSU", "SpeciesCategory", "IndividualTotalLengthCentimeter"))



plot(d$Density.x, type = "l")
points(d$Density.y, col = 2, type = "l")

plot(d$Density.x - d$Density.y, col = 2, type = "l")

















# Check whether all EDSUs are present in NASCData:
allEDSUs <- unique(f$StoxAcoustic$Log$EDSU)
length(allEDSUs)
plot(f$StoxAcoustic$Log[, c("Longitude", "Latitude")])

allNASCEDSUs <- unique(f$NASC$EDSU)
length(allNASCEDSUs)
points(f$NASC[, c("Longitude", "Latitude")], col = 2)

f$DefineAcousticPSU
# NASC contains only EDSUs with data for the selected species.

# Check whether all Stations are present in LengthDistributionData:
allStations <- unique(f$StoxBiotic$Station$Station)
length(allStations)
plot(f$StoxBiotic$Station[, c("Longitude", "Latitude")])

allLengthDistributionStations <- unique(f$LengthDistribution$Station)
length(allLengthDistributionStations)
allLengthDistributionWithPSULayerStations <- unique(f$LengthDistributionWithPSULayer$Station)
length(allLengthDistributionWithPSULayerStations)
points(f$LengthDistribution[, c("Longitude", "Latitude")], col = 2)

f$DefineSweptAreaPSU
# NASC contains only EDSUs with data for the selected species.



sp::plot(f$DefineStratumPolygon)
points(f$StoxBiotic$Station[, c("Longitude", "Latitude")])








library(RstoxAPI)
options(deparse.max.lines = 10)

projectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "baseline"


# Add process 4, StratumArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "BioticAssignmentWeighting", 
		functionName = "BioticAssignmentWeighting", 
		functionInputs = list(
			BioticAssignment = "DefineBioticAssignment",
			StoxBioticData = "StoxBiotic", 
			LengthDistributionData = "LengthDistribution",
			NASCData = "NASC"
		),
		functionParameters = list(
			WeightingMethod = "Equal", 
			Radius = 25, 
			LengthExponent = 2
		)
	), 
	returnProcessTable = FALSE
)


# Add process 4, StratumArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "BioticAssignmentWeighting2", 
		functionName = "BioticAssignmentWeighting", 
		functionInputs = list(
			BioticAssignment = "DefineBioticAssignment",
			StoxBioticData = "StoxBiotic", 
			LengthDistributionData = "LengthDistribution",
			NASCData = "NASC"
		),
		functionParameters = list(
			WeightingMethod = "NumberOfLengthSamples", 
			Radius = 25, 
			LengthExponent = 2, 
			MaxNumberOfLengthSamples = 20
		)
	), 
	returnProcessTable = FALSE
)




# Add process 4, StratumArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "BioticAssignmentWeighting3", 
		functionName = "BioticAssignmentWeighting", 
		functionInputs = list(
			BioticAssignment = "DefineBioticAssignment",
			StoxBioticData = "StoxBiotic", 
			LengthDistributionData = "LengthDistribution",
			NASCData = "NASC"
		),
		functionParameters = list(
			WeightingMethod = "NormalizedTotalWeight", 
			Radius = 25, 
			LengthExponent = 2, 
			MaxNumberOfLengthSamples = 100
		)
	), 
	returnProcessTable = FALSE
)




# Add process 4, StratumArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "BioticAssignmentWeighting4", 
		functionName = "BioticAssignmentWeighting", 
		functionInputs = list(
			BioticAssignment = "DefineBioticAssignment",
			StoxBioticData = "StoxBiotic", 
			LengthDistributionData = "LengthDistribution",
			NASCData = "NASC"
		),
		functionParameters = list(
			WeightingMethod = "NormalizedTotalCount", 
			Radius = 25, 
			LengthExponent = 2, 
			MaxNumberOfLengthSamples = 100
		)
	), 
	returnProcessTable = FALSE
)


# Add process 4, StratumArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "BioticAssignmentWeighting5", 
		functionName = "BioticAssignmentWeighting", 
		functionInputs = list(
			BioticAssignment = "DefineBioticAssignment",
			StoxBioticData = "StoxBiotic", 
			LengthDistributionData = "LengthDistribution",
			NASCData = "NASC"
		),
		functionParameters = list(
			WeightingMethod = "SumWeightedCount", 
			Radius = 25, 
			LengthExponent = 2, 
			MaxNumberOfLengthSamples = 100
		)
	), 
	returnProcessTable = FALSE
)


# Add process 4, StratumArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "BioticAssignmentWeighting6", 
		functionName = "BioticAssignmentWeighting", 
		functionInputs = list(
			BioticAssignment = "DefineBioticAssignment",
			StoxBioticData = "StoxBiotic", 
			LengthDistributionData = "LengthDistribution",
			NASCData = "NASC"
		),
		functionParameters = list(
			WeightingMethod = "InverseSumWeightedCount", 
			Radius = 25, 
			LengthExponent = 2, 
			MaxNumberOfLengthSamples = 100
		)
	), 
	returnProcessTable = FALSE
)




library(RstoxAPI)
options(deparse.max.lines = 10)

inputFile <- function(fileName, projectPath = NULL, folder = "acoustic") {
	inputFolder <- getRstoxFrameworkDefinitions("paths")$stoxFolders[["Input"]]
	if(length(projectPath)) {
		folder <- file.path(projectPath, inputFolder, folder)
		dir.create(folder, recursive = TRUE, showWarnings = FALSE)
	}
	else {
		folder <- file.path(inputFolder, folder)
	}
	
	file.path(folder, basename(fileName))
}

projectPath <- "~/workspace/stox/project/Test_Rstox3"
resourceProjectPath <- "~/workspace/stox/project/Test_Rstox3files2"
bioticresourceProjectPath <- file.path(resourceProjectPath, "biotic")
acousticresourceProjectPath <- file.path(resourceProjectPath, "acoustic")
inputresourceProjectPath <- file.path(resourceProjectPath, "input")

system.time(RstoxFramework::createProject(projectPath, template = "UserDefinedTemplate", ow = TRUE))

#RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox3")
##### Create the test project: #####

# Add process 3, DefineStratumPolygon:
inputFileNames <- list.files(inputresourceProjectPath, full.names = TRUE)
file.copy(
	inputFileNames, 
	inputFile(inputFileNames, projectPath = projectPath, folder = "input")
)
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineStratumPolygon", 
		functionName = "DefineStratumPolygon", 
		processParameters = list(
			showInMap = TRUE
		),
		functionParameters = list(
			DefinitionMethod = "ResourceFile", 
			FileName = inputFile(inputFileNames, projectPath = NULL, folder = "input")
		)
	), 
	returnProcessTable = FALSE
)

# Add process 4, StratumArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "StratumArea", 
		functionName = "StratumArea", 
		functionInputs = list(
			StratumPolygon = "DefineStratumPolygon"
		),
		functionParameters = list(
			AreaMethod = "Accurate"
		)
	), 
	returnProcessTable = FALSE
)
##### End of test project: #####


system.time(closeProject(projectPath, save = TRUE))
system.time(o <- openProject(projectPath))

projectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "baseline"
system.time(f <- runModel(projectPath, modelName))


system.time(closeProject(projectPath, save = "JSON"))
system.time(o <- openProject(projectPath, type = "JSON"))


system.time(o <- openProject(projectPath, type = "JSON", force = TRUE))






















system.time(writeProjectDescription(projectPath, "JSON"))
system.time(ss <- readProjectDescription(projectPath, "JSON"))
projectDescription <- getProjectMemoryData(projectPath, named.list = TRUE)


addHaulToAssignment("PSU0001", "Layer02", "xxxxxx", projectPath, "baseline", "P030")


a <- addHaulToAssignment(PSU = "PSU0001", Layer = "WaterColumn", Haul = "2017840/5/2017/3206/40/37066", projectPath, modelName, processID = "P030")


RstoxFramework::saveProject(projectPath, "JSON")


getFunctionHelpAsHtml(projectPath, modelName, "P001")



d <- getProcessOutput(projectPath, modelName, "P005")



rearrangeProcesses(projectPath, modelName, c("P011"), "P003")


rearrangeProcesses(projectPath, modelName, c("P001"), "P036")
setProcessPropertyValue("processArguments", "processName", "ddddddddd", projectPath, modelName, "P009")






addEDSU(PSU = "PSU1", EDSU = "2017838/2017-02-13T19:54:09.000Z", projectPath, modelName, "P011")




# Update Biotic variable:
BioticData <- ReadBiotic("~/workspace/stox/project/BS_swept_area_cod_2019/input/biotic/biotic_cruiseNumber_0146_2019_UFJN_VLNY_Vilnyus_2019-05-03T22.03.07.377Z.xml")
VariableConversionTable <- data.table::data.table(
	TableName = "catchsample", 
	VariableName = "commonname", 
	Value = c("sild'G03", "laksesild", "vassild"), 
	NewValue = "EndsWithSild"
)
BioticData2 <- ConvertStoxBioticVariables(BioticData, VariableConversionTable = VariableConversionTable)

# Update StoxBiotic variable:
StoxBioticData <- StoxBiotic(BioticData)
VariableConversionTable <- data.table::data.table(
	TableName = "SpeciesCategory", 
	VariableName = "SpeciesCategory", 
	Value = c(
		"sild'G03/161722.G03/126417/NA", 
		"krokulke/167208/127193/NA", 
		"paddeulke/167408/127235/NA", 
		"sølvtorsk/164772/126435/NA", 
		"torsk/164712/126436/NA"
	), 
	NewValue = c(
		"Herring", 
		"Ulke", 
		"Ulke", 
		"Cod", 
		"Cod"
	)
)
StoxBioticData2 <- ConvertStoxBioticVariables(StoxBioticData, VariableConversionTable = VariableConversionTable)
# The SpeciesCategory has changed:
StoxBioticData2$SpeciesCategory










sizes <- 10^(1:5)
times <- rep(NA, length(sizes))

for(ind in seq_along(sizes)) {
	DT1 <- data.table::data.table(
		a = sort(sample(1:3, sizes[ind], replace = TRUE))
	)
	
	DT2 <- data.table::data.table(
		a = sort(sample(1:2, sizes[ind], replace = TRUE))
	)
	
	times[ind] <- system.time(DT <- data.table::fsetdiff(DT1, DT2))[3]
}

data.table::data.table(
	sizes = sizes, 
	times = times
)










projectPath <- "~/Downloads/Test4"
RstoxFramework::openProject(projectPath, force = TRUE)
modelName <- "baseline"
processTable <- getProcessTable(projectPath, modelName)
f2 <- runModel(projectPath, modelName)



temp <-removeStratum("1", projectPath, modelName, "P001")
								
								



# Run the test project of cod 2019:
projectPath <- "~/workspace/stox/project/Test30_sandeel2020"
RstoxFramework::openProject(projectPath, force = TRUE)
modelName <- "baseline"
processTable <- getProcessTable(projectPath, modelName)
f2 <- runModel(projectPath, modelName)










# Run the test project of cod 2019:
projectPath <- "~/workspace/stox/project/BS_swept_area_cod_2019"
RstoxFramework::openProject(projectPath, force = TRUE)
modelName <- "baseline"
processTable <- getProcessTable(projectPath, modelName)
f2 <- runModel(projectPath, modelName)


# Get the data from the old project:
library(Rstox)
g <- getBaseline("~/workspace/stox/project/Barents Sea Northeast Arctic cod bottom trawl index in winter_2019_noFishstationFilter")

# Check Abundance:
Abundance_old <- data.table::as.data.table(g$outputData$Abundance)
Abundance_old[, Stratum := as.character(SampleUnit)]
Abundance_new <- f2$Abundance
Abundance_new <- subset(Abundance_new, !is.na(IndividualTotalLengthCentimeter))

cat_old <- c("Stratum", "LengthGroup", "LengthInterval")
data.table::setorderv(Abundance_old, cat_old)
cat_new <- c("Stratum", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter")
data.table::setorderv(Abundance_new, cat_new)
head(cbind(Abundance_old[, c(..cat_old, "Abundance")], Abundance_new[, c(..cat_new, "Abundance")]), 200)

plot(Abundance_old$Abundance)
points(Abundance_new$Abundance, col = 2, cex = 0.6)

plot(Abundance_old$Abundance / Abundance_new$Abundance) # Rather similar. Some difference that may be due to stratum/PSU definitions


# Check SuperIndividuals:
SuperIndividuals_old <- data.table::as.data.table(g$outputData$SuperIndAbundance)
SuperIndividuals_old[, Stratum := as.character(Stratum)]
SuperIndividuals_new <- f2$SuperIndividuals

cat_old <- c("Stratum", "LenGrp", "LenIntv", "serialnumber")
data.table::setorderv(SuperIndividuals_old, cat_old)
cat_new <- c("Stratum", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter", "Haul")
data.table::setorderv(SuperIndividuals_new, cat_new)

SuperIndividuals_old[, c(..cat_old, "Abundance")]
SuperIndividuals_new[, c(..cat_new, "Abundance")]


head(cbind(SuperIndividuals_old[, c(..cat_old, "Abundance")], SuperIndividuals_new[, c(..cat_new, "Abundance")]), 200)

plot(SuperIndividuals_old$Abundance)
points(SuperIndividuals_new$Abundance, col = 2, cex = 0.6)




useOldStationCode <- function(DT) {
	parts <- strsplit(DT$Haul, "/")
	oldStation <- paste(sapply(parts, head, 1), sapply(parts, tail, 1), sep = "/")
	DT[, Station := ..oldStation]
}



StationHaulLink <- merge(f2$StoxBiotic$Station, f2$StoxBiotic$Haul)[, c("Station", "Haul")]
StationHaulLink[, NewStation := Station]
useOldStationCode(StationHaulLink)





StationLengthDist <- g$outputData$StationLengthDist[!is.na(g$outputData$StationLengthDist$WeightedCount), ]
# Convert to data.table:
StationLengthDist <- data.table::as.data.table(StationLengthDist)

# Get the RegroupLengthDistribution, and rename the Stations to the convension of the old StoX (cruise/serialnumber):
StationLengthDistNew <- f2$RegroupLengthDistribution[!is.na(Haul), ]
useOldStationCode(StationLengthDistNew)


# Order the length distributions of the old and new StoX:
StationLengthDist <- StationLengthDist[order(StationLengthDist$Station), ]
StationLengthDistNew <- StationLengthDistNew[order(StationLengthDistNew$Station), ]

# Observe that there are stations not filtered out in the new project, which could have been filtered out using FilterBiotic():
l1 <- unique(StationLengthDist$Station)
l2 <- unique(StationLengthDistNew$Station)
ll <- intersect(l1, l2)
cbind(l1[1:40], l2[1:40])
setdiff(l1, l2)
setdiff(l2, l1)

# Keep only the stations common to both projects:
StationLengthDist <- StationLengthDist[Station %in% ll, ]
StationLengthDistNew <- StationLengthDistNew[Station %in% ll, ]

# The WeightedCount is almost identical:
plot(StationLengthDist$WeightedCount)
points(StationLengthDistNew$WeightedCount, col = 2)

all.equal(StationLengthDist$WeightedCount, StationLengthDistNew$WeightedCount)
range(StationLengthDist$WeightedCount - StationLengthDistNew$WeightedCount)





TotalLengthDist <- data.table::as.data.table(g$outputData$TotalLengthDist)
bioticassignment <- data.table::as.data.table(g$processData$bioticassignment)
suassignment <- data.table::as.data.table(g$processData$suassignment)

StationPSULink <- merge(TotalLengthDist, bioticassignment, all = TRUE)
StationPSULink <- merge(StationPSULink, suassignment, all = TRUE)
StationPSULink <- StationPSULink[, c("Station", "SampleUnit")]
StationPSULink <- unique(StationPSULink)



#StationLengthDistNew_S <- data.table::copy(StationLengthDistNew)
#StationLengthDistNew_S[, PSU := paste0("S", as.numeric(substr(PSU, 4, 6)))]


SweptAreaDensity <- data.table::as.data.table(g$outputData$SweptAreaDensity)

merge(SweptAreaDensity, StationPSULink)



setkey(SweptAreaDensity, SampleUnit)
setkey(m, SampleUnit)
SweptAreaDensity[m, Station := newName]
testData




SweptAreaDensity[, PSU := SampleUnit]


# Get the RegroupLengthDistribution, and rename the Stations to the convension of the old StoX (cruise/serialnumber):
SweptAreaDensityNew <- data.table::copy(f2$SweptAreaDensity)
dd <- merge(SweptAreaDensityNew, f2$DefineSweptAreaPSU$Station_PSU)
# SweptAreaDensityNew[, PSU := paste0("S", as.numeric(substr(PSU, 4, 6)))]
useOldStationCode(StationLengthDistNew)


# Order the length distributions of the old and new StoX:
SweptAreaDensity <- SweptAreaDensity[order(SweptAreaDensity$PSU), ]
SweptAreaDensityNew <- SweptAreaDensityNew[order(SweptAreaDensityNew$PSU), ]

# Observe that there are stations not filtered out in the new project, which could havev been filtered out using FilterBiotic():
l1 <- unique(SweptAreaDensity$PSU)
l2 <- unique(SweptAreaDensityNew$PSU)
ll <- intersect(l1, l2)
cbind(l1[1:40], l2[1:40])
# Some stations are outside of the stratum system in the new project (what should we do with these?):
setdiff(l1, l2)
# Some stations are not filtered out in the new project:
setdiff(l2, l1)

# Keep only the stations common to both projects:
SweptAreaDensity <- SweptAreaDensity[PSU %in% ll, ]
SweptAreaDensityNew <- SweptAreaDensityNew[PSU %in% ll, ]

# The WeightedCount is almost identical:
plot(StationLengthDist$WeightedCount)
points(StationLengthDistNew$WeightedCount, col = 2)

all.equal(StationLengthDist$WeightedCount, StationLengthDistNew$WeightedCount)
range(StationLengthDist$WeightedCount - StationLengthDistNew$WeightedCount)






AbundanceNew <- data.table::copy(f2$Abundance)
Abundance <- data.table::as.data.table(g$outputData$AbundanceByLength)
AbundanceNew
Abundance
par(mfrow = c(5,5), mar = c(1,1,1,1), oma = c(0,0,0,0))
for(s in unique(AbundanceNew$Stratum)) {
	plot(Abundance[SampleUnit == s]$Abundance, cex = 1.2)
	points(AbundanceNew[Stratum == s]$Abundance, col = 2)
}

par(mfrow = c(5,5), mar = c(1,4,1,1), oma = c(0,0,0,0))
for(s in unique(AbundanceNew$Stratum)) {
	plot((Abundance[SampleUnit == s]$Abundance - AbundanceNew[Stratum == s]$Abundance) / Abundance[SampleUnit == s]$Abundance)
}




# 



dup <- function(x) {
	duplicated(x) | duplicated(x, fromLast = TRUE)
}

by <- c("PSU", "SpeciesCategory", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter")
w <- which(dup(LengthDistributionData[, ..by]))
LengthDistributionData[w]
by <- c("PSU", "Haul", "SpeciesCategory", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter")
w <- which(dup(LengthDistributionData[, ..by]))
LengthDistributionData[w]
by <- c("PSU", "Haul", "SampleKey", "SpeciesCategory", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter")
w <- which(dup(LengthDistributionData[, ..by]))
LengthDistributionData[w]
by <- c("PSU", "CruiseKey", "Haul", "SampleKey", "SpeciesCategory", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter")
w <- which(dup(LengthDistributionData[, ..by]))
LengthDistributionData[w]
by <- c("PSU", "CruiseKey", "Haul", "StationKey", "SampleKey", "SpeciesCategory", "IndividualTotalLengthCentimeter", "LengthResolutionCentimeter")
w <- which(dup(LengthDistributionData[, ..by]))
LengthDistributionData[w]







# Compare the swept area density:
SweptAreaDensity <- data.table::as.data.table(g$outputData$SweptAreaDensity)
SweptAreaDensityNew <- f2$SweptAreaDensity






profvis({
	g2 <- getFilterOptionsAll("~/workspace/stox/project/Test_Rstox3", "baseline", "P002")
})


g2 <- getFilterOptionsAll("~/workspace/stox/project/Test_Rstox3", "baseline", "P002")





system.time(g2 <- getFilterOptionsAll("~/workspace/stox/project/Test_Rstox3", "baseline", "P009", include.numeric = FALSE))



g1 <- getFilterOptions("~/workspace/stox/project/Test_Rstox3", "baseline", "P004", "Station")
g2 <- getFilterOptionsAll("~/workspace/stox/project/Test_Rstox3", "baseline", "P004")






projectPath <- "~/workspace/stox/project/Test4"
closeProject(projectPath)
openProject(projectPath, force = TRUE)
modelName <- "baseline"
system.time(processTable <- getProcessTable(projectPath, modelName))
setUseProcessDataToFALSE(projectPath, modelName, "P012")
modifyProcess(projectPath, modelName, "DefineBioticAssignment", list(functionParameters = list(DefinitionMethod = "None")))
system.time(f2 <- runModel(projectPath, modelName))

system.time(g4 <- getInteractiveData(projectPath, "baseline", "P010"))


system.time(g <- getProcessOutputTableNames(projectPath, modelName, "P012"))




RstoxFramework::addHaulToAssignment(Stratum = "s1", PSU = "PSU1", Layer = "WaterColumn", Haul = "2017102/4/2017/4174/2/70162", projectPath, modelName, "P012")







projectPath <- "~/workspace/stox/project/testStoX30_lengtdistribution"
closeProject(projectPath)
openProject(projectPath)
modelName <- "baseline"
system.time(processTable <- getProcessTable(projectPath, modelName))
system.time(f3 <- runModel(projectPath, modelName))





















# Known issues:
# 1. Un-informative error when trying to modify function name to non-existing function ()
RstoxFramework:::modifyFunctionName(projectPath, modelName, "P034", "StoXBiotic")
RstoxFramework:::modifyFunctionName(projectPath, modelName, "P034", "RstoxData::StoxBiotic2")



system.time(f <- runModel(projectPath, modelName, startProcess = 1, endProcess = 1))
system.time(f <- runModel(projectPath, modelName, endProcess = 1))


system.time(closeProject(projectPath = projectPath))
system.time(openProject(projectPath = projectPath))








VariableConversionTable <- DefineBioticVariableConversion(FileName = "~/workspace/stox/project/TestDefineVariables2.txt")
b <- ConvertBioticVariables(f2$ReadBiotic, ConversionMethod = "PreDefined", BioticVariableConversion = VariableConversionTable)










projectPath <- "~/workspace/stox/project/BS_swept_area_cod_2019"
openProject(projectPath)
modelName <- "baseline"
addProcess(projectPath, modelName, list(functionName = "FilterStoxBiotic"))
gg <- getProjectMemoryData(projectPath, modelName)
saveProject(projectPath)
closeProject(projectPath)
openProject(projectPath)
system.time(processTable <- getProcessTable(projectPath, modelName))


RstoxFramework:::modifyFunctionName(projectPath, modelName, "P037", "RstoxData::StoxBiotic2")
removeProcess(projectPath, modelName, "P037")

RstoxFramework:::getFunctionName(
	projectPath = projectPath, 
	modelName = modelName, 
	processID = "P033"
)



rearrangeProcesses(projectPath, modelName, c("P003"), "P002")

rearrangeProcesses(projectPath, modelName, c("P001"), "P003")


resetModel(projectPath, modelName, "P003")
resetModel(projectPath, modelName, "P073")

resetModel(projectPath, modelName)
resetModel(projectPath, modelName)


RstoxAPI::runFunction("getModelInfo", args = list())

runFunction("getAvailableTemplatesDescriptions", args = list())
jsonlite::toJSON(runFunction("getActiveProcessID", args = list(projectPath = projectPath, modelName = "baseline")), auto_unbox = TRUE, pretty = TRUE)





	
	
setProcessPropertyValue("functionParameters", "FilterExpression", "{\"biotic_cruiseNumber_0146_2019_UFJN_VLNY_Vilnyus_2019-05-03T22.03.07.377Z.xml/individual\":\"(length < 0.07)\"}", projectPath, modelName, "P002")


# 1: Move processes [DONE]
rearrangeProcesses(projectPath, modelName, c("P014", "P013"), "P010")
g <- getProcessTable(projectPath, modelName)
# Produces function input errors:
rearrangeProcesses(projectPath, modelName, c("P017", "P029"), "P010")
getProcessTable(projectPath, modelName)

# 2: Change process name - change process index table [DONE]
# 3: activeProcessID to the current, and a flag processModified which is TRUE if the user modifies something
setProcessPropertyValue("processArguments", "processName", "StoxAcoustic2", projectPath, modelName, "P009")

# 4: Function name "" shuold be allowed
setProcessPropertyValue("processArguments", "functionName", "", projectPath, modelName, "P006")

# 6: nchar of process name should be > 0 [DONE]
setProcessPropertyValue("processArguments", "processName", "", projectPath, modelName, "P009")


setProcessPropertyValue("processArguments", "enabled", FALSE, projectPath, modelName, "P001")


setProcessPropertyValue("functionParameters", "FilterExpression", "", projectPath, modelName, "P004")


RstoxFramework:::modifyFunctionName(projectPath, modelName, "P015", "")

addEmptyProcess(projectPath, modelName, processName = NULL)


# Set file names
setProcessPropertyValue("functionParameters", "FileNames", c("~/workspace/stox/project/Test_Rstox3/process/projectSession/projectMemory/current/maxProcessIntegerID.txt", "~/workspace/stox/project/Test_Rstox3/process/projectSession/projectMemory/current/processIndexTable.txt"), projectPath, modelName, "P001")

getProcessPropertySheet(projectPath, modelName, processID = "P004")


resp <- POST("http://localhost:5307/ocpu/library/RstoxAPI/R/runFunction/json?auto_unbox=TRUE", body = list(what="'getActiveProcessID'", args="{\"projectPath\":\"/Users/arnejh/workspace/stox/project/Test_Rstox3\",\"modelName\":\"baseline\"}", removeCall=F), encode = 'form')







RstoxFramework:::getProcessData(projectPath, modelName, "P011")
addAcousticPSU("9", NULL, projectPath, modelName, "P011")
RstoxFramework:::getProcessData(projectPath, modelName, "P011")
addAcousticPSU("9", NULL, projectPath, modelName, "P011")


#RstoxFramework::saveProject("~/workspace/stox/project/Test_Rstox3")

processOutput <- mapply(getProcessOutput, "~/workspace/stox/project/Test_Rstox3", "baseline", processTable$processID)
names(processOutput) <- processTable$processName
TSD::dim_all(processOutput)


d1 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "baseline", "P003")
newStratum <- "{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"geometry\":{\"type\":\"Polygon\",\"coordinates\":[[[-5.504451280614937,73.6547169381465],[4.890626652422555,73.06960536370295],[-5.7386499995453875,70.67817566972614],[-5.504451280614937,73.6547169381465]]]},\"properties\":{\"polygonName\":\"s101\"},\"id\":1000}]}"
addStratum(newStratum, "~/workspace/stox/project/Test_Rstox3", "baseline", "P003")
system.time(f <- runModel("~/workspace/stox/project/Test_Rstox3", modelName = "baseline", startProcess = 3, endProcess = 3))
d2 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "baseline", "P003")


newStratum <- "{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"geometry\":{\"type\":\"Polygon\",\"coordinates\":[[[-5.504451280614937,73.6547169381465],[4.890626652422555,73.06960536370295],[-5.7386499995453875,70.67817566972614],[-5.504451280614937,73.6547169381465]]]},\"properties\":{\"polygonName\":\"sp101\"},\"id\":1000}]}"
addStratum(newStratum, "~/workspace/stox/project/Test_Rstox3", "baseline", "P003")
system.time(f <- runModel("~/workspace/stox/project/Test_Rstox3", modelName = "baseline", startProcess = 3, endProcess = 3))
d3 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "baseline", "P003")




n <- processOutput$NASC
system.time(nn <- RstoxBase:::meanData(n, targetResolution = "Stratum"))
system.time(s <- RstoxBase:::sumData(n, targetResolution = "Layer"))
dim(n)
dim(nn)
dim(s)

n <- processOutput$LengthDistribution
system.time(nn <- RstoxBase:::meanData(n, targetResolution = "Stratum"))
system.time(s <- RstoxBase:::sumData(n, targetResolution = "Layer"))
dim(n)
dim(nn)
dim(s)


g1 <- getFilterOptions("~/workspace/stox/project/Test_Rstox3", "baseline", "P003", "Individual")
g2 <- getFilterOptions("~/workspace/stox/project/Test_Rstox3", "baseline", "P001", "biotic_cruiseNumber_2017838_Eros_2019-02-19T08.33.14.905Z.xml/fishstation")
names(g1)
names(g2)
jsonlite::toJSON(g1, auto_unbox = TRUE, pretty = TRUE)
jsonlite::toJSON(g2, auto_unbox = TRUE, pretty = TRUE)


l <- list(
	negate = TRUE, 
	linkOperator = "&", 
	group = list(
		list(
			negate = FALSE, 
			linkOperator = "|", 
			group = list(
				list(
					negate = FALSE, 
					field = "age", 
					operator = "!=", 
					value = "fisk ; sild"
				), 
				list(
					negate = TRUE, 
					field = "sttype", 
					operator = "<", 
					value = 5
				)
			)
		), 
		list(
			negate = FALSE, 
			field = "name", 
			operator = "%in%", 
			value = c(1, 2, 4.5)
		)
	)
)

ll <- list2expression(l)
lll <- expression2list(ll, NULL)
names(lll)
names(l)
identical(lll, l)
all.equal(lll, l)




system.time(g <- getEDSUData("~/workspace/stox/project/Test_Rstox3", "baseline", "P006"))
system.time(gg <- getMapData("~/workspace/stox/project/Test_Rstox3", "baseline", "P003"))
system.time(ggg <- getMapData("~/workspace/stox/project/Test_Rstox3", "baseline", "P006"))
system.time(g4 <- getInteractiveData("~/workspace/stox/project/Test_Rstox3", "baseline", "P007"))
system.time(g5 <- getInteractiveData("~/workspace/stox/project/Test_Rstox3", "baseline", "P009"))
system.time(g5 <- getInteractiveData("~/workspace/stox/project/Test_Rstox3", "baseline", "P030"))

projectPathprojectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "baseline"
processID <- "P017"
p <- RstoxFramework::getProcessPropertySheet(projectPath, modelName, processID)
p


projectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "baseline"
processID <- "P007"
AcosticPSU <- RstoxFramework:::getProcessData(projectPath, modelName, processID)
PSU <- "T0005"
EDSU <- c("2017838/2017-02-13T19:41:41.000Z", "2017838/2017-02-13T19:47:54.000Z", "2017838/2017-02-13T19:54:09.000Z")
AcosticPSU$EDSU_PSU[EDSU %in% eval(EDSU), PSU := eval(PSU)]





StoxBiotic <- processOutput$StoxBiotic
BioticAssignment <- processOutput$DefineBioticAssignment
MergedStoxBiotic <- RstoxData::MergeStoxBiotic(StoxBiotic)
abundanceResolutionVariables <- RstoxBase:::getAllResolutionVariables("AbundanceData")
assignedHauls <- BioticAssignment[, .(Haul = unique(Haul)), by = abundanceResolutionVariables]




IndividualsData <- processOutput$IndividualsSweptArea
AbundanceData <- processOutput$Abundance
SuperIndividualsData <- merge(IndividualsData, AbundanceData, by = intersect(names(IndividualsData), names(AbundanceData)))
LengthDistributionData <- processOutput$LengthDistribution





	
LengthDistributionData <- processOutput$RegroupLengthDistribution
BioticAssignment <- processOutput$DefineBioticAssignment
d <- AssignmentLengthDistribution(LengthDistributionData, BioticAssignment)
	

dataTypeDefinition <- getRstoxBaseDefinitions("dataTypeDefinition")
mapData <- mapply(getMapData, "~/workspace/stox/project/Test_Rstox3", "baseline", processTable$processID)
names(mapData) <- processTable$processName
TSD::dim_all(mapData)



library(sp)
plot(processOutput$DefineStratumPolygon)
points(processOutput$StoxAcoustic$Log$Longitude, processOutput$StoxAcoustic$Log$Latitude)
points(processOutput$StoxBiotic$Station$Longitude, processOutput$StoxBiotic$Station$Latitude, col = 2)











projectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "baseline"
processID <- "P009"

temp <-addHaulToAssignment(PSU = "T2267", Layer = 1, Haul = "24076/3536111", projectPath, modelName, processID)

temp <-removeHaulFromAssignment(PSU = "T2267", Layer = 1, Haul = "24076/3536111", projectPath, modelNamemodelName, processID)



projectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "baseline"
processID <- "P007"

temp <- addAcousticPSU("S2", PSU = NULL, projectPath, modelName, processID)



projectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "baseline"
processID <- "P003"
StratumPolygon <- processOutput$DefineStratumPolygon

newStratum <- StratumPolygon$StratumPolygon
newStratumnewStratum@polygons <- newStratum$newStratum@polygons[1]
temp <- addStratum(newStratum, projectPath, modelName, processID)
temp <- RstoxFramework:::getProcessData(projectPath, modelName, processID)

temp <- modifyStratum(newStratum, projectPath, modelName, processID)
temp <- RstoxFramework:::getProcessData(projectPath, modelName, processID)






projectPath <- "~/workspace/stox/project/Test_Rstox3"

system.time(RstoxFramework::createProject(projectPath, template = "Test3.0", ow = TRUE))
temp <- RstoxFramework:::modifyFunctionName("~/workspace/stox/project/Test_Rstox3", "baseline", "P001", "ReadBiotic")

# Add process 7, DefineAcousticLayer:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineAcousticLayer", 
		functionName = "DefineAcousticLayer", 
		functionInputs = list(
			StoxAcousticData = "StoxAcoustic"
		)
	)
)


d1 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox3", "baseline", "P001")
jsonlite::toJSON(d1, pretty = TRUE, auto_unbox = TRUE)

d2 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox3", "baseline", "P004")
jsonlite::toJSON(d2, pretty = TRUE, auto_unbox = TRUE)

d3 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox3", "baseline", "P005")
jsonlite::toJSON(d3, pretty = TRUE, auto_unbox = TRUE)


d7 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox3", "baseline", "P017")
jsonlite::toJSON(d7, pretty = TRUE, auto_unbox = TRUE)





stratum <- "{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"geometry\":{\"type\":\"Polygon\",\"coordinates\":[[[4.3000000000000025,62.249999997276205],[3.7666666700000015,62.249999997276205],[3.7666666699999984,62.48999999713977],[-1.6640291377292762,64.84799456068586],[6.116666670000004,63.499999996573344],[7.000000000000003,63.499999996573344],[4.3000000000000025,62.249999997276205]]]},\"properties\":{\"polygonName\":\"2\"},\"id\":2},{\"type\":\"Feature\",\"geometry\":{\"type\":\"Polygon\",\"coordinates\":[[[4.3000000000000025,62.249999997276205],[3.7666666700000015,62.249999997276205],[3.7666666699999984,62.48999999713977],[-1.6640291377292762,64.84799456068586],[6.116666670000004,63.499999996573344],[7.000000000000003,63.499999996573344],[4.3000000000000025,62.249999997276205]]]},\"properties\":{\"polygonName\":\"5\"},\"id\":5}]}"
modifyStratum(stratum, projectPath = "~/workspace/stox/project/Test_Rstox3_cod2019", modelName = "baseline", processID = "P003")












########################################################
##### Create test projects from official projects: #####
########################################################

# Function to create paths to input files:
inputFile <- function(fileName, projectPath = NULL, folder = "acoustic") {
	inputFolder <- getRstoxFrameworkDefinitions("paths")$stoxFolders[["Input"]]
	if(length(projectPath)) {
		folder <- file.path(projectPath, inputFolder, folder)
		dir.create(folder, recursive = TRUE, showWarnings = FALSE)
	}
	else {
		folder <- file.path(inputFolder, folder)
	}
	
	file.path(folder, basename(fileName))
}


# Funciton to add ReadBiotic after copying the files:
addReadBiotic <- function(projectPath, skeleton, ...) {
	bioticResourceProjectPath <- file.path(skeleton$projectPathOriginal, "input", "biotic")
	bioticFileNames <- list.files(bioticResourceProjectPath, full.names = TRUE)
	file.copy(
		bioticFileNames, 
		inputFile(bioticFileNames, projectPath = projectPath, folder = "biotic")
	)
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "ReadBiotic", 
			functionName = "ReadBiotic", 
			functionParameters = list(
				FileNames = inputFile(bioticFileNames, projectPath = NULL, folder = "biotic")
			)
		)
	)
	
	temp
}
# Funciton to add StoxBiotic:
addFilterBiotic <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "FilterBiotic", 
			functionName = "FilterBiotic", 
			functionInputs = list(
				BioticData = "ReadBiotic"
			), 
			
		)
	)
	
	temp
}
# Funciton to add StoxBiotic:
addStoxBiotic <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "StoxBiotic", 
			functionName = "StoxBiotic", 
			functionInputs = list(
				BioticData = "ReadBiotic"
			)
		)
	)
	
	temp
}
# Funciton to add ReadBiotic after copying the files:
addReadAcoustic <- function(projectPath, skeleton, ...) {
	acousticResourceProjectPath <- file.path(skeleton$projectPathOriginal, "input", "acoustic")
	acousticFileNames <- list.files(acousticResourceProjectPath, full.names = TRUE)
	file.copy(
		acousticFileNames, 
		inputFile(acousticFileNames, projectPath = projectPath, folder = "acoustic")
	)
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "ReadAcoustic", 
			functionName = "ReadAcoustic", 
			functionParameters = list(
				FileNames = inputFile(acousticFileNames, projectPath = NULL, folder = "acoustic")
			)
		)
	)
	
	temp
}
# Funciton to add StoxBiotic:
addStoxAcoustic <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "StoxAcoustic", 
			functionName = "StoxAcoustic", 
			functionInputs = list(
				BioticData = "ReadAcoustic"
			)
		)
	)
	
	temp
}
# Funciton to add DefineStratumPolygon:
addDefineStratumPolygon <- function(projectPath, skeleton, ...) {
	inputFileNames <- skeleton$polygonFile
	file.copy(
		inputFileNames, 
		inputFile(inputFileNames, projectPath = projectPath, folder = "input")
	)
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "DefineStratumPolygon", 
			functionName = "DefineStratumPolygon", 
			functionParameters = list(
				FileName = inputFile(inputFileNames, projectPath = NULL, folder = "input")
			)
		)
	)
	
	temp
}
# Funciton to add StratumArea:
addStratumArea <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "StratumArea", 
			functionName = "StratumArea", 
			functionInputs = list(
				StratumPolygon = "DefineStratumPolygon"
			)
		)
	)
	
	temp
}
# Funciton to add DefineSweptAreaPSU:
addDefineSweptAreaPSU <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "DefineSweptAreaPSU", 
			functionName = "DefineSweptAreaPSU", 
			functionInputs = list(
				StratumPolygon = "DefineStratumPolygon", 
				StoxBioticData = "StoxBiotic"
			)
		)
	)
	
	temp
}
# Funciton to add DefineSweptAreaLayer:
addDefineSweptAreaLayer <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "DefineSweptAreaLayer", 
			functionName = "DefineSweptAreaLayer", 
			functionInputs = list(
				StoxBioticData = "StoxBiotic"
			), 
			functionParameters = list(
				DefinitionMethod = "WaterColumn"
				#DefinitionMethod = "HighestResolution"
			)
		)
	)
	
	temp
}
# Funciton to add DefineAcousticPSU:
addDefineAcousticPSU <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "DefineAcousticPSU", 
			functionName = "DefineAcousticPSU", 
			functionInputs = list(
				StratumPolygon = "DefineStratumPolygon", 
				StoxBioticData = "StoxAcoustic"
			)
		)
	)
	
	temp
}
# Funciton to add DefineAcousticLayer:
addDefineAcousticLayer <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "DefineAcousticLayer", 
			functionName = "DefineAcousticLayer", 
			functionInputs = list(
				StoxBioticData = "StoxAcoustic"
			), 
			functionParameters = list(
				DefinitionMethod = "WaterColumn"
				#DefinitionMethod = "HighestResolution"
			)
		)
	)
	
	temp
}
# Funciton to add LengthDistribution:
addLengthDistribution <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "LengthDistribution", 
			functionName = "LengthDistribution", 
			functionInputs = list(
				StoxBioticData = "StoxBiotic", 
				SweptAreaPSU = "DefineSweptAreaPSU", 
				SweptAreaLayer = "DefineSweptAreaLayer"
			), 
			functionParameters = list(
				LengthDistributionType = "Normalized"
			)
		)
	)
	
	temp
}
# Funciton to add LengthDistribution:
addLengthDependentCatchCompensation <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "LengthDependentCatchCompensation", 
			functionName = "LengthDependentCatchCompensation", 
			functionInputs = list(
				LengthDistributionData = "LengthDistribution"
			), 
			functionParameters = list(
				LengthDependentSweepWidthParameters = data.table::data.table(
					SpeciesCategory = "brosme/164740/126447/NA", 
					LMin = 15.0, 
					LMax = 62.0, 
					Alpha = 5.91, 
					Beta = 0.43
				)
			)
		)
	)
	
	temp
}
# Funciton to add LengthDistribution:
addRegroupLengthDistribution <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "RegroupLengthDistribution", 
			functionName = "RegroupLengthDistribution", 
			functionInputs = list(
				LengthDistributionData = "LengthDependentCatchCompensation"
			), 
			functionParameters = list(
				LengthInterval = 5
			)
		)
	)
	
	temp
}
# Funciton to add LengthDistribution:
addNASC <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "NASC", 
			functionName = "NASC", 
			functionInputs = list(
				StoxBioticData = "StoxAcoustic", 
				SweptAreaPSU = "DefineAcousticPSU", 
				SweptAreaLayer = "DefineAcousticLayer"
			)
		)
	)
	
	temp
}


addProcessByProcessName <- function(processName, skeleton) {
	functionName <- paste0("add", processName)
	do.call(functionName, list(projectPath = skeleton$projectPath, skeleton = skeleton))
}


# Function to create a project skeleton:
createTestProjectSkeleton <- function(projectName, projectPathOriginal, sts = NULL, year = NULL, dir = "~/workspace/stox/project", download = FALSE) {
	
	# If sts and year is given, create the path to the original project:
	if(length(sts) && length(year)) {
		projectPathOriginal <- file.path(dir, paste(sts, year, sep = "_"))
	}
	# Download the specific year of the given survey time series
	if(download) {
		Rstox::getNMDdata(sts, subset = year, dir = dir, ow = TRUE)
	}
	
	# Run the original project and save the baseline:
	stratumpolygon <- getBaseline(projectPathOriginal, proc = FALSE, input = "proc")$stratumpolygon
	
	# acousticresourceProjectPath <- file.path(projectPathOriginal, "input", "acoustic")
	
	# Create the project: 
	projectPath <- file.path(dir, projectName)
	RstoxFramework::createProject(projectPath, template = "UserDefinedTemplate", ow = TRUE)
	
	# Create the stratum file:
	polygonFile <- file.path(Rstox::getProjectPaths(projectPathOriginal)$inputDir, "stratumpolygon.txt")
	data.table::fwrite(stratumpolygon[, c("Stratum", "Polygon" )], file = polygonFile, sep ="\t", col.names = FALSE)
	
	
	# Return the original and new output:
	out <- list(
		projectPath = projectPath, 
		projectPathOriginal = projectPathOriginal, 
		polygonFile = polygonFile
	)
	return(out)
}

# Create a swept area project:
createTestProject <- function(projectName, projectPathOriginal, processes = c("ReadBiotic", "StoxBiotic", "DefineStratumPolygon", "StratumArea", "DefineSweptAreaPSU", "DefineSweptAreaLayer", "LengthDistribution"), sts = NULL, year = NULL, dir = "~/workspace/stox/project", download = FALSE) {
	
	skeleton <- createTestProjectSkeleton(
		projectName = projectName, 
		projectPathOriginal = projectPathOriginal, 
		sts = sts, 
		year = year, 
		dir = dir, 
		download = download
	)
	
	# acousticresourceProjectPath <- file.path(projectPathOriginal, "input", "acoustic")
	lapply(processes, addProcessByProcessName, skeleton = skeleton)
	
	
	#
	## Add process 1, ReadBiotic:
	#addReadBiotic(projectPath = projectPath, skeleton = skeleton)
	## Add process 2, StoxBiotic:
	#addStoxBiotic(projectPath = projectPath)
	## Add process 3, DefineStratumPolygon:
	#addDefineStratumPolygon(projectPath = projectPath, skeleton = skeleton)
	## Add process 4, StratumArea:
	#addStratumArea(projectPath = projectPath)
	## Add process 5, DefineSweptAreaPSU:
	#addDefineSweptAreaPSU(projectPath = projectPath)
	## Add process 6, DefineSweptAreaLayer:
	#addDefineSweptAreaLayer(projectPath = projectPath)
	## Add process 7, LengthDistribution:
	#addLengthDistribution(projectPath = projectPath)
	
	
	# Get the names of the processes: 
	processTable <- getProcessTable(skeleton$projectPath, "baseline")
	
	# Run the baseline of the new project:
	system.time(f <- runModel(skeleton$projectPath, modelName = "baseline"))
	
	# Get the process output:
	processOutput <- mapply(getProcessOutput, skeleton$projectPath, "baseline", processTable$processID)
	names(processOutput) <- processTable$processName
	
	# Run the original project and save the baseline:
	original <- getBaseline(skeleton$projectPathOriginal)
	
	# Return the original and new output:
	out <- list(
		original = original, 
		new = processOutput
	)
	return(out)
}


library(Rstox)
library(RstoxFramework)
options(deparse.max.lines = 10)


# Create the Barents sea cod 2019 project:
# First time you will need to set download = TRUE to download the specific year of the survey time series:
sts <- "Barents Sea Northeast Arctic cod bottom trawl index in winter"
year <- 2019
projectName = "Test_Rstox3_cod2019"
cod2019 <- createTestProject(
	projectName = projectName, 
	sts = sts, 
	year = year, 
	dir = "~/workspace/stox/project", 
	processes = c("ReadBiotic", "StoxBiotic", "FilterStoxBiotic", "DefineStratumPolygon", "StratumArea", "DefineSweptAreaPSU", "DefineSweptAreaLayer", "LengthDistribution", "LengthDependentCatchCompensation", "RegroupLengthDistribution")
)

cod2019$original$outputData$StationLengthDist
cod2019$new$LengthDistribution

# Create the herring 2018 project:
# First time you will need to set download = TRUE to download the specific year of the survey time series:
sts <- "Norwegian Sea NOR Norwegian spring-spawning herring acoustic abundance index in Feb-Mar"
year <- 2018
projectName = "Test_Rstox3_herring2018"
herring2018 <- createTestProject(
	projectName = projectName, 
	sts = sts, 
	year = year, 
	dir = "~/workspace/stox/project", 
	processes = c("ReadBiotic", "StoxBiotic", "ReadAcoustic", "StoxAcoustic", "DefineStratumPolygon", "StratumArea", "DefineAcousticPSU", "DefineAcousticLayer", "LengthDistribution", "NASC")
)


system.time(f <- runModel("~/workspace/stox/project/Test_Rstox3_herring2018", modelName = "baseline", endProcess = 3))
############################################
############################################



d <- RstoxBase:::meanData(cod2019$new$LengthDistribution)




# If trouble with download limit on GitHub:
# usethis::browse_github_pat() # Generate new token, then copy it (R:GITHUB_PAT)
# usethis::edit_r_environ() # A file is opened. Paste the token to this file. Then viola!



#l <- load('~/workspace/stox/project/Test_Rstox3/process/project.RData')
projectPath <- "~/workspace/stox/project/Test_Rstox3"
projectDescription <- getProjectMemoryData(projectPath, named.list = TRUE)
geo <- geojsonio::geojson_json(projectDescription$baseline$P001$processData$StratumPolygon)
p <- pretty(as.character(geo), indent = 4)


# 1. Convert spatial to geojson string: 
convertProcessDataToGeojson <- function(projectDescription) {
	# Run through the processes and convert SpatialPolygonsDataFrame to geojson string:
	for(modelName in names(projectDescription)) {
		for(processIndex in seq_along(projectDescription [[modelName]])) {
			for(processDataIndex in names(projectDescription [[modelName]] [[processIndex]]$processData)) {
				this <- projectDescription [[modelName]] [[processIndex]]$processData[[processDataIndex]]
				if("SpatialPolygonsDataFrame" %in% class(this)) {
					projectDescription [[modelName]] [[processIndex]]$processData[[processDataIndex]] <- geojsonio::geojson_json(this)
				}
			}
		}
	}
	
	return(projectDescription)
}





projectDescription3 <- convertProcessDataToGeojson(projectDescription)

# 2. Convert to json structure:
for(ind in seq_along(projectDescription3)) {
	#names(projectDescription3[[ind]]) <- "process"
	projectDescription3[[ind]] <- c(
		modelName = unname(names(projectDescription3)[ind]),
		processes = list(unname(projectDescription3[[ind]]))
	)
}
projectDescription3 <- unname(projectDescription3)

# 3. Write project.json file:
system.time(j <- jsonlite::toJSON(projectDescription3, pretty = TRUE, auto_unbox = TRUE))
write(j, "wtest.json")

# 4. Read project.json file to R list:
system.time(g <- jsonlite::read_json("wtest.json"))


# 5. Reintroduce spatial from geojson string:
convertGeojsonToProcessData <- function(projectDescription) {
	# Run through the processes and convert SpatialPolygonsDataFrame to geojson string:
	for(modelName in names(projectDescription)) {
		for(processIndex in seq_along(projectDescription [[modelName]])) {
			for(processDataIndex in names(projectDescription [[modelName]] [[processIndex]]$processData)) {
				this <- projectDescription [[modelName]] [[processIndex]]$processData[[processDataIndex]]
				if(is.character(this) && grepl("FeatureCollection", substring(this, 1, 100))) {
					projectDescription [[modelName]] [[processIndex]]$processData[[processDataIndex]] <- geojsonio::geojson_sp(this)
				}
			}
		}
	}
	
	return(projectDescription)
}








for(ind in seq_along(projectDescription3)) {
	projectDescription3[[ind]] <- c(
		modelName = unname(names(projectDescription3)[ind]),
		projectDescription3[[ind]]
	)
}



pr <- projectDescription3
pr[[1]]$processes <- pr[[1]]$processes[c(1, 2, 11)]
j <- jsonlite::toJSON(pr, pretty = TRUE, auto_unbox = TRUE)
write(j, "wtest.json")



projectDescription2 <- projectDescription
projectDescription2$baseline$P001$processData$StratumPolygon <- geojsonio::geojson_json(projectDescription2$baseline$P001$processData$StratumPolygon)
j <- jsonlite::toJSON(projectDescription3[[1]][1:2], force = FALSE, pretty = TRUE, auto_unbox = TRUE)
jsonlite::write_json(j, "test.json")
write(j, "wtest.json")
g1 <- jsonlite::read_json("test.json")
g2 <- jsonlite::read_json("wtest.json")


j <- jsonlite::toJSON(projectDescription$baseline[1:2], force = TRUE, pretty = TRUE, auto_unbox = TRUE)
jsonlite::write_json(j, "test.json")
write(j, "wtest.json")

g <- jsonlite::read_json("test.json")
gg <- jsonlite::fromJSON(g[[1]])



j <- jsonlite::toJSON(projectDescription, force = TRUE, pretty = TRUE, auto_unbox = TRUE)
jsonlite::write_json(j, "test.json")
g <- jsonlite::read_json("test.json")
gg <- jsonlite::fromJSON(g[[1]])


# Filter list structure:
l <- list(
	group = list(
		linkOperator = "|", 
		negate = FALSE,
		expression = list(
			negate = FALSE,
			columnName = "IndividualTotalLengthCentimeter", 
			operator = "<", 
			value = 100
		), 
		group = list(
			linkOperator = "|", 
			negate = FALSE,
			expression = list(
				negate = FALSE,
				columnName = "SpeciesCategory", 
				operator = "==", 
				value = "sild'G03/161722.G03/126417/NA"
			), 
			group = list(
				linkOperator = "&", 
				negate = FALSE,
				expression = list(
					negate = FALSE,
					columnName = "SpeciesCategory", 
					operator = "==", 
					value = "torsk/164712/126436/NA"
				), 
				expression = list(
					negate = TRUE,
					columnName = "IndividualRoundWeightGram", 
					operator = ">=", 
					value = 800
				)
			)
		)
	)
)
jsonlite::toJSON(l)

buildExpression <- function(expression) {
	if(is.list(expression)) {
		output <- do.call(paste, expression[c("columnName", "operator", "value")])
		if(expression$negate) {
			output <- paste("!(", output, ")")
		}
	}
	else {
		output <- expression
	}
	
	output
}
ll <- rapply(l, buildExpression, how = "replace")




# Filter list structure:
l <- list(
	group = list(
		linkOperator = "|", 
		negate = FALSE,
		group = list(
			expression = data.table::data.table(
				negate = FALSE,
				columnName = "IndividualTotalLengthCentimeter", 
				operator = "%in%", 
				value = list(c(12,13,14))
			)
		), 
		group = list(
			linkOperator = "|", 
			negate = FALSE,
			group = list(
				expression = data.table::data.table(
					negate = FALSE,
					columnName = "SpeciesCategoryKey", 
					operator = "==", 
					value = "sild'G03/161722.G03/126417/NA"
				)
			), 
			group = list(
				linkOperator = "&", 
				negate = FALSE,
				group = list(
					expression = data.table::data.table(
						negate = FALSE,
						columnName = "SpeciesCategoryKey", 
						operator = "==", 
						value = "torsk/164712/126436/NA"
					)
				), 
				group = list(
					expression = data.table::data.table(
						negate = TRUE,
						columnName = "IndividualRoundWeightGram", 
						operator = ">=", 
						value = 200
					)
				)
			), 
			group = list(
				expression = data.table::data.table(
					negate = FALSE,
					columnName = "SpeciesCategoryKey", 
					operator = "==", 
					value = "lodde/162035/126735/NA"
				)
			)
		)
	)
)
jsonlite::toJSON(l)



buildExpression <- function(expression) {
	output <- paste(expression$columnName, expression$operator, if(is.character(expression$value)) deparse(paste(expression$value)) else paste(expression$value))
	if(expression$negate) {
		output <- paste("!(", output, ")")
	}
	
	output
}



recurseToDataTable <- function (L, fun) {
	if(inherits(L, "data.table")) {
		fun(L)
	}
	else if(is.list(L)){
		lapply(L, recurseToDataTable, fun)
	}
	else{
		L
	}
}

recurseToGroup <- function (L) {
	# If there are still groups, continue:
	if(is.list(L) && "group" %in% names(L)) {
		if(any(unlist(lapply(L[names(L) == "group"], names)) == "group")) {
			paste(
				"( ", 
				paste(
					unlist( lapply(L[names(L) == "group"], recurseToGroup) ), 
					collapse = paste0(
						") ", 
						L$linkOperator, 
						" ("
					)
				), 
				" )"
			)
		}
		# If there are no groups, execute:
		else if(all(unlist(lapply(L[names(L) == "group"], names) == "expression"))) {
			paste(
				"( ", 
				paste(unlist(L[names(L) == "group"]), collapse = paste0(") ", L$linkOperator, " (")), 
				" )"
			)
		}
	}
	else {
		L
	}
}

r <- recurseToDataTable(l, buildExpression)
rr <- recurseToGroup(r)





buildExpression <- function(expression) {
	if(is.data.frame(expression)) {
		output <- paste(expression[c("columnName", "operator", "value")])
		if(expression$negate) {
			output <- paste("!(", output, ")")
		}
	}
	else {
		output <- expression
	}
	
	output
}
ll <- rapply(l, buildExpression, how = "replace")












system.time(r.ices <- readXmlFile("~/Code/Github/StoX/Releases/StoX 2.9.6/TestProjects/Acoustic_578-1019-2019207.xml"))
system.time(r.ices <- readXmlFile("~/Code/Github/StoX/Releases/StoX 2.9.6/TestProjects/Acoustic_578-1019-2019207.xml", stream = FALSE))

system.time(a.ices <- ReadAcoustic("~/Code/Github/StoX/Releases/StoX 2.9.6/TestProjects/Acoustic_578-1019-2019207.xml"))
system.time(A.ices <- StoxAcoustic(a.ices))

system.time(a.nmd <- ReadAcoustic("~/Code/Github/StoX/Releases/StoX 2.9.6/TestProjects/echosounder_cruiseNumber_2017838_Eros.xml"))
system.time(A.nmd <- StoxAcoustic(a.nmd))



system.time(b.ices <- ReadBiotic("~/Code/Github/StoX/Releases/StoX 2.9.6/TestProjects/Biotic_578-1019-2018207.xml"))
system.time(B.ices <- StoxBiotic(b.ices))

system.time(b.nmd <- ReadBiotic("~/workspace/stox/project/BS_swept_area_cod_2019/input/biotic/biotic_cruiseNumber_0146_2019_UFJN_VLNY_Vilnyus_2019-05-03T22.03.07.377Z.xml"))
system.time(B.nmd <- StoxBiotic(b.nmd))


system.time(b.nmd <- ReadBiotic("~/workspace/stox/project/StoXVerTest/StagedProjOrig/Rstox_1.8_StoXLib_1.76/2017_O-gr_Lengthbased_Selectivity_Several_species/input/biotic/4-2017-1173-56_HH.out.xml"))
system.time(B.nmd <- StoxBiotic(b.nmd))






F.nmd <- FilterStoxAcoustic(A.nmd,  FilterExpression = list(ChannelReference = "ChannelReferenceType == \"P\"", AcousticCategory = "AcousticCategory == \"12\""))
						
						
system.time(nasc.ices <- NASC(A.ices))


# Test of effect of displaced origin in stratum area calculation. The conclusion is to use indiivdual origins, since these are more appropriate for each stratum:
lat_0 <- seq(-85,85,5)
lon_0 <- seq(-20,30,1)

stratumPolygon <- f$DefineStratumPolygon
stratumPolygon <- head(stratumPolygon, 1)
sp::proj4string(stratumPolygon) <- sp::CRS("+proj=longlat +ellps=WGS84")	

area <- array(NA, dim = c(length(lon_0), length(lat_0)))

for(i in seq_along(lat_0)) {
	for(j in seq_along(lon_0)) {
		area[j, i] <- rgeos::gArea(sp::spTransform(stratumPolygon, sp::CRS(
			paste0(
				"+proj=laea +lat_0=", 
				lat_0[i], 
				" +lon_0=", 
				lon_0[j], 
				" +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=kmi +no_defs"
			)
		)))
	}
}

rgl::persp3d(lon_0, lat_0, area, col = "pink", xlab = "lon", ylab = "lat")










projectPath <- "~/workspace/stox/project/BS NEA cod 2019"
openProject(projectPath)
modelName <- "baseline"
system.time(processTable <- getProcessTable(projectPath, modelName))
setProcessPropertyValue("functionParameters", "FilterExpression", projectPath, "baseline", "P002")











d1 <- data.table::data.table(a = 1:12, b = 3, c = "Hei")
d2 <- data.table::data.table(a = seq_len(1e6), b = 3, c = "Hei")
d3 <- data.table::data.table(a = 1:12, b = "/process/projectSession/memory/models/baseline/P006/functionName/functionName_20200430T171700.433Z.rds")
system.time(d <- readRDS("~/workspace/stox/project/Test_Rstox3/process/projectSession/memory/history/projectMemory_20200430T171713.680Z.rds"))


system.time(for(i in 1:10) saveRDS(d1, "test.rds"))
system.time(for(i in 1:10) readRDS("test.rds"))
system.time(for(i in 1:10) saveRDS(d2, "test.rds"))
system.time(for(i in 1:10) readRDS("test.rds"))
system.time(for(i in 1:10) saveRDS(d3, "test.rds"))
system.time(for(i in 1:10) readRDS("test.rds"))
system.time(for(i in 1:10) saveRDS(d, "test.rds"))
system.time(for(i in 1:10) readRDS("test.rds"))


system.time(for(i in 1:10) fst::write_fst(as.data.frame(d1), "test.fst"))
system.time(for(i in 1:10) data.table::as.data.table(fst::read_fst("test.fst")))
system.time(for(i in 1:10) fst::write_fst(as.data.frame(d2), "test.fst"))
system.time(for(i in 1:10) data.table::as.data.table(fst::read_fst("test.fst")))
system.time(for(i in 1:10) fst::write_fst(as.data.frame(d3), "test.fst"))
system.time(for(i in 1:10) data.table::as.data.table(fst::read_fst("test.fst")))
system.time(for(i in 1:10) fst::write_fst(as.data.frame(d), "test.fst"))
system.time(for(i in 1:10) data.table::as.data.table(fst::read_fst("test.fst")))


# Hadleys example:
df <- as.data.frame(matrix(runif(256*65536), nrow = 256))

print("Write RDS")
for(i in 1) print(system.time(saveRDS(df, "df.rds")))
for(i in 1:3) print(system.time(saveRDS(df, "df.rds", compress = FALSE)))
print("Write Feather")
for(i in 1:3) print(system.time(feather::write_feather(df, "df.feather")))
print("Write FST")
for(i in 1:3) print(system.time(fst::write_fst(as.data.frame(df), "df.fst")))

file.info("df.rds")$size * 1e-6
file.info("df.feather")$size * 1e-6
file.info("df.fst")$size * 1e-6


print("Read RDS")
for(i in 1:3) print(system.time(readRDS("df.rds")))
print("Read Feather")
for(i in 1:3) print(system.time(feather::read_feather("df.feather")))
print("Read FST")
for(i in 1:3) print(system.time(data.table::as.data.table(fst::read_fst("df.fst"))))


df <- f$DefineBioticAssignment

print("Write RDS")
for(i in 1) print(system.time(saveRDS(df, "df.rds")))
for(i in 1:3) print(system.time(saveRDS(df, "df.rds", compress = FALSE)))
print("Write Feather")
for(i in 1:3) print(system.time(feather::write_feather(df, "df.feather")))
print("Write FST")
for(i in 1:3) print(system.time(fst::write_fst(as.data.frame(df), "df.fst")))

file.info("df.rds")$size * 1e-6
file.info("df.feather")$size * 1e-6
file.info("df.fst")$size * 1e-6


print("Read RDS")
for(i in 1:3) print(system.time(readRDS("df.rds")))
print("Read Feather")
for(i in 1:3) print(system.time(feather::read_feather("df.feather")))
print("Read FST")
for(i in 1:3) print(system.time(data.table::as.data.table(fst::read_fst("df.fst"))))




# Test speed of writeRDS vs fst::write_fst(as.data.frame(x), path = filePath):
string <- "/process/projectSession/memory/models/baseline/P006/functionName/functionName_20200430T171700.433Z.rds"
string6 <- rep(string, 6)

system.time(
	for(i in seq_len(1e4)) {
		saveRDS(string, "test.rds")
	}
)

system.time(
	for(i in seq_len(1e4)) {
		readRDS("test.rds")
	}
)

system.time(
	for(i in seq_len(1e4 / 6)) {
		saveRDS(string6, "test.rds")
	}
)

system.time(
	for(i in seq_len(1e4 / 6)) {
		readRDS("test.rds")
	}
)



system.time(
	for(i in seq_len(1e4)) {
		writeChar(string, "test.rds")
	}
)

system.time(
	for(i in seq_len(1e4)) {
		readChar("test.rds", 200)
	}
)

system.time(
	for(i in seq_len(1e4 / 6)) {
		writeChar(string6, "test.rds")
	}
)

system.time(
	for(i in seq_len(1e4 / 6)) {
		readChar("test.rds", 200 * 6)
	}
)


system.time(
	for(i in seq_len(1e4)) {
		data.table::fwrite(list(string), "test.rds")
	}
)

system.time(
	for(i in seq_len(1e4)) {
		readChar("test.rds", 200)
	}
)

system.time(
	for(i in seq_len(1e4 / 6)) {
		writeChar(string6, "test.rds")
	}
)

system.time(
	for(i in seq_len(1e4 / 6)) {
		readChar("test.rds", 200 * 6)
	}
)





system.time(
	for(i in seq_len(1e4)) {
		writeChar(string, "test.txt")
	}
)
system.time(
	for(i in seq_len(1e4)) {
		scan("test.txt", what = "character",  quiet = TRUE, encoding = "UTF-8", skipNul = TRUETRUE)
	}
)
system.time(
	for(i in seq_len(1e4 / 6)) {
		writeChar(string6, "test.txt")
	}
)
system.time(
	for(i in seq_len(1e4 / 6)) {
		scan("test.txt", what = "character",  quiet = TRUE, encoding = "UTF-8", skipNul = TRUE)
	}
)


system.time(
	for(i in seq_len(1e4)) {
		readChar("test.txt", 200)
	}
)


system.time(
	for(i in seq_len(1e4)) {
		f <- file( "test.txt")
		close(f)
	}
)















# Test RstoxAPI 1.1.9:


library(RstoxAPI)
options(deparse.max.lines = 10)

# Run the cod project of 2019:
projectPath_SweptArea <- "~/workspace/stox/project/Example_BS_swept_area_cod_2019"
modelName <- "baseline"
# Open the project:
system.time(openProject(projectPath_SweptArea))
# Run the project:
system.time(output_SweptArea <- runModel(projectPath_SweptArea, modelName, fileOutput = FALSE))
# Total estimated biomass in tonnes:
TSB_SweptArea <- sum(output_SweptArea$SuperIndividuals$Biomass * 1e-6, na.rm = TRUE) # 240808.4


# Run the sandeel project of 2020:
projectPath_AcousticTrawl <- "~/workspace/stox/project/Example_North Sea_Lesser_sandeel_2020_impute"
modelName <- "baseline"
# Open the project:
system.time(openProject(projectPath_AcousticTrawl))
# Run the project:
system.time(output_AcousticTrawl <- runModel(projectPath_AcousticTrawl, modelName, fileOutput = FALSE))
# Total estimated biomass in tonnes:
TSB_AcousticTrawl <- sum(output_AcousticTrawl$SuperIndividuals$Biomass * 1e-6, na.rm = TRUE) # 664411.5



# Compare to the StoX 2.7 projects:
library(Rstox)

# Inside the IRM firewall, get the official estimate:
#projectPath_SweptArea2.7 <- getNMDdata("Barents Sea Northeast Arctic cod bottom trawl index in winter", subset = "2019", ow = TRUE)
# Run the project:
projectPath_SweptArea2.7 <- "~/workspace/stox/project/Barents Sea Northeast Arctic cod bottom trawl index in winter_2019"
system.time(output_SweptArea2.7 <- getBaseline(projectPath_SweptArea2.7))
# Total estimated biomass in tonnes:
TSB_SweptArea2.7 <- sum(output_SweptArea2.7$outputData$SuperIndAbundance$Abundance * output_SweptArea2.7$outputData$SuperIndAbundance$IndividualWeightGram * 1e-6, na.rm = TRUE) # 248810.2


# Inside the IMR firewall, get the official estimate:
#projectPath_AcousticTrawl2.7 <- getNMDdata("North Sea NOR lesser sandeel acoustic abundance estimate in spring", subset = "2020", ow = TRUE)
# Run the project:
projectPath_AcousticTrawl2.7 <- "~/workspace/stox/project/North Sea NOR lesser sandeel acoustic abundance estimate in spring_2020"
system.time(output_AcousticTrawl2.7 <- getBaseline(projectPath_AcousticTrawl2.7))
# Total estimated biomass in tonnes:
TSB_AcousticTrawl2.7 <- sum(output_AcousticTrawl2.7$outputData$SuperIndAbundance$Abundance * output_AcousticTrawl2.7$outputData$SuperIndAbundance$IndividualWeightGram * 1e-6, na.rm = TRUE) # 664209.5





# The estimates from StoX 2.7 to 2.9.13 differ in the decimal:
TSB <- data.table::data.table(
	StoX = c("2.7", "2.9.13", "relative difference"), 
	TSB_SweptArea = c(TSB_SweptArea2.7, TSB_SweptArea, (TSB_SweptArea2.7 - TSB_SweptArea) / TSB_SweptArea2.7), 
	TSB_AcousticTrawl = c(TSB_AcousticTrawl2.7, TSB_AcousticTrawl, (TSB_AcousticTrawl2.7 - TSB_AcousticTrawl) / TSB_AcousticTrawl2.7)
)
TSB

# Correct:
#StoX TSB_SweptArea TSB_AcousticTrawl
#1:                 2.7  2.531620e+05      6.642095e+05
#2:              2.9.13  2.451803e+05      6.641215e+05
#3: relative difference  3.152801e-02      1.325416e-04


# Wrong?
# StoX TSB_SweptArea TSB_AcousticTrawl
# 1:                 2.7  2.531620e+05      6.642095e+05
# 2:              2.9.13  2.872358e+05      6.749699e+05
# 3: relative difference -1.345928e-01     -1.620029e-02




# Test RstoxAPI 1.1.9:

library(RstoxAPI)

# Run the cod project of 2019:
projectPath_SweptArea <- "~/workspace/stox/project/Example_SpeciesCategoryCatch"
modelName <- "baseline"
# Open the project:
system.time(openProject(projectPath_SweptArea))
# Run the project:
system.time(output_SweptArea <- runModel(projectPath_SweptArea, modelName))



pr <- "~/workspace/stox/project/Example_North Sea_Lesser_sandeel_2020_impute test PSU"
modelName <- "baseline"
system.time(a <- runModel(pr, modelName, 1, 6, force.restart = TRUE))





# Test bootstrap:
library(RstoxAPI)

# Run the sandeel project of 2020:
projectPath_AcousticTrawl <- "~/workspace/stox/project/Example_North Sea_Lesser_sandeel_2020"
modelName <- "baseline"
# Open the project:
system.time(openProject(projectPath_AcousticTrawl))
# Run the project:
system.time(output_AcousticTrawl <- runModel(projectPath_AcousticTrawl, modelName, fileOutput = FALSE))

system.time(processTable <- getProcessTable(projectPath_AcousticTrawl, modelName))

b <- Bootstrap(projectPath_AcousticTrawl, BootstrapMethodTable <- data.table::data.table(
	ProcessName = c(
		"LengthDistribution", 
		"NASC"
	), 
	ResampleFunction = c(
		"ResampleHauls", 
		"ResampleEDSUs"
	), 
	ResampleBy = c(
		"Stratum", 
		"Stratum"
	), 
	Seed = c(
		1, 
		2
	)
), NumberOfBootstraps = 2,  NumberOfCores = 1)














projectPath <- "~/workspace/stox/project/Mackerel2020_test3.0"
modelName <- "baseline"
# Open the project:
system.time(openProject(projectPath))
system.time(f <- runModel(projectPath, modelName, fileOutput = TRUE))







library(Rstox)

copyStratumPolygonFromStoX2.7 <- function(projectName, file = file.path(getProjectPaths(projectName)$outputDir, "StratumPolygon.txt")) {
	if(file.exists(projectName)) {
		processData <- getBaseline(pr, proc = FALSE, input = "proc")
		stratumPolygon <- processData$stratumpolygon
		stratumPolygon <- stratumPolygon[names(stratumPolygon) != "IncludeInTotal"]
		write.table(stratumPolygon, file = file, col.names = FALSE, row.names = FALSE, sep = "\t")
	}
	else {
		stop("projectName and file must be given and valid")
	}
}



addAcousticPSUFromStoX2.7 <- function(projectName, projectName2.7) {
	AcousticPSU <- getAcousticPSUFromStoX2.7(projectName2.7)
	addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processData = AcousticPSU, 
			functionParameters = list(
				useProcessData = TRUE
			)
		)
	)
}


getAcousticPSUFromStoX2.7 <- function(projectName2.7) {
	if(file.exists(projectName2.7)) {
		processData <- getBaseline(projectName2.7, proc = FALSE, input = "proc")
		EDSU <- strsplit(processData$edsupsu$EDSU, "/")
		
		EDSU_StoX2.7ToStoX3 <- function(x) {
			paste(
				x[1], 
				strftime(as.POSIXct(paste0(x[3], x[4]), format='%Y-%m-%d%H:%M:%OS', tz='GMT'), format='%Y-%m-%dT%H:%M:%OS3Z'), 
				sep = "/"
			)
		}
		
		EDSU <- sapply(EDSU, EDSU_StoX2.7ToStoX3)
		
		EDSUPSU <- data.table::data.table(
			EDSU = EDSU, 
			PSU = processData$edsupsu$PSU
		)
		
		StratumPSU <- data.table::as.data.table(
			rev(processData$psustratum)
		)
		
		out <- list(
			Stratum_PSU = Stratum_PSU, 
			EDSUPSU = EDSUPSU
		)
		
		return(out)
	}
	else {
		stop("projectName and file must be given and valid")
	}
}




projectName  <- 
projectName2.7 <- 
addAcousticPSUFromStoX2.7(
	projectName = projectName, 
	projectName2.7 = projectName2.7
)



XML::xmlToList(node, addAttributes = TRUE, simplify = FALSE)


projectXML <- "~/workspace/stox/project/Arne/IESNS2020_ns_herring/process/project.xml"








s <- strsplit(processData$edsupsu$EDSU, "/")




pr <- "~/workspace/stox/project/Arne/IESNS2020_ns_herring"
d <- copyStratumPolygonFromStoX2.7(pr)

pr <- "~/workspace/stox/project/Arne/2019"
d <- copyStratumPolygonFromStoX2.7(pr)

pr <- "~/workspace/stox/project/Arne/2018"
d <- copyStratumPolygonFromStoX2.7(pr)

pr <- "~/workspace/stox/project/Arne/2017"
d <- copyStratumPolygonFromStoX2.7(pr)




library(RstoxAPI)

# Run the sandeel project of 2020:
projectPath_2020 <- "~/Projects/SONAR/Echosounder_MS70_FisherySonar_StoX3.0/StoX/IESNS_Only_G.O.Sars_2020"
modelName <- "baseline"
# Open the project:
system.time(openProject(projectPath_2020))
# Run the project:
system.time(output <- runModel(projectPath_2020, modelName, fileOutput = FALSE))


system.time(output <- runModel(projectPath_2020, modelName = "analysis", fileOutput = FALSE))



system.time(output <- runModel("~/Projects/SONAR/Echosounder_MS70_FisherySonar_StoX3.0/StoX/IESNS_Only_G.O.Sars_2020", modelName = "analysis", fileOutput = FALSE))





backwardsCompability_tableParameters <- function(projectPath) {
	projectDescription <-  readProjectDescription(projectPath)
	
	toRename <- c("RedefinitionTable", "TranslationTable", "ConversionTable", "LengthExponentTable", "TargetStrengthDefinitionTable", "SpeciesLinkTable", "SweepWidthTable", "VariableReplacementTable")
	
	renameTableParameters <- function(process, toRename) {
		atTableParameters <- which(names(process$functionParameters) %in% toRename)
		#print(atTableParameters)
		if(length(atTableParameters)) {
			for(at in atTableParameters) {
				toReplaceWith <- substr(
					names(process$functionParameters)[at], 
					1, 
					nchar(names(process$functionParameters)[at]) - 5
				)
				#print(process$functionParameters)
				#print(toReplaceWith)
				
				names(process$functionParameters)[at] <- toReplaceWith
			}
		}
		return(process)
	}
	
	projectDescription$baseline <- lapply(projectDescription$baseline, renameTableParameters, toRename = toRename)
	projectDescriptionFile <- getProjectPaths(projectPath, paste0("project", "RData", "File"))
	
	RstoxFramework:::writeProjectDescriptionRData(
		projectDescription = projectDescription, 
		projectDescriptionFile = projectDescriptionFile
	)
}




projectPath <- "~/workspace/stox/project/Example_BS_swept_area_cod_2019"
backwardsCompability_tableParameters(projectPath)

projectPath <- "~/workspace/stox/project/Example_North Sea_Lesser_sandeel_2020_impute"
backwardsCompability_tableParameters(projectPath)

projectPath <- "~/workspace/stox/project/Example_SpeciesCategoryCatch"
backwardsCompability_tableParameters(projectPath)

projectPath <- "~/workspace/stox/project/Test_Rstox3"
backwardsCompability_tableParameters(projectPath)

projectPath <- "~/Projects/SONAR/Echosounder_MS70_FisherySonar_StoX3.0/StoX/IESNS_Only_G.O.Sars_2020"
backwardsCompability_tableParameters(projectPath)







renameParameters <- function(process, parameterRenaming) {
	atRename <- which(names(process$functionParameters) %in% parameterRenaming$old)
	#print(atTableParameters)
	if(length(atRename)) {
		for(at in atRename) {
			renameWith <- parameterRenaming[old == names(process$functionParameters)[at], "new"]
			names(process$functionParameters)[at] <- renameWith
		}
	}
	return(process)
}


backwardsCompability_renameParameters <- function(projectPath) {
	
	parameterRenaming <- data.table::data.table(
		old = c(
			"RedefinitionTable", 
			"TranslationTable", 
			"ConversionTable",
			"LengthExponentTable",
			"TargetStrengthDefinitionTable",
			"SpeciesLinkTable",
			"SweepWidthTable",
			"VariableReplacementTable", 
			"VariableName"
		),
		new = c(
			"Redefinition", 
			"Translation", 
			"Conversion",
			"LengthExponent",
			"TargetStrengthDefinition",
			"SpeciesLink",
			"SweepWidth",
			"VariableReplacement", 
			"VariableNames"
		)
	)
	
	browser()
	# Get the project description:
	projectDescription <-  readProjectDescription(projectPath)
	
	# Rename the parameters:
	projectDescription$baseline <- lapply(projectDescription$baseline, renameParameters, parameterRenaming = parameterRenaming)
	
	# Write the project description:
	projectDescriptionFile <- getProjectPaths(projectPath, paste0("project", "RData", "File"))
	RstoxFramework:::writeProjectDescriptionRData(
		projectDescription = projectDescription, 
		projectDescriptionFile = projectDescriptionFile
	)
}


projectPath <- "~/workspace/stox/project/Example_SpeciesCategoryCatch"
backwardsCompability_renameParameters(projectPath)









library(RstoxAPI)
projectPath <- "~/Projects/SONAR/Echosounder_MS70_FisherySonar_StoX3.0/StoX/IESNS_Only_G.O.Sars_2020"
openProject(projectPath)
b <- runModel(projectPath, "baseline")

RstoxFramework::saveProject(projectPath, "JSON", force = TRUE)
projectDescription <- getProjectMemoryData(projectPath, named.list = TRUE)

project.json <- RstoxFramework::getProjectPaths(projectPath)$projectJSONFile
r <- RstoxFramework:::readProjectDescriptionJSON(project.json)

for(i in seq_along(r$baseline)) {print(i); print(all.equal(r$baseline[[i]], projectDescription$baseline[[i]]))}

a <- runModel(projectPath, "analysis")





library(RstoxAPI)
projectPath <- "~/Projects/SONAR/Echosounder_MS70_FisherySonar_StoX3.0/StoX/IESNS_Only_G.O.Sars_2020_clean"
b <- runModel(projectPath, "baseline")

RstoxFramework::saveProject(projectPath, "JSON", force = TRUE)
projectDescription <- getProjectMemoryData(projectPath, named.list = TRUE)

project.json <- RstoxFramework::getProjectPaths(projectPath)$projectJSONFile
r <- RstoxFramework:::readProjectDescriptionJSON(project.json)

for(i in seq_along(r$baseline)) {print(i); print(all.equal(r$baseline[[i]], projectDescription$baseline[[i]]))}

a <- runModel(projectPath, "analysis")




library(RstoxAPI)
projectPath <- "~/Projects/SONAR/Echosounder_MS70_FisherySonar_StoX3.0/StoX/IESNS_Only_G.O.Sars_2020_clean_HighestResolution"
b <- runModel(projectPath, "baseline")










projectPath <- "~/workspace/stox/project/Example_North Sea_Lesser_sandeel_2020_impute"
openProject(projectPath)

b <- runModel(projectPath, "baseline")

RstoxFramework::saveProject(projectPath, "JSON", force = TRUE)
projectDescription <- getProjectMemoryData(projectPath, named.list = TRUE)

project.json <- RstoxFramework::getProjectPaths(projectPath)$projectJSONFile
r <- RstoxFramework:::readProjectDescriptionJSON(project.json)

for(i in seq_along(r$baseline)) {print(i); print(all.equal(r$baseline[[i]], projectDescription$baseline[[i]]))}


r$baseline[[2]]$processData
projectDescription$baseline[[2]]$processData

r$baseline[[18]]$processData
projectDescription$baseline[[18]]$processData




projectPath <- "~/workspace/stox/project/Example_BS_swept_area_cod_2019"
openProject(projectPath)
b <- runModel(projectPath, "baseline")

RstoxFramework::saveProject(projectPath, "JSON", force = TRUE)
projectDescription <- getProjectMemoryData(projectPath, named.list = TRUE)

project.json <- RstoxFramework::getProjectPaths(projectPath)$projectJSONFile
r <- RstoxFramework:::readProjectDescriptionJSON(project.json)

for(i in seq_along(r$baseline)) {print(i); print(all.equal(r$baseline[[i]], projectDescription$baseline[[i]]))}




projectPath <- "~/workspace/stox/project/Example_SpeciesCategoryCatch"
openProject(projectPath)
b <- runModel(projectPath, "baseline")

RstoxFramework::saveProject(projectPath, "JSON", force = TRUE)
projectDescription <- getProjectMemoryData(projectPath, named.list = TRUE)

project.json <- RstoxFramework::getProjectPaths(projectPath)$projectJSONFile
r <- RstoxFramework:::readProjectDescriptionJSON(project.json)

for(i in seq_along(r$baseline)) {print(i); print(all.equal(r$baseline[[i]], projectDescription$baseline[[i]]))}


projectPath <- "~/workspace/stox/project/Example_ICESAcoustic"
b <- runModel(projectPath, "baseline", 1, 2)




# Test of parseParameter:
objects <- list(
	NULL, 
	"", 
	numeric(), 
	character(), 
	1,
	1L,
	1.1,
	"a", 
	runif(3), 
	c("a", "b"), 
	c(a = "a", b = "b"), 
	data.table::data.table(), 
	data.table::data.table(a = 1, b = 2), 
	data.table::data.table(a = 1:3, b = "2"), 
	RstoxFramework:::emptyNamedList(), 
	list(a = 1), 
	list(a = 1, b = 4), 
	list(1, 4), 
	list(a = "1", b = 1:3), 
	list(a = "1", b = list(c = runif(3)))
)

json <- lapply(objects, RstoxFramework:::toJSON_Rstox)


objects2 <- lapply(json, jsonlite::fromJSON, simplifyVector = TRUE)
objects3 <- lapply(json, RstoxFramework::parseParameter, simplifyVector = TRUE)
objects3 <- lapply(objects3, function(x) if(is.data.frame(x)) data.table::as.data.table(x) else x)

mapply(list, objects, json, objects3, objects3, SIMPLIFY = FALSE)

mapply(function(x, y) if(sum(data.table::is.data.table(x) , data.table::is.data.table(y)) == 1) identical(x, y) else all.equal(x, y), objects, objects3, SIMPLIFY = FALSE)









# Try inserting log dependent PSUs based on the echosounder data at different PSU distances:

addLogBasedAcousticPSU <- function(StoxAcousticData, PSUDistance = 10) {
    # Get log:
    logStartStop <- seq(
        floor(StoxAcousticData$Log$Log / PSUDistance) * PSUDistance, 
        ceiling(StoxAcousticData$Log$Log / PSUDistance) * PSUDistance, 
        by = PSUDistance
    )
    atPSU <- findInterval(StoxAcousticData$Log$Log, logStartStop)
    PSU <- RstoxBase::getPSUName(atPSU, RstoxBase::getRstoxBaseDefinitions("AcousticPSUPrefix"))
    
    
    list(
        Stratum_PSU = data.table::data.table(
            Stratum = 1,
            PSU = PSU
        ), 
        EDSU_PSU = data.table::data.table(
            EDSU = StoxAcousticData$Log$EDSU,
            PSU = PSU
        )
    )
}


procjectPath <- "~/Projects/SONAR/Echosounder_MS70_FisherySonar_StoX3.0/StoX/IESNS_Only_G.O.Sars_2020_clean_HighestResolution_toMeanNASC"
d <- runModel(procjectPath, modelName = "baseline", replaceDataList = list(DefineAcousticPSU_EK60 = addLogBasedAcousticPSU))














BootstrapMethodTable1 <- data.table::data.table(
    ProcessName = c("MeanNASC", "BioticAssignmentWeighting"), 
    ResampleFunction = c("ResampleMeanNASCData", "ResampleBioticAssignment"),
    Seed = c(1, 2)
)
BootstrapMethodTable2 <- BootstrapMethodTable1
BootstrapMethodTable2[2, 1] <-"DefineBioticAssignment"

e1 <- runModel(
    projectPath="/Users/arnejh/Code/Github/StoX/Releases/2.9.16/TestProjects/Example_North Sea_Lesser_sandeel_2020_impute_2.9.16",
    modelName="analysis", 
    replaceArgsList = list(Bootstrap = list(BootstrapMethodTable = BootstrapMethodTable1))
)

e2 <- runModel(
    projectPath="/Users/arnejh/Code/Github/StoX/Releases/2.9.16/TestProjects/Example_North Sea_Lesser_sandeel_2020_impute_2.9.16",
    modelName="analysis", 
    replaceArgsList = list(Bootstrap = list(BootstrapMethodTable = BootstrapMethodTable2))
)





e1 <- getModelData(
    projectPath="/Users/arnejh/Code/Github/StoX/Releases/2.9.16/TestProjects/Example_North Sea_Lesser_sandeel_2020_impute_2.9.16",
    modelName="analysis", 1, 1)

e2 <- getModelData(
    projectPath="/Users/arnejh/Code/Github/StoX/Releases/2.9.16/TestProjects/Example_North Sea_Lesser_sandeel_2020_impute_2.9.16",
    modelName="analysis", 1, 1)





f1 <- runModel(
    projectPath="/Users/arnejh/Code/Github/StoX/Releases/2.9.16/TestProjects/Example_North Sea_Lesser_sandeel_2020_impute_2.9.16",
    modelName="analysis", 1, 1)

f2 <- runModel(
    projectPath="/Users/arnejh/Code/Github/StoX/Releases/2.9.16/TestProjects/Example_North Sea_Lesser_sandeel_2020_impute_2.9.16",
    modelName="analysis", 1, 1)







##### 2020-11-11: Test replaceArgs: #####

library(RstoxFramework)
projectPath="~/Projects/SONAR/Echosounder_MS70_FisherySonar_StoX3.0/StoX/IESNS_Only_G.O.Sars_2020_clean_HighestResolution_toMeanNASC"

d <- runModel(projectPath, modelName="baseline")
d10 <- runModel(projectPath, modelName="baseline", replaceArgs = list(DefineAcousticPSU_EK60 = list(UseProcessData = FALSE, Interval = 10)))










projectPath <- "~/Projects/SONAR/Echosounder_MS70_FisherySonar_StoX3.0/StoX/IESNS_Only_G.O.Sars_2020_clean_HighestResolution"
b <- runModel(projectPath, "baseline")
































# Test RstoxAPI 1.1.9:


library(RstoxFramework)
options(deparse.max.lines = 10)

# Run the cod project of 2019:
projectPath_SweptArea <- "~/Code/Github/StoX/Releases/2.9.19/TestProjects/Example_BS_swept_area_cod_2019_2.9.19"
modelName <- "baseline"
# Open the project:
system.time(openProject(projectPath_SweptArea))
# Run the project:
system.time(output_SweptArea <- runModel(projectPath_SweptArea, modelName, fileOutput = FALSE))
# Total estimated biomass in tonnes:
TSB_SweptArea <- sum(output_SweptArea$SuperIndividuals$Biomass * 1e-6, na.rm = TRUE) # 240808.4


# Run the sandeel project of 2020:
projectPath_AcousticTrawl <- "~/Code/Github/StoX/Releases/2.9.19/TestProjects/Example_North Sea_Lesser_sandeel_2020_impute_2.9.19"
modelName <- "baseline"
# Open the project:
system.time(openProject(projectPath_AcousticTrawl))
# Run the project:
system.time(output_AcousticTrawl <- runModel(projectPath_AcousticTrawl, modelName, fileOutput = FALSE))
# Total estimated biomass in tonnes:
TSB_AcousticTrawl <- sum(output_AcousticTrawl$SuperIndividuals$Biomass * 1e-6, na.rm = TRUE) # 664411.5



# Compare to the StoX 2.7 projects:
library(Rstox)

# Inside the IRM firewall, get the official estimate:
#projectPath_SweptArea2.7 <- getNMDdata("Barents Sea Northeast Arctic cod bottom trawl index in winter", subset = "2019", ow = TRUE)
# Run the project:
projectPath_SweptArea2.7 <- "~/workspace/stox/project/Barents Sea Northeast Arctic cod bottom trawl index in winter_2019"
system.time(output_SweptArea2.7 <- getBaseline(projectPath_SweptArea2.7))
# Total estimated biomass in tonnes:
TSB_SweptArea2.7 <- sum(output_SweptArea2.7$outputData$SuperIndAbundance$Abundance * output_SweptArea2.7$outputData$SuperIndAbundance$IndividualWeightGram * 1e-6, na.rm = TRUE) # 248810.2


# Inside the IMR firewall, get the official estimate:
#projectPath_AcousticTrawl2.7 <- getNMDdata("North Sea NOR lesser sandeel acoustic abundance estimate in spring", subset = "2020", ow = TRUE)
# Run the project:
projectPath_AcousticTrawl2.7 <- "~/workspace/stox/project/North Sea NOR lesser sandeel acoustic abundance estimate in spring_2020"
system.time(output_AcousticTrawl2.7 <- getBaseline(projectPath_AcousticTrawl2.7))
# Total estimated biomass in tonnes:
TSB_AcousticTrawl2.7 <- sum(output_AcousticTrawl2.7$outputData$SuperIndAbundance$Abundance * output_AcousticTrawl2.7$outputData$SuperIndAbundance$IndividualWeightGram * 1e-6, na.rm = TRUE) # 664209.5





# The estimates from StoX 2.7 to 2.9.13 differ in the decimal:
TSB <- data.table::data.table(
	StoX = c("2.7", "2.9.13", "relative difference"), 
	TSB_SweptArea = c(TSB_SweptArea2.7, TSB_SweptArea, (TSB_SweptArea2.7 - TSB_SweptArea) / TSB_SweptArea2.7), 
	TSB_AcousticTrawl = c(TSB_AcousticTrawl2.7, TSB_AcousticTrawl, (TSB_AcousticTrawl2.7 - TSB_AcousticTrawl) / TSB_AcousticTrawl2.7)
)
TSB

# Correct:
#StoX TSB_SweptArea TSB_AcousticTrawl
#1:                 2.7  2.531620e+05      6.642095e+05
#2:              2.9.13  2.451803e+05      6.641215e+05
#3: relative difference  3.152801e-02      1.325416e-04


# Wrong?
# StoX TSB_SweptArea TSB_AcousticTrawl
# 1:                 2.7  2.531620e+05      6.642095e+05
# 2:              2.9.13  2.872358e+05      6.749699e+05
# 3: relative difference -1.345928e-01     -1.620029e-02







