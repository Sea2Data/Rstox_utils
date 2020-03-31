library(RstoxFramework)
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
	)
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
			FilterExpression = list("biotic_cruiseNumber_2017838_Eros_2019-02-19T08.33.14.905Z.xml/individual" = "length > 0.02")
		)
	)
)

# Add process 2, StoxBiotic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "StoxBiotic", 
		functionName = "StoxBiotic", 
		functionInputs = list(
			BioticData = "ReadBiotic"
		), 
		functionParameters = list(
			cores = 1
		)
	)
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
			FilterExpression = list(SpeciesCategory = "SpeciesCategory == \"sild'G03/161722.G03/126417/NA\"")
		)
	)
)


# Add process 3, DefineStrata:
inputFileNames <- list.files(inputresourceProjectPath, full.names = TRUE)
file.copy(
	inputFileNames, 
	inputFile(inputFileNames, projectPath = projectPath, folder = "input")
)
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineStrata", 
		functionName = "DefineStrata", 
		functionParameters = list(
			FileName = inputFile(inputFileNames, projectPath = NULL, folder = "input")
		)
	)
)

# Add process 4, StratumArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "StratumArea", 
		functionName = "StratumArea", 
		functionInputs = list(
			StratumPolygon = "DefineStrata"
		)
	)
)


# Add process 4, StratumArea:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "StratumAreaSimple", 
		functionName = "StratumArea", 
		functionInputs = list(
			StratumPolygon = "DefineStrata"
		), 
		functionParameters = list(
			AreaMethod = "Simple"
		)
	)
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
	)
)

# Add process 24, FilterAcoustic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "FilterAcoustic", 
		functionName = "FilterAcoustic", 
		functionInputs = list(
			AcousticData = "ReadAcoustic"
		), 
		functionParameters = list(
			FilterExpression = list("echosounder_cruiseNumber_2017838_Eros.xml/ch_type" = "type == \"P\"")
		)
	)
)


# Add process 6, StoxAcoustic:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "StoxAcoustic", 
		functionName = "StoxAcoustic", 
		functionInputs = list(
			AcousticData = "ReadAcoustic"
		)
	)
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
				ChannelReference = "ChannelReferenceType == \"P\""
			)
		)
	)
)


# # Add process 6b, MergeStoxAcoustic:
# temp <- addProcess(
# 	projectPath = projectPath, 
# 	modelName = "baseline", 
# 	values = list(
# 		processName = "MergeStoxAcoustic", 
# 		functionName = "MergeStoxAcoustic", 
# 		functionInputs = list(
# 			StoxAcousticData = "StoxAcoustic"
# 		)
# 	)
# )

## Add process 6c, MergeStoxBiotic:
#temp <- addProcess(
#	projectPath = projectPath, 
#	modelName = "baseline", 
#	values = list(
#		processName = "MergeStoxBiotic", 
#		functionName = "MergeStoxBiotic", 
#		functionInputs = list(
#			StoxBioticData = "StoxBiotic"
#		)
#	)
#)

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
			StratumPolygon = "DefineStrata", 
			StoxAcousticData = "StoxAcoustic"
		)
	)
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
			DefinitionMethod = "WaterColumn"
		)
	)
)

# Add process 9, DefineSweptAreaPSU:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineSweptAreaPSU", 
		functionName = "DefineSweptAreaPSU", 
		functionInputs = list(
			StratumPolygon = "DefineStrata", 
			StoxBioticData = "FilterStoxBiotic"
		)
	)
)

# Add process 10, DefineSweptAreaLayer:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineSweptAreaLayer", 
		functionName = "DefineSweptAreaLayer", 
		functionInputs = list(
			StoxBioticData = "FilterStoxBiotic"
		), 
		functionParameters = list(
			#DefinitionMethod = "WaterColumn"
			DefinitionMethod = "HighestResolution"
		)
	)
)


# Add process 11, LengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "LengthDistribution", 
		functionName = "LengthDistribution", 
		functionInputs = list(
			StoxBioticData = "FilterStoxBiotic", 
			SweptAreaPSU = "DefineSweptAreaPSU", 
			SweptAreaLayer = "DefineSweptAreaLayer"
		), 
		functionParameters = list(
			LengthDistributionType = "Normalized"
		)
	)
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
			LengthInterval = 5
		)
	)
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
			LengthDependentSweepWidthParameters = data.table::data.table(
				SpeciesCategory = c(
					"sild'G03/161722.G03/126417/NA", 
					"laksesild/162187/127312/NA"
				), 
				Alpha = 5.91, 
				Beta = 0.43, 
				LMin = c(15, 14), 
				LMax = 62
			)
		)
	)
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
		)
	)
)

# Add process 15, MeanLengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "MeanLengthDistribution", 
		functionName = "MeanLengthDistribution", 
		functionInputs = list(
			LengthDistributionData = "SumLengthDistribution"
		)
	)
)

# Add process 14, SumLengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "SumLengthDistributionCompensated", 
		functionName = "SumLengthDistribution", 
		functionInputs = list(
			LengthDistributionData = "LengthDependentCatchCompensation"
		)
	)
)

# Add process 15, MeanLengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "MeanLengthDistributionCompensated", 
		functionName = "MeanLengthDistribution", 
		functionInputs = list(
			LengthDistributionData = "SumLengthDistributionCompensated"
		)
	)
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
	)
)


# Add process 17, NASC:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "NASC", 
		functionName = "NASC", 
		functionInputs = list(
			StoxAcousticData = "FilterStoxAcoustic", 
			AcousticLayer = "DefineAcousticLayer", 
			AcousticPSU = "DefineAcousticPSU"
		)
	)
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
			TargetResolution = "Layer"
		)
	)
)


# Add process 19, MeanNASC:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "MeanNASC", 
		functionName = "MeanNASC", 
		functionInputs = list(
			NASCData = "SumNASC"
		), 
		functionParameters = list(
			TargetResolution = "Stratum"
		)
	)
)

# Add process 20, DefineAcousticTargetStrength:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineAcousticTargetStrength", 
		functionName = "DefineAcousticTargetStrength", 
		functionParameters = list(
			ParameterTable = data.table::data.table(
				AcousticCategory = "12", 
				Frequency = 38000, 
				m = 20, 
				a = -71.9, 
				d = 0
			)
		)
	)
)


# Add process 20, SweptAreaDensity:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "SweptAreaDensityPreDefined", 
		functionName = "SweptAreaDensity", 
		functionInputs = list(
			LengthDistributionData = "MeanLengthDistributionCompensated"
		), 
		functionParameters = list(
			SweepWidthMethod = "PreDefined"
		)
	)
)


# Add process 20, SweptAreaDensity:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "SweptAreaDensityConstant", 
		functionName = "SweptAreaDensity", 
		functionInputs = list(
			LengthDistributionData = "MeanLengthDistribution"
		), 
		functionParameters = list(
			SweepWidthMethod = "Constant", 
			SweepWidth = 25
		)
	)
)

# Add process 21, DefineBioticAssignment:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "DefineBioticAssignment", 
		functionName = "DefineBioticAssignment", 
		functionInputs = list(
			NASCData = "NASC", 
			StoxAcousticData = "StoxAcoustic", 
			AcousticPSU = "DefineAcousticPSU", 
			StoxBioticData = "FilterStoxBiotic", 
			StratumPolygon = "DefineStrata"
		)
	)
)


#### Add process 22, AssignmentLengthDistribution:
###temp <- addProcess(
###	projectPath = projectPath, 
###	modelName = "baseline", 
###	values = list(
###		processName = "AssignmentLengthDistribution", 
###		functionName = "AssignmentLengthDistribution", 
###		functionInputs = list(
###			LengthDistributionData = "LengthDependentCatchCompensation", 
###			BioticAssignment = "DefineBioticAssignment"
###		)
###	)
###)


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
	)
)

# Add process 24, Abundance:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "Abundance", 
		functionName = "Abundance", 
		functionInputs = list(
			DensityData = "MeanDensity", 
			StratumArea = "StratumArea"
		)
	)
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
			DensityType = "Acoustic"
		)
	)
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
			LengthDistributionData = "LengthDependentCatchCompensation"
		), 
		functionParameters = list(
			DensityType = "SweptArea"
		)
	)
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
			AbundWeightMethod = "Equal"
		)
	)
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
			AbundWeightMethod = "HaulDensity"
		)
	)
)



#temp <- addProcess(
#	projectPath = projectPath, 
#	modelName = "baseline", 
#	values = list(
#		processName = "PrepareRecaEstimate2", 
#		functionName = "RstoxFDA::PrepareRecaEstimate", 
#		functionInputs = list(
#			StoxBioticData = "StoxBiotic", 
#			StoxLandingData = "StoxBiotic"
#		)
#	)
#)




# Add process 18, BioticStationAssignment:
#temp <- addProcess(
#	projectPath = projectPath, 
#	modelName = "baseline", 
#	values = list(
#		processName = "DefineBioticAssignment", 
#		functionName = "DefineBioticAssignment", 
#		functionInputs = list(
#			NASCData = "NASC", 
#			StoxBioticData = "StoxBiotic", 
#			StratumPolygon = "DefineStrata"
#		), 
#		functionParameters = list(
#			DefinitionMethod = "Stratum"
#		)
#	)
#)



##### End of test project: #####

stoxLibrary <- getRstoxFrameworkDefinitions("stoxLibrary")
names(stoxLibrary)
getAvailableStoxFunctionNames("baseline")
getAvailableStoxFunctionNames("analysis")
getAvailableStoxFunctionNames("report")

projectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "baseline"
system.time(processTable <- getProcessTable(projectPath, modelName))
#system.time(processTable2 <- scanForModelError(projectPath, modelName))
#system.time(processTable2 <- RstoxFramework:::getProcessesSansProcessData(projectPath, modelName))

#RstoxFramework::saveProject(projectPath, "JSON")
#RstoxFramework::saveProject(projectPath)

system.time(f <- runModel(projectPath, modelName))

RstoxFramework::saveProject(projectPath, "JSON")


getFunctionHelpAsHtml(projectPath, modelName, "P001")







system.time(f <- runModel(projectPath, modelName, startProcess = 1, endProcess = 17))
system.time(f <- runModel(projectPath, modelName, endProcess = 1))


system.time(closeProject(projectPath = projectPath))
system.time(openProject(projectPath = projectPath))

addProcess(projectPath, modelName)
RstoxFramework:::modifyFunctionName(projectPath, modelName, "P034", "StoXBiotic")
removeProcess(projectPath, modelName, "P033")

RstoxFramework:::getFunctionName(
	projectPath = projectPath, 
	modelName = modelName, 
	processID = "P033"
)

removeProcess(projectPath, modelName, processID = "P005")


rearrangeProcesses(projectPath, modelName, c("P003"), "P002")


resetModel(projectPath, modelName, "P003")
resetModel(projectPath, modelName, "P073")

resetModel(projectPath, modelName)
resetModel(projectPath, modelName)


RstoxAPI::runFunction("getModelInfo", args = list())

runFunction("getAvailableTemplatesDescriptions", args = list())
jsonlite::toJSON(runFunction("getActiveProcessID", args = list(projectPath = projectPath, modelName = "baseline")), auto_unbox = TRUE, pretty = TRUE)




setProcessMemoryNew(projectPath, modelName, "P001", "processName", "ddddd")
setProcessMemoryNew(projectPath, modelName, "P001", "functionParameters", list(FileNames ="asdfas"))

RstoxFramework:::getArgumentFilePaths(projectPath, modelName = NULL, processID = NULL, argumentName = NULL)


	
	



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


RstoxFramework:::modifyFunctionName(projectPath, modelName, "P015", "")

addEmptyProcess(projectPath, modelName, processName = NULL)


# Set file names
setProcessPropertyValue("functionParameters", "FileNames", c("~/workspace/stox/project/Test_Rstox3/process/projectSession/projectMemory/current/maxProcessIntegerID.txt", "~/workspace/stox/project/Test_Rstox3/process/projectSession/projectMemory/current/processIndexTable.txt"), projectPath, modelName, "P001")

getProcessPropertySheet(projectPath, modelName, processID = "P006")


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


g1 <- getFilterOptions("~/workspace/stox/project/Test_Rstox3", "baseline", "P002", "Individual")
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
system.time(gg <- getMapData("~/workspace/stox/project/Test_Rstox3", "baseline", "P002"))
system.time(ggg <- getMapData("~/workspace/stox/project/Test_Rstox3", "baseline", "P006"))
system.time(g4 <- getInteractiveData("~/workspace/stox/project/Test_Rstox3", "baseline", "P007"))
system.time(g5 <- getInteractiveData("~/workspace/stox/project/Test_Rstox3", "baseline", "P009"))

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
plot(processOutput$DefineStrata)
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
StratumPolygon <- processOutput$DefineStrata

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
# Funciton to add DefineStrata:
addDefineStrata <- function(projectPath, skeleton, ...) {
	inputFileNames <- skeleton$polygonFile
	file.copy(
		inputFileNames, 
		inputFile(inputFileNames, projectPath = projectPath, folder = "input")
	)
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "baseline", 
		values = list(
			processName = "DefineStrata", 
			functionName = "DefineStrata", 
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
				StratumPolygon = "DefineStrata"
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
				StratumPolygon = "DefineStrata", 
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
				StratumPolygon = "DefineStrata", 
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
createTestProject <- function(projectName, projectPathOriginal, processes = c("ReadBiotic", "StoxBiotic", "DefineStrata", "StratumArea", "DefineSweptAreaPSU", "DefineSweptAreaLayer", "LengthDistribution"), sts = NULL, year = NULL, dir = "~/workspace/stox/project", download = FALSE) {
	
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
	## Add process 3, DefineStrata:
	#addDefineStrata(projectPath = projectPath, skeleton = skeleton)
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
	processes = c("ReadBiotic", "StoxBiotic", "FilterStoxBiotic", "DefineStrata", "StratumArea", "DefineSweptAreaPSU", "DefineSweptAreaLayer", "LengthDistribution", "LengthDependentCatchCompensation", "RegroupLengthDistribution")
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
	processes = c("ReadBiotic", "StoxBiotic", "ReadAcoustic", "StoxAcoustic", "DefineStrata", "StratumArea", "DefineAcousticPSU", "DefineAcousticLayer", "LengthDistribution", "NASC")
)


system.time(f <- runModel("~/workspace/stox/project/Test_Rstox3_herring2018", modelName = "baseline", endProcess = 3))
############################################
############################################



d <- RstoxBase:::meanData(cod2019$new$LengthDistribution)




# If trouble with download limit on GitHub:
# usethis::browse_github_pat() # Generate new token, then copy it (R:GITHUB_PAT)
# usethis::edit_r_environ() # A file is opened. Paste the token to this file. Then viola!



l <- load('~/workspace/stox/project/Test_Rstox3/process/project.RData')
geo <- geojsonio::geojson_json(projectDescription$baseline$P003$processData$StratumPolygon)
p <- pretty(as.character(geo), indent = 4)


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












f <- list(
	Individual = "SpeciesCategory != \"sild'G03/161722.G03/126417/NA\"", 
	Station = "Longitude >= 62"
)

ff <- jsonlite::toJSON(f, auto_unbox = TRUE)

ff
{"Individual":"SpeciesCategory != \"sild'G03/161722.G03/126417/NA\"","Station":"Longitude >= 62"} 

as.character(ff)

"{\"Individual\":\"SpeciesCategory != \\\"sild'G03/161722.G03/126417/NA\\\"\",\"Station\":\"Longitude >= 62\"}"





system.time(r.ices <- readXmlFile("~/Code/Github/StoX/Releases/StoX 2.9.6/TestProjects/Acoustic_578-1019-2019207.xml"))
system.time(r.ices <- readXmlFile("~/Code/Github/StoX/Releases/StoX 2.9.6/TestProjects/Acoustic_578-1019-2019207.xml", stream = FALSE))

system.time(a.ices <- ReadAcoustic("~/Code/Github/StoX/Releases/StoX 2.9.6/TestProjects/Acoustic_578-1019-2019207.xml"))
system.time(A.ices <- StoxAcoustic(a.ices))

system.time(a.nmd <- ReadAcoustic("~/Code/Github/StoX/Releases/StoX 2.9.6/TestProjects/echosounder_cruiseNumber_2017838_Eros.xml"))
system.time(A.nmd <- StoxAcoustic(a.nmd))



system.time(b.ices <- ReadBiotic("~/Code/Github/StoX/Releases/StoX 2.9.6/TestProjects/Biotic_578-1019-2018207.xml"))
system.time(B.ices <- StoxBiotic(b.ices))

system.time(b.nmd <- ReadBiotic("~/Code/Github/StoX/Releases/StoX 2.9.6/TestProjects/biotic_cruiseNumber_2017838_Eros_2019-02-19T08.33.14.905Z.xml"))
system.time(B.nmd <- StoxBiotic(b.nmd))


system.time(b.nmd <- ReadBiotic("~/workspace/stox/project/StoXVerTest/StagedProjOrig/Rstox_1.8_StoXLib_1.76/2017_O-gr_Lengthbased_Selectivity_Several_species/input/biotic/4-2017-1173-56_HH.out.xml"))
system.time(B.nmd <- StoxBiotic(b.nmd))






F.nmd <- FilterStoxAcoustic(A.nmd,  FilterExpression = list(ChannelReference = "ChannelReferenceType == \"P\"", AcousticCategory = "AcousticCategory == \"12\""))
						
						
system.time(nasc.ices <- NASC(A.ices))


# Test of effect of displaced origin in stratum area calculation. The conclusion is to use indiivdual origins, since these are more appropriate for each stratum:
lat_0 <- seq(-85,85,5)
lon_0 <- seq(-20,30,1)

stratumPolygon <- f$DefineStrata
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