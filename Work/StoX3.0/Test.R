
system.time(RstoxFramework::createProject("~/workspace/stox/project/Test_Rstox copy3", template = "Test3.0", ow = TRUE))

RstoxFramework::closeProject("~/workspace/stox/project/Test_Rstox copy3")

RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox copy3")

RstoxFramework::saveProject("~/workspace/stox/project/Test_Rstox copy3")

RstoxFramework::saveAsProject("~/workspace/stox/project/Test_Rstox copy3", "~/workspace/stox/project/Test_Rstox copy4", ow = TRUE)

RstoxFramework::isOpenProject("~/workspace/stox/project/Test_Rstox copy3")
RstoxFramework::isOpenProject("~/workspace/stox/project/Test_Rstox copy4")
RstoxFramework::closeProject("~/workspace/stox/project/Test_Rstox copy4")
RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox copy4")
RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox copy3")




system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy3", "baseline", "ReadBiotic", list(functionParameters = list(FileNames = "~/workspace/stox/project/StoXVerTest_temp/unix_18.2.0/Proj/Bar_Se_Nort_Arc_co_bo_tr_in_i_wi_2017_bioticV3.0/input/biotic/biotic_cruiseNumber_2017102_G+O+Sars_2019-01-15T03.27.17.822Z.xml"))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy3", "baseline", "DefineStrata", list(functionParameters = list(FileName = c("~/workspace/stox/reference/stratum/angola_15-500M.txt")))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy3", "baseline", "StratumArea", list(functionInputs = list(StratumPolygon = "DefineStrata"))))




system.time(f1 <- runProcess("~/workspace/stox/project/Test_Rstox copy3", "baseline", "P001"))

system.time(f2 <- runProcess("~/workspace/stox/project/Test_Rstox copy3", "baseline", "P002"))

system.time(f3 <- runProcess("~/workspace/stox/project/Test_Rstox copy3", "baseline", "P003"))


system.time(f <- runModel("~/workspace/stox/project/Test_Rstox copy3", modelName = "baseline"))

system.time(f <- runFunction(runModel, list("~/workspace/stox/project/Test_Rstox copy3", modelName = "baseline")))





getProcessTable("~/workspace/stox/project/Test_Rstox copy3", "baseline")


g <- getProcessOutput("~/workspace/stox/project/Test_Rstox copy3", "baseline", "P002")



d1 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox copy3", "baseline", "P001")
d2 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox copy3", "baseline", "P002")
d3 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox copy3", "baseline", "P003")
jsonlite::toJSON(d1, pretty = TRUE)
jsonlite::toJSON(d2, pretty = TRUE)
jsonlite::toJSON(d3, pretty = TRUE)
jsonlite::toJSON(d2, pretty = TRUE, auto_unbox = TRUE)
jsonlite::toJSON(d1, pretty = TRUE, auto_unbox = TRUE)
jsonlite::toJSON(d3, pretty = TRUE, auto_unbox = TRUE)


d3 <- setProcessPropertyValue(groupName = "processArguments", name = "functionName", value = "ReadBiotic", "~/workspace/stox/project/Test_Rstox copy3", "baseline", "P003")

d3 <- setProcessPropertyValue(groupName = "processArguments", name = "functionName", value = "DefineStrata", "~/workspace/stox/project/Test_Rstox3", "baseline", "P003")

v <- c("a")
a <- array(v))
dt <- data.table::data.table(a)
jsonlite::toJSON(dt, auto_unbox = TRUE)

v <- c("a")
a <- array(v)
dt <- data.table::data.table(a)
is.array(dt$a)







# Create a portable project:

RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox copy2")

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy2", "baseline", "ReadBiotic", list(functionParameters = list(FileNames = "~/workspace/stox/project/Test_Rstox copy2/Input/Biotic/biotic_cruiseNumber_2017102_G+O+Sars_2019-01-15T03.27.17.822Z.xml"))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy2", "baseline", "DefineStrata", list(functionParameters = list(FileName = c("~/workspace/stox/project/Test_Rstox copy2/Input/angola_15-500M.txt")))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy2", "baseline", "StratumArea", list(functionInputs = list(StratumPolygon = "DefineStrata"))))


system.time(addProcess("~/workspace/stox/project/Test_Rstox copy2", "baseline", list(processName = "ReadAcoustic", functionName = "RstoxBase::ReadAcoustic", functionParameters = list(FileNames = "Input/Acoustic/echosounder_cruiseNumber_2019203_Johan+Hjort.xml"))))








system.time(RstoxFramework::createProject("~/workspace/stox/project/Test_Rstox3", template = "Test3.0", ow = TRUE))

#RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox3")



system.time(modifyProcess("~/workspace/stox/project/Test_Rstox3", "baseline", "ReadBiotic", list(functionParameters = list(FileNames = "Input/Biotic/biotic_cruiseNumber_2017102_G+O+Sars_2019-01-15T03.27.17.822Z.xml"))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox3", "baseline", "DefineStrata", list(functionParameters = list(FileName = c("Input/norwegian_sea2014.txt")))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox3", "baseline", "StratumArea", list(functionInputs = list(StratumPolygon = "DefineStrata"))))


system.time(addProcess("~/workspace/stox/project/Test_Rstox3", "baseline", list(processName = "ReadAcoustic", functionName = "RstoxBase::ReadAcoustic", functionParameters = list(FileNames = "Input/Acoustic/libas_ListUserFile20__L40.0-2259.9.txt"))))

system.time(addProcess("~/workspace/stox/project/Test_Rstox3", "baseline", list(processName = "StoxAcoustic", functionName = "RstoxData::StoxAcoustic", functionInputs = list(AcousticData = "ReadAcoustic"))))

system.time(addProcess("~/workspace/stox/project/Test_Rstox3", "baseline", list(processName = "DefineAcousticPSU", functionName = "RstoxBase::DefineAcousticPSU", functionInputs = list(StratumPolygon = "DefineStrata", StoxAcousticData = "StoxAcoustic"))))

system.time(addProcess("~/workspace/stox/project/Test_Rstox3", "baseline", list(processName = "DefineAcousticLayer", functionName = "RstoxBase::DefineAcousticLayer", functionInputs = list(StoxAcousticData = "StoxAcoustic"))))





getProcessTable("~/workspace/stox/project/Test_Rstox3", "baseline")


system.time(f <- runModel("~/workspace/stox/project/Test_Rstox3", modelName = "baseline"))

RstoxFramework::saveProject("~/workspace/stox/project/Test_Rstox3")


g1 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "baseline", "P001")
g1

g2 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "baseline", "P002")
g2

g3 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "baseline", "P003")
g3

g4 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "baseline", "P004")
g4

g5 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "baseline", "P005")
g5

g6 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "baseline", "P006")
g6

g7 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "baseline", "P007")
g7




















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


# Add process 2, StoxBiotic:
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
			DefinitionMethod = "HighestResolution"
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
			StoxBioticData = "StoxBiotic"
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
			StoxBioticData = "StoxBiotic"
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
			StoxBioticData = "StoxBiotic", 
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
				SpeciesCategory = "sild'G03/161722.G03/126417/NA", 
				Alpha = 5.91, 
				Beta = 0.43, 
				LMin = 15, 
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
			LengthDistributionData = "LengthDependentCatchCompensation"
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
			StoxAcousticData = "StoxAcoustic", 
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
			NASCData = "NASC"
		), 
		functionParameters = list(
			TargetResolution = "Stratum"
		)
	)
)

# Add process 20, SweptAreaDensity:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "SweptAreaDensity", 
		functionName = "SweptAreaDensity", 
		functionInputs = list(
			LengthDistributionData = "MeanLengthDistribution"
		), 
		functionParameters = list(
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
			StoxBioticData = "StoxBiotic", 
			StratumPolygon = "DefineStrata"
		)
	)
)


## Add process 22, AssignmentLengthDistribution:
#temp <- addProcess(
#	projectPath = projectPath, 
#	modelName = "baseline", 
#	values = list(
#		processName = "AssignmentLengthDistribution", 
#		functionName = "AssignmentLengthDistribution", 
#		functionInputs = list(
#			LengthDistributionData = "LengthDependentCatchCompensation", 
#			BioticAssignment = "DefineBioticAssignment"
#		)
#	)
#)


# Add process 23, MeanDensity:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "baseline", 
	values = list(
		processName = "MeanDensity", 
		functionName = "MeanDensity", 
		functionInputs = list(
			DensityData = "SweptAreaDensity"
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
			StoxBioticData = "StoxBiotic", 
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
			StoxBioticData = "StoxBiotic", 
			LengthDistributionData = "LengthDependentCatchCompensation"
		), 
		functionParameters = list(
			DensityType = "SweptArea"
		)
	)
)

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
			FilterExpression = list(Individual = "SpeciesCategoryKey != \"sild'G03/161722.G03/126417/NA\"")
		)
	)
)



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

processTable <- getProcessTable("~/workspace/stox/project/Test_Rstox3", "baseline")

system.time(f <- runModel("~/workspace/stox/project/Test_Rstox3", modelName = "baseline"))


RstoxFramework::saveProject("~/workspace/stox/project/Test_Rstox3")

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
processID <- "P007"
RstoxFramework::getProcessPropertySheet(projectPath, modelName, processID)



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

d3 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox3", "baseline", "P003")
jsonlite::toJSON(d3, pretty = TRUE, auto_unbox = TRUE)


d7 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox3", "baseline", "P007")
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
	processes = c("ReadBiotic", "StoxBiotic", "DefineStrata", "StratumArea", "DefineSweptAreaPSU", "DefineSweptAreaLayer", "LengthDistribution", "LengthDependentCatchCompensation", "RegroupLengthDistribution")
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



