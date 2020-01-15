
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




system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "ReadBiotic", list(functionParameters = list(FileNames = "~/workspace/stox/project/StoXVerTest_temp/unix_18.2.0/Proj/Bar_Se_Nort_Arc_co_bo_tr_in_i_wi_2017_bioticV3.0/input/biotic/biotic_cruiseNumber_2017102_G+O+Sars_2019-01-15T03.27.17.822Z.xml"))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "DefineStrata", list(functionParameters = list(FileName = c("~/workspace/stox/reference/stratum/angola_15-500M.txt")))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "StratumArea", list(functionInputs = list(StratumPolygon = "DefineStrata"))))




system.time(f1 <- runProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "P001"))

system.time(f2 <- runProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "P002"))

system.time(f3 <- runProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "P003"))


system.time(f <- runModel("~/workspace/stox/project/Test_Rstox copy3", modelName = "Baseline"))

system.time(f <- runFunction(runModel, list("~/workspace/stox/project/Test_Rstox copy3", modelName = "Baseline")))





getProcessTable("~/workspace/stox/project/Test_Rstox copy3", "Baseline")


g <- getProcessOutput("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "P002")



d1 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "P001")
d2 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "P002")
d3 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "P003")
jsonlite::toJSON(d1, pretty = TRUE)
jsonlite::toJSON(d2, pretty = TRUE)
jsonlite::toJSON(d3, pretty = TRUE)
jsonlite::toJSON(d2, pretty = TRUE, auto_unbox = TRUE)
jsonlite::toJSON(d1, pretty = TRUE, auto_unbox = TRUE)
jsonlite::toJSON(d3, pretty = TRUE, auto_unbox = TRUE)


d3 <- setProcessPropertyValue(groupName = "processArguments", name = "functionName", value = "ReadBiotic", "~/workspace/stox/project/Test_Rstox copy3", "Baseline", "P003")

d3 <- setProcessPropertyValue(groupName = "processArguments", name = "functionName", value = "DefineStrata", "~/workspace/stox/project/Test_Rstox3", "Baseline", "P003")

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

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy2", "Baseline", "ReadBiotic", list(functionParameters = list(FileNames = "~/workspace/stox/project/Test_Rstox copy2/Input/Biotic/biotic_cruiseNumber_2017102_G+O+Sars_2019-01-15T03.27.17.822Z.xml"))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy2", "Baseline", "DefineStrata", list(functionParameters = list(FileName = c("~/workspace/stox/project/Test_Rstox copy2/Input/angola_15-500M.txt")))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy2", "Baseline", "StratumArea", list(functionInputs = list(StratumPolygon = "DefineStrata"))))


system.time(addProcess("~/workspace/stox/project/Test_Rstox copy2", "Baseline", list(processName = "ReadAcoustic", functionName = "RstoxBase::ReadAcoustic", functionParameters = list(FileNames = "Input/Acoustic/echosounder_cruiseNumber_2019203_Johan+Hjort.xml"))))








system.time(RstoxFramework::createProject("~/workspace/stox/project/Test_Rstox3", template = "Test3.0", ow = TRUE))

#RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox3")



system.time(modifyProcess("~/workspace/stox/project/Test_Rstox3", "Baseline", "ReadBiotic", list(functionParameters = list(FileNames = "Input/Biotic/biotic_cruiseNumber_2017102_G+O+Sars_2019-01-15T03.27.17.822Z.xml"))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox3", "Baseline", "DefineStrata", list(functionParameters = list(FileName = c("Input/norwegian_sea2014.txt")))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox3", "Baseline", "StratumArea", list(functionInputs = list(StratumPolygon = "DefineStrata"))))


system.time(addProcess("~/workspace/stox/project/Test_Rstox3", "Baseline", list(processName = "ReadAcoustic", functionName = "RstoxBase::ReadAcoustic", functionParameters = list(FileNames = "Input/Acoustic/libas_ListUserFile20__L40.0-2259.9.txt"))))

system.time(addProcess("~/workspace/stox/project/Test_Rstox3", "Baseline", list(processName = "StoxAcoustic", functionName = "RstoxData::StoxAcoustic", functionInputs = list(AcousticData = "ReadAcoustic"))))

system.time(addProcess("~/workspace/stox/project/Test_Rstox3", "Baseline", list(processName = "DefineAcousticPSU", functionName = "RstoxBase::DefineAcousticPSU", functionInputs = list(StratumPolygon = "DefineStrata", StoxAcousticData = "StoxAcoustic"))))

system.time(addProcess("~/workspace/stox/project/Test_Rstox3", "Baseline", list(processName = "DefineAcousticLayer", functionName = "RstoxBase::DefineAcousticLayer", functionInputs = list(StoxAcousticData = "StoxAcoustic"))))





getProcessTable("~/workspace/stox/project/Test_Rstox3", "Baseline")


system.time(f <- runModel("~/workspace/stox/project/Test_Rstox3", modelName = "Baseline"))

RstoxFramework::saveProject("~/workspace/stox/project/Test_Rstox3")


g1 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "Baseline", "P001")
g1

g2 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "Baseline", "P002")
g2

g3 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "Baseline", "P003")
g3

g4 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "Baseline", "P004")
g4

g5 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "Baseline", "P005")
g5

g6 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "Baseline", "P006")
g6

g7 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "Baseline", "P007")
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
	modelName = "Baseline", 
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
	modelName = "Baseline", 
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
	modelName = "Baseline", 
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
	modelName = "Baseline", 
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
	modelName = "Baseline", 
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
	modelName = "Baseline", 
	values = list(
		processName = "StoxAcoustic", 
		functionName = "StoxAcoustic", 
		functionInputs = list(
			AcousticData = "ReadAcoustic"
		)
	)
)

# Add process 7, DefineAcousticPSU:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "Baseline", 
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
	modelName = "Baseline", 
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
	modelName = "Baseline", 
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
	modelName = "Baseline", 
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
	modelName = "Baseline", 
	values = list(
		processName = "LengthDistribution", 
		functionName = "LengthDistribution", 
		functionInputs = list(
			StoxBioticData = "StoxBiotic", 
			SweptAreaPSU = "DefineSweptAreaPSU", 
			SweptAreaLayer = "DefineSweptAreaLayer"
		), 
		functionParameters = list(
			LengthDistributionType = "NormalizedLengthDistribution"
		)
	)
)

# Add process 12, NASC:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "Baseline", 
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
# Add process 13, BioticStationAssignment:
#temp <- addProcess(
#	projectPath = projectPath, 
#	modelName = "Baseline", 
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

processTable <- getProcessTable("~/workspace/stox/project/Test_Rstox3", "Baseline")

system.time(f <- runModel("~/workspace/stox/project/Test_Rstox3", modelName = "Baseline"))


RstoxFramework::saveProject("~/workspace/stox/project/Test_Rstox3")

processOutput <- mapply(getProcessOutput, "~/workspace/stox/project/Test_Rstox3", "Baseline", processTable$processID)
names(processOutput) <- processTable$processName
TSD::dim_all(processOutput)




mapData <- mapply(getMapData, "~/workspace/stox/project/Test_Rstox3", "Baseline", processTable$processID)
names(mapData) <- processTable$processName
TSD::dim_all(mapData)



library(sp)
plot(processOutput$DefineStrata)
points(processOutput$StoxAcoustic$Log$Longitude, processOutput$StoxAcoustic$Log$Latitude)
points(processOutput$StoxBiotic$Station$Longitude, processOutput$StoxBiotic$Station$Latitude, col = 2)











projectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "Baseline"
processID <- "P009"

temp <-addHaulToAssignment(PSU = "T2267", Layer = 1, Haul = "24076/3536111", projectPath, modelName, processID)

temp <-removeHaulFromAssignment(PSU = "T2267", Layer = 1, Haul = "24076/3536111", projectPath, modelNamemodelName, processID)



projectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "Baseline"
processID <- "P007"

temp <- addAcousticPSU("S2", PSU = NULL, projectPath, modelName, processID)



projectPath <- "~/workspace/stox/project/Test_Rstox3"
modelName <- "Baseline"
processID <- "P003"

newStratum <- StratumPolygon$StratumPolygon
newStratumnewStratum@polygons <- newStratum$newStratum@polygons[1]
temp <- addStratum(newStratum, projectPath, modelName, processID)
temp <- RstoxFramework:::getProcessData(projectPath, modelName, processID)

temp <- modifyStratum(newStratum, projectPath, modelName, processID)
temp <- RstoxFramework:::getProcessData(projectPath, modelName, processID)






projectPath <- "~/workspace/stox/project/Test_Rstox3"

system.time(RstoxFramework::createProject(projectPath, template = "Test3.0", ow = TRUE))
temp <- RstoxFramework:::modifyFunctionName("~/workspace/stox/project/Test_Rstox3", "Baseline", "P001", "ReadBiotic")

# Add process 7, DefineAcousticLayer:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "Baseline", 
	values = list(
		processName = "DefineAcousticLayer", 
		functionName = "DefineAcousticLayer", 
		functionInputs = list(
			StoxAcousticData = "StoxAcoustic"
		)
	)
)


d1 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox3", "Baseline", "P001")
jsonlite::toJSON(d1, pretty = TRUE, auto_unbox = TRUE)

d2 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox3", "Baseline", "P004")
jsonlite::toJSON(d2, pretty = TRUE, auto_unbox = TRUE)

d3 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox3", "Baseline", "P003")
jsonlite::toJSON(d3, pretty = TRUE, auto_unbox = TRUE)


d7 <- getProcessPropertySheet("~/workspace/stox/project/Test_Rstox3", "Baseline", "P007")
jsonlite::toJSON(d7, pretty = TRUE, auto_unbox = TRUE)





stratum <- "{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"geometry\":{\"type\":\"Polygon\",\"coordinates\":[[[4.3000000000000025,62.249999997276205],[3.7666666700000015,62.249999997276205],[3.7666666699999984,62.48999999713977],[-1.6640291377292762,64.84799456068586],[6.116666670000004,63.499999996573344],[7.000000000000003,63.499999996573344],[4.3000000000000025,62.249999997276205]]]},\"properties\":{\"polygonName\":\"2\"},\"id\":2},{\"type\":\"Feature\",\"geometry\":{\"type\":\"Polygon\",\"coordinates\":[[[4.3000000000000025,62.249999997276205],[3.7666666700000015,62.249999997276205],[3.7666666699999984,62.48999999713977],[-1.6640291377292762,64.84799456068586],[6.116666670000004,63.499999996573344],[7.000000000000003,63.499999996573344],[4.3000000000000025,62.249999997276205]]]},\"properties\":{\"polygonName\":\"5\"},\"id\":5}]}"
modifyStratum(stratum, projectPath = "~/workspace/stox/project/Test_Rstox3_cod2019", modelName = "Baseline", processID = "P003")












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
		modelName = "Baseline", 
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
		modelName = "Baseline", 
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
		modelName = "Baseline", 
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
		modelName = "Baseline", 
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
		modelName = "Baseline", 
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
		modelName = "Baseline", 
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
		modelName = "Baseline", 
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
		modelName = "Baseline", 
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
		modelName = "Baseline", 
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
		modelName = "Baseline", 
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
		modelName = "Baseline", 
		values = list(
			processName = "LengthDistribution", 
			functionName = "LengthDistribution", 
			functionInputs = list(
				StoxBioticData = "StoxBiotic", 
				SweptAreaPSU = "DefineSweptAreaPSU", 
				SweptAreaLayer = "DefineSweptAreaLayer"
			), 
			functionParameters = list(
				LengthDistributionType = "NormalizedLengthDistribution"
			)
		)
	)
	
	temp
}
# Funciton to add LengthDistribution:
addRegroupLengthDistribution <- function(projectPath, ...) {
	temp <- addProcess(
		projectPath = projectPath, 
		modelName = "Baseline", 
		values = list(
			processName = "RegroupLengthDistribution", 
			functionName = "RegroupLengthDistribution", 
			functionInputs = list(
				LengthDistribution = "LengthDistribution"
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
		modelName = "Baseline", 
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
	processTable <- getProcessTable(skeleton$projectPath, "Baseline")
	
	# Run the baseline of the new project:
	system.time(f <- runModel(skeleton$projectPath, modelName = "Baseline"))
	
	# Get the process output:
	processOutput <- mapply(getProcessOutput, skeleton$projectPath, "Baseline", processTable$processID)
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
	processes = c("ReadBiotic", "StoxBiotic", "DefineStrata", "StratumArea", "DefineSweptAreaPSU", "DefineSweptAreaLayer", "LengthDistribution", "RegroupLengthDistribution")
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


system.time(f <- runModel("~/workspace/stox/project/Test_Rstox3_herring2018", modelName = "Baseline", endProcess = 3))
############################################
############################################

























library(Rstox)
sts <- "Barents Sea Northeast Arctic cod bottom trawl index in winter"
year <- 2019
#projectPathOriginal <- Rstox::getNMDdata(sts, subset = year, ow = TRUE)
projectPathOriginal <- file.path( "~/workspace/stox/project", paste(sts, year, sep = "_"))

d <- getBaseline(projectPathOriginal)
polygonFile <- file.path(getProjectPaths(projectPathOriginal)$inputDir, "stratumpolygon.txt")
data.table::fwrite(d$processData$stratumpolygon[, c("Stratum", "Polygon" )], file = polygonFile, sep ="\t", col.names = FALSE)




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

projectPath <- "~/workspace/stox/project/Test_Rstox3_cod2019"
resourceProjectPath <- projectPathOriginal
bioticresourceProjectPath <- file.path(resourceProjectPath, "input", "biotic")
acousticresourceProjectPath <- file.path(resourceProjectPath, "input", "acoustic")

system.time(RstoxFramework::createProject(projectPath, template = "UserDefinedTemplate", ow = TRUE))

##### Create the project: #####


bioticFileNames <- list.files(bioticresourceProjectPath, full.names = TRUE)
file.copy(
	bioticFileNames, 
	inputFile(bioticFileNames, projectPath = projectPath, folder = "biotic")
)
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "Baseline", 
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
	modelName = "Baseline", 
	values = list(
		processName = "StoxBiotic", 
		functionName = "StoxBiotic", 
		functionInputs = list(
			BioticData = "ReadBiotic"
		)
	)
)

# Add process 3, DefineStrata:
inputFileNames <- polygonFile
file.copy(
	inputFileNames, 
	inputFile(inputFileNames, projectPath = projectPath, folder = "input")
)
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "Baseline", 
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
	modelName = "Baseline", 
	values = list(
		processName = "StratumArea", 
		functionName = "StratumArea", 
		functionInputs = list(
			StratumPolygon = "DefineStrata"
		)
	)
)


# Add process 5, DefineSweptAreaPSU:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "Baseline", 
	values = list(
		processName = "DefineSweptAreaPSU", 
		functionName = "DefineSweptAreaPSU", 
		functionInputs = list(
			StratumPolygon = "DefineStrata", 
			StoxBioticData = "StoxBiotic"
		)
	)
)

# Add process 6, DefineSweptAreaLayer:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "Baseline", 
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


# Add process 7, LengthDistribution:
temp <- addProcess(
	projectPath = projectPath, 
	modelName = "Baseline", 
	values = list(
		processName = "LengthDistribution", 
		functionName = "LengthDistribution", 
		functionInputs = list(
			StoxBioticData = "StoxBiotic", 
			SweptAreaPSU = "DefineSweptAreaPSU", 
			SweptAreaLayer = "DefineSweptAreaLayer"
		), 
		functionParameters = list(
			LengthDistributionType = "NormalizedLengthDistribution"
		)
	)
)




##### End of project: #####


stoxLibrary <- getRstoxFrameworkDefinitions("stoxLibrary")
names(stoxLibrary)

processTable <- getProcessTable(projectPath, "Baseline")

system.time(f <- runModel(projectPath, modelName = "Baseline"))

processOutput <- mapply(getProcessOutput, projectPath, "Baseline", processTable$processID)
names(processOutput) <- processTable$processName
TSD::dim_all(processOutput)

processOutput$LengthDistribution
d$outputData$StationLengthDist

d <- RstoxBase:::meanData(processOutput$LengthDistribution, "Stratum")
dim(d)









usethis::browse_github_pat() # Generate new token, then copy it
usethis::edit_r_environ() # A file is opened. Paste the token to this file. Then viola!



