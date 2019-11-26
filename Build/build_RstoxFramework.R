# Build 1.0:
RstoxBuild::buildRstoxPackage(
	"RstoxFramework", 
	version = "1.0", 
	Rversion = "3.5", 
	pckversion = list(
		data.table = "1.10.4-3"
	), 
	suggests="testthat", 
	check=FALSE
)





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












acousticFile <- function(fileName, projectPath = NULL) {
	inputFolder <- getRstoxFrameworkDefinitions("stoxFolderStructureList")$Input_Acoustic
	paste(c(projectPath, inputFolder, fileName), collapse = .Platform$file.sep)
}
bioticFile <- function(fileName, projectPath = NULL) {
	inputFolder <- getRstoxFrameworkDefinitions("stoxFolderStructureList")$Input_Biotic
	paste(c(projectPath, inputFolder, fileName), collapse = .Platform$file.sep)
}
landingFile <- function(fileName, projectPath = NULL) {
	inputFolder <- getRstoxFrameworkDefinitions("stoxFolderStructureList")$Input_Landing
	paste(c(projectPath, inputFolder, fileName), collapse = .Platform$file.sep)
}
inputFile <- function(fileName, projectPath = NULL) {
	inputFolder <- getRstoxFrameworkDefinitions("paths")$stoxFolders["Input"]
	paste(c(projectPath, inputFolder, fileName), collapse = .Platform$file.sep)
}


projectPath <- "~/workspace/stox/project/Test_Rstox3"
filesDir <- "~/workspace/stox/project/Test_Rstox3files"

system.time(RstoxFramework::createProject(projectPath, template = "UserDefinedTemplate", ow = TRUE))

#RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox3")


# Add process 1, ReadBiotic:
bioticFileName <- "biotic_cruiseNumber_2017102_G+O+Sars_2019-01-15T03.27.17.822Z.xml"
file.copy(
	file.path(filesDir, bioticFileName), 
	bioticFile(bioticFileName, projectPath = projectPath)
)
addProcess(
	projectPath = projectPath, 
	modelName = "Baseline", 
	values = list(
		processName = "ReadBiotic", 
		functionName = "ReadBiotic", 
		functionParameters = list(
			FileNames = bioticFile(bioticFileName)
		)
	)
)

# Add process 2, DefineStrata:
stratumFileName <- "norwegian_sea2014.txt"
file.copy(
	file.path(filesDir, stratumFileName), 
	inputFile(stratumFileName, projectPath = projectPath)
)
addProcess(
	projectPath = projectPath, 
	modelName = "Baseline", 
	values = list(
		processName = "DefineStrata", 
		functionName = "DefineStrata", 
		functionParameters = list(
			FileName = inputFile(stratumFileName)
		)
	)
)

# Add process 3, StratumArea:
addProcess(
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

# Add process 4, ReadAcoustic:
#acousticFileName <- "libas_ListUserFile20__L40.0-2259.9.txt"
acousticFileName <- "libas_ListUserFile20__L40.0-2259.9.txt"
file.copy(
	file.path(filesDir, acousticFileName), 
	acousticFile(acousticFileName, projectPath = projectPath)
)
addProcess(
	projectPath = projectPath, 
	modelName = "Baseline", 
	values = list(
		processName = "ReadAcoustic", 
		functionName = "ReadAcoustic", 
		functionParameters = list(
			FileNames = acousticFile(acousticFileName)
		)
	)
)

# Add process 5, StoxAcoustic:
addProcess(
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

# Add process 6, DefineAcousticPSU:
addProcess(
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

# Add process 7, DefineAcousticLayer:
addProcess(
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

processTable <- getProcessTable("~/workspace/stox/project/Test_Rstox3", "Baseline")

system.time(f <- runModel("~/workspace/stox/project/Test_Rstox3", modelName = "Baseline"))

RstoxFramework::saveProject("~/workspace/stox/project/Test_Rstox3")

processOutput <- mapply(getProcessOutput, "~/workspace/stox/project/Test_Rstox3", "Baseline", processTable$processID)
lapply(processOutput, lengths)




g1 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "Baseline", "P001")
g1

g2 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "Baseline", "P001", pretty = TRUE)
g1

g1 <- getProcessOutput("~/workspace/stox/project/Test_Rstox3", "Baseline", "P001")
g1





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

















Hello,


Attached is an updated version of the design document. I have cleaned up old content, particularly in section 1 and 2, according to discussions during the past weeks.


The design document contains a new Section 6. Guidelines for developers which those of you who are developing the StoX function packages (RstoxData, RstoxBase, RstoxFDA) should read carefully. The key moments are the following:
	
	
	We should use the RstoxBuild package to build the StoX function packages.
RstoxBuild has been changed so that package dependencies (imports) are specified when building the packages using RstoxBuild::buildRstoxPackage (previously these packages were identified from the NAMESPACE file). All dependencies should have a minimum version, according to the notes on reproducibility in Section 6. Guidelines for developers
Data types are defined in the package inventing the data type. See Section 6. Guidelines for developers for how to document the data types.


Attached is a test project running the functions ReadBiotic, DefineStrata, StratumArea, ReadAcoustic, StoxAcoustic, DefineAcousticPSU and DefineAcousticLayer. The project can be run using the following code.

# Install RstoxFramework and RstoxBase from the master branches on GitHub
# devtools::install_github("StoXProject/RstoxFramework")
# devtools::install_github("StoXProject/RstoxBase")

# Place the project in a folder named "workspace" in your home folder, and run the following:

library(RstoxFramework)
projectPath <- "~/workspace/stox/project/Test_Rstox3"
openProject(projectPath)
# Run the model:
system.time(f <- runModel(projectPath, modelName = "Baseline"))
# Get the output from the model:
processOutput <- mapply(getProcessOutput, projectPath, "Baseline", processTable$processID)
lapply(processOutput, lengths)



