# Build 1.0:
RstoxBuild::buildRstoxPackage("RstoxFramework", version="1.0", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), suggests="testthat", check=FALSE)



system.time(RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox copy123"))
system.time(RstoxFramework::openProject("~/workspace/stox/project"))
system.time(RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox copy3/tull"))



system.time(RstoxFramework::createProject("~/workspace/stox/project/Test_Rstox copy3", template = "Test3.0", ow = TRUE))

RstoxFramework::closeProject("~/workspace/stox/project/Test_Rstox copy3")

RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox copy3")

RstoxFramework::saveProject("~/workspace/stox/project/Test_Rstox copy3")

RstoxFramework::saveAsProject("~/workspace/stox/project/Test_Rstox copy3", "~/workspace/stox/project/Test_Rstox copy4")

RstoxFramework::isOpenProject("~/workspace/stox/project/Test_Rstox copy3")
RstoxFramework::isOpenProject("~/workspace/stox/project/Test_Rstox copy4")
RstoxFramework::closeProject("~/workspace/stox/project/Test_Rstox copy4")
RstoxFramework::openProject("~/workspace/stox/project/Test_Rstox copy4")




system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "ReadBiotic", list(functionParameters = list(FileNames = "~/workspace/stox/project/StoXVerTest_temp/unix_18.2.0/Proj/Bar_Se_Nort_Arc_co_bo_tr_in_i_wi_2017_bioticV3.0/input/biotic/biotic_cruiseNumber_2017102_G+O+Sars_2019-01-15T03.27.17.822Z.xml"))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "DefineStrata", list(functionParameters = list(FileName = c("~/workspace/stox/reference/stratum/angola_15-500M.txt")))))

system.time(modifyProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "StratumArea", list(functionInputs = list(StratumPolygon = "DefineStrata"))))




system.time(f1 <- runProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "P001"))

system.time(f2 <- runProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "DefineStrata"))

system.time(f3 <- runProcess("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "StratumArea"))


system.time(f <- runModel("~/workspace/stox/project/Test_Rstox copy3", modelName = "Baseline"))


resolveProjectPath("~/workspace/stox/project/Test_Rstox copy3/Process/projectSession/projectDescription/currentProjectDescription.rds")


getProcessTable("~/workspace/stox/project/Test_Rstox copy3", "Baseline")


getProcessOutput("~/workspace/stox/project/Test_Rstox copy3", "Baseline", "P003")

