# This script demonstrates use of the R package Rstox, which makes the functionality in the stock assessment software StoX available for independent use in R. 
# Rstox can also be used for downloading data from The Norwegian Marine Data Centre (NMD), 
#  placing these data in StoX projects which can be run both in StoX and in Rstox (see example 1 - 4).
# Survey estimation with precision of the test project is shown in example 5.
# Direct variance estimation without bootstrapping is shown in example 6. 
# Overriding parameters of a StoX project is shown i example 7.


# Install Rstox as described on https://github.com/Sea2Data/Rstox

library(Rstox)


######################################
##### Example 0, read XML files: #####
######################################
# The simples use of the Rstox package is reading biotic, acoustic or landing XMl files into R.

# The function readXMLfiles() reads XML files using the functionality in StoX, where multiple files are combined. The input 'files' can be a list of both biotic, acoustic or landing files, or the path to a directory holding sub-directories with such files:

# A single directory with sub-directories "biotic", "acoustic" or "landing":
dataDir <- system.file("extdata", "Test_Rstox", "input", package="Rstox")
dataDir
dat1 <- readXMLfiles(dataDir)
ls.str(dat1)

# A list of biotic and acoustic files:
f <- list(
	biotic=list.files(file.path(dataDir, "biotic"), full.names=TRUE),
	acoustic=list.files(file.path(dataDir, "acoustic"), full.names=TRUE))
f
dat2 <- readXMLfiles(f)
ls.str(dat2)


#################################################
##### Example 1, downloading cruise series: #####
#################################################
# List cruise series:
CS <- getNMDinfo("cs", recursive=FALSE)
CS
# Pick out cruise series nr 1:
myCS <- CS[1]
getNMDinfo(c("cs", myCS))

# As of Rstox_1.7 the model to use can be specified when generating the projects. Here a model reading the biotic and acoustic data and generating the length distribution per station is used:
model <- list("ReadBioticXML", "ReadAcousticXML", StationLengthDist=list(LengthDistType="LengthDist", BioticData="ReadBioticXML"))
modelBio <- list("ReadBioticXML", StationLengthDist=list(LengthDistType="LengthDist", BioticData="ReadBioticXML"))

# Downloads the first and second year (subset=1:2) of the cruise series "Norwegian Sea International ecosystem cruise in May" to separate StoX projects for each year (group="year"):
# As of Rstox_1.7 the all downloaded projects of a cruise series or survey time series can be placed in a sub directory in the default workspace, specified by subdir=TRUE. This avoids crowding the workspace with many projects. Also the names of the cruise series and survey time series may be very long (up to 100 characters). It is recommended to abbreviate the project names, particularly when using subdir=TRUE (abbreviate using abbrev=TRUE):
system.time(projects <- getNMDdata(cruise=myCS, group="year", subset=1:2, model=model, abbrev=TRUE, subdir=TRUE, ow=TRUE))
# Show the project paths:
projects
# To get a list of all projects in the default project directory, run the following
allpr <- openProject(list())
allpr


# Choose the first year:
pr <- projects[1]
# Get the data from the baseline:
g <- getBaseline(pr)

# There are three elements in the list, 'parameters', 'outputData' and 'processData', where 'outputData' contain the data you need:
names(g)
# The 'outputData' is a list of the processes in the StoX template "StationLengthDistTemplate".
names(g$outputData)
# The element g$outputData$ReadBioticXML contains raw data from the process 'ReadBioticXML':
names(g$outputData$ReadBioticXML)
# Show individual sample data:
dim(g$outputData$ReadBioticXML[["ReadBioticXML_BioticData_Individual.txt"]])
head(g$outputData$ReadBioticXML[["ReadBioticXML_BioticData_Individual.txt"]])

# Show length dirstibution per station:
head(g$outputData$StationLengthDist)


#######################################################
##### Example 2, downloading serial number range: #####
#######################################################
# It is also possible to download data given serial numbers:
system.time(projects <- getNMDdata(year=2015, serialno=c(23201:23241, 23221:24281), model=modelBio, ow=TRUE))

g <- getBaseline(projects)
head(g$outputData$ReadBioticXML[["ReadBioticXML_BioticData_Individual.txt"]])


###################################################
##### Example 3, downloading a single cruise: #####
###################################################
# It is also possible to download data given a serial number range:
system.time(projects <- getNMDdata(cruise="2015114", shipname="G.O.Sars", model=model, ow=TRUE))

# Unfortunately, here only the biotic data are imported. Importing also the acoustic data requires a different model, and this will be available in a later version:
g <- getBaseline(projects)
head(g$outputData$ReadBioticXML[["ReadBioticXML_BioticData_Individual.txt"]])


############################################################################
##### Example 4, downloading a survey time series (full StoX project): #####
############################################################################
STS <- getNMDinfo("sts", recursive=FALSE)
STS
# See the cruises of the cruise series "Barents Sea NOR-RUS ecosystem cruise in autumn":
mySTS <- STS[1]
getNMDinfo(c("sts", mySTS))

# Download all years of the survey time series:
system.time(projects <- getNMDdata(mySTS, abbrev=TRUE, subdir=TRUE, ow=TRUE))

# Link the data to the projects due to a bug in the zipped files:
updateProject(projects[2])

g <- getBaseline(projects[2])
head(g$outputData$ReadBioticXML[["ReadBioticXML_BioticData_Individual.txt"]])


##############################################################################
##### Example 5, running survey estimation on the Rstox example project: #####
##############################################################################
# Create the test project based on data from the Internationa Ecosystem Survey in the Nordic Sea for herring 2016:
pr <- "Test_Rstox"
createProject(pr, ow=TRUE)
# Here we could have used other files stored locally. See the description of 'files' in ?createProject.

# Get the output and inputs of the baseline:
g <- getBaseline(pr)

# Show superindividuals:
head(g$outputData$SuperIndAbundance)

# Run the bootstrapping to generate estimates of the variability in the data (cv):
system.time(rb <- runBootstrap(pr, nboot=50, cores=1, seed=1, acousticMethod=PSU~Stratum, bioticMethod=EDSU~Stratum))

# Fill in missing data (missing length, weight and so on) based on the age information:
system.time(rb <- imputeByAge(pr))

# Save the bootstrap data:
saveProjectData(pr)

# Generate plots and reports:
plotfiles <- getPlots(pr)
reportfiles <- getReports(pr)



########################################################################
##### Example 6, direct variance estimation of swept area project: #####
########################################################################

# Download an example swept area project, year 2015 indicatd by subset=23:
sts <- getNMDinfo("sts")
sts[["Barents Sea Northeast Arctic cod bottom trawl index in winter"]]
pr_sweptarea2015 <- getNMDdata("Barents Sea Northeast Arctic cod bottom trawl index in winter", subset=23,abbrev=TRUE, subdir=TRUE, ow=TRUE)
# Select year 2015:
v_sweptarea <- varianceEstimation(pr_sweptarea2015)
v_sweptarea

# Run also bootstrap method "SweptAreaLength", and compare the cv's:
runBootstrap(pr_sweptarea2015, bootstrapMethod="SweptAreaLength", nboot=50, cores=5)
r <- getReports(pr_sweptarea2015, grp1="Stratum")


# Plot the cv from varianceEstimation() and from runBootstrap():
plot(v_sweptarea $torsk$Stratum[, c("SampleUnit", "CVDensity")], type="o", col=4, ylim=c(0,1))
points(r$bootstrap$abnd[, c("Stratum", "Ab.Sum.cv")], type="o", col=2)

# Somewhat lower cv from varianceEstimation(): 
summary(r$bootstrap$abnd$Ab.Sum.cv / v_sweptarea $torsk$Stratum$CVDensity)



###########################################################
##### Example 7, override parameters in the baseline: #####
###########################################################

##### a) Override parameter tables: #####

### splitNASC: ###

# Download an example splitNASC project:
pr_splitNASC <- createProject("ftp://ftp.imr.no/StoX/Example%20projects/Example_splitNASC_NorthSea_Herring_AcousticSurvey_DanishData_2015.zip", ow=TRUE)

# Get the data from the baseline:
g <- getBaseline(pr_splitNASC)

# Check parameter settings:
g$parameters$SplitNASC_MIX$SpeciesTS
# Convert SpeciesTS table from string to data.frame:
SpeciesTS <- parString2data.frame(g$parameters$SplitNASC_MIX$SpeciesTS)
plot(SpeciesTS$a)
# Add some noise on the TS:
set.seed(0)
add <- rnorm(nrow(SpeciesTS))
SpeciesTS$a <- SpeciesTS$a + add
points(SpeciesTS$a, col=3)
# Convert back to string:
string <- data.frame2parString(SpeciesTS)
string

# Rerun the baseline with changed SpecieTS:
g2 <- getBaseline(pr_splitNASC, SplitNASC_MIX=list(SpeciesTS=string))

# Inducing a difference in the outputData:
identical(g, g2)
all.equal(g, g2)




##### b) Override in the test project (ftp://ftp.imr.no/StoX/Example%20projects/Example_acoustic_InternationaEcosystemSurveyNordic%20Sea_herring_2016.zip): #####

g <- getBaseline("Test_Rstox")

g$parameters$SweptAreaDensity$SweepWidth

g2 <- getBaseline("Test_Rstox", SweptAreaDensity=list(SweepWidth=30))

# Inducing a difference in the outputData:
identical(g, g2)
all.equal(g, g2)

