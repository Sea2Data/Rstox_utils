# This script demonstrates use of the R package Rstox, which makes the functionality in the stock assessment software StoX available for independent use in R. Rstox can also be used for downloading data from The Norwegian Marine Data Centre (NMD), placing these data in StoX projects which can be run both in StoX and in Rstox, and this use is the topic of these examples:

# Install Rstox as described in ftp://ftp.imr.no/StoX/Download/Rstox/README

library(Rstox)

#################################################
##### Example 1, downloading cruise series: #####
#################################################
# List cruise series:
CS <- getNMDinfo("cs")
names(CS)
# Pick out cruise series nr 1:
myCS <- names(CS)[1]
CS[myCS]

# Downloads the first and second year (subset=1:2) of the cruise series "Norwegian Sea International ecosystem cruise in May" to separate StoX projects for each year (group="year"):
# The StoX projects use the default template called "StationLengthDistTemplate", enabling reading biotic data into R:
system.time(projects <- getNMDdata(cruise=myCS, group="year", subset=1:2, StationLengthDist=list(LengthDistType="LengthDist"), ow=TRUE))
# The projects are put in the following directory:
getWorkspace()

# Choose the second year:
pr <- "Norwegian Sea International ecosystem cruise in May_year_1997_shipName_G O Sars"

# Get the data from the baseline:
g <- getBaseline(pr)

# There are three elements in the list, 'parameters', 'outputData' and 'processData', where 'outputData' contain the data you need:
names(g)
# The 'outputData' is a list of the processes in the StoX template "StationLengthDistTemplate".
names(g$outputData)
# The element g$outputData$ReadBioticXML contains raw data from the process 'ReadBioticXML':
names(g$outputData$ReadBioticXML)
# Show individual sample data:
dim(g$outputData$ReadBioticXML[["1_ReadBioticXML_BioticData_Individual.txt"]])
head(g$outputData$ReadBioticXML[["1_ReadBioticXML_BioticData_Individual.txt"]])

# Show length dirstibution per station:
head(g$outputData$StationLengthDist)


#######################################################
##### Example 2, downloading serial number range: #####
#######################################################
# It is also possible to download data given a serial number range:
system.time(projects <- getNMDdata(year=2015, serialno=23201:23251, ow=TRUE))

g <- getBaseline(projects)
head(g$outputData$ReadBioticXML[["1_ReadBioticXML_BioticData_Individual.txt"]])


##################################################
##### Example 3, downloading a sigle cruise: #####
##################################################
# It is also possible to download data given a serial number range:
system.time(projects <- getNMDdata(cruise="2015114", shipname="G.O.Sars", ow=TRUE))

# Unfortunately, here only the biotic data are imported. Importing also the acoustic data requires a different model, and this will be available in a later version:
g <- getBaseline(projects)
head(g$outputData$ReadBioticXML[["1_ReadBioticXML_BioticData_Individual.txt"]])


############################################################################
##### Example 4, downloading a survey time series (full StoX project): #####
############################################################################
STS <- getNMDinfo("sts")
names(STS)
# See the cruises of the cruise series "Barents Sea NOR-RUS ecosystem cruise in autumn":
mySTS <- "Norwegian Acoustic Sandeel Surveys in the North Sea"
STS[mySTS]

# Download all years of the survey time series:
system.time(projects <- getNMDdata(mySTS, ow=TRUE))

# Link the data to the projects due to a bug in the zipped files:
updateProject(projects[2])

g <- getBaseline(projects[2])
head(g$outputData$ReadBioticXML[["5_ReadBioticXML_BioticData_Individual.txt"]])


##############################################################################
##### Example 5, running survey estimation on the Rstox example project: #####
##############################################################################
# Create the test project based on data from the Internationa Ecosystem Survey in the Nordic Sea for herring 2016:
pr <- "Test_Rstox"
createProject(pr, files=system.file("extdata", "Test_Rstox", package="Rstox"), ow=TRUE)
# Here we could have used other files stored locally. See the description of 'files' in ?createProject.

# Get the output and inputs of the baseline:
g <- getBaseline(pr)

# Show superindividuals:
head(g$outputData$SuperIndAbundance)

# Run the bootstrapping to generate estimates of the variability in the data (cv):
system.time(rb <- runBootstrap(pr, nboot=50, cores=1, seed=1234, acousticMethod=PSU~Stratum, bioticMethod=EDSU~Stratum))

# Fill in missing data (missing length, weight and so on) based on the age information:
system.time(rb <- imputeByAge(pr))

# Save the bootstrap data:
saveProjectData(pr)

# Generate plots and reports:
plotfiles <- getPlots(pr)
reportfiles <- getReports(pr)
