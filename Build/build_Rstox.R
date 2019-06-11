##### Create and install the package stox in R: #####



##### Load devtools: #####
#library("RstoxBuild")
#
## Source the utility functions:
##source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")
#
#
## Define the directory of the working copy:
##dir <- list(
##  arnejh = list(
##    Rstox = "~/Code/Github/Rstox/Rstox", 
##    Rstox_utils = "~/Code/Github/Rstox_utils/Rstox_utils"
##  ), 
##  atlet = list(
##    Rstox = "F:/GIT/Rstox", 
##    Rstox_utils = "F:/GIT/Rstox_utils"
##  ), 
##  edvinf = list(
##		Rstox = "~/code/github/rstox_/", 
##		Rstox_utils = "~/code/github/Rstox_utils/"
##	)
##)
#dir <- list(
#  arnejh = "~/Code/Github/Rstox/Rstox", 
#  atlet = "F:/GIT/Rstox", 
#  edvinf = "~/code/github/rstox_/"
#)


#source(file.path(dir[[Sys.info()["user"]]]$Rstox_utils, "Rstox_utils.R"))

# Build 1.6.2:
# build_Rstox(dir_Rstox, version="1.6.2", Rversion="3.3.3", official=FALSE, check=FALSE)


# # Build 1.6.3:
# build_Rstox(dir_Rstox, version="1.6.3", Rversion="3.3.3", official=FALSE, check=FALSE, pckversion=list(data.table="1.10.4-3"))


# Build 1.6.4:
# build_Rstox(dir_Rstox, version="1.6.4", Rversion="3.4.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, check=FALSE)


# Build 1.6.5:
#build_Rstox(dir_Rstox, version="1.6.5", Rversion="3.4.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, check=FALSE)


# Build 1.7:
#build_Rstox(dir_Rstox, version="1.7", Rversion="3.4.2", pckversion=list(data.table="1.10.4-3"), official=TRUE, check=FALSE)

# Build 1.7.1:
#build_Rstox(dir_Rstox, version="1.7.1", Rversion="3.4.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, check=FALSE)

# Build 1.7.2:
#build_Rstox(dir_Rstox, version="1.7.2", Rversion="3.4.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, check=FALSE)

# Build 1.8:
#build_Rstox(dir_Rstox, version="1.8", Rversion="3.4.3", pckversion=list(data.table="1.10.4-3"), official=TRUE, check=TRUE)

# Build 1.8.1:
#build_Rstox(dir_Rstox, version="1.8.1", Rversion="3.4.3", pckversion=list(data.table="1.10.4-3"), official=FALSE, check=FALSE)

#build_Rstox(dir_Rstox, version="1.8.1", Rversion="3.4.3", pckversion=list(data.table="1.10.4-3"), official=FALSE, suggests=c("ggmap", "ncdf4", "pgirmess"), check=FALSE)

# Build 1.9:
# build_Rstox(dir_Rstox, version="1.9", Rversion="3.4.3", pckversion=list(data.table="1.10.4-3"), official=TRUE, suggests=c("ggmap", "ncdf4", "pgirmess"), check=FALSE)



# Build 1.9.1:
#build_Rstox(dir[[Sys.info()["user"]]]$Rstox, version="1.9.1", Rversion="3.5.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, suggests=c("ggmap", "ncdf4", "pgirmess", "eca", "plotrix"), check=FALSE)


# Build 1.9.2:
#build_Rstox(dir[[Sys.info()["user"]]]$Rstox, version="1.9.2", Rversion="3.5.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, suggests=c("ggmap", "ncdf4", "pgirmess", "eca", "plotrix"), check=FALSE)


# Build 1.9.3:
#build_Rstox(dir[[Sys.info()["user"]]]$Rstox, version="1.9.3", Rversion="3.5.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, suggests=c("ggmap", "ncdf4", "pgirmess", "eca", "plotrix"), check=FALSE)

# Build 1.9.4:
# Changed to Rversion="3.5" as per the warning during build with chekc=TRUE, where patch number os recommended against:
#build_Rstox(dir[[Sys.info()["user"]]]$Rstox, version="1.9.4", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), official=FALSE, suggests=c("ggmap", "ncdf4", "pgirmess", "eca", "plotrix"), check=FALSE)

# Build 1.9.5:
# Changed to Rversion="3.5" as per the warning during build with chekc=TRUE, where patch number os recommended against:
# build_Rstox(dir[[Sys.info()["user"]]]$Rstox, version="1.9.5", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), official=FALSE, suggests=c("ggmap", "ncdf4", "pgirmess", "eca", "plotrix"), check=FALSE)


# Build 1.10:
#build_Rstox(dir[[Sys.info()["user"]]]$Rstox, version="1.10", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), official=TRUE, suggests=c("ggmap", "ncdf4", "pgirmess", "eca", "plotrix"), check=FALSE)



# Build 1.10.1:
#build_Rstox(dir[[Sys.info()["user"]]]$Rstox, version="1.10.1", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), official=FALSE, suggests=c("ggmap", "ncdf4", "pgirmess", "eca", "plotrix"), check=FALSE)

# Build 1.11:
#build_Rstox(dir[[Sys.info()["user"]]]$Rstox, version="1.11", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), official=TRUE, suggests=c("ggmap", "ncdf4", "pgirmess", "eca", "plotrix"), check=FALSE)



# Build 1.11.1:
#build_Rstox(dir[[Sys.info()["user"]]]$Rstox, version="1.11.1", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), official=FALSE, suggests=c("ggmap", "ncdf4", "pgirmess", "Reca", "plotrix"), check=FALSE)





# Build 1.11.1:
RstoxBuild::buildRstoxPackage("Rstox", version="1.11.1", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), suggests=c("ncdf4", "pgirmess", "Reca", "plotrix"), check=FALSE)

# Before moving to the new account!!!!!!!!!!!!111:
RstoxBuild::buildRstoxPackage("Rstox", version="1.11.1", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), suggests=c("ncdf4", "pgirmess", "Reca", "plotrix"), check=FALSE, githubRoot = "https://github.com/IMR-StoX")





RstoxBuild::buildRstoxPackage("RstoxFramework", version="1.0", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), check=FALSE)

RstoxBuild::buildRstoxPackage("RstoxData", version="1.0", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), check=FALSE)

RstoxBuild::buildRstoxPackage("RstoxECA", version="1.0", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), check=FALSE)

RstoxBuild::buildRstoxPackage("RstoxSurveyPlanner", version="1.0", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), check=FALSE)

RstoxBuild::buildRstoxPackage("RstoxTempdoc", version="1.0", Rversion="3.5", check=FALSE)

RstoxBuild::buildRstoxPackage("RstoxBuild", version="1.0", Rversion="3.5", check=FALSE)




# buildRstoxPackage("RstoxBuild", version="1.0", Rversion="3.5", check=FALSE, rootDir="~/Code/Github")
