# Script for automatic testing of Rstox on a selection of test projects. First working version completed on 2018-04-09:

# WARNING: This script MUST be run in plain R, since Sys.info()["release"] returns "10 x64" in plain R and ">=8 x64" in R.app.

# Certain rules apply for the test projects:
# 1. There cannot be any spaces (" ") in the project paths
# 2. There cannot be any underscores ("_") in the process names


# Load image packages:
install.packages(c("png", "jpeg", "tiff", "R.utils", "Rcpp", "rlang"))




#################################
########## Rstox_1.10.1 ##########
#################################

###### RESTART R/Rstudio!!! ######

##### Install the latest develop version of Rstox: #####
# Install the packages that Rstox depends on. Note that this updates all the specified packages to the latest (binary) version:
dep.pck <- c("data.table", "ggplot2", "pbapply", "rgdal", "rgeos", "rJava", "sp", "XML")
install.packages(dep.pck, repos="http://cran.us.r-project.org", type="binary")

# Install Rstox:
install.packages("ftp://ftp.imr.no/StoX/Download/Rstox/Versions/Alpha/Rstox_1.10.1/Rstox_1.10.1.tar.gz", repos=NULL)
devtools::install_github("https://github.com/StoXProject/RstoxBuild")

# Load Rstox:
library(Rstox)
library(RstoxBuild)

# Install Reca on Windows (on Mac there are several steps required, see GitHub, "Rstox_utils/Work/mac_binaries/README.txt"):
###devtools::install_github("NorskRegnesentral/Reca")
# Install the Reca package you got via Wetransfer:
# ecafolder <- file.path(getProjectPaths()$projectRoot, "eca_0.11")
# install.packages(ecafolder, repos=NULL, type="source")


##### Run the projects with Rstox 1.10.1 and diff with Rstox 1.9.4: #####
source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")
#source('~/Code/Github/Rstox_utils/Rstox_utils/Rstox_utils.R', chdir = TRUE)


# system.time(automatedRstoxTest(dir="~/workspace/stox/project/Automated_testing", copyFromServer=TRUE, process=c("run", "diff"), nlines=50))
system.time(automatedRstoxTest(copyFromServer=TRUE, process=c("run", "diff"), nlines=50, skipError=TRUE))



#################################
########## Rstox_1.11.1 ##########
#################################

###### RESTART R/Rstudio!!! ######

##### Install the latest develop version of Rstox: #####
# Install the packages that Rstox depends on. Note that this updates all the specified packages to the latest (binary) version:
dep.pck <- c("data.table", "ggplot2", "pbapply", "rgdal", "rgeos", "rJava", "sp", "XML")
install.packages(dep.pck, repos="http://cran.us.r-project.org", type="binary")

# Install Rstox:
devtools::install_github("Sea2Data/Rstox", ref="develop")
devtools::install_github("https://github.com/StoXProject/RstoxBuild")

# Load Rstox:
library(Rstox)
library(RstoxBuild)

# Install Reca on Windows (on Mac there are several steps required, see GitHub, "Rstox_utils/Work/mac_binaries/README.txt"):
###devtools::install_github("NorskRegnesentral/Reca")
# Install the Reca package you got via Wetransfer:
# ecafolder <- file.path(getProjectPaths()$projectRoot, "eca_0.11")
# install.packages(ecafolder, repos=NULL, type="source")


# system.time(automatedRstoxTest(dir="~/workspace/stox/project/Automated_testing", copyFromServer=TRUE, process=c("run", "diff"), nlines=50))
system.time(automatedRstoxTest(copyFromServer=TRUE, process=c("run", "diff"), nlines=50, skipError=TRUE))









# Run this at the office while connected with a cable. 
copyCurrentToServer(toCopy=c("Diff", "Output", "ProjOrig"), msg=TRUE, n=3, overwrite=TRUE)

