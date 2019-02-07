# Script for automatic testing of Rstox on a selection of test projects. First working version completed on 2018-04-09:

# WARNING: This script MUST be run in plain R, since Sys.info()["release"] returns "10 x64" in plain R and ">=8 x64" in R.app.

# Certain rules apply for the test projects:
# 1. There cannot be any spaces (" ") in the project paths
# 2. There cannot be any underscores ("_") in the process names


# Load image packages:
install.packages(c("png", "jpeg", "tiff", "R.utils", "Rcpp", "rlang"))




###############################
########## Rstox_1.8 ##########
###############################

###### RESTART R/Rstudio!!! ######

###### Install Rstox_1.8: #####

# Install the packages that Rstox depends on. Note that this updates all the specified packages to the latest (binary) version:
dep.pck <- c("data.table", "ggplot2", "pbapply", "rgdal", "rgeos", "rJava", "sp", "XML")
install.packages(dep.pck, repos="http://cran.us.r-project.org", type="binary")

# Install Rstox:
install.packages("ftp://ftp.imr.no/StoX/Download/Rstox/Versions/Rstox_1.8/Rstox_1.8.tar.gz", repos=NULL)
# Load Rstox:
library(Rstox)

###### Run the projects with Rstox 1.8 with no diff: #####
source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")

system.time(automatedRstoxTest(copyFromServer=TRUE, process=c("run", "diff"), nlines=50))
#     user   system  elapsed 
# 1156.712  271.704 3834.652 


#################################
########## Rstox_1.8.1 ##########
#################################

###### RESTART R/Rstudio!!! ######

##### Install the latest develop version of Rstox: #####
# Install the packages that Rstox depends on. Note that this updates all the specified packages to the latest (binary) version:
dep.pck <- c("data.table", "ggplot2", "pbapply", "rgdal", "rgeos", "rJava", "sp", "XML")
install.packages(dep.pck, repos="http://cran.us.r-project.org", type="binary")

# Install Rstox:
install.packages("ftp://ftp.imr.no/StoX/Download/Rstox/Versions/Alpha/Rstox_1.8.1/Rstox_1.8.1.tar.gz", repos=NULL)

# Load Rstox:
library(Rstox)

##### Run the projects with Rstox 1.8.1 and diff with Rstox 1.8: #####
source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")

system.time(automatedRstoxTest(copyFromServer=TRUE, process=c("run", "diff"), nlines=50))
#     user   system  elapsed 
# 2044.219  394.894 4639.868 


#################################
########## Rstox_1.9.3 ##########
#################################

###### RESTART R/Rstudio!!! ######

##### Install the latest develop version of Rstox: #####
# Install the packages that Rstox depends on. Note that this updates all the specified packages to the latest (binary) version:
dep.pck <- c("data.table", "ggplot2", "pbapply", "rgdal", "rgeos", "rJava", "sp", "XML")
install.packages(dep.pck, repos="http://cran.us.r-project.org", type="binary")

# Install Rstox:
install.packages("ftp://ftp.imr.no/StoX/Download/Rstox/Versions/Alpha/Rstox_1.9.3/Rstox_1.9.3.tar.gz", repos=NULL)

# Load Rstox:
library(Rstox)

# Install Reca on Windows (on Mac there are several steps required, see GitHub, "Rstox_utils/Work/mac_binaries/README.txt"):
###devtools::install_github("NorskRegnesentral/Reca")
# Install the Reca package you got via Wetransfer:
# ecafolder <- file.path(getProjectPaths()$projectRoot, "eca_0.11")
# install.packages(ecafolder, repos=NULL, type="source")


##### Run the projects with Rstox 1.9.3 and diff with Rstox 1.8.1: #####
source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")
#source('~/Code/Github/Rstox_utils/Rstox_utils/Rstox_utils.R', chdir = TRUE)


# system.time(automatedRstoxTest(dir="~/workspace/stox/project/Automated_testing", copyFromServer=TRUE, process=c("run", "diff"), nlines=50))
system.time(automatedRstoxTest(copyFromServer=TRUE, process=c("run", "diff"), nlines=50, skipError=TRUE))
#     user   system  elapsed 
# 2044.219  394.894 4639.868 
# New mac
#     user   system  elapsed 
# 3030.232  306.394 2742.444 




# Run this at the office while connected with a cable. 
copyCurrentToServer(toCopy=c("Diff", "Output", "ProjOrig"), msg=TRUE, n=3, overwrite=TRUE)

	