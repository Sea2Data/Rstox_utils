# Script for automatic testing of Rstox on a selection of test projects. First working version completed on 2018-04-09:

# Certain rules apply for the test projects:
# 1. There cannot be any spaces (" ") in the project paths
# 2. There cannot be any underscores ("_") in the process names


# Load image packages:
install.packages(c("png", "jpeg", "tiff", "tools", "R.utils", "Rcpp", "rlang"))




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
########## Rstox_1.9.2 ##########
#################################

###### RESTART R/Rstudio!!! ######

##### Install the latest develop version of Rstox: #####
# Install the packages that Rstox depends on. Note that this updates all the specified packages to the latest (binary) version:
dep.pck <- c("data.table", "ggplot2", "pbapply", "rgdal", "rgeos", "rJava", "sp", "XML")
install.packages(dep.pck, repos="http://cran.us.r-project.org", type="binary")

# Install Rstox:
install.packages("ftp://ftp.imr.no/StoX/Download/Rstox/Versions/Alpha/Rstox_1.9.2/Rstox_1.9.2.tar.gz", repos=NULL)

# Install Reca on Windows (on Mac there are several steps required, see GitHub, "Rstox_utils/Work/mac_binaries/README.txt"):
###devtools::install_github("NorskRegnesentral/Reca")
# Install the Reca package you got via Wetransfer:
# ecafolder <- file.path(getProjectPaths()$projectRoot, eca_0.11)
# install.packages(ecafolder, repos=NULL, type="source")


# Load Rstox:
library(Rstox)

##### Run the projects with Rstox 1.9.2 and diff with Rstox 1.8.1: #####
source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")
#source('~/Code/Github/Rstox_utils/Rstox_utils/Rstox_utils.R', chdir = TRUE)


# system.time(automatedRstoxTest(dir="~/workspace/stox/project/Automated_testing", copyFromServer=TRUE, process=c("run", "diff"), nlines=50))
system.time(automatedRstoxTest(copyFromServer=TRUE, process=c("run", "diff"), nlines=50))
#     user   system  elapsed 
# 2044.219  394.894 4639.868 
# New mac
#     user   system  elapsed 
# 3030.232  306.394 2742.444 




# Run this at the office while connected with a cable. 
#copyCurrentToServer(dir="~/workspace/stox/project/Automated_testing", root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoX_version_test/Automated_testing", toCopy=c("Diff", "Output", "Projects_original"), msg=TRUE, n=3)

	