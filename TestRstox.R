##### Create and install the package stox in R: #####

##### Load required packages: #####
library(Rstox)
library(png)
library(jpeg)
library(tiff)
library(tools)
library(R.utils)

# Source the utility functions:
#source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")


# Set the directory of the test projects in your local system:
# Note that this path MUST be a path without spaces, and that the project names also need to be without spaces, for the automated testing to funciton on Windows. Unless, "Status 1" errors will occur when the FC command is used.
dir_Rstox <- "~/workspace/stox/project/Test projects/Automated_testing_small"
dir_Rstox_utils <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox_utils/Rstox_utils"
source(file.path(dir_Rstox_utils, "Rstox_utils.R"))


 
##### Install Rstox 1.7, which is the first Rstox version to include in the automatic testing: #####
#dep.pck <- c("data.table", "ggplot2", "pbapply", "rgdal", "rgeos", "rJava", "sp", "XML")
#install.packages(dep.pck, repos="http://cran.us.r-project.org", type="binary")
#install.packages("ftp://ftp.imr.no/StoX/Download/Rstox/Versions/Rstox_1.7/Rstox_1.7.tar.gz", repos=NULL)

##### For Rstox 1.7, run the projects only (no diff): #####
#automatedRstoxTest(dir_Rstox, copyFromOriginal=TRUE, process=c("run"))


##### Install the latest develop version of Rstox: #####
##### This is Rstox 1.7.2: #####
# devtools::install_github("Sea2Data/Rstox", ref="develop")

##### Run the projects and diff with Rstox 1.7: #####
#system.time(automatedRstoxTest(dir_Rstox, copyFromOriginal=TRUE, process=c("run", "diff"), cores=2))


	###### Install Rstox_1.8: #####
	#install.packages("ftp://ftp.imr.no/StoX/Download/Rstox/Rstox_1.8.tar.gz", repos=NULL)

	###### Run the projects with Rstox 1.8 and diff with Rstox 1.7.2: #####
	#system.time(automatedRstoxTest(dir_Rstox, copyFromOriginal=TRUE, process=c("run", "diff"), cores=2))



	##### Install the latest develop version of Rstox: #####
	##### This is Rstox 1.8.1: #####
	# devtools::install_github("Sea2Data/Rstox", ref="develop")

	##### Run the projects with Rstox 1.8.1 and diff with Rstox 1.7.2: #####
	system.time(automatedRstoxTest(dir_Rstox, copyFromOriginal=TRUE, process=c("run", "diff"), cores=2))






	################## RESTART R #####################



##### Create and install the package stox in R: #####

##### Load required packages: #####
library(Rstox)
library(png)
library(jpeg)
library(tiff)
library(tools)
library(R.utils)

# Source the utility functions:
#source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")


# Set the directory of the test projects in your local system:
# Note that this path MUST be a path without spaces, and that the project names also need to be without spaces, for the automated testing to funciton on Windows. Unless, "Status 1" errors will occur when the FC command is used.
dir_Rstox <- "~/workspace/stox/project/Test projects/Automated_testing"
dir_Rstox_utils <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox_utils/Rstox_utils"
source(file.path(dir_Rstox_utils, "Rstox_utils.R"))



	###### Install Rstox_1.8: #####
	#install.packages("ftp://ftp.imr.no/StoX/Download/Rstox/Rstox_1.8.tar.gz", repos=NULL)

	###### Run the projects with Rstox 1.8 and diff with Rstox 1.7.2: #####
	#system.time(automatedRstoxTest(dir_Rstox, copyFromOriginal=TRUE, process=c("run", "diff"), cores=2))



	##### Install the latest develop version of Rstox: #####
	##### This is Rstox 1.8.1: #####
	# devtools::install_github("Sea2Data/Rstox", ref="develop")

	##### Run the projects with Rstox 1.8.1 and diff with Rstox 1.7.2: #####
	system.time(automatedRstoxTest(dir_Rstox, copyFromOriginal=TRUE, process=c("run", "diff"), nlines=100))













##### Create and install the package stox in R: #####

##### Load required packages: #####
library(Rstox)
library(png)
library(jpeg)
library(tiff)
library(tools)
library(R.utils)

# Source the utility functions:
source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")

# Set the directory of the test projects in your local system:
if(.Platform$OS.type == "windows"){
	root <- "\\\\delphi"
}
else{
	root <- "/Volumes"
}

dir_Rstox <- file.path(root, "pc_prog/S2D/stox/StoX_version_test/Automated_testing", getPlatformID())




##########################################
########## RESTART R/Rstudio!!! ##########
##########################################

###### Install Rstox_1.8: #####
install.packages("ftp://ftp.imr.no/StoX/Download/Rstox/Rstox_1.8.tar.gz", repos=NULL)

###### Run the projects with Rstox 1.8 and diff with Rstox 1.7.2: #####
system.time(automatedRstoxTest(dir_Rstox, copyFromOriginal=TRUE, process=c("run", "diff"), cores=2))


##########################################
########## RESTART R/Rstudio!!! ##########
##########################################

##### Install the latest develop version of Rstox: #####
### This is Rstox 1.8.1: #####
# devtools::install_github("Sea2Data/Rstox", ref="develop")

##### Run the projects with Rstox 1.8.1 and diff with Rstox 1.7.2: #####
system.time(automatedRstoxTest(dir_Rstox, copyFromOriginal=TRUE, process=c("run", "diff"), nlines=100))









