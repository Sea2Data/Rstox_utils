# Script for automatic testing of Rstox on a selection of test projects. First working version completed on 2018-04-09:

# Certain rules apply for the test projects:
# 1. There cannot be any spaces (" ") in the project paths
# 2. There cannot be any underscores ("_") in the process names


# Load image packages:
# install.packages("png", "jpeg", "tiff", "tools", "R.utils")


# Define the path to the directory holding the automated testing:
dir <- "~/workspace/stox/project/Test projects/Automated_testing"


###############################
########## Rstox_1.8 ##########
###############################

###### RESTART R/Rstudio!!! ######

###### Install Rstox_1.8: #####
install.packages("ftp://ftp.imr.no/StoX/Download/Rstox/Rstox_1.8.tar.gz", repos=NULL)

###### Run the projects with Rstox 1.8 and diff with Rstox 1.7.2: #####
source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")

system.time(automatedRstoxTest(dir=dir, copyFromServer=TRUE, process=c("run", "diff"), nlines=50))
#     user   system  elapsed 
# 1156.712  271.704 3834.652 


#################################
########## Rstox_1.8.1 ##########
#################################

###### RESTART R/Rstudio!!! ######

##### Install the latest develop version of Rstox: #####
devtools::install_github("Sea2Data/Rstox", ref="develop")

##### Run the projects with Rstox 1.8.1 and diff with Rstox 1.7.2: #####
source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")

system.time(automatedRstoxTest(dir=dir, copyFromServer=FALSE, process=c("run", "diff"), nlines=50))
#     user   system  elapsed 
# 2044.219  394.894 4639.868 





copyCurrentToServer(dir, root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoXAutoTest", toCopy=c("Diff", "Output", "Projects_original"), msg=TRUE, n=2)

	