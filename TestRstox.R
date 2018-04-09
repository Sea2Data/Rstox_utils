# Script for automatic testing of Rstox on a selection of test projects. First working version completed on 2018-04-09:


# Define the path to the directory holding the automated testing:
local_dir <- "~/workspace/stox/project/Test projects/Automated_testing"


###############################
########## Rstox_1.8 ##########
###############################

###### RESTART R/Rstudio!!! ######

###### Install Rstox_1.8: #####
install.packages("ftp://ftp.imr.no/StoX/Download/Rstox/Rstox_1.8.tar.gz", repos=NULL)

###### Run the projects with Rstox 1.8 and diff with Rstox 1.7.2: #####
source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")

system.time(automatedRstoxTest(copyFromOriginal=TRUE, process=c("run", "diff"), nlines=100))


#################################
########## Rstox_1.8.1 ##########
#################################

###### RESTART R/Rstudio!!! ######

##### Install the latest develop version of Rstox: #####
devtools::install_github("Sea2Data/Rstox", ref="develop")

##### Run the projects with Rstox 1.8.1 and diff with Rstox 1.7.2: #####
source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")

# Approximately 1 hour on cabeled network, 4 on VPN:
system.time(automatedRstoxTest(copyFromOriginal=TRUE, process=c("run", "diff"), nlines=100))






source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")


dir <- "~/workspace/stox/project/Test projects/Automated_testing_small-kopi"

system.time(automatedRstoxTest(dir=dir, root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoX_version_test/Automated_testing_small", copyFromOriginal=TRUE, process=c("run", "diff"),  nlines=100))

copyCurrentToServer(dir, root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoX_version_test/Automated_testing_small", toCopy=c("Diff", "Output", "Projects_original"), msg=TRUE)

	