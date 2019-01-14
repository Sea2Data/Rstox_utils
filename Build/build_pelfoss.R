##### Create and install the package stox in R: #####

##### Load devtools: #####
library("devtools")

# Define the directory of the working copy:
dir <- list(
	arnejh = list(
		pelfoss = "~/Code/github/pelfoss/pelfoss/", 
		Rstox_utils = "~/Code/github/Rstox_utils/Rstox_utils/"
	)
)

# Get user:
user <- Sys.info()["user"]

#source(file.path(dir_Rstox_utils, "Rstox_utils.R"))
source(file.path(dir[[user]]$Rstox_utils, "Rstox_utils.R"))

# Build 1.1.1:
#build_pelfoss(dir_Rstox, version="1.1.1", Rversion="3.5.0", official=FALSE, check=FALSE)



# Build 1.1.1:
#build_pelfoss(dir_Rstox, version="1.1.1", Rversion="3.5.0", official=FALSE, check=TRUE)


# Build 1.2:
#build_pelfoss(dir_Rstox, version="1.2", Rversion="3.5.0", official=TRUE, check=TRUE)



# Build 1.2:
#build_pelfoss(dir_Rstox, version="1.2.1", Rversion="3.5.0", official=FALSE, check=TRUE)



# Build 1.2:
build_pelfoss(dir[[user]]$pelfoss, version="1.2.2", Rversion="3.5.0", official=FALSE, check=TRUE)




