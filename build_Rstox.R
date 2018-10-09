##### Create and install the package stox in R: #####

##### Load devtools: #####
library("devtools")

# Source the utility functions:
#source("https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Rstox_utils.R")


# Define the directory of the working copy:
#arnejh
dir_Rstox <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox/Rstox"
dir_Rstox_utils <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox_utils/Rstox_utils"
source(file.path(dir_Rstox_utils, "Rstox_utils.R"))
# aasmunds
#dir_Rstox <- "C:/Projects/Sea2Data/NewBeam/trunk/beam/StoX/StoX-Main/src/main/resources/stox/system/r"


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

build_Rstox(dir_Rstox, version="1.9.1", Rversion="3.5.0", pckversion=list(data.table="1.10.4-3"), official=FALSE, suggests=c("ggmap", "ncdf4", "pgirmess"), check=FALSE)






