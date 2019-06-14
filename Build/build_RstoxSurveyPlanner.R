
# Build 1.11.1:
RstoxBuild::build_Rstox_package("Rstox", version="1.11.1", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), suggests=c("ncdf4", "pgirmess", "Reca", "plotrix"), check=FALSE)



RstoxBuild::build_Rstox_package("RstoxFramework", version="1.0", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), check=FALSE)

RstoxBuild::build_Rstox_package("RstoxData", version="1.0", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), check=FALSE)

RstoxBuild::build_Rstox_package("RstoxECA", version="1.0", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), check=FALSE)

RstoxBuild::build_Rstox_package("RstoxSurveyPlanner", version="1.0", Rversion="3.5", pckversion=list(data.table="1.10.4-3"), check=FALSE)

RstoxBuild::build_Rstox_package("RstoxTempdoc", version="1.0", Rversion="3.5", check=FALSE)

RstoxBuild::build_Rstox_package("RstoxBuild", version="1.0", Rversion="3.5", check=FALSE)




# build_Rstox_package("RstoxBuild", version="1.0", Rversion="3.5", check=FALSE, rootDir="~/Code/Github")
