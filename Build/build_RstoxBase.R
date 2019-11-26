# Build 1.0:
RstoxBuild::buildRstoxPackage(
	"RstoxBase", 
	version = "1.0", 
	Rversion = "3.6", 
	imports = list(
		data.table="1.10.4-3", 
		rgdal = "1.4.7",
		rgeos = "0.5.2",
		sp = "1.3.2"
	), 
	suggests = "testthat", 
	check = FALSE
	)
