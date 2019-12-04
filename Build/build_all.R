
# Build Rstox 1.11.1:
RstoxBuild::buildRstoxPackage(
	"Rstox", 
	version = "1.11.1", 
	Rversion = "3.5", 
	imports = list(
		data.table = "1.10.4-3", 
		MASS = NULL,
		methods = NULL,
		RColorBrewer = NULL,
		ggplot2 = NULL,
		gridExtra = NULL,
		plotrix = NULL,
		pbapply = NULL,
		rgdal = NULL,
		rgeos = NULL,
		rJava = NULL,
		sp = NULL,
		XML = NULL,
		scatterpie = NULL
	), 
	suggests = c("pgirmess", "Reca"), 
	githubRoot = "https://github.com/Sea2Data", 
	check = FALSE
)


# Build RstoxBase:
RstoxBuild::buildRstoxPackage(
	"RstoxBase", 
	version = "1.0", 
	Rversion = "3.5", 
	imports = list(
		data.table="1.10.4-3", 
		rgdal = "1.4.7",
		rgeos = "0.5.2",
		sp = "1.3.2"
	), 
	suggests = "testthat", 
	check = FALSE
)


# Build RstoxFramework:
RstoxBuild::buildRstoxPackage(
	"RstoxFramework", 
	version = "1.0", 
	Rversion = "3.5", 
	imports = list(
		data.table = "1.12.6", 
		geojsonio = "0.8.0", 
		jsonlite = "1.6", 
		sp = "1.3.2"
	), 
	suggests = "testthat", 
	check = FALSE
)


# Build RstoxData:
RstoxBuild::buildRstoxPackage(
	"RstoxData", 
	version = "1.0", 
	Rversion = "3.5", 
	imports = list(
		data.table = "1.12.6", 
		Rcpp = "1.0.0", 
		xml2 = "1.2.2", 
		readr = "1.3.1"
	), 
	suggests = "testthat", 
	check = FALSE
)


# Build RstoxFDA:
RstoxBuild::buildRstoxPackage(
  "RstoxFDA",
  version = "1.0",
  Rversion = "3.6",
  imports = list(
    stats = "3.5.0",
    methods = "3.5.0",
    utils = "3.5.0",
    readr = "1.3.1",
    data.table = "1.12.6",
    ggplot2 = "3.2.1",
    RColorBrewer = "1.1-2",
    gridExtra = "2.3",
    sp = "1.3.2",
    RstoxData = "0.6.1",
    RstoxBase = "1.0"
  ),
  suggests = c("testthat", "Reca"),
  check = FALSE
)


# Build RstoxTempdoc:
RstoxBuild::buildRstoxPackage(
	"RstoxTempdoc", 
	version = "1.0", 
	Rversion = "3.5", 
	check = FALSE
)


# Build RstoxBuild:
RstoxBuild::buildRstoxPackage(
	"RstoxBuild", 
	version = "1.0", 
	Rversion = "3.5", 
	check = FALSE, 
	imports = c("usethis", "devtools"), 
	suggests = c("Rstox", "png", "jpeg", "tiff", "rJava", "callr")
)
