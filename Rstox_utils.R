# Emergenscy create a funciton for building the pelfoss package, that is a copy of the correponding code for the Rstox package but altered to fit the pelfoss package. We should rather work through the :
build_pelfoss <- function(buildDir, pkgName="pelfoss", version="1.0", Rversion="3.3.1", pckversion=list(), official=FALSE, check=FALSE, exportDir=NULL, suggests=NULL) {
	
	########## Functions ##########
	# Function used for writing the README file automatically, including package dependencies, R and pelfoss version and release notes:
	writePelfossREADME <- function(READMEfile, NEWSfile, version, Rversion, betaAlpha, betaAlphaString, imports, official=FALSE){
		# Write pelfoss and R version in the first two lines. THIS SHOULD NEVER BE CHANGED, SINCE STOX READS THESE TWO LINES TO CHECK VERSIONS:
		write(paste0("# pelfoss version: ", version, " (latest ", betaAlphaString, ", ", format(Sys.time(), "%Y-%m-%d"), ")"), READMEfile)
		write(paste0("# R version: ", Rversion), READMEfile, append=TRUE)
		
		write("", READMEfile, append=TRUE)
		# Package description and installation code:
		write("# The package pelfoss contains code to simulate acoustic-trawl surveys based on biomass fields from population dynamic models, and produces survey estimates using the Rstox package:", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# IMPORTANT: The package pelfoss stongly depends on the package Rstox. Install Rstox as described on https://github.com/Sea2Data/Rstox", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# Install the other packages that perlfoss depends on. Note that this updates all the specified packages to the latest (binary) version:", READMEfile, append=TRUE)
		write(paste0("dep.pck <- c(\"", paste0(setdiff(imports, "Rstox"), collapse="\", \""), "\")"), READMEfile, append=TRUE)
		write("install.packages(dep.pck, repos=\"http://cran.us.r-project.org\", type=\"binary\")", READMEfile, append=TRUE)
		
		write("", READMEfile, append=TRUE)
		write("# Install pelfoss from github using the devtools package:", READMEfile, append=TRUE)
		write("devtools::install_github(\"Sea2Data/pelfoss\")", READMEfile, append=TRUE)
		write("# Alternatively install the latest develop version: devtools::install_github(\"Sea2Data/pelfoss\", ref=\"develop\")", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		
		
		# Write release notes:
		write("", READMEfile, append=TRUE)
		write(paste0("# Release notes pelfoss_", version, ":"), READMEfile, append=TRUE)
	
		# Read the changes:
		l <- readLines(NEWSfile)
		# Split into vesions:
		atversion <- which(substr(l, 1, 1) == "#")
		# Strip off "#", and extract the version string:
		versionStringInChanges <- l[atversion]
		startversion <- regexpr("Version ", versionStringInChanges)
		startversion <- startversion + attributes(startversion)$match.length
		versionStringInChanges <- substring(versionStringInChanges, startversion)
		endversion <- regexpr(" ", versionStringInChanges) - 1
		versionStringInChanges <- substr(versionStringInChanges, 1, endversion)
		# Split into versions and format to insert into the README file:
		l <- split(l, findInterval(seq_along(l), c(atversion, length(l)+1)))
		names(l) <- versionStringInChanges
		# Remove the version line:
		l <- lapply(l, function(xx) xx[substr(xx, 1, 1) != "#"])
		thisl <- l[[version]]
		hasText <- which(nchar(thisl)>1)
		thisl[hasText] <- paste0("# ", seq_along(hasText), ". ", thisl[hasText])
		write(thisl, READMEfile, append=TRUE)
		
		if(official){
			NEWSlink <- "https://github.com/Sea2Data/pelfoss/blob/master/NEWS"
		}
		else{
			NEWSlink <- "https://github.com/Sea2Data/pelfoss/blob/alpha/NEWS"
		}
		
		write("", READMEfile, append=TRUE)
		write(paste0("# For historical release notes see ", NEWSlink), READMEfile, append=TRUE)
	
	}
	
	# Functions used for extracting the imports in Rstox, in order to inform about these in the README file. This will not be needed once the package is on CRAN:
	discardBasePackages <- function(x){
		inst <- installed.packages()
		Base <- inst[, "Package"][inst[,"Priority"] %in% c("base", "recommended")]
		sort(setdiff(x, Base))
	}
	getImports <- function(buildDir, version=list()){
		# Read the NAMESPACE file and get the package dependencies:
		buildDirList <- list.files(buildDir, recursive=TRUE, full.names=TRUE)
		imports <- NULL
		if(length(buildDirList)){
			print(buildDirList[basename(buildDirList) == "NAMESPACE"])
			NAMESPACE <- readLines(buildDirList[basename(buildDirList) == "NAMESPACE"])
			atImports <- grep("import", NAMESPACE)
			imports <- NAMESPACE[atImports]
			imports <- sapply(strsplit(imports, "(", fixed=TRUE), "[", 2)
			imports <- sapply(strsplit(imports, ")", fixed=TRUE), "[", 1)
			imports <- unique(sapply(strsplit(imports, ",", fixed=TRUE), "[", 1))
			#inst <- installed.packages()
			#Base <- inst[, "Package"][inst[,"Priority"] %in% c("base", "recommended")]
			#imports <- sort(setdiff(imports, Base))
			imports <- discardBasePackages(imports)
			#notBase <- inst[, "Package"][!(inst[,"Priority"]) %in% "base"]
			#imports <- sort(imports[!imports %in% notBase])
		}
		
		# Add required version for packages:
		if(length(version)){
			# Remove version information for packages that are not present in the imports:
			if(!all(names(version) %in% imports)){
				warning("Not all package versions specified in 'version' are present as imports.")
			}
			version <- version[names(version) %in% imports]
			atversion <- match(names(version), imports)
			for(i in seq_along(version)){
				imports[atversion[i]] <- paste0(imports[atversion[i]], " (>= ", version[i], ")")
			}
		}
		return(imports)
	}
	########## End of functions ##########
	
	# Clear the installed package:
	try(lapply(.libPaths(), function(xx) remove.packages(pkgName, xx)), silent=TRUE)
	
	if(length(exportDir)==0){
		exportDir <- file.path(dirname(buildDir), "builds")
	}
	if(length(grep(exportDir, buildDir))>0){
		stop("The 'exportDir' cannot be contained in the 'buildDir', since the exports are build from the 'buildDir'")
	}
	#exportDir <- file.path(buildDir, "bundle")
	manDir <- file.path(buildDir, "man")
	DESCRIPTIONfile <- file.path(buildDir, "DESCRIPTION")
	NAMESPACEfile <- file.path(buildDir, "NAMESPACE")
		unlink(NAMESPACEfile, recursive=TRUE, force=TRUE)
	onLoadFile = file.path(buildDir, "R", "onLoad.R")
	onAttachFile = file.path(buildDir, "R", "onAttach.R")
	
	# Changed added to make the package name identical to the name of the GitHub release:
	thisExportDir <- file.path(exportDir, paste(pkgName, version, sep="_"))
	suppressWarnings(dir.create(thisExportDir))
	READMEfile <- file.path(buildDir, "README")
	
	READMEfileExport <- file.path(thisExportDir, "README")
	NEWSfile <- file.path(buildDir, "NEWS")
	
	
	##### Save the following content to the onLoad.R file in the "R" directory: #####
	onLoadText = paste(
		".onLoad <- function(libname, pkgname){}", sep="\n")
	write(onLoadText, onLoadFile)
	##########
	
	##### Save a Java memory message to the onAttach.R file in the "R" directory: #####
	onAttachText = paste(
		".onAttach <- function(libname, pkgname){}", sep="\n")
	write(onAttachText, onAttachFile)
	##########
	
	##### Add required fields to the DESCRIPTION file (below is the full content of the DESCRIPTION file): #####
	# Depends is replaced by @import specified by functions"
	DESCRIPTIONtext = paste(
		paste0("Package: ", pkgName),
		"Title: Simulate acosutic-trawl surveys and produce survey estimated using Rstox",
		paste0("Version: ", version),
		"Authors@R: c(",
		"  person(\"Arne Johannes\", \"Holmin\", role = c(\"aut\",\"cre\"), email = \"arnejh@imr.no\"))",
		"Author: Arne Johannes Holmin [aut, cre]",
		"Maintainer: Arne Johannes Holmin <arnejh@imr.no>",
		paste0("Depends: R (>= ", Rversion, ")"), 
		"Description: The package pelfoss contains code to simulate acoustic-trawl surveys based on biomass fields from population dynamic models, and produces survey estimates using the Rstox package.",
		"BugReports: https://github.com/Sea2Data/pelfoss/issues", 
		"License: LGPL-3",
		"LazyData: true\n", sep="\n")
	write(DESCRIPTIONtext, DESCRIPTIONfile)
	##########
	
	##### Create documentation: #####
	# Remove current documentation first:
	unlink(manDir, recursive=TRUE, force=TRUE)
	document(buildDir)
	
	# Alter the DESCRIPTION file to contain the imports listed in the NAMESPACE file:
	imports <- getImports(buildDir, version=pckversion)
	if(length(imports)){
		cat("Imports:\n		", file=DESCRIPTIONfile, append=TRUE)
		cat(paste(imports, collapse=",\n		"), file=DESCRIPTIONfile, append=TRUE)
		cat("", file=DESCRIPTIONfile, append=TRUE)
	}
	# Add also the suggests:
	if(length(suggests)){
		lapply(suggests, devtools::use_package, type="suggests", pkg=buildDir)
	}
	##########
	
	##### Run R cmd check with devtools: #####
	if(check){
		devtools::check(pkg=buildDir)
	}
	##########
	
	### Generate the README file: ###
	betaAlpha <- length(gregexpr(".", version, fixed=TRUE)[[1]]) + 1
	betaAlphaString <- c("", "beta", "alpha")[betaAlpha]
	# Read the NAMESPACE file and get the package dependencies. This is needed since we are not on CRAN:
	writePelfossREADME(READMEfile, NEWSfile, version, Rversion, betaAlpha, betaAlphaString, imports=getImports(buildDir), official=official)
	file.copy(READMEfile, READMEfileExport, overwrite=TRUE)
	##########
	
	##### Create platform independent bundle of source package: #####
	dir.create(thisExportDir, recursive=TRUE)
	pkgFileVer <- build(buildDir, path=thisExportDir)
	# To comply with GitHub, rename to using hyphen (whereas build() hardcodes using "_"):
	versionString <- paste0("pelfoss_", version, ".tar.gz")
	pkgFileVerHyphen <- file.path(thisExportDir, versionString)
	file.rename(pkgFileVer, pkgFileVerHyphen)
	
	##### Unload the package: #####
	unload(buildDir)
	##########
	
	##### Install local source package by utils (independent of dev-tools), and check that it loads: #####
	install.packages(pkgFileVerHyphen, repos=NULL, type="source", lib=.libPaths()[1])
	library(pelfoss)
	##########
}


# Function used for building and testing the Rstox package. 
# Use this in the continous development of Rstox. 
# Rstox can also be built from the develop brach of Sea2Data/Rstox, but the function build_Rstox() generates the README and DESCRIPTION file, treats dependencies and tests the package and examples if check=TRUE:
build_Rstox <- function(buildDir, pkgName="Rstox", version="1.0", Rversion="3.3.1", pckversion=list(), official=FALSE, check=FALSE, exportDir=NULL, suggests=NULL) {
	
	########## Functions ##########
	# Function used for writing the README file automatically, including package dependencies, R and Rstox version and release notes:
	writeRstoxREADME <- function(READMEfile, NEWSfile, version, Rversion, betaAlpha, betaAlphaString, imports, official=FALSE){
		# Write Rstox and R version in the first two lines. THIS SHOULD NEVER BE CHANGED, SINCE STOX READS THESE TWO LINES TO CHECK VERSIONS:
		write(paste0("# Rstox version: ", version, " (latest ", betaAlphaString, ", ", format(Sys.time(), "%Y-%m-%d"), ")"), READMEfile)
		write(paste0("# R version: ", Rversion), READMEfile, append=TRUE)
		
		write("", READMEfile, append=TRUE)
		# Package description and installation code:
		write("# The package Rstox contains most of the functionality of the stock assesment utility StoX, which is an open source approach to acoustic and swept area survey calculations. Download Rstox from ftp://ftp.imr.no/StoX/Download/Rstox or install by running the following commands in R:", READMEfile, append=TRUE)
		
		write("", READMEfile, append=TRUE)
		write("# Install the packages that Rstox depends on. Note that this updates all the specified packages to the latest (binary) version:", READMEfile, append=TRUE)
		write(paste0("dep.pck <- c(\"", paste0(imports, collapse="\", \""), "\")"), READMEfile, append=TRUE)
		# WARNING: IT IS CRUSIAL TO ENCLUDE THE repos IN THIS CALL, FOR STOX TO SOURCE THE README FILE PROPERLY (RESULTS IN AN ERROR IF ABSENT) IT SEEMS "R CMD BATCH source(TheReadMeFile)" RETURNS AN ERROR WHEN repos IS NOT SET (2016-12-16):
		write("install.packages(dep.pck, repos=\"http://cran.us.r-project.org\", type=\"binary\")", READMEfile, append=TRUE)
		#write("install.packages(dep.pck, type=\"binary\")", READMEfile, append=TRUE)
		
		write("", READMEfile, append=TRUE)
		write("# Install Rstox:", READMEfile, append=TRUE)
		# Get the version string, the name of the Rstox tar file, the ftp root and, finally, the ftp directory and full path to the Rstox tar file:
		# Changed added to make the package name identical to the name of the GitHub release:
		#versionString <- paste0("Rstox_", version)
		versionString <- paste("Rstox", version, sep="_")
		tarName <- paste0(versionString, ".tar.gz")
		ftpRoot <- "ftp://ftp.imr.no/StoX/Download/Rstox"
		if(betaAlpha==3){
			ftpDir <- file.path(ftpRoot, "Versions", "Alpha", versionString)
		}
		else{
			if(official){
				ftpDir <- ftpRoot
			}
			else{
				ftpDir <- file.path(ftpRoot, "Versions", versionString)
			}
		}
		tarFile <- file.path(ftpDir, tarName)
		# Write the Rstox install command:
		write(paste0("install.packages(\"", tarFile, "\", repos=NULL)"), READMEfile, append=TRUE)
		
		write("", READMEfile, append=TRUE)
		write("# Alternatively, install the latest development version from GitHub.", READMEfile, append=TRUE)
		write(paste0("# Note that this does not guarantee a stable version."), READMEfile, append=TRUE)
		write(paste0("# For official versions of Rstox, refer to the ftp server ", ftpDir, " as described above."), READMEfile, append=TRUE)
		write("# Install from github using the devtools package:", READMEfile, append=TRUE)
		write("# devtools::install_github(\"Sea2Data/Rstox\", ref=\"develop\")", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# R should be installed as the 64 bit version. On Windows 10, ONLY the 64 bit version should be used.", READMEfile, append=TRUE)
		write("# To do this, uncheck the box \"32-bit Files\" when selecting components to install.", READMEfile, append=TRUE)
		write("# If you are re-installing an R that has both 32 and 64 bit, you will need to uninstall R first.", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# On Windows systems with adminstrator requirements, it is recommended to install R in C:/users/<user>/documents/R.", READMEfile, append=TRUE)
		write("# Also if you are using Rstudio, please make sure that you are using the correct R version (in case you have", READMEfile, append=TRUE)
		write("# multiple versions installed). The R version can be selected in Tools > Global Options.", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# Note that 64 bit Java is required to run Rstox", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# On Windows, install Java from this webpage: https://www.java.com/en/download/windows-64bit.jsp,", READMEfile, append=TRUE)
		write("# or follow the instructions found on ftp://ftp.imr.no/StoX/Tutorials/", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# On Mac, getting Java and Rstox to communicate can be challenging.", READMEfile, append=TRUE)
		write("# If you run into problems such as \"Unsupported major.minor version ...\", try the following:", READMEfile, append=TRUE)
		write("# Update java, on", READMEfile, append=TRUE)
		write("# \thttp://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html", READMEfile, append=TRUE)
		write("# If this does not work install first the JDK and then the JRE:", READMEfile, append=TRUE)
		write("# \thttp://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html", READMEfile, append=TRUE)
		write("# \thttp://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html", READMEfile, append=TRUE)
		write("# Rstox sohuld also work with Java 11, presently available only as Development Kit:", READMEfile, append=TRUE)
		write("# \thttps://www.oracle.com/technetwork/java/javase/downloads/jdk11-downloads-5066655.html", READMEfile, append=TRUE)
		write("# You may want to check that the downloaded version is first in the list by running the following in the Terminal:", READMEfile, append=TRUE)
		write("# \t/usr/libexec/java_home -V", READMEfile, append=TRUE)
		write("# \tjava -version", READMEfile, append=TRUE)
		write("# Then run this in the Terminal.app (you will be asked for password, but the password will not show as you type.", READMEfile, append=TRUE)
		write("# It is possible to type the password in a text editor first and then paste it into the Terminal.):", READMEfile, append=TRUE)
		write("# \tsudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib", READMEfile, append=TRUE)
		write("# \tsudo R CMD javareconf", READMEfile, append=TRUE)
		write("# Open R (close and then open if already open) and install rJava:", READMEfile, append=TRUE)
		#write("# \tinstall.packages('rJava', type='source')", READMEfile, append=TRUE)
		write("# \tinstall.packages('rJava', type=\"binary\")", READMEfile, append=TRUE)
		write("# If this fails, try installing from source instead using install.packages('rJava', type='source')", READMEfile, append=TRUE)
		write("# Then the installed Rstox should work.", READMEfile, append=TRUE)
		
		# Write release notes:
		write("", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write(paste0("# Release notes Rstox_", version, ":"), READMEfile, append=TRUE)
	
		# Read the changes:
		l <- readLines(NEWSfile)
		# Split into vesions:
		atversion <- which(substr(l, 1, 1) == "#")
		# Strip off "#", and extract the version string:
		versionStringInChanges <- l[atversion]
		startversion <- regexpr("Version ", versionStringInChanges)
		startversion <- startversion + attributes(startversion)$match.length
		versionStringInChanges <- substring(versionStringInChanges, startversion)
		endversion <- regexpr(" ", versionStringInChanges) - 1
		versionStringInChanges <- substr(versionStringInChanges, 1, endversion)
		# Split into versions and format to insert into the README file:
		l <- split(l, findInterval(seq_along(l), c(atversion, length(l)+1)))
		names(l) <- versionStringInChanges
		# Remove the version line:
		l <- lapply(l, function(xx) xx[substr(xx, 1, 1) != "#"])
		thisl <- l[[version]]
		hasText <- which(nchar(thisl)>1)
		thisl[hasText] <- paste0("# ", seq_along(hasText), ". ", thisl[hasText])
		write(thisl, READMEfile, append=TRUE)
		
		if(official){
			NEWSlink <- "https://github.com/Sea2Data/Rstox/blob/master/NEWS"
		}
		else{
			NEWSlink <- "https://github.com/Sea2Data/Rstox/blob/alpha/NEWS"
		}
		
		write("", READMEfile, append=TRUE)
		write(paste0("# For historical release notes see ", NEWSlink), READMEfile, append=TRUE)
	
	}
	
	# Functions used for extracting the imports in Rstox, in order to inform about these in the README file. This will not be needed once the package is on CRAN:
	discardBasePackages <- function(x){
		inst <- installed.packages()
		Base <- inst[, "Package"][inst[,"Priority"] %in% c("base", "recommended")]
		sort(setdiff(x, Base))
	}
	getImports <- function(buildDir, version=list()){
		# Read the NAMESPACE file and get the package dependencies:
		buildDirList <- list.files(buildDir, recursive=TRUE, full.names=TRUE)
		imports <- NULL
		if(length(buildDirList)){
			print(buildDirList[basename(buildDirList) == "NAMESPACE"])
			NAMESPACE <- readLines(buildDirList[basename(buildDirList) == "NAMESPACE"])
			atImports <- grep("import", NAMESPACE)
			imports <- NAMESPACE[atImports]
			imports <- sapply(strsplit(imports, "(", fixed=TRUE), "[", 2)
			imports <- sapply(strsplit(imports, ")", fixed=TRUE), "[", 1)
			imports <- unique(sapply(strsplit(imports, ",", fixed=TRUE), "[", 1))
			#inst <- installed.packages()
			#Base <- inst[, "Package"][inst[,"Priority"] %in% c("base", "recommended")]
			#imports <- sort(setdiff(imports, Base))
			imports <- discardBasePackages(imports)
			#notBase <- inst[, "Package"][!(inst[,"Priority"]) %in% "base"]
			#imports <- sort(imports[!imports %in% notBase])
		}
		
		# Add required version for packages:
		if(length(version)){
			# Remove version information for packages that are not present in the imports:
			if(!all(names(version) %in% imports)){
				warning("Not all package versions specified in 'version' are present as imports.")
			}
			version <- version[names(version) %in% imports]
			atversion <- match(names(version), imports)
			for(i in seq_along(version)){
				imports[atversion[i]] <- paste0(imports[atversion[i]], " (>= ", version[i], ")")
			}
		}
		return(imports)
	}
	########## End of functions ##########
	
	
	# Clear the installed package:
	try(lapply(.libPaths(), function(xx) remove.packages(pkgName, xx)), silent=TRUE)
	
	if(length(exportDir)==0){
		exportDir <- file.path(dirname(buildDir), "builds")
	}
	if(length(grep(exportDir, buildDir))>0){
		stop("The 'exportDir' cannot be contained in the 'buildDir', since the exports are build from the 'buildDir'")
	}
	#exportDir <- file.path(buildDir, "bundle")
	manDir <- file.path(buildDir, "man")
	DESCRIPTIONfile <- file.path(buildDir, "DESCRIPTION")
	NAMESPACEfile <- file.path(buildDir, "NAMESPACE")
		unlink(NAMESPACEfile, recursive=TRUE, force=TRUE)
	onLoadFile = file.path(buildDir, "R", "onLoad.R")
	onAttachFile = file.path(buildDir, "R", "onAttach.R")
	
	# Changed added to make the package name identical to the name of the GitHub release:
	thisExportDir <- file.path(exportDir, paste(pkgName, version, sep="_"))
	suppressWarnings(dir.create(thisExportDir))
	READMEfile <- file.path(buildDir, "README")
	
	READMEfileExport <- file.path(thisExportDir, "README")
	NEWSfile <- file.path(buildDir, "NEWS")
	
	
	##### Save the following content to the onLoad.R file in the "R" directory: #####
	# JAVA_HOME is unset to be able to load rJava.dll in R CMD BATCH
	# jPackage is moved to rstox.init for dynamic import of rJava
	# The local Rstox environment is created here, in which all useful outputs from functions are placed, and saved at the end of any code:
	#onLoadText = paste(
	#	".onLoad <- function(libname, pkgname){",
	#	"	",
	#	"	if(Sys.getenv(\"JAVA_HOME\")!=\"\") Sys.setenv(JAVA_HOME=\"\")",
	#	"	# options(java.parameters=\"-Xmx2g\")",
	#	"# Create a Rstox environment in which the baseline objects of the various projects are placed. This allows for a check for previously run baseline models and avoids memory leakage:", 
	#	"	assign(\"RstoxEnv\", new.env(), envir=.GlobalEnv)",
	#	"	# Assign fundamental variables to the RstoxEnv:",
	#	"	Definitions <- list(",
	#	"		JavaMem = 2e9, ",
	#	"		StoXFolders = c(\"input\", \"output\", \"process\"), ",
	#	"		NMD_data_types = c(\"echosounder\", \"biotic\", \"landing\"), ",
	#	"		StoX_data_types = c(\"acoustic\", \"biotic\", \"landing\"), ",
	#	"		StoX_data_type_keys = c(acoustic=\"echosounder_dataset\", biotic=\"missions xmlns\", landing=\"Sluttseddel\"), ",
	#	"		project_types = c(\"AcousticTrawl\", \"SweptAreaLength\", \"SweptAreaTotal\"), ",
	#	"		processLevels = c(\"bootstrap\", \"bootstrapImpute\"), ",
	#	"		modelTypeJavaNames = c(\"baseline\", \"baseline-report\", \"r\", \"r-report\", \"name\"), ",
	#	"		modelTypeJavaFuns = c(\"getBaseline\", \"getBaselineReport\", \"getRModel\", \"getRModelReport\", \"getProjectName\")",
	#	"		)",
	#	"	assign(\"Definitions\", Definitions, envir=get(\"RstoxEnv\"))",
	#	"	assign(\"Projects\", list(), envir=get(\"RstoxEnv\"))",
	#	"	# Set the Java memory:",
	#	"	setJavaMemory(Definitions$JavaMem)", 
	#	"	print(initiateRstoxEnv)", 
	#	"}", 
	#	sep="\n"
	#)
	onLoadText = paste(
		".onLoad <- function(libname, pkgname){",
		"	",
		"	if(Sys.getenv(\"JAVA_HOME\")!=\"\") Sys.setenv(JAVA_HOME=\"\")",
		"	# options(java.parameters=\"-Xmx2g\")",
		"	# Initiate the Rstox envitonment:", 
		"	Definitions <- initiateRstoxEnv()", 
		"	# Set the Java memory:",
		"	setJavaMemory(Definitions$JavaMem)", 
		"}", 
		sep="\n"
	)
	write(onLoadText, onLoadFile)
	##########
	
	##########
	# Define the onAttach funciton for the Rstox package:
	if(official){
		onAttachText = paste(
			".onAttach <- function(libname, pkgname){",
			"	",
			paste0("	packageStartupMessage(\"", pkgName, "_", version, "\n**********\nIf problems with Java Memory such as java.lang.OutOfMemoryError occurs, see ?setJavaMemory.\n**********\n\", appendLF=FALSE)"),
			"}", 
			sep="\n"
		)
	}
	else{
		onAttachText = paste(
			".onAttach <- function(libname, pkgname){",
			"	",
			paste0("	packageStartupMessage(\"", pkgName, "_", version, "\n**********\nWARNING: This version of Rstox is an unofficial/developer version and bugs should be expected.\nIf problems with Java Memory such as java.lang.OutOfMemoryError occurs, see ?setJavaMemory.\n**********\n\", appendLF=FALSE)"),
			"}", 
			sep="\n"
		)
	}
	write(onAttachText, onAttachFile)
	##########
	
	##### Add required fields to the DESCRIPTION file (below is the full content of the DESCRIPTION file): #####
	# Depends is replaced by @import specified by functions"
	DESCRIPTIONtext = paste(
		paste0("Package: ", pkgName),
		"Title: Running Stox functionality independently in R",
		paste0("Version: ", version),
		"Authors@R: c(",
		"  person(\"Arne Johannes\", \"Holmin\", role = c(\"aut\",\"cre\"), email = \"arnejh@imr.no\"),",
		"  person(\"Edvin\", \"Fuglebakk\", role = \"ctb\"),",
		"  person(\"Gjert Endre\", \"Dingsoer\", role = \"ctb\"),",
		"  person(\"Aasmund\", \"Skaalevik\", role = \"ctb\"),",
		"  person(\"Espen\", \"Johnsen\", role = \"ctb\"))",
		"Author: Arne Johannes Holmin [aut, cre],",
		"  Edvin Fuglebakk [ctr],",
		"  Gjert Endre Dingsoer [ctr],",
		"  Aasmund Skaalevik [ctr],",
		"  Espen Johnsen [ctr]",
		"Maintainer: Arne Johannes Holmin <arnejh@imr.no>",
		paste0("Depends: R (>= ", Rversion, ")"), 
		"Description: This package contains most of the functionality of the StoX software, which is used for assessment of fish and other marine resources based on biotic and acoustic survey and landings data, among other uses. Rstox is intended for further analyses of such data, facilitating iterations over an arbitrary number of parameter values and data sets.",
		"BugReports: https://github.com/Sea2Data/Rstox/issues", 
		"License: LGPL-3",
		"LazyData: true", sep="\n")
	write(DESCRIPTIONtext, DESCRIPTIONfile)
	##########
	
	##### Create documentation: #####
	# Remove current documentation first:
	unlink(manDir, recursive=TRUE, force=TRUE)
	document(buildDir)
	
	# Alter the DESCRIPTION file to contain the imports listed in the NAMESPACE file:
	imports <- getImports(buildDir, version=pckversion)
	if(length(imports)){
		cat("Imports:\n		", file=DESCRIPTIONfile, append=TRUE)
		cat(paste(imports, collapse=",\n		"), file=DESCRIPTIONfile, append=TRUE)
		cat("", file=DESCRIPTIONfile, append=TRUE)
	}
	# Add also the suggests:
	if(length(suggests)){
		lapply(suggests, devtools::use_package, type="suggests", pkg=buildDir)
	}
	##########
	
	##### Run R cmd check with devtools: #####
	if(check){
		devtools::check(pkg=buildDir)
		# args = "--no-examples"
	}
	##########
	
	### Generate the README file: ###
	betaAlpha <- length(gregexpr(".", version, fixed=TRUE)[[1]]) + 1
	betaAlphaString <- c("", "beta", "alpha")[betaAlpha]
	# Read the NAMESPACE file and get the package dependencies. This is needed since we are not on CRAN:
	writeRstoxREADME(READMEfile, NEWSfile, version, Rversion, betaAlpha, betaAlphaString, imports=getImports(buildDir), official=official)
	file.copy(READMEfile, READMEfileExport, overwrite=TRUE)
	##########
	
	##### Create platform independent bundle of source package: #####
	dir.create(thisExportDir, recursive=TRUE)
	pkgFileVer <- build(buildDir, path=thisExportDir)
	# To comply with GitHub, rename to using hyphen (whereas build() hardcodes using "_"):
	versionString <- paste0("Rstox_", version, ".tar.gz")
	pkgFileVerHyphen <- file.path(thisExportDir, versionString)
	file.rename(pkgFileVer, pkgFileVerHyphen)
	
	##### Unload the package: #####
	unload(buildDir)
	##########
	
	##### Install local source package by utils (independent of dev-tools), and check that it loads: #####
	install.packages(pkgFileVerHyphen, repos=NULL, type="source", lib=.libPaths()[1])
	library(Rstox)
	##########
}









#*********************************************
#*********************************************
#' Get the platform ID of an operating system
#'
#' @param var	The element of Sys.info() used as identifyer of the platform.
#'
#' @export
#' @keywords internal
#'
getPlatformID <- function(var="release"){
	paste(.Platform$OS.type, var, paste(strsplit(Sys.info()[var], " ", fixed=TRUE)[[1]], collapse="_"), sep="_")
}

#*********************************************
#*********************************************
#' Small funciton to get a list of the file paths to the sub folders of the test directory (Projects_original, Projects, Output, Diff)
#'
#' @param x	The test directory
#'
#' @export
#' @keywords internal
#'
getTestFolderStructure <- function(x){
	
	# Accept the platform specific directory:
	if("Projects_original" %in% list.dirs(x, recursive=FALSE, full.names=FALSE)){
		x <- dirname(x)
	}
	
	platformFolderName <- getPlatformID()
	
	list(
		Staged_Projects_original = file.path(x, "Staged_Projects_original"), 
		Projects_original = file.path(x, platformFolderName, "Projects_original"), 
		Projects_original1 = file.path(x, platformFolderName, "Projects_original", "Rstox_1.0_StoXLib_1.0"), 
		Projects = file.path(x, platformFolderName, "Projects"), 
		Output = file.path(x, platformFolderName, "Output"), 
		Diff = file.path(x, platformFolderName, "Diff"))
}

#*********************************************
#*********************************************
#' Get the latest directory of 
#'
#' @param dir	The directory of subdirectories named with the Rstox and stox-lib versions.
#' @param op	The operator used to get the latest. Do not mess with this unless you know what you are doing.
#' @param n		The number of directories to return, as used in utils::tail().
#'
#' @export
#' @keywords internal
#'
getLatestDir_old <- function(dir, op="<", n=1){
	
	# Function for converting the Rstox version to a numeric value suitable for sorting, by multiplying each digit in the version number by scaling factor which are largest for the first digits (e.g., Rstox_1.10.3 gives 1 * 1e4 + 10 * 1e2 + 3 = 11003):
	version2numeric <- function(x){
		x <- lapply(strsplit(x, ".", fixed=TRUE), as.numeric)
		x <- sapply(x, function(y) sum(y * 10^(6 - 2 * seq_along(y))))
		x
	}
	
	if(length(dir)==0){
		return(NULL)
	}
	# Get the Rstox and stox-lib versions:
	current <- lapply(getRstoxVersion(), as.character)
	currentString <- paste(current, sep="_", collapse="_")
	currentRstox <- version2numeric(current$Rstox)
	currentStoXLib <- version2numeric(current$StoXLib)
	# Weight Rstox higher:
	current <- 10^10 * currentRstox + currentStoXLib
	
	# List all directories:
	All <- list.dirs(dir, recursive=FALSE)
	if(length(All)==0){
		warning(paste0("No projects in the test folder '", dir, "'"))
	}
	
	# Get the Rstox and stox-lib versions encoded in the folder names:
	RstoxVersions <- sapply(strsplit(basename(All), "_"), "[", 2)
	if(length(RstoxVersions)==0){
		return(NULL)
	}
	StoXLibVersions <- sapply(strsplit(basename(All), "_"), "[", 4)
	RstoxVersions <- version2numeric(RstoxVersions)
	StoXLibVersions <- version2numeric(StoXLibVersions)
	# Weight Rstox higher:
	Versions <- 10^10 * RstoxVersions + StoXLibVersions
	
	# There has to be at least one previous version:
	latest <- do.call(op, list(Versions, current))
	if(!any(is.na(latest)) && any(latest)){
		#return(All[max(which(latest))])
		return(All[tail(sort(which(latest)), n)])
	}
	else{
		warning(paste0("No directories with Rstox version before Rstox_StoXLib version \"", currentString, "\" in the directory \"", dir, "\""))
		return(NULL)
	}
}

getLatestDir <- function(dir, op="<", n=1){
	
	# Function for converting the Rstox version to a numeric value suitable for sorting, by multiplying each digit in the version number by scaling factor which are largest for the first digits (e.g., Rstox_1.10.3 gives 1 * 1e4 + 10 * 1e2 + 3 = 11003):
	version2numeric <- function(x){
		x <- lapply(strsplit(x, ".", fixed=TRUE), as.numeric)
		x <- sapply(x, function(y) sum(y * 10^(6 - 2 * seq_along(y))))
		x
	}
	extractVersionstrings <- function(x){
		x[seq(2, length(x), by=2)]
	}
	versionScaled <- function(x){
		sum(x * 10^(10 * seq(length(x) - 1, 0)))
	}
	
	if(length(dir)==0){
		return(NULL)
	}
	# Get the Rstox and stox-lib versions:
	current <- sapply(getRstoxVersion(), as.character)
	currentString <- paste(current, sep="_", collapse="_")
	currentNumeric <- sapply(current, version2numeric)
	current <- versionScaled(currentNumeric)
	
	# List all directories:
	All <- list.dirs(dir, recursive=FALSE)
	All <- All[grep("Rstox", All)]
	if(length(All)==0){
		warning(paste0("No projects in the test folder '", dir, "'"))
	}
	
	# Get the Rstox and stox-lib versions encoded in the folder names:
	versions <- strsplit(basename(All), "_")
	# Pick out every other element, which are the version strings:
	versionStrings <- lapply(versions, extractVersionstrings)
	versionStrings <- lapply(versionStrings, version2numeric)
	versions <- sapply(versionStrings, versionScaled)
	
	# There has to be at least one previous version:
	latest <- do.call(op, list(versions, current))
	if(!any(is.na(latest)) && any(latest)){
		#return(All[max(which(latest))])
		# Allow for specifying e.g. the third latest:
		if(is.integer(n)){
			atlatest <- sort(which(latest))
			out <- All[atlatest[length(atlatest) - n + 1]]
		}
		else{
			out <- All[tail(sort(which(latest)), n)]
		}
		return(out)
	}
	else{
		warning(paste0("No directories with Rstox version before Rstox_StoXLib version \"", currentString, "\" in the directory \"", dir, "\""))
		return(NULL)
	}
}



#*********************************************
#*********************************************
#' Copy the 'n' latest directories of the folders 'toCopy' from the local 'from' to the central 'to' directory.
#'
#' @param from		The local directory holding the version testing.
#' @param to		The central directory holding the version testing.
#' @param toCopy	The sub folders to copy files from.
#' @param overwrite	Logical: If TRUE, overwrite the files on the central directory.
#' @param msg		Logical: If TRUE, print progress to the console.
#' @param op,n		See \code{\link{getLatestDir}}.
#'
#' @export
#' @keywords internal
#'
copyLatestToServer <- function(local, server, toCopy=c("Diff", "Output", "Projects_original"), overwrite=TRUE, msg=FALSE, op="<", n=1){
	
	# Function for copying from one subdirectory:
	copyLatestOne <- function(folder, local, server, overwrite=TRUE, msg=FALSE, op=op, n=1){
		local <- getLatestDir(local[[folder]], op=op, n=n)
		if(length(local)){
			# Check for the existence of the folder (as opposed to using 'overwrite' in file.copy(), which copies all files which do not exist in the destination).
			if(file.exists(server[[folder]]) && !overwrite){
				warning(paste0("The folder ", server[[folder]], " already exists and was not overwritten. Use overwrite=TRUE to overwrite from ", local))
			}
			else{
				temp <- file.copy(local, server[[folder]], recursive=TRUE, overwrite=overwrite)
				if(msg){
					cat("Copied", local, "to", server[[folder]], "\n")
				}
			}
		}
	}
	
	# Get the folder structure of the local and central directory:
	local <- getTestFolderStructure(path.expand(local))
	server <- getTestFolderStructure(path.expand(server))
	
	# Copy for all specified subdirectories:
	invisible(lapply(toCopy, copyLatestOne, local, server, overwrite=overwrite, msg=msg))
}
copyStaged_Projects_original <- function(server, local, overwrite=TRUE, op="<", n=1){
	
	local <- getTestFolderStructure(path.expand(local))$Staged_Projects_original
	server <- getTestFolderStructure(path.expand(server))$Staged_Projects_original
	
	# Get the latest local folder, to which staged projects on the server will be copied:
	localLatest <- getLatestDir(local, op="<", n=1)
	
	# Look for the corresponding folder on the server:
	serverLatest <- file.path(server, basename(localLatest))
	
	
	# Copy if 'serverLatest' exists and is not empty:
	serverLatestDirs <- list.dirs(serverLatest, recursive=FALSE)
	if(length(serverLatestDirs)){
		# Delete the local Staged_Projects_original:
		unlink(localLatest, recursive=TRUE, force=TRUE)
	
		message("Copying the following projects from the server to the local system:\n\t", paste(serverLatestDirs, collapse="\n\t"))
	
		file.copy(serverLatest, local, recursive=TRUE, overwrite=overwrite)
	}
	else{
		message("No staged original projects copied from the server to the local system")
	}
	
	
	### # Delete the local Staged_Projects_original:
	### unlink(local, recursive=TRUE, force=TRUE)
	### 
	### file.copy(server, dirname(local), recursive=TRUE, overwrite=overwrite)
}

#*********************************************
#*********************************************
#' Get the path to the server, depending on the local platform (Mac, Windows).
#'
#' @param root	A list of specifyers for the root directory to the central server.
#' @param path	The relative path from the root.
#'
#' @export
#' @keywords internal
#'
getServerPath <- function(root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoX_version_test/Automated_testing"){
	root <- root[[.Platform$OS.type]]
	if(length(root)==0){
		stop(paste0("The OS.type ", .Platform$OS.type, " does not match any of the names of 'root' (", paste(names(root), collapse=", "), ")"))
	}
	# There should be one directory per system, named by the output of getPlatformID():
	server <- file.path(root, path, getPlatformID())
	if(!file.exists(server)){
		warning(paste0("The server location ", server, " does not exist. Please create it manually for the given platform ID (obtained by getPlatformID()): ", getPlatformID()))
	}
	server
}

#*********************************************
#*********************************************
#' Copy the test run to the central srever.
#'
#' @param dir		The local directory holding the version testing.
#' @param root		A list of specifyers for the root directory to the central server.
#' @param path		The relative path from the root.
#' @param toCopy	The sub folders to copy files from.
#' @param overwrite	Logical: If TRUE, overwrite the files on the central directory.
#' @param msg		Logical: If TRUE, print progress to the console.
#' @param n			The number of runs (one runfor each version tested) to copy.
#'
#' @export
#' @keywords internal
#'
copyCurrentToServer <- function(dir, root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoX_version_test/Automated_testing", toCopy=c("Diff", "Output", "Projects_original"), overwrite=FALSE, msg=FALSE, n=1){
	server <- getServerPath(root=root, path=path)
	copyLatestToServer(dir, server, toCopy=toCopy, overwrite=overwrite, msg=msg, op="<=", n=n)
}

#*********************************************
#*********************************************
#' Function for running the r scripts of a project and copying the relevant output files to the "Output" directory:
#'
#' @param projectName		The path to the project.
#' @param progressFile		The paht to the progress file.
#' @param outputDir			The path to the directory in which to put the output files to be compared for the project (everything in "output" except trash files, baseline and baseline report output, and the project.xml file).
#'
#' @export
#' @keywords internal
#'
runProject <- function(projectName, progressFile, outputDir, ind=NULL){
	
	RstoxVersion <- getRstoxVersion()
	
	# Run the scripts and print info to the progress file:
	write(paste0(now(TRUE), "Starting project", paste0(" ", ind), ": ", projectName), progressFile, append=TRUE)
	
	
	cat(paste0("\n\n------------------------------------------------------------\nRunning project", paste0(" ", ind), ": ", projectName, ":\n"))
	
	# Run the baseline and baseline report (the latter with input=NULL):
	# The parameter 'modelType', enabling reading Baseline Report, was introduced in 1.8.1:
	# 2018-04-19 Added saveProject() since we wish to pick up changes in the project.xml files:
	if(RstoxVersion$Rstox > "1.8"){
		write(paste0(now(TRUE), "Running Baseline and Baseline Report"), progressFile, append=TRUE)
		baselineOutput <- getBaseline(projectName, exportCSV=TRUE, modelType="baseline", input=c("par", "proc"), drop=FALSE)
		saveProject(projectName)
		baselineReportOutput <- getBaseline(projectName, exportCSV=TRUE, modelType="report", input=c("par", "proc"), drop=FALSE)
		saveProject(projectName)
	}
	else{
		write(paste0(now(TRUE), "Running Baseline"), progressFile, append=TRUE)
		baselineOutput <- getBaseline(projectName, exportCSV=TRUE, input=c("par", "proc"), drop=FALSE)
		saveProject(projectName)
	}
	
	# Get the path to the scripts to run:
	r_script <- file.path(projectName, "output", "R", "r.R")
	rreport_script <- file.path(projectName, "output", "R", "r-report.R")
	# Generate the r scripts:
	generateRScripts(projectName)

	write(paste0(now(TRUE), "Running r.R"), progressFile, append=TRUE)
	if(file.exists(r_script)){
		source(r_script)
	}
	write(paste0(now(TRUE), "Running r-report.R"), progressFile, append=TRUE)
	if(file.exists(rreport_script)){
		source(rreport_script)
	}
	write(paste0(now(TRUE), "Ending project", paste0(" ", ind), ": ", projectName), progressFile, append=TRUE)
	write("", progressFile, append=TRUE)
	closeProject(projectName)
	
	# Copy output files to the output directory:
	unlink(outputDir, recursive=TRUE, force=TRUE)
	suppressWarnings(dir.create(outputDir, recursive=TRUE))
	output <- file.path(projectName, "output")
	file.copy(output, outputDir, recursive=TRUE)
	
	# Delete trash:
	trash <- list.dirs(outputDir)
	trash <- trash[grep("trash", trash)]
	unlink(trash, recursive=TRUE, force=TRUE)
	
	# Save also the output from baseline and baseline report to an RData file:
	save(baselineOutput, file=file.path(outputDir, "baselineOutput.RData"))
	if(RstoxVersion$Rstox > "1.8"){
		save(baselineReportOutput, file=file.path(outputDir, "baselineReportOutput.RData"))
	}
	
	# Copy the project.xml file:
	from <- getProjectPaths(projectName)$projectXML
	to <- file.path(outputDir, "project.xml")
	file.copy(from=from, to=to, overwrite=TRUE)
	
	cat("\n")
}
runProjectNew <- function(projectName, save=FALSE, msg=TRUE, close=FALSE){
	
	writeMessageToConsoleOrFile <- function(text, msg, add.time=FALSE){
		if(is.character(msg) && file.exists(msg)){
			write(paste0(if(add.time) now(TRUE), text), msg, append=TRUE)
		}
		else if(isTRUE(msg)){
			message(text)
		}
	}
	
	
	RstoxVersion <- getRstoxVersion()
	
	# Run the baseline and baseline report (the latter with input=NULL):
	# The parameter 'modelType', enabling reading Baseline Report, was introduced in 1.8.1:
	# 2018-04-19 Added saveProject() since we wish to pick up changes in the project.xml files:
	baselineReportOutput <- NULL
	if(RstoxVersion$Rstox > "1.8"){
		if(f(progressFile)){
			write(paste0(now(TRUE), "Running Baseline and Baseline Report"), progressFile, append=TRUE)
		}
		else if(msg){
			message("Running Baseline and Baseline Report")
		}
		baselineOutput <- getBaseline(projectName, exportCSV=TRUE, modelType="baseline", input=NULL, drop=FALSE)
		if(save){
			saveProject(projectName)
		}
		
		baselineReportOutput <- getBaseline(projectName, exportCSV=TRUE, modelType="report", input=NULL, drop=FALSE)
		if(save){
			saveProject(projectName)
		}
	}
	else{
		if(length(progressFile)){
			write(paste0(now(TRUE), "Running Baseline"), progressFile, append=TRUE)
		}
		else if(msg){
			message("Running Baseline")
		}
		baselineOutput <- getBaseline(projectName, exportCSV=TRUE, input=NULL, drop=FALSE)
		if(save){
			saveProject(projectName)
		}
	}
	
	# Get the path to the scripts to run:
	r_script <- file.path(projectName, "output", "R", "r.R")
	rreport_script <- file.path(projectName, "output", "R", "r-report.R")
	# Generate the r scripts:
	generateRScripts(projectName)

	# Run the scripts and print info to the progress file:
	if(length(progressFile)){
		write(paste0(now(TRUE), "Starting project ", i, ": ", projectName), progressFile, append=TRUE)
	}
	
	if(length(progressFile)){
		write(paste0(now(TRUE), "Running r.R"), progressFile, append=TRUE)
	}
	else if(msg){
		message("Running r.R")
	}
	if(file.exists(r_script)){
		source(r_script)
	}
	
	if(length(progressFile)){
		write(paste0(now(TRUE), "Running r-report.R"), progressFile, append=TRUE)
	}
	else if(msg){
		message("Running r-report.R")
	}
	if(file.exists(rreport_script)){
		source(rreport_script)
	}
	
	if(length(progressFile)){
		write(paste0(now(TRUE), "Ending project ", i, ": ", projectName), progressFile, append=TRUE)
		write("", progressFile, append=TRUE)
	}
	
	if(close){
		closeProject(projectName)
	}
	
	list(baselineOutput=baselineOutput, baselineReportOutput=baselineReportOutput)
}
copyProjectRun <- function(projectName, progressFile, outputDir){
	
	run <- runProject(projectName, save=TRUE, progressFile=progressFile, msg=TRUE, close=TRUE)
		
	# Copy output files to the output directory:
	unlink(outputDir, recursive=TRUE, force=TRUE)
	suppressWarnings(dir.create(outputDir, recursive=TRUE))
	output <- file.path(projectName, "output")
	file.copy(output, outputDir, recursive=TRUE)
	
	# Delete trash:
	trash <- list.dirs(outputDir)
	trash <- trash[grep("trash", trash)]
	unlink(trash, recursive=TRUE, force=TRUE)
	
	# Save also the output from baseline and baseline report to an RData file:
	save(run$baselineOutput, file=file.path(outputDir, "baselineOutput.RData"))
	if(length(run$baselineReportOutput)){
		save(run$baselineReportOutput, file=file.path(outputDir, "baselineReportOutput.RData"))
	}
	
	# Copy the project.xml file:
	from <- getProjectPaths(projectName)$projectXML
	to <- file.path(outputDir, "project.xml")
	file.copy(from=from, to=to, overwrite=TRUE)
	
	cat("\n")
}


#*********************************************
#*********************************************
#' Function for running all test projects and comparing outputs with previous outputs.
#'
#' @param dir				The local directory holding the version testing.
#' @param root				A list of specifyers for the root directory to the central server.
#' @param path				The relative path from the root.
#' @param copyFromServer	Logical: If TRUE, copy the latest original projects, outputs and diffs in the server to the local directory.
#' @param process			Which steps to run in the testing used mostly to reduce processing time for development and bug fixing.
#' @param diffs				Which diffs to include, also used to reduce processing time.
#' @param nlines			The number of lines to display for diffs between text files.
#'
#' @export
#' @keywords internal
#'
automatedRstoxTest <- function(dir, root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoX_version_test/Automated_testing", copyFromServer=TRUE, process=c("run", "diff"),  diffs=c("Rdata", "images", "text", "baseline"), nlines=50, mem.size=16e9, nwarnings=10000, n=1L){
#automatedRstoxTest <- function(dir, copyFromServer=TRUE, process=c("run", "diff"),  nlines=-1L, root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoXAutoTest"){
	
	# Load image packages:
	library(png)
	library(jpeg)
	library(tiff)
	# Load utilities packages:
	library(tools)
	library(R.utils)
	
	setJavaMemory(mem.size)
	oldnwarnings <- options()$nwarnings
	options(nwarnings=nwarnings)  
	
	# The function readBaselineFiles() was introduced in Rstox 1.8.1:
	if(getRstoxVersion()$Rstox <= "1.8"){
		readBaselineFiles <- function(x){
			# Return NULL if no files are given:
			if(length(x)==0){
				return(NULL)
			}
			
			# Function for converting string to logical:
			string2logical <- function(y){
				string2logicalOne <- function(z){
					if(length(z)>0 && any(z %in% c("true", "false"))){
					 	z <- as.logical(z)
					}
					z
				}
				as.data.frame(lapply(y, string2logicalOne), stringsAsFactors=FALSE)
			}

			# Read one text connection:
			if("textConnection" %in% class(x)){
				out <- read.csv(x, sep="\t", stringsAsFactors=FALSE, na.strings="-", encoding="UTF-8", quote=NULL)
				out <- string2logical(out)
			}
			# Read the files:
			else{
				out <- lapply(x, function(y) read.csv(y, sep="\t", stringsAsFactors=FALSE, na.strings="-", encoding="UTF-8", quote=NULL))
				for(i in seq_along(out)){
					out[[i]] <- string2logical(out[[i]])
				}

				# Get the names of the processes and data frames:
				x_split <- strsplit(basename(x), "_")
				#dataFrameNames <- sapply(lapply(x_split, "[", -1), paste, collapse="_")
				#processNames <- sapply(x_split, "[", 2)
				dataFrameNames <- sapply(lapply(x_split, "[", -1), paste, collapse="_")
				processNames <- sapply(x_split, function(y) paste(y[seq(2, length(y)-2)], sep="_"))
				
				# Set the names of the data frames:
				names(out) <- dataFrameNames
				out <- split(out, processNames)
				out <- lapply(out, function(y) if(length(y)==1) y[[1]] else y)
			}
			out
		}
	}
	

	# Order the sub data frames:
	sortByName <- function(x){
		if(length(x)){
			x[order(names(x))]
		}
		else{
			x
		}
	}
	
	deleteOutput <- function(dir){
		unlink(list.files(file.path(dir, "output"), full.names=TRUE, recursive=TRUE), force=TRUE)
		#unlink(file.path(dir, "output", "baseline"), recursive=TRUE, force=TRUE)
		#unlink(file.path(dir, "output", "r"), recursive=TRUE, force=TRUE)
	}
	deleteOutput <- function(x){
		if(length(x)==1 && !isProject(x[1])){
			x <- list.dirs(x, recursive=FALSE)
		}
		output <- file.path(x, "output")
		files <- list.files(output, recursive=TRUE, full.names=TRUE)
		unlink(files)
	}
	
	pasteAndHash <- function(...){
		out <- paste0("# ", c(...))
		out <- paste0(out, collapse="\n")
		out
	}
	
	pasteWithLineShift <- function(...){
		out <- paste0("# ", c(...))
		out <- paste0(out, collapse="\n")
		out
	}
		
	# Convert to all forward- or all backslashed:
	# Forwardslash should work on all systems, so the default is back=FALSE:
	setSlashes <- function(x, back=FALSE, platform=NULL){
		if(identical(platform, "windows")){
			back <- TRUE
		}
		if(back){
			gsub("/", "\\", x, fixed=TRUE)
		}
		else{
			gsub("\\", "/", x, fixed=TRUE)
		}
	}
	
	# Function for getting the common files:
	getFilesByExt <- function(dir1, dir2, ext=NULL, recursive=TRUE, ignore.case=TRUE){
		# Function for getting all image files in a vector of files (returning a list with names corresponding to the file extensions):
		getFilesByExtOne <- function(x, ext=NULL){
			if(length(ext)){
				fileext <- tools::file_ext(x)
				if(ignore.case){
					x <- x[tolower(fileext) %in% tolower(ext)]
				}
				else{
					x <- x[fileext %in% ext]
				}
			}
			x
		}
		getMatches <- function(files1, files2, dir1, dir2){
			commonFiles <- intersect(files1, files2)
			# This was an error, the correct would be to use the commonFiles as basename:
			#commonPaths1 <- file.path(dir1, files1)
			#commonPaths2 <- file.path(dir2, files2)
			commonPaths1 <- file.path(dir1, commonFiles)
			commonPaths2 <- file.path(dir2, commonFiles)
			onlyInFirst <- setdiff(files1, files2)
			onlyInSecond <- setdiff(files2, files1)
			list(commonFiles=commonFiles, commonPaths1=commonPaths1, commonPaths2=commonPaths2, onlyInFirst=onlyInFirst, onlyInSecond=onlyInSecond)
		}
		
		
		
		# Get matching and differing files:
		files1 <- getFilesByExtOne(list.files(dir1, recursive=recursive, full.names=FALSE), ext=ext)
		files2 <- getFilesByExtOne(list.files(dir2, recursive=recursive, full.names=FALSE), ext=ext)
		out <- getMatches(files1, files2, dir1, dir2)
		# Add the input directories:
		out$dir1 <- dir1
		out$dir2 <- dir2
		out$projectName <- basename(dir2)
		
		# Set all slashes to the appropriate direction:
		out <- lapply(out, setSlashes, platform = .Platform$OS.type)
		#out <- lapply(out, setSlashes)
		
		out
	}
	
	# Function for running the r scripts of a project and copying the relevant output files to the "Output" directory:
	###runProject <- function(projectName, progressFile, outputDir){
	###	
	###	RstoxVersion <- getRstoxVersion()
	###	
	###	cat(paste0("\n\n------------------------------------------------------------\nRunning project ", i, ": ", projectName, ":\n"))
	###	
	###	# Run the baseline and baseline report (the latter with input=NULL):
	###	# The parameter 'modelType', enabling reading Baseline Report, was introduced in 1.8.1:
	###	# 2018-04-19 Added saveProject() since we wish to pick up changes in the project.xml files:
	###	if(RstoxVersion$Rstox > "1.8"){
	###		write(paste0(now(TRUE), "Running Baseline and Baseline Report"), progressFile, append=TRUE)
	###		baselineOutput <- getBaseline(projectName, exportCSV=TRUE, modelType="baseline", input=NULL, drop=FALSE)
	###		saveProject(projectName)
	###		baselineReportOutput <- getBaseline(projectName, exportCSV=TRUE, modelType="report", input=NULL, drop=FALSE)
	###		saveProject(projectName)
	###	}
	###	else{
	###		write(paste0(now(TRUE), "Running Baseline"), progressFile, append=TRUE)
	###		baselineOutput <- getBaseline(projectName, exportCSV=TRUE, input=NULL, drop=FALSE)
	###		saveProject(projectName)
	###	}
	###	
	###	# Get the path to the scripts to run:
	###	r_script <- file.path(projectName, "output", "R", "r.R")
	###	rreport_script <- file.path(projectName, "output", "R", "r-report.R")
	###	# Generate the r scripts:
	###	generateRScripts(projectName)
	###
	###	# Run the scripts and print info to the progress file:
	###	write(paste0(now(TRUE), "Starting project ", i, ": ", projectName), progressFile, append=TRUE)
	###	
	###	
	###	write(paste0(now(TRUE), "Running r.R"), progressFile, append=TRUE)
	###	if(file.exists(r_script)){
	###		source(r_script)
	###	}
	###	write(paste0(now(TRUE), "Running r-report.R"), progressFile, append=TRUE)
	###	if(file.exists(rreport_script)){
	###		source(rreport_script)
	###	}
	###	write(paste0(now(TRUE), "Ending project ", i, ": ", projectName), progressFile, append=TRUE)
	###	write("", progressFile, append=TRUE)
	###	closeProject(projectName)
	###	
	###	# Copy output files to the output directory:
	###	unlink(outputDir, recursive=TRUE, force=TRUE)
	###	suppressWarnings(dir.create(outputDir, recursive=TRUE))
	###	output <- file.path(projectName, "output")
	###	file.copy(output, outputDir, recursive=TRUE)
	###	
	###	# Delete trash:
	###	trash <- list.dirs(outputDir)
	###	trash <- trash[grep("trash", trash)]
	###	unlink(trash, recursive=TRUE, force=TRUE)
	###	
	###	# Save also the output from baseline and baseline report to an RData file:
	###	save(baselineOutput, file=file.path(outputDir, "baselineOutput.RData"))
	###	if(RstoxVersion$Rstox > "1.8"){
	###		save(baselineReportOutput, file=file.path(outputDir, "baselineReportOutput.RData"))
	###	}
	###	
	###	# Copy the project.xml file:
	###	from <- getProjectPaths(projectName)$projectXML
	###	to <- file.path(outputDir, "project.xml")
	###	file.copy(from=from, to=to, overwrite=TRUE)
	###	
	###	cat("\n")
	###}
	
	printProjectName <- function(x, progressFile){
		cat(x$projectName, "...", "\n", sep="")
		toWrite <- paste0("\n##### PROJECT: ", x$projectName, ": #####")
		write(toWrite, file=progressFile, append=TRUE)
	}
	
	reportFilesIntersects <- function(x, progressFile, type="Projects", addProjectName=FALSE){
		if(addProjectName){
			printProjectName(x, progressFile)
			#toWrite <- paste0("##### ", x$projectName, ": #####")
			#write(toWrite, file=progressFile, append=TRUE)
		}
		
		# (1) Common files:
		toWrite <- paste0("# ", type, " common for both directories")
		# Add the common files, ot the string "NONE":
		if(length(x$commonFiles)){
			toWrite <- paste0(c(
				toWrite, 
				x$dir1, 
				"# and", 
				paste0(x$dir2, ":\n"), 
				paste0("\t", x$commonFiles, collapse="\n"), 
				""), collapse="\n"
			)
		}
		else{
			toWrite  <- paste0(c(
				toWrite, 
				"\tNONE", 
				""), collapse="\n"
			)
		}
		write(toWrite, file=progressFile, append=TRUE)
	
		# (2) Files only in the first directory:
		toWrite <- paste0(c(
			paste0("# ", type, " present only in the later directory: "), 
			paste0(x$dir1, ":\n"))
		)
		# Add the common files, ot the string "NONE":
		if(length(x$onlyInFirst)){
			toWrite <- paste0(c(
				toWrite, 
				paste0("\t", x$onlyInFirst, collapse="\n"), 
				""), collapse="\n"
			)
		}
		else{
			toWrite  <- paste0(c(
				toWrite, 
				"\tNONE", 
				""), collapse="\n"
			)
		}
		write(toWrite, file=progressFile, append=TRUE)
	
		# (3) Files only in the second directory:
		toWrite <- paste0(c(
			paste0("# ", type, " present only in the former directory:"), 
			paste0(x$dir2, ":\n"))
		)
		# Add the common files, ot the string "NONE":
		if(length(x$onlyInSecond)){
			toWrite <- paste0(c(
				toWrite, 
				paste0("\t", x$onlyInSecond, collapse="\n"), 
				""), collapse="\n"
			)
		}
		else{
			toWrite  <- paste0(c(
				toWrite, 
				"\tNONE", 
				""), collapse="\n"
			)
		}
		write(toWrite, file=progressFile, append=TRUE)
	
		
		
		
		## Files only in the first directory:
		#if(length(x$onlyInFirst)){
		#	#toWrite <- paste0("# ", type, " present only in the directory\n# ", x$dir1, ":\n", paste("\t", x$onlyInFirst, collapse="\n"), "\n")
		#	toWrite <- paste0(c(
		#		paste0("# ", type, " present only in the directory"), 
		#		paste0(x$dir1, ":\n"), 
		#		paste0("\t", x$onlyInFirst, collapse="\n"), 
		#		""), collapse="\n"
		#	)
		#}
		#else{
		#	toWrite  <- "NONE"
		#}
		#write(toWrite, file=progressFile, append=TRUE)
		#
		## Files only in the second directory:
		#if(length(x$onlyInSecond)){
		#	#toWrite <- paste0("# ", type, " present only in the directory\n# ", x$dir2, ":\n", paste("\t", x$onlyInSecond, collapse="\n"), "\n")
		#	toWrite <- paste0(c(
		#		paste0("# ", type, " present only in the directory"), 
		#		paste0(x$dir2, ":\n"), 
		#		paste0("\t", x$onlyInSecond, collapse="\n"), 
		#		""), collapse="\n"
		#	)
		#}
		#else{
		#	toWrite  <- "NONE"
		#}
		#write(toWrite, file=progressFile, append=TRUE)
		
	}
	
	printHeader <- function(header, progressFile, w=60){
		# Print to console
		cat("\n", header, "...", "\n", sep="")
		
		# Prepare for printing to file:
		ncharHeader <- nchar(header)
		nstars <- (w - ncharHeader - 2) / 2
		hash <- paste(rep("#", w), collapse="")
		hash1 <- paste(rep("#", ceiling(nstars)), collapse="")
		hash2 <- paste(rep("#", floor(nstars)), collapse="")
		header <- paste(hash1, header, hash2)
		header <- paste("", "", hash, header, hash, "", sep="\n")
		# Print to file:
		write(header, file=progressFile, append=TRUE)
	}
	
	getAllFiles <- function(dir1, dir2, progressFile){
		# Get the projects of the first and second directory (including common and different projects):
		projects <- getFilesByExt(dir1, dir2, recursive=FALSE)
		
		printHeader("Projects", progressFile, w=30)
		reportFilesIntersects(projects, progressFile=progressFile, type="Projects")
		
		# Get the different files per project, in a list, for clarity:
		RDataFiles <- lapply(seq_along(projects$commonFiles), function(i) getFilesByExt(dir1=projects$commonPaths1[i], dir2=projects$commonPaths2[i], ext="RData"))
		imageFiles <- lapply(seq_along(projects$commonFiles), function(i) getFilesByExt(dir1=projects$commonPaths1[i], dir2=projects$commonPaths2[i], ext=c("png", "jpg", "jpeg", "tif", "tiff")))
		textFiles <- lapply(seq_along(projects$commonFiles), function(i) getFilesByExt(dir1=projects$commonPaths1[i], dir2=projects$commonPaths2[i], ext=c("txt", "xml")))
		
		printHeader("RData files", progressFile, w=30)
		lapply(RDataFiles, reportFilesIntersects, progressFile=progressFile, type="RData files", addProjectName=TRUE)
		printHeader("Image files", progressFile, w=30)
		lapply(imageFiles, reportFilesIntersects, progressFile=progressFile, type="Image files", addProjectName=TRUE)
		printHeader("Text files", progressFile, w=30)
		lapply(textFiles, reportFilesIntersects, progressFile=progressFile, type="Text files", addProjectName=TRUE)
		
		list(RDataFiles=RDataFiles, imageFiles=imageFiles, textFiles=textFiles)
	}
	
	
	RDataDiff <- function(files, progressFile){
		diffRData <- function(i, files, progressFile){
			all.equalOne <- function(name, progressFile){
				#write(paste0("\tObject: ", name), file=progressFile, append=TRUE)
				all.equal(tempenvironment1[[name]], tempenvironment2[[name]])
			}
			
			file <- files$commonFiles[i]
			dir1 <- files$dir1
			dir2 <- files$dir2
			#write(paste0("File: ", file), file=progressFile, append=TRUE)
			# Read the files and diff:
			file1 <- file.path(dir1, file)
			file2 <- file.path(dir2, file)
			assign("tempenvironment1", new.env(), envir=.GlobalEnv)
			assign("tempenvironment2", new.env(), envir=.GlobalEnv)
			x1 <- load(file1, envir=tempenvironment1)
			x2 <- load(file2, envir=tempenvironment2)
			
			
			diffs <- lapply(x1, all.equalOne, progressFile=progressFile)
			nodiff <- unlist(lapply(diffs, isTRUE))
			
			
			# Print info also for no differences:
			write("{", file=progressFile, append=TRUE)
			out <- paste0(c("# (Code 0) No differences in the following RData files: ", file1, "# and", file2), collapse="\n")
			
			# Print info about different names:
			if(!all.equal(x1, x2)){
				objectList1 <- paste0("# OBJECTS: ", paste0(x1, collapse=", "), ":")
				objectList2 <- paste0("# OBJECTS: ", paste0(x2, collapse=", "), ":")
				out <- paste0(c("# (Code 1) Non-identical object NAMES in the following RData files: ", file1, objectList1, "# and", file2, objectList2), collapse="\n")
				#out <- paste("# ", c("Non-identical object NAMES in files", file1, objectList1, "and", file2, objectList2))
				#out <- paste(out, collapse="\n# ")
			}
			
			# Print info about different objects:
			if(!all(nodiff)){
				objectList <- paste0("# OBJECTS: ", paste0(x1[!nodiff], collapse=", "), ":")
				
				out <- paste0(c("# (Code 2) Non-identical objects in the following RData files: ", file1, "# and", file2), collapse="\n")
				
				howToInspect <- c(
					paste0("file1 <- \"", setSlashes(file1), "\""),
					paste0("file2 <- \"", setSlashes(file2), "\""),
					"assign(\"tempenvironment1\", new.env(), envir=.GlobalEnv)",
					"assign(\"tempenvironment2\", new.env(), envir=.GlobalEnv)",
					"x1 <- load(file1, envir=tempenvironment1)",
					"x2 <- load(file2, envir=tempenvironment2)", 
					"str(tempenvironment1[[x1]])", 
					"str(tempenvironment2[[x2]])", 
					"all.equal(tempenvironment1[[x1]], tempenvironment2[[x2]])"
					)
				
				out <- paste(out, "# Inspect the differences by using the following code:", sep="\n")
				out <- paste(out, paste0("\t", c(howToInspect), collapse="\n"), sep="\n")
			
				out <- paste(out, paste0(c(objectList), collapse="\n"), sep="\n")
				out <- paste(out, unlist(lapply(diffs[!nodiff], function(x) paste("\t", x, collapse="\n"))), sep="\n")
			}
			write(unlist(out), file=progressFile, append=TRUE)
			write("}\n", file=progressFile, append=TRUE)
		}
	
		# Compare images:
		printProjectName(files, progressFile)
		#write("\n\n********************", file=progressFile, append=TRUE)
		#write(paste0("***** ", files$projectName, " *****"), file=progressFile, append=TRUE)
		
		write("{", file=progressFile, append=TRUE)
		out <- lapply(seq_along(files$commonFiles), diffRData, files=files, progressFile=progressFile)
		write("}", file=progressFile, append=TRUE)
	}
	
	# Function to check diffs between images, and printing the diffs to file:
	imDiff <- function(files, progressFile, diffdir){
		imDiffOne <- function(file, dir1, dir2, progressFile, diffdir){
			
			if(length(file)==0){
				write("No images", progressFile, append=TRUE)
			}
			
			# Get the read and write functions:
			validExt <- list(png="png", jpeg=c("jpg", "jpeg"), tiff=c("tif", "tiff"))
			ext <- tools::file_ext(file)
			if(ext %in% validExt$png){
				readFun <- png::readPNG
				writeFun <- png::writePNG
			}
			else if(ext %in% validExt$jpeg){
				readFun <- jpeg::readJPEG
				writeFun <- jpeg::writeJPEG
			}
			else if(ext %in% validExt$tiff){
				readFun <- tiff::readTIFF
				writeFun <- tiff::writeTIFF
			}
		
			# Read the files and diff:
			file1 <- file.path(dir1, file)
			file2 <- file.path(dir2, file)
			x1 <- readFun(file1)
			x2 <- readFun(file2)
				
			write("{", file=progressFile, append=TRUE)
			if(!all(x1==x2)){
				x12 <- x1 - x2
				# Modify the diff image to fit the [0, 1] range, and set all identical values to 0:
				#x12 <- (x12 + 1) / 2
				# Take the absolute difference and invert:
				x12 <- 1 - abs(x12)
				#x12[x1 == x2] <- 1
			
				# Paste together the first the diff and the second image:
				dimx <- dim(x12)
				out <- array(double(3 * prod(dimx)), dim=c(dimx[1], 3 * dimx[2], dimx[3]))
				out[, seq_len(dimx[2]),] <- x1
				out[, seq_len(dimx[2]) + dimx[2],] <- x12
				out[, seq_len(dimx[2]) + 2 * dimx[2],] <- x2
				# Reset the alpha-channel, if present, assuming constant alpha throughout the image:
				if(dimx[3]==4){
					out[,,4] <- x1[1,1,4]
				}
				
				# Write to file if differing:
				thisdiffdir <- file.path(diffdir, basename(dir1))
				outfile <- file.path(thisdiffdir, basename(file))
				suppressWarnings(dir.create(thisdiffdir))
				writeFun(out, outfile)
				# Write a log to the progressFile:
				
				
				out <- paste0(c("# (Code 2) Differences in the following images", file1, "# and", file2, paste0("# See an image with the current to the left, the diff in the middle, and the previous image to the right in the file \"", outfile, "\"")), collapse="\n")
				write(out, progressFile, append=TRUE)
				
				
				#write(paste0("(Code 2) Images\n\t", file1, " and\n\t", file2, "\ndiffer. See an image with the current to the left, the diff in the middle, and the previous image to the right in the file \n\t", outfile, "\n"), progressFile, append=TRUE)
			}
			else{
				out <- paste0(c("# (Code 0) No differences in the following images", file1, "# and", file2), collapse="\n")
				write(out, progressFile, append=TRUE)
			}
			write("}\n", file=progressFile, append=TRUE)
		}
		
		printProjectName(files, progressFile)
		
		write("{", file=progressFile, append=TRUE)
		out <- lapply(files$commonFiles, imDiffOne, dir1=files$dir1, dir2=files$dir2, progressFile=progressFile, diffdir=diffdir)
		write("}", file=progressFile, append=TRUE)
	}
	
	# Function for running diff between the previous and new output files:
	# 'files' is a list as returned from the function getFilesByExt(), which uses getMatches() to return the following elements: 'commonFiles', 'commonPaths1', 'commonPaths2', 'onlyInFirst', 'onlyInSecond':
	diffTextFiles <- function(files, progressFile, diffdir, nlines=50){
		
		diffTextFilesOne <- function(file, dir1, dir2, progressFile, diffdir, nlines){
			file1 <- setSlashes(file.path(dir1, file), platform = .Platform$OS.type)
			file2 <- setSlashes(file.path(dir2, file), platform = .Platform$OS.type)
			nlinesFile1 <- R.utils::countLines(file1)
			nlinesFile2 <- R.utils::countLines(file2)
			# Write tepmorary files with less lines:
			tempfile1 <- file.path(tempdir(), "tempfile1")
			tempfile2 <- file.path(tempdir(), "tempfile2")
			if(nlinesFile1 > nlines){
				temp <- readLines(file1, n=nlines)
				writeLines(temp, tempfile1)
			}
			else{
				tempfile1 <- file1
			}
			if(nlinesFile2 > nlines){
				temp <- readLines(file2, n=nlines)
				writeLines(temp, tempfile2)
			}
			else{
				tempfile2 <- file2
			}
			
			tempdiff <- file.path(path.expand(diffdir), "tempdiff.txt")
			
			# 2018-04-03 Added by Johnsen: New function to be used with sink(). This is the only way of getting output from FC to file on Windows:
			system1 = function(...) cat(base::system(..., intern = TRUE), sep = "\n")
			
			# Platform dependent diff commands:
			if(.Platform$OS.type == "windows"){
				# No longer needed
				#file1 <- gsub("/", "\\", file1, fixed=TRUE)
				#file2 <- gsub("/", "\\", file2, fixed=TRUE)
				#tempdiff <- gsub("/", "\\", tempdiff, fixed=TRUE)
				
				## # 2018-04-03 Added by Johnsen: Add backslach at the start of the path to get it working on Windows (UNIX accepts this):
				## file1 <- paste0("\\",file1) ## Espen include
				## file2 <- paste0("\\",file2) ## Espen include
				## tempdiff <- paste0("\\",tempdiff) ## Espen include
			
				# 2018-04-03 Added by Johnsen: Do not redirect the output to a file, but rather use sink() below instead:
				cmd <- paste0(
					"fc /LB",
					nlines, 
					" ",
					shQuote(tempfile1, type="cmd2"), 
					" ",
					shQuote(tempfile2, type="cmd2")
				)
				
				inspect <- paste0(
					"fc /LB",
					nlines, 
					" ",
					shQuote(file1, type="cmd2"), 
					" ",
					shQuote(file2, type="cmd2")
				)
				
				#cmd <- paste(c(
				#	"FC", 
				#	shQuote(file1, type="cmd2"), 
				#	shQuote(file2, type="cmd2"), 
				#	paste0(">", shQuote(tempdiff))), collapse=" ")
			}
			else if(.Platform$OS.type == "unix"){
				### cmd <- paste(c(
				### 	"diff", 
				### 	#"-r", 
				### 	shQuote(file1), 
				### 	shQuote(file2), 
				### 	#paste0(">", shQuote(tempdiff))), collapse=" "
				### )
				
				
				
				### cmd <- paste0(
				### 	"diff <(head -n ", 
				### 	nlines, 
				### 	" ", 
				### 	shQuote(tempfile1), 
				### 	") <(head -n ", 
				### 	nlines, 
				### 	" ", 
				### 	shQuote(tempfile2), 
				### 	")" 
				### )
				
				cmd <- paste0(
					"diff ", 
					shQuote(tempfile1), 
					" ", 
					shQuote(tempfile2)
				)
				
				inspect <- paste0(
					"diff ", 
					shQuote(file1), 
					" ", 
					shQuote(file2)
				)
				
				
			}
			else{
				stop("Unknown system. Must be one of UNIX or Windows")
			}
			
			
			
			# 2018-04-03 Added by Johnsen: Use sink() to get the output from the diff/fc:
			#sink(file=tempdiff, append=TRUE) ## Not sure if all messages should go to the same file. Arne Johannes to decide
			sink(file=tempdiff, append=FALSE) ## Not sure if all messages should go to the same file. Arne Johannes to decide
			system1(cmd)
			# End writing to file
			sink(type = "message")
			sink()
			
			
			### # Run the diff as a system call and print to the temp file:
			### system(cmd)
			### # -x '*.bmp' -x '*.jpeg' -x '*.png' -x '*.tiff' -x '*.RData'
			
			# Read the tempdiff file and append to the progress file:
			#diffinfo <- readLines(tempdiff, n=nlines)
			diffinfo <- readLines(tempdiff)
			# Determine whether the tempdiff reported any diffs
			noDiffUnix <- sum(nchar(diffinfo)) <= 1
			noDiffWindows <- length(grep("no differences encountered", diffinfo))
			
			write("{", file=progressFile, append=TRUE)
			if(noDiffUnix || noDiffWindows){
				out <- paste0(c("# (Code 0) No differences in the following text files", file1, "# and", file2), collapse="\n")
				write(out, progressFile, append=TRUE)
			}
			else{
				out <- paste0(c("# (Code 2) Differences in the following text files", file1, "# and", file2), collapse="\n")
				out <- c(out, paste0("\t", diffinfo))
				
				if(any(c(nlinesFile1, nlinesFile2) > nlines)){
					out <- c(
						out, 
						"\t...", 
						paste0("\tNumber of lines exceeding 'nlines' (", nlines, ") in the files ", file1, " (", nlinesFile1, ") and/or ", file2, " (", nlinesFile2, ")"), 
						paste0(
							"\tInspect the full diff by the following command in the ", 
							if(.Platform$OS.type == "windows") " CMD app on Windows, increasing the number immediately following fc /LB (which \"Sets the maximum consecutive mismatches to the specified
            number of lines\"): " else " Terminal on Mac: ", 
							inspect
						)
					)
				}
				
				
				write(out, progressFile, append=TRUE)
			}
			write("}\n", file=progressFile, append=TRUE)
			
			
			
			
			
			
			
			
			
			
			
			#
			#nlinesFile1 <- R.utils::countLines(file1)
			#nlinesFile2 <- R.utils::countLines(file2)
			#if(nlinesFile1 != nlinesFile2){
			#	diffinfo <- c(
			#		paste0("# Number of lines differ in files ", file1, " (", nlinesFile1, ") and ", file2, " (", nlinesFile2, ")"), 
			#		diffinfo)
			#}
			#
			#
			#if(any(c(nlinesFile1, nlinesFile2) > nlines) && length(diffinfo)>1){
			#	diffinfo <- c(diffinfo, "...", paste0("(Number of lines differing: ", R.utils::countLines(tempdiff), ")"))
			#}
			## Add the command used:
			#diffinfo <- c(diffinfo, paste0("Command used to generate diff: ", cmd))
			#
			##write("\n\n********************", file=progressFile, append=TRUE)
			#
			#write("{", file=progressFile, append=TRUE)
			#if(noDiffUnix || noDiffWindows){
			#	out <- paste0(c("# (Code 0) No differences in the following text files", file1, "# and", file2), collapse="\n")
			#	write(out, progressFile, append=TRUE)
			#}
			#else{
			#	out <- paste0(c("# (Code 2) Differences in the following text files", file1, "# and", file2), collapse="\n")
			#	write(out, progressFile, append=TRUE)
			#	write(paste0("\t", diffinfo), file=progressFile, append=TRUE)
			#}
			#write("}\n", file=progressFile, append=TRUE)
			
			if(file.exists(tempdiff)){
				unlink(tempdiff)
			}
		}
	
		# Compare text files:
		printProjectName(files, progressFile)
		
		write("{", file=progressFile, append=TRUE)
		out <- lapply(files$commonFiles, diffTextFilesOne, dir1=files$dir1, dir2=files$dir2, progressFile=progressFile, diffdir=diffdir, nlines=nlines)
		write("}", file=progressFile, append=TRUE)
		
		
		return(NULL)
	}
	
	diffBaseline <- function(dir, progressFile){
		
		RstoxVersion <- getRstoxVersion()
		
		all.equalRstoxStoX <- function(Rstox, StoX, name, progressFile){
			write_all.equal <- function(name, x, y, progressFile){
				d <- all.equal(x[[name]], y[[name]])
				Code <- 0
				write("{", file=progressFile, append=TRUE)
				if(!isTRUE(d)){
					Code <- 2
					write(paste0("# (Code 2) Differences in output from process ", name, " from Rstox and StoX"), file=progressFile, append=TRUE)
					write(paste("\t", d), file=progressFile, append=TRUE)
				}
				else{
					write(paste0("# (Code 0) No differences in output from process ", name, " from Rstox and StoX"), file=progressFile, append=TRUE)
				}
				write("}\n", file=progressFile, append=TRUE)
				
				Code
			}
			
			namesRstox <- names(Rstox)
			namesStoX <- names(StoX)
			commonDF <- intersect(namesRstox, namesStoX)
			onlyInRstox <- setdiff(namesRstox, namesStoX)
			onlyInStoX <- setdiff(namesStoX, namesRstox)
			
			write(paste0("\n### MODEL TYPE: ", name, "\n"), file=progressFile, append=TRUE)
			
			# Inform files present only in one or the other:
			if(length(commonDF)){
				write("# Data frames common for Rstox and StoX: ", file=progressFile, append=TRUE)
				lapply(paste("\t", commonDF), write, file=progressFile, append=TRUE)
			}
			if(length(onlyInRstox)){
				write("# Data frames only in Rstox: ", file=progressFile, append=TRUE)
				lapply(paste("\t", onlyInRstox), write, file=progressFile, append=TRUE)
			}
			if(length(onlyInStoX)){
				write("# Data frames only in StoX: ", file=progressFile, append=TRUE)
				lapply(paste("\t", onlyInStoX), write, file=progressFile, append=TRUE)
			}
			
			# Compare each data frame og the project:¨
			unlist(lapply(commonDF, write_all.equal, x=Rstox, y=StoX, progressFile=progressFile))
		}
		
		# Get the baseline and baseline report saved in RData files:
		baselineOutputFiles <- file.path(dir, c("baselineOutput.RData", "baselineReportOutput.RData"))
		present <- file.exists(baselineOutputFiles)
		if(!any(present)){
			return(NULL)
		}
		baselineOutputFiles <- baselineOutputFiles[present]
		
		# Load the data to a list:
		### readBaselineOutputFiles <- function(x){
		### 	mget(load(x))$outputData
		### }
		
		dataFromRstox <- unlist(lapply(baselineOutputFiles, function(x) mget(load(x))), recursive=FALSE)
		dataFromRstox <- lapply(dataFromRstox, "[[", "outputData")
		# Due to a fundamental problem of interpreting the process name from the baseline and baseline report output csv files (ProcessName_OutputType_Level.txt), where _Level may be missing, and any user introduced "_" in the process names will make the interpretation ambigous, we group the processes according to the first element of the process name after separating by underscore. This is done to allow comparison between Rstox and StoX:
		
		#cropProcessName <- function(x){
		#	if(length(x)==0){
		#		return(x)
		#	}
		#	x_names <- names(x)
		#	x_names <- strsplit(x_names, "_")
		#	x_names <- sapply(x_names, head, 1)
		#	#names(x) <- x_names
		#	x <- split(x, x_names)
		#	x <- lapply(x, function(y) if(length(y)==1) y[[1]] else y)
		#	x
		#	#x <- lapply(x, "[[", 1)
		#	#x <- lapply(x, function(y) if(is.list(y) && !is.data.frame(y)) unlist(y, recursive=FALSE) else y)
		#}
		#dataFromRstox <- lapply(dataFromRstox, cropProcessName)
		
		
		# Read also the txt-files from baseline and baseline report:
		baselineDirs <- file.path(dir, "output", "baseline", c("data", "report"))
		baselineFiles <- lapply(baselineDirs, list.files, recursive=TRUE, full.names=TRUE)
		names(baselineFiles) <- c("baselineOutput", "baselineReportOutput")
		
		# Read the data to a list:
		dataFromStoX <- lapply(baselineFiles, readBaselineFiles)
		
		# Keep only the modelType present in the Rstox output file:
		dataFromStoX <- dataFromStoX[names(dataFromRstox)]
		
		# Sort the data by name
		dataFromRstox <- lapply(dataFromRstox, function(x) lapply(x, sortByName))
		dataFromStoX <- lapply(dataFromStoX, function(x) lapply(x, sortByName))
		
		#write("\n", file=progressFile, append=TRUE)
		printProjectName(list(projectName=basename(dir)), progressFile)
		
		write("{", file=progressFile, append=TRUE)
		Code <- unlist(lapply(names(dataFromRstox), function(x) all.equalRstoxStoX(Rstox=dataFromRstox[[x]], StoX=dataFromStoX[[x]], name=x, progressFile=progressFile)))
		
		if(any(Code==2) && RstoxVersion$Rstox > "1.8"){
			howToInspect <- c(
				paste0("dataFromRstox <- unlist(lapply(c(", paste0("\"", baselineOutputFiles, "\"", collapse=", "), "), function(x) mget(load(x))), recursive=FALSE)"),
				"dataFromRstox <- lapply(dataFromRstox, \"[[\", \"outputData\")", 
				paste0("baselineDirs <- file.path(\"", dir, "\", \"output\", \"baseline\", c(\"data\", \"report\"))"),
				"baselineFiles <- lapply(baselineDirs, list.files, recursive=TRUE, full.names=TRUE)",
				"names(baselineFiles) <- c(\"baselineOutput\", \"baselineReportOutput\")", 
				"dataFromStoX <- lapply(baselineFiles, readBaselineFiles)", 
				"ls.str(dataFromRstox)", 
				"ls.str(dataFromStoX)"
			)
			write("# Inspect the differences by using the following code:", file=progressFile, append=TRUE)
			write(paste0("\t", c(howToInspect), collapse="\n"), file=progressFile, append=TRUE)
		}
		
		write("}", file=progressFile, append=TRUE)
		#write("\n", file=progressFile, append=TRUE)
	}
	
	# Set the directory of the test projects:
	server <- getServerPath(root=root, path=path)
	#root <- root[[.Platform$OS.type]]
	#if(length(root)==0){
	#	stop(paste0("The OS.type ", .Platform$OS.type, " does not match any of the names of 'root' (", paste(names(root), collapse=", "), ")"))
	#}
	##root <- ifelse(.Platform$OS.type == "windows", "\\\\delphi", "/Volumes")
	## There should be one directory per system, named by the output of getPlatformID():
	#server <- file.path(root, path, getPlatformID())
	
	# Make sure the paths are expanded:
	server <- path.expand(server)
	dir <- path.expand(dir)
	suppressWarnings(dir.create(dir))
	
	dirList <- getTestFolderStructure(dir)
	# Create the folder structure if missing:
	lapply(dirList, dir.create, recursive=TRUE, showWarnings=FALSE)
	
	# Name the folder for the output files by the time and Rstox version:
	RstoxVersion <- getRstoxVersion()
	folderName <- paste(names(RstoxVersion), unlist(lapply(RstoxVersion, as.character)), sep="_", collapse="_")
	
	# 1. Copy the latest original projects, outputs and diffs on the server to the local directory:
	#if("run" %in% process && copyFromServer){
	if(copyFromServer){
		#cat("Copying original projects from \"", server, "\" to ", dir, "\n", sep="")
		cat("Copying original projects from \"", server, "\" to ", dir, "\n", sep="")
		copyStaged_Projects_original(dirname(server), dir)
	}
	
	# Get the latest projects:
	ProjectsDir_original <- getLatestDir(dirList$Projects_original, n=n)
	# Get paths to the original projects and previous output folders:
	ProjectsList_original <- list.dirs(ProjectsDir_original, recursive=FALSE)
	ProjectsDir <- dirList$Projects
	Staged_ProjectsDir_original <- file.path(dirList$Staged_Projects_original, basename(ProjectsDir_original))
	Staged_ProjectsList_original <- list.dirs(Staged_ProjectsDir_original, recursive=FALSE)
	
	# First copy all files from ProjectsDir_original to ProjectsDir
	if("run" %in% process){
		unlink(ProjectsDir, recursive=TRUE, force=TRUE)
		suppressWarnings(dir.create(ProjectsDir))
		
		# Copy first staged projects:
		if(length(Staged_ProjectsList_original)){
			cat("Copying projects from \n\t\"", Staged_ProjectsDir_original, "\"\n to \n\t", ProjectsDir_original, "\n", sep="")
			lapply(Staged_ProjectsList_original, file.copy, ProjectsDir_original, overwrite=TRUE, recursive=TRUE)
			# Update ProjectsList_original:
			ProjectsList_original <- list.dirs(ProjectsDir_original, recursive=FALSE)
		}
		
		cat("Copying projects from \n\t\"", ProjectsDir_original, "\"\n to \n\t", ProjectsDir, "\n", sep="")
		lapply(ProjectsList_original, file.copy, ProjectsDir, overwrite=TRUE, recursive=TRUE)
		
		# Then delete all output files for safety:
		lapply(list.dirs(ProjectsDir, recursive=FALSE), deleteOutput)
	}
	
	# Get all project paths:
	projectPaths <- list.dirs(ProjectsDir, recursive=FALSE)
	
	# Get the outputs directory and the sub directory of the new outputs:
	Output <- dirList$Output
	newOutput <- file.path(Output, folderName)
	
	
	# List all projects in the latest and new output directory:
	newOutputList <- file.path(newOutput, basename(projectPaths))
	
	# Then run through all projects, printing progress to a file:
	suppressWarnings(dir.create(dirList$Diff))
	progressFile <- file.path(dirList$Diff, "progress.R")
	unlink(progressFile)
	
	if("run" %in% process){
		if(length(projectPaths)==0){
			stop("'Projects' folder empty or invalid")
		}
		for(i in seq_along(projectPaths)){
			runProject(projectName=projectPaths[i], progressFile=progressFile, outputDir=newOutputList[i], ind=i)
		}
	}
	
	# Copy the projects that were run to a new folder in the Projects_original, but first delete any output:
	if("run" %in% process){
		newProjectsDir_original <- file.path(dirname(ProjectsDir_original), folderName)
		suppressWarnings(dir.create(newProjectsDir_original))
		ProjectsList <- list.dirs(ProjectsDir, recursive=FALSE)
		
		# Delete output:
		lapply(ProjectsList, deleteOutput)
		
		lapply(ProjectsList, file.copy, newProjectsDir_original, overwrite=TRUE, recursive=TRUE)
		
		# Also delete the projects in "Projects":
		#unlink(ProjectsDir, recursive=TRUE, force=TRUE)
	}
	
	# Get the lastest sub directory of the previously generated outputs:
	latestOutput <- getLatestDir(dirList$Output, n=n)
	
	if("diff" %in% process && length(latestOutput)){
		VersionComparisonString <- paste(basename(newOutput), basename(latestOutput), sep="_")
	
		#diffdir <- path.expand(file.path(dir, "Diff", paste("Diff", basename(newOutput), basename(latestOutput), sep="_")))
		diffdir <- path.expand(file.path(dirList$Diff, VersionComparisonString))
		setSlashes(diffdir)
		unlink(diffdir, recursive=TRUE, force=TRUE)
		suppressWarnings(dir.create(diffdir))
		
		# Get all files common and different between the old and new run, separated into file types RData, image and text:
		printHeader("1. Common and differing projects and files", progressFile)
		write("{", file=progressFile, append=TRUE)
		
		cat("Comparing\n", newOutput, "\nand\n", latestOutput, "\n")
		
		allFiles <- getAllFiles(newOutput, latestOutput, progressFile)
		write("}", file=progressFile, append=TRUE)
		
		
		# Special diff of RData files:
		if("Rdata" %in% diffs){
			printHeader("2. Comparing RData files", progressFile)
			write("\n{", file=progressFile, append=TRUE)
			lapply(allFiles$RDataFiles, RDataDiff, progressFile=progressFile)
			write("}", file=progressFile, append=TRUE)
		}
		
		# Special diff of images:
		if("images" %in% diffs){
			printHeader("3. Comparing image files", progressFile)
			write("{", file=progressFile, append=TRUE)
			lapply(allFiles$imageFiles, imDiff, progressFile=progressFile, diffdir=diffdir)
			write("}", file=progressFile, append=TRUE)
		}
		
		# Diff text files:
		if("text" %in% diffs){
			printHeader("4. Comparing text files", progressFile)
			write("{", file=progressFile, append=TRUE)
			lapply(allFiles$textFiles, diffTextFiles, progressFile=progressFile, diffdir=diffdir, nlines=nlines)
			write("}", file=progressFile, append=TRUE)
		}
		
		# Diff also the baseline output and the files written by baseline:
		if("baseline" %in% diffs){
			printHeader("5. Comparing Rstox and StoX baseline output", progressFile)
			write("{", file=progressFile, append=TRUE)
			lapply(newOutputList, diffBaseline, progressFile=progressFile)
			write("}", file=progressFile, append=TRUE)
		}
		
		
		write("\nPlease also run the example script on ftp://ftp.imr.no/StoX/Download/Rstox/Examples\n", file=progressFile, append=TRUE)
	
		# Add indentation corresponding to the curly brackets:
		l <- readLines(progressFile)
		n <- double(length(l))
		atStart <- which(startsWith(l, "{")) + 1
		atEnd <- which(startsWith(l, "}"))
		n[atStart] <- n[atStart]  + 1
		n[atEnd] <- n[atEnd] -1
		tabs <- strrep("\t", cumsum(n))
		l <- paste0(tabs, l)
		writeLines(l, progressFile)
		
		# Add warnings():
		d <- warnings()
		d <- paste0("[", formatC(seq_along(d), width=nchar(length(d)), format="d", flag="0"), "]", "\t", d, names(d))
		d <- c(paste("THERE WERE", length(d), " WARNINGS:"), d)
		write(d, progressFile, append=TRUE)
		
		# Copy the progress file to the current diff directory:
		finalProgressFile <- file.path(diffdir, paste0("PROGRESS_", VersionComparisonString, ".R"))
		# The notesFile is a copy of the progress file, in which the reviewer should input comments to each diff. This will be made more automatic in later versions, where the diff will be saved in a list of strings which wil be numbered and each diff must be approved or resolved.
		finalNotesFile <- file.path(dirname(dirList$Diff), paste0("NOTES_", VersionComparisonString, ".R"))
		
		file.copy(progressFile, finalProgressFile, overwrite=TRUE)
		file.copy(progressFile, finalNotesFile, overwrite=TRUE)
		unlink(progressFile, force=TRUE)
	}
	
	options(nwarnings=oldnwarnings)
}

# Function for getting a string with the current time: 
now <- function(brackets=FALSE){
	out <- format(Sys.time(),tz="UTC", "%Y-%m-%d_%H.%M.%S")
	if(brackets){
		out <- paste0("[", out, "] ")
	}
	out
}


