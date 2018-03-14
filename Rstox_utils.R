# Function used for building and testing the Rstox package. 
# Use this in the continous development of Rstox. 
# Rstox can also be built from the develop brach of Sea2Data/Rstox, but the function buildRstox() generates the README and DESCRIPTION file, treats dependencies and tests the package and examples if check=TRUE:
buildRstox <- function(buildDir, pkgName="Rstox", version="1.0", Rversion="3.3.1", pckversion=list(), official=FALSE, check=FALSE, exportDir=NULL, suggests=NULL) {
	
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
		write("# R should be installed as the 64 bit version (and 64 bit version ONLY for Windows 10. To do this, uncheck the box \"32-bit Files\" when selecting components to install. If you are re-intalling an R that has both 32 and 64 bit, you will need to uninstall R first).", READMEfile, append=TRUE)
		write("# On Windows systems with adminstrator requirements, it is recommended to install R in C:/users/<user>/documents/R.", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# Note that 64 bit Java is required to run Rstox", READMEfile, append=TRUE)
		write("# On Windows, install Java from this webpage: https://www.java.com/en/download/windows-64bit.jsp,", READMEfile, append=TRUE)
		write("# or follow the instructions found on ftp://ftp.imr.no/StoX/Tutorials/", READMEfile, append=TRUE)
		write("# On Mac, getting Java and Rstox to communicate can be challenging.", READMEfile, append=TRUE)
		write("# If you run into problems such as \"Unsupported major.minor version ...\", try the following:", READMEfile, append=TRUE)
		write("# Update java, on", READMEfile, append=TRUE)
		write("# \thttp://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html", READMEfile, append=TRUE)
		write("# If this does not work install first the JDK and then the JRE:", READMEfile, append=TRUE)
		write("# \thttp://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html", READMEfile, append=TRUE)
		write("# \thttp://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html", READMEfile, append=TRUE)
		write("# You may want to check that the downloaded version is first in the list by running the following in the Terminal:", READMEfile, append=TRUE)
		write("# \t/usr/libexec/java_home -V", READMEfile, append=TRUE)
		write("# \tjava -version", READMEfile, append=TRUE)
		write("# Then run this in the Terminal.app:", READMEfile, append=TRUE)
		write("# \tsudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib", READMEfile, append=TRUE)
		write("# \tsudo R CMD javareconf", READMEfile, append=TRUE)
		write("# Open R (close and then open if already open) and install rJava:", READMEfile, append=TRUE)
		write("# \tinstall.packages('rJava', type='source')", READMEfile, append=TRUE)
		write("# Then the installed Rstox should work.", READMEfile, append=TRUE)
		
		# Write release notes:
		write("", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# Release notes:", READMEfile, append=TRUE)
	
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
	onLoadText = paste(
		".onLoad <- function(libname, pkgname){",
		"	",
		"	if(Sys.getenv(\"JAVA_HOME\")!=\"\") Sys.setenv(JAVA_HOME=\"\")",
		"	options(java.parameters=\"-Xmx2g\")",
		"# Create a Rstox environment in which the baseline objects of the various projects are placed. This allows for a check for previously run baseline models and avoids memory leakage:", 
		"	assign(\"RstoxEnv\", new.env(), envir=.GlobalEnv)",
		"	# Assign fundamental variables to the RstoxEnv:",
		"	Definitions <- list(",
		"		StoXFolders = c(\"input\", \"output\", \"process\"), ",
		"		NMD_data_types = c(\"echosounder\", \"biotic\", \"landing\"), ",
		"		StoX_data_types = c(\"acoustic\", \"biotic\", \"landing\"), ",
		"		StoX_data_type_keys = c(acoustic=\"echosounder_dataset\", biotic=\"missions xmlns\", landing=\"Sluttseddel\"), ",
		"		model_types = c(\"AcousticTrawl\", \"SweptAreaLength\", \"SweptAreaTotal\"), ",
		"		processLevels = c(\"bootstrap\", \"bootstrapImpute\")",
		"		)",
		"	assign(\"Definitions\", Definitions, envir=get(\"RstoxEnv\"))",
		"	assign(\"Projects\", list(), envir=get(\"RstoxEnv\"))",
	"}", sep="\n")
	write(onLoadText, onLoadFile)
	##########
	
	##### Save a Java memory message to the onAttach.R file in the "R" directory: #####
	onAttachText = paste(
		".onAttach <- function(libname, pkgname){",
		"	",
		paste0("	packageStartupMessage(\"", pkgName, "_", version, "\n**********\nIf problems with Java Memory such as java.lang.OutOfMemoryError occurs, try increasing the Java memory by running setJavaMemory(4e9), and possibly using an even higher value than 4 gigabytes\n**********\n\", appendLF=FALSE)"),
	"}", sep="\n")
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


# Function for running all test projects and comparing outputs with previous outputs:
automatedRstoxTest <- function(dir, copyFromOriginal=TRUE, process=c("run", "diff"), cores=1){
	
	# Function borrowed from the TSD package (parallel version of lapply):
	papply <- function(X, FUN, ..., cores=1, outfile="", msg="Processing... "){
		availableCores <- parallel::detectCores()
		# If memory runs out, a system call to determine number of cores might fail, thus detectCores() could return NA
		# defaulting to single core if this is the case
		if(is.na(availableCores)){
			availableCores <- 1
		}
		if(cores > availableCores){
			warning(paste0("Only ", availableCores, " cores available (", cores, " requested)"))
		}
		nruns <- length(X)
		cores <- min(cores, nruns, availableCores)
	
		# Generate the clusters of time steps:
		if(cores>1){
			cat(paste0(msg, "(", nruns, " runs using ", cores, " cores in parallel):\n"))
			cl <- parallel::makeCluster(cores)
			# Bootstrap:
			out <- pbapply::pblapply(X, FUN, ..., cl=cl)
			# End the parallel bootstrapping:
			parallel::stopCluster(cl)
		}
		else{
			cat(paste0(msg, "(", nruns, " runs):\n"))
			out <- pbapply::pblapply(X, FUN, ...)
		}
		return(out)
	}
	
	# Function for getting a string with the current time: 
	now <- function(brackets=FALSE){
		out <- format(Sys.time(),tz="UTC", "%Y-%m-%d_%H.%M.%S")
		if(brackets){
			out <- paste0("[", out, "] ")
		}
		out
	}
	
	# Function for running the r scripts of a project and copying the relevant output files to the "Output" directory:
	runProject <- function(projectName, progressFile, outputDir, RstoxVersion){
		# Get the path to the scripts to run:
		r_script <- file.path(projectName, "output", "R", "r.R")
		rreport_script <- file.path(projectName, "output", "R", "r-report.R")
		# Generate the r scripts:
		generateRScripts(projectName)
	
		# Run the scripts and print info to the progress file:
		cat(paste0("Running project ", i, ": ", projectName, ":\n"))
		write(paste0(now(TRUE), "Starting project ", i, ": ", projectName), progressFile, append=TRUE)
		
		# Run the baseline and baseline report (the latter with input=NULL):
		# The parameter 'modelType', enabling reading Baseline Report, was introduced in 1.8.1:
		if(RstoxVersion$Rstox > "1.8"){
			write(paste0(now(TRUE), "Running Baseline and Baseline Report"), progressFile, append=TRUE)
			baselineOutput <- getBaseline(projectName, exportCSV=TRUE, modelType="baseline")
			baselineReportOutput <- getBaseline(projectName, exportCSV=TRUE, modelType="report", input=NULL)
		}
		else{
			write(paste0(now(TRUE), "Running Baseline"), progressFile, append=TRUE)
			baselineOutput <- getBaseline(projectName, exportCSV=TRUE)
		}
		
		
		write(paste0(now(TRUE), "Running r.R"), progressFile, append=TRUE)
		if(file.exists(r_script)){
			source(r_script)
		}
		write(paste0(now(TRUE), "Running r-report.R"), progressFile, append=TRUE)
		if(file.exists(rreport_script)){
			source(rreport_script)
		}
		write(paste0(now(TRUE), "Ending project ", i, ": ", projectName), progressFile, append=TRUE)
		write("", progressFile, append=TRUE)
		closeProject(projectName)
		
		# Copy output files to the output directory:
		dir.create(outputDir, recursive=TRUE)
		output <- file.path(projectName, "output")
		file.copy(output, outputDir, recursive=TRUE)
		
		##### 
		##### outputDirs <- file.path(projectName, "output", c(outer(c("baseline", "r"), c("data", "report"), file.path)))
		##### outputFiles <- unlist(lapply(outputDirs, files, full.names=TRUE))
		##### 
		##### 
		
		### outputRDataDir <- file.path(projectName, "output", "r", "data")
		### outputRReportDir <- file.path(projectName, "output", "r", "report")
		### # List the files:
		### datafiles <- list.files(outputRDataDir, full.names=TRUE)
		### datafiles <- datafiles[!file.info(datafiles)$isdir]
		### reportfiles <- list.files(outputRReportDir, full.names=TRUE)
		### reportfiles <- reportfiles[!file.info(reportfiles)$isdir]
		# Copy and replace:
		### from <- c(datafiles, reportfiles)
		### to <- file.path(outputDir, basename(from))
		##### from <- outputFiles
		##### to <- file.path(outputDir, basename(from))
		##### 
		##### dir.create(outputDir, recursive=TRUE)
		##### file.copy(from=from, to=to, overwrite=TRUE)
		
		# Save also the output from baseline and baseline report to an RData file:
		save(baselineOutput, file=file.path(outputDir, "baselineOutput.RData"))
		if(RstoxVersion$Rstox > "1.8"){
			save(baselineReportOutput, file=file.path(outputDir, "baselineReportOutput.RData"))
		}
		
		
		
		# Copy the project.xml file:
		from <- getProjectPaths("Test_Rstox")$projectXML
		to <- file.path(outputDir, "project.xml")
		file.copy(from=from, to=to, overwrite=TRUE)
		
		cat("\n")
	}
	
	# Function for running diff between the previous and new output files:
	diffOutputfiles <- function(projectNames, dir1, dir2, diffdir, progressFile){
		
		# List all projects in the latest and new output directory:
		dir1 <- file.path(dir1, basename(projectPaths))
		dir2 <- file.path(dir2, basename(projectPaths))
	
		tempdiff <- file.path(path.expand(diffdir), "tempdiff.txt")
		for(i in seq_along(dir1)){
			# Platform dependent diff commands:
			if(.Platform$OS.type == "windows"){
				cmd <- paste(c("FC", shQuote(dir1[i]), shQuote(dir2[i]), paste0(">", shQuote(tempdiff))), collapse=" ")
			}
			else if(.Platform$OS.type == "unix"){
				cmd <- paste(c("diff", "-r", shQuote(dir1[i]), shQuote(dir2[i]), paste0(">", shQuote(tempdiff))), collapse=" ")
			}
			else{
				stop("Unknown system. Must be one of UNIX or Windows")
			}
	
			# Run the diff as a system call and print to the temp file:
			system(cmd)
			# -x '*.bmp' -x '*.jpeg' -x '*.png' -x '*.tiff' -x '*.RData'
			
			# Read the tempdiff file and append to the progress file:
			diffinfo <- readLines(tempdiff)
			write("************************************************************", file=progressFile, append=TRUE)
			write(paste0("Diff for test project", projectNames[i]), file=progressFile, append=TRUE)
			write("************************************************************", file=progressFile, append=TRUE)
			if(length(diffinfo)){
				write(diffinfo, file=progressFile, append=TRUE)
			}
			else{
				write("No difference", file=progressFile, append=TRUE)
			}
			write("", file=progressFile, append=TRUE)
		}
		write("", file=progressFile, append=TRUE)
		write("", file=progressFile, append=TRUE)
		return(NULL)
	}
	
	# Function for getting the common images:
	getFilesByExt <- function(dir1, dir2, ext=c("png", "jpg", "jpeg", "tif", "tiff")){
		# Function for getting all image files in a vector of files (returning a list with names corresponding to the file extensions):
		getImagesFilesByExt <- function(x, ext=c("png", "jpg", "jpeg", "tif", "tiff")){
			fileext <- tools::file_ext(x)
			out <- x[fileext %in% ext]
			out
		}
		getMatches <- function(images1, images2){
			common <- intersect(images1, images2)
			onlyInFirst <- setdiff(images1, images2)
			onlyInSecond <- setdiff(images2, images1)
			list(common=common, onlyInFirst=onlyInFirst, onlyInSecond=onlyInSecond)
		}
		
		# Get matching and differing image files:
		images1 <- getImagesFilesByExt(list.files(dir1, recursive=TRUE), ext=ext)
		images2 <- getImagesFilesByExt(list.files(dir2, recursive=TRUE), ext=ext)
		common <- getMatches(images1, images2)
		# Add the input directories:
		common$dir1 <- dir1
		common$dir2 <- dir2
		common
	}
	
	# Function to check diffs between images, and printing the diffs to file:
	imDiff <- function(dir1, dir2, diffdir, progressFile, cores=1){
		imDiffOne <- function(file, dir1, dir2, diffdir, progressFile){
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
				thisdiffdir <- file.path(diffdir, dirname(file))
				outfile <- file.path(thisdiffdir, basename(file))
				suppressWarnings(dir.create(thisdiffdir))
				writeFun(out, outfile)
				# Write a log to the progressFile:
				write(paste0("\nImages\n\t", file1, " and\n\t", file2, "\ndiffer. See an image with the current to the left, the diff in the middle, and the previous image to the right in the file \n\t", outfile, "\n"), progressFile, append=TRUE)
			}
		}
		# Get the images to compare:
		images <- getFilesByExt(dir1=dir1, dir2=dir2)
		# Compare images:
		papply(images$common, imDiffOne, dir1=images$dir1, dir2=images$dir2, diffdir=diffdir, progressFile=progressFile, cores=cores)
	}
	
	# Function for testing difference between RData files:
	RDataDiff <- function(dir1, dir2, progressFile){
		diffRData <- function(file, dir1, dir2, progressFile){
			all.equalOne <- function(name, progressFile){
				write(paste0("\tObject: ", name), file=progressFile, append=TRUE)
				all.equal(tempenvironment1[[name]], tempenvironment2[[name]])
			}
			write(paste0("File: ", file), file=progressFile, append=TRUE)
			# Read the files and diff:
			file1 <- file.path(dir1, file)
			file2 <- file.path(dir2, file)
			assign("tempenvironment1", new.env(), envir=.GlobalEnv)
			assign("tempenvironment2", new.env(), envir=.GlobalEnv)
			x1 <- load(file1, envir=tempenvironment1)
			x2 <- load(file2, envir=tempenvironment2)
			
			out <- NULL
			if(!all.equal(x1, x2)){
				out <- c(
					out, 
					paste0("Non-identical object names in files ", file1, " (", paste0(x1, collapse=", "), ") and ", file2, " (", paste0(x2, collapse=", "), ")")
					)
				# write(paste0("Non-identical object names in files ", file1, " (", paste0(x1, collapse=", "), ") and ", file2, " (", paste0(x2, collapse=", "), ")"), file=progressFile, append=TRUE)
			}
			diffs <- lapply(x1, all.equalOne, progressFile=progressFile)
			nodiff <- unlist(lapply(diffs, isTRUE))
			
			if(!all(nodiff)){
				out <- c(
					out, 
					paste0("Non-identical objects in files ", file1, " and ", file2, "(", paste0(x1[!nodiff], collapse=", "), "):"), 
					unlist(lapply(diffs[!nodiff], function(x) paste("\t", x))), 
					""
					)
				#write(paste0("\t\tNon-identical objects in files ", file1, " and ", file2, "(", paste0(x1[!nodiff], collapse=", "), "):"), file=progressFile, append=TRUE)
				#write("\t\t************************************************************", file=progressFile, append=TRUE)
				#lapply(diffs[!nodiff], function(x) write(paste("\t\t", x), file=progressFile, append=TRUE))
				#write("\t\t************************************************************", file=progressFile, append=TRUE)
			}
			out
		}
	
		write("************************************************************", file=progressFile, append=TRUE)
		write("Comapring RData files", file=progressFile, append=TRUE)
		write("************************************************************", file=progressFile, append=TRUE)
		# Get the RData files to compare:
		RDataFiles <- getFilesByExt(dir1=dir1, dir2=dir2, ext="RData")
		# Compare images:
		out <- lapply(RDataFiles$common, diffRData, dir1=RDataFiles$dir1, dir2=RDataFiles$dir2, progressFile=progressFile)
		write("\n\n************************************************************", file=progressFile, append=TRUE)
		write(unlist(out), file=progressFile, append=TRUE)
		write("************************************************************", file=progressFile, append=TRUE)
	}
	
	diffBaseline <- function(dir, progressFile){
		readBaselineFiles <- function(files){
			# Read the files:
			out <- lapply(files, function(x) read.csv(x, sep="\t", quote=NULL))
			
			# Get the names of the processes and data frames:
			files_split <- strsplit(basename(files), "_")
			dataFrameNames <- sapply(files_split, "[", -1)
			processNames <- sapply(files_split, "[", 2)
			
			# Set the names of the data frames:
			names(out) <- dataFrameNames
			out <- split(out, processNames)
			out
		}
		
		all.equalRstoxStoX <- function(Rstox, StoX){
			namesRstox <- names(Rstox)
			namesStoX <- names(StoX)
			commonDF <- intersect(namesRstox, namesStoX)
			onlyInRstox <- setdiff(namesRstox, namesStoX)
			onlyInStoX <- setdiff(namesStoX, namesRstox)
			
			# Inform files only present in one or the other:
			if(length(onlyInRstox)){
				write("Data frames only in Rstox: ", file=progressFile, append=TRUE)
				lapply(onlyInRstox, write, file=progressFile, append=TRUE)
			}
			if(length(onlyInStoX)){
				write("Data frames only in StoX: ", file=progressFile, append=TRUE)
				lapply(onlyInStoX, write, file=progressFile, append=TRUE)
			}
			if(length(commonDF)){
				write("Data frames common for Rstox and StoX: ", file=progressFile, append=TRUE)
				lapply(commonDF, write, file=progressFile, append=TRUE)
			}
			
			write_all.equal <- function(x, y, name, file){
				d <- all.equal(x, y)
				if(length(d)){
					write("************************************************************", file=progressFile, append=TRUE)
					write(paste0("Comparing the data frame ", name, " from Rstox and StoX"), file=progressFile, append=TRUE)
					write(d, file=progressFile, append=TRUE)
					write("************************************************************", file=progressFile, append=TRUE)
				}
				#else{
				#	write("No difference", file=progressFile, append=TRUE)
				#}
			}
		}
		
		
		# Get the baseline and baseline report saved in RData files:
		baselineOutputFiles <- file.path(dir, c("baselineOutput.RData", "baselineReportOutput.RData"))
		present <- file.exists(baselineOutputFiles)
		if(!any(present)){
			return(NULL)
		}
		baselineOutputFiles <- baselineOutputFiles[present]
		# Load the data to a list:
		dataFromRstox <- lapply(baselineOutputFiles, function(x) mget(load(x)))
		
		# Read also the txt-files from baseline and baseline report:
		baselineDirs <- file.path(dir, "baseline", c("data", "report"))
		baselineFiles <- lapply(baselineDirs, list.files, recursive=TRUE, full.names=TRUE)
		names(baselineFiles) <- c("baselineOutput", "baselineReportOutput")
		# Read the data to a list:
		
		dataFromStoX <- lapply(baselineFiles, readBaselineFiles)
		# Keep only the modelType present in the Rstox output file:
		dataFromStoX <- dataFromStoX[names(dataFromRstox)]
		
		lapply(names(dataFromRstox), function(x) all.equalRstoxStoX(Rstox=dataFromRstox[[x]], StoX=dataFromStoX[[x]]))
	}
	
	getLatestDir <- function(dir, Rstox){
		All <- list.dirs(dir, recursive=FALSE)
		# Get Rstox versions (requiring that the Rstox version is between the first and possibly second underscore, typically "Rstox_1.8.1"):
		RstoxVersionS <- sapply(strsplit(basename(All), "_"), "[", 2)
		# Select the latest:
		before <- which(RstoxVersionS < Rstox)
		if(length(before)==0){
			warning(paste0("No directories with Rstox version before Rstox version ", Rstox))
			return(NULL)
		}
		# Return the latest before the input Rstox version:
		All[max(before)]
	}
	
	deleteOutput <- function(dir){
		unlink(file.path(dir, "output", "baseline"), recursive=TRUE, force=TRUE)
		unlink(file.path(dir, "output", "r"), recursive=TRUE, force=TRUE)
	}
	
	
	# Name the folder for the output files by the time and Rstox version:
	RstoxVersion <- getRstoxVersion()
	RstoxVersionString <- paste(names(RstoxVersion), unlist(lapply(RstoxVersion, as.character)), sep="_", collapse="_")
	
	#folderName <- paste0("Rstox_", RstoxVersion, "_", now())
	folderName <- paste0("Rstox_", RstoxVersion$Rstox)
	
	dir <- path.expand(dir)
	
	# Get the latest projects:
	ProjectsDir_original <- getLatestDir(file.path(dir, "Projects_original"), RstoxVersion$Rstox)
	
	# Get paths to the original projects and previous output folders:
	ProjectsList_original <- list.dirs(ProjectsDir_original, recursive=FALSE)
	ProjectsDir <- file.path(dir, "Projects")
	
	# First copy all files from ProjectsDir_original to ProjectsDir
	if(copyFromOriginal){
		unlink(ProjectsDir, recursive=TRUE, force=TRUE)
		dir.create(ProjectsDir)
		lapply(ProjectsList_original, file.copy, ProjectsDir, recursive=TRUE)
		
		# Then delete all output files for safety:
		lapply(list.dirs(ProjectsDir, recursive=FALSE), deleteOutput)
	}
	
	# Get all project paths:
	projectPaths <- list.dirs(ProjectsDir, recursive=FALSE)
	
	# Get the outputs directory and the sub directory of the new outputs:
	Output <- file.path(dir, "Output")
	newOutput <- file.path(Output, folderName)
	
	
	#oldOutputs <- list.dirs(Output, recursive=FALSE)
	#oldOutputs <- setdiff(oldOutputs, newOutput)
	## Pick the latest output folder:
	#latestOutput <- tail(oldOutputs, 1)
	# List all projects in the latest and new output directory:
	newOutputList <- file.path(newOutput, basename(projectPaths))
	#latestOutputList <- file.path(latestOutput, basename(projectPaths))
	
	# Then run through all projects, printing progress to a file:
	suppressWarnings(dir.create(file.path(dir, "Diff")))
	progressFile <- file.path(dir, "Diff", "progress.txt")
	unlink(progressFile)
	
	if("run" %in% process){
		for(i in seq_along(projectPaths)){
			runProject(projectName=projectPaths[i], progressFile=progressFile, outputDir=newOutputList[i], RstoxVersion=RstoxVersion)
		}
	}
	
	# Copy the projects that were run to a new folder in the Projects_original:
	newProjectsDir_original <- file.path(dirname(ProjectsDir_original), RstoxVersionString)
	dir.create(newProjectsDir_original)
	ProjectsList <- list.dirs(ProjectsDir, recursive=FALSE)
	lapply(ProjectsList, file.copy, newProjectsDir_original, recursive=TRUE)
	
	# Get the lastest sub directory of the previously generated outputs:
	latestOutput <- getLatestDir(file.path(dir, "Output"), RstoxVersion$Rstox)
	print(latestOutput)
	
	diffdir <- path.expand(file.path(dir, "Diff", paste("Diff", basename(newOutput), basename(latestOutput), sep="_")))
	suppressWarnings(dir.create(diffdir))
	
	if("diff" %in% process && length(latestOutput)){
		# Run the UNIX diff:
		diffOutputfiles(projectName=projectPaths, dir1=newOutput, dir2=latestOutput, diffdir=diffdir, progressFile=progressFile)
		
		# Special diff of RData files:
		RDataDiff(dir1=newOutput, dir2=latestOutput, progressFile=progressFile)
	
		# Special diff of images:
		imDiff(dir1=newOutput, dir2=latestOutput, diffdir=diffdir, progressFile=progressFile, cores=cores)
		
		# Diff also the baseline output and the files written by baseline:
		diffBaseline(dir=newOutput, progressFile=progressFile)
	}
	
	write("\nPlease also run the example script on ftp://ftp.imr.no/StoX/Download/Rstox/Examples\n", file=progressFile, append=TRUE)
	
	# Copy the progress file to the current diff directory:
	file.copy(progressFile, diffdir)
}


