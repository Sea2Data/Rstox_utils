#*********************************************
#*********************************************
#' Generate species matrix.
#'
#' @param cs				A vector of cruise series names.
#' @param startYear			The first year from which to generate the species matrix.
#' @param downloadProjects	Logical: If TRUE download the projects, which must be done the first time, or if one needs to re-download.
#' @param timeout			Used on Windows if problems with incompletely downloded data occurs (server problems).
#' @param model				The model to use in the projects. Per default only two processes are needed: ReadBioticXML reads the biotic data, and FilterBiotic filters away stations, gear, species and so on. Set FilterBiotic to filter data from ReadBioticXML.
#' @param model				Parameters such as FilterBiotic <- list(BioticData="ReadBioticXML",  FishStationExpr = "gear =~['3270','3271', '3293', '']  and gearcondition < 3 and trawlquality =~['1','3']  and fishstationtype != ['2','C']", SampleExpr = "genetics != '7'").
#' 
#' @return A list of matrices (dropped to a matrix if only one variable is specified in \code{var}) with stations as rows and station information and the species present in the \code{ref} file as columns.
#'
#' @export
#' @rdname GenerateSpeciesMatrix
#' 
## arne.johannes.holmin@hi.no
## espen.johnsen@hi.no
#' 
generateSpeciesMatrix <- function(cs, ref, startYear = 2014, downloadProjects = FALSE, timeout = 60, model = list("ReadBioticXML", FilterBiotic=list(BioticData="ReadBioticXML")), ...){
	# Get the subsets of the cruise series defined by the startYear (requires group="year" in getNMDdata()):
	cs_info <- getNMDinfo("cs")
	cs_years <- lapply(cs_info[cs], function(x) sort(as.numeric(unique(x[, "year"]))))
	subsets <- lapply(cs_years, function(x) which(x >= startYear))

	# Get the years of each cruise series:
	years <- lapply(seq_along(subsets), function(i) cs_years[[i]][subsets[[i]]])
	names(years) <- cs

	# Download the data and create StoX projects (use run=TRUE only the first time to avoid downloading each time the code is run. The output is the project names.):
	if(downloadProjects == TRUE){
	  system.time(pr <- lapply(seq_along(cs), function(i) getNMDdata(cs[i], subdir=TRUE, abbrev=TRUE, subset=subsets[[i]], model=model, group="year", ow=TRUE, timeout=timeout)))
	  }

	# This is only get the project names, and takes approximately 2 minutes (not neccesary if line above is done)
	if(downloadProjects == FALSE){
	  system.time(pr <- lapply(seq_along(cs), function(i) getNMDdata(cs[i], subdir=TRUE, abbrev=TRUE, subset=subsets[[i]], model=model, group="year", run=FALSE)))
	  }
	pr <- lapply(seq_along(pr), function(i) setNames(pr[[i]], years[[i]]))
	names(pr) <- cs

	# Compare "noname" in newest biotic.xml with reference table. Any species missing in the reference file?
	# Define the path to the reference file linking species and species category:
	ref.list <- read.csv2(ref, encoding = "UTF-8")
	pr.last.survey1 <- pr[[1]][length(pr[[1]])]
	pr.last.survey2 <- pr[[2]][length(pr[[2]])]

	# Function
	out.noname <- function(pr,ref.list.noname=ref.list$noname){
		s1 <- getBaseline(pr, proc="ReadBioticXML")
		dat1 <- (s1$outputData$ReadBioticXML$ReadBioticXML_BioticData_CatchSample.txt)
    	
		## Note that all names are converted to lower case 
		noname.pr <- tolower(sort(as.character(unique(dat1$noname[dat1$sampletype < 50]))))
		ref.list.noname <- tolower(sort(as.character(ref.list.noname)))
    	
		## What is in XML file but not in Reference file
		only.in.xml <- setdiff(noname.pr,ref.list.noname)
		print(only.in.xml)
	}
	out.noname(pr.last.survey2)
	out.noname(pr.last.survey1)

	# Generate species matrices for all projects, returned in a list per cruise series, each being a list of years, each being a list of the variables weight and count:
	# This may take approximately several minutes:
	specVar <- "noname" ## Fra biotic og må være på plass i ref. fil (dette er linken)
	catVar <- "Speccat" ## Fra referanse fil
	bioticProc <- "FilterBiotic"
	stationVar <- c("cruise", "serialno")
	var <- c("weight", "count")

	## All years using the same Speccat
	print(system.time(speciesMatrices <- lapply(pr, function(x) lapply(x, aggregateBySpeciesCategory, ref=ref, specVar=specVar, catVar=catVar, bioticProc=bioticProc, stationVar=stationVar, var=var, FilterBiotic=FilterBiotic, msg=TRUE))))

	## Select different Speccat for different years:: Edda requested this option
	year.of.change <- 2017
	id1 <- as.numeric(unlist(attributes(pr[[1]]))) < year.of.change 
	id2 <- as.numeric(unlist(attributes(pr[[2]]))) < year.of.change
	pr.old <- list(pr[[1]][id1],pr[[2]][id2]) ##Before year 
	attributes(pr.old) <- attributes(pr)
	pr.new <- list(pr[[1]][!id1],pr[[2]][!id2])
	attributes(pr.new) <- attributes(pr)

	## Latest years
	catVar <- "Speccat" ## Fra referanse fil
	print(system.time(speciesMatrices.new <- lapply(pr.new, function(x) lapply(x, aggregateBySpeciesCategory, ref=ref, specVar=specVar, catVar=catVar, bioticProc=bioticProc, stationVar=stationVar, var=var, FilterBiotic=FilterBiotic, msg=TRUE))))

	## First years
	catVar <- "Speccat" ## Fra referanse fil
	print(system.time(speciesMatrices.old <- lapply(pr.old, function(x) lapply(x, aggregateBySpeciesCategory, ref=ref, specVar=specVar, catVar=catVar, bioticProc=bioticProc, stationVar=stationVar, var=var, FilterBiotic=FilterBiotic, msg=TRUE))))

	## Combine old and new to speciesMatrices
	appendList <- function (x, val) 
	{
		stopifnot(is.list(x), is.list(val))
		xnames <- names(x)
		for (v in names(val)) {
			x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) 
				appendList(x[[v]], val[[v]])
				else c(x[[v]], val[[v]])
		}
		x
	}

	speciesMatrices.both <- appendList(speciesMatrices.old, speciesMatrices.new)
	#print(ls.str(speciesMatrices.both))
	#print(head(speciesMatrices.both[[1]][[1]]))
    #
	#print(ls.str(speciesMatrices))
	#print(head(speciesMatrices[[1]][[1]]))

	## Test data if they are equal
	cat("Identical matrices?")
	print(identical(speciesMatrices.both,speciesMatrices))

	# Merge all years for each cruise series and each variable:
	mergeAllYears <- function(x, var){
		mergeAllYears_OneVar <- function(var, x){
			do.call(rbind, lapply(x, "[[", var))
		}
		out <- lapply(var, mergeAllYears_OneVar, x=x)
		names(out) <- var
		out
	}

	## Do you want to use speciesMatrices or speciesMatrices.both
	speciesMatricesMerged.both <- lapply(speciesMatrices.both, mergeAllYears, var=var)
	#ls.str(speciesMatricesMerged.both)
	# Some overview of data
	#head(speciesMatricesMerged.both$`Barents Sea NOR-RUS ecosystem cruise in autumn`$weight) ## 
	#names(speciesMatricesMerged.both$`Barents Sea NOR-RUS ecosystem cruise in autumn`$count) ## 
    #
	#head(speciesMatricesMerged.both$`Barents Sea NOR-RUS demersal fish cruise in winter`$weight) ## 
	#names(speciesMatricesMerged.both$`Barents Sea NOR-RUS demersal fish cruise in winter`$count) ## 

	speciesMatricesMerged <- lapply(speciesMatrices, mergeAllYears, var=var)
	#ls.str(speciesMatricesMerged)

	# Plot the dates:
	par(mfrow=c(1,2))
	lapply(speciesMatricesMerged.both, function(x) plot(as.Date(x$weight$startdate, format="%d/%m/%Y"), ylab="startdate"))
	## 


	win.w<-speciesMatricesMerged$`Barents Sea NOR-RUS demersal fish cruise in winter`$weight
	win.c<-speciesMatricesMerged$`Barents Sea NOR-RUS demersal fish cruise in winter`$count
	eco.w<-speciesMatricesMerged$`Barents Sea NOR-RUS ecosystem cruise in autumn`$weight
	eco.c<-speciesMatricesMerged$`Barents Sea NOR-RUS ecosystem cruise in autumn`$wcount

	#write.table(eco.c, "C:\\Users\\eddaj\\Desktop\\ecocount.csv", sep=";")

	list(
		speciesMatrices = speciesMatrices, 
		speciesMatrices.both = speciesMatrices.both, 
		speciesMatricesMerged = speciesMatricesMerged, 
		speciesMatricesMerged.both = speciesMatricesMerged.both, 
		win.w = win.w, 
		win.c = win.c, 
		eco.w = eco.w, 
		eco.c = eco.c
	)

	
}


# Downloading may take 30 minutes for all years. Set downloadProjects to TRUE only first time to establish the projects:



#library(devtools)
# If the newest Rstox develop version is required:
#devtools::install_github("Sea2Data/Rstox", ref="develop", force=TRUE)
library(Rstox)

# Check version
sessionInfo()

# Define the years 
 ## As example

# Define the cruise series to generate species matrices from:
#cs <- c("Barents Sea NOR-RUS demersal fish cruise in winter")
cs <- c("Barents Sea NOR-RUS ecosystem cruise in autumn", "Barents Sea NOR-RUS demersal fish cruise in winter")


FilterBiotic <- list(BioticData="ReadBioticXML", FishStationExpr = "gear =~['3270','3271', '3293', '']  and gearcondition < 3 and trawlquality =~['1','3']  and fishstationtype != ['2','C']", SampleExpr = "genetics != '7'")


#ref <- "~/Documents/Produktivt/Prosjekt/PHD_Holmin/2018-01-17 Artsmatriser/artsliste.csv"
ref <- "\\\\delphi\\Prosjekt\\Ecosystem_survey\\Team økotokt\\Biodiversitet fisk\\Artsliste fisk Barentshavet.csv"



M <- generateSpeciesMatrix(cs, ref=ref, startYear = 2014, downloadProjects = FALSE, timeout = 60, model = list("ReadBioticXML", FilterBiotic=list(BioticData="ReadBioticXML")), FilterBiotic=FilterBiotic)

# The matrices win.w, win.c, eco.w and eco.c are maybe the most interesting:
lapply(M, dim)

# Show the years:
lapply(M$speciesMatrices, names)










