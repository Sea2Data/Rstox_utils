

# Find files that are RstoxData, RstoxBase or RstoxFramework that are not in the off table:
removeNonOfficial <- function(dir, validFileExtensions = c("tar.gz", "tgz", "zip"), keepOnlyOfficial = FALSE) {
	
	filePattern <- paste0(paste0(".", validFileExtensions, collapse = "|"), "$")
	l <- list.files(dir, recursive = FALSE, full.names = TRUE, pattern = ".tar.gz|.tgz|.zip$")
	off <- RstoxFramework:::readOfficialRstoxPackageVersionsFile(toTable = TRUE)
	
	if(keepOnlyOfficial) {
		off <- subset(off, Official == TRUE)
	}
	
	
	
	relevantPackagePattern <- paste(names(off), collapse = "|")
	areRelevantPackage <- grepl(relevantPackagePattern, l)
	
	officialPackageNames <- unlist(mapply(paste, names(off), off, sep = "_", SIMPLIFY = FALSE))
	officialPackageNames <- c(outer(officialPackageNames, validFileExtensions, paste, sep = "."))
	officialPackageNamesPattern <- paste(officialPackageNames, collapse = "|")
	
	
	areOfficial <- grepl(officialPackageNamesPattern, l)

	doRemove <- areRelevantPackage & !areOfficial
	toRemove <- l[doRemove]
	
	
	# Add the latest:
	pkgfilename <- file.path(dir, "PACKAGES")
	m <- read.dcf(pkgfilename, all = TRUE)
	m <- m[order(m[, "Package"], numeric_version(m[, "Version"])), ]
	m <- m[cumsum(table(m[, "Package"])),]
	latest <- paste(m$Package, m$Version, sep = "_")
	latest <- c(outer(latest, validFileExtensions, paste, sep = "."))
	latest <- file.path(path.expand(dir), latest)
	
	toRemove <- setdiff(toRemove, latest)
	
	# Remove also RstoxAPI.
	toRemove <- c(toRemove, subset(l, grepl("RstoxAPI", l)))
	
	file.remove(toRemove)
	
	
	
	
	# Un-comment this in the scripts:
	# drat::updateRepo(dir)
	# Be aware that this may mess up the order of the packages, as drat::updateRepo() uses drat:::.update_packages_index() that uses tools::update_PACKAGES(), which sorts the packages alphabeticallly. If this is a problem, uncomment the line again and make another commit to fix the order (e.g. if 1.7.2-9001 is ordered before 1.7.2, push a 1.7.3 after commenting the line)

}




bin_dirs <- list.dirs("~/Code/Github/repo/repo/bin")
hasOnlyFiles <- lengths(lapply(bin_dirs, fs::dir_ls, type = "file")) > 0
bin_dirs <- subset(bin_dirs, hasOnlyFiles)
src_dir <- "~/Code/Github/repo/repo/src/contrib"


removeNonOfficial(src_dir, keepOnlyOfficial = TRUE)
lapply(bin_dirs, removeNonOfficial, keepOnlyOfficial = TRUE)


