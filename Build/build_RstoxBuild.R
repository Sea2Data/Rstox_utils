# The package RstoxBuild can build itself. However, this is not without risk, as an error in the build will break this option. If an error occurs during build of RstoxBuild, rebuild the package by sourcing the source code with the following command (possibly replacing with the 'version' and 'Rversion'). :

# Define the directory holding all the Rstox packages stored in folders named by the package, so that the path to the package RstoxBuild under 'rootDir' is RstoxBuild/RstoxBuild which contains the DESCRIPTION, NAMESPACE, R (folder), etc:
buildRstoxBuildFromSource <- function(rootDir = "~/Code/Github"){
	# Source the sorce code of RstoxBuild:
	RstoxBuildSourceCodeFile <- file.path(rootDir, "RstoxBuild", "RstoxBuild", "R", "build.R")
	source(RstoxBuildSourceCodeFile)
	# Then build the RstoxBuild package from the sourced function buildRstoxPackage():
	buildRstoxPackage("RstoxBuild", version="1.0", Rversion="3.5", check=FALSE, rootDir=rootDir)
}
# buildRstoxBuildFromSource()


# Build RstoxBuild:
RstoxBuild::buildRstoxPackage(
	"RstoxBuild", 
	version = "1.0", 
	Rversion = "3.5", 
	check = FALSE, 
	imports = c("usethis", "devtools"), 
	suggests = c("Rstox", "png", "jpeg", "tiff", "rJava", "callr")
)



Aiming for knirkefri development and maintenance of the various Rstox packages the function RstoxBuild::buildRstoxPackage() in the package RstoxBuild does the following:

1. Defines the following specifications of the package:
	a) title: (Mandatory) The title shown in the DESCRIPTION file and when running help(packageName), where packageName is the name of the package.
	b) description: (Mandatory) The description shown in the DESCRIPTION file, when running help(packageName), and in the README file.
	c) details: (Mandatory) The details shown when running help(packageName).
	d) authors: (Mandatory) A list of authors, each given as a list with at least the following elements (see ?person for options):
		- given: The given name of the author
		- family: The given name of the author
		- role: The role name of the author, must be c("cre", "aut") for the package maintainer.
		- email: The email, required for the package maintainer.
	e) .onLoad: (Optional) A function to run on load of the package, such as definitions or global options of the package.
	f) .onAttach: (Optional) A function to run on attach of the package, such as start up message.
	g) misc: (Optional) Other information shown in the README file

2. Writes .onLoad (if given) to the file "onLoad.R", .onAttach (if given) to the file "onAttach.R", and the file "pkgname.R" holding title, description and details shown when running help(packageName).

3. Writes the DESCRIPTION file with title, description, authors, R dependency, URLs and license. 

4. Documents the package using devtools::document(), which also creates the NAMESPACE file.

5. Adds imports, suggests and linkedto to the DESCRIPTION file (the imports are retrieved from the NAMESPACE file).

6. Compiles the and documents any C++ files. These are ignored in point 4 to avoid an error returned from devtools::document().

7. Optionally checks the package with devtools::check(). 

8. Writes the README file in the following structure:
	- Two lines specifying R and package version
	- Package description
	- Install instructions, commonly specified for all Rstox packages
	- Any miscellaneous info, such as specifications for Java, known errors or specific install instrucitons
	- Release notes retrieved from the NEWS file
	
9. Unloads and installs the package.

10. Writes package manual (PDF).

11. Loads the package with library().



To run the function RstoxBuild::buildRstoxPackage() the following are required:

1. A folder named "R" holding the R code.

2. A NEWS file in the following style (version lines with package name, version and release date, followed by changes bulleted by asterix):

	RstoxBuild v1.0 (Release date: 2019-05-03)
	==============

	Changes:

	* First build. Added the functions buildRstoxPackage(), Rstox_package_specs(), getDESCRIPTION(), getREADME(), getImports(), addImportsToDESCRIPTION(), getPkgname(), getGitHub_InstallPath(), getGitHub_NewsLink(), getNews(), isMaster(), and specifications for the packages Rstox, RstoxData, RstoxECA, RstoxSurveyPlanner, RstoxTempdoc and RstoxBuild.
	
3. Any additional files such as resource files in the inst-folder or C++ code in the src-folder.

4. Changes to title, description, details, authors, .onLoad, .onAttach or misc must be done in the RstoxBuild package. You may ask the package meintainer to implement such changes. The .onLoad, .onAttach must be given as a string to write to file.


	
Convension:

