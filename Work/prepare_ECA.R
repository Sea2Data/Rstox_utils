library(Rstox)
options(java.parameters="-Xmx8g")
# Edvin:
dir <- "/Users/a5362/code/github/Rstox_utils/Work"
ecadir <- "/Users/a5362/code/github/Rstox_utils/Work/tmp/a"
# Arne Johannes:
#dir <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox_utils/Rstox_utils/Work"
#sildeprosjekt: /delphi/Felles/alle/stox/ECA/2015/ECA_sild_2015. Legg til sild == '161724' i filter (annen kode for sild'g03)

projectname <- "ECA_torsk_2015"
#projectname <- "ECA_sild_2015"
#baselineOutput <- getBaseline(projectname)

source(paste(dir, "ECA_input_checks.R", sep="/"))
source(paste(dir, "ECA_input_conversion.R", sep="/"))


get_default_result_dir <- function(projectname, location=getProjectPaths(projectname)$RDataDir){
  return(file.path(location, "reca"))
}


#' see doc for eca.estimate for most parameters
#' @param maxlength maximum length of fish in the data set in cm. If null the value will be extracted from the data.
#' @param resultdir location where R-ECA will store temporal files. Defaults (if null) to a subdirectory of getProjectPaths(projectname)$RDataDir called `reca` whcih will be created if it does not already exist
#' @return outputdir
prepRECA <- function(projectname, resultdir=NULL, minage=1, maxage=20, delta.age=0.001, maxlength=NULL, use_otolithtype=TRUE, hatchDaySlashMonth="01/01"){
  
    if (is.null(resultdir)){
      warning(paste("temporally using non-default ecadir:",path.expand("~/recatmp")))
      resultdir <- get_default_result_dir(projectname, path.expand("~/recatmp"))
      if(!(file.exists(resultdir))){
        dir.create(resultdir, recursive=T)
      }
    }
    if(!(file.exists(resultdir))){
      stop(paste("Directory", resultdir, "does not exist."))
    }
    warning("checking filepath length and char comp")
    if (grepl(" ", resultdir) | nchar(resultdir)>48){
      stop(paste("Please make ecadir", "(current:", resultdir, ") contain no spaces and be shorter than 48 characters."))
    }
  
    warning("write doc for prepECA")
    eca <- baseline2eca(projectname)
    
    #max length in cm
    if (is.null(maxlength)){
      maxlength <- max(eca$biotic$length)      
    }
    #consider if it makes sense to extract from data for minage and maxage as well

    ecaParameters <- list(resultdir=resultdir, minage=minage, maxage=maxage, delta.age=delta.age, maxlength=maxlength, use_otolithtype=use_otolithtype, hatchDaySlashMonth=hatchDaySlashMonth)

    #
    # convert data
    #
    
    GlobalParameters <- getGlobalParameters(eca, ecaParameters)
    Landings <- getLandings(eca, ecaParameters)
    AgeLength <- getLengthGivenAge_Biotic(eca, ecaParameters)
    WeightLength <- getWeightGivenLength_Biotic(eca, ecaParameters)
    
    #
    # Run checks
    #
    
    checkAgeLength(AgeLength)
    checkWeightLength(WeightLength)
    checkCovariateConsistency(AgeLength, Landings$AgeLengthCov)
    checkCovariateConsistency(WeightLength, Landings$WeightLengthCov)
    checkLandings(Landings)
    checkGlobalParameters(GlobalParameters)
    
    #
    # store results
    #
    
    setProjectData(projectName=projectname, var=list(GlobalParameters=GlobalParameters, Landings=Landings, WeightLength=WeightLength, AgeLength=AgeLength), name="prepareRECA")
    #save(GlobalParameters, Landings, WeightLength, AgeLength, file=file.path(outputdir, paste0(projectname, ".RData")))
    #return(outputdir)
}
prepRECA(projectname)
