library(Rstox)
options(java.parameters="-Xmx6g")
# Edvin:
dir <- "/Users/a5362/code/github/Rstox_utils/Work"
outpath <- "/Users/a5362/code/github/Rstox_utils/Work/output"
# Arne Johannes:
#dir <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox_utils/Rstox_utils/Work"
#outpath <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox_utils/output"
#sildeprosjekt: /delphi/Felles/alle/stox/ECA/2015/ECA_sild_2015. Legg til sild == '161724' i filter (annen kode for sild'g03)

projectname <- "ECA_torsk_2015"
#projectname <- "ECA_sild_2015"
#baselineOutput <- getBaseline(projectname)

source(paste(dir, "ECA_input_checks.R", sep="/"))

source(paste(dir, "ECA_input_conversion.R", sep="/"))


#' see doc for eca.estimate for most parameters
#' @param maxlength maximum length of fish in the data set in cm. If null the value will be extracted from the data.
prepECA <- function(projectname, resultdir="ECAres", minage=1, maxage=20, delta.age=0.001, maxlength=NULL, use_otolithtype=TRUE, hatchDaySlashMonth="01/01"){
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
    # save data
    #
    
    save(GlobalParameters, Landings, WeightLength, AgeLength, file=file.path(outpath, paste0(projectname, ".RData")))
}
prepECA(projectname)
