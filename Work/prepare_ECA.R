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

source(paste(dir, "workarounds.R", sep="/"))

source(paste(dir, "ECA_input_checks.R", sep="/"))

source(paste(dir, "ECA_input_conversion.R", sep="/"))




prepECA <- function(projectname){
    eca <- baseline2eca(projectname)
    
    #
    # workarounds
    # Should be eliminated (moved to stox-processes, baseline2eca or functions in this script)
    #
    # replace by data filters in stox or extend reference lists
    # e.g.: stations missing both area and position
    # 
    eca <- filter_missing_data(eca)
    #eca <- impute_catchweight(eca) #Sjekk hva disse er (2015, snr: 39002-39080)
    
    #estimate in in prepECA, redefine function
    #eca <- estimate_catchcount(eca) 
    
    # Koding og filtrering av otolitter må håndteres før use_otolit=TRUE can brukes.
    # ECA crasher om ikke otlittkolonne eskisterer avklar med Hanne
    # eca <- fix_otolithtypes(eca)
    
    #/workarounds
    
    ecaParameters <- list(use_otolithtype=TRUE, hatchDaySlashMonth="01/01")

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
