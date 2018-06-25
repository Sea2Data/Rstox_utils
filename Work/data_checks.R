# We need to implement reports that users can use to decide on data filtration and model parameters.
# Breakdown on samples pr. area and gear, etc.
# We should have a look at current eca reports for that purpose

# We also need to check for some easy-to-make mistakes:
# 
# hetergenous measurements:
# table(baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_CatchSample.txt$lengthmeasurement)
# Note that this needs a field not exported by baseline2eca atm
#
# hetergenous station types (blank, station 1 (part of day-station) and station 2 (gear-experiments))
#
# reference fleet quality coding on stations:
# code 7: all catch sampled
# code 8: only landed fraction sampled
# table(baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_FishStation.txt$trawlquality)
# Note that this needs a field not exported by baseline2eca atm
#
# similarly group at catchsample can be used to indicate if a sample is from landed fraction or from all catch (relevant for q-7 stations in coastal reference fleet)
# table(baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_CatchSample.txt$group)
#
# For stationtype 1 (mainly High-seas reference fleet), q7-stations are subsamples of the entire catch, and total catches are obtained by locating corresponding q8-day-stations.
#

# check that all fixed effect are sampled at all levels

# check what falls in unsampled groups for random effects


# sjekker mot landinger på eca objekt.
# prøvetilfang
# outliers
# fractsjoner ikke dekket

# sjekker av filtreringer mot biotic. Finn ut hvor det går inn i stox-processen:
#- blanding av prøvetyper etc, som skissert over.