# We need to implement reports that users can use to decide on data filtration and model parameters.
# Breakdown on samples pr. area and gear, etc.
# We should have a look at current eca reports for that purpose

# We also need to check for some easy-to-make mistakes:
# 
# hetergenous measurements:
# table(baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_CatchSample.txt$lengthmeasurement)
# Note that this needs a field not exported by baseline2eca atm
#
# reference fleet quality coding on stations:
# code 7: all catch sampled
# code 8: only landed fraction sampled
# table(baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_FishStation.txt$trawlquality)
# Note that this needs a field not exported by baseline2eca atm
#
# similarly group at catchsample can be used to indicate if a sample is from landed fraction or from all catch (relevant for q-7 stations)
# table(baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_CatchSample.txt$group)
#