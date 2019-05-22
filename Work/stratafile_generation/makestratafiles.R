source("load_shapefiles.R")
source("merge_polygons.R")
source("convert_shapefile.R")
source("create_neighbour_matrix.R")
#
# area merge for RECA kolmule 2017, based on Sondre Aanes parameter file for the old eca
#
kolmulemapping <- list()
kolmulemapping[["allareas"]] <- c("08", "09", "28", "41", "42", "04", "05", "06", "07", "12", "30", "31", "34", "36", "37", "43", "47", "48", "49")
kolmule2017 <- merge_polygons(homr_tom2017, kolmulemapping, "HAVOMR")
convertToWKT(kolmule2017, file.path("output", "RECA_kolmule_2017_stratafile.txt"), "komule2017")

#
# area merge for RECA nssk 2018, based on Åge Fotlands parameter file for the old eca
#
nssk_mapping <- list()
nssk_mapping[["Northsea"]] <- c("08", "28", "40", "41", "42", "43", "47")
nssk_mapping[["Skageraak"]] <- c("09")
nssk2018 <- merge_polygons(homr_tom2017, nssk_mapping, "HAVOMR")
convertToWKT(nssk2018, file.path("output", "RECA_nssk_2018_stratafile.txt"), "HAVOMR")
nssk2018_neighbours <- extract_neighbours(nssk2018, namecol="HAVOMR")
save_neighbours(nssk2018_neighbours, file.path("output", "RECA_nssk_2018_neighbours.txt"))
