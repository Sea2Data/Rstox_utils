library(rgeos)
source("load_shapefiles.R")
targetprojection <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

convertToWKT <- function(shapefile, output, namecol, projection=targetprojection){
  shp <- spTransform(shapefile, CRS(projection))
  
  f<-file(output, open="w")
  for (i in 1:nrow(shp)){
    poly <- shp[i,]
    write(paste(as.character(poly[[namecol]]), writeWKT(poly, byid = F),sep="\t"), f)
  }
  close(f)
  
}

#convertToWKT(homr_fom2018, file.path("output", "mainarea_fdir_fom2018_stratafile.txt"), "HAVOMR")
#convertToWKT(homr_tom2017, file.path("output", "mainarea_fdir_tom2017_stratafile.txt"), "HAVOMR")
#convertToWKT(ices_tom2017, file.path("output", "icesarea_tom2017_stratafile.txt"), "ICES_area")
