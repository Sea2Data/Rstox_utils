library(rgdal)
library(sp)

homr_tom2017 <- readOGR("data/FDIR_HOMR_tom2017", "Hovedomr_der _t.o.m. 2017_")
homr_fom2018 <- readOGR("data/FDIR_HOMR_fom2018", "Hovedomr_der _f.o.m. 2018_")
loc_fom2018 <- readOGR("data/FDIR_lokasjoner_fom2018", "Lokasjoner _f.o.m. 2018_")
loc_tom2017 <- readOGR("data/FDIR_lokasjoner_tom2017", "Lokasjoner _t.o.m. 2017_")
ices_fom2018_all <- readOGR("data/ICES_omr_fom2018", "ICES-omr_der _f.o.m. 2018_") #different area coding schemes needs to be simplified
ices_tom2017 <- readOGR("data/ICES_omr_tom2017", "ices_areas_simplifiedt")

testplotshapefiles <- function(projection=" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"){
  # had to mix and match a bit from fdir downloads, so testing to see if OK.
  fdir_old <- readOGR("/Users/a5362/code/github/imrParsers/inst/extdata/resources/FDIR_Statistikk_hovedomraader", "FDIR_Statistikk_hovedomraader")
  
  plot(spTransform(homr_tom2017, CRS(projection)), col="blue", main="2017")
  plot(spTransform(loc_tom2017, CRS(projection)), border="red", add=T, lty=4)
  plot(spTransform(fdir_old, CRS(projection)), main="tom2017", add=T)
  
  
  plot(spTransform(homr_fom2018, CRS(projection)), col="blue", main="2018")
  plot(spTransform(loc_fom2018, CRS(projection)), border="red", add=T, lty=4)
  plot(spTransform(fdir_old, CRS(projection)), main="fom2018", add=T)
  
  plot(spTransform(homr_fom2018, CRS(projection)), border="blue", main="2017 vs 2018")
  plot(spTransform(homr_tom2017, CRS(projection)), border="red", add=T, lty=3)

  plot(spTransform(loc_fom2018, CRS(projection)), border="blue", main="2017 vs 2018")
  plot(spTransform(loc_tom2017, CRS(projection)), border="red", add=T, lty=3)
  
  plot(spTransform(ices_tom2017, CRS(projection)), border="blue", main="ICES vs FDIR tom 2017")
  plot(spTransform(homr_tom2017, CRS(projection)), border="red", add=T, lty=3)
 
  #need to simplify ices fom 2018 before plotting (too detailed coastlines)
   
}

