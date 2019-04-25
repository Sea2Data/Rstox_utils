library(rgdal)
library(sp)

homr_tom2017 <- readOGR("data/FDIR_HOMR_tom2017", "Hovedomr_der _t.o.m. 2017_")
homr_fom2018 <- readOGR("data/FDIR_HOMR_fom2018", "Hovedomr_der _f.o.m. 2018_")
loc_fom2018 <- readOGR("data/FDIR_lokasjoner_fom2018", "Lokasjoner _f.o.m. 2018_")
loc_tom2017 <- readOGR("data/FDIR_lokasjoner_tom2017", "Lokasjoner _t.o.m. 2017_")
ices_tom2017 <- readOGR("data/ICES_omr_tom2017", "ices_areas_simplifiedt")
ices_fom2018 <- readOGR("data/ICES_omr_fom2018", "ICES-omr_der _f.o.m. 2018_")

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
  
    
}

merge_homr_loc <- function(homr, loc, projection=" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"){
  homr <- spTransform(homr, CRS(projection))
  loc <- spTransform(loc, CRS(projection))

  if (sum(is.na(over(SpatialPoints(coordinates(homr), proj4string = CRS(proj4string(homr))), homr)))){
    stop()
  }
  if (sum(is.na(over(SpatialPoints(coordinates(loc), proj4string = CRS(proj4string(loc))), loc)))){
    stop()
  }
  
  loctab <- as.data.frame(coordinates(loc))
  loctab <- cbind(loc@data[,c("HAVOMR", "Lokasjon")], loctab)
  names(loctab) <- c("omr", "lok", "lon", "lat")
  loctab <- loctab[,c("omr", "lok", "lat", "lon")]
  
  omrtab <- as.data.frame(coordinates(homr))
  omrtab <- cbind(homr@data[,c("HAVOMR")], omrtab)
  names(omrtab) <- c("omr", "lon", "lat")
  omrtab$lok <- NA
  omrtab <- omrtab[,c("omr", "lok", "lat", "lon")]
  
  return(rbind(omrtab, loctab))
}

testmerge <- function(projection=" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"){
  ot <- merge_homr_loc(homr_tom2017, loc_tom2017, projection=projection)  
  
  random_a <- sample(homr_tom2017$HAVOMR, 2)
  homr <- spTransform(homr_tom2017, CRS(projection))
  plot(homr[homr$HAVOMR %in% random_a,], col="blue", main="testm")
  points(ot[ot$omr %in% random_a & is.na(ot$lok), c("lon", "lat")], col="red")
  
  random_loc <- sample(loc_tom2017$Lokasjon, 2)
  loc <- spTransform(loc_tom2017, CRS(projection))
  plot(loc[loc$HAVOMR %in% random_a & loc$Lokasjon %in% random_loc,], border="red")
  points(ot[ot$omr %in% random_a & ot$lok %in% random_loc & !is.na(ot$lok), c("lon", "lat")], col="red")
  
  ot <- merge_homr_loc(homr_tom2017, loc_tom2017, projection=projection)
  
}
save_file <- function(tab, filename){
  tab <- tab[order(tab$omr, tab$lok),]
  tab$lon <- format(round(tab$lon, 5), nsmall=2)
  tab$lat <- format(round(tab$lat, 5), nsmall=2)
  write.table(tab, file=filename, sep="\t", row.names=F, quote = F, na = "")
}

ot <- merge_homr_loc(homr_fom2018, loc_fom2018)
save_file(ot, "mainarea_fdir_from_2018_incl.txt")
ot <- merge_homr_loc(homr_tom2017, loc_tom2017)
save_file(ot, "mainarea_fdir_to_2017_incl.txt")