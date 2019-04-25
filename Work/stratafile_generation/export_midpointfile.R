source("load_shapefiles.R")
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
save_file(ot, file.path("output", "mainarea_fdir_from_2018_incl.txt"))
ot <- merge_homr_loc(homr_tom2017, loc_tom2017)
save_file(ot, file.path("output", "mainarea_fdir_to_2017_incl.txt"))