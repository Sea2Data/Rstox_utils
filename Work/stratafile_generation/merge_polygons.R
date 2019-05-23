library(raster)

#' @param shapes SpatialPolygonsDataFrama
#' @param groups list mapping new names to groups of old names as identified in shapes by namecol
#' @param namecol column identifying names of areas
merge_polygons <- function(shapes, groups, namecol){
  
  names <- c()
  newpolygons <- NULL
  for (g in names(groups)){
    newname <- g
    oldnames <- groups[[newname]]
    
    polygons <- shapes[shapes[[namecol]] %in% oldnames,]
    newpolygon <- raster::aggregate(polygons)

    if (is.null(newpolygons)){
      newpolygons <- newpolygon
    }
    else{
      newpolygons <- union(newpolygons, newpolygon)
    }
    
    names <- c(names, newname)
  }
  
  names <- as.data.frame(list(names))
  names(names) <- namecol
  
  
  return(SpatialPolygonsDataFrame(newpolygons, names))
}

# Can be used for recasting the ICES fom 2018 areas, but it needs some cleaning of suspicious features first
# make mappiing for recasting:
# ll<-as.list(as.character(unique(ices_fom2018_all$SubArea)))
# names(ll) <- unlist(ll)

