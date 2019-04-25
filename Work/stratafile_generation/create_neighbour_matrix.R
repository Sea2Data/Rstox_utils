library(rgeos)
library(raster)
library(spdep)

#' Read wkt file and create a SpatialPolygonsDataFrame with stratum names in the column name
extract_polygons <- function(wkt_file){
  con=file(wkt_file,open="r")
  lines=readLines(con) 
  close(con)
  
  polygons <- NULL
  for (l in lines){
    ss<-strsplit(l, "\t")[[1]]
    name <- ss[[1]]
    wkt <- ss[[2]]
    polygon<-readWKT(wkt, p4s="+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    polygon$name<-name
    if (is.null(polygons)){
      polygons <- polygon
    }
    else{
      polygons <- bind(polygons, polygon)
    }
  }
  return(polygons)
}

#' Extract a neighbour matrix from a SpatialPolygonsDataFrama as returned by extract_polygons
#' @return binary matrix (0: not neighbours, 1: neighbours), with strata names as rows and columns
extract_neighbours <- function(polygons){
  poly <- extract_polygons(wkt_file)
  row.names(poly) <- as.character(poly$name)
  nb <- poly2nb(poly)
  mat <- nb2mat(nb, style="B")
  colnames(mat) <- rownames(mat)
  return(mat)  
}

#' Save neighbours to format accepted by stox
save_neighbours <- function(neighbourmatrix, outfile){
  out <- file(outfile, open="w")
  write("stratum\tneighbours", out)
  write("\n")
  for (i in 1:nrow(neighbourmatrix)){
    write(paste(rownames(neighbourmatrix)[i], paste(colnames(neighbourmatrix)[neighbourmatrix[i,]==1], collapse=","), sep="\t"), out)
    write("\n")
  }
  close(out)
}

wkt_file = "data/mainarea.txt"
save_neighbours(extract_neighbours(extract_polygons(wkt_file)), "mainarea_neighbour.txt")
