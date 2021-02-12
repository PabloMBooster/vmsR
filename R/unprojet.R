unprojet <- function(data, proj4 = NULL, area = NULL){
  require(sp)
  options(warn = - 1)
  xy <- data[c("lon2","lat2")]
  data[c("LONGITUDE","LATITUDE")] <- coordinates(spTransform(SpatialPointsDataFrame(xy,data,
                                                                                    proj4string = CRS(area)),
                                                             CRS(proj4)))
  return(data)
}
