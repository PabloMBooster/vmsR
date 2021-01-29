projet <- function(data, proj4 = NULL, area = NULL){
  require(sp)

  xy <- data[c("Lon","Lat")]
  data[c("LONGITUDE_M","LATITUDE_M")] <- coordinates(spTransform(SpatialPointsDataFrame(xy, data,
                                                                                        proj4string = CRS(proj4),
                                                                                        match.ID = TRUE),CRS(area)))
  return(data)
}

