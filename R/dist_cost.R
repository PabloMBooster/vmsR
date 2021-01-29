dist_cost <- function(lon, lat, polygon = vmsR::PERU_SP, area ="+proj=longlat", proj = "+proj=utm +zone=18 ellips=WGS84"){
  require(sp)
  require(rgeos)
  require(vmsR)

  temp = data.frame(lon = lon, lat = lat)
  posiciones = temp[,c("lon", "lat")]
  #- Convert VMS data to SpatialPolygons
  spTa              = SpatialPoints(data.frame(posiciones))
  proj4string(spTa) = CRS(area)
  spTa.proj         = spTransform(spTa, CRS(proj))
  #- Read shapefile of Peru
  Peru              = as(PERU_SP, "SpatialPolygons")
  proj4string(Peru) = CRS("+proj=longlat")
  Peru.proj         = spTransform(Peru, CRS("+proj=utm +zone=18 ellips=WGS84"))
  dists = gDistance(spgeom1 = spTa.proj, spgeom2=Peru.proj, byid=T) #
  distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas
  return(distance)
}
