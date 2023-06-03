dist_cost <- function(lon, lat, polygon = NULL, area =NULL){
  require(sf)
  # convert to sf and project on a projected coord system

  if(is.null(polygon)){
    area <- st_as_sfc(PERU_SP, crs = 4326L)
    area <- st_set_crs(area, 4326L)
    # points with long/lat coords
    pnts <-
      data.frame(
        long = lon,
        lat = lat
      )

    # convert to sf with the same crs
    pnts_sf <- st_as_sf(pnts, crs = 4326L, coords = c("long", "lat"))
    pnts_sf = st_set_crs(pnts_sf, 4326L)

  }else{
    area <- st_as_sfc(PERU_SP, crs = area)
    area <- st_set_crs(area, area)
    # points with long/lat coords
    pnts <-
      data.frame(
        long = lon,
        lat = lat
      )

    # convert to sf with the same crs
    pnts_sf <- st_as_sf(pnts, crs = area, coords = c("long", "lat"))
    pnts_sf = st_set_crs(pnts_sf, area)

  }

  # check if crs are equal
  all.equal(st_crs(pnts_sf),st_crs(area))
  output = as.numeric(st_distance(pnts_sf, area)*0.000539957)

  return(output)

}

#
# dist_cost <- function(lon, lat, polygon = vmsR::PERU_SP, area ="+proj=longlat", proj = "+proj=utm +zone=18 ellips=WGS84"){
#   #require(sp)
#   #require(rgeos)
#   #require(vmsR)
#   #require(rgdal)
#
#   temp = data.frame(lon = lon, lat = lat)
#   posiciones = temp[,c("lon", "lat")]
#   #- Convert VMS data to SpatialPolygons
#   spTa              = SpatialPoints(data.frame(posiciones))
#   proj4string(spTa) = CRS(area)
#   spTa.proj         = spTransform(spTa, CRS(proj))
#   #- Read shapefile of Peru
#   Peru              = as(PERU_SP, "SpatialPolygons")
#   proj4string(Peru) = CRS("+proj=longlat")
#   Peru.proj         = spTransform(Peru, CRS("+proj=utm +zone=18 ellips=WGS84"))
#   dists = gDistance(spgeom1 = spTa.proj, spgeom2=Peru.proj, byid=T) #
#   distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas
#   return(distance)
# }
