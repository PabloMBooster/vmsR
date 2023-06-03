estima_dc2 <- function(lon, lat){

  require(sf)
  # convert to sf and project on a projected coord system
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

  # check if crs are equal
  all.equal(st_crs(pnts_sf),st_crs(area))
  output = as.numeric(st_distance(pnts_sf, area)*0.000539957)

  return(output)
}
