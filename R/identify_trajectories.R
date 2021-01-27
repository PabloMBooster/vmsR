identify_trajectories <- function(data,   dharbor = 2, rmin = 6,
                                  vmax = 16, vmin = 3, hmax = 2.3){

  library(dplyr)
  #library(geoR)
  library(rgeos)
  library(sp)

  id_viajes <- lapply(split(data, data$Cod_Barco, drop = TRUE), function(x){
    y <- identify_trip(data = x, dharbor = dharbor, rmin = rmin, vmax = vmax, vmin = vmin, hmax = hmax)

    as.data.frame(y)
  })
  id_viajes <- id_viajes %>% lapply(as.data.frame) %>% bind_rows()
  #id_viajes <- viaje_registro_compartido(data = id_viajes)
  #id_viajes <- eliminar_emisiones_erradas(data = id_viajes)
  id_viajes$dist_costa <- estima_dc2(lon = id_viajes$Lon, lat = id_viajes$Lat)


  return(id_viajes)
}
