indicadores2 <- function(data, ...){

  datos_track <- zarpe_arribo_trayectoria(data = data)
  datos_track <- datos_track[datos_track$calas == 1,]
  x <- datos_track[datos_track$fecha == datos_track$fecha[1],]
  index <- lapply(split(datos_track, datos_track$fecha, drop = TRUE),function(x){

    fecha             <- x$fecha[1]
    barcos            <- length(unique(x$Cod_Barco))
    area0             <- area_cubierta(dc = x$dist_costa,lat = x$Lat_calas, grado = 1/12)
    area              <- area0$area# area ocupada
    dc_mean           <- round(mean(x$dist_costa, na.rm = TRUE),1)
    lat_mean          <- round(mean(x$Lat_calas, na.rm = TRUE),1)
    lat_max     <- round(max(x$Lat, na.rm = TRUE),1)
    lat_min     <- round(min(x$Lat, na.rm = TRUE),1)
    lon_max     <- round(max(x$Lon, na.rm = TRUE),1)
    lon_min     <- round(min(x$Lon, na.rm = TRUE),1)


    cbind.data.frame(fecha, barcos, area, dc_mean, lat_mean, lat_max, lat_min, lon_max, lon_min)

  })
  suppressWarnings({index <- index %>% lapply(data.frame) %>% bind_rows()})
  return(index)
}
