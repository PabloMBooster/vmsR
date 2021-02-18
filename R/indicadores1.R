indicadores1 <- function(data, ...){

  data_viaje       <- por_viaje(data)
  data_viaje$fecha <- as.POSIXct(format(data_viaje$fecha_arribo, format = "%Y-%m-%d"))
  data_viaje       <- data_viaje[data_viaje$num_lances > 0,]
  x = data_viaje[data_viaje$fecha %in% data_viaje$fecha[1],]
  index_viaje      <- lapply(split(data_viaje, data_viaje$fecha, drop = TRUE),function(x){
    if(length(which(duplicated(x$Cod_Barco, fromLast = T))) > 0){
      x                   <- x[-which(duplicated(x$Cod_Barco, fromLast = T)),]
    }


    fecha               <- x$fecha[1]
    barcos              <- length(unique(x$Cod_Barco))
    dur                 <- round(mean(x$duracion, na.rm = TRUE),1)# duracion promedio
    n_lances            <- round(mean(x$num_lances, na.rm = TRUE),1)# numero de lances promedio
    rec_mean            <- round(mean(x$recorrido, na.rm = TRUE),1)# recorrido promedio
    rec_total           <- round(sum(x$recorrido, na.rm = TRUE),1)# recorrido total
    vel_max             <- round(mean(x$vel_max, na.rm = TRUE),1)# velocidad maxima

    t_entre_calas  <- NA
    if(length(x$tiempo_entre_calas[x$tiempo_entre_calas != 0])> 0){
      t_entre_calas  <- round(mean(x$tiempo_entre_calas[x$tiempo_entre_calas != 0], na.rm = TRUE),1)
    }

    lat_max     <- round(max(x$lat_max, na.rm = TRUE),1)
    lat_min     <- round(min(x$lat_min, na.rm = TRUE),1)
    lon_max     <- round(max(x$lon_max, na.rm = TRUE),1)
    lon_min     <- round(min(x$lon_min, na.rm = TRUE),1)

    cbind.data.frame(fecha, barcos, dur, n_lances, rec_mean, rec_total, t_entre_calas, vel_max)
  })

  index_viaje <- index_viaje %>% lapply(data.frame) %>% bind_rows()
  return(index_viaje)
}
