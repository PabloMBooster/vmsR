identify_trip <- function(data = data, dharbor = 2, rmin = 6,
                          vmax = 16, vmin = 3,hmax = 2.3){

  dist_harbor <- data[["Dist_Harbor"]]  # distance to harbor
  speed       <- data[["Vel_Cal"]]      # speed between record
  if(is.na(speed[1])) speed[1] <- 0
  if(is.na(data$Time[1])) data$Time[1] <- 0
  if(is.na(data$Dist_Emisiones[1])) data$Dist_Emisiones[1] <- 0

  data$Time[is.infinite(data$Time)] <- 0
  data$Time[is.na(data$Time)] <- 0


  #registration on land:1 / sea:0
  data$id <- 0
  data$id[dist_harbor < dharbor] <- 1

  data$trip = NA
  idist_harbor  <- which(data$id %in% 1)
  Location      <- data.frame(trip_start = idist_harbor)

  if(dim(Location)[1] == 0){
    Location = data.frame(trip_start =c(1,1))
  }

  Location$length = c(diff(Location$trip_start), 1)
  Location$rep = 0
  if(max(Location$length) > rmin){ ## trips with more than 6 records

    Location            <- Location[Location$length > rmin,] # 4
    Location$trip_end   <- Location$trip_start + Location$length

    #trips that share records
    Location$same       <- c(NA,Location$trip_end[-length(Location$trip_start)] - Location$trip_start[-1])
    if(length(which(Location$same == 0))>0){
      Location[which(Location$same == 0),"rep"] = 1
    }
    Location$length <- Location$trip_end - Location$trip_start # volvemos a calcular los records del viaje
    trip <- rep(1:length(Location$length),Location$length+1) # generamos las secuencia de la trayectoria
    #trip_rep <- rep(Location$rep,Location$length+1)
    trip_share_record <- unique(sort(c(which(Location$rep %in% 1), which(Location$rep %in% 1)-1)))# viajes q comparten un registro

    recordsTrip <- NULL
    for(u in seq_along(Location$length)){
      emisiones      <- Location$trip_start[u]:Location$trip_end[u]
      recordsTrip    <- c(recordsTrip, emisiones) # secuencia de registros
    }
    data_trip       <- data[recordsTrip,]
    data_trip$trip  <- trip
    data_trip$Records <- recordsTrip
    data_trip$share_record <- 0
    data_trip$share_record[data_trip$trip %in% trip_share_record]<- 1

    if(dim(data_trip)[1] == 0){
      data_trip = data
      data_trip$mistake <- NA
      data_trip$id <- NA
      data_trip$trip <- NA
      data_trip$Records <- NA
      data_trip$share_record <- NA
    }
  }

  if(!is.na(data_trip$id[1])){
    ## identifying errors
    data_trip$mistake <- 0
    clean_viajes <- lapply(split(data_trip, data_trip$trip, drop = TRUE), function(x){
      y <- x[-1,]
      if(max(y$Vel_Cal) > vmax){ # velocidad maxima
        y$mistake <- 1
      }
      if(length(y$Cod_Barco) < rmin){ # registros por viaje
        y$mistake <- 1
      }
      if(max(y$Dist_Emisiones) > vmax*hmax){ # distancia maxima erntre emisiones
        y$mistake <- 1
      }
      if(min(y$Time)==0){ # tiempo entre emisiones mayor a 0
        y$mistake <- 1
      }
      if(min(y$Time) > hmax){ # tiempo maximo entre emisiones
        y$mistake <- 1
      }
      if(min(y$Vel_VMS[-length(y$Vel_VMS)]) > vmin){ # Velocidad minima del barco sin considerar el zarpe y arribo
        y$mistake <- 3
      }

      as.data.frame(y)
    })
    clean_viajes <- clean_viajes %>% lapply(as.data.frame) %>% bind_rows()
    clean_viajes$mistake[clean_viajes$share_record == 1] <- 2 # el error se presenta al inicio y final del viaje

  }else{
    clean_viajes <- data_trip
    clean_viajes$mistake <- 4
  }
  return(clean_viajes)
}

