identify_trip <- function(data = data, dharbor = 2, rmin = 6,
                           vmax = 16, vmin = 3,hmax = 2.3,dur_viaje = 6, polygon = PERU_SP){

  require(dplyr)
  #require(vmsR)
  data$Vel.Cal[1][is.na(data$Vel.Cal[1])] <- 0
  #data$Time[1][is.na(data$Time[1])] <- 0
  data$Dist_Emisiones[1][is.na(data$Dist_Emisiones[1])] <- 0
  data$Time[is.infinite(data$Time)] <- 0
  data$Time[is.na(data$Time)] <- 0

  # record land:1 / sea:0
  data$id <- 0
  data$id[data$Dist_Harbor < dharbor] <- 1

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

    Location$length <- Location$trip_end - Location$trip_start ## vector length
    trip <- rep(1:length(Location$length),Location$length+1) ## track sequence
    #trip_rep <- rep(Location$rep,Location$length+1)
    trip_share_record <- unique(sort(c(which(Location$rep %in% 1), which(Location$rep %in% 1)-1)))# share record

    #id sequence
    recordsTrip <- NULL
    for(u in seq_along(Location$length)){
      emisiones      <- Location$trip_start[u]:Location$trip_end[u]
      recordsTrip    <- c(recordsTrip, emisiones) #
    }
    data_trip       <- data[recordsTrip,]
    data_trip$trip  <- trip
    data_trip$Records <- recordsTrip
    data_trip$share_record <- 0
    data_trip$share_record[data_trip$trip %in% trip_share_record]<- 1

    ## vessel that don't move
    # if(dim(data_trip)[1] == 0){
    #   data_trip = data
    #   data_trip$mistake <- NA
    #   data_trip$id <- NA
    #   data_trip$trip <- NA
    #   data_trip$Records <- NA
    #   data_trip$share_record <- NA
    # }

  }else{
    data_trip = data
    data_trip$mistake <- NA
    data_trip$id <- NA
    data_trip$trip <- NA
    data_trip$Records <- NA
    data_trip$share_record <- NA

  }
  ## identifying mistakes
  if(!is.na(data_trip$id[1])){

    data_trip$mistake <- 0
    clean_viajes <- lapply(split(data_trip, data_trip$trip, drop = TRUE), function(x){
      y <- x[-1,]
      if(max(y$Vel.Cal) > vmax){ # maximun vel
        x$mistake <- 1 #has some error in velocity
      }
      if(length(y$Cod_Barco) < rmin){ # minimum number of record by trip
        x$mistake <- 1
      }
      #if(max(y$Dist_Emisiones) > vmax*hmax){ # umbral maximun dist
      #y$mistake <- 1 #has some error in distance
      #}
      if(min(y$Time)==0){ # time equals 0
        x$mistake <- 1 #has some error in time
      }
      if(max(y$Time) > hmax){ # umbral maximun time
        x$mistake <- 1 #has some error in time
      }
      if(min(y$Vel.Cal[-length(y$Vel.Cal)]) > vmin){ # umbral minimun velocity
        x$mistake <- 3 # it is not a fishing trip
      }

      as.data.frame(x)
    })
    clean_viajes <- clean_viajes %>% lapply(as.data.frame) %>% bind_rows()
    clean_viajes$mistake[clean_viajes$share_record == 1] <- 2 # error at the beginning to the end

  }else{
    clean_viajes <- data_trip
    clean_viajes$mistake <- 4 # vessels that dont move
  }

  clean_viajes$dist_costa <- estima_dc2(lon = clean_viajes$Lon, lat = clean_viajes$Lat, polygon = polygon)

  clean_viajes$Cod_Viaje_VMS <- paste0(clean_viajes$Cod_Barco, "-",clean_viajes$trip)
  duracion_viaje                   <-  tapply(clean_viajes$Time, clean_viajes$Cod_Viaje_VMS, sum)
  omit_viajes                      <-  names(duracion_viaje[duracion_viaje <= dur_viaje])

  if(length(omit_viajes) > 0){
    clean_viajes  <- clean_viajes[!clean_viajes$Cod_Viaje_VMS %in%  omit_viajes,]
  }

  return(clean_viajes)
}
