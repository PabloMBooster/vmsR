identify_trip <- function(data = data, dharbor = 2, vharbor = 2, rmin = 6,
                          vmax = 16, hmax = 2.3, dmin = 5, see = FALSE){

  dist_harbor <- data[["Dist_Harbor"]]  # distance to harbor
  speed       <- data[["Vel_Cal"]]      # speed between record
  if(is.na(speed[1])) speed[1] <- 0
  if(is.na(data$Time[1])) data$Time[1] <- 0
  data$Dist_Emisiones[is.na(data$Dist_Emisiones)] <- 0

  data$mistake <- 0
  data$mistake[data$Time > hmax] <- 1                # la hora entre emisiones no mayor a hmax
  data$mistake[data$Dist_Emisiones*data$Time > vmax*hmax] <- 1 # la distancia entre emision no mayor a vmax*hmax
  data$mistake[data$Vel_Cal > vmax] <- 1             # velocidad menor a vmax
  data$mistake[is.infinite(data$Time) & is.na((data$Time))] <- 1
  data$Time[1] <- data$Time[2]

  # positions on land and sea
  n  <- length(dist_harbor)
  id <- rep(0, n)
  for (i in 1:n){
    if(dist_harbor[i] < dharbor){ # 2
      if(speed[i] < vharbor){ # 2
        #       if(demision[i] < emiPUERTO){ # 1
        id[i] <- 1
      }else {
        id[i] <- 0
      }
      #    }
    }
  }
  data$id <- id

  #UBICAR LOS VIAJES
  data$trip = NA

  idist_harbor  <- which(data$id %in% 1)
  Location        = data.frame(trip_start = idist_harbor)
  if(dim(Location)[1] == 0){
    Location = data.frame(trip_start =c(1,1))
  }
  Location$length = c(diff(Location$trip_start),1)

  if(max(Location$length) > rmin){
    Location        = Location[Location$length > rmin,] # 4
    Location$trip_end   = Location$trip_start + Location$length
    Location$same   = c(NA,Location$trip_end[-length(Location$trip_start)] - Location$trip_start[-1])
    if(length(which(Location$same == 0))>0){
      Location[which(Location$same == 0),"trip_start"] = Location[which(Location$same == 0),"trip_start"] + 1
    }

    Location$length = Location$trip_end - Location$trip_start
    trip <- rep(1:length(Location$length),Location$length+1)

    recordsTrip <- NULL
    for(u in 1:length(Location$length)){
      emisiones      <- Location$trip_start[u]:Location$trip_end[u]
      recordsTrip <- c(recordsTrip, emisiones)
    }
    data_trip       = data[recordsTrip,]
    data_trip$trip  = trip

    data_trip = data_trip[-Location$trip_start,]

    # filter trips ><>><>><>><>><>><>><>><>><>><>

    # omitTrip        = unique(data_trip[data_trip$Vel_Cal > vmax # 15
    #                              & !is.na(data_trip$Vel_Cal), "viaje"])
    # data_trip       = data_trip[!data_trip$trip %in% omitTrip, ]
    # omitTrip        = unique(data_trip[data_trip$Time > hmax # 2.3
    #                                & !is.na(data_trip$Time), "viaje"])
    # data_trip       = data_trip[!data_trip$trip %in% omitTrip, ]
    # omitdist_harbor = tapply(data_trip$Dist_Harbor, data_trip$trip, max)
    # omitdist_harbor = as.numeric(names(omitdist_harbor[omitdist_harbor < dmin])) # 5
    # data_trip       = data_trip[!data_trip$trip %in% omitdist_harbor, ]

    # ><>><>><>><>><>><>><>><>><>><>

    if(dim(data_trip)[1] == 0){
      data_trip = data
    }
    if(dim(data_trip)[1] > 0){
      if(isTRUE(see)){
        print(length(unique(data_trip$trip)))
      }#else{
    }
  }
  else{
    data_trip = data
  }

  # clean trips
  #omit_trips <- data_trip[data_trip$mistake == 1, "trip"]
  #data_trip <- data_trip[!data_trip$trip %in% omit_trips,]
  return(data_trip)
}


identify_trip0 <- function(data = data, dharbor = 2, vharbor = 5, rmin = 4,
                          vmax = 20, hmax = 2.3, dmin = 5, see = FALSE){

  dist_harbor     <- data[["dist_harbor"]]  # distancia a puerto
  velocity <- data[["velocity_2"]] # velocidad de emision
  #demision   <- data[["distanciaEmision"]] # distancia entre emisiones

  data$mistake <- 0
  # 0 sin mistake
  # 1 velocidad 0 cuando el barco esta en mar
  # 2 el intervalo de horas es mayor a 2
  # 3 la hora en inf y se le pone 0
  velocity[1][is.na(velocity[1])] <- 0
  #demision[1][is.na(demision[1])] <- 0
  velocity[is.na(velocity)] <- 30 # para se eliminado despues
  data$mistake[dist_harbor > dharbor & is.na(velocity)] <- 1 # 2

  #UBICAR LOS PUNTOS EN TIERRA Y MAR
  n  <- length(dist_harbor)
  id <- rep(0, n)
  for (i in 1:n){
    if(dist_harbor[i] < dist_harbor){ # 2
      if(velocity[i] < vharbor){ # 2
        #       if(demision[i] < emiPUERTO){ # 1
        id[i] <- 1
      }else {
        id[i] <- 0
      }
      #    }
    }
  }
  data$id <- id
  #UBICAR LOS VIAJES
  data$mistake[is.infinite(data$time) & is.na((data$time))] <- 3
  data$time[is.infinite(data$time) & is.na(data$time)] <- 3
  data$mistake[data$time > hmax] <- 2
  data$trip = NA

  idist_harbor  <- which(data$id %in% 1)
  Location        = data.frame(trip_start = idist_harbor)
  if(dim(Location)[1] == 0){
    Location = data.frame(trip_start =c(1,1))
  }
  Location$length = c(diff(Location$trip_start),1)

  if(max(Location$length) > rmin){
    Location        = Location[Location$length > rmin,] # 4
    Location$trip_end   = Location$trip_start + Location$length
    Location$same   = c(NA,Location$trip_end[-length(Location$trip_start)] - Location$trip_start[-1])
    if(length(which(Location$same == 0))>0){
      Location[which(Location$same == 0),"trip_start"] = Location[which(Location$same == 0),"trip_start"] + 1
    }

    Location$length = Location$trip_end - Location$trip_start
    trip <- rep(1:length(Location$length),Location$length+1)

    registerTrip <- NULL
    for(u in 1:length(Location$length)){
      emisiones      <- Location$trip_start[u]:Location$trip_end[u]
      registerTrip <- c(registerTrip, emisiones)
    }
    data_trip       = data[registerTrip,]
    data_trip$trip  = trip


    # Clean trips <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    data_trip = data_trip[-Location$trip_start,]
    omitTrip  = unique(data_trip[data_trip$velocity_2 > vmax # 15
                                 & !is.na(data_trip$velocity_2), "viaje"])
    data_trip   = data_trip[!data_trip$trip %in% omitTrip, ]
    omitTrip    = unique(data_trip[data_trip$time > hmax # 2.3
                                   & !is.na(data_trip$time), "viaje"])
    data_trip   = data_trip[!data_trip$trip %in% omitTrip, ]
    omitdist_harbor = tapply(data_trip$dist_harbor, data_trip$trip, max)
    omitdist_harbor = as.numeric(names(omitdist_harbor[omitdist_harbor< dmin])) # 5
    data_trip   = data_trip[!data_trip$trip %in% omitdist_harbor, ]

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # generar un reporte de numero de barcos / total de viajes / viajes malos
    if(dim(data_trip)[1] == 0){
      data_trip = data
    }
    if(dim(data_trip)[1] > 0){
      if(isTRUE(see)){
        print(length(unique(data_trip$trip)))
      }#else{
    }
  }
  else{
    data_trip = data
  }
  return(data_trip)
}
