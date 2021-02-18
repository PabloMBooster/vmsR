# identificar_cala <- function(data = data, directory = getwd(), thres = 0.51, neurons = 4, loops = 10){
#
#   require(nnet)
#
#   data$date <- as.POSIXct(strptime(as.character(data$Date), format = "%Y-%m-%d %H:%M:%S"))
#   data$date_GMT <- as.POSIXct(data$Date,tz='GMT')
#   data$Cod_Barco <- as.factor(data$Cod_Barco)
#   #data$Clase_Emision <- as.factor(data$Clase_Emision)
#   #data$Zona <- as.factor(data$Zona)
#   data$Puerto_0_Mar_1 <- as.factor(data$id)
#   #data$Cala <- as.factor(data$Cala)
#   #data$Primera_Cala <- as.factor(data$Primera_Cala)
#   #data$Cod_Viaje_VMS <- as.factor(data$Cod_Viaje_VMS)
#   #data$Cod_Viaje_Cruz <- as.factor(data$Cod_Viaje_Cruz)
#   #data$Flota <- as.factor(data$Flota)
#   #data$Pesca_Viaje <- as.factor(data$Pesca_Viaje)
#
#   fechas <- unclass(as.POSIXlt(data$date))
#   data$hora <- fechas$hour + fechas$min/60 + fechas$sec/3600
#
#   data$hora_transf <- cos(data$hora*pi/12)
#   data$cambio_rumbo_transf <- cos(data$Cambio_Rumbo_Calc*pi/180)
#   data$Cod_Viaje_VMS <- paste0(data$Cod_Barco, "-",data$trip)
#   data$Dif_Tiempo <- data$Time
#   data$Puerto_0_Mar_1 <- data$id
#   cod_viajes <- unique(data$Cod_Viaje_VMS)
#
#   data$Change_Speed_1 <- rep(NA,dim(data)[1])
#   data$Change_Speed_2 <- rep(NA,dim(data)[1])
#   data$Acel_1 <- rep(NA,dim(data)[1])
#   data$Acel_2 <- rep(NA,dim(data)[1])
#   data$Cambio_Rumbo_Tiempo <- rep(NA,dim(data)[1])
#
#   for (i in seq_along(cod_viajes)){
#     lignes <- which(data$Cod_Viaje_VMS == cod_viajes[i])
#     data$Change_Speed_1[lignes] <- c(NA,diff(data$Vel_Cal[lignes]))
#     data$Change_Speed_2[lignes] <- c(diff(data$Vel_Cal[lignes]),NA)
#     Dif_T_3 <- c(NA,data$Dif_Tiempo[lignes]) + c(data$Dif_Tiempo[lignes],NA)
#     data$Acel_1[lignes] <- data$Change_Speed_1[lignes]/Dif_T_3[1:(length(Dif_T_3)-1)]
#     data$Acel_2[lignes] <-  c(data$Acel_1[lignes[-1]],NA) #c(data$Acel.1[lignes[2:length(lignes)]],NA)
#     data$Cambio_Rumbo_Tiempo[lignes] <- data$cambio_rumbo_transf[lignes]/Dif_T_3[1:(length(Dif_T_3)-1)]
#   }
#
#   #data[which(is.na(data$Acel_2)), "Cod_Viaje_VMS"]
#   #data[data$Cod_Viaje_VMS %in%  "CO-12974-PM-8",]
#
#   variables <- c("Dist_Emisiones","Vel_Cal","Change_Speed_1","Change_Speed_2","Acel_1","Acel_2","hora_transf",
#                  "cambio_rumbo_transf","Cambio_Rumbo_Tiempo")
#
#   ind_change_speed_1 <- which(is.na(data[,variables[3]]))
#   ind_change_speed_2 <- which(is.na(data[,variables[4]]))
#
#   data$calas <- 0
#   data0 <- data
#   # data0$Cala <- 0
#   data <- data[-c(ind_change_speed_1,ind_change_speed_2),]
#
#   data2 <- data[,variables]
#
#   names(data2) <- c("Dist_Emisiones","Vel.Cal","Change_Speed_1","Change_Speed_2","Acel.1","Acel.2","hora.transf",
#                     "cambio_rumbo_transf","Cambio.Rumbo.Tiempo")
#
#   variables    <- c("Dist_Emisiones","Vel.Cal","Change_Speed_1","Change_Speed_2","Acel.1","Acel.2","hora.transf",
#                     "cambio_rumbo_transf","Cambio.Rumbo.Tiempo")
#
#   data_scaled <- scale(data2[,variables],center=TRUE,scale=TRUE)
#   if(is.null(data$match)) {data$match <- NA}
#
#   covariables <- c("Cod_Barco","Lon","Lat","Puerto_0_Mar_1","Dist_Emisiones","Dif_Tiempo",#"Name_Vessel",
#                    "calas","trip",
#                    "date", "dist_costa","match")
#
#   ##
#   data_scaled <- cbind(data[,covariables],data_scaled)
#
#
#   predichas <- matrix(NA,nrow=dim(data_scaled)[1],ncol=loops)
#   for (ii in 1:loops){
#
#     namefile = system.file("result_nnet", paste0("ann_year_loop_",ii,"_neurons_4.RData"),package = "tasaR")
#     #namefile = paste0(directory,"/result_nnet/ann_year_loop_",ii,"_neurons_",neurons)
#     #load(file = paste0(namefile, ".RData"))
#     load(namefile)
#     predicted <- predict(best.net,data_scaled)
#     predichas[,ii] <- predicted[,2]
#
#   }
#   sets <- as.numeric(apply(predichas,1,mean) > thres)
#
#   data0$calas[-c(ind_change_speed_1,ind_change_speed_2)] <- sets
#
#   if(!is.na(data$match[1])){
#     data01 <- posiciones_cala(data0)
#   }
#
#   return(data0)
# }


seleccionar_vms <- function(data, ...){
  library(dplyr)

  new_vms <- lapply(split(data, data$trip, drop = TRUE), function(x){

    duracion  <- floor(sum(x$Time, na.rm = T))
    seq_vms   <- round(seq(from = 1, to = length(x$Cod_Barco), length.out = duracion))
    y         <- x[seq_vms,]
    y$match   <- seq_vms
    as.data.frame(y)
  })
  new_vms <- new_vms %>% lapply(as.data.frame) %>% bind_rows()
  return(new_vms)
}


posiciones_cala <- function(data){

  data$ncalas <- 0
  data$calas  <- 0
  data$Lon_calas <- NA
  data$Lat_calas <- NA

  id_calas <- lapply(split(data, data$Cod_Viaje_VMS, drop = TRUE), function(x){

    if(sum(x$id_calas) > 0){
      ncalas   <-  1:(length(which(diff(which(x$id_calas == 1)) > 1)) + 1)
      x$ncalas[x$id_calas == 1] <- rep(ncalas , diff(which(x$id_calas == 0))[diff(which(x$id_calas == 0)) > 1] - 1)


      for(i in unique(unique(ncalas))){

        x[x$ncalas == i, "calas"][which.min(x[x$ncalas == i, "Vel.Cal"])[1]] <- 1
        x[x$ncalas == i, "Lon_calas"][which.min(x[x$ncalas == i, "Vel.Cal"])[1]] <- x[x$ncalas == i, "Lon"][which.min(x[x$ncalas == i, "Vel.Cal"])[1]]
        x[x$ncalas == i, "Lat_calas"][which.min(x[x$ncalas == i, "Vel.Cal"])[1]] <- x[x$ncalas == i, "Lat"][which.min(x[x$ncalas == i, "Vel.Cal"])[1]]

      }

    }
    as.data.frame(x)
  })

  id_calas <- id_calas %>% lapply(as.data.frame) %>% bind_rows()
  # id_calas[id_calas$ id_calas$dist_costa < 5
  return(id_calas)
}


## ><>
# data$Cod_Barco_trip <- paste0(data$Cod_Barco,"-",data$trip)
# x <- data[data$Cod_Barco_trip %in% data$Cod_Barco_trip[1],]

por_viaje <- function(data){

  data$Cod_Barco_trip <- paste0(data$Cod_Barco,"-",data$trip)

  data_viaje <- lapply(split(data, data$Cod_Barco_trip, drop = TRUE), function(x){

    if(sum(x$calas) > 0){
      Cod_Barco        <- x$Cod_Barco[1]
      Name_Vessel      <- x$Name_vessel[1]
      puerto_zarpe     <- x$Harbor[1]
      puerto_arribo    <- x$Harbor[length(x$Harbor)]
      fecha_zarpe      <- x$Date[1]
      fecha_arribo     <- x$Date[length(x$Date)]
      duracion         <- difftime(time1 = x$Date[length(x$Date)], time2 = x$Date[1], units = "hours") #sum(x$Time, na.rm = T)
      recorrido        <- sum(x$Dist_Emisiones, na.rm = T)
      dist_costa_max   <- max(x$dist_costa, na.rm = T)
      dist_costa_pesca <- mean(x$dist_costa[x$calas == 1], na.rm = T)
      maxDist_zarpe    <- max(dist_ortodromica(x1 = x$Lon[1],y1 = x$Lat[1],x2 = x$Lon,y2 = x$Lat),na.rm = T)
      maxDist_arribo   <- max(dist_ortodromica(x1 = x$Lon[length(x$Lon)],y1 = x$Lat[length(x$Lon)],x2 = x$Lon,y2 = x$Lat),na.rm = T)
      sinuosidad       <- recorrido/(maxDist_zarpe+maxDist_arribo)
      course_zarpe     <- x$Course[1]
      course_arribo    <- x$Course[length(x$Course)]
      num_lances       <- sum(x$calas, na.rm = T)# ><>

      if(sum(x$calas, na.rm = T)==1){
        tiempo_entre_calas = 0

        lon_cala_mean    <- x$Lon[x$calas == 1]
        lat_cala_mean    <- x$Lat[x$calas == 1]
        lon_primera_cala <- x$Lon[x$calas == 1]
        lat_primera_cala <- x$Lat[x$calas == 1]
        lon_ultima_cala  <- x$Lon[x$calas == 1]
        lat_ultima_cala  <- x$Lat[x$calas == 1]

        recorrido_cala_a_cala  <- 0

      }else{
        tiempo_entre_calas =  as.numeric(difftime(time1 = x$Date[x$calas == 1][length(x$Date[x$calas == 1])],time2 = x$Date[x$calas == 1][1], units = "hours"))

        lon_cala_mean <- mean(x$Lon[x$calas == 1], na.rm = T)
        lat_cala_mean <- mean(x$Lat[x$calas == 1], na.rm = T)
        lon_primera_cala <- x$Lon[x$calas == 1][1]
        lat_primera_cala <- x$Lat[x$calas == 1][1]
        lon_ultima_cala  <- x$Lon[x$calas == 1][length(x$Lon[x$calas == 1])]
        lat_ultima_cala  <- x$Lat[x$calas == 1][length(x$Lon[x$calas == 1])]
        ubicacion_calas  <- range(which(x$calas == 1))
        recorrido_cala_a_cala  <- sum(x$Dist_Emisiones[ubicacion_calas[1]:ubicacion_calas[2]])
      }

      vel_max          <- max(x$Vel.Cal, na.rm = T)

      lat_max <- max(x$Lat, na.rm = T)
      lat_min <- min(x$Lat, na.rm = T)
      lon_max <- max(x$Lon, na.rm = T)
      lon_min <- min(x$Lon, na.rm = T)

    }else{
      Cod_Barco        <- x$Cod_Barco[1]
      Name_Vessel      <- x$Name_vessel[1]
      puerto_zarpe     <- x$Harbor[1]
      puerto_arribo    <- x$Harbor[length(x$Harbor)]
      fecha_zarpe      <- x$Date[1]
      fecha_arribo     <- x$Date[length(x$Date)]
      duracion         <- difftime(time1 = x$Date[length(x$Date)], time2 = x$Date[1], units = "hours") #sum(x$Time, na.rm = T)
      recorrido        <- sum(x$Dist_Emisiones[-1], na.rm = T)
      dist_costa_max   <- max(x$dist_costa, na.rm = T)
      dist_costa_pesca <- 0
      maxDist_zarpe    <- max(dist_ortodromica(x1 = x$Lon[1],y1 = x$Lat[1],x2 = x$Lon,y2 = x$Lat),na.rm = T)
      maxDist_arribo   <- max(dist_ortodromica(x1 = x$Lon[length(x$Lon)],y1 = x$Lat[length(x$Lon)],x2 = x$Lon,y2 = x$Lat),na.rm = T)
      sinuosidad       <- recorrido/(maxDist_zarpe+maxDist_arribo)
      course_zarpe     <- x$Course[1]
      course_arribo    <- x$Course[length(x$Course)]

      num_lances         <- 0
      tiempo_entre_calas <- 0
      vel_max          <- max(x$Vel.Cal, na.rm = T)
      lat_max <- max(x$Lat, na.rm = T)
      lat_min <- min(x$Lat, na.rm = T)
      lon_max <- max(x$Lon, na.rm = T)
      lon_min <- min(x$Lon, na.rm = T)
      lon_cala_mean <- 0
      lat_cala_mean <- 0
      lon_primera_cala <- 0
      lat_primera_cala <- 0
      lon_ultima_cala  <- 0
      lat_ultima_cala  <- 0
      recorrido_cala_a_cala  <- 0
    }
    cbind.data.frame(Cod_Barco, Name_Vessel,  fecha_zarpe, fecha_arribo, puerto_zarpe, puerto_arribo,
                     duracion, recorrido, maxDist_zarpe, maxDist_arribo,dist_costa_max, dist_costa_pesca, num_lances, tiempo_entre_calas, vel_max,
                     lat_max, lat_min, lon_max, lon_min, course_zarpe, course_arribo, sinuosidad, recorrido_cala_a_cala)
    #as.data.frame(data_vessel)
  })
  suppressWarnings({data_viaje <- data_viaje %>% lapply(as.data.frame) %>% bind_rows()})

  return(data_viaje)
}

#x <- data[data$trip == 1,]
## revisar el efecto de usar o no esta funcion
validacion_cala <- function(data){

  validacion <- lapply(split(data, data$trip, drop = TRUE), function(x){
    #print(x$Cod_Viaje_VMS[1])
    # velocidades mayores a 2 y que registren cala # observar
    x$calas[x$calas == 1 & x$Vel.Cal > 2] <- 0
    x$Lon_calas[x$calas != 1 ] <- NA
    x$Lat_calas[x$calas != 1 ] <- NA

    # calas identificadas dentro de las 5 millas
    x$calas[x$calas == 1 & x$dist_costa < 5] <- 0
    x$Lon_calas[x$calas != 1] <- NA
    x$Lat_calas[x$calas != 1] <- NA

    # calas cercas a puerto
    x$calas[x$calas == 1 & x$Dist_Harbor < 5] <- 0
    x$Lon_calas[x$calas != 1] <- NA
    x$Lat_calas[x$calas != 1] <- NA

    # los tres primeros registros no son considerados como cala
    x$calas[1:3] <- 0
    x$Lon_calas[1:3] <- NA
    x$Lat_calas[1:3] <- NA
    #
    # # los tres ultimos registros no son considerados como cala
    x$calas[(length(x$calas)-2):length(x$calas)] <- 0
    x$Lon_calas[(length(x$calas)-2):length(x$calas)] <- NA
    x$Lat_calas[(length(x$calas)-2):length(x$calas)] <- NA
    as.data.frame(x)

  })

  validacion <- validacion %>% lapply(as.data.frame) %>% bind_rows()

  validacion[,-which(names(validacion) %in% c("calas","ncalas"))]
  return(validacion)
}


##
match_vms <- function(data, new_data, ...){
  library(dplyr)

  data_match <- lapply(split(data, data$trip, drop = TRUE), function(x){
    w <- new_data[new_data$trip %in% x$trip[1],]

    # x$calas   <- NA
    # x$ncalas     <- NA
    x$calas      <- 0
    x$Lon_calas  <- NA
    x$Lat_calas  <- NA

    #x$calas[w$match]<- w$calas
    #x$ncalas[w$ncalas] <- w$ncalas
    x$calas[w$match]   <- w$calas
    x$Lon_calas[w$match] <- w$Lon_calas
    x$Lat_calas[w$match] <- w$Lat_calas


    as.data.frame(x)

  })
  data_match <- data_match %>% lapply(as.data.frame) %>% bind_rows()

  return(data_match)
}


##******************************************************

##******************************************************
# viaje_registro_compartido <- function(data){
#   id =  paste0("bb",row.names(data))
#   id = substring(id, nchar(id)-1, nchar(id))
#   id = which(id %in% ".1")
#
#   for(i in id){
#     fecha = seq.POSIXt(from = data[i, "Date"], to = data[i + 1, "Date"], by = "hour")
#     data[i ,"Date"]           <- fecha[length(fecha)-1]
#     data[i ,"Vel_Cal"]        <- 0
#     data[i ,"Dist_Emisiones"] <- 0
#     data[i ,"Time"]           <- 1
#     data[i + 1,"Time"]        <- as.numeric(diff.POSIXt(data[c(i,i+1),"Date"]))
#   }
#   return(data)
# }

##******************************************************
#x <- data[data$trip %in% data$trip[1], ]
# eliminar_emisiones_erradas <- function(data){
#   library(dplyr)
#
#
#   limpiar_filas <- lapply(split(data, data$trip, drop = TRUE), function(x){
#
#     obs_time       <- x$Time[is.na(x$Time)]
#     error_fila     <- rep(0, length(x$Lat))
#     error_fila[-1] <- abs(diff(round(x$Lat,1)))
#     # error en latitud
#     ubicar_error_lat <- which(error_fila > 0.5)
#     if(length(ubicar_error_lat) != 0){
#       x    <- x[-ubicar_error_lat,]
#     }
#     # error en time
#     ubicar_error_time <- which(round(x$Time,2) < 0.05)
#     if(length(ubicar_error_time) != 0){
#       x    <- x[-ubicar_error_time,]
#     }
#
#     if(x$Time[1] > 2.7){
#       x$Time[1] = 1
#     }
#
#     as.data.frame(x)
#   })
#   limpiar_filas <- limpiar_filas %>% lapply(as.data.frame) %>% bind_rows()
#   return(limpiar_filas)
# }

##******************************************************
estima_dc2 <- function(lon, lat, polygon = NULL){
  require(sp)
  require(rgeos)

  temp = data.frame(lon = lon, lat = lat)
  posiciones = temp[,c("lon", "lat")]
  #- Convert VMS data to SpatialPolygons
  spTa              = SpatialPoints(data.frame(posiciones))
  proj4string(spTa) = CRS("+proj=longlat")
  spTa.proj         = spTransform(spTa, CRS("+proj=utm +zone=18 ellips=WGS84"))
  #- Read shapefile of Peru
  if(is.null(polygon)){
    Peru              = as(PERU_SP, "SpatialPolygons")
    proj4string(Peru) = CRS("+proj=longlat")
    Peru.proj         = spTransform(Peru, CRS("+proj=utm +zone=18 ellips=WGS84"))
    dists = gDistance(spgeom1 = spTa.proj, spgeom2=Peru.proj, byid=T) #
    distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas
  }else{
    if(class(polygon) == "SpatialPolygonsDataFrame"){
      stop("polygon is not SpatialPolygonsDataFrame class")
    }
    Peru              = as(polygon, "SpatialPolygons")
    proj4string(Peru) = CRS("+proj=longlat")
    Peru.proj         = spTransform(Peru, CRS("+proj=utm +zone=18 ellips=WGS84"))
    dists = gDistance(spgeom1 = spTa.proj, spgeom2=Peru.proj, byid=T) #
    distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas
  }
  return(distance)
}
