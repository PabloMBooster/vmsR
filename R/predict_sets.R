getVar <- function(data){
  library(dplyr)
  variables <- lapply(split(data, data$Cod_Viaje_VMS, drop = TRUE), function(x){
    #lignes <- which(x$Cod_Viaje_VMS == cod_viajes[i])
    x$Change_Speed_1 <- c(NA,diff(x$Vel.Cal))
    x$Change_Speed_2 <- c(diff(x$Vel.Cal),NA)
    Dif_T_3 <- c(NA,x$Time) + c(x$Time,NA)
    x$Acel.1 <- x$Change_Speed_1/Dif_T_3[1:(length(Dif_T_3)-1)]
    x$Acel.2 <-  c(x$Acel.1[-1],NA)
    x$Cambio.Rumbo.Tiempo <- x$cambio_rumbo_transf/Dif_T_3[1:(length(Dif_T_3)-1)]
    as.data.frame(x)

  })
  variables <- variables %>% lapply(as.data.frame) %>% bind_rows()
  return(variables)
}



#### version nueva predict_sets
# predict_sets <- function(data = data, thres = 0.51, loops = 10){
#   variables <- c("Dist_Emisiones","Vel.Cal","Change_Speed_1","Change_Speed_2","Acel.1","Acel.2","hora.transf",
#                  "cambio_rumbo_transf","Cambio.Rumbo.Tiempo")
#
#   ind_change_speed_1 <- which(is.na(data[,variables[3]]))
#   ind_change_speed_2 <- which(is.na(data[,variables[4]]))
#
#   data$calas  <- 0
#   data2       <- data[-c(ind_change_speed_1,ind_change_speed_2),]
#
#
#   data2       <- data2[,variables]
#   data_scaled <- scale(data2,center=TRUE,scale=TRUE)
#
#   predichas <- matrix(NA,nrow=dim(data_scaled)[1],ncol=loops)
#   for (ii in 1:loops){
#     namefile = system.file("result_nnet", paste0("ann_year_loop_",ii,"_neurons_4.RData"),package = "vmsR")
#     load(namefile)
#     predicted <- predict(best.net,data_scaled)
#     predichas[,ii] <- predicted[,2]
#
#   }
#   sets  <- as.numeric(apply(predichas,1,mean) > thres)
#   data$calas[-c(ind_change_speed_1,ind_change_speed_2)] <- sets
#   data  <- posiciones_cala(data = data)
#   #calas <- data$calas
#
#   return(data)
# }

#### version antigua predict_sets
predict_sets <- function(data = data, directory = getwd(), thres = 0.51, neurons = 4, loops = 10){

  require(nnet)

  data$date <- as.POSIXct(strptime(as.character(data$Date), format = "%Y-%m-%d %H:%M:%S"))
  #data$date_GMT <- as.POSIXct(data$date,tz='GMT')
  data$Cod_Barco <- as.factor(data$Cod_Barco)
  #data$Clase_Emision <- as.factor(data$Clase_Emision)
  #data$Zona <- as.factor(data$Zona)
  data$Puerto_0_Mar_1 <- as.factor(data$id)
  #data$Cala <- as.factor(data$Cala)
  #data$Primera_Cala <- as.factor(data$Primera_Cala)
  #data$Cod_Viaje_VMS <- as.factor(data$Cod_Viaje_VMS)
  #data$Cod_Viaje_Cruz <- as.factor(data$Cod_Viaje_Cruz)
  #data$Flota <- as.factor(data$Flota)
  #data$Pesca_Viaje <- as.factor(data$Pesca_Viaje)

  fechas <- unclass(as.POSIXlt(data$date))
  data$hora <- fechas$hour + fechas$min/60 + fechas$sec/3600

  data$hora_transf <- cos(data$hora*pi/12)
  data$cambio_rumbo_transf <- cos(data$Change_Course_Calc*pi/180)
  data$Cod_Viaje_VMS <- paste0(data$Cod_Barco, "-",data$trip)
  data$Dif_Tiempo <- data$Time
  data$Puerto_0_Mar_1 <- data$id
  cod_viajes <- unique(data$Cod_Viaje_VMS)

  data$Change_Speed_1 <- rep(NA,dim(data)[1])
  data$Change_Speed_2 <- rep(NA,dim(data)[1])
  data$Acel_1 <- rep(NA,dim(data)[1])
  data$Acel_2 <- rep(NA,dim(data)[1])
  data$Cambio_Rumbo_Tiempo <- rep(NA,dim(data)[1])

  for (i in seq_along(cod_viajes)){
    lignes <- which(data$Cod_Viaje_VMS == cod_viajes[i])
    data$Change_Speed_1[lignes] <- c(NA,diff(data$Vel.Cal[lignes]))
    data$Change_Speed_2[lignes] <- c(diff(data$Vel.Cal[lignes]),NA)
    Dif_T_3 <- c(NA,data$Dif_Tiempo[lignes]) + c(data$Dif_Tiempo[lignes],NA)
    data$Acel_1[lignes] <- data$Change_Speed_1[lignes]/Dif_T_3[1:(length(Dif_T_3)-1)]
    data$Acel_2[lignes] <-  c(data$Acel_1[lignes[-1]],NA) #c(data$Acel.1[lignes[2:length(lignes)]],NA)
    data$Cambio_Rumbo_Tiempo[lignes] <- data$cambio_rumbo_transf[lignes]/Dif_T_3[1:(length(Dif_T_3)-1)]
  }

  #data[which(is.na(data$Acel_2)), "Cod_Viaje_VMS"]
  #data[data$Cod_Viaje_VMS %in%  "CO-12974-PM-8",]

  variables <- c("Dist_Emisiones","Vel.Cal","Change_Speed_1","Change_Speed_2","Acel_1","Acel_2","hora_transf",
                 "cambio_rumbo_transf","Cambio_Rumbo_Tiempo")

  ind_change_speed_1 <- which(is.na(data[,variables[3]]))
  ind_change_speed_2 <- which(is.na(data[,variables[4]]))

  data$id_calas <- 0
  data0 <- data
  # data0$Cala <- 0
  data <- data[-c(ind_change_speed_1,ind_change_speed_2),]

  data2 <- data[,variables]

  names(data2) <- c("Dist_Emisiones","Vel.Cal","Change_Speed_1","Change_Speed_2","Acel.1","Acel.2","hora.transf",
                    "cambio_rumbo_transf","Cambio.Rumbo.Tiempo")

  variables    <- c("Dist_Emisiones","Vel.Cal","Change_Speed_1","Change_Speed_2","Acel.1","Acel.2","hora.transf",
                    "cambio_rumbo_transf","Cambio.Rumbo.Tiempo")

  data_scaled <- scale(data2[,variables],center=TRUE,scale=TRUE)
  #data_scaled[,5][is.na(data_scaled[,5])] <- median(data_scaled[,5], na.rm = T)
  #data_scaled[,9][is.na(data_scaled[,9])] <- median(data_scaled[,9], na.rm = T)
  # covariables <- c("Nombre_Barco","Cod_Barco","Lon","Lat","Puerto_0_Mar_1","Dist_Puerto","Dif_Tiempo",
  #                  "Lon_Obs_Ini_Cala","Lat_Obs_Ini_Cala","Cala","Primera_Cala","Dist_Cala_Emis","Cod_Viaje_VMS",
  #                  "Cod_Viaje_Cruz","Flota","Pesca_Viaje","date","date_GMT")

  covariables <- c("Name_Vessel","Cod_Barco","Lon","Lat","Puerto_0_Mar_1","Dist_Emisiones","Dif_Tiempo",
                   "id_calas","Cod_Viaje_VMS",
                   "Date", "match")
  #which(names(data) %in% covariables)
  ##
  data_scaled <- cbind(data[,which(names(data) %in% covariables)],data_scaled)


  predichas <- matrix(NA,nrow=dim(data_scaled)[1],ncol=loops)
  for (ii in 1:loops){

    namefile = system.file("result_nnet", paste0("ann_year_loop_",ii,"_neurons_4.RData"),package = "tasaR")
    #namefile = paste0(directory,"/result_nnet/ann_year_loop_",ii,"_neurons_",neurons)
    #load(file = paste0(namefile, ".RData"))
    load(namefile)
    predicted <- predict(best.net,data_scaled)
    predichas[,ii] <- predicted[,2]

  }
  sets <- as.numeric(apply(predichas,1,mean) > thres)

  data0$id_calas[-c(ind_change_speed_1,ind_change_speed_2)] <- sets

  #output <- posiciones_cala(data0)
  #predict_sets <- data0$Cala
  #id_viajes$Cod_Barco_trip <- paste0(data0$Cod_Barco, "_",data0$trip)
  data0 <- posiciones_cala(data0)
  data0 <- validacion_cala(data0)
  return(data0)
}

### identify_cala
identify_set <- function(data = data, thres = 0.51, loops = 10, freq_vms = "low"){

  if(freq_vms == "high"){
    data_sel   <- seleccionar_vms(data)
    data_sel   <- predict_sets(data = data_sel, thres = thres, loops = loops)
    data_sel   <- validacion_cala(data = data_sel)
    output     <- match_vms(data = data, new_data = data_sel)
  }
  if(freq_vms == "low"){
    data$match <- 0
    data_sel   <- predict_sets(data = data, thres = thres, loops = loops)

    nameVMS <- c("Cod_Barco", "Name_vessel", "Date","Lon", "Lat", "Vel_VMS", "Course"
      ,"Harbor","Dist_Harbor","Time", "Dist_Emisiones", "Vel.Cal", "Rumbo_Calc", "Cambio_Rumbo_Calc",
      "mistake","id", "trip", "Records", "trip_rep", "dist_costa", "Cod_Viaje_VMS", "calas", "Lon_calas",
      "Lat_calas")
    #which(names(data_sel) %in% nameVMS)

    output <- data_sel[, which(names(data_sel) %in% nameVMS)]
  }
  output   <- validacion_cala(data = output)
    return(output)
}
