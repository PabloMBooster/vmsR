predict_sets <- function(data = data, directory = getwd(), thres = 0.51, neurons = 4, loops = 10){

  require(nnet)

  data$date <- as.POSIXct(strptime(as.character(data$Fecha_Matlab), format = "%Y-%m-%d %H:%M"))
  data$date_GMT <- as.POSIXct(data$date,tz='GMT')
  data$Cod_Barco <- as.factor(data$Cod_Barco)
  data$Clase_Emision <- as.factor(data$Clase_Emision)
  data$Zona <- as.factor(data$Zona)
  data$Puerto_0_Mar_1 <- as.factor(data$Puerto_0_Mar_1)
  data$Cala <- as.factor(data$Cala)
  data$Primera_Cala <- as.factor(data$Primera_Cala)
  data$Cod_Viaje_VMS <- as.factor(data$Cod_Viaje_VMS)
  data$Cod_Viaje_Cruz <- as.factor(data$Cod_Viaje_Cruz)
  data$Flota <- as.factor(data$Flota)
  data$Pesca_Viaje <- as.factor(data$Pesca_Viaje)

  fechas <- unclass(as.POSIXlt(data$date))
  data$hora <- fechas$hour + fechas$min/60 + fechas$sec/3600

  data$hora_transf <- cos(data$hora*pi/12)
  data$cambio_rumbo_transf <- cos(data$Cambio_Rumbo_Calc*pi/180)
  cod_viajes <- unique(data$Cod_Viaje_VMS)

  data$Change_Speed_1 <- rep(NA,dim(data)[1])
  data$Change_Speed_2 <- rep(NA,dim(data)[1])
  data$Acel_1 <- rep(NA,dim(data)[1])
  data$Acel_2 <- rep(NA,dim(data)[1])
  data$Cambio_Rumbo_Tiempo <- rep(NA,dim(data)[1])

  for (i in seq_along(cod_viajes)){
    lignes <- which(data$Cod_Viaje_VMS == cod_viajes[i])
    data$Change_Speed_1[lignes] <- c(NA,diff(data$Vel_Cal[lignes]))
    data$Change_Speed_2[lignes] <- c(diff(data$Vel_Cal[lignes]),NA)
    Dif_T_3 <- c(NA,data$Dif_Tiempo[lignes]) + c(data$Dif_Tiempo[lignes],NA)
    data$Acel_1[lignes] <- data$Change_Speed_1[lignes]/Dif_T_3[1:(length(Dif_T_3)-1)]
    data$Acel_2[lignes] <-  c(data$Acel_1[lignes[-1]],NA) #c(data$Acel.1[lignes[2:length(lignes)]],NA)
    data$Cambio_Rumbo_Tiempo[lignes] <- data$cambio_rumbo_transf[lignes]/Dif_T_3[1:(length(Dif_T_3)-1)]
  }

  variables <- c("Dist_Emisiones","Vel_Cal","Change_Speed_1","Change_Speed_2","Acel_1","Acel_2","hora_transf",
                 "cambio_rumbo_transf","Cambio_Rumbo_Tiempo")

  ind_change_speed_1 <- which(is.na(data[,variables[3]]))
  ind_change_speed_2 <- which(is.na(data[,variables[4]]))

  data0 <- data
  data0$Cala <- 0
  data <- data[-c(ind_change_speed_1,ind_change_speed_2),]

  data_scaled <- scale(data[,variables],center=TRUE,scale=TRUE)

  covariables <- c("Nombre_Barco","Cod_Barco","Lon","Lat","Puerto_0_Mar_1","Dist_Puerto","Dif_Tiempo",
                   "Lon_Obs_Ini_Cala","Lat_Obs_Ini_Cala","Cala","Primera_Cala","Dist_Cala_Emis","Cod_Viaje_VMS",
                   "Cod_Viaje_Cruz","Flota","Pesca_Viaje","date","date_GMT")
  data_scaled <- cbind(data[,covariables],data_scaled)

  predichas <- matrix(NA,nrow=dim(data_scaled)[1],ncol=loops)
  for (ii in 1:loops){
    namefile = paste0(directory,"/Result_nnet/ann_year_loop_",ii,"_neurons_",neurons)
    load(file = paste0(namefile, ".RData"))
    predicted <- predict(best_net,data_scaled)
    predichas[,ii] <- predicted[,2]

  }
  sets <- as.numeric(apply(predichas,1,mean) > thres)

  data0$Cala[-c(ind_change_speed_1,ind_change_speed_2)] <- sets
  predict_sets <- data0$Cala

  return(predict_sets)
}
