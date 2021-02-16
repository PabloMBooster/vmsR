predict_sets2 <- function(data = data, directory = getwd(), thres = 0.51, neurons = 4, loops = 10, namefile = NULL){

  require(nnet)

  data$date <- as.POSIXct(strptime(as.character(data$Date), format = "%Y-%m-%d %H:%M:%S"))
  data$Cod_Barco <- as.factor(data$Cod_Barco)
  data$Puerto_0_Mar_1 <- as.factor(data$id)

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

  covariables <- c("Name_Vessel","Cod_Barco","Lon","Lat","Puerto_0_Mar_1","Dist_Emisiones","Dif_Tiempo",
                   "id_calas","Cod_Viaje_VMS",
                   "Date", "match")
  #which(names(data) %in% covariables)
  ##
  data_scaled <- cbind(data[,which(names(data) %in% covariables)],data_scaled)


  predichas <- matrix(NA,nrow=dim(data_scaled)[1],ncol=loops)
  for (ii in 1:loops){

    if(is.null(namefile)){
      namefile = system.file("result_nnet", paste0("ann_year_loop_",ii,"_neurons_4.RData"),package = "vmsR")
    }else{
      namefile = paste0(namefile, paste0("ann_year_loop_",ii,"_neurons_4.RData"))
    }

    load(namefile)
    predicted <- predict(best.net,data_scaled)
    predichas[,ii] <- predicted[,2]

  }
  sets <- as.numeric(apply(predichas,1,mean) > thres)

  data0$id_calas[-c(ind_change_speed_1,ind_change_speed_2)] <- sets

  data0 <- posiciones_cala(data0)
  data0 <- validacion_cala(data0)
  return(data0)
}
