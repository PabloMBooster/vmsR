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

predict_sets <- function(data = data, thres = 0.51, loops = 10){

  variables <- c("Dist_Emisiones","Vel.Cal","Change_Speed_1","Change_Speed_2","Acel.1","Acel.2","hora.transf",
                 "cambio_rumbo_transf","Cambio.Rumbo.Tiempo")

  ind_change_speed_1 <- which(is.na(data[,variables[3]]))
  ind_change_speed_2 <- which(is.na(data[,variables[4]]))

  data$calas  <- 0
  data2       <- data[-c(ind_change_speed_1,ind_change_speed_2),]

  data2       <- data2[,variables]
  data_scaled <- scale(data2,center=TRUE,scale=TRUE)

  predichas <- matrix(NA,nrow=dim(data_scaled)[1],ncol=loops)
  for (ii in 1:loops){
    namefile = system.file("result_nnet", paste0("ann_year_loop_",ii,"_neurons_4.RData"),package = "vmsR")
    load(namefile)
    predicted <- predict(best.net,data_scaled)
    predichas[,ii] <- predicted[,2]

  }
  sets  <- as.numeric(apply(predichas,1,mean) > thres)
  data$calas[-c(ind_change_speed_1,ind_change_speed_2)] <- sets
  data  <- posiciones_cala(data = data)
  calas <- data$calas

  return(calas)
}

