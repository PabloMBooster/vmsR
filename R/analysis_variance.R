variance_speed <- function(x){

  x <- x[,c(-1,length(x))]
  ivar <- var(x)
  isd  <- sd(x)
  # criterio para identificar viajes de traslado
  return(criterio)

}

# data_vms[!is.na(data_vms$Cala), "Vel_Cal"]
# omit_viaje <- unique(data_vms[data_vms$Cala == 1 & !is.na(data_vms$Cala) & data_vms$Vel_Cal > 2, "Cod_Viaje_VMS"])
# data_vms2  <- data_vms[!data_vms$Cod_Viaje_VMS %in% omit_viaje, ]
#
# data_vms2$Vel_VMS[!is.na(data_vms2$Cala) & data_vms2$Cala == 1]
#
# # calibration
# loops = 2
# neurons = 4
# MSE_max = 0.04
# nnet_out <- training_nnet(data = data_vms2, directory = getwd(), neurons = neurons, MSE_max = MSE_max, loops = 50)


