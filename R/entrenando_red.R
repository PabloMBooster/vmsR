# plot(data_vms$Lon_Obs_Ini_Cala, data_vms$Lat_Obs_Ini_Cala)
#
# data_vms[!is.na(data_vms$Cala), "Lon"][1]
# data_vms[!is.na(data_vms$Cala), "Lon_Obs_Ini_Cala"][1]
#
# plot(data_vms[!is.na(data_vms$Cala), "Lon"], data_vms[!is.na(data_vms$Cala), "Lat"], cex = 0.5)
# points(data_vms[!is.na(data_vms$Cala), "Lon_Obs_Ini_Cala"], data_vms[!is.na(data_vms$Cala), "Lat_Obs_Ini_Cala"], col = 2, cex = 0.5)
#
# plot(data_vms[!is.na(data_vms$Cala), "Lon"], data_vms[!is.na(data_vms$Cala), "Lon_Obs_Ini_Cala"])
# plot(data_vms[!is.na(data_vms$Cala), "Lat"], data_vms[!is.na(data_vms$Cala), "Lat_Obs_Ini_Cala"])
# Dif <- data_vms[!is.na(data_vms$Cala), "Lon"] - data_vms[!is.na(data_vms$Cala), "Lon_Obs_Ini_Cala"]
#
# ## entrenar un red solo con velocidades menores a dos
#
# data_vms2 <- data_vms[!is.na(data_vms$Cala) & data_vms$Vel_Cal < 2,]
#
