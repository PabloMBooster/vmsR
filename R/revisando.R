
# #
# require(vmsR)
# data0 <- data_vms_raw[data_vms_raw$Cod_Barco %in% unique(data_vms_raw$Cod_Barco)[1],]
# data0 <- data[data$Lat < 5,]
# data0 <- data[,1:7]
#
# data  <- processing_vms(data = data0, harbor = harbor)
# data1 <- identify_trajectories(data = data, dharbor = 2,rmin = 6,vmax = 16,hmax = 2.3,vmin = 3)
#
#
#
# identify_trip

# head(harbor)
# head(data_vms_raw)
#
# data_vessel  <- data_vms_raw[data_vms_raw$Cod_Barco == "CO-16854-PM",]# 12232
# output       <- processing_vms(data = data_vessel, vessel = "Cod_Barco", harbor = harbor)
# trip_vessel  <- identify_trip(data = output, vharbor = 2, rmin = 6, hmax = 2.7)
# apply(table(data1$trip, data1$mistake), 2,function(x) length(x[x > 0]))
# # map
#
# map_vms(trip_vessel$Lon[trip_vessel$trip == 1], trip_vessel$Lat[trip_vessel$trip == 1], trip_vessel$Vel_Cal[trip_vessel$trip == 1])

## previo tenemos que usar imagemagic
## funcion para crear las animaciones
## funcion para guardar
## funcion para ver estadisticos
#apply(table(data1$trip, data1$mistake), 2,function(x) length(x[x > 0]))

