# require(lubridate)
# require(vmsR)
# data0 <- data_vms_raw[data_vms_raw$Cod_Barco %in% unique(data_vms_raw$Cod_Barco)[1],]
# data0 <- data[data$Lat < 5,]
# data0 <- data[,1:7]
#
# data  <- processing_vms(data = data0, harbor = harbor)
# data1 <- identify_trip(data = data, dharbor = 2,rmin = 6,vmax = 16,hmax = 2.3,vmin = 3)


# # peru esta se uso
# area = "+init=epsg:24891" # similar
# proj4 = "+proj=utm +south +zone=18 +ellps=WGS84" #opcional
#
# library(lubridate)
# projet.data <- projet(data1, area = area, proj4 = proj4)
# oo = intervms(data = projet.data, scale.temporal = 60, Date = "Date")
#

#
# trip <- data1[data1$trip %in% data1$trip[1],]
#
# png("mapa.png")
# map_vms(x = trip$Lon,y = trip$Lat,velocity = trip$Vel_Cal,legend_vel =T )
# dev.off()

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
## interpolacion
## funcion para crear las animaciones
## funcion para guardar
## funcion para ver estadisticos

#apply(table(data1$trip, data1$mistake), 2,function(x) length(x[x > 0]))

