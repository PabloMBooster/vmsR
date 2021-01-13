#oo <- data_vms[data_vms$Cod_Viaje_VMS == unique(data_vms$Cod_Viaje_VMS)[1],]
# # data_vms[data_vms$Cod_Viaje_VMS == unique(data_vms$Cod_Viaje_VMS)[1],]
#
#map_vms(x = oo$Lon, y = oo$Lat, velocity = oo$Vel_Cal)
# lines(linePeru)
#
# ## cargar datos
# ## buscar datos vms 2013
# names(dat) <- c("num_vessel", "date", "clase", "tipo_rec","lon", "lat", "num_zona", "velocity", "course", "name_vessel")
# names(dat) <- c("Cod_Barco", "Date", "Clase_Emision", "tipo_rec","Lon", "Lat", "Zona", "Vel_VMS", "Rumbo_VMS", "Nombre_Barco")
# names(dat) <- c("Cod_Barco", "Nombre_Barco", "Date","Lon", "Lat", "Vel_VMS", "Rumbo_VMS")
# unique(dat[dat$Cod_Barco == 17997,"Nombre_Barco"])
# dat <- dat[dat$Cod_Barco == 17997,]
#
# data <- dat[,c("num_vessel", "date","lon", "lat", "velocity", "course", "name_vessel")]
# data <- dat[,c("Cod_Barco", "Nombre_Barco", "Date","Lon", "Lat", "Vel_VMS", "Rumbo_VMS")]
#
# #data$date <- modTime(data$date)
# data$Date <- modTime(data$Date)
# data$Date <- as.POSIXct(strptime(data$Date , format = "%d/%m/%Y %H:%M"))
# #
# obs_vessel <- table(data$Nombre_Barco)
# obs_vessel <- obs_vessel[obs_vessel > 3000]
# data <- data[data$Nombre_Barco %in% names(obs_vessel),]
# #
# data$Fecha_Matlab <- modTime(data$Fecha_Matlab)
# data_vessel <- data[data$num_vessel == data$num_vessel[1], ]
# #
# data <- data[data$Cod_Barco == unique(data$Cod_Barco)[1],]
# oo <- table(data$Cod_Barco)
# # oo <-oo[oo < 10]
# data <- data[!data$Cod_Barco %in% names(oo),]
# #
# output <- processing_vms(data = data_vms_raw, vessel = "Cod_Barco", harbor = harbor)
# codigo <- apply(as.data.frame(output$Cod_Barco), 1, extract_number)
# #


#require(foreign)
#dat <- read.dbf("D:/disco_rocio/sisesat_2015_2016_FINAL/2016/01_15enero2016pel.dbf")
#dat <- read.dbf("E:/disco_rocio/sisesat_2015_2016_FINAL/2016/01_15enero2016pel.dbf")


#### <><#### <><#### <><#### <><#### <><#### <><#### <><#### <><#### <><#### <><
# dat <- read.csv("D:/disco_rocio/disco_duro2/vms_prueba_tesis/data_vms/2013/data2013.csv")
# dat <- dat[,-1]
# names(dat) <- c("Cod_Barco", "Name_vessel","Date","Lon", "Lat", "Vel_VMS", "Course")
# dat$Date <- as.POSIXct(strptime(dat$Date, format = "%d/%m/%Y %H:%M"))
# #

# data0 <- data_vms_raw[data_vms_raw$Cod_Barco == "CO-16854-PM",]# 12232
# output <- processing_vms(data = data0, vessel = "Cod_Barco", harbor = harbor)



# data_vms_raw <- dat
#
# save(data_vms_raw, file = "data/data_vms_raw.RData")



# names(dat) <- c("num_vessel", "date", "clase", "tipo_rec","lon", "lat", "num_zona", "velocity", "course", "name_vessel")
#
# data <- dat[,c("num_vessel", "date","lon", "lat", "velocity", "course", "name_vessel")]
# as.POSIXct(strptime(data$Date, format = "%d/%m/%Y %H:%M"))
#
# data$date <- modTime(data$date)
#
# names(data) <- c("Cod_Barco", "Date", "Lon", "Lat", "Vel_VMS", "Course", "Name_vessel")
# data0 <- data[data$Cod_Barco == 12074,]# 12232
#
#output <- processing_vms(data = data_vms_raw, vessel = "Cod_Barco", harbor = harbor)
#
# plot(output$Dist_Harbor[output$Dist_Harbor < 1], type = "l", ylim = c(0,15))
# lines(output$Vel_Cal[output$Dist_Harbor < 1], col = 2)
# #
# trip_vessel <- identify_trip(data = output, vharbor = 2, rmin = 6, hmax = 2.7)
# trip_vessel0 <- trip_vessel[trip_vessel$trip == 12,]
#
# plot(trip_vessel0$Dist_Emisiones, type = "l")
#
# map_vms(trip_vessel0$Lon, trip_vessel0$Lat, trip_vessel0$Vel_Cal)
# which(is.na(output$Time))
# output[1:9, "Date"]
#
#
# processing_vessel <- output[codigo ==  21142,]

#### <><#### <><#### <><#### <><#### <><#### <><
# trip_vessel <- identify_trip(data = processing_vessel, vharbor = 2, rmin = 6, hmax = 2.7)
# trip_vessel2 <- identify_trip(data = processing_vessel, dharbor = 2, vharbor = 2, rmin = 6, hmax = 2.3)
# output2 <- identify_trip(data = processing_vessel)

# peruXY <- linePeru
# save(peruXY, file = "C:/pablo/D/github/vmsR/data/peruXY.RData")

# output$Dist_Emisiones
# ddd  <- cut(output$Dist_Emisiones, breaks = c(0,2,4,6,8,10,12,14,16,650))
# vvv  <- cut(output$Vel_Cal, breaks = c(0,2,4,6,8,10,12,14,16,650))
# output$Dist_Harbor
#
# output2 <- output[output$Cod_Barco == unique(output$Cod_Barco)[5],]
# table(is.na(output2$Time))
#
# ilen <- function(x) length(x[is.na(x)])
# sum(tapply(output$Time, as.character(output$Cod_Barco), ilen))
#
#
# output_Barco <- NULL
# for(i in unique(output$Cod_Barco)){
#   Barco   <- output[output$Cod_Barco == unique(output$Cod_Barco)[i], ]
#   id_trip <- identify_trip(Barco)
#   output_Barco <- rbind(Barco, id_trip)
# }


# identificacion del viaje debe ser por cada barco

# multinucleo identificar viajes

