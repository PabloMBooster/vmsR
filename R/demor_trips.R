# oo <- data_vms[data_vms$Cod_Viaje_VMS == unique(data_vms$Cod_Viaje_VMS)[1],]
# # data_vms[data_vms$Cod_Viaje_VMS == unique(data_vms$Cod_Viaje_VMS)[1],]
#
# map_vms(oo$Lon, oo$Lat, oo$Vel_VMS)
# lines(linePeru)
#
# ## cargar datos
# ## buscar datos vms 2013
# ¿
#
# names(dat) <- c("num_vessel", "date", "clase", "tipo_rec","lon", "lat", "num_zona", "velocity", "course", "name_vessel")
# names(dat) <- c("Cod_Barco", "Date", "Clase_Emision", "tipo_rec","Lon", "Lat", "Zona", "Vel_VMS", "Rumbo_VMS", "Nombre_Barco")
# names(dat) <- c("Cod_Barco", "Nombre_Barco", "Date","Lon", "Lat", "Vel_VMS", "Rumbo_VMS")
# # unique(dat[dat$Cod_Barco == 17997,"Nombre_Barco"])
# # dat <- dat[dat$Cod_Barco == 17997,]
#
# data <- dat[,c("num_vessel", "date","lon", "lat", "velocity", "course", "name_vessel")]
# data <- dat[,c("Cod_Barco", "Nombre_Barco", "Date","Lon", "Lat", "Vel_VMS", "Rumbo_VMS")]
#
# #data$date <- modTime(data$date)
# data$Date <- modTime(data$Date)
# data$Date <- as.POSIXct(strptime(data$Date , format = "%d/%m/%Y %H:%M"))
#
# obs_vessel <- table(data$Nombre_Barco)
# obs_vessel <- obs_vessel[obs_vessel > 3000]
# data <- data[data$Nombre_Barco %in% names(obs_vessel),]
#
# data$Fecha_Matlab <- modTime(data$Fecha_Matlab)
# #data_vessel <- data[data$num_vessel == data$num_vessel[1], ]
#
# data <- data[data$Cod_Barco == unique(data$Cod_Barco)[1],]
# oo <- table(data$Cod_Barco)
# oo <-oo[oo < 10]
# data <- data[!data$Cod_Barco %in% names(oo),]
#
# output <- processing_vms(data = data, vessel = "Cod_Barco", harbor = harbor)
# codigo <- apply(as.data.frame(output$Cod_Barco), 1, extract_number)
#
# require(foreign)
# dat <- read.dbf("D:/disco_rocio/sisesat_2015_2016_FINAL/2016/01_15enero2016pel.dbf")
#
# names(dat) <- c("num_vessel", "date", "clase", "tipo_rec","lon", "lat", "num_zona", "velocity", "course", "name_vessel")
#
# data <- dat[,c("num_vessel", "date","lon", "lat", "velocity", "course", "name_vessel")]
# data$date <- modTime(data$date)
#
# data0 <- data[data$num_vessel == 17997,]
#
# processing_vessel <- output[codigo ==  21142,]
#
# trip_vessel <- identify_trip(data = processing_vessel, vharbor = 2, rmin = 6, hmax = 2.7)
# trip_vessel2 <- identify_trip(data = processing_vessel, dharbor = 2, vharbor = 2, rmin = 6, hmax = 2.3)
# output2 <- identify_trip(data = processing_vessel)



