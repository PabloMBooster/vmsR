# DEMO --------------------------------------------------------------------
#require(vmsR)
# data(data_vms_raw)




#data_vms_proc <- processing_vms(data = data_vms_raw, vessel = "Cod_Barco", harbor = harbor)
#data_vms_tray <- identify_trip(data = data_vms_proc, vharbor = 2, rmin = 6, hmax = 2.7)

# revisar la estima_dc2
# iniciar la presentación
#data_vms_tray2  <- identify_trajectories(data = data_vms_proc, vharbor = 2, rmin = 6*6, hmax = 2.7)
#data_vms_sele  <- seleccionar_vms(data = data_vms_tray)

# cada 10 min
# data_vms_cala  <- identificar_cala(data = data_vms_sele) ## funciona para ambas frecuencias
# data_vms_cala  <- validacion_cala(data = data_vms_cala)
# data_vms_cala  <- match_vms(data = data_vms_tray, new_data = data_vms_cala)
#




# data_vms_trip    <- data_vms_tra[data_vms_tra$trip == data_vms_tra$trip[1],]
#
# map_vms(data_vms_trip$Lon, data_vms_trip$Lat, data_vms_trip$Vel_Cal, )
# text(data_vms_trip$Lon, data_vms_trip$Lat,
#      labels = round(data_vms_trip$Rumbo_Calc), pos = round(runif(n = 10,min = 1,max = 4)), cex = 0.8)

# realizar un rmarkdown
# mejoramos el mapa
# entrenamiento de la red neuronal
# leer la tesis de Rocio
# darle una mirada a las ayudas del paquete

# para la clase

# 1 instalacion del paquete vmsR
# 2 breve explicacion de github
# 3 temario
# 4 introducciòn al vms
# 5 datos (propiedades de los datos) ---> animacion de los datos y describimos todas las variables

# identificacion de las trayectorias de los viajes de pesca
# que cosa necesitamos para indentificar las trayectorias
# las emisiones cada 1 hora y cada 10 min (tenemos dos frecuencias)
# Cala zero (cuales son los motivos de obtener cala 0)






# plot(c(x1,x2),c(y1,y2))
# arrows(x0 = x1,y0 = y1,x1 = x2,y1 = y2)
#
# course1 <- calculate_course(lon = c(x1,x2), lat = c(y1,y2))
# course2 <- get_course(lon = c(x1,x2), lat = c(y1,y2))


#
# dist_ortodromica0(x1,y1,x2,y2)
# dist_ortodromica(x1,y1,x2,y2)
# dist_ortodromica2(x1,y1,x2,y2)
# dist_ortodromica3(x1,y1,x2,y2)
#
#
# course1 <- calculate_course(lon = data_vms_pro$Lon[1:100], lat = data_vms_pro$Lat[1:100])
# course2 <- get_course(lon = data_vms_pro$Lon[1:100], lat = data_vms_pro$Lat[1:100])
# cbind(course1, course2)
