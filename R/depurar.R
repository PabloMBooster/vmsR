#require(vmsR)

# base_barco <- data_vms[data_vms$Cod_Barco %in% unique(data_vms$Cod_Barco)[1],]
# c1 <- get_course(lon = base_barco$Lon,lat = base_barco$Lat)
# c2 <- calculate_course(lon = base_barco$Lon,lat = base_barco$Lat)
#
# ## eliminar get_course
# ## ahora implementar funcion de rocío
# cbind(c1,c2)
# x11()
#
# plot(base_barco$Lon, base_barco$Lat, col = "white")
# Arrows(x0 = base_barco$Lon[-length(base_barco$Lon)],
#        y0 = base_barco$Lat[-length(base_barco$Lon)],
#        x1 = base_barco$Lon[-1],
#        y1 = base_barco$Lat[-1], arr.length = 0.2, col = 3)
# text(base_barco$Lon, base_barco$Lat, pos = 2,
#      labels = round(c2), cex = 0.6, col = 1)
#
# map_vms(x = base_barco$Lon,y = base_barco$Lat,
#         velocity = base_barco$Vel_Cal, )
#
#
# rumbo <- function(lon, lat, dist){
#
#   vectorRumbo <- NULL
#   for(i in 2:length(lon)){
#     rad <- pi/180
#
#     x2 <-  lon[i]*rad
#     y2 <-  lat[i]*rad
#     x1 <-  lon[i-1]*rad
#     y1 <-  lat[i-1]*rad
#     d1 <-  dist[i]
#
#     theta <- (180/pi)*acos((sin(y2)-sin(y1)*cos(d1))/(cos(y1)*sin(d1)))
#     diffLon <- x1-x2
#
#     # if(diffLon <= 0){
#     #   theta = theta
#     # }else{
#     #   theta = 360-theta
#     # }
#     vectorRumbo <- rbind(vectorRumbo, theta)
#   }
#   vectorRumbo[1] <- NA
#   vectorRumbo <- as.numeric(vectorRumbo)
# return(vectorRumbo)
# }
#
# # c2 <- calculate_course(lon = base_barco$Lon,lat = base_barco$Lat)
# # c3 <- rumbo(lon = base_barco$Lon, lat = base_barco$Lat, dist = base_barco$Dist_Emisiones)
# cbind(c2,c3, c4)
#
# f(x,y)=180-90*(1+sign(x))* (1-sign(y^2))-45*(2+sign(x))*sign(y)
#
# c4 <- estimateAngle(x =base_barco$Lon,y = base_barco$Lat)



#
# cl = makeCluster(nodes, type = "SOCK")
# registerDoSNOW(cl)
#
# cluster  = foreach(l = seq_along(listaBarco), .inorder = FALSE) %dopar% {
#   .getData2(listaBarco[[l]])
# }
# stopCluster(cl)
#
# for(w in seq_along(cluster)){
#   write.csv(cluster[[w]], file = paste0(directorio,"/",carpeta,"/",paste(cluster[[w]]$CODIGO[1], ".csv", sep = "")))
# }
# require(vmsR)
# base_barco <- data_vms[data_vms$Cod_Barco %in% unique(data_vms$Cod_Barco)[1],]
#
#
# c2 <- rumbo(lon = base_barco$Lon,lat = base_barco$Lat)
#c1 <- calculate_course(lon = base_barco$Lon,lat = base_barco$Lat)
# cbind(c2, c1)
