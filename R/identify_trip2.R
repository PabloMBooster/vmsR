#data = output
# identify_trip <- function(data = data, dharbor = 2, vharbor = 2, rmin = 6,
#                           vmax = 16, hmax = 2.3, dmin = 5, see = FALSE){
#
#   dist_harbor <- data[["Dist_Harbor"]]  # distance to harbor
#   speed       <- data[["Vel_Cal"]]      # speed between record
#   if(is.na(speed[1])) speed[1] <- 0
#   if(is.na(data$Time[1])) data$Time[1] <- 0
#   if(is.na(data$Dist_Emisiones[1])) data$Dist_Emisiones[1] <- 0
#
#
#   data$mistake <- 0
#   data$mistake[data$Time > hmax] <- 1                          # la hora entre emisiones no mayor a hmax
#   data$mistake[data$Dist_Emisiones*data$Time > vmax*hmax] <- 1 # la distancia entre emision no mayor a vmax*hmax
#   data$mistake[data$Vel_Cal > vmax] <- 1                       # velocidad menor a vmax
#   data$mistake[is.infinite(data$Time) & is.na(data$Time)] <- 1 # detectar NA e Inf
#
#   # mar:0 / tierra:1
#   data$id <- 0
#   data$id[dist_harbor < dharbor] <- 1
#
#   #trips
#   data$trip = NA
#
#   idist_harbor  <- which(data$id %in% 1)
#   Location      <- data.frame(trip_start = idist_harbor)
#
#   if(dim(Location)[1] == 0){
#     Location = data.frame(trip_start =c(1,1))
#   }
#
#   Location$length = c(diff(Location$trip_start), 1)
#   Location$rep = 0
#   if(max(Location$length) > rmin){ ## por lo menos existe un viaje con mas de 6 records
#
#     Location            <- Location[Location$length > rmin,] # 4
#     Location$trip_end   <- Location$trip_start + Location$length
#
#     # viajes que comparten un records
#     Location$same       <- c(NA,Location$trip_end[-length(Location$trip_start)] - Location$trip_start[-1])
#     if(length(which(Location$same == 0))>0){
#       Location[which(Location$same == 0),"rep"] = 1
#     }
#
#     Location$length <- Location$trip_end - Location$trip_start # volvemos a calcular los records del viaje
#     trip <- rep(1:length(Location$length),Location$length+1) # generamos las secuencia de la trayectoria
#     trip_rep <- rep(Location$rep,Location$length+1)
#
#     recordsTrip <- NULL
#     for(u in seq_along(Location$length)){
#       emisiones      <- Location$trip_start[u]:Location$trip_end[u]
#       recordsTrip    <- c(recordsTrip, emisiones) # secuencia de registros
#     }
#     data_trip       <- data[recordsTrip,]
#     data_trip$trip  <- trip
#     data_trip$Records <- recordsTrip
#     data_trip$trip_rep<- trip_rep
# ## ><>><>><>><>><>
#     # clean trips
# # coompletar
#
# ## ><>><>><>><>><>
#
#     if(dim(data_trip)[1] == 0){
#       data_trip = data
#     }
#     if(dim(data_trip)[1] > 0){
#       if(isTRUE(see)){
#         print(length(unique(data_trip$trip)))
#       }#else{
#     }
#   }
#   else{
#     data_trip = data
#   }
#
#   # clean trips
#   #omit_trips <- data_trip[data_trip$mistake == 1, "trip"]
#   #data_trip <- data_trip[!data_trip$trip %in% omit_trips,]
#   return(data_trip)
# }


# clean trips
# clean_trips <- function(data, vmax, hmax, rmin, dmin){
#
#   # maximun speed of trip (< 15 knots)
#   obs_speed         <- tapply(data$Vel_Cal, data$trip, max, na.rm = TRUE)
#   # time beteewn records (< 2.3 hours)
#   obs_time          <- tapply(data$Time, data$trip, max, na.rm = TRUE)
#   # number of records by trip (> 6 records)
#   obs_num_records   <- tapply(data$Dist_Harbor, data$trip, length)
#   # minimun distance to harbor (10 mn)
#   obs_dharbor       <- tapply(data$Dist_Harbor, data$trip, max, na.rm = TRUE)
#
#   names(obs_speed[obs_speed > vmax])
#   names(obs_time[obs_time > hmax])
#   names(obs_num_records[obs_num_records < rmin])
#   names(obs_dharbor[obs_dharbor > dmin])
#
#   return(data)
# }



# omitTrip        = unique(data_trip[data_trip$Vel_Cal > vmax # 15
#                              & !is.na(data_trip$Vel_Cal), "viaje"])
# data_trip       = data_trip[!data_trip$trip %in% omitTrip, ]
# omitTrip        = unique(data_trip[data_trip$Time > hmax # 2.3
#                                & !is.na(data_trip$Time), "viaje"])
# data_trip       = data_trip[!data_trip$trip %in% omitTrip, ]
# omitdist_harbor = tapply(data_trip$Dist_Harbor, data_trip$trip, max)
# omitdist_harbor = as.numeric(names(omitdist_harbor[omitdist_harbor < dmin])) # 5
# data_trip       = data_trip[!data_trip$trip %in% omitdist_harbor, ]
#
# pdf("mapas2.pdf")
#
# for(i in unique(data_trip$trip)){
#   plot(data_trip[data_trip$trip == i, "Lon"], data_trip[data_trip$trip == i, "Lat"], type = "o")
#   points(data_trip2[data_trip2$trip == i, "Lon"], data_trip2[data_trip2$trip == i, "Lat"], cex = 0.5, pch = 16, col = 2)
#   text(x = data_trip[data_trip$trip == i, "Lon"], y = data_trip[data_trip$trip == i, "Lat"],
#        labels = data_trip[data_trip$trip == i, "Records"], pos = 1, cex = 0.4)
#   lines(peruXY)
#   legend("toprigh", legend = round(data_trip[data_trip$trip == i, "Dist_Harbor"][1],1))
# }
#
# dev.off()
#
#
# write.csv(data_trip, "data_trip3.csv")
