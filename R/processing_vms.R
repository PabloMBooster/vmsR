processing_vms <- function(data, vessel = "Cod_Barco", harbor){

  library(dplyr)

  id_vessel <- lapply(split(data, data$Cod_Barco, drop = TRUE), function(data_vessel){

    data_vessel     <- data_vessel[order(data_vessel$Date),]
    eharbor         <- distance_harbor(data_vessel, harbor)
    data_vessel$Harbor      <- eharbor$name_harbor
    data_vessel$Dist_Harbor <- eharbor$dist_harbor

    data_vessel$Time      <- NA
    data_vessel$Time[2:(length(data_vessel[,1]))] <- (julian(data_vessel$Date[1:(length(data_vessel[,1])-1)])-julian(data_vessel$Date[2:(length(data_vessel[,1]))]))*24*(-1)

    data_vessel$Dist_Emisiones <- NA
    data_vessel$Dist_Emisiones[2:(length(data_vessel[,1]))] <- dist_ortodromica(data_vessel$Lon[1:(length(data_vessel[,1])-1)],data_vessel$Lat[1:(length(data_vessel[,1])-1)],data_vessel$Lon[2:length(data_vessel[,1])],data_vessel$Lat[2:length(data_vessel[,1])])

    data_vessel$Vel_Cal <- NA
    data_vessel$Vel_Cal <- data_vessel$Dist_Emisiones/data_vessel$Time
    data_vessel$Vel_Cal[is.na(data_vessel$Vel_Cal)] <- 0

    #data_vessel$change_speed_1  <- NA
    #data_vessel$change_speed_1[-1]  <- diff(data_vessel$velocity_2)

    #data_vessel$change_speed_2  <- NA
    #data_vessel$change_speed_2[-length(data_vessel$change_speed_1)]  <- diff(data_vessel$velocity_2)

    data_vessel$Rumbo_Calc   <- NA
    data_vessel$Rumbo_Calc   <- calculate_course(lon = data_vessel$Lon, lat = data_vessel$Lat)
    data_vessel$Cambio_Rumbo_Calc <- apply(matrix(data_vessel$Rumbo_Calc), 1, change_course) # change_course function

    as.data.frame(data_vessel)

  })
  id_vessel <- id_vessel %>% lapply(as.data.frame) %>% bind_rows()


  # check_date < length(is.na(id_vessel$Date))
  #
  # if(exists(check_date)){
  #   print(paste0("the date variable has ",check_date,"NA revise it"))
  # }
  return(id_vessel)
}

