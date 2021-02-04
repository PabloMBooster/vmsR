processing_vms <- function(data, vessel = "Cod_Barco", harbor){

  library(dplyr)


  id_vessel <- lapply(split(data, data[[vessel]], drop = TRUE), function(data_vessel){

    data_vessel     <- data_vessel[order(data_vessel$Date),]

    if(length(which(duplicated(data_vessel$Date)== T)) > 0){
      data_vessel     <- data_vessel[-which(duplicated(data_vessel$Date)== T),] ## remove duplicated record
    }

    eharbor         <- distance_harbor(data_vessel, harbor)
    data_vessel$Harbor      <- eharbor$name_harbor
    data_vessel$Dist_Harbor <- eharbor$dist_harbor



    data_vessel$Time      <- NA
    data_vessel$Time[2:(length(data_vessel[,1]))] <- (julian(data_vessel$Date[1:(length(data_vessel[,1])-1)])-julian(data_vessel$Date[2:(length(data_vessel[,1]))]))*24*(-1)

    data_vessel$Dist_Emisiones <- NA
    data_vessel$Dist_Emisiones[2:(length(data_vessel[,1]))] <- dist_ortodromica(data_vessel$Lon[1:(length(data_vessel[,1])-1)],data_vessel$Lat[1:(length(data_vessel[,1])-1)],data_vessel$Lon[2:length(data_vessel[,1])],data_vessel$Lat[2:length(data_vessel[,1])])

    data_vessel$Vel.Cal <- NA
    data_vessel$Vel.Cal <- data_vessel$Dist_Emisiones/data_vessel$Time
    data_vessel$Vel.Cal[is.na(data_vessel$Vel.Cal)] <- 0

    data_vessel$Course_Calc   <- NA
    data_vessel$Course_Calc   <- calculate_course(lon = data_vessel$Lon, lat = data_vessel$Lat)
    data_vessel$Change_Course_Calc[2:length(data_vessel$Course_Calc)] <- abs(data_vessel$Course_Calc[-1]-data_vessel$Course_Calc[-length(data_vessel$Course_Calc)])
    #data_vessel$Change_Course_Calc <- apply(matrix(data_vessel$Rumbo_Calc), 1, change_course) # change_course function

    as.data.frame(data_vessel)

  })
  id_vessel <- id_vessel %>% lapply(as.data.frame) %>% bind_rows()

  return(id_vessel)
}

