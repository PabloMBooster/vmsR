processing_vms <- function(data, vessel = "num_vessel", harbor){
  library(dplyr)

  id_vessel <- lapply(split(data, data$num_vessel, drop = TRUE),function(data_vessel){

    data_vessel     <- data_vessel[order(data_vessel$date),]
    eharbor         <- distance_harbor(data_vessel, harbor)
    data_vessel$name_harbor <- eharbor$name_harbor
    data_vessel$dist_harbor <- eharbor$dist_harbor

    data_vessel$time      <- NA
    data_vessel$time[2:(length(data_vessel[,1]))] <- (julian(data_vessel$date[1:(length(data_vessel[,1])-1)])-julian(data_vessel$date[2:(length(data_vessel[,1]))]))*24*(-1)

    data_vessel$distance <- NA
    data_vessel$distance[2:(length(data_vessel[,1]))] <- distORTODROMICA(data_vessel$lon[1:(length(data_vessel[,1])-1)],data_vessel$lat[1:(length(data_vessel[,1])-1)],data_vessel$lon[2:length(data_vessel[,1])],data_vessel$lat[2:length(data_vessel[,1])])

    data_vessel$velocity_2 <- NA
    data_vessel$velocity_2 <- data_vessel$distance/data_vessel$time

    data_vessel$change_speed_1  <- NA
    data_vessel$change_speed_1[-1]  <- diff(data_vessel$velocity_2)

    data_vessel$change_speed_2  <- NA
    data_vessel$change_speed_2[-length(data_vessel$change_speed_1)]  <- diff(data_vessel$velocity_2)

    data_vessel$course_2   <- NA
    data_vessel$course_2   <- calculate_course(x = data_vessel$lon, y = data_vessel$lat)
    data_vessel$new_course <- apply(matrix(data_vessel$course_2), 1, change_course) # change_course function

    as.data.frame(data_vessel)

  })
  id_vessel <- id_vessel %>% lapply(as.data.frame) %>% bind_rows()
  return(id_vessel)
}



