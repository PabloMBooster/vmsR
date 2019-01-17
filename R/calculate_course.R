calculate_course <- function(lon,lat){
  vectorRumbo <- NULL
  vectorRumbo[1] <- NA
  for(i in 2:length(lon)){
    x2 <-  lon[i]
    y2 <-  lat[i]
    x1 <-  lon[i-1]
    y1 <-  lat[i-1]
    y   <- sin((x2-x1)*pi/180) * cos(y2*pi/180)
    x   <- cos(y1*pi/180) * sin(y2*pi/180) - sin(y1*pi/180) * cos(y2*pi/180) * cos((x2-x1)*pi/180)
    Rumbo <- atan2(y,x)*180/pi
    Rumbo <- (Rumbo + 360)%%360
    vectorRumbo <- rbind(vectorRumbo,Rumbo)
  }
  vectorRumbo <- as.vector(vectorRumbo)
  return(vectorRumbo)#, cambioRumbo = cambioRumbo))
}
