calculate_course <- function(x,y){
  vectorCourse <- NULL
  vectorCourse [1] <- NA
  for(i in 2:length(x)){
    x2 <-  x[i]
    y2 <-  y[i]
    x1 <-  x[i-1]
    y1 <-  y[i-1]
    y   <- sin((x2-x1)*pi/180) * cos(y2*pi/180)
    x   <- cos(y1*pi/180) * sin(y2*pi/180) - sin(y1*pi/180) * cos(y2*pi/180) * cos((x2-x1)*pi/180)
    course  <- atan2(y,x)*180/pi
    course  <- (course  + 360)%%360
    vectorCourse  <- rbind(vectorCourse , course)
  }

  vectorCourse  <- as.vector(vectorCourse)
  return(vectorCourse)#, cambioRumbo = cambioRumbo))
}
