dist_ortodromica0 <- function (x1,y1,x2,y2)
{
  cosD = acos(sin(y1)*sin(y2)+cos(y1)*cos(y2)*cos(abs(x1-x2)))
  d = cosD*60
  return(d)
}


dist_ortodromica <- function (x1,y1,x2,y2)
{
  phi1 = x1*pi/180
  phi2 = x2*pi/180
  lambda1 = y1*pi/180
  lambda2 = y2*pi/180
  diflambda = abs(lambda1-lambda2)

  angle = acos(sin(phi1)*sin(phi2)+cos(phi1)*cos(phi2)*cos(diflambda))
  r = 3440.0696544 # radio de la tierra en millas nauticas
  d = r*angle

  ## https://en.wikipedia.org/wiki/Great-circle_distance
  return(d)
}


dist_ortodromica2 <- function (x1,y1,x2,y2)
{
  lon.mn1 = -x1 * 60 * cos(-y1 * pi/180)
  lon.mn2 = -x2 * 60 * cos(-y2 * pi/180)
  lat.mn1 = -y1 * 60
  lat.mn2 = -y2 * 60
  out = sqrt((lon.mn1 - lon.mn2)^2 + (lat.mn1 - lat.mn2)^2)
  return(out)
}

dist_ortodromica3 <- function (x1,y1,x2,y2)
{
  a = 90-y1
  b = 90-y2
  P = x1 + x2
  cosp = cos(a)*cos(b)+sin(a)*sin(b)*cos(P)
  p    = acos(cosp)
  r = 3440.0696544 # radio de la tierra en millas nauticas
  diametro = 2*pi*r
  dist_grado = diametro/360
  dist = p*dist_grado

  return(dist)
}


get_course <- function(lon, lat){
  vectorCourse <- NULL
  vectorCourse[1] <- NA

  for(i in 2:length(lon)){
    x1 <-  lon[i]
    y1 <-  lat[i]
    x2 <-  lon[i-1]
    y2 <-  lat[i-1]

    diffX  <- abs(x2-x1)
    cotgC  <- ((cos(y1)*tan(y2))-(sin(y1)*cos(diffX)))/sin(diffX)
    tangC  <- 1/cotgC
    course <- 360 - atan(tangC)

    vectorCourse <- rbind(vectorCourse,course)
  }
  vectorCourse <- as.vector(vectorCourse)

  return(vectorCourse)
}
