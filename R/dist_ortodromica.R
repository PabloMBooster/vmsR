dist_ortodromica <- function (x1,y1,x2,y2){

  A1 = x1*pi/180
  A2 = x2*pi/180
  B1 = y1*pi/180
  B2 = y2*pi/180

  grad = acos(sin(B1)*sin(B2)+cos(B1)*cos(B2)*cos(A1-A2))

  gradinrad = 180 * grad/pi
  d = (60 * gradinrad)

  #r = 3440.0696544 # radio terrestre en millas nauticas
  #d = r*grad # ver https://en.wikipedia.org/wiki/Great-circle_distance
  return(d)
}


