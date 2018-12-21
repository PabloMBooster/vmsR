dist_ortodromica <- function (x1,y1,x2,y2)
{
  lon.mn1 = -x1 * 60 * cos(-y1 * pi/180)
  lon.mn2 = -x2 * 60 * cos(-y2 * pi/180)
  lat.mn1 = -y1 * 60
  lat.mn2 = -y2 * 60
  out = sqrt((lon.mn1 - lon.mn2)^2 + (lat.mn1 - lat.mn2)^2)
  return(out)
}
