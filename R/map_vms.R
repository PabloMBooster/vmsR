map_vms <- function(x = x, y = y, velocity = velocity, xlab = "LONGITUDE", #text.lab = NULL,
                   ylab = "LATITUDE", text.lab = FALSE, legend_vel = FALSE, file_harbor = NULL){
  require(shape)
  require(geoR)

  dx = abs(max(x) - min(x))
  dy = abs(max(y) - min(y))

  if(dx > dy){
    idx = (dx - dy)/2
    maxY = max(y) + idx
    minY = min(y) - idx
  }else{
    maxY = max(y)
    minY = min(y)
  }

  if(dy > dx){
    idy = (dy - dx)/2
    maxX = max(x) + idy
    minX = min(x) - idy
  }else{
    maxX = max(x)
    minX = min(x)
  }

  xlim = c(minX,maxX)
  ylim = c(minY,maxY)

  newPoint <- pointZarpe(x, y)

  x0 <- newPoint$x2
  y0 <- newPoint$y2
  x1 <- rev(rev(x0)[-1])
  y1 <- rev(rev(y0)[-1])
  x2 <- x0[-1]
  y2 <- y0[-1]

  plot(x0, y0,  type = "l", xlim = xlim, ylim = ylim, ylab = ylab, xlab = xlab)
  map(xlim = xlim, ylim = ylim, fill = TRUE, col = "khaki1", lty = 0, add = T)
  map(xlim = xlim, ylim = ylim, fill = FALSE, interior = TRUE, col = 1, add = TRUE)
  box()

  if(!is.null(file_harbor)){
    text(x = file_harbor$lon,
         y = file_harbor$lat,
         labels = as.character(file_harbor$harbor), col = 4)
  }

  Arrows(x1, y1, x2, y2, arr.type = "curved", code = 2,lty = 1,
         arr.length = 0.2, arr.adj = 1, col = velCol(velocity))
   if(isTRUE(text.lab)){
     text(x, y, text.lab, pos = 4, cex = 0.6)
   }

  if(isTRUE(legend_vel)){

    XB = (minX + maxX)/2
    XdiffAB = minX-maxX
    XA1 = B + XdiffAB/4*3
    XA2 = B + XdiffAB/4*2
    XA3 = B + XdiffAB/4

    Y1 = minY*0.98
    Y2 = minY*0.96
    YB = (minY + maxY)/2
    legend.krige(x.leg = c(XA1, XB), y.leg = c(Y1, Y2), scale.vals = c(NA,NA,NA,NA),values = 0:20,
                 vertical=F, col=c(2,2,7,7,5,5,5,3,3,3,3,3,3,3,3,3))
    text(x = XA2*1.001, y = Y2*0.99, labels = "Vel (knot)", cex.lab = 0.6, font = 1)
    text(x = c(XA1, XA2*1.0067,XA3*1.0138,XB*1.005), y = Y2*1.03,
         labels = c("[0-2]","[2-5]","[5-8]","[8-15]"), cex.lab =  0.6, font = 1)
  }

}
