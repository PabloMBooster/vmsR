map_vms <- function(x = x, y = y, velocity = velocity, xlab = "LONGITUDE", #text.lab = NULL,
                   ylab = "LATITUDE", add = FALSE){
  require(shape)

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
  lines(peruXY)
  Arrows(x1, y1, x2, y2, arr.type = "curved", code = 2,lty = 1,
         arr.length = 0.2, arr.adj = 1, col = velCol(velocity))
  #  if(isTRUE(text.lab)){
  #    text(x, y, text.lab, pos = 4, cex = 0.6)
  #  }
}
