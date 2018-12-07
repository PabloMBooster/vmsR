.velCol <- function(x){
  xcol = x
  xcol[x >= 8] <- 3
  xcol[x >= 5 & x < 8] <- 5
  xcol[x > 2 & x < 5] <- 7
  xcol[x <= 2] <- 2
  return(xcol)
}


.pointZarpe <- function(x,y){
  x2 <- x[1] + 0.01
  y2 <- y[1]
  x2 <- c(x2,x)
  y2 <- c(y2,y)
  return(list(x2 = x2, y2 = y2))
}
