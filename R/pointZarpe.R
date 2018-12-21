pointZarpe <- function(x,y){
  x2 <- x[1] + 0.01
  y2 <- y[1]
  x2 <- c(x2,x)
  y2 <- c(y2,y)
  return(list(x2 = x2, y2 = y2))
}
