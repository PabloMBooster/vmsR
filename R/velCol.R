velCol <- function(x){
  xcol = x
  xcol[x >= 8] <- 3
  xcol[x >= 5 & x < 8] <- 5
  xcol[x > 2 & x < 5] <- 7
  xcol[x <= 2] <- 2
  return(xcol)
}
