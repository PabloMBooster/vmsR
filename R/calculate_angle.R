equation_angle <- function(x,y){
  dot.prod <- x%*%y
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))*180/pi
  return(theta)
}
#
calculate_angle <- function(x,y){
  vec_angle <- NULL
  for(z in 2:(length(x)-1)){
    A <- matrix(NA,nrow = 2)
    B <- matrix(NA,nrow = 2)
    X1 <- as.matrix(x[c(z-1,z)])
    Y1 <- as.matrix(y[c(z-1,z)])
    X2 <- as.matrix(x[c(z,z+1)])
    Y2 <- as.matrix(y[c(z,z+1)])
    A[1] <- X1[1]-X1[2]
    A[2] <- Y1[1]-Y1[2]
    B[1] <- X2[2]-X2[1]
    B[2] <- Y2[2]-Y2[1]
    angle <- equationAngle(t(A),B)
    vec_angle <- rbind(vec_angle,angle)
  }
  return(vec_angle)
}
