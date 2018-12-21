idPuerto2 <- function(x){
  out <- NULL
  harbor$Num <- 1:length(harbor$harbor)
  for(i in 1:length(x)){
    puerto = as.character(harbor[harbor$Num %in%  x[i],"harbor2"])
    out = rbind(out, puerto)
  }
  out <- as.vector(out)
  return(out)
}
