#modificarRumbo
.change_course <- function(x){
  out <- min(x,360-x)
  out[is.infinite(out)] <- NA
  return(out)
}
