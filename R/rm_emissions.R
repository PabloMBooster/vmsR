#sameRow
.rm_emissions <- function(x){
  resta     <- difftime(x[-1],x[-length(x)])
  idrow <-  which(resta == 0)+1
  return(idrow)
}
