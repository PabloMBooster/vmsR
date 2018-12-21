distance_harbor <- function(data_vessel, harbor){

  dist <- matrix(0,nrow = 1, ncol = nrow(harbor)*2)

  dist[,seq(1,nrow(harbor)*2,by=2)] <- matrix(as.numeric(t(as.matrix(harbor[,"lon"]))),nrow = 1)
  dist[,seq(2,nrow(harbor)*2,by=2)] <- matrix(as.numeric(t(as.matrix(harbor[,"lat"]))),nrow = 1)
  dist       <- as.vector(dist)
  dist0      <- rep(dist,rep(length(data_vessel[,1]),length(dist)))
  dim(dist0) <- c(length(data_vessel[,1]),dim(harbor)[1]*2)

  all_dist <- NULL
  for(lonp in seq(1,nrow(harbor)*2,by=2)){
    idist     <-  dist_ortodromica(data_vessel$Lon, data_vessel$Lat, dist0[,lonp], dist0[,lonp+1])  # Ortodromica ??
    all_dist  <-  cbind(all_dist,idist)
  }

  all_dist      <- data.frame(all_dist)
  id_harbor     <- apply(all_dist, 1, which.min)
  name_harbor   <- idPuerto2(id_harbor)
  dist_harbor   <- apply(all_dist, 1, min) ## distancia al puerto de origen

  return(list(name_harbor = name_harbor, dist_harbor = dist_harbor))
}
