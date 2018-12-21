modTime <- function(x){
  xDate <- strsplit(as.character(x), split = " ")
  fecha <- unlist(lapply(xDate,function(xvect) return(xvect[1])))
  hora <- unlist(lapply(xDate,function(xvect) return(xvect[2])))
  AMPM <- unlist(lapply(xDate,function(xvect) return(xvect[3])))

  hora[is.na(AMPM)] <- "00:00:00"

  H <- substr(hora,1,2)

  H[which(AMPM %in% c("PM","P.M.","p.m.","pm") & H != "12")] <- as.character(as.numeric(H[which(AMPM %in% c("PM","P.M.","p.m.","pm") & H != "12")])+12)
  #H[which(AMPM %in% c("PM","P.M.","p.m.","pm") & H == "12")] <- "00"
  horacorr <- paste(H,substring(hora,3,8),sep = "")

  #horacorr[horacorr > 24] <- paste("00",substring(horacorr[horacorr > 24],3,9),sep = "")

  xDatecorr <- as.POSIXct(strptime(paste(fecha,horacorr,sep=" "), format = "%d/%m/%Y %H:%M:%S"))
  return(xDatecorr)
}
