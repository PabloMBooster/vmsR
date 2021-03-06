intervms <- function(data, id_trip = "Cod_Viaje_VMS", scale.temporal = 60, Date = "Date"){
  library(lubridate)

  interpL <- lapply(split(data, data[[id_trip]], drop = TRUE),function(x){

    print(x[[id_trip]][1])

    t2 <- seq(from=floor_date(x$Date[1],unit="hour"), ceiling_date(x$Date[length(x$Date)],unit="hour"),by=60*scale.temporal) # that's because I'm doing it by hour
    ini = which.min(t2 - x$Date[1] < -0.000000001) - 1
    fin = which.max(t2 - x$Date[length(x$Date)] > -0.000000001)
    t2  = t2[ini:fin]

    interp <- lapply(t2[2:(length(t2)-1)],function(tempo){

      dif.tiempo <- difftime(x$Date,tempo)
      ind1 <- which(dif.tiempo<0)[sum(dif.tiempo<0)]
      ind3 <- ind1+1
      lon2 <- (x$LONGITUDE_M[ind3] - x$LONGITUDE_M[ind1])*as.numeric(difftime(tempo,x$Date[ind1],units = "hour"))/as.numeric(difftime(x$Date[ind3],x$Date[ind1],units="hours")) + x$LONGITUDE_M[ind1]
      lat2 <- (x$LATITUDE_M[ind3] - x$LATITUDE_M[ind1])*as.numeric(difftime(tempo,x$Date[ind1],units = "hour"))/as.numeric(difftime(x$Date[ind3],x$Date[ind1],units="hours")) + x$LATITUDE_M[ind1]
      ind.comp <- which.min(abs(difftime(x$Date,tempo,units = "mins")))

      cbind.data.frame(lon2,lat2,Cod_Barco=x$Cod_Barco[ind.comp], Vel_VMS=x$Vel_VMS[ind.comp], Course=x$Course[ind.comp], id=x$id[ind.comp],
                       Harbor=x$Harbor[ind.comp], Dist_Harbor=x$Dist_Harbor[ind.comp],	Time=x$Time[ind.comp], Dist_Emisiones=x$Dist_Emisiones[ind.comp], Vel.Cal=x$Vel.Cal[ind.comp], Course_Calc=x$Course_Calc[ind.comp], Change_Course_Calc=x$Change_Course_Calc[ind.comp],
                       Cod_Viaje_VMS=x$Cod_Viaje_VMS[ind.comp], dist_costa=x$dist_costa[ind.comp], ind.t = as.numeric(abs(as.numeric(difftime(tempo,x$Date[ind.comp],units="hour")))>1),
                       Date.INTERP=tempo)

    })
    matriz.int <- interp %>% lapply(as.data.frame) %>% bind_rows()

    matriz.int.i = x[1,]
    matriz.int.i$lon2 = matriz.int.i$LONGITUDE_M
    matriz.int.i$lat2 = matriz.int.i$LATITUDE_M
    matriz.int.i$Date.INTERP = t2[1]

    matriz.int.i$ind.t = matriz.int$ind.t[1]
    matriz.int.f = x[length(x[,1]),]
    matriz.int.f$lon2 = matriz.int.f$LONGITUDE_M
    matriz.int.f$lat2 = matriz.int.f$LATITUDE_M
    matriz.int.f$Date.INTERP = t2[length(t2)]
    matriz.int.f$ind.t = matriz.int$ind.t[1]
    matriz.int = rbind(matriz.int.i[,names(matriz.int)], matriz.int, matriz.int.f[,names(matriz.int)])
  })
  interpoL <- interpL %>% lapply(as.data.frame) %>% bind_rows()

  vms.interpol <- unprojet(interpoL,proj4 =  proj4, area = area)
  return(vms.interpol)
}
