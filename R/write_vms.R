write_vms <- function(data, directorio = NULL, file = NULL, ...){

  if(is.null(file)){
    file = paste0("resultados_",substring(Sys.time(),1,10))
  }
  if(!is.null(directorio)){
    dir.create(file.path(directorio, file))
    dir.create(file.path(directorio, file, "trayectorias"))
    dir.create(file.path(directorio, file, "viajes"))
    dir.create(file.path(directorio, file, "mapas"))
  }else{
    dir.create(file.path(getwd(), file))
    dir.create(file.path(getwd(), file, "trayectorias"))
    dir.create(file.path(getwd(), file, "viajes"))
    dir.create(file.path(getwd(), file, "mapas"))
  }
  # dir.create(file.path(directorio, file,"/viajes/"))
  # quebrar los datos y guardar .csv y mapas
  data$Vel_Cal <- data$Vel.Cal
  data$Name_Vessel <- data$Name_vessel
  id_vessel <- lapply(split(data, data$Cod_Barco, drop = TRUE), function(data_vessel){

    write.csv(data_vessel, file = file.path(directorio, file,"trayectorias", paste0(data_vessel$Name_Vessel[1],".csv")))
    data_vessel_viaje = por_viaje(data_vessel)
    write.csv(data_vessel_viaje, file = file.path(directorio, file,"viajes", paste0(data_vessel$Name_Vessel[1],".csv")))

    pdf(file.path(directorio, file, "mapas", paste0(data_vessel$Name_Vessel[1], ".pdf"))) #

    for(i in sort(unique(data_vessel$trip))){
      data_viaje = data_vessel[data_vessel$trip == i,]
      dur <- round(as.numeric(difftime(time1 = data_viaje$Date[length(data_viaje$Date)], time2 = data_viaje$Date[1], units = "hours")),1)
      map_vms(x = data_viaje$Lon, y = data_viaje$Lat, velocity = data_viaje$Vel_Cal)
      legend("toprigh", legend = c(paste0("viaje = ", data_viaje$trip[1]),
                                   paste0("v_max = ", round(max(data_viaje$Vel_Cal,na.rm = T),1), " knots"),
                                   paste0("rec = ", round(sum(data_viaje$Dist_Emisiones, na.rm = T)), " millas"),
                                   paste0("dur = ", dur, " horas"),
                                   paste0("t_max = ", round(max(data_viaje$Time[-1], na.rm = T),1)," horas"),
                                   paste0("dc_max = ", round(max(data_viaje$dist_costa, na.rm = T))," millas")),
             text.col = c(1,ifelse(max(data_viaje$Vel_Cal,na.rm = T) > 15,2,1),
                          1,1,ifelse(max(data_viaje$Time[-1], na.rm = T) > 3,2,1),
                          ifelse(max(data_viaje$dist_costa, na.rm = T) < 10,2,1)), bg = "gray90", inset = .02, text.font = 3, cex = 0.7)

      legend("bottomleft", legend = c("Velocidad \n (knots)", " [>= 8]", " [>= 5 y < 8]", " [> 2 y < 5]", " [<= 2]"),
             lty = 1, lwd = 2, col = c(NA, 3, 5, 7, 2), bty = "n", pt.cex = 2)
      points(x = data_viaje$Lon_calas, y = data_viaje$Lat_calas, pch = 8, cex = 1.2)
      text(x = harbor[c(13,18,23,33,46,55,62,72,85),"lon"],
           y = harbor[c(13,18,23,33,46,55,62,72,85),"lat"],
           labels = as.character(harbor[c(13,18,23,33,46,55,62,72,85),"harbor2"]), col = 4)
      title(main = paste0(data_viaje$Date[1], " - ", data_viaje$Date[length(data_viaje$Date)]))
    }
    dev.off()
  })
}
