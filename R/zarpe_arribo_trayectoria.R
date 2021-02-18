zarpe_arribo_trayectoria <- function(data, ...){

  zarpe_arribo <- lapply(split(data, data$Cod_Viaje_VMS, drop = TRUE),function(x){

    x$fecha_zarpe             <- x$Date[1]
    x$fecha_arribo            <- x$Date[length(x$Date)]
    x$fecha                   <- as.POSIXct(format(x$fecha_arribo, format = "%Y-%m-%d"))
    as.data.frame(x)
  })
  zarpe_arribo <- zarpe_arribo %>% lapply(data.frame) %>% bind_rows()
  return(zarpe_arribo)
}
