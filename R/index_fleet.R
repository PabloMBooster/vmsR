
# INDICADORES DE FLOTA ----------------------------------------------------

index_fleet <- function(data_vms, cut = "Temporada",...){
  require(dplyr)
  require(ineq)
  require(fenix)

  index <- lapply(split(data_vms, data_vms[[cut]], drop = TRUE),function(x){
    temp         <- x$Temporada[1]
    area         <- covered_area2(dc = x$dc,lat = x$Lat, grado = 1/12)

    area_pesca        <- area$area
    dc_mean           <- mean(x$dc, na.rm = T)
    lat_mean          <- mean(x$Lat, na.rm = T)
    median_area       <- median(table(area$name_area))
    mean_area         <- mean(table(area$name_area))
    max_area          <- max(table(area$name_area))
    N_areas           <- length(table(area$name_area))
    areas_con_10      <- sum(order(table(area$name_area), decreasing = T)[1:round(0.1*length(N_areas))])/sum(table(area$name_area))*100

    acum.area = cumsum(sort(as.numeric(table(area$name_area)),decreasing = TRUE))
    acum.rel  = acum.area/max(acum.area)
    n.area    = length(acum.area)
    index75   = as.numeric(quantile(acum.rel, probs = 0.75))
    index50   = as.numeric(quantile(acum.rel, probs = 0.50))
    index25   = as.numeric(quantile(acum.rel, probs = 0.25))
    #vmax      = max(as.numeric(table(area_pesca$name_area)))

    # otros indicadores
    vector     <- sort(as.numeric(table(area$name_area)),decreasing = TRUE)

    Igini      <- Gini(vector, corr = TRUE)
    # Irs        <- RS(vector, na.rm = TRUE)
    # Itkinson   <- Atkinson(vector, parameter = 0.5, na.rm = TRUE)
    # Itheil     <- Theil(vector, parameter = 0, na.rm = TRUE)
    # Ikolm      <- Kolm(vector, parameter = 1, na.rm = TRUE)
    # Ivar.coeff <- var.coeff(vector, square = FALSE, na.rm = TRUE)
    # Ientropy   <- entropy(vector, parameter = 0.5, na.rm = TRUE)
    cbind.data.frame(temp, area_pesca, dc_mean, lat_mean, index25, index50, index75,
                     Igini, median_area, mean_area, max_area, N_areas, areas_con_10)#, Irs, Itkinson, Itheil, Ikolm, Ivar.coeff, Ientropy)
  })
  index <- index %>% lapply(data.frame) %>% bind_rows()

  return(index)
}

# load("C:/pablo/D/2018/segunda_temporada/sisesat/sisesat_temporada_2018_2.RData")
# require(fenix)
# dataSAT$Temporada <- DateTemp(dataSAT$Fecha)
# dataSAT$dc <- estima_dc(dataSAT$X, dataSAT$Y)
# dataSAT$Lat <- dataSAT$Y
#
# index <- index_fleet(data_vms = dataSAT)
