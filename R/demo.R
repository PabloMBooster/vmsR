# cargamos los datos
anho_entrenamiento = 2013; year = 2013

dossier.0  <- paste("D:/disco_rocio/disco_duro2/2016/red_neuronal_input/",year, sep = "")
directorio <- paste("D:/disco_rocio/disco_duro2/2016/red_neuronal_input/",year, sep = "")
setwd(dossier.0)
dossier.ant <-dossier.0
data <- read.csv(paste(dossier.ant,"/",'base_',year,'_final.csv',sep=''))
data <- data[,-which(colnames(data)=="X")]

colnames(data) <- c("Nombre_Barco","Cod_Barco","Fecha_Matlab","Clase_Emision","Lon","Lat","Zona","Vel_VMS","Rumbo_VMS",
                    "Puerto_0_Mar_1","Dist_Puerto","Dif_Tiempo","Dist_Emisiones","Vel_Cal","Cambio_Rumbo_Calc",
                    "Lon_Obs_Ini_Cala","Lat_Obs_Ini_Cala","Cala","Primera_Cala","Dist_Cala_Emis","Cod_Viaje_VMS",
                    "Cod_Viaje_Cruz","Flota","Pesca_Viaje")

#nnet_out <- calibration_nnet(data = data, directory = getwd(), neurons=4, MSE_max=0.04, nb_loop = 10)

length(unique(data$Nombre_Barco))

nombres2 <- c("MARIA","FRANCHESCA","LUZ","FERIHE","CECILIA","ALEXIA","XIOMARA","PAOLA",
  "KRISTEL","GERALDINE","KIARA","KAREN","CRISCELY","ANITA","YAJAIRA","ALMENDRA","DIANA","JENIFER","JESYCA", "YVONNE",
  "MACARENA","ELIZABETH", "AMPARITO","SOFIA","AKEMI", "CINTHIA", "ISABEL", "ERICKA", "VERONICA","ANGELITA","GRECIA",
  "KARLA", "VERALUCIA", "ESTEFANY", "SANDRA", "SAORI", "BULMA", "KATY", "WENDY", "VANIA", "BERENICE", "BEATRIZ",
  "BRENDA","DANIELA", "ADRIANA", "FERNANDA", "AMELIA", "CHARITO", "SIRENA", "CAROL", "PATRICIA", "FIORELLA", "XIMENA")

data$Nombre_Barco <- as.character(data$Nombre_Barco)
for(i in seq_along(unique(data$Nombre_Barco))){
  data[data$Nombre_Barco == unique(data$Nombre_Barco)[i], "Nombre_Barco"] <- nombres2[i]
}

data$Cod_Barco <- (data$Cod_Barco*1000)/2
data_vms <- data

#
save(data_vms, file = "C:/pablo/D/github/vmsR/data/data_vms.RData")


#nnet_out <- calibration_nnet(data = data, directory = getwd(), neurons=4, MSE_max=0.04, nb_loop = 10)
