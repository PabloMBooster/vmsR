training_nnet <- function(data, directory, formula, neurons = 4, loops = 50, thres_min = 0.4, # dossier.0, directorio,
                           thres_max = 0.6, MSE_max = 0.04, prop_train = 0.75, T1 = 180, T2 = 360,
                           linout = FALSE, entropy = FALSE, softmax = TRUE,
                           censored = FALSE, skip = FALSE, rang = 0.7, decay = 0,
                           maxit = 100, Hess = FALSE, trace = FALSE, MaxNWts = 1000,
                           abstol = 1.0e-4, reltol = 1.0e-8){

  subDir <- "Result_nnet"
  dir.create(file.path(directory, subDir), showWarnings = FALSE)
  input_dir <- paste0(directory,"/",subDir,"/")


  # data$date      <- as.POSIXct(strptime(as.character(data$Fecha_Matlab), format = "%Y-%m-%d %H:%M"))
  # #data$date_GMT  <- as.POSIXct(data$date,tz='GMT')
  # data$Cod_Barco <- as.factor(data$Cod_Barco)
  # #data$Clase_Emision  <- as.factor(data$Clase_Emision)
  # #data$Zona           <- as.factor(data$Zona)
  # data$Puerto_0_Mar_1 <- as.factor(data$Puerto_0_Mar_1)
  # data$Cala           <- as.factor(data$Cala)
  # data$Primera_Cala   <- as.factor(data$Primera_Cala)
  # data$Cod_Viaje_VMS  <- as.factor(data$Cod_Viaje_VMS)
  # #data$Cod_Viaje_Cruz <- as.factor(data$Cod_Viaje_Cruz)
  # data$Flota       <- as.factor(data$Flota)
  # data$Pesca_Viaje <- as.factor(data$Pesca_Viaje)
  #
  # ## partimos del supuesto de que todos los viajes son de anchoveta y tienen máximo 2 horas entre emisiones consecutivas
  # fechas <- unclass(as.POSIXlt(data$date))
  # data$hora <- fechas$hour + fechas$min/60 + fechas$sec/3600
  #
  # # Para que la red no tenga problemas al interpretar las variables circulares de hora y cambio de rumbo
  # # (por ejemplo, directamente no es capaz de reconocer que una distancia entre 23:00 h y 02:00 h es m�?nima, lo que igual sucede con los grados sexagesimales),
  # # debe realizárseles una transformación.
  # data$hora_transf <- cos(data$hora*pi/12)
  # # Es la transformación realizada a la variable hora. Se escogió as�? porque
  # # como las calas se realizan entre la mañana y la tarde, en horas más próximas a la medianoche tendrá valores cercanos a 0 y como al mediod�?a, valores próximos a -1.
  # # De ese modo con la variable transformada la red estará en mayores condiciones de discriminar. (lo de cala y garrete)
  #
  # data$cambio_rumbo_transf <- cos(data$Cambio_Rumbo_Calc*pi/180)
  # # es la transformación realizada a la variable cambio de rumbo. Esta vez no
  # # se escogió la transformación por la distribución de frecuencias de la variable, sino por el criterio de que un mayor cambio de rumbo es un indicador de pesca y un menor cambio, de no pesca.
  # # Entonces esta transformación coloca a los cambios mayores valores cercanos a 0 y a los cambios menores, valores cercanos a 1.
  #
  # cod_viajes <- unique(data$Cod_Viaje_VMS)
  # #cod.viajes <- unique(data$Cod.Viaje.VMS)
  #
  # data$Change_Speed_1 <- rep(NA,dim(data)[1])
  # data$Change_Speed_2 <- rep(NA,dim(data)[1])
  # data$Acel_1 <- rep(NA,dim(data)[1])
  # data$Acel_2 <- rep(NA,dim(data)[1])
  # data$Cambio_Rumbo_Tiempo <- rep(NA,dim(data)[1])
  #
  #
  # for (i in seq_along(cod_viajes)){
  #   lignes <- which(data$Cod_Viaje_VMS == cod_viajes[i])
  #
  #   data$Change_Speed_1[lignes] <- c(NA,diff(data$Vel_Cal[lignes]))
  #   data$Change_Speed_2[lignes] <- c(diff(data$Vel_Cal[lignes]),NA)
  #   Dif_T_3 <- c(NA,data$Dif_Tiempo[lignes]) + c(data$Dif_Tiempo[lignes],NA)
  #   data$Acel_1[lignes] <- data$Change_Speed_1[lignes]/Dif_T_3[1:(length(Dif_T_3)-1)]
  #   data$Acel_2[lignes] <-  c(data$Acel_1[lignes[-1]],NA)
  #   data$Cambio_Rumbo_Tiempo[lignes] <- data$cambio_rumbo_transf[lignes]/Dif_T_3[1:(length(Dif_T_3)-1)]
  # }

  #variables <- c("Dist_Emisiones","Vel_Cal","Change_Speed_1","Change_Speed_2","Acel_1","Acel_2","hora_transf",
  #               "cambio_rumbo_transf","Cambio_Rumbo_Tiempo")

  variables <- c("Dist_Emisiones","Vel.Cal","Change_Speed_1","Change_Speed_2","Acel_1","Acel_2","hora_transf",
                 "cambio_rumbo_transf","Cambio_Rumbo_Tiempo")

  ind_change_speed_1 <- which(is.na(data[,variables[3]]))
  ind_change_speed_2 <- which(is.na(data[,variables[4]]))


  data <- data[-c(ind_change_speed_1,ind_change_speed_2),]
  data_scaled <- scale(data[,variables],center=TRUE,scale=TRUE)


  covariables <- c("Name_vessel","Cod_Barco","Lon","Lat","Puerto_0_Mar_1","Dist_Harbor","Time",#"Dif_Tiempo",
                   "Lon_Obs_Ini_Cala","Lat_Obs_Ini_Cala","Cala","Primera_Cala","Dist_Cala_Emis","Cod_Viaje_VMS",
                   "date")
  data_scaled <- cbind(data[,covariables],data_scaled)

  N = log(1-0.95)/log(1-0.1)
  repetitions = round(N)

  name_traj_ID <- "Cod_Viaje_VMS"

  thresholds = seq(from=thres_min,to=thres_max,by=0.01)

  prom_co = 0
  prom_ci = 100
  prom_dif_ratio = 1
  train_perf <- matrix(NA,nrow=11,ncol=loops)
  test_perf <- matrix(NA,nrow=14,ncol=loops)
  comparacion <- matrix(NA,nrow=6,ncol=loops)
  # T1 = 180
  # T2 = 360
  th = 1

  library(nnet)
  cat(paste0("\n","><>><>><>><>><>><>><>><><>><>><>><>"))
  cat(paste0("\n","><>  Artificial Neural Network  <><"))
  cat(paste0("\n","><> ...  Start calibration  ... <><"))
  cat(paste0("\n","><>><>><>><>><>><>><>><>><>><>><>><>","\n","\n"))

  output <- list() # remove
  output$samples <-  list() # remove
  output$ann     <-  list() # remove

  while (prom_co <= prom_ci && prom_dif_ratio > 0.05){
    ptm <- proc.time()[3] # Start the clock!
    ii=1
    vraisloops=0
    thres = thresholds[th]
    while (ii <= loops){
      vraisloops = vraisloops + 1
      sets <- training_validation_sets(data_scaled,name_traj_ID,prop_train)
      training_set <- sets$training_set
      validation_set <- sets$validation_set
      muestras <- list(training=training_set,validation=validation_set)
      namefile = paste0("muestra_loop_",ii)

      save(muestras, file = paste0(input_dir, namefile, ".RData")) # Rdata

      output$samples[[ii]] <- assign(paste0("samples_", ii), muestras) # remove

      nnet_train <- training_set
      nnet_train <- cbind(nnet_train, training_set$Primera_Cala == 0)
      names(nnet_train)[dim(training_set)[2]+1] <- 'Cala_0'
      nnet_train <- cbind(nnet_train, training_set$Primera_Cala == 1)
      names(nnet_train)[dim(training_set)[2]+2] <- 'Cala_1'
      training_set$Cala_nnet <- class.ind(training_set$Primera_Cala)
      training_set$Cala <- as.factor(training_set$Cala)
      nets <- vector("list", repetitions)
      sse <- rep(NA,repetitions)
      #     aic <- rep(NA,repetitions)

      for (rep in 1:repetitions){
        # nets[[rep]] = nnet(Cala_nnet ~ Vel_Cal + Acel_1 + Acel_2 + hora_transf +
        #                      Cambio_Rumbo_Tiempo, data=training_set, size=neurons, softmax=TRUE,
        #                    trace = FALSE)

        nets[[rep]] = nnet(formula = formula, data = training_set,size = neurons,
                           linout = linout, entropy = entropy, softmax=softmax,
                           censored = censored, skip = skip, rang = rang, decay = decay,
                           maxit = maxit, Hess = Hess, trace = trace, MaxNWts = MaxNWts,
                           abstol = abstol, reltol = reltol)

        sse[rep] = sum(nets[[rep]]$residuals^2)
        #       aic[rep] = dim(training.set)[1]*log(sse[rep]/dim(training.set)[1]) + 2*length(nets[[rep]]$wts)
      }
      best_net = nets[[which.min(sse)]]
      namefile = paste0("ann_year_loop_",ii,"_neurons_",neurons)


      save(best_net, file = paste0(input_dir, namefile, ".RData"))

      output$ann[[ii]] <- assign(paste0("ann_loop", ii,"_neurons_",neurons), best_net)

      calas_predichas <- as.numeric(best_net$fitted.values[,2] > thres)
      CM <- table(data.frame(predicho=calas_predichas == 1, real = training_set$Primera_Cala == 1))
      print(CM)
      if(sum(dim(CM)) == 4){
        train_perf[1,ii] <- CM[1,2]+CM[2,2] # calas observadas
        train_perf[2,ii] <- CM[2,1]+CM[2,2] # calas identificadas
        train_perf[3,ii] <- CM[2,2] # calas verdaderas
        train_perf[4,ii] <- CM[2,1] # calas falsas
        train_perf[5,ii] <- sse[which.min(sse)]/(dim(training_set)[1]*2)
        train_perf[6,ii] <- CM[1,2] # falsos negativos
        comparacion[1,ii] <- train_perf[1,ii]/dim(training_set)[1]*100
        comparacion[2,ii] <- train_perf[2,ii]/dim(training_set)[1]*100
        comparacion[3,ii] <- comparacion[2,ii] - comparacion[1,ii]
        train_perf[11,ii] <- (CM[1,1]+CM[2,2])/sum(CM)
      }else{
        train_perf[1,ii] <- CM[1,2]+0 # calas observadas
        train_perf[2,ii] <- 0 # calas identificadas
        train_perf[3,ii] <- 0 # calas verdaderas
        train_perf[4,ii] <- 0 # calas falsas
        train_perf[5,ii] <- sse[which.min(sse)]/(dim(training_set)[1]*2)
        train_perf[6,ii] <- CM[1,2] # falsos negativos
        comparacion[1,ii] <- train_perf[1,ii]/dim(training_set)[1]*100
        comparacion[2,ii] <- train_perf[2,ii]/dim(training_set)[1]*100
        comparacion[3,ii] <- comparacion[2,ii] - comparacion[1,ii]
        train_perf[11,ii] <- (CM[1,1]+0)/sum(CM)

      }
      if (train_perf[5,ii] >= MSE_max){
        if (ii == 1){
          tiempo <- proc.time()[3] - ptm # Stop the clock
          if (tiempo >= T1 || vraisloops == 200){
            MSE_max <- MSE_max + 0.01
            ptm <- proc.time()[3]
            vraisloops <- 0
          }
        }else if(ii == 2){
          tiempo <- proc.time()[3] - ptm
          if (tiempo >= T2 || vraisloops == 400){
            MSE_max <- MSE_max + 0.01
            ptm <- proc.time()[3]
            vraisloops <- 0
            ii <- 1
            print(paste("Rising MSE.max to: ", MSE_max,sep=""))
          }
        }
        writeLines("")
        print(paste("Number of trained networks: ", ii,sep=""))
        writeLines("")
        print(paste("MSE of the trained network: ", round(train_perf[5,ii],3),sep=""))
      }else{

        writeLines("")
        print(paste("Number of trained networks: ", ii, sep=""))
        writeLines("")
        print(paste("MSE of the trained network: ", round(train_perf[5,ii],3),sep=""))

        validation_set$Cala_nnet <- class.ind(validation_set$Primera_Cala)
        predicted <- predict(best_net,validation_set)
        test_predichas <- as.numeric(predicted[,2] > thres)
        CM <- table(data.frame(predicho= test_predichas == 1, real = validation_set$Primera_Cala == 1))
        if(sum(dim(CM)) == 4){
          test_perf[1,ii] <- CM[1,2]+CM[2,2] # calas observadas
          test_perf[2,ii] <- CM[2,1]+CM[2,2] # calas identificadas
          test_perf[3,ii] <- CM[2,2] # calas verdaderas
          test_perf[4,ii] <- CM[2,1] # calas falsas
          test_perf[5,ii] <- sum((predicted[,2] - (as.numeric(validation_set$Primera_Cala)-1))^2)/dim(validation_set)[1]
          comparacion[4,ii] <- test_perf[1,ii]/dim(validation_set)[1]*100
          comparacion[5,ii] <- test_perf[2,ii]/dim(validation_set)[1]*100
          comparacion[6,ii] <- comparacion[2,ii] - comparacion[1,ii]
          test_perf[6,ii] <- CM[1,2] # falsos negativos
          test_perf[14,ii] <- (CM[1,1]+CM[2,2])/sum(CM)
          test_perf[7,ii] <- min(CM[2,1],CM[1,2]) # número de calas mal ubicadas
        }else{
          test_perf[1,ii] <- CM[1,2]+0 # calas observadas
          test_perf[2,ii] <- 0 # calas identificadas
          test_perf[3,ii] <- 0 # calas verdaderas
          test_perf[4,ii] <- 0 # calas falsas
          test_perf[5,ii] <- sum((predicted[,2] - (as.numeric(validation_set$Primera_Cala)-1))^2)/dim(validation_set)[1]
          comparacion[4,ii] <- test_perf[1,ii]/dim(validation_set)[1]*100
          comparacion[5,ii] <- test_perf[2,ii]/dim(validation_set)[1]*100
          comparacion[6,ii] <- comparacion[2,ii] - comparacion[1,ii]
          test_perf[6,ii] <- CM[1,2] # falsos negativos
          test_perf[14,ii] <- (CM[1,1]+0)/sum(CM)
          test_perf[7,ii] <- min(0,CM[1,2]) # número de calas mal ubicadas
        }
        writeLines("")
        print(paste("MSE of the tested partition: ", round(test_perf[5,ii],3),sep=""))
        if (ii == 10){
          dif <- test_perf[5,1:ii] - train_perf[5,1:ii]
          num = sum(dif > 0.01)
          if (num < 8){
            ii <- ii + 1
          }else{
            MSE_max <- MSE_max + 1
            ii <- 1
            ptm <- proc.time()[3]
            vraisloops <- 0
            print(paste("Rising MSE.max to: ", MSE_max,sep=""))
          }
        }else{
          ii <- ii + 1
        }
      }
    }
    train_perf[7,] <- train_perf[3,]/train_perf[1,] # proporcion de calas verdaderas respecto a las observadas
    train_perf[8,] <- train_perf[4,]/train_perf[1,] # proporcion de calas falsas respecto a las observadas
    train_perf[9,] <- train_perf[6,]/train_perf[1,] # proporcion de falsos negativos respecto a las calas observadas
    train_perf[10,] <- (train_perf[2,] - train_perf[1,])/train_perf[1,] # ratio de la diferencia entre número de calas identificadas y observadas, respecto al número de calas observadas
    test_perf[8,] <- test_perf[2,] - test_perf[1,] # diferencia entre número de calas observadas e identificadas
    test_perf[9,] <- test_perf[3,]/test_perf[1,] # proporcion de calas verdaderas respecto a las observadas
    test_perf[10,] <- test_perf[4,]/test_perf[1,] # proporcion de calas falsas respecto a las observadas
    test_perf[11,] <- test_perf[6,]/test_perf[1,] # proporcion de falsos negativos respecto a las calas observadas
    test_perf[12,] <- (test_perf[2,] - test_perf[1,])/test_perf[1,] # ratio de la diferencia entre número de calas identificadas y observadas, respecto al número de calas observadas
    test_perf[13,] <- test_perf[7,]/test_perf[1,] # proporcion de calas mal ubicadas respecto al número de calas observadas
    prom_train <- apply(train_perf,1,mean,na.rm=TRUE)
    prom_test <- apply(test_perf,1,mean,na.rm=TRUE)
    prom_comp <- apply(comparacion,1,mean,na.rm=TRUE)
    prom_co_0 <- prom_co
    prom_ci_0 <- prom_ci
    prom_co <- prom_test[1]
    prom_ci <- prom_test[2]
    prom_dif_ratio <- prom_test[12]
    th <- th + 1
  }
  #######

  if (th > 2 && prom_co - prom_ci > 10 && prom_ci_0 - prom_co_0 <  10){
    thres <- threshold[th-2]
    ii <- 1
    vraisloops <- 1
    ptm <- proc.time()[3] # Start the clock!
    while (ii <= loops){
      vraisloops = vraisloops + 1
      sets <- training_validation_sets(data_scaled,name_traj_ID,prop_train)
      training_set <- sets$training_set
      validation_set <- sets$validation_set
      muestras <- list(training=training_set,validation=validation_set)
      namefile = paste0("muestra_loop_",ii)


      save(muestras, file = paste0(input_dir, namefile, ".RData"))

      output$samples[[ii]] <- assign(paste0("samples_", ii), muestras)

      nnet_train <- training_set
      nnet_train <- cbind(nnet_train, training_set$Primera_Cala == 0)
      names(nnet_train)[dim(training_set)[2]+1] <- 'Cala_0'
      nnet_train <- cbind(nnet_train, training_set$Primera_Cala == 1)
      names(nnet_train)[dim(training_set)[2]+2] <- 'Cala_1'
      training_set$Cala_nnet <- class.ind(training_set$Primera_Cala)
      nets <- vector("list", repetitions)
      sse <- rep(NA,repetitions)
      for (rep in 1:repetitions){
        # nets[[rep]] = nnet(Cala.nnet ~ Vel_Cal + Acel_1 + Acel_2 + hora_transf +
        #                      Cambio_Rumbo_Tiempo,data=training_set,size=neurons,softmax=TRUE,
        #                    trace = FALSE)

        nets[[rep]] = nnet(formula = formula, data = training_set,size = neurons,
                           linout = linout, entropy = entropy, softmax=softmax,
                           censored = censored, skip = skip, rang = rang, decay = decay,
                           maxit = maxit, Hess = Hess, trace = trace, MaxNWts = MaxNWts,
                           abstol = abstol, reltol = reltol)

        sse[rep] = sum(nets[[rep]]$residuals^2)
      }

      best_net = nets[[which.min(sse)]]
      namefile = paste0("ann_loop_",ii,"_neurons_",neurons)

      save(best_net, file = paste0(input_dir, namefile, ".RData"))

      output$ann[[ii]] <- assign(paste0("ann_loop", ii,"_neurons_",neurons), best_net)

      calas_predichas <- as.numeric(best_net$fitted.values[,2] > thres)
      print(calas.predichas)
      CM <- table(data.frame(predicho=calas_predichas == 1, real = training_set$Primera_Cala == 1))

      if(sum(dim(CM)) == 4){
        train_perf[1,ii] <- CM[1,2]+CM[2,2] # calas observadas
        train_perf[2,ii] <- CM[2,1]+CM[2,2] # calas identificadas
        train_perf[3,ii] <- CM[2,2] # calas verdaderas
        train_perf[4,ii] <- CM[2,1] # calas falsas
        train_perf[5,ii] <- sse[which.min(sse)]/(dim(training_set)[1]*2)
        train_perf[6,ii] <- CM[1,2] # falsos negativos
        comparacion[1,ii] <- train_perf[1,ii]/dim(training_set)[1]*100
        comparacion[2,ii] <- train_perf[2,ii]/dim(training_set)[1]*100
        comparacion[3,ii] <- comparacion[2,ii] - comparacion[1,ii]
        train_perf[11,ii] <- (CM[1,1]+CM[2,2])/sum(CM)
      }else{
        train_perf[1,ii] <- CM[1,2]+0 # calas observadas
        train_perf[2,ii] <- 0 # calas identificadas
        train_perf[3,ii] <- 0 # calas verdaderas
        train_perf[4,ii] <- 0 # calas falsas
        train_perf[5,ii] <- sse[which.min(sse)]/(dim(training_set)[1]*2)
        train_perf[6,ii] <- CM[1,2] # falsos negativos
        comparacion[1,ii] <- train_perf[1,ii]/dim(training_set)[1]*100
        comparacion[2,ii] <- train_perf[2,ii]/dim(training_set)[1]*100
        comparacion[3,ii] <- comparacion[2,ii] - comparacion[1,ii]
        train_perf[11,ii] <- (CM[1,1]+0)/sum(CM)
      }
      if (train_perf[5,ii] >= MSE_max){
        if (ii == 1){
          tiempo <- proc.time()[3] - ptm # Stop the clock
          if (tiempo >= T1 || vraisloops == 200){
            MSE_max <- MSE_max + 0.01
            ptm <- proc.time()[3]
            vraisloops <- 0
          }
        }else if(ii == 2){
          tiempo <- proc.time()[3] - ptm
          if (tiempo >= T2 || vraisloops == 400){
            MSE_max <- MSE_max + 0.01
            ptm <- proc.time()[3]
            vraisloops <- 0
            ii <- 1
            print(paste("Rising MSE.max to: ", MSE_max,sep=""))
          }
        }
        writeLines("")
        print(paste("Number of trained networks: ", ii,sep=""))
        writeLines("")
        print(paste("MSE of the trained network: ", round(train_perf[5,ii],3),sep=""))
      }else{

        writeLines("")
        print(paste("Number of trained networks: ", ii,sep=""))
        writeLines("")
        print(paste("MSE of the trained network: ", round(train_perf[5,ii],3),sep=""))

        validation_set$Cala_nnet <- class.ind(validation_set$Primera_Cala)
        predicted <- predict(best_net,validation_set)
        test_predichas <- as.numeric(predicted[,2] > thres)
        CM <- table(data.frame(predicho = test_predichas == 1, real = validation_set$Primera_Cala == 1))
        if(sum(dim(CM)) == 4){
          test_perf[1,ii] <- CM[1,2]+CM[2,2] # calas observadas
          test_perf[2,ii] <- CM[2,1]+CM[2,2] # calas identificadas
          test_perf[3,ii] <- CM[2,2] # calas verdaderas
          test_perf[4,ii] <- CM[2,1] # calas falsas
          test_perf[5,ii] <- sum((predicted[,2] - (as.numeric(validation_set$Primera_Cala)-1))^2)/dim(validation_set)[1]
          comparacion[4,ii] <- test_perf[1,ii]/dim(validation_set)[1]*100
          comparacion[5,ii] <- test_perf[2,ii]/dim(validation_set)[1]*100
          comparacion[6,ii] <- comparacion[2,ii] - comparacion[1,ii]
          test_perf[6,ii] <- CM[1,2] # falsos negativos
          test_perf[14,ii] <- (CM[1,1]+CM[2,2])/sum(CM)
          test_perf[7,ii] <- min(CM[2,1],CM[1,2]) # número de calas mal ubicadas
        }else{
          test_perf[1,ii] <- CM[1,2]+0 # calas observadas
          test_perf[2,ii] <- 0 # calas identificadas
          test_perf[3,ii] <- 0 # calas verdaderas
          test_perf[4,ii] <- 0 # calas falsas
          test_perf[5,ii] <- sum((predicted[,2] - (as.numeric(validation_set$Primera_Cala)-1))^2)/dim(validation_set)[1]
          comparacion[4,ii] <- test_perf[1,ii]/dim(validation_set)[1]*100
          comparacion[5,ii] <- test_perf[2,ii]/dim(validation_set)[1]*100
          comparacion[6,ii] <- comparacion[2,ii] - comparacion[1,ii]
          test_perf[6,ii] <- CM[1,2] # falsos negativos
          test_perf[14,ii] <- (CM[1,1]+0)/sum(CM)
          test_perf[7,ii] <- min(0,CM[1,2]) # número de calas mal ubicadas
        }
        writeLines("")
        print(paste("MSE of the tested partition: ", round(test_perf[5,ii],3),sep=""))
        if (ii == 10){
          dif <- test_perf[5,1:ii] - train_perf[5,1:ii]
          num = sum(dif > 0.01)
          if (num < 8){
            ii <- ii + 1
          }else{
            MSE_max <- MSE_max + 1
            ii <- 1
            ptm <- proc.time()[3]
            vraisloops <- 0
            print(paste("Rising MSE.max to: ", MSE_max,sep=""))
          }
        }else{
          ii <- ii + 1
        }
      }
    }
    train_perf[7,] <- train_perf[3,]/train_perf[1,] # proporcion de calas verdaderas respecto a las observadas
    train_perf[8,] <- train_perf[4,]/train_perf[1,] # proporcion de calas falsas respecto a las observadas
    train_perf[9,] <- train_perf[6,]/train_perf[1,] # proporcion de falsos negativos respecto a las calas observadas
    train_perf[10,] <- (train_perf[2,] - train_perf[1,])/train_perf[1,] # ratio de la diferencia entre número de calas identificadas y observadas, respecto al número de calas observadas
    test_perf[8,] <- test_perf[2,] - test_perf[1,] # diferencia entre número de calas identificadas y observadas
    test_perf[9,] <- test_perf[3,]/test_perf[1,] # proporcion de calas verdaderas respecto a las observadas
    test_perf[10,] <- test_perf[4,]/test_perf[1,] # proporcion de calas falsas respecto a las observadas
    test_perf[11,] <- test_perf[6,]/test_perf[1,] # proporcion de falsos negativos respecto a las calas observadas
    test_perf[12,] <- (test_perf[2,] - test_perf[1,])/test_perf[1,] # ratio de la diferencia entre número de calas identificadas y observadas, respecto al número de calas observadas
    test_perf[13,] <- test_perf[7,]/test_perf[1,] # proporcion de calas mal ubicadas respecto al número de calas observadas

    prom_train <- apply(train_perf,1,mean,na.rm=TRUE)
    prom_test <- apply(test_perf,1,mean,na.rm=TRUE)
    prom_comp <- apply(comparacion,1,mean,na.rm=TRUE)
    prom_co.0 <- prom_co
    prom_ci.0 <- prom_ci
    prom_co <- prom_test[1]
    prom_ci <- prom_test[2]
    prom_dif_ratio <- prom_test[12]

  }

  # train:
  # 1: co; 2: ci; 3: cv; 4: cf; 5: mse; 6: fn; 7: %cv; 8: %cf; 9: %fn; 10: %si; 11: %acc
  # test:
  # 1: co; 2: ci; 3: cv; 4: cf; 5: mse; 6: fn; 7: mu; 8: dif; 9: %cv;10: %cf; 11: %fn; 12: %si;
  # 13: %mu; 14: %acc


  desv_train <- apply(train_perf,1,sd,na.rm=TRUE)
  desv_test <- apply(test_perf,1,sd,na.rm=TRUE)
  desv_comp <- apply(comparacion,1,sd,na.rm=TRUE)
  # como todas son medias, (incluyendo la del promedio, sacamos intervalo de confianza de media)
  error_test <- qt(0.975,df=loops-1)*desv_test/sqrt(loops)
  left_test  <- prom_test - error_test
  right_test <- prom_test + error_test
  error_train<- qt(0.975,df=loops-1)*desv_train/sqrt(loops)
  left_train  <- prom_train - error_train
  right_train <- prom_train + error_train
  error_comp <- qt(0.975,df=loops-1)*desv_comp/sqrt(loops)
  left_comp  <- prom_comp - error_comp
  right_comp <- prom_comp + error_comp

  results_train <- matrix(c(left_test[14],prom_test[14],right_test[14],desv_test[14],
                            left_test[9],prom_test[9],right_test[9],desv_test[9],
                            left_test[3],prom_test[3],right_test[3],desv_test[3],
                            left_test[13],prom_test[13],right_test[13],desv_test[13],
                            left_test[7],prom_test[7],right_test[7],desv_test[7],
                            left_test[12],prom_test[12],right_test[12],desv_test[12],
                            left_test[8],prom_test[8],right_test[8],desv_test[8],
                            left_comp[4],prom_comp[4],right_comp[4],desv_comp[4],
                            left_test[1],prom_test[1],right_test[1],desv_test[1],
                            left_comp[5],prom_comp[5],right_comp[5],desv_comp[5],
                            left_test[2],prom_test[2],right_test[2],desv_test[2],
                            left_test[10],prom_test[10],right_test[10],desv_test[10],
                            left_test[4],prom_test[4],right_test[4],desv_test[4],
                            left_test[11],prom_test[11],right_test[11],desv_test[11],
                            left_test[6],prom_test[6],right_test[6],desv_test[6],
                            left_test[5],prom_test[5],right_test[5],desv_test[5],
                            left_train[11],prom_train[11],right_train[11],desv_train[11],
                            left_train[7],prom_train[7],right_train[7],desv_train[7],
                            left_train[3],prom_train[3],right_train[3],desv_train[3],
                            left_train[10],prom_train[10],right_train[10],desv_train[10],
                            left_comp[1],prom_comp[1],right_comp[1],desv_comp[1],
                            left_train[1],prom_train[1],right_train[1],desv_train[1],
                            left_comp[2],prom_comp[2],right_comp[2],desv_comp[2],
                            left_train[2],prom_train[2],right_train[2],desv_train[2],
                            left_train[8],prom_train[8],right_train[8],desv_train[8],
                            left_train[4],prom_train[4],right_train[4],desv_train[4],
                            left_train[9],prom_train[9],right_train[9],desv_train[9],
                            left_train[6],prom_train[6],right_train[6],desv_train[6],
                            left_train[5],prom_train[5],right_train[5],desv_train[5]
  ),nrow=29,ncol=4,byrow=TRUE)

  colnames(results_train) <- c("lim_inf","mean","lim_sup","sd")
  rownames(results_train) <- c("test_acc_prop","test_cv_prop","test_cv_num","test_mu_prop",
                               "test_mu_num","test_si_prop","test_si_num","test_co_perc",
                               "test_co_num","test_ci_perc","test_ci_num","test_cf_prop",
                               "test_cf_num","test_fn_prop","test_fn_num","test_mse",
                               "train_acc_prop","train_cv_prop","train_cv_num","train_si_prop",
                               "train_co_perc","train_co_num","train_ci_perc","train_ci_num",
                               "train_cf_prop","train_cf_num","train_fn_prop","train_fn_num",
                               "train_mse")

  write.csv(results_train,file=paste0(input_dir,"TrainInd_neurons_",neurons,".csv",sep=""))

  output$results_train <- results_train
  parametros <- matrix(c(loops,neurons,thres),nrow=3,ncol=1)

  write.table(parametros,file=paste0(input_dir,"TrainPar_loops_neurons_thres.txt"),
              row.names=FALSE,col.names=FALSE)


  output$parameters <- parametros

  nombre <- paste0(input_dir, "TrainInd_Texto_neurons_",neurons,".txt")
  sink(nombre)
  cat("Result mean for partitions of test:")
  cat("\n")
  cat("percentage of successes: ", round(prom_test[14]*100,2), "% (", round(left_test[14]*100,2), "%," , round(right_test[14]*100,2), "%)")
  cat("\n")
  cat("percentage of true sets: ", round(prom_test[9]*100,2), "% (", round(left_test[9]*100,2), "%," , round(right_test[9]*100,2), "%)")
  cat("\n")
  cat("number of true sets:  ", prom_test[3], " (", round(left_test[3],2), "," , round(right_test[3],2), ")")
  cat("\n")
  cat("percentage of set bad located: ", round(prom_test[13]*100,2), "% (", round(left_test[13]*100,2), "%," , round(right_test[13]*100,2), "%)")
  cat("\n")
  cat("number of set bad located:  ", prom_test[7], " (", round(left_test[7],2), "," , round(right_test[7],2), ")")
  cat("\n")
  cat("percentage of set overidentified: ", round(prom_test[12]*100,2), "% (", round(left_test[12]*100,2), "%," , round(right_test[12]*100,2), "%)")
  cat("\n")
  cat("number of overidentified sets: ", prom_test[8], " (", round(left_test[8],2), "," , round(right_test[8],2), ")")
  cat("\n")
  cat("percentage of observed sets: ", round(prom_comp[4],2), "% (", round(left_comp[4],2), "%," , round(right_comp[4],2), "%)")
  cat("\n")
  cat("number of observed sets: ", prom_test[1], " (", round(left_test[1],2), "," , round(right_test[1],2), ")")
  cat("\n")
  cat("percentage of identified sets: ", round(prom_comp[5],2), "% (", round(left_comp[5],2), "%," , round(right_comp[5],2), "%)")
  cat("\n")
  cat("number of identified sets: ", prom_test[2], " (", round(left_test[2],2), "," , round(right_test[2],2), ")")
  cat("\n")
  cat("percentage of false sets:", round(prom_test[10]*100,2), "% (", round(left_test[10]*100,2), "%," , round(right_test[10]*100,2), "%)")
  cat("\n")
  cat("number of false sets", prom_test[4], " (", round(left_test[4],2), "," , round(right_test[4],2), ")")
  cat("\n")
  cat("percentage of false negatives: ", round(prom_test[11]*100,2), "% (", round(left_test[11]*100,2), "%," , round(right_test[11]*100,2), "%)")
  cat("\n")
  cat("number of false negatives", prom_test[6], " (", round(left_test[6],2), "," , round(right_test[6],2), ")")
  cat("\n")
  cat("MSE:", round(prom_test[5],3), " (", round(left_test[5],3), "," , round(right_test[5],3), ")")
  cat("\n")
  cat("\n")
  cat("average results for training partitions:")
  cat("\n")
  cat("percentage of successes: ", round(prom_train[11]*100,2), "% (", round(left_train[11]*100,2), "%," , round(right_train[11]*100,2), "%)")
  cat("\n")
  cat("percentage of true sets: ", round(prom_train[7]*100,2), "% (", round(left_train[7]*100,2), "%," , round(right_train[7]*100,2), "%)")
  cat("\n")
  cat("number of overidentified sets:  ", prom_train[3], " (", round(left_train[3],2), "," , round(right_train[3],2), ")")
  cat("\n")
  cat("percentage of overidentified sets: ", round(prom_train[10]*100,2), "% (", round(left_train[10]*100,2), "%," , round(right_train[10]*100,2), "%)")
  cat("\n")
  # cat("Número de calas sobreidentificadas: ", prom.test[8])
  cat("percentage of observed sets: ", round(prom_comp[1],2), "% (", round(left_comp[1],2), "%," , round(right_comp[1],2), "%)")
  cat("\n")
  cat("number of observed sets: ", prom_train[1], " (", round(left_train[1],2), "," , round(right_train[1],2), ")")
  cat("\n")
  cat("percentage of identified sets: ", round(prom_comp[2],2), "% (", round(left_comp[2],2), "%," , round(right_comp[2],2), "%)")
  cat("\n")
  cat("number of identified sets: ", prom_train[2], " (", round(left_train[2],2), "," , round(right_train[2],2), ")")
  cat("\n")
  cat("percentage of false sets:", round(prom_train[8]*100,2), "% (", round(left_train[8]*100,2), "%," , round(right_train[8]*100,2), "%)")
  cat("\n")
  cat("number of false sets", prom_train[4], " (", round(left_train[4],2), "," , round(right_train[4],2), ")")
  cat("\n")
  cat("percentage of false negatives: ", round(prom_train[9]*100,2), "% (", round(left_train[9]*100,2), "%," , round(right_train[9]*100,2), "%)")
  cat("\n")
  cat("number of false negatives", prom_train[6], " (", round(left_train[6],2), "," , round(right_train[6],2), ")")
  cat("\n")
  cat("MSE:", round(prom_train[5],3), " (", round(left_train[5],3), "," , round(right_train[5],3), ")")
  cat("\n")
  cat("\n")
  cat("Umbral:", thres)
  sink()

  cat(paste0("\n","><>><>><>><>><>><>><>><><>><>"))
  cat(paste0("\n","><>  The process is over  <><"))
  cat(paste0("\n","><>><>><>><>><>><>><>><><>><>","\n","\n"))

  return(output)
}
