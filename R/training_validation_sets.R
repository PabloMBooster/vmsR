training_validation_sets <- function(data, name_traj_ID, prop_train){

  training <- sample(levels(data[,name_traj_ID]),round(length(levels(data[,name_traj_ID]))*prop_train),replace=FALSE)
  lines <- NULL
  for (i in 1:length(training)){
    lines <- c(lines,which(data[,name_traj_ID] == training[i]))
  }
  training_set <- data[lines,]
  validation_set <- data[-lines,]
  return(list(training_set = training_set,validation_set = validation_set))

}
