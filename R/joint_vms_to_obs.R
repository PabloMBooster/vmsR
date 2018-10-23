joint_vms_to_obs <- function(vmsData, obsData){

  vmsData$set = 0
  vmsData$dist_set = NA
  vmsData$only_set = 0

  vessel   = unique(obsData$num_vessel)[unique(obsData$num_vessel) %in% unique(vmsData$num_vessel)]
  obsData  = obsData[obsData$num_vessel %in% vessel,]
  vmsData  = vmsData[vmsData$num_vessel %in% vessel,]

  for(i in vessel){

    vms_vessel = vmsData[vmsData$num_vessel == i,]
    obs_vessel = obsData[obsData$num_vessel == i,]
    for(ii in unique(vms_vessel$trip)){
      vms_vessel_trip = vms_vessel[vms_vessel$trip == ii,]
      for(iii in unique(obs_vessel$trip)){
        obs_vessel_trip = obs_vessel[obs_vessel$trip == iii,]
        if(as.numeric(abs(obs_vessel_trip$day_setsail[1] - vms_vessel_trip$date[1]), units = "hours") < 6){
          if(as.numeric(abs(obs_vessel_trip$day_arrival[1] - vms_vessel_trip$date[length(vms_vessel_trip$date)]), units = "hours") < 4){
            for(iv in 1:length(vms_vessel_trip[,1])){
              vsm_vessel_trip_records = vms_vessel_trip[iv,]
              for(v in 1:length(obs_vessel_trip[,1])){
                obs_vessel_trip_set = obs_vessel_trip[v,]
                if(obs_vessel_trip_set$start_set <= vsm_vessel_trip_records$date){
                  if(obs_vessel_trip_set$end_set >= vsm_vessel_trip_records$date){
                    vmsData[vmsData$num_vessel == i & vmsData$trip == ii & vmsData$date == vsm_vessel_trip_records$date,"set"] = 1
                    vmsData[vmsData$num_vessel == i & vmsData$trip == ii & vmsData$date == vsm_vessel_trip_records$date,"dist_set"] = distORTODROMICA(vsm_vessel_trip_records$lon, vsm_vessel_trip_records$lat, obs_vessel_trip_set$lon*-1, obs_vessel_trip_set$lat*-1)
                    vmsData[vmsData$num_vessel == i & vmsData$trip == ii,"only_set"] = 1
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  #vmsData <- vmsData[vmsData$only_set == 1,]
  return(vmsData)
}
