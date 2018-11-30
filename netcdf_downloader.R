#building 3-d arrays [variable, model, time] for 10 towns for 1950-2005
for (n in 1:N){
  #assinging lon and lat from csv
  x <- locations$lon[n]
  y <- locations$lat[n]
  #changing into coordinates 
  coord <- c(360+x,y)
  #locating appropriate latitude index
  flat = match(abs(lat - coord[2]) < 1/32, 1)
  latindex = which(flat %in% 1)
  #locating appropriate longitude index
  flon = match(abs(lon - coord[1]) < 1/32, 1)
  lonindex = which(flon %in% 1)
  #creating a blank 3-d array
  db_hist <- array(dim=c(20,20,20454))
  #looping through variables and models
  for(i in 1:6){
    for(j in 1:20){
      nc_1 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_historical[1]))
      var_1 <- as.numeric(var.get.nc(nc_1, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_2 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_historical[2]))
      var_2 <- as.numeric(var.get.nc(nc_2, variable=4,
                                     start = c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_3 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_historical[3]))
      var_3 <- as.numeric(var.get.nc(nc_3, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,5844)))
      #combining results
      var <- c(var_1,var_2,var_3)
      #filling the 3-d array
      db_hist[i,j,] <- var
      #crudely printing the progress
      print(paste(locations$town[n],variable_1[i],model[j],"historic simulation completed"))
    }
  }
  #saving the 3-d array with the appropriate town name
  assign(paste0(locations$town[n],"_hist"),db_hist)
}

#building 3-d arrays [variable, model, time] for 10 towns for RCP 4.5
for (n in 1:N){
  #assinging lon and lat from csv
  x <- locations$lon[n]
  y <- locations$lat[n]
  #changing into coordinates 
  coord <- c(360+x,y)
  #locating appropriate latitude index
  flat = match(abs(lat - coord[2]) < 1/32, 1)
  latindex = which(flat %in% 1)
  #locating appropriate longitude index
  flon = match(abs(lon - coord[1]) < 1/32, 1)
  lonindex = which(flon %in% 1)
  #creating a blank 3-d array
  db_45 <- array(dim=c(20,20,34333))
  #looping through variables and models
  for(i in 1:6){
    for(j in 1:20){
      nc_1 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_45[1]))
      var_1 <- as.numeric(var.get.nc(nc_1, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_2 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_45[2]))
      var_2 <- as.numeric(var.get.nc(nc_2, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_3 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_45[3]))
      var_3 <- as.numeric(var.get.nc(nc_3, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_4 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_45[4]))
      var_4 <- as.numeric(var.get.nc(nc_4, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_5 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_45[5]))
      var_5 <- as.numeric(var.get.nc(nc_5, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,5113)))
      #combining results
      var <- c(var_1,var_2,var_3,var_4,var_5)
      #filling the 3-d array
      db_45[i,j,] <- var
      #crudely printing the progress
      print(paste(locations$town[n],variable_1[i],model[j],"RCP 4.5 completed"))
    }
  }
  #saving the 3-d array with the appropriate town name
  assign(paste0(locations$town[n],"_45"),db_45)
}

#building 3-d arrays [variable, model, time] for 10 towns for RCP 8.5
for (n in 1:N){
  #assinging lon and lat from csv
  x <- locations$lon[n]
  y <- locations$lat[n]
  #changing into coordinates 
  coord <- c(360+x,y)
  #locating appropriate latitude index
  flat = match(abs(lat - coord[2]) < 1/32, 1)
  latindex = which(flat %in% 1)
  #locating appropriate longitude index
  flon = match(abs(lon - coord[1]) < 1/32, 1)
  lonindex = which(flon %in% 1)
  #creating a blank 3-d array
  db_85 <- array(dim=c(20,20,34333))
  #looping through variables and models
  for(i in 1:6){
    for(j in 1:20){
      nc_1 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_85[1]))
      var_1 <- as.numeric(var.get.nc(nc_1, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_2 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_85[2]))
      var_2 <- as.numeric(var.get.nc(nc_2, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_3 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_85[3]))
      var_3 <- as.numeric(var.get.nc(nc_3, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_4 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_85[4]))
      var_4 <- as.numeric(var.get.nc(nc_4, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_5 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_85[5]))
      var_5 <- as.numeric(var.get.nc(nc_5, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,5113)))
      #combining results
      var <- c(var_1,var_2,var_3,var_4,var_5)
      #filling the 3-d array
      db_85[i,j,] <- var
      #crudely printing the progress
      print(paste(locations$town[n],variable_1[i],model[j],"RCP 8.5 completed"))
    }
  }
  #saving the 3-d array with the appropriate town name
  assign(paste0(locations$town[n],"_85"),db_85)
}
