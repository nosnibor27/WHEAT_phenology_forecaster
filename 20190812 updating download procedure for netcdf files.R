#base URL
url_1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/"

#model list
model <- c("bcc-csm1-1", "bcc-csm1-1-m","BNU-ESM","CanESM2",
           "CSIRO-Mk3-6-0","GFDL-ESM2G","GFDL-ESM2M","HadGEM2-CC365",
           "HadGEM2-ES365","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR",
           "IPSL-CM5B-LR","MIROC5","MIROC-ESM","MIROC-ESM-CHEM",
           "MRI-CGCM3","NorESM1-M","CNRM-CM5","CCSM4")

#model condition list becuase CCSM4 is different
condition <- c(rep("_r1i1p1_",19),"_r6i1p1_")

#timestep lists
timestep_historical <- c("historical_1950_1969_CONUS_daily.nc",
                         "historical_1970_1989_CONUS_daily.nc",
                         "historical_1990_2005_CONUS_daily.nc")
timestep_45 <- c("rcp45_2006_2025_CONUS_daily.nc",
                 "rcp45_2026_2045_CONUS_daily.nc",
                 "rcp45_2046_2065_CONUS_daily.nc",
                 "rcp45_2066_2085_CONUS_daily.nc",
                 "rcp45_2086_2099_CONUS_daily.nc")
timestep_85 <- c("rcp85_2006_2025_CONUS_daily.nc",
                 "rcp85_2026_2045_CONUS_daily.nc",
                 "rcp85_2046_2065_CONUS_daily.nc",
                 "rcp85_2066_2085_CONUS_daily.nc",
                 "rcp85_2086_2099_CONUS_daily.nc")

#variable list
variable_1 <- c("_tasmax_","_tasmin_","_pr_","_huss_","_was_","_rsds_")

for (a in 1:N){
  #reference longitude and latitude sequence
  lon <- seq(235.40625,292.96875,0.0625)
  lat <- seq(25.15625,52.84375,0.0625)
  #assinging lon and lat from csv
  x <- locations$lon[a]
  y <- locations$lat[a]
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
  for(b in 1:6){
    for(c in 1:20){
      nc_1 <- open.nc(paste0(url_1,model[c],"/macav2livneh",variable_1[b],
                             model[c],condition[c],timestep_historical[1]))
      var_1 <- as.numeric(var.get.nc(nc_1, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_2 <- open.nc(paste0(url_1,model[c],"/macav2livneh",variable_1[b],
                             model[c],condition[c],timestep_historical[2]))
      var_2 <- as.numeric(var.get.nc(nc_2, variable=4,
                                     start = c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_3 <- open.nc(paste0(url_1,model[c],"/macav2livneh",variable_1[b],
                             model[c],condition[c],timestep_historical[3]))
      var_3 <- as.numeric(var.get.nc(nc_3, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,5844)))
      #combining results
      var <- c(var_1,var_2,var_3)
      #filling the 3-d array
      db_hist[b,c,] <- var
      #crudely printing the progress
      print(paste(locations$town[a],variable_1[b],model[c],"historic simulation completed"))
      a_town <- a
      b_var <- b
      c_model <- c
    }
  }
  #saving the 3-d array with the appropriate town name
  assign(paste0(locations$town[a],"_hist"),db_hist)
  saveRDS(get(paste0(locations$town[a],"_hist")),
          paste0(locations$town[a],"_hist.rdata"))
  print(paste(locations$town[a],"saved"))
  #creating a blank 3-d array
  db_85 <- array(dim=c(20,20,34333))
  #looping through variables and models
  for(f in 1:6){
    for(g in 1:20){
      nc_1 <- open.nc(paste0(url_1,model[g],"/macav2livneh",variable_1[f],
                             model[g],condition[g],timestep_85[1]))
      var_1 <- as.numeric(var.get.nc(nc_1, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_2 <- open.nc(paste0(url_1,model[g],"/macav2livneh",variable_1[f],
                             model[g],condition[g],timestep_85[2]))
      var_2 <- as.numeric(var.get.nc(nc_2, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_3 <- open.nc(paste0(url_1,model[g],"/macav2livneh",variable_1[f],
                             model[g],condition[g],timestep_85[3]))
      var_3 <- as.numeric(var.get.nc(nc_3, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_4 <- open.nc(paste0(url_1,model[g],"/macav2livneh",variable_1[f],
                             model[g],condition[g],timestep_85[4]))
      var_4 <- as.numeric(var.get.nc(nc_4, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_5 <- open.nc(paste0(url_1,model[g],"/macav2livneh",variable_1[f],
                             model[g],condition[g],timestep_85[5]))
      var_5 <- as.numeric(var.get.nc(nc_5, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,5113)))
      #combining results
      var <- c(var_1,var_2,var_3,var_4,var_5)
      #filling the 3-d array
      db_85[f,g,] <- var
      #crudely printing the progress
      print(paste(locations$town[a],variable_1[f],model[g],"RCP 8.5 completed"))
      a_town <- a
      f_var <- f
      g_model <- g
    }
  }
  #saving the 3-d array with the appropriate town name
  assign(paste0(locations$town[a],"_85"),db_85)
  saveRDS(get(paste0(locations$town[a],"_85")),
          paste0(locations$town[a],"_85.rdata"))
  print(paste(locations$town[a],"saved"))
}

