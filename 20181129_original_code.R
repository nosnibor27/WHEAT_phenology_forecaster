#dependent packages
library(RNetCDF)
library(zoo)
library(rethinking)

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

#variable list, only gathering maximum and minimum daily temperature
variable_1 <- c("_tasmax_","_tasmin_","_pr_","_huss_","_was_","_rsds_")

#location file
locations <- read.csv("whitman_latah_test_towns.csv",
                      stringsAsFactors=FALSE)

#number of towns in location file
N <- nrow(locations)

#reference longitude and latitude sequence
lon <- seq(235.40625,292.96875,0.0625)
lat <- seq(25.15625,52.84375,0.0625)

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
      print(paste(locations$town[n],variable_1[i],model[j],"completed"))
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
      print(paste(locations$town[n],variable_1[i],model[j],"completed"))
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
      print(paste(locations$town[n],variable_1[i],model[j],"completed"))
    }
  }
  #saving the 3-d array with the appropriate town name
  assign(paste0(locations$town[n],"_85"),db_85)
}

#saving the 3-d arrays (historical simulation)
for(n in 1:N){
  saveRDS(get(paste0(locations$town[n],"_hist")),
          paste0(locations$town[n],"_hist.rdata"))
  print(paste(locations$town[n],"saved"))
}

#saving the 3-d arrays (RCP 4.5)
for(n in 1:N){
  saveRDS(get(paste0(locations$town[n],"_45")),
          paste0(locations$town[n],"_45.rdata"))
  print(paste(locations$town[n],"saved"))
}

#saving the 3-d arrays (RCP 8.5)
for(n in 1:N){
  saveRDS(get(paste0(locations$town[n],"_85")),
          paste0(locations$town[n],"_85.rdata"))
  print(paste(locations$town[n],"saved"))
}

#loading the 3-d arrays (historical simulation)
for(n in 1:N){
  assign(paste0(locations$town[n],"_hist"),
         readRDS(paste0(locations$town[n],"_hist.rdata")))
  print(paste(locations$town[n],"loaded"))
}

#loading the 3-d arrays (RCP 4.5)
for(n in 1:N){
  assign(paste0(locations$town[n],"_45"),
         readRDS(paste0(locations$town[n],"_45.rdata")))
  print(paste(locations$town[n],"loaded"))
}

#loading the 3-d arrays (RCP 8.5)
for(n in 1:N){
  assign(paste0(locations$town[n],"_85"),
         readRDS(paste0(locations$town[n],"_85.rdata")))
  print(paste(locations$town[n],"loaded"))
}

#function to calculate growing degree-days (GDD) with a base temp of 0 C
GDD <- function(max_temp,min_temp){
  ifelse((((max_temp-273.15)+(min_temp-273.15))/2)<0,0,
         (((max_temp-273.15)+(min_temp-273.15))/2))
}

#adding a GDD dimension [variable = 7] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[7,j,] <- GDD(A[1,j,],A[2,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"now has GDD for",model[j]))
  }
}

#adding a GDD dimension [variable = 7] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[7,j,] <- GDD(A[1,j,],A[2,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"now has GDD for",model[j]))
  }
}

#adding a GDD dimension [variable = 7] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[7,j,] <- GDD(A[1,j,],A[2,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"now has GDD for",model[j]))
  }
}

#adding a GDD dimension [variable = 7] to all the RCP 8.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[7,j,] <- GDD(A[1,j,],A[2,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"now has GDD for",model[j]))
  }
}

#date sequences
historical_dates <- as.Date(seq(18262,38715,1),origin="1900-01-01")
future_dates <- as.Date(seq(38716,73048,1),origin="1900-01-01")
#date sequences as day of the year
historical_doy <- as.integer(format(historical_dates,"%j"))
future_doy <- as.integer(format(future_dates,"%j"))

#function to calculate photoperiod (PP) which includes civil twilight
PP <- function(J,x,y){
  #solar declination
  solar_dec <- 0.409*sin(((2*pi/365)*J)-1.39)
  #converting to radians
  long <- (pi/180)*x
  lat <- (pi/180)*y
  #sunset time
  sunset <- (24/(2*pi))*((-long)-acos(((sin(lat)*sin(solar_dec))-sin(-0.10472))
                                      /(cos(lat)*cos(solar_dec))))
  #sunrise time
  sunrise <- (24/(2*pi))*((-long)+acos(((sin(lat)*sin(solar_dec))-sin(-0.10472))
                                       /(cos(lat)*cos(solar_dec))))
  #need to add 24 to sunset because negative
  #need to convert from UTC
  #quick and dirty timezone calculation from longitude using 360/24
  ((sunset-round(x/15))+24)-(sunrise-round(x/15))
}

#calculating historical and future PP for each location
for (n in 1:N){
  assign(paste0(locations$town[n],"_PP_hist"),
         PP(historical_doy,locations$lon[n],locations$lat[n]))
}
for (n in 1:N){
  assign(paste0(locations$town[n],"_PP_future"),
         PP(future_doy,locations$lon[n],locations$lat[n]))
}

#function for relative development rate (RDR)
#c is the genetic photoperiod coefficient while p is for photoperiod
#c is variety specific and ranges from 0.002 to 0.006 
RDR <- function(c,p){
  1-c*(20-p)^2
}

#assigning genetic photoperiod coefficient using "Nugaines" wheat (G_1)
G_1 <- 0.0044

#calculating historical and future RDR for each location
for (n in 1:N){
  B <-  get(paste0(locations$town[n],"_PP_hist"))
  assign(paste0(locations$town[n],"_RDR_hist"),RDR(G_1,B))
}
for (n in 1:N){
  B <-  get(paste0(locations$town[n],"_PP_future"))
  assign(paste0(locations$town[n],"_RDR_future"),RDR(G_1,B))
}

#adding a GDD*RDR dimension [variable = 8] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #copying the RDR of interest
  C <-  get(paste0(locations$town[n],"_RDR_hist"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD*RDR by model
    A[8,j,] <- A[7,j,]*C
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"now has GDD*RDR for",model[j]))
  }
}

#adding a GDD*RDR dimension [variable = 8] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #copying the RDR of interest
  C <-  get(paste0(locations$town[n],"_RDR_future"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD*RDR by model
    A[8,j,] <- A[7,j,]*C
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"now has GDD*RDR for",model[j]))
  }
}

#adding a GDD*RDR dimension [variable = 8] to all the RCP 8.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #copying the RDR of interest
  C <-  get(paste0(locations$town[n],"_RDR_future"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD*RDR by model
    A[8,j,] <- A[7,j,]*C
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"now has GDD*RDR for",model[j]))
  }
}

#adding planting dates starting with year sequences
historical_years <- seq(1950,2004,1)
future_years <- seq(2006,2098,1)
#vector of early, mid, and late planting dates
planting_dates <- c("-09-01","-10-01","-11-01")
#creating a vector for all planting dates
p_hist_e <- paste0(historical_years,planting_dates[1])
p_hist_m <- paste0(historical_years,planting_dates[2])
p_hist_l <- paste0(historical_years,planting_dates[3])
p_fut_e <- paste0(future_years,planting_dates[1])
p_fut_m <- paste0(future_years,planting_dates[2])
p_fut_l <- paste0(future_years,planting_dates[3])
#creating zoo objects
historical_zoo = read.zoo(as.data.frame(historical_dates),format = "%Y-%m-%d")
future_zoo = read.zoo(as.data.frame(future_dates),format = "%Y-%m-%d")
#setting reference indices
historical_index = index(historical_zoo)
future_index = index(future_zoo)
#starting what will become the planting date vectors
pd_hist_e <- 0
pd_hist_m <- 0
pd_hist_l <- 0
pd_fut_e <- 0
pd_fut_m <- 0
pd_fut_l <- 0
#looking up index values and adding to planting date vector
for (d in p_hist_e){
  pointer = which.min(abs(as.Date(d) - historical_index))
  pd_hist_e <- append(pd_hist_e,pointer,after=length(pd_hist_e))
}
for (d in p_hist_m){
  pointer = which.min(abs(as.Date(d) - historical_index))
  pd_hist_m <- append(pd_hist_m,pointer,after=length(pd_hist_m))
}
for (d in p_hist_l){
  pointer = which.min(abs(as.Date(d) - historical_index))
  pd_hist_l <- append(pd_hist_l,pointer,after=length(pd_hist_l))
}
for (d in p_fut_e){
  pointer = which.min(abs(as.Date(d) - future_index))
  pd_fut_e <- append(pd_fut_e,pointer,after=length(pd_fut_e))
}
for (d in p_fut_m){
  pointer = which.min(abs(as.Date(d) - future_index))
  pd_fut_m <- append(pd_fut_m,pointer,after=length(pd_fut_m))
}
for (d in p_fut_l){
  pointer = which.min(abs(as.Date(d) - future_index))
  pd_fut_l <- append(pd_fut_l,pointer,after=length(pd_fut_l))
}
#removing initial zero from series
pd_hist_e <- pd_hist_e[-1]
pd_hist_m <- pd_hist_m[-1]
pd_hist_l <- pd_hist_l[-1]
pd_fut_e <- pd_fut_e[-1]
pd_fut_m <- pd_fut_m[-1]
pd_fut_l <- pd_fut_l[-1]
#seed germination requires 1 day
pd_hist_e <- pd_hist_e + 1
pd_hist_m <- pd_hist_m + 1
pd_hist_l <- pd_hist_l + 1
pd_fut_e <- pd_fut_e + 1
pd_fut_m <- pd_fut_m + 1
pd_fut_l <- pd_fut_l + 1

#ANDREW'S ADAPTATION OF THE CERES-WHEAT GROWTH MODEL

#STAGE 1: germination to emergence

#thermal time for emergence as a function of planting depth (in inches)
#deeper planting depth increases time needed for the cleoptile to expand to the surface
GERM <- function(planting_depth){
  emergence <- 40 + 10.2*(planting_depth*2.54)
  return(emergence)
}

#calculating stage 1 using a planting depth of 2 inches
stage_1 <- GERM(2)

#STAGE 2: germination to terminal spikelet 
###ONLY STAGE THAT USES GDD*RDR IN ORIGINAL CERES-WHEAT GROWTH MODEL###
#need to set leaf appearance rate based on thermal time per leaf (phyllochron) 
#when not known a good recommended estimate is 95
phyllochron <- 95

#calculating thermal time for stage 2
stage_2 <- 400*(phyllochron/95)

#STAGE 3: terminal spikelet initiation to end of leaf growth
stage_3 <- 3*phyllochron

#STAGE 4: preanthesis ear growth
stage_4 <- 2*phyllochron

#STAGE 5: preanthesis ear growth to beginning of grain filling 
###WHERE FLOWERING OCCURS###
stage_5 <- 200

#STAGE 6: grain filling
stage_6 <- 500

#STAGE 7: physiological maturity to harvest
stage_7 <- 250

#Feng et al reference
#stage_1 = 70
#stage_2 = 400-70

#function to calculate index locations of wheat phenostagers
INDEXER <- function(start,vector_1,vector_2){
  S_1 <- as.vector(sapply(
    start,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_1)[1]))
  S_2 <- as.vector(sapply(
    start + S_1,
    function(x) which(cumsum(vector_2[x:length(vector_2)]) >= stage_2)[1]))
  S_3 <- as.vector(sapply(
    start + S_1 + S_2,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_3)[1]))
  S_4 <- as.vector(sapply(
    start + S_1 + S_2 + S_3,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_4)[1]))
  S_5 <- as.vector(sapply(
    start + S_1 + S_2 + S_3 + S_4,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_5)[1]))
  S_6 <- as.vector(sapply(
    start + S_1 + S_2 + S_3 + S_4 + S_5,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_6)[1]))
  S_7 <- as.vector(sapply(
    start + S_1 + S_2 + S_3 + S_4 + S_5 + S_6,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_7)[1]))
  idx_1 <- start + S_1
  idx_2 <- start + S_1 + S_2
  idx_3 <- start + S_1 + S_2 + S_3
  idx_4 <- start + S_1 + S_2 + S_3 + S_4
  idx_5 <- start + S_1 + S_2 + S_3 + S_4 + S_5
  idx_6 <- start + S_1 + S_2 + S_3 + S_4 + S_5 + S_6
  idx_7 <- start + S_1 + S_2 + S_3 + S_4 + S_5 + S_6 + S_7
  string <- c(idx_1,idx_2,idx_3,idx_4,idx_5,idx_6,idx_7)
  matrix(string,nrow=length(start),ncol=7)
}

#function to collect quantiles
QUANT <- function(x) quantile(x,probs = c(0.1,0.25,0.5,0.75,0.9))

#creating index arrays for each location (historical)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #creating an indexing array [model, year, stage, planting]
  index_array_hist <- array(dim=c(20,55,7,3))
  for(j in 1:20){
    matrix_1 <- INDEXER(pd_hist_e,A[7,j,],A[8,j,])
    matrix_2 <- INDEXER(pd_hist_m,A[7,j,],A[8,j,])
    matrix_3 <- INDEXER(pd_hist_l,A[7,j,],A[8,j,])
    index_array_hist[j,,,1] <- matrix_1
    index_array_hist[j,,,2] <- matrix_2
    index_array_hist[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_index_hist"),index_array_hist)
  #printing progress
  print(paste(locations$town[n],"wheat stage index completed"))
}

#creating index arrays for each location (RCP 4.5)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #creating an indexing array [model, year, stage, planting]
  index_array_fut <- array(dim=c(20,93,7,3))
  for(j in 1:20){
    matrix_1 <- INDEXER(pd_fut_e,A[7,j,],A[8,j,])
    matrix_2 <- INDEXER(pd_fut_m,A[7,j,],A[8,j,])
    matrix_3 <- INDEXER(pd_fut_l,A[7,j,],A[8,j,])
    index_array_fut[j,,,1] <- matrix_1
    index_array_fut[j,,,2] <- matrix_2
    index_array_fut[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_index_45"),index_array_fut)
  #printing progress
  print(paste(locations$town[n],"wheat stage index completed"))
}

#creating index arrays for each location (RCP 8.5)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #creating an indexing array [model, year, stage, planting]
  index_array_fut <- array(dim=c(20,93,7,3))
  for(j in 1:20){
    matrix_1 <- INDEXER(pd_fut_e,A[7,j,],A[8,j,])
    matrix_2 <- INDEXER(pd_fut_m,A[7,j,],A[8,j,])
    matrix_3 <- INDEXER(pd_fut_l,A[7,j,],A[8,j,])
    index_array_fut[j,,,1] <- matrix_1
    index_array_fut[j,,,2] <- matrix_2
    index_array_fut[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_index_85"),index_array_fut)
  #printing progress
  print(paste(locations$town[n],"wheat stage index completed"))
}

PLOT_DOY <- function(n,s){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- median(historical_doy[B[j,k,s,2]:B[j,k,s+1,2]])
      hist_matrix[j,] <- hist_vector
    }
  }
  quantile_hist <- apply(hist_matrix,2,QUANT)
  rcp45_vector <- rep(0,93)
  rcp45_matrix <- matrix(NA,20,93)
  D <- get(paste0(locations$town[n],"_index_45"))
  for(j in 1:20){
    for(k in 1:93){
      rcp45_vector[k] <- median(future_doy[D[j,k,s,2]:D[j,k,s+1,2]])
      rcp45_matrix[j,] <- rcp45_vector
    }
  }
  quantile_45 <- apply(rcp45_matrix,2,QUANT)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  G <- get(paste0(locations$town[n],"_index_85"))
  for(j in 1:20){
    for(k in 1:93){
      rcp85_vector[k] <- median(future_doy[G[j,k,s,2]:G[j,k,s+1,2]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,365),xlab="Year",ylab="J",
       main="Day of year",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=2,labels=seq(1950,2100,25))
  axis(2,at=seq(0,365,30),las=2)
  shade(quantile_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_45[c(1,5),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_45[c(2,4),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  shade(quantile_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  lines(y=quantile_hist[3,],x=seq(1:55),col="black",lwd=2)
  lines(y=quantile_45[3,],x=seq(55,147,1),col="blue",lwd=2)
  lines(y=quantile_85[3,],x=seq(55,147,1),col="red",lwd=2)
}

PLOT_AVG_TEMP <- function(n,s){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- mean(A[7,j,B[j,k,s,2]:B[j,k,s+1,2]])
      hist_matrix[j,] <- hist_vector
    }
  }
  quantile_hist <- apply(hist_matrix,2,QUANT)
  rcp45_vector <- rep(0,93)
  rcp45_matrix <- matrix(NA,20,93)
  C <- get(paste0(locations$town[n],"_45"))
  D <- get(paste0(locations$town[n],"_index_45"))
  for(j in 1:20){
    for(k in 1:93){
      rcp45_vector[k] <- mean(C[7,j,D[j,k,s,2]:D[j,k,s+1,2]])
      rcp45_matrix[j,] <- rcp45_vector
    }
  }
  quantile_45 <- apply(rcp45_matrix,2,QUANT)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  E <- get(paste0(locations$town[n],"_85"))
  G <- get(paste0(locations$town[n],"_index_85"))
  for(j in 1:20){
    for(k in 1:93){
      rcp85_vector[k] <- mean(E[7,j,G[j,k,s,2]:G[j,k,s+1,2]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,40),xlab="Year",ylab="?C",
       main="Mean temperature",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=2,labels=seq(1950,2100,25))
  axis(2,at=seq(0,40,5),las=2)
  shade(quantile_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_45[c(1,5),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_45[c(2,4),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  shade(quantile_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  lines(y=quantile_hist[3,],x=seq(1:55),col="black",lwd=2)
  lines(y=quantile_45[3,],x=seq(55,147,1),col="blue",lwd=2)
  lines(y=quantile_85[3,],x=seq(55,147,1),col="red",lwd=2)
}

#adding elevation
elevation_grid <- open.nc("metdata_elevationdata.nc")
elevation_ref <- var.get.nc(elevation_grid,variable=2)

locations$elevation <- rep(NA,N)

for (n in 1:N){
  x <- locations$lon[n]
  y <- locations$lat[n]
  lat <- var.get.nc(elevation_grid,"lat")
  lon <- var.get.nc(elevation_grid,"lon")
  flat = match(abs(lat - y) < 1/48, 1)
  latindex = which(flat %in% 1)
  flon = match(abs(lon - x) < 1/48, 1)
  lonindex = which(flon %in% 1)
  locations$elevation[n] <- 0.1*elevation_ref[lonindex,latindex]
}

RH <- function(elevation,max_temp,min_temp,specific_humidity){
  #atmospheric pressure based on site elevation (kPa)
  atmo_pressure <- 101.3*((293-0.0065*elevation)/293)^5.26
  #mean air temperature (C)
  avg_temp <- ((max_temp-273.15)+(min_temp-273.15))/2
  #calculating dew temp
  dew_temp <- (((1/273.15)-(1.844*10^-4)*log((specific_humidity*atmo_pressure/0.622)/0.6113))^-1)-273.15
  #saturation vapor pressure
  Es <- ((0.6108*exp((17.27*(max_temp-273.15))/((max_temp-273.15)+237.3)))+(0.6108*exp((17.27*(min_temp-273.15))/((min_temp-273.15)+237.3))))/2
  #actual vapor pressure
  Ea <- (0.6108*exp((17.27*(dew_temp))/((dew_temp)+237.3)))
  #calculating RH
  ifelse((Ea/Es)>1,1,(Ea/Es))
}

#adding a RH dimension [variable = 9] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[9,j,] <- RH(locations$elevation[n], A[1,j,], A[2,j,], A[4,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"now has RH for",model[j]))
  }
}

#adding a RH dimension [variable = 9] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[9,j,] <- RH(locations$elevation[n], A[1,j,], A[2,j,], A[4,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"now has RH for",model[j]))
  }
}

#adding a RH dimension [variable = 9] to all the RCP 8.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[9,j,] <- RH(locations$elevation[n], A[1,j,], A[2,j,], A[4,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"now has RH for",model[j]))
  }
}

PLOT_RH <- function(n,s){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- mean(A[9,j,B[j,k,s,2]:B[j,k,s+1,2]])
      hist_matrix[j,] <- hist_vector
    }
  }
  quantile_hist <- apply(hist_matrix,2,QUANT)
  rcp45_vector <- rep(0,93)
  rcp45_matrix <- matrix(NA,20,93)
  C <- get(paste0(locations$town[n],"_45"))
  D <- get(paste0(locations$town[n],"_index_45"))
  for(j in 1:20){
    for(k in 1:93){
      rcp45_vector[k] <- mean(C[9,j,D[j,k,s,2]:D[j,k,s+1,2]])
      rcp45_matrix[j,] <- rcp45_vector
    }
  }
  quantile_45 <- apply(rcp45_matrix,2,QUANT)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  E <- get(paste0(locations$town[n],"_85"))
  G <- get(paste0(locations$town[n],"_index_85"))
  for(j in 1:20){
    for(k in 1:93){
      rcp85_vector[k] <- mean(E[9,j,G[j,k,s,2]:G[j,k,s+1,2]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab="Year",ylab="%",
       main="Relative humidity",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=2,labels=seq(1950,2100,25))
  axis(2,at=seq(0,1,0.1),las=2,labels=seq(0,100,10))
  shade(quantile_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_45[c(1,5),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_45[c(2,4),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  shade(quantile_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  lines(y=quantile_hist[3,],x=seq(1:55),col="black",lwd=2)
  lines(y=quantile_45[3,],x=seq(55,147,1),col="blue",lwd=2)
  lines(y=quantile_85[3,],x=seq(55,147,1),col="red",lwd=2)
}

VPD <- function(elevation,max_temp,min_temp,specific_humidity){
  #atmospheric pressure based on site elevation (kPa)
  atmo_pressure <- 101.3*((293-0.0065*elevation)/293)^5.26
  #mean air temperature (C)
  avg_temp <- ((max_temp-273.15)+(min_temp-273.15))/2
  #calculating dew temp
  dew_temp <- (((1/273.15)-(1.844*10^-4)*log((specific_humidity*atmo_pressure/0.622)/0.6113))^-1)-273.15
  #saturation vapor pressure
  Es <- ((0.6108*exp((17.27*(max_temp-273.15))/((max_temp-273.15)+237.3)))+(0.6108*exp((17.27*(min_temp-273.15))/((min_temp-273.15)+237.3))))/2
  #actual vapor pressure
  Ea <- (0.6108*exp((17.27*(dew_temp))/((dew_temp)+237.3)))
  #calculating VPD
  Es-Ea
}

#adding a VPD dimension [variable = 10] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[10,j,] <- VPD(locations$elevation[n], A[1,j,], A[2,j,], A[4,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"now has VPD for",model[j]))
  }
}

#adding a VPD dimension [variable = 10] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[10,j,] <- VPD(locations$elevation[n], A[1,j,], A[2,j,], A[4,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"now has VPD for",model[j]))
  }
}

#adding a VPD dimension [variable = 10] to all the RCP 8.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[10,j,] <- VPD(locations$elevation[n], A[1,j,], A[2,j,], A[4,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"now has VPD for",model[j]))
  }
}

LOT_VPD <- function(n,s){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- mean(A[10,j,B[j,k,s,2]:B[j,k,s+1,2]])
      hist_matrix[j,] <- hist_vector
    }
  }
  quantile_hist <- apply(hist_matrix,2,QUANT)
  rcp45_vector <- rep(0,93)
  rcp45_matrix <- matrix(NA,20,93)
  C <- get(paste0(locations$town[n],"_45"))
  D <- get(paste0(locations$town[n],"_index_45"))
  for(j in 1:20){
    for(k in 1:93){
      rcp45_vector[k] <- mean(C[10,j,D[j,k,s,2]:D[j,k,s+1,2]])
      rcp45_matrix[j,] <- rcp45_vector
    }
  }
  quantile_45 <- apply(rcp45_matrix,2,QUANT)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  E <- get(paste0(locations$town[n],"_85"))
  G <- get(paste0(locations$town[n],"_index_85"))
  for(j in 1:20){
    for(k in 1:93){
      rcp85_vector[k] <- mean(E[10,j,G[j,k,s,2]:G[j,k,s+1,2]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,3),xlab="Year",ylab="kPa",
       main="Vapor pressure deficit",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=2,labels=seq(1950,2100,25))
  axis(2,at=seq(0,3,0.5),las=2)
  shade(quantile_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_45[c(1,5),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_45[c(2,4),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  shade(quantile_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  lines(y=quantile_hist[3,],x=seq(1:55),col="black",lwd=2)
  lines(y=quantile_45[3,],x=seq(55,147,1),col="blue",lwd=2)
  lines(y=quantile_85[3,],x=seq(55,147,1),col="red",lwd=2)
}

HDX <- function(elevation,max_temp,min_temp,specific_humidity){
  #atmospheric pressure based on site elevation (kPa)
  atmo_pressure <- 101.3*((293-0.0065*elevation)/293)^5.26
  #mean air temperature (C)
  avg_temp <- ((max_temp-273.15)+(min_temp-273.15))/2
  #calculating dew temp
  dew_temp <- (((1/273.15)-(1.844*10^-4)*log((specific_humidity*atmo_pressure/0.622)/0.6113))^-1)-273.15
  #calculating humidex
  avg_temp + 0.5555*(6.11*exp(5417.7530*((1/273.16)-(1/(273.15+dew_temp))))-10)
}

#adding a HDX dimension [variable = 11] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[11,j,] <- HDX(locations$elevation[n], A[1,j,], A[2,j,], A[4,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"now has HDX for",model[j]))
  }
}

#adding a HDX dimension [variable = 11] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[11,j,] <- HDX(locations$elevation[n], A[1,j,], A[2,j,], A[4,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"now has HDX for",model[j]))
  }
}

#adding a HDX dimension [variable = 11] to all the RCP 8.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[11,j,] <- HDX(locations$elevation[n], A[1,j,], A[2,j,], A[4,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"now has HDX for",model[j]))
  }
}

PLOT_HDX <- function(n,s){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- mean(A[11,j,B[j,k,s,2]:B[j,k,s+1,2]])
      hist_matrix[j,] <- hist_vector
    }
  }
  quantile_hist <- apply(hist_matrix,2,QUANT)
  rcp45_vector <- rep(0,93)
  rcp45_matrix <- matrix(NA,20,93)
  C <- get(paste0(locations$town[n],"_45"))
  D <- get(paste0(locations$town[n],"_index_45"))
  for(j in 1:20){
    for(k in 1:93){
      rcp45_vector[k] <- mean(C[11,j,D[j,k,s,2]:D[j,k,s+1,2]])
      rcp45_matrix[j,] <- rcp45_vector
    }
  }
  quantile_45 <- apply(rcp45_matrix,2,QUANT)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  E <- get(paste0(locations$town[n],"_85"))
  G <- get(paste0(locations$town[n],"_index_85"))
  for(j in 1:20){
    for(k in 1:93){
      rcp85_vector[k] <- mean(E[11,j,G[j,k,s,2]:G[j,k,s+1,2]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,30),xlab="Year",ylab="?C",
       main="Humidex",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=2,labels=seq(1950,2100,25))
  axis(2,at=seq(0,30,5),las=2)
  shade(quantile_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_45[c(1,5),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_45[c(2,4),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  shade(quantile_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  lines(y=quantile_hist[3,],x=seq(1:55),col="black",lwd=2)
  lines(y=quantile_45[3,],x=seq(55,147,1),col="blue",lwd=2)
  lines(y=quantile_85[3,],x=seq(55,147,1),col="red",lwd=2)
}

ETo <- function(elevation,max_temp,min_temp,specific_humidity,wind_speed,rad,J,lat){
  #atmospheric pressure based on site elevation (kPa)
  atmo_pressure <- 101.3*((293-0.0065*elevation)/293)^5.26
  #psychrometric constant
  psy_constant<- 0.000665*atmo_pressure
  #mean air temperature (C)
  avg_temp <- ((max_temp-273.15)+(min_temp-273.15))/2
  #converting surface downwelling shortwave radiation from W/m^2 to MJ/m^2 per day
  solar_rad <- 0.0864*rad
  #slope of the saturation vapor pressure-temperature curve
  delta <- (2503*exp((17.27*avg_temp)/(avg_temp+237.3)))/(avg_temp+237.3)^2
  #calculating dew temp
  dew_temp <- (((1/273.15)-(1.844*10^-4)*log((specific_humidity*atmo_pressure/0.622)/0.6113))^-1)-273.15
  #wind speed at 2 m above ground (measured from 10 m)
  wind <- wind_speed*(4.87/(log(67.8*10-5.42)))
  #inverse relative distance factor
  dist_factor <- 1+0.033*cos((2*pi/365)*J)
  #solar declination
  solar_dec <- 0.409*sin(((2*pi/365)*J)-1.39)
  #sunset hour angle
  sunset_hour <- acos(-tan(lat)*tan(solar_dec))
  #extraterrestrial radiation for 24 hour period
  extra_rad <- (24/pi)*4.92*dist_factor*(sunset_hour*sin(lat)*sin(solar_dec)+cos(lat)*cos(solar_dec)*sin(sunset_hour))
  #clear sky radiation
  clrsky_solar_rad <- (0.75+(2*10^-5)*elevation)*(extra_rad)
  #cloudiness function
  #bound (solar_rad/clrsky_solar_rad) ratio between 0.3 and 1
  cloudy <- 1.35*(ifelse((solar_rad/clrsky_solar_rad)<0.3,0.3,ifelse((solar_rad/clrsky_solar_rad)>1,1,(solar_rad/clrsky_solar_rad))))-0.35
  #saturation vapor pressure
  Es <- ((0.6108*exp((17.27*(max_temp-273.15))/((max_temp-273.15)+237.3)))+(0.6108*exp((17.27*(min_temp-273.15))/((min_temp-273.15)+237.3))))/2
  #actual vapor pressure
  Ea <- (0.6108*exp((17.27*(dew_temp))/((dew_temp)+237.3)))
  #net long wave radiation
  nlw_rad <- (4.901*10^-9)*cloudy*(0.34-0.14*sqrt(Ea))*((max_temp^4+min_temp^4)/2)
  #net shortwave radiation: albedo fixed at 0.23
  nsw_rad <- (1-0.23)*solar_rad
  #net radiation
  net_rad <- nsw_rad-nlw_rad
  #final equation, if negative output is 0
  ifelse((0.408*delta*net_rad + psy_constant*(900/(avg_temp+273))*wind*(Es-Ea))/(delta + psy_constant*(1+0.34*wind))<0,0,
         (0.408*delta*net_rad + psy_constant*(900/(avg_temp+273))*wind*(Es-Ea))/(delta + psy_constant*(1+0.34*wind)))
}

#adding a ETo dimension [variable = 11] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[12,j,] <- ETo(locations$elevation[n],
                    A[1,j,], A[2,j,], A[4,j,], A[5,j,], A[6,j,],
                    J=historical_doy,lat=(pi/180)*locations$lat)
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"now has ETo for",model[j]))
  }
}

for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[12,j,] <- ETo(locations$elevation[n],
                    A[1,j,], A[2,j,], A[4,j,], A[5,j,], A[6,j,],
                    J=future_doy,lat=(pi/180)*locations$lat)
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"now has ETo for",model[j]))
  }
}

for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #looping through the models
  for(j in 1:20){
    #calculating GDD by model
    A[12,j,] <- ETo(locations$elevation[n],
                    A[1,j,], A[2,j,], A[4,j,], A[5,j,], A[6,j,],
                    J=future_doy,lat=(pi/180)*locations$lat)
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"now has ETo for",model[j]))
  }
}

PLOT_ETo <- function(n,s){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- sum(A[12,j,B[j,k,s,2]:B[j,k,s+1,2]])
      hist_matrix[j,] <- hist_vector
    }
  }
  quantile_hist <- apply(hist_matrix,2,QUANT)
  rcp45_vector <- rep(0,93)
  rcp45_matrix <- matrix(NA,20,93)
  C <- get(paste0(locations$town[n],"_45"))
  D <- get(paste0(locations$town[n],"_index_45"))
  for(j in 1:20){
    for(k in 1:93){
      rcp45_vector[k] <- sum(C[12,j,D[j,k,s,2]:D[j,k,s+1,2]])
      rcp45_matrix[j,] <- rcp45_vector
    }
  }
  quantile_45 <- apply(rcp45_matrix,2,QUANT)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  E <- get(paste0(locations$town[n],"_85"))
  G <- get(paste0(locations$town[n],"_index_85"))
  for(j in 1:20){
    for(k in 1:93){
      rcp85_vector[k] <- sum(E[12,j,G[j,k,s,2]:G[j,k,s+1,2]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,100),xlab="Year",ylab="mm",
       main="Potential evapotranspiration",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=2,labels=seq(1950,2100,25))
  axis(2,at=seq(0,100,10),las=2)
  shade(quantile_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_45[c(1,5),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_45[c(2,4),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  shade(quantile_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  lines(y=quantile_hist[3,],x=seq(1:55),col="black",lwd=2)
  lines(y=quantile_45[3,],x=seq(55,147,1),col="blue",lwd=2)
  lines(y=quantile_85[3,],x=seq(55,147,1),col="red",lwd=2)
}

PLOT_RAIN <- function(n,s){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- sum(A[3,j,B[j,k,s,2]:B[j,k,s+1,2]])
      hist_matrix[j,] <- hist_vector
    }
  }
  quantile_hist <- apply(hist_matrix,2,QUANT)
  rcp45_vector <- rep(0,93)
  rcp45_matrix <- matrix(NA,20,93)
  C <- get(paste0(locations$town[n],"_45"))
  D <- get(paste0(locations$town[n],"_index_45"))
  for(j in 1:20){
    for(k in 1:93){
      rcp45_vector[k] <- sum(C[3,j,D[j,k,s,2]:D[j,k,s+1,2]])
      rcp45_matrix[j,] <- rcp45_vector
    }
  }
  quantile_45 <- apply(rcp45_matrix,2,QUANT)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  E <- get(paste0(locations$town[n],"_85"))
  G <- get(paste0(locations$town[n],"_index_85"))
  for(j in 1:20){
    for(k in 1:93){
      rcp85_vector[k] <- sum(E[3,j,G[j,k,s,2]:G[j,k,s+1,2]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,100),xlab="Year",ylab="mm",
       main="Total rainfall",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=2,labels=seq(1950,2100,25))
  axis(2,at=seq(0,100,10),las=2)
  shade(quantile_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade(quantile_45[c(1,5),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_45[c(2,4),],lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade(quantile_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  shade(quantile_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.15))
  lines(y=quantile_hist[3,],x=seq(1:55),col="black",lwd=2)
  lines(y=quantile_45[3,],x=seq(55,147,1),col="blue",lwd=2)
  lines(y=quantile_85[3,],x=seq(55,147,1),col="red",lwd=2)
}