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
    print(paste(locations$town[n],"historical simulation now has GDD for",model[j]))
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
    print(paste(locations$town[n],"RCP 4.5 now has GDD for",model[j]))
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
    print(paste(locations$town[n],"RCP 8.5 now has GDD for",model[j]))
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
    print(paste(locations$town[n],"historical simulation now has GDD*RDR for",model[j]))
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
    print(paste(locations$town[n],"RCP 4.5 now has GDD*RDR for",model[j]))
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
    print(paste(locations$town[n],"RCP 8.5 now has GDD*RDR for",model[j]))
  }
}

#year sequences to incorporate with planting dates
historical_years <- seq(1950,2004,1)
future_years <- seq(2006,2098,1)

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

#ADAPTATION OF THE CERES-WHEAT GROWTH MODEL

#STAGE 1: germination to emergence

#thermal time for emergence as a function of planting depth (in inches)
#deeper planting depth increases time needed for the cleoptile to expand to the surface
GERM <- function(depth){
  emergence <- 40 + 10.2*(depth*2.54)
  return(emergence)
}

#calculating stage 1 using the specified  planting depth
stage_1 <- GERM(planting_depth)

#STAGE 2: germination to terminal spikelet 
###ONLY STAGE THAT USES GDD*RDR IN ORIGINAL CERES-WHEAT GROWTH MODEL###
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

#function to calculate number of days (span) to complete each stage
SPANNER <- function(start,vector_1,vector_2){
  S_1 <- as.vector(sapply(
    start,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_1)[1]))
  S_2 <- as.vector(sapply(
    start + S_1,
    function(x) which(cumsum(vector_2[x:length(vector_1)]) >= stage_2)[1]))
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
  #missing the addition of "start" present in INDEXER function
  idx_1 <- S_1
  idx_2 <- S_1 + S_2
  idx_3 <- S_1 + S_2 + S_3
  idx_4 <- S_1 + S_2 + S_3 + S_4
  idx_5 <- S_1 + S_2 + S_3 + S_4 + S_5
  idx_6 <- S_1 + S_2 + S_3 + S_4 + S_5 + S_6
  idx_7 <- S_1 + S_2 + S_3 + S_4 + S_5 + S_6 + S_7
  string <- c(idx_1,idx_2,idx_3,idx_4,idx_5,idx_6,idx_7)
  matrix(string,nrow=length(start),ncol=7)
}

#creating duration arrays for each location (historical)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #creating an indexing array [model, year, stage, planting]
  span_array_hist <- array(dim=c(20,55,7,3))
  for(j in 1:20){
    matrix_1 <- SPANNER(pd_hist_e,A[7,j,],A[8,j,])
    matrix_2 <- SPANNER(pd_hist_m,A[7,j,],A[8,j,])
    matrix_3 <- SPANNER(pd_hist_l,A[7,j,],A[8,j,])
    span_array_hist[j,,,1] <- matrix_1
    span_array_hist[j,,,2] <- matrix_2
    span_array_hist[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_span_hist"),span_array_hist)
  #printing progress
  print(paste(locations$town[n],"historical simulation wheat stage duration completed"))
}

#creating duration arrays for each location (RCP 4.5)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #creating an indexing array [model, year, stage, planting]
  span_array_fut <- array(dim=c(20,93,7,3))
  for(j in 1:20){
    matrix_1 <- SPANNER(pd_fut_e,A[7,j,],A[8,j,])
    matrix_2 <- SPANNER(pd_fut_m,A[7,j,],A[8,j,])
    matrix_3 <- SPANNER(pd_fut_l,A[7,j,],A[8,j,])
    span_array_fut[j,,,1] <- matrix_1
    span_array_fut[j,,,2] <- matrix_2
    span_array_fut[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_span_45"),span_array_fut)
  #printing progress
  print(paste(locations$town[n],"RCP 4.5 wheat stage duration completed"))
}

#creating duration arrays for each location (RCP 8.5)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #creating an indexing array [model, year, stage, planting]
  span_array_fut <- array(dim=c(20,93,7,3))
  for(j in 1:20){
    matrix_1 <- SPANNER(pd_fut_e,A[7,j,],A[8,j,])
    matrix_2 <- SPANNER(pd_fut_m,A[7,j,],A[8,j,])
    matrix_3 <- SPANNER(pd_fut_l,A[7,j,],A[8,j,])
    span_array_fut[j,,,1] <- matrix_1
    span_array_fut[j,,,2] <- matrix_2
    span_array_fut[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_span_85"),span_array_fut)
  #printing progress
  print(paste(locations$town[n],"RCP 8.5 wheat stage duration completed"))
}

#function to calculate index locations of wheat phenostages
INDEXER <- function(start,vector_1,vector_2){
  S_1 <- as.vector(sapply(
    start,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_1)[1]))
  S_2 <- as.vector(sapply(
    start + S_1,
    function(x) which(cumsum(vector_2[x:length(vector_1)]) >= stage_2)[1]))
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
  print(paste(locations$town[n],"wheat stage index for historical simulation completed"))
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
  print(paste(locations$town[n],"wheat stage index for RCP 4.5 completed"))
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
  print(paste(locations$town[n],"wheat stage index for RCP 8.5 completed"))
}
