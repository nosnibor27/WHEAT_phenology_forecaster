#relative himidity function
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
    print(paste(locations$town[n],"historical simulation now has RH for",model[j]))
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
    print(paste(locations$town[n],"RCP 4.5 now has RH for",model[j]))
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
    print(paste(locations$town[n],"RCP 8.5 now has RH for",model[j]))
  }
}

#vapor pressure deficit function
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
    print(paste(locations$town[n],"historical simulation now has VPD for",model[j]))
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
    print(paste(locations$town[n],"RCP 4.5 now has VPD for",model[j]))
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
    print(paste(locations$town[n],"RCP 8.5 now has VPD for",model[j]))
  }
}

#humidex function
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
    print(paste(locations$town[n],"historical simulation now has HDX for",model[j]))
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
    print(paste(locations$town[n],"RCP 4.5 now has HDX for",model[j]))
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
    print(paste(locations$town[n],"RCP 8.5 now has HDX for",model[j]))
  }
}

#Reference grass potential evapotranspiration
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
                    J=historical_doy,lat=(pi/180)*locations$lat[n])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"historical simulation now has ETo for",model[j]))
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
                    J=future_doy,lat=(pi/180)*locations$lat[n])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"RCP 4.5 now has ETo for",model[j]))
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
                    J=future_doy,lat=(pi/180)*locations$lat[n])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"RCP 8.5 now has ETo for",model[j]))
  }
}

#relative ascospore discharge by temperature
FHB_risk_1 <- function(avg_temp){
  ((5.317*(avg_temp/35)^1.501)*(1-(avg_temp/35)))^4.983
}

#adding a FHB_risk_1 dimension [variable = 13] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_1 by model
    A[13,j,] <- FHB_risk_1(A[7,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"historical simulation now has FHB_risk_1 for",model[j]))
  }
}

#adding a FHB_risk_1 dimension [variable = 13] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_1 by model
    A[13,j,] <- FHB_risk_1(A[7,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"RCP 4.5 now has FHB_risk_1 for",model[j]))
  }
}

#adding a FHB_risk_1 dimension [variable = 13] to all the RCP 8.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_1 by model
    A[13,j,] <- FHB_risk_1(A[7,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"RCP 8.5 now has FHB_risk_1 for",model[j]))
  }
}

#relative ascospore discharge by relative humidity
FHB_risk_2 <- function(rh){
  0.894^(100-rh*100)
}

#adding a FHB_risk_2 dimension [variable = 14] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_2 by model
    A[14,j,] <- FHB_risk_2(A[9,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"historical simulation now has FHB_risk_2 for",model[j]))
  }
}

#adding a FHB_risk_2 dimension [variable = 14] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_2 by model
    A[14,j,] <- FHB_risk_2(A[9,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"RCP 4.5 now has FHB_risk_2 for",model[j]))
  }
}

#adding a FHB_risk_2 dimension [variable = 14] to all the RCP 8.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_2 by model
    A[14,j,] <- FHB_risk_2(A[9,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"RCP 8.5 now has FHB_risk_2 for",model[j]))
  }
}

#relative ascospore discharge by relative humidity (trail et al)
FHB_risk_3 <- function(rh){
  0.0136*exp(3.8599*rh)
}

#adding a FHB_risk_3 dimension [variable = 15] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_3 by model
    A[15,j,] <- FHB_risk_3(A[9,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"historical simulation now has FHB_risk_3 for",model[j]))
  }
}

#adding a FHB_risk_3 dimension [variable = 15] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_3 by model
    A[15,j,] <- FHB_risk_3(A[9,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"RCP 4.5 now has FHB_risk_3 for",model[j]))
  }
}

#adding a FHB_risk_3 dimension [variable = 15] to all the RCP 8.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_3 by model
    A[15,j,] <- FHB_risk_3(A[9,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"RCP 8.5 now has FHB_risk_3 for",model[j]))
  }
}

#logical test days above relative humidity threshold
FHB_risk_4 <- function(rh,level=0.7){
  ifelse(rh>level,1,0)
}

#adding a FHB_risk_4 dimension [variable = 16] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_4 by model
    A[16,j,] <- FHB_risk_4(A[9,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"historical simulation now has FHB_risk_4 for",model[j]))
  }
}

#adding a FHB_risk_4 dimension [variable = 16] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_4 by model
    A[16,j,] <- FHB_risk_4(A[9,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"RCP 4.5 now has FHB_risk_4 for",model[j]))
  }
}

#adding a FHB_risk_4 dimension [variable = 16] to all the RCP 8.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #looping through the models
  for(j in 1:20){
    #calculating FHB_risk_4 by model
    A[16,j,] <- FHB_risk_4(A[9,j,])
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"RCP 8.5 now has FHB_risk_4 for",model[j]))
  }
}

