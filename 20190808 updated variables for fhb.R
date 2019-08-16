#variables added to array----

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
  0.894^(1-rh)
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

#relative ascospore discharge by relative humidity (trail et al)
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

#functions using model averages----

#perithecia production by temperature and duration
PERITHECIA_P_TEMP <- function(temp, time){
  if(temp>35){
    0
  }else{
    (((5.937*(temp/35)*(1-(temp/35)))^9.334)/
       (1+exp(2.807-(1.106*time))))/40
  }
}


PERITHECIA_P_TEMP(37,14)

#maximum is 39.5855221208437
#breaks down above 35 C
for (i in 1:35){
  for (j in 20:60){
  print(paste(PERITHECIA_P_TEMP(i,j),i,j))
  }
}

#perithecia maturation by temperature and duration
PERITHECIA_M_TEMP <- function(temp, time){
  if(temp<15){
    0
  }else if(temp>30){
    0
  }else{
    ((0.907*((temp-15)/15)*(1-((temp-15)/15)))^0.2)/
      (1+exp(5.618-(0.195*time)))
  }
}



PERITHECIA_P_RH <- function(rh,time){
  (0.850^(100-rh*100))/(1+exp(2.325-0.128*time))
}

PERITHECIA_M_RH <- function(rh,time){
  (0.801^(100-rh*100))/(1+exp(3.401-0.121*time))
}


PERITHECIA_P_RH <- function(rh,time){
  (0.850^(1-rh))/(1+exp(2.325-0.128*time))
}



