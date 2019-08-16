
FHB_DASHBOARD <- function (n,s,p){
  #stage label
  stage_lab <- c("Emergence","Tillering","Booting",
                 "Flowering","Grain filling","Maturity","Harvesting")
  
  #gathering arrays and index records
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  #C <- get(paste0(locations$town[n],"_45"))
  #D <- get(paste0(locations$town[n],"_index_45"))
  E <- get(paste0(locations$town[n],"_85"))
  G <- get(paste0(locations$town[n],"_index_85"))
  H <- get(paste0(locations$town[n],"_span_hist"))
  #I <- get(paste0(locations$town[n],"_span_45"))
  J <- get(paste0(locations$town[n],"_span_85"))
  print(paste("Arrays for",locations$town[n],"collected successfully"))
  
  #calculating average duration
  if(s==1){
    duration_hist <- apply(H[,,s,p],2,QUANT)
    duration_85 <- apply(J[,,s,p],2,QUANT)
  }else{
    duration_hist <- apply(H[,,s,p]-H[,,s-1,p],2,QUANT)
    duration_85 <- apply(J[,,s,p]-J[,,s-1,p],2,QUANT)
  }
  #calculating index offset
  elapsed_hist <- apply(H[,,s,p],2,QUANT)
  elapsed_85 <- apply(J[,,s,p],2,QUANT)
  print("Stage durations calculated")
  
  #calculating average temperature
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[7,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    temp_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[7,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    temp_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[7,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    temp_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[7,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    temp_85 <- apply(rcp85_matrix,2,QUANT)
  }
  print("Finished indexing average temperature")
  
  #calculating average relative humidity
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[9,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    rh_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[9,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    rh_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[9,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    rh_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[9,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    rh_85 <- apply(rcp85_matrix,2,QUANT)
  }
  print("Finished indexing average relative humidity")
  
  #calculating average wind speed
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[5,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    wind_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[5,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    wind_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[5,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    wind_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[5,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    wind_85 <- apply(rcp85_matrix,2,QUANT)
  }
  print("Finished indexing average wind speed")
  
  #calculating total rainfall
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- sum(A[3,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    rain_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- sum(E[3,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    rain_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- sum(A[3,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    rain_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- sum(E[3,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    rain_85 <- apply(rcp85_matrix,2,QUANT)
  }
  print("Finished indexing total rainfall")
  
  #calculating average fhb_risk_1 (relative ascospore discharge by temperature)
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[13,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_1_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[13,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_1_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[13,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_1_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[13,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_1_85 <- apply(rcp85_matrix,2,QUANT)
  }
  print("Finished indexing FHB risk 1 (relative ascospore discharge by temperature)")
  
  #calculating average fhb_risk_2 (relative ascospore discharge by relative humidity)
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[14,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_2_hist <- hist_matrix
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[14,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_2_85 <- rcp85_matrix
  }else{
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[14,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_2_hist <- hist_matrix
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[14,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_2_85 <- rcp85_matrix
  }
  print("Finished indexing FHB risk 2 (relative ascospore discharge by relative humidity)")
  
  #calculating average fhb_risk_3 (relative ascospore discharge by relative humidity)
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[15,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_3_hist <- hist_matrix
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[15,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_3_85 <- rcp85_matrix
  }else{
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[14,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_3_hist <- hist_matrix
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[15,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_3_85 <- rcp85_matrix
  }
  print("Finished indexing FHB risk 3 (relative ascospore discharge by relative humidity)")
  
  #need to average fhb risk 2 and 3 into risk 4
  fhb_risk_4_hist_combo <- rbind(fhb_risk_2_hist,fhb_risk_3_hist)
  fhb_risk_4_85_combo <- rbind(fhb_risk_2_85,fhb_risk_3_85)
  fhb_risk_4_hist <- apply(fhb_risk_4_hist_combo,2,QUANT)
  fhb_risk_4_85 <- apply(fhb_risk_4_85_combo,2,QUANT)
  print("Finished averaging FHB risk 4 (relative ascospore discharge by relative humidity)")
  
  #calculating average fhb_risk_5 (relative spore cloud density)
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[20,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_5_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[20,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_5_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[20,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_5_hist <- apply(hist_matrix,2,QUANT)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[20,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_5_85 <- apply(rcp85_matrix,2,QUANT)
  }
  print("Finished indexing FHB risk 5 (relative spore cloud density)")
  
  #summarizing results for perithecia production and maturation based on temp, rh, and duration
  ppt_hist <- matrix(NA,5,55)
  for (i in 1:5){
    for (j in 1:55){
      ppt_hist[i,j] <- PERITHECIA_P_TEMP(temp_hist[i,j],duration_hist[i,j])
    }
  }
  ppt_85 <- matrix(NA,5,93)
  for (i in 1:5){
    for (j in 1:93){
      ppt_85[i,j] <- PERITHECIA_P_TEMP(temp_85[i,j],duration_85[i,j])
    }
  }
  ppr_hist <- matrix(NA,5,55)
  for (i in 1:5){
    for (j in 1:55){
      ppr_hist[i,j] <- PERITHECIA_P_RH(rh_hist[i,j],duration_hist[i,j])
    }
  }
  ppr_85 <- matrix(NA,5,93)
  for (i in 1:5){
    for (j in 1:93){
      ppr_85[i,j] <- PERITHECIA_P_RH(rh_85[i,j],duration_85[i,j])
    }
  }
  pmt_hist <- matrix(NA,5,55)
  for (i in 1:5){
    for (j in 1:55){
      pmt_hist[i,j] <- PERITHECIA_M_TEMP(temp_hist[i,j],duration_hist[i,j])
    }
  }
  pmt_85 <- matrix(NA,5,93)
  for (i in 1:5){
    for (j in 1:93){
      pmt_85[i,j] <- PERITHECIA_M_TEMP(temp_85[i,j],duration_85[i,j])
    }
  }
  pmr_hist <- matrix(NA,5,55)
  for (i in 1:5){
    for (j in 1:55){
      pmr_hist[i,j] <- PERITHECIA_M_RH(rh_hist[i,j],duration_hist[i,j])
    }
  }
  pmr_85 <- matrix(NA,5,93)
  for (i in 1:5){
    for (j in 1:93){
      pmr_85[i,j] <- PERITHECIA_M_RH(rh_85[i,j],duration_85[i,j])
    }
  }
  print("Finished calculating risk: perithecia production and maturation")
  
  layout(matrix(c(1,2,2,2,
                  3,3,3,4,
                  5,5,6,6,
                  7,8,9,10,
                  11,12,13,10),
                nrow=5,ncol=4,byrow = TRUE))
  par(mar=c(2,2.75,3,1))
  
  
  #plotting duration
  plot(0,type="n",xlim=c(0,150),ylim=c(0,max(c(duration_hist,duration_85))),xlab=" ",ylab=paste(stage_lab[s],"duration (days)"),
       main="Duration (days)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,300,10),las=2)
  shade((duration_hist[c(1,5),]),lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade((duration_hist[c(2,4),]),lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade((duration_85[c(1,5),]),lim=seq(55,147,1),col=col.alpha("hotpink",0.35))
  shade((duration_85[c(2,4),]),lim=seq(55,147,1),col=col.alpha("hotpink",0.35))
  lines(y=(duration_hist[3,]),x=seq(1:55),col="black",lwd=1)
  lines(y=(duration_85[3,]),x=seq(55,147,1),col="black",lwd=1)
  
  #plotting completion date
  start_date <- c(pd_hist_e[1],pd_hist_m[1],pd_hist_l[1])
  month_doy <- c(1,32,60,91,121,152,182,213,244,274,305,335)
  month_lab <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  month_lty <- rep(2,12)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,365),xlab=" ",ylab=paste(stage_lab[s],"completion date"),
       main="Completion date",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=month_doy,labels=month_lab,las=2)
  for (m in 1:12){
    abline(h=month_doy[m],lty=month_lty[m])
  }
  if(s==1){
    shade((start_date[p]+elapsed_hist[c(1,5),]),lim=seq(1,55,1),col=col.alpha("black",0.35))
    shade((start_date[p]+elapsed_hist[c(2,4),]),lim=seq(1,55,1),col=col.alpha("black",0.35))
    shade((start_date[p]+elapsed_85[c(1,5),]),lim=seq(55,147,1),col=col.alpha("hotpink",0.35))
    shade((start_date[p]+elapsed_85[c(2,4),]),lim=seq(55,147,1),col=col.alpha("hotpink",0.35))
    lines(y=(start_date[p]+elapsed_hist[3,]),x=seq(1:55),col="black",lwd=1)
    lines(y=(start_date[p]+elapsed_85[3,]),x=seq(55,147,1),col="black",lwd=1)
  }else{
    shade((start_date[p]+elapsed_hist[c(1,5),])-365,lim=seq(1,55,1),col=col.alpha("black",0.35))
    shade((start_date[p]+elapsed_hist[c(2,4),])-365,lim=seq(1,55,1),col=col.alpha("black",0.35))
    shade((start_date[p]+elapsed_85[c(1,5),])-365,lim=seq(55,147,1),col=col.alpha("hotpink",0.35))
    shade((start_date[p]+elapsed_85[c(2,4),])-365,lim=seq(55,147,1),col=col.alpha("hotpink",0.35))
    lines(y=(start_date[p]+elapsed_hist[3,])-365,x=seq(1:55),col="black",lwd=1)
    lines(y=(start_date[p]+elapsed_85[3,])-365,x=seq(55,147,1),col="black",lwd=1)
  }
  
  #plotting rainfall
  plot(0,type="n",xlim=c(0,150),ylim=c(0,max(c(rain_hist,rain_85))),
       xlab=" ",ylab=paste(stage_lab[s],"total rainfall (mm)"),
       main="Total rainfall (mm)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,400,10),las=2)
  shade(rain_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(rain_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(rain_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("blue",0.35))
  shade(rain_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("blue",0.35))
  lines(y=rain_hist[3,],x=seq(1:55),col="black",lwd=1)
  lines(y=rain_85[3,],x=seq(55,147,1),col="black",lwd=1)
  
  #plotting wind speed
  plot(0,type="n",xlim=c(0,150),ylim=c(0,max(c(wind_hist,wind_85))),
       xlab=" ",
       ylab=paste(stage_lab[s],"average wind speed (m/s)"),
       main="Average wind speed (m/s)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,40,1),las=2)
  shade(wind_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(wind_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(wind_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("cyan",0.35))
  shade(wind_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("cyan",0.35))
  lines(y=wind_hist[3,],x=seq(1:55),col="black",lwd=1)
  lines(y=wind_85[3,],x=seq(55,147,1),col="black",lwd=1)
  
  #plotting average temperature
  plot(0,type="n",xlim=c(0,150),ylim=c(0,30),xlab=" ",
       ylab=" ",
       main="Average temperature (\u00B0C)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,40,5),las=2)
  shade(temp_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(temp_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(temp_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("green4",0.35))
  shade(temp_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("green4",0.35))
  lines(y=temp_hist[3,],x=seq(1:55),col="black",lwd=1)
  lines(y=temp_85[3,],x=seq(55,147,1),col="black",lwd=1)
  
  #plotting average relative humidity
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab=" ",ylab=paste(stage_lab[s],"average relative humidity (%)"),
       main="Average relative humidity (%)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,1,0.1),las=2,labels=seq(0,100,10))
  shade(rh_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(rh_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(rh_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("green4",0.35))
  shade(rh_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("green4",0.35))
  lines(y=rh_hist[3,],x=seq(1:55),col="black",lwd=1)
  lines(y=rh_85[3,],x=seq(55,147,1),col="black",lwd=1)
  
  #plotting perithecia production by temp
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab=" ",ylab="Relative risk (%)",
       main="Perithecia production by T (%)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,1,0.25),las=2,labels=seq(0,100,25))
  shade(ppt_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(ppt_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(ppt_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  shade(ppt_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  lines(y=ppt_hist[3,],x=seq(1:55),col="black",lwd=1)
  lines(y=ppt_85[3,],x=seq(55,147,1),col="black",lwd=1)
  
  #plotting perithecia maturation by temp
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab=" ",ylab="Relative risk (%)",
       main="Perithecia maturation by T (%)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,1,0.25),las=2,labels=seq(0,100,25))
  shade(pmt_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(pmt_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(pmt_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  shade(pmt_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  lines(y=pmt_hist[3,],x=seq(1:55),col="black",lwd=1)
  lines(y=pmt_85[3,],x=seq(55,147,1),col="black",lwd=1)
  
  #plotting ascospore discharge by temp
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab=" ",ylab="Relative risk (%)",
       main="Ascospore discharge by T (%)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,1,0.25),las=2,labels=seq(0,100,25))
  shade(fhb_risk_1_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(fhb_risk_1_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(fhb_risk_1_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  shade(fhb_risk_1_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  lines(y=fhb_risk_1_hist[3,],x=seq(1:55),col="black",lwd=1)
  lines(y=fhb_risk_1_85[3,],x=seq(55,147,1),col="black",lwd=1)
  
  #plotting spore cloud
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab=" ",ylab="Relative risk (%)",
       main="Relative spore cloud (%)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,1,0.25),las=2,labels=seq(0,100,25))
  shade(fhb_risk_5_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(fhb_risk_5_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(fhb_risk_5_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("purple",0.35))
  shade(fhb_risk_5_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("purple",0.35))
  lines(y=fhb_risk_5_hist[3,],x=seq(1:55),col="black",lwd=1)
  lines(y=fhb_risk_5_85[3,],x=seq(55,147,1),col="black",lwd=1)
  
  #plotting perithecia production by relative humidity
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab=" ",ylab="Relative risk (%)",
       main="Perithecia production by RH (%)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,1,0.25),las=2,labels=seq(0,100,25))
  shade(ppr_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(ppr_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(ppr_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  shade(ppr_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  lines(y=ppr_hist[3,],x=seq(1:55),col="black",lwd=1)
  lines(y=ppr_85[3,],x=seq(55,147,1),col="black",lwd=1)
  
  #plotting perithecia maturation by relative humidity
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab=" ",ylab="Relative risk (%)",
       main="Perithecia maturation by RH (%)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,1,0.25),las=2,labels=seq(0,100,25))
  shade(pmr_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(pmr_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(pmr_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  shade(pmr_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  lines(y=pmr_hist[3,],x=seq(1:55),col="black",lwd=1)
  lines(y=pmr_85[3,],x=seq(55,147,1),col="black",lwd=1)
  
  #plotting ascospore discharge by relative humidity
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab=" ",ylab="Relative risk (%)",
       main="Ascospore discharge by RH (%)",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,1,0.25),las=2,labels=seq(0,100,25))
  shade(fhb_risk_4_hist[c(1,5),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(fhb_risk_4_hist[c(2,4),],lim=seq(1,55,1),col=col.alpha("black",0.35))
  shade(fhb_risk_4_85[c(1,5),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  shade(fhb_risk_4_85[c(2,4),],lim=seq(55,147,1),col=col.alpha("red",0.35))
  lines(y=fhb_risk_4_hist[3,],x=seq(1:55),col="black",lwd=1)
  lines(y=fhb_risk_4_85[3,],x=seq(55,147,1),col="black",lwd=1)
}

FHB_DASHBOARD(3,5,1)

