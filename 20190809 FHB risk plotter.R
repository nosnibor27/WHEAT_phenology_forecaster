n <- 1
s <- 4
p <- 2

FHB_RISK_PLOTTER <- function(n,s,p){
  #stage label
  stage_lab <- c("Emergence","Tillering","Booting",
                 "Flowering","Grain filling","Maturity","Harvesting")
  #gathering arrays and index records
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  C <- get(paste0(locations$town[n],"_45"))
  D <- get(paste0(locations$town[n],"_index_45"))
  E <- get(paste0(locations$town[n],"_85"))
  G <- get(paste0(locations$town[n],"_index_85"))
  #calculating average duration
  H <- get(paste0(locations$town[n],"_span_hist"))
  I <- get(paste0(locations$town[n],"_span_45"))
  J <- get(paste0(locations$town[n],"_span_85"))
  if(s==1){
    duration_hist <- apply(H[,,s,p],2,QUANT)
    duration_45 <- apply(I[,,s,p],2,QUANT)
    duration_85 <- apply(J[,,s,p],2,QUANT)
  }else{
    duration_hist <- apply(H[,,s,p]-H[,,s-1,p],2,QUANT)
    duration_45 <- apply(I[,,s,p]-I[,,s-1,p],2,QUANT)
    duration_85 <- apply(J[,,s,p]-J[,,s-1,p],2,QUANT)
  }
  #calculating average temperature
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
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
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[7,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    temp_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
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
    
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[7,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    temp_45 <- apply(rcp45_matrix,2,QUANT)
    
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[7,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    temp_85 <- apply(rcp85_matrix,2,QUANT)
  }
  #calculating average relative humidity
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[9,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    rh_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[9,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    rh_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[9,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    rh_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[9,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    rh_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[9,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    rh_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[9,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    rh_85 <- apply(rcp85_matrix,2,QUANT)
  }
  #summarizing results for perithecia production and maturation based on temp, rh, and time
  #historical simulation
  ppt_hist <- rep(NA,55)
  for (i in 1:55){
    ppt_hist[i] <- PERITHECIA_P_TEMP(temp_hist[3,i],duration_hist[3,i])
  }
  pmt_hist <- rep(NA,55)
  for (i in 1:55){
    pmt_hist[i] <- PERITHECIA_M_TEMP(temp_hist[3,i],duration_hist[3,i])
  }
  ppr_hist <- rep(NA,55)
  for (i in 1:55){
    ppr_hist[i] <- PERITHECIA_P_RH(rh_hist[3,i],duration_hist[3,i])
  }
  pmr_hist <- rep(NA,55)
  for (i in 1:55){
    pmr_hist[i] <- PERITHECIA_M_RH(rh_hist[3,i],duration_hist[3,i])
  }
  #RCP 4.5
  ppt_45 <- rep(NA,93)
  for (i in 1:93){
    ppt_45[i] <- PERITHECIA_P_TEMP(temp_45[3,i],duration_45[3,i])
  }
  pmt_45 <- rep(NA,93)
  for (i in 1:93){
    pmt_45[i] <- PERITHECIA_M_TEMP(temp_45[3,i],duration_45[3,i])
  }
  ppr_45 <- rep(NA,93)
  for (i in 1:93){
    ppr_45[i] <- PERITHECIA_P_RH(rh_45[3,i],duration_45[3,i])
  }
  pmr_45 <- rep(NA,93)
  for (i in 1:93){
    pmr_45[i] <- PERITHECIA_M_RH(rh_45[3,i],duration_45[3,i])
  }
  #RCP 8.5
  ppt_85 <- rep(NA,93)
  for (i in 1:93){
    ppt_85[i] <- PERITHECIA_P_TEMP(temp_85[3,i],duration_85[3,i])
  }
  pmt_85 <- rep(NA,93)
  for (i in 1:93){
    pmt_85[i] <- PERITHECIA_M_TEMP(temp_85[3,i],duration_85[3,i])
  }
  ppr_85 <- rep(NA,93)
  for (i in 1:93){
    ppr_85[i] <- PERITHECIA_P_RH(rh_85[3,i],duration_85[3,i])
  }
  pmr_85 <- rep(NA,93)
  for (i in 1:93){
    pmr_85[i] <- PERITHECIA_M_RH(rh_85[3,i],duration_85[3,i])
  }
  #Summarizing FHB risk variables
  #fhb_risk_1: relative ascospore discharge by temperature
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[13,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_1_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[13,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    fhb_risk_1_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[13,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_1_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[13,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_1_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[13,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    fhb_risk_1_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[13,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_1_85 <- apply(rcp85_matrix,2,QUANT)
  }
  #fhb_risk_2: relative ascospore discharge by relative humidity
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[14,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_2_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[14,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    fhb_risk_2_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[14,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_2_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[14,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_2_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[14,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    fhb_risk_2_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[14,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_2_85 <- apply(rcp85_matrix,2,QUANT)
  }
  #fhb_risk_3: relative ascospore discharge by relative humidity (trail et al)
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[15,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_3_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[15,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    fhb_risk_3_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[15,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_3_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[15,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_3_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[15,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    fhb_risk_3_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[15,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_3_85 <- apply(rcp85_matrix,2,QUANT)
  }
  #fhb_risk_4: logical test days above relative humidity threshold
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[16,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_4_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[16,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    fhb_risk_4_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[16,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_4_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[16,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    fhb_risk_4_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[16,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    fhb_risk_4_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[16,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    fhb_risk_4_85 <- apply(rcp85_matrix,2,QUANT)
  }
  
  #plotting
  layout(matrix(c(1,1,2),nrow=1))
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab="Year",ylab="Relative risk (%)",
       main=NULL,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,1,0.1),las=2,labels=seq(0,100,10))
  abline(v=55)
  
  lines(y=fhb_risk_1_hist[3,],x=seq(1:55),col="red",lwd=2)
  lines(y=fhb_risk_1_85[3,],x=seq(55,147,1),col="red",lwd=2)
  
  lines(y=ppt_hist,x=seq(1:55),col="orange",lwd=2)
  lines(y=ppt_85,x=seq(55,147,1),col="orange",lwd=2)
  
  lines(y=ppr_hist,x=seq(1:55),col="blue",lwd=2)
  lines(y=ppr_85,x=seq(55,147,1),col="blue",lwd=2)
  
  lines(y=pmt_hist,x=seq(1:55),col="green",lwd=2)
  lines(y=pmt_85,x=seq(55,147,1),col="green",lwd=2)
  
  lines(y=pmr_hist,x=seq(1:55),col="cyan",lwd=2)
  lines(y=pmr_85,x=seq(55,147,1),col="cyan",lwd=2)
  
  lines(y=fhb_risk_2_hist[3,],x=seq(1:55),col="purple",lwd=2)
  lines(y=fhb_risk_2_85[3,],x=seq(55,147,1),col="purple",lwd=2)
  
  lines(y=fhb_risk_3_hist[3,],x=seq(1:55),col="purple",lwd=2)
  lines(y=fhb_risk_3_85[3,],x=seq(55,147,1),col="purple",lwd=2)
  
  lines(y=fhb_risk_4_hist[3,],x=seq(1:55),col="black",lwd=2)
  lines(y=fhb_risk_4_85[3,],x=seq(55,147,1),col="black",lwd=2)
  
  plot(0,type="n",xlim=c(0,1),ylim=c(0,1),xlab=" ",ylab=" ",
        main=NULL,axes=FALSE)
  legend("center",
              legend=c("Ascospore discharge by temperature",
                       "Ascospore discharge by relative humidity",
                       "Perithecia production by temperature",
                       "Perithecia production by relative humidity",
                       "Perithecia maturation by temperature",
                       "Perithecia maturation by relative humidity",
                       "Days where relative humidity exceeds 70%"),
              col=c("red",
                    "purple",
                    "orange",
                    "blue",
                    "green",
                    "cyan",
                    "black"),
              lty=1,lwd=2,cex=1,bty="n",
         title = paste0(locations$town[n],": ",stage_lab[s]," stage"))
  
}


FHB_RISK_PLOTTER(1,4,1)


       