#plotting functions----
PLOT_STAGE <- function(n,s,p,t){
  A <- get(paste0(locations$town[n],"_span_hist"))
  B <- get(paste0(locations$town[n],"_span_45"))
  C <- get(paste0(locations$town[n],"_span_85"))
  quantile_hist <- apply(A[,,s,p],2,QUANT)
  quantile_45 <- apply(B[,,s,p],2,QUANT)
  quantile_85 <- apply(C[,,s,p],2,QUANT)
  start_date <- c(pd_hist_e[1],pd_hist_m[1],pd_hist_l[1])
  month_doy <- c(1,32,60,91,121,152,182,213,244,274,305,335)
  month_lab <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  month_lty <- c(1,rep(2,11))
  plot(0,type="n",xlim=c(0,150),ylim=c(0,220),xlab="Year",ylab=" ",
       main=paste("Completion date",t),axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=month_doy,labels=month_lab,las=2)
  for (m in 1:12){
    abline(h=month_doy[m],lty=month_lty[m])
  }
  if(s==1){
    shade((start_date[p]+quantile_hist[c(1,5),]),lim=seq(1,55,1),col=col.alpha("black",0.15))
    shade((start_date[p]+quantile_hist[c(2,4),]),lim=seq(1,55,1),col=col.alpha("black",0.15))
    shade((start_date[p]+quantile_45[c(1,5),]),lim=seq(55,147,1),col=col.alpha("blue",0.15))
    shade((start_date[p]+quantile_45[c(2,4),]),lim=seq(55,147,1),col=col.alpha("blue",0.15))
    shade((start_date[p]+quantile_85[c(1,5),]),lim=seq(55,147,1),col=col.alpha("red",0.15))
    shade((start_date[p]+quantile_85[c(2,4),]),lim=seq(55,147,1),col=col.alpha("red",0.15))
    lines(y=(start_date[p]+quantile_hist[3,]),x=seq(1:55),col="black",lwd=2)
    lines(y=(start_date[p]+quantile_45[3,]),x=seq(55,147,1),col="blue",lwd=2)
    lines(y=(start_date[p]+quantile_85[3,]),x=seq(55,147,1),col="red",lwd=2)
  }else{
    shade((start_date[p]+quantile_hist[c(1,5),])-365,lim=seq(1,55,1),col=col.alpha("black",0.15))
    shade((start_date[p]+quantile_hist[c(2,4),])-365,lim=seq(1,55,1),col=col.alpha("black",0.15))
    shade((start_date[p]+quantile_45[c(1,5),])-365,lim=seq(55,147,1),col=col.alpha("blue",0.15))
    shade((start_date[p]+quantile_45[c(2,4),])-365,lim=seq(55,147,1),col=col.alpha("blue",0.15))
    shade((start_date[p]+quantile_85[c(1,5),])-365,lim=seq(55,147,1),col=col.alpha("red",0.15))
    shade((start_date[p]+quantile_85[c(2,4),])-365,lim=seq(55,147,1),col=col.alpha("red",0.15))
    lines(y=(start_date[p]+quantile_hist[3,])-365,x=seq(1:55),col="black",lwd=2)
    lines(y=(start_date[p]+quantile_45[3,])-365,x=seq(55,147,1),col="blue",lwd=2)
    lines(y=(start_date[p]+quantile_85[3,])-365,x=seq(55,147,1),col="red",lwd=2)
  }
}

PLOT_AVG_TEMP <- function(n,s,p,t){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  C <- get(paste0(locations$town[n],"_45"))
  D <- get(paste0(locations$town[n],"_index_45"))
  E <- get(paste0(locations$town[n],"_85"))
  G <- get(paste0(locations$town[n],"_index_85"))
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[7,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[7,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[7,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[7,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[7,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[7,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }
  plot(0,type="n",xlim=c(0,150),ylim=c(0,25),xlab="Year",
       ylab=expression(paste(degree,"C")),
       main=paste("Average temperature",t),axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
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


PLOT_RH <- function(n,s,p,t){
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  C <- get(paste0(locations$town[n],"_45"))
  D <- get(paste0(locations$town[n],"_index_45"))
  E <- get(paste0(locations$town[n],"_85"))
  G <- get(paste0(locations$town[n],"_index_85"))
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
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[9,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[9,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[9,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[9,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[9,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab="Year",ylab="%",
       main=paste("Relative humidity",t),axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
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

PLOT_RAIN <- function(n,s,p,t){
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  C <- get(paste0(locations$town[n],"_45"))
  D <- get(paste0(locations$town[n],"_index_45"))
  E <- get(paste0(locations$town[n],"_85"))
  G <- get(paste0(locations$town[n],"_index_85"))
  if(s==1){
    pd_matrix_hist <- rbind(pd_hist_e,pd_hist_m,pd_hist_l)
    pd_matrix_fut <- rbind(pd_fut_e,pd_fut_m,pd_fut_l)
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- sum(A[3,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- sum(C[3,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- sum(E[3,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- sum(A[3,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- sum(C[3,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- sum(E[3,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }
  plot(0,type="n",xlim=c(0,150),ylim=c(0,150),
       xlab="Year",ylab="mm",
       main=paste("Total rainfall",t),axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,400,25),las=2)
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

#constant parameters----

#planting depth (in inches)
planting_depth <- 2

#vector of early, mid, and late planting dates (input as "-MM-DD")
planting_dates <- c("-09-01","-10-01","-11-01")

#adding humidity variables to the arrays
source("additional_weather_variables.r")

#final plot
par(mfcol=c(4,2))

#need to set leaf appearance rate based on thermal time per leaf (phyllochron) 
#when not known a good recommended estimate is 95
phyllochron <- 95

#running the CERES-Wheat model using the inputs above
source("CERES_wheat_model_no_RDR.r")

PLOT_STAGE(1,4,2,"(PHINT = 95)")
PLOT_AVG_TEMP(1,4,2,"(PHINT = 95)")
PLOT_RH(1,4,2,"(PHINT = 95)")
PLOT_RAIN(1,4,2,"(PHINT = 95)")

phyllochron <- 162

#running the CERES-Wheat model using the inputs above
source("CERES_wheat_model_no_RDR.r")

PLOT_STAGE(1,4,2,"(PHINT = 162)")
PLOT_AVG_TEMP(1,4,2,"(PHINT = 162)")
PLOT_RH(1,4,2,"(PHINT = 162)")
PLOT_RAIN(1,4,2,"(PHINT = 162)")


