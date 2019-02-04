#Plotting functions from Richard McElreath's "rethinking" package
col.alpha <- function( acol , alpha=0.2 ) {
  acol <- col2rgb(acol)
  acol <- rgb(acol[1]/255,acol[2]/255,acol[3]/255,alpha)
  acol
}
shade <- function( object , lim , label=NULL , col=col.alpha("black",0.15) , border=NA , ... ) {
  if ( missing(lim) ) stop( "Interval limits missing." )
  if ( missing(object) ) stop( "No density or formula object." )
  from <- lim[1]
  to <- lim[2]
  if ( class(object)=="formula" ) {
    # formula input
    x1 <- eval( object[[3]] )
    y1 <- eval( object[[2]] )
    x <- x1[ x1>=from & x1<=to ]
    y <- y1[ x1>=from & x1<=to ]
  }
  if ( class(object)=="density" ) {
    # density input
    x <- object$x[ object$x>=from & object$x<=to ]
    y <- object$y[ object$x>=from & object$x<=to ]
  }
  if ( class(object)=="matrix" & length(dim(object))==2 ) {
    # matrix defining confidence region around a curve
    y <- c( object[1,] , object[2,][ncol(object):1] ) # reverse second row
    x <- c( lim , lim[length(lim):1] ) # lim needs to be x-axis values
  }
  # draw
  if ( class(object)=="matrix" ) {
    polygon( x , y , col=col , border=border , ... )
  } else {
    polygon( c( x , to , from ) , c( y , 0 , 0 ) , col=col , border=border , ... )
  }
  # label?
  if ( !is.null(label) ) {
    lx <- mean(x)
    ly <- max(y)/2
    text( lx , ly , label )
  }
}

#function to collect quantiles
QUANT <- function(x) quantile(x,probs = c(0.1,0.25,0.5,0.75,0.9))

stage_lab <- c("Emergence","Tillering","Booting",
               "Flowering","Grain filling","Maturity","Harvesting")

PLOT_STAGE <- function(n,s,p){
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
  plot(0,type="n",xlim=c(0,150),ylim=c(0,365),xlab="Year",ylab=paste(stage_lab[s],"completion date"),
       main=NULL,axes=FALSE)
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

PLOT_DURATION <- function(n,s,p){
  A <- get(paste0(locations$town[n],"_span_hist"))
  B <- get(paste0(locations$town[n],"_span_45"))
  C <- get(paste0(locations$town[n],"_span_85"))
  if(s==1){
    quantile_hist <- apply(A[,,s,p],2,QUANT)
    quantile_45 <- apply(B[,,s,p],2,QUANT)
    quantile_85 <- apply(C[,,s,p],2,QUANT)
  }else{
    quantile_hist <- apply(A[,,s,p]-A[,,s-1,p],2,QUANT)
    quantile_45 <- apply(B[,,s,p]-B[,,s-1,p],2,QUANT)
    quantile_85 <- apply(C[,,s,p]-C[,,s-1,p],2,QUANT)
  }
  plot(0,type="n",xlim=c(0,150),ylim=c(0,max(c(quantile_hist,quantile_45,quantile_85))),xlab="Year",ylab=paste(stage_lab[s],"duration (days)"),
       main=NULL,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,300,10),las=2)
  shade((quantile_hist[c(1,5),]),lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade((quantile_hist[c(2,4),]),lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade((quantile_45[c(1,5),]),lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade((quantile_45[c(2,4),]),lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade((quantile_85[c(1,5),]),lim=seq(55,147,1),col=col.alpha("red",0.15))
  shade((quantile_85[c(2,4),]),lim=seq(55,147,1),col=col.alpha("red",0.15))
  lines(y=(quantile_hist[3,]),x=seq(1:55),col="black",lwd=2)
  lines(y=(quantile_45[3,]),x=seq(55,147,1),col="blue",lwd=2)
  lines(y=(quantile_85[3,]),x=seq(55,147,1),col="red",lwd=2)
}

PLOT_AVG_TEMP <- function(n,s,p){
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
  plot(0,type="n",xlim=c(0,150),ylim=c(0,40),xlab="Year",
       ylab=paste(stage_lab[s],"average temperature (°C)"),
       main=NULL,axes=FALSE)
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


PLOT_RH <- function(n,s,p){
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
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab="Year",ylab=paste(stage_lab[s],"average relative humidity (%)"),
       main=NULL,axes=FALSE)
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

PLOT_VPD <- function(n,s,p){
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
        hist_vector[k] <- mean(A[10,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[10,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[10,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[10,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[10,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[10,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }
  plot(0,type="n",xlim=c(0,150),ylim=c(0,3),xlab="Year",ylab=paste(stage_lab[s],"average vapor pressure deficit (kPa)"),
       main=NULL,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
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

PLOT_HDX <- function(n,s,p){
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
        hist_vector[k] <- mean(A[11,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[11,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[11,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- mean(A[11,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- mean(C[11,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- mean(E[11,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }
  plot(0,type="n",xlim=c(0,150),ylim=c(0,30),xlab="Year",ylab=paste(stage_lab[s],"average humidex (°C)"),
       main=NULL,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
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

PLOT_ETo <- function(n,s,p){
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
        hist_vector[k] <- sum(A[12,j,pd_matrix_hist[p,k]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- sum(C[12,j,pd_matrix_fut[p,k]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- sum(E[12,j,pd_matrix_fut[p,k]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }else{
    hist_vector <- rep(0,55)
    hist_matrix <- matrix(NA,20,55)
    for(j in 1:20){
      for(k in 1:55){
        hist_vector[k] <- sum(A[12,j,B[j,k,s-1,p]:B[j,k,s,p]])
        hist_matrix[j,] <- hist_vector
      }
    }
    quantile_hist <- apply(hist_matrix,2,QUANT)
    rcp45_vector <- rep(0,93)
    rcp45_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp45_vector[k] <- sum(C[12,j,D[j,k,s-1,p]:D[j,k,s,p]])
        rcp45_matrix[j,] <- rcp45_vector
      }
    }
    quantile_45 <- apply(rcp45_matrix,2,QUANT)
    rcp85_vector <- rep(0,93)
    rcp85_matrix <- matrix(NA,20,93)
    for(j in 1:20){
      for(k in 1:93){
        rcp85_vector[k] <- sum(E[12,j,G[j,k,s-1,p]:G[j,k,s,p]])
        rcp85_matrix[j,] <- rcp85_vector
      }
    }
    quantile_85 <- apply(rcp85_matrix,2,QUANT)
  }
  k_c <- c(0.7,0.85,1,1.15,0.85,0.55,0.25)
  quantile_hist <- quantile_hist*k_c[s]
  quantile_45 <- quantile_45*k_c[s]
  quantile_85 <- quantile_85*k_c[s]
  plot(0,type="n",xlim=c(0,150),ylim=c(0,max(c(quantile_hist,quantile_45,quantile_85))),
       xlab="Year",ylab=paste(stage_lab[s],"total evapotranspiration (mm)"),
       main=NULL,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,400,10),las=2)
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

PLOT_RAIN <- function(n,s,p,title){
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
  plot(0,type="n",xlim=c(0,150),ylim=c(0,max(c(quantile_hist,quantile_45,quantile_85))),
       xlab="Year",ylab=paste(stage_lab[s],"total rainfall (mm)"),
       main=NULL,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,400,10),las=2)
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



PLOT_DASHBOARD_1 <- function(n,s,p){
  layout(matrix(c(1,2,2,2,1,3,3,3,4,5,6,7), nrow = 3, ncol = 4, byrow = TRUE))
  par(oma=c(0.5,0.5,5,0.5))
  par(mar=c(4,4,0.2,0.2))
  PLOT_STAGE(n,s+1,p)
  PLOT_AVG_TEMP(n,s,p)
  PLOT_RH(n,s,p)
  PLOT_VPD(n,s,p)
  PLOT_HDX(n,s,p)
  PLOT_RAIN(n,s,p)
  PLOT_ETo(n,s,p)
  mtext(locations$sites[n],side=3,line=1,outer=TRUE,cex=3)
}

PLOT_DASHBOARD_2 <- function(n,s,p){
  layout(matrix(c(1,2,3,1,4,5), nrow = 2, ncol = 3, byrow = TRUE))
  par(oma=c(0.5,0.5,5,0.5))
  par(mar=c(4,4,0.2,0.2))
  PLOT_STAGE(n,s+1,p)
  PLOT_AVG_TEMP(n,s,p)
  PLOT_RH(n,s,p)
  PLOT_RAIN(n,s,p)
  PLOT_ETo(n,s,p)
  mtext(locations$sites[n],side=3,line=1,outer=TRUE,cex=3)
}

PLOT_DASHBOARD_3 <- function(n,p){
  par(mfrow=c(2,6))
  PLOT_AVG_TEMP(n,1,p,"Emergence")
  PLOT_AVG_TEMP(n,2,p,"Tillering")
  PLOT_AVG_TEMP(n,3,p,"Booting")
  PLOT_AVG_TEMP(n,4,p,"Flowering")
  PLOT_AVG_TEMP(n,5,p,"Grain Filling")
  PLOT_AVG_TEMP(n,6,p,"Maturity")
  PLOT_RH(n,1,p,"Emergence")
  PLOT_RH(n,2,p,"Tillering")
  PLOT_RH(n,3,p,"Booting")
  PLOT_RH(n,4,p,"Flowering")
  PLOT_RH(n,5,p,"Grain Filling")
  PLOT_RH(n,6,p,"Maturity")
}

