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

PLOT_DOY <- function(n,s,p){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- median(historical_doy[B[j,k,s,p]:B[j,k,s+1,p]])
      hist_matrix[j,] <- hist_vector
    }
  }
  quantile_hist <- apply(hist_matrix,2,QUANT)
  rcp45_vector <- rep(0,93)
  rcp45_matrix <- matrix(NA,20,93)
  D <- get(paste0(locations$town[n],"_index_45"))
  for(j in 1:20){
    for(k in 1:93){
      rcp45_vector[k] <- median(future_doy[D[j,k,s,p]:D[j,k,s+1,p]])
      rcp45_matrix[j,] <- rcp45_vector
    }
  }
  quantile_45 <- apply(rcp45_matrix,2,QUANT)
  rcp85_vector <- rep(0,93)
  rcp85_matrix <- matrix(NA,20,93)
  G <- get(paste0(locations$town[n],"_index_85"))
  for(j in 1:20){
    for(k in 1:93){
      rcp85_vector[k] <- median(future_doy[G[j,k,s,p]:G[j,k,s+1,p]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,365),xlab="Year",ylab="J",
       main="Day of year",axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
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

PLOT_AVG_TEMP <- function(n,s,p){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- mean(A[7,j,B[j,k,s,p]:B[j,k,s+1,p]])
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
      rcp45_vector[k] <- mean(C[7,j,D[j,k,s,p]:D[j,k,s+1,p]])
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
      rcp85_vector[k] <- mean(E[7,j,G[j,k,s,p]:G[j,k,s+1,p]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,40),xlab="Year",ylab="C",
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
  mtext("Mean Temperature",side=3,line=-1.5)
}

PLOT_RH <- function(n,s,p){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- mean(A[9,j,B[j,k,s,p]:B[j,k,s+1,p]])
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
      rcp45_vector[k] <- mean(C[9,j,D[j,k,s,p]:D[j,k,s+1,p]])
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
      rcp85_vector[k] <- mean(E[9,j,G[j,k,s,p]:G[j,k,s+1,p]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,1),xlab="Year",ylab="%",
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
  mtext("Relative humidity",side=3,line=-1.5)
}

PLOT_VPD <- function(n,s,p){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- mean(A[10,j,B[j,k,s,p]:B[j,k,s+1,p]])
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
      rcp45_vector[k] <- mean(C[10,j,D[j,k,s,p]:D[j,k,s+1,p]])
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
      rcp85_vector[k] <- mean(E[10,j,G[j,k,s,p]:G[j,k,s+1,p]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,3),xlab="Year",ylab="kPa",
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
  mtext("Vapor pressure deficit",side=3,line=-1.5)
}

PLOT_HDX <- function(n,s,p){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- mean(A[11,j,B[j,k,s,p]:B[j,k,s+1,p]])
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
      rcp45_vector[k] <- mean(C[11,j,D[j,k,s,p]:D[j,k,s+1,p]])
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
      rcp85_vector[k] <- mean(E[11,j,G[j,k,s,p]:G[j,k,s+1,p]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,30),xlab="Year",ylab="C",
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
  mtext("Humidex",side=3,line=-1.5)
}

PLOT_ETo <- function(n,s,p){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- sum(A[12,j,B[j,k,s,p]:B[j,k,s+1,p]])
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
      rcp45_vector[k] <- sum(C[12,j,D[j,k,s,p]:D[j,k,s+1,p]])
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
      rcp85_vector[k] <- sum(E[12,j,G[j,k,s,p]:G[j,k,s+1,p]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,100),xlab="Year",ylab="mm",
       main=NULL,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
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
  mtext("Potential evapotranspiration",side=3,line=-1.5)
}

PLOT_RAIN <- function(n,s,p){
  hist_vector <- rep(0,55)
  hist_matrix <- matrix(NA,20,55)
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_index_hist"))
  for(j in 1:20){
    for(k in 1:55){
      hist_vector[k] <- sum(A[3,j,B[j,k,s,p]:B[j,k,s+1,p]])
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
      rcp45_vector[k] <- sum(C[3,j,D[j,k,s,p]:D[j,k,s+1,p]])
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
      rcp85_vector[k] <- sum(E[3,j,G[j,k,s,p]:G[j,k,s+1,p]])
      rcp85_matrix[j,] <- rcp85_vector
    }
  }
  quantile_85 <- apply(rcp85_matrix,2,QUANT)
  plot(0,type="n",xlim=c(0,150),ylim=c(0,100),xlab="Year",ylab="mm",
       main=NULL,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
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
  mtext("Rainfall",side=3,line=-1.5)
}

PLOT_STAGE <- function(n,s,p){
  A <- get(paste0(locations$town[n],"_span_hist"))
  B <- get(paste0(locations$town[n],"_span_45"))
  C <- get(paste0(locations$town[n],"_span_85"))
  quantile_hist <- apply(A[,,s,p],2,QUANT)
  quantile_45 <- apply(B[,,s,p],2,QUANT)
  quantile_85 <- apply(C[,,s,p],2,QUANT)
  start_date <- c(pd_hist_e[1],pd_hist_m[1],pd_hist_l[1])
  month_doy <- c(1,32,60,91,121,152,182,213,244,274,305)
  month_lab <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")
  month_lty <- c(1,rep(2,10))
  stage_lab <- c("Germination","Emergence","Tillering","Booting",
                 "Flowering","Grain filling","Maturity")
  plot(0,type="n",xlim=c(0,150),ylim=c(0,365),xlab="Year",ylab="Day of the year",
       main=NULL,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,350,50),las=2)
  for (m in 1:11){
    abline(h=month_doy[m],lty=month_lty[m])
    text(x=150,y=month_doy[m],labels = month_lab[m],pos=3)
  }
  shade((start_date[p]+quantile_hist[c(1,5),])-365,lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade((start_date[p]+quantile_hist[c(2,4),])-365,lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade((start_date[p]+quantile_45[c(1,5),])-365,lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade((start_date[p]+quantile_45[c(2,4),])-365,lim=seq(55,147,1),col=col.alpha("blue",0.15))
  shade((start_date[p]+quantile_85[c(1,5),])-365,lim=seq(55,147,1),col=col.alpha("red",0.15))
  shade((start_date[p]+quantile_85[c(2,4),])-365,lim=seq(55,147,1),col=col.alpha("red",0.15))
  lines(y=(start_date[p]+quantile_hist[3,])-365,x=seq(1:55),col="black",lwd=2)
  lines(y=(start_date[p]+quantile_45[3,])-365,x=seq(55,147,1),col="blue",lwd=2)
  lines(y=(start_date[p]+quantile_85[3,])-365,x=seq(55,147,1),col="red",lwd=2)
  mtext(stage_lab[s],side=3,line=1.5,cex=2)
  legend("top", inset=.02, title="Scenario",
         c("Historical simulation","RCP 4.5","RCP 8.5"), fill=c("black","blue","red"))
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
