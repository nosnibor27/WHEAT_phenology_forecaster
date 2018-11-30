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