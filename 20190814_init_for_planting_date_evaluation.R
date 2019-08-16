#Add sites of interest here using "Town State"
sites <- c("Moses Lake Washington")

#creating a data frame
locations <- data.frame(sites,stringsAsFactors = FALSE)

#number of sites in location file
N <- nrow(locations)

#adding column for town name (used for labelling arrays)
locations$town <- word(sites,1)

#adding columns for longitude and latitude
locations$lon <- rep(NA,N)
locations$lat <- rep(NA,N)

# API for Google Maps
google_key = 'AIzaSyBU6cCdTozkE1zwrbhTHPUXoF_gHYKbiog'
register_google(key = google_key)

#looking up coordinates
for (n in 1:N){
  gps <- geocode(sites[n],source = "google")
  locations$lon[n] <- as.numeric(gps[1])
  locations$lat[n] <- as.numeric(gps[2])
}

#adding elevation data
elevation_grid <- open.nc("metdata_elevationdata.nc")
elevation_ref <- var.get.nc(elevation_grid,variable=2)

#adding elevation column
locations$elevation <- rep(NA,N)

#adding elevation values
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
#loading relevant array
for(n in 1:N){
  assign(paste0(locations$town[n],"_hist"),
         readRDS(paste0(locations$town[n],"_hist.rdata")))
  print(paste(locations$town[n],"historical simulation loaded"))
}
for(n in 1:N){
  assign(paste0(locations$town[n],"_85"),
         readRDS(paste0(locations$town[n],"_85.rdata")))
  print(paste(locations$town[n],"RCP 8.5 loaded"))
}

#adding adding variables to the arrays
source("additional_weather_variables_rcp_85_only.r")

#inputs for CERES-Wheat growth model

#planting depth (in inches)
planting_depth <- 2

#vector of planting dates (input as "-MM-DD")
planting_dates <- c("-09-15","-10-10","-10-20")

#assigning genetic photoperiod coefficient using "Nugaines" wheat (G_1)
G_1 <- 0.004075

#need to set leaf appearance rate based on thermal time per leaf (phyllochron) 
#when not known a good recommended estimate is 95
phyllochron <- 120

#running the CERES-Wheat model using the inputs above
source("CERES_wheat_model_RDR_only.r")



PLOT_DURATION <- function(n,s,p){
  A <- get(paste0(locations$town[n],"_span_hist"))
  C <- get(paste0(locations$town[n],"_span_85"))
  if(s==1){
    quantile_hist <- apply(A[,,s,p],2,QUANT)
    quantile_85 <- apply(C[,,s,p],2,QUANT)
  }else{
    quantile_hist <- apply(A[,,s,p]-A[,,s-1,p],2,QUANT)
    quantile_85 <- apply(C[,,s,p]-C[,,s-1,p],2,QUANT)
  }
  plot(0,type="n",xlim=c(0,150),ylim=c(0,max(c(quantile_hist,quantile_85))),xlab="Year",ylab=paste(stage_lab[s],"duration (days)"),
       main=NULL,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,300,10),las=2)
  shade((quantile_hist[c(1,5),]),lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade((quantile_hist[c(2,4),]),lim=seq(1,55,1),col=col.alpha("black",0.15))
  shade((quantile_85[c(1,5),]),lim=seq(55,147,1),col=col.alpha("red",0.15))
  shade((quantile_85[c(2,4),]),lim=seq(55,147,1),col=col.alpha("red",0.15))
  lines(y=(quantile_hist[3,]),x=seq(1:55),col="black",lwd=2)
  lines(y=(quantile_85[3,]),x=seq(55,147,1),col="red",lwd=2)
}

PLOT_DURATION(1,4,1)
PLOT_DURATION(1,4,2)
PLOT_DURATION(1,4,3)

#vector of additional planting dates (input as "-MM-DD")
planting_dates <- c("-08-15","-9-10","-12-20")

#running the CERES-Wheat model using the inputs above
source("CERES_wheat_model_RDR_only.r")

PLOT_DURATION(1,4,1)
PLOT_DURATION(1,4,2)
PLOT_DURATION(1,4,3)