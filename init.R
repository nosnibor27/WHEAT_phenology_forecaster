#dependent packages
library(ggmap)
library(stringr)
library(RNetCDF)
library(zoo)

#Add sites of interest here using "Town State"
sites <- c("Colfax Washington",
           "Pendleton Oregon",
           "Havre Montana",
           "Akron Colorado",
           "Hutchinson Kansas",
           "Enid Oklahoma")

#creating a data frame
locations <- data.frame(sites,stringsAsFactors = FALSE)

#number of sites in location file
N <- nrow(locations)

#adding column for town name (used for labelling arrays)
locations$town <- word(sites,1)

#adding columns for longitude and latitude
locations$lon <- rep(NA,N)
locations$lat <- rep(NA,N)

#looking up coordinates
for (n in 1:N){
  gps <- geocode(sites[n],source = "dsk")
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

#downloading climate data
source("netcdf_downloader.r")

#option to save arrays
source("saving_arrays.r")

#option to load saved arrays
source("loading_arrays.r")

#inputs for wheat grown model

#planting depth (in inches)
planting_depth <- 2

#vector of early, mid, and late planting dates (input as "-MM-DD")
planting_dates <- c("-09-01","-10-01","-11-01")

#assigning genetic photoperiod coefficient using "Nugaines" wheat (G_1)
G_1 <- 0.0044

#need to set leaf appearance rate based on thermal time per leaf (phyllochron) 
#when not known a good recommended estimate is 95
phyllochron <- 95

#running the CERES-Wheat model using the inputs above
source("CERES_wheat_model.r")

#adding humidity variables to the arrays
source("additional_weather_variables.r")

#plotting functions
source("plotting.r")

