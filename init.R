#dependent packages
library(ggmap)
library(stringr)
library(RNetCDF)
library(zoo)

#Add sites of interest here using "Town State"
sites <- c("Moscow Idaho",
           "Troy Idaho",
           "Genesee Idaho",
           "Deary Idaho",
           "Potlatch Idaho",
           "Pullman Washington",
           "Palouse Washington",
           "Tekoa Washington",
           "Endicott Washington",
           "Hay Washington",
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

#base URL
url_1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/"

#model list
model <- c("bcc-csm1-1", "bcc-csm1-1-m","BNU-ESM","CanESM2",
           "CSIRO-Mk3-6-0","GFDL-ESM2G","GFDL-ESM2M","HadGEM2-CC365",
           "HadGEM2-ES365","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR",
           "IPSL-CM5B-LR","MIROC5","MIROC-ESM","MIROC-ESM-CHEM",
           "MRI-CGCM3","NorESM1-M","CNRM-CM5","CCSM4")

#model condition list becuase CCSM4 is different
condition <- c(rep("_r1i1p1_",19),"_r6i1p1_")

#timestep lists
timestep_historical <- c("historical_1950_1969_CONUS_daily.nc",
                         "historical_1970_1989_CONUS_daily.nc",
                         "historical_1990_2005_CONUS_daily.nc")
timestep_45 <- c("rcp45_2006_2025_CONUS_daily.nc",
                 "rcp45_2026_2045_CONUS_daily.nc",
                 "rcp45_2046_2065_CONUS_daily.nc",
                 "rcp45_2066_2085_CONUS_daily.nc",
                 "rcp45_2086_2099_CONUS_daily.nc")
timestep_85 <- c("rcp85_2006_2025_CONUS_daily.nc",
                 "rcp85_2026_2045_CONUS_daily.nc",
                 "rcp85_2046_2065_CONUS_daily.nc",
                 "rcp85_2066_2085_CONUS_daily.nc",
                 "rcp85_2086_2099_CONUS_daily.nc")

#variable list
variable_1 <- c("_tasmax_","_tasmin_","_pr_","_huss_","_was_","_rsds_")

#downloading climate data
source("netcdf_downloader.r")

#option to save arrays
source("saving_arrays.r")

#option to load saved arrays
source("loading_arrays.r")

#inputs for CERES-Wheat growth model

#planting depth (in inches)
planting_depth <- 2

#vector of early, mid, and late planting dates (input as "-MM-DD")
planting_dates <- c("-09-01","-10-01","-11-01")

#assigning genetic photoperiod coefficient using "Nugaines" wheat (G_1)
G_1 <- 0.004075

#need to set leaf appearance rate based on thermal time per leaf (phyllochron) 
#when not known a good recommended estimate is 95
phyllochron <- 100

#running the CERES-Wheat model using the inputs above
source("CERES_wheat_model.r")

#adding humidity variables to the arrays
source("additional_weather_variables.r")

#plotting functions
source("plotting.r")

#plotting dashboard example



