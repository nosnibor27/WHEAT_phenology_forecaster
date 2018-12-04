# Forecasting climate conditions during phenological stages of winter wheat under climate change

This repository is, at it's core, an applied indexing problem.

The master file is `init.R`. Locations in the form of "Town Location" are concatenated into a vector called `sites`. The `geocode` function from the `ggmap` package is used to query the appropriate coordinates and add them to the `locations` dataframe. The "Town Location" format is important as "Town" will be utilized when naming the subsequent arrays. Elevation data is queried next by locating the appropriate raster within the gridMET database using a netCDF file.

Data is requested from the Northwest Knowledge Network THREDDS server. The URL is constructed using combinations of the base URL named `url_1`, the list of global climate models named `model`, a specification for which model was used named `condition`, and a list of available climate variables named `variable_1`. While aggregated data is available, it is easier on the server to request the daily climate data in pieces of around ~5000 to ~7000 days at a time instead of the full ~20,000 to ~33,000 days for the historical and future datasets, respectively. Those are specified using either `timestep_historical`, `timestep_45`, or `timestep_85`.

Data is requested by running `netcdf_downloader.R`. The functions `open.nc` and `var.get.nc` from the `RNetCDF` package are used to collect data from the appropriate raster within the dataset for each location. Unfortunately, it takes forever to download and the `for` loop is not parallelized. I currently have data for the following locations:

- Moscow, ID
- Genesee, ID
- Troy, ID
- Deary, ID
- Potlatch, ID
- Pullman, WA
- Palouse, Wa
- Tekoa, WA
- Hay, WA
- Endicott, Wa
- Pendleton, OO
- Havre, MT
- Akron, CO
- Hutchinson, KS
- Enid, OK

Data for Pullman, WA is in the `DATA` folder. Update line 8 of `init.R` to `sites <- c("Pullman Washington")` and run the appropriate code to set `N` to 1 and query latitude, longitude, and elevation data. The `locations` dataframe can also be used to load in previously downloaded arrays by running `loading_arrays.R`. When starting from saved arrays, be sure that `model` is in your environment as it gets called during subsequent processing while printing progress.

After the arrays for each location have been collected, they can be saved as .rdata files by running `saving_arrays.R`. 

There are 4 user-defined inputs for the CERES-Wheat growth model: `planting_depth`, `planting_dates`, a genetic photoperiod coefficient `G_1`, and `phyllochron`. The planting depth is in inches. Currently, 3 planting dates are used to cover the reported historical range. They are currently the 1st of September, October, and November and represent early-, mid-, and late-season planting dates. They can be changed to any value, provided they are in the "-MM-DD" format. Wheat varieties have different growth characteristics due to photoperiod, which is defined using `G_1`. The range referenced in the original CERES-Wheat material is from 0.002 to 0.006. The current default is 0.0044 which is for "Nugaines" wheat. The other important variety-specific term is `phyllochron` which is the leaf appearance rate based on thermal time per leaf. When this value is not known the recommended default is 95.

Running `CERES_wheat_model.R` initializes the wheat growth model. It begins by calculating growing degree-days (GDD) for each location using daily maximum and minimum temperature. Photoperiod is then calculated for each location which is used along with `G_1` to calculate the daily relative development rate (RDR). A modified GDD variable is then created by multiplying GDD by RDR for each day. The index locations for all planting dates are then determined and saved as vectors. The amount of GDD for each developmental stage of wheat is calculated and then the `INDEXER` function locates the index where different stages start and finish. The `SPANNER` function is used to calculate duration of the wheat phenostages and is called when using `PLOT_STAGE`.The results are compiled into  4-d arrays.

After all the index locations have been determined, additional humidity related variables are calculated for each location by running `additional_weather_variables.R`. The current list of calculated variables are: relative humidity, vapor pressure deficit, humidex (or humisery), and potential evapotranspiration.

Special functions for plotting results are created when running `plotting.R`. There are 3 arguments: location, stage, and planting date. `PLOT_DOY(2,5,2)` will plot the day of the year for the 2nd town in the `locations` dataframe, the 5th stage of wheat development which is where flowering occurs, for the 2nd planting date. Both `PLOT_DASHBOARD_1` and `PLOT_DASHBOARD_2` are used to make summary plots.
