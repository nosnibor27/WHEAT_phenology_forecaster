# WW_phenology_forecaster

## Background
  This repository is, at it's core, an applied indexing problem. Imagine you have an Excel spreadsheet with two columns: date and average daily temperature. The main goal within this repository to determine the number of days to hit a specified sum, given a specific starting date. This approach is analogous to having an income column (in $/day) and trying to forecast how many days it will take to clear rent or accumulate enough savings for a major purchase. What I am doing is determining how many days it would take for average temperatures to sum past a given threshhold, and the units become growing degree-days (GDD). The duration of growth for an agricultural crop is usually directly proportional to temperature, and using the sum of average daily temperatures to predict growth stages has been known since Reaumur in the 1700s. Since a plant's time scale is so closely coupled with the thermal environment, it folows to think of thermal time as a plant's view of time. Accumulating thermal time in the form of total GDD allows for the determination of the dates when various plant growth stages are likely to occur. The underlying code was adapted from [here](https://stackoverflow.com/questions/21248946/calculation-of-sum-of-numbers-to-reach-a-certain-point) and presented below.
  
```r
S_1 <- as.vector(sapply(
    start,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_1)[1]))
S_2 <- as.vector(sapply(
    start + S_1,
    function(x) which(cumsum(vector_2[x:length(vector_2)]) >= stage_2)[1]))
```

In the above example, `S_1` is the resulting vector from calculating how many days after each entry in `start` it takes to hit the number of GDD specified for `stage_1` using the daily average temperature values from `vector_1`. In this case, `S_1` is the number of days it takes for the seedlings to emerge after germinating. What is nice is that I can add `S_1` to `start` to calculate the index location where emergence occurs and also continue using a new growth stage specified using `stage_2`. I can also sum along different sources of temperature data, and in my case `vector_2` is a modified version of GDD which accounts for the delay induced by the genetic susceptibility of a given wheat cultivar to changes in photoperiod. This process continues for 7 stages of winter wheat development.



The master file is `init.R`. Locations in the form of "Town Location" are concatenated into a vector called `sites`. The `geocode` function from the `ggmap` package is used to query the appropriate coordinates and add them to the `locations` dataframe. The "Town Location" format is important as "Town" will be utilized when naming the subsequent arrays. Elevation data is queried next by locating the appropriate raster within the gridMET database using a netCDF file.

Data is requested from the Northwest Knowledge Network THREDDS server. The URL is constructed using combinations of the base URL named `url_1`, the list of global climate models named `model`, a specification for which model was used named `condition`, and a list of available climate variables named `variable_1`. While aggregated data is available, it is easier on the server to request the daily climate data in pieces of around ~5000 to ~7000 days at a time instead of the full ~20,000 to ~33,000 days for the historical and future datasets, respectively. Those are specified using either `timestep_historical`, `timestep_45`, or `timestep_85`.

Data is collected by running `netcdf_downloader.R`. The functions `open.nc` and `var.get.nc` from the `RNetCDF` package are used to collect data from the appropriate raster within the dataset for each location. Unfortunately, it takes forever to download and the `for` loop is not parallelized. I currently have data for the following locations:

|Idaho|Washington|Other states|
|:---:|:---:|:---:|
|Moscow|Pullman|Pendleton, OR|
|Genesee|Palouse|Havre, MT|
|Troy|Tekoa|Akron, CO|
|Deary|Endicott|Hutchinson, KS|
|Potlatch|Hay|Enid, OK|

After the arrays for each location have been collected, they can be saved as .rdata files by running `saving_arrays.R`. 

Data for Pullman, WA is in the `DATA` folder. Update line 8 of `init.R` to `sites <- c("Pullman Washington")` and run the appropriate code to set `N` to 1 and query latitude, longitude, and elevation data. The `locations` dataframe can also be used to load in previously downloaded arrays by running `loading_arrays.R`. When starting from saved arrays, be sure that `model` is in your environment as it gets called during subsequent processing while printing progress.

There are 4 user-defined inputs for the CERES-Wheat growth model: `planting_depth`, `planting_dates`, a genetic photoperiod coefficient `G_1`, and `phyllochron`. The planting depth is in inches. Currently, 3 planting dates are used to cover the reported historical range. They are by default the 1st of September, October, and November and represent early-, mid-, and late-season planting dates. They can be changed to any value, provided they are in the "-MM-DD" format. Wheat varieties have different growth characteristics due to photoperiod, which is defined using `G_1`. The range referenced in the original CERES-Wheat material is from 0.002 to 0.006. The current default is 0.0044 which is for "Nugaines" wheat. The other important variety-specific term is `phyllochron` which is the leaf appearance rate based on thermal time per leaf. When this value is not known the recommended default is 95. The [Southern Idaho Dryland Winter Wheat Production Guide](https://www.cals.uidaho.edu/edcomm/pdf/BUL/BUL0827.pdf) lists the following phyllochrons for 4 winter wheat varities:

- Stephens = 100
- Centurk = 126
- Scout = 122
- Newton = 113

Running `CERES_wheat_model.R` initializes the wheat growth model. It begins by calculating growing degree-days (GDD) for each location using daily maximum and minimum temperature. Photoperiod is then calculated for each location which is used along with `G_1` to calculate the daily relative development rate (RDR). A modified GDD variable is then created by multiplying GDD by RDR for each day. The index locations for all planting dates are then determined and saved as vectors. The amount of GDD for each developmental stage of wheat is calculated and then the `INDEXER` function locates the index where different stages start and finish. The `SPANNER` function is used to calculate duration of the wheat phenostages and is called when using `PLOT_STAGE`.The results are compiled into  4-d arrays.

After all the index locations have been determined, additional humidity related variables are calculated for each location by running `additional_weather_variables.R`. The current list of calculated variables are: relative humidity, vapor pressure deficit, humidex (or humisery), and potential evapotranspiration.

Special functions for plotting results are created when running `plotting.R`. There are currently 3 arguments: location `n`, stage `s`, and planting date `p`. Within the plotting functions are the scripts for subsetting and collecting climate data between consecutive phenological stages. `PLOT_RH(2,4,2)` will plot the average relative humidity for the 2nd town in the `locations` dataframe from the 4th to 5th stage of wheat development which is where flowering occurs using the 2nd planting date. Both `PLOT_DASHBOARD_1` and `PLOT_DASHBOARD_2` are used to make summary plots.

Summary figures for 6 locations across the continental Unided States are available in `plotting_examples`.Flowering for Pullman, WA generated using `PLOT_DASHBOARD_2` is shown below.

![](https://raw.githubusercontent.com/nosnibor27/WW_phenology_forecaster/master/plotting_examples/Pullman_flowering.png)

The planting date used was October 1st. The darker shaded region is the inter-quartile range (25% to 75%), the lighter region is the inter-decile range (10 to 90%) and the thick line is the median (50%) for the 20 model ensemble.


I can average climate conditions between the following phenological stages of winter wheat listed below. I included some supplemental information for each stage from Ritchie et al. (1991) and I also added the corresponding stage names from Feng. et al (2017) in parentheses if it helps. I also added the number of growing degree-days for each stage, assuming a phyllochron of 100 (instead of 95).


## Emergence to terminal spikelet (emergence)

* 420 GDD (based on phyllochron)
* This is the only stage that uses relative development rate based on the genetic photoperiod coefficient

## Terminal spikelet to end of ear growth (tillering)

* 300 GDD (3 phyllochrons)
* "considered to be strictly under temperature control"

## Preanthesis ear growth (booting)

* 200 GDD (2 phyllochrons)
* "ear develops very rapidly in this stage and is a major sink for assimilates"
* "probably the most important stage determining grain numbers per plant expected to develop into full size kernels"

## Preanthesis ear growth to grain filling (flowering)

* Fixed at 200 GDD
* "during this phase flowering takes place"
* could perhaps be labelled "maximum ear size and volume to the time when linear grain mass accumulation begins"
* this is the stage I've been using for Fusarium Head Blight risk

## Grain filling to physiological maturity (grain filling)

* Fixed at 500 GDD
* Variety specific, using inputs from Ritchie et al. (1991) can vary from 430 to 590 GDD (I could sample from a normal distribution as shown previously if needed)

## Physiological maturity to harvest (maturity)

* Fixed at 250 GDD
