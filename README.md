# WHEAT_phenology_forecaster

## Background

This repository is, at it's core, an applied indexing problem. Imagine you have an Excel spreadsheet with two columns: date and average daily temperature. The main goal within this repository to determine the number of days to hit a specified sum, given a specific starting date. This approach is analogous to having an income column (in $/day) and trying to forecast how many days it will take to clear rent or accumulate enough savings for a major purchase. 

What I am doing is determining how many days it would take for average temperatures to sum past a given threshhold, and the units become growing degree-days (GDD). The duration of growth for an agricultural crop is usually directly proportional to temperature, and using the sum of average daily temperatures to predict growth stages has been known since Reaumur in the 1700s. Since a plant's time scale is so closely coupled with the thermal environment, it follows to think of thermal time as a plant's view of time. Accumulating thermal time in the form of total GDD allows for the determination of the dates when various plant growth stages are likely to occur.

The underlying code which makes all this possible was adapted from [here](https://stackoverflow.com/questions/21248946/calculation-of-sum-of-numbers-to-reach-a-certain-point) and a snippet is presented below.
  
```r
S_1 <- as.vector(sapply(
    start,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_1)[1]))
S_2 <- as.vector(sapply(
    start + S_1,
    function(x) which(cumsum(vector_2[x:length(vector_2)]) >= stage_2)[1]))
```

In the above example, `S_1` is the resulting vector from calculating how many days after each entry in `start` it takes to hit the number of GDD specified for `stage_1` using the daily average temperature values from `vector_1`. In this case, `S_1` is the number of days it takes for the seedlings to emerge after germinating. What is nice is that I can add `S_1` to `start` to calculate the index location where emergence occurs and continue using a new growth stage specified using `stage_2`. I can also sum along different sources of temperature data, and in my case `vector_2` is a modified version of GDD which accounts for the delay induced by the genetic susceptibility of a given wheat cultivar to changes in photoperiod. 

This process continues for 7 stages of winter wheat development, and I end up with an array of index locations for various wheat development stages. I can then use these index locations as bounds for subsetting other daily climate variables so that I can calculate things like average relative humidity during tillering or total rainfall during grain filling.

Why am I doing this? There is a disease of winter wheat called *Fusarium Head Blight* (FHB) which is favored by hot and humid conditions during flowering. Infected grains are contaminated with a fungal poison colloquially referred to as *vomitoxin* and growers suffer economic losses from being unable to sell grains which have been contaminated above the regulatory limit of 1 part per million. 

In states like Kansas where the majority of winter wheat is grown most of the annual precipition occurs in the spring. This is wonderful because the crop is receiving moisture at a time when it is needing it the most during development, but also creates the favorable hot and humid conditions for the development of FHB. In a state like Washington or Idaho, where I live, most of the rainfall occurs during the winter. The deep soils serve as a bank for moisture and winter wheat makes withdrawals during the spring. This also means that the climate is much too dry during flowering for the development of FHB, and it is exceedingly rare for grains grown in the Inland Pacific Northwest to be contaminated by vomitoxin.

However, the climate is forecasted to change and it is poorly understood wether or not the Inland Pacific Northwest will continue to produce high quality grains free from vomitoxin. Fortunately, there exists a wonderful dataset of 6 climate variables at daily time steps from 20 global climate models (GCM). Even better, a group of climatologists have used high resolution historical data to statistically downscale the GCM outputs with a pixel size of approximately 100 × 100 km (half the area of Wales) to 6 × 6 km (half the area of Manhattan) for [the entire continental United States](https://climate.northwestknowledge.net/IntegratedScenarios/gallery_data.php). 

The inital scripts within this repository involve gathering the data for a given point location completely within R. Daily data is available from 1950 to 2005 which represents a historical baseline and from 2006 to 2099 for 2 different climate change scenarios. The scenarios are either RCP 4.5 where humanity gets its act together around mid-century and the moderate warming stabilizes, or RCP 8.5 where the warming trend continues with business-as-usual emissions.

Most of the contention concerning climate change is not whether or not a change will occur, but the magnitude and direction of the change itself. Likewise, there wouldn't be contention in saying that wheat development is related to temperature but rather what proportion was used. 

A relatively simple approach involves copying values from university extension publications, but it is not clear where those values originated from. Also, values from [North Dakota](https://ndawn.ndsu.nodak.edu/help-wheat-growing-degree-days.html) could be different from [Montana](http://msuextension.org/publications/agandnaturalresources/mt200103ag.pdf) or [Arizona](https://cals.arizona.edu/crop/presentations/2012/ShawnaLoper.pdf). 

Which one should I use? I can't just ignore modelling wheat altogether because earlier recorded flowering dates over the last century is already an indicator that the climate is changing. Just summarizing climate by a particular month neglects the timing difference expected under increased temperatures. Using extension publications also doesn't address the cultivar differences which influence the relationship between temperature and development.

I ended up adapting a wheat growth model named CERES-Wheat to determine the number of GDD for various developmental stages of wheat. It is a process-based model and there is rich documentation in the form of a [book chapter](https://www.amazon.com/Modeling-Plant-Soil-Systems-AGRONOMY/dp/0891181067) which even ends with the original FORTRAN code. There are 2 relevant stages which get calculated: the point where preanthesis ear growth ends and the point where linear grain mass accumulation begins. Between these two points is where flowering occurs, and the 200 GDD window between these two stages is what I use for evaluating FHB risk. For a given point location I can calculate the average temperature or relative humidity during flowering for a specific planting date and compare the climate change values to the historical baseline. If relative humidity and temperature during flowering are forecasted to increase under climage change this may suggest increased FHB risk in the future. The nice part about leaving all the parameters out in the open is that we can look into how changes in planting date may mitigate FHB risk to better prepare growers.

## Methodology

The master file is `init.R`. Locations in the form of "Town Location" are concatenated into a vector called `sites`. The `geocode` function from the `ggmap` package is used to query the appropriate coordinates and add them to the `locations` dataframe. The "Town Location" format is important as "Town" will be utilized when naming the subsequent arrays. Elevation data is queried next by locating the appropriate raster within the [gridMET](http://www.climatologylab.org/gridmet.html) database using a netCDF file.

Data is requested from the [Northwest Knowledge Network THREDDS server](http://thredds.northwestknowledge.net:8080/thredds/catalog/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/catalog.html). The URL is constructed using combinations of the base URL named `url_1`, the list of global climate models named `model`, a specification for which model was used named `condition`, and a list of available climate variables named `variable_1`. While aggregated data is available, it is easier on the server to request the daily climate data in pieces of around ~5000 to ~7000 days at a time instead of the full ~20,000 to ~33,000 days for the historical and future datasets, respectively. Those are specified using either `timestep_historical`, `timestep_45`, or `timestep_85`.

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

There are 4 user-defined inputs for the CERES-Wheat growth model: `planting_depth`, `planting_dates`, a genetic photoperiod coefficient `G_1`, and `phyllochron`. The `planting_depth` is in inches. I am currently using a default of 2.

Currently, 3 planting dates are used to cover the [reported historical range](https://swat.tamu.edu/media/90113/crops-typicalplanting-harvestingdates-by-states.pdf). They are by default the 1st of September, October, and November and represent early-, mid-, and late-season planting dates. They can be changed to any value, provided they are in the "-MM-DD" format. 

Wheat varieties have different growth characteristics due to photoperiod, which is defined using `G_1`. The range referenced in the original CERES-Wheat material is from 0.002 to 0.006. The current default is 0.004075 which is the average of the 12 varieties evaluated. 

The other important variety-specific term is `phyllochron` which is the leaf appearance rate based on thermal time per leaf. When this value is not known the recommended default for CERES-Wheat is 95. I set the default to 100 for simplicity. The [Southern Idaho Dryland Winter Wheat Production Guide](https://www.cals.uidaho.edu/edcomm/pdf/BUL/BUL0827.pdf) lists the following phyllochrons for 4 winter wheat varities:

- Stephens = 100
- Centurk = 126
- Scout = 122
- Newton = 113

The inputs can be set to whatever values you desire, and this is encouraged to better understand how model assumptions can influence the result. Generally, a deeper `planting_depth` will require more time for the cleoptile to expand enough to emerge past the surface. A higher value for `G_1` means that there will be a greater delay during tillering due to decreases in daylength. A longer `phyllochron` will also mean more thermal time is needed to progress past certain developmental stages. However, the one input which accounts for the most variability in the resulting climate conditions is `planting_dates`.

Running `CERES_wheat_model.R` initializes the wheat growth model. It begins by calculating GDD for each location using daily maximum and minimum temperature. Photoperiod is then calculated for each location which is used along with `G_1` to calculate the daily relative development rate (RDR). A modified GDD variable is then created by multiplying GDD by RDR for each day. The index locations for all planting dates are then determined and saved as vectors. The amount of GDD for each developmental stage of wheat is calculated and then the `INDEXER` function locates the index where different stages start and finish. The `SPANNER` function is used to calculate duration of the wheat phenostages. The results are compiled into  4-d arrays.

After all the index locations have been determined, additional humidity related variables are calculated for each location by running `additional_weather_variables.R`. The current list of calculated variables are: relative humidity, vapor pressure deficit, humidex (or humisery), and potential evapotranspiration.

## Putting it all together

- ![#f03c15](https://placehold.it/15/f03c15/000000?text=+) test
- ![#c5f015](https://placehold.it/15/c5f015/000000?text=+) `#c5f015`
- ![#1589F0](https://placehold.it/15/1589F0/000000?text=+) `#1589F0`

<p align="center">
  <img src="https://raw.githubusercontent.com/nosnibor27/WW_phenology_forecaster/master/plotting_examples/Pullman_VPD_all_stages_all_dates.png" alt="data dump"/>
</p>

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
