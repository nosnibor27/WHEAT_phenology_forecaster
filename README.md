# WHEAT_phenology_forecaster

## Background

This repository is, at it's core, an applied indexing problem. Imagine you have an Excel spreadsheet with two columns: date and average daily temperature. The main goal within this repository to determine the number of days to hit a specified sum, given a specific starting date. This approach is analogous to having an income column (in $/day) for a savings account and trying to calculate how many days it will take to  accumulate enough money for a major purchase.

What I am doing is determining how many days it would take for average temperatures to sum past a given threshhold, and the units become growing degree-days (GDD) which is a measure of thermal time. Since a plant's time scale is so closely coupled with the thermal environment, it follows to think of thermal time as a plant's view of time. Accumulating thermal time in the form of total GDD allows for the determination of the dates when various plant growth stages are likely to occur.

The underlying code which makes all this possible was adapted from a [Stack Overflow thread](https://stackoverflow.com/questions/21248946/calculation-of-sum-of-numbers-to-reach-a-certain-point) and a snippet is presented below.
  
```r
S_1 <- as.vector(sapply(
    start,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_1)[1]))
S_2 <- as.vector(sapply(
    start + S_1,
    function(x) which(cumsum(vector_2[x:length(vector_2)]) >= stage_2)[1]))
```

In the above example, `S_1` is the resulting vector from calculating how many days after each planting date entry in `start` it takes to hit the number of GDD specified for `stage_1` using the daily average temperature values from `vector_1`. In this case, `S_1` is the number of days it takes for a wheat seedling to emerge after germinating. What is nice is that I can add `S_1` to `start` to calculate the index location where emergence occurs and then continue forward using a new growth stage specified using `stage_2`. 

I can also sum along different sources of temperature data, and in my case `vector_2` is a modified version of GDD which accounts for the delay induced by the genetic susceptibility of a given wheat cultivar to changes in photoperiod. This process continues for 7 stages, and I end up with an array of index locations for various wheat developmental stages. I can then use these index locations as bounds for subsetting other daily climate variables so that I can calculate things like average relative humidity during tillering or total rainfall during grain filling.

Following the income analogy above, the same scripts could be used to calculate the dates for how long it would take to save enough money for different consecutive major purchases, given any date to you wish to start contributing to a savings account. Multiple vectors could represent different revenue streams.

Why am I doing this? There is a disease of winter wheat called *Fusarium Head Blight* (FHB) which is favored by hot and humid conditions during flowering. Infected grains become contaminated with a fungal poison colloquially referred to as *vomitoxin* and growers suffer economic losses from being unable to sell grains above the regulatory limit of 1 part per million. 

In states like Kansas where the majority of wheat is grown most of the annual precipition occurs in the spring. This is beneficial because the wheat crop is receiving abundant moisture at a time when it is needing it the most during growth, but also creates the favorable hot and humid conditions for the development of FHB. 

In a state like Washington or Idaho, where I live, most of the rainfall occurs during the winter. The deep soils serve as a bank for moisture and a wheat crop makes withdrawals during growth in the spring. This also means that the climate is much too dry during flowering for the development of FHB, and it is exceedingly rare for grains grown in the Inland Pacific Northwest to be contaminated by vomitoxin.

However, the climate is forecasted to change and it is poorly understood whether or not the Inland Pacific Northwest will continue to produce high quality grains free of vomitoxin. Fortunately, there exists a wonderful dataset of 6 climate variables at daily time steps from 20 global climate models (GCM). Even better, a group of climatologists have used high resolution historical data to statistically downscale the GCM outputs from a pixel size of approximately 100 × 100 km (half the area of Wales) to 6 × 6 km (half the area of Manhattan) for [the entire continental United States](https://climate.northwestknowledge.net/IntegratedScenarios/gallery_data.php). This preserves the time scales and meterological patterns simulated by the GCMs and is at a spatial scale suitable for growers.

The inital scripts within this repository involve automating the data collection procedure for multiple point locations completely within R, so you don't have to [manually create your own CSV files](https://climate.northwestknowledge.net/IntegratedScenarios/data_csv.php) and stitch them together into an Excel spreadsheet as I did when I first started out collecting climate data. Daily data is available from 1950 to 2005 which represents a historical baseline and from 2006 to 2099 for 2 different climate change scenarios. The scenarios are either RCP 4.5 where humanity gets its act together around mid-century and the moderate warming stabilizes, or RCP 8.5 where the warming trend continues with business-as-usual emissions.

Most of the contention concerning climate change is not whether or not a change will occur, but rather the magnitude and direction of the change itself. Likewise, I don't suspect much contention in saying that wheat development is proportional to temperature but rather what values were used. 

A relatively simple approach for determining which GDD values to use involves copying data from university extension publications, but it is not clear where those values originated from. Also, values from [North Dakota](https://ndawn.ndsu.nodak.edu/help-wheat-growing-degree-days.html) could be different from [Montana](http://msuextension.org/publications/agandnaturalresources/mt200103ag.pdf) or [Arizona](https://cals.arizona.edu/crop/presentations/2012/ShawnaLoper.pdf). 

Which one should I use? I can't just ignore modelling wheat altogether because earlier recorded flowering dates over the last century is already an indicator that the climate is changing. Just summarizing climate by a particular month neglects the timing difference in developmental stages expected under increased temperatures. Using extension publications also doesn't address the cultivar differences which influence the relationship between temperature and development.

I ended up adapting a wheat growth model named CERES-Wheat to determine the number of GDD for various developmental stages of wheat. It is a process-based model and there is rich documentation in the form of a [book chapter](https://www.amazon.com/Modeling-Plant-Soil-Systems-AGRONOMY/dp/0891181067) which even ends with the original FORTRAN code. For FHB there are 2 relevant stages which get calculated: the point where preanthesis ear growth ends and the point where linear grain mass accumulation begins. Between these two points is where flowering occurs, and the 200 GDD window between these two stages is what I use for evaluating FHB risk. 

For a given point location I can calculate the average temperature or relative humidity during flowering for a specific planting date and compare the climate change values to the historical baseline. If relative humidity and temperature during flowering are forecasted to increase under climage change this may suggest increased FHB risk in the future. The nice part about leaving all the parameters out in the open is that we can look into how changes in the input parameters may mitigate FHB risk and better prepare growers. 

In trying to calculate a time window for flowering I ended up being able to calculate climate summaries for all other developmental stages as well. If you have a research question which could involve other developmental stages, or even domain knowledge about how to better interpret the results given different developmental stages, please let me know so we can expand the scope to reach a wider audience.

## Downloading daily climate data

The master file is `init.R`. Locations in the form of "Town Location" are concatenated into a vector called `sites`. The `geocode` function from the `ggmap` package is used to query the appropriate coordinates and add them to the `locations` dataframe. The "Town Location" format is important as "Town" will be utilized when naming the subsequent arrays. Elevation data is queried next by locating the appropriate raster within the [gridMET](http://www.climatologylab.org/gridmet.html) database using the netCDF file `metdata_elevationdata.nc`.

Data is requested from the [Northwest Knowledge Network THREDDS server](http://thredds.northwestknowledge.net:8080/thredds/catalog/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/catalog.html). The URL is constructed using combinations of the base URL named `url_1`, the list of global climate models named `model`, a specification for which model was used named `condition`, and a list of available climate variables named `variable_1`.

While aggregated data is available, it is easier on the server to request the daily climate data in pieces of around ~5000 to ~7000 days at a time instead of the full ~20,000 to ~33,000 days for the historical and future datasets, respectively. Those are specified using either `timestep_historical`, `timestep_45`, or `timestep_85`.

Data is collected by running `netcdf_downloader.R`. The functions `open.nc` and `var.get.nc` from the `RNetCDF` package are used to collect data from the appropriate raster within the dataset for each location. Unfortunately, it takes forever to download and the `for` loop is not multithreadeded. 

I am in the process of using the `foreach` package to parrallelize server requests which should hopefully speed up the time it takes to download the necessary climate data. However, I currently have data saved for the following locations which I can upload upon request:

|Idaho|Washington|Other states|
|:---:|:---:|:---:|
|Moscow|Pullman|Pendleton, OR|
|Genesee|Palouse|Havre, MT|
|Troy|Tekoa|Akron, CO|
|Deary|Endicott|Hutchinson, KS|
|Potlatch|Hay|Enid, OK|

After the arrays for each location have been collected, they can be saved as .rdata files by running `saving_arrays.R`. 

Data for Pullman, WA is in the `DATA` folder. Update line 8 of `init.R` to `sites <- c("Pullman Washington")` and run the appropriate code to set `N` to 1 and query latitude, longitude, and elevation data. 

The `locations` dataframe can also be used to load in previously downloaded arrays by running `loading_arrays.R`. When starting from saved arrays, be sure that `model` is in your environment as it gets called during subsequent processing while printing progress.

## The CERES-Wheat growth model

There are 4 user-defined inputs for the CERES-Wheat growth model: `planting_depth`, `planting_dates`, a genetic photoperiod coefficient `G_1`, and `phyllochron`. The `planting_depth` is in inches, with the current default being 2.

Currently, 3 planting dates are used to cover the [reported historical range](https://downloads.usda.library.cornell.edu/usda-esmis/files/vm40xr56k/dv13zw65p/w9505297d/planting-10-29-2010.pdf). They are by default the 1st of September, October, and November and represent early-, mid-, and late-season planting dates. They can be changed to any value, provided they are in the "-MM-DD" format. 

Wheat varieties have different growth characteristics due to photoperiod, which is defined using `G_1`. The range referenced in the original CERES-Wheat material is from 0.002 to 0.006. The current default is 0.004075 which is the average of the 12 varieties evaluated. 

The other important variety-specific term is the `phyllochron` which is the leaf appearance rate based on thermal time per leaf. When this value is not known the recommended default for CERES-Wheat is 95, and I set the default to 100 for simplicity which is close to the average of [24 winter wheat varieties](http://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=1082&context=usdaarsfacpub) compiled across various observational datasets. The [Southern Idaho Dryland Winter Wheat Production Guide](https://www.cals.uidaho.edu/edcomm/pdf/BUL/BUL0827.pdf) lists the following phyllochrons for 4 winter wheat varities:

- Stephens = 100
- Centurk = 126
- Scout = 122
- Newton = 113

The inputs can be set to whatever values you desire, and this is encouraged to better understand how model assumptions can influence the result. Generally, a deeper `planting_depth` will require more time for the cleoptile to expand enough to emerge past the surface. A higher value for `G_1` means that there will be a greater delay during tillering due to decreases in daylength over the winter months. A longer `phyllochron` will also mean more thermal time is needed to progress past all developmental stages prior to flowering. However, the one input which accounts for the most variability in the resulting climate conditions is `planting_dates`.

**Note:** The defaults I provide to the CERES-Wheat model are for winter wheat. If you would like to evaluate spring wheat, the recommended value for `phyllochron` is 75 and change the planting dates to something like `planting_dates <- c("-04-01","-05-01","-06-01")`.

Running `CERES_wheat_model.R` initializes the wheat growth model. It begins by calculating GDD for each location using daily maximum and minimum temperature. Photoperiod is then calculated for each location which is used along with `G_1` to calculate the daily relative development rate (RDR). A modified GDD variable is then created by multiplying GDD by RDR for each day. The index locations for all planting dates are then determined and saved as vectors. The amount of GDD for each developmental stage of wheat is calculated and then the `INDEXER` function locates the index where different stages start and finish. The `SPANNER` function is used to calculate duration of the wheat phenostages. The results are compiled into  4-d arrays.

After all the index locations have been determined, additional humidity related variables are calculated for each location by running `additional_weather_variables.R`. The current list of calculated variables are: relative humidity, vapor pressure deficit, humidex (or humisery), and potential evapotranspiration. I may be able to include more climate variables upon request. Plotting functions are available in `plotting.R`, but are in a constant state of development.

## Putting it all together

I have plotted below the results for Pullman, WA for every growth stage I can index between, except planting date to germination. The different stages and their bounds are summarized below

- **Emergence**: from seedling emergence to terminal spikelet initiation
- **Tillering**: from terminal spikelet initiation to end of leaf growth
- **Booting**: from end of leaf growth to maximum ear size and volume
- **Flowering**: from maximum ear size and volume to the beginning of linear grain mass accumulation
- **Grain fill**: from the beginning of linear grain mass accumulation to physiological maturity
- **Maturity**:  from physiological maturity to harvest

The weather variable I am averaging between developmental stages to create the plots below is vapor pressure deficit (VPD). VPD uses the same information as relative humidity, but calculates a difference between saturation and ambient vapor pressure instead of a ratio between the two. 

To give some biological relevance for the VPD values (in kPa) I am coloring various ranges according to [this image](http://www.just4growers.com/media/23631/vpd_2degree.gif). The different colors are summarized below

- ![](https://placehold.it/15/ffbfbf/000000?text=+) **0 - 0.4**; Danger zone (under transpiration)
- ![](https://placehold.it/15/bfbfff/000000?text=+) **0.4 - 0.8**; Low transpiration (propagation / early vegetative growth)
- ![](https://placehold.it/15/bfffbf/000000?text=+) **0.8 - 1.2**; Healthy transpiration (late vegetative growth / early flowering)
- ![](https://placehold.it/15/ffffbf/000000?text=+) **1.2 - 1.6**; High transpiration (mid / late flowering)
- ![](https://placehold.it/15/ffbfbf/000000?text=+) **> 1.6**; Danger zone (over transpiration)

I am also plotting 3 different planting dates. The different columns represent either a September 1st, October 1st, or November 1st planting date. Uncertainty across models is visualized by coloring the inter-decile range (10 to 90%) and the inter-quartile range (25 to 75%) of the 20 model ensemble in gray. The thick lines represent the 20 model median and are colored black, blue, and red for the historical simulation, RCP 4.5, and RCP 8.5, respectively.

A decreased VPD during flowering may be suggestive of increased FHB risk. Unfortunately, there are two different ways that VPD can decrease. Either the ambient vapor pressure increases while saturation vapor pressure remains constant, or the saturation vapor pressure decreases while the ambient vapor presure remains constant. The former would be suggestive of increased FHB risk: the temperature remains warm and the humidity increases. The latter is the case for Pullman: the temperature decreases since flowering is forecasted to occur earlier in the year. This is visualized in other functions with plot more of a dashboard summary, and the results for flowering and maturity for 6 locations across the continental United States is available in the `plotting_examples` folder of this repository.

It would appear that the climate conditions during flowering for the Inland Pacific Northwest are forecased to become cooler and wetter rather than warmer and wetter which would be suggestive of increased FHB risk. Deviations in VPD from the historical baseline are greater when using an earlier planting date, so growers may be able to preserve the status quo in the future by delaying their planting date to later in the year.

<p align="center">
  <img src="https://raw.githubusercontent.com/nosnibor27/WW_phenology_forecaster/master/plotting_examples/Pullman_VPD_all_stages_all_dates.png" alt="data dump"/>
</p>
