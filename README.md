ETT Water Quality Stations and Samples
======================================

Jeffrey D Walker, PhD  
[Walker Environmental Research LLC](https://walkerenvres.com)  
<jeff@walkerenvres.com>

Prepared for: [Massachusetts Bays National Estuary Partnership (MassBays)](https://www.mass.gov/orgs/massachusetts-bays-national-estuary-partnership)

## Overview

This repo contains R code for fetching, processing, and exporting water quality stations and samples from the [U.S. Water Quality Portal (WQP)](https://www.waterqualitydata.us/) for use in the Ecohealth Tracking Tool (ETT).

This code was developed using R v4.1.3 and the specific versions of each package as listed in the `sessionInfo.txt` file. This code should continue to work in future versions of R and as various dependencies are updated to newer versions.

The most likely future incompatibility will come from the [`dataRetrieval`](https://github.com/USGS-R/dataRetrieval) package, which is used to fetch the stations and samples from the WQP. Problems may occur if the USGS and EPA change the WQP API specification or returned data format. In these cases, portions of the code will need to be updated to utilize the new API or data formats.  

## Instructions

### 1. Install R and RStudio

Download and install the latest version of R for your operating system from [cran](https://cran.r-project.org/).

Download and install [RStudio IDE](https://www.rstudio.com/products/rstudio/download/) (recommended, but no required).

### 2. Download R Code

Download the R code in this repo from the github repository: https://github.com/walkerjeffd/massbays-ett-wq

Click the green Code button, then Download Zip.

![Download Code](https://user-images.githubusercontent.com/981144/173918107-2f16630e-f3ec-44ab-87ad-f2a6e64ec100.png)

Decompress the zip file, and then open the project file (`ett.Rproj`) in RStudio.

### 3. Install Packages

Before running any code, install the necessary packages. You only need to run this once.

```r
install.packages(c("tidyverse", "janitor", "logger", "units", "sf", "dataRetrieval"))
```

### 4. Run R Code

Open the `main.R` script in RStudio, and run it line-by-line. Comments are provided to explain what each line does.

### 5. Copy Output Files to Web Server

After running the R code, the exported files needed for the ETT web application can be found in `out/ett`.

A [GeoJSON](https://geojson.org/) file containing the WQ stations will be saved to `out/ett/stations.json`. This file can be converted to a shapefile using various on-line tools such as the [MyGeodata Converter](https://mygeodata.cloud/converter/geojson-to-shp).

The water quality data are saved to files within the `out/ett/wq` folder. Each file contains the sample results for all ETT parameters at a single station. The filename contains the station ID (e.g., `out/ett/wq/11NPSWRD_WQX-CACO_BEECH_FOR.json` contains all water quality data for station `11NPSWRD_WQX-CACO_BEECH_FOR`).

To update the ETT web application, copy the output files from `out/ett` to the `data/` folder on the web server.

## License

See `LICENSE` file.

