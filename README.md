ETT Water Quality Stations and Samples
======================================

Jeffrey D Walker, PhD (@walkerjeffd)  
Walker Environmental Research LLC

prepared for Massachusetts Bays National Estuary Partnership (MassBays)

## Overview

This repo contains R code for fetching, processing, and exporting water quality stations and samples from the [U.S. Water Quality Portal](https://www.waterqualitydata.us/) for use in the Ecohealth Tracking Tool (ETT).

## Instructions

### Install R and RStudio

Download and install the latest version of R for your operating system from [cran](https://cran.r-project.org/).

Download and install [RStudio IDE](https://www.rstudio.com/products/rstudio/download/) (recommended, but no required).

### Install Packages

First, install the necessary packages in your R environment ( is recommended). You only need to run this once.

```r
install.packages(c("tidyverse", "janitor", "logger", "units", "sf", "dataRetrieval"))
```

