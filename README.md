# BrazilMet

<div class="fluid-row" id="header">
    <img src='man/figures/logo_BrazilMet.png' height='150' width='auto' align='right'>


<!-- badges: start -->
<!-- badges: end -->

The BrazilMet package aims to facilitate the acquisition and processing of meteorological data from INMET automatic stations.


## Installation

To install the last version of BrazilMet package follow this steps:

``` r
devtools::install_github("FilgueirasR/BrazilMet")

```

## Example

This is a basic example which shows you how to run the cropDemand package:

``` r
## basic example code

#devtools::install_github("FilgueirasR/BrazilMet")
library(BrazilMet)

see_stations_info()

df<-download_AWS_INMET_daily(station = "A001", start_date = "2001-01-01", end_date = "2001-12-31")

df$eto <- daily_eto_FAO56(lat = df$`Latitude (degrees)`,
                       tmin = df$`Tair_min (c)`,
                       tmax = df$`Tair_max (c)`,
                       tmean = df$`Tair_mean (c)`,
                       Rs = df$`Sr (Mj m-2 day-1)`,
                       u2 = df$`Ws_2 (m s-1)`,
                       Patm = df$`Patm (mB)`,
                       RH_max = df$`Rh_max (porc)`,
                       RH_min = df$`Rh_min (porc)`,
                       z = df$`Altitude (m)`,
                       date = df$Date)

```
