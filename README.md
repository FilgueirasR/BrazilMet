![CRAN Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/BrazilMet)
![CRAN Download Badge](https://cranlogs.r-pkg.org/badges/BrazilMet)

# BrazilMet

<div class="fluid-row" id="header">
    <img src='docs/figures/logo_BrazilMet.png' height='150' width='auto' align='right'>

<!-- badges: start -->
[![R-CMD-check](https://github.com/FilgueirasR/BrazilMet/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FilgueirasR/BrazilMet/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview  

**BrazilMet** is an R package designed to facilitate the acquisition and processing of meteorological data from INMET stations. It includes functions for downloading, calculating atmospheric parameters, estimating evapotranspiration, and more.

---

## 🌍 Features  

### 📥 Downloading Data  
- Automatic weather station data from INMET (daily aggregation).  
- Automatic weather station data from INMET (hourly intervals).  
- Climatological normals from conventional INMET weather stations.  

### 🌤️ Atmospheric Parameter Calculations  
- Atmospheric pressure calculation based on altitude.  
- Psychrometric constant calculation based on atmospheric pressure.  

### 💧 Evapotranspiration Estimation  
- Reference evapotranspiration (ETo) via Hargreaves-Samani.  
- Reference evapotranspiration (ETo) via Penman-Monteith FAO-56.  
- Potential evapotranspiration (ETp) via Thornthwaite.
- Correction for sunlight hours and the number of days in the month for Thornthwaite ETp.
- Design reference evapotranspiration.

### ☀️ Radiation Parameter Estimation  
- Extraterrestrial radiation for daily periods.  
- Solar radiation estimation (Angstrom formula & temperature-based methods).  
- Clear-sky solar radiation with/without calibrated values.  
- Net solar (shortwave) radiation, net longwave radiation, and net radiation.  

### 💨 Air Humidity & Wind Speed Parameters  
- Mean saturation vapor pressure.  
- Actual vapor pressure derived from dew point or relative humidity.  
- Vapor pressure deficit and relative humidity calculations.  
- Wind speed at 2 meters above ground level.  

### 📍 Station Selection & Information  
- Automatic weather station (AWS) metadata.  
- Selection of AWS stations using an `sf` object.  

---

## ⚡ Installation  

You can install the latest version of **BrazilMet** from GitHub:  

```r
# Install devtools if not already installed
install.packages("devtools")

# Install BrazilMet
devtools::install_github("FilgueirasR/BrazilMet")

```
#  🚀 Usage Example - ETo estimation

Here’s a quick example of how to download INMET station data and estimate reference evapotranspiration (ETo) using FAO-56 for multiple stations and years:

```r
# Load the package
library(BrazilMet)

# View available station information
see_stations_info()

# Download daily weather data for two stations
df <- download_AWS_INMET_daily(stations = c("A001", "A042"),
                               start_date = "2023-01-01",
                               end_date = "2024-12-31")

# Calculate daily ETo using the FAO-56 method
df$eto <- daily_eto_FAO56(lat = df$latitude_degrees,
                          tmin = df$tair_min_c,
                          tmax = df$tair_max_c,
                          tmean = df$tair_mean_c,
                          Rs = df$sr_mj_m2,
                          u2 = df$ws_2_m_s,
                          Patm = df$patm_mb,
                          RH_max = df$rh_max_porc,
                          RH_min = df$rh_min_porc,
                          z = df$altitude_m,
                          date = df$date)

```
#  🚀 Usage Example design ETo calculation

Here’s a quick example of how to download INMET station data and estimate reference evapotranspiration (ETo) using FAO-56, followed by the calculation of the design ETo.

```r

library(BrazilMet)
stations <- BrazilMet::see_stations_info()

df <- BrazilMet::download_AWS_INMET_daily(stations = "A001", start_date = "2000-01-01", end_date = "2025-03-31")

df$eto <- daily_eto_FAO56(lat = df$latitude_degrees,
                          tmin = df$tair_min_c,
                          tmax = df$tair_max_c,
                          tmean = df$tair_mean_c,
                          Rs = df$sr_mj_m2,
                          u2 = df$ws_2_m_s,
                          Patm = df$patm_mb,
                          RH_max = df$rh_max_porc,
                          RH_min = df$rh_min_porc,
                          z = df$altitude_m,
                          date = df$date)

BrazilMet::design_eto(eto_daily_data = df, percentile = .80)

```
🤝 Contributing

We welcome contributions from the community! Whether it’s reporting issues, suggesting improvements, or submitting pull requests, your help is greatly appreciated.

How to contribute:
Fork the repository

Create a new branch (git checkout -b feature-branch)

Make your changes and commit (git commit -m "Description of changes")

Push to your branch (git push origin feature-branch)

Open a pull request

Let’s make BrazilMet even better together! 🚀
