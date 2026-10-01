# 🌱💧 Design ETo calculation

## 🚀 Design ETo calculation estimation

This article shows a quick example of how to download INMET station data
and estimate reference evapotranspiration (ETo) using FAO-56, followed
by the calculation of the design ETo.

## 📦 Load the package

``` r

library(BrazilMet)
```

## 🌍 View available INMET stations

Before downloading data, you can check the available weather stations
with:

``` r

see_stations_info()
#> # A tibble: 564 × 8
#>    station_municipality uf    situation_operation latitude_degrees
#>    <chr>                <chr> <chr>                          <dbl>
#>  1 Abrolhos             BA    breakdown                     -18.0 
#>  2 Acarau               CE    breakdown                      -3.12
#>  3 Afonso Claudio       ES    operating                     -20.1 
#>  4 Agua Boa             MT    operating                     -14.0 
#>  5 Agua Clara           MS    operating                     -20.4 
#>  6 Aguas Emendadas      DF    operating                     -15.6 
#>  7 Aguas Vermelhas      MG    operating                     -15.8 
#>  8 Aimores              MG    operating                     -19.5 
#>  9 Alegre               ES    operating                     -20.8 
#> 10 Alegrete             RS    operating                     -29.7 
#> # ℹ 554 more rows
#> # ℹ 4 more variables: longitude_degrees <dbl>, altitude_m <dbl>,
#> #   operation_start_date <dttm>, station_code <chr>
```

## ⬇️ Download daily weather data

Let’s download daily meteorological data for one stations between
January 2000 until March 2025:

``` r

df <- BrazilMet::download_AWS_INMET_daily(stations = "A001",
                                          start_date = "2000-01-01",
                                          end_date = "2025-03-31")
#> Downloading data for: 2000
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2000.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2000: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2000.zip'
#> Downloading data for: 2001
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2001.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2001: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2001.zip'
#> Downloading data for: 2002
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2002.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2002: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2002.zip'
#> Downloading data for: 2003
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2003.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2003: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2003.zip'
#> Downloading data for: 2004
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2004.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2004: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2004.zip'
#> Downloading data for: 2005
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2005.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2005: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2005.zip'
#> Downloading data for: 2006
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2006.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2006: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2006.zip'
#> Downloading data for: 2007
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2007.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2007: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2007.zip'
#> Downloading data for: 2008
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2008.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2008: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2008.zip'
#> Downloading data for: 2009
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2009.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2009: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2009.zip'
#> Downloading data for: 2010
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2010.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2010: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2010.zip'
#> Downloading data for: 2011
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2011.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2011: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2011.zip'
#> Downloading data for: 2012
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2012.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2012: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2012.zip'
#> Downloading data for: 2013
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2013.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2013: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2013.zip'
#> Downloading data for: 2014
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2014.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2014: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2014.zip'
#> Downloading data for: 2015
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2015.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2015: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2015.zip'
#> Downloading data for: 2016
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2016.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2016: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2016.zip'
#> Downloading data for: 2017
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2017.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2017: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2017.zip'
#> Downloading data for: 2018
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2018.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2018: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2018.zip'
#> Downloading data for: 2019
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2019.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2019: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2019.zip'
#> Downloading data for: 2020
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2020.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2020: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2020.zip'
#> Downloading data for: 2021
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2021.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2021: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2021.zip'
#> Downloading data for: 2022
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2022.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2022: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2022.zip'
#> Downloading data for: 2023
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2023.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2023: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2023.zip'
#> Downloading data for: 2024
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2024.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2024: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2024.zip'
#> Downloading data for: 2025
#> Warning in utils::download.file(url =
#> paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", : URL
#> 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2025.zip': Timeout of 600
#> seconds was reached
#> Warning in value[[3L]](cond): Failed to download data for year 2025: cannot
#> open URL 'https://portal.inmet.gov.br/uploads/dadoshistoricos/2025.zip'
#> Warning in BrazilMet::download_AWS_INMET_daily(stations = "A001", start_date =
#> "2000-01-01", : No data was downloaded for the specified stations and period.
```

The resulting data frame includes temperature, solar radiation, wind
speed, humidity, and atmospheric pressure

## 🧠 Calculate daily ETo using FAO-56

Now we use the daily_eto_FAO56() function to estimate daily ETo values:

``` r

df$eto <- daily_eto_FAO56(
  lat    = df$latitude_degrees,
  tmin   = df$tair_min_c,
  tmax   = df$tair_max_c,
  tmean  = df$tair_mean_c,
  Rs     = df$sr_mj_m2,
  u2     = df$ws_2_m_s,
  Patm   = df$patm_mb,
  RH_max = df$rh_max_porc,
  RH_min = df$rh_min_porc,
  z      = df$altitude_m,
  date   = df$date
)
#> Warning in daily_eto_FAO56(lat = df$latitude_degrees, tmin = df$tair_min_c, :
#> NAs introduced by coercion
```

## 💧 Design ETo calculation

And after the ETo calculation, we use the design_eto() function to
estimate the design ETo for irrigation project purpose:

``` r

# Ensure date column is in Date format
df$date <- as.Date(df$date)

eto_design <- BrazilMet::design_eto(eto_daily_data = df, percentile = .80)
#> Warning in BrazilMet::design_eto(eto_daily_data = df, percentile = 0.8): It is
#> recommended to use at least 10 years of data to calculate the design reference
#> evapotranspiration.
```

## 📝 Printing the design ETo based on an 80% probability of occurrence

Below is a basic line plot of daily ETo:

``` r


print(eto_design)
#>   design_eto
#> 1         NA
```

## ✅ Summary

The BrazilMet package allows you to download official INMET weather data
and compute ETo using the FAO-56 method in a reproducible and efficient
way. This is essential for irrigation planning, crop modeling, and
climate-based decision support.

## 🔗 Useful links

<https://github.com/FilgueirasR/BrazilMet>
