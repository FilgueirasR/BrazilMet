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

Let’s download daily meteorological data for one station between January
2000 and March 2025 (the station A001 started operating in May 2000):

``` r

df <- download_AWS_INMET_daily(
  stations   = "A001",
  start_date = "2000-01-01",
  end_date   = "2025-03-31"
)
```

The resulting data frame includes temperature, solar radiation, wind
speed, humidity, and atmospheric pressure.

To keep this article reproducible without depending on the INMET server,
the data downloaded with the call above are bundled with the package and
loaded here:

``` r

df <- readRDS(system.file("extdata", "A001_daily_2000_2025.rds", package = "BrazilMet"))
df$date <- as.Date(df$date)
```

## 🧩 Fill gaps in the weather data

A long series has many sensor failures, and any missing input makes ETo
`NA` on that day.
[`fill_gaps()`](https://filgueirasr.github.io/BrazilMet/reference/fill_gaps.md)
fills short gaps (up to three days) by linear interpolation and the
remaining ones with the mean of the same day of the year in the other
years. Every filled value is flagged in a `*_filled` column:

``` r

df <- fill_gaps(df, method = "both", max_gap = 3)
#> Filled values (remaining NA): tair_mean_c = 559 (0), tair_min_c = 560 (0), tair_max_c = 560 (0), rh_max_porc = 600 (0), rh_min_porc = 600 (0), ws_2_m_s = 680 (0), patm_mb = 597 (0), sr_mj_m2 = 696 (0)
```

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
```

## 💧 Design ETo calculation

And after the ETo calculation, we use the design_eto() function to
estimate the design ETo for irrigation project purpose:

``` r

# Ensure date column is in Date format
df$date <- as.Date(df$date)

eto_design <- BrazilMet::design_eto(eto_daily_data = df, percentile = .80)
```

## 📝 Printing the design ETo based on an 80% probability of occurrence

Below is a basic line plot of daily ETo:

``` r


print(eto_design)
#>   design_eto
#> 1   5.624202
```

## ✅ Summary

The BrazilMet package allows you to download official INMET weather data
and compute ETo using the FAO-56 method in a reproducible and efficient
way. This is essential for irrigation planning, crop modeling, and
climate-based decision support.

## 🔗 Useful links

<https://github.com/FilgueirasR/BrazilMet>
