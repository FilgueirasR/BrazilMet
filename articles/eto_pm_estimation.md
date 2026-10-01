# 💧 ETo Calculation Based on FAO-56 Penman-Monteith

## 🚀 Reference Evapotranspiration (ETo) Estimation

This article demonstrates how to use the BrazilMet package to compute
reference evapotranspiration (ETo) based on the FAO-56 Penman-Monteith
method, using weather data from INMET automatic stations.

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

Let’s download daily meteorological data for station A001 between
January 2023 and December 2024:

``` r

df <- download_AWS_INMET_daily(
  stations   = c("A001"),
  start_date = "2023-01-01",
  end_date   = "2024-12-31"
)
```

The resulting data frame includes temperature, solar radiation, wind
speed, humidity, and atmospheric pressure.

To keep this article reproducible without depending on the INMET server,
the data downloaded with the call above are bundled with the package and
loaded here:

``` r

df <- readRDS(system.file("extdata", "A001_daily_2023_2024.rds", package = "BrazilMet"))
```

## 🧠 Calculate daily ETo using FAO-56

Station data have occasional sensor failures, and any missing input
makes ETo `NA` on that day.
[`fill_gaps()`](https://filgueirasr.github.io/BrazilMet/reference/fill_gaps.md)
fills gaps of up to three days by linear interpolation and flags the
filled values in `*_filled` columns:

``` r

df <- fill_gaps(df, max_gap = 3)
#> Filled values (remaining NA): tair_mean_c = 29 (0), tair_min_c = 29 (0), tair_max_c = 29 (0), rh_max_porc = 30 (0), rh_min_porc = 30 (0), ws_2_m_s = 33 (0), patm_mb = 28 (0), sr_mj_m2 = 7 (0)
```

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

## 📊 Plotting ETo results

Below is a basic line plot of daily ETo:

``` r


library(ggplot2)

# Ensure date column is in Date format
df$date <- as.Date(df$date)

ggplot(df, aes(x = date, y = eto)) +
  geom_line(color = "darkblue", linewidth = 1) +
  labs(
    title = "Reference Evapotranspiration (FAO-56)",
    x = "Date",
    y = "ETo (mm/day)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )
```

![](eto_pm_estimation_files/figure-html/plot-eto-ggplot-1.png)

## ✅ Summary

The BrazilMet package allows you to download official INMET weather data
and compute ETo using the FAO-56 method in a reproducible and efficient
way. This is essential for irrigation planning, crop modeling, and
climate-based decision support.

## 🔗 Useful links

<https://github.com/FilgueirasR/BrazilMet>
