# Solar radiation data derived from air temperature differences

If global radiation is not measure at station, it can be estimated with
this function.

## Usage

``` r
sr_tair_calculation(latitude, date, tmax, tmin, location_krs)
```

## Arguments

- latitude:

  A dataframe with latitude in decimal degrees that you want to
  calculate the ra.

- date:

  A dataframe with the dates that you want to calculate the ra.

- tmax:

  A dataframe with Maximum daily air temperature (Celsius)

- tmin:

  A dataframe with Minimum daily air temperature (Celsius)

- location_krs:

  Adjustment coefficient based in location. Please decide between
  "coastal or "interior". If coastal the krs will be 0.19, if interior
  the krs will be 0.16.

## Value

A data.frame object with solar radiation data

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
sr_tair <- sr_tair_calculation(latitude, date, tmax, tmin, location_krs)
} # }
```
