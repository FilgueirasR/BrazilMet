# Solar radiation data from a nearby weather station

The solar radiation data is calculated based in a nearby weather
station.

## Usage

``` r
rs_nearby_calculation(rs_reg, ra_reg, ra)
```

## Arguments

- rs_reg:

  A dataframe with the solar radiation at the regional location (MJ m-2
  day-1).

- ra_reg:

  A dataframe with the extraterrestrial radiation at the regional
  location (MJ m-2 day-1).

- ra:

  A dataframe with the extraterrestrial radiation for daily periods
  (ra).

## Value

A data.frame object with the Solar radiation data based on a nearby
weather station

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
rs_nearby_df <- rs_nearby_calculation(rs_reg, ra_reg, ra)
} # }
```
