# Extraterrestrial radiation for daily periods (ra)

ra is expressed in MJ m-2 day-1

## Usage

``` r
ra_calculation(latitude, date)
```

## Arguments

- latitude:

  A dataframe with latitude in decimal degrees that you want to
  calculate the ra.

- date:

  A dataframe with the dates that you want to calculate the ra.

## Value

A data.frame with the extraterrestrial radiation for daily periods

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
ra <- ra_calculation(latitude, date)
} # }
```
