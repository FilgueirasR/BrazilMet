# Relative humidity (rh) calculation

Relative humidity is calculated in this function based on minimum air
temperature of the day and the air temperature of the moment.

## Usage

``` r
rh_calculation(tmin, tmean)
```

## Arguments

- tmin:

  A dataframe with minimum daily air temperature (Celsius)

- tmean:

  A dataframe with mean air temperature (Celsius) that you want to
  calculate the relative humidity.

## Value

A data.frame object with the relative humidity calculated

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
rh <- rh_calculation(tmin, tmean)
} # }
```
