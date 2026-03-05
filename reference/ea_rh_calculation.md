# Actual vapour pressure (ea) derived from relative humidity data

Actual vapour pressure (ea) derived from relative humidity data

## Usage

``` r
ea_rh_calculation(tmin, tmax, rh_min, rh_mean, rh_max)
```

## Arguments

- tmin:

  A dataframe with minimum daily air temperature (Celsius)

- tmax:

  A dataframe with maximum daily air temperature (Celsius)

- rh_min:

  A dataframe with minimum daily relative air humidity (percentage).

- rh_mean:

  A dataframe with mean daily relative air humidity (percentage).

- rh_max:

  A dataframe with maximum daily relative air humidity (percentage).

## Value

Returns a data.frame object with the with ea from relative humidity
data.

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
ea <- ea_rh_calculation(tmin, tmax, rh_min, rh_mean, rh_max)
} # }
```
