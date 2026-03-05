# Vapour pressure deficit (es - ea)

Vapour pressure deficit (es - ea)

## Usage

``` r
es_ea_calculation(tmin, tmax, tdew, rh_min, rh_mean, rh_max, ea_method)
```

## Arguments

- tmin:

  A dataframe with minimum daily air temperature (Celsius).

- tmax:

  A dataframe with maximum daily air temperature (Celsius).

- tdew:

  A dataframe with dewpoint temperature (Celsius).

- rh_min:

  A dataframe with minimum daily relative air humidity (percentage).

- rh_mean:

  A dataframe with mean daily relative air humidity (percentage).

- rh_max:

  A dataframe with maximum daily relative air humidity (percentage).

- ea_method:

  The methodology to calculate the actual vapour pressure. Assume the
  "rh" (default) for relative humidity procedure and "dew" for dewpoint
  temperature procedure.

## Value

Returns a data.frame object with the ea from relative humidity data.

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
ea <- es_ea_calculation(tmin, tmax, tdew, rh_min, rh_mean, rh_max, ea_method)
} # }
```
