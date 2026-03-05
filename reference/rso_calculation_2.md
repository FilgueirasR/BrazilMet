# Clear-sky solar radiation when calibrated values are not available

Clear-sky solar radiation is calculated in this function for near sea
level or when calibrated values for as and bs are available.

## Usage

``` r
rso_calculation_2(z, ra)
```

## Arguments

- z:

  Station elevation above sea level (m)

- ra:

  Extraterrestrial radiation for daily periods (ra).

## Value

A data.frame object with the clear-sky solar radiation

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
rso_df <- rso_calculation_2(z, ra)
} # }
```
