# Clear-sky solar radiation with calibrated values available

Clear-sky solar radiation is calculated in this function for near sea
level or when calibrated values for as and bs are available.

## Usage

``` r
rso_calculation_1(as, bs, ra)
```

## Arguments

- as:

  A dataframe with latitude in decimal degrees that you want to
  calculate the ra. The values of as = 0.25 is recommended by Allen et
  al. (1998).

- bs:

  A dataframe with the dates that you want to calculate the ra. The
  values of bs = 0.50 is recommended by Allen et al. (1998).

- ra:

  Extraterrestrial radiation for daily periods (ra).

## Value

A data.frame object with the clear-sky radiation data

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
rso_df <- rso_calculation_1(as, bs, ra)
} # }
```
