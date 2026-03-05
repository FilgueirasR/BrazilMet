# Solar radiation based in Angstrom formula (sr_ang)

If global radiation is not measure at station, it can be estimated with
this function.

## Usage

``` r
sr_ang_calculation(latitude, date, n, as, bs)
```

## Arguments

- latitude:

  A dataframe with latitude in decimal degrees that you want to
  calculate the ra.

- date:

  A dataframe with the dates that you want to calculate the ra.

- n:

  The actual duration of sunshine. This variable is recorded with
  Campbell-Stokes sunshine recorder.

- as:

  A dataframe with latitude in decimal degrees that you want to
  calculate the ra. The values of as = 0.25 is recommended by Allen et
  al. (1998).

- bs:

  A dataframe with the dates that you want to calculate the ra. The
  values of bs = 0.50 is recommended by Allen et al. (1998).

## Value

A data.frame object with solar radiation data

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
sr_ang <- sr_ang_calculation(latitude, date, n, as, bs)
} # }
```
