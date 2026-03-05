# Net longwave radiation (rnl)

Net outgoing longwave radiation is calculate with this function

## Usage

``` r
rnl_calculation(tmin, tmax, ea, rs, rso)
```

## Arguments

- tmin:

  A dataframe with Minimum daily air temperature (Celsius)

- tmax:

  A dataframe with Maximum daily air temperature (Celsius)

- ea:

  A dataframe with the actual vapour pressure (KPa).

- rs:

  A dataframe with the incomimg solar radiation (MJ m-2 day-1).

- rso:

  A dataframe with the clear-sky radiation (MJ m-2 day-1)

## Value

A data.frame object with the net longwave radiation.

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
rnl_df <- rnl_calculation(tmin, tmax, ea, rs, rso)
} # }
```
