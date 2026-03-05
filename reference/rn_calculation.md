# Net radiation (rn)

The net radiation (MJ m-2 day-1) is the difference between the incoming
net shortwave radiation (rns) and the outgoing net longwave radiation
(rnl).

## Usage

``` r
rn_calculation(rns, rnl)
```

## Arguments

- rns:

  The incomimg net shortwave radiation (MJ m-2 day-1).

- rnl:

  The outgoing net longwave radiation (MJ m-2 day-1).

## Value

A data.frame object with the net radiation data.

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
rn <- rn_calculation(rns, rnl)
} # }
```
