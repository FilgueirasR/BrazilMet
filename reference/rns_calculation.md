# Net solar or net shortwave radiation (rns)

The rns results form the balance between incoming and reflected solar
radiation (MJ m-2 day-1).

## Usage

``` r
rns_calculation(albedo, rs)
```

## Arguments

- albedo:

  Albedo or canopy reflectance coefficient. The 0.23 is the value used
  for hypothetical grass reference crop (dimensionless).

- rs:

  The incoming solar radiation (MJ m-2 day-1).

## Value

A data.frame object with the net solar or net shortwave radiation data.

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
ra <- rns_calculation(albedo, rs)
} # }
```
