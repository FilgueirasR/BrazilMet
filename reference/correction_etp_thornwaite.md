# Correction for Thorntwaite - Potential evapotranspiration

Correction for Thorntwaite - Potential evapotranspiration

## Usage

``` r
correction_etp_thornwaite(etp, date, lat)
```

## Arguments

- etp:

  A column of a dataframe containing Thorntwaite potential
  evapotranspiration (ETp) data without adjustments for sunlight hours
  and the number of days in the month.".

- date:

  A column of dataframe with date (i.e: 2025-01-02).

- lat:

  A column of dataframe with latitude in degrees.

## Value

Returns a vector object with the Thorntwaite ETp corrected for sunlight
hours and the number of days in the month.

## Author

Roberto Filgueiras.

## Examples

``` r
if (FALSE) { # \dontrun{
etp_cor <- correction_etp_thornwaite(etp, date, lat)
} # }
```
