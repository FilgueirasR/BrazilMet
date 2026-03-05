# Design reference evapotranspiration (Design ETo)

Function to calculate the reference evapotranspitation for irrigation
design.

## Usage

``` r
design_eto(eto_daily_data, percentile = 0.8, grouping = NULL)
```

## Arguments

- eto_daily_data:

  A data frame containing daily reference evapotranspiration values (mm
  day-1) named as "ETo", and other column with dates named "date". To
  estimate the design reference evapotranspiration, it is recommended to
  use more than 10 years of historical data.

- percentile:

  The relative position of a value within the data distribution. The
  recomendation to desing irrigating is greater than 80 percent (0.8).
  For instance: The 90th (0.90) percentile of reference
  evapotranspiration (ETo) represents a value that is surpassed in only
  10 percent of the observed period.

- grouping:

  specifies the column used to group the data, typically by station. Use
  this parameter to calculate the design ETo separately for each
  station, based on the corresponding station code.

## Value

Returns a numeric object with the desing ETo for the respective data and
percentile.

## Author

Roberto Filgueiras

## Examples

``` r
if (FALSE) { # \dontrun{
design_eto_value <- design_eto(eto, percentile)
} # }
```
