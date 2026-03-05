# Get Max Reference Evapotranspiration Values by Geographic Location

Extract maximum reference evapotranspiration (max ETo) values for one or
more locations of interest, using data from the dataset provided by Dias
(2018).

## Usage

``` r
get_max_eto_at_location(img, lat, long)
```

## Arguments

- img:

  SpatRaster with the maximum reference evapotranspiration (ETo) grid
  downloaded from max_eto_grid_download.

- lat:

  Numeric. Latitude of the location in decimal degrees.

- long:

  Numeric. Longitude of the location in decimal degrees.

## Value

A \`data.frame\` object containing the maximum reference
evapotranspiration (ETo) values per pair of coordinates.

## Author

Roberto Filgueiras.

## Examples

``` r
if (FALSE) { # \dontrun{
# Visualize Brazilian states (optional)

max_eto_df <- get_max_reference_eto_by_location(img, 
                                                long = c(-51.95, -43.23),
                                                lat = c(-23.52, -21.34))
} # }
```
