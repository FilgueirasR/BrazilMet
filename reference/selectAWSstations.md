# Select Automatic Weather Stations

Select automatic weather stations of INMET based on sf object.

## Usage

``` r
selectAWSstations(aoi, as_sf = FALSE)
```

## Arguments

- aoi:

  sf object. Area of interest.

- as_sf:

  logical. Inmet stations inventory result should be export as sf
  object. Default = FALSE.

## Value

a vector or sf object with station located at area of interest (aoi)

## Examples

``` r
if (FALSE) { # \dontrun{

es = geobr::read_municipality(code_muni = "ES")

esStations = selectStations(aoi = es, as_sf = F); es


} # }
```
