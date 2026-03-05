# Download climatological normals from Conventional weather stations (CWS) of Inmet

This function will download the climatological normals from CWS stations
available in Inmet site.

## Usage

``` r
download_climate_normals(variable, range_time)
```

## Arguments

- variable:

  The variables available to download. The available variables are:
  rainfall_norm, t2m_norm, rh_norm, ws_norm and etp_norm.

- range_time:

  The range of time which the climatological normals were computed. The
  available range of time is: "1991-2020".

## Value

Returns a data.frame with climatological normal required.

## Author

Roberto Filgueiras

## Examples

``` r
if (FALSE) { # \dontrun{
df <- download_climate_normals(
  variable = "rainfall_norm",
  range_time = "1991-2020"
)
} # }
```
