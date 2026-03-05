# Download of hourly data from automatic weather stations (AWS) of INMET-Brazil

This function will download the hourly AWS data of INMET for whatever
station of interest, based on the period of time selected (start_date
and end_date) and station code.

## Usage

``` r
hourly_weather_station_download(stations, start_date, end_date)
```

## Arguments

- stations:

  The station code (ID - WMO code) for download. To see the station ID,
  please see the function \*see_stations_info\*.

- start_date:

  Date that start the investigation, should be in the following format
  (1958-01-01 /Year-Month-Day)

- end_date:

  Date that end the investigation, should be in the following format
  (2017-12-31 /Year-Month-Day)

## Value

Returns a data.frame with the AWS data requested

## Author

Roberto Filgueiras

## Examples

``` r
if (FALSE) { # \dontrun{
df <- hourly_weather_station_download(
                                      stations = c("A001", "A042"),
                                      start_date = "2022-08-12",
                                      end_date = "2022-08-16")
} # }
```
