# Download of hourly data from automatic weather stations (AWS) of INMET-Brazil in daily aggregates

This function will download the hourly AWS data of INMET and it will
aggregate the data in a daily time scale, based on the period of time
selected (start_date and end_date).

## Usage

``` r
download_AWS_INMET_daily(stations, start_date, end_date)
```

## Arguments

- stations:

  The stations code (ID - WMO code) for download. To see the station ID,
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

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
df <- download_AWS_INMET_daily(
  stations = c("A001", "A042"),
  start_date = "2016-01-01",
  end_date = "2018-12-31"
)
} # }
```
