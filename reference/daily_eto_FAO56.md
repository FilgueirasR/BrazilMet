# ETo calculation based on FAO-56 Penman-Monteith methodology, with data from automatic weather stations (AWS) downloaded and processed in function \*daily_download_AWS_INMET\*

This function will calculate the reference evapotranspiration (ETo)
based on FAO-56 (Allen et al., 1998) with the automatic weather stations
(AWS) data, downloaded and processed in function
\*daily_download_AWS_INMET\*.

## Usage

``` r
daily_eto_FAO56(lat, tmin, tmax, tmean, Rs, u2, Patm, RH_max, RH_min, z, date)
```

## Arguments

- lat:

  A numeric value of the Latitude of the AWS (decimal degrees).

- tmin:

  A dataframe with Minimum daily air temperature (Celsius).

- tmax:

  A dataframe with Maximum daily air temperature (Celsius).

- tmean:

  A dataframe with Mean daily air temperature (Celsius).

- Rs:

  A dataframe with mean daily solar radiation (MJ m-2 day-1).

- u2:

  A dataframe with Wind speed at two meters high (m s-2).

- Patm:

  A dataframe with atmospheric Pressure (mB).

- RH_max:

  A dataframe with Maximum relative humidity (percentage).

- RH_min:

  A dataframe with Minimum relative humidity (percentage).

- z:

  A numeric value of the altitude of AWS (m).

- date:

  A data.frame with the date information (YYYY-MM-DD).

## Value

Returns a data.frame with the AWS data requested

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
eto <- daily_eto_FAO56(lat, tmin, tmax, tmean, Rs, u2, Patm, RH_max, RH_min, z, date)
} # }
```
