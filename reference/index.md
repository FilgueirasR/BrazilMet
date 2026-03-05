# Package index

## Downloading data

Tools for accessing and downloading weather and climate data from
external sources.

- [`download_AWS_INMET_daily()`](https://filgueirasr.github.io/BrazilMet/reference/download_AWS_INMET_daily.md)
  : Download of hourly data from automatic weather stations (AWS) of
  INMET-Brazil in daily aggregates
- [`download_climate_normals()`](https://filgueirasr.github.io/BrazilMet/reference/download_climate_normals.md)
  : Download climatological normals from Conventional weather stations
  (CWS) of Inmet
- [`hourly_weather_station_download()`](https://filgueirasr.github.io/BrazilMet/reference/hourly_weather_station_download.md)
  : Download of hourly data from automatic weather stations (AWS) of
  INMET-Brazil
- [`max_eto_grid_download()`](https://filgueirasr.github.io/BrazilMet/reference/max_eto_grid_download.md)
  : Download maximum reference evapotranspiration (ETo) grids for Brazil

## Atmospheric Parameter Calculations

Functions for computing atmosphere pressure Psychrometric constant.

- [`Patm()`](https://filgueirasr.github.io/BrazilMet/reference/Patm.md)
  : Atmospheric pressure (Patm)
- [`psy_const()`](https://filgueirasr.github.io/BrazilMet/reference/psy_const.md)
  : Psychrometric constant

## Evapotranspiration Estimation

Methods for calculating reference evapotranspiration using approaches
like Penman-Monteith, or even design evapotranspiration.

- [`daily_eto_FAO56()`](https://filgueirasr.github.io/BrazilMet/reference/daily_eto_FAO56.md)
  : ETo calculation based on FAO-56 Penman-Monteith methodology, with
  data from automatic weather stations (AWS) downloaded and processed in
  function \*daily_download_AWS_INMET\*
- [`eto_hs()`](https://filgueirasr.github.io/BrazilMet/reference/eto_hs.md)
  : Hargreaves - Samani ETo
- [`etp_thorntwaite()`](https://filgueirasr.github.io/BrazilMet/reference/etp_thorntwaite.md)
  : Thorntwaite - Potential evapotranspiration
- [`correction_etp_thornwaite()`](https://filgueirasr.github.io/BrazilMet/reference/correction_etp_thornwaite.md)
  : Correction for Thorntwaite - Potential evapotranspiration
- [`get_max_eto_at_location()`](https://filgueirasr.github.io/BrazilMet/reference/get_max_eto_at_location.md)
  : Get Max Reference Evapotranspiration Values by Geographic Location
- [`design_eto()`](https://filgueirasr.github.io/BrazilMet/reference/design_eto.md)
  : Design reference evapotranspiration (Design ETo)

## Radiation Parameter Estimation

functions for estimating incoming solar radiation and related radiation
parameters.

- [`ra_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/ra_calculation.md)
  : Extraterrestrial radiation for daily periods (ra)
- [`sr_ang_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/sr_ang_calculation.md)
  : Solar radiation based in Angstrom formula (sr_ang)
- [`sr_tair_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/sr_tair_calculation.md)
  : Solar radiation data derived from air temperature differences
- [`rso_calculation_1()`](https://filgueirasr.github.io/BrazilMet/reference/rso_calculation_1.md)
  : Clear-sky solar radiation with calibrated values available
- [`rs_nearby_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/rs_nearby_calculation.md)
  : Solar radiation data from a nearby weather station
- [`rso_calculation_2()`](https://filgueirasr.github.io/BrazilMet/reference/rso_calculation_2.md)
  : Clear-sky solar radiation when calibrated values are not available
- [`rns_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/rns_calculation.md)
  : Net solar or net shortwave radiation (rns)
- [`rnl_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/rnl_calculation.md)
  : Net longwave radiation (rnl)
- [`rn_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/rn_calculation.md)
  : Net radiation (rn)
- [`radiation_conversion()`](https://filgueirasr.github.io/BrazilMet/reference/radiation_conversion.md)
  : Conversion factors for radiation

## Air Humidity & Wind Speed Parameters

Functions to compute relative humidity, saturation vapor pressure, and
wind speed adjustments.

- [`es_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/es_calculation.md)
  : Mean saturation vapour pressure (es)
- [`ea_dew_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/ea_dew_calculation.md)
  : Actual vapour pressure (ea) derived from dewpoint temperature
- [`ea_rh_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/ea_rh_calculation.md)
  : Actual vapour pressure (ea) derived from relative humidity data
- [`es_ea_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/es_ea_calculation.md)
  : Vapour pressure deficit (es - ea)
- [`rh_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/rh_calculation.md)
  : Relative humidity (rh) calculation
- [`u2_calculation()`](https://filgueirasr.github.io/BrazilMet/reference/u2_calculation.md)
  : Wind speed at 2 meters high

## Station Selection & Information

Tools for listing, filtering, and retrieving the weather stations of
interest in Brazil.

- [`see_stations_info()`](https://filgueirasr.github.io/BrazilMet/reference/see_stations_info.md)
  : Localization of the automatic weather station of INMET
- [`selectAWSstations()`](https://filgueirasr.github.io/BrazilMet/reference/selectAWSstations.md)
  : Select Automatic Weather Stations
