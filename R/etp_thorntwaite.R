#' Thorntwaite - Potential evapotranspiration
#' @param tmean A dataframe with Mean monthly air temperature (Celsius)
#' @examples
#' \dontrun{
#' etp <- etp_thorntwaite(tmean)
#' }
#' @export
#' @return Returns a data.frame object with the Thorntwaite ETp data
#' @author Roberto Filgueiras.

etp_thorntwaite <- function(tmean) {
  I <- sum((tmean / 5) ^ 1.514)
  alpha <- 6.75e-7 * I^3 - 7.71e-5 * I^2 + 1.792e-2 * I + 0.49239
  etp <- 16 * (10 * tmean / I) ^ alpha
  etp<- round(etp, 2)
  return(etp)
}

#' Correction for Thorntwaite - Potential evapotranspiration
#' @param etp A column of a dataframe containing Thorntwaite potential evapotranspiration (ETp) data without adjustments for sunlight hours and the number of days in the month.".
#' @param date A column of dataframe with date (i.e: 2025-01-02).
#' @param lat A column of dataframe with latitude in degrees.
#' @examples
#' \dontrun{
#' etp_cor <- correction_etp_thornwaite(etp, date, lat)
#' }
#' @export
#' @return Returns a vector object with the Thorntwaite ETp corrected for sunlight hours and the number of days in the month.
#' @author Roberto Filgueiras.

correction_etp_thornwaite <- function(etp, date, lat) {
  
  rad <- function(deg) {(deg * pi) / (180)}
  
  doy <- strftime(date, format = "%j") |>
    as.numeric()
  
  days_in_month <- lubridate::days_in_month(date)
  
  delta<- 23.45 * sin(((rad(360)) * (284 + doy))/365)
  hn = acos(-tan(rad(lat)) * tan(rad(delta))) * (180/pi)
  N = 2*hn/15
  
  etp_cor <- etp*(N/12)*(days_in_month/30)
  etp_cor<- round(etp_cor, 2)
  
  return(etp_cor)
}
