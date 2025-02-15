#' Thorntwaite - Potential evapotranspiration
#' @param tmean A dataframe with Mean monthly air temperature (°C)
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
