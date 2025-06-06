#' Hargreaves - Samani ETo
#' @param tmin A numeric object with Maximum daily air temperature (Celsius)
#' @param tmean A numeric object with Mean daily air temperature (Celsius)
#' @param tmax A numeric object with Maximum daily air temperature (Celsius)
#' @param ra A numeric object of extraterrestrial radiation (MJ m-2 day-1)
#' @examples
#' \dontrun{
#' eto_hs <- eto_hs(tmin, tmean, tmax, ra)
#' }
#' @export
#' @return Returns a numeric object with the ETo HS data
#' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha

eto_hs <- function(tmin, tmean, tmax, ra) {
  HS <- 0.0023 * (tmean + 17.8) * ((tmax - tmin)^0.5) * (0.408 * ra)

  return(HS)
}
