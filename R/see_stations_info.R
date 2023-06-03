#' Localization of the automatic weather station of INMET
#' @description Function to see the localization of the automatic weather station of INMET.
#' @importFrom readxl read_xlsx
#' @examples
#' \dontrun{
#' see_stations_info()
#' }
#' @return A data.frame with informations of OMM code, latitude, longitude and altitude of all AWS stations available in INMET.
#' @export
#' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha


see_stations_info <- function() {
  a <- readxl::read_xlsx(system.file("extdata", paste0("Localization_AWS", ".xlsx"), package = "BrazilMet"))
  return(a)
}
