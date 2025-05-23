#' Select Automatic Weather Stations
#'
#' @description
#' Select automatic weather stations of INMET based on sf object.
#'
#'
#' @param aoi sf object. Area of interest.
#' @param as_sf logical. Inmet stations inventory result should be export as sf object. Default = FALSE.
#'
#' @return a vector or sf object with station located at area of interest (aoi)
#'
#' @import sf
#'
#' @export
#'
#' @examples

#' \dontrun{
#'
#' es = geobr::read_municipality(code_muni = "ES")
#'
#' esStations = selectStations(aoi = es, as_sf = F); es
#'
#'
#' }

selectAWSstations <- function(aoi, as_sf = FALSE) {
  stopifnot(
    `\`as_sf\` must be logical vector` = is.logical(as_sf) & length(as_sf) == 1,
    `\`aoi\` must be a polygon of class \`sf \` (sf package)` = class(aoi)[1] ==
      "sf"
  )

  est <- sf::st_as_sf(see_stations_info(), coords = c("longitude_degrees", "latitude_degrees")) %>%
    sf::st_set_crs(4674) %>%
    sf::st_transform(4326) %>%
    sf::st_intersection(aoi %>%
    sf::st_transform(4326)) %>% 
    suppressWarnings()

  if (as_sf == FALSE) {
    est <- est %>%
      pull(6)
  }

  return(est)
}
