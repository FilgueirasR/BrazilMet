#' Get Max Reference Evapotranspiration Values by Geographic Location
#'
#' @description
#' Extract maximum reference evapotranspiration (max ETo) values for one or more locations of interest, using data from the dataset provided by Dias (2018).
#'
#' @param img SpatRaster with the maximum reference evapotranspiration (ETo) grid downloaded from max_eto_grid_download.
#' @param long Numeric. Longitude of the location in decimal degrees.
#' @param lat Numeric. Latitude of the location in decimal degrees. 
#' @return A `data.frame` object containing the maximum reference evapotranspiration (ETo) values per pair of coordinates.
#' @importFrom terra crs
#' @importFrom terra rast
#' @importFrom terra vect
#' @importFrom terra res
#' @importFrom terra buffer
#' @importFrom terra extract
#' 
#' @examples
#' \dontrun{
#' # Visualize Brazilian states (optional)
#' 
#' max_eto_df <- get_max_reference_eto_by_location(img, 
#'                                                 long = c(-51.95, -43.23),
#'                                                 lat = c(-23.52, -21.34))
#' }
#'
#' @export


get_max_eto_at_location <- function(img, lat, long) {
  
  # Converte para data.frame com colunas nomeadas
  coords_df <- data.frame(long, lat)
  colnames(coords_df) <- c("long", "lat")
  
  # Cria o objeto 'vect' com CRS da imagem
  vct <- terra::vect(coords_df, geom = c("long", "lat"), crs = terra::crs(img))
  
  # Calcula buffer (mesmo valor para todos os pontos)
  buffer_dist <- terra::res(img)[1] * 1.5 * 111320
  buffer <- terra::buffer(vct, buffer_dist)
  
  # Extrai valores médios dentro do buffer
  values_df <- terra::extract(img,
                                  buffer,
                                  fun = mean,
                                  na.rm = TRUE,
                                  small = TRUE)
  
  values_df <- cbind(coords_df, values_df)[,-3]
  # Retorna os valores extraídos (com ID se quiser identificar)
  return(values_df)
}