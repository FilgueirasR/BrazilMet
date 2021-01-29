#' Wind speed at 2 meters high
#' @description Wind speed at two meters high can be calculated with this function.
#' @param uz measured wind speed at z meters above ground surface
#' @param z height of measurement above ground surface.
#' @examples
#' \dontrun{
#' u2_df <- u2_calculation(uz, z)
#' }
#' @export


u2_calculation <- function(uz, z){
  u2 <- uz*(4.87/(log(67.8*z - 5.42)))
  u2 <- as.data.frame(u2)
  colnames(u2) <- "u2"
  return(u2)
}
