#' Mean saturation vapour pressure (es)
#' @param tmin A dataframe with Minimum daily air temperature (°C)
#' @param tmax A dataframe with Maximum daily air temperature (°C)
#' @examples
#' \dontrun{
#' es <-es_calculation(tmin, tmax)
#' }
#' @export
#' @return Returns a data.frame object with the with es data
#' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha 


es_calculation <- function(tmin, tmax){
  # - Mean saturation vapor pressure derived from air temperature 
  
  e_tmax<- 0.6108*exp(17.27*tmax/(tmax + 237.3)) #e_tmax (kPa)
  e_tmin<- 0.6108*exp(17.27*tmin/(tmin + 237.3)) #e_tmin (kPa)
  es<- (e_tmax + e_tmin)/2
  es<-as.data.frame(es)
}


#' Actual vapour pressure (ea) derived from dewpoint temperature
#' @param tdew A dataframe with dewpoint temperature (°C)
#' @examples
#' \dontrun{
#' ea <-ea_dew_calculation(tdew)
#' }
#' @export
#' @return Returns a data.frame object with the with ea from dewpoint data
#' @author  Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha 


ea_dew_calculation <- function(tdew){
  ea_dew <- 0.6108*exp(17.27*tdew/(tdew + 237.3))
}


