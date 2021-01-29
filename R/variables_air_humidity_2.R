#' Actual vapour pressure (ea) derived from relative humidity data
#' @param tmin A dataframe with minimum daily air temperature (°C)
#' @param tmax A dataframe with maximum daily air temperature (°C)
#' @param rh_min A dataframe with minimum daily relative air humidity
#' @param rh_mean A dataframe with mean daily relative air humidity
#' @param rh_max A dataframe with maximum daily relative air humidity
#' @examples
#' \dontrun{
#' ea <- ea_rh_calculation(tmin, tmax, rh_min, rh_mean, rh_max)
#' }
#' @export
#' @return Returns a data.frame object with the with ea from relative humidity data. 


ea_rh_calculation <- function(tmin, tmax, rh_min, rh_mean, rh_max){
  if(is.null(tmax) | is.null(rh_min)){
    
    ea_rh <- as.data.frame((0.6108*exp(17.27*tmin/(tmin + 237.3)))*(rh_max/100))} else {}
  
  if(is.null(rh_max)){
    
    e_tmax<- 0.6108*exp(17.27*tmax/(tmax + 237.3)) #e_tmax (kPa)
    e_tmin<- 0.6108*exp(17.27*tmin/(tmin + 237.3)) #e_tmin (kPa)
    ea_rh <- as.data.frame((rh_mean/100)*((e_tmin + e_tmax)/2))
    
  }else{e_tmax<- 0.6108*exp(17.27*tmax/(tmax + 237.3)) #e_tmax (kPa)
        e_tmin<- 0.6108*exp(17.27*tmin/(tmin + 237.3))
        ea_rh <- as.data.frame((e_tmin*(rh_max/100) + e_tmax*(rh_min/100))/2)}
        colnames(ea_rh)[1]<- "ea_rh"
        return(ea_rh)
}

#' Vapour pressure deficit (es - ea)
#' @param tmin A dataframe with minimum daily air temperature (°C)
#' @param tmax A dataframe with maximum daily air temperature (°C)
#' @param tdew A dataframe with dewpoint temperature (°C)
#' @param rh_min A dataframe with minimum daily relative air humidity
#' @param rh_mean A dataframe with mean daily relative air humidity
#' @param rh_max A dataframe with maximum daily relative air humidity
#' @param ea_method The methodology to calculate the Actual vapour pressure. Assume "rh" (default) for relative humidity procedure and "dew" for dewpoint temperature procedure.
#' @examples
#' \dontrun{
#' ea <- es_ea_calculation(tmin, tmax, tdew, rh_min, rh_mean, rh_max, ea_method)
#' }
#' @export
#' @return Returns a data.frame object with the with ea from relative humidity data. 


es_ea_calculation <- function(tmin, tmax, tdew, rh_min, rh_mean, rh_max, ea_method){

 if(is.null(ea_method)){ea_method <- "rh"}
# - Mean saturation vapor pressure derived from air temperature 

  e_tmax<- 0.6108*exp(17.27*tmax/(tmax + 237.3)) #e_tmax (kPa)
  e_tmin<- 0.6108*exp(17.27*tmin/(tmin + 237.3)) #e_tmin (kPa)
  es<- (e_tmax + e_tmin)/2
  es<-as.data.frame(es)
  
  if(ea_method == "dew"){
    

    # - Actual vapour pressure (ea) derived from dewpoint temperature
    
    ea <- as.data.frame(0.6108*exp(17.27*tdew/(tdew + 237.3)))
  
  } else {
    # - Actual vapor pressure (ea) derived from relative humidity data
    
    if(is.null(tmax) | is.null(rh_min)){ea <- (0.6108*exp(17.27*tmin/(tmin + 237.3)))*(rh_max/100)} else {
      
    }
    if(is.null(rh_max)){
      e_tmax<- 0.6108*exp(17.27*tmax/(tmax + 237.3)) #e_tmax (kPa)
      e_tmin<- 0.6108*exp(17.27*tmin/(tmin + 237.3)) #e_tmin (kPa)
      ea <- as.data.frame((rh_mean/100)*((e_tmin + e_tmax)/2))
      
    }else{
    e_tmax<- 0.6108*exp(17.27*tmax/(tmax + 237.3)) #e_tmax (kPa)
    e_tmin<- 0.6108*exp(17.27*tmin/(tmin + 237.3))
    ea <- as.data.frame((e_tmin*(rh_max/100) + e_tmax*(rh_min/100))/2)}
    
  }
 es_ea <- as.data.frame(es - ea)
 colnames(es_ea)<- "es_ea"
 return(es_ea)
}

#' Relative humidity (rh) calculation
#' @description Psychrometric constant (kPa/°C) is calculated in this function.
#' @param tmin A dataframe with minimum daily air temperature (°C)
#' @param t A dataframe with air temperature (°C) of the moment that you want to calculate the relative humidity.
#' @examples
#' \dontrun{
#' rh <- rh_calculation(tmin, t)
#' }
#' @export


rh_calculation <- function(tmin, t){
  e_tmin <- 0.6108*exp(17.27*tmin/(tmin + 237.3))
  e_t <- 0.6108*exp(17.27*t/(t + 237.3))
  rh <- 100*(e_tmin/e_t)
  rh<-as.data.frame(rh)
  colnames(rh) <- "rh"
  return(rh)
  
}

