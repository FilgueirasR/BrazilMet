#' Atmospheric pressure (P)
#' 
#' @param z Elevation above sea level (m)
#' @examples
#' \dontrun{
#' P <- p(z)
#' }
#' @export
#' @return Returns a data.frame object with the with ETo HS data 

P <- function(z){
  P <- 101.3*((293 - 0.0065*z)/293)^5.26
  P <- as.data.frame(P)
  colnames(P)<- "P (mB)"
   return(P)
  }
  

#' Psychrometric constant
#' @description Psychrometric constant (kPa/°C) is calculated in this function.
#' @param P Atmospheric pressure (kPa)
#' @examples
#' \dontrun{
#' psy_df <- psy_const(P)
#' }
#' @export


psy_const <- function(P){
  psy_const <- 0.000665*P
  psy_const<- as.data.frame(psy_const)
  colnames(psy_const)<- "psy_const"
  return(psy_const)
  
}
