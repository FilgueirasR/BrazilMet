#' Hargreaves - Samani ETo
#' @param tmin A dataframe with Maximum daily air temperature (°C)
#' @param tmean A dataframe with Minimum daily air temperature (°C)
#' @param tmax A dataframe with Maximum daily air temperature (°C)
#' @param ra A dataframe of extraterrestrial radiation
#' @examples
#' \dontrun{
#' eto_hs <-eto_hs(tmin, tmean, tmax, ra)
#' }
#' @export
#' @return Returns a data.frame object with the with ETo HS data
#' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha  

 eto_hs <- function(tmin, tmean, tmax, ra){
        HS<- as.data.frame(0.0023*(tmean + 17.8)*((tmax - tmin)^0.5)*(0.408*ra))
        colnames(HS)[1] <- "Eto_HS"
        return(HS)
        }