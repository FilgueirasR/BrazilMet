#' Extraterrestrial radiation for daily periods (ra)
#' \if{html}{\figure{logo_BrazilMet.png}{options: height= 300 width=auto style = float:right alt= Our logo}}
#' @description ra is expressed in MJ m-2 day-1
#' @param latitude A dataframe with latitude in decimal degrees that you want to calculate the ra.
#' @param date A dataframe with the dates that you want to calculate the ra.
#' @examples
#' \dontrun{
#' ra <- ra_calculation(latitude, date)
#' }
#' @export
#' @return A data.frame with the extraterrestrial radiation for daily periods
#' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha


 ra_calculation <- function(latitude, date){
   
   julian_day <- as.data.frame(as.numeric(format(date, "%j")))
   lat_rad <- (pi/180)*(latitude)
   dr<-1+0.033*cos((2*pi/365)*julian_day)
   solar_declination<-0.409*sin(((2*pi/365)*julian_day)-1.39)
   sunset_hour_angle<-acos(-tan(lat_rad)*tan(solar_declination))
   ra <- ((24*(60))/pi)*(0.0820)*dr*(sunset_hour_angle*sin(lat_rad)*sin(solar_declination)+cos(lat_rad)*cos(solar_declination)*sin(sunset_hour_angle))
   ra <- as.data.frame(ra)
   colnames(ra)<- "ra"
   return(ra)
 }
 
 
 #' Solar radiation based in Angstrom formula (sr_ang)
 #' \if{html}{\figure{logo_BrazilMet.png}{options: height= 300 width=auto style = float:right alt= Our logo}}
 #' @description If global radiation is not measure at station, it can be estimated with this function. 
 #' @param latitude A dataframe with latitude in decimal degrees that you want to calculate the ra.
 #' @param date A dataframe with the dates that you want to calculate the ra.
 #' @param n The actual duration of sunshine. This variable is recorded with Campbell-Stokes sunshine recorder.
 #' @param as A dataframe with latitude in decimal degrees that you want to calculate the ra. The values of as = 0.25 is recommended by Allen et al. (1998).
 #' @param bs A dataframe with the dates that you want to calculate the ra. The values of bs = 0.50 is recommended by Allen et al. (1998).
 #' @examples
 #' \dontrun{
 #' sr_ang <- sr_ang_calculation(latitude, date, n, as, bs)
 #' }
 #' @export
 #' @return A data.frame object with solar radiation data
 #' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha
 
sr_ang_calculation <- function(latitude, date, n, as, bs){
   
  julian_day <- as.data.frame(as.numeric(format(date, "%j")))
  lat_rad <- (pi/180)*(latitude)
  dr<-1+0.033*cos((2*pi/365)*julian_day)
  solar_declination<-0.409*sin(((2*pi/365)*julian_day)-1.39)
  sunset_hour_angle<-acos(-tan(lat_rad)*tan(solar_declination))
  ra <- ((24*(60))/pi)*(0.0820)*dr*(sunset_hour_angle*sin(lat_rad)*sin(solar_declination)+cos(lat_rad)*cos(solar_declination)*sin(sunset_hour_angle))
  ra <- as.data.frame(ra)
  N <- (24/pi)*sunset_hour_angle
  sr_ang <- (as + bs*(n/N))*ra
  sr_ang <- as.data.frame(sr_ang)
  colnames(sr_ang)<- "sr_ang"
  return(sr_ang)
  
}
 
#' Solar radiation data derived from air temperature differences
#' \if{html}{\figure{logo_BrazilMet.png}{options: height= 300 width=auto style = float:right alt= Our logo}}
#' @description If global radiation is not measure at station, it can be estimated with this function. 
#' @param latitude A dataframe with latitude in decimal degrees that you want to calculate the ra.
#' @param date A dataframe with the dates that you want to calculate the ra.
#' @param location_krs Adjustment coeficient based in location. Please decide between "coastal or "interior". If coastal the krs will be 0.19, if interior the krs will be 0.16.
#' @param tmin A dataframe with Minimum daily air temperature (°C)
#' @param tmax A dataframe with Maximum daily air temperature (°C)
#' @examples
#' \dontrun{
#' sr_tair <- sr_tair_calculation(latitude, date, tmax, tmin, location_krs)
#' }
#' @export
#' @return A data.frame object with solar radiation data
#' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha

sr_tair_calculation <- function(latitude, date, tmax, tmin, location_krs){
   if(location_krs == "interior"){krs <- 0.16} else {if(location_krs == "coastal"){krs <- 0.19} else {krs <- NULL}}
   
   
   julian_day <- as.data.frame(as.numeric(format(date, "%j")))
   lat_rad <- (pi/180)*(latitude)
   dr<-1+0.033*cos((2*pi/365)*julian_day)
   solar_declination<-0.409*sin(((2*pi/365)*julian_day)-1.39)
   sunset_hour_angle<-acos(-tan(lat_rad)*tan(solar_declination))
   ra <- ((24*(60))/pi)*(0.0820)*dr*(sunset_hour_angle*sin(lat_rad)*sin(solar_declination)+cos(lat_rad)*cos(solar_declination)*sin(sunset_hour_angle))
   ra <- as.data.frame(ra)
   sr_tair <- krs*sqrt(tmax - tmin)*ra
   sr_tair <- as.data.frame(sr_tair)
   colnames(sr_tair)<- "sr_tair"
   return(sr_tair)
}

#' Clear-sky solar radiation with calibrated values available
#' \if{html}{\figure{logo_BrazilMet.png}{options: height= 300 width=auto style = float:right alt= Our logo}}
#' @description Clear-sky solar radiation is calculated in this function for near sea level or when calibrated values for as and bs are available.
#' @param as A dataframe with latitude in decimal degrees that you want to calculate the ra. The values of as = 0.25 is recommended by Allen et al. (1998).
#' @param bs A dataframe with the dates that you want to calculate the ra. The values of bs = 0.50 is recommended by Allen et al. (1998).
#' @param ra Extraterrestrial radiation for daily periods (ra).
#' @examples
#' \dontrun{
#' rso_df <- rso_calculation_1(as, bs, ra)
#' }
#' @export
#' @return A data.frame object with the clear-sky radiation data
#' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha

 rso_calculation_1 <- function(as, bs, ra){
   
   rso1 <- (as + bs)*ra
   rso1 <- as.data.frame(rso1)
   colnames(rso1)<- "rso1"
   return(rso1)
 }
 
 
 #' Solar radiation data from a nearby weather station
 #' \if{html}{\figure{logo_BrazilMet.png}{options: height= 300 width=auto style = float:right alt= Our logo}}
 #' @description The solar radiation data is calculated based in a nearby weather station.
 #' @param rs_reg A dataframe with the solar radiation at the regional location (MJ m-2 day-1).
 #' @param ra_reg A dataframe with the extraterrestrial radiation at the regional location (MJ m-2 day-1).
 #' @param ra A dataframe with the extraterrestrial radiation for daily periods (ra).
 #' @examples
 #' \dontrun{
 #' rs_nearby_df <- rs_nearby_calculation(rs_reg, ra_reg, ra)
 #' }
 #' @export
 #' @return A data.frame object with the Solar radiation data based on a nearby weather station
 #' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha 
 
 rs_nearby_calculation <- function(rs_reg, ra_reg, ra){
    
    rs_nearby <- (rs_reg/ra_reg)*ra
    rs_nearby <- as.data.frame(rs_nearby)
    colnames(rs_nearby)<- "rs_nearby"
    return(rs_nearby)
 }
 
 #' Clear-sky solar radiation when calibrated values are not available
 #' \if{html}{\figure{logo_BrazilMet.png}{options: height= 300 width=auto style = float:right alt= Our logo}}
 #' @description Clear-sky solar radiation is calculated in this function for near sea level or when calibrated values for as and bs are available.
 #' @param z Station elevation above sea level (m)
 #' @param ra Extraterrestrial radiation for daily periods (ra).
 #' @examples
 #' \dontrun{
 #' rso_df <- rso_calculation_2(z, ra)
 #' }
 #' @export
 #' @return A data.frame object with the clear-sky solar radiation
 #' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha
 
 rso_calculation_2 <- function(z, ra){
   
   rso2 <- (0.75 + 0.00002*z)*ra
   rso2 <- as.data.frame(rso2)
   colnames(rso2)<- "rso2"
   return(rso2)
 }

 #' Net solar or net shortwave radiation (rns)
 #' \if{html}{\figure{logo_BrazilMet.png}{options: height= 300 width=auto style = float:right alt= Our logo}}
 #' @description The rns results form the balance between incoming and reflected solar radiation (MJ m-2 day-1).
 #' @param albedo Albedo or canopy reflectance coefficient. The 0.23 is the value used for hypothetical grass reference crop (dimensionless).
 #' @param rs The incomimg solar radiation (MJ m-2 day-1).
 #' @examples
 #' \dontrun{
 #' ra <- rns_calculation(albedo, rs)
 #' }
 #' @export
 #' @return A data.frame object with the net solar or net shortwave radiation data.
 #' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha
 
 
 rns_calculation <- function(albedo, rs){
   
   rns <- (1 - albedo)*rs
   rns <- as.data.frame(rns)
   colnames(rns) <- "rns"
   return(rns)
 }
 
 #' Net longwave radiation (rnl)
 #' \if{html}{\figure{logo_BrazilMet.png}{options: height= 300 width=auto style = float:right alt= Our logo}}
 #' @description Net outgoing longwave radiation is calculate with this function 
 #' @param tmin A dataframe with Minimum daily air temperature (°C)
 #' @param tmax A dataframe with Maximum daily air temperature (°C)
 #' @param ea A dataframe with the actual vapour pressure (KPa).
 #' @param rs A dataframe with the incomimg solar radiation (MJ m-2 day-1).
 #' @param rso A dataframe with the clear-sky radiation (MJ m-2 day-1)
 #' @examples
 #' \dontrun{
 #' rnl_df <- rnl_calculation(tmin, tmax, ea, rs, rso)
 #' }
 #' @export
 #' @return A data.frame object with the net longwave radiation.
 #' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha
 
 
 rnl_calculation <- function(tmin, tmax, ea, rs, rso){
   
   sb_constant <- 0.000000004903
   rs_rso<-rs/rso
   if(rs_rso > 1){rs_rso<-1} else {rs_rso}
   rnl <- sb_constant*((((tmax+273.15)^4) + ((tmin + 273.15)^4))/2)*(0.34 - (0.14*sqrt(ea)))*((1.35*(rs_rso))-0.35)
   rnl<- as.data.frame(rnl)
   colnames(rnl) <- "rnl"
   return(rnl)
 }
 
 #' Net radiation (rn)
 #' \if{html}{\figure{logo_BrazilMet.png}{options: height= 300 width=auto style = float:right alt= Our logo}}
 #' @description  The net radiation (MJ m-2 day-1) is the difference between the incoming net shortwave radiation (rns) and the outgoing net longwave radiation (rnl).
 #' @param rns The incomimg net shortwave radiation (MJ m-2 day-1).
 #' @param rnl The outgoing net longwave radiation (MJ m-2 day-1).
 #' @examples
 #' \dontrun{
 #' rn <- rn_calculation(rns, rnl)
 #' }
 #' @export
 #' @return A data.frame object with the net radiation data.
 #' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha
 
 
 rn_calculation <- function(rns, rnl){
   
   rn <- (rns - rnl)
   rn <- as.data.frame(rn)
   colnames(rn) <- "rn"
   return(rn)
 }
 
 