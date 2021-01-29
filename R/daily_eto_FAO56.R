#' Eto calculation based on FAO-56 Penman-Monteith methodology, with data from automatic weather stations (AWS) downloaded and processed in function *daily_download_AWS_INMET*
#' \if{html}{\figure{logo_BrazilMet.png}{options: height= 300 width=auto style = float:right alt= Our logo}}
#' @description This function will calculate the reference evapotranspiration (ETo) based on FAO-56 (Allen et al., 1998) with the the automatic weather stations (AWS) downloaded and processed in function *daily_download_AWS_INMET*.
#' @param lat  A numeric value of the Latitude of the AWS in decimal degrees
#' @param tmin A dataframe with Minimum daily air temperature (°C)
#' @param tmax A dataframe with Maximum daily air temperature (°C)
#' @param tmean A dataframe with Mean daily air temperature (°C)
#' @param Rs A dataframe with mean daily solar radiation (MJ m-2 day-1)
#' @param u2 A dataframe with Wind speed at meters high (m s-2)
#' @param P A dataframe with atmospheric Pressure (mmHg)
#' @param RH_max A dataframe with Maximum relative humidity
#' @param RH_min A dataframe with Minimum relative humidity
#' @param z A numeric value of the altitude of AWS
#' @param date A dataframe with the date information 
#' @import stringr
#' @import dplyr
#' @import utils
#' @importFrom stats aggregate
#' @importFrom stats na.omit
#' @importFrom utils download.file
#' @importFrom utils read.csv
#' @importFrom utils unzip
#' @importFrom dplyr full_join
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' eto<-daily_eto_FAO56(lat, tmin, tmax, tmean, Rs, u2, P, RH_max, RH_min, z, date)
#' }
#' @export
#' @return Returns a dataframe with the AWS data requested
#' @author Roberto Filgueiras



daily_eto_FAO56 <- function(lat, tmin, tmax, tmean, Rs, u2, P, RH_max, RH_min, z, date){
 
  
  delta<-(4098*(0.6108*exp(17.27*tmean/(tmean+237.30))))/(tmean +237.30)^2
  
  
  #Step 5 - convertion of P from mmHg to kPa
  P<- P/10
  
  #Step 6 - Psychrometric constant (KPa °C-1)
  
  psy_constant<- 0.000665*P
  
  #Step 7 - Delta Term (DT) (auxiliary calculation for radiation term)
  
  DT = (delta/(delta + psy_constant*(1+0.34*u2)))
  
  #Step 8 - Psi term (PT) (auxliary calculation for Wind Term)
  
  PT<- (psy_constant)/(delta + psy_constant*(1 + 0.34*u2))
  
  #Step 9 - temperature term (TT) (auxiliary calculation for wind Term)
  
  TT <- (900/(tmean + 273))*u2
  
  #Step 10 - Mean saturation vapor pressure derived from air temperature 
  
  e_t<- 0.6108*exp(17.27*tmean/(tmean + 237.3))  #e_t (kPa)
  e_tmax<- 0.6108*exp(17.27*tmax/(tmax + 237.3)) #e_tmax (kPa)
  e_tmin<- 0.6108*exp(17.27*tmin/(tmin + 237.3)) #e_tmin (kPa)
  es<- (e_tmax + e_tmin)/2
  
  # Step 11 actual vapor pressure - ea (kPa)
  
  ea<- (e_tmin * (RH_max/100) + e_tmax*(RH_min/100))/2
  
  # Step 12 The inverse relative distance Earth-Sun (dr) and solar declination (solar_decli)
  
  j<- as.numeric(format(date, "%j"))# julian day 
  dr <- 1 + 0.033*cos(2*pi*j/365)
  solar_decli <- 0.409*sin((2*pi*j/365)- 1.39)
  
  #Step 13 - Conversion of latitude (lat) in degrees (decimal degrees) to radian (lat_rad)
  
  lat_rad<- (pi/180)*lat
  
  #Step 14 - sunset hour angle (ws) rad
  
  ws<- acos(-tan(lat_rad)*tan(solar_decli))
  
  #Step 15 - Extraterrestrial radiation - ra (MJ m-2 day-1)
  
  Gsc <- 0.0820 #(MJ m-2 min)
  ra <- (24*(60)/pi)*Gsc*dr*((ws*sin(lat_rad)*sin(solar_decli)) + (cos(lat_rad)*cos(solar_decli)*sin(ws)))
  
  # Step 16 - Clear sky solar radiation (rso)
  
  rso<- (0.75 + (2*10^-5)*z)*ra
  
  #Step 17 - Net solar or net shortwave radiation (Rns)
  #Rs is the incoming solar radiation ( Mj m-2 day-1)
  
  Rns<- (1- 0.23)*Rs
  
  #Step 18 - Net outgoing long wave radiation (Rnl) (MJ m-2 day-1)
  
  sigma<- 4.903*10^-9 # MJ K-4 m-2 day -1
  Rnl <- sigma*((((tmax +273.16)^4) + ((tmin + 273.16)^4))/2)*(0.34 - 0.14*sqrt(ea))*(1.35*(Rs/rso) - 0.35)
  
  #Step 19 - Net Radiation (Rn)
  
  Rn <- Rns - Rnl
  
  #TO express the Rn in equivalent of evaporation (mm) 
  
  Rng <- 0.408 *Rn
  
  # Final Step - FS1. Radiation tem (ETrad)
  
  ETrad<- DT*Rng
  
  # Final Step FS2 - Wind term (ETwind)
  
  ETwind <- PT*TT*(es - ea)
  
  # Final Reference evapotranspiration value 
  
  ETo <- ETwind + ETrad
  
}



  

  



