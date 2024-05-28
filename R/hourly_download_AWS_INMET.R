#' Download of hourly data from automatic weather stations (AWS) of INMET-Brazil
#' @description This function will download the hourly AWS data of INMET for whatever station of interest, based on the period of time selected (start_date and end_date) and station code. The limit acquisition in the same requisition for hourly data is one year.
#' @param station The station code (ID - WMO code) for download. To see the station ID, please see the function *see_stations_info*.
#' @param start_date Date that start the investigation, should be in the following format (1958-01-01 /Year-Month-Day)
#' @param end_date Date that end the investigation, should be in the following format (2017-12-31 /Year-Month-Day)
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' df <- weather_station_download(station = "A001", start_date = "2022-08-12", end_date = "2022-08-16")
#' }
#' @export
#' @return Returns a data.frame with the AWS data requested
#' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha


hourly_weather_station_download <- function(start_date, end_date, station) {
  a <- httr::GET(url = paste0("https://apitempo.inmet.gov.br/estacao/", start_date, "/", end_date, "/", station))
  dados <- jsonlite::fromJSON(rawToChar(a$content))
  if (a$status_code == 200) {
    message("Data successful downloaded")
  }

  # latitude<-read.csv(file = "data/dados_F501_H_2013-12-26_2022-05-04.csv", header = F, sep = ':', dec = ",", nrows = 8)
  latitude <- as.numeric(dados$VL_LATITUDE[1])

  # longitude<-read.csv(file = "data/dados_F501_H_2013-12-26_2022-05-04.csv", header = F, sep = ':', dec = ",", nrows = 8)
  longitude <- as.numeric(dados$VL_LONGITUDE[1])

  dados$HR_MEDICAO <- as.numeric(as.factor(dados$HR_MEDICAO))
  dados$data_hora <- paste0(dados$DT_MEDICAO, dados$HR_MEDICAO)
  dados$data_hora <- as.POSIXct(strptime(dados$data_hora, format = "%Y-%m-%d %H"))



  for (i in 1:nrow(dados)) {
    if (longitude > -37.5) (dados$data_hora[i] <- dados$data_hora[i] - as.difftime(2, units = "hours")) else if (longitude > -52.5) (dados$data_hora[i] <- dados$data_hora[i] - as.difftime(3, units = "hours")) else if (longitude > -67.5) (dados$data_hora[i] <- dados$data_hora[i] - as.difftime(4, units = "hours")) else if (longitude > -82.5) (dados$data_hora[i] <- dados$data_hora[i] - as.difftime(5, units = "hours"))
  }

  dados <- dados %>% dplyr::select(c(
    "date_hour",
    "rr_h",
    "pp_h",
    "swr_h",
    "tm_h",
    "dtm_h",
    "tn_h",
    "tx_h",
    "dtx_h",
    "dtn_h",
    "rhx_h",
    "rhn_h",
    "rhm_h",
    "wd_h",
    "gw_h",
    "ws_h"
  ))


  dados <- dados %>% dplyr::mutate_if(is.character, as.numeric)
  dados <- dados %>% dplyr::mutate(
    hora = lubridate::hour(date_hour),
    AWS = station
  )

  return(dados)
}
