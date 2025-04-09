#' Download of hourly data from automatic weather stations (AWS) of INMET-Brazil
#' @description This function will download the hourly AWS data of INMET for whatever station of interest, based on the period of time selected (start_date and end_date) and station code.
#' @param stations The station code (ID - WMO code) for download. To see the station ID, please see the function *see_stations_info*.
#' @param start_date Date that start the investigation, should be in the following format (1958-01-01 /Year-Month-Day)
#' @param end_date Date that end the investigation, should be in the following format (2017-12-31 /Year-Month-Day)
#' @import dplyr
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom tibble as_tibble
#' @importFrom lubridate hours
#' @examples
#' \dontrun{
#' df <- hourly_weather_station_download(
#'                                       stations = c("A001", "A042"),
#'                                       start_date = "2022-08-12",
#'                                       end_date = "2022-08-16")
#' }
#' @export
#' @return Returns a data.frame with the AWS data requested
#' @author Roberto Filgueiras

hourly_weather_station_download <- function(stations, start_date, end_date) {
  
  X <- patm_max_mb <- patm_min_mb <- hour <- NULL
  dew_tmin_c <- dew_tmax_c <- tair_min_c <- tair_max_c <- tair_dry_bulb_c <- NULL
  rainfall_mm <- rh_max_porc <- rh_min_porc <- rh_mean_porc <- NULL
  ws_2_m_s <- ws_gust_m_s <- wd_degrees <- sr_kj_m2 <- sr_mj_m2 <- NULL
  date_hour <- UTC_offset <- date_hour_local <- date_hour_utc <- NULL
  
  altitude_m <- dew_tmean_c <- latitude_degrees <- longitude_degrees <- patm_mb <- NULL
  ra_mj_m2 <- station_code <- tair_mean_c <- uf <- ws_2_m_s <- NULL
  
  start_year <- substr(start_date, 1, 4)
  end_year <- substr(end_date, 1, 4)
  
  df_sequence <- data.frame()
  
  for (year in seq(from = as.numeric(start_year), to = as.numeric(end_year))) {
    message("Downloading data for: ", year)

    tempdir <- tempfile()
    tf <- paste0(gsub("\\", "/", tempdir, fixed = TRUE), ".zip")
    outdir <- gsub("\\", "/", tempdir, fixed = TRUE)
    options(timeout = 600)
    
    utils::download.file(url = paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", year, ".zip"), 
                         destfile = tf, method = "auto", cacheOK = F, quiet = T)
    
    a <- unzip(zipfile = tf, exdir = outdir, junkpaths = T)
    
    df_all_stations <- data.frame()

    for(station in stations){
      
      station_file <- list.files(outdir, pattern = station, full.names = T, all.files = T)
      
      if (length(station_file) == 0) {
        message("There is no data for this period for this station. Choose another period!")
      } else {
        
        dfx <- read.csv(file = station_file, 
                        header = T, 
                        sep = ";",
                        skip = 8,
                        na = "-9999",
                        dec = ",",
                        check.names = F)
        
        header_info <- read.csv(file = station_file, header = F, sep = ";")
        
        OMM <- header_info[4, 2]
        UF <- header_info[2, 2]
        station <- header_info[3, 2]
        
        # Função para converter coordenadas no formato correto
        convert_coord <- function(coord) {
          #lat_part <- substr(coord, 1, 3)
          lat_part <- sub(",.*", "", coord) # 
          
          #dec_part <- substr(coord, 5, 10)
          dec_part <- sub(".*,", "", coord)
          as.numeric(paste0(lat_part, ".", dec_part))}
        
        # Extrai e converte os valores desejados
        latitude <- convert_coord(header_info[5, 2])
        longitude <- convert_coord(header_info[6, 2])
        
        # Ajuste da altitude
        altitude <- as.numeric(gsub(",", ".", header_info[7, 2]))
        
        names(dfx) <- c(
          "date", "hour", "rainfall_mm", "patm_mb",
          "patm_max_mb", "patm_min_mb", "sr_kj_m2",
          "tair_dry_bulb_c", "dew_tmean_c", "tair_max_c", "tair_min_c", "dew_tmax_c",
          "dew_tmin_c", "rh_max_porc", "rh_min_porc", "rh_mean_porc", "wd_degrees",
          "ws_gust_m_s", "ws_2_m_s", "X"
        )

        dfx <- dplyr::select(dfx, -X, -patm_max_mb, -patm_min_mb)
        dfx <- tibble::as_tibble(dfx)
        dfx <- dplyr::mutate(dfx, date = as.Date(date), 
                             hour = as.numeric(substr(hour, 1, 2)))
        
        dfx$date_hour <- paste0(dfx$date," ", dfx$hour)
        dfx$date_hour <- as.POSIXct(strptime(dfx$date_hour, format = "%Y-%m-%d %H"))

         dfx <- dfx %>%
           dplyr::mutate(
             # Define o offset por estado
             UTC_offset = case_when(
               UF == "AC" ~ -5,  # UTC-5 (Acre)
               UF %in% c("AM", "MT", "RO", "RR") ~ -4,  # UTC-4 (Amazonas, Mato Grosso, Rondônia, Roraima)
               UF %in% c("MS", "GO", "DF", "TO", "BA", "SE", "AL", "PE", "PB", 
                         "RN", "CE", "PI", "MA", "PA", "AP", "SP", "RJ", "MG", "ES", 
                         "PR", "SC", "RS") ~ -3,  # UTC-3 (Maior parte do Brasil)
               TRUE ~ 0  # Caso não encontre a UF, mantém UTC
             ),
             # Ajusta para horário local
             date_hour_local = date_hour + hours(UTC_offset)) %>% 
           dplyr::mutate(
             # Extraindo a data e a hora corretamente
             date = as.POSIXct(strptime(date_hour_local, format = "%Y-%m-%d")),  # Apenas a data
             hour = format(date_hour_local, "%H:%M:%S")  # Apenas a hora
           ) %>%
           select(-UTC_offset)
names(dfx)
  
        dfx_temp <- dplyr::select(dfx, hour, date, dew_tmin_c, dew_tmean_c, dew_tmax_c, tair_min_c, tair_dry_bulb_c, tair_max_c, date_hour, date_hour_local)
        #dfx_temp <- dplyr::mutate(dfx_temp, tair_mean_c = ((tair_min_c + tair_max_c) / 2))
        #dfx_temp <- dplyr::mutate(dfx_temp, dew_tmean_c = ((dew_tmin_c + dew_tmax_c) / 2))
        
        dfx_prec <- dplyr::select(dfx, hour, date, rainfall_mm)
        
        dfx_press <- dplyr::select(dfx, hour, date, patm_mb)
        
        dfx_ur <- dplyr::select(dfx, hour, date, rh_max_porc, rh_min_porc, rh_mean_porc)
        
        dfx_vv <- dplyr::select(dfx, hour, date, ws_2_m_s, ws_gust_m_s, wd_degrees)
        #dfx_vv <- dplyr::mutate(dfx_vv, u2 = (4.868 / (log(67.75 *10 - 5.42))) * ws_2_m_s)
          
        dfx_RG <- dplyr::select(dfx, hour, date, sr_kj_m2)
        dfx_RG <- dplyr::mutate(dfx_RG, sr_mj_m2 = sr_kj_m2 / 1000)
        dfx_RG <- dplyr::select(dfx_RG, date, hour, sr_kj_m2, sr_mj_m2)########

        dfx_hour <- dplyr::full_join(dfx_temp, dfx_prec, by = c("date", "hour"))
        dfx_hour <- dplyr::full_join(dfx_hour, dfx_press, by = c("date", "hour"))
        dfx_hour <- dplyr::full_join(dfx_hour, dfx_ur,  by = c("date", "hour"))
        dfx_hour <- dplyr::full_join(dfx_hour, dfx_vv,  by = c("date", "hour"))
        dfx_hour <- dplyr::full_join(dfx_hour, dfx_RG,  by = c("date", "hour"))
        dfx_hour <- dplyr::mutate(dfx_hour, OMM = OMM)
        df <- dfx_hour
        
        df <- dplyr::filter(df, date >= start_date & date <= end_date)

        df <- df |> 
          dplyr::mutate(
            station = station,
            UF = UF,
            longitude_degrees = longitude,
            latitude_degrees = latitude,
            altitude_m = altitude)|> 
          dplyr::arrange(station, date) |>
          dplyr::rename("station_code" = "OMM",
                        "uf" = "UF",
                        #"ws_2_m_s" = "u2",
                        "date_hour_utc" = "date_hour")|>
          dplyr::select(c(station_code,
                          station,
                          uf,
                          date,
                          hour,
                          date_hour_utc,
                          tair_dry_bulb_c,
                          tair_min_c,
                          tair_max_c,
                          dew_tmean_c,
                          dew_tmin_c,
                          dew_tmax_c,
                          rainfall_mm,
                          patm_mb,
                          rh_mean_porc,
                          rh_max_porc,
                          rh_min_porc,
                          ws_2_m_s,
                          ws_gust_m_s,
                          wd_degrees,
                          sr_kj_m2,
                          sr_mj_m2,
                          longitude_degrees,
                          latitude_degrees,
                          altitude_m))
        
      }
      
      df_all_stations <- rbind(df_all_stations, df)
    }
    
    df_sequence <- rbind(df_sequence, df_all_stations)
    
    df_sequence <- df_sequence 
  }
  
  return(df_sequence)
}
