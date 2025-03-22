#' Download of hourly data from automatic weather stations (AWS) of INMET-Brazil in daily aggregates
#' @description This function will download the hourly AWS data of INMET and it will aggregate the data in a daily time scale, based on the period of time selected (start_date and end_date).
#' @param stations The stations code (ID - WMO code) for download. To see the station ID, please see the function *see_stations_info*.
#' @param start_date Date that start the investigation, should be in the following format (1958-01-01 /Year-Month-Day)
#' @param end_date Date that end the investigation, should be in the following format (2017-12-31 /Year-Month-Day)
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
#' @importFrom dplyr rename
#' @examples
#' \dontrun{
#' df <- download_AWS_INMET_daily(stations = c("A001","A042"),
#'                                start_date = "2016-01-01",
#'                                 end_date = "2018-12-31")
#' }
#' @export
#' @return Returns a data.frame with the AWS data requested
#' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha

download_AWS_INMET_daily <- function(stations, start_date, end_date) {

  X <- patm_max_mb <- patm_min_mb <- hour <- NULL
  dew_tmin_c <- dew_tmax_c <- tair_min_c <- tair_max_c <- dry_bulb_t_c <- NULL
  rainfall_mm <- rh_max_porc <- rh_min_porc <- rh_mean_porc <- NULL
  ws_10_m_s <- ws_gust_m_s <- wd_degrees <- sr_kj_m2 <- sr_mj_m2 <- NULL
  date_hour<- UTC_offset <- date_hour_local <- NULL
  
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
          lat_part <- substr(coord, 1, 3)
          dec_part <- substr(coord, 5, 10)
          as.numeric(paste0(lat_part, ".", dec_part))}
        
        # Extrai e converte os valores desejados
        latitude <- convert_coord(header_info[5, 2])
        longitude <- convert_coord(header_info[6, 2])
        
        # Ajuste da altitude
        altitude <- as.numeric(gsub(",", ".", header_info[7, 2]))

        names(dfx) <- c(
          "date", "hour", "rainfall_mm", "patm_mb",
          "patm_max_mb", "patm_min_mb", "sr_kj_m2",
          "dry_bulb_t_c", "dew_tmean_c", "tair_max_c", "tair_min_c", "dew_tmax_c",
          "dew_tmin_c", "rh_max_porc", "rh_min_porc", "rh_mean_porc", "wd_degrees",
          "ws_gust_m_s", "ws_10_m_s", "X"
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
        
        
#estudar melhor essa condicao
       # if (nrow(dfx) < 4380 & diff_days > 120) {} else {
          #dfx_temp <- na.omit(dplyr::select(dfx, hour, date, dew_tmin_c, dew_tmax_c, tair_min_c, tair_max_c, dry_bulb_t_c))
          dfx_temp <- dplyr::select(dfx, hour, date, dew_tmin_c, dew_tmax_c, tair_min_c, tair_max_c, dry_bulb_t_c)
         
           n_dfx_temp <- dplyr::group_by(dfx_temp, date) |>
            dplyr::summarise(n = n()) |>
            dplyr::filter(n == 24)
          
          if (nrow(n_dfx_temp) == 0) {} else {
            dfx_temp <- dplyr::left_join(dfx_temp, n_dfx_temp, by = "date")
            dfx_temp <- dplyr::filter(dfx_temp, n == 24)
            dfx_temp <- dplyr::mutate(dfx_temp, tair_mean_c = ((tair_min_c + tair_max_c) / 2))
            dfx_temp <- dplyr::mutate(dfx_temp, dew_tmean_c = ((dew_tmin_c + dew_tmax_c) / 2))
            
            dfx_temp_mean_day <- stats::aggregate(tair_mean_c ~ date, dfx_temp, mean)
            dfx_temp_min_day <- stats::aggregate(tair_min_c ~ date, dfx_temp, min)
            dfx_temp_max_day <- stats::aggregate(tair_max_c ~ date, dfx_temp, max)
            dfx_to_min_day <- stats::aggregate(dew_tmin_c ~ date, dfx_temp, min)
            dfx_to_max_day <- stats::aggregate(dew_tmax_c ~ date, dfx_temp, max)
            dfx_to_mean_day <- stats::aggregate(dew_tmean_c ~ date, dfx_temp, mean)
            dfx_tbs_day <- stats::aggregate(dry_bulb_t_c ~ date, dfx_temp, mean)
            
            dfx_temps_day <- dfx_temp_mean_day %>%
              left_join(dfx_temp_min_day, by = "date") %>%
              left_join(dfx_temp_max_day, by = "date") %>%
              left_join(dfx_to_mean_day, by = "date") %>%
              left_join(dfx_to_min_day, by = "date") %>%
              left_join(dfx_to_max_day, by = "date") %>%
              left_join(dfx_tbs_day, by = "date")}
          
            #dfx_prec <- na.omit(dplyr::select(dfx, hour, date, rainfall_mm))
            dfx_prec <- dplyr::select(dfx, hour, date, rainfall_mm)
            dfx_prec <- dplyr::group_by(dfx_prec, date)
            
            if (nrow(dfx_prec) == 0) {} else {
              
              dfx_prec_day <- stats::aggregate(rainfall_mm ~ date, dfx_prec, sum)
              
            }
            
             #dfx_press <- na.omit(dplyr::select(dfx, hour, date, patm_mb))
              dfx_press <- dplyr::select(dfx, hour, date, patm_mb)
              
              n_dfx_press <- dplyr::group_by(dfx_press, date) |>
                              dplyr::summarise(n = n()) |>
                              dplyr::filter(n == 24)
              
              if (nrow(n_dfx_press) == 0) {} else {
                dfx_press <- dplyr::left_join(dfx_press, n_dfx_press, by = "date")
                dfx_press <- dplyr::filter(dfx_press, n == 24)
                
                dfx_press_mean_day <- stats::aggregate(patm_mb ~ date, dfx_press, mean)}
              
                #dfx_ur <- na.omit(dplyr::select(dfx, hour, date, rh_max_porc, rh_min_porc, rh_mean_porc))
                dfx_ur <- dplyr::select(dfx, hour, date, rh_max_porc, rh_min_porc, rh_mean_porc)
                
                n_dfx_ur <- dplyr::group_by(dfx_ur, date) |>
                  dplyr::summarise(n = n()) |>
                  dplyr::filter(n == 24)
                
                if (nrow(n_dfx_ur) == 0) {} else {
                  dfx_ur <- dplyr::left_join(dfx_ur, n_dfx_ur, by = "date")
                  dfx_ur <- dplyr::filter(dfx_ur, n == 24)
                  
                  dfx_ur_mean_day <- stats::aggregate(rh_mean_porc ~ date, dfx_ur, mean)
                  dfx_ur_min_day <- aggregate(rh_min_porc ~ date, dfx_ur, min)
                  
                  dfx_ur_max_day <- stats::aggregate(rh_max_porc ~ date, dfx_ur, max)
                  
                  dfx_urs_day <- dfx_ur_mean_day|>
                    dplyr::left_join(dfx_ur_max_day, by = "date")|>
                    dplyr::left_join(dfx_ur_min_day, by = "date")}
      
                  #dfx_vv <- na.omit(dplyr::select(dfx, hour, date, ws_10_m_s, ws_gust_m_s, wd_degrees))
                  dfx_vv <- dplyr::select(dfx, hour, date, ws_10_m_s, ws_gust_m_s, wd_degrees)
                  
                  n_dfx_vv <- dplyr::group_by(dfx_vv, date) |>
                    dplyr::summarise(n = n()) |>
                    dplyr::filter(n == 24)
                  
                  if (nrow(n_dfx_vv) == 0) {} else {
                    dfx_vv <- dplyr::left_join(dfx_vv, n_dfx_vv, by = "date")
                    dfx_vv <- dplyr::filter(dfx_vv, n == 24)
                    dfx_vv <- dplyr::mutate(dfx_vv, u2 = (4.868 / (log(67.75 *10 - 5.42))) * ws_10_m_s)
                    
                    dfx_vv_mean_day <- aggregate(ws_10_m_s ~ date, dfx_vv, mean)
                    dfx_vv_meanu2_day <- aggregate(u2 ~ date, dfx_vv, mean)
                    dfx_vv_raj_day <- stats::aggregate(ws_gust_m_s ~ date, dfx_vv, max)
                    
                    dfx_vv_dir_day <- stats::aggregate(wd_degrees ~ date, dfx_vv, mean)
                    
                    dfx_vvs_day <- dfx_vv_mean_day|>
                      dplyr::left_join(dfx_vv_meanu2_day, by = "date")|>
                      dplyr::left_join(dfx_vv_raj_day, by = "date")|>
                      dplyr::left_join(dfx_vv_dir_day, by = "date")}
                    
                    dfx_RG <- dplyr::select(dfx, hour, date, sr_kj_m2)
                    
                    dfx_RG <- dplyr::mutate(dfx_RG, sr_mj_m2 = sr_kj_m2 / 1000)
                    #dfx_RG <- na.omit(dplyr::select(dfx_RG, sr_kj_m2))
                    dfx_RG <- dplyr::select(dfx_RG, date, sr_mj_m2)########
                    
                    dfx_RG <- dplyr::filter(dfx_RG, sr_mj_m2 > 0)
                    
                    n_RG <- dplyr::group_by(dfx_RG, date) |>
                      summarise(n = n()) |>
                      filter(n >= 12)
                    
                    if (nrow(n_RG) == 0) {} else {
                      dfx_RG <- dplyr::left_join(dfx_RG, n_RG, by = "date")
                      dfx_RG <- dplyr::filter(dfx_RG, n >= 12)
                      
                      dfx_RG_sum_day <- aggregate(sr_mj_m2 ~ date, dfx_RG, sum)
                      
                      dfx_RG_sum_day <- dfx_RG_sum_day |> 
                        dplyr::mutate(julian_day = as.numeric(format(date, "%j")))
                      
                      lat_rad <- (pi / 180) * (latitude)
                      
                      dr <- 1 + 0.033 * cos((2 * pi / 365) *
                        dfx_RG_sum_day$julian_day)

                      solar_declination <- 0.409 * sin(((2 *pi / 365) * dfx_RG_sum_day$julian_day) - 1.39)
                      sunset_hour_angle <- acos(-tan(lat_rad) * tan(solar_declination))
                      
                      ra <- ((24 * (60)) / pi) * (0.082) *
                        dr * (sunset_hour_angle * sin(lat_rad) *
                          sin(solar_declination) + cos(lat_rad) *
                            cos(solar_declination) * sin(sunset_hour_angle))
                     
                      ra <- as.data.frame(ra)
                      dfx_RG_sum_day <- dplyr::bind_cols(dfx_RG_sum_day, ra)}
                        
                      
                    dfx_day <- dplyr::full_join(dfx_temps_day, dfx_prec_day, by = "date")
                      dfx_day <- dplyr::full_join(dfx_day, dfx_press_mean_day, by = "date")
                      dfx_day <- dplyr::full_join(dfx_day, dfx_urs_day, by = "date")
                      dfx_day <- dplyr::full_join(dfx_day, dfx_vvs_day, by = "date")
                      dfx_day <- dplyr::full_join(dfx_day, dfx_RG_sum_day, by = "date")
                      dfx_day <- dplyr::mutate(dfx_day, OMM = OMM)
                      df <- dfx_day
        
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
                        "ws_2_m_s" = "u2",
                        "ra_mj_m2" = "ra")|>
          dplyr::select(c(station_code,
                          station,
                          uf,
                          date,
                          tair_mean_c,
                          tair_min_c,
                          tair_max_c,
                          dew_tmean_c,
                          dew_tmin_c,
                          dew_tmax_c,
                          dry_bulb_t_c,
                          rainfall_mm,
                          patm_mb,
                          rh_mean_porc,
                          rh_max_porc,
                          rh_min_porc,
                          ws_10_m_s,
                          ws_2_m_s,
                          ws_gust_m_s,
                          wd_degrees,
                          sr_mj_m2,
                          ra_mj_m2,
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
