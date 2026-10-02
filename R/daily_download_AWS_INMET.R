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
#' @importFrom dplyr distinct
#' @importFrom dplyr bind_rows
#' @importFrom lubridate hours
#' @examples
#' \dontrun{
#' df <- download_AWS_INMET_daily(
#'   stations = c("A001", "A042"),
#'   start_date = "2016-01-01",
#'   end_date = "2018-12-31"
#' )
#' }
#' @export
#' @return Returns a data.frame with the AWS data requested
#' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha

download_AWS_INMET_daily <- function(stations, start_date, end_date) {
  # Desabilitar mensagens verbosas do tidyverse/vctrs
  old_opts <- options(
    rlib_message_verbosity = "quiet",
    readr.show_col_types = FALSE,
    readr.show_progress = FALSE
  )
  on.exit(options(old_opts), add = TRUE)
  
  # Validação de inputs
  if (missing(stations) || length(stations) == 0) {
    stop("Parameter 'stations' must be provided and cannot be empty.")
  }
  if (missing(start_date) || missing(end_date)) {
    stop("Parameters 'start_date' and 'end_date' must be provided.")
  }
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", start_date) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", end_date)) {
    stop("Dates must be in format 'YYYY-MM-DD'.")
  }
  if (as.Date(start_date) > as.Date(end_date)) {
    stop("'start_date' cannot be after 'end_date'.")
  }
  
  X <- patm_max_mb <- patm_min_mb <- hour <- NULL
  dew_tmin_c <- dew_tmax_c <- tair_min_c <- tair_max_c <- tair_dry_bulb_c <- NULL
  rainfall_mm <- rh_max_porc <- rh_min_porc <- rh_mean_porc <- NULL
  ws_2_m_s <- ws_gust_m_s <- wd_degrees <- sr_kj_m2 <- sr_mj_m2 <- NULL
  date_hour <- UTC_offset <- date_hour_local <- NULL

  altitude_m <- dew_tmean_c <- latitude_degrees <- longitude_degrees <- patm_mb <- NULL
  ra_mj_m2 <- station_code <- tair_mean_c <- uf <- ws_2_m_s <- NULL

  start_year <- substr(start_date, 1, 4)
  end_year <- substr(end_date, 1, 4)

  df_sequence <- list()
  seq_idx <- 1

  for (year in seq(from = as.numeric(start_year), to = as.numeric(end_year))) {
    message("Downloading data for: ", year)

    tempdir <- tempfile()
    tf <- paste0(gsub("\\", "/", tempdir, fixed = TRUE), ".zip")
    outdir <- gsub("\\", "/", tempdir, fixed = TRUE)

    download_success <- .download_file(
      url = paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", year, ".zip"),
      destfile = tf
    )
    
    if (!download_success) {
      next
    }

    # Tentar unzip com tratamento de erro
    unzip_success <- tryCatch({
      a <- unzip(zipfile = tf, exdir = outdir, junkpaths = T)
      TRUE
    }, error = function(e) {
      warning("Failed to unzip file for year ", year, ": ", e$message)
      FALSE
    })
    
    if (!unzip_success) {
      next
    }

    df_all_stations <- list()
    station_idx <- 1

    for (station in stations) {
      df <- NULL  # Inicializar df para cada estação
      station_file <- list.files(outdir, pattern = station, full.names = T, all.files = T)

      if (length(station_file) == 0) {
        message("There is no data for this period for this station. Choose another period!")
      } else {
        # Verificar se há múltiplos arquivos para a mesma estação
        if (length(station_file) > 1) {
          message(paste0("Found ", length(station_file), " files for station ", station, " in year ", year, ". Processing all files..."))
        }
        
        # Processar cada arquivo encontrado
        dfx_list <- list()
        header_info <- NULL
        OMM <- NULL
        UF <- NULL
        station_name <- NULL
        latitude <- NULL
        longitude <- NULL
        altitude <- NULL
        
        for (i in seq_along(station_file)) {
          # Ler dados do arquivo - suprimindo todas as mensagens e warnings
          invisible(capture.output({
            dfx_temp <- suppressWarnings(suppressMessages(read.csv(
              file = station_file[i],
              header = T,
              sep = ";",
              skip = 8,
              na = "-9999",
              dec = ",",
              check.names = F
            )))
          }))
          
          dfx_list[[i]] <- dfx_temp
          
          # Ler informações do cabeçalho do primeiro arquivo
          if (i == 1) {
            invisible(capture.output({
              header_info <- suppressWarnings(suppressMessages(read.csv(file = station_file[i], header = F, sep = ";")))
            }))
            
            OMM <- header_info[4, 2]
            UF <- header_info[2, 2]
            station_name <- header_info[3, 2]
            
            # Função para converter coordenadas no formato correto
            convert_coord <- function(coord) {
              lat_part <- sub(",.*", "", coord)
              dec_part <- sub(".*,", "", coord)
              as.numeric(paste0(lat_part, ".", dec_part))
            }
            
            # Extrai e converte os valores desejados
            latitude <- convert_coord(header_info[5, 2])
            longitude <- convert_coord(header_info[6, 2])
            
            # Ajuste da altitude
            altitude <- as.numeric(gsub(",", ".", header_info[7, 2]))
          }
        }
        
        # Combinar todos os dados dos arquivos
        invisible(capture.output({
          dfx <- suppressWarnings(dplyr::bind_rows(dfx_list))
        }))
        
        # Usar o nome da estação do cabeçalho
        station <- station_name

        names(dfx) <- c(
          "date", "hour", "rainfall_mm", "patm_mb",
          "patm_max_mb", "patm_min_mb", "sr_kj_m2",
          "tair_dry_bulb_c", "dew_tmean_c", "tair_max_c", "tair_min_c", "dew_tmax_c",
          "dew_tmin_c", "rh_max_porc", "rh_min_porc", "rh_mean_porc", "wd_degrees",
          "ws_gust_m_s", "ws_2_m_s", "X"
        )

        dfx <- dplyr::select(dfx, -X, -patm_max_mb, -patm_min_mb)
        dfx <- tibble::as_tibble(dfx)
        dfx <- dplyr::mutate(dfx,
          date = as.Date(date),
          hour = as.numeric(substr(hour, 1, 2))
        )
        
        # Remover duplicatas antes de processar (importante quando há múltiplos arquivos)
        dfx <- dfx %>%
          dplyr::distinct(date, hour, .keep_all = TRUE)

        dfx$date_hour <- paste0(dfx$date, " ", dfx$hour)
        dfx$date_hour <- as.POSIXct(strptime(dfx$date_hour, format = "%Y-%m-%d %H"))

        dfx <- dfx %>%
          dplyr::mutate(
            # Define o offset por estado
            UTC_offset = case_when(
              UF == "AC" ~ -5, # UTC-5 (Acre)
              UF %in% c("AM", "MT", "RO", "RR") ~ -4, # UTC-4 (Amazonas, Mato Grosso, Rondônia, Roraima)
              UF %in% c(
                "MS", "GO", "DF", "TO", "BA", "SE", "AL", "PE", "PB",
                "RN", "CE", "PI", "MA", "PA", "AP", "SP", "RJ", "MG", "ES",
                "PR", "SC", "RS"
              ) ~ -3, # UTC-3 (Maior parte do Brasil)
              TRUE ~ 0 # Caso não encontre a UF, mantém UTC
            ),
            # Ajusta para horário local
            date_hour_local = date_hour + hours(UTC_offset)
          ) %>%
          dplyr::mutate(
            # Extraindo a data e a hora corretamente
            date = as.POSIXct(strptime(date_hour_local, format = "%Y-%m-%d")), # Apenas a data
            hour = format(date_hour_local, "%H:%M:%S") # Apenas a hora
          ) %>%
          select(-UTC_offset)

        agg_safe_fillna <- function(df, formula, fun, ...) {
          var <- all.vars(formula)[1]
          group_var <- all.vars(formula)[2]
          
          if (all(is.na(df[[var]]))) {
            dates <- unique(df[[group_var]])
            return(data.frame(date = dates, tmp = NA_real_)) |>
              stats::setNames(c(group_var, var))
          } else {
            return(stats::aggregate(formula, df, fun, ...))
          }
        }
        
        dfx_temp <- dplyr::select(dfx, hour, date, dew_tmin_c, dew_tmean_c, dew_tmax_c, tair_min_c, tair_max_c, tair_dry_bulb_c)

        # Filtra linhas que não têm todos os campos relevantes como NA
        dfx_temp <- dfx_temp %>%
          dplyr::filter(!(is.na(tair_min_c) & is.na(tair_max_c) & is.na(tair_dry_bulb_c) & is.na(dew_tmin_c) & is.na(dew_tmean_c) & is.na(dew_tmax_c)))

        n_dfx_temp <- dplyr::group_by(dfx_temp, date) |>
          dplyr::summarise(n = n()) |>
          dplyr::filter(n == 24)

        if (nrow(n_dfx_temp) == 0) {
          message(paste0("No valid data for this period in this station: ", OMM, " - year ", year, " - Group of air temperature variables"))
        } else {
          dfx_temp <- dplyr::left_join(dfx_temp, n_dfx_temp, by = "date")
          dfx_temp <- dplyr::filter(dfx_temp, n == 24)

          dfx_temp_mean_day <- agg_safe_fillna(dfx_temp, tair_dry_bulb_c ~ date, mean, na.rm = TRUE)
          names(dfx_temp_mean_day)[2] <- "tair_dry_bulb_c"
          dfx_temp_min_day  <- agg_safe_fillna(dfx_temp, tair_min_c ~ date, min, na.rm = TRUE)
          names(dfx_temp_min_day)[2] <- "tair_min_c"
          dfx_temp_max_day  <- agg_safe_fillna(dfx_temp, tair_max_c ~ date, max, na.rm = TRUE)
          names(dfx_temp_max_day)[2] <- "tair_max_c"
          dfx_to_min_day    <- agg_safe_fillna(dfx_temp, dew_tmin_c ~ date, min, na.rm = TRUE)
          names(dfx_to_min_day)[2] <- "dew_tmin_c"
          dfx_to_max_day    <- agg_safe_fillna(dfx_temp, dew_tmax_c ~ date, max, na.rm = TRUE)
          names(dfx_to_max_day)[2] <- "dew_tmax_c"
          dfx_to_mean_day   <- agg_safe_fillna(dfx_temp, dew_tmean_c ~ date, mean, na.rm = TRUE)
          names(dfx_to_mean_day)[2] <- "dew_tmean_c"
          
          joins <- list(dfx_temp_min_day, dfx_temp_max_day, dfx_to_mean_day, dfx_to_min_day, dfx_to_max_day)
          
          dfx_temps_day <- dfx_temp_mean_day
          
          for (j in joins) {
            if (!is.null(j)) {
              dfx_temps_day <- left_join(dfx_temps_day, j, by = "date")
            }
          }
          
          dfx_temps_day <- dfx_temps_day %>%
            dplyr::rename("tair_mean_c" = "tair_dry_bulb_c")
        }

        dfx_prec <- dplyr::select(dfx, hour, date, rainfall_mm)
        dfx_prec <- dplyr::group_by(dfx_prec, date)

        # Filtra linhas que não têm todos os campos relevantes como NA
        dfx_prec <- dfx_prec %>%
          dplyr::filter(!(is.na(rainfall_mm)))

        if (nrow(dfx_prec) == 0) {
          message(paste0("No valid data for this period in this station: ", OMM, " - year ", year, " - Rainfall group"))
        } else {
          dfx_prec_day   <- agg_safe_fillna(dfx_prec, rainfall_mm ~ date, sum, na.rm = TRUE)
          names(dfx_prec_day)[2] <- "rainfall_mm"
        }

        dfx_press <- dplyr::select(dfx, hour, date, patm_mb)

        dfx_press <- dfx_press %>%
          dplyr::filter(!(is.na(patm_mb)))

        n_dfx_press <- dplyr::group_by(dfx_press, date) |>
          dplyr::summarise(n = n()) |>
          dplyr::filter(n == 24)

        if (nrow(n_dfx_press) == 0) {
          message(paste0("No valid data for this period in this station: ", OMM, " - year ", year, " - Atmosphere pressure group"))
        } else {
          dfx_press <- dplyr::left_join(dfx_press, n_dfx_press, by = "date")
          dfx_press <- dplyr::filter(dfx_press, n == 24)

          dfx_press_mean_day   <- agg_safe_fillna(dfx_press, patm_mb ~ date, mean, na.rm = TRUE)
          names(dfx_press_mean_day)[2] <- "patm_mb"
        }

        dfx_ur <- dplyr::select(dfx, hour, date, rh_max_porc, rh_min_porc, rh_mean_porc)

        # Filtra linhas que não têm todos os campos relevantes como NA
        dfx_ur <- dfx_ur %>%
          dplyr::filter(!(is.na(rh_max_porc)) & !(is.na(rh_min_porc)) & !(is.na(rh_mean_porc)))

        n_dfx_ur <- dplyr::group_by(dfx_ur, date) |>
          dplyr::summarise(n = n()) |>
          dplyr::filter(n == 24)

        if (nrow(n_dfx_ur) == 0) {
          message(paste0("No valid data for this period in this station: ", OMM, " - year ", year, " - Relative Humidity group"))
        } else {
          dfx_ur <- dplyr::left_join(dfx_ur, n_dfx_ur, by = "date")
          dfx_ur <- dplyr::filter(dfx_ur, n == 24)

          dfx_ur_mean_day <- agg_safe_fillna(dfx_ur, rh_mean_porc ~ date, mean, na.rm = TRUE)
          names(dfx_ur_mean_day)[2] <- "rh_mean_porc"
          dfx_ur_min_day <- agg_safe_fillna(dfx_ur, rh_min_porc ~ date, min, na.rm = TRUE)
          names(dfx_ur_min_day)[2] <- "rh_min_porc"
          dfx_ur_max_day <- agg_safe_fillna(dfx_ur, rh_max_porc ~ date, max, na.rm = TRUE)
          names(dfx_ur_max_day)[2] <- "rh_max_porc"

          joins <- list(dfx_ur_min_day, dfx_ur_max_day)
          
          dfx_urs_day <- dfx_ur_mean_day
          
          for (j in joins) {
            if (!is.null(j)) {
              dfx_urs_day <- left_join(dfx_urs_day, j, by = "date")
            }
          }
        }

        dfx_vv <- dplyr::select(dfx, hour, date, ws_2_m_s, ws_gust_m_s, wd_degrees)

        dfx_vv <- dfx_vv %>%
          dplyr::filter(!(is.na(ws_2_m_s)) & !(is.na(ws_gust_m_s)) & !(is.na(wd_degrees)))

        n_dfx_vv <- dplyr::group_by(dfx_vv, date) |>
          dplyr::summarise(n = n()) |>
          dplyr::filter(n == 24)

        if (nrow(n_dfx_vv) == 0) {
          message(paste0("No valid data for this period in this station: ", OMM, " - year ", year, " - wind speed group"))
        } else {
          dfx_vv <- dplyr::left_join(dfx_vv, n_dfx_vv, by = "date")
          dfx_vv <- dplyr::filter(dfx_vv, n == 24)

          dfx_vv_mean_day <- agg_safe_fillna(dfx_vv, ws_2_m_s ~ date, mean, na.rm = TRUE)
          names(dfx_vv_mean_day)[2] <- "ws_2_m_s"
          dfx_vv_raj_day <- agg_safe_fillna(dfx_vv, ws_gust_m_s ~ date, max, na.rm = TRUE)
          names(dfx_vv_raj_day)[2] <- "ws_gust_m_s"
          dfx_vv_dir_day <- agg_safe_fillna(dfx_vv, wd_degrees ~ date, mean, na.rm = TRUE)
          names(dfx_vv_dir_day)[2] <- "wd_degrees"
          
          joins <- list(dfx_vv_raj_day, dfx_vv_dir_day)
          
          dfx_vvs_day <- dfx_vv_mean_day
          
          for (j in joins) {
            if (!is.null(j)) {
              dfx_vvs_day <- left_join(dfx_vvs_day, j, by = "date")
            }
          }
        }

        dfx_RG <- dplyr::select(dfx, hour, date, sr_kj_m2)

        dfx_RG <- dfx_RG %>%
          dplyr::filter(!(is.na(sr_kj_m2)))

        dfx_RG <- dplyr::mutate(dfx_RG, sr_mj_m2 = sr_kj_m2 / 1000)
        dfx_RG <- dplyr::select(dfx_RG, date, sr_mj_m2)

        dfx_RG <- dplyr::filter(dfx_RG, sr_mj_m2 > 0)

        n_RG <- dplyr::group_by(dfx_RG, date) |>
          summarise(n = n()) |>
          filter(n >= 12)

        if (nrow(n_RG) == 0) {
          message(paste0("No valid data for this period in this station: ", OMM, " - year ", year, " - Radiation group"))
        } else {
          dfx_RG <- dplyr::left_join(dfx_RG, n_RG, by = "date")
          dfx_RG <- dplyr::filter(dfx_RG, n >= 12)

          dfx_RG_sum_day <- agg_safe_fillna(dfx_RG, sr_mj_m2 ~ date, sum, na.rm = TRUE)
          names(dfx_RG_sum_day)[2] <- "sr_mj_m2"
          
          dfx_RG_sum_day <- dfx_RG_sum_day |>
            dplyr::mutate(julian_day = as.numeric(format(date, "%j")))

          lat_rad <- (pi / 180) * (latitude)

          dr <- 1 + 0.033 * cos((2 * pi / 365) *
            dfx_RG_sum_day$julian_day)

          solar_declination <- 0.409 * sin(((2 * pi / 365) * dfx_RG_sum_day$julian_day) - 1.39)
          sunset_hour_angle <- acos(-tan(lat_rad) * tan(solar_declination))

          ra <- ((24 * (60)) / pi) * (0.082) *
            dr * (sunset_hour_angle * sin(lat_rad) *
              sin(solar_declination) + cos(lat_rad) *
                cos(solar_declination) * sin(sunset_hour_angle))

          ra <- as.data.frame(ra)
          dfx_RG_sum_day <- dplyr::bind_cols(dfx_RG_sum_day, ra)
        }



        if (exists("dfx_temps_day") && nrow(dfx_temps_day) > 0 &&
          exists("dfx_prec_day") && nrow(dfx_prec_day) > 0 &&
          exists("dfx_press_mean_day") && nrow(dfx_press_mean_day) > 0 &&
          exists("dfx_urs_day") && nrow(dfx_urs_day) > 0 &&
          exists("dfx_vvs_day") && nrow(dfx_vvs_day) > 0 &&
          exists("dfx_RG_sum_day") && nrow(dfx_RG_sum_day) > 0) {
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
              altitude_m = altitude
            ) |>
            dplyr::arrange(station, date) |>
            dplyr::rename(
              "station_code" = "OMM",
              "uf" = "UF",
              "ra_mj_m2" = "ra"
            ) |>
            dplyr::select(c(
              station_code,
              station,
              uf,
              date,
              tair_mean_c,
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
              sr_mj_m2,
              ra_mj_m2,
              longitude_degrees,
              latitude_degrees,
              altitude_m
            ))
        } else {}
      }

      if (!is.null(df) && nrow(df) > 0) {
        df_all_stations[[station_idx]] <- df
        station_idx <- station_idx + 1
      }
    }

    if (length(df_all_stations) > 0) {
      df_sequence[[seq_idx]] <- dplyr::bind_rows(df_all_stations)
      seq_idx <- seq_idx + 1
    }
  }

  # Combinar todos os dados e remover duplicatas
  if (length(df_sequence) == 0) {
    warning("No data was downloaded for the specified stations and period.")
    return(data.frame())
  }
  
  df_final <- dplyr::bind_rows(df_sequence)
  
  # Remover duplicatas mantendo a primeira ocorrência
  df_final <- df_final %>%
    dplyr::distinct(station_code, date, .keep_all = TRUE) %>%
    dplyr::arrange(station_code, date)
  
  return(df_final)
}
