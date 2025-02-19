#' Download of hourly data from automatic weather stations (AWS) of INMET-Brazil in daily aggregates
#' @description This function will download the hourly AWS data of INMET and it will aggregate the data in a daily time scale, based on the period of time selected (start_date and end_date).The function only works for downloading data from the same year.
#' @param station The station code (ID - WMO code) for download. To see the station ID, please see the function *see_stations_info*.
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
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' df <- download_AWS_INMET_daily(station = "A001", start_date = "2001-01-01", end_date = "2001-12-31")
#' }
#' @export
#' @return Returns a data.frame with the AWS data requested
#' @author Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F. da Cunha


download_AWS_INMET_daily <- function(station, start_date, end_date) {
  start_year <- substr(start_date, 1, 4)
  end_year <- substr(end_date, 1, 4)

  if (as.numeric(end_year) - as.numeric(start_year) == 0) {
    tempdir <- tempfile()
    tf <- paste0(gsub("\\", "/", tempdir, fixed = TRUE), ".zip")
    outdir <- gsub("\\", "/", tempdir, fixed = TRUE)

    utils::download.file(url = paste0(
      "https://portal.inmet.gov.br/uploads/dadoshistoricos/",
      year, ".zip"
    ), destfile = tf, method = "auto", cacheOK = F)

    a <- unzip(zipfile = tf, exdir = outdir, junkpaths = T)
    pasta <- paste0(outdir)

    if (length(list.files(pasta,
      pattern = station, full.names = T,
      all.files = T
    )) == 0) {
      message("There is no data for this period for this station. Choose another period!")
    } else {
      list.files(pasta, pattern = station)
      b <- read.csv(
        file = list.files(pasta,
          pattern = station,
          full.names = T
        ), header = T, sep = ";", skip = 8,
        na = "-9999", dec = ",", check.names = F
      )
      a <- as.data.frame(a)
      X <- `pressao-max(mB)` <- `pressao-min(mB)` <- Data <- Hora <- NULL
      `to-min(C)` <- `to-max(C)` <- `tar_min(C)` <- `tar_max(C)` <- `tbs(C)` <- NULL
      `ppt-h (mm)` <- `pressao (mB)` <- `UR-max` <- `UR-min` <- UR <- NULL
      `U10 (m/s)` <- `U-raj (m/s)` <- `U-dir(degrees)` <- `RG(Kj/m2)` <- `RG(Mj/m2)` <- NULL
      df <- data.frame(matrix(ncol = 18, nrow = 0))
      colnames(df) <- c(
        "Data", "Hora", "ppt-h (mm)", "pressao (mB)",
        "RG(Mj/m2)", "tbs-(C)", "tpo(C)", "tar_max(C)", "tar_min(C)",
        "to-max(C)", "to-min(C)", "UR-max", "UR-min", "UR",
        "U-dir(degrees)", "U-raj (m/s)", "U10 (m/s)", "OMM"
      )
      OMM <- read.csv(file = list.files(pasta,
        pattern = station,
        full.names = T
      ), header = F, sep = ";")
      OMM <- (OMM[4, 2])

      UF <- read.csv(file = list.files(pasta, pattern = station, full.names = T), header = F, sep = ";")
      UF <- (UF[2, 2])

      Station <- read.csv(file = list.files(pasta, pattern = station, full.names = T), header = F, sep = ";")
      Station <- (Station[3, 2])

      dfx <- read.csv(
        file = list.files(pasta,
          pattern = station,
          full.names = T
        ), header = T, sep = ";", skip = 8,
        na = "-9999", dec = ",", check.names = F
      )
      names(dfx)
      names(dfx) <- c(
        "Data", "Hora", "ppt-h (mm)", "pressao (mB)",
        "pressao-max(mB)", "pressao-min(mB)", "RG(Kj/m2)",
        "tbs(C)", "tpo(C)", "tar_max(C)", "tar_min(C)", "to-max(C)",
        "to-min(C)", "UR-max", "UR-min", "UR", "U-dir(degrees)",
        "U-raj (m/s)", "U10 (m/s)", "X"
      )
      latitude <- read.csv(file = list.files(pasta,
        pattern = station,
        full.names = T
      ), header = F, sep = ";", dec = ",")
      latitude <- latitude[5, 2]
      lat <- substr(latitude, 1, 3)
      itude <- substr(latitude, 5, 10)
      latitude <- as.numeric(paste0(lat, ".", itude))
      longitude <- read.csv(file = list.files(pasta,
        pattern = station,
        full.names = T
      ), header = F, sep = ";", dec = ",")
      longitude <- longitude[6, 2]
      long <- substr(longitude, 1, 3)
      itude <- substr(longitude, 5, 10)
      longitude <- as.numeric(paste0(long, ".", itude))
      altitude <- read.csv(file = list.files(pasta,
        pattern = station,
        full.names = T
      ), header = F, sep = ";", dec = ",")
      altitude <- altitude[7, 2]
      altitude <- gsub(",", replacement = ".", altitude)
      altitude <- as.numeric(altitude)
      dfx <- dplyr::select(dfx, -X, -`pressao-max(mB)`, -`pressao-min(mB)`)
      dfx <- tibble::as_tibble(dfx)
      dfx <- dplyr::mutate(dfx, Data = as.Date(Data), Hora = as.numeric(as.factor(Hora)))
      dfx$date_hora <- paste0(dfx$Data, dfx$Hora)
      dfx$date_hora <- as.POSIXct(strptime(dfx$date_hora, format = "%Y-%m-%d %H"))
      for (i in 1:nrow(dfx)) {
        if (longitude > -37.5) {
          (dfx$date_hora[i] <- dfx$date_hora[i] - as.difftime(2,
            units = "hours"
          ))
        } else if (longitude > -52.5) {
          (dfx$date_hora[i] <- dfx$date_hora[i] - as.difftime(3,
            units = "hours"
          ))
        } else if (longitude > -67.5) {
          (dfx$date_hora[i] <- dfx$date_hora[i] - as.difftime(4,
            units = "hours"
          ))
        } else if (longitude > -82.5) {
          (dfx$date_hora[i] <- dfx$date_hora[i] - as.difftime(5,
            units = "hours"
          ))
        }
      }
      dfx$Data <- as.POSIXct(strptime(dfx$date_hora, format = "%Y-%m-%d"))
      dfx$Hora <- format(
        as.POSIXct(dfx$date_hora, format = "%Y-%m-%d %H"),
        "%H"
      )
      diff_days <- as.Date(end_date) - as.Date(start_date)

      if (nrow(dfx) < 4380 & diff_days > 120) {
      } else {
        dfx_temp <- na.omit(dplyr::select(
          dfx, Hora, Data,
          `to-min(C)`, `to-max(C)`, `tar_min(C)`, `tar_max(C)`,
          `tbs(C)`
        ))
        n_dfx_temp <- group_by(dfx_temp, Data) %>%
          summarise(n = n()) %>%
          filter(n == 24)
        if (nrow(n_dfx_temp) == 0) {
        } else {
          dfx_temp <- left_join(dfx_temp, n_dfx_temp, by = "Data")
          dfx_temp <- dplyr::filter(dfx_temp, n == 24)
          dfx_temp <- dplyr::mutate(dfx_temp, tar_mean = (`tar_min(C)` +
            `tar_max(C)`) / 2)
          dfx_temp <- dplyr::mutate(dfx_temp, to_mean = (`to-min(C)` +
            `to-max(C)`) / 2)
          dfx_temp_mean_day <- aggregate(
            tar_mean ~ Data,
            dfx_temp, mean
          )
          dfx_temp_min_day <- aggregate(`tar_min(C)` ~
            Data, dfx_temp, min)
          dfx_temp_max_day <- aggregate(`tar_max(C)` ~
            Data, dfx_temp, max)
          dfx_to_min_day <- aggregate(
            `to-min(C)` ~ Data,
            dfx_temp, min
          )
          dfx_to_max_day <- aggregate(
            `to-max(C)` ~ Data,
            dfx_temp, max
          )
          dfx_to_mean_day <- aggregate(
            to_mean ~ Data,
            dfx_temp, mean
          )
          dfx_tbs_day <- aggregate(
            `tbs(C)` ~ Data, dfx_temp,
            mean
          )
          dfx_temps_day <- cbind(
            dfx_temp_mean_day, dfx_temp_min_day,
            dfx_temp_max_day, dfx_to_mean_day, dfx_to_min_day,
            dfx_to_max_day, dfx_tbs_day
          )
          dfx_temps_day <- dplyr::select(
            dfx_temps_day,
            -3, -5, -7, -9, -11, -13
          )
          dfx_prec <- na.omit(dplyr::select(
            dfx, Hora,
            Data, `ppt-h (mm)`
          ))
          dfx_prec <- group_by(dfx_prec, Data)
          if (nrow(dfx_prec) == 0) {
          } else {
            dfx_prec_day <- aggregate(
              `ppt-h (mm)` ~ Data,
              dfx_prec, sum
            )
            dfx_press <- na.omit(dplyr::select(
              dfx, Hora,
              Data, `pressao (mB)`
            ))
            n_dfx_press <- group_by(dfx_press, Data) %>%
              summarise(n = n()) %>%
              filter(n == 24)
            if (nrow(n_dfx_press) == 0) {
            } else {
              dfx_press <- left_join(dfx_press, n_dfx_press,
                by = "Data"
              )
              dfx_press <- dplyr::filter(dfx_press, n ==
                24)
              dfx_press_mean_day <- aggregate(`pressao (mB)` ~
                Data, dfx_press, mean)
              dfx_ur <- na.omit(dplyr::select(
                dfx, Hora,
                Data, `UR-max`, `UR-min`, UR
              ))
              n_dfx_ur <- group_by(dfx_ur, Data) %>%
                summarise(n = n()) %>%
                filter(n == 24)
              if (nrow(n_dfx_ur) == 0) {
              } else {
                dfx_ur <- left_join(dfx_ur, n_dfx_ur, by = "Data")
                dfx_ur <- dplyr::filter(dfx_ur, n == 24)
                dfx_ur_mean_day <- aggregate(
                  UR ~ Data,
                  dfx_ur, mean
                )
                dfx_ur_min_day <- aggregate(`UR-min` ~
                  Data, dfx_ur, min)
                dfx_ur_max_day <- aggregate(`UR-max` ~
                  Data, dfx_ur, max)
                dfx_urs_day <- cbind(
                  dfx_ur_mean_day, dfx_ur_max_day,
                  dfx_ur_min_day
                )
                dfx_urs_day <- dplyr::select(
                  dfx_urs_day,
                  -3, -5
                )
                dfx_vv <- na.omit(dplyr::select(
                  dfx, Hora,
                  Data, `U10 (m/s)`, `U-raj (m/s)`, `U-dir(degrees)`
                ))
                n_dfx_vv <- group_by(dfx_vv, Data) %>%
                  summarise(n = n()) %>%
                  filter(n == 24)
                if (nrow(n_dfx_vv) == 0) {
                } else {
                  dfx_vv <- left_join(dfx_vv, n_dfx_vv,
                    by = "Data"
                  )
                  dfx_vv <- dplyr::filter(dfx_vv, n ==
                    24)
                  dfx_vv <- mutate(dfx_vv, u2 = (4.868 / (log(67.75 *
                    10 - 5.42))) * `U10 (m/s)`)
                  dfx_vv_mean_day <- aggregate(`U10 (m/s)` ~
                    Data, dfx_vv, mean)
                  dfx_vv_meanu2_day <- aggregate(
                    u2 ~ Data,
                    dfx_vv, mean
                  )
                  dfx_vv_raj_day <- aggregate(`U-raj (m/s)` ~
                    Data, dfx_vv, max)
                  dfx_vv_dir_day <- aggregate(`U-dir(degrees)` ~
                    Data, dfx_vv, mean)
                  dfx_vvs_day <- cbind(
                    dfx_vv_mean_day,
                    dfx_vv_meanu2_day, dfx_vv_raj_day,
                    dfx_vv_dir_day
                  )
                  dfx_vvs_day <- dplyr::select(
                    dfx_vvs_day,
                    -3, -5, -7
                  )
                  dfx_RG <- dplyr::select(
                    dfx, Hora, Data,
                    `RG(Kj/m2)`
                  )
                  dfx_RG <- dplyr::mutate(dfx_RG, `RG(Mj/m2)` = `RG(Kj/m2)` / 1000)
                  dfx_RG <- na.omit(dplyr::select(
                    dfx_RG,
                    -`RG(Kj/m2)`
                  ))
                  dfx_RG <- dplyr::filter(dfx_RG, `RG(Mj/m2)` >
                    0)
                  n_RG <- group_by(dfx_RG, Data) %>%
                    summarise(n = n()) %>%
                    filter(n >= 12)
                  if (nrow(n_RG) == 0) {
                  } else {
                    dfx_RG <- left_join(dfx_RG, n_RG, by = "Data")
                    dfx_RG <- dplyr::filter(dfx_RG, n >=
                      12)
                    dfx_RG_sum_day <- aggregate(`RG(Mj/m2)` ~
                      Data, dfx_RG, sum)
                    julian_day <- as.data.frame(as.numeric(format(
                      dfx_RG_sum_day$Data,
                      "%j"
                    )))
                    names(julian_day) <- "julian_day"
                    dfx_RG_sum_day <- cbind(
                      dfx_RG_sum_day,
                      julian_day
                    )
                    lat_rad <- (pi / 180) * (latitude)
                    dr <- 1 + 0.033 * cos((2 * pi / 365) *
                      dfx_RG_sum_day$julian_day)
                    summary(dr)
                    solar_declination <- 0.409 * sin(((2 *
                      pi / 365) * dfx_RG_sum_day$julian_day) -
                      1.39)
                    sunset_hour_angle <- acos(-tan(lat_rad) *
                      tan(solar_declination))
                    ra <- ((24 * (60)) / pi) * (0.082) *
                      dr * (sunset_hour_angle * sin(lat_rad) *
                        sin(solar_declination) + cos(lat_rad) *
                          cos(solar_declination) * sin(sunset_hour_angle))
                    ra <- as.data.frame(ra)
                    dfx_RG_sum_day <- cbind(
                      dfx_RG_sum_day,
                      ra
                    )
                    dfx_day <- dplyr::full_join(dfx_temps_day,
                      dfx_prec_day,
                      by = "Data"
                    )
                    dfx_day <- dplyr::full_join(dfx_day, dfx_press_mean_day,
                      by = "Data"
                    )
                    dfx_day <- dplyr::full_join(dfx_day, dfx_urs_day,
                      by = "Data"
                    )
                    dfx_day <- dplyr::full_join(dfx_day, dfx_vvs_day,
                      by = "Data"
                    )
                    dfx_day <- dplyr::full_join(dfx_day, dfx_RG_sum_day,
                      by = "Data"
                    )
                    dfx_day <- dplyr::mutate(dfx_day, OMM = OMM)
                    df <- rbind(df, dfx_day)
                  }
                }
              }
            }
          }
        }
      }
      df <- filter(df, Data >= start_date & Data <= end_date)
      df <- df %>% mutate(
        Station = Station,
        UF = UF,
        longitude = longitude,
        latitude = latitude,
        altitude = altitude
      )
      colnames(df) <- c(
        "date", "tair_mean_c", "tair_min_c",
        "tair_max_c", "dew_tmean_c", "dew_tmin_c",
        "dew_tmax_c", "dry_bulb_t_c", "rainfall_mm",
        "patm_mB", "rh_mean_porc", "rh_max_porc", "rh_min_porc",
        "ws_10_m_s", "ws_2_m_s", "ws_gust_m_s",
        "wd_degrees", "sr_mj_m2_day", "doy", "ra_mj_m2_day",
        "station_code", "station", "uf", "longitude_degrees", "latitude_degrees",
        "altitude_m"
      )
    }
  } else {
    df_sequence <- data.frame()

    for (year in seq(from = as.numeric(start_year), to = as.numeric(end_year))) {
      message("Downloading data for: ", year)

      tempdir <- tempfile()
      tf <- paste0(gsub("\\", "/", tempdir, fixed = TRUE), ".zip")
      outdir <- gsub("\\", "/", tempdir, fixed = TRUE)

      utils::download.file(url = paste0(
        "https://portal.inmet.gov.br/uploads/dadoshistoricos/",
        year, ".zip"
      ), destfile = tf, method = "auto", cacheOK = F)

      a <- unzip(zipfile = tf, exdir = outdir, junkpaths = T)
      pasta <- paste0(outdir)

      if (length(list.files(pasta,
        pattern = station, full.names = T,
        all.files = T
      )) == 0) {
        message("There is no data for this period for this station. Choose another period!")
      } else {
        list.files(pasta, pattern = station)
        b <- read.csv(
          file = list.files(pasta,
            pattern = station,
            full.names = T
          ), header = T, sep = ";", skip = 8,
          na = "-9999", dec = ",", check.names = F
        )
        a <- as.data.frame(a)
        X <- `pressao-max(mB)` <- `pressao-min(mB)` <- Data <- Hora <- NULL
        `to-min(C)` <- `to-max(C)` <- `tar_min(C)` <- `tar_max(C)` <- `tbs(C)` <- NULL
        `ppt-h (mm)` <- `pressao (mB)` <- `UR-max` <- `UR-min` <- UR <- NULL
        `U10 (m/s)` <- `U-raj (m/s)` <- `U-dir(degrees)` <- `RG(Kj/m2)` <- `RG(Mj/m2)` <- NULL
        df <- data.frame(matrix(ncol = 18, nrow = 0))
        colnames(df) <- c(
          "Data", "Hora", "ppt-h (mm)", "pressao (mB)",
          "RG(Mj/m2)", "tbs-(C)", "tpo(C)", "tar_max(C)", "tar_min(C)",
          "to-max(C)", "to-min(C)", "UR-max", "UR-min", "UR",
          "U-dir(degrees)", "U-raj (m/s)", "U10 (m/s)", "OMM"
        )
        OMM <- read.csv(file = list.files(pasta,
          pattern = station,
          full.names = T
        ), header = F, sep = ";")
        OMM <- (OMM[4, 2])

        UF <- read.csv(file = list.files(pasta, pattern = station, full.names = T), header = F, sep = ";")
        UF <- (UF[2, 2])

        Station <- read.csv(file = list.files(pasta, pattern = station, full.names = T), header = F, sep = ";")
        Station <- (Station[3, 2])

        dfx <- read.csv(
          file = list.files(pasta,
            pattern = station,
            full.names = T
          ), header = T, sep = ";", skip = 8,
          na = "-9999", dec = ",", check.names = F
        )
        names(dfx)
        names(dfx) <- c(
          "Data", "Hora", "ppt-h (mm)", "pressao (mB)",
          "pressao-max(mB)", "pressao-min(mB)", "RG(Kj/m2)",
          "tbs(C)", "tpo(C)", "tar_max(C)", "tar_min(C)", "to-max(C)",
          "to-min(C)", "UR-max", "UR-min", "UR", "U-dir(degrees)",
          "U-raj (m/s)", "U10 (m/s)", "X"
        )
        latitude <- read.csv(file = list.files(pasta,
          pattern = station,
          full.names = T
        ), header = F, sep = ";", dec = ",")
        latitude <- latitude[5, 2]
        lat <- substr(latitude, 1, 3)
        itude <- substr(latitude, 5, 10)
        latitude <- as.numeric(paste0(lat, ".", itude))
        longitude <- read.csv(file = list.files(pasta,
          pattern = station,
          full.names = T
        ), header = F, sep = ";", dec = ",")
        longitude <- longitude[6, 2]
        long <- substr(longitude, 1, 3)
        itude <- substr(longitude, 5, 10)
        longitude <- as.numeric(paste0(long, ".", itude))
        altitude <- read.csv(file = list.files(pasta,
          pattern = station,
          full.names = T
        ), header = F, sep = ";", dec = ",")
        altitude <- altitude[7, 2]
        altitude <- gsub(",", replacement = ".", altitude)
        altitude <- as.numeric(altitude)
        dfx <- dplyr::select(dfx, -X, -`pressao-max(mB)`, -`pressao-min(mB)`)
        dfx <- tibble::as_tibble(dfx)
        dfx <- dplyr::mutate(dfx, Data = as.Date(Data), Hora = as.numeric(as.factor(Hora)))
        dfx$date_hora <- paste0(dfx$Data, dfx$Hora)
        dfx$date_hora <- as.POSIXct(strptime(dfx$date_hora, format = "%Y-%m-%d %H"))
        for (i in 1:nrow(dfx)) {
          if (longitude > -37.5) {
            (dfx$date_hora[i] <- dfx$date_hora[i] - as.difftime(2,
              units = "hours"
            ))
          } else if (longitude > -52.5) {
            (dfx$date_hora[i] <- dfx$date_hora[i] - as.difftime(3,
              units = "hours"
            ))
          } else if (longitude > -67.5) {
            (dfx$date_hora[i] <- dfx$date_hora[i] - as.difftime(4,
              units = "hours"
            ))
          } else if (longitude > -82.5) {
            (dfx$date_hora[i] <- dfx$date_hora[i] - as.difftime(5,
              units = "hours"
            ))
          }
        }
        dfx$Data <- as.POSIXct(strptime(dfx$date_hora, format = "%Y-%m-%d"))
        dfx$Hora <- format(
          as.POSIXct(dfx$date_hora, format = "%Y-%m-%d %H"),
          "%H"
        )
        diff_days <- as.Date(end_date) - as.Date(start_date)

        if (nrow(dfx) < 4380 & diff_days > 120) {
        } else {
          dfx_temp <- na.omit(dplyr::select(
            dfx, Hora, Data,
            `to-min(C)`, `to-max(C)`, `tar_min(C)`, `tar_max(C)`,
            `tbs(C)`
          ))
          n_dfx_temp <- group_by(dfx_temp, Data) %>%
            summarise(n = n()) %>%
            filter(n == 24)
          if (nrow(n_dfx_temp) == 0) {
          } else {
            dfx_temp <- left_join(dfx_temp, n_dfx_temp, by = "Data")
            dfx_temp <- dplyr::filter(dfx_temp, n == 24)
            dfx_temp <- dplyr::mutate(dfx_temp, tar_mean = (`tar_min(C)` +
              `tar_max(C)`) / 2)
            dfx_temp <- dplyr::mutate(dfx_temp, to_mean = (`to-min(C)` +
              `to-max(C)`) / 2)
            dfx_temp_mean_day <- aggregate(
              tar_mean ~ Data,
              dfx_temp, mean
            )
            dfx_temp_min_day <- aggregate(`tar_min(C)` ~
              Data, dfx_temp, min)
            dfx_temp_max_day <- aggregate(`tar_max(C)` ~
              Data, dfx_temp, max)
            dfx_to_min_day <- aggregate(
              `to-min(C)` ~ Data,
              dfx_temp, min
            )
            dfx_to_max_day <- aggregate(
              `to-max(C)` ~ Data,
              dfx_temp, max
            )
            dfx_to_mean_day <- aggregate(
              to_mean ~ Data,
              dfx_temp, mean
            )
            dfx_tbs_day <- aggregate(
              `tbs(C)` ~ Data, dfx_temp,
              mean
            )
            dfx_temps_day <- cbind(
              dfx_temp_mean_day, dfx_temp_min_day,
              dfx_temp_max_day, dfx_to_mean_day, dfx_to_min_day,
              dfx_to_max_day, dfx_tbs_day
            )
            dfx_temps_day <- dplyr::select(
              dfx_temps_day,
              -3, -5, -7, -9, -11, -13
            )
            dfx_prec <- na.omit(dplyr::select(
              dfx, Hora,
              Data, `ppt-h (mm)`
            ))
            dfx_prec <- group_by(dfx_prec, Data)
            if (nrow(dfx_prec) == 0) {
            } else {
              dfx_prec_day <- aggregate(
                `ppt-h (mm)` ~ Data,
                dfx_prec, sum
              )
              dfx_press <- na.omit(dplyr::select(
                dfx, Hora,
                Data, `pressao (mB)`
              ))
              n_dfx_press <- group_by(dfx_press, Data) %>%
                summarise(n = n()) %>%
                filter(n == 24)
              if (nrow(n_dfx_press) == 0) {
              } else {
                dfx_press <- left_join(dfx_press, n_dfx_press,
                  by = "Data"
                )
                dfx_press <- dplyr::filter(dfx_press, n ==
                  24)
                dfx_press_mean_day <- aggregate(`pressao (mB)` ~
                  Data, dfx_press, mean)
                dfx_ur <- na.omit(dplyr::select(
                  dfx, Hora,
                  Data, `UR-max`, `UR-min`, UR
                ))
                n_dfx_ur <- group_by(dfx_ur, Data) %>%
                  summarise(n = n()) %>%
                  filter(n == 24)
                if (nrow(n_dfx_ur) == 0) {
                } else {
                  dfx_ur <- left_join(dfx_ur, n_dfx_ur, by = "Data")
                  dfx_ur <- dplyr::filter(dfx_ur, n == 24)
                  dfx_ur_mean_day <- aggregate(
                    UR ~ Data,
                    dfx_ur, mean
                  )
                  dfx_ur_min_day <- aggregate(`UR-min` ~
                    Data, dfx_ur, min)
                  dfx_ur_max_day <- aggregate(`UR-max` ~
                    Data, dfx_ur, max)
                  dfx_urs_day <- cbind(
                    dfx_ur_mean_day, dfx_ur_max_day,
                    dfx_ur_min_day
                  )
                  dfx_urs_day <- dplyr::select(
                    dfx_urs_day,
                    -3, -5
                  )
                  dfx_vv <- na.omit(dplyr::select(
                    dfx, Hora,
                    Data, `U10 (m/s)`, `U-raj (m/s)`, `U-dir(degrees)`
                  ))
                  n_dfx_vv <- group_by(dfx_vv, Data) %>%
                    summarise(n = n()) %>%
                    filter(n == 24)
                  if (nrow(n_dfx_vv) == 0) {
                  } else {
                    dfx_vv <- left_join(dfx_vv, n_dfx_vv,
                      by = "Data"
                    )
                    dfx_vv <- dplyr::filter(dfx_vv, n ==
                      24)
                    dfx_vv <- mutate(dfx_vv, u2 = (4.868 / (log(67.75 *
                      10 - 5.42))) * `U10 (m/s)`)
                    dfx_vv_mean_day <- aggregate(`U10 (m/s)` ~
                      Data, dfx_vv, mean)
                    dfx_vv_meanu2_day <- aggregate(
                      u2 ~ Data,
                      dfx_vv, mean
                    )
                    dfx_vv_raj_day <- aggregate(`U-raj (m/s)` ~
                      Data, dfx_vv, max)
                    dfx_vv_dir_day <- aggregate(`U-dir(degrees)` ~
                      Data, dfx_vv, mean)
                    dfx_vvs_day <- cbind(
                      dfx_vv_mean_day,
                      dfx_vv_meanu2_day, dfx_vv_raj_day,
                      dfx_vv_dir_day
                    )
                    dfx_vvs_day <- dplyr::select(
                      dfx_vvs_day,
                      -3, -5, -7
                    )
                    dfx_RG <- dplyr::select(
                      dfx, Hora, Data,
                      `RG(Kj/m2)`
                    )
                    dfx_RG <- dplyr::mutate(dfx_RG, `RG(Mj/m2)` = `RG(Kj/m2)` / 1000)
                    dfx_RG <- na.omit(dplyr::select(
                      dfx_RG,
                      -`RG(Kj/m2)`
                    ))
                    dfx_RG <- dplyr::filter(dfx_RG, `RG(Mj/m2)` >
                      0)
                    n_RG <- group_by(dfx_RG, Data) %>%
                      summarise(n = n()) %>%
                      filter(n >= 12)
                    if (nrow(n_RG) == 0) {
                    } else {
                      dfx_RG <- left_join(dfx_RG, n_RG, by = "Data")
                      dfx_RG <- dplyr::filter(dfx_RG, n >=
                        12)
                      dfx_RG_sum_day <- aggregate(`RG(Mj/m2)` ~
                        Data, dfx_RG, sum)
                      julian_day <- as.data.frame(as.numeric(format(
                        dfx_RG_sum_day$Data,
                        "%j"
                      )))
                      names(julian_day) <- "julian_day"
                      dfx_RG_sum_day <- cbind(
                        dfx_RG_sum_day,
                        julian_day
                      )
                      lat_rad <- (pi / 180) * (latitude)
                      dr <- 1 + 0.033 * cos((2 * pi / 365) *
                        dfx_RG_sum_day$julian_day)
                      summary(dr)
                      solar_declination <- 0.409 * sin(((2 *
                        pi / 365) * dfx_RG_sum_day$julian_day) -
                        1.39)
                      sunset_hour_angle <- acos(-tan(lat_rad) *
                        tan(solar_declination))
                      ra <- ((24 * (60)) / pi) * (0.082) *
                        dr * (sunset_hour_angle * sin(lat_rad) *
                          sin(solar_declination) + cos(lat_rad) *
                            cos(solar_declination) * sin(sunset_hour_angle))
                      ra <- as.data.frame(ra)
                      dfx_RG_sum_day <- cbind(
                        dfx_RG_sum_day,
                        ra
                      )
                      dfx_day <- dplyr::full_join(dfx_temps_day,
                        dfx_prec_day,
                        by = "Data"
                      )
                      dfx_day <- dplyr::full_join(dfx_day, dfx_press_mean_day,
                        by = "Data"
                      )
                      dfx_day <- dplyr::full_join(dfx_day, dfx_urs_day,
                        by = "Data"
                      )
                      dfx_day <- dplyr::full_join(dfx_day, dfx_vvs_day,
                        by = "Data"
                      )
                      dfx_day <- dplyr::full_join(dfx_day, dfx_RG_sum_day,
                        by = "Data"
                      )
                      dfx_day <- dplyr::mutate(dfx_day, OMM = OMM)
                      df <- rbind(df, dfx_day)
                    }
                  }
                }
              }
            }
          }
        }
        df <- filter(df, Data >= start_date & Data <= end_date)
        df <- df %>% mutate(
          Station = Station,
          UF = UF,
          longitude = longitude,
          latitude = latitude,
          altitude = altitude
        )
        colnames(df) <- c(
          "date", "tair_mean_c", "tair_min_c",
          "tair_max_c", "dew_tmean_c", "dew_tmin_c",
          "dew_tmax_c", "dry_bulb_t_c", "rainfall_mm",
          "patm_mB", "rh_mean_porc", "rh_max_porc", "rh_min_porc",
          "ws_10_m_s", "ws_2_m_s", "ws_gust_m_s",
          "wd_degrees", "sr_mj_m2_day", "doy", "ra_mj_m2_day",
          "station_code", "station", "uf", "longitude_degrees", "latitude_degrees",
          "altitude_m"
        )
      }

      df_sequence <- rbind(df_sequence, df)
      df_sequence <- dplyr::arrange(df_sequence, date)
    }
  }

  if (as.numeric(end_year) - as.numeric(start_year) != 0) {
    return(df_sequence)
  } else {
    (return(df))
  }
}
