#' Download of hourly data from automatic weather stations (AWS) of INMET-Brazil
#' @description This function will download the hourly AWS data of INMET for whatever station of interest, based on the period of time selected (start_date and end_date) and station code. The limit acquisition in the same requisition for hourly data is one year.
#' @param station The station code (ID - WMO code) for download. To see the station ID, please see the function *see_stations_info*.
#' @param start_date Date that start the investigation, should be in the following format (1958-01-01 /Year-Month-Day)
#' @param end_date Date that end the investigation, should be in the following format (2017-12-31 /Year-Month-Day)
#' @import dplyr
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' df <- hourly_weather_station_download(
#'                                       station = "A001",
#'                                       start_date = "2022-08-12",
#'                                       end_date = "2022-08-16")
#' }
#' @export
#' @return Returns a data.frame with the AWS data requested
#' @author Roberto Filgueiras


hourly_weather_station_download <- function(start_date, end_date, station) {
  
  X <- pressure_max_mb <- pressure_min_mb <- date_hour <- hour <- ppt_h_mm <- pressure_mb <- rj_kj_m2 <- tbs_c <- NULL
  tpo_c <- tar_max_c <- tar_main_c <- to_max_c <- to_min_c <- ur_min <- ur <- u_dir_degrees <- u_raj_m_s <- u10_m_s <- NULL
  rg_kj_m2 <- tar_min_c <- ur_max <- NULL
  
  year <- substr(start_date, 1, 4)
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

    df <- data.frame(matrix(ncol = 18, nrow = 0))
    colnames(df) <- c(
      "data", "hora", "ppt_h_mm", "pressao_mB",
      "rg_mj_m2", "tbs_c", "tpo_c", "tar_max_c", "tar_min_c",
      "to_max_c", "to_min_c", "ur_max", "ur_min", "ur",
      "u_dir_degrees", "u_raj_m_s", "u10_m_s", "omm"
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
      "date", "date_hour", "ppt_h_mm", "pressure_mb",
      "pressure_max_mb", "pressure_min_mb", "rg_kj_m2",
      "tbs_c", "tpo_c", "tar_max_c", "tar_min_c", "to_max_c",
      "to_min_c", "ur_max", "ur_min", "ur", "u_dir_degrees",
      "u_raj_m_s", "u10_m_s", "X"
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
    dfx <- dplyr::select(dfx, -X, -pressure_max_mb, -pressure_min_mb)
    dfx <- tibble::as_tibble(dfx)
    dfx <- dplyr::mutate(dfx, date = as.Date(date), hour = as.numeric(as.factor(date_hour)))
    dfx$date_hour <- paste0(dfx$date, dfx$date_hour)
    dfx$date_hour <- as.POSIXct(strptime(dfx$date_hour, format = "%Y-%m-%d %H"))
    i <- 1
    for (i in 1:nrow(dfx)) {
      if (longitude > -37.5) {
        (dfx$date_hour[i] <- as.Date(dfx$date_hour[i]) - as.difftime(2,
          units = "hours"
        ))
      } else if (longitude > -52.5) {
        (dfx$date_hour[i] <- dfx$date_hour[i] - as.difftime(3,
          units = "hours"
        ))
      } else if (longitude > -67.5) {
        (dfx$date_hour[i] <- dfx$date_hour[i] - as.difftime(4,
          units = "hours"
        ))
      } else if (longitude > -82.5) {
        (dfx$date_hour[i] <- dfx$date_hour[i] - as.difftime(5,
          units = "hours"
        ))
      }
    }
    dfx$date <- as.POSIXct(strptime(dfx$date_hour, format = "%Y-%m-%d"))
    dfx$hour <- format(
      as.POSIXct(dfx$date_hour, format = "%Y-%m-%d %H"),
      "%H"
    )

    dfx <- dfx %>%
      dplyr::select(-c(date_hour)) %>%
      dplyr::select(date, hour, ppt_h_mm, pressure_mb, rg_kj_m2, tbs_c, tpo_c, tar_max_c, tar_min_c, to_max_c, to_min_c, ur_max, ur_min, ur, u_dir_degrees, u_raj_m_s, u10_m_s)


    return(dfx)
  }
}
