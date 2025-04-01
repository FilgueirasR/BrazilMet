#' Download climatological normals from Conventional weather stations (CWS) of Inmet
#' @description This function will download the climatological normals from CWS stations available in Inmet site.
#' @param variable The variables available to download. The available variables are: rainfall_norm, t2m_norm, rh_norm, ws_norm and etp_norm.
#' @param range_time The range of time which the climatological normals were computed. The available range of time is: "1991-2020".
#' @import dplyr
#' @import utils
#' @importFrom readxl read_xlsx
#' @importFrom utils download.file
#' @importFrom dplyr across
#' @importFrom dplyr where
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom stringi stri_trans_general
#' @examples
#' \dontrun{
#' df <- download_climate_normals(
#'   variable = "rainfall_norm",
#'   range_time = "1991-2020"
#' )
#' }
#' @export
#' @return Returns a data.frame with climatological normal required.
#' @author Roberto Filgueiras


download_climate_normals <- function(variable, range_time) {
  station_municipality <- NULL

  if (variable == "rainfall_norm") {
    var <- "PREC"
  }
  if (variable == "t2m_norm") {
    var <- "TMEDSECA"
  }
  if (variable == "rh_norm") {
    var <- "UR"
  }
  if (variable == "ws_norm") {
    var <- "VENTIN"
  }
  if (variable == "etp_norm") {
    var <- "EVAPOTM"
  }
  if (!(variable %in% c("rainfall_norm", "t2m_norm", "rh_norm", "ws_norm", "etp_norm"))) {
    message("Please, select one of the available variables: rainfall_norm, t2m_norm, rh_norm, ws_norm, etp_norm!")
  }

  if (!(range_time %in% c("1991-2020"))) {
    message("Please, select range_time available - 1991-2020!")
  }
  

  base_url <- paste0("https://portal.inmet.gov.br/uploads/normais/Normal-Climatologica-", var, ".xlsx")

  tempdir <- tempfile()
  tf <- paste0(gsub("\\", "/", tempdir, fixed = TRUE), ".xlsx")
  outdir <- gsub("\\", "/", tempdir, fixed = TRUE)
  options(timeout = 600)

  utils::download.file(
    url = base_url,
    destfile = tf, mode = "wb"
  )

  a <- readxl::read_xlsx(tf, skip = 2, col_names = TRUE) |>
    rename_with(~ stringi::stri_trans_general(., "Latin-ASCII")) |>
    dplyr::mutate(dplyr::across(c(
      "Janeiro",
      "Fevereiro",
      "Marco",
      "Abril",
      "Maio",
      "Junho",
      "Julho",
      "Agosto",
      "Setembro",
      "Outubro",
      "Novembro",
      "Dezembro",
      "Ano"
    ), as.numeric)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 4))) |>
    suppressWarnings()

  names(a) <- c(
    "station_code", "station_municipality", "uf", "january", "february", "march", "april", "may", "june",
    "july", "august", "september", "october", "november", "december", "year"
  )

  a <- a |>
    dplyr::mutate(station_municipality = tolower(station_municipality))

  message("- For seasons marked with an asterisk (*), the requirement to consider only years with 'complete months' when calculating the average was relaxed by Inmet!")

  return(a)
}

