#' Download maximum reference evapotranspiration (ETo) grids for Brazil
#'
#' @description
#' Downloads maximum reference evapotranspiration (ETo) grids for Brazil, intended for irrigation design purposes. 
#' The dataset was developed by Dias (2018).
#'
#' @param dir_out Character. Directory where the downloaded raster file will be saved.
#' @param product Character. Specifies which maximum ETo product to download.  
#' Available options include:
#' \itemize{
#'   \item \code{max_12_months}: maximum ETo over the full year.
#'   \item \code{max_jan} to \code{max_dec}: monthly maximum ETo for each respective month (January to December).
#' }
#'
#' @return A `SpatRaster` object containing the downloaded maximum reference evapotranspiration (ETo) grid.
#' @author Roberto Filgueiras.
#'
#' @references
#' Dias, S. H. B. (2018). *Evapotranspiração de referência para projeto de irrigação no Brasil utilizando o produto MOD16*. Dissertação (Mestrado) – Universidade Federal de Viçosa.
#'
#' @importFrom terra rast
#'
#' @examples
#' \dontrun{
#' # Download the annual maximum ETo grid
#' img_max_eto <- max_eto_grid_download(dir_out = tempdir(), product = "max_12_months")
#' }
#'
#' @export

max_eto_grid_download <- function(dir_out, product = "max_12_months") {

  months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  month_names <- c("January", "February", "March", "April", "May", "June", "July",
                   "August", "September", "October", "November", "December")
  products <- paste0("max_", months)

  if (length(product) != 1 || !(product %in% c("max_12_months", products))) {
    stop("'product' must be one of: max_12_months, ", paste(products, collapse = ", "), ".")
  }
  if (!dir.exists(dir_out)) {
    stop("'dir_out' does not exist: ", dir_out)
  }

  if (product == "max_12_months") {
    file_url <- "ETproject_Max_12Months.tif"
    outfile <- file.path(dir_out, "maximum_eto_12months.tif")
  } else {
    i <- match(product, products)
    file_url <- sprintf("%02d_ETproject_%s.tif", i, month_names[i])
    outfile <- file.path(dir_out, paste0("img_", product, ".tif"))
  }

  base_url <- paste0("https://zenodo.org/records/3946836/files/", file_url, "?download=1")

  message("Downloading the maximum reference evapotranspiration image!")
  if (!.download_file(url = base_url, destfile = outfile, timeout = 1000)) {
    message("The image could not be downloaded. Please check your internet connection and try again.")
    return(invisible(NULL))
  }

  img <- terra::rast(outfile)
  message("Done!")
  img
}