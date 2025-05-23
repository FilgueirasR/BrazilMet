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
#' @importFrom terra crs
#' @importFrom terra rast
#' @importFrom terra writeRaster
#' 
#' @examples
#' \dontrun{
#' # Visualize Brazilian states (optional)
#' see_brazil_states()
#'
#' # Download maximum ETo grid (annual)
#' img_max_eto <- max_eto_grid_download(dir_out = "data/", product = "max_12_months")
#' }
#'
#' @export



max_eto_grid_download <- function(dir_out, product = "max_12_months"){
  
    if(product == "max_12_months"){
      baseurl <- "https://zenodo.org/records/3946836/files/ETproject_Max_12Months.tif?download=1"
      name_img<- "maximum_eto_12months.tif"
      outfile<-paste0(dir_out, "/", name_img)
      message("Downloading the maximum reference evapotranspiration image!")
      options(timeout = 1000)
      utils::download.file(url = baseurl, destfile = outfile, mode = "wb", quiet = TRUE)
      img <- terra::rast(outfile)
    }
  
  if (product %in% c("max_jan", "max_feb", "max_mar", "max_apr", "max_may", "max_jun", "max_jul", "max_aug", "max_sep", "max_oct", "max_nov", "max_dec")) {
  
    suffix_url <- switch(product,
                          "max_jan" = "01_ETproject_January.tif?download=1",
                          "max_feb" = "02_ETproject_February.tif?download=1",
                          "max_mar" = "03_ETproject_March.tif?download=1",
                          "max_apr" = "04_ETproject_April.tif?download=1",
                          "max_may" = "05_ETproject_May.tif?download=1",
                          "max_jun" = "06_ETproject_June.tif?download=1",
                          "max_jul" = "07_ETproject_July.tif?download=1",
                          "max_aug" = "08_ETproject_August.tif?download=1",
                          "max_sep" = "09_ETproject_September.tif?download=1",
                          "max_oct" = "10_ETproject_October.tif?download=1",
                          "max_nov" = "11_ETproject_November.tif?download=1",
                          "max_dec" = "12_ETproject_December.tif?download=1")
    
    base_url <- paste0("https://zenodo.org/records/3946836/files/", suffix_url)
    name_img <- paste0("img_", product, ".tif")
    outfile <- paste0(dir_out, "/", name_img)
    utils::download.file(url = baseurl, destfile = outfile, mode = "wb")
    img <- terra::rast(outfile)
                       
  }
  message("Done!")
  return(img)
}

