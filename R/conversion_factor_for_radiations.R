#' Conversion factors for radiation
#' \if{html}{\figure{logo_BrazilMet.png}{options: height= 300 width=auto style = float:right alt= Our logo}}
#' 
#' @description Function to convert the radiation data. The conversion name can be understand as follow:
#' 
#' \itemize{
#'   \item  conversion_1 = MJ m-2 day-1 to J cm-2 day-1;
#'   \item  conversion_2 = MJ m-2 day-1 to cal cm-2 day-1;
#'   \item  conversion_3 = MJ m-2 day-1 to W m-2;
#'   \item  conversion_4 = MJ m-2 day-1 to mm day-1;
#'   \item  conversion_5 = cal cm-2 day-1 to MJ m-2 day-1;
#'   \item  conversion_6 = cal cm-2 day-1 to J cm-2 day-1;
#'   \item  conversion_7 = cal cm-2 day-1 to W m-2;
#'   \item  conversion_8 = cal cm-2 day-1 to mm day-1;
#'   \item  conversion_9 =  W m-2 to MJ m-2 day-1;
#'   \item  conversion_10  = W m-2 to J cm-2 day-1;
#'   \item  conversion_11  = W m-2 to cal cm-2 day-1;
#'   \item  conversion_12  = W m-2 to mm day-1;
#'   \item  conversion_13  = mm day-1 to MJ m-2 day-1;
#'   \item  conversion_14  = mm day-1 to J cm-2 day-1;
#'   \item  conversion_15  = mm day-1 to cal cm-2 day-1;
#'   \item  conversion_16  = mm day-1 to W m-2.
#'  }
#' @param  data_to_convert A dataframe with radiation values to convert.
#' @param  conversion_name A character with the conversion_name summarize in the description of this function.
#' @examples
#' \dontrun{
#' radiation_conversion_df <- radiation_conversion(data_to_convert = df$rad,
#'                                                 conversion_name = "conversion_1")
#' }
#' @export
#' @return A dataframe object wit the converted radiation.

radiation_conversion <- function(data_to_convert, conversion_name){
  conversion_factor <- switch (conversion_name,
    "conversion_1" = 100,
    "conversion_2" = 23.9,
    "conversion_3" = 11.6,
    "conversion_4" = 0.408,
    "conversion_5" = 0.041868,
    "conversion_6" = 4.1868,
    "conversion_7" = 0.485,
    "conversion_8" = 0.0171,
    "conversion_9" = 0.0864,
    "conversion_10" = 8.64,
    "conversion_11" = 2.06,
    "conversion_12" = 0.035,
    "conversion_13" = 2.45,
    "conversion_14" = 245,
    "conversion_15" = 58.5,
    "conversion_16" = 28.4
  )
  
  rad_corverted <-data_to_convert*conversion_factor
  rad_converted <- as.data.frame(rad_converted)
  colnames(rad_converted) <- paste0("rc_", conversion_name)
  return(rad_converted)
  
}