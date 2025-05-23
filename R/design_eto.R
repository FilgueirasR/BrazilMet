#' Design reference evapotranspiration (Design ETo)
#' @description Function to calculate the reference evapotranspitation for irrigation design.
#' @param eto_daily_data A data frame containing daily reference evapotranspiration values (mm day-1) named as "ETo", and other column with dates named "date". To estimate the design reference evapotranspiration, it is recommended to use more than 10 years of historical data.
#' @param percentile The relative position of a value within the data distribution. The recomendation to desing irrigating is greater than 80 percent (0.8). For instance: The 90th (0.90) percentile of reference evapotranspiration (ETo) represents a value that is surpassed in only 10 percent of the observed period.
#' @param grouping specifies the column used to group the data, typically by station. Use this parameter to calculate the design ETo separately for each station, based on the corresponding station code.
#' @importFrom stats quantile
#' @examples
#' \dontrun{
#' design_eto_value <- design_eto(eto, percentile)
#' }
#' @export
#' @return Returns a numeric object with the desing ETo for the respective data and percentile.
#' @author Roberto Filgueiras

design_eto <- function(eto_daily_data, percentile = 0.8, grouping = NULL) {
  eto <- NULL  
  
  required_cols <- c("date", "eto")
  missing_cols <- setdiff(required_cols, names(eto_daily_data))
  
  if (length(missing_cols) > 0) {
    stop(paste("The following required column(s) are missing:", paste(missing_cols, collapse = ", ")))
  }
  
  
  years <- unique(format(eto_daily_data$date, "%Y"))
  if (length(years) < 10) {
    warning("It is recommended to use at least 10 years of data to calculate the design reference evapotranspiration.")
  }
  
  
  if (!is.null(grouping)) {
    missing_group_cols <- setdiff(grouping, names(eto_daily_data))
    if (length(missing_group_cols) > 0) {
      stop(paste(
        "The following grouping column(s) are missing in the data frame:",
        paste(missing_group_cols, collapse = ", ")
      ))
    }
  }
  
  if (!is.null(grouping)) {
    design_eto <- eto_daily_data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
      dplyr::summarise(
        design_eto = stats::quantile(eto, probs = percentile, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    design_eto <- eto_daily_data %>%
      dplyr::summarise(
        design_eto = stats::quantile(eto, probs = percentile, na.rm = TRUE)
      )
  }
  
  return(design_eto)
}
