#' Fill gaps in daily weather data
#'
#' @description Fills missing values (`NA`) in daily weather variables, such as
#' the data downloaded with [download_AWS_INMET_daily()], so they can be used
#' by functions that do not accept missing values, like [water_balance()].
#' Short gaps are filled by linear interpolation between the neighbouring
#' observations, and, optionally, remaining gaps are filled with the mean of the
#' same day of the year across the available years (climatology).
#'
#' Precipitation must not be filled with this function: it is intermittent, so
#' interpolation or averaging would create rainfall that did not occur. A
#' column of flags (`<variable>_filled`) identifies every value that was filled,
#' so filled data are never mistaken for observations.
#'
#' @param data A data.frame with one row per day.
#' @param vars A character vector with the names of the columns to fill. The
#'   default is the set of INMET variables used by [daily_eto_FAO56()].
#'   Rainfall columns are not allowed.
#' @param date Name of the column with the dates (`Date` or `POSIXct`).
#'   Within each group, dates must be unique and sorted in ascending order.
#' @param method `"linear"` (default) interpolates gaps of at most `max_gap`
#'   days between two observed values; short gaps at the beginning or end of
#'   the series take the nearest observation. `"climatology"` fills every gap with the
#'   mean of the same day of the year in the other years (needs more than one
#'   year of data). `"both"` applies linear interpolation first and
#'   climatology to what remains.
#' @param max_gap Maximum length, in days, of a gap filled by linear
#'   interpolation. Longer gaps are left as `NA` (or filled by climatology when
#'   `method = "both"`). Default is 3.
#' @param group Optional name of a column (e.g. the station code) used to fill
#'   each series independently when the data have more than one station.
#' @param flag If `TRUE` (default), adds a logical column `<variable>_filled`
#'   for each filled variable.
#'
#' @return The input data.frame with the missing values filled and, if
#'   `flag = TRUE`, one extra `<variable>_filled` column per variable. A
#'   message reports how many values were filled and how many remain missing.
#'
#' @examples
#' \dontrun{
#' df <- download_AWS_INMET_daily("A001", "2023-01-01", "2024-12-31")
#'
#' # Short gaps only
#' df_filled <- fill_gaps(df)
#'
#' # Short gaps by interpolation, the rest by climatology
#' df_filled <- fill_gaps(df, method = "both", max_gap = 3)
#' }
#'
#' @importFrom stats approx
#' @export
#' @author Roberto Filgueiras

fill_gaps <- function(data,
                      vars = c("tair_mean_c", "tair_min_c", "tair_max_c",
                               "rh_max_porc", "rh_min_porc", "ws_2_m_s",
                               "patm_mb", "sr_mj_m2"),
                      date = "date",
                      method = c("linear", "climatology", "both"),
                      max_gap = 3,
                      group = NULL,
                      flag = TRUE) {

  method <- match.arg(method)

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  if (!is.character(date) || length(date) != 1 || !date %in% names(data)) {
    stop("'date' must be the name of a column in 'data'.")
  }
  if (!is.character(vars) || length(vars) == 0 || !all(vars %in% names(data))) {
    stop("'vars' must be names of columns in 'data'.")
  }
  if (any(grepl("rain|precip|ppt|chuva", vars, ignore.case = TRUE))) {
    stop("Precipitation cannot be filled with fill_gaps(): it is intermittent, ",
         "so interpolation or averaging would create rainfall that did not occur.")
  }
  if (!all(vapply(data[vars], is.numeric, logical(1)))) {
    stop("All columns in 'vars' must be numeric.")
  }
  if (!is.numeric(max_gap) || length(max_gap) != 1 || is.na(max_gap) || max_gap < 1) {
    stop("'max_gap' must be a single number >= 1.")
  }
  if (!is.null(group) && (!is.character(group) || length(group) != 1 || !group %in% names(data))) {
    stop("'group' must be the name of a column in 'data'.")
  }

  dates <- data[[date]]
  dates <- if (inherits(dates, "POSIXt")) as.Date(format(dates, "%Y-%m-%d")) else as.Date(dates)
  if (anyNA(dates)) {
    stop("The date column cannot contain missing or invalid dates.")
  }

  groups <- if (is.null(group)) rep(1L, nrow(data)) else data[[group]]
  report <- data.frame(variable = vars, filled = 0L, remaining = 0L)

  for (v in vars) {
    filled_flag <- rep(FALSE, nrow(data))

    for (g in unique(groups)) {
      idx <- which(groups == g)
      d <- dates[idx]

      if (is.unsorted(d, strictly = TRUE)) {
        stop("Dates must be unique and sorted in ascending order",
             if (!is.null(group)) " within each group", ".")
      }

      x <- data[[v]][idx]
      new_x <- x

      if (method %in% c("linear", "both")) {
        new_x <- .fill_linear(new_x, d, max_gap)
      }
      if (method %in% c("climatology", "both")) {
        new_x <- .fill_climatology(new_x, d, ref = x)
      }

      filled_flag[idx] <- is.na(x) & !is.na(new_x)
      data[[v]][idx] <- new_x
    }

    report$filled[report$variable == v] <- sum(filled_flag)
    report$remaining[report$variable == v] <- sum(is.na(data[[v]]))

    if (flag) data[[paste0(v, "_filled")]] <- filled_flag
  }

  message(
    "Filled values (remaining NA): ",
    paste0(report$variable, " = ", report$filled, " (", report$remaining, ")", collapse = ", ")
  )

  data
}

# Linear interpolation of interior gaps no longer than `max_gap` days; short gaps at the
# series edges take the nearest observation.
.fill_linear <- function(x, d, max_gap) {
  n <- length(x)
  if (all(is.na(x))) return(x)

  runs <- rle(is.na(x))
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1

  for (k in which(runs$values)) {
    s <- starts[k]
    e <- ends[k]

    if (s == 1 || e == n) {
      if (as.numeric(d[e] - d[s]) + 1 <= max_gap) {
        x[s:e] <- if (s == 1) x[e + 1] else x[s - 1]
      }
      next
    }

    gap_days <- as.numeric(d[e + 1] - d[s - 1]) - 1
    if (gap_days <= max_gap) {
      x[s:e] <- approx(
        x    = as.numeric(d[c(s - 1, e + 1)]),
        y    = x[c(s - 1, e + 1)],
        xout = as.numeric(d[s:e])
      )$y
    }
  }
  x
}

# Fills NA with the mean of the same day of the year, computed from the observed values in `ref`.
.fill_climatology <- function(x, d, ref) {
  doy <- as.integer(format(d, "%j"))
  clim <- tapply(ref, doy, mean, na.rm = TRUE)

  miss <- is.na(x)
  x[miss] <- as.numeric(clim[as.character(doy[miss])])
  x[is.nan(x)] <- NA
  x
}
