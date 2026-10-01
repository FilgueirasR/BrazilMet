#' Sequential water balance (Thornthwaite-Mather)
#'
#' @description Calculates the soil water balance using precipitation and
#' potential evapotranspiration for a sequence of periods, following the
#' Thornthwaite & Mather (1955) method. The time step of the balance (daily,
#' weekly, monthly or any other custom interval) is entirely defined by the
#' resolution of `ppt` and `etp`: the recursive logic (accumulated potential
#' water loss, soil water storage, actual evapotranspiration, deficit, excess)
#' is the same regardless of the period length, so daily PPT/ETP produce a
#' daily balance, weekly sums produce a weekly balance, and so on. The
#' available water capacity of the soil (AWC, also known as CAD) can be
#' freely adjusted to match the crop/soil being modeled.
#'
#' @param ppt A numeric vector with precipitation (or irrigation + precipitation)
#'   for each period, in the same units as `etp`.
#' @param etp A numeric vector with potential (or reference) evapotranspiration
#'   for each period, the same length as `ppt`.
#' @param AWC A single positive numeric value with the available water capacity
#'   (CAD) of the soil, in the same depth units as `ppt`/`etp` (e.g. mm). Can be
#'   set to any value required by the soil/crop being simulated.
#' @param period Optional vector identifying each period (e.g. dates, day of
#'   year, week number, or month number), the same length as `ppt`. Used only
#'   to label the output; it does not affect the calculation.
#' @param year Optional vector identifying the year associated with each
#'   period, the same length as `ppt`. Used only to label the output.
#' @param group Optional vector (e.g. station ID or site name), the same
#'   length as `ppt`, used to run independent balances for different series
#'   stacked in the same input (each group restarts the recursion assuming the
#'   soil starts at field capacity). Rows of the same group must be contiguous
#'   and already ordered by time. If `NULL` (default), a single continuous
#'   balance is calculated for the whole input.
#' @param time_step A label indicating the time step represented by each row:
#'   `"daily"`, `"weekly"`, `"monthly"` or `"custom"`. This is only stored for
#'   reference/documentation purposes and does not change the calculation.
#'
#' @return A data.frame with the (optional) `group`, `year` and `period`
#'   columns, the inputs `ppt` and `etp`, the `time_step` used, and the
#'   calculated columns: `ppt_etp` (P - ETP), `neg_ac` (accumulated potential
#'   water loss, "negativo acumulado"), `arm` (soil water storage), `alt`
#'   (change in storage), `etr` (actual evapotranspiration), `def` (water
#'   deficit), `exc` (water excess), and `awc_arm` (percentage of AWC stored).
#'
#' @examples
#' \dontrun{
#' # Monthly balance
#' balance_monthly <- water_balance(
#'   ppt = c(120, 110, 100, 80, 60, 40, 30, 35, 55, 75, 95, 115),
#'   etp = c(80, 85, 90, 95, 100, 105, 110, 105, 100, 95, 90, 85),
#'   AWC = 100,
#'   period = 1:12,
#'   year = rep(2024, 12),
#'   time_step = "monthly"
#' )
#'
#' # Daily balance with a smaller AWC (e.g. shallow-rooted crop)
#' balance_daily <- water_balance(
#'   ppt = daily_df$rainfall_mm,
#'   etp = daily_df$eto,
#'   AWC = 30,
#'   period = daily_df$date,
#'   time_step = "daily"
#' )
#' }
#'
#' @export
#' @author Roberto Filgueiras

water_balance <- function(ppt, etp, AWC, period = NULL, year = NULL, group = NULL,
                           time_step = c("daily", "weekly", "monthly", "custom")) {

  time_step <- match.arg(time_step)

  n <- length(ppt)

  if (length(etp) != n) {
    stop("'ppt' and 'etp' must have the same length.")
  }
  if (!is.numeric(ppt) || !is.numeric(etp)) {
    stop("'ppt' and 'etp' must be numeric vectors.")
  }
  if (anyNA(ppt) || anyNA(etp)) {
    stop("'ppt' and 'etp' cannot contain NA values.")
  }
  if (!is.numeric(AWC) || length(AWC) != 1 || AWC <= 0) {
    stop("'AWC' (CAD) must be a single positive numeric value.")
  }
  if (!is.null(period) && length(period) != n) {
    stop("'period' must have the same length as 'ppt' and 'etp'.")
  }
  if (!is.null(year) && length(year) != n) {
    stop("'year' must have the same length as 'ppt' and 'etp'.")
  }
  if (!is.null(group) && length(group) != n) {
    stop("'group' must have the same length as 'ppt' and 'etp'.")
  }

  ppt_etp <- ppt - etp

  # Recursively applies the Thornthwaite-Mather soil water storage logic for
  # one continuous sequence, assuming the soil starts at field capacity (AWC).
  run_sequence <- function(ppt_etp) {

    m <- length(ppt_etp)
    neg_ac <- numeric(m)
    arm <- numeric(m)

    prev_arm <- AWC
    prev_neg_ac <- 0
    prev_ppt_etp <- 0 # pretend the soil comes from a previous period at field capacity

    for (i in seq_len(m)) {

      if (ppt_etp[i] < 0) {
        neg_ac[i] <- round(prev_neg_ac + ppt_etp[i])
        arm[i] <- round(AWC * exp(neg_ac[i] / AWC))
      } else if ((ppt_etp[i] + prev_arm) >= AWC) {
        arm[i] <- AWC
        neg_ac[i] <- 0
      } else {
        arm[i] <- round(ppt_etp[i] + prev_arm)
        neg_ac[i] <- round(AWC * log(arm[i] / AWC))
      }

      prev_arm <- arm[i]
      prev_neg_ac <- neg_ac[i]
      prev_ppt_etp <- ppt_etp[i]
    }

    list(neg_ac = neg_ac, arm = arm)
  }

  if (is.null(group)) {
    result <- run_sequence(ppt_etp)
    neg_ac <- result$neg_ac
    arm <- result$arm
  } else {
    neg_ac <- numeric(n)
    arm <- numeric(n)
    for (g in unique(group)) {
      idx <- which(group == g)
      result <- run_sequence(ppt_etp[idx])
      neg_ac[idx] <- result$neg_ac
      arm[idx] <- result$arm
    }
  }

  # Change in soil water storage (undefined/zero for the first period of
  # each group, since there is no previous period to compare against).
  alt <- arm - c(AWC, arm[-n])
  if (!is.null(group)) {
    first_idx <- !duplicated(group)
    alt[first_idx] <- arm[first_idx] - AWC
  }

  # Actual evapotranspiration (cannot exceed ETP, which rounding of `alt` could cause)
  etr <- ifelse(ppt_etp < 0, pmin(round(ppt + abs(alt)), etp), etp)

  # Water deficit
  def <- etp - etr

  # Water excess
  exc <- ifelse(arm < AWC, 0, round(ppt_etp - alt))

  # Percentage of AWC stored in the soil
  awc_arm <- round((arm / AWC) * 100, 0)

  conteudo <- data.frame(
    ppt = ppt,
    etp = etp,
    time_step = time_step,
    ppt_etp = ppt_etp,
    neg_ac = neg_ac,
    arm = arm,
    alt = alt,
    etr = etr,
    def = def,
    exc = exc,
    awc_arm = awc_arm
  )

  if (!is.null(group)) conteudo <- cbind(group = group, conteudo)
  if (!is.null(year)) conteudo <- cbind(year = year, conteudo)
  if (!is.null(period)) conteudo <- cbind(period = period, conteudo)

  return(conteudo)

}