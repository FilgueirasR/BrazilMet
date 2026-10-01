# Sequential water balance (Thornthwaite-Mather)

Calculates the soil water balance using precipitation and potential
evapotranspiration for a sequence of periods, following the Thornthwaite
& Mather (1955) method. The time step of the balance (daily, weekly,
monthly or any other custom interval) is entirely defined by the
resolution of \`ppt\` and \`etp\`: the recursive logic (accumulated
potential water loss, soil water storage, actual evapotranspiration,
deficit, excess) is the same regardless of the period length, so daily
PPT/ETP produce a daily balance, weekly sums produce a weekly balance,
and so on. The available water capacity of the soil (AWC, also known as
CAD) can be freely adjusted to match the crop/soil being modeled.

## Usage

``` r
water_balance(
  ppt,
  etp,
  AWC,
  period = NULL,
  year = NULL,
  group = NULL,
  time_step = c("daily", "weekly", "monthly", "custom")
)
```

## Arguments

- ppt:

  A numeric vector with precipitation (or irrigation + precipitation)
  for each period, in the same units as \`etp\`.

- etp:

  A numeric vector with potential (or reference) evapotranspiration for
  each period, the same length as \`ppt\`.

- AWC:

  A single positive numeric value with the available water capacity
  (CAD) of the soil, in the same depth units as \`ppt\`/\`etp\` (e.g.
  mm). Can be set to any value required by the soil/crop being
  simulated.

- period:

  Optional vector identifying each period (e.g. dates, day of year, week
  number, or month number), the same length as \`ppt\`. Used only to
  label the output; it does not affect the calculation.

- year:

  Optional vector identifying the year associated with each period, the
  same length as \`ppt\`. Used only to label the output.

- group:

  Optional vector (e.g. station ID or site name), the same length as
  \`ppt\`, used to run independent balances for different series stacked
  in the same input (each group restarts the recursion assuming the soil
  starts at field capacity). Rows of the same group must be contiguous
  and already ordered by time. If \`NULL\` (default), a single
  continuous balance is calculated for the whole input.

- time_step:

  A label indicating the time step represented by each row: \`"daily"\`,
  \`"weekly"\`, \`"monthly"\` or \`"custom"\`. This is only stored for
  reference/documentation purposes and does not change the calculation.

## Value

A data.frame with the (optional) \`group\`, \`year\` and \`period\`
columns, the inputs \`ppt\` and \`etp\`, the \`time_step\` used, and the
calculated columns: \`ppt_etp\` (P - ETP), \`neg_ac\` (accumulated
potential water loss, "negativo acumulado"), \`arm\` (soil water
storage), \`alt\` (change in storage), \`etr\` (actual
evapotranspiration), \`def\` (water deficit), \`exc\` (water excess),
and \`awc_arm\` (percentage of AWC stored).

## Author

Roberto Filgueiras

## Examples

``` r
if (FALSE) { # \dontrun{
# Monthly balance
balance_monthly <- water_balance(
  ppt = c(120, 110, 100, 80, 60, 40, 30, 35, 55, 75, 95, 115),
  etp = c(80, 85, 90, 95, 100, 105, 110, 105, 100, 95, 90, 85),
  AWC = 100,
  period = 1:12,
  year = rep(2024, 12),
  time_step = "monthly"
)

# Daily balance with a smaller AWC (e.g. shallow-rooted crop)
balance_daily <- water_balance(
  ppt = daily_df$rainfall_mm,
  etp = daily_df$eto,
  AWC = 30,
  period = daily_df$date,
  time_step = "daily"
)
} # }
```
