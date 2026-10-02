make_df <- function(x) {
  data.frame(date = as.Date("2024-01-01") + seq_along(x) - 1, tair_mean_c = x)
}

test_that("short interior gaps are interpolated and long ones are kept", {
  df <- make_df(c(1, NA, 3, NA, NA, NA, NA, 8, 9, 10))

  out <- suppressMessages(fill_gaps(df, vars = "tair_mean_c", max_gap = 3))

  expect_equal(out$tair_mean_c[2], 2)
  expect_true(all(is.na(out$tair_mean_c[4:7])))
  expect_equal(out$tair_mean_c_filled, c(FALSE, TRUE, rep(FALSE, 8)))

  out4 <- suppressMessages(fill_gaps(df, vars = "tair_mean_c", max_gap = 4))
  expect_equal(out4$tair_mean_c[3:8], 3:8)
})

test_that("short gaps at the edges take the nearest observation", {
  df <- make_df(c(NA, 2, 3, NA))

  out <- suppressMessages(fill_gaps(df, vars = "tair_mean_c", max_gap = 3))

  expect_equal(out$tair_mean_c, c(2, 2, 3, 3))
})

test_that("climatology fills long gaps with the mean of the same day of the year", {
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day")
  doy <- as.numeric(format(dates, "%j"))
  df <- data.frame(date = dates, tair_mean_c = doy + (format(dates, "%Y") == "2024"))
  df$tair_mean_c[10:20] <- NA

  out <- suppressMessages(fill_gaps(df, vars = "tair_mean_c", method = "both", max_gap = 3))

  expect_false(anyNA(out$tair_mean_c))
  expect_equal(out$tair_mean_c[10], 11) # only 2024 is observed on that day
})

test_that("groups are filled independently", {
  df <- rbind(
    data.frame(st = "A", date = as.Date("2024-01-01") + 0:3, tair_mean_c = c(1, NA, 3, 4)),
    data.frame(st = "B", date = as.Date("2024-01-01") + 0:3, tair_mean_c = c(10, 11, NA, 13))
  )

  out <- suppressMessages(fill_gaps(df, vars = "tair_mean_c", group = "st"))

  expect_equal(out$tair_mean_c, c(1, 2, 3, 4, 10, 11, 12, 13))
})

test_that("flag = FALSE returns no flag columns", {
  out <- suppressMessages(fill_gaps(make_df(c(1, NA, 3)), vars = "tair_mean_c", flag = FALSE))

  expect_false("tair_mean_c_filled" %in% names(out))
})

test_that("invalid input is rejected", {
  df <- make_df(c(1, NA, 3))
  df$rainfall_mm <- 0

  expect_error(fill_gaps(df, vars = "rainfall_mm"), "Precipitation")
  expect_error(fill_gaps(df, vars = "missing_column"), "names of columns")
  expect_error(fill_gaps(df[3:1, ], vars = "tair_mean_c"), "sorted")
  expect_error(fill_gaps(df, vars = "tair_mean_c", max_gap = 0), "max_gap")
})
