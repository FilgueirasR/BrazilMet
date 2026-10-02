test_that("storage never exceeds AWC and deficit/excess are non-negative", {
  set.seed(1)
  ppt <- pmax(0, rnorm(200, 3, 6))
  etp <- pmax(0, rnorm(200, 4, 1))

  bal <- water_balance(ppt, etp, AWC = 100, time_step = "daily")

  expect_equal(nrow(bal), 200)
  expect_true(all(bal$arm <= 100))
  expect_true(all(bal$def >= 0))
  expect_true(all(bal$exc >= 0))
  expect_true(all(bal$etr <= bal$etp))
})

test_that("a wet period keeps the soil at field capacity", {
  bal <- water_balance(ppt = rep(10, 5), etp = rep(2, 5), AWC = 50)

  expect_true(all(bal$arm == 50))
  expect_true(all(bal$def == 0))
  expect_equal(bal$etr, rep(2, 5))
})

test_that("groups restart the balance at field capacity", {
  ppt <- c(0, 0, 0, 0)
  etp <- c(5, 5, 5, 5)

  bal <- water_balance(ppt, etp, AWC = 100, group = c("A", "A", "B", "B"))

  expect_equal(bal$arm[1], bal$arm[3])
})

test_that("labels are stored in the output", {
  bal <- water_balance(c(5, 0), c(3, 3), AWC = 50, period = 1:2, year = c(2024, 2024), time_step = "monthly")

  expect_equal(bal$period, 1:2)
  expect_equal(bal$year, c(2024, 2024))
  expect_equal(unique(bal$time_step), "monthly")
})

test_that("invalid input is rejected", {
  expect_error(water_balance(1:3, 1:2, AWC = 100), "same length")
  expect_error(water_balance(c(1, NA), c(1, 1), AWC = 100), "NA")
  expect_error(water_balance(1:2, 1:2, AWC = -1), "AWC")
})
