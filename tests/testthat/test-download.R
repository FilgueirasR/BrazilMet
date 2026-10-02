test_that(".download_file fails gracefully and restores options", {
  old_timeout <- getOption("timeout")
  old_agent <- getOption("HTTPUserAgent")

  ok <- suppressWarnings(
    BrazilMet:::.download_file("https://invalid.invalid/file.zip", tempfile(), timeout = 5)
  )

  expect_false(ok)
  expect_equal(getOption("timeout"), old_timeout)
  expect_equal(getOption("HTTPUserAgent"), old_agent)
})

test_that("max_eto_grid_download validates its arguments", {
  expect_error(max_eto_grid_download(tempdir(), product = "max_xyz"), "'product' must be one of")
  expect_error(max_eto_grid_download(file.path(tempdir(), "does_not_exist")), "does not exist")
})
