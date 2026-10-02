# Downloads `url` to `destfile`, restoring the user's options afterwards. Returns TRUE on success
# and FALSE (with a warning) otherwise, so callers can fail gracefully without internet access.
# INMET resets connections from clients without a browser-like User-Agent.
.download_file <- function(url, destfile, timeout = 600) {
  old <- options(
    timeout = timeout,
    HTTPUserAgent = "Mozilla/5.0 (compatible; BrazilMet R package)"
  )
  on.exit(options(old), add = TRUE)

  tryCatch({
    utils::download.file(url, destfile, mode = "wb", cacheOK = FALSE, quiet = TRUE)
    TRUE
  }, error = function(e) {
    warning("Failed to download '", url, "': ", conditionMessage(e), call. = FALSE)
    FALSE
  })
}
