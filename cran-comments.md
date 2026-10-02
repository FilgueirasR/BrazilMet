## Test environments BrazilMet 0.5.0

* local Windows 11, R 4.5.1
* win-builder (release and devel)

### R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE: unable to verify current time. This is an environment issue (no access to a time server) and not related to the package.

### Changes in this version

* New functions `water_balance()` and `fill_gaps()`.
* Download functions no longer modify the user's `timeout` option and fail gracefully with a warning or message when the data cannot be downloaded.
* Fixed a bug in `max_eto_grid_download()` for the monthly products.
* Added unit tests.


## Test environments BrazilMet 0.4.0

### R CMD check results

Duration: 4m 53.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


## Test environments BrazilMet 0.3.0

### R CMD check results

Duration: 27.2s

0 errors √ | 0 warnings √ | 1 note ✖

checking for future file timestamps ... NOTE
  unable to verify current time
  

## Test environments BrazilMet 0.2.0

### R CMD check results

Duration: 33.7s

0 errors √ | 0 warnings √ | 0 notes √

* This is a new release.

## Test environments BrazilMet 0.1.0
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)

### R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
