# Download maximum reference evapotranspiration (ETo) grids for Brazil

Downloads maximum reference evapotranspiration (ETo) grids for Brazil,
intended for irrigation design purposes. The dataset was developed by
Dias (2018).

## Usage

``` r
max_eto_grid_download(dir_out, product = "max_12_months")
```

## Arguments

- dir_out:

  Character. Directory where the downloaded raster file will be saved.

- product:

  Character. Specifies which maximum ETo product to download. Available
  options include:

  - `max_12_months`: maximum ETo over the full year.

  - `max_jan` to `max_dec`: monthly maximum ETo for each respective
    month (January to December).

## Value

A \`SpatRaster\` object containing the downloaded maximum reference
evapotranspiration (ETo) grid.

## References

Dias, S. H. B. (2018). \*Evapotranspiração de referência para projeto de
irrigação no Brasil utilizando o produto MOD16\*. Dissertação (Mestrado)
– Universidade Federal de Viçosa.

## Author

Roberto Filgueiras.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download the annual maximum ETo grid
img_max_eto <- max_eto_grid_download(dir_out = tempdir(), product = "max_12_months")
} # }
```
