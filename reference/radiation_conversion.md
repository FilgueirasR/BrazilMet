# Conversion factors for radiation

Function to convert the radiation data. The conversion name can be
understand as follow:

- conversion_1 = MJ m-2 day-1 to J cm-2 day-1;

- conversion_2 = MJ m-2 day-1 to cal cm-2 day-1;

- conversion_3 = MJ m-2 day-1 to W m-2;

- conversion_4 = MJ m-2 day-1 to mm day-1;

- conversion_5 = cal cm-2 day-1 to MJ m-2 day-1;

- conversion_6 = cal cm-2 day-1 to J cm-2 day-1;

- conversion_7 = cal cm-2 day-1 to W m-2;

- conversion_8 = cal cm-2 day-1 to mm day-1;

- conversion_9 = W m-2 to MJ m-2 day-1;

- conversion_10 = W m-2 to J cm-2 day-1;

- conversion_11 = W m-2 to cal cm-2 day-1;

- conversion_12 = W m-2 to mm day-1;

- conversion_13 = mm day-1 to MJ m-2 day-1;

- conversion_14 = mm day-1 to J cm-2 day-1;

- conversion_15 = mm day-1 to cal cm-2 day-1;

- conversion_16 = mm day-1 to W m-2.

## Usage

``` r
radiation_conversion(data_to_convert, conversion_name)
```

## Arguments

- data_to_convert:

  A data.frame with radiation values to convert.

- conversion_name:

  A character with the conversion_name summarize in the description of
  this function.

## Value

A data.frame object wit the converted radiation.

## Author

Roberto Filgueiras, Luan P. Venancio, Catariny C. Aleman and Fernando F.
da Cunha

## Examples

``` r
if (FALSE) { # \dontrun{
radiation_conversion_df <- radiation_conversion(
  data_to_convert = df$rad,
  conversion_name = "conversion_1"
)
} # }
```
