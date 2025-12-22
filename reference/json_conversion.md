# Convert model estimate metadata objects to json format

Convert model estimate metadata objects to json format

## Usage

``` r
json_conversion(type, params, analysis_lut)
```

## Arguments

- type:

  type of metadata to convert to JSON format for storage in the
  analysis_lut

- params:

  user-input parameters defined during the model estimation process

- analysis_lut:

  lookup table created during the model estimation process which stores
  a session-specific analysis_id key and metadata about the analysis.

## Value

Returns the analysis_lut with a new field containing a JSON string of
the `type`

## See also

Other ETL:
[`export_estimates()`](https://wdfw-fp.github.io/creelutils/reference/export_estimates.md),
[`map_data_grade()`](https://wdfw-fp.github.io/creelutils/reference/map_data_grade.md),
[`prep_export()`](https://wdfw-fp.github.io/creelutils/reference/prep_export.md),
[`process_estimates_bss()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_bss.md),
[`process_estimates_pe()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_pe.md),
[`transform_estimates()`](https://wdfw-fp.github.io/creelutils/reference/transform_estimates.md),
[`write_lut()`](https://wdfw-fp.github.io/creelutils/reference/write_lut.md),
[`write_stratum()`](https://wdfw-fp.github.io/creelutils/reference/write_stratum.md),
[`write_total()`](https://wdfw-fp.github.io/creelutils/reference/write_total.md)

## Examples

``` r
if (FALSE) { # \dontrun{
json_conversion(type = "script")
json_conversion(type = "r_session")
} # }
```
