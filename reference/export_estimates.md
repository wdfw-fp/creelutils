# Export creel model estimates

Primary control function of the export, transform, and load (ETL)
process. Takes standardized model outputs and user-input parameters to
export the model estimates appropriately.

## Usage

``` r
export_estimates(params, analysis_lut, creel_estimates, conn = NULL)
```

## Arguments

- params:

  User-input parameters defined during the model estimation process.

- analysis_lut:

  Lookup table created during the model estimation process which stores
  a session-specific analysis_id key and metadata about the analysis.

- creel_estimates:

  List object containing model estimates in a standardized format.
  Typically passed from 'transform_estimates' function.

- conn:

  Database connection object. If NULL (default), a new connection will
  be established.

## Value

??

## See also

Other ETL:
[`json_conversion()`](https://wdfw-fp.github.io/creelutils/reference/json_conversion.md),
[`map_data_grade()`](https://wdfw-fp.github.io/creelutils/reference/map_data_grade.md),
[`prep_export()`](https://wdfw-fp.github.io/creelutils/reference/prep_export.md),
[`process_estimates_bss()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_bss.md),
[`process_estimates_pe()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_pe.md),
[`transform_estimates()`](https://wdfw-fp.github.io/creelutils/reference/transform_estimates.md),
[`write_lut()`](https://wdfw-fp.github.io/creelutils/reference/write_lut.md),
[`write_stratum()`](https://wdfw-fp.github.io/creelutils/reference/write_stratum.md),
[`write_total()`](https://wdfw-fp.github.io/creelutils/reference/write_total.md)
