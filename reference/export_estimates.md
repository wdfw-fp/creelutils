# Export creel model estimates

Primary control function of the export, transform, and load (ETL)
process. Takes standardized model outputs and user-input parameters to
export the model estimates appropriately.

## Usage

``` r
export_estimates(
  resolved_params,
  est_catch_groups,
  analysis_lut,
  creel_estimates,
  conn
)
```

## Arguments

- resolved_params:

  User-input parameters defined during the model estimation process.

- est_catch_groups:

  Output of `resolve_catch_groups()`.

- analysis_lut:

  Lookup table created during the model estimation process which stores
  a session-specific analysis_id key and metadata about the analysis.

- creel_estimates:

  List object containing model estimates in a standardized format.
  Typically passed from 'transform_estimates' function.

- conn:

  Connection to the creel database.

## Value

Invisibly, NULL. Called for side effects.

## See also

Other ETL:
[`finalize_analysis_lut()`](https://wdfw-fp.github.io/creelutils/reference/finalize_analysis_lut.md),
[`json_conversion()`](https://wdfw-fp.github.io/creelutils/reference/json_conversion.md),
[`map_data_grade()`](https://wdfw-fp.github.io/creelutils/reference/map_data_grade.md),
[`prep_export()`](https://wdfw-fp.github.io/creelutils/reference/prep_export.md),
[`process_estimates_bss()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_bss.md),
[`process_estimates_pe()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_pe.md),
[`transform_estimates()`](https://wdfw-fp.github.io/creelutils/reference/transform_estimates.md),
[`update_analysis_comment()`](https://wdfw-fp.github.io/creelutils/reference/update_analysis_comment.md)
