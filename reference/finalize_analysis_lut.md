# Finalize the analysis look-up table for export

Completes the session `analysis_lut` created by
`generate_analysis_lut()` immediately prior to export. Adds ETL-time
metadata (r_session_json, data_grade), and, when a database connection
is supplied, resolves project/fishery names to database UUIDs and
validates the final column set against the model_analysis_lut schema.

## Usage

``` r
finalize_analysis_lut(analysis_lut, resolved_params, conn = NULL)
```

## Arguments

- analysis_lut:

  lookup table created during the model estimation process which stores
  a session-specific analysis_id key and metadata about the analysis.

- resolved_params:

  user-input parameters defined during the model estimation process.

- conn:

  a valid `DBI` connection, or NULL for local export. @seealso
  [`connect_creel_db()`](https://wdfw-fp.github.io/creelutils/reference/connect_creel_db.md)

## Value

The finalized analysis_lut, ready for
[`write_lut()`](https://wdfw-fp.github.io/creelutils/reference/write_lut.md)
(database) or local archiving.

## Details

With `con = NULL` (local export) the lut retains `project_name` and
`fishery_name`; id resolution is deterministic and occurs whenever the
session is later exported to the database.

## See also

Other ETL:
[`export_estimates()`](https://wdfw-fp.github.io/creelutils/reference/export_estimates.md),
[`json_conversion()`](https://wdfw-fp.github.io/creelutils/reference/json_conversion.md),
[`map_data_grade()`](https://wdfw-fp.github.io/creelutils/reference/map_data_grade.md),
[`prep_export()`](https://wdfw-fp.github.io/creelutils/reference/prep_export.md),
[`process_estimates_bss()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_bss.md),
[`process_estimates_pe()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_pe.md),
[`transform_estimates()`](https://wdfw-fp.github.io/creelutils/reference/transform_estimates.md),
[`update_analysis_comment()`](https://wdfw-fp.github.io/creelutils/reference/update_analysis_comment.md)
