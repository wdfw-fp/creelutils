# Prep export

Prepares standardized model estimates for database export. Joins the
catch group crosswalk (returned by `resolve_catch_groups()` in
CreelEstimates) to attach `model_catch_group_id` to catch and CPUE
estimate rows, joins angler type UUIDs on the stratum table, and drops
local-only name/label columns to match the model_estimates_stratum and
model_estimates_total schemas.

## Usage

``` r
prep_export(conn, creel_estimates, catch_group_lut)
```

## Arguments

- conn:

  a valid `DBI` connection. @seealso
  [`establish_db_con()`](https://wdfw-fp.github.io/creelutils/reference/establish_db_con.md)

- creel_estimates:

  list object containing standardized model estimates returned by
  [`transform_estimates()`](https://wdfw-fp.github.io/creelutils/reference/transform_estimates.md).

- catch_group_lut:

  catch group crosswalk for the fishery, returned by
  `resolve_catch_groups()`. Must contain `combined_catch_group` and
  `model_catch_group_id`.

## Value

`creel_estimates` with `stratum` and `total` tables prepared for
[`write_stratum()`](https://wdfw-fp.github.io/creelutils/reference/write_stratum.md)
and
[`write_total()`](https://wdfw-fp.github.io/creelutils/reference/write_total.md).

## Details

Effort rows carry no catch group; their `model_catch_group_id` is NA and
written as NULL (the column is nullable on both tables).

## See also

Other ETL:
[`export_estimates()`](https://wdfw-fp.github.io/creelutils/reference/export_estimates.md),
[`finalize_analysis_lut()`](https://wdfw-fp.github.io/creelutils/reference/finalize_analysis_lut.md),
[`json_conversion()`](https://wdfw-fp.github.io/creelutils/reference/json_conversion.md),
[`map_data_grade()`](https://wdfw-fp.github.io/creelutils/reference/map_data_grade.md),
[`process_estimates_bss()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_bss.md),
[`process_estimates_pe()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_pe.md),
[`transform_estimates()`](https://wdfw-fp.github.io/creelutils/reference/transform_estimates.md),
[`update_analysis_comment()`](https://wdfw-fp.github.io/creelutils/reference/update_analysis_comment.md)
