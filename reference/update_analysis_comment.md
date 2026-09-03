# Update the comment on an existing analysis record

Update the comment on an existing analysis record

## Usage

``` r
update_analysis_comment(conn, analysis_id, comment)
```

## Arguments

- conn:

  An open DBI connection to the creel database.

- analysis_id:

  UUID of the analysis row to update.

- comment:

  New comment text.

## Value

Invisibly, the number of rows updated (1 on success).

## See also

Other ETL:
[`export_estimates()`](https://wdfw-fp.github.io/creelutils/reference/export_estimates.md),
[`finalize_analysis_lut()`](https://wdfw-fp.github.io/creelutils/reference/finalize_analysis_lut.md),
[`json_conversion()`](https://wdfw-fp.github.io/creelutils/reference/json_conversion.md),
[`map_data_grade()`](https://wdfw-fp.github.io/creelutils/reference/map_data_grade.md),
[`prep_export()`](https://wdfw-fp.github.io/creelutils/reference/prep_export.md),
[`process_estimates_bss()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_bss.md),
[`process_estimates_pe()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_pe.md),
[`transform_estimates()`](https://wdfw-fp.github.io/creelutils/reference/transform_estimates.md)

## Examples

``` r
if (FALSE) { # \dontrun{
update_analysis_comment(
  con,
  analysis_id = "d609a0e4-653c-4225-b6e3-24c9b236f882",
  comment = "Re-upload after filtering outlier interview in section 3 on the 12th"
)
} # }
```
