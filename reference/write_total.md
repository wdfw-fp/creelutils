# Write Total

Write Total

## Usage

``` r
write_total(con, creel_estimates_db)
```

## Arguments

- con:

  a valid DBI connection. @seealso
  [`establish_db_con()`](https://wdfw-fp.github.io/creelutils/reference/establish_db_con.md)

- creel_estimates_db:

  a list object containing the standardized model outputs that have been
  processed by
  [`prep_export()`](https://wdfw-fp.github.io/creelutils/reference/prep_export.md)
  to join certain fields with database lookup tables prior to
  exportation.

## See also

Other ETL:
[`export_estimates()`](https://wdfw-fp.github.io/creelutils/reference/export_estimates.md),
[`json_conversion()`](https://wdfw-fp.github.io/creelutils/reference/json_conversion.md),
[`map_data_grade()`](https://wdfw-fp.github.io/creelutils/reference/map_data_grade.md),
[`prep_export()`](https://wdfw-fp.github.io/creelutils/reference/prep_export.md),
[`process_estimates_bss()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_bss.md),
[`process_estimates_pe()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_pe.md),
[`transform_estimates()`](https://wdfw-fp.github.io/creelutils/reference/transform_estimates.md),
[`write_lut()`](https://wdfw-fp.github.io/creelutils/reference/write_lut.md),
[`write_stratum()`](https://wdfw-fp.github.io/creelutils/reference/write_stratum.md)
