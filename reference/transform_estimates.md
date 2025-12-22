# Transform individual model outputs into a single object

Transform individual model outputs into a single object

## Usage

``` r
transform_estimates(dwg, transformed_pe_data, transformed_bss_data)
```

## Arguments

- dwg:

  list object containing freshwater creel dataset, returned by
  [`fetch_dwg()`](https://wdfw-fp.github.io/creelutils/reference/fetch_dwg.md)

- transformed_pe_data:

  standardized outputs from the PE model, returned by
  `process_pe_estimates()`

- transformed_bss_data:

  standardized outputs from the BSS model, returned by
  `process_bss_estimates()`

## Value

list object containing standardized model estimates

## See also

Other ETL:
[`export_estimates()`](https://wdfw-fp.github.io/creelutils/reference/export_estimates.md),
[`json_conversion()`](https://wdfw-fp.github.io/creelutils/reference/json_conversion.md),
[`map_data_grade()`](https://wdfw-fp.github.io/creelutils/reference/map_data_grade.md),
[`prep_export()`](https://wdfw-fp.github.io/creelutils/reference/prep_export.md),
[`process_estimates_bss()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_bss.md),
[`process_estimates_pe()`](https://wdfw-fp.github.io/creelutils/reference/process_estimates_pe.md),
[`write_lut()`](https://wdfw-fp.github.io/creelutils/reference/write_lut.md),
[`write_stratum()`](https://wdfw-fp.github.io/creelutils/reference/write_stratum.md),
[`write_total()`](https://wdfw-fp.github.io/creelutils/reference/write_total.md)
