# Get analysis lookup table

Convenience wrapper to query the model_analysis_lut table with optional
filtering.

## Usage

``` r
query_analysis_lut(conn = NULL, analysis_id = NULL, fishery_name = NULL)
```

## Arguments

- conn:

  A valid database connection from
  [`establish_db_con()`](https://wdfw-fp.github.io/creelutils/reference/establish_db_con.md)

- analysis_id:

  Optional character string for exact analysis_id match

- fishery_name:

  Optional character string for pattern matching in analysis_name

## Value

Tibble of matching records from model_analysis_lut
